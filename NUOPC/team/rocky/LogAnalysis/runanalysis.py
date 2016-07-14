#!/usr/bin/python

import sys
import getopt
import json
import pprint

# The comps dictionary maintains the current state.
# This is the basic structure:
#
# comps {
#    "ATM": {"kind" : "MODEL",
#            "IMP" : {"IPDv01p1":"1", ...},
#            "RPM" : {"RunPhase1":"1", ...},
#            "FPM" : {"FinalizePhase1":"1",...},
#            "States" : [
#                       { "namespace":"ATM", "intent":"Import",
#                         "Fields":[{"standardName":"sst", "connected":"true"}, {}, ...]
#                       },
#	                { "namespace":"ATM", "intent":"Export",
#                         "States":[ {...}, {...} ]  #nested states 
#                       }]
#            "RunSequence" : [0, "IPDv01p1", "IPDv01p2", ...]  # sequence of phases
#            
#           },
#    "OCN": {...}
# }
#
#

comps = {}

def populateComponent(jComp):               
    if "CompName$NUOPC$Instance" in jComp:
        compName = jComp["CompName$NUOPC$Instance"]
        if compName not in comps:
            comps[compName] = {}
        comp = comps[compName]
        comp["kind"] = jComp.get("Kind$NUOPC$Component")
        comp["RunSequence"] = []
        #comp["ImportFields"] = {}
	#comp["ExportFields"] = {}
        #comp["UnknownFields"] = {}

        comp["States"] = []
        
        if "InitializePhaseMap$NUOPC$Component" in jComp:
            comp["IPM"] = {}
            if type(jComp["InitializePhaseMap$NUOPC$Component"]) is list:
                for kv in jComp["InitializePhaseMap$NUOPC$Component"]:
                    kvl = kv.split("=")
                    comp["IPM"][kvl[0]] = kvl[1]
            else:
                kvl = jComp["InitializePhaseMap$NUOPC$Component"].split("=")
                comp["IPM"][kvl[0]] = kvl[1]

        # only for Drivers
        if "InternalInitializePhaseMap$NUOPC$Driver" in jComp:
            comp["IIPM"] = {}
            if type(jComp["InternalInitializePhaseMap$NUOPC$Driver"]) is list:
                for kv in jComp["InternalInitializePhaseMap$NUOPC$Driver"]:
                    kvl = kv.split("=")
                    comp["IIPM"][kvl[0]] = kvl[1]
            else:
                kvl = jComp["InternalInitializePhaseMap$NUOPC$Driver"].split("=")
                comp["IIPM"][kvl[0]] = kvl[1]

        if "RunPhaseMap$NUOPC$Component" in jComp:
            comp["RPM"] = {}
            if type(jComp["RunPhaseMap$NUOPC$Component"]) is list:
                for kv in jComp["RunPhaseMap$NUOPC$Component"]:
                    kvl = kv.split("=")
                    comp["RPM"][kvl[0]] = kvl[1]
            else:
                kvl = jComp["RunPhaseMap$NUOPC$Component"].split("=")
                comp["RPM"][kvl[0]] = kvl[1]

        if "FinalizePhaseMap$NUOPC$Component" in jComp:
            comp["FPM"] = {}
            if type(jComp["FinalizePhaseMap$NUOPC$Component"]) is list:
                for kv in jComp["FinalizePhaseMap$NUOPC$Component"]:
                    kvl = kv.split("=")
                    comp["FPM"][kvl[0]] = kvl[1]
            else:
                kvl = jComp["FinalizePhaseMap$NUOPC$Component"].split("=")
                comp["FPM"][kvl[0]] = kvl[1]

def lookupPhaseLabels(compName, method, phase):
    retList = []
    
    if not comps.get(compName):
	return retList

    phaseMap = {}
    if method == "init":
        # combine dictionaries into one
        phaseMap = comps[compName].get("IPM",{})
        phaseMapInternal = comps[compName].get("IIPM",{})
        phaseMap = dict(phaseMap.items() + phaseMapInternal.items())
    elif method == "run":
        phaseMap = comps[compName].get("RPM",{})
    elif method == "final":
        phaseMap = comps[compName].get("FPM",{})

    for (k,v) in phaseMap.items():
        if v == phase:
            retList.append(k)
    return retList

def handlePhase(jEvent, level):
    phaseLabels = lookupPhaseLabels(jEvent["compName"], jEvent["method"], jEvent["phase"])
    if len(phaseLabels) == 0:
        phaseStr = jEvent["phase"]
    else:
        phaseStr = " ".join(str(x) for x in phaseLabels)
        
    timestamp = ""
    if jEvent.get("currTime", "") != "":
	    timestamp = "["+jEvent["currTime"]+"]"
   
    if jEvent["name"] == "start_phase":
	print ("    "*level) + "=>" + jEvent["compName"] + " (" + jEvent["method"] + " " + phaseStr + ")\t" + timestamp
        comps[jEvent["compName"]]["RunSequence"].append(phaseStr)
    else:
	print ("    "*level) + "<=" + jEvent["compName"] + " (" + jEvent["method"] + " " + phaseStr + ")\t" + timestamp
        pass

def handleState(jState, level, stateList):
    namespace = jState.get("Namespace$NUOPC$Instance")
    intent = jState.get("intent$NUOPC$Instance", "")
    if "IMPORT" in intent:
        intent = "Import"
    elif "EXPORT" in intent:
 	intent = "Export"
    else:
	intent = "Unknown"
        
    # see if already in list
    try:
        stateDict = next(s for s in stateList if s.get("namespace")==namespace and s.get("intent")==intent)
    except StopIteration:
        stateDict = {}
        stateDict["namespace"] = namespace
        stateDict["intent"] = intent
        stateList.append(stateDict)

    # handle linked
    linkList = jState.get("linkList", [])
    if len(linkList) > 0:
        if "field" in linkList[0].keys():
            fieldList = stateDict.setdefault("Fields", [])
            handleFieldList(linkList, level, fieldList)
        if "state" in linkList[0].keys():
            stateList = stateDict.setdefault("States", [])
            handleStateList(linkList, level, stateList)
   
    #print ("    "*level)  + " => " + stateIntent + ": " + str(len(compFields)) + " fields: " # + str(compFields.keys())    

def handleStateList(jStateList, level, stateList):
    for s in jStateList:
        handleState(s["state"], level, stateList)

def handleFieldList(jFieldList, level, fieldList):
    for f in jFieldList:
        fieldDict = f["field"]
        standardName = fieldDict.get("StandardName$CF$Extended")
        connected = fieldDict.get("Connected$NUOPC$Instance")
        units = fieldDict.get("Units$CF$General")

        # see if already in list
        try:
            fieldItem = next(i for i in fieldList if i.get("standardName")==standardName)
        except StopIteration:
            fieldItem = {}
            fieldItem["standardName"] = standardName
            fieldList.append(fieldItem)

        fieldItem["connected"] = connected
        fieldItem["units"] = units

def printBanner(text):
    print ""
    print "*"*77
    print text
    print "*"*77
    print ""


def main(argv):
#    try:
#        opts, args = getopt.getopt(argv[1:],"hi:o:",["ifile=","ofile="])
#    except getopt.GetoptError:
#        print 'runanalysis.py <logfile>'
#        sys.exit(2)
#    
#    for opt, arg in opts:
#        if opt == '-h':
#            print 'runanalysis.py <logfile>'
#            sys.exit()
#        elif opt in ("-i", "--ifile"):
#            inputfile = arg
#        elif opt in ("-o", "--ofile"):
#            outputfile = arg
    
    logfile = argv[1]
    print "Reading log file: " + logfile
   
    errorCount = 0
    warningCount = 0

    #determine where in each line JSON starts
    #differs due to PET0, PET00, PET000, etc.
    idx = logfile.find(".")
    jsonstart = 42+idx-5
    
    #first pass, get component info
    with open(logfile) as f:
        for line in f:
            if line[20:24] == "JSON":
		try:	                
		    jLine = json.loads(line[jsonstart:])
		except:
		    print "Failed to parse JSON:"
		    print line[jsonstart:]
		    print ""
		    sys.exit(2)             
		if "comp" in jLine:
                    populateComponent(jLine["comp"])
            elif line[20:24] == "WARN":
                warningCount += 1
            elif line[20:24] == "ERRO":
                errorCount += 1

    # print summary info
    printBanner("SUMMARY")
    print "Total Errors: " + str(errorCount)
    print "Total Warnings: " + str(warningCount)

    printBanner("RUN SEQUENCE")

    #second pass, read events
    with open(logfile) as f:
        level = 0
        for line in f:
            if (line[20:24] == "JSON"):
                jLine = json.loads(line[jsonstart:])
                if "event" in jLine:
                    jEvent = jLine["event"]
                    if jEvent["name"] == "start_phase":
                        handlePhase(jEvent, level)
                        level += 1
                    elif jEvent["name"] == "end_phase":
                        level -= 1
                        handlePhase(jEvent, level)
                elif "state" in jLine:
                    jState = jLine["state"]
                    namespace = jState.get("Namespace$NUOPC$Instance")
                    if namespace is not None:
                        compDict = comps.get(namespace)
                        if compDict is not None:
                            stateList = compDict["States"]
                            handleState(jState, level, stateList)
                        else:
                            #print "Warning: No component record for state: " + namespace
                            pass
                    else:
                        #print "Warning:  State has no namespace"
                        pass
    

    printBanner("COMPONENT INFO")

    for c in comps:
        if comps[c].get("kind"):
            print ""
            print ""
            print "Component:  " + c + " (" + str(comps[c].get("kind")) + ")"
            print "*"*77
            print ""
            print "  Import Fields:"
            print "  " + ("-"*75)
            printState(next((s for s in comps[c]["States"] if s["intent"]=="Import"), None))
            print ""
	    print "  Export Fields:"
            print "  " + ("-"*75)
            printState(next((s for s in comps[c]["States"] if s["intent"]=="Export"), None))
    
#    printBanner("INTERNAL STATE")
#    pp = pprint.PrettyPrinter(indent=4)
#    pp.pprint(comps)

def printState(stateDict):
    if stateDict is None:
        return

    #print component info
    fmt ="  {0: <10}{1: <42}{2: <13} {3: <5}"
    print fmt.format("Namespace", "Standard Name", "Units", "Connected")
    print "  " + ("="*75)    
    for r in flattenState(stateDict):
        units = r[2]
        if "unknown" in units:
            units = "unknown"
        print fmt.format(r[0], r[1], units, r[3])

def flattenState(stateDict):
    retList = []
    if len(stateDict.get("Fields", [])) > 0:
        for f in stateDict["Fields"]:
            retList.append((stateDict.get("namespace"), f["standardName"], f.get("units"), f["connected"]))
    elif len(stateDict.get("States", [])) > 0:
        for s in stateDict["States"]:
            retList.extend(flattenState(s))
    return retList

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print "Usage: python runanalysis.py <filename>"
        print "   where <filename> is an ESMF Log file"
    else:
        main(sys.argv)
