#!/usr/bin/python

import sys
import getopt
import json
#import pprint

# The comps dictionary maintains the current state.
# This is the basic structure:
#
# comps {
#    "ATM": {"kind" : "MODEL",
#            "IMP" : {"IPDv01p1":"1", ...},
#            "RPM" : {"RunPhase1":"1", ...},
#            "FPM" : {"FinalizePhase1":"1",...},
#            "Fields" : { "sea_surface_temp" : {},
#                         "uwind" : {}
#                       },
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
        comp["Fields"] = {}

        if "InitializePhaseMap$NUOPC$Component" in jComp:
            comp["IPM"] = {}
            for kv in jComp["InitializePhaseMap$NUOPC$Component"]:
                kvl = kv.split("=")
                comp["IPM"][kvl[0]] = kvl[1]

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
    if method == "init":
        phaseMap = comps[compName]["IPM"]
    elif method == "run":
        phaseMap = comps[compName]["RPM"]
    elif method == "final":
        phaseMap = comps[compName]["FPM"]
    else:
        phaseMap = {}

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
        
    if jEvent["name"] == "start_phase":
        print ("    "*level) + jEvent["compName"] + " (" + jEvent["method"] + " " + phaseStr + ")"
        comps[jEvent["compName"]]["RunSequence"].append(phaseStr)
    else:
        pass

def handleState(jState):
    compName = jState.get("Namespace$NUOPC$Instance")
    stateIntent = jState.get("intent$NUOPC$Instance")
    fieldList = jState.get("linkList", [])
    if compName is not None and comps.get(compName):
        compFields = comps[compName]["Fields"]
        for fld in fieldList:
            fieldName = fld["field"].get("StandardName$CF$Extended")
            if fieldName is not None:
                if compFields.get(fieldName) is None:
                    compFields[fieldName] = {}
            if stateIntent is not None:
                compFields[fieldName]["Intent"] = stateIntent
            isConnected = fld["field"].get("Connected$NUOPC$Instance")
            if isConnected is not None:
                compFields[fieldName]["Connected"] = isConnected

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
    print "Log file: " + logfile

    #first pass, get component info
    with open(logfile) as f:
        for line in f:
            if (line[20:24] == "JSON"):
                jLine = json.loads(line[42:])
                if "comp" in jLine:
                    populateComponent(jLine["comp"])
                
    #second pass, read events
    with open(logfile) as f:
        level = 0
        for line in f:
            if (line[20:24] == "JSON"):
                jLine = json.loads(line[42:])
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
                    handleState(jState)
                    
    #print component info
    for c in comps:
        if comps[c].get("kind"):
            print ""
            print ""
            print "Component:  " + c + " (" + str(comps[c].get("kind")) + ")"
            print "***************************************************"
            print ""
            print "Fields:"
            print "-------"
            for f in comps[c].get("Fields", []):
                print f + ": Connected=" + comps[c]["Fields"][f].get("Connected")              

#    pp = pprint.PrettyPrinter(indent=4)
#    pp.pprint(comps)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print "Usage: python runanalysis.py <filename>"
        print "   where <filename> is an ESMF Log file"
    else:
        main(sys.argv)
