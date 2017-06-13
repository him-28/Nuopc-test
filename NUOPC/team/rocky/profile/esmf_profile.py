#
# Requires Python 3
#
# Usage: python esmf_profile.py /path/to/traceout
#

import sys
from babeltrace import *

if len(sys.argv) < 2:
        raise TypeError("Usage: python esmf_profile.py /path/to/traceout ")

traces = TraceCollection()
trace_handle = traces.add_trace(sys.argv[1], "ctf")
if trace_handle is None:
	raise IOError("Error adding trace")

# dictionary of statistics, keyed on PET number
#
# {0: { 'region1' : {'min':X, 'max':Y, 'count':Z, 'sum':W, 'avg':A}, 'region2' : {...} },
#  1: { .... }
#
petstats = {}


# dictionary of call stacks, keyed on PET number
callstacks = {}

for event in traces.events:
        if event.name == "region_enter":

                ts = event.timestamp
                pet = event["pet"]
                regname = event["name"]

                if callstacks.get(pet) is None:
                        callstacks[pet] = []  #new stack

                callstack = callstacks[pet]
                callstack.append((ts, regname))
                
        elif event.name == "region_exit":

                ts = event.timestamp
                pet = event["pet"]
                regname = event["name"]

                callstack = callstacks[pet]
                if callstack is None:
                        raise RuntimeError("Inconsistent call stack in trace.")
                
                popped = callstack.pop()
                if popped[1] != regname:
                        raise RuntimeError("Inconsistent call stack in trace.")

                total_time = ts - popped[0]

                stats = petstats.get(pet)
                if stats is None:
                        stats = {}
                        petstats[pet] = stats

                statsReg = stats.get(regname)
                if statsReg is None:
                        statsReg = {'min':sys.maxsize, 'max':-1, 'count':0, 'sum':0}
                        stats[regname] = statsReg

                statsReg['count'] = statsReg['count'] + 1
                statsReg['sum'] = statsReg['sum'] + total_time

                if total_time < statsReg['min']:
                        statsReg['min'] = total_time
                if total_time > statsReg['max']:
                        statsReg['max'] = total_time


# compute averages
for p in petstats.keys(): # list of PETs
        stats = petstats[p]
        for r in stats.keys(): # list of regions
                statsReg = stats[r]
                statsReg['avg'] = statsReg['sum'] / statsReg['count']

                
# print table
for p in petstats.keys():
        print("\n")
        print(("="*28 + " STATISTICS FOR PET {} (times in microseconds) " + "="*26).format(p))        
        col_headers = ["Region", "Count", "Total (incl)", "Min (incl)", "Max (incl)", "Avg (incl)"]
        print("{:<40} {:<8} {:<12} {:<12} {:<12} {:<12}".format(*col_headers))
        print("="*100)

        # sort based on highest total time
        stats = petstats[p]
        sortedRegs = sorted(stats.items(), key=lambda i: i[1]['sum'], reverse=True)

        for regItem in sortedRegs:
                sts = regItem[1]
                row = "{:<40} {:<8} {:<12.3f} {:<12.3f} {:<12.3f} {:<12.3f}".format(
                        regItem[0], sts["count"], sts["sum"]/1000, sts["min"]/1000, sts["max"]/1000, sts["avg"]/1000)
                print(row)



