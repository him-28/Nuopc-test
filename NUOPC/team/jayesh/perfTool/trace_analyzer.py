#!/usr/bin/env python
from perf_tool_std_imports import *
import sys, glob, re
import argparse
from operator import itemgetter
from babeltrace import *

class TraceAnalyzer(object):
  def __init__(self, exe_dir_name):
    if exe_dir_name is None:
      exe_dir_name = "."
    self.exe_dir_name = exe_dir_name
    # dictionary of per trace statistics, keyed on PET number
    #
    # {0: { 'region1' : {'min':X, 'max':Y, 'count':Z, 'sum':W, 'avg':A}, 'region2' : {...} },
    #  1: { .... }
    #
    self._petstats = {}

    # Per trace Overall region stats
    # { 'region1' : {'min':X, 'min_pet':PET_X, 'max':Y, 'max_pet':PET_Y, 'sum':S, 'count':N, 'avg_overall_min_pet':A, 'overall_min_pet':PET_A},
    #   'region2' : {...} }
    self._overall_regstats = {}

    # Per trace Overall NUOPC run overhead PET stats
    # {0:{'call_overhead_sum':S, 'call_count':N}}
    self._overall_nuopc_run_overhead = {}

    # dictionary of per trace call stacks, keyed on PET number
    self._callstacks = {}

    # List of tuples [(npet1, total_avg_overhead_time), (npet2, ...), ...]
    self._total_avg_overhead = []

    # Read all the traces in the directory, exe_dir_name
    self.read_all_trace(exe_dir_name)

  def read_all_trace(self, exe_dir_name):
    regex = r"(.*)_npets_([0-9]+)"
    #print("Searching for traces in "+ exe_dir_name.strip())
    for trace_dir_name in glob.glob(exe_dir_name+"/traceout*"):
      #if re.search(regex, trace_dir_name): 
      #  match = re.search(regex, trace_dir_name)
        #npets = int(match.group(2))
      npets, time_overhead = self.read_trace(trace_dir_name)
      self._total_avg_overhead.append((npets,time_overhead))
        #print(str(npets) + "\t" + str(time_overhead))
    # Sort by npets
    self._total_avg_overhead = sorted(self._total_avg_overhead, key=itemgetter(0))

  def get_tot_avg_overhead(self):
    return self._total_avg_overhead

  def get_tot_avg_overhead_str(self):
    HEADER_STR = "=================================\n"
    HEADER_STR += "npets\tTotal Avg Time(micro secs)\n"
    HEADER_STR += "=================================\n"
    tot_avg_overhead_str = HEADER_STR
    for item in self._total_avg_overhead:
      npets, time_overhead = item
      tot_avg_overhead_str += str(npets) + "\t" + str(time_overhead) + "\n"

    return tot_avg_overhead_str

  def print_statistics(self, opt="none"):
    self.print_trace(opt)

  def read_trace(self, trace_dir_name):
    REGEX_NUOPC_RUN_REGNAME = r"^NUOPC_ModelBase:Run$"
    NUOPC_GRIDCOMP_CALLTHROUGH_REGNAME = "NUOPC_Driver:GridComp:Callthrough"
    GRIDCOMP_CALLTHROUGH_COST_REGNAME_PREFIX = "Callthrough_Cost:"
    REGEX_COMP_ADVANCE_REGNAME = r"^((?!NUOPC).)*ModelAdvance$"
    self._overall_nuopc_run_overhead = {'min':sys.maxsize, 'min_pet':-1, 'max':-1, 'max_pet':-1, 'sum':0, 'count':0}

    #print("Reading trace in "+trace_dir_name)
    npets = 0
    # dict of latest grid comp callthrough tstamps keyed on PET number
    latest_gridcomp_callthrough_tstamp = {}
    gridcomp_callthrough_cost_regname = ""
    self._petstats = {}
    self._overall_regstats = {}
    self._overall_nuopc_run_overhead = {}
    self._callstacks = {}
    traces = TraceCollection()
    trace_handle = traces.add_trace(trace_dir_name, "ctf")
    if trace_handle is None:
      raise IOError("Error adding trace")

    for event in traces.events:
      if event.name == "region_enter":
        ts = event.timestamp
        pet = event["pet"]
        regname = event["name"]

        #print(str(pet) + ": ENTER:" + regname)
        if self._callstacks.get(pet) is None:
          self._callstacks[pet] = []  #new stack

        callstack = self._callstacks[pet]
        callstack.append((ts, regname))
        if(pet >= npets):
          npets = pet + 1

        if regname == NUOPC_GRIDCOMP_CALLTHROUGH_REGNAME:
          if latest_gridcomp_callthrough_tstamp.get(pet) is None:
            latest_gridcomp_callthrough_tstamp[pet] = -1
          #print(str(pet) + ": ENTER:" + regname + ":" + str(latest_gridcomp_callthrough_tstamp[pet]))
          if latest_gridcomp_callthrough_tstamp[pet] != -1:
            raise RuntimeError("Recursive calls to gridcomp callthrough (not handled right now)")
          
          latest_gridcomp_callthrough_tstamp[pet] = ts

        if re.search(REGEX_COMP_ADVANCE_REGNAME,regname): 
          if latest_gridcomp_callthrough_tstamp.get(pet) is None:
            raise RuntimeError("Unhandled gridcomp callthrough (No gridcomp calls)")

          if latest_gridcomp_callthrough_tstamp[pet] == -1:
            raise RuntimeError("Unhandled gridcomp callthrough (No gridcomp calls in stack)")

          total_time = ts - latest_gridcomp_callthrough_tstamp[pet]

          gridcomp_callthrough_cost_regname = GRIDCOMP_CALLTHROUGH_COST_REGNAME_PREFIX + regname
          stats = self._petstats.get(pet)
          if stats is None:
            stats = {}

          statsReg = stats.get(gridcomp_callthrough_cost_regname)
          if statsReg is None:
            statsReg = {'min':sys.maxsize, 'max':-1, 'count':0, 'sum':0}

          statsReg['count'] = statsReg['count'] + 1
          statsReg['sum'] = statsReg['sum'] + total_time

          stats[gridcomp_callthrough_cost_regname] = statsReg
          self._petstats[pet] = stats

          if total_time < statsReg['min']:
            statsReg['min'] = total_time
          if total_time > statsReg['max']:
            statsReg['max'] = total_time

          overall_regstat = self._overall_regstats.get(gridcomp_callthrough_cost_regname)
          if overall_regstat is None:
            overall_regstat = {'min':sys.maxsize, 'min_pet':-1, 'max':-1, 'max_pet':-1, 'sum':0, 'count':0}
          if total_time < overall_regstat['min']:
            overall_regstat['min'] = total_time
            overall_regstat['min_pet'] = pet

          if total_time > overall_regstat['max']:
            overall_regstat['max'] = total_time
            overall_regstat['max_pet'] = pet

          overall_regstat['sum'] = overall_regstat['sum'] + total_time
          overall_regstat['count'] = overall_regstat['count'] + 1
          self._overall_regstats[gridcomp_callthrough_cost_regname] = overall_regstat
                    
      elif event.name == "region_exit":
        ts = event.timestamp
        pet = event["pet"]
        regname = event["name"]

        #print(str(pet) + ":EXIT:" + regname + ":" + str(latest_gridcomp_callthrough_tstamp))
        if regname == NUOPC_GRIDCOMP_CALLTHROUGH_REGNAME:
          if latest_gridcomp_callthrough_tstamp.get(pet) is None:
            raise RuntimeError("Inconsistent callthrough stack in trace.")
          latest_gridcomp_callthrough_tstamp[pet] = -1

        callstack = self._callstacks[pet]
        if callstack is None:
          raise RuntimeError("Inconsistent call stack in trace.")
                    
        popped = callstack.pop()
        if popped[1] != regname:
          raise RuntimeError("Inconsistent call stack in trace.")

        total_time = ts - popped[0]

        stats = self._petstats.get(pet)
        if stats is None:
          stats = {}

        statsReg = stats.get(regname)
        if statsReg is None:
          statsReg = {'min':sys.maxsize, 'max':-1, 'count':0, 'sum':0}

        statsReg['count'] = statsReg['count'] + 1
        statsReg['sum'] = statsReg['sum'] + total_time

        stats[regname] = statsReg
        self._petstats[pet] = stats

        if total_time < statsReg['min']:
          statsReg['min'] = total_time
        if total_time > statsReg['max']:
          statsReg['max'] = total_time
                    
        overall_regstat = self._overall_regstats.get(regname)
        if overall_regstat is None:
          overall_regstat = {'min':sys.maxsize, 'min_pet':-1, 'max':-1, 'max_pet':-1, 'sum':0, 'count':0}
        self._overall_regstats[regname] = overall_regstat
        if total_time < overall_regstat['min']:
          overall_regstat['min'] = total_time
          overall_regstat['min_pet'] = pet

        if total_time > overall_regstat['max']:
          overall_regstat['max'] = total_time
          overall_regstat['max_pet'] = pet

        overall_regstat['sum'] = overall_regstat['sum'] + total_time
        overall_regstat['count'] = overall_regstat['count'] + 1

        pet_overall_nuopc_run_overhead = self._overall_nuopc_run_overhead.get(pet)
        if pet_overall_nuopc_run_overhead is None:
          pet_overall_nuopc_run_overhead = {'call_overhead_sum':0, 'call_count':0}

        #NUOPC avg run overhead = Max (Sum (NUOPC_ModelBase:Run) - Sum(Component Model Advance routines) ) / Number of calls to NUOPC_ModelBase:Run
        if re.search(REGEX_NUOPC_RUN_REGNAME,regname): 
          #print(regname, " is a NUOPC run region: ", total_time, " : ", pet, "\n")
          pet_overall_nuopc_run_overhead['call_overhead_sum'] = pet_overall_nuopc_run_overhead['call_overhead_sum'] + total_time
          pet_overall_nuopc_run_overhead['call_count'] = pet_overall_nuopc_run_overhead['call_count'] + 1

        if re.search(REGEX_COMP_ADVANCE_REGNAME,regname): 
          #print(regname, " is a Comp advance region: ", total_time, " : ", pet, "\n")
          pet_overall_nuopc_run_overhead['call_overhead_sum'] = pet_overall_nuopc_run_overhead['call_overhead_sum'] - total_time

        self._overall_nuopc_run_overhead[pet] = pet_overall_nuopc_run_overhead


    # compute averages
    for p in self._petstats.keys(): # list of PETs
      stats = self._petstats[p]
      for r in stats.keys(): # list of regions
        statsReg = stats[r]
        statsReg['avg'] = statsReg['sum'] / statsReg['count']

    # compute average for the slowest PET
    for r in self._overall_regstats:
      overall_regstat = self._overall_regstats[r]
      sum_overall_min_pet = 0
      count_overall_min_pet = 1
      overall_min_pet = 0
      for p in self._petstats.keys(): # list of PETs
        stats = self._petstats[p]
        if stats.get(r) is not None:
          statsReg = stats[r]
          if statsReg['sum'] > sum_overall_min_pet:
            sum_overall_min_pet = statsReg['sum']
            count_overall_min_pet = statsReg['count']
            overall_min_pet = p

      overall_regstat['avg_overall_min_pet'] = sum_overall_min_pet / count_overall_min_pet
      overall_regstat['overall_min_pet'] = overall_min_pet

    # compute max NUOPC run overhead
    max_overhead = 0
    max_pet_overhead_sum = 0
    max_pet_overhead_count = 0
    for p in self._overall_nuopc_run_overhead.keys():
      pet_overall_nuopc_run_overhead = self._overall_nuopc_run_overhead[p]
      #print("Overhead ", pet_overall_nuopc_run_overhead['call_overhead_sum'], " : ", pet_overall_nuopc_run_overhead['call_count'], " : ", p)
      if pet_overall_nuopc_run_overhead['call_overhead_sum'] > max_pet_overhead_sum:
        max_pet_overhead_sum = pet_overall_nuopc_run_overhead['call_overhead_sum']
        max_pet_overhead_count = pet_overall_nuopc_run_overhead['call_count']

    if max_pet_overhead_count > 0:
      max_overhead = (max_pet_overhead_sum / max_pet_overhead_count)/1000

    return (npets, max_overhead)

  def print_trace(self, opt):
    # print table
    if opt == "pet" or opt == "all":
      for p in self._petstats.keys():
        print("\n")
        print(("="*28 + " STATISTICS FOR PET {} (times in microseconds) " + "="*26).format(p))        
        col_headers = ["Region", "Count", "Total (incl)", "Min (incl)", "Max (incl)", "Avg (incl)"]
        print("{:<40} {:<8} {:<12} {:<12} {:<12} {:<12}".format(*col_headers))
        print("="*100)

        # sort based on highest total time
        stats = self._petstats[p]
        sortedRegs = sorted(stats.items(), key=lambda i: i[1]['sum'], reverse=True)

        for regItem in sortedRegs:
          sts = regItem[1]
          row = "{:<40} {:<8} {:<12.3f} {:<12.3f} {:<12.3f} {:<12.3f}".format(
                 regItem[0], sts["count"], sts["sum"]/1000, sts["min"]/1000, sts["max"]/1000, sts["avg"]/1000)
          print(row)

      print("\n")

    if opt == "overall" or opt == "all" :
      print(("="*28 + " OVERALL STATISTICS FOR REGIONS (times in microseconds) " + "="*26))        
      col_headers = ["Region", "Min", "\"Min PET\"", "Max", "\"Max PET\"", "Avg (Overall min PET)", "\"Overall min PET\""]
      print("{:<40} {:<12} {:<8} {:<12} {:<8} {:<12} {:<8}".format(*col_headers))
      print("="*100)
      sorted_overall_regstats = sorted(self._overall_regstats.items())
      for regItem in sorted_overall_regstats:
        sts = regItem[1]
        row = "{:<40} {:<12.3f} {:<8} {:<12.3f} {:<8} {:<12.3f} {:<8}".format(
               regItem[0], sts["min"]/1000, sts["min_pet"], sts["max"]/1000, sts["max_pet"], sts["avg_overall_min_pet"]/1000, sts["overall_min_pet"]) 
        print(row)


#######################################
# Parse command line
#######################################
def parse_cmd_line():
  logger.debug("Parsing command line")
  parser = argparse.ArgumentParser(description='Reading command line args')
  parser.add_argument('--trace-dir',required=True,help="(required) Directory, named traceout*,  containing the trace files")
  parser.add_argument('--print-statistics',type=str,default="none",required=False,help="Print statistics to stdout (none, all, pet, overall)")
  parser.add_argument('--out',required=False, default=False,help="(required) Output processor for the data (stdout, gnuplot, sqldb) ")
  parser.add_argument('--verbose',required=False,action='store_true',help="Enable verbose logging ")
  args = parser.parse_args()
  return args

################################
# Main func
################################
def _main_func():
  args = parse_cmd_line()
  
  if(args.verbose):
    logging.basicConfig(level=logging.DEBUG)

  tr_analyzer = TraceAnalyzer(args.trace_dir)
  if(args.out == "stdout"):
    print(tr_analyzer.get_tot_avg_overhead_str())

  if(args.print_statistics != "none"):
    tr_analyzer.print_statistics(args.print_statistics)

################################

logger = logging.getLogger(LOGFNAME)
if __name__ == "__main__":
  _main_func()


