#!/usr/bin/env python
from perf_tool_std_imports import *
import machine
import model
import batch
import argparse
import trace_analyzer
import plotter

#######################################
# Parse command line
#######################################
def parse_cmd_line():
  logger.debug("Parsing command line")
  parser = argparse.ArgumentParser(description='Reading command line args')
  parser.add_argument('--configure-script',required=True,help="(required) Script used to configure the model")
  parser.add_argument('--build-script',required=True,help="(required) Script used to build the model")
  parser.add_argument('--exe',required=True,help="(required) Full path to the executable")
  parser.add_argument('--nodes',required=True,help="(required) Max nodes used to run the tool",type=int)
  parser.add_argument('--pet-start',required=True,help="(required) Start of range of PETs used for running the tool",type=int)
  parser.add_argument('--pet-end',required=True,help="(required) End of range of PETs used for running the tool",type=int)
  parser.add_argument('--pet-stride',required=True,help="(required) Stride of range of PETs used for running the tool",type=int)
  parser.add_argument('--batch-system',required=True,help="(required) Batch system used (default (no batch system), qsub, ...)")
  parser.add_argument('--out',required=True,help="(required) Output processor for the data (stdout, gnuplot, sqldb) ")
  parser.add_argument('--rebuild-reqd',default=False,help="(required) Does the model need to be rebuilt after changing the number of PETs? ")
  parser.add_argument('--batch-mode',default=False,help="Use this option if running inside a batch job")
  parser.add_argument('--verbose',default=False,help="Enable verbose logging ")
  args = parser.parse_args()
  return args

################################
# Main func
################################
def _main_func():
  args = parse_cmd_line()

  if(args.verbose):
    logging.basicConfig(level=logging.DEBUG)

  # Create a model, configure and build it
  mod = model.Model(args.configure_script, args.build_script, args.exe)
  mod.configure()
  mod.build()

  # All machine-specific details are in the Machine obj
  mach = machine.Machine(args.nodes)

  # Create a batch object based on the user-specified batch system
  bat = batch.Batch(args.batch_system)

  # Run the model using the batch system
  bat.range_run(mod, mach, args.pet_start, args.pet_end, args.pet_stride)

  # Get a stream analyzer, analyze runtime traces and display output
  tr_analyzer = trace_analyzer.TraceAnalyzer(os.path.dirname(args.exe))
  if(args.out == "stdout"):
    print(tr_analyzer.get_tot_avg_overhead_str())
  elif(args.out == "gnuplot"):
    plot = plotter.Plotter()
    plot.plot(tr_analyzer.get_tot_avg_overhead())
   

################################

logger = logging.getLogger(LOGFNAME)
if __name__ == "__main__":
  _main_func()


