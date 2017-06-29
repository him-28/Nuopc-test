from perf_tool_std_imports import *
import subprocess

#########################################
# Class plotter : plots a data list
# of [(npets,time), ...]
#########################################
class Plotter:
  def __init__(self, config_fname=None):
    DEFAULT_GNU_PLOTTER_CONFIG_FNAME = "gnuplot_config.txt"
    self._config_fname = DEFAULT_GNU_PLOTTER_CONFIG_FNAME
    if config_fname is not None:
      self._config_fname = config_fname

  # Plot the data in data_list
  def plot(self, data_list):
    GNU_PLOTTER_EXE_NAME = "gnuplot"
    PLOT_DATA_FNAME = "tracedata.txt"
    PLOT_FNAME = "overhead.png"

    # Create the plot data file
    data_file = open(PLOT_DATA_FNAME, "w")
    total_avg_overhead_str = ""
    for npets,time_overhead in data_list:
      total_avg_overhead_str += str(npets) + "\t" + str(time_overhead) + "\n"
    data_file.write(total_avg_overhead_str)
    data_file.close()

    # Get gnuplot to plot the file using 
    cmd = [GNU_PLOTTER_EXE_NAME, self._config_fname]
    subprocess.check_call(cmd)

    logger.info("Plot showing NUOPC overhead :" + PLOT_FNAME + " is created")

################################

logger = logging.getLogger(LOGFNAME)
