from perf_tool_std_imports import *
################################################
# Class Machine : contains all machine-specific
# details
################################################
class Machine(object):
  def __init__(self, nnodes):
    # Number of nodes to use on this machine
    self.nnodes = nnodes

####################################
logger = logging.getLogger(LOGFNAME)
