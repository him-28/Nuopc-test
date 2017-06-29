from perf_tool_std_imports import *
import subprocess
#########################################
# Class Model : Contains all details of
# the model being run
########################################
class Model(object):
  def __init__( self,
                config_script,
                build_script,
                exe_name):
    if not(os.path.isfile(config_script)) or not(os.access(config_script, os.R_OK)):
      logger.error("Unable to access file:" + config_script)
    self.config_script = config_script
    if not(os.path.isfile(build_script)) or not(os.access(build_script, os.R_OK)):
      logger.error("Unable to access file:" + build_script)
    self.build_script = build_script
    self.exe_name = exe_name

  # Configures the model
  def configure(self):
    logger.debug("Configuring " + self.exe_name)
    subprocess.check_call([self.config_script])

  # Builds the model
  def build(self):
    logger.debug("Building " + self.exe_name)
    subprocess.check_call([self.build_script])

  # Not used right now - running is done via the batch class
  def run(self, batch_system, npets):
    logger.debug("Running "+ self.exe_name)

##########################################
logger = logging.getLogger(LOGFNAME)
