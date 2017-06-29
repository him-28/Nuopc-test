from perf_tool_std_imports import *
import machine
import model
import xml_parser
import subprocess

######################################
# Batch Class : contains all details
# of the batch subsystem used to launch
# jobs
# The template for the job script is
# read from a file named 
# <batch_sys_name>_batch_template,xml
######################################
class Batch(object):
  def __init__(self, batch_sys_name):
    XML_TAG_BATCH_SYS_HDR = "batch_system_header"
    XML_TAG_MPIEXEC_CMD = "mpiexec_command"
    XML_TAG_BATCH_SUBMIT_CMD = "batch_submit_command"
    fname = "./" + batch_sys_name + "_batch_template.xml"
    if not(os.path.isfile(fname)) or not(os.access(fname, os.R_OK)):
      logger.error("Unable to access file : " + fname)

    self.batch_sys_name = batch_sys_name  
    self._batch_sys_config_fname = fname
    # Read the batch template file
    xml_data = xml_parser.XMLData(self._batch_sys_config_fname)
    self.batch_system_header_template = xml_data.get_val(XML_TAG_BATCH_SYS_HDR)
    self.mpiexec_cmd_template = xml_data.get_val(XML_TAG_MPIEXEC_CMD).strip()
    if self.mpiexec_cmd_template is None:
      logger.error("Unable to get mpiexec command from config file :" +fname)

    self.batch_submit_command = xml_data.get_val(XML_TAG_BATCH_SUBMIT_CMD)

  # Run the model on a range of pets (pet_start, pet_end, pet_stride)
  def range_run(self, model, machine, pet_start, pet_end, pet_stride):
    NNODES_TEMPL_STR = "NUOPC_PERF_TOOL_NNODES"
    NPETS_TEMPL_STR = "NUOPC_PERF_TOOL_NPETS"
    APP_EXE_TEMPL_STR = "NUOPC_PERF_TOOL_APP_EXE"
    BATCH_FILE_FNAME = "_perf_tool_batch_script"
    BASH_HDR = "#!/bin/bash\n"
    ESMF_TRACE_HDR = "export ESMF_RUNTIME_TRACE=ON\nexport ESMF_RUNTIME_TRACE_COMPONENT=OFF"

    if pet_end <= pet_start or pet_stride <= 0:
      logger.error("Invalid pet start/end/stride provided")

    # Create the batch script contents
    batch_script_contents = ""

    if not(os.path.isfile(model.exe_name)) or not(os.access(model.exe_name, os.R_OK)):
      logger.error("Unable to find exe : " + model.exe_name)

    batch_script_contents += BASH_HDR
    if not self.batch_system_header_template:
      hdr_line = self.batch_system_header_template
      hdr_line = hdr_line.replace(NNODES_TEMPL_STR, str(machine.nnodes))
      batch_script_contents += hdr_line
    
    batch_script_contents += ESMF_TRACE_HDR + "\n"
    mpiexec_cmd_line = self.mpiexec_cmd_template
    mpiexec_cmd_line = mpiexec_cmd_line.replace(APP_EXE_TEMPL_STR, model.exe_name)

    for pet in range(pet_start, pet_end, pet_stride):
      batch_script_contents += mpiexec_cmd_line.replace(NPETS_TEMPL_STR, str(pet)) + "\n"
      batch_script_contents += "mv ./traceout ./traceout_npets_" + str(pet) + "\n"

    # Create the batch file
    batch_file = open(BATCH_FILE_FNAME, "w")
    batch_file.write(batch_script_contents)
    batch_file.close()

    # Submit the batch job
    if self.batch_submit_command is not None:
      cmd = self.batch_submit_command + " " + BATCH_FILE_FNAME
    else:
      st = os.stat(BATCH_FILE_FNAME)
      os.chmod(BATCH_FILE_FNAME, st.st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)
      cmd = "./" + BATCH_FILE_FNAME

    logger.debug("Running command, cmd="  + cmd)
    subprocess.check_call([cmd])

  # Run the model on npets
  def run(self, model, machine, npets):
    logger.debug("Running model" + model.exe_name + " with batch " + self.batch_sys_name)
    self.range_run(model, machine, npets, npets+1, 1)

####################################
logger = logging.getLogger(LOGFNAME)

