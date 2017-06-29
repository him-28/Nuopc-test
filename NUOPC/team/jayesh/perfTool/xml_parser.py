import xml.etree.ElementTree as ET
########################################
# Class XMLData : Helper class to parse
# an XML file
########################################
class XMLData(object):
  def __init__(self, xml_fname):
    self.xml_fname = xml_fname
    self.xml_tree = ET.parse(xml_fname)

  # Get value corresponding to xml_tag
  def get_val(self, xml_tag):
    xml_tree_root = self.xml_tree.getroot()
    for item in xml_tree_root.iter(xml_tag):
      return item.text
