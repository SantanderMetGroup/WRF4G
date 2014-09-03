import xml.dom.minidom

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: rsl2.py 2250 2014-08-27 09:04:57Z carlos $"

class Rsl2Parser(object):

  """
   Parser of RSL2 files. It is used like this:
   
           rsl    = RSL2Parser(filename)
           stdout = rsl.parse()
  """
  elems_to_parse = ['executable','stdout','stderr','directory','count','jobType',
                      'queue','maxTime','maxWallTime','maxCpuTime','minMemory','maxMemory','ppn', 'nodes']

  def __init__(self, filename):
    """
    @param filename : file name or file 
    @type filename : string
    """
    self._values = { }
    self._str = xml.dom.minidom.parse(filename)

  def parseValue(self, name):
    """
    Gets data from <job><name></name></job> and adds it to self._values
    @param name : name of value
    @type name : string
    """
    val = self._str.getElementsByTagName(name)
    if val:
        self._values[name] = val[0].firstChild.data

  def parseEnvironment(self):
    """
    Gets all the data enclosed in <job><environment>...</environment></job> and adds
    it to self._values
    """
    environments = self._str.getElementsByTagName('environment')
    if environments:
        self._values['environment'] = dict((elem.getElementsByTagName('name')[0].firstChild.data,
                                       elem.getElementsByTagName('value')[0].firstChild.data) \
                                       for elem in environments)

  def parser(self):
    """
    Does the parsing. It is necessary to call this method after creating the object
    @return  : dictionary with RSL values
    """
    [self.parseValue(name) for name in self.elems_to_parse]
    self.parseEnvironment()
    return self._values

