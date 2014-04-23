import commands
import os
import sys

from os.path import dirname, abspath, join 
from wrf4g   import GW_BIN_LOCATION

__version__  = '1.5.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"
    
class job(object):
    """
    gwjob CLASS
    """
    def create_template(self,name,arguments,np=1,req='',environ='',inputsandbox='',verbose=False):
        """
        Create template
        """
        if verbose:
            debug='-x'
        else:
            debug=''
        ftemplate=name + ".gw"
        try:
            f = open (ftemplate,'w')
            template="""NAME = %s
EXECUTABLE   = /bin/bash 
ARGUMENTS    = "%s ./bin/WRF4G.sh %s"
INPUT_FILES  = %s
RANK         = QUEUE_FREENODECOUNT
REQUIREMENTS = %s
ENVIRONMENT  = %s
NP           = %d""" % (name,debug,arguments,inputsandbox,req,environ,np)
            f.write(template)
        finally:
            f.close()
        self.template=ftemplate
    
    def submit(self,dep=None,priority=0,type_dep="afterany"):
        if dep != None:
            depend="-d %s -r %s" % (dep, type_dep)
        else:
            depend=''
        command="%s/gwsubmit -p %d -v %s -t %s"%(GW_BIN_LOCATION,priority,depend,self.template)
        (err,out)=commands.getstatusoutput(command)
        os.unlink(self.template)
        if err != 0:
            raise Exception(out)
        else:
            return out[8:]
        
    def kill(self,job_array):
        for i in job_array:
            command="%s/gwkill -9 %s"%(GW_BIN_LOCATION,i)
            (err,out)=commands.getstatusoutput(command)
            if out:
                raise Exception(out)
                
    def change_priority(self,priority,job_array):
        for i in job_array:
            command="%s/gwkill -9 -p %d %s"%(GW_BIN_LOCATION,priority,i)
            (err,out)=commands.getstatusoutput(command)
            if out:
                raise Exception(out)
                                                
