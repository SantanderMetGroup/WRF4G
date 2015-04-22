import commands
import os
import sys

from os.path import dirname, abspath, join 
from drm4g   import DRM4G_BIN

__version__  = '1.5.2'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"
    
class Job( object ):
    """
    Class to use DRM4G 
    """
    def create_template(self,name,arguments,np=1,req='',environ='',inputsandbox=''):
        """
        Create template
        """
        ftemplate=name + ".gw"
        try:
            f = open (ftemplate,'w')
            template="""NAME = %s
EXECUTABLE   = /usr/bin/env python 
ARGUMENTS    = ./bin/pilot_wrf.py %s"
INPUT_FILES  = %s
REQUIREMENTS = %s
ENVIRONMENT  = %s
NP           = %d""" % (name,arguments,inputsandbox,req,environ,np)
            f.write(template)
        finally:
            f.close()
        self.template=ftemplate
    
    def submit(self,dep=None,priority=0,type_dep="afterany"):
        if dep != None:
            depend="-d %s -r %s" % (dep, type_dep)
        else:
            depend=''
        command="%s/gwsubmit -p %d -v %s -t %s"%(DRM4G_BIN,priority,depend,self.template)
        (err,out)=commands.getstatusoutput(command)
        os.unlink(self.template)
        if err != 0:
            raise Exception(out)
        else:
            return out[8:]
        
    def kill(self,job_array):
        for i in job_array:
            command="%s/gwkill -9 %s"%(DRM4G_BIN,i)
            (err,out)=commands.getstatusoutput(command)
            if out:
                raise Exception(out)
                
    def change_priority(self,priority,job_array):
        for i in job_array:
            command="%s/gwkill -9 -p %d %s"%(DRM4G_BIN,priority,i)
            (err,out)=commands.getstatusoutput(command)
            if out:
                raise Exception(out)
                                                
