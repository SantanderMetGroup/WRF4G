import commands
import os
import sys

from os.path import dirname, abspath, join 
from wrf4g   import WRF4G_DEPLOYMENT_DIR

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

DRM4G_BIN = join( WRF4G_DEPLOYMENT_DIR, 'opt', 'drm4g', 'bin' )
    
class Job( object ):
    """
    Class to use DRM4G 
    """
    def create_template(self,name,directory,arguments,np=1,req='',environ='',inputsandbox='',outputsandbox=''):
        """
        Create template
        """
        ftemplate = join( directory,  name + ".gw" )
        try: 
            f = open(ftemplate,'w')
            template="""NAME = %s
EXECUTABLE   = /usr/bin/env python 
ARGUMENTS    = ./bin/pilot_wrf.py %s
INPUT_FILES  = %s
OUTPUT_FILES = %s
REQUIREMENTS = %s
ENVIRONMENT  = %s
NP           = %d""" % (name,arguments,inputsandbox,outputsandbox,req,environ,np)
            f.write(template)
        finally:
            f.close()
        self.template=ftemplate
    
    def submit(self,dep=None,priority=0,type_dep="afterok"):
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
        
    def kill(self,job):
        command="%s/gwkill -9 %s"%(DRM4G_BIN,job)
        (err,out)=commands.getstatusoutput(command)
        if out:
            raise Exception(out)
                
    def change_priority(self,priority,job):
        command="%s/gwkill -9 -p %d %s"%(DRM4G_BIN,priority,job)
        (err,out)=commands.getstatusoutput(command)
        if out:
            raise Exception(out)
                                                
