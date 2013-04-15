import commands
import os
import sys
from os.path import dirname, abspath 

WRF4G_LOCATION = dirname(dirname(dirname(abspath(__file__))))
GW_LOCATION = WRF4G_LOCATION + '/opt/drm4g_gridway'
    
class job:
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
            ARGUMENTS    = "%s ./WRF4G.sh %s"
            INPUT_FILES  = %s
            RANK         = QUEUE_FREENODECOUNT
            REQUIREMENTS = %s
            ENVIRONMENT  = %s
            NP           = %d"""%(name,debug,arguments,inputsandbox,req,environ,np)
            f.write(template)
        finally:
            f.close()
        self.template=ftemplate
    
    def submit(self,dep=None,priority=0,type_dep="afterany"):
        if dep != None:
            depend="-d %s -r %s" % (dep, type_dep)
        else:
            depend=''
        command="%s/bin/gwsubmit -p %d -v %s -t %s"%(GW_LOCATION,priority,depend,self.template)
        (err,out)=commands.getstatusoutput(command)
        os.unlink(self.template)
        if err != 0:
            raise Exception(out)
        else:
            return out[8:]
        
    def kill(self,job_array):
        for i in job_array:
            command="%s/bin/gwkill -9 %s"%(GW_LOCATION,i)
            (err,out)=commands.getstatusoutput(command)
            if out:
                raise Exception(out)
                
    def change_priority(self,priority,job_array):
        for i in job_array:
            command="%s/bin/gwkill -9 -p %d %s"%(GW_LOCATION,priority,i)
            (err,out)=commands.getstatusoutput(command)
            if out:
                raise Exception(out)
                                                
