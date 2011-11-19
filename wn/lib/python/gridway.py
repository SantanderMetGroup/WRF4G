import commands
import os
import sys


class job:
   """ gwjob CLASS
   """
   def __init__(self,id=None):
       self.gwloc=os.environ.get('GW_LOCATION')
       if os.environ.get('GW_LOCATION') == None:
           sys.stderr.write('GW_LOCATION is not defined. Please define it and try again\n')
           sys.exit(1)

  
   def create_template(self,name,arguments,np=1,req='',environ='',inputsandbox='',verbose=False):
       """Create template
       """
       if verbose:
          debug='-x'
       else:
          debug=''
       
       ftemplate=name + ".gw"
       f = open (ftemplate,'w')
       template="""NAME = %s
EXECUTABLE   = /bin/bash 
ARGUMENTS    = "%s ./WRF4G.sh %s"
INPUT_FILES  = %s
RANK         = QUEUE_FREENODECOUNT 
REQUIREMENTS = %s
ENVIRONMENT  = %s
NP           = %d
"""%(name,debug,arguments,inputsandbox,req,environ,np)
       f.write(template)
       f.close()
       self.template=ftemplate

   
   def submit(self,dep=None,priority=0):
     
       if dep != None:
           depend="-d %s"%dep
       else:
           depend=''
        
       command="%s/bin/gwsubmit -p %d -v %s -t %s"%(self.gwloc,priority,depend,self.template)
       (err,out)=commands.getstatusoutput(command)
       os.unlink(self.template)
       if err != 0: 
         sys.stderr.write(out + '\n')
         sys.exit(1)
       else:
          return out[8:]
        
        
