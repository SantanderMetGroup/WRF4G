import commands
import os


class job:
   """ gwjob CLASS
   """
   def __init__(self,name,arguments,np=1,req='',dep=None,gwloc='/home/valva/gridway-5.7'):
       self.name=name
       self.arguments=arguments
       self.np=np
       self.req=req
       self.dep=dep
       self.gwloc=gwloc
       
   def create_template(self):
    ftemplate=self.name + ".gw"
    f = open (ftemplate,'w')
    template="""NAME = %s
EXECUTABLE = /bin/bash 
ARGUMENTS = "./WRF4G_ini.sh %s"
INPUT_FILES   = WRF4G_ini.sh
RANK = (CPU_MHZ * 2) + FREE_MEM_MB 
NP=%d"""%(self.name,self.arguments,self.np)
    
    if self.req != '':
        template=template + "\nREQUIREMENTS = %s"%self.req
    
    f.write(template)
    f.close()
    return ftemplate
        
   def submit(self):
        
        template=self.create_template()

        if self.dep != None:
            depend="-d %d"%self.dep
        else:
            depend=''
        
        command="GW_LOCATION=%s %s/bin/gwsubmit -v %s -t %s"%(self.gwloc,self.gwloc,depend,template)
        (err,out)=commands.getstatusoutput(command)
        if err != 0: 
            print err
            exit(9)
        return out[8:]
        
        #os.unlink(template)
        
