# This Python file uses the following encoding: utf-8

#Irene Canales Fernández

import sys,os

# varible Django
PWD = os.environ["PWD"]
pwd_django = PWD +'/Django-1.0.4'
sys.path.append(pwd_django)
os.environ["DJANGO_SETTINGS_MODULE"]="wrf4g.settings"
#variable MySQL

try:
    f=open("/etc/redhat-release","r")
except Exception,e:
    print e
    os.exit(-1)
else:
    pos ="".join(f.readlines()).find("release 5")
    f.close()
    if pos == -1:    
         pwd_mysql = PWD + "/MySQL-python"
         sys.path.append(pwd_mysql)
         os.environ["LD_LIBRARY_PATH"] = os.environ["LD_LIBRARY_PATH"] + ':' + PWD + '/lib64' 
    else:
         pwd_mysql = PWD + "/MySQL-python5"
         sys.path.append(pwd_mysql)
         os.environ["LD_LIBRARY_PATH"] = os.environ["LD_LIBRARY_PATH"] + ':' + PWD + '/lib64_5' 


import thread,threading
from datetime import datetime, date, time
from wrf4g.wrf4gapp.models import Experiment_db;



class Experiment(threading.Thread):

   
    def __init__(self,name):
        threading.Thread.__init__(self)
       
	self.name=name
        
        self.Experiment_db=Experiment_db()    
    
       
    def run(self):
        print "run"
        self.Experiment_db.name=self.name
        g=datetime.now()
        self.Experiment_db.start_date=g
        p=datetime.now()
        self.Experiment_db.end_date=p        
        self.Experiment_db.save()


        #Ce.objects.get(name="ce85")
        
        
        
        #self.objects.get()

  



list_names_exp=["exp1","exp2","exp3"]

   
for name in list_names_exp: # se crea un thread por cada ce
    
    t=Experiment(name)
    t.start()      

