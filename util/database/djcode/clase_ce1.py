# This Python file uses the following encoding: utf-8

#Irene Canales Fernández

import thread,threading
from ces.cesapp.models import Ce

class Ce_database(Ce,threading.Thread):

   
    def __init__(self,name,vo):
        threading.Thread.__init__(self)
	self.name=name
        self.vo=vo
    
         
       
    def run(self):
        print "run"
        self.save()


list_names_ces=["ce11","ce21","ce31"]
vo="esr"
   
for name in list_names_ces: # se crea un thread por cada ce
    
    t=Ce_database(name,vo)
    t.start()      

