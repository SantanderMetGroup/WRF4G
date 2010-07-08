# This Python file uses the following encoding: utf-8

#Irene Canales Fernández

import thread,threading
from ces.cesapp.models import Ce

class Ce_database(threading.Thread):

   
    def __init__(self,name,vo):
        threading.Thread.__init__(self)
       
	self.name=name
        self.vo=vo
        self.Ce_db=Ce()    
    
       
    def run(self):
        print "run"
        self.Ce_db.name=self.name
        #self.Ce_db.vo=self.vo
        #self.Ce_db.save()
        Ce.objects.get(name="ce85")
        
        
        
        #self.objects.get()

  



list_names_ces=["ce85","ce86","ce87"]
vo="esr"
   
for name in list_names_ces: # se crea un thread por cada ce
    
    t=Ce_database(name,vo)
    t.start()      

