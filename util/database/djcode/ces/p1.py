import time,datetime,os,threading,random,string,thread
from ce import *

#Clase Cethread
#Clase extendida de threading directamente e indirectamente de la Clase ce     
      
class Cethread(threading.Thread):

    lock=threading.Lock() # cerrojo para controlar la escritura 
                          # correcta de cada ce en el fichero
    semaforo = threading.Semaphore(4) #limitar a 4 el numero de accesos 

    def __init__ (self, 
          ce,
          nombretabla,
          verbose=False,
          *otros):

        threading.Thread.__init__(self)
        self.ce=ce
        self.nombretabla=nombretabla
        self.verbose=verbose
    
