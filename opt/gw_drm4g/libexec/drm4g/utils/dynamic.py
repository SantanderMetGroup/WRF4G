from __future__       import with_statement
import Queue
import sys
import traceback
from threading        import Thread
from threading        import Lock

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: dynamic.py 2250 2014-08-27 09:04:57Z carlos $"


"""
Use example
from dynamic import ThreadPool
pool = ThreadPool(2, 10)
pool.add_task(func, arg)
"""

threads_active = 0
threads_min    = 1
timeout        = 1
lock           = Lock()

class Worker(Thread):
    """
    Thread executing tasks from a given tasks queue
    """
    def __init__(self, tasks):
        Thread.__init__(self)
        self.tasks = tasks
        self.setDaemon(True)
        self.start()
    
    def run(self):
        while True:
            try:
                try: 
                    func, args, kargs = self.tasks.get( timeout  ) 
                except Queue.Empty:
                    with lock : 
                        global threads_active, threads_min
                        if threads_active > threads_min:
                            threads_active -= 1
                            break
                        else : 
                            continue
                except Exception: 
                    sys.exit(-1)
                else:
                    try: 
                        func(*args, **kargs)
                    except : 
                        traceback.print_exc(file=sys.stdout)
            except Exception: 
                pass

class ThreadPool:
    """
    Pool of threads consuming tasks from a queue
    """
    def __init__(self, num_threads_min, num_threads_max):
        self.threads_max = num_threads_max
        self.tasks = Queue.Queue()
        with lock :
            global threads_active, threads_min
            threads_active =  num_threads_min
            threads_min    = num_threads_min
        for _ in range(num_threads_min): 
            Worker(self.tasks)

    def add_task(self, func, *args, **kargs):
        """
        Add a task to the queue
        """
        with lock :
            global threads_active
            try:
                if threads_active < self.threads_max: 
                    Worker(self.tasks)
                    threads_active += 1
                self.tasks.put((func, args, kargs))
            except :
                traceback.print_exc(file=sys.stdout) 
            
