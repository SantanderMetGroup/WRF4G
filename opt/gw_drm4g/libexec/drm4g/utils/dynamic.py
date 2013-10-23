import Queue
from threading import Thread
from threading import Lock
import sys

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: dynamic.py 1915 2013-09-05 11:51:19Z carlos $"


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
                        else : continue
                except Exception: sys.exit(-1)
                else:
                    try: 
                        func(*args, **kargs)
                    except Exception, e: 
                        print e
            except Exception: pass

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
            except Exception, e: print e 
