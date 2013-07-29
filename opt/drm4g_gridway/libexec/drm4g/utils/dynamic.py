import Queue
from threading import Thread
from threading import Lock
import sys

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: dynamic.py 1023 2011-07-07 15:46:10Z carlos $"


"""
Use example
from dynamic import ThreadPool
pool = ThreadPool(2, 10)
pool.add_task(func, arg)
"""

threads_active = 0
threads_min    = 1
lock = Lock()

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
                try: func, args, kargs = self.tasks.get(timeout = 0.5) 
                except Queue.Empty:
                    lock.acquire()
                    try:
                        global threads_active, threads_min
                        if threads_active > threads_min:
			    threads_active -= 1
			    break
                        else : continue
                    finally: lock.release()
                except Exception: sys.exit(-1)
                else:
                    try: func(*args, **kargs)
                    except Exception, e: print e
            except Exception: pass

class ThreadPool:
    """
    Pool of threads consuming tasks from a queue
    """
    def __init__(self, num_threads_min, num_threads_max):
        self.threads_max = num_threads_max
        self.tasks = Queue.Queue()
        lock.acquire()
        global threads_active, threads_min
        try: 
            threads_active =  num_threads_min
            threads_min = num_threads_min
        finally: lock.release()
        for _ in range(num_threads_min): 
            Worker(self.tasks)

    def add_task(self, func, *args, **kargs):
        """
        Add a task to the queue
        """
        lock.acquire()
        global threads_active
        try: 
            try:
                if threads_active < self.threads_max: 
                    Worker(self.tasks)
                    threads_active += 1
                self.tasks.put((func, args, kargs))
            except Exception, e: print e 
        finally: lock.release()
