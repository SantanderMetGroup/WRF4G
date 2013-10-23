import Queue
import heapq

class PrioQueue(Queue.Queue):
    """Variant of Queue that retrieves open entries in priority order 
    (lowest first).Entries are typically tuples of the form:  
    (priority number, data).
    """
    def _init(self, maxsize):
        self.maxsize = maxsize
        self.queue = []
    
    def _qsize(self, len=len):
        return len(self.queue)
    
    def _put(self, item, heappush = heapq.heappush):
        heappush(self.queue, item)
    
    def _get(self, heappop = heapq.heappop):
        return heappop(self.queue)
