import sys
import os
import threading
from drm4g.utils.logger import *
from drm4g.core.configure import hostparse
from drm4g.utils.dynamic import ThreadPool
from drm4g.utils.message import Send
from drm4g.global_settings import COMMUNICATOR, RESOURCE_MANAGER
from drm4g.utils.importlib import import_module
import traceback

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: im_mad.py 1199 2011-10-17 17:07:38Z carlos $"

class GwImMad (object):
    """
    Information manager MAD 
	
	The format to send a request to the Information MAD, through its standard input, is: 
	
		OPERATION HID HOST ARGS
		
	Where:
	-OPERATION: Can be one of the following:
		-INIT: Initializes the MAD (i.e. INIT - - -).
		-DISCOVER: Discovers hosts (i.e. DISCOVER - - - ).
		-MONITOR: Monitors a host (i.e. MONITOR HID HOST -).
		-FINALIZE: Finalizes the MAD (i.e. FINALIZE - - -).
	-HID: if the operation is MONITOR, it is a host identifier, chosen by GridWay. Otherwise it is ignored.
	-HOST: If the operation is MONITOR it specifies the host to monitor. Otherwise it is ignored.
			
	The format to receive a response from the MAD, through its standard output, is:
	    
	    OPERATION HID RESULT INFO
	    
	Where:
	-OPERATION: Is the operation specified in the request that originated the response.
	-HID: It is the host identifier, as provided in the submission request.
	-RESULT: It is the result of the operation. Could be SUCCESS or FAILURE.
	-INFO: If RESULT is FAILURE, it contains the cause of failure. Otherwise, if OPERATION 
		is   DISCOVER, it contains a list of discovered host, or if OPERATION is MONITOR, 
		it contains a list of host attributes.
    """

    logger = get_logger('drm4g.core.im_mad')
    message = Send()
    
    def __init__(self):
        self._min_thread = 2
        self._max_thread = 10 
        self._host_list_configuration  = { }
        self._list_resource = { }
 
    def do_INIT(self, args):
        """
        Initializes the MAD (i.e. INIT - - -)
        @param args : arguments of operation
        @type args : string
        """
        try:
            self._host_list_configuration = hostparse()
            for key, val in self._host_list_configuration.items():
                com = getattr(import_module(COMMUNICATOR[val.SCHEME]), 'Communicator')()
                com.hostName = val.HOST
                com.userName = val.USERNAME
                com.workDirectory = val.GW_RUNDIR
                com.connect()
                resource = getattr(import_module(RESOURCE_MANAGER[val.LRMS_TYPE]), 'Resource')()
                resource.Communicator = com 
                self._list_resource[key] = resource
            out = 'INIT - SUCCESS -'
        except Exception, e:
            out = 'INIT - FAILURE %s' % (str(e))
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)
        
    def do_DISCOVER(self, args):
        """
        Discovers hosts (i.e. DISCOVER - - -)
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, HID, HOST, ARGS = args.split()
        out = 'DISCOVER %s SUCCESS %s' % (HID, ' '.join([host for host in self._host_list_configuration.keys()]))
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)
 
    def do_MONITOR(self, args):
        """
        Monitors a host (i.e. MONITOR HID HOST -)
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, HID, HOST, ARGS = args.split()
        try:
            if self._host_list_configuration.has_key(HOST):
                resource = self._list_resource[HOST]
                if self._host_list_configuration[HOST].NODECOUNT:
                    resource.total_cpu = int(self._host_list_configuration[HOST].NODECOUNT)
                    resource.staticNodes(HID)
                else:
                    resource.dynamicNodes()
                host_properties   = resource.hostProperties()
                queues_properties = resource.queues(self._host_list_configuration[HOST])
                out = 'MONITOR %s SUCCESS %s %s' % (HID, host_properties, queues_properties)
            else: 
                out = 'MONITOR %s FAILURE %s is not configured' % (HID, HOST)
        except Exception, e:
            out = 'MONITOR %s FAILURE %s' % (HID, str(e))
            self.logger.log(DEBUG, ' '.join(traceback.format_exc().splitlines()))
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)           
 
    def do_FINALIZE(self, args):
        """
        Finalizes the MAD (i.e. FINALIZE - - -)
        @param args : arguments of operation
        @type args : string
        """
        out =  'FINALIZE - SUCCESS -'
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)
        sys.exit(0)
        
    methods = { 'INIT'	  : do_INIT,
                'DISCOVER': do_DISCOVER,
                'MONITOR' : do_MONITOR,
                'FINALIZE': do_FINALIZE,
                }
                
    def processLine(self):
        """
        Choose the OPERATION through the command line
        """
        try:
            pool = ThreadPool(self._min_thread, self._max_thread)
            while True:
                input = sys.stdin.readline().split()
                self.logger.log(DEBUG, '<-- '  + ' '.join(input))
                OPERATION = input[0].upper()
                if len(input) == 4 and self.methods.has_key(OPERATION):
                    if OPERATION != 'MONITOR':
                        self.methods[OPERATION](self, ' '.join(input))
                    else:
                        pool.add_task(self.methods[OPERATION], self, ' '.join(input))
                else:
                    self.message.stdout('WRONG COMMAND')
                    self.logger.log(DEBUG, '--> WRONG COMMAND')
        except Exception, e:
            self.logger.log(DEBUG, '--> ' + str(e))
            
