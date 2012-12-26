import sys
import os
import threading
import logging
from drm4g.core.configure import readHostList, parserHost
from drm4g.managers import HostInformation
from drm4g.utils.dynamic import ThreadPool
from drm4g.utils.message import Send
from drm4g.global_settings import COMMUNICATOR, RESOURCE_MANAGER
from drm4g.utils.importlib import import_module
import traceback

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: im_mad.py 1357 2012-01-10 19:59:38Z carlos $"

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

    logger = logging_wrf4g.getLogger('drm4g.core.im_mad')
    message = Send()
    
    def __init__(self):
        self._min_thread = 4
        self._max_thread = 10 
        self._host_list_conf = { }
        self._resource_list  = { }
 
    def do_INIT(self, args):
        """
        Initializes the MAD (i.e. INIT - - -)
        @param args : arguments of operation
        @type args : string
        """
        out = 'INIT - SUCCESS -'
        self.message.stdout(out)
        self.logger.debug(out)
        
    def do_DISCOVER(self, args):
        """
        Discovers hosts (i.e. DISCOVER - - -)
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, HID, HOST, ARGS = args.split()
        try:
            hostList = readHostList()
            out = 'DISCOVER %s SUCCESS %s' % (HID, ' '.join([hostname for hostname in hostList.keys()]))
        except Exception, e:
            out = 'DISCOVER - FAILURE %s' % (str(e))
        self.message.stdout(out)
        self.logger.debug(out)
 
    def do_MONITOR(self, args):
        """
        Monitors a host (i.e. MONITOR HID HOST -)
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, HID, HOST, ARGS = args.split()
        try:
            if not self._host_list_conf.has_key(HOST):
                hostList = readHostList()
                for hostname, url in hostList.items():
                    if HOST == hostname:
                        hostConf = parserHost(hostname, url)
                        self._host_list_conf[hostname] = hostConf
                        if not self._resource_list.has_key(hostname):
                            com = getattr(import_module(COMMUNICATOR[hostConf.SCHEME]), 'Communicator')()
                            com.hostName = hostConf.HOST
                            com.userName = hostConf.USERNAME
                            com.connect()
                            resource = getattr(import_module(RESOURCE_MANAGER[hostConf.LRMS_TYPE]), 'Resource')()
                            resource.Communicator = com
                            self._resource_list[hostname] = resource
            hostConf = self._host_list_conf[HOST]
            if self._resource_list.has_key(HOST):
                resource = self._resource_list[HOST]
                if hostConf.NODECOUNT:
                    resource.TotalCpu, resource.FreeCpu  = resource.staticNodes(HID, hostConf.NODECOUNT)
                else:
                    resource.TotalCpu, resource.FreeCpu  = resource.dynamicNodes()
                hostInfo = HostInformation()
                hostInfo.Name, hostInfo.OsVersion, hostInfo.Arch, hostInfo.Os  = resource.hostProperties()
                hostInfo.NodeCount                       = resource.TotalCpu
                hostInfo.CpuModel  , hostInfo.CpuMhz     = resource.cpuProperties()
                hostInfo.SizeMemMB , hostInfo.FreeMemMB  = resource.memProperties()
                hostInfo.SizeDiskMB, hostInfo.FreeDiskMB = resource.diskProperties()
                hostInfo.LrmsName  , hostInfo.LrmsType   = resource.lrmsProperties()
                hostInfo.addQueue(resource.queuesProperties(hostConf.QUEUE_NAME, hostConf.PROJECT)) 
                out = 'MONITOR %s SUCCESS %s' % (HID, hostInfo.info())
            else:
                out = 'MONITOR %s FAILURE %s is not configured correctly' % (HID, HOST)
        except Exception, e:
            out = 'MONITOR %s FAILURE %s' % (HID, str(e))
        self.message.stdout(out)
        self.logger.debug(out)
 
    def do_FINALIZE(self, args):
        """
        Finalizes the MAD (i.e. FINALIZE - - -)
        @param args : arguments of operation
        @type args : string
        """
        out =  'FINALIZE - SUCCESS -'
        self.message.stdout(out)
        self.logger.debug(out)
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
                self.logger.debug(' '.join(input))
                OPERATION = input[0].upper()
                if len(input) == 4 and self.methods.has_key(OPERATION):
                    if OPERATION != 'MONITOR':
                        self.methods[OPERATION](self, ' '.join(input))
                    else:
                        pool.add_task(self.methods[OPERATION], self, ' '.join(input))
                else:
                    self.message.stdout('WRONG COMMAND')
                    self.logger.debug(out)
        except Exception, e:
            self.logger.warning(str(e))
            
