import re
import xml.dom.minidom
import os
import subprocess
from drm4g.utils.logger import *

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 1232 2011-10-26 09:09:40Z carlos $"


def sec_to_H_M_S(sec):
    m, s = divmod(int(sec), 60)
    h, m = divmod(m, 60) 
    return "%d:%02d:%02d" % (h, m, s) 

class ResourceException(Exception):
    pass 
    
class Resource (object):

    host_properties = {
        'HOSTNAME'     : 'NULL',
        'ARCH'         : 'NULL',
        'OS_NAME'      : 'NULL',
        'OS_VERSION'   : 'NULL',
        'CPU_MODEL'    : 'NULL', 
        'CPU_MHZ'      :  0, 
        'CPU_FREE'     :  0,
        'CPU_SMP'      :  0, 
        'NODECOUNT'    :  0,
        'FREENODECOUNT':  0,
        'FREE_MEM_MB'  :  0, 
        'SIZE_MEM_MB'  :  0,
        'FREE_DISK_MB' :  0, 
        'SIZE_DISK_MB' :  0,
        'LRMS_NAME'    : 'NULL', 
        'LRMS_TYPE'    : 'NULL',
        } 
    #Operation hostProperties
    re_host = re.compile(r'(?P<HOSTNAME>.+) (?P<OS_VERSION>.+) (?P<ARCH>.+) (?P<OS_NAME>.+)')
    re_cpu  = re.compile(r'^model name\s*:\s*(?P<CPU_MODEL>.+)\s*cpu MHz\s*:\s*(?P<CPU_MHZ>\d+).*')
    re_mem  = re.compile(r'MemTotal:\s*(?P<SIZE_MEM_MB>\d+).*\s*MemFree:\s*(?P<FREE_MEM_MB>\d+)')

    logger = get_logger('drm4g.managers')
   
    def __init__(self):
        self._communicator = None
        self._totalCpu     = 0
        self._freeCpu      = 0

    def setCommunicator(self, communicator):
        self._communicator = communicator

    def getCommunicator(self):
        return self._communicator	
     
    def setFreeCpu(self, freeCpu):
        self._freeCpu = freeCpu

    def getFreeCpu(self):
        return self._freeCpu

    def getTotalCpu(self):
        return self._totalCpu

    def setTotalCpu(self, totalCpu):
        self._totalCpu = totalCpu

    Communicator = property(getCommunicator, setCommunicator)    
    free_cpu     = property(getFreeCpu, setFreeCpu)
    total_cpu    = property(getTotalCpu, setTotalCpu)    

    def dynamicNodes(self):
        pass

    def staticNodes(self, host_id):
        command_proc = subprocess.Popen('LANG=POSIX gwhost -x %s' % (host_id),
                shell = True,
                stdout = subprocess.PIPE,
                stderr = subprocess.PIPE,
                env = os.environ)
        stdout, stderr = command_proc.communicate()
        if stderr: 
            raise ResourceException(' '.join(stderr.split('\n')))
        out_parser = xml.dom.minidom.parseString(stdout)
        busy = int(out_parser.getElementsByTagName('USED_SLOTS')[0].firstChild.data)
        free_cpu = self.total_cpu - busy
        if free_cpu < 0 : self.free_cpu = 0
        else: self.free_cpu = free_cpu
        
    def queues(self, Host = None):
        pass
        
    def hostProperties(self):
        properties = self.host_properties.copy()
        properties['NODECOUNT'] = self.total_cpu
        try:
            out, err = self.Communicator.execCommand('LANG=POSIX uname -n -r -p -o')
            if not err:
                re_host = self.re_host.search(out)
                if re_host: properties.update(re_host.groupdict())
        except Exception, e:
            self.logger.log(DEBUG, "Could not obtain OS: " + str(e))
            self.logger.log(DEBUG, "stdout: " + out)
            self.logger.log(DEBUG, "stderr: " + err)
        try:
            out, err = self.Communicator.execCommand('LANG=POSIX grep -e \"model name\" -e \"cpu MHz\" /proc/cpuinfo')
            if not err: 
                re_cpu = self.re_cpu.match(out)
                if re_cpu: 
                    properties['CPU_MODEL']= re_cpu.groupdict()['CPU_MODEL'] 
                    properties['CPU_MHZ']= int(re_cpu.groupdict()['CPU_MHZ'])
        except Exception, e:
            self.logger.log(DEBUG, "Could not obtain CPU: " + str(e))
            self.logger.log(DEBUG, "stdout: " + out)
            self.logger.log(DEBUG, "stderr: " + err)
        try:
            out, err = self.Communicator.execCommand('LANG=POSIX grep -e MemTotal -e MemFree /proc/meminfo')
            if not err:
                re_mem = self.re_mem.search(out)
                if re_mem: properties.update(dict((k, int(v)/1024) for k, v in re_mem.groupdict().items()))
        except Exception, e:
            self.logger.log(DEBUG, "Could not obtain Mem: " + str(e))
            self.logger.log(DEBUG, "stdout: " + out)
            self.logger.log(DEBUG, "stderr: " + err)
        try:
            out, err = self.Communicator.execCommand('LANG=POSIX df -B 1K $HOME')
            if not err:
                properties['SIZE_DISK_MB'] = int(out.split('\n')[1].split()[1])
                properties['FREE_DISK_MB'] = int(out.split('\n')[1].split()[3])
        except Exception, e:
            self.logger.log(DEBUG, "Could not obtain Mem_Disk: " + str(e))
            self.logger.log(DEBUG, "stdout: " + out)
            self.logger.log(DEBUG, "stderr: " + err)
        return self._host_string(properties)
    
    def _host_string(self, properties):
        for k, v in properties.items():
            if type(v) == str: properties[k] = '"%s"' % (v)
            else: properties[k] = '%d' % (v)
        return ' '.join(['%s=%s' % (k, v) for k, v in properties.items()])
        
    def _queues_string(self, properties):
        args = []
        for i, vals in enumerate(properties):
            for k, v in vals.items():
                if type(v) == str: aux = '"%s"' % (v)
                else: aux = '%d' % (v)
                args.append('%s[%d]=%s' % (k, i, aux))
        return ' '.join(args)

class JobException(Exception):
    pass 

class Job (object):
    
    def __init__(self):
        self._communicator = None
        self._jobId        = "NULL"
        self._status       = "NULL"
        self._lock = __import__('threading').Lock()
    
    def setStatus(self, status):
        self._lock.acquire()
        try:
            self._status = status
        finally:
            self._lock.release()

    def getStatus(self):
        self._lock.acquire()
        try:
            return self._status
        finally:
            self._lock.release()
    Status = property(getStatus, setStatus)

    def setCommunicator(self, communicator):
        self._communicator = communicator

    def getCommunicator(self):
        return self._communicator
    Communicator = property(getCommunicator, setCommunicator)   

    def setJobId(self, jobId):
        self._jobId = jobId

    def getJobId(self):
        return self._jobId
    JobId = property(getJobId, setJobId)
    
    def refreshJobStatus(self):
        self.Status = self.jobStatus()
        
    def createWrapper(self, localWrapperDirectory, stringTemplate):
        try:
            f = open(localWrapperDirectory, 'w')
        except Exception, e:
            raise JobException('Error creating wrapper_drm4g :' + str(e))
        else:
            f.write(stringTemplate)
            f.close()

    def copyWrapper(self, localWrapperDirectory, remoteWrapperDirectory):
        """
        Copy wrapper_drm4g file to remote host
        """
        source_url = 'file://%s' % (localWrapperDirectory)
        destination_url = 'gsiftp://_/~/%s' % (remoteWrapperDirectory)
        try:
            self.Communicator.copy(source_url, destination_url, 'X')
        except Exception, e:
            raise JobException('Error copying wrapper_drm4g :' + str(e))

    # To overload 
   
    def jobSubmit(self, remoteFileNameWrapper):
        pass
        
    def jobStatus(self):
        pass
        
    def jobCancel(self):
        pass
        
    def jobTemplate(self, rsl2):
        pass

    

class Queue (object):
    
    _name           = "NULL"
    _nodes          = "0"
    _freeNodes      = "0"
    _maxTime        = "0"
    _maxCpuTime     = "0"
    _maxCount       = "0"
    _maxRunningJobs = "0"
    _maxJobsWoutCpu = "0"
    _status         = "NULL"
    _dispatchType   = "NULL"
    _priority       = "NULL"
    
    def setName(self, name):
        self._name = name
    
    def getName(self):
        return self._name
    Name = property(getName, setName)
    
    def setNodes(self, nodes):
        self._name = nodes
    
    def getNodes(self):
        return self._nodes
    Nodes = property(getNodes, setNodes)
    
    def setFreeNodes(self, freeNodes):
        self._freeNodes = freeNodes
    
    def getFreeNodes(self):
        return self._freeNodes
    FreeNodes = property(getFreeNodes, setFreeNodes)
    
    def setMaxTime(self, maxTime):
        self._maxTime = maxTime
    
    def getMaxTime(self):
        return self._maxTime
    MaxTime = property(getMaxTime, setMaxTime)
    
    def setMaxCpuTime(self, maxCpuTime):
        self._maxCpuTime = maxCpuTime
    
    def getMaxCpuTime(self):
        return self._maxCpuTime
    MaxCpuTime = property(getMaxCpuTime, setMaxCpuTime)
    
    def setMaxCount(self, maxCount):
        self._maxCount = maxCount
    
    def getMaxCount(self):
        return self._maxCount
    MaxCount = property(getMaxCount, setMaxCount)
    
    def setMaxRunningJobs(self, maxRunningJobs):
        self._maxRunningJobs = maxRunningJobs
    
    def getMaxRunningJobs(self):
        return self._maxRunningJobs
    MaxRunningJobs = property(getMaxRunningJobs, setMaxRunningJobs)

    def setMaxJobsWoutCpu(self, maxJobsWoutCpu):
        self._maxJobsWoutCpu = maxJobsWoutCpu
    
    def getMaxJobsWoutCpu(self):
        return self._maxJobsWoutCpu
    MaxJobsWoutCpu = property(getMaxJobsWoutCpu, setMaxJobsWoutCpu)
    
    def setStatus(self, status):
        self._status = status
    
    def getStatus(self):
        return self._status
    Status = property(getStatus, setStatus)
    
    def setDispatchType(self, dispatchType):
        self._dispatchType = dispatchType
    
    def getDispatchType(self):
        return self._dispatchType
    DispatchType = property(getDispatchType, setDispatchType)
    
    def setPriority(self, priority):
        self._priority = priority
    
    def getPriority(self):
        return self._priority
    Priority = property(getPriority, setPriority)

    def info (self, i):
        """
        @return: the information of the queue
        @rtype: string
        """
        return  'QUEUE_NAME[' + i + ']="' + self.Name + '" QUEUE_NODECOUNT[' + i + ']=' + self.Nodes + \
            ' QUEUE_FREENODECOUNT[' + i + ']=' + self.FreeNodes + ' QUEUE_MAXTIME[' + i +']=' + self.MaxTime + \
            ' QUEUE_MAXCPUTIME[' + i + ']=' + self.MaxCpuTime + ' QUEUE_MAXCOUNT[' + i + ']=' + self.MaxCount + \
            ' QUEUE_MAXRUNNINGJOBS[' + i + ']=' + self.MaxRunningJobs + ' QUEUE_MAXJOBSINQUEUE[' + i + ']=' + \
            self.MaxJobsWoutCpu + ' QUEUE_STATUS[' + i + ']="' + self.Status + '" QUEUE_DISPATCHTYPE[' + i + ']="'+ \
            self.DispatchType + '" QUEUE_PRIORITY[' + i +']="' + self.Priority + '" '
    
class Host (object):
    
    _name       = "NULL"
    _arch       = "NULL"
    _os         = "NULL"
    _osVersion  = "NULL"
    _cpuModel   = "NULL"
    _cpuMhz     = "0"
    _freeCpu    = "0"
    _cpuSmp     = "0"
    _nodeCount  = "0"
    _sizeMemMB  = "0"
    _freeMemMB  = "0"
    _sizeDiskMB = "0"
    _freeDiskMB = "0"
    _forkName   = "NULL"
    _lrmsName   = "NULL"
    _lrmsType   = "NULL"
    _queues     = []
    
    def addQueue(self, queue):
        self._queues.append(queue)
        
    def showQueues(self):
        return self._queues
    
    def setName(self, name):
        self._name = name
        
    def getName(self):
        return self._name
    Name = property(getName, setName)

    def setArch(self, arch):
        self._arch = arch
        
    def getArch(self):
        return self._arch
    Arch = property(getArch, setArch)

    def setOs(self, os):
        self._os = os
    
    def getOs(self):
        return self._os
    Os = property(getOs, setOs)
    
    def setOsVersion(self, osVersion):
        self._osVersion = osVersion
        
    def getOsVersion(self):
        return self._osVersion    
    OsVersion = property(getOsVersion, setOsVersion)

    def setCpuModel(self, cpuModel):
        self._cpuModel = cpuModel
    
    def getCpuModel(self):
        return self._cpuModel
    CpuModel = property(getCpuModel, setCpuModel)
    
    def setCpuMhz(self, cpuMhz):
        self._cpuMhz = cpuMhz

    def getCpuMhz(self):
        return self._cpuMhz
    CpuMhz = property(getCpuMhz, setCpuMhz)

    def setFreeCpu(self, freeCpu):
        self._freeCpu = freeCpu
        
    def getFreeCpu(self):
        return self._freeCpu
    FreeCpu = property(getFreeCpu, setFreeCpu)

    def setCpuSmp(self, cpuSmp):
        self._cpuSmp = cpuSmp
        
    def getCpuSmp(self):
        return self._cpuSmp 
    CpuSmp = property(getCpuSmp, setCpuSmp)
    
    def setNodeCount(self, nodeCount):
        self._nodeCount = nodeCount
        
    def getNodeCount(self):
        return self._nodeCount
    NodeCount = property(getNodeCount, setNodeCount)
    
    def setSizeMemMB(self, sizeMemMB):
        self._sizeMemMB = sizeMemMB
    
    def getSizeMemMB(self):
        return self._sizeMemMB 
    SizeMemMB = property(getSizeMemMB, setSizeMemMB)

    def setFreeMemMB(self, freeMemMB):
        self._freeMemMB = freeMemMB
        
    def getFreeMemMB(self):
        return self._freeMemMB  
    FreeMemMB = property(getFreeMemMB, setFreeMemMB)    
    
    def setSizeDiskMB(self, sizeDiskMB):
        self._sizeDiskMB = sizeDiskMB
    
    def getSizeDiskMB(self):
        return self._sizeDiskMB
    SizeDiskMB = property(getSizeDiskMB, setSizeDiskMB)

    def setFreeDiskMB(self, freeDiskMB):
        self._freeDiskMB = freeDiskMB
        
    def getFreeDiskMB(self):
        return self._freeDiskMB    
    FreeDiskMB = property(getFreeDiskMB, setFreeDiskMB)
    
    def setForkName(self, forkName):
        self._forkName = forkName
        
    def getForkName(self):
        return self._forkName    
    ForkName = property(getForkName, setForkName)
    
    def setLrmsName(self, lrmsName):
        self._lrmsName = lrmsName
    
    def getLrmsName(self):
        return self._lrmsName
    LrmsName = property(getLrmsName, setLrmsName)
    
    def setLrmsType(self, lrmsType):
        self._lrmsType = lrmsType
        
    def getLrmsType(self):
        return self._lrmsType
    LrmsType = property(getLrmsType, setLrmsType)

    def info (self): 
        """
        @return: the information of the host and the host queues
        @rtype: string 
        """
        info_host = 'HOSTNAME="' + self.Name + '" ARCH="' + self.Arch + '" OS_NAME="' + \
            self.Os + ' OS_VERSION="' + self.OsVersion + '" CPU_MODEL="' + self.CpuModel + \
            ' CPU_MHZ=' + self.CpuMhz + ' CPU_FREE=' + self.FreeCpu + ' CPU_SMP=' + self.CpuSmp + \
            ' NODECOUNT=' + self.NodeCount + ' SIZE_MEM_MB=' + self.SizeMemMB + ' FREE_MEM_MB=' + \
            self.FreeMemMB + ' SIZE_DISK_MB=' + self.SizeDiskMB + ' FREE_DISK_MB=' + self.FreeDiskMB + \
            ' FORK_NAME="' + self.ForkName + ' LRMS_NAME="' + self.LrmsName + '" LRMS_TYPE="' + self.LrmsType + '" '
        for i, queue in enumerate(self._queues):
            info_host += queue.info(i)
        return info_host		    
       
