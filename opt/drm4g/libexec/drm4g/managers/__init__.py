import sys
import xml.dom.minidom
import os
import subprocess
import logging

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: __init__.py 2811 2015-09-22 11:33:32Z carlos $"

logger = logging.getLogger(__name__)

def totalCores( cores ):
    return sum( [ int( core ) for core in cores.split( ',' ) ] )

def sec_to_H_M_S( sec ):
    """
    Convert seconds into HH:MM:SS 
    """
    m, s = divmod( int( sec ), 60)
    h, m = divmod( m, 60 ) 
    return "%d:%02d:%02d" % ( h, m, s )

class ResourceException(Exception): 
    pass 

class JobException(Exception):      
    pass

class Resource (object):
    """
    Class to obtain information about compute resources
    """
   
    def __init__(self):
	self.name           = None
	self.features       = dict()
	self.Communicator   = None
	self.host_list      = []
    
    def ldapsearch(self, filt = '', attr = '*', bdii = 'lcg-bdii.cern.ch:2170', base = 'Mds-Vo-name=local,o=grid'):
	""" 
	Wrapper for ldapserch.
	Input parameters:
	filt:    Filter used to search ldap, default = '', means select all
	attr:    Attributes returned by ldapsearch, default = '*', means return all
	host:    Host used for ldapsearch, default = 'lcg-bdii.cern.ch:2170', can be changed by $LCG_GFAL_INFOSYS
	
	Return each element of list is dictionary with keys:
	'dn':                 Distinguished name of ldapsearch response
	'objectClass':        List of classes in response
	'attr':               Dictionary of attributes
	"""
	cmd = 'ldapsearch -x -LLL -l 5 -h %s -b %s "%s" "%s"' % ( bdii, base, filt, attr )
	out, err = self.Communicator.execCommand( cmd )
	assert not err, err
	
	response = []
	lines = []
	for line in out.split( "\n" ):
	    if line.find( " " ) == 0:
		lines[-1] += line.strip()
	    else:
		lines.append( line.strip() )
	record = None
	for line in lines:
	    if line.find( 'dn:' ) == 0:
		record = {'dn':line.replace( 'dn:', '' ).strip(),
			  'objectClass':[],
			  'attr':{'dn':line.replace( 'dn:', '' ).strip()}}
		response.append( record )
		continue
	    if record:
		if line.find( 'objectClass:' ) == 0:
		    record['objectClass'].append( line.replace( 'objectClass:', '' ).strip() )
		    continue
		if line.find( 'Glue' ) == 0:
		    index = line.find( ':' )
		    if index > 0:
			attr = line[:index]
			value = line[index + 1:].strip()
			if attr in record['attr']:
			    if type( record['attr'][attr] ) == type( [] ):
				record['attr'][attr].append( value )
			    else:
				record['attr'][attr] = [record['attr'][attr], value]
			else:
			    record['attr'][attr] = value
	return response
    
    def hosts(self):
	"""
	It will return a string with the host available in the resource.
	"""
        if 'vo' in self.features and 'cream' in self.features :	
	    self.host_list = self._hosts_vo( )
	    return ' '.join( self.host_list )
	else :
	    self.host_list = [ self.name ]
	    return self.name
		
    def host_properties(self, host ):
	"""
	Obtain the features of each host
	"""
        if 'vo' in self.features and 'cream' in self.features :	
	    return self._host_vo_properties( host )
	else :
	    return self._host_properties( host ) 

    def _hosts_vo(self):
	"""
	It will return a list with the sites available in the VO.
	"""
	filt      =   '(&(objectclass=GlueCE)(GlueCEAccessControlBaseRule=VO:%s)(GlueCEImplementationName=CREAM)' % ( 
												       self.features[ 'vo' ] , 
												       )
	attr      = 'GlueCEHostingCluster'
	bdii      = self.features.get( 'bdii', '$LCG_GFAL_INFOSYS' )
        result    = []
        if 'host_filter' in self.features :	
	    for host in self.features[ 'host_filter' ].split( ',' ) :
                ce_filter = '(GlueCEHostingCluster=%s))' % host.strip()
                result.append( self.ldapsearch( filt + ce_filter , attr , bdii ) )               
        else :
            result.append( self.ldapsearch( filt + ')' , attr , bdii ) )
        hosts = []
        for value in result :
            for each_host in value :
                host = "%s::%s" % ( self.name , each_host['attr'][attr] )
                hosts.append(host)
        return hosts
        
    def _host_vo_properties(self , host ): 
        """
        It will return a string with the features of hosts on Grid environment
        """
        _ , host       = host.split( '::' )
        host_info      = HostInformation()
        host_info.Name = host
        
        # First search
        filt   =  '(&(objectclass=GlueCE)(GlueCEAccessControlBaseRule=VO:%s)(GlueCEImplementationName=CREAM)(GlueCEInfoHostName=%s))' % ( self.features[ 'vo' ] , host )
        attr   = '*'
        bdii   = self.features.get( 'bdii', '$LCG_GFAL_INFOSYS' )  
        result = self.ldapsearch( filt , attr , bdii )
    
        for value in result :
            queue                = Queue()
            queue.Name           = value['attr']["GlueCEName"]
            queue.Nodes          = value['attr']["GlueCEInfoTotalCPUs"]
            queue.FreeNodes      = value['attr']["GlueCEStateFreeCPUs"]
            queue.MaxTime        = value['attr']["GlueCEPolicyMaxWallClockTime"]
            queue.MaxCpuTime     = value['attr']["GlueCEPolicyMaxCPUTime"]
            queue.MaxJobsInQueue = value['attr']["GlueCEPolicyMaxTotalJobs"]
            queue.MaxRunningJobs = value['attr']["GlueCEPolicyMaxRunningJobs"]
            queue.Status         = value['attr']["GlueCEStateStatus"]
            queue.Priority       = value['attr']["GlueCEPolicyPriority"]
            host_info.addQueue(queue)
            host_info.Nodes      = value['attr']["GlueCEInfoTotalCPUs"]
            host_info.LrmsName   = value['attr']["GlueCEUniqueID"].split('/')[1].rsplit('-',1)[0]
            host_info.LrmsType   = value['attr']["GlueCEInfoLRMSType"]
        host_info.addQueue( Queue() )
        # Second search    
        filt   = "(&(objectclass=GlueHostOperatingSystem)(GlueSubClusterName=%s))"  % ( host )
        attr   = '*'
        bdii   = self.features.get( 'bdii', '$LCG_GFAL_INFOSYS' )  
        result = self.ldapsearch( filt , attr , bdii )
        
        try :        
            host_info.Os         = result[0]['attr'][ "GlueHostOperatingSystemName" ]
            host_info.OsVersion  = result[0]['attr'][ "GlueHostOperatingSystemVersion" ]
            host_info.Arch       = result[0]['attr'][ "GlueHostArchitecturePlatformType" ]
            host_info.CpuSmp     = result[0]['attr'][ "GlueHostArchitectureSMPSize" ]
        except Exception as err:
            logger.error("The result of '%s' is wrong: %s " % ( filt , str( result ) ) ) 
  
        return host_info.info()
    
    def _host_properties(self , host ):
        """
        It will return a string with the features of hosts on HPC environment
        """
        host_info       = HostInformation()
        host_info.Name  = host
        host_info.Name, host_info.OsVersion, host_info.Arch, host_info.Os = self.system_information()
        
        q_features = [ ( q_elem.strip(), jobr_elem.strip(), jobq_elem.strip() ) 
                      for q_elem, jobr_elem, jobq_elem in zip( 
                                                  self.features[ 'queue' ].split( ',' ) , 
                                                  self.features[ 'max_jobs_running' ].split( ',' ) ,
                                                  self.features[ 'max_jobs_in_queue' ].split( ',' )
                                                  ) ]
        for queue_name, max_jobs_running, max_jobs_in_queue in q_features  :
            queue                = Queue()
            queue.Name           = queue_name
            queue.MaxRunningJobs = max_jobs_running 
            queue.MaxJobsInQueue = max_jobs_in_queue
            host_info.addQueue( self.additional_queue_properties( queue ) )
        host_info.addQueue( Queue() )
        host_info.LrmsName = self.features[ 'lrms' ]
        host_info.LrmsType = self.features[ 'lrms' ]
        return host_info.info()
       
    def system_information(self):
        """
        It will return a tuple with hostname, OS version, architecture, OS name
        """
        out, err = self.Communicator.execCommand('uname -n -r -m -o')
        if not err:
            return out.split()  
        else:
            logger.error("Error executing `uname` command: %s" % ' '.join( err.split( '\n' ) ) )
            return ('NULL', 'NULL', 'NULL', 'NULL')
          
    # To overload
    def additional_queue_properties(self, Queue):
        return Queue

class Job (object):
    """
    Class to manage jobs
    """
    
    def __init__(self):
        self.resfeatures  = dict()
        self.Communicator = None
        self.JobId        = None
        self._status      = None
        self._lock        = __import__('threading').Lock()
    
    def setStatus(self, status):
        with self._lock :
            self._status = status

    def getStatus(self):
        with self._lock :
            return self._status
    
    def refreshJobStatus(self):
        with self._lock :
            self._status = self.jobStatus()
    
    def get_abs_directory(self, directory ):
        out, err = self.Communicator.execCommand( 'ls -d %s' % directory )
        if not err:
            return out.strip('\n')
        else:
            output = "Could not obtain  the '%s' directory : %s" % ( directory , str ( err ) )
            logger.error( output )
            raise JobException( output )
        
    def createWrapper(self, local_directory, template):
        try:
            f = open(local_directory, 'w')
        except Exception as e:
            raise JobException('Error creating wrapper_drm4g :' + str(e))
        else:
            f.write(template)
            f.close()

    def copyWrapper(self, local_directory, remote_directory):
        """
        Copy wrapper_drm4g file to the remote host
        """
        source_url      = 'file://%s'       % local_directory
        destination_url = 'gsiftp://_/%s' % remote_directory
        try:
            self.Communicator.copy(source_url, destination_url, 'X')
        except Exception as e:
            raise JobException("Error copying wrapper_drm4g : %s" % str(e) )

    # To overload 
    def jobSubmit( self , wrapper_dir ) :
        pass
        
    def jobStatus( self ) :
        pass
        
    def jobCancel( self ) :
        pass
        
    def jobTemplate( self , rsl ) :
        pass

class Queue( object ) :
    
    def __init__(self):
        self.Name           = ""
        self.Nodes          = "0"
        self.FreeNodes      = "0"
        self.MaxTime        = "0"
        self.MaxCpuTime     = "0"
        self.MaxCount       = "0"
        self.MaxRunningJobs = "0"
        self.MaxJobsInQueue = "0"
        self.Status         = "NULL"
        self.DispatchType   = "NULL"
        self.Priority       = "NULL"

    def info (self, i):
        """
        @return: the information of the queue
        @rtype: string
        """
        i = str(i)
        return  'QUEUE_NAME[' + i + ']="' + self.Name + '" QUEUE_NODECOUNT[' + i + ']=' + self.Nodes + \
            ' QUEUE_FREENODECOUNT[' + i + ']=' + self.FreeNodes + ' QUEUE_MAXTIME[' + i +']=' + self.MaxTime + \
            ' QUEUE_MAXCPUTIME[' + i + ']=' + self.MaxCpuTime + ' QUEUE_MAXCOUNT[' + i + ']=' + self.MaxCount + \
            ' QUEUE_MAXRUNNINGJOBS[' + i + ']=' + self.MaxRunningJobs + ' QUEUE_MAXJOBSINQUEUE[' + i + ']=' + \
            self.MaxJobsInQueue  + ' QUEUE_STATUS[' + i + ']="' + self.Status + '" QUEUE_DISPATCHTYPE[' + i + ']="'+ \
            self.DispatchType + '" QUEUE_PRIORITY[' + i +']="' + self.Priority + '" '
    
class HostInformation( object ) :
   
    def __init__(self):
        self.Name       = ""
        self.Arch       = "NULL"
        self.Os         = "NULL"
        self.OsVersion  = "NULL"
        self.CpuModel   = "NULL"
        self.CpuMhz     = "0"
        self.FreeCpu    = "0"
        self.CpuSmp     = "0"
        self.Nodes      = "0"
        self.SizeMemMB  = "0"
        self.FreeMemMB  = "0"
        self.SizeDiskMB = "0"
        self.FreeDiskMB = "0"
        self.ForkName   = "NULL"
        self.LrmsName   = "NULL"
        self.LrmsType   = "NULL"
        self._queues    = [] 
 
    def addQueue(self, queue):
        self._queues.append(queue)
        
    def showQueues(self):
        return self._queues

    def info(self): 
        """
        @return: the information of the host and the host queues
        @rtype: string 
        """
        info_host = 'HOSTNAME="' + self.Name + '" ARCH="' + self.Arch + '" OS_NAME="' + \
            self.Os + '" OS_VERSION="' + self.OsVersion + '" CPU_MODEL="' + self.CpuModel + \
            '" CPU_MHZ=' + self.CpuMhz + ' CPU_FREE=' + self.FreeCpu + ' CPU_SMP=' + self.CpuSmp + \
            ' NODECOUNT=' + self.Nodes + ' SIZE_MEM_MB=' + self.SizeMemMB + ' FREE_MEM_MB=' + \
            self.FreeMemMB + ' SIZE_DISK_MB=' + self.SizeDiskMB + ' FREE_DISK_MB=' + self.FreeDiskMB + \
            ' FORK_NAME="' + self.ForkName + '" LRMS_NAME="' + self.LrmsName + '" LRMS_TYPE="' + self.LrmsType + '" FIXED_PRIORITY="0" '
        info_queue = ""
        for i, queue in enumerate(self._queues):
            info_queue += queue.info(i)
        return info_host + info_queue		    
       
