import drm4g.managers
import re
import xml.dom.minidom
from string import Template
from drm4g.managers import sec_to_H_M_S

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: sge.py 1362 2012-01-17 12:14:28Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
QCONF = 'LANG=POSIX qconf'
QHOST = 'LANG=POSIX qhost'
QSUB  = 'LANG=POSIX qsub'  
QSTAT = 'LANG=POSIX qstat' 
QDEL  = 'LANG=POSIX qdel'  

class Resource (drm4g.managers.Resource):

    def lrmsProperties(self):
        return ('SGE', 'SGE')

    def dynamicNodes(self):
        out, err = self.Communicator.execCommand('%s -xml' % (QHOST))
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        out_parser = xml.dom.minidom.parseString(out)
        slots = [elem.getElementsByTagName('hostvalue')[1].firstChild.data \
            for elem in out_parser.getElementsByTagName('qhost')[0].getElementsByTagName('host')]
        total_cpu = sum([int(elem) for elem in slots if elem != '-']) 
        out, err = self.Communicator.execCommand('%s -s r -xml' % (QSTAT))
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        out_parser = xml.dom.minidom.parseString(out) 
        busy = sum ([int(elem.firstChild.data) for elem in out_parser.getElementsByTagName('slots')])
        return (str(total_cpu), str(total_cpu - busy))

    def queuesProperties(self, searchQueue, project):
        if not searchQueue and project:
            queue              = drm4g.managers.Queue()
            queue.Name         = 'default'
            queue.Nodes        = self.TotalCpu
            queue.FreeNodes    = self.FreeCpu
            queue.DispatchType = 'batch'
            return [queue]
        else:
            out, err = self.Communicator.execCommand('%s -sql' % (QCONF))
            if err:
                raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
            queues = []
            for queueName in out.strip('\n').split():
                if queueName == searchQueue or not searchQueue:
                    queue              = drm4g.managers.Queue()
                    queue.Name         = queueName
                    queue.DispatchType = 'batch'
                    queue.Nodes        = self.TotalCpu
                    queue.FreeNodes    = self.FreeCpu
                    out, err = self.Communicator.execCommand('%s -sq %s' % (QCONF, queueName))
                    if err:
                        raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
                    re_Walltime = re.compile(r'h_rt\s*(\d+):(\d+):\d+')
                    if re_Walltime.search(out):
                        try:
                            hours, minutes   = re_Walltime.search(out).groups()
                            queue.MaxTime    = str(int(hours) * 60 + int(minutes))
                        except: pass
                    re_Cputime = re.compile(r'h_cpu\s*(\d+):(\d+):\d+')
                    if re_Cputime.search(out):
                        try: 
                            hours, minutes   = re_Cputime.search(out).groups()
                            queue.MaxCpuTime = str(int(hours) * 60 + int(minutes))
                        except: pass
                    queues.append(queue)
            return queues

class Job (drm4g.managers.Job):

    #sge job status <--> GridWay job status
    states_sge = {
        'd'  : 'FAILED',   #An error occurred with the job
        'Eqw': 'FAILED',   #An error occurred with the job
        'Ew' : 'FAILED',   #An error occurred with the job
        'h'  : 'PENDING',  #Job is hold
        'r'  : 'ACTIVE',   #Job is running
        'R'  : 'FAILED',   #Job is restarted
        'Rr' : 'FAILED',   #Job is restarted
        's'  : 'SUSPENDED',#Job is suspended
        'S'  : 'SUSPENDED',#Job is suspended
        't'  : 'PENDING',  #Job is transfering
        'T'  : 'PENDING',  #Job is Threshold 
        'w'  : 'PENDING',  #Job is waiting	
        'qw' : 'PENDING',  #Job is waiting
        }

    def jobSubmit(self, path_script):
        out, err = self.Communicator.execCommand('%s %s' % (QSUB, path_script))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))
        re_job  = re.compile(r'^Your job (\d*) .*').search(out)
        return re_job.group(1)

    def jobStatus(self):
        out, err = self.Communicator.execCommand('%s | grep %s' % (QSTAT, self.JobId))
        if not out:
            return 'DONE'
        else:
            state = out.split()[4]
            return self.states_sge.setdefault(state, 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (QDEL, self.JobId))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#$ -N JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        if parameters.has_key('PROJECT'): 
            args += '#$ -P $PROJECT\n'
        if parameters['queue'] != 'default':
            args += '#$ -q $queue\n'
        args += '#$ -o $stdout\n'
        args += '#$ -e $stderr\n'
        if parameters.has_key('maxWallTime'): 
            args += '#$ -l h_rt=%s\n' % (sec_to_H_M_S(parameters['maxWallTime']))
        if parameters.has_key('maxCpuTime'): 
            args += '#$ -l cput=%s\n' % (sec_to_H_M_S(parameters['maxCpuTime']))
        if parameters.has_key('maxMemory'): 
            args += '#$ -l mem_free=%sM\n' % (parameters['maxMemory'])
        if (parameters['jobType'] == "mpi") or (int(parameters['count']) > 1):
            args += '#$ -pe $mpi $count\n'
        args += '#$ -v %s\n' % (','.join(['%s=%s' %(k, v) for k, v in parameters['environment'].items()]))
        args += 'cd $directory\n'
        if parameters['jobType'] == "mpi":
            args += 'mpiexec -np $count $executable\n'
        else:
            args += '$executable\n'
        return Template(args).safe_substitute(parameters)

   
            
        
