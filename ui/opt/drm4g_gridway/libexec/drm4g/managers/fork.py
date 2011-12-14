import drm4g.managers 
from string import Template
import os

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: fork.py 1301 2011-11-16 09:18:35Z carlos $"

SH = 'LANG=POSIX /bin/bash'

class Resource (drm4g.managers.Resource):    

    host_properties = {
        'LRMS_NAME'    : 'FORK',
        'LRMS_TYPE'    : 'FORK',
        }
 
    queue_default = {
        'QUEUE_NAME'           : 'default',
        'QUEUE_NODECOUNT'      : 0,
        'QUEUE_FREENODECOUNT'  : 0,
        'QUEUE_MAXTIME'        : 0,
        'QUEUE_MAXCPUTIME'     : 0,
        'QUEUE_MAXCOUNT'       : 0,
        'QUEUE_MAXRUNNINGJOBS' : 0,
        'QUEUE_MAXJOBSINQUEUE' : 0,
        'QUEUE_STATUS'         : '0',
        'QUEUE_DISPATCHTYPE'   : 'Immediate',
        'QUEUE_PRIORITY'       : 'NULL',
        }

    def dynamicNodes(self):
        out, err = self.Communicator.execCommand('LANG=POSIX grep -c processor /proc/cpuinfo')
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        self.total_cpu = int(out.rstrip('\n')) 
        out, err = self.Communicator.execCommand('LANG=POSIX ps -ef | grep .wrapper | grep -v grep | wc -l') 
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))        
        self.free_cpu = self.total_cpu - int(out.rstrip('\n'))

    def queues(self, Host):
        self.queue_default['QUEUE_NODECOUNT']     = self.total_cpu
        self.queue_default['QUEUE_FREENODECOUNT'] = self.free_cpu
        return self._queues_string([self.queue_default])

class Job (drm4g.managers.Job):
    
    def jobSubmit(self, path_script):
        out, err = self.Communicator.execCommand('%s %s' % (SH, path_script))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))
        job_id = out.rstrip('\n')
        return job_id          

    def jobStatus(self):
        out, err = self.Communicator.execCommand('LANG=POSIX ps -e')
        if [line for line in out.splitlines() if self.JobId in line]:
            return 'ACTIVE'
        else:   
            return 'DONE'
    
    def jobCancel(self):
        jobs_to_kill = [self.JobId]
        while jobs_to_kill:
            for job in jobs_to_kill:
                 out, err = self.Communicator.execCommand('LANG=POSIX ps ho pid --ppid %s' % (job)) 
                 jobs_to_kill = [line.lstrip() for line in out.splitlines()] + jobs_to_kill
                 out, err = self.Communicator.execCommand('LANG=POSIX kill -9 %s' % (job))
                 if err:
                     raise drm4g.managers.JobException('Could not kill %s : %s' % (job, ' '.join(err.split('\n'))))
                 jobs_to_kill.remove(job)

    def jobTemplate(self, parameters):
        line  = '#!/bin/bash\n'
        line += ''.join(['export %s=%s\n' % (k, v) for k, v in parameters['environment'].items()])
        line += 'cd $directory\n' 
        line += 'nohup $executable 2> $stderr > $stdout &\n' 
        line += 'echo $$!\n'
        return Template(line).safe_substitute(parameters)


