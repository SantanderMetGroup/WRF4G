import drm4g.managers 
from string import Template
import xml.dom.minidom
import re
import time
from drm4g.managers import sec_to_H_M_S

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: marenostrum.py 1122 2011-08-22 07:56:18Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
MNSUBMIT = 'mnsubmit' #mnsubmit - submits a job script to the queue system 
CHECKJOB = 'checkjob' #checkjob - obtains detailed information about a specific job
MNCANCEL = 'mncancel' #mncancel - removes his/her job from the queue system, 
                      #           canceling the execution of the job if it was already running
MNQ      = 'mnq'      #mnq      - shows all the jobs submitted

class Resource (drm4g.managers.Resource):

    host_properties = {
        'LRMS_NAME'    : 'MNSLURM',
        'LRMS_TYPE'    : 'MNSLURM',
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
        'QUEUE_DISPATCHTYPE'   : 'batch',
        'QUEUE_PRIORITY'       : 'NULL',
        }


    def dynamicNodes(self):
        out, err = self.Communicator.execCommand('%s --xml' % (MNQ))
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        out_parser = xml.dom.minidom.parseString(out)
        cluster = out_parser.getElementsByTagName('Data')[0].getElementsByTagName('cluster')[0]
        self.total_cpu = int(cluster.getAttribute('LocalUpProcs'))
        self.free_cpu = self.total_cpu - int(cluster.getAttribute('LocalAllocProcs'))

    def queues(self, Host):
        self.queue_default['QUEUE_NODECOUNT']     = self.total_cpu
        self.queue_default['QUEUE_FREENODECOUNT'] = self.free_cpu
        return self._queues_string([self.queue_default])

class Job (drm4g.managers.Job):
    
    #clock_wall_time is mandatory
    walltime_default = '172800' # 48 hours 
    #mn job status <--> GridWay job status
    states_mn = {
                  'BatchHold' : 'PENDING',
                  'SystemHold': 'PENDING',
                  'UserHold'  : 'PENDING',   #Job is idle and is not eligible to run due to a user, admin, or batch system hold.
                  'Canceling' : 'PENDING',   #Job is in the process of being cancelled.
                  'Removed'   : 'FAILED',   #Job has run to its requested walltime successfully but has been canceled by the scheduler or resource manager due to exceeding its walltime or violating another policy; includes jobs canceled by users or administrators either before or after a job has started.
                  'NotQueued' : 'FAILED',    #Indicates a system problem in most cases.
                  'Vacated'   :	'FAILED',    #Job canceled after walltime=partial execution due to a system failure.
                  'Running'   : 'ACTIVE',    #Job is currently executing the user application
                  'Migrated'  : 'PENDING',   #This is a transitional state that indicates that the job is in being handed off to the native SLURM resource manager on a specific machine in preparation for running
                  'Idle'      : 'PENDING',   #Job is queued and eligible to run but is not yet executing
                  'Deferred'  : 'FAILED',    #Job can not be run for one reason or another, however Moab will will continue to evaluate the job periodically for run eligibility.
                  'Staging'   :	'PENDING',   #The job has been submitted by Moab to the native batch system for it to run and the native batch system has not confirmed yet that the job is actually running.
                  'Starting'  : 'PENDING',   #The batch system has attempted to start the job and the job is currently performing pre-start tasks which may including provisioning resources, staging data,executing system pre-launch scripts, etc.
                  'Completed' :	'DONE',      #Job has completed running
                  'Suspended' : 'SUSPENDED', #Job was running but has been suspended by the scheduler or an admin.
                }                 
    
    def jobSubmit(self, path_script):
        out, err = self.Communicator.execCommand('%s %s' % (MNSUBMIT, path_script))
        re_job_id = re.compile(r'Submitted batch job (\d*)').search(err)
        if re_job_id:
            time.sleep(60)
            return re_job_id.group(1)
        else:
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobStatus(self):
        out, err = self.Communicator.execCommand('%s %s --xml' % (CHECKJOB, self.JobId))
        if err:
            return 'DONE'
        else:
            out_parser = xml.dom.minidom.parseString(out)
            state = out_parser.getElementsByTagName('Data')[0].getElementsByTagName('job')[0].getAttribute('EState')
            return self.states_mn.setdefault(state, 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (MNCANCEL, self.JobId))
        if not "job '%s' cancelled" % (self.JobId) in out:
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '# @ job_name = JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        args += '# @ initialdir = $directory\n'
        args += '# @ output = $stdout\n'
        args += '# @ error  = $stderr\n'
        args += '# @ total_tasks = $count\n'
        if parameters.has_key('tasksPerNode'):
            args += '# @ tasks_per_node =$tasksPerNode\n'
        if parameters.has_key('maxWallTime'):
            walltime = sec_to_H_M_S(parameters['maxWallTime'])
        else:
            walltime = self.walltime_default
        args += '# @ wall_clock_limit = %s\n' % (walltime)
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in parameters['environment'].items()])
        args += 'srun $executable\n'
        return Template(args).safe_substitute(parameters)

