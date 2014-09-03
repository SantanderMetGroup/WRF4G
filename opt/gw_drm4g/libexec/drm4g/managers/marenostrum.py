import xml.dom.minidom
import re
import time
import drm4g.managers 
from string import Template

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: marenostrum.py 2250 2014-08-27 09:04:57Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
MNSUBMIT = 'LANG=POSIX mnsubmit' #mnsubmit - submits a job script to the queue system 
CHECKJOB = 'LANG=POSIX checkjob' #checkjob - obtains detailed information about a specific job
MNCANCEL = 'LANG=POSIX mncancel' #mncancel - removes his/her job from the queue system, canceling the execution of the job if it was already running
MNQ      = 'LANG=POSIX mnq'      #mnq      - shows all the jobs submitted

class Resource (drm4g.managers.Resource):
    pass

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
    
    def jobSubmit(self, pathScript):
        out, err = self.Communicator.execCommand('%s %s' % (MNSUBMIT, pathScript))
        re_job_id = re.compile(r'Submitted batch job (\d*)').search(err)
        if re_job_id:
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
        if parameters.has_key('ppn'):
            args += '# @ tasks_per_node =$ppn\n'
        if parameters.has_key('maxWallTime'):
            walltime = parameters['maxWallTime']
        else:
            walltime = self.walltime_default
        args += '# @ wall_clock_limit = %s\n' % (walltime)
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in parameters['environment'].items()])
        args += '\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)

