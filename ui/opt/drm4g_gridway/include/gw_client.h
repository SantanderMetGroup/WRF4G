/* -------------------------------------------------------------------------- */
/* Copyright 2002-2011, GridWay Project Leads (GridWay.org)                   */
/*                                                                            */
/* Licensed under the Apache License, Version 2.0 (the "License"); you may    */
/* not use this file except in compliance with the License. You may obtain    */
/* a copy of the License at                                                   */
/*                                                                            */
/* http://www.apache.org/licenses/LICENSE-2.0                                 */
/*                                                                            */
/* Unless required by applicable law or agreed to in writing, software        */
/* distributed under the License is distributed on an "AS IS" BASIS,          */
/* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.   */
/* See the License for the specific language governing permissions and        */
/* limitations under the License.                                             */
/* -------------------------------------------------------------------------- */

#ifndef _GW_CLIENT_H
#define _GW_CLIENT_H

#include <pthread.h>

#include "gw_rm_msg.h"
#include "gw_job.h"
#include "gw_common.h"

#ifdef HAVE_LIBDB
#include "gw_acct.h"
#endif

/* -------------------------------------------------------------------------- */
/* DATA TYPES                                                                 */
/* -------------------------------------------------------------------------- */

typedef struct gw_client_s
{
  pthread_mutex_t  mutex;
  
  char *           owner;
  char *           group;
  char *           proxy_path;
  
  int              gwd_port;
  char             hostname[GW_MSG_STRING_HOST];
  
  gw_boolean_t     initialize;
  
  int              number_of_jobs;
  gw_msg_job_t **  job_pool;
  
  int              number_of_hosts;
  gw_msg_host_t ** host_pool;
  
}gw_client_t;

typedef enum {
   GW_CLIENT_SIGNAL_KILL,
   GW_CLIENT_SIGNAL_KILL_HARD,   
   GW_CLIENT_SIGNAL_STOP,
   GW_CLIENT_SIGNAL_RESUME,
   GW_CLIENT_SIGNAL_HOLD,
   GW_CLIENT_SIGNAL_RELEASE,
   GW_CLIENT_SIGNAL_RESCHEDULE,
   GW_CLIENT_SIGNAL_LIMIT
} gw_client_signal_t;

/* -------------------------------------------------------------------------- */
/* 1.- INITIALIZATION ROUTINES                                                */
/* -------------------------------------------------------------------------- */

/** Initialize GridWay client API library and create a new client session. 
 *  gw_client_init() function MUST BE called once per program BEFORE any GridWay 
 *  related functions are used. This function initializes the client session 
 *  structure and creates a connection with the GridWay daemon.
 *
 *  @return
 *      - NULL on failure
 *      - Pointer to client session structure
 */

gw_client_t* gw_client_init();


/** Disengage from GridWay library. This routine ends this client session, 
 *  but does not effect any jobs (e.g., queued and running jobs remain 
 *  queued and running). This function frees internal structures and closes 
 *  the connection with gwd.
 */            

void gw_client_finalize();

/* -------------------------------------------------------------------------- */
/* 1.- JOB SUBMITION ROUTINES                                                 */
/* -------------------------------------------------------------------------- */

/** This function submits a single job with the attributes defined in a job 
 *  template file.
 *    
 *  @param template Path to the template file with the job description. This path
 *  MUST be an absolute Path.
 *
 *  @param init_state  Initial state for the job. Allowed values are: 
 *  GW_JOB_STATE_PENDING (job will be scheduled) GW_JOB_STATE_HOLD (job will 
 *  be held, and will not be scheduled until it receives a RELEASE signal.
 *  If other value is specified GW_JOB_STATE_PENDING will be used as default.
 * 
 *  @param job_id unique identification as provided by gwd.
 * 
 *  @param deps job dependencies a '-1' terminated array of job_ids. Once all
 *  these jobs have finished, the job will be released by GridWay. The job is 
 *  submitted in hold state.
 * 
 *  @param fixed_priority initial priority for the job.
 *
 *  @return On success job_id will hold the job unique identification as 
 *  provided by the GridWay system
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED a GridWay related problem: bad job template,
 *        job directories can not be created or the max. job limit has been reached.
 */

gw_return_code_t gw_client_job_submit(char *         template,
                                      gw_job_state_t init_state,
                                      int *          job_id,
                                      int *          deps,
                                      int            fixed_priority);

/** This function submits a set of parametric jobs that can be run concurrently. 
 *  For each parametric job the same template is used. Each job is identical except of it's task 
 *  and job id's:
 *      - task_id (GW_TASK_ID) ranges from 0 to tasks -1.
 *      - Job ids, could not be consecutive numbers
 *    
 *  @param template Path to the template file with the job description. This path
 *  MUST be an absolute Path.
 * 
 *  @param tasks Number of tasks in the array
 *
 *  @param init_state  Initial state for the job. Allowed values are: 
 *  GW_JOB_STATE_PENDING (job will be scheduled) GW_JOB_STATE_HOLD (job will 
 *  be held, and will not be scheduled until it receives a RELEASE signal.
 *  If other value is specified GW_JOB_STATE_PENDING will be used as default.
 * 
 *  @param array_id unique array identification as provided by gwd.
 * 
 *  @param job_ids  array with the ids of the jobs that conform the array. This array
 *  is dynamically allocated, and MUST be freed by the calling program.
 * 
 *  @param deps job dependencies a '-1' terminated array of job_ids. Once all
 *  these jobs have finished, the job will be released by GridWay. The job is 
 *  submitted in hold state.
 * 
 *  @param fixed_priority initial priority for the job.
 * 
 *  @return On success array_id will hold the array unique identification as 
 *  provided by the GridWay system, and job_ids the the ids of the jobs that conform the array.
 *  On error job_ids is equal to NULL, and array_id is -1.
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED_NO_MEMORY job_ids array could not be allocated.
 *      - GW_RC_FAILED a GridWay related problem: bad job template,
 *        job directories can not be created or the max. job limit has been reached.
 */
 
gw_return_code_t gw_client_array_submit(char *         template,
                                        int            tasks, 
                                        gw_job_state_t init_state,                                        
                                        int *          array_id, 
                                        int **         job_ids,
                                        int *          deps,
                                        int            pstart,
                                        int            pinc,
                                        int            fixed_priority);

/* -------------------------------------------------------------------------- */
/* 2.- JOB STATUS ROUTINES                                                    */
/* -------------------------------------------------------------------------- */

/** Obtains the status of a given job.
 * 
 *  @param job_id The job unique identification as provided by the GridWay 
 *  system. The job_id SHOULD be obtained from a gw_client_job_submit() or 
 *  gw_client_array_submit() function calls.
 *
 *  @param *job_status The actual state of the job. The definition of the 
 *  gw_msg_job_t structure can be found in gw_rm_msg.h file.
 * 
 *  @return On success job_status will hold the current job status for this job.
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED_BAD_JOB_ID job does not exist.
 */

gw_return_code_t gw_client_job_status(int job_id, gw_msg_job_t *job_status);

/** Obtains the status of the whole pool. This function updates the job_pool of the 
 *  client session structure. The new data can be saefly read from the client session
 *  structure. NULL values in the pool array are used if the job does not exist. 
 *  
 *  @return On success the client job pool will be updated with the current status for all the jobs.
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 */
 
gw_return_code_t gw_client_job_status_all( );

/** Obtains the history of a job.
 * 
 *  @param job_id The job unique identification as provided by the GridWay 
 *  system. The job_id SHOULD be obtained from a gw_client_job_submit() or 
 *  gw_client_array_submit() function calls.
 *
 *  @param history_list An array of history records, the 
 *  gw_msg_history_t structure can be found in gw_rm_msg.h file.
 *   
 *  @param num_records Number of elements in the history array
 * 
 *  @return On success the history_list array will store the execution history
 *  of the job, and num_records will be the number of elements in the array.
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED_BAD_JOB_ID job does not exist.
 */

gw_return_code_t gw_client_job_history(int                 job_id, 
                                       gw_msg_history_t ** history_list, 
                                       int *               num_records);

/* -------------------------------------------------------------------------- */
/* 3.- JOB CONTROL ROUTINES                                                   */
/* -------------------------------------------------------------------------- */

/** This function allows the job specified by jobid to be signaled 
 *  Possible signals are:
 *
 *      - GW_CLIENT_SIGNAL_STOP A job will be stopped, and restart files 
 *  tranferred back to the client. These files if provided by the running job 
 *  will be used on RESUME to re-start execution.
 *
 *      - GW_CLIENT_SIGNAL_RESUME  A previously stopped job will be resumed.
 *  If re-start files are provided the job will used them to re-start execution,
 *  from the last checkpointing context. (Only STOPed jobs can be RESUMEd)
 *
 *      - GW_CLIENT_SIGNAL_KILL The job will be killed, it execution can be 
 *  synchronized through the wait routines. 
 *
 *      - GW_CLIENT_SIGNAL_HOLD The job will not be scheduled to any host until it 
 *  receives a RELEASE signal (only PENDING jobs can be held).
 * 
 *      - GW_CLIENT_SIGNAL_RELEASE The job will be go on to the PENDING state, and 
 *  so it will be scheduled. (only jobs in HOLD state can be RELEASEd).
 * 
 *      - GW_CLIENT_SIGNAL_RESCHEDULE The reschedule flag of the job will be raised. 
 *  The GridWay system will try to migrate the job to a better host. Note
 *  that a migration occurs only if a better host is found.
 *
 *  @param job_id The job unique identification as provided by the GridWay 
 *  system. The job_id SHOULD be obtained from a gw_client_job_submit() or 
 *  gw_client_array_submit() function calls.
 *
 *  @param signal The signal to be sent to the job whose value 
 *  may be one of the following: GW_CLIENT_SIGNAL_STOP, GW_CLIENT_SIGNAL_RESUME,
 *  GW_CLIENT_SIGNAL_KILL, GW_CLIENT_SIGNAL_HOLD, GW_CLIENT_SIGNAL_RELEASE,
 *  GW_CLIENT_SIGNAL_RESCHEDULE.
 * 
 *  @param blocking If set to GW_TRUE the function call will block until the 
 *  action associated to the signal is completed. This parameter only is considered
 *  for GW_CLIENT_SIGNAL_KILL and GW_CLIENT_SIGNAL_STOP, in this case a the return value
 *  GW_RC_SUCCESS, does not imply that the action has been succesfully performed.
 * 
 *  @return On success the signal will be passed to the job, and the requested 
 *  action will be performed.
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED undefined signal 
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED_BAD_JOB_ID job does not exist.
 *      - GW_RC_FAILED_BAD_JOB_STATE, the signal cannot be send to the job in
 *        its current state
 * 
 */
 
gw_return_code_t gw_client_job_signal (int                job_id, 
                                       gw_client_signal_t signal, 
                                       gw_boolean_t       blocking);

/** This function allows signals all the jobs in a given array, specified by 
 *  array_id
 *  
 *  The function is similar to gw_client_job_signal 
 */
                                       
gw_return_code_t gw_client_array_signal (int                array_id, 
                                         gw_client_signal_t signal, 
                                         gw_boolean_t       blocking);
                                       
                                       
/* -------------------------------------------------------------------------- */
/* 4.- JOB SYNCHRONIZATION ROUTINES                                           */
/* -------------------------------------------------------------------------- */

/** Waits until the execution of job finishes. (Blocking)
 * 
 *  @param job_id The job unique identification as provided by the GridWay 
 *  system. The job_id SHOULD be obtained from a gw_client_job_submit() or 
 *  gw_client_array_submit() function calls.
 *
 *  @param *exit_code The exit code of the job, it SHOULD be used when the function returns
 *  GW_RC_SUCCESS
 * 
 *  @param timeout wait only timeout seconds
 * 
 *  @return On success the row job_id of the match_matrix .
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED_BAD_JOB_ID job does not exist.
 *      - GW_RC_FAILED_JOB_KILLED job was killed.
 *      - GW_RC_FAILED_TIMEOUT timeout expired.
 *      - GW_RC_FAILED_JOB_FAIL job execution failed.
 */

gw_return_code_t gw_client_wait(int job_id, int *exit_code, signed long timeout);

/** Waits until the execution of a set of jobs finishes. (Blocking)
 * 
 *  @param *job_ids The job unique identifications as provided by the GridWay 
 *  system. The job_ids SHOULD be obtained from a gw_client_job_submit() or 
 *  gw_client_array_submit() function calls.
 *
 *  @param any if set to GW_TRUE only waits for the first job in the set and returns.
 *  In this case job_ids[0] will hold the identification of the job that has finished.
 * 
 *  @param **exit_code The exit codes of the jobs, it SHOULD be used when the function returns
 *  GW_RC_SUCCESS, exit_code of job_ids[i] will be stored in exit_code[i].
 *
 *  @param timeout wait only timeout seconds
 * 
 *  @return On success the row job_id of the match_matrix .
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED_BAD_JOB_ID a job does not exist.
 *      - GW_RC_FAILED_JOB_KILLED a job was killed.
 *      - GW_RC_FAILED_TIMEOUT timeout expired
 *      - GW_RC_FAILED_JOB_FAIL a job execution failed.
 */

gw_return_code_t gw_client_wait_set(int *job_id, int **exit_codes, gw_boolean_t any, signed long timeout);

/* -------------------------------------------------------------------------- */
/* 5.- HOST STATUS ROUTINES                                                   */
/* -------------------------------------------------------------------------- */

/** Obtains the status of a given host.
 * 
 *  @param host_id The host unique identification as provided by the GridWay 
 *  system. 
 *
 *  @param *host_status The actual state of the host. The definition of the 
 *  gw_msg_host_t structure can be found in gw_rm_msg.h file.
 * 
 *  @return On success host_status will hold the current status for this host.
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED_BAD_HOST_ID host does not exist.
 */

gw_return_code_t gw_client_host_status(int host_id, gw_msg_host_t *host_status);

/** Obtains the status of the whole pool. This function updates the host_pool of the 
 *  client session structure. The new data can be saefly read from the client session
 *  structure. NULL values in the pool array are used if the host does not exist.
 *  
 *  @return On success the client host pool will be updated with the current status for all the hosts.
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 */

gw_return_code_t gw_client_host_status_all( );

/* -------------------------------------------------------------------------- */
/* 6.- HOST MATCHING ROUTINES                                                 */
/* -------------------------------------------------------------------------- */

/** Matches a job with the host pool. This function updates the row job_id of the 
 *  match_matrix of the client session structure. The new data can be saefly read from the 
 *  client session structure. NULL values in the match_matrix are used if the host does not exist.
 * 
 *  @param job_id The job unique identification as provided by the GridWay 
 *  system. The job_id SHOULD be obtained from a gw_client_job_submit() or 
 *  gw_client_array_submit() function calls.
 *
 *  @return On success the row job_id of the match_matrix .
 *      - GW_RC_SUCCESS on success.
 *      - GW_RC_FAILED_INIT if gw_client_init() function has not been
 *        previously called.
 *      - GW_RC_FAILED_CONNECTION A socket related problem, an error string is
 *        printed on stderr.
 *      - GW_RC_FAILED_BAD_JOB_ID job does not exist.
 */

gw_return_code_t gw_client_match_job(int                 job_id,
                                     int                 array_id,
                                     gw_msg_match_t **   match_list, 
                                     int *               num_records);

/* -------------------------------------------------------------------------- */
/* 7.- USER STATUS ROUTINES                                                   */
/* -------------------------------------------------------------------------- */

gw_return_code_t gw_client_user_status(gw_msg_user_t **user_status, int *num_users);


/* -------------------------------------------------------------------------- */
/* 8.- ACCOUNTING ROUTINES                                                    */
/* -------------------------------------------------------------------------- */
#ifdef HAVE_LIBDB
gw_return_code_t gw_client_user_accts(const char *user, gw_acct_t ***user_accts, int *num_users, time_t from_time);

gw_return_code_t gw_client_host_accts(const char *host, gw_acct_t ***host_accts, int *num_hosts, time_t from_time);

gw_return_code_t gw_client_host_and_user_accts(const char *host, const char *user, gw_acct_t ***host_accts, int *num_hosts, time_t from_time);
#endif

#endif
