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

#ifndef _GW_MSG_H
#define _GW_MSG_H


#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>

#include "gw_common.h"
#include "gw_template.h"
#include "gw_host.h"
#include "gw_conf.h"
#include "gw_history.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#define GW_MSG_STRING_LONG   1024
#define GW_MSG_STRING_HOST   40
#define GW_MSG_STRING_USER_AT_HOST   66
#define GW_MSG_STRING_SHORT  25
#define GW_MSG_STATS_LENGTH  5

#define GW_MSG_START_TIME 0
#define GW_MSG_EXIT_TIME  1
#define GW_MSG_CPU_TIME   2
#define GW_MSG_XFR_TIME   3
#define GW_MSG_LAST_CHK   4

/* ------------------------------------------------------------------------- */
/* ------------------------------------------------------------------------- */
/* ------------------------------------------------------------------------- */

#define gw_rm_copy_str_short(src,dst)\
if (src != NULL){\
    strncpy(dst, src, GW_MSG_STRING_SHORT);\
    if ( strlen(src) >= GW_MSG_STRING_SHORT )\
    dst[GW_MSG_STRING_SHORT-1] = '\0';\
 }\
 else\
    dst[0]='\0'

#define gw_rm_copy_str_host(src,dst)\
if (src != NULL){\
    strncpy(dst, src, GW_MSG_STRING_HOST);\
    if ( strlen(src) >= GW_MSG_STRING_HOST )\
    dst[GW_MSG_STRING_HOST-1] = '\0';\
 }\
 else\
    dst[0]='\0'

#define gw_rm_copy_str_user_at_host(src,dst)\
if (src != NULL){\
    strncpy(dst, src, GW_MSG_STRING_USER_AT_HOST);\
    if ( strlen(src) >= GW_MSG_STRING_USER_AT_HOST )\
    dst[GW_MSG_STRING_USER_AT_HOST-1] = '\0';\
 }\
 else\
    dst[0]='\0'

#define gw_rm_copy_str_long(src,dst)\
if (src != NULL){\
    strncpy(dst, src, GW_MSG_STRING_LONG);\
    if ( strlen(src) >= GW_MSG_STRING_LONG )\
    dst[GW_MSG_STRING_LONG-1] = '\0';\
 }\
 else\
    dst[0]='\0'


/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef enum
{
  GW_MSG_SUBMIT,
  GW_MSG_SUBMIT_ARRAY,
  GW_MSG_WAIT,
  GW_MSG_KILL,
  GW_MSG_KILL_ASYNC,
  GW_MSG_KILL_HARD, 
  GW_MSG_STOP,
  GW_MSG_STOP_ASYNC,  
  GW_MSG_RESUME,
  GW_MSG_HOLD,
  GW_MSG_RELEASE,
  GW_MSG_RESCHEDULE,
  GW_MSG_JOB_STATUS,
  GW_MSG_JOB_POOL_STATUS,
  GW_MSG_JOB_HISTORY,
  GW_MSG_HOST_STATUS,
  GW_MSG_HOST_POOL_STATUS,
  GW_MSG_JOB_MATCH,
  GW_MSG_END_JOB,
  GW_MSG_END,
  GW_MSG_DISENGAGE,
  GW_MSG_USERS
} gw_msg_type_t;

typedef enum
{
  GW_MSG_WAIT_JOB,
  GW_MSG_WAIT_ANY,
  GW_MSG_WAIT_ANY_FIRST
} gw_msg_wait_type_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_msg_s
{
    gw_msg_type_t msg_type;	
    gw_return_code_t rc;

    int client_socket;

    gw_msg_wait_type_t wait_type;

    char owner[GW_MSG_STRING_SHORT];
    char group[GW_MSG_STRING_SHORT];
    char proxy_path[GW_MSG_STRING_LONG];

    int	init_state;	
    //gw_template_t jt;

    int job_id;
    int array_id;	
    int number_of_tasks;
    int exit_code;
    
    int pstart;
    int pinc;
    
    int fixed_priority;
 
} gw_msg_t;   

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_msg_submit_s
{
    gw_msg_t msg;
    gw_template_t jt;

} gw_msg_submit_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_msg_job_s
{
    gw_msg_type_t msg_type;
    gw_return_code_t rc;

    char owner[GW_MSG_STRING_SHORT];
    char name[GW_MSG_STRING_SHORT];
  // This host includes the hostname/lrms combination, using larger size than host
    char host[GW_MSG_STRING_USER_AT_HOST];
    
    int id;
    int array_id;
    int task_id;
    int total_tasks;
    
    int uid;

    int fixed_priority;
    time_t deadline;

    gw_jobtype_t type;
    int np;
	
    int em_state;
    int job_state;
    
    time_t start_time;
    time_t exit_time;
    time_t xfr_time;
    time_t cpu_time;
    
    int exit_code;

    int restarted;
    int client_waiting;
         
    gw_boolean_t reschedule;
   
} gw_msg_job_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_msg_history_s
{
	gw_msg_type_t msg_type;
	gw_return_code_t rc;

    int rank;
    int host_id;
    
    time_t start_time;
    time_t exit_time;
    
    time_t prolog_stime;
    time_t prolog_etime;

    time_t pre_wrapper_stime;
    time_t pre_wrapper_etime;

    time_t wrapper_stime;
    time_t wrapper_etime;

    time_t epilog_stime;
    time_t epilog_etime;

    time_t migration_stime;
    time_t migration_etime;
       
    char rdir[GW_MSG_STRING_SHORT];
  // Next contains hostname/lrms
	char em_rc[GW_MSG_STRING_USER_AT_HOST];
    char queue[GW_MSG_STRING_SHORT];
    	
    int tries;
    gw_migration_reason_t reason;
       
} gw_msg_history_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_msg_host_s 
{
	gw_msg_type_t msg_type;
	gw_return_code_t rc;
	
    char em_mad[GW_MSG_STRING_SHORT];
    char tm_mad[GW_MSG_STRING_SHORT];
    char im_mad[GW_MSG_STRING_SHORT]; 

    int used_slots;
	int running_jobs;
	
    int host_id;
    int fixed_priority;

    char hostname[GW_MSG_STRING_HOST];
    char arch[GW_MSG_STRING_SHORT];
    char os_name[GW_MSG_STRING_SHORT];
    char os_version[GW_MSG_STRING_SHORT];

    char cpu_model[GW_MSG_STRING_SHORT];
    int  cpu_mhz;
    int  cpu_free;
    int  cpu_smp;
    int  nodecount;

    int size_mem_mb;
    int free_mem_mb;
    int size_disk_mb;
    int free_disk_mb;

    char fork_name[GW_MSG_STRING_SHORT];
    char lrms_name[GW_MSG_STRING_SHORT];
    char lrms_type[GW_MSG_STRING_SHORT];

	int  number_of_queues;
    char queue_name[GW_HOST_MAX_QUEUES][GW_MSG_STRING_SHORT];
    int  queue_nodecount[GW_HOST_MAX_QUEUES];
    int  queue_freenodecount[GW_HOST_MAX_QUEUES];
    int  queue_maxtime[GW_HOST_MAX_QUEUES];
    int  queue_maxcputime[GW_HOST_MAX_QUEUES];
    int  queue_maxcount[GW_HOST_MAX_QUEUES];
    int  queue_maxrunningjobs[GW_HOST_MAX_QUEUES];
    int  queue_maxjobsinqueue[GW_HOST_MAX_QUEUES];
    char queue_status[GW_HOST_MAX_QUEUES][GW_MSG_STRING_SHORT];
    char queue_dispatchtype[GW_HOST_MAX_QUEUES][GW_MSG_STRING_SHORT];
    char queue_priority[GW_HOST_MAX_QUEUES][GW_MSG_STRING_SHORT];

	int  number_of_int_vars;   
   	char gen_var_int_name[GW_HOST_MAX_GENVARS][GW_MSG_STRING_SHORT];
   	int  gen_var_int_value[GW_HOST_MAX_GENVARS];

	int  number_of_str_vars;
   	char gen_var_str_name[GW_HOST_MAX_GENVARS][GW_MSG_STRING_SHORT];
   	char gen_var_str_value[GW_HOST_MAX_GENVARS][GW_MSG_STRING_LONG];   	
    
} gw_msg_host_t;


typedef struct gw_msg_match_s 
{
	gw_msg_type_t	 msg_type;
	gw_return_code_t rc;
	
	int              host_id;
	int              job_id;
	int              fixed_priority;

	gw_boolean_t     matched;

    char hostname[GW_MSG_STRING_HOST];
	int  running_jobs;
	
	int  number_of_queues;
    char queue_name[GW_HOST_MAX_QUEUES][GW_MSG_STRING_SHORT];
    int  match[GW_HOST_MAX_QUEUES];
    int  rank [GW_HOST_MAX_QUEUES];
    int  slots[GW_HOST_MAX_QUEUES];
    
} gw_msg_match_t;


typedef struct gw_msg_user_s 
{
    gw_msg_type_t msg_type;
    gw_return_code_t rc;
	
    int user_id;
    char name[GW_MSG_STRING_SHORT];	
    char dn[GW_MSG_STRING_LONG];	
    
	int active_jobs;
	int running_jobs;
	
	time_t idle;
	
	int  num_ems;
	int  em_pid[GW_MAX_MADS];
	char em_name[GW_HOST_MAX_QUEUES][GW_MSG_STRING_SHORT];
	
	int  num_tms;
	int  tm_pid[GW_MAX_MADS];
	char tm_name[GW_HOST_MAX_QUEUES][GW_MSG_STRING_SHORT];
    
} gw_msg_user_t;

#endif
