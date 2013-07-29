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

#ifndef GW_DM_SCHEDULER_H_
#define GW_DM_SCHEDULER_H_

#include "gw_client.h"

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

typedef struct gw_sch_host_s
{
	int  hid;
	char name[GW_MSG_STRING_HOST];

    int  used_slots;
	int  running_jobs;

	int  dispatched;

} gw_sch_host_t;

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

typedef struct gw_sch_user_host_s
{
    int    ha_id;
    int    hid;
  
    time_t banned;
    time_t last_banned;
  
    float  avrg_transfer;
    float  avrg_execution;
    float  avrg_suspension;
    
    float last_transfer;
    float last_execution;
    float last_suspension;

} gw_sch_user_host_t;

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

typedef struct gw_sch_user_s
{
	int   uid;
	char  name[GW_MSG_STRING_SHORT];
	
	int   running_jobs;
	int   active_jobs;
	int   total_jobs;

	int   dispatched;
	
	int   share;
	int   next_ujid;
	int * sub_windows;
	
	gw_sch_user_host_t * hosts;
	
} gw_sch_user_t;

/* -------------------------------------------------------------------- */

typedef struct gw_sch_queue_s
{
	int  ha_id;
	int  uha_id;
	
	char qname[GW_MSG_STRING_SHORT];
	int  slots;

	int   fixed;
	float nfixed;
	
	int   rank;
	float nrank;
	
	float usage;
	float nusage;
	
	float priority;
	
} gw_sch_queue_t;

/* -------------------------------------------------------------------- */

typedef struct gw_sch_job_s
{
	int ua_id;
		
	int jid;
	int aid;
        
    int np;
	
    gw_migration_reason_t reason;
	
	int              num_mhosts;
    gw_sch_queue_t * mhosts;
    
    time_t schedule_time;
    time_t deadline_time;

    float priority;
        
    int   fixed;
    float nfixed;
    
    int   waiting;
    float nwaiting;
    
    int   deadline;
    float ndeadline;
    
    int   share;
    float nshare;
    
} gw_sch_job_t;

/* -------------------------------------------------------------------- */

typedef struct gw_scheduler_s
{
  int             num_users;
  gw_sch_user_t * users;
  
  int             num_hosts;
  gw_sch_host_t * hosts;
  
  int             num_jobs;
  gw_sch_job_t *  jobs;
  
  gw_sch_conf_t   sch_conf;
  
  time_t          next_user_window;
  time_t          next_host_window;
  
} gw_scheduler_t;

/* -------------------------------------------------------------------- */

typedef void (*gw_scheduler_function_t) (gw_scheduler_t * sched,
									     void *           user_arg);

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

void gw_scheduler_loop(gw_scheduler_function_t scheduler, void *user_arg);

void gw_scheduler_print (const char mode, const char *str_format,...);

void gw_scheduler_ctime(time_t the_time, char *str);

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

void gw_scheduler_add_host(gw_scheduler_t * sched, 
                           int              hid,
                           int              uslots,
                           int              rjobs,
                           char *           hostname);
                           
void gw_scheduler_update_usage_host(gw_scheduler_t * sched);

float gw_scheduler_host_estimated_time(gw_sch_user_host_t * th,
                                              gw_scheduler_t *     sched);
                                              
void gw_scheduler_host_policies (gw_scheduler_t * sched, int jid);                                              
                           
/* -------------------------------------------------------------------- */

void gw_scheduler_add_user(gw_scheduler_t * sched, 
                           int              uid, 
                           int              ajobs, 
                           int              rjobs, 
                           char *           name);

void gw_scheduler_del_user(gw_scheduler_t * sched, int uid);

void gw_scheduler_user_update_windows(gw_scheduler_t * sched);
                               
/* -------------------------------------------------------------------- */

void gw_scheduler_job_failed(gw_scheduler_t *      sched, 
                             int                   hid, 
                             int                   uid, 
                             gw_migration_reason_t reason);
                             
void gw_scheduler_job_success(gw_scheduler_t * sched, 
                              int              hid, 
                              int              uid, 
                              float            txfr,
                              float            tsus,
                              float            texe);

void gw_scheduler_job_add(gw_scheduler_t *      sched,
                          int                   jid, 
                          int                   aid,
                          int                   np,
                          gw_migration_reason_t reason,
                          int                   fixed_priority,
                          int                   uid,
                          time_t                deadline);
                     
void gw_scheduler_job_del(gw_scheduler_t *      sched,
                          int                   jid,
                          int                   dispatched);

void gw_scheduler_matching_arrays(gw_scheduler_t * sched);

/* -------------------------------------------------------------------- */

void gw_scheduler_job_policies (gw_scheduler_t * sched);

#endif /*GW_DM_SCHEDULER_H_*/
