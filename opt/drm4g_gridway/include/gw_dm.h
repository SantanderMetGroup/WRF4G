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

#ifndef _GW_DM_H
#define _GW_DM_H

#include <pthread.h>
#include "gw_action.h"
#include "gw_array_pool.h"
#include "gw_job_pool.h"
#include "gw_dm_mad.h"

#define GW_DM_MAX_ACTION    20
#define GW_DM_MAX_INFO      1024
#define GW_DM_MAX_RESULT    10
#define GW_DM_MAX_STRING    32768
#define GW_DM_MAX_ID        5

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_dm_s
{
    pthread_t       thread_id;
    pthread_t       listener_thread;
    
    pthread_mutex_t mutex;
        
    gw_am_t  am;
    gw_am_t  *tm_am;
    gw_am_t  *em_am;
    gw_am_t  *rm_am;
    
    int         registered_mads;
    gw_dm_mad_t dm_mad[GW_MAX_MADS];
    
    gw_boolean_t scheduling;
} gw_dm_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
extern gw_dm_t gw_dm;
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_dm_t * gw_dm_init();
void      gw_dm_finalize();

void gw_dm_set_tm_am ( gw_am_t *tm_am );
void gw_dm_set_em_am ( gw_am_t *em_am );
void gw_dm_set_rm_am ( gw_am_t *rm_am );

void gw_dm_start ();

void gw_dm_listener(void *arg);

/* -------------------------------------------------------------------------- */

int gw_dm_register_mad(const char *executable, const char *name, 
            const char *arg);

gw_dm_mad_t* gw_dm_get_mad(const char *name);

int gw_dm_set_pipes (fd_set *in_pipes, int *num_mads);

gw_dm_mad_t * gw_dm_get_mad_by_fd(int fd);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_dm_hold       (void *_job_id);
void gw_dm_release    (void *_job_id);
void gw_dm_stop       (void *_job_id);
void gw_dm_resume     (void *_job_id);
void gw_dm_kill       (void *_job_id);
void gw_dm_kill_hard  (void *_job_id);
void gw_dm_wait       (void *_job_id);
void gw_dm_reschedule (void *_job_id);
void gw_dm_stopped    (void *_job_id);
void gw_dm_fail_epilog(void *_job_id);
void gw_dm_schedule   (void *_null);
void gw_dm_aalloc     (void *_msg);
void gw_dm_jalloc     (void *_msg);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int gw_dm_dispatch_job (int job_id, int host_id, char *queue_name, int rank);

int gw_dm_dispatch_tasks (int    array_id,
                          int    ntasks, 
                          int    host_id,
                          char * queue_name, 
                          int    rank);

void gw_dm_uncheck_job(int job_id);
                          
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_dm_prolog           ( void *_job_id );
void gw_dm_migr_prolog      ( void *_job_id );
void gw_dm_prolog_done_cb   ( void *_job_id );
void gw_dm_prolog_failed_cb ( void *_job_id );

void gw_dm_pre_wrapper       ( void *_job_id );
void gw_dm_wrapper           ( void *_job_id );
void gw_dm_wrapper_done_cb   ( void *_job_id );
void gw_dm_wrapper_failed_cb ( void *_job_id );

void gw_dm_epilog_std       (void *_job_id);
void gw_dm_migr_epilog      (void *_job_id);
void gw_dm_stop_epilog      (void *_job_id);
void gw_dm_kill_epilog      (void *_job_id);
void gw_dm_epilog_fail      (void *_job_id);
void gw_dm_epilog           (void *_job_id);
void gw_dm_epilog_restart   (void *_job_id);
void gw_dm_epilog_done_cb   (void *_job_id);
void gw_dm_epilog_failed_cb (void *_job_id);

void gw_dm_migr_cancel ( void *_job_id );

void gw_dm_zombie  (void *_job_id);
void gw_dm_pending (void *_job_id);
void gw_dm_failed  (void *_job_id);
void gw_dm_stopped (void *_job_id);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif



