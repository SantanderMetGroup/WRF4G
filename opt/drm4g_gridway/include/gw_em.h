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

#ifndef _GW_EM_H
#define _GW_EM_H

#include <pthread.h>
#include "gw_action.h"
#include "gw_job_pool.h"
#include "gw_em_mad.h"
#include "gw_conf.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#define GW_EM_MAX_ACTION    20

#define GW_EM_MAX_INFO      65536

#define GW_EM_MAX_RESULT    10

#define GW_EM_MAX_STRING    65536

#define GW_EM_MAX_JOB_ID    5

#define GW_EM_TIMER_PERIOD  5

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_em_s {

    pthread_t listener_thread;
    pthread_t thread_id;
    
    pthread_mutex_t mutex;
    
    int     um_em_pipe_r;
    int     um_em_pipe_w;    
    
    gw_am_t am;        
    gw_am_t *dm_am;
    
} gw_em_t;

/* -------------------------------------------------------------------------- */

extern gw_em_t gw_em;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#define issubmitted(gw_em_state)(\
(gw_em_state == GW_EM_STATE_ACTIVE) ||\
(gw_em_state == GW_EM_STATE_PENDING) ||\
(gw_em_state == GW_EM_STATE_SUSPENDED) )


/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_em_t* gw_em_init();
                                        
void gw_em_finalize();

void gw_em_set_dm_am(gw_am_t *gwd_am);                    

/* -------------------------------------------------------------------------- */

void gw_em_submit(void *_job_id);

void gw_em_pending(void *_job_id);

void gw_em_active(void *_job_id);

void gw_em_suspended(void *_job_id);

void gw_em_cancel(void *_job_id);

void gw_em_done(void *_job_id);

void gw_em_failed(void *_job_id);

void gw_em_timer();

/* -------------------------------------------------------------------------- */

void gw_em_start();

void gw_em_listener(void *arg);

int gw_rand(int max);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif
