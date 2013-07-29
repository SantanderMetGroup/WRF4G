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

#ifndef _GW_TM_H
#define _GW_TM_H

#include <pthread.h>
#include "gw_action.h"
#include "gw_job_pool.h"
#include "gw_tm_mad.h"
#include "gw_conf.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#define GW_TM_MAX_ACTION    20

#define GW_TM_MAX_INFO      65536

#define GW_TM_MAX_RESULT    10

#define GW_TM_MAX_STRING    65536

#define GW_TM_MAX_JOB_ID    5

#define GW_TM_MAX_XFR_ID    5

#define GW_TM_TIMER_PERIOD  5

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_tm_s {

    pthread_t listener_thread;
    pthread_t thread_id;
    
    pthread_mutex_t mutex;

	int     um_tm_pipe_r;
	int     um_tm_pipe_w;
	
    gw_am_t am;        
    gw_am_t *dm_am;
    
} gw_tm_t;

/* -------------------------------------------------------------------------- */

extern gw_tm_t gw_tm;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_tm_t* gw_tm_init();
                                        
void gw_tm_finalize();

void gw_tm_set_dm_am(gw_am_t *dm_am);                    

/* -------------------------------------------------------------------------- */

void gw_tm_prolog(void *_job_id);

void gw_tm_prolog_stage_in(gw_job_t * job);

void gw_tm_prolog_cp_cb(gw_job_t * job, int cp_xfr_id, gw_boolean_t failure);

int gw_tm_prolog_build_urls(gw_job_t *   job, 
                            char *       src, 
                            char *       dst, 
                            char **      src_url,
                            char **      dst_url);

/* -------------------------------------------------------------------------- */

void gw_tm_epilog(void *_job_id);

void gw_tm_epilog_stage_out(gw_job_t * job);

void gw_tm_epilog_cp_cb(gw_job_t * job, int cp_xfr_id, gw_boolean_t failure);

int gw_tm_epilog_build_urls(gw_job_t *   job, 
                            char *       src, 
                            char *       dst, 
                            char **      src_url,
                            char **      dst_url);                            

/* -------------------------------------------------------------------------- */

void gw_tm_timer(void *_job_id);

/* -------------------------------------------------------------------------- */

void gw_tm_checkpoint_cp(gw_job_t * job);

void gw_tm_checkpoint_cp_cb(gw_job_t * job, int cp_xfr_id, gw_boolean_t failure);                            
                            
/* -------------------------------------------------------------------------- */

void gw_tm_start();

void gw_tm_listener(void *arg);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif
