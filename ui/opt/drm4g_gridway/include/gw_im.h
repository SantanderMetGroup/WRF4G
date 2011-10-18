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

#ifndef _GW_IM_H
#define _GW_IM_H

#include <pthread.h>
#include "gw_action.h"
#include "gw_job_pool.h"
#include "gw_im_mad.h"
#include "gw_conf.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#define GW_IM_MAX_ACTION    20
#define GW_IM_MAX_INFO      65536
#define GW_IM_MAX_RESULT    10
#define GW_IM_MAX_STRING    65536
#define GW_IM_MAX_HOST_ID   5

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_im_s {

    pthread_t thread_id;
    pthread_t listener_thread;
    
    pthread_mutex_t mutex;
    
    int         registered_mads;
    gw_im_mad_t im_mad[GW_MAX_MADS];

    int         active_queries;
    
    gw_am_t   am;        
    gw_am_t * dm_am;
    
} gw_im_t;

extern gw_im_t gw_im;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_im_t* gw_im_init();
                                        
void gw_im_finalize();

void gw_im_set_dm_am(gw_am_t *dm_am);                    

/* -------------------------------------------------------------------------- */

int gw_im_register_mad(const char *executable, const char *name, 
            const char *arg);

gw_im_mad_t * gw_im_get_mad_by_name(const char *name);
gw_im_mad_t * gw_im_get_mad_by_fd(int fd);

/* -------------------------------------------------------------------------- */

void gw_im_discover(char *mad_name);

void gw_im_monitor(gw_host_t * host);

void gw_im_timer(void *_null);

/* -------------------------------------------------------------------------- */

void gw_im_start();

void gw_im_listener(void *arg);

/* -------------------------------------------------------------------------- */

int gw_im_set_pipes (fd_set *in_pipes, int *num_mads);

/* -------------------------------------------------------------------------- */

#endif
