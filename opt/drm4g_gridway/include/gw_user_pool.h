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

#ifndef _GW_USER_POOL_H
#define _GW_USER_POOL_H

#include <pthread.h>
#include "gw_common.h"
#include "gw_conf.h"
#include "gw_user.h"
#include "gw_im.h"

typedef struct gw_user_pool_s
{
	pthread_mutex_t mutex;            
                  
    int             number_of_users;   
    gw_user_t **    pool;
    
    int             um_em_pipe_w;
    int             um_tm_pipe_w;

} gw_user_pool_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_user_pool_t * gw_user_pool_init();

void gw_user_pool_finalize();
void gw_user_pool_set_mad_pipes(int um_em_pipe_w, int um_tm_pipe_w);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int gw_user_pool_user_allocate (const char *user, const char *proxy_path, int *user_id);

void gw_user_pool_user_free (int user_id);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_em_mad_t * gw_user_pool_get_em_mad (int user_id, const char *name);

gw_em_mad_t * gw_user_pool_get_em_mad_by_fd (int user_id, int fd);

gw_tm_mad_t * gw_user_pool_get_tm_mad (int user_id, const char *name);

gw_tm_mad_t * gw_user_pool_get_tm_mad_by_fd (int user_id, int fd);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int gw_user_pool_set_em_pipes (fd_set *       in_pipes,
                               int *          fds, 
                               int *          num_fds, 
                               gw_em_mad_t ** em_mads, 
                               int            um_em_pipe_r);
                               
int gw_user_pool_set_tm_pipes (fd_set *       in_pipes, 
                               int *          fds, 
                               int *          num_fds, 
                               gw_tm_mad_t ** tm_mads, 
                               int            um_tm_pipe_r);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_boolean_t gw_user_pool_exists (const char *name, const char *proxy_path, int *user_id);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_user_pool_inc_jobs(int uid, int jobs);

void gw_user_pool_dec_jobs(int uid);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_user_pool_inc_running_jobs(int uid, int jobs);

void gw_user_pool_dec_running_jobs(int uid);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_user_pool_check_users(time_t period);

gw_boolean_t gw_user_pool_get_info(int uid, gw_msg_user_t *msg);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_user_pool_dm_recover (gw_dm_mad_t * dm_mad);

#endif
