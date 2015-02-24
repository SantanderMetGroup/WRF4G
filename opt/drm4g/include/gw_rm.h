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

#ifndef _GW_RM_H
#define _GW_RM_H

#include <pthread.h>
#include <sys/socket.h>      
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "gw_action.h"
#include "gw_rm_connection_list.h"
#include "gw_rm_msg.h"
			  
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_rm_s
{
  pthread_t thread_id;
  pthread_t listener_thread;

  struct sockaddr_in rm_addr;
  int                socket;

  gw_am_t *dm_am;
  gw_am_t am;
  
  pthread_mutex_t        connection_list_mutex;
  gw_connection_list_t * connection_list;
  
}gw_rm_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

extern gw_rm_t gw_rm;

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

gw_rm_t* gw_rm_init();
void     gw_rm_finalize(void *_null);
void     gw_rm_start(void *_null);
void     gw_rm_set_dm_am (gw_am_t *dm_am);

void gw_rm_listener(void *_null);

void gw_rm_submit(void *_msg);

void gw_rm_connection(void *_connection);
void gw_rm_connection_submit(void *_connection);

void gw_rm_hold_success(void *_job_id);    
void gw_rm_hold_failed (void *_job_id);

void gw_rm_release_success(void *_job_id);    
void gw_rm_release_failed (void *_job_id);
    
void gw_rm_kill_success(void *_job_id);    
void gw_rm_kill_failed (void *_job_id);

void gw_rm_reschedule_success(void *_job_id);    
void gw_rm_reschedule_failed (void *_job_id);
    
void gw_rm_stop_success(void *_job_id);    
void gw_rm_stop_failed (void *_job_id);
    
void gw_rm_resume_success(void *_job_id);
void gw_rm_resume_failed (void *_job_id);

void gw_rm_wait_remove_anys(int client_socket);
void gw_rm_wait_success(void *_job_id);    
void gw_rm_wait_failed (void *_job_id);

void gw_rm_job_status(int client_socket, int job_id);
void gw_rm_job_pool_status(int client_socket);

void gw_rm_host_status(int client_socket, int host_id);
void gw_rm_host_pool_status(int client_socket);

void gw_rm_job_match(int client_socket, int job_id);
void gw_rm_array_match(int client_socket, int array_id);

void gw_rm_job_history(int client_socket, int job_id);

void gw_rm_user_pool(int client_socket);


/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

#endif
