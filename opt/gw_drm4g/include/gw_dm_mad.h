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

#ifndef _GW_DM_MAD_H
#define _GW_DM_MAD_H

#include <sys/types.h>
#include <unistd.h>
#include "gw_common.h"
#include "gw_conf.h"
#include "gw_history.h"

/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/

typedef struct gw_dm_mad_s
{
    char *name;
    char *executable;
    char *argument;

    char *em_mad_name;
    char *tm_mad_name;

    int  mad_dm_pipe;
    int  dm_mad_pipe;

    pid_t pid;
    
} gw_dm_mad_t;

/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/
    
int gw_dm_mad_init(gw_dm_mad_t *dm_mad, const char *exe, const char *name,
		const char *args);
		
/* ---------------------------------------------------------------------------*/
                        
void gw_dm_mad_schedule(gw_dm_mad_t *dm_mad);

/* ---------------------------------------------------------------------------*/

void gw_dm_mad_host_monitor(gw_dm_mad_t * dm_mad, 
                            int           hid, 
                            int           uslots, 
                            int           rjobs, 
                            char *        name);
                            
/* ---------------------------------------------------------------------------*/
                            
void gw_dm_mad_user_add(gw_dm_mad_t * dm_mad, 
                        int           uid, 
                        int           aslots, 
                        int           rslots, 
                        char *        name);
                        
void gw_dm_mad_user_del(gw_dm_mad_t * dm_mad, 
                        int           uid);

/* ---------------------------------------------------------------------------*/
                        
void gw_dm_mad_job_failed(gw_dm_mad_t *         dm_mad, 
                          int                   hid,
                          int                   uid,
                          gw_migration_reason_t reason);
                          
                          
void gw_dm_mad_job_del(gw_dm_mad_t *  dm_mad, 
                       int            jid);

void gw_dm_mad_task_del(gw_dm_mad_t * dm_mad, 
                        int           aid);
                          
void gw_dm_mad_job_success(gw_dm_mad_t * dm_mad, 
                           int           hid,
                           int           uid,
                           float         xfr,
                           float         sus,
                           float         exe);
                           
void gw_dm_mad_job_schedule(gw_dm_mad_t *         dm_mad, 
                            int                   jid,
                            int                   aid,
                            int                   uid,
                            gw_migration_reason_t reason);

/* ---------------------------------------------------------------------------*/

void gw_dm_mad_finalize(gw_dm_mad_t *dm_mad);

int gw_dm_mad_reload (gw_dm_mad_t *dm_mad);

/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/

#endif
