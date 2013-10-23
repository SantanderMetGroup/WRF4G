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

#ifndef _GW_EM_MAD_H
#define _GW_EM_MAD_H

#include <sys/types.h>
#include <unistd.h>
#include "gw_common.h"
#include "gw_conf.h"

/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/

/* Type definition in gw_job.h */
struct gw_job_s;

typedef char* (*gw_rsl_function_t) (struct gw_job_s *);

typedef struct gw_em_mad_s
{
    char *name;
    char *executable;
    char *args;
    char *mode;
    char *owner;
    char *proxy_path;

    int  mad_em_pipe;
    int  em_mad_pipe;

    pid_t pid;
    
    gw_rsl_function_t wrapper_rsl;
    gw_rsl_function_t pre_wrapper_rsl;

} gw_em_mad_t;


/* -------------------------------------------------------------------------- */

int gw_em_mad_init(gw_em_mad_t * em_mad, 
                   const char *  exe,
                   const char *  name,
                   const char *  args,
                   const char *  mode,
                   const char *  owner);
                        
void gw_em_mad_submit(gw_em_mad_t *em_mad, int jid, char *rm_contact, char *rsl);

void gw_em_mad_recover(gw_em_mad_t *em_mad, int jid, char *job_contact);

void gw_em_mad_cancel(gw_em_mad_t *em_mad, int jid);

void gw_em_mad_poll(gw_em_mad_t *em_mad, int jid);

void gw_em_mad_finalize(gw_em_mad_t *em_mad);

int  gw_em_mad_reload (gw_em_mad_t *em_mad);

/* ---------------------------------------------------------------------------*/

#endif
