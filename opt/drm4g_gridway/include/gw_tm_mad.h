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

#ifndef _GW_TM_MAD_H
#define _GW_TM_MAD_H

#include <sys/types.h>
#include <unistd.h>
#include "gw_common.h"
#include "gw_conf.h"

/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/

typedef struct gw_tm_mad_s
{
    char *name;
    char *executable;
    char *argument;
    char *owner;
    
    char *url;

    int  mad_tm_pipe;
    int  tm_mad_pipe;

    pid_t pid;
    
} gw_tm_mad_t;

/* ---------------------------------------------------------------------------*/
    
int gw_tm_mad_init(gw_tm_mad_t *tm_mad,
                   const char *exe,
                   const char *name,
		           const char *args,
		           const char *owner);

void gw_tm_mad_mkdir    (gw_tm_mad_t *mad, int xfr_id, const char *dir);

void gw_tm_mad_rmdir    (gw_tm_mad_t *mad, int xfr_id, const char *dir);

void gw_tm_mad_cp(gw_tm_mad_t * tm_mad, 
                  int           xfr_id, 
                  int           cp_xfr_id,
                  char          modex, 
                  const char *  src, 
                  const char *  dst);
                  
void gw_tm_mad_start    (gw_tm_mad_t *mad, int xfr_id);

void gw_tm_mad_end      (gw_tm_mad_t *mad, int xfr_id);

void gw_tm_mad_exists   (gw_tm_mad_t *mad, int xfr_id, const char *dir);

void gw_tm_mad_finalize (gw_tm_mad_t *mad);

int gw_tm_mad_reload (gw_tm_mad_t *tm_mad);

/* ---------------------------------------------------------------------------*/

#endif
