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

#ifndef _GW_IM_MAD_H
#define _GW_IM_MAD_H

#include <sys/types.h>
#include <unistd.h>
#include "gw_common.h"
#include "gw_conf.h"

/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/

extern int active_im_mads; /* To control the number of active IM MADs */

/* ---------------------------------------------------------------------------*/
/* ---------------------------------------------------------------------------*/

typedef enum {
   GW_IM_MAD_STATE_IDLE,
   GW_IM_MAD_STATE_DISCOVERING,
   GW_IM_MAD_STATE_LIMIT
} gw_im_mad_state_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */


typedef struct gw_im_mad_s
{
    char *name;
    char *executable;
    char *argument;

    char *em_mad_name;
    char *tm_mad_name;

    int  mad_im_pipe;
    int  im_mad_pipe;

    pid_t pid;
    
    gw_im_mad_state_t state;
} gw_im_mad_t;

/* ---------------------------------------------------------------------------*/
    
int gw_im_mad_init(gw_im_mad_t *im_mad, const char *exe, const char *name, 
		const char *args, char *em_mad_name, char *tm_mad_name);

                        
void gw_im_mad_discover(gw_im_mad_t *im_mad);

void gw_im_mad_monitor(gw_im_mad_t *im_mad, int hid, char *hostname);

void gw_im_mad_finalize(gw_im_mad_t *im_mad);

int gw_im_mad_reload (gw_im_mad_t *im_mad);

/* ---------------------------------------------------------------------------*/

#endif
