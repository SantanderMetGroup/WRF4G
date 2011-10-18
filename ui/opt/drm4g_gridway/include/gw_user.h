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

#ifndef _GW_USER_H
#define _GW_USER_H

#include <pthread.h>
#include "gw_common.h"
#include "gw_conf.h"
#include "gw_em_mad.h"
#include "gw_tm_mad.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_user_s
{
    char * name;
    char * proxy_path;
    char * dn;
	
	int    active_jobs;
	int    running_jobs;
	time_t idle;
	
	int         em_mads;
	gw_em_mad_t em_mad[GW_MAX_MADS];
    
    int         tm_mads;
    gw_tm_mad_t tm_mad[GW_MAX_MADS];
    
} gw_user_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int   gw_user_init(gw_user_t *user, const char *name, const char *proxy_path);

void  gw_user_destroy(gw_user_t *user);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int gw_em_register_mad(gw_user_t *  user,
                       const char * executable, 
                       const char * name,
                       const char * args,
                       const char * mode);

gw_em_mad_t* gw_em_get_mad(gw_user_t * user, const char *name);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int gw_tm_register_mad(gw_user_t *  user,
                       const char * executable, 
                       const char * name, 
                       const char * arg);

gw_tm_mad_t* gw_tm_get_mad(gw_user_t * user, const char *name);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif
