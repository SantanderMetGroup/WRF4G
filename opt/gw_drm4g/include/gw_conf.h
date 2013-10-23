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

#ifndef _GW_CONF_H
#define _GW_CONF_H

#include "gw_common.h"
#include "gw_sch_conf.h"
#include <time.h>

/* gwd defaults */

#define GW_NUMBER_OF_ARRAYS_DEFAULT 20
#define GW_NUMBER_OF_JOBS_DEFAULT   200
#define GW_NUMBER_OF_HOSTS_DEFAULT  500
#define GW_NUMBER_OF_USERS_DEFAULT  30

#define GW_SCHEDULING_INTERVAL_DEFAULT  30
#define GW_DISCOVERY_INTERVAL_DEFAULT   300
#define GW_MONITORING_INTERVAL_DEFAULT  180

#define GW_GWD_PORT_DEFAULT              6725
#define GW_MAX_NUMBER_OF_CLIENTS_DEFAULT 20

#define GW_MAX_IDLE_TIME                360

#define GW_MAX_JOBS_PER_SCHED           10
#define GW_MAX_JOBS_PER_HOST            10
#define GW_MAX_JOBS_PER_USER            20

/* job template defaults */

#define GW_CHECKPOINT_INTERVAL_DEFAULT  300
#define GW_RESCHEDULE_ON_FAILURE_DEFAULT GW_TRUE
#define GW_NUMBER_OF_RETRIES_DEFAULT     3

#define GW_SUSPENSION_TIMEOUT_DEFAULT 300
#define GW_CPULOAD_THRESHOLD_DEFAULT  70

#define GW_RESCHEDULING_INTERVAL_DEFAULT  0
#define GW_RESCHEDULING_THRESHOLD_DEFAULT 300

#define GW_POLL_INTERVAL_DEFAULT        300

#define GW_MAX_ACTIVE_IM_QUERIES_DEFAULT 10

#define GW_WRAPPER_DEFAULT "libexec/gw_wrapper.sh"

/* MADs */
#define GW_MAX_MADS   5

#define GW_MAD_IM_MAX 5
#define GW_MAD_EM_MAX 4
#define GW_MAD_TM_MAX 3
#define GW_MAD_DM_MAX 3
 
/* Indexes for IM MAD */ 
 
#define GW_MAD_IM_NAME_INDEX 0
#define GW_MAD_IM_PATH_INDEX 1
#define GW_MAD_IM_ARGS_INDEX 2
#define GW_MAD_IM_TM_INDEX   3
#define GW_MAD_IM_EM_INDEX   4
 
/* Indexes for EM MAD */ 
 
#define GW_MAD_EM_NAME_INDEX 0
#define GW_MAD_EM_PATH_INDEX 1
#define GW_MAD_EM_ARGS_INDEX 2
#define GW_MAD_EM_RSL_INDEX  3
 
/* Indexes for TM MAD */ 
 
#define GW_MAD_TM_NAME_INDEX 0
#define GW_MAD_TM_PATH_INDEX 1
#define GW_MAD_TM_ARGS_INDEX 2 

/* Indexes for DM MAD */ 

#define GW_MAD_DM_NAME_INDEX 0
#define GW_MAD_DM_PATH_INDEX 1
#define GW_MAD_DM_ARGS_INDEX 2

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

typedef struct gw_conf_s
{
    gw_boolean_t multiuser;
    gw_boolean_t dispose;
    
    char *conf_file;
    char *sch_conf_file;    
    char *template_default;
    char *gw_location;
    char *gw_globus_seg;
    char *gw_acct;
    
    char *gwadmin;
    
    int number_of_arrays;
    int number_of_jobs;
    int number_of_hosts;
    int number_of_users;
            
    time_t scheduling_interval;
    time_t poll_interval;
    time_t discovery_interval;
    time_t monitoring_interval;

    int max_active_im_queries;
    
    int  gwd_port;
    int  max_number_of_clients;
    
    char  ***im_mads;
    char  ***tm_mads;
    char  ***em_mads;
    char  **dm_mad;
    
    gw_sch_conf_t sch_conf;
} gw_conf_t;

/* ------------------------------------------------------------------------- */

extern gw_conf_t gw_conf;

/* ------------------------------------------------------------------------- */

typedef enum {
    GWD_PORT,
    MAX_NUMBER_OF_CLIENTS,
    NUMBER_OF_ARRAYS,
    NUMBER_OF_JOBS,
    NUMBER_OF_HOSTS,
    NUMBER_OF_USERS,
    JOBS_PER_SCHED,
    JOBS_PER_HOST,
    JOBS_PER_USER,
    SCHEDULING_INTERVAL,
    DISCOVERY_INTERVAL,
    MONITORING_INTERVAL,
    POLL_INTERVAL,
    MAX_ACTIVE_IM_QUERIES,
    IM_MAD,
    TM_MAD,
    EM_MAD,
    DM_SCHED,
} gw_conf_var_t;

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

int gw_conf_init (gw_boolean_t multiuser, gw_boolean_t dispose);

int gw_conf_lex_parser ();

int gw_loadconf ();

#endif
