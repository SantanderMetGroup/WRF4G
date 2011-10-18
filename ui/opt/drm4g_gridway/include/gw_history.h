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

#ifndef _GW_HISTORY_H
#define _GW_HISTORY_H

#include "gw_em_mad.h"
#include "gw_tm_mad.h"
#include "gw_host.h"

#include <sys/times.h>
#include <stdio.h>

#define GW_HISTORY_COMMAND_XML "gwhistory"
#define GW_HISTORY_COMMAND_OPEN_SIZE_XML 24
/* -------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------*/

#define GW_HISTORY_MAX_STATS 16

#define START_TIME              0
#define EXIT_TIME               1

#define SUSPENSION_TIME         2
#define LAST_SUSPENSION_TIME    3

#define ACTIVE_TIME             4
#define LAST_ACTIVE_TIME        5

#define PROLOG_START_TIME       6
#define PROLOG_EXIT_TIME        7

#define WRAPPER_START_TIME      8
#define WRAPPER_EXIT_TIME       9

#define PRE_WRAPPER_START_TIME  10
#define PRE_WRAPPER_EXIT_TIME   11

#define EPILOG_START_TIME       12
#define EPILOG_EXIT_TIME        13

#define MIGRATION_START_TIME    14
#define MIGRATION_EXIT_TIME     15

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */


typedef enum {
    GW_REASON_NONE,
    GW_REASON_USER_REQUESTED,
    GW_REASON_SUSPENSION_TIME,
    GW_REASON_RESCHEDULING_TIMEOUT,
    GW_REASON_SELF_MIGRATION,
    GW_REASON_PERFORMANCE,
    GW_REASON_STOP_RESUME,
    GW_REASON_EXECUTION_ERROR,
    GW_REASON_KILL,
    GW_REASON_LIMIT
} gw_migration_reason_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_history_xfr_s 
{
    int tries;

    char *src_url;
    char *dst_url;
    char *alt_src_url;

    char mode;
	
    gw_boolean_t done;
    gw_boolean_t success;
} gw_history_xfr_t;

/* -------------------------------------------------------------------------- */

struct gw_history_s;

typedef struct gw_history_s 
{
    gw_host_t *host;
    int rank;
    time_t stats[GW_HISTORY_MAX_STATS];
       
    gw_tm_mad_t *tm_mad;
    char *rdir;

    gw_em_mad_t *em_mad;     
    char *em_rc;
    char *em_fork_rc;
    char *queue;
    	
    int tries;
	int failed_polls;
	int counter;
	
    gw_migration_reason_t reason;

    struct gw_history_s * next;
} gw_history_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_job_history_init(gw_history_t **job_history);

int  gw_job_history_destroy(gw_history_t **job_history);

int gw_job_history_add(gw_history_t ** job_history, 
        gw_host_t  *host,
        int rank, 
        char *queue, 
        const char *fork,
        const char *lrms, 
        const char *lrms_name, 
        const char *owner,
        const char *job_home, 
        int jid,
        int uid,
        gw_boolean_t recover);

int  gw_job_history_number_of_records(gw_history_t *job_history);

int  gw_job_history_is_in_records(gw_history_t *job_history, char *hostname);

void gw_job_history_print(FILE *fd, gw_history_t *job_history);

time_t gw_job_history_get_prolog_time(gw_history_t *history);
time_t gw_job_history_get_wrapper_time(gw_history_t *history);
time_t gw_job_history_get_epilog_time(gw_history_t *history);
time_t gw_job_history_get_migration_time(gw_history_t *history);
time_t gw_job_history_get_pre_wrapper_time(gw_history_t *history);

char *gw_reason_string(gw_migration_reason_t reason);

/* -------------------------------------------------------------------------- */

#endif
