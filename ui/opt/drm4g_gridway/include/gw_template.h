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

#ifndef _GW_TEMPLATE_H
#define _GW_TEMPLATE_H

#define GW_JT_PATH  512
#define GW_JT_STR   256
#define GW_JT_FILES 20
#define GW_JT_DEPS  50

#include "gw_common.h"

#include <time.h>

/* -------------------------------------------------------------------------- */

typedef enum {
    GW_JOB_TYPE_SINGLE,
    GW_JOB_TYPE_MULTIPLE,
    GW_JOB_TYPE_MPI
} gw_jobtype_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_template_s
{
    char name[GW_JT_STR];

    char file[GW_JT_PATH];
    char job_home[GW_JT_PATH];
    char user_home[GW_JT_PATH];

	char executable[GW_JT_STR];
	char arguments[GW_JT_STR];
	
	char pre_wrapper[GW_JT_STR];
	char pre_wrapper_arguments[GW_JT_STR];
	
	int  num_input_files;
	char input_files[GW_JT_FILES][2][GW_JT_STR];

	int  num_output_files;
	char output_files[GW_JT_FILES][2][GW_JT_STR];

	int  num_restart_files;
	char restart_files[GW_JT_FILES][GW_JT_STR];
	
	int  num_env;
	char environment[GW_JT_FILES][2][GW_JT_STR];
	
	char stdin_file[GW_JT_STR];
	char stdout_file[GW_JT_STR];
	char stderr_file[GW_JT_STR];
	
	char requirements[GW_JT_STR];
	char rank[GW_JT_STR];
	
	time_t rescheduling_interval;
	time_t rescheduling_threshold;
	
    time_t checkpoint_interval;
    char   checkpoint_url[GW_JT_STR];    

	time_t suspension_timeout;
	int cpuload_threshold;

	gw_boolean_t reschedule_on_failure;
	int number_of_retries;

	char wrapper[GW_JT_STR];
	char monitor[GW_JT_STR];
	
	int  job_deps[GW_JT_DEPS];

    int type;
    int np;

    time_t deadline;
} gw_template_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef enum {
	CPULOAD_THRESHOLD,
	EXECUTABLE,
	ARGUMENTS,
	ENVIRONMENT,
	INPUT_FILES,
	NUMBER_OF_RETRIES,
	OUTPUT_FILES,
	PRE_WRAPPER,
	PRE_WRAPPER_ARGUMENTS,
	RANK,
	REQUIREMENTS,
	RESCHEDULE_ON_FAILURE,
	RESCHEDULING_INTERVAL,
	RESCHEDULING_THRESHOLD,
	RESTART_FILES,
	STDERR_FILE,
	STDIN_FILE,
	STDOUT_FILE,
	SUSPENSION_TIMEOUT,
	WRAPPER,
    MONITOR,
    CHECKPOINT_INTERVAL,
    CHECKPOINT_URL,
    JOB_DEPENDENCIES,
    TYPE,
    NP,
    DEADLINE,
    NAME
} gw_template_var_t;
	              
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int gw_template_init(gw_template_t *jt, const char *jt_file);

int gw_template_parser(gw_template_t *jt);

void gw_template_print(gw_template_t *jt);

char *gw_template_jobtype_string(gw_jobtype_t type);

char *gw_template_deadline_string(time_t deadline);


/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif
