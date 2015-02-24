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

#ifndef _GW_JOB_TEMPLATE_H
#define _GW_JOB_TEMPLATE_H

#include <time.h>
#include <stdio.h>
#include "gw_common.h"
#include "gw_template.h"
#include "gw_conf.h"
#include "gw_rm_msg.h"

#define GW_LOCAL_FILE  0
#define GW_REMOTE_FILE 1

#define GW_ENV_VAR     0
#define GW_ENV_VAL     1

#define GW_TEMPLATE_ARRAY_SIZE 20

/* -------------------------------------------------------------------------- */

typedef struct gw_job_template_s
{
    char * name;

    char * file;
    char * job_home;
    char * user_home;

	char * executable;
	char * arguments;
	
	char * pre_wrapper;
	char * pre_wrapper_arguments;
	
	int  num_input_files;
	char *** input_files;

	int  num_output_files;
	char *** output_files;

	int  num_restart_files;
	char ** restart_files;
	
	int  num_env;
	char *** environment;
	
	char * stdin_file;
	char * stdout_file;
	char * stderr_file;
	
	char * requirements;
	char * rank;
	
	time_t rescheduling_interval;
	time_t rescheduling_threshold;
	
    time_t checkpoint_interval;
    char *checkpoint_url;    

	time_t suspension_timeout;
	int cpuload_threshold;

	gw_boolean_t reschedule_on_failure;
	int number_of_retries;

	char * wrapper;
	char * monitor;
	
	int * job_deps;

    int type;
    int np;	

    time_t deadline;
} gw_job_template_t;
      
/* -------------------------------------------------------------------------- */

void gw_job_template_init (gw_job_template_t *jt, const gw_template_t *ct);

void gw_job_template_destroy(gw_job_template_t *job_template );

void gw_job_template_print(FILE *fd, gw_job_template_t *job_template);

void gw_job_template_to_file(FILE *fd, gw_job_template_t *job_template);

#endif
