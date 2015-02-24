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


#ifndef GW_DRMAA_JT_H_
#define GW_DRMAA_JT_H_

#include "drmaa.h"
#include "gw_common.h"

#include <pthread.h>

/* ************************************************************************** *
 * ************************************************************************** *
 *     JOB TEMPLATE MODULE FUNCTION IMPLEMENTATION (PRIVATE INTERFACE)        *
 *     ---------------------------------------------------------------        *
 *                                                                            *
 * DO NOT INCLUDE THIS FILE IN YOUR DRMAA PROGRAMS!!                          *
 * ************************************************************************** *
 * ************************************************************************** */

struct drmaa_job_template_s 
{   
	pthread_mutex_t mutex;
	
	char *  executable;
	
	int     num_arg;
	char ** arguments;
	
	int     num_env;
	char ** environment;

	int     num_ifiles;
	char ** input_files;
	
	int     num_ofiles;
	char ** output_files;

	int     num_rfiles;
	char ** restart_files;
  
  	char *  stdin_jt;
  	char *  stdout_jt;
  	char *  stderr_jt;
  	
  	char * rank;
  	char * requirements;

  	char * reschedule_on_failure;
  	char * number_of_retries;
  	
  	char * job_name;
  	char * job_wd;
  	
  	char * js_state;
    
    char * deadline_time;
    char * priority;
    
    char * type;
    char * np;
  	
/* Not relevant for the current GridWay implementation, will be ignored */
	
	char * block_email;
	char * hlimit;
	char * slimit;
	char * category;
	char * join_files;
	char * native;
	char * start;
	char * transfer;
	char * wct_hlimit;
	char * wct_slimit;

	int     num_v_email;
	char ** v_email;

};

/* -------------------------------------------------------------------------- */
 
typedef enum {
	D_EXECUTABLE,
	D_ARGUMENTS,
	D_ENVIRONMENT,
	D_INPUT_FILES,
	D_OUTPUT_FILES,
	D_RESTART_FILES,
	D_STDERR_FILE,
	D_STDIN_FILE,
	D_STDOUT_FILE,
	D_RANK,
	D_REQUIREMENTS,
	D_RESCHEDULE_ON_FAILURE,
	D_NUMBER_OF_RETRIES,
	D_JOB_NAME,
	D_JOB_WD,
	D_JS_STATE,
    D_DEADLINE,
    D_TYPE,
    D_NP,
    NONE
} drmaa_gw_template_names_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int  drmaa_gw_jt_init    (drmaa_job_template_t ** jt);
void drmaa_gw_jt_destroy (drmaa_job_template_t *  jt);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void drmaa_gw_jt_get_ptr (drmaa_job_template_t * jt,
                          const char *           name,
                          void **                val,
                          int  **                num_val);
                          
int  drmaa_gw_jt_set_vval(drmaa_job_template_t * jt, 
                          const char *           var_name,
                          const char **          values);
                          
int  drmaa_gw_jt_set_val (drmaa_job_template_t * jt, 
                          const char *           var_name,
                          const char *           values);

char * drmaa_gw_jt_parse (const char * value);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

char * drmaa_gw_jt_file  (const drmaa_job_template_t *jt);


int    gw_drmaa_jt_write (drmaa_job_template_t * jt);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
                          
#endif /*GW_DRMAA_JT_H_*/
