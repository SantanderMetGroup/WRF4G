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

#ifndef _GW_JOB_H
#define _GW_JOB_H

#include <stdio.h>
#include <limits.h>

#include "gw_history.h"
#include "gw_host.h"
#include "gw_job_template.h"
#include "gw_common.h"
#include "gw_xfr_files.h"
#include "gw_rm_msg.h"

#define GW_PS_COMMAND_XML "gwps"
// Next size corresponds to the maximum 4+1+USERNAME="..."+1+HOSTNAME="..."
#define GW_PS_COMMAND_OPEN_SIZE_XML 27+GW_MSG_STRING_USER_AT_HOST

#define GW_RSL_ENVIRONMENT_LENGTH 16386

#define GW_JOB_MAX_PRIORITY       20
#define GW_JOB_MIN_PRIORITY       00
#define GW_JOB_DEFAULT_PRIORITY   INT_MIN

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef enum {
   GW_EM_STATE_INIT,
   GW_EM_STATE_PENDING,
   GW_EM_STATE_SUSPENDED,
   GW_EM_STATE_ACTIVE,
   GW_EM_STATE_FAILED,
   GW_EM_STATE_DONE,
   GW_EM_STATE_HARD_KILL,
   GW_EM_STATE_LIMIT
} gw_em_state_t;

/* -------------------------------------------------------------------------- */

typedef enum {
   GW_TM_STATE_INIT,    
   GW_TM_STATE_PROLOG,
   GW_TM_STATE_PROLOG_FAILED,
   GW_TM_STATE_EPILOG,
   GW_TM_STATE_EPILOG_FAILED,   
   GW_TM_STATE_CHECKPOINT,
   GW_TM_STATE_CHECKPOINT_CANCEL,
   GW_TM_STATE_HARD_KILL,   
   GW_TM_STATE_LIMIT
} gw_tm_state_t;

/* -------------------------------------------------------------------------- */

typedef enum {
   GW_JOB_STATE_INIT,       
   GW_JOB_STATE_PENDING,
   GW_JOB_STATE_HOLD,
   GW_JOB_STATE_PROLOG,
   GW_JOB_STATE_PRE_WRAPPER,
   GW_JOB_STATE_WRAPPER,
   GW_JOB_STATE_EPILOG,
   GW_JOB_STATE_EPILOG_STD,
   GW_JOB_STATE_EPILOG_RESTART,
   GW_JOB_STATE_EPILOG_FAIL,
   GW_JOB_STATE_STOP_CANCEL,
   GW_JOB_STATE_STOP_EPILOG,
   GW_JOB_STATE_STOPPED,
   GW_JOB_STATE_KILL_CANCEL,
   GW_JOB_STATE_KILL_EPILOG,
   GW_JOB_STATE_MIGR_CANCEL,
   GW_JOB_STATE_MIGR_PROLOG,
   GW_JOB_STATE_MIGR_EPILOG,
   GW_JOB_STATE_ZOMBIE,
   GW_JOB_STATE_FAILED,
   GW_JOB_STATE_LIMIT      
} gw_job_state_t;

/* -------------------------------------------------------------------------- */

/* Copied from globus_gram_protocol_constants.h */

typedef enum
{
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_PENDING=1,
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_ACTIVE=2,
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_FAILED=4,
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_DONE=8,
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_SUSPENDED=16,
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_UNSUBMITTED=32,
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_STAGE_IN=64,
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_STAGE_OUT=128,
    GLOBUS_GRAM_PROTOCOL_JOB_STATE_ALL=0xFFFFF
}
globus_gram_protocol_job_state_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_job_s
{
    pthread_mutex_t  mutex;

    char * owner;
    char * directory;
    char * log_file;
    char * env_file;

    int user_id;
    	
    gw_job_template_t template;
    
    int id;
    int array_id;
    int task_id;
    int total_tasks;
    
    int pstart;
    int pinc;
    
    int fixed_priority;

    int type_dep;
    
    gw_em_state_t  em_state;
    gw_tm_state_t  tm_state;
    gw_job_state_t job_state;
    
    time_t start_time;
    time_t exit_time;

    time_t next_poll_time;
    time_t last_rescheduling_time;
    time_t last_checkpoint_time;
    
    int exit_code;

    int restarted;
    int client_waiting;
    
    gw_xfrs_t xfrs;
    gw_xfrs_t chk_xfrs;    
        
    gw_history_t *history;
    
    gw_boolean_t reschedule;

    char  * max_cpu_time;
    int max_time;
    char * max_walltime;
    int max_memory;
    int min_memory;
    int processes_per_node;
    int nodes;
} gw_job_t;


/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */
/*              Function Prototypes to init/destroy job structure             */
/* -------------------------------------------------------------------------- */

int gw_job_init(gw_job_t *job, int job_id);
int gw_job_fill(gw_job_t *job, const gw_msg_submit_t *msg_submit);
int gw_job_recover(gw_job_t *job);
int gw_job_recover_state_transition(gw_job_t *job,
        gw_job_state_t previous_job_state, gw_job_state_t job_state,
        time_t timestamp, FILE *history_file);
int gw_job_recover_last_state_transition(gw_job_t *job,
        gw_job_state_t previous_job_state, gw_job_state_t job_state,
        time_t timestamp);
int gw_job_recover_history_record(FILE *history_file, gw_job_t *job);
int gw_job_recover_job_contact(gw_job_t *job);
void gw_job_destroy (gw_job_t *job);

char * gw_job_recover_get_contact(gw_job_t *job);

/* -------------------------------------------------------------------------- */
/*      Function Prototypes to perform persistent job state transitions       */
/* -------------------------------------------------------------------------- */

void gw_job_set_state (gw_job_t *job, gw_job_state_t job_state,
        gw_boolean_t recover);
gw_job_state_t gw_job_get_state (gw_job_t *job);
char *gw_job_get_state_name (gw_job_state_t job_state);
gw_job_state_t gw_job_get_state_code(char *job_state);
globus_gram_protocol_job_state_t gw_job_get_gram_state(
        gw_job_state_t job_state);

/* -------------------------------------------------------------------------- */
/*    Function Prototypes to handle dynamic job template variables            */
/* -------------------------------------------------------------------------- */

char *gw_job_substitute (const char *var, gw_job_t *job);

/* -------------------------------------------------------------------------- */
/*    Function Prototypes to generate dynamic job environment                 */
/* -------------------------------------------------------------------------- */

int gw_job_is_wrapper_based(gw_job_t *job);

int    gw_job_environment(gw_job_t *job);
char * gw_job_rsl_environment(gw_job_t *job);
char * gw_job_rsl2_environment(gw_job_t *job);
char * gw_job_rsl2_extensions(gw_job_t *job);

/* -------------------------------------------------------------------------- */
/*    Function Prototypes to print job information & logging                  */
/* -------------------------------------------------------------------------- */

void gw_job_print (gw_job_t *job,const char* module, const char mode, const char *str_format,...);
void gw_job_print_history(gw_job_t *job);

char *gw_job_state_string(gw_job_state_t state); 
char *gw_em_state_string(gw_em_state_t state);

#endif
