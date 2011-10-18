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

#ifndef _GW_HOST_H
#define _GW_HOST_H

#include <sys/times.h>
#include <stdio.h>
#include <pthread.h>
#include "gw_common.h"


#define GW_HOST_MAX_QUEUES       67
//Double the GW_HOST_MAX_QUEUES for 2 variables
#define GW_HOST_MAX_GENVARS      134
#define GW_HOST_CPU_FREE_LIMIT   50

#define GW_HOST_MAX_PRIORITY       99
#define GW_HOST_MIN_PRIORITY       00

#define GW_HOST_COMMAND_XML "gwhost"
//  Next size corresponds to the maximum 6+1+JOB_ID="..." (don't expect more than 99 million jobs)
#define GW_HOST_COMMAND_OPEN_SIZE_XML 24

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_genvar_int_s 
{
    char *name;
    int  value;
} gw_genvar_int_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_genvar_str_s 
{
    char *name;
    char *value;
} gw_genvar_str_t;

/* -------------------------------------------------------------------------- */

typedef enum 
{
   GW_HOST_STATE_INIT,
   GW_HOST_STATE_UNKNOWN,
   GW_HOST_STATE_DISCOVERED,
   GW_HOST_STATE_MONITORING,
   GW_HOST_STATE_MONITORED,
   GW_HOST_STATE_LIMIT
} gw_host_state_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_host_s 
{
    pthread_mutex_t  mutex;

    char *em_mad;
    char *tm_mad;
    char *im_mad; 

    int  used_slots;
    int  running_jobs;

    int  host_id;
    int  fixed_priority;

    char *hostname;
    char *arch;
    char *os_name;
    char *os_version;

    char *cpu_model;
    int   cpu_mhz;
    int   cpu_free;
    int   cpu_smp;
    int   nodecount;

    int  size_mem_mb;
    int  free_mem_mb;
    int  size_disk_mb;
    int  free_disk_mb;

    char *fork_name;
    char *lrms_name;
    char *lrms_type;

    char *queue_name[GW_HOST_MAX_QUEUES];
    int   queue_nodecount[GW_HOST_MAX_QUEUES];
    int   queue_freenodecount[GW_HOST_MAX_QUEUES];
    int   queue_maxtime[GW_HOST_MAX_QUEUES];
    int   queue_maxcputime[GW_HOST_MAX_QUEUES];
    int   queue_maxcount[GW_HOST_MAX_QUEUES];
    int   queue_maxrunningjobs[GW_HOST_MAX_QUEUES];
    int   queue_maxjobsinqueue[GW_HOST_MAX_QUEUES];
    char *queue_status[GW_HOST_MAX_QUEUES];
    char *queue_dispatchtype[GW_HOST_MAX_QUEUES];
    char *queue_priority[GW_HOST_MAX_QUEUES];
    
    gw_genvar_int_t genvar_int[GW_HOST_MAX_GENVARS];
    gw_genvar_str_t genvar_str[GW_HOST_MAX_GENVARS];
    
    gw_host_state_t state;
    
    time_t last_monitoring_time;
} gw_host_t;

/* -------------------------------------------------------------------------- */

typedef enum 
{
    HOSTNAME,
    ARCH,
    OS_NAME,
    OS_VERSION,
    CPU_MODEL,
    CPU_MHZ,
    CPU_FREE,
    CPU_SMP,
    NODECOUNT,
    SIZE_MEM_MB,
    FREE_MEM_MB,
    SIZE_DISK_MB,
    FREE_DISK_MB,
    FORK_NAME,
    LRMS_NAME,
    LRMS_TYPE,
    QUEUE_NAME,
    QUEUE_NODECOUNT,
    QUEUE_FREENODECOUNT,
    QUEUE_MAXTIME,
    QUEUE_MAXCPUTIME,
    QUEUE_MAXCOUNT,
    QUEUE_MAXRUNNINGJOBS,
    QUEUE_MAXJOBSINQUEUE,
    QUEUE_STATUS,
    QUEUE_DISPATCHTYPE,
    QUEUE_PRIORITY,
    GENERIC
} gw_host_var_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_host_init(gw_host_t * host, 
                  char *      hostname, 
                  int         host_id, 
                  int         fixed_priority,
                  char *      em_mad, 
                  char *      tm_mad, 
                  char *      im_mad);

void gw_host_destroy(gw_host_t *host);

void gw_host_update(int host_id, char *attrs);

void gw_host_clear_dynamic_info(int host_id);

void gw_host_print(FILE *fd, gw_host_t *host);

/*----------------------------------------------------------------------------*/

void gw_host_dec_rjobs(gw_host_t *host);

void gw_host_dec_uslots(gw_host_t *host, int slots);

void gw_host_dec_slots(gw_host_t *host, int slots);

void gw_host_inc_rjobs(gw_host_t *host);

void gw_host_inc_uslots(gw_host_t *host, int slots);

void gw_host_inc_slots(gw_host_t *host, int slots);

void gw_host_inc_slots_nb(gw_host_t *host, int slots);

void gw_host_inc_rjobs_nb(gw_host_t *host);

/* -------------------------------------------------------------------------- */

int          gw_host_update_attr(gw_host_t *host, char *attrs);
gw_boolean_t gw_host_check_reqs(gw_host_t *host, int queue, char *reqs);
int          gw_host_compute_rank(gw_host_t *host, int queue, char *rank);
int          gw_host_client_check_syntax(gw_host_t *host, char *reqs, char *rank);
/* -------------------------------------------------------------------------- */

const char *gw_host_get_varname(gw_host_var_t var);

void gw_host_set_var_int(gw_host_var_t var, int index, int value, gw_host_t *host);
void gw_host_set_var_str(gw_host_var_t var, int index, char *value, gw_host_t *host);
void gw_host_set_var_null(gw_host_var_t var, int index, gw_host_t *host);

void gw_host_set_genvar_int(char *var, int index, int value, gw_host_t *host);
void gw_host_set_genvar_str(char *var, int index, char *value, gw_host_t *host);
void gw_host_set_genvar_null(char *var, int index, gw_host_t *host);

/* -------------------------------------------------------------------------- */

int   gw_host_get_var_int(gw_host_var_t var, int index, gw_host_t *host);
char *gw_host_get_var_str(gw_host_var_t var, int index, gw_host_t *host);

int   gw_host_get_genvar_int(char *var, int index, gw_host_t *host);
char *gw_host_get_genvar_str(char *var, int index, gw_host_t *host);

/* -------------------------------------------------------------------------- */

#endif
