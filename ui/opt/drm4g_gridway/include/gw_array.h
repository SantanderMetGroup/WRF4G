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

#ifndef _GW_ARRAY_H
#define _GW_ARRAY_H

#include <pthread.h>
#include "gw_common.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_array_s
{
    pthread_mutex_t mutex;            

    int array_id;
    int last_task_id;                     

    int number_of_tasks;              

    int freed_tasks;                  
    int *job_ids;                        

} gw_array_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int gw_array_init(gw_array_t *array, int number_of_tasks, int array_id);

void gw_array_destroy(gw_array_t *array);

int  gw_array_add_task(gw_array_t *array, int job_id);

int  gw_array_del_task(gw_array_t *array, int task_id);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif
