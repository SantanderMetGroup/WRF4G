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

#ifndef _GW_ARRAY_POOL_H
#define _GW_ARRAY_POOL_H

#include <pthread.h>
#include "gw_array.h"
#include "gw_common.h"
#include "gw_rm_msg.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_array_pool_s
{
    pthread_mutex_t mutex;            
                  
    int number_of_arrays;
    int last_array_id;   

    gw_array_t **pool;
    
} gw_array_pool_t;


/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_array_pool_t * gw_array_pool_init();

void gw_array_pool_finalize();

int gw_array_pool_array_allocate (const gw_msg_t * msg, 
                                  int              number_of_tasks, 
                                  int *            array_id);

void gw_array_pool_array_free (int array_id);

gw_array_t* gw_array_pool_get_array (int array_id, gw_boolean_t lock);

int gw_array_pool_get_number_tasks (int array_id);

int gw_array_pool_get_num_arrays();

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif
