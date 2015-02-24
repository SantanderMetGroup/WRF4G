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

#ifndef _GW_UM_H
#define _GW_UM_H

#include <pthread.h>
#include "gw_action.h"
#include "gw_user_pool.h"
#include "gw_conf.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_iu_s {

    pthread_t       thread_id; 
    pthread_mutex_t mutex;
    
    gw_am_t am;        
    
} gw_um_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_um_t* gw_um_init();
                                        
void gw_um_finalize();

void gw_um_start();

void gw_um_timer(void *_null);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif
