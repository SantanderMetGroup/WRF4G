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

#ifndef  _GW_ACTION_H
#define  _GW_ACTION_H

#include <pthread.h>

/* ************************************************************************** *
 * ************************************************************************** *
 *                              MODULE CONSTANTS                              *
 * ************************************************************************** *
 * ************************************************************************** */
 
#define GW_ACTION_ID_LENGTH     40
#define GW_ACTION_NUMBER        40

#define GW_ACTION_FINALIZE "GW_ACTION_FINALIZE"
#define GW_ACTION_TIMER    "GW_ACTION_TIMER"

#define GW_ACTION_THREADED     0
#define GW_ACTION_SEQUENTIAL   1

/* ************************************************************************** *
 * ************************************************************************** *
 *                             MODULE DATA TYPES                              *
 * ************************************************************************** *
 * ************************************************************************** */

typedef void (*gw_action_handler_t) (void * action_arg);

typedef struct gw_action_s gw_action_t;

typedef struct gw_action_list_s gw_action_list_t;

typedef struct gw_am_s gw_am_t;

/* -------------------------------------------------------------------------- *
 * -------------------------------------------------------------------------- */

struct gw_action_s
{
    int                  threaded;
    char                 action_id[GW_ACTION_ID_LENGTH];
    gw_action_handler_t  action_handler;
};

/* -------------------------------------------------------------------------- *
 * -------------------------------------------------------------------------- */

struct gw_action_list_s
{
    int              action_idx;
    void             *action_arg;
    gw_action_list_t *next_action;
};

/* -------------------------------------------------------------------------- *
 * -------------------------------------------------------------------------- */

struct gw_am_s
{
    pthread_mutex_t mutex;
    pthread_cond_t  cond;

    int              pending_actions;
    gw_action_list_t *action_list;

    int         registered_actions;
    gw_action_t action_rgs[GW_ACTION_NUMBER];
};

/* ************************************************************************** *
 * ************************************************************************** *
 *               MODULE FUNCTION PROTOTYPES (PUBLIC INTERFACE)                *
 * ************************************************************************** *
 * ************************************************************************** */


int gw_am_init   (gw_am_t  *gw_action_mngr);
int gw_am_destroy(gw_am_t  *gw_action_mngr);

/* -------------------------------------------------------------------------- *
 * -------------------------------------------------------------------------- */
 
int  gw_am_register(const char *action_id, int threaded, 
            gw_action_handler_t action_handler, gw_am_t *action_mngr);
            
void gw_am_loop(gw_am_t *action_mngr, time_t  wait_timeout, void *timer_args);

/* -------------------------------------------------------------------------- *
 * -------------------------------------------------------------------------- */ 

int  gw_am_trigger(gw_am_t *action_mngr,const char *action_id,void *action_arg);

#endif
