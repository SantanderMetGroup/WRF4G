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

#ifndef _GW_LOG_H
#define _GW_LOG_H

#include <pthread.h>

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_log_s
{
    pthread_mutex_t  mutex;

    char *log_file;         

} gw_log_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_log_init(const char *log_file);
void gw_log_destroy();

void gw_log_print(const char *module, const char type, const char *str_format,...);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

#endif
