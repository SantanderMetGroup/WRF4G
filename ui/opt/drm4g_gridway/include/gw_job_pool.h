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

#ifndef _GW_JOB_POOL_H
#define _GW_JOB_POOL_H

#include <pthread.h>
#include "gw_job.h"
#include "gw_em_mad.h"
#include "gw_dm_mad.h"
#include "gw_action.h"

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

typedef struct gw_job_pool_s
{
    pthread_mutex_t mutex;

    int number_of_jobs;
    int last_job_id;

    gw_job_t **pool;

} gw_job_pool_t;


typedef struct gw_job_dep_matrix_s
{
	pthread_mutex_t mutex;
	
	int **deps;
	
} gw_job_dep_matrix_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

gw_job_pool_t * gw_job_pool_init();

void gw_job_pool_finalize();

int gw_job_pool_allocate ();

int gw_job_pool_allocate_by_id (int job_id);

void gw_job_pool_free (int job_id);

gw_job_t* gw_job_pool_get (int job_id, int lock);

int gw_job_pool_get_num_jobs();

int gw_job_pool_em_recover (gw_em_mad_t * em_mad);

int gw_job_pool_dm_recover (gw_dm_mad_t * em_mad);

void gw_job_pool_tm_recover (gw_am_t *dm_am);

void gw_job_pool_dep_check(int job_id);

void gw_job_pool_dep_set(int job_id, int *deps);

void gw_job_pool_dep_cp (const int * src, int **dst);

void gw_job_pool_dep_consistency();

#endif
