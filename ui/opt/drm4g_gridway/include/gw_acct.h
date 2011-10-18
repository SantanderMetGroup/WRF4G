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

#ifndef GW_ACCT_H_
#define GW_ACCT_H_

#ifdef HAVE_LIBDB

#include <db.h>
#include "gw_rm_msg.h"
#include "gw_job.h"

#define GW_ACCT_COMMAND_XML "gwacct"
// Next size corresponds to the maximum 6+1+USERNAME="..."+1+HOSTNAME="..."
#define GW_ACCT_COMMAND_OPEN_SIZE_XML 29+GW_MSG_STRING_USER_AT_HOST
/* -------------------------------------------------------------------------- */

typedef struct gw_acct_key_s
{
	time_t start_time;
	int    job_id;	
	int    restarted;	
} gw_acct_key_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_acct_data_s
{
	char                  username[GW_MSG_STRING_SHORT];
	char                  hostname[GW_MSG_STRING_HOST];
	int                   rank;
    time_t                stats[GW_HISTORY_MAX_STATS];
    
    gw_migration_reason_t reason;
} gw_acct_data_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_acct_s
{
	char    name[GW_MSG_STRING_HOST];
	
	time_t	transfer;
	time_t  execution;
	time_t  suspension;
	
	int     tot;
	int     succ;
	int     user;
    int     susp;
    int     disc;
    int     self;
    int     perf;
    int     s_r;
    int     err;
    int     kill;
} gw_acct_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_acct_db_s
{
	DB_ENV *env_db;

	DB * acct_db;
	
	DB * uinx_db;
	DBC* ucursor;
	
	DB * hinx_db;
	DBC* hcursor;
	
} gw_acct_db_t;

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

int  gw_acct_db_open(gw_boolean_t server);
void gw_acct_db_close();

void gw_acct_db_error(const DB_ENV *dbenv, const char *prefix, const char *msg);

int gw_acct_uidx_cb(DB * uidx_db, const DBT *key, const DBT *data, DBT *result);
int gw_acct_hidx_cb(DB * hidx_db, const DBT *key, const DBT *data, DBT *result);

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */

void gw_acct_write_job(gw_job_t *job);

int gw_acct_join_search(const char *hostname, 
                        const char *username,
                        gw_acct_t *accts,
                        time_t from_time);
                        
int gw_acct_join_search_by_user(const char * username, 
                                gw_acct_t *** accts,
                                int *        nrecs,
                                time_t from_time);
                                
int gw_acct_join_search_by_host(const char * hostname, 
                                gw_acct_t *** accts,
                                int *        nrecs,
                                time_t from_time);  
                                
int gw_acct_join_search_by_host_and_user(const char * hostname, 
										 const char * username,
										 gw_acct_t *** accts,
                                         int *        nrecs,
                                         time_t from_time);

#endif

#endif /*GW_ACCT_H_*/
