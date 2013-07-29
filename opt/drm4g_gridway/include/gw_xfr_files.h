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

#ifndef _GW_XFR_FILES_H
#define _GW_XFR_FILES_H

#include "gw_common.h"

/* -------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------*/
/* -------------------------------------------------------------------------- */

typedef struct gw_xfr_s {
	
	int          tries;

	char *       src_url;
	char *       dst_url;
	char *       alt_src_url;
	
	char         mode;
	
	gw_boolean_t done;
	gw_boolean_t success;
	
	int          counter;

} gw_xfr_t;

/* -------------------------------------------------------------------------- */

typedef struct gw_xfrs_s {
	
	int        number_of_xfrs;
	int        failure_limit;
	
	gw_xfr_t * xfrs;
	
} gw_xfrs_t;

/* -------------------------------------------------------------------------- */

void gw_xfr_init    (gw_xfrs_t *xfrs, int number_of_xfrs, int tries);
void gw_xfr_destroy (gw_xfrs_t *xfrs);
int  gw_xfr_pending (gw_xfrs_t *xfrs);

/* -------------------------------------------------------------------------- */
/* ---------------------------------------------------------------------------*/
/* -------------------------------------------------------------------------- */

#endif
