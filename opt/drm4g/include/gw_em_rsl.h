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

#ifndef _GW_EM_RSL_H
#define _GW_EM_RSL_H

#include "gw_job.h"

#define GW_RSL_LENGTH 			  16384

char* gw_generate_wrapper_rsl(gw_job_t *job);
char* gw_generate_wrapper_rsl_nsh(gw_job_t *job); /* For resources with non-shared home (nsh) */

char* gw_generate_rsl2(gw_job_t *job);
char* gw_generate_wrapper_rsl2(gw_job_t *job);    /* For wrapper-based execution */
char* gw_generate_nowrapper_rsl2(gw_job_t *job);  /* For non-wrapper-based execution */

char* gw_generate_pre_wrapper_rsl(gw_job_t *job);
char* gw_generate_pre_wrapper_rsl2(gw_job_t *job);

char* gw_generate_wrapper_xrsl(gw_job_t *job);    /* Nordugrid xrsl */

char* gw_generate_wrapper_jdl(gw_job_t *job);     /* CREAM JDL */
char* gw_generate_wrapper_jsdl(gw_job_t *job);    /* OGF JSDL (for BES) */

#endif
