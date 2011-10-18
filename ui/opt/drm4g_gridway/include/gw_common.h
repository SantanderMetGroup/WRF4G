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

#include <stdio.h>

#ifndef _GW_COMMON_H
#define _GW_COMMON_H

#define GWNSTR(str) ((str)?(str):"")

#define GW_VERSION PACKAGE_STRING

#ifdef GW_GLOBUS_DIRS
#define GW_ETC_DIR "etc/gridway"
#define GW_VAR_DIR "var/gridway"
#define GW_TEST_DIR "test/gridway"
#else
#define GW_ETC_DIR "etc"
#define GW_VAR_DIR "var"
#define GW_TEST_DIR "test"
#endif

/*----------------------------------------------------------------------------*/
/* General Types                                                              */
/*----------------------------------------------------------------------------*/

typedef enum {
   GW_FALSE,
   GW_TRUE
} gw_boolean_t;

/* -------------------------------------------------------------------------- */

typedef enum{
  GW_RC_SUCCESS,                      
  GW_RC_FAILED,           
  GW_RC_FAILED_FILE_ACCESS,
  GW_RC_FAILED_BAD_JOB_ID,
  GW_RC_FAILED_BAD_ARRAY_ID,
  GW_RC_FAILED_BAD_HOST_ID,  
  GW_RC_FAILED_BAD_JOB_STATE,
  GW_RC_FAILED_NO_MEMORY, 
  GW_RC_FAILED_THREAD,
  GW_RC_FAILED_INIT,
  GW_RC_FAILED_CONNECTION,
  GW_RC_FAILED_JOB_KILLED,
  GW_RC_FAILED_USER,
  GW_RC_FAILED_PERM,
  GW_RC_FAILED_JT,
  GW_RC_FAILED_JOB_FAIL,
  GW_RC_FAILED_TIMEOUT,
  GW_RC_LIMIT                         
} gw_return_code_t;

/* -------------------------------------------------------------------------- */

/* Stringize define values */
#define __GWSTR(x) #x
#define GW2STR(x) __GWSTR(x)


/* -------------------------------------------------------------------------- */

/* The next typedef affects the drmaa and some other functions in gw_common.c */
typedef char gw_short_string_t[50];

/* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */


char *gw_ret_code_string(gw_return_code_t code);

void gw_print (FILE *fd, const char* module, const char mode, const char *str_format,...);

#endif
