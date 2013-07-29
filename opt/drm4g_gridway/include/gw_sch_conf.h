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

#ifndef _GW_SCH_CONF_H
#define _GW_SCH_CONF_H

#define GW_SCH_DIPATCH_CHUNK_DEFAULT        15
#define GW_SCH_MAX_RUNNING_USER_DEFAULT     30

#define GW_SCH_FP_WEIGHT_DEFAULT            1
#define GW_SCH_FP_DEFAULT                   0

#define GW_SCH_SH_WEIGHT_DEFAULT            1
#define GW_SCH_SH_DEFAULT                   5
#define GW_SCH_SH_WINDOW_DEPTH_DEFAULT      5
#define GW_SCH_SH_WINDOW_SIZE_DEFAULT       1

#define GW_SCH_WT_WEIGHT_DEFAULT            0

#define GW_SCH_DL_WEIGHT_DEFAULT            1
#define GW_SCH_DL_HALF_DEFAULT              1 

#define GW_SCH_MAX_RUNNING_RESOURCE_DEFAULT 10
#define GW_SCH_RP_WEIGHT_DEFAULT            1
#define GW_SCH_RP_DEFAULT                   1

#define GW_SCH_RA_WEIGHT_DEFAULT            1

#define GW_SCH_FR_MAX_BANNED_DEFAULT        3600
#define GW_SCH_FR_BANNED_C_DEFAULT          650

#define GW_SCH_UG_WEIGHT_DEFAULT            1
#define GW_SCH_UG_WINDOW_DEFAULT            3
#define GW_SCH_UG_RATIO_DEFAULT             0.25

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

typedef enum 
{
  DISABLE,
  DISPATCH_CHUNK,
  MAX_RUNNING_USER, 
  FP_WEIGHT,
  FP_USER,
  FP_GROUP,
  SH_WEIGHT,
  SH_USER,
  SH_WINDOW_DEPTH,
  SH_WINDOW_SIZE,
  WT_WEIGHT,
  DL_WEIGHT,
  DL_HALF,
  MAX_RUNNING_RESOURCE,  
  RP_WEIGHT,
  RP_HOST,
  RP_IM,
  RA_WEIGHT,
  FR_MAX_BANNED,
  FR_BANNED_C,
  UG_WEIGHT,
  UG_HISTORY_WINDOW,
  UG_HISTORY_RATIO 
} gw_sch_var_t;

/* -------------------------------------------------------------------- */

typedef struct gw_sch_conf_array_s
{
  char * name;
  int    value;
}gw_sch_conf_array_t;

/* -------------------------------------------------------------------- */

typedef struct gw_sch_conf_s
{
  int   disable;
  
  int   max_dispatch;
  int   max_resource;
  int   max_user;
  
  float wfixed;
  int   ufixed_default;
  
  int                   nufixed;
  gw_sch_conf_array_t * ufixed;
  
  int                   ngfixed;
  gw_sch_conf_array_t * gfixed;

  float wshare;
  int   ushare_default;

  int                   nushare;
  gw_sch_conf_array_t * ushare;
  float                 window_size;
  int                   window_depth;
  
  float wwaiting;
  
  float wdeadline;
  int   dl_half;
  
  float wrfixed;
  int   rfixed_default;
  
  int                   nifixed;
  gw_sch_conf_array_t * ifixed;

  int                   nhfixed;
  gw_sch_conf_array_t * hfixed;
  
  float wrank;
  
  int    fr_max_banned;
  float  fr_banned_c;
  
  float wusage;
  float ug_window;
  float ug_ratio;
  
} gw_sch_conf_t;

/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */
/* -------------------------------------------------------------------- */

void gw_sch_conf_init (gw_sch_conf_t *conf);

int gw_sch_loadconf(gw_sch_conf_t *conf, char *file);

int gw_sch_get_user_fixed(gw_sch_conf_t *conf, const char *user);

void gw_sch_set_user_fixed(gw_sch_conf_t *conf, const char *user, int priority);

int gw_sch_get_group_fixed(gw_sch_conf_t *conf, const char *group);

void gw_sch_set_group_fixed(gw_sch_conf_t *conf, const char *group, int priority);

int gw_sch_get_user_share(gw_sch_conf_t *conf, const char *user);

void gw_sch_set_user_share(gw_sch_conf_t *conf, const char *user, int share);

void gw_sch_set_im_fixed(gw_sch_conf_t *conf, const char *im, int priority);

int gw_sch_get_im_fixed(gw_sch_conf_t *conf, const char *im);

void gw_sch_set_host_fixed(gw_sch_conf_t *conf, const char *host, int priority);

int gw_sch_get_host_fixed(gw_sch_conf_t *conf, const char *host);

void gw_sch_set_var (gw_sch_conf_t * conf, int var, float val);

void gw_sch_set_svar(gw_sch_conf_t * conf, int var, char *str, int val);

/* -------------------------------------------------------------------- */

int gw_sch_get_user_priority(gw_sch_conf_t * conf, 
                             const char *    user, 
                             const char *    group);

int gw_sch_get_host_priority(gw_sch_conf_t * conf, 
                             const char *    host, 
                             const char *    im);
                             
/* -------------------------------------------------------------------- */

#endif
