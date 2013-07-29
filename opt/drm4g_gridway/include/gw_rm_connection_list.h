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

#ifndef _GW_RM_CONNECTION_LIST_H
#define _GW_RM_CONNECTION_LIST_H

#include "gw_rm_msg.h"

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

typedef struct gw_connection_list_s gw_connection_list_t;

/*----------------------------------------------------------------------------*/

struct gw_connection_list_s
{
	int 	            socket_fs;
	gw_msg_type_t       msg_id;
	gw_msg_wait_type_t  wait_type;
	int 				job_id;

	gw_connection_list_t * next;
};

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

void gw_connection_list_init(gw_connection_list_t **list);

void gw_connection_list_destroy(gw_connection_list_t **list);

void gw_connection_list_add(gw_connection_list_t ** list,
							int                     socket_fs,
							gw_msg_type_t           msg_id,
                            gw_msg_wait_type_t      wait_type,
							int                     job_id);

gw_connection_list_t * gw_connection_list_get(gw_connection_list_t ** list,							
				   							  gw_msg_type_t           msg_id,
							                  int                     job_id);
							                  
gw_connection_list_t * gw_connection_list_get_by_client(gw_connection_list_t ** list,							
				   							  			gw_msg_type_t           msg_id,
							                  			int                     client_socket);							                  

gw_boolean_t gw_connection_list_wait_in_list(gw_connection_list_t * list,							
				   				             gw_msg_wait_type_t     wait_type,
							                 int                    client_socket);

void gw_connection_list_delete(gw_connection_list_t ** list, int client_socket);							                 
							                  			
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
            
#endif
