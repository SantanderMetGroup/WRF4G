#include <stdio.h>
#include <stdlib.h>

#include "protos.h"
#include "registry.h"
#include "data.h"
#include <string.h>
#include <strings.h>
#include "sym.h"

int
gen_namelist_defines ( char * dirname , int sw_dimension )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char  fn[NAMELEN] ;
  node_t *p ;
  
  sprintf( fn, "namelist_defines%s.inc", sw_dimension?"":"2" ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

  fprintf(fp,"integer    :: first_item_in_struct\n") ;
  for ( p = Domain.fields ; p != NULL ; p = p-> next )
  {
    if ( p->node_kind & RCONFIG )
    {
      if ( sw_dimension )
      {
	if      ( !strcmp( p->nentries, "1" ) )
          fprintf(fp,"%s :: %s\n",p->type->name ,p->name) ;
	else if (  strcmp( p->nentries, "-" ) )  /* if not equal to "-" */
          fprintf(fp,"%s , DIMENSION(%s) :: %s\n",p->type->name ,p->nentries,p->name) ;
      }
      else
      {
        fprintf(fp,"%s :: %s\n",p->type->name ,p->name) ;
      }
    }
  }
  fprintf(fp,"integer    :: last_item_in_struct\n") ;

  close_the_file( fp ) ;
  return(0) ;
}

int
gen_namelist_defaults ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char  *fn = "namelist_defaults.inc" ;
  node_t *p ;

  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

  for ( p = Domain.fields ; p != NULL ; p = p-> next )
  {
    if ( p->node_kind & RCONFIG && strcmp(p->dflt,"-") && strcmp(p->dflt,""))
    {
      if ( !strncmp ( p->type->name , "character", 9 ) ) {
        fprintf(fp,"%s = \"%s\"\n",p->name ,p->dflt) ;
      } else {
        fprintf(fp,"%s = %s\n",p->name ,p->dflt) ;
      }
    }
  }

  close_the_file( fp ) ;
  return(0) ;
}


int
gen_namelist_statements ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "namelist_statements.inc" ;
  char  howset[NAMELEN] ;
  char *p1, *p2 ;
  node_t *p ;

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

  for ( p = Domain.fields ; p != NULL ; p = p-> next )
  {
    if ( p->node_kind & RCONFIG )
    {
      strcpy(howset,p->howset) ;
      if (( p1 = strtok(howset,",")) != NULL )
      {
        p2 = strtok(NULL,",") ;
        if ( !strcmp(p1,"namelist") )
        {
          if ( p2 == NULL )
	  {
	    fprintf(stderr,
	    "Warning: no namelist section specified for nl %s\n",p->name) ;
	    continue ;
	  }
	  fprintf(fp,"NAMELIST /%s/ %s\n",p2,p->name) ;
        }
      }
    }
  }

  close_the_file( fp ) ;
  return(0) ;
}

int
gen_namelist_script ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char  *fn = "namelist_script.inc" ;
  node_t *p,*q ;
  char *p1, *p2, *p3, *p4 ;
  char *i;
  char  howset1[NAMELEN] ;
  char  howset2[NAMELEN] ;

  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;

  sym_forget() ;

  fprintf(fp,"# Machine generated, do not edit\n\n") ;
  fprintf(fp,"FILE=${1:-namelist.input}\n\n");

  for ( p = Domain.fields ; p != NULL ; p = p-> next )
  {
    if ( p->node_kind & RCONFIG )
    {
      strcpy(howset1,p->howset) ;
      p1 = strtok(howset1,",") ;
      p2 = strtok(NULL,",") ;
      if ( !strcmp(p1,"namelist") ) {
        if ( p2 == NULL ) {
          fprintf(stderr,
          "Warning: no namelist section specified for nl %s\n",p->name) ;
          continue ;
        }
	if (sym_get( p2 ) == NULL) { /* not in table yet */
          fprintf(fp,"echo \\&%s >> $FILE\n",p2) ;
          for ( q = Domain.fields ; q != NULL ; q = q-> next ) {
            if ( q->node_kind & RCONFIG) {
              strcpy(howset2,q->howset) ;
              p3 = strtok(howset2,",") ;
              p4 = strtok(NULL,",") ;
              if ( p4 == NULL ) {
                continue ;
              }

              if ( !strcmp(p2,p4)) {
                fprintf(fp,"if test ! -z \"$NL_") ;
                for (i=q->name; *i!='\0'; i++) {
                  fputc(toupper(*i),fp); 
                }
                if ( !strncmp(q->type->name,"character",9)) {
                   fprintf(fp,"\"; then echo \"%s=\\\"${NL_",q->name) ;
                   for (i=q->name; *i!='\0'; i++) {
                     fputc(toupper(*i),fp); 
                   }
                   fprintf(fp,"}\\\",\"") ;
                } else {
                  fprintf(fp,"\"; then echo \"%s=${NL_",q->name) ;
                  for (i=q->name; *i!='\0'; i++) {
                    fputc(toupper(*i),fp); 
                  }
                  fprintf(fp,"},\"") ;
                }

                fprintf(fp," >> $FILE;fi\n") ;
              }

            }
          }
          fprintf(fp,"echo / >> $FILE\n") ;
	  sym_add(p2) ;
	}
      }
    }
  }
  
  fprintf(fp,"echo \\&namelist_quilt >> $FILE\n");
  fprintf(fp,"if test ! -z \"$NL_NIO_TASKS_PER_GROUP\"; then echo \"nio_tasks_per_group=${NL_NIO_TASKS_PER_GROUP},\" >> $FILE;fi\n");
  fprintf(fp,"if test ! -z \"$NL_NIO_GROUPS\"; then echo \"nio_groups=${NL_NIO_GROUPS},\" >> $FILE;fi\n");
  fprintf(fp,"echo / >> $FILE\n");

  fclose( fp ) ;
  return(0) ;
}


int
gen_get_nl_config ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "_nl_config.inc" ;
  char * gs, * intnt ;
  char  howset[NAMELEN] ;
  node_t *p ;
  int sw ;
  int num_rconfigs = 0 ;
  int i, half, j ;

  for ( p = Domain.fields ; p != NULL ; p = p-> next ) { if ( p->node_kind & RCONFIG ) { num_rconfigs++ ; } }  /* howmany deez guys? */

  for ( half = 0, j=0 ; half < num_rconfigs ; half += (num_rconfigs+1)/2, j++ ) {    /* break the files in half so we don't kill the compilers as much */
  for ( sw = 0 ; sw < 2 ; sw++ ) {

  if ( sw == 0 ) { gs = "get" ; intnt = "OUT" ; } else { gs = "set" ; intnt = "IN" ; }

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s_%d%s",dirname,gs,j,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

  for ( p = Domain.fields, i = -1 ; p != NULL ; p = p-> next )
  {
    if ( p->node_kind & RCONFIG ) {
       i++ ;
    if ( (i >= half) && (i < half + (num_rconfigs+1)/2) )
    {
      strcpy(howset,p->howset) ;
      fprintf(fp,"SUBROUTINE nl_%s_%s ( id_id , %s )\n",gs,p->name, p->name) ;
      if ( sw_fort_kludge ) {
        fprintf(fp,"  USE module_configure, ONLY : model_config_rec \n") ;
      }
      fprintf(fp,"  %s , INTENT(%s) :: %s\n",p->type->name,intnt,p->name) ;
      fprintf(fp,"  INTEGER id_id\n") ;
      fprintf(fp,"  CHARACTER*80 emess\n") ;
      if ( sw == 0 ) /* get */
      {
        if ( !strcmp( p->nentries, "1" )) {
          if ( ! sw_fort_kludge ) {
            fprintf(fp,"  IF ( id_id .NE. 1 ) THEN\n") ;
            fprintf(fp,"    call wrf_debug(1,&\n'WARNING in nl_%s_%s: %s applies to all domains. First arg ignored.')\n",
                            gs,p->name, p->name ) ;
            fprintf(fp,"  ENDIF\n" ) ;
          }
          if ( !strncmp(p->type->name,"character",9)) {
            fprintf(fp,"  %s = trim(model_config_rec%%%s)\n",p->name,p->name) ;
          }else{
            fprintf(fp,"  %s = model_config_rec%%%s\n",p->name,p->name) ;
          }
        } else {
          if ( ! sw_fort_kludge ) {
            if        ( !strcmp( p->nentries, "max_domains" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%%max_dom ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range domain number: ',id_id\n",gs,p->name) ;
	    } else if ( !strcmp( p->nentries, "max_moves" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%%num_moves ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range move number: ',id_id\n",gs,p->name) ;
	    } else if ( !strcmp( p->nentries, "max_eta" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%%e_vert(1) ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range eta_level number: ',id_id\n",gs,p->name) ;
	    } else if ( !strcmp( p->nentries, "max_outer_iterations" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. max_outer_iterations ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range eps number: ',id_id\n",gs,p->name) ;
	    } else if ( !strcmp( p->nentries, "max_instruments" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. max_instruments ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range instruments number: ',id_id\n",gs,p->name) ;
	    } else {
	      fprintf(stderr,"Registry WARNING: multi element rconfig entry must be either max_domains, max_moves, max_eta, max_outer_iterations, or max_instruments \n") ;
	    }
            fprintf(fp,"    CALL wrf_error_fatal(emess)\n") ;
            fprintf(fp,"  ENDIF\n" ) ;
          }
          fprintf(fp,"  %s = model_config_rec%%%s(id_id)\n",p->name,p->name) ;
        }
      }
      else   /* set */
      {
        if ( !strcmp( p->nentries, "1" )) {
          if ( ! sw_fort_kludge ) {
            fprintf(fp,"  IF ( id_id .NE. 1 ) THEN\n") ;
            fprintf(fp,"    call wrf_debug(1,&\n'WARNING in nl_%s_%s: %s applies to all domains. First arg ignored.')\n",
                            gs,p->name, p->name ) ;
            fprintf(fp,"  ENDIF\n" ) ;
          }
          if ( !strncmp(p->type->name,"character",9)) {
            fprintf(fp,"  model_config_rec%%%s = trim(%s) \n",p->name,p->name) ;
          }else{
            fprintf(fp,"  model_config_rec%%%s = %s \n",p->name,p->name) ;
          }
        } else {
          if ( ! sw_fort_kludge ) {
            if        ( !strcmp( p->nentries, "max_domains" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%%max_dom ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range domain number: ',id_id\n",gs,p->name) ;
	    } else if ( !strcmp( p->nentries, "max_moves" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%%num_moves ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range move number: ',id_id\n",gs,p->name) ;
	    } else if ( !strcmp( p->nentries, "max_eta" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. model_config_rec%%e_vert(1) ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range eta_level number: ',id_id\n",gs,p->name) ;
	    } else if ( !strcmp( p->nentries, "max_outer_iterations" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. max_outer_iterations ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range eps number: ',id_id\n",gs,p->name) ;
	    } else if ( !strcmp( p->nentries, "max_instruments" )) {
              fprintf(fp,"  IF ( id_id .LT. 1 .OR. id_id .GT. max_instruments ) THEN\n") ;
              fprintf(fp,"    WRITE(emess,*)'nl_%s_%s: Out of range instruments number: ',id_id\n",gs,p->name) ;
	    } else {
	      fprintf(stderr,"Registry WARNING: multi element rconfig entry must be either max_domains, max_moves, max_eta, max_outer_iterations, or max_instruments \n") ;
	    }
            fprintf(fp,"    CALL wrf_error_fatal(emess)\n") ;
            fprintf(fp,"  ENDIF\n" ) ;
          }
          fprintf(fp,"  model_config_rec%%%s(id_id) = %s\n",p->name,p->name) ;
        }
      }
      fprintf(fp,"  RETURN\n") ;
      fprintf(fp,"END SUBROUTINE nl_%s_%s\n",gs,p->name ) ;
    }
    }
  }
  close_the_file( fp ) ;
  }
  } /* halfs */
  return(0) ;
}

int
gen_config_assigns ( char * dirname )
{
  FILE * fp ;
  char  fname[NAMELEN] ;
  char * fn = "config_assigns.inc" ;
  char  tmp[NAMELEN] ;
  node_t *p ;

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

  fprintf(fp,"! Contains config assign statements for module_domain.F.\n") ;
  fprintf(fp,"#ifndef SOURCE_RECORD\n") ;
  fprintf(fp,"#  define SOURCE_RECORD cfg%%\n") ;
  fprintf(fp,"#endif\n") ;
  fprintf(fp,"#ifndef SOURCE_REC_DEX\n") ;
  fprintf(fp,"#  define SOURCE_REC_DEX\n") ;
  fprintf(fp,"#endif\n") ;
  fprintf(fp,"#ifndef DEST_RECORD\n") ;
  fprintf(fp,"#  define DEST_RECORD new_grid%%\n") ;
  fprintf(fp,"#endif\n") ;

  for ( p = Domain.fields ; p != NULL ; p = p-> next )
  {
    if ( p->node_kind & RCONFIG )
    {
      if ( !strcmp( p->nentries, "1" ))
        strcpy( tmp, "" ) ;
      else
        strcpy( tmp, "SOURCE_REC_DEX" ) ;
      fprintf(fp," DEST_RECORD %-26s = SOURCE_RECORD %s %s\n",p->name,p->name,tmp) ;
    }
  }
  close_the_file( fp ) ;
  return(0) ;
}

int
gen_config_reads ( char * dirname )
{
  FILE * fp ;
  int i, n_nml ;
  char  fname[NAMELEN] ;
  char * fn = "config_reads.inc" ;
  char  howset[NAMELEN] ;
  char *p1, *p2 ;
  node_t *p ;

  strcpy( fname, fn ) ;
  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  if ((fp = fopen( fname , "w" )) == NULL ) return(1) ;
  print_warning(fp,fname) ;

  fprintf(fp,"! Contains namelist statements for module_config.F.\n") ;
  fprintf(fp,"#ifndef NAMELIST_READ_UNIT\n") ;
  fprintf(fp,"#  define NAMELIST_READ_UNIT nml_read_unit\n") ;
  fprintf(fp,"#endif\n") ;
  fprintf(fp,"#ifndef NAMELIST_WRITE_UNIT\n") ;
  fprintf(fp,"#  define NAMELIST_WRITE_UNIT nml_write_unit\n") ;
  fprintf(fp,"#endif\n") ;
  fprintf(fp,"!\n") ;

  sym_forget() ;

  /*
     Count how many namelists are defined in the registry
  */
  n_nml = 0 ;
  for ( p = Domain.fields ; p != NULL ; p = p-> next )
  {
    if ( p->node_kind & RCONFIG )
    {
      strcpy(howset,p->howset) ;
      p1 = strtok(howset,",") ;
      p2 = strtok(NULL,",") ;
      if ( !strcmp(p1,"namelist") )
      {
	if (sym_get( p2 ) == NULL)  /* not in table yet */
	{
          n_nml ++ ;
	  sym_add(p2) ;
	}
      }
    }
  }

  sym_forget() ;

  fprintf(fp," nml_read_error = .FALSE.\n") ;
  fprintf(fp," NML_LOOP : DO i=1,%i\n", n_nml) ;
  fprintf(fp,"    REWIND ( UNIT = NAMELIST_READ_UNIT )\n") ;
  fprintf(fp,"    SELECT CASE ( i )\n") ;
  i = 1;
  for ( p = Domain.fields ; p != NULL ; p = p-> next )
  {
    if ( p->node_kind & RCONFIG )
    {
      strcpy(howset,p->howset) ;
      p1 = strtok(howset,",") ;
      p2 = strtok(NULL,",") ;
      if ( !strcmp(p1,"namelist") )
      {
        if ( p2 == NULL )
        {
          fprintf(stderr,
          "Warning: no namelist section specified for nl %s\n",p->name) ;
          continue ;
        }
	if (sym_get( p2 ) == NULL)  /* not in table yet */
	{
          fprintf(fp,"       CASE ( %i ) \n",i) ;
          fprintf(fp,"          nml_name = \"%s\"\n",p2) ;
          fprintf(fp,"          READ   ( UNIT = NAMELIST_READ_UNIT , NML = %s , ERR=9201, END=9202 )\n",p2) ;
          fprintf(fp,"#ifndef NO_NAMELIST_PRINT\n") ;
          fprintf(fp,"          WRITE ( UNIT = NAMELIST_WRITE_UNIT, NML = %s )\n",p2) ;
          fprintf(fp,"#endif\n") ;
          fprintf(fp,"          CYCLE NML_LOOP\n") ;
          i ++ ;
	  sym_add(p2) ;
	}
      }
    }
  }
  fprintf(fp,"    END SELECT\n") ;
  fprintf(fp,"9201 CALL wrf_message(\"Error while reading namelist \"//TRIM(nml_name))\n") ;
  fprintf(fp,"    nml_read_error = .TRUE.\n") ;

  fprintf(fp,"    IF ( TRIM(nml_name) .EQ. \"dynamics\") THEN\n") ;
  fprintf(fp,"        CALL wrf_alt_nml_dynamics(nml_read_unit, TRIM(nml_name))\n") ;
  fprintf(fp,"    ENDIF\n") ;
  fprintf(fp,"    IF ( TRIM(nml_name) .EQ. \"physics\") THEN\n") ;
  fprintf(fp,"        CALL wrf_alt_nml_physics(nml_read_unit, TRIM(nml_name))\n") ;
  fprintf(fp,"    ENDIF\n") ;
  fprintf(fp,"    IF ( TRIM(nml_name) .EQ. \"fdda\") THEN\n") ;
  fprintf(fp,"        CALL wrf_alt_nml_fdda(nml_read_unit, TRIM(nml_name))\n") ;
  fprintf(fp,"    ENDIF\n") ;
  fprintf(fp,"    IF ( TRIM(nml_name) .EQ. \"wrfvar1\") THEN\n") ;
  fprintf(fp,"        CALL wrfvar_alt_nml_wrfvar1(nml_read_unit, TRIM(nml_name))\n") ;
  fprintf(fp,"    ENDIF\n") ;
  fprintf(fp,"    IF ( TRIM(nml_name) .EQ. \"wrfvar2\") THEN\n") ;
  fprintf(fp,"        CALL wrfvar_alt_nml_wrfvar2(nml_read_unit, TRIM(nml_name))\n") ;
  fprintf(fp,"    ENDIF\n") ;
  fprintf(fp,"    IF ( TRIM(nml_name) .EQ. \"wrfvar4\") THEN\n") ;
  fprintf(fp,"        CALL wrfvar_alt_nml_wrfvar4(nml_read_unit, TRIM(nml_name))\n") ;
  fprintf(fp,"    ENDIF\n") ;
  fprintf(fp,"    IF ( TRIM(nml_name) .EQ. \"wrfvar14\") THEN\n") ;
  fprintf(fp,"        CALL wrfvar_alt_nml_wrfvar14(nml_read_unit, TRIM(nml_name))\n") ;
  fprintf(fp,"    ENDIF\n") ;
  fprintf(fp,"    CYCLE NML_LOOP\n") ;
  fprintf(fp,"9202 CALL wrf_message(\"Namelist \"//TRIM(nml_name)//\" not found in namelist.input.\"// & \n") ;
  fprintf(fp,"                      \" Using registry defaults for variables in \"//TRIM(nml_name))\n") ;
  fprintf(fp," END DO NML_LOOP\n") ;
  fprintf(fp," \n") ;
  fprintf(fp," IF ( nml_read_error ) CALL wrf_error_fatal(\"Errors while reading one or more namelists from namelist.input.\")\n") ;

  close_the_file( fp ) ;
  return(0) ;
}

