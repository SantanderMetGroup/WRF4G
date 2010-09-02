MODULE module_com_diagnostics

  USE module_list_diagnostics

  CONTAINS
! Module to process selected diagnose variables from netCDF fields in vertical p coordinates
! GMS. UC: January 2010. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!!! COMPILATION
!
! Execute compilation.bash
!
!!!!!!!!!!!!!!! Subroutines
! com_diagnostics: Subroutine to compute all desired diagnostics

!   567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567

SUBROUTINE com_diagnostics(dbg, ifiles, Ninfiles, Diags, Ndiags, deltaX, deltaY, deltaT,        &
  dimsIname, dimsOname, NdimsO, ofile, g_att_file, car_x, car_y)
! Subroutine to compute all desired diagnostics

  USE module_list_diagnostics
  USE module_gen_tools
  USE module_nc_tools
  USE module_constants
  USE module_types
  USE module_calc_tools, ONLY: calc_method_gen6D

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  INTEGER, INTENT(IN)                                    :: dbg
  INTEGER, INTENT(IN)                                    :: Ninfiles, g_att_file
  CHARACTER(LEN=500), DIMENSION(Ninfiles), INTENT(IN)    :: ifiles
  INTEGER, INTENT(IN)                                    :: NdimsO
  INTEGER, INTENT(IN)                                    :: Ndiags
  CHARACTER(LEN=250), DIMENSION(Ndiags), INTENT(IN)      :: Diags
  CHARACTER(LEN=500), INTENT(IN)                         :: ofile
  REAL, INTENT(IN)                                       :: deltaX, deltaY, deltaT
  CHARACTER(LEN=50), DIMENSION(4), INTENT(IN)            :: dimsIname
  CHARACTER(LEN=50), DIMENSION(NdimsO), INTENT(IN)       :: dimsOname
  LOGICAL, INTENT(IN)                                    :: car_x, car_y

! Local vars
  INTEGER                                                :: i, idiag, iinput
  INTEGER                                                :: Nvardiag
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:)            :: rMATinputsA, rMATinputsB,           &
    rMATinputsC, rMATinputsD, rMATinputsE, rMATinputsF 
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:)              :: gen_result6D
  INTEGER, ALLOCATABLE, DIMENSION(:,:)                   :: DimMatInputs
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:,:)               :: Idiagnostic4D
  INTEGER, ALLOCATABLE, DIMENSION(:,:,:)		 :: Idiagnostic3D
  INTEGER, ALLOCATABLE, DIMENSION(:,:) 		         :: Idiagnostic2D
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)                  :: Rdiagnostic4D
  REAL, ALLOCATABLE, DIMENSION(:,:,:)                    :: Rdiagnostic3D
  REAL, ALLOCATABLE, DIMENSION(:,:)                      :: Rdiagnostic2D
  CHARACTER(LEN=100), ALLOCATABLE, DIMENSION(:,:,:,:)    :: Cdiagnostic4D
  CHARACTER(LEN=100), ALLOCATABLE, DIMENSION(:,:,:)      :: Cdiagnostic3D
  CHARACTER(LEN=100), ALLOCATABLE, DIMENSION(:,:)        :: Cdiagnostic2D
  INTEGER, ALLOCATABLE, DIMENSION(:,:)                   :: foundvariables
  INTEGER                                                :: ierr, oid, jvar, rcode
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: DIAGvariables
  CHARACTER(LEN=250)                                     :: messg
  CHARACTER(LEN=50)                                      :: section
  TYPE(variabledef)                                      :: diagcom

!!!!!!!!!!!!!!!!! Variables
! ifiles: vector with input files
! Ninfiles: number of infiles
! car_[x/y]: Whether x and y coordinates are cartesian ones
! Diags: vector with diagnostic variables
! diagcom: diagnostic to compute (as variabledef type)
! Ndiags: number of diagnostic variables
! delta_[X/Y/T]: respective delta in direction X/Y [m] and time T [s]
! dimsIname: name of 4-basic dimensions (X, Y, Z, T) in input file
! dimsOname: name of dimension in output file
! NdimsO: number of dimensions in output file
! ofile: outputfile
! g_att_file: file from which all global attributes will putted in output file
! Ndimsin: number of variables of input file
! Idiagnostic4D: integer 4D computed diagnostic
! Idiagnostic4D: integer 3D computed diagnostic
! Idiagnostic4D: integer 2D computed diagnostic
! Rdiagnostic4D: real 4D computed diagnostic
! Rdiagnostic4D: real 3D computed diagnostic
! Rdiagnostic4D: real 2D computed diagnostic
! Cdiagnostic4D: character 4D computed diagnostic
! Cdiagnostic4D: character 3D computed diagnostic
! Cdiagnostic4D: character 2D computed diagnostic
! DIAGvariables: vector with name of wanted variables for the diagnostic
! foundvariables: matrix of location of desired variables (line as variable number according to 
!   DIAGvariables) 
!   col1: number of files (according to Ninfiles)    col2: id variable in from file of 'col1'
! Nvardiag: number of variables to compute the specific diagnostic
! rMATinputs[A:F]: real matrix with DIAGvariables values (7D; 6D netCDF variable dimensions, 
!   #order of Nvardiag with given dimensions) of A to F differend shapes and number of dimensions
! DimMatInputs: matrix with dimensions of DIAGvariables values (7D; 6D netCDF dimensions, #order 
!   of Nvardiag)
! jvar: number of variable in output file
  
  section="'module_com_diagnostics'"

  IF (dbg >= 100) PRINT *,'Diagnostics '//TRIM(section)//'... .. .'

! output file creation & dimensions
!!
  CALL create_output(dbg, ofile, dimsIname, NdimsO, dimsOname, ifiles(g_att_file), car_x, car_y,&
     ifiles, Ninfiles)

  rcode = nf_open(TRIM(ofile), NF_WRITE, oid)
!  rcode = nf_create(TRIM(ofile), NF_CLOBBER, oid)

!!!
!!
! Computing diagnostics
!!
!!!
  IF (dbg >= 100) PRINT *,"  Computing diagnostics ..."
  jvar=nc_last_idvar(dbg, oid)
    
  compute_diags: DO idiag=1, Ndiags
   
   CALL diagnostic_var_inf(dbg, diags(idiag), diagcom)

!!
!  Generic for all diagnostics
!!
!!!!!!!    !!!!!!!    !!!!!!    !!!!!!!    !!!!!!!    !!!!!!!    !!!!!!!

! Setting matrixs with diagnostic input variables file location, id and particular dimensions
       IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
       ALLOCATE(foundvariables(diagcom%NinVarnames, 2))
       
       IF (ALLOCATED(DimMatInputs)) DEALLOCATE(DimMatInputs)
       ALLOCATE(DimMatInputs(diagcom%NinVarnames,6))
       
       CALL search_variables(dbg, ifiles, Ninfiles, diagcom%INvarnames, diagcom%NinVarnames,    &
         foundvariables, DimMatInputs)

! Defining new diagnostic variable in output file
       IF (.NOT.exists_var(oid, diagcom%name)) THEN
         CALL def_variable(dbg, oid, diagcom)
         jvar=nc_last_idvar(dbg, oid)
       ELSE
         rcode = nf_inq_varid (oid, diagcom%name, jvar)
       END IF

! Looking if diagnostic can be calculated by a generic method (see 'generic_calcs6D' in
!    'module_constants' for input variables of the same rank and shape

       IF (ANY(diagcom%method == generic_calcs6D)) THEN
         PRINT *,"  calculated via the generic method '"//TRIM(diagcom%method)//"'"
         IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
         ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),          &
           DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6), diagcom%NinVarnames),       &
	   STAT=ierr)
         IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsA'"
       
! real generic output will be keep in 'gen_result6D' 6D-matrix
!!
         IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
         ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),         &
           DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
         IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                          &
	   " allocating 'gen_result6D'"
       
         DO iinput=1,diagcom%NinVarnames
           CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),               &
  	     DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
         END DO
         CALL calc_method_gen6D(dbg, diagcom%method, DimMatInputs(1,:), diagcom%NinVarnames,    &
	   rMATinputsA, diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

       ENDIF

!!
! Specific computations
!!
!!!!!!!    !!!!!!!    !!!!!!    !!!!!!!    !!!!!!!    !!!!!!!    !!!!!!!    !!!!!!!    !!!!!!!

   diag_var: SELECT CASE(diags(idiag))

! CLT. Total Cloud fraction
!!
    CASE ('CLT')

! Setting diagnstic id in outputfile
       IF (dbg >= 75) PRINT *,'  Computing CLT...'

! Preparation of diagnostic result
       IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
       ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1),DimMatInputs(1,2),DimMatInputs(1,4)),STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'diagnostic3D'"

! method computation
!!

! Creation of specific matrix for computation of diagnostic according to the method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

       IF (.NOT.ANY(diagcom%method == generic_calcs6D)) THEN
       
         methodsel: SELECT CASE (diagcom%method)
           CASE ('Sundqvist')
             PRINT *,"  calculated via the '"//TRIM(diagcom%method)//"' method"
             IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
             ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
               DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
             IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                      &
      	       " allocating 'rMATinputsA'"
       
             DO iinput=1,1
               CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),           &
      	         DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
             END DO

             CALL clt(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),             &
	       DimMatInputs(1,4), rMATinputsA(:,:,:,:,1,1,1), Rdiagnostic3D)

           CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

         END SELECT methodsel

       ELSE
!  Variable result from generic method
!!
         Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)
       END IF
       
         IF (dbg >= 75) THEN
           PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
           PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),            &
	     halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
         ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (2)
!        Text diagnostic
           rcode = nf_put_var_text (oid, jvar, Cdiagnostic3D)
           CALL error_nc(section, rcode)
         CASE (4)
!        integer diagnostic
           rcode = nf_put_var_real (oid, jvar, Idiagnostic3D)
           CALL error_nc(section, rcode)
         CASE (5)
!        Real diagnostic
           rcode = nf_put_var_real (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to do with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

       DEALLOCATE(Rdiagnostic3D)
       DEALLOCATE(foundvariables, DimMatInputs)
       DEALLOCATE(rMATinputsA)
             
    END SELECT diag_var

  END DO compute_diags

  rcode = nf_close(oid)
  RETURN
END SUBROUTINE com_diagnostics

END MODULE module_com_diagnostics
