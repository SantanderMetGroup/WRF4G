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

  USE netcdf
  USE module_list_diagnostics
  USE module_gen_tools
  USE module_nc_tools
  USE module_constants
  USE module_types
  USE module_calc_tools, ONLY: calc_method_gen6D, Earth_wind

  IMPLICIT NONE

!  INCLUDE 'netcdf.inc'

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
  INTEGER                                                :: i, j, idiag, iinput
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
  CHARACTER(LEN=50)                                      :: section, indivmethod
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

  rcode = nf90_open(TRIM(ofile), NF90_WRITE, oid)
!  rcode = nf90_create(TRIM(ofile), NF90_CLOBBER, oid)

!!!
!!
! Computing diagnostics
!!
!!!
  IF (dbg >= 100) PRINT *,"  Computing diagnostics ..."
  jvar=nc_last_idvar(dbg, oid)
    
  compute_diags: DO idiag=1, Ndiags
   PRINT *,'  --- - --- - --- - ---  '
   PRINT *,'  '
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
         rcode = nf90_inq_varid (oid, diagcom%name, jvar)
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
    CASE ('clt')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing clt...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1),DimMatInputs(1,2),DimMatInputs(1,4)),STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'diagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,1,:,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodsel: SELECT CASE (diagcom%method)

          CASE ('Sundqvist')
! Sundqvist, 1989, Mont. Weather Rev.

            PRINT *,"  calculated via the '"//TRIM(diagcom%method)//"' method"
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
	      " allocating 'rMATinputsA'"
       
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
    	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            CALL clt(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),              &
	      DimMatInputs(1,4), rMATinputsA(:,:,:,:,1,1,1), Rdiagnostic3D)

          CASE DEFAULT
	    messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	    CALL diag_fatal(messg)

        END SELECT methodsel

      END IF
       
        IF (dbg >= 75) THEN
          PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
          PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),             &
            halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
        ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!         Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to do write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

       DEALLOCATE(Rdiagnostic3D)
       DEALLOCATE(rMATinputsA)

! dth. Thickness of depth layers
!!
    CASE ('dth')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing dth...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic2D)) DEALLOCATE(Rdiagnostic2D)
      ALLOCATE(Rdiagnostic2D(DimMatInputs(1,1),1),STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'diagnostic2D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic2D(:,1)=gen_result6D(:,1,1,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methoddth: SELECT CASE (diagcom%method)

          CASE DEFAULT
	    messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	    CALL diag_fatal(messg)

        END SELECT methoddth

      END IF
       
        IF (dbg >= 75) THEN
          PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
          PRINT *,'  1/2 example value:', Rdiagnostic2D(halfdim(DimMatInputs(1,1)), 1)
        ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!         Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic2D(:,1))
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to do write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

       DEALLOCATE(Rdiagnostic2D)

! evspsbl. Surface water evaporation
!!
    CASE ('evspsbl')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing evspsbl...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodevspsbl: SELECT CASE (diagcom%method)
    
          CASE ('wrfdirect')
	  
! Field is the flux from surface evaporation at a given moment 'SFCEVP'
            
! SFCEVP
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
      
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO


! Difference between fields
       
            Rdiagnostic3D=rMATinputsA(:,:,:,1,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodevspsbl

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA)
             
! hfls. Surface latent heat flux
!!
    CASE ('hfls')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing hfls...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodhfls: SELECT CASE (diagcom%method)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodhfls

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)

! hfso. Soil heat flux
!!
    CASE ('hfso')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing hfso...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodhfso: SELECT CASE (diagcom%method)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodhfso

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)

! hfss. Surface sensible heat flux
!!
    CASE ('hfss')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing hfss...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodhfss: SELECT CASE (diagcom%method)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodhfss

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)

! hurs. 2m relative humidity
!!
    CASE ('hurs')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing hurs...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic4D)) DEALLOCATE(Rdiagnostic4D)
      ALLOCATE(Rdiagnostic4D(DimMatInputs(1,1), DimMatInputs(1,2), 1, DimMatInputs(1,3)),       &
        STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic4D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic4D(:,:,1,:)=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodhurs: SELECT CASE (diagcom%method)

          CASE ('Tetens')
! Tetens equation (Tetens, 1930)
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),3), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
            DO iinput=1,3
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
                DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            CALL hurs(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),             &
              rMATinputsA(:,:,:,:,1,1,1), rMATinputsA(:,:,:,:,1,1,2),rMATinputsA(:,:,:,:,1,1,3),&
              Rdiagnostic4D(:,:,1,:))
	      
          CASE ('its90')
! ITS-90 formulation http://www.decatur.de/javascript/dew/resources/its90formulas.pdf

            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),3), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
            DO iinput=1,3
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
                DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            CALL hurs_its90(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),             &
              rMATinputsA(:,:,:,:,1,1,1), rMATinputsA(:,:,:,:,1,1,2),rMATinputsA(:,:,:,:,1,1,3),&
              Rdiagnostic4D(:,:,1,:))

          CASE ('Bolton')
! Bolton equation (Bolton, 1980, Mont. Weather Rev.)

            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),3), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
            DO iinput=1,3
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
                DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            CALL hurs_Bolton(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),             &
              rMATinputsA(:,:,:,:,1,1,1), rMATinputsA(:,:,:,:,1,1,2),rMATinputsA(:,:,:,:,1,1,3),&
              Rdiagnostic4D(:,:,1,:))
          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodhurs

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic4D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), 1, halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic4D, (/1, 1, 1, 1/), (/DimMatInputs(1,1), &
	     DimMatInputs(1,2), 1, DimMatInputs(1,3) /))
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic4D)
      DEALLOCATE(rMATinputsA)

! huss. 2m specific humidty
!!
    CASE ('huss')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing huss...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodhuss: SELECT CASE (diagcom%method)
    
          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodhuss

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D, (/1, 1, 1, 1/), (/DimMatInputs(1,1), &
	     DimMatInputs(1,2), 1, DimMatInputs(1,3)/))
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)

! mrso. Total soil moisture
!!
    CASE ('mrso')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing mrso...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,4)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,1,:,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodmrso: SELECT CASE (diagcom%method)

          CASE ('wrf')
	  
! SMOIS
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

! DZS
            IF (ALLOCATED(rMATinputsB)) DEALLOCATE(rMATinputsB)
              ALLOCATE(rMATinputsB(DimMatInputs(2,1), DimMatInputs(2,2), DimMatInputs(2,3),     &
                DimMatInputs(2,4), DimMatInputs(2,5), DimMatInputs(2,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsB'"
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput+1,:),          &
	        DimMatInputs(2,:), rMATinputsB(:,:,:,:,:,:,iinput))
            END DO
	  
            CALL mrso(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),             &
	      DimMatInputs(1,4), rMATinputsA(:,:,:,:,1,1,1), rMATinputsB(:,:,1,1,1,1,1),        &
	      Rdiagnostic3D)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodmrso

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA, rMATinputsB)

! mrros. Surface runoff flux
!!
    CASE ('mrros')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing mrros...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodmrros: SELECT CASE (diagcom%method)
    
          CASE ('wrf')
	  
! Field is the flux from surface run off at given moment 'SFROFF'
            
! SFROFF
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
      
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            Rdiagnostic3D=rMATinputsA(:,:,:,1,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodmrros

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA)

! mrsos. Soil layer moisture
!!
    CASE ('mrsos')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing mrsos...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic4D)) DEALLOCATE(Rdiagnostic4D)
      ALLOCATE(Rdiagnostic4D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),           &
        DimMatInputs(1,4)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic4D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic4D=gen_result6D(:,:,:,:,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodmrsos: SELECT CASE (diagcom%method)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodmrsos

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic4D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)), halfdim(DimMatInputs(1,4)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic4D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic4D)

! pr. Precipitation flux
!!
    CASE ('pr')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing pr...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodpr: SELECT CASE (diagcom%method)
    
          CASE ('wrf')
	  
! Field is the flux from precipitation at given moment 'RAINCV' and 'RAINNCV'
            
! RAINCV, RAINNCV
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),2), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
      
            DO iinput=1,2
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
            ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
  	      " allocating 'gen_result6D'"

! Difference between fields
       
            indivmethod='sumall6D'
            CALL calc_method_gen6D(dbg, indivmethod, DimMatInputs(1,:), 2, rMATinputsA,         &
  	      diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

            Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodpr

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA)

! prc. Convective precipitation flux
!!
    CASE ('prc')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing prc...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodprc: SELECT CASE (diagcom%method)
    
          CASE ('wrf')
	  
! Field is the flux from precipitation at given moment 'RAINCV'
            
! RAINCV
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
      
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
            ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
  	      " allocating 'gen_result6D'"

! Difference between fields
       
            indivmethod='direct6D'
            CALL calc_method_gen6D(dbg, indivmethod, DimMatInputs(1,:), 1, rMATinputsA,         &
  	      diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

            Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodprc

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA)

! prls. Stratiform precipitation flux
!!
    CASE ('prls')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing prls...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodprls: SELECT CASE (diagcom%method)
    
          CASE ('wrf')
	  
! Field is the flux from precipitation at given moment 'RAINNCV'
            
! RAINNCV
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
      
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
            ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
  	      " allocating 'gen_result6D'"

! Difference between fields
       
            indivmethod='direct6D'
            CALL calc_method_gen6D(dbg, indivmethod, DimMatInputs(1,:), 1, rMATinputsA,         &
  	      diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

            Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodprls

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA)

! prsn. snow precipitation flux
!!
    CASE ('prsn')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing prsn...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodprsn: SELECT CASE (diagcom%method)
    
          CASE ('wrf')
	  
! Field is the flux from precipitation at given moment 'SNOW'
            
! SNOW
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
      
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

! Difference between fields
       
            Rdiagnostic3D=rMATinputsA(:,:,:,1,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodprsn

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA)

! prw. Total column water content
!!
    CASE ('prw')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing prw...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,4)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,1,:,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodprw: SELECT CASE (diagcom%method)

          CASE ('sigma_wrf')
! Vertical integration in eta sigma coordinates with wrfout variables
	  
! QVAPOR
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

! MU, MUB
            IF (ALLOCATED(rMATinputsB)) DEALLOCATE(rMATinputsB)
            ALLOCATE(rMATinputsB(DimMatInputs(2,1), DimMatInputs(2,2), DimMatInputs(2,3),       &
              DimMatInputs(2,4), DimMatInputs(2,5), DimMatInputs(2,6),2), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsB'"
       
            DO iinput=2,3
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(2,:), rMATinputsB(:,:,:,:,:,:,iinput-1))
            END DO

! DNW
            IF (ALLOCATED(rMATinputsC)) DEALLOCATE(rMATinputsC)
            ALLOCATE(rMATinputsC(DimMatInputs(4,1), DimMatInputs(4,2), DimMatInputs(4,3),       &
              DimMatInputs(4,4), DimMatInputs(4,5), DimMatInputs(4,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsC'"

            DO iinput=4,4
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(4,:), rMATinputsC(:,:,:,:,:,:,iinput-3))
            END DO

            CALL prw(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),              &
	      DimMatInputs(1,4), rMATinputsA(:,:,:,:,1,1,1), rMATinputsB(:,:,:,:,1,1,1)+        &
	      rMATinputsB(:,:,:,:,1,1,2), -rMATinputsC(:,:,1,1,1,1,1), Rdiagnostic3D)
	      
          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodprw

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA, rMATinputsB, rMATinputsC)

! ps. Surface pressure
!!
    CASE ('ps')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing ps...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodps: SELECT CASE (diagcom%method)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodps

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)

! psl. Sea level pressure
!!
    CASE ('psl')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing psl...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,4)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodpsl: SELECT CASE (diagcom%method)

          CASE ('wrf_Benjamin')
! Taking as reference a pressure high enough above topography, Benjamin and Miller (1990)

! P, PB, T, QVAPOR. Keep in mind (in wrf): T=T+300.

            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),4), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
            DO iinput=1,4
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

! Transformation of potential temperature to temperature

            IF (ALLOCATED(rMATinputsC)) DEALLOCATE(rMATinputsC)
            ALLOCATE(rMATinputsC(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsC'"
	    
	    CALL pot_t(dbg, dimMatInputs(1,1), dimMatInputs(1,2), dimMatInputs(1,3),            &
              dimMatInputs(1,4), rMATinputsA(:,:,:,:,:,:,1)+rMATinputsA(:,:,:,:,:,:,2),         &
	      rMATinputsA(:,:,:,:,:,:,3)+300., rMATinputsC(:,:,:,:,:,:,1))

! PSFC, HGT
            IF (ALLOCATED(rMATinputsB)) DEALLOCATE(rMATinputsB)
            ALLOCATE(rMATinputsB(DimMatInputs(5,1), DimMatInputs(5,2), DimMatInputs(5,3),       &
              DimMatInputs(5,4), DimMatInputs(5,5), DimMatInputs(5,6),2), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
            DO iinput=1,2
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(4+iinput,:),          &
	        DimMatInputs(5,:), rMATinputsB(:,:,:,:,:,:,iinput))
            END DO

            CALL SeaLevelPress(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),    &
	      DimMatInputs(1,4), diagcom%constant, rMATinputsA(:,:,:,:,:,:,1)+                  &
              rMATinputsA(:,:,:,:,:,:,2), rMATinputsB(:,:,:,:,:,:,1),rMATinputsB(:,:,:,:,:,:,2),&
	      rMATinputsC(:,:,:,:,:,:,1), rMATinputsA(:,:,:,:,:,:,4), Rdiagnostic3D)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodpsl

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA, rMATinputsB, rMATinputsC)

! rls. Net LW surface radiation
!!
    CASE ('rls')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing rls...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/deltaT

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodrls: SELECT CASE (diagcom%method)

          CASE ('wrf_accum')
! Field is the difference between ACLWDNB, ACLWUPB whom are accumulations and need to be
!   diferenciated
            
! ACLWDNB, ACLWUPB
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),2), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
             IF (ALLOCATED(rMATinputsB)) DEALLOCATE(rMATinputsB)
            ALLOCATE(rMATinputsB(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsB'"
      
            DO iinput=1,2
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
            ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
  	      " allocating 'gen_result6D'"

! Difference between fields
            rMATinputsB(:,:,:,:,:,:,1)=rMATinputsA(:,:,:,:,:,:,1)-rMATinputsA(:,:,:,:,:,:,2)
       
            indivmethod='diff_T6D'
            CALL calc_method_gen6D(dbg, indivmethod, DimMatInputs(1,:), 1, rMATinputsB,         &
  	      diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

            Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
            messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	    CALL diag_fatal(messg)

        END SELECT methodrls

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT
      
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA, rMATinputsB)

! rlds. LW surface flux radiation
!!
    CASE ('rlds')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing rlds...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodrlds: SELECT CASE (diagcom%method)

          CASE DEFAULT
            messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	    CALL diag_fatal(messg)

        END SELECT methodrlds

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT
      
      DEALLOCATE(Rdiagnostic3D)

! rlut. Top of atmosphere net LW radiation flux
!!
    CASE ('rlut')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing rlut...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)!/diff_dimtimes(dbg, oid, 4, 1, 2)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodrlut: SELECT CASE (diagcom%method)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodrlut

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT
      
      DEALLOCATE(Rdiagnostic3D)

! rss. Net SW surface radiation
!!
    CASE ('rss')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing rss...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodrss: SELECT CASE (diagcom%method)

          CASE ('wrf_accum')
! Field is the difference between ACSWDNB, ACSWUPB whom are accumulations and need to be
!   diferenciated
            
! ACSWDNB, ACSWUPB
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),2), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
             IF (ALLOCATED(rMATinputsB)) DEALLOCATE(rMATinputsB)
            ALLOCATE(rMATinputsB(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsB'"
      
            DO iinput=1,2
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
            ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
  	      " allocating 'gen_result6D'"

! Difference between fields
            rMATinputsB(:,:,:,:,:,:,1)=rMATinputsA(:,:,:,:,:,:,1)-rMATinputsA(:,:,:,:,:,:,2)
       
            indivmethod='diff_T6D'
            CALL calc_method_gen6D(dbg, indivmethod, DimMatInputs(1,:), 1, rMATinputsB,         &
  	      diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

            Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodrss

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT

      DEALLOCATE(Rdiagnostic3D)

! rsds. SW surface flux radiation
!!
    CASE ('rsds')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing rsds...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodrsds: SELECT CASE (diagcom%method)

          CASE DEFAULT
            messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	    CALL diag_fatal(messg)

        END SELECT methodrsds

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT
      
      DEALLOCATE(Rdiagnostic3D)

! rsdt. Incoming/downward SW at top of atmosphere radiation
!!
    CASE ('rsdt')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing rsdt...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodrsdt: SELECT CASE (diagcom%method)

          CASE ('wrf_accum')
! Field is the difference between ACSWDNT is accumulated and need to be time-diferenciated
            
! ACSWDNT
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
      
            DO iinput=1,1
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
            ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
  	      " allocating 'gen_result6D'"

! Difference between fields
      
            indivmethod='diff_T6D'
            CALL calc_method_gen6D(dbg, indivmethod, DimMatInputs(1,:), 1, rMATinputsA,         &
  	      diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

            Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodrsdt

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA)

! rst. Net SW at top of atmosphere radiation
!!
    CASE ('rst')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing rst...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodrst: SELECT CASE (diagcom%method)

          CASE ('wrf_accum')
! Field is the difference between ACSWDNT, ACSWUPT whom are accumulations and need to be
!   diferenciated
            
! ACSWDNT, ACSWUPT
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
            ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),2), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
             IF (ALLOCATED(rMATinputsB)) DEALLOCATE(rMATinputsB)
            ALLOCATE(rMATinputsB(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsB'"
      
            DO iinput=1,2
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO

            IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
            ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
  	      " allocating 'gen_result6D'"

! Difference between fields
            rMATinputsB(:,:,:,:,:,:,1)=rMATinputsA(:,:,:,:,:,:,1)-rMATinputsA(:,:,:,:,:,:,2)
       
            indivmethod='diff_T6D'
            CALL calc_method_gen6D(dbg, indivmethod, DimMatInputs(1,:), 1, rMATinputsB,         &
  	      diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

! Time-Interval for flux value...
            Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)/diff_dimtimes(dbg, oid, 4, 1, 2)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodrst

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA, rMATinputsB)

! sst. Sea surface temperature
!!
    CASE ('sst')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing sst...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)
	WHERE (Rdiagnostic3D == 0.) Rdiagnostic3D=missingval

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodsst: SELECT CASE (diagcom%method)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodsst

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)

! tas. 2m temperature
!!
    CASE ('tas')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing tas...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodtas: SELECT CASE (diagcom%method)
    
          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodtas

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D, (/1, 1, 1, 1/), (/DimMatInputs(1,1), &
	     DimMatInputs(1,2), 1, DimMatInputs(1,3)/))
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)

! tdps. dew point temperature at 2m
!!
    CASE ('tdps')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing tdps...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodtdps: SELECT CASE (diagcom%method)

          CASE ('Tetens')
! Tetens equation (Tetens, 1930)
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
              ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),     &
                DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),3), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
! Q2, PSFC, T2
            DO iinput=1,3
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO
	  
            CALL tdps(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),             &
              rMATinputsA(:,:,:,1,1,1,1), rMATinputsA(:,:,:,1,1,1,2),                           &
	      rMATinputsA(:,:,:,1,1,1,3), Rdiagnostic3D)

          CASE ('its90')
! ITS-90 formulation http://www.decatur.de/javascript/dew/resources/its90formulas.pdf
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
              ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),     &
                DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),3), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
! Q2, PSFC, T2
            DO iinput=1,3
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO
	  
            CALL tdps_its90(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              rMATinputsA(:,:,:,1,1,1,1), rMATinputsA(:,:,:,1,1,1,2),                           &
	      rMATinputsA(:,:,:,1,1,1,3), Rdiagnostic3D)

          CASE ('Bolton')
! Bolton equation (Bolton, 1980, Mont. Weather Rev.)
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
              ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),     &
                DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),3), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
! Q2, PSFC, T2
            DO iinput=1,3
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO
	  
            CALL tdps(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),             &
              rMATinputsA(:,:,:,1,1,1,1), rMATinputsA(:,:,:,1,1,1,2),                           &
	      rMATinputsA(:,:,:,1,1,1,3), Rdiagnostic3D)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodtdps

      END IF
       
      IF (dbg >= 75) THEN
        PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
        PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),               &
          halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
      ENDIF

! Writting diagnostic in output file
      SELECT CASE (diagcom%type)
        CASE (5)
!       Real diagnostic
          rcode = nf90_put_var (oid, jvar, Rdiagnostic3D, (/1,1,1,1/), (/DimMatInputs(1,1),     &
	    DimMatInputs(1,2), 1, DimMatInputs(1,3)/) )
          CALL error_nc(section, rcode)
	CASE DEFAULT
	  messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

      END SELECT

      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(rMATinputsA)

! ts. Surface temperature
!!
    CASE ('ts')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing ts...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic3D)) DEALLOCATE(Rdiagnostic3D)
      ALLOCATE(Rdiagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodts: SELECT CASE (diagcom%method)

          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodts

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic3D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic3D)
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic3D)

! uas. 2m westward wind
!!
    CASE ('uas')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing uas...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic4D)) DEALLOCATE(Rdiagnostic4D)
      ALLOCATE(Rdiagnostic4D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3), 2),       &
        STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D(:,:,:)=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methoduas: SELECT CASE (diagcom%method)
     
          CASE ('wrf10')
! Winds in wrfout are not Earth referenciated. U10, V10. SINA, COSA wrfout variables contains
! values to make rotation to surface Earth reference of winds
	  
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
              ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),     &
                DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),4), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
! U10, V10, SINA, COSA

            DO iinput=1,4
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO
	  
            CALL Earth_wind(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              rMATinputsA(:,:,:,:,:,:,1), rMATinputsA(:,:,:,:,:,:,2), rMATinputsA(:,:,:,:,:,:,3)&
	      ,  rMATinputsA(:,:,:,:,:,:,4), Rdiagnostic4D(:,:,:,1), Rdiagnostic4D(:,:,:,2))
	      
          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methoduas

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic4D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)), 1)
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic4D(:,:,:,1),(/1,1,2,1/),                 &
	     (/DimMatInputs(1,1), DimMatInputs(1,2), 1, DimMatInputs(1,3) /))
           CALL error_nc(section, rcode)

	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
           CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic4D)

! vas. 2m westward wind
!!
    CASE ('vas')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing vas...'
        PRINT *,'  ----- ---- --- -- -'
      END IF

! Preparation of diagnostic result
      IF (ALLOCATED(Rdiagnostic4D)) DEALLOCATE(Rdiagnostic4D)
      ALLOCATE(Rdiagnostic4D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3), 2),       &
        STAT=ierr)
      IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'Rdiagnostic3D'"

! method computation
!!

      IF (ANY(diagcom%method == generic_calcs6D)) THEN 
!  Variable result from generic method
!!
        Rdiagnostic3D(:,:,:)=gen_result6D(:,:,:,1,1,1)

      ELSE
! Creation of specific matrix for computation of diagnostic according to the NON-generic method. 
! Necessary input matrixs can be of different rank and/or shape MATinputs[A/F] (if necessary)

        methodvas: SELECT CASE (diagcom%method)
     
          CASE ('wrf10')
! Winds in wrfout are not Earth referenciated. U10, V10. SINA, COSA wrfout variables contains
! values to make rotation to surface Earth reference of winds
	  
            IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
              ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),     &
                DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),4), STAT=ierr)
            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating "//       &
	      "'rMATinputsA'"
       
! U10, V10, SINA, COSA
            DO iinput=1,4
              CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),            &
	        DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
            END DO
	  
            CALL Earth_wind(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),       &
              rMATinputsA(:,:,:,:,:,:,1), rMATinputsA(:,:,:,:,:,:,2), rMATinputsA(:,:,:,:,:,:,3)&
	      ,  rMATinputsA(:,:,:,:,:,:,4), Rdiagnostic4D(:,:,:,1), Rdiagnostic4D(:,:,:,2))
	      
          CASE DEFAULT
	     messg="Nothing to do with '"//TRIM(diagcom%method)//"' method"
	     CALL diag_fatal(messg)

        END SELECT methodvas

       END IF
       
       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagcom%name)//' idvar:',jvar
         PRINT *,'  1/2 example value:', Rdiagnostic4D(halfdim(DimMatInputs(1,1)),              &
           halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)), 2)
       ENDIF

! Writting diagnostic in output file
       SELECT CASE (diagcom%type)
         CASE (5)
!        Real diagnostic
           rcode = nf90_put_var (oid, jvar, Rdiagnostic4D(:,:,:,2),(/1,1,2,1/),                 &
	     (/DimMatInputs(1,1), DimMatInputs(1,2), 1, DimMatInputs(1,3) /))
           CALL error_nc(section, rcode)
	 CASE DEFAULT
	   messg="Nothing to write with variable type '"//CHAR(diagcom%type+48)//"'"
          CALL diag_fatal(messg)

       END SELECT

      DEALLOCATE(Rdiagnostic4D)

!!!!!!!!!!!!!!!!!!!!!!!!!!!! NO VARIABLE DEFINED

    CASE DEFAULT

      messg="Nothing to do with '"//TRIM(diagcom%name)//"' variable !!"
      CALL diag_fatal(messg)

    END SELECT diag_var

    IF (ALLOCATED(gen_Result6D)) DEALLOCATE(gen_Result6D)
    IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
    IF (ALLOCATED(DimMatInputs)) DEALLOCATE (DimMatInputs)

  END DO compute_diags

  rcode = nf90_close(oid)
  RETURN
END SUBROUTINE com_diagnostics

END MODULE module_com_diagnostics
