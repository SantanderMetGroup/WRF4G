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
    CASE ('CLT')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing CLT...'
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
       DEALLOCATE(foundvariables, DimMatInputs)
       DEALLOCATE(rMATinputsA)

! DTH. Thickness of depth layers
!!
    CASE ('DTH')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing DTH...'
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

       IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
       DEALLOCATE(Rdiagnostic2D)
       DEALLOCATE(foundvariables, DimMatInputs)

! EVSPSBL. Surface water evaporation
!!
    CASE ('EVSPSBL')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing EVSPSBL...'
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

!            IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
!            ALLOCATE(gen_result6D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),      &
!              DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6)), STAT=ierr)
!            IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//                       &
!  	      " allocating 'gen_result6D'"

! Difference between fields
       
!            indivmethod='direct6D'
!            CALL calc_method_gen6D(dbg, indivmethod, DimMatInputs(1,:), 1, rMATinputsA,         &
!  	      diagcom%constant, diagcom%Noptions, diagcom%options, gen_result6D)

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

      IF (ALLOCATED(gen_Result6D)) DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)
             
! HFLS. Surface latent heat flux
!!
    CASE ('HFLS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing HFLS...'
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

      IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! HFSS. Surface sensible heat flux
!!
    CASE ('HFSS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing HFSS...'
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

      IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! HURS. 2m relative humidity
!!
    CASE ('HURS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing HURS...'
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
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)

! HUSS. 2m specific humidty
!!
    CASE ('HUSS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing HUSS...'
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

      DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! MRSO. Total soil moisture
!!
    CASE ('MRSO')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing MRSO...'
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

          CASE ('standard')
	  
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
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA, rMATinputsB)

! MRROS. Surface runoff flux
!!
    CASE ('MRROS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing MRROS...'
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

      IF (ALLOCATED(gen_Result6D)) DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)

! MRSOS. Soil layer moisture
!!
    CASE ('MRSOS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing MRSOS...'
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

      IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
      DEALLOCATE(Rdiagnostic4D)
      DEALLOCATE(foundvariables, DimMatInputs)

! PR. Precipitation flux
!!
    CASE ('PR')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing PR...'
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

      IF (ALLOCATED(gen_Result6D)) DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)

! PRC. Convective precipitation flux
!!
    CASE ('PRC')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing PRC...'
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

      DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)

! PRLS. Stratiform precipitation flux
!!
    CASE ('PRLS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing PRLS...'
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

      IF (ALLOCATED(gen_Result6D)) DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)

! PRW. Total column water content
!!
    CASE ('PRW')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing PRW...'
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
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA, rMATinputsB, rMATinputsC)

! PS. Surface pressure
!!
    CASE ('PS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing PS...'
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

      IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! RLS. Net LW surface radiation
!!
    CASE ('RLS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing RLS...'
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
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA, rMATinputsB)

! RLDS. LW surface flux radiation
!!
    CASE ('RLDS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing RLDS...'
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
      
      IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! RLUT. Top of atmosphere net LW radiation flux
!!
    CASE ('RLUT')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing RLUT...'
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
      DEALLOCATE(foundvariables, DimMatInputs)

! RSS. Net SW surface radiation
!!
    CASE ('RSS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing RSS...'
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
      DEALLOCATE(foundvariables, DimMatInputs)

! RSDS. SW surface flux radiation
!!
    CASE ('RSDS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing RSDS...'
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
      
      IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! RSDT. Incoming/downward SW at top of atmosphere radiation
!!
    CASE ('RSDT')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing RSDT...'
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
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)

! RST. Net SW at top of atmosphere radiation
!!
    CASE ('RST')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing RST...'
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
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA, rMATinputsB)

! SST. Sea surface temperature
!!
    CASE ('SST')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing SST...'
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

      IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! TAS. 2m temperature
!!
    CASE ('TAS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing TAS...'
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

      DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! TDPS. dew point temperature at 2m
!!
    CASE ('TDPS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing TDPS...'
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
      DEALLOCATE(foundvariables, DimMatInputs)
      DEALLOCATE(rMATinputsA)

! TS. Surface temperature
!!
    CASE ('TS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing TS...'
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

      IF (ALLOCATED(gen_result6D)) DEALLOCATE(gen_result6D)
      DEALLOCATE(Rdiagnostic3D)
      DEALLOCATE(foundvariables, DimMatInputs)

! UAS. 2m westward wind
!!
    CASE ('UAS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing UAS...'
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

      IF (ALLOCATED(gen_Result6D)) DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic4D)
      DEALLOCATE(foundvariables, DimMatInputs)

! VAS. 2m westward wind
!!
    CASE ('VAS')

      IF (dbg >= 75) THEN
        PRINT *,'  ----- ---- --- -- -'
        PRINT *,'  Computing VAS...'
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

      IF (ALLOCATED(gen_Result6D)) DEALLOCATE(gen_Result6D)
      DEALLOCATE(Rdiagnostic4D)
      DEALLOCATE(foundvariables, DimMatInputs)

!!!!!!!!!!!!!!!!!!!!!!!!!!!! NO VARIABLE DEFINED

    CASE DEFAULT

      messg="Nothing to do with '"//TRIM(diagcom%name)//"' variable !!"
      CALL diag_fatal(messg)

    END SELECT diag_var

  END DO compute_diags

  rcode = nf90_close(oid)
  RETURN
END SUBROUTINE com_diagnostics

END MODULE module_com_diagnostics
