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
  INTEGER, ALLOCATABLE, DIMENSION(:,:)                   :: DimMatInputs
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)                  :: diagnostic4D
  REAL, ALLOCATABLE, DIMENSION(:,:,:)                    :: diagnostic3D
  REAL, ALLOCATABLE, DIMENSION(:,:)                      :: diagnostic2D
  INTEGER, ALLOCATABLE, DIMENSION(:,:)                   :: foundvariables
  INTEGER                                                :: Ndimsin
  INTEGER                                                :: ierr, oid, jvar, rcode
  INTEGER                                                :: lastjvar
  INTEGER                                                :: Ndimsdiag
  INTEGER, DIMENSION(6)                                  :: jshape
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: DIAGvariables
  CHARACTER(LEN=50)                                      :: varname, units, diagname, stdesc,   &
    coordinates
  CHARACTER(LEN=250)                                     :: longdesc
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!!! Variables
! ifiles: vector with input files
! Ninfiles: number of infiles
! car_[x/y]: Whether x and y coordinates are cartesian ones
! Diags: vector with diagnostic variables
! Ndiags: number of diagnostic variables
! delta_[X/Y/T]: respective delta in direction X/Y [m] and time T [s]
! dimsIname: name of 4-basic dimensions (X, Y, Z, T) in input file
! dimsOname: name of dimension in output file
! NdimsO: number of dimensions in output file
! ofile: outputfile
! g_att_file: file from which all global attributes will putted in output file
! Ndimsin: number of variables of input file
! diagnostic4D: 4D computed diagnostic
! diagnostic4D: 3D computed diagnostic
! diagnostic4D: 2D computed diagnostic
! DIAGvariables: vector with name of wanted variables
! foundvariables: matrix of location of desired variables (line as variable number according to 
!   DIAGvariables) 
!   col1: number of files (according to Ninfiles)    col2: id variable in from file of 'col1'
! Nvardiag: number of variables to compute the specific diagnostic
! rMATinputs[A:F]: real matrix with DIAGvariables values (7D; 6D netCDF variable dimensions, 
!   #order of Nvardiag with given dimensions) of A to F differend shapes and number of dimensions
! DimMatInputs: matrix with dimensions of DIAGvariables values (7D; 6D netCDF dimensions, #order 
!   of Nvardiag)
! jvar: number of variable in output file
! lastjvar: number of the last variable of the output file
! varname: name of diagnostic variable
! units: units of variable
! longdesc: long description attribute of variable
! stdesc: standard CF convetion name of variable
! dv[1/2/3/4]: 4D diagnostic range of 4 dimensions
  
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
  jshape=1
  jvar=nc_last_idvar(dbg, oid)
    
  compute_diags: DO idiag=1, Ndiags
   diagname=(diags(idiag))
   diag_var: SELECT CASE(diagname)

! CLT. Total Cloud fraction
!!
    CASE ('CLT')

! Setting diagnstic id in outputfile
       IF (dbg >= 75) PRINT *,'  Computing CLT...'

! Looking for diagnostic information in 'variables_diagnostics.inf'
       CALL diagnostic_inf_Ninvar(diagname, dbg, Nvardiag)
       IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
       ALLOCATE(DIAGvariables(Nvardiag), STAT=ierr)
       IF (ierr /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//" allocating 'DIAGvariables'"

       CALL diagnostic_inf(dbg, diagname, Nvardiag, DIAGvariables, Ndimsdiag, jshape,           &
         longdesc, stdesc, units)

! Setting matrixs with diagnostic input variables file location, id and particular dimensions
       IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
       ALLOCATE(foundvariables(Nvardiag, 2))
       
       IF (ALLOCATED(DimMatInputs)) DEALLOCATE(DimMatInputs)
       ALLOCATE(DimMatInputs(Nvardiag,6))
       
       CALL search_variables(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, foundvariables,    &
         DimMatInputs)

! Creation of specific matrix for computation of diagnostic. Necessary input matrixs can be of 
! different rank and/or shape MATinputs[A/F] (if necessary)

       IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
       ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),            &
         DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsA'"
       
       DO iinput=1,1
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),                 &
	   DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
       END DO

! Defining new diagnostic variable in output file
       IF (.NOT.exists_var(oid, diagname)) THEN
         jvar = jvar + 1
         coordinates='lon lat'
         CALL def_nc_var(oid, jvar, diagname, 5, 3, jshape, "XY ", longdesc, stdesc, units, "-",&
           coordinates, dbg)
       ELSE
         rcode = nf_inq_varid (oid, diagname, jvar)
       END IF

! Preparation and compuatation of diagnostic
       IF (ALLOCATED(diagnostic3D)) DEALLOCATE(diagnostic3D)
       ALLOCATE(diagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,4)), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'diagnostic3D'"

       CALL clt(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3), DimMatInputs(1,4),&
         rMATinputsA(:,:,:,:,1,1,1), diagnostic3D)

       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagname)//' idvar:',jvar
         PRINT *,'  1/2 example value:', diagnostic3D(halfdim(DimMatInputs(1,1)),               &
	    halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
       ENDIF

! Writting diagnostic in output file
       rcode = nf_put_var_real (oid, jvar, diagnostic3D)
       CALL error_nc(section, rcode)

       DEALLOCATE(DIAGvariables)
       DEALLOCATE(diagnostic3D)
       DEALLOCATE(foundvariables, DimMatInputs)
       DEALLOCATE(rMATinputsA)
             
! HURS. 2m relative humidity
!!
    CASE ('HURS')

! Setting diagnstic id in outputfile
       IF (dbg >= 75) PRINT *,'  Computing HURS...'

! Looking for diagnostic information in 'variables_diagnostics.inf'
       CALL diagnostic_inf_Ninvar(diagname, dbg, Nvardiag)
       IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
       ALLOCATE(DIAGvariables(Nvardiag), STAT=ierr)
       IF (ierr /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//" allocating 'DIAGvariables'"

       CALL diagnostic_inf(dbg, diagname, Nvardiag, DIAGvariables, Ndimsdiag, jshape,           &
         longdesc, stdesc, units)

! Setting matrixs with diagnostic input variables file location, id and particular dimensions
       IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
       ALLOCATE(foundvariables(Nvardiag, 2))
       
       IF (ALLOCATED(DimMatInputs)) DEALLOCATE(DimMatInputs)
       ALLOCATE(DimMatInputs(Nvardiag,6))
       
       CALL search_variables(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, foundvariables,    &
         DimMatInputs)

! Creation of specific matrix for computation of diagnostic. Necessary input matrixs can be of 
! different rank and/or shape MATinputs[A/F] (if necessary)

       IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
       ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),            &
         DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),3), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsA'"
       
       DO iinput=1,3
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),                 &
	   DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
       END DO

! Defining new diagnostic variable in output file
       IF (.NOT.exists_var(oid, diagname)) THEN
         jvar = jvar + 1
         coordinates='lon lat'
         CALL def_nc_var(oid, jvar, diagname, 5, 3, jshape, "XY ", longdesc, stdesc, units, "-",&
           coordinates, dbg)
       ELSE
         rcode = nf_inq_varid (oid, diagname, jvar)
       END IF

! Preparation and compuatation of diagnostic
       IF (ALLOCATED(diagnostic3D)) DEALLOCATE(diagnostic3D)
       ALLOCATE(diagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'diagnostic3D'"

       CALL hurs(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),                  &
         rMATinputsA(:,:,:,:,1,1,1), rMATinputsA(:,:,:,:,2,1,1), rMATinputsA(:,:,:,:,3,1,1),    &
         diagnostic3D)

       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagname)//' idvar:',jvar
         PRINT *,'  1/2 example value:', diagnostic3D(halfdim(DimMatInputs(1,1)),               &
	   halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
       rcode = nf_put_var_real (oid, jvar, diagnostic3D)
       CALL error_nc(section, rcode)

       DEALLOCATE(DIAGvariables)
       DEALLOCATE(diagnostic3D)
       DEALLOCATE(foundvariables, DimMatInputs)
       DEALLOCATE(rMATinputsA)

! MRSO. Total soil moisture
!!
    CASE ('MRSO')

! Setting diagnstic id in outputfile
       IF (dbg >= 75) PRINT *,'  Computing MRSO...'

! Looking for diagnostic information in 'variables_diagnostics.inf'
       CALL diagnostic_inf_Ninvar(diagname, dbg, Nvardiag)
       IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
       ALLOCATE(DIAGvariables(Nvardiag), STAT=ierr)
       IF (ierr /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//" allocating 'DIAGvariables'"

       CALL diagnostic_inf(dbg, diagname, Nvardiag, DIAGvariables, Ndimsdiag, jshape,           &
         longdesc, stdesc, units)

! Setting matrixs with diagnostic input variables file location, id and particular dimensions
       IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
       ALLOCATE(foundvariables(Nvardiag, 2))
       
       IF (ALLOCATED(DimMatInputs)) DEALLOCATE(DimMatInputs)
       ALLOCATE(DimMatInputs(Nvardiag,6))
       
       CALL search_variables(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, foundvariables,    &
         DimMatInputs)

! Creation of specific matrix for computation of diagnostic. Necessary input matrixs can be of 
! different rank and/or shape MATinputs[A/F] (if necessary)

       IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
       ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),            &
         DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsA'"
       
       DO iinput=1,1
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),                 &
	   DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
       END DO

       IF (ALLOCATED(rMATinputsB)) DEALLOCATE(rMATinputsB)
       ALLOCATE(rMATinputsB(DimMatInputs(2,1), DimMatInputs(2,2), DimMatInputs(2,3),            &
         DimMatInputs(2,4), DimMatInputs(2,5), DimMatInputs(2,6),1), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsB'"
       DO iinput=1,1
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput+1,:),               &
	   DimMatInputs(2,:), rMATinputsB(:,:,:,:,:,:,iinput))
       END DO

! Defining new diagnostic variable in output file
       IF (.NOT.exists_var(oid, diagname)) THEN
         jvar = jvar + 1
         coordinates='lon lat'
         CALL def_nc_var(oid, jvar, diagname, 5, 3, jshape, "XY ", longdesc, stdesc, units, "-",&
           coordinates, dbg)
       ELSE
         rcode = nf_inq_varid (oid, diagname, jvar)
       END IF

! Preparation and compuatation of diagnostic
       IF (ALLOCATED(diagnostic3D)) DEALLOCATE(diagnostic3D)
       ALLOCATE(diagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,4)), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'diagnostic3D'"

       CALL mrso(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),DimMatInputs(1,4),&
         rMATinputsA(:,:,:,:,1,1,1), rMATinputsB(:,:,1,1,1,1,1), diagnostic3D)

       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagname)//' idvar:',jvar
         PRINT *,'  1/2 example value:', diagnostic3D(halfdim(DimMatInputs(1,1)),               &
	   halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
       ENDIF

! Writting diagnostic in output file
!       rcode = nf_put_vara_real (oid, jvar, start_dims, dims_out-1, diagnostic3D)
       rcode = nf_put_var_real (oid, jvar, diagnostic3D)
       CALL error_nc(section, rcode)

       DEALLOCATE(DIAGvariables)
       DEALLOCATE(diagnostic3D)
       DEALLOCATE(foundvariables, DimMatInputs)
       DEALLOCATE(rMATinputsA, rMATinputsB)

! PRW. Total column water content
!!
    CASE ('PRW')

! Setting diagnstic id in outputfile
       IF (dbg >= 75) PRINT *,'  Computing PRW...'

! Looking for diagnostic information in 'variables_diagnostics.inf'
       CALL diagnostic_inf_Ninvar(diagname, dbg, Nvardiag)
       IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
       ALLOCATE(DIAGvariables(Nvardiag), STAT=ierr)
       IF (ierr /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//" allocating 'DIAGvariables'"

       CALL diagnostic_inf(dbg, diagname, Nvardiag, DIAGvariables, Ndimsdiag, jshape,           &
         longdesc, stdesc, units)

! Setting matrixs with diagnostic input variables file location, id and particular dimensions
       IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
       ALLOCATE(foundvariables(Nvardiag, 2))
       
       IF (ALLOCATED(DimMatInputs)) DEALLOCATE(DimMatInputs)
       ALLOCATE(DimMatInputs(Nvardiag,6))
       
       CALL search_variables(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, foundvariables,    &
         DimMatInputs)

! Creation of specific matrix for computation of diagnostic. Necessary input matrixs can be of 
! different rank and/or shape MATinputs[A/F] (if necessary)

! QVAPOR
       IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
       ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),            &
         DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsA'"
       
       DO iinput=1,1
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),                 &
	   DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
       END DO

! MU, MUB
       IF (ALLOCATED(rMATinputsB)) DEALLOCATE(rMATinputsB)
       ALLOCATE(rMATinputsB(DimMatInputs(2,1), DimMatInputs(2,2), DimMatInputs(2,3),            &
         DimMatInputs(2,4), DimMatInputs(2,5), DimMatInputs(2,6),2), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsB'"
       
       DO iinput=2,3
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),                 &
	   DimMatInputs(2,:), rMATinputsB(:,:,:,:,:,:,iinput-1))
       END DO

! DNW
       IF (ALLOCATED(rMATinputsC)) DEALLOCATE(rMATinputsC)
       ALLOCATE(rMATinputsC(DimMatInputs(4,1), DimMatInputs(4,2), DimMatInputs(4,3),            &
         DimMatInputs(4,4), DimMatInputs(4,5), DimMatInputs(4,6),1), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsC'"

       DO iinput=4,4
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),                 &
	   DimMatInputs(4,:), rMATinputsC(:,:,:,:,:,:,iinput-3))
       END DO
       
! Defining new diagnostic variable in output file
       IF (.NOT.exists_var(oid, diagname)) THEN
         jvar = jvar + 1
         coordinates='lon lat'
         CALL def_nc_var(oid, jvar, diagname, 5, 3, jshape, "XY ", longdesc, stdesc, units, "-",&
           coordinates, dbg)
       ELSE
         rcode = nf_inq_varid (oid, diagname, jvar)
       END IF

! Preparation and compuatation of diagnostic
       IF (ALLOCATED(diagnostic3D)) DEALLOCATE(diagnostic3D)
       ALLOCATE(diagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,4)), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'diagnostic3D'"

       CALL prw(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3), DimMatInputs(1,4),&
         rMATinputsA(:,:,:,:,1,1,1), rMATinputsB(:,:,:,:,2,1,1)+rMATinputsB(:,:,:,:,3,1,1),     &
	 -rMATinputsC(:,:,1,1,1,1,1), diagnostic3D)

       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagname)//' idvar:',jvar
         PRINT *,'  1/2 example value:', diagnostic3D(halfdim(DimMatInputs(1,1)),               &
	   halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,4)))
       ENDIF

! Writting diagnostic in output file
       rcode = nf_put_var_real (oid, jvar, diagnostic3D)
       CALL error_nc(section, rcode)

       DEALLOCATE(DIAGvariables)
       DEALLOCATE(diagnostic3D)
       DEALLOCATE(foundvariables, DimMatInputs)
       DEALLOCATE(rMATinputsA, rMATinputsB, rMATinputsC)       

! TDPS. dew point temperature at 2m
!!
    CASE ('TDPS')

! Setting diagnstic id in outputfile
       IF (dbg >= 75) PRINT *,'  Computing TDPS...'

! Looking for diagnostic information in 'variables_diagnostics.inf'
       CALL diagnostic_inf_Ninvar(diagname, dbg, Nvardiag)
       IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
       ALLOCATE(DIAGvariables(Nvardiag), STAT=ierr)
       IF (ierr /= 0) PRINT *,TRIM(errmsg)//' in '//TRIM(section)//" allocating 'DIAGvariables'"

       CALL diagnostic_inf(dbg, diagname, Nvardiag, DIAGvariables, Ndimsdiag, jshape,           &
         longdesc, stdesc, units)

! Setting matrixs with diagnostic input variables file location, id and particular dimensions
       IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
       ALLOCATE(foundvariables(Nvardiag, 2))
       
       IF (ALLOCATED(DimMatInputs)) DEALLOCATE(DimMatInputs)
       ALLOCATE(DimMatInputs(Nvardiag,6))
       
       CALL search_variables(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, foundvariables,    &
         DimMatInputs)

! Creation of specific matrix for computation of diagnostic. Necessary input matrixs can be of 
! different rank and/or shape MATinputs[A/F] (if necessary)

       IF (ALLOCATED(rMATinputsA)) DEALLOCATE(rMATinputsA)
       ALLOCATE(rMATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),            &
         DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),2), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'rMATinputsA'"
       
       DO iinput=1,2
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),                 &
	   DimMatInputs(1,:), rMATinputsA(:,:,:,:,:,:,iinput))
       END DO

! Defining new diagnostic variable in output file
       IF (.NOT.exists_var(oid, diagname)) THEN
         jvar = jvar + 1
         coordinates='lon lat'
         CALL def_nc_var(oid, jvar, diagname, 5, 3, jshape, "XY ", longdesc, stdesc, units, "-",&
           coordinates, dbg)
       ELSE
         rcode = nf_inq_varid (oid, diagname, jvar)
       END IF

! Preparation and compuatation of diagnostic
       IF (ALLOCATED(diagnostic3D)) DEALLOCATE(diagnostic3D)
       ALLOCATE(diagnostic3D(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3)), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//'in section '//TRIM(section)//" allocating 'diagnostic3D'"

       CALL tdps(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),                   &
         rMATinputsA(:,:,:,:,1,1,1), rMATinputsA(:,:,:,:,2,1,1), diagnostic3D)

       IF (dbg >= 75) THEN
         PRINT *,'  VAR: '//TRIM(diagname)//' idvar:',jvar
         PRINT *,'  1/2 example value:', diagnostic3D(halfdim(DimMatInputs(1,1)),                &
	   halfdim(DimMatInputs(1,2)), halfdim(DimMatInputs(1,3)))
       ENDIF

! Writting diagnostic in output file
!       rcode = nf_put_vara_real (oid, jvar, start_dims, dims_out-1, diagnostic3D)
       rcode = nf_put_var_real (oid, jvar, diagnostic3D)
       CALL error_nc(section, rcode)

       DEALLOCATE(DIAGvariables)
       DEALLOCATE(diagnostic3D)
       DEALLOCATE(foundvariables, DimMatInputs)
       DEALLOCATE(rMATinputsA)


    END SELECT diag_var

  END DO compute_diags

  rcode = nf_close(oid)
  RETURN
END SUBROUTINE com_diagnostics

END MODULE module_com_diagnostics
