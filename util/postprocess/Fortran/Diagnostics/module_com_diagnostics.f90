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

SUBROUTINE com_diagnostics(dbg, ifiles, Ninfiles, dx, dy, dz, dt, Diags, Ndiags, deltaX, deltaY,&
  dimsVname, ofile, g_att_file, car_x, car_y)
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
  INTEGER, INTENT(IN)                                    :: dx, dy, dz, dt
  INTEGER, INTENT(IN)                                    :: Ndiags
  CHARACTER(LEN=250), DIMENSION(Ndiags), INTENT(IN)      :: Diags
  CHARACTER(LEN=500), INTENT(IN)                         :: ofile
  REAL, INTENT(IN)                                       :: deltaX, deltaY
  CHARACTER(LEN=50), DIMENSION(4), INTENT(IN)            :: dimsVname
  LOGICAL, INTENT(IN)                                    :: car_x, car_y

! Local vars
  INTEGER                                                :: i, idiag, iinput
  INTEGER                                                :: Nvardiag
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:,:,:,:)            :: MATinputsA, MATinputsB, MATinputsC, &
    MATinputsD, MATinputsE, MATinputsF 
  INTEGER, ALLOCATABLE, DIMENSION(:,:)                   :: DimMatInputs
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)                  :: diagnostic4D
  REAL, ALLOCATABLE, DIMENSION(:,:,:)                    :: diagnostic3D
  REAL, ALLOCATABLE, DIMENSION(:,:)                      :: diagnostic2D
  INTEGER, ALLOCATABLE, DIMENSION(:,:)                   :: foundvariables
  INTEGER                                                :: ierr, oid, jvar, rcode
  INTEGER                                                :: Ndimsdiag
  INTEGER, DIMENSION(4)                                  :: start_dims, dims_out, dims_in, dimido
  INTEGER, DIMENSION(6)                                  :: jshape
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: DIAGvariables
  CHARACTER(LEN=50)                                      :: varname, units, diagname, stdesc
  CHARACTER(LEN=20)                                      :: coordinates
  CHARACTER(LEN=250)                                     :: longdesc
!  CHARACTER(LEN=80)                                      :: nf_strerror
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!!! Variables
! ifiles: vector with input files
! Ninfiles: number of infiles
! car_[x/y]: Whether x and y coordinates are cartesian ones
! d[x/y/z/t]: 4D dimensions input range
! Diags: vector with diagnostic variables
! Ndiags: number of diagnostic variables
! ofile: outputfile
! g_att_file: file from which all global attributes will putted in output file
! dimido: assigned id of the 4-dimensions (dx, dy, dz, dt)
! diagnostic4D: 4D computed diagnostic
! diagnostic4D: 3D computed diagnostic
! diagnostic4D: 2D computed diagnostic
! DIAGvariables: vector with name of wanted variables
! foundvariables: matrix of location of desired variables (line as variable number according to 
!   DIAGvariables) 
!   col1: number of files (according to Ninfiles)    col2: id variable in from file of 'col1'
! Nvardiag: number of variables to compute the specific diagnostic
! MATinputs[A:F]: matrix with DIAGvariables values (7D; 6D netCDF dimensions, #order of Nvardiag)
!   of A to F differend shapes and number of dimensions
! DimMatInputs: matrix with dimensions of DIAGvariables values (7D; 6D netCDF dimensions, #order 
!   of Nvardiag)
! jvar: mnumber of variable in output file
! varname: name of diagnostic variable
! units: units of variable
! longdesc: long description attribute of variable
! stdesc: standard CF convetion name of variable
! dv[1/2/3/4]: 4D diagnostic range of 4 dimensions
  
  section="'module_com_diagnostics'"

  IF (dbg >= 100) PRINT *,'Diagnostics '//TRIM(section)//'... .. .'

! output file creation & dimensions
!!
  CALL create_output(dbg, ofile, dx, dy, dz, dt, dimsVname, ifiles(g_att_file), car_x, car_y)
  STOP

!  rcode = nf_open(TRIM(ofile), OR(NF_WRITE), oid)
  rcode = nf_create(TRIM(ofile), NF_CLOBBER, oid)

!!!
!!
! Computing diagnostics
!!
!!!
  IF (dbg >= 100) PRINT *,"Computing diagnostics ..."
  start_dims=1
  jshape=1
  jvar=1
  dims_in=RESHAPE((/dx, dy, dz, dt/),(/4/))

  compute_diags: DO idiag=1, Ndiags
   diagname=(diags(idiag))
   diag_var: SELECT CASE(diagname)

! PV. Potential Vorticity
!!
     CASE('PV')
       jvar = jvar + 1
       IF (dbg >= 75) PRINT *,'Computing PV...'

       CALL diagnostic_inf_Ninvar(diagname, dbg, Nvardiag)
       IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
       ALLOCATE(DIAGvariables(Nvardiag), STAT=ierr)
       IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

       CALL diagnostic_inf(dbg, diagname, Nvardiag, DIAGvariables, Ndimsdiag, jshape,           &
         longdesc, stdesc, units)

       DO i=1,Ndimsdiag
         dims_out(i)=dims_in(jshape(i))
       END DO 

       IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
       ALLOCATE(foundvariables(Nvardiag, 2))
       
       CALL search_variables(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, foundvariables,    &
         DimMatInputs)

       coordinates='long lat plev'
       CALL def_nc_var(oid, jvar, diagname, 5, 4, jshape, "XYZ", longdesc, stdesc, units, "-",  &
         coordinates, dbg)

       IF (ALLOCATED(diagnostic4D)) DEALLOCATE(diagnostic4D)
       ALLOCATE(diagnostic4D(dx, dy, dz, dt), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//" allocating 'diagnostic4D'"
       
!       CALL PV(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, dims_in, Ndimsdiag, dims_out,  &
!         deltaX, deltaY, diagnostic4D)

!       IF (dbg >= 75) THEN
!         PRINT *,'VAR: '//TRIM(diagname)//' idvar:',jvar
!         PRINT *,'1/2 example value:', diagnostic4D(dims_out(1)/2, dims_out(2)/2, dims_out(3)/2,&
!           dims_out(4)/2)

!       ENDIF

!       rcode = nf_put_vara_real (oid, jvar, start_dims, dims_out, diagnostic4D)
!       IF (rcode /= 0) PRINT *,nf_strerror(rcode)

!       DEALLOCATE(DIAGvariables)
!       DEALLOCATE(diagnostic4D)

! TCFR. Total Cloud fraction
!!
    CASE ('CLT')

! Setting diagnstic id in outputfile
       jvar = jvar + 1
       IF (dbg >= 75) PRINT *,'Computing TCFR...'

! Looking for diagnostic information in 'variables_diagnostics.inf'
       CALL diagnostic_inf_Ninvar(diagname, dbg, Nvardiag)
       IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
       ALLOCATE(DIAGvariables(Nvardiag), STAT=ierr)
       IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

       CALL diagnostic_inf(dbg, diagname, Nvardiag, DIAGvariables, Ndimsdiag, jshape,           &
         longdesc, stdesc, units)

! Setting diagnostic dimensions
       DO i=1,Ndimsdiag
         dims_out(i)=dims_in(jshape(i))
       END DO 


! Setting matrixs with diagnostic input variables file location, id and particular dimensions
       IF (ALLOCATED(foundvariables)) DEALLOCATE(foundvariables)
       ALLOCATE(foundvariables(Nvardiag, 2))
       
       IF (ALLOCATED(DimMatInputs)) DEALLOCATE(DimMatInputs)
       ALLOCATE(DimMatInputs(Nvardiag,6))
       
       CALL search_variables(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, foundvariables,    &
         DimMatInputs)

! Creation of specific matrix for computation of diagnostic. Necessary input matrixs can be of 
! different range of shape MATinputs[A/F] (if necessary)

       IF (ALLOCATED(MATinputsA)) DEALLOCATE(MATinputsA)
       ALLOCATE(MATinputsA(DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3),             &
         DimMatInputs(1,4), DimMatInputs(1,5), DimMatInputs(1,6),1), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//" allocating 'MATinputsA'"
       
       DO iinput=1,1
         CALL fill_inputs_real(dbg, ifiles, Ninfiles, foundvariables(iinput,:),                 &
	   DimMatInputs(1,:), MATinputsA(:,:,:,:,:,:,iinput))
       END DO

! Defining new diagnostic variable in output file
       coordinates='long lat'
       CALL def_nc_var(oid, jvar, diagname, 5, 3, jshape, "XY ", longdesc, stdesc, units, "-",  &
         coordinates, dbg)

! Preparation and compuatation of diagnostic
       IF (ALLOCATED(diagnostic3D)) DEALLOCATE(diagnostic3D)
       ALLOCATE(diagnostic3D(dx, dy, dt), STAT=ierr)
       IF (ierr /= 0) PRINT *,errmsg//" allocating 'diagnostic3D'"

       CALL clt(dbg, DimMatInputs(1,1), DimMatInputs(1,2), DimMatInputs(1,3), DimMatInputs(1,4),&
         MATinputsA(:,:,:,:,1,1,1), diagnostic3D)

       IF (dbg >= 75) THEN
         PRINT *,'VAR: '//TRIM(diagname)//' idvar:',jvar
         PRINT *,'1/2 example value:', diagnostic3D(halfdim(dims_out(1)), halfdim(dims_out(2))   &
	   , halfdim(dims_out(3)))
       ENDIF


! Writting diagnostic in output file
       rcode = nf_put_vara_real (oid, jvar, start_dims, dims_out, diagnostic3D)
       IF (rcode /= 0) PRINT *,TRIM(errmsg)//" in "//TRIM(section)//" "//nf_strerror(rcode)

       DEALLOCATE(DIAGvariables)
       DEALLOCATE(diagnostic3D)
       DEALLOCATE(foundvariables, DimMatInputs)
       DEALLOCATE(MATinputsA)
      
    END SELECT diag_var

  END DO compute_diags

  rcode = nf_close(oid)
  RETURN
END SUBROUTINE com_diagnostics

END MODULE module_com_diagnostics
