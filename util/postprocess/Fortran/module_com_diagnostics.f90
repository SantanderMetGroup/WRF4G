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
  ofile)
! Subroutine to compute all desired diagnostics

  USE module_list_diagnostics
  USE module_gen_tools
!  USE module_PV

  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

  LOGICAL, INTENT(IN)                                    :: dbg
  INTEGER, INTENT(IN)                                    :: Ninfiles
  CHARACTER(LEN=500), DIMENSION(Ninfiles), INTENT(IN)    :: ifiles
  INTEGER, INTENT(IN)                                    :: dx, dy, dz, dt
  INTEGER, INTENT(IN)                                    :: Ndiags
  CHARACTER(LEN=250), DIMENSION(Ndiags), INTENT(IN)      :: Diags
  CHARACTER(LEN=500), INTENT(IN)                         :: ofile
  CHARACTER(LEN=50), ALLOCATABLE, DIMENSION(:)           :: DIAGvariables
  REAL, INTENT(IN)                                       :: deltaX, deltaY
! Local vars
  INTEGER                                                :: i, idiag
  INTEGER                                                :: Nvardiag
  REAL, ALLOCATABLE, DIMENSION(:,:,:,:)                  :: diagnostic4D
  REAL, ALLOCATABLE, DIMENSION(:,:,:)                    :: diagnostic3D
  REAL, ALLOCATABLE, DIMENSION(:,:)                      :: diagnostic2D
  INTEGER                                                :: ierr, oid, jvar, rcode
  INTEGER                                                :: Ndimsdiag
  INTEGER, DIMENSION(4)                                  :: start_dims, dims_out, dims_in
  INTEGER, DIMENSION(6)                                  :: jshape
  CHARACTER(LEN=50)                                      :: varname, units, diagname
  CHARACTER(LEN=250)                                     :: longdesc
!  CHARACTER(LEN=80)                                      :: nf_strerror

!!!!!!!!!!!!!!!!! Variables
! ifiles: vector with input files
! Ninfiles: number of infiles
! d[x/y/z/t]: 4D dimensions input range
! Diags: vector with diagnostic variables
! Ndiags: number of diagnostic variables
! ofile: outputfile
! diagnostic4D: 4D computed diagnostic
! diagnostic4D: 3D computed diagnostic
! diagnostic4D: 2D computed diagnostic
! Nvardiag: number of variables to compute the specific diagnostic
! Ndimsdiag: number of dimensions of diagnostic variable
! jvar: mnumber of variable in output file
! varname: name of diagnostic variable
! units: units of variable
! longdesc: long description attribute of variable
! dv[1/2/3/4]: 4D diagnostic range of 4 dimensions

! output file creation
!!
  rcode = nf_create(ofile, 0, oid)
  rcode = nf_def_dim(oid, 'lon', dx, 1)
  rcode = nf_def_dim(oid, 'lat', dy, 2)
  rcode = nf_def_dim(oid, 'plev', dz, 3)
  rcode = nf_def_dim(oid, 'Time', dt, 4)
!!!
!!
! Computing diagnostics
!!
!!!
  start_dims=1
  jshape=1
  jvar=1
  dims_in=RESHAPE((/dx, dy, dz, dt/),(/4/))

  compute_diags: DO idiag=1, Ndiags
   diagname=(diags(idiag))
   diag_var: SELECT CASE(diagname)
     CASE('PV')
       jvar = jvar + 1
       PRINT *,'Computing PV...'

       CALL diagnostic_inf_Ninvar(diagname, dbg, Nvardiag)
       IF (ALLOCATED(DIAGvariables)) DEALLOCATE(DIAGvariables)
       ALLOCATE(DIAGvariables(Nvardiag), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'DIAGvariables'"

       CALL diagnostic_inf(dbg, diagname, Nvardiag, DIAGvariables, Ndimsdiag, jshape,           &
         longdesc, units)

       DO i=1,Ndimsdiag
         dims_out(i)=dims_in(jshape(i))
       END DO 

       CALL def_nc_var(oid, jvar, diagname, 5, 4, jshape, "XYZ", longdesc, units, "-",          &
         'long lat plev       ', dbg)

       IF (ALLOCATED(diagnostic4D)) DEALLOCATE(diagnostic4D)
       ALLOCATE(diagnostic4D(dx, dy, dz, dt), STAT=ierr)
       IF (ierr /= 0) PRINT *,"Error allocating 'diagnostic4D'"
       CALL PV(dbg, ifiles, Ninfiles, DIAGvariables, Nvardiag, dims_in, Ndimsdiag, dims_out,  &
         deltaX, deltaY, diagnostic4D)

       IF (dbg) THEN
         PRINT *,'VAR: PV idvar:',jvar
         PRINT *,'1/2 example value:', diagnostic4D(dims_out(1)/2, dims_out(2)/2, dims_out(3)/2,&
           dims_out(4)/2)
       ENDIF

       rcode = nf_put_vara_real (oid, jvar, start_dims, dims_out, diagnostic4D)
       IF (rcode /= 0) PRINT *,nf_strerror(rcode)

       DEALLOCATE(DIAGvariables)
       DEALLOCATE(diagnostic4D)
    END SELECT diag_var

  END DO compute_diags

  rcode = nf_close(oid)
  RETURN
END SUBROUTINE com_diagnostics

END MODULE module_com_diagnostics
