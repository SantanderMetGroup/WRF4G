MODULE module_diagnostic
  CONTAINS
! Diagnostic modules of computation of atmospheric doagnostic variables from netcdf files
! GMS. UC: December 2009. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 module_diagnostic.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -c

  SUBROUTINE PV(debg, ncfiles, Nfiles, varnames, Nvar, dx, dy, dz, dt, variableout)  
!   Subroutine to compute Potential Vorticity in PVU (in 1.E-5 PVU s-1)

    USE module_constants

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'
!   567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567

    INTEGER, INTENT(IN)                                  :: Nfiles, Nvar
    CHARACTER(LEN=50), DIMENSION(Nvar), INTENT(IN)       :: varnames
    CHARACTER(LEN=500), DIMENSION(Nfiles), INTENT(IN)    :: ncfiles
    INTEGER, DIMENSION(Nfiles)                           :: ncids
    INTEGER                                              :: dx, dy, dz, dt
    INTEGER                                              :: idvar, ifile, rcode, ncid
    INTEGER, DIMENSION(Nvar)                             :: varfound
    REAL, DIMENSION(dx, dy, dz, dt)                      :: inu, inv, intemp 
    REAL, DIMENSION(dx, dy, dt)                          :: incor, inmapfac 
    REAL, DIMENSION(dx, dy, dz, dt), INTENT(OUT)         :: variableout
    LOGICAL, INTENT(IN)                                  :: debg
!!! Local variables 
    INTEGER                                              :: it, i, j, k
    INTEGER                                              :: ivar
    REAL, DIMENSION(dz)                                  :: inplev
    REAL, DIMENSION(dz)                                  :: exnf
!    REAL, DIMENSION(dx,dy,dz,dt)                         :: exnf
    REAL                                                 :: gridinc
    REAL, DIMENSION(dx, dy, dz, dt)                      :: dvx, dtx, duy, dty, dup, dvp, dtemp 
    CHARACTER(LEN=50)                                    :: section

!!!!!!!!!!!!!!!!!! Variables
!      ncfiles: names of netcdf files as input 
!      Nfiles: number of netcdf files as input
!      varnames: vector with variable names to compute diagnostic
!      Nvar: number of variables to compute diagnostics
!      ncids: ids of netCDF files
!      idvar: var id within ncfile
!      varfound: vector that controls if necessary variables have been found
!      dx, dy, dz, dt: range of matrix dimensions
!      variableout: output of subroutine

!!! Sorted necessary variables !!!
!     1: inu: U wind component [ms-1; dx, dy, dz, dt]
!     2: inv: V wind component [ms-1; dx, dy, dz, dt]
!     3: intemp: temperature [K; dx, dy, dz, dt]
!     4: incor: coriolis [s-1; dx, dy]
!     5: inmapfac: mapfactor due to map projection [; dx, dy] 
!     6: inplev: pressure levels [Pa; dz]

!!! Local variables
!     exnf: exeter vertical level [; dz]
!     gridinc: grid increment [m; 1]
!     dvx: x-direction derivate of v wind [ms-1m-1; dx, dy, dz, dt]
!     dtx: x-direction derivate of temperature [ms-1m-1; dx, dy, dz, dt]
!     duy: y-direction derivate of u wind [ms-1m-1; dx, dy, dz, dt]
!     dty: y-direction derivate of temperature [ms-1m-1; dx, dy, dz, dt]
!     dup: p derivate of u wind [ms-1m-1; dx, dy, dz, dt]
!     dvp: p derivate of v wind [ms-1m-1; dx, dy, dz, dt]
!     dtemp: p derivate of temperature [ms-1m-1; dx, dy, dz, dt]

    gridinc=15000.

! Adquiring initial variables
!!
    section='PVsub'
    varfound=0

    files_loop: DO ifile=1, Nfiles
      rcode = nf_open(TRIM(ncfiles(ifile)), 0, ncids(ifile)) 
      IF (debg) PRINT *,"Reading in file: '"//TRIM(ncfiles(ifile))//"' ..."
      IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)

      variables: DO ivar=1, Nvar
        IF (debg) PRINT *,ivar,':',varfound(ivar),"Looking for variable '"//TRIM(varnames(ivar))//"' "
        rcode = nf_inq_varid(ncids(ifile), TRIM(varnames(ivar)), idvar)
        IF (rcode /= 0) PRINT *,"Error in '"//TRIM(section)//"' "//nf_strerror(rcode)

        SELECT CASE (ivar)
        CASE(1)
          IF (varfound(ivar) /= 1) THEN
            rcode = nf_get_var_real ( ncids(ifile), idvar, inu ) 
            IF (rcode == 0) THEN
              varfound(ivar)=1 
            ELSE
              PRINT *,"Error reading '"//TRIM(varnames(ivar))//"' "//nf_strerror(rcode)
            END IF
          END IF 
        CASE(2)
          IF (varfound(ivar) /= 1) THEN
            rcode = nf_get_var_real ( ncids(ifile), idvar, inv ) 
            IF (rcode == 0) varfound(ivar)=1 
          END IF 
        CASE(3)
          IF (varfound(ivar) /= 1) THEN
            rcode = nf_get_var_real ( ncids(ifile), idvar, intemp ) 
            IF (rcode == 0) varfound(ivar)=1 
          END IF 
        CASE(4)
          IF (varfound(ivar) /= 1) THEN
            rcode = nf_get_var_real ( ncids(ifile), idvar, incor ) 
            IF (rcode == 0) varfound(ivar)=1 
          END IF 
        CASE(5)
          IF (varfound(ivar) /= 1) THEN
            rcode = nf_get_var_real ( ncids(ifile), idvar, inmapfac ) 
            IF (rcode == 0) varfound(ivar)=1 
          END IF 
        CASE(6)
          IF (varfound(ivar) /= 1) THEN
            rcode = nf_get_var_real ( ncids(ifile), idvar, inplev ) 
            IF (rcode == 0) varfound(ivar)=1 
          END IF 
        END SELECT
      END DO variables
    END DO files_loop

    DO ivar=1, Nvar
      IF (varfound(ivar) /=1 ) PRINT *,"Necessary variable '"//TRIM(varnames(ivar))//"' not found!" 
    END DO
    IF (.not. ALL(varfound/=0))  STOP

    IF (debg) THEN
      PRINT *,'Sample values of input variables'
      PRINT *,'inu: ',inu(dx/2, dy/2, dz/2, dt/2)
      PRINT *,'inv: ',inu(dx/2, dy/2, dz/2, dt/2)
      PRINT *,'intemp: ',intemp(dx/2, dy/2, dz/2, dt/2)
      PRINT *,'incor: ',incor(dx/2, dy/2, dt/2)
      PRINT *,'inmapfac: ',inmapfac(dx/2, dy/2, dt/2)
      PRINT *,'inplev: ',inplev(dz/2)
    END IF

! Variable calculation (PV units 10E-6 SI)
!!
!    DO j=1,dy
!      exnf(:,:,:,:)=SPREAD((100000./inplev)**rocp, 3, dx*dt*dy)
      exnf=(100000./inplev)**rocp
!    END DO
    IF (debg) PRINT *,'exnf: ',exnf(dz/2)
! x derivate
!!
    DO i=2,dx-1
      dvx(i,:,:,:)=(inv(i+1,:,:,:)-inv(i-1,:,:,:))/gridinc
      dtx(i,:,:,:)=(intemp(i+1,:,:,:)-intemp(i-1,:,:,:))/gridinc
    END DO
    IF (debg) PRINT *,'dvx: ',dvx(dx/2, dy/2, dz/2, dt/2)
    IF (debg) PRINT *,'dtx: ',dtx(dx/2, dy/2, dz/2, dt/2)

! y derivate
!!
    DO j=2,dy-1
      duy(:,j,:,:)=(inv(:,j+1,:,:)-inv(:,j-1,:,:))/gridinc
      dty(:,j,:,:)=(intemp(:,j+1,:,:)-intemp(:,j-1,:,:))/gridinc
    END DO
    IF (debg) PRINT *,'duy: ',duy(dx/2, dy/2, dz/2, dt/2)
    IF (debg) PRINT *,'dty: ',dty(dx/2, dy/2, dz/2, dt/2)

! Applying map correction
!!
    DO k=1,dz
        dvx(:,:,k,:)=dvx(:,:,k,:)*inmapfac
        duy(:,:,k,:)=duy(:,:,k,:)*inmapfac
        dtx(:,:,k,:)=dtx(:,:,k,:)*inmapfac
        dty(:,:,k,:)=dty(:,:,k,:)*inmapfac
    END DO
    IF (debg) PRINT *,'inmapfac: ',inmapfac(dx/2, dy/2, dt/2)

    CALL z_derivate(inu, dx, dy, dz, dt, inplev, dup)
    CALL z_derivate(inv, dx, dy, dz, dt, inplev, dvp)
    CALL z_derivate(intemp, dx, dy, dz, dt, inplev, dtemp)

    IF (debg) PRINT *,'dup: ',dup(dx/2, dy/2, dz/2, dt/2)
    IF (debg) PRINT *,'dvp: ',dvp(dx/2, dy/2, dz/2, dt/2)
    IF (debg) PRINT *,'dtemp: ',dtemp(dx/2, dy/2, dz/2, dt/2)

! Variable computation
!!
    DO k=1, dz
      variableout(:,:,k,:)=-g*exnf(k)*((incor(:,:,:)+dvx(:,:,k,:)                     &
        -duy(:,:,k,:))*(dtemp(:,:,k,:)- rocp*(intemp(:,:,k,:)+tkelvin)                     &
        /inplev(k))-                                                                       & 
        -dtx(:,:,k,:)*dvp(:,:,k,:)+dty(:,:,k,:)*dup(:,:,k,:))*1.E6 
    END DO
    IF (debg) PRINT *,'variableout: ',variableout(dx/2, dy/2, dz/2, dt/2)

    CALL borders3d(variableout, dx, dy, dz, dt)

    WHERE(variableout >= extremeval) variableout=errorval
    WHERE(variableout <= -extremeval) variableout=errorval
    IF (debg) PRINT *,'PV without extreme values: ',variableout(dx/2, dy/2, dz/2, dt/2)
 
    PRINT *,"'PV' diagnostics SUCCESSFULLY computed..."
    RETURN
 
  END SUBROUTINE PV

  SUBROUTINE borders3D(var,dimx,dimy,dimz,dimt)
! Subroutine to compute border values of 3D fields

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: dimx, dimy, dimz, dimt
  REAL, DIMENSION(dimx, dimy, dimz, dimt), INTENT(INOUT) :: var

!!!!!!!!!!! Local variables
  INTEGER                                                :: i,j,k,l
  REAL, DIMENSION(dimx, dimy, dimz, dimt)                :: temporalvar

!!!!!!!!!!! Variables
! var: variable to compute its border values
! dimx, dimy, dimz, dimt: dimension ranges

  DO j=2,dimy-1
    temporalvar(1,j,1:dimz,1:dimt)=2.*var(2,j,1:dimz,1:dimt)-var(3,j,1:dimz,1:dimt)
    temporalvar(dimx,j,1:dimz,1:dimt)=2.*var(dimx-1,j,1:dimz,1:dimt)-var(dimx-2,j,1:dimz,1:dimt)
  END DO
  DO i=2,dimx-1
    temporalvar(i,1,1:dimz,1:dimt)=2.*var(i,2,1:dimz,1:dimt)-var(i,3,1:dimz,1:dimt)
    temporalvar(i,dimy,1:dimz,1:dimt)=2.*var(i,dimy-1,1:dimz,1:dimt)-var(i,dimy-2,1:dimz,1:dimt)
  END DO
  temporalvar(1,1,1:dimz,1:dimt)=2.*var(2,2,1:dimz,1:dimt)-var(3,3,1:dimz,1:dimt)
  temporalvar(dimx,dimy,1:dimz,1:dimt)=2.*var(dimx-1,dimy-1,1:dimz,1:dimt)-var(dimx-2,dimy-2,   &
    1:dimz,1:dimt)
  temporalvar(1,dimy,1:dimz,1:dimt)=2.*var(2,dimy-1,1:dimz,1:dimt)-var(3,dimy-2,1:dimz,1:dimt)
  temporalvar(dimx,1,1:dimz,1:dimt)=2.*var(dimx-1,2,1:dimz,1:dimt)-var(dimx-2,3,1:dimz,1:dimt)

  RETURN

  END SUBROUTINE borders3D

  SUBROUTINE z_derivate(field, dimx, dimy, dimz, dimt, p_lev, field_p)
! Subroutine to compute z_derivate of a field

  IMPLICIT NONE

  INTEGER                                                :: k
  INTEGER, INTENT(IN)                                    :: dimx, dimy, dimz, dimt
  REAL, DIMENSION(dimx, dimy, dimz, dimt), INTENT(IN)    :: field
  REAL, DIMENSION(dimz), INTENT(IN)                      :: p_lev                        
  REAL, DIMENSION(dimx, dimy, dimz, dimt), INTENT(OUT)   :: field_p

  field_p=0.
! z derivate
!!
    zlevels: DO k=1,dimz
      IF (k == 1) THEN
        field_p(:,:,k,:)=(field(:,:,k,:)-field(:,:,2,:))/(p_lev(1)-p_lev(2)) 
      ELSE IF ( k == dimz ) THEN
        field_p(:,:,k,:)=(field(:,:,k-1,:)-field(:,:,dimz,:))/(p_lev(dimz-1)-p_lev(dimz))
      ELSE
        field_p(:,:,k,:)=((p_lev(k)-p_lev(k+1))*(field(:,:,k-1,:)-field(:,:,k,:))/              &
          (p_lev(k-1)-p_lev(k)) +                                                               &
          (p_lev(k-1)-p_lev(k))*(field(:,:,k,:)-field(:,:,k+1,:))/(p_lev(k)-p_lev(k+1)))/       &
          (p_lev(k-1)-p_lev(k+1))
      END IF
    END DO zlevels
  END SUBROUTINE z_derivate

END MODULE module_diagnostic
