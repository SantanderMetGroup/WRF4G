MODULE module_diagnostic
  CONTAINS
! Diagnostic modules of computation of atmospheric doagnostic variables from netcdf files
! GMS. UC: December 2009. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 module_diagnostic.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -c

!  USE module_constants

  SUBROUTINE PV(ncfiles, Nfiles, varnames, Nvar, dx, dy, dz, dt, variableout)  
!   Subroutine to compute Potential Vorticity in PVU (in 1.E-5 PVU s-1)

    USE module_constants

    IMPLICIT NONE

    INCLUDE 'netcdf.inc'
!   567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567

    INTEGER, INTENT(IN)                                  :: Nfiles, Nvar
    CHARACTER(LEN=11), DIMENSION(Nvar), INTENT(IN)       :: varnames
!    INTEGER, DIMENSION(Nvar), INTENT(IN)                 :: Dimvar
    CHARACTER(LEN=300), DIMENSION(Nfiles), INTENT(IN)    :: ncfiles
    INTEGER, DIMENSION(Nfiles)                           :: ncids
    INTEGER                                              :: dx, dy, dz, dt
    INTEGER                                              :: idvar, ifile, rcode, ncid
    INTEGER, DIMENSION(Nvar)                             :: varfound
    REAL, DIMENSION(dx, dy, dz, dt)                      :: inu, inv, intemp 
    REAL, DIMENSION(dx, dy)                              :: incor, inmapfac 
    REAL, DIMENSION(dx, dy, dz, dt), INTENT(OUT)         :: variableout
!!! Local variables 
    INTEGER                                              :: it, i, j, k
    INTEGER                                              :: ivar
    REAL, DIMENSION(dz)                                  :: inplev
    REAL, DIMENSION(dx,dy,dz)                            :: exnf
    REAL                                                 :: gridinc
    REAL, DIMENSION(dx, dy, dz, dt)                      :: dvx, dtx, duy, dty, dup, dvp, dtemp 

!!!!!!!!!!!!!!!!!! Variables
!      ncfiles: names of netcdf files as input 
!      Nfiles: number of netcdf files as input
!      varnames: vector with variable names to compute diagnostic
!      Nvar: number of variables to compute diagnostics
!!      Dimvar: dimension of variables to compute diagnostics
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

! Adquiring initial variables
!!
    varfound=0
    DO ifile=1, Nfiles
      ncids(ifile)=70+ifile
      rcode = nf_open(ncfiles(ifile), 0, ncids(ifile)) 
      DO ivar=1, Nvar
        rcode = nf_inq_varid(ncids(ifile), TRIM(varnames(ivar)), idvar)
        SELECT CASE (ivar)
        CASE(1)
          rcode = nf_get_var_real ( ncid, ncids(ifile), inu ) 
          varfound(ivar)=1
        CASE(2)
          rcode = nf_get_var_real ( ncid, ncids(ifile), inv ) 
          varfound(ivar)=1
        CASE(3)
          rcode = nf_get_var_real ( ncid, ncids(ifile), intemp )
          varfound(ivar)=1
        CASE(4)
          rcode = nf_get_var_real ( ncid, ncids(ifile), incor )
          varfound(ivar)=1
        CASE(5)
          rcode = nf_get_var_real ( ncid, ncids(ifile), inmapfac )
          varfound(ivar)=1
        CASE(6)
          rcode = nf_get_var_real ( ncid, ncids(ifile), inplev )
          varfound(ivar)=1
        END SELECT
      END DO
    END DO
    DO ivar=1, Nvar
      IF (varfound(ivar)==0) THEN
      PRINT *,"Necessary variable '"//TRIM(varnames(ivar))//"'not found!"
      ENDIF
    END DO
    IF (ALL(varfound/=0).NEQV..TRUE.)  STOP

! Variable calculation (PV units 10E-6 SI)
!!
    DO j=1,dy
      exnf(1:dx,j,1:dz)=SPREAD((100000./inplev)**rocp, 1, dx)
    END DO
! x derivate
!!
    DO i=2,dx-1
      dvx(i,1:dy,1:dz,1:dt)=(inv(i+1,1:dy,1:dz,1:dt)-inv(i-1,1:dy,1:dz,1:dt))/gridinc
      dtx(i,1:dy,1:dz,1:dt)=(intemp(i+1,1:dy,1:dz,1:dt)-intemp(i-1,1:dy,1:dz,1:dt))/gridinc
    END DO

! y derivate
!!
    DO j=2,dy-1
      duy(1:dx,j,1:dz,1:dt)=(inv(1:dx,j+1,1:dz,1:dt)-inv(1:dx,j-1,1:dz,1:dt))/gridinc
      dty(1:dx,j,1:dz,1:dt)=(intemp(1:dx,j+1,1:dz,1:dt)-intemp(1:dx,j-1,1:dz,1:dt))/gridinc
    END DO

! Applying map correction
!!
    DO k=1,dz
      DO it=1,dt
        dvx(1:dx,1:dy,k,it)=dvx(1:dx,1:dy,k,it)*inmapfac
        duy(1:dx,1:dy,k,it)=duy(1:dx,1:dy,k,it)*inmapfac
        dtx(1:dx,1:dy,k,it)=dtx(1:dx,1:dy,k,it)*inmapfac
        dty(1:dx,1:dy,k,it)=dty(1:dx,1:dy,k,it)*inmapfac
      END DO
    END DO

! z derivate
!!
    zlevels: DO k=1,dz
      IF (k == 1) THEN
        dup(1:dx,1:dy,k,1:dt)=(inu(1:dx,1:dy,k,1:dt)-inu(1:dx,1:dy,2,1:dt))/(inplev(1)-inplev(2)) 
        dvp(1:dx,1:dy,k,1:dt)=(inv(1:dx,1:dy,k,1:dt)-inv(1:dx,1:dy,2,1:dt))/(inplev(1)-inplev(2)) 
        dtemp(1:dx,1:dy,k,1:dt)=(intemp(1:dx,1:dy,k,1:dt)-intemp(1:dx,1:dy,2,1:dt))/(inplev(1)- &
        inplev(2)) 
      ELSE IF ( k == dz ) THEN
        dup(1:dx,1:dy,k,1:dt)=(inu(1:dx,1:dy,k,1:dt)-inu(1:dx,1:dy,dz,1:dt))/(inplev(dz-1)-     &
          inplev(dz))
        dvp(1:dx,1:dy,k,1:dt)=(inv(1:dx,1:dy,k,1:dt)-inv(1:dx,1:dy,dz,1:dt))/(inplev(dz-1)-     &
          inplev(dz))
        dtemp(1:dx,1:dy,k,1:dt)=(intemp(1:dx,1:dy,k,1:dt)-intemp(1:dx,1:dy,dz,1:dt))/           &
          (inplev(dz-1)-inplev(dz))
      ELSE
        dup(1:dx,1:dy,k,1:dt)=((inplev(k)-inplev(k+1))*(inu(1:dx,1:dy,k-1,1:dt)-                &
          inu(1:dx,1:dy,k,1:dt))/(inplev(k-1)-inplev(k))+                                       &
          (inplev(k-1)-inplev(k))*(inu(1:dx,1:dy,k,1:dt)-inu(1:dx,1:dy,k+1,1:dt))/(inplev(k)-   & 
          inplev(k+1)))/(inplev(k-1)-inplev(k+1))
        dvp(1:dx,1:dy,k,1:dt)=((inplev(k)-inplev(k+1))*(inv(1:dx,1:dy,k-1,1:dt)-                &
          inv(1:dx,1:dy,k,1:dt))/(inplev(k-1)-inplev(k))+                                       &
          (inplev(k-1)-inplev(k))*(inv(1:dx,1:dy,k,1:dt)-inv(1:dx,1:dy,k+1,1:dt))/(inplev(k)-   &
          inplev(k+1)))/(inplev(k-1)-inplev(k+1))
        dtemp(1:dx,1:dy,k,1:dt)=((inplev(k)-inplev(k+1))*(intemp(1:dx,1:dy,k-1,1:dt)-           &
          intemp(1:dx,1:dy,k,1:dt))/(inplev(k-1)-inplev(k))+                                    &
          (inplev(k-1)-inplev(k))*(intemp(1:dx,1:dy,k,1:dt)-intemp(1:dx,1:dy,k+1,1:dt))/        &
          (inplev(k)-inplev(k+1)))/(inplev(k-1)-inplev(k+1))
      END IF
    END DO zlevels

! Variable computation
!!
    DO k=1, dz
      DO it=1, dt
        variableout(1:dx,1:dy,k,it)=-g*exnf(1:dx,1:dy,k)*((incor(1:dx,1:dy)+dvx(1:dx,1:dy,k,it) &
          -duy(1:dx,1:dy,k,it))*(dtemp(1:dx,1:dy,k,it)- rocp*(intemp(1:dx,1:dy,k,it)+tkelvin)   &
          /inplev(k))-             & 
          -dtx(1:dx,1:dy,k,it)*dvp(1:dx,1:dy,k,it)+dty(1:dx,1:dy,k,it)*dup(1:dx,1:dy,k,it))*1.E6 
      END DO
    END DO

    CALL borders3d(variableout, dx, dy, dz, dt)

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

END MODULE module_diagnostic
