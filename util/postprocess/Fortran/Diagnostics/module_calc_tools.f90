MODULE module_calc_tools
  CONTAINS
! General modules to make specific calculations
! GMS. UC: January 2010. version v0.0
!
!!!!!!!!!! COMPILATION
!
!! OCEANO: pgf90 module_diagnostic.f90 -L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree -c

!!!!!!!!!! Subroutines
! borders3D: Subroutine to compute border values of 3D fields
! z_derivate: Subroutine to compute z_derivate of a field

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

END MODULE module_calc_tools
