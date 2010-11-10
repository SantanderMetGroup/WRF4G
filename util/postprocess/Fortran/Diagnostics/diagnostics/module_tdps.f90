MODULE module_tdps
  
  CONTAINS
! Diagnostic module of computation of dew point temperature at 2 m
! GMS. UC: August 2010. v0.0
!

  SUBROUTINE tdps(debg, dx, dy, dt, q2, psfc, t2, tempd2)
!  Subroutine to compute td2 in K

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dt
  REAL, DIMENSION(dx,dy,dt), INTENT(IN)                   :: q2, psfc, t2
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: tempd2
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,k,it, ijk, nozero
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  REAL(KIND=Rhigh), DIMENSION(dx, dy, dt)                 :: e, c
  
!!!!!!!!!!!!!! Variables
! dx, dy, dt: dimensions of fields
! q2: 2m mixing ratio [kg kg-1]
! psfc: surface prssure (assumed the same at 2m) [Pa]
! tempd2: 2m de point temperature [K]
! e: vapor pressure in air [Pa]

  section="'tdps'"
  
  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'  Dimensions: ',dx,CHAR(44), dy,CHAR(44), dt

  IF (debg >= 75)  PRINT *,'  Computing tdps....'

  tempd2=0.
  e=q2*(psfc/100.)/(epsilon_gamma+q2)
  c=log10(e/es_base_tetens)
!  WHERE (t2-tkelvin <= 0.) tempd2=(c*es_Btetens_ice)/(es_Atetens_ice-c)
!  WHERE (t2-tkelvin > 0.) tempd2=(c*es_Btetens_vapor)/(es_Atetens_vapor-c)
  DO i=1,dx
    DO j=1,dy
      DO k=1,dt
        IF ((t2(i,j,k)-tkelvin) <= 0.) THEN
!          tempd2(i,j,k)=(c(i,j,k)*es_Btetens_ice)/(es_Atetens_ice-c(i,j,k))
          tempd2(i,j,k)=(c(i,j,k)*es_Btetens_vapor)/(es_Atetens_vapor-c(i,j,k))
	ELSE
          tempd2(i,j,k)=(c(i,j,k)*es_Btetens_vapor)/(es_Atetens_vapor-c(i,j,k))
	END IF
      END DO
    END DO
  END DO

!  tempd2=(c*es_Btetens_vapor)/(es_Atetens_vapor-c)+tkelvin
  tempd2=tempd2+tkelvin
  
  IF (debg >= 100) THEN
    PRINT *,'  dim/2 q2: ',q2(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 psfc: ',psfc(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 t2: ',t2(halfdim(dx),halfdim(dy),halfdim(dt))    
    PRINT *,'  dim/2 e: ',e(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 c: ',c(halfdim(dx),halfdim(dy),halfdim(dt))
  END IF

  IF (debg >= 100) THEN
    PRINT *,'  162-132 values_____________'
    PRINT *,'  t2: ',t2(162,132,1) - tkelvin
    PRINT *,'  q2: ',q2(162,132,1)
    PRINT *,'  psfc: ',psfc(162,132,1)
    PRINT *,'  e: ',e(162,132,1)
    PRINT *,'  c: ',c(162,132,1)
    PRINT *,'  tempd2: ',tempd2(162,132,1) - tkelvin

  END IF
  IF (debg >= 75) PRINT *,'  tdps at the center dimx/2:', halfdim(dx),' dimy/2:',               &
    halfdim(dy),' dt/2:', halfdim(dt), ' =', tempd2(halfdim(dx),halfdim(dy),halfdim(dt))

  END SUBROUTINE tdps

  SUBROUTINE tdps_its90(debg, dx, dy, dt, q2, psfc, t2, tempd2)
!  Subroutine to compute td2 in K following ITS-90 scale

  USE module_constants
  USE module_gen_tools, ONLY : halfdim, diag_fatal 

  IMPLICIT NONE
  
  INCLUDE 'netcdf.inc'
!    567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567
  INTEGER, INTENT(IN)                                     :: dx, dy, dt
  REAL, DIMENSION(dx,dy,dt), INTENT(IN)                   :: q2, psfc, t2
  REAL, DIMENSION(dx,dy,dt), INTENT(OUT)                  :: tempd2
  INTEGER, INTENT(IN)                                     :: debg

! Local
  INTEGER                                                 :: i,j,k,l, nozero
  CHARACTER(LEN=50)                                       :: section
  CHARACTER(LEN=250)                                      :: message
  INTEGER                                                 :: halfdim
  REAL, DIMENSION(dx, dy, dt)                             :: e, c
  REAL(KIND=Rhigh), DIMENSION(3)                          :: c_tdi
  REAL(KIND=Rhigh), DIMENSION(4)                          :: c_tdv, d_tdv, d_tdi
  REAL(KIND=Rhigh), DIMENSION(8)                          :: g_es
  REAL(KIND=Rhigh), DIMENSION(8)                          :: k_es
  REAL(KIND=Rhigh), DIMENSION(dx,dy,dt)                   :: lnes
  REAL  (KIND=Rhigh)                                      :: td2A, td2B
  
!!!!!!!!!!!!!! Variables
! dx, dy, dt: dimensions of fields
! q2: 2m mixing ratio [kg kg-1]
! psfc: surface prssure (assumed the same at 2m) [Pa]
! tempd2: 2m de point temperature [K]
! e: vapor pressure in air [Pa]

  section="'tdps'"
  
  g_es(1) = -2.8365744*10.**3.
  g_es(2) = -6.028076559*10.**3.
  g_es(3) = 1.954263612*10.
  g_es(4) = -2.737830188*10**(-2.)
  g_es(5) = 1.6261698*10**(-5.)
  g_es(6) = 7.0229056*10.**(-10.)
  g_es(7) = -1.8680009*10.**(-13.)
  g_es(8) = 2.7150305

  k_es(1) = -5.8666426*10.**3.
  k_es(2) = 2.232870244*10.
  k_es(3) = 1.39387003*10.**(-2.)
  k_es(4) = -3.4262402*10.**(-5.)
  k_es(5) = 2.7040955*10.**(-8.)
  k_es(6) = 6.7063522*10.**(-1.)

  c_tdv(1) = 2.0798233*10.**2.
  c_tdv(2) = -2.0156028*10.
  c_tdv(3) = 4.6778925*10.**(-1.)
  c_tdv(4) = -9.2288067*10.**(-6.)
  d_tdv(1) = 1.
  d_tdv(2) = -1.3319669*10.**(-1.)
  d_tdv(3) = 5.6577518*10.**(-3.)
  d_tdv(4) = -7.5172865*10.**(-5.)

  c_tdi(1) = 2.1257969*10.**2.
  c_tdi(2) = -1.0264612*10.
  c_tdi(3) = 1.4354796*10.**(-1.)
  d_tdi(1) = 1.
  d_tdi(2) = -8.2871619*10.**(-2.)
  d_tdi(3) = 2.3540411*10.**(-3.)
  d_tdi(4) = -2.4363951*10.**(-5)

  IF (debg >= 75) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 100) PRINT *,'  Dimensions: ',dx,CHAR(44), dy,CHAR(44), dt

  IF (debg >= 75)  PRINT *,'  Computing tdps following ITS-90 scale....'

  tempd2=0.
  lnes=0.

  DO i=1,dx
    DO j=1,dy
      DO k=1,dt
! Ice
!!
        IF (t2(i,j,k) - tkelvin <= 0.01) THEN
          DO l=1,5
            lnes(i,j,k)=lnes(i,j,k)+k_es(l)*t2(i,j,k)**((l-2)*1.)
          END DO
	  lnes(i,j,k)=lnes(i,j,k)+k_es(6)*LOG(t2(i,j,k))
          td2A=0.
	  DO l=1,3
	    td2A=td2A+c_tdi(l)*lnes(i,j,k)**((l-1)*1.)
	  END DO
          td2B=0.
	  DO l=1,4
	    td2B=td2B+d_tdi(l)*lnes(i,j,k)**((l-1)*1.)
	  END DO 
! Vapor
!!
        ELSE
          DO l=1,7
            lnes(i,j,k)=lnes(i,j,k)+g_es(l)*t2(i,j,k)**((l-3)*1.)
          END DO
	  lnes(i,j,k)=lnes(i,j,k)+g_es(8)*LOG(t2(i,j,k))	 
          td2A=0.
	  DO l=1,4
	    td2A=td2A+c_tdv(l)*lnes(i,j,k)**((l-1)*1.)
          END DO 
          td2B=0.
	  DO l=1,4
	    td2B=td2B+d_tdv(l)*lnes(i,j,k)**((l-1)*1.)
	  END DO
	END IF
	IF (i == halfdim(dx) .AND. j == halfdim(dy) .AND. k == halfdim(dt)) PRINT *,td2A, td2B,  &
	  td2A/td2B
        tempd2(i,j,k)=REAL(td2A/td2B)
       END DO
    END DO
  END DO
  
  IF (debg >= 100) THEN
    PRINT *,'  dim/2 q2: ',q2(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 psfc: ',psfc(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 t2: ',t2(halfdim(dx),halfdim(dy),halfdim(dt))    
    PRINT *,'  dim/2 lnes -1: ',lnes(halfdim(dx)-1,halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 lnes: ',lnes(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 lnes +1: ',lnes(halfdim(dx)+1,halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 tdps -1: ',tempd2(halfdim(dx)-1,halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 tdps: ',tempd2(halfdim(dx),halfdim(dy),halfdim(dt))
    PRINT *,'  dim/2 tdps +1: ',tempd2(halfdim(dx)+1,halfdim(dy),halfdim(dt))
  END IF

  IF (debg >= 75) PRINT *,'  tdps at the center dimx/2:', halfdim(dx),' dimy/2:',               &
    halfdim(dy),' dt/2:', halfdim(dt), ' =', tempd2(halfdim(dx),halfdim(dy),halfdim(dt))

  END SUBROUTINE tdps_its90

END MODULE module_tdps
