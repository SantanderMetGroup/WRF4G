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
! diff_dates: Subroutine to copute difference between 2 dates in [AAAA]-[MM]-[DD]_[HH]:[MI]:[SS] 
!    format in 's': seconds, 'm': minutes, 'h': hours, 'd': days. NOTE: Assuming dateA < dateB
! diff_days: Function to give the difference in days between two years
! juliand_day: Function to give the julian day of a date
! year_leap: Subroutine to give if a year is leap year or not
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

SUBROUTINE diff_dates(debg, dateA, dateB, units, difference)
! Subroutine to copute difference between 2 dates in [AAAA]-[MM]-[DD]_[HH]:[MI]:[SS] 
!    format in 's': seconds, 'm': minutes, 'h': hours, 'd': days. NOTE: Assuming dateA < dateB

  USE module_gen_tools, ONLY: diag_fatal, string_int
  USE module_constants

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                     :: debg
  CHARACTER(LEN=19), INTENT(IN)                           :: dateA, dateB
  CHARACTER(LEN=1), INTENT(IN)                            :: units
  REAL, INTENT(OUT)                                       :: difference

! Local
  INTEGER                                                 :: yearA, monthA, dayA, hourA, minA, secA
  INTEGER                                                 :: yearB, monthB, dayB, hourB, minB, secB
  INTEGER                                                 :: juliandayA, juliandayB
  INTEGER                                                 :: diffdaysA, diffdaysB
  REAL                                                    :: diffsecA, diffsecB
  CHARACTER(LEN=20)                                       :: word
  CHARACTER(LEN=50)                                       :: section, unitsname

!!!!!!! Variables
! dateA: initial date
! dateB: ending date
! units: result in 's': seconds, 'm': minutes, 'h': hours, 'd': days
! differences: difference between dates dateB - dateA (taking as reference yearref-01-01_00:00:00)
! diffsecA: distance in seconds of date A to yearref-01-01_00:00:00
! diffsecB: distance in seconds of date B to yearref-01-01_00:00:00

  section="'diff_dates'"
  IF (debg >= 150 ) PRINT *,'Section '//TRIM(section)//'... .. .'

! Taking A date as integer
!! 
  word=dateA(1:4)
  yearA=string_int(debg, word)
  word=dateA(6:7)
  monthA=string_int(debg, word)
  word=dateA(9:10)
  dayA=string_int(debg, word)
  word=dateA(12:13)
  hourA=string_int(debg, word)
  word=dateA(15:16)
  minA=string_int(debg, word)
  word=dateA(18:19)
  secA=string_int(debg, word)

! Taking B date as integer
!! 
  word=dateB(1:4)
  yearB=string_int(debg, word)
  word=dateB(6:7)
  monthB=string_int(debg, word)
  word=dateB(9:10)
  dayB=string_int(debg, word)
  word=dateB(12:13)
  hourB=string_int(debg, word)
  word=dateB(15:16)
  minB=string_int(debg, word)
  word=dateB(18:19)
  secB=string_int(debg, word)

  juliandayA=julian_day(debg, yearA, monthA, dayA)
  juliandayB=julian_day(debg, yearB, monthB, dayB)

  IF (debg >= 150) THEN
    PRINT *,'Read date A: ',yearA,'-',monthA,'-',dayA,'_',hourA,':',minA,':',secA
    PRINT *,'Read date B: ',yearB,'-',monthB,'-',dayB,'_',hourB,':',minB,':',secB
    PRINT *,'Julian day A:', juliandayA
    PRINT *,'Julian day B:', juliandayB
  END IF

  diffdaysA=diff_days(debg, yearref, yearA)
  diffdaysB=diff_days(debg, yearref, yearB)

! A complete day is retrieved, since jan-01 will be count twice
  diffsecA=(diffdaysA+juliandayA-1)*24.*3600.+hourA*3600.+minA*60.+secA
  diffsecB=(diffdaysB+juliandayB-1)*24.*3600.+hourB*3600.+minB*60.+secB

! Adding a day to prevent lose of a complete day
  difference=diffsecB - diffsecA + 3600*24
  IF (debg >= 150) THEN
    PRINT *,'days and Seconds since ',yearref,'-01-01_00:00:00 of__________'
    PRINT *,'yearA: ',diffdaysA+juliandayA-1, diffsecA
    PRINT *,'yearB: ',diffdaysB+juliandayB-1, diffsecB
  END IF


  unitsname='second'
  IF (units == 'm') THEN
    difference = difference / 60.
    unitsname='minute'
  ELSE IF (units == 'h') THEN
    difference = difference / 3600.
    unitsname='hour'
  ELSE IF (units == 'd') THEN
    difference = difference / (3600. * 24.)
    unitsname='day'
  END IF
  
  IF (debg >= 150 ) PRINT *,'Differnce between dates: ',difference,unitsname
  
  RETURN
END SUBROUTINE diff_dates

INTEGER FUNCTION diff_days(debg, yearC, yearD)
! Function to give the difference in days between two years

  USE module_constants

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg, yearC, yearD
  
! Local
  INTEGER                                                 :: iyear, sign
  INTEGER                                                 :: yeardiff, numleap, nearestleap
  INTEGER                                                 :: yearA, yearB
  CHARACTER(LEN=50)                                       :: section
  LOGICAL                                                 :: year_is_leap

!!!!!!! Variables
! year[C/D]: years to compute the difference
! yeardiff: difference between years in year (yearD - yearC)
! numleanp: number of leap years
! nearestleap: leap year most close to yearA

  section="'diff_days'"

  IF (debg >= 100 ) PRINT *,'Section: '//TRIM(section)//'... .. .'
  
  IF (yearD < yearC) THEN
    yearA=yearD
    yearB=yearC
  ELSE
    yearA=yearC
    yearB=yearD
  END IF

  sign=1
  IF (yearA < yearleap ) sign=-1
  
! yearleap was a leap year
  nearestleap=0
  DO iyear=0, ABS(yearA - yearleap), 4
    IF (ABS(yearleap + sign*iyear - yearA) < 4) nearestleap  = yearleap + sign*iyear
  END DO

! nearest leap before yearA should be given after
  IF (nearestleap < yearA) nearestleap = nearestleap + sign*4
  
! nearestleap after 'yearB' yeardiff should not be corrected. In other case, number of leap years 
! between yearA and yearB must be computed

  diff_days = (yearB - yearA)*365

  IF ( nearestleap < yearB) THEN
    yeardiff = yearB - nearestleap
    numleap = INT(yeardiff/4)
    diff_days = diff_days + numleap
  END IF
  
! Looking to leap of years A and B
  CALL year_leap(yearA, year_is_leap)
  IF (year_is_leap) diff_days = diff_days + 1

  IF (yearD < yearC) diff_days=-diff_days

  IF (debg >= 150 ) THEN
    PRINT *,'yearC: ',yearC,' yearD: ',yearD
    PRINT *,'leap year closest to lowest year: ',nearestleap,' (within years interval)'
    PRINT *,'Number of leap years within the year interval: ',numleap
    PRINT *,'Number of days between years: ',diff_days
  END IF
    
END FUNCTION diff_days

INTEGER FUNCTION julian_day(debg, year, month, day)
! Function to give the julian day of a date

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: debg, year, month, day

! Local
  INTEGER                                                 :: imonth
  CHARACTER(LEN=50)                                       :: section
  INTEGER, DIMENSION(12)                                  :: months
  LOGICAL                                                 :: is_year_leap

  months=(/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 ,31/)

  section="'julian_day'"
  IF (debg >= 150 ) PRINT *,'Section: '//TRIM(section)//'... .. .'
  
  CALL year_leap(year, is_year_leap)
  
  IF (is_year_leap ) months(2)=29

  julian_day=day
  DO imonth=1, month-1
    julian_day=julian_day+months(imonth)
  END DO

END FUNCTION julian_day

  SUBROUTINE year_leap(yr, is_leap)
! Subroutine to give if a year is leap year or not

  USE module_constants

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: yr
  LOGICAL, INTENT(OUT)                                    :: is_leap
  
! Local
  INTEGER                                                 :: diffyears 
  CHARACTER(LEN=50)                                       :: section
  
  section="'year_leap'"

  is_leap=.FALSE.
! yearleap was a leap year so if difference in years for 'yr' must be multiple of 4 to be a leap 
!   year
  
  diffyears=ABS(1976-yr)
  IF (MOD(diffyears,4) == 0) is_leap=.TRUE.

  END SUBROUTINE year_leap

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
