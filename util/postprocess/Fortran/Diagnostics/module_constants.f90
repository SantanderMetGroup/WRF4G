MODULE module_constants
! Diagnostic modules with constants of computation of atmospheric doagnostic variables from netCDF
!   files 
! GMS. UC: December 2009. version v0.0
!

  IMPLICIT NONE
  
!   456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789

  SAVE

  CHARACTER(LEN=250), PARAMETER                            :: blanks=' ' 
  REAL, PARAMETER                                          :: Cp = 7./2.*287.04
  INTEGER, PARAMETER                                       :: Ihigh=SELECTED_INT_KIND(10)
  REAL, PARAMETER                                          :: Lv=2500000.
  REAL, PARAMETER                                          :: Pi=3.141592653589793
  INTEGER, PARAMETER                                       :: Rhigh=SELECTED_REAL_KIND(8)
  REAL, PARAMETER                                          :: Rd=287.04
  REAL, PARAMETER                                          :: Rt=6371227.
  REAL, PARAMETER                                          :: Rv=461.5
  REAL, PARAMETER                                          :: e0=611.
  REAL, PARAMETER                                          :: epsilon_gamma=0.62197
  CHARACTER(LEN=50)                                        :: errmsg='ERROR - error - ERROR'//   &
    ' - error - ERROR'
  REAL, PARAMETER                                          :: es_base_tetens=610.78
  REAL, PARAMETER                                          :: es_Atetens_vapor=7.5
  REAL, PARAMETER                                          :: es_Btetens_vapor=237.3
  REAL, PARAMETER                                          :: es_Atetens_ice=9.5
  REAL, PARAMETER                                          :: es_Btetens_ice=265.5
  REAL, PARAMETER                                          :: extremeval=10000000000.
  REAL, PARAMETER                                          :: grav=9.807
  REAL, PARAMETER                                          :: missingval=-99999.
  REAL, PARAMETER                                          :: pref = 100000.
  REAL, PARAMETER                                          :: rocp=2./7.
  REAL, PARAMETER                                          :: tkelvin=273.15
  INTEGER, PARAMETER                                       :: yearleap = 1900

!!!!!!!!!!!!!!!!!! Variables
!      blanks: 50 character string of blanks
!      Cp: dry air heat capacity at constant pressure
!      Ihigh: High precision integer KIND definition
!      Lv: latent heat of vaporization of water [Jkg-1]
!      Pi: number pi [ACOS(-1.)]
!      Rhigh: High precision real KIND definition
!      Rd: gas constant for dry air [J K-1kg-1]
!      Rt:  
!      Rv: gas constant for water vapor [J K-1kg-1]
!      e0: reference saturation vapor pressure (at T=273.15 K) [Pa]
!      eps_gamma: epsilon constant from 'psychrometer constant' (gamma)
!      error: error value
! Tetens equation (Tetens, 1930) for saturated vapor pressure, es=es_base*10**((T*A)/(T+B)) [Pa]
!      es_base_tetens: base value [Pa]
!      es_Atetens_vapor: constant A for vapor [C]
!      es_Btetens_vapor: constant B for vapor [C]
!      es_Atetens_ice: constant A for ice [C]
!      es_Btetens_vapor: constant B for ice [C]
!      extremeval: allowed maximum(minimum) value for diagnostic variables
!      grav: gravity acceleration [ms-2]
!      missingval: value for missings
!      pref: pressure reference value (g.e. pot. temp.)
!      rocp: 
!      tkelvin: conversion from celsius degrees to kelvin [K]
!      yearleap: year as reference of leap years

!  CONTAINS

!  SUBROUTINE generic_calcs
! Definition of vectors with generic calcs

  INTEGER, PARAMETER                                     :: Ngen1D=4, Ngen6D=7
  CHARACTER(LEN=50), DIMENSION(Ngen1D)                   :: generic_calcs1D=(/'direct', 'sumct',&
    'prodct', 'sumall'/) 
  CHARACTER(LEN=50), DIMENSION(Ngen6D)                   :: generic_calcs6D=(/ 'direct6D',      &
    'diff_T6D', 'max6D', 'sumct6D', 'prodct6D', 'sum_spec6D', 'sumall6D'/)

!!!!!! Variables
! generic_calcs1D: generic calcs with 1D vectors with the same sahpe. Values must appear in
!    'calc_method1D'
! generic_calcs6D: generic calcs with 6D matrixs wiht the same rank and shape. Values must appear in
!    'calc_method_gen6D'

!  END SUBROUTINE generic_calcs

END MODULE module_constants
