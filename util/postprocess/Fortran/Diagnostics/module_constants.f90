MODULE module_constants
! Diagnostic modules with constants of computation of atmospheric doagnostic variables from netCDF
!   files 
! GMS. UC: December 2009. version v0.0
!

  IMPLICIT NONE
  
!   456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789

  SAVE

  CHARACTER(LEN=50), PARAMETER                             :: blanks=' '
  REAL, PARAMETER                                          :: Lv=2500000.
  REAL, PARAMETER                                          :: Pi=3.141596
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
  REAL, PARAMETER                                          :: rocp=2./7.
  REAL, PARAMETER                                          :: tkelvin=273.15
  INTEGER, PARAMETER                                       :: yearref = 1970
  INTEGER, PARAMETER                                       :: yearleap = 1900

!!!!!!!!!!!!!!!!!! Variables
!      blanks: 50 character string of blanks
!      Lv: latent heat of vaporization of water [Jkg-1]
!      Pi: number pi [ACOS(-1.)]
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
!      rocp: 
!      tkelvin: conversion from celsius degrees to kelvin [K]
!      yearref: year as reference
!      yearleap: year as reference of leap years

  CONTAINS

  SUBROUTINE dimension_type

! Dimension type definition
!!
    TYPE dimension
      CHARACTER(LEN=50)                                   :: name
      INTEGER                                             :: id
      CHARACTER(LEN=1)                                    :: type
      CHARACTER(LEN=1)                                    :: axis
      CHARACTER(LEN=50)                                   :: INname
      INTEGER                                             :: range
      INTEGER                                             :: NinVarnames
      CHARACTER(LEN=250), POINTER, DIMENSION(:)           :: INvarnames
      CHARACTER(LEN=50)                                   :: method
      REAL                                                :: constant
      INTEGER, POINTER, DIMENSION(:)                      :: indimensions
      CHARACTER(LEN=250)                                  :: stdname
      CHARACTER(LEN=250)                                  :: lonname
      CHARACTER(LEN=50)                                   :: units
      INTEGER                                             :: Nvalues
      REAL, POINTER, DIMENSION(:)                         :: values
      CHARACTER(LEN=250)                                  :: coords
      CHARACTER(LEN=50)                                   :: positive
      CHARACTER(LEN=250)                                  :: form
    END TYPE dimension

!!!!!!!!!!!!!! Variables
! name: dimension diagnostic name
! id: dimension id
! type: type of dimension: 
!    H: horizontal dimension    V: vertical dimension     T: temporal dimension
! axis: space axis to which it references
! INname: dimension name as it appears in input file
! range: 1D range of dimension
! NinVarnames: number of variables from input files to compute dimension
! INvarnames: names of variables from input fields to compute dimension
! method: method to compute dimension
!    direct: values are the same from INname/dim_in_varnames [for num_dimInVarnames=1]
!    sumct: values are the same from INname/dim_in_varnames plus a constant [for
!       num_dimInVarnames=1] 
!    prodct: values are the same from INname/dim_in_varnames multiplyed by a constant [for
!       num_dimInVarnames=1] 
!    sumall: values are the result of the sum of all [INname/dim_in_varnames]
!    xxxxxx: specific for this dimension (xxxxx must have some sense in 'calc_method1D' (in
!       'module_gen_tools') or in 'compute_dimensions' for 'name' (in 'module_nc_tools')
! constant: constant value for method='constant'
! indimensions: which dimension of each 'dim_in_varnames' have to be used to compute dimension [for
!   num_dimInVarnames=1]
! stdname: standard CF convection name of dimension
! lonname: long name of dimension
! units: units of dimension
! Nvalues: number of values for the dimension
! values: values of the dimension
! coords: coordinates in which is based dimension (specific of dimtype=H)
! positive: sign of increment of dimension (specific of dimtype=V)
! form: formula of dimension (specific of dimtype=V)

  END SUBROUTINE dimension_type

END MODULE module_constants
