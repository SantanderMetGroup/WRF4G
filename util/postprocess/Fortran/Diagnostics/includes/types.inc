! Diagnostic types of variables used for computation of atmospheric doagnostic variables from netCDF
!   files 
! GMS. UC: September 2010. version v0.0
!

!   456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789

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

