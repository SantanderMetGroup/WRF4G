MODULE module_types
! Diagnostic types of variables used for computation of atmospheric doagnostic variables from netCDF
!   files 
! GMS. UC: September 2010. version v0.0
!

!   456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789

  USE module_constants

! Dimension type definition
!!
    TYPE dimensiondef
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
      INTEGER                                             :: Noptions
      INTEGER, POINTER, DIMENSION(:)                      :: options
      CHARACTER(LEN=250)                                  :: stdname
      CHARACTER(LEN=250)                                  :: lonname
      CHARACTER(LEN=50)                                   :: units
      INTEGER                                             :: Nvalues
      REAL(KIND=Rhigh), POINTER, DIMENSION(:)             :: values
      CHARACTER(LEN=250)                                  :: coords
      CHARACTER(LEN=50)                                   :: positive
      CHARACTER(LEN=250)                                  :: form
    END TYPE dimensiondef

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
! method: method to compute diagnostic (a pre-defined list of generic methods is given in
!  'generic_calcs1D' (for 1D computations), 'generic_calcs6D' (for 6D comp.) (in
!  'module_constants') for input 'INvariables' with same rank and shape 
!    direct: values are the same from INname/dim_in_varnames [for num_dimInVarnames=1]
!    sumct: values are the same from INname/dim_in_varnames plus a constant [for
!       num_dimInVarnames=1] 
!    prodct: values are the same from INname/dim_in_varnames multiplyed by a constant [for
!       num_dimInVarnames=1] 
!    sumall: values are the result of the sum of all [INname/dim_in_varnames]
!    direct6D: values are the same from INname/dim_in_varnames [for num_dimInVarnames=1]
!    sumct6D: values are the same from INname/dim_in_varnames plus a constant [for
!       num_dimInVarnames=1] 
!    prodct6D: values are the same from INname/dim_in_varnames multiplyed by a constant [for
!       num_dimInVarnames=1] 
!    sumall6D: values are the result of the sum of all [INname/dim_in_varnames]
!    xxxxxx: specific for this dimension (xxxxx must have some sense in 'calc_method1D' (in
!       'module_gen_tools') or in 'compute_dimensions' for 'name' (in 'module_nc_tools')
! constant: constant value for method='constant'
! indimensions: which dimension of each 'dim_in_varnames' have to be used to compute dimension [for
!   num_dimInVarnames=1]
! Noptions: number of extra values to compute diagnostic (specific for each 'method')
! options: integer values of optional values for diagnostic
! stdname: standard CF convection name of dimension
! lonname: long name of dimension
! units: units of dimension
! Nvalues: number of values for the dimension
! values: values of the dimension
! coords: coordinates in which is based dimension (specific of dimtype=H)
! positive: sign of increment of dimension (specific of dimtype=V)
! form: formula of dimension (specific of dimtype=V)

! Variable type definition
!!
    TYPE variabledef
      CHARACTER(LEN=50)                                   :: name
      INTEGER                                             :: id
      INTEGER                                             :: type
      INTEGER                                             :: NinVarnames
      CHARACTER(LEN=250), POINTER, DIMENSION(:)           :: INvarnames      
      INTEGER                                             :: rank
      INTEGER, POINTER, DIMENSION(:)                      :: shape
      CHARACTER(LEN=250)                                  :: stdname
      CHARACTER(LEN=250)                                  :: lonname
      CHARACTER(LEN=50)                                   :: units
      CHARACTER(LEN=50)                                   :: method
      REAL                                                :: constant
      INTEGER                                             :: Noptions
      INTEGER, POINTER, DIMENSION(:)                      :: options
      CHARACTER(LEN=250)                                  :: coords
      CHARACTER(LEN=250)                                  :: form
    END TYPE variabledef

!!!!!!!!!!!!!! Variables
! name: variable/diagnostic name
! id: variable id
! type: type of variable: 
!    2: string      4: integer    5: real
! NinVarnames: number of variables from input files to compute diagnostic
! INvarnames: names of variables from input fields to compute diagnostic
! rank: rank of variable
! shape: shape of variable (order will follow dimension names in
!    'namelist.dimension_outnames'
! stdname: standard CF convection name of diagnostic
! lonname: long name of diagnostic
! units: units of diagnostic
! method: method to compute diagnostic (a pre-defined list of generic methods is given in
!    'generic_calcs6D' (in 'module_constants') for input 'INvariables' with same rank and shape
!    direct6D: values are the same from INname/dim_in_varnames [for num_dimInVarnames=1]
!    sumct6D: values are the same from INname/dim_in_varnames plus a constant [for
!       num_dimInVarnames=1] 
!    prodct6D: values are the same from INname/dim_in_varnames multiplyed by a constant [for
!       num_dimInVarnames=1] 
!    sumall6D: values are the result of the sum of all [INname/dim_in_varnames]
!    xxxxxx: specific for this variable (xxxxx must have some sense in 'calc_method_gen6D' (in
!       'module_calc_tools') or in 'module_com_diagnostics' for 'name'
! constant: constant value for 'method'
! Noptions: number of extra values to compute diagnostic (specific for each 'method')
! options: integer values of optional values for diagnostic
! coords: coordinates in which is based variable (optional)
! form: formula of dimension (optional)

END MODULE module_types
