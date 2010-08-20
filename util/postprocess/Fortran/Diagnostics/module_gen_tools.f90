MODULE module_gen_tools
  USE module_constants
  
  CONTAINS
! Subroutines/functions to make some general processes 
! GMS. UC: January 2010. version v0.0
! Following previous work of many authors for vis5D as 'userfuncs'
!
!!!!!!!!! Subroutines/Functions
! diag_fatal: Subroutine to give fatal error with a message
! diagnostic_inf: Subroutine to read diagnostic variable information from 
!     'variables_diagnostics.inf' external ASCII file
! diagnostic_inf_Ninvar: Subroutine to give number of input variables to copute varDIAG diagnostic
! halfdim: Function to give the half value of a dimension
! search_variables: Subroutine to search variables from a given netcCDF file
! string_int: Function to transform a string to a real value
! string_real: Subroutine to transform a string to a real value
! string_Realvalues: Subroutine to obtain real values from a 'namelist' string separated by comas
! string_values: Subroutine to obtain values from a 'namelist' string separated by comas
! number_values: Subroutine to obtain number of variables from a 'namelist' variable with coma    
!    separation

SUBROUTINE diag_fatal(msg)
! Subroutine to give fatal error with a message

  IMPLICIT NONE
  
  CHARACTER(LEN=250), INTENT(IN)                          :: msg
  
  PRINT *,msg
  STOP

END SUBROUTINE diag_fatal

SUBROUTINE diagnostic_inf(debg, varDIAG, Ninvar, varinnames, NdimDIAG, shapeDIAG, longdescDIAG, &
  stddescDIAG, unitsDIAG)
! Subroutine to read diagnostic variable information from 'variables_diagnostics.inf' external 
! ASCII file. File format:
!*varDIAG*
! Num_input_variables:  Ninvar 
! Input_variables:      varinnames (coma separated) 
! diagnostic_dim:       NdimDIAG 
! diagnostic_dim_shape: shapeDIAG (coma separated) 
! diagnostic_longdesc:  longdescDIAG 
! diagnostic_std_name:  stddescDIAG
! diagnostic_units:     unitsDIAG 
!    (blank line)
!*varDIAG*
! (...)

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: varDIAG
  INTEGER, INTENT(IN)                                    :: Ninvar
  INTEGER, INTENT(OUT)                                   :: NdimDIAG
  INTEGER, DIMENSION(6), INTENT(OUT)                     :: shapeDIAG
  CHARACTER(LEN=50), DIMENSION(Ninvar), INTENT(OUT)      :: varinnames
  CHARACTER(LEN=250), INTENT(OUT)                        :: longdescDIAG
  CHARACTER(LEN=50), INTENT(OUT)                         :: unitsDIAG, stddescDIAG
  INTEGER, INTENT(IN)                                    :: debg
! Local variables
  INTEGER                                                :: i, ilin
  INTEGER                                                :: iunit, ios
  INTEGER                                                :: Llabel, posvarDIAG
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used

!!!!!!!!!!!!!! Variables
!  varDIAG: variable diagnostic name
!  Ninvar: number of input variables necessary to compute diagnostic variable
!  varinnames: varable input names variables
!  NdimDIAG: number of dimension of diagnostic variable
!  shapeDIAG: order of id dimensions of diagnostic varible
!  longdescDIAG: long description of diagnostic variable
!  stddescDIAG: standard CF convection name
!  unitsDIAG: units of diagnostic variable
!  posvarDIAG: position of diagnostic variable inside information file

  section="'diagnostic_inf'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf' 
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"
   ilin=1
   posvarDIAG=0
   DO 
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(varDIAG)) THEN
       posvarDIAG=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posvarDIAG == 0) THEN
     PRINT *,"Diagnostic variable '"//TRIM(varDIAG)//"' not found in 'variables_diagnostics.inf'"
     STOP
   END IF

   REWIND (iunit)

   DO ilin=1,posvarDIAG
     READ(iunit,*)car
   END DO
   shapeDIAG=1

   READ(iunit,*)car, car
   READ(iunit,*)car, (varinnames(i),i=1,Ninvar)
   READ(iunit,*)car, NdimDIAG
   READ(iunit,*)car, (shapeDIAG(i), i=1,NdimDIAG)
   READ(iunit,*)car, longdescDIAG
   READ(iunit,*)car, stddescDIAG   
   READ(iunit,*)car, unitsDIAG 

   CLOSE(iunit)

   IF (debg >= 75) THEN
     PRINT *,"Read information for '"//TRIM(varDIAG)//"' variable________"
     PRINT *,'  Number of necessary input variables:', Ninvar
     PRINT *,'  Name of input variables:', ('  '//TRIM(varinnames(i)//'  '),i=1,Ninvar)
     PRINT *,'  Number of dimensions:', NdimDIAG
     PRINT *,'  Shape of dimensions:', (shapeDIAG(i), i=1,NdimDIAG)
     PRINT *,'  Long description:', TRIM(longdescDIAG)
     PRINT *,'  CF-standard name:', TRIM(stddescDIAG)
     PRINT *,'  Units:', TRIM(unitsDIAG)
   END IF
  
END SUBROUTINE diagnostic_inf

SUBROUTINE diagnostic_inf_Ninvar(varDIAG, debg, Ninvar)
! Subroutine to give number of input variables to copute varDIAG diagnostic from
!  'variables_diagnostics.inf' external ASCII file. File format:
!*varDIAG*
! Num_input_variables:  Ninvar
! Input_variables:      varinnames (coma separated)
! diagnostic_dim:       NdimDIAG
! diagnostic_dim_shape: shapeDIAG (coma separated)
! diagnostic_longdesc:  longdescDIAG
! diagnostic_units:     unitsDIAG
!    (blank line)
!*varDIAG*
! (...)

  IMPLICIT NONE

  CHARACTER(LEN=50), INTENT(IN)                          :: varDIAG
  INTEGER, INTENT(IN)                                    :: debg
  INTEGER, INTENT(OUT)                                   :: Ninvar
! Local variables
  INTEGER                                                :: iunit, ios, ilin
  INTEGER                                                :: posvarDIAG, Llabel
  CHARACTER(LEN=1)                                       :: car
  CHARACTER(LEN=50)                                      :: label, section
  LOGICAL                                                :: is_used

  section="'diagnostic_inf_Ninvar'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

!  Read parameters from diagnostic variables information file 'variables_diagnostics.inf'
   DO iunit=10,100
     INQUIRE(unit=iunit, opened=is_used)
     IF (.not. is_used) EXIT
   END DO
   OPEN(iunit, file='variables_diagnostics.inf', status='old', form='formatted', iostat=ios)
   IF ( ios /= 0 ) STOP "ERROR opening 'variables_diagnostics.inf'"
   ilin=1
   posvarDIAG=0
   DO
     READ(iunit,*,END=100)label
     Llabel=LEN_TRIM(label)
     IF (label(2:Llabel-1)==TRIM(varDIAG)) THEN
       posvarDIAG=ilin
       EXIT
     END IF
     ilin=ilin+1
   END DO

 100 CONTINUE

   IF (posvarDIAG == 0) THEN
     PRINT *,"Diagnostic variable '"//TRIM(varDIAG)//"' not found in 'variables_diagnostics.inf'"
     STOP
   END IF

  REWIND (iunit)

   DO ilin=1,posvarDIAG
     READ(iunit,*)car
   END DO

   READ(iunit,*)car,Ninvar

   IF (debg >= 75) THEN
     PRINT *,"Read information for '"//TRIM(varDIAG)//"' variable________"
     PRINT *,' Number of necessary input variables:', Ninvar
   END IF
   close(iunit)

END SUBROUTINE diagnostic_inf_Ninvar

INTEGER FUNCTION halfdim(dim)
! Function to give the half value of a dimension

  IMPLICIT NONE
  
  INTEGER, INTENT(IN)                                     :: dim
  
  halfdim = dim/2
  IF (dim < 2) halfdim = 1     ! Assuming non-zero dimension range
  
END FUNCTION halfdim

INTEGER FUNCTION string_int(debg, string)
! Function to transform a string to an integer value

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg
  CHARACTER(LEN=20), INTENT(IN)                          :: string

! Local
  INTEGER                                                :: i,j
  INTEGER                                                :: Lstring, point, Lpre, Laft
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!! Variables
! string: string with integer number
! Lstring: length of string

  section="'string_int'"  
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 150) PRINT *,"String: '"//TRIM(string)//"'"

  Lstring=LEN_TRIM(string)

  string_int=0
  DO i=1,Lstring
    string_int=string_int+(IACHAR(string(i:i))-48)*10.**(Lstring-i)
  END DO 


  IF (debg >= 150) PRINT *,'Integer:',string_int
  
  RETURN
END FUNCTION string_int

SUBROUTINE string_real(debg, string, value)
! Subroutine to transform a string to a real value

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg
  CHARACTER(LEN=20), INTENT(IN)                          :: string
  REAL, INTENT(OUT)                                      :: value       
! Local
  INTEGER                                                :: i,j
  INTEGER                                                :: Lstring, point, Lpre, Laft
  CHARACTER(LEN=50)                                      :: section

!!!!!!!!!!!!!!!! Variables
! string: string with real number
! value: real value 
! Lstring: length of string
! point: position of point
! Lpre: length of string previous to the point
! Laft: length of string after the point

  section="'string_real'"  
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 150) PRINT *,"String: '"//TRIM(string)//"'"

  Lstring=LEN_TRIM(string)
  point=INDEX(string, '.')
  Lpre=point-1
  Laft=Lstring-(point+1)

  value=0.
  DO i=1,Lpre
    value=value+(IACHAR(string(i:i))-48)*10.**(Lpre-i)
  END DO 

  DO i=1,Laft
    value=value+(IACHAR(string(point+i:point+i))-48)*10.**(-i)
  END DO

!  PRINT *,'String:',string,'Lpre:',Lpre,'Laft:',Laft,'Real:',value
  RETURN
END SUBROUTINE string_real 

SUBROUTINE string_Realvalues(debg, string, Nvalues, values)
! Subroutine to obtain real values from a 'namelist' string separated by comas

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: Nvalues, debg
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  REAL, DIMENSION(Nvalues), INTENT(OUT)                  :: values
! Local
  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval, point
  CHARACTER(LEN=50)                                      :: section
  CHARACTER(LEN=20)                                      :: Svalues

  section="'string_Realvalues'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
  IF (debg >= 150) PRINT *,"string: '"//TRIM(string)//"'"

  Lstring = len_trim(string)

  ival=1
  jval=1
  DO i=1,Lstring
    IF (string(i:i) == ',') THEN
      eval=i-1
      Svalues=ADJUSTL(string(ival:eval))
      CALL string_real(debg, Svalues, values(jval))
      ival=i+1
      jval=jval+1
    END IF

  END DO

  Svalues=ADJUSTL(string(ival:Lstring))
  CALL string_real(debg, Svalues, values(jval))

  IF (debg >= 150) THEN
    PRINT *,'Found values_______________'
    DO jval=1,Nvalues
      PRINT *,values(jval)
    END DO
  END IF
  RETURN

  CONTAINS

  SUBROUTINE string_real(debg, string, value)
!   Subroutine to transform a string to a real value

    IMPLICIT NONE

    INTEGER, INTENT(IN)                                  :: debg
    CHARACTER(LEN=20), INTENT(IN)                        :: string
    REAL, INTENT(OUT)                                    :: value
!   Local
    INTEGER                                              :: i,j
    INTEGER                                              :: Lstring, point, Lpre, Laft
    CHARACTER(LEN=50)                                    :: section

!!!!!!!!!!!!!!!! Variables
!   string: string with real number
!   value: real value 
!   Lstring: length of string
!   point: position of point
!   Lpre: length of string previous to the point
!   Laft: length of string after the point

   section="'string_real'"
   IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'
   IF (debg >= 150) PRINT *,"String: '"//TRIM(string)//"'"

    Lstring=LEN_TRIM(string)
    point=INDEX(string, '.')
    Lpre=point-1
    Laft=Lstring-(point+1)

    value=0.
    DO i=1,Lpre
      value=value+(IACHAR(string(i:i))-48)*10.**(Lpre-i)
    END DO

    DO i=1,Laft
      value=value+(IACHAR(string(point+i:point+i))-48)*10.**(-i)
    END DO

!    PRINT *,'String:',string,'Lpre:',Lpre,'Laft:',Laft,'Real:',value
    RETURN
  END SUBROUTINE string_real

END SUBROUTINE string_Realvalues

SUBROUTINE string_values(string, debg, Nvalues, values)
! Subroutine to obtain values from a 'namelist' string separated by comas

  IMPLICIT NONE 

  INTEGER, INTENT(IN)                                    :: Nvalues
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  CHARACTER(LEN=250), DIMENSION(Nvalues), INTENT(OUT)    :: values
  INTEGER, INTENT(IN)                                    :: debg
! Locals
  INTEGER                                                :: i, jval
  INTEGER                                                :: Lstring
  INTEGER                                                :: ival, eval
  CHARACTER(LEN=50)                                      :: section

  section="'string_values'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  Lstring = LEN_TRIM(string)

  ival=1
  jval=1
  DO i=1,Lstring
    IF (string(i:i) == ',') THEN
      eval=i-1
      values(jval)=ADJUSTL(string(ival:eval))
      ival=i+1
      jval=jval+1
    END IF
  
  END DO

  values(jval)=ADJUSTL(string(ival:Lstring))

  IF (debg >= 150) THEN
    PRINT *,'Found values_______________'
    DO jval=1,Nvalues
      PRINT *,'************'//TRIM(values(jval))//'********'
    END DO
  END IF
  RETURN

END SUBROUTINE string_values

SUBROUTINE number_values(string, debg, Nvalues)
! Subroutine to obtain number of variables from a 'namelist' variable with coma separation

  IMPLICIT NONE

  INTEGER, INTENT(IN)                                    :: debg
  CHARACTER(LEN=3000), INTENT(IN)                        :: string
  INTEGER, INTENT(OUT)                                   :: Nvalues
! local
  INTEGER                                                :: i, Lstring
  CHARACTER(LEN=50)                                      :: section
  
  section="'number_values'"
  IF (debg >= 150) PRINT *,'Section '//TRIM(section)//'... .. .'

  Lstring = len_trim(string)

  Nvalues=1

  DO i=1,Lstring

    IF (string(i:i) == ',') THEN
      Nvalues=Nvalues+1
    END IF
  END DO

!  PRINT *,'There are ',Nvalues,' values'
  RETURN

END SUBROUTINE number_values

END MODULE module_gen_tools
