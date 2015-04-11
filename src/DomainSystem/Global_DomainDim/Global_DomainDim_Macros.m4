

  MACRO({PREAMBLE_GLOBAL}, {{
  integer, parameter :: NDIM = $1
  
  !> precision
  integer, parameter :: FLOAT = selected_real_kind(14, 30)
  real(FLOAT), parameter :: EPS = epsilon(1._FLOAT)
  real(FLOAT), parameter :: TOL = 2000*EPS
  
  !> formatting
  character(*), parameter :: REALFMT = '(f10.4)'  !< print format for rls
  integer, parameter :: REALSP = 10 
   
  interface str
     module procedure int2str
     module procedure lgc2str
     module procedure real2str
     module procedure lgcDim1Array2str
     module procedure intDim1Array2str
     module procedure realDim1Array2str
  end interface
  }})


  !-------------------------------------------------------------------
  !- LogicalVector and its overloaded interfaces
  !-------------------------------------------------------------------

  MACRO({TYPEDEFS_LOGICALVECTOR}, {{
  type, public :: LogicalVectorType
     private
     logical, dimension(NDIM) :: values = .false.
   contains
     procedure :: setElement => setElement_logicalVector
     procedure :: getValues => getValues_logicalVector
     procedure :: getValue => getValue_logicalVector
  end type LogicalVectorType
  
  interface assignment(=)
     module procedure assign_logicalVector_logicalVector
     module procedure assign_logicalVector_scalar
     module procedure assign_logicalVector_logicalDim1
  end interface

  interface vector
     module procedure logicalVector
  end interface

  interface operator(.not.)
     module procedure not_logicalVector
  end interface

  interface operator(==)
     module procedure equalTo_logicalVector
  end interface

  interface operator(/=)
     module procedure notEqualTo_logicalVector
  end interface

  interface all
     module procedure all_logicalVector
  end interface
  
  interface any
     module procedure any_logicalVector
  end interface

  interface count
     module procedure count_logicalVector
  end interface
  
  interface str
     module procedure logicalVector2str
  end interface
  }})  

  
  !-------------------------------------------------------------------
  !- IntVector, SizeVector and their overloaded interfaces
  !-------------------------------------------------------------------
  
  MACRO({TYPEDEFS_INTVECTOR}, {{
  type, public :: IntVectorType
     private
     ! the mask vector is used to represent grid shapes that are
     ! subdimensional to the domain space.  E.g. an XZ grid would
     ! be given the mask [T F T].
     logical, dimension(NDIM) :: mask = .true.
     integer, dimension(NDIM) :: values = 0
   contains
     procedure :: setElement => setElement_intVector
     procedure :: setMask_intVector_scalar
     procedure :: setMask_intVector_logicalVector
     generic :: setMask => setMask_intVector_scalar, &
          setMask_intVector_logicalVector
     
     procedure :: getValues => getValues_intVector
     procedure :: getValue => getValue_intVector
     $1
  end type IntVectorType
    
  
  type, extends(IntVectorType), public :: SizeVectorType
   contains
     procedure :: address2index
     procedure :: index2address
     $2
  end type SizeVectorType

  
  interface vector
     module procedure intVector
  end interface
  
  interface unitVector
     module procedure intUnitVector
  end interface

  interface assignment(=)
     module procedure assign_intVector_intVector
     module procedure assign_intVector_scalar
     module procedure assign_intVector_intDim1
  end interface

  interface operator(+)
     module procedure add_intVector_scalar
     module procedure add_intVector_intVector
  end interface
  
  interface operator(-)
     module procedure subtract_intVector_scalar
     module procedure subtract_intVector_intVector
  end interface

!!$  interface operator(*)
!!$     module procedure multiply_intVector_scalar
!!$     module procedure multiply_intVector_intVector
!!$  end interface
!!$  
!!$  interface operator(/)
!!$     module procedure divide_intVector_scalar
!!$     module procedure divide_intVector_intVector
!!$  end interface
     
  interface operator(==)
     module procedure equalTo_intVector_scalar
     module procedure equalTo_intVector_intVector
  end interface
     
  interface operator(/=)
     module procedure notEqualTo_intVector_scalar
     module procedure notEqualTo_intVector_intVector
  end interface
  
  interface real
     module procedure real_intVector
  end interface

  interface product
     module procedure product_intVector
  end interface

  
  interface str
     module procedure intVector2str
  end interface
  }})

  !-------------------------------------------------------------------
  !- RealVector and its overloaded interfaces
  !-------------------------------------------------------------------

  MACRO({TYPEDEFS_REALVECTOR}, {{
  type, public :: RealVectorType
     private
     real(FLOAT), dimension(NDIM) :: values = 0._FLOAT
   contains
     procedure :: setElement => setElement_realVector
     procedure :: magnitude => magnitude_realVector
     procedure :: normalise => normalise_realVector
     procedure :: applyMask => applyMask_realVector
     procedure :: dotProduct => dotProduct_realVector
     procedure :: crossProductMagnitude => crossProductMagnitude_realVector
     procedure :: getValues => getValues_realVector
     procedure :: getValue => getValue_realVector
     procedure :: anyNaN => anyNaN_realVector
  end type RealVectorType

  interface vector
     module procedure realVector
  end interface
  
  interface unitVector
     module procedure realUnitVector_byComponents
     module procedure realUnitVector_byAngle
  end interface

  interface assignment(=)
     module procedure assign_realVector_realVector
     module procedure assign_realVector_scalar
     module procedure assign_realVector_realDim1
  end interface

  interface operator(+)
     module procedure add_realVector_scalar
     module procedure add_realVector_realVector
  end interface
  
  interface operator(-)
     module procedure subtract_realVector_scalar
     module procedure subtract_realVector_realVector
  end interface

  interface operator(*)
     module procedure multiply_realVector_scalar
     module procedure multiply_realVector_realVector
  end interface
  
  interface operator(/)
     module procedure divide_realVector_scalar
     module procedure divide_realVector_realVector
  end interface
     
  interface operator(==)
     module procedure equalTo_realVector
  end interface
     
  interface operator(<)
     module procedure lessThan_realVector
  end interface
     
  interface operator(>)
     module procedure greaterThan_realVector
  end interface
     
  interface operator(<=)
     module procedure lessThanOrEqualTo_realVector
  end interface
     
  interface operator(>=)
     module procedure greaterThanOrEqualTo_realVector
  end interface
  
  interface str
     module procedure realVector2str
  end interface
  }})
  
  
  !-------------------------------------------------------------------
  !- RealTensor and its overloaded interfaces
  !-------------------------------------------------------------------

  MACRO({TYPEDEFS_REALTENSOR}, {{
  type, public :: RealTensorType
     private
     real(FLOAT), dimension(NDIM, NDIM) :: values = 0._FLOAT
   contains
     procedure :: setElement => setElement_realTensor
     procedure :: setRow_realTensor_scalar
     procedure :: setRow_realTensor_realVector
     procedure :: setRow_realTensor_realTensor
     generic :: setRow => setRow_realTensor_scalar, &
          setRow_realTensor_realVector, setRow_realTensor_realTensor
     procedure :: setColumn_realTensor_scalar
     procedure :: setColumn_realTensor_realVector
     procedure :: setColumn_realTensor_realTensor
     generic :: setColumn => setColumn_realTensor_scalar, &
          setColumn_realTensor_realVector, &
          setColumn_realTensor_realTensor
     procedure :: getRow => getRow_realTensor
     procedure :: getColumn => getColumn_realTensor
     procedure :: det => det_realTensor
  end type RealTensorType
  

  interface tensor
     module procedure realTensor
  end interface

  interface assignment(=)
     module procedure assign_realTensor_realTensor
     module procedure assign_realTensor_scalar
     module procedure assign_realTensor_realDim2
  end interface
  }})

  

    MACRO({PROCEDURES_GLOBAL}, {{
  !> finds the number of digits in \a x.
  !> This function is pure [F95].
  elemental function nDigits( x )
    integer, intent(in) :: x
    integer :: nDigits
    integer, dimension(2), parameter  :: LOOKUP = [1, 0]
    nDigits = floor(log10(max(real(x), 1.)) + 1)
    ! this adds one digit for a minus sign:
    nDigits = nDigits + LOOKUP(1 + (sign(1, x)+1)/2)
  end function nDigits
    

  !> Turns positive integer X into a character string STR.  
  !> This function is pure [F95].
  pure function int2str( x ) result( str )

    ! arguments and local variables
    integer, intent(in) :: x
    character( len=nDigits(x) ) :: str
    integer :: n, i, j, y, y0

    ! find '0' in the ASCII character set
    y0 = ichar('0')
    
    ! loop over each digit in X
    n = nDigits(x)
    do i = 1, n
       y = x/10**(i-1)     ! truncate digits to the right
       y = modulo(y, 10)   ! truncate digits to the left
       j = n + 1 - i       ! get the corresponding index to STR
       ! look up result in the ASCII character set and store
       str(j:j) = char(y0 + y)
    end do

  end function int2str
  

  !> Turns logical value X into a character string STR.  
  pure function lgc2str( x ) result( str )
    logical, intent(in) :: x
    character(1) :: str
    write(str, '(l1)') x
  end function lgc2str

  
  !> Turns logical array X into a character string STR.  
  pure function lgcDim1Array2str( x ) result( str )
    logical, dimension(:), intent(in) :: x
    character( 2*size(x) ) :: str
    write(str, '('//int2str(size(x))//'l2)') x
  end function lgcDim1Array2str

  
  !> Turns integer array X into a character string STR.  
  pure function intDim1Array2str( x ) result( str )
    integer, dimension(:), intent(in) :: x
    character( (1 + maxval(nDigits(x)))*size(x) ) :: str
    write(str, '('//int2str(size(x))//'('' '', i'//int2str(maxval(&
         nDigits(x)))//'))') x
  end function intDim1Array2str
  

  !> Turns character string \a str into an integer \a x.
  !> This function is pure [F95].
  pure function str2int( str ) result( x )

    ! arguments and local variables
    character(*), intent(in) :: str
    integer :: x
    integer :: n, i, j, y, y0

    ! find '0' in the ASCII character set
    y0 = ichar('0')

    ! initialise x, get number of digits
    x = 0
    n = len_trim( str )

    ! loop over digits adding up multiples of powers of 10
    do i = 1, n
       j = n + 1 - i
       y = ichar( str(i:i) ) - y0
       x = x + y*(10**j)
    end do

  end function str2int
  

  
  !> Returns an exponent to be displayed when printing out array
  !> values that are too big or too small for REALFMT.
  !>
  !> \a arrayMaxAbs is the maximum absolute value of the array.
  !> Within a certain range, the REALFMT can display values effectively
  !> and so \a commonExponent will be 0.
  pure function commonExponent( arrayMaxAbs )
    real(FLOAT), intent(in) :: arrayMaxAbs
    integer :: commonExponent
    integer, parameter :: THRESHOLDBIG = 3, THRESHOLDSMALL = -2
    if ( arrayMaxAbs < EPS ) then
       ! forget the calculation if values are close to zero.
       commonExponent = 0
    else
       ! this calculates the exponent
       commonExponent = int(floor(log10(arrayMaxAbs)))
       ! this determines whether exponent=0 is acceptable for
       ! printing the values in REALFMT
       if ( (THRESHOLDSMALL < commonExponent) .and. &
            (commonExponent < THRESHOLDBIG) ) then
          commonExponent = 0
       end if
    end if
  end function commonExponent

  
  !> Converts an exponent into a real amplitude.  e.g. amp(2) = 100.
  pure function amp( exponent )
    integer, intent(in) :: exponent
    real(FLOAT) :: amp
    amp = 10._FLOAT**exponent
  end function amp

  
  !> If \a exponent is an integer ±### (2 digits max), then exp2str is
  !> '1.0e±### *'.  But blank if \a exponent = 0.
  pure function exp2str( exponent )
    integer, intent(in) :: exponent
    character(11) :: exp2str
    exp2str(:) = ' '
    if ( exponent /= 0 ) then
       write ( exp2str, '(a, i2, a)') '1.0e', exponent
    end if
  end function exp2str

  
  pure function real2str( scalar ) 
    real(FLOAT), intent(in) :: scalar
    character( REALSP ) :: real2str
    write ( real2str, '('//REALFMT//')' ), scalar
  end function real2str
  

  
  pure function realDim1Array2str( vector ) 
    real(FLOAT), dimension(:), intent(in) :: vector
    logical :: isTooBig
    integer :: e
    character(1 + size(vector)*REALSP ) :: realDim1Array2str
!!$    e = commonExponent( maxval( abs(vector) ) )
!!$    write ( str, '(2a, '//int2str(size(vector))//REALFMT//', a)' ) &
!!$         exp2str(e), '[', vector/amp(e), ']'
    
    write ( realDim1Array2str, '('//str(size(vector))&
         //REALFMT//')' ) vector
  end function realDim1Array2str
  }})

  !-------------------------------------------------------------------
  !- LogicalVector procedures
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_LOGICALVECTOR}, {{
  pure function logicalVector( values )
    logical, dimension(NDIM), intent(in) :: values
    type(LogicalVectorType) :: logicalVector
    logicalVector%values = values
  end function logicalVector

  
  elemental subroutine assign_logicalVector_logicalVector( tgt, src )
    type(LogicalVectorType), intent(out) :: tgt
    type(LogicalVectorType), intent(in) :: src
    tgt%values = src%values
  end subroutine assign_logicalVector_logicalVector
  

  elemental subroutine assign_logicalVector_scalar( tgt, src )
    type(LogicalVectorType), intent(out) :: tgt
    logical, intent(in) :: src
    tgt%values = src
  end subroutine assign_logicalVector_scalar
  

  pure subroutine assign_logicalVector_logicalDim1( tgt, src )
    type(LogicalVectorType), intent(out) :: tgt
    logical, dimension(NDIM), intent(in) :: src
    tgt%values = src
  end subroutine assign_logicalVector_logicalDim1
  

  elemental function not_logicalVector( obj )
    class(LogicalVectorType), intent(in) :: obj
    type(LogicalVectorType) :: not_logicalVector
    not_logicalVector%values = not( obj%values )
  end function not_logicalVector
    
  
  elemental function equalTo_logicalVector( a, b )
    type(LogicalVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: equalTo_logicalVector
    equalTo_logicalVector = (a%values == b%values)
  end function equalTo_logicalVector
    
  
  elemental function notEqualTo_logicalVector( a, b )
    type(LogicalVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: notEqualTo_logicalVector
    notEqualTo_logicalVector = .not. (a == b)
  end function notEqualTo_logicalVector


  elemental function any_logicalVector( obj )
    class(LogicalVectorType), intent(in) :: obj
    logical :: any_logicalVector
    any_logicalVector = any( obj%values )
  end function any_logicalVector


  elemental function all_logicalVector( obj )
    class(LogicalVectorType), intent(in) :: obj
    logical :: all_logicalVector
    all_logicalVector = all( obj%values )
  end function all_logicalVector


  elemental function count_logicalVector( obj )
    class(LogicalVectorType), intent(in) :: obj
    integer :: count_logicalVector
    count_logicalVector = count( obj%values )
  end function count_logicalVector


  pure function getValues_logicalVector( obj )
    class(LogicalVectorType), intent(in) :: obj
    logical, dimension(NDIM) :: getValues_logicalVector
    getValues_logicalVector = obj%values
  end function getValues_logicalVector

  
  pure subroutine setElement_logicalVector( obj, index, scalar )
    class(LogicalVectorType), intent(inout) :: obj
    integer, intent(in) :: index
    logical, intent(in) :: scalar
    obj%values( index ) = scalar
  end subroutine setElement_logicalVector


  pure function getValue_logicalVector( obj, index )
    class(LogicalVectorType), intent(in) :: obj
    integer, intent(in) :: index
    logical :: getValue_logicalVector
    getValue_logicalVector = obj%values(index)
  end function getValue_logicalVector


  pure function logicalVector2str( vector ) 
    class(LogicalVectorType), intent(in) :: vector
    character( 2*NDIM ) :: logicalVector2str
    logicalVector2str = str( vector%values )
  end function logicalVector2str


  pure function getValues_intVector( obj )
    class(IntVectorType), intent(in) :: obj
    integer, dimension(NDIM) :: getValues_intVector
    getValues_intVector = obj%values
  end function getValues_intVector

  
  pure function getValue_intVector( obj, index )
    class(IntVectorType), intent(in) :: obj
    integer, intent(in) :: index
    integer :: getValue_realVector
    getValue_intVector = obj%values(index)
  end function getValue_intVector
  }})

  
  !-------------------------------------------------------------------
  !- IntVector procedures
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_INTVECTOR}, {{
  pure function intVector( values )
    integer, dimension(NDIM), intent(in) :: values
    type(IntVectorType) :: intVector
    intVector%values = values
  end function intVector
  

  !> creates a unit vector in the direction iDim, e.g. in 3D space,
  !> unitVector(2) evaluates to (/ 0, 1, 0 /).
  pure function intUnitVector( iDim )
    integer, intent(in) :: iDim 
    type(IntVectorType) :: intUnitVector    
    intUnitVector = 0
    intUnitVector%values(iDim) = 1
  end function intUnitVector


  
  elemental subroutine assign_intVector_intVector( tgt, src )
    class(IntVectorType), intent(out) :: tgt
    class(IntVectorType), intent(in) :: src
    tgt%mask = src%mask
    tgt%values = src%values
  end subroutine assign_intVector_intVector
  

  elemental subroutine assign_intVector_scalar( tgt, src )
    class(IntVectorType), intent(out) :: tgt
    integer, intent(in) :: src
    tgt%values = src
  end subroutine assign_intVector_scalar
  

  pure subroutine assign_intVector_intDim1( tgt, src )
    class(IntVectorType), intent(out) :: tgt
    integer, dimension(NDIM), intent(in) :: src
    tgt%values = src
  end subroutine assign_intVector_intDim1
  
  
 
  elemental function add_intVector_scalar( a, b )
    class(IntVectorType), intent(in) :: a
    integer, intent(in) :: b
    type(IntVectorType) :: add_intVector_scalar
    where ( a%mask )
       add_intVector_scalar%values = a%values + b
    elsewhere
       add_intVector_scalar%values = a%values
    end where
  end function add_intVector_scalar
  

  elemental function add_intVector_intVector( a, b )
    class(IntVectorType), intent(in) :: a, b
    type(IntVectorType) :: add_intVector_intVector
    where ( a%mask )
       add_intVector_intVector%values = a%values
    elsewhere
       add_intVector_intVector%values = 0._FLOAT
    end where
    where ( b%mask )
       add_intVector_intVector%values = &
            add_intVector_intVector%values + b%values
    end where
  end function add_intVector_intVector
  

  elemental function subtract_intVector_scalar( a, b )
    class(IntVectorType), intent(in) :: a
    integer, intent(in) :: b
    type(IntVectorType) :: subtract_intVector_scalar
    where ( a%mask )
       subtract_intVector_scalar%values = a%values - b
    elsewhere
       subtract_intVector_scalar%values = a%values
    end where
  end function subtract_intVector_scalar
  

  elemental function subtract_intVector_intVector( a, b )
    class(IntVectorType), intent(in) :: a, b
    type(IntVectorType) :: subtract_intVector_intVector
    where ( a%mask )
       subtract_intVector_intVector%values = a%values
    elsewhere
       subtract_intVector_intVector%values = 0._FLOAT
    end where
    where ( b%mask )
       subtract_intVector_intVector%values = &
            subtract_intVector_intVector%values - b%values
    end where
  end function subtract_intVector_intVector


  elemental function equalTo_intVector_scalar( a, b )
    class(IntVectorType), intent(in) :: a
    integer, intent(in) :: b
    type(LogicalVectorType) :: equalTo_intVector_scalar
    logical, dimension(NDIM) :: work
    where ( (a%mask == .true.) .and. (a%values == b) )
       work = .true.
    elsewhere
       work = .false. 
    end where
    equalTo_intVector_scalar = work
  end function equalTo_intVector_scalar


  elemental function equalTo_intVector_intVector( a, b )
    class(IntVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: equalTo_intVector_intVector
    logical, dimension(NDIM) :: work
    where ( (a%mask == .true.) .and. (b%mask == .true.) .and. (&
         a%values == b%values) )
       work = .true.
    elsewhere ( (a%mask == .false.) .and. (b%mask == .false.) )
       work = .true.
    elsewhere
       work = .false. 
    end where
    equalTo_intVector_intVector = work
  end function equalTo_intVector_intVector
  
  
  elemental function notEqualTo_intVector_scalar( a, b )
    class(IntVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: notEqualTo_intVector_scalar
    notEqualTo_intVector_scalar = .not. (a == b)
  end function notEqualTo_intVector_scalar
  
  
  elemental function notEqualTo_intVector_intVector( a, b )
    class(IntVectorType), intent(in) :: a
    integer, intent(in) :: b
    type(LogicalVectorType) :: notEqualTo_intVector_intVector
    notEqualTo_intVector_intVector = .not. (a == b)
  end function notEqualTo_intVector_intVector

  
  pure subroutine setElement_intVector( obj, index, scalar )
    class(IntVectorType), intent(inout) :: obj
    integer, intent(in) :: index
    integer, intent(in) :: scalar
    obj%values( index ) = scalar
  end subroutine setElement_intVector

  
  elemental subroutine setMask_intVector_scalar( obj, scalar )
    class(IntVectorType), intent(inout) :: obj
    logical, intent(in) :: scalar
    obj%mask = scalar
  end subroutine setMask_intVector_scalar

  
  elemental subroutine setMask_intVector_logicalVector( obj, mask )
    class(IntVectorType), intent(inout) :: obj
    type(LogicalVectorType), intent(in) :: mask
    obj%mask = mask%getValues()
  end subroutine setMask_intVector_logicalVector
  

  elemental function dotProduct_intVector( obj, vector )
    class(IntVectorType), intent(in) :: obj
    class(IntVectorType), intent(in) :: vector
    integer :: dotProduct_intVector
    dotProduct_intVector = dot_product( obj%values, vector%values )
  end function dotProduct_intVector


  pure function intVector2str( vector ) 
    class(IntVectorType), intent(in) :: vector
    character(  (1 + maxval(nDigits(vector%values)))*NDIM ) :: &
         intVector2str
    intVector2str = str( vector%values )
  end function intVector2str

  
  elemental function real_intVector( obj )
    class(IntVectorType), intent(in) :: obj
    type(RealVectorType) :: real_intVector
    real_intVector = real( obj%values, FLOAT )
  end function real_intVector

  
  elemental function product_intVector( obj )
    class(IntVectorType), intent(in) :: obj
    integer :: product_intVector
    product_intVector = product( obj%values )
  end function product_intVector


  pure function sizeVector( values )
    integer, dimension(NDIM), intent(in) :: values
    type(SizeVectorType) :: sizeVector
    sizeVector%values = values
  end function sizeVector

  
  !> when obj represents a grid size, this converts a multidimensional
  !> array address into the 1D array index implied by Fortran array
  !> element ordering.
  pure function address2index( obj, gridAddress )
    class(SizeVectorType), intent(in) :: obj
    class(IntVectorType), intent(in) :: gridAddress
    integer :: address2index, i, n
    address2index = gridAddress%values(1)
    n = 1
    do i = 2, NDIM
       n = n*obj%values(i-1)
       address2index = address2index + (gridAddress%values(i) - 1)*n
    end do
  end function address2index

  
  !> when obj represents a grid size, this converts a 1D array index
  !> into the multidimensional array address implied by Fortran array
  !> element ordering.
  pure function index2address( obj, index )
    class(SizeVectorType), intent(in) :: obj
    integer, intent(in) :: index
    type(IntVectorType) :: index2address
    integer :: i, n
    index2address = 0
    n = 1
    do i = 1, NDIM
       index2address%values(i) = mod( (index-1)/n, &
            obj%values(i) ) + 1
       n = n*obj%values(i)
    end do
  end function index2address
  }})  
  

    
  !-------------------------------------------------------------------
  !- RealVector procedures
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_REALVECTOR}, {{
  pure function realVector( values )
    real(FLOAT), dimension(NDIM), intent(in), optional :: values
    type(RealVectorType) :: realVector
    if ( present(values) ) then
       realVector%values = values
    else
       realVector%values = 0._FLOAT
    end if
  end function realVector

  
  ! as above, but normalise
  pure function realUnitVector_byComponents( values )
    real(FLOAT), dimension(NDIM), intent(in) :: values
    type(RealVectorType) :: realUnitVector_byComponents
    realUnitVector_byComponents = realVector( values )
    call realUnitVector_byComponents%normalise()
  end function realUnitVector_byComponents
  

  ! as above, but normalise
  pure function realUnitVector_byAngle( angle )
    real(FLOAT), intent(in) :: angle
    type(RealVectorType) :: realUnitVector_byAngle
    realUnitVector_byAngle = realVector( [cos(angle), sin(angle)] )
  end function realUnitVector_byAngle


  elemental subroutine assign_realVector_realVector( tgt, src )
    type(RealVectorType), intent(out) :: tgt
    type(RealVectorType), intent(in) :: src
    tgt%values = src%values
  end subroutine assign_realVector_realVector
  

  elemental subroutine assign_realVector_scalar( tgt, src )
    type(RealVectorType), intent(out) :: tgt
    real(FLOAT), intent(in) :: src
    tgt%values = src
  end subroutine assign_realVector_scalar
  

  pure subroutine assign_realVector_realDim1( tgt, src )
    type(RealVectorType), intent(out) :: tgt
    real(FLOAT), dimension(NDIM), intent(in) :: src
    tgt%values = src
  end subroutine assign_realVector_realDim1

  
  elemental function equalTo_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: equalTo_realVector
    equalTo_realVector = abs( a%values - b%values ) < EPS
  end function equalTo_realVector

  
  elemental function lessThan_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: lessThan_realVector
    lessThan_realVector = a%values < b%values
  end function lessThan_realVector
  
  
  elemental function greaterThan_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: greaterThan_realVector
    greaterThan_realVector = a%values > b%values
  end function greaterThan_realVector

  
  elemental function lessThanOrEqualTo_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: lessThanOrEqualTo_realVector
    lessThanOrEqualTo_realVector = a%values <= b%values
  end function lessThanOrEqualTo_realVector
  
  
  elemental function greaterThanOrEqualTo_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(LogicalVectorType) :: greaterThanOrEqualTo_realVector
    greaterThanOrEqualTo_realVector = a%values >= b%values
  end function greaterThanOrEqualTo_realVector

  
  elemental function add_realVector_scalar( a, b )
    type(RealVectorType), intent(in) :: a
    real(FLOAT), intent(in) :: b
    type(RealVectorType) :: add_realVector_scalar
    add_realVector_scalar%values = a%values + b
  end function add_realVector_scalar
  

  elemental function add_realVector_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(RealVectorType) :: add_realVector_realVector
    add_realVector_realVector%values = a%values + b%values
  end function add_realVector_realVector
  

  elemental function subtract_realVector_scalar( a, b )
    type(RealVectorType), intent(in) :: a
    real(FLOAT), intent(in) :: b
    type(RealVectorType) :: subtract_realVector_scalar
    subtract_realVector_scalar%values = a%values - b
  end function subtract_realVector_scalar
  

  elemental function subtract_realVector_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(RealVectorType) :: subtract_realVector_realVector
    subtract_realVector_realVector%values = a%values - b%values
  end function subtract_realVector_realVector
  

  elemental function multiply_realVector_scalar( a, b )
    type(RealVectorType), intent(in) :: a
    real(FLOAT), intent(in) :: b
    type(RealVectorType) :: multiply_realVector_scalar
    multiply_realVector_scalar%values = a%values * b
  end function multiply_realVector_scalar
  

  elemental function multiply_realVector_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(RealVectorType) :: multiply_realVector_realVector
    multiply_realVector_realVector%values = a%values * b%values
  end function multiply_realVector_realVector
  

  elemental function divide_realVector_scalar( a, b )
    type(RealVectorType), intent(in) :: a
    real(FLOAT), intent(in) :: b
    type(RealVectorType) :: divide_realVector_scalar
    divide_realVector_scalar%values = a%values / b
  end function divide_realVector_scalar
  

  elemental function divide_realVector_realVector( a, b )
    type(RealVectorType), intent(in) :: a, b
    type(RealVectorType) :: divide_realVector_realVector
    divide_realVector_realVector%values = a%values / b%values
  end function divide_realVector_realVector

  
  pure subroutine setElement_realVector( obj, index, scalar )
    class(RealVectorType), intent(inout) :: obj
    integer, intent(in) :: index
    real(FLOAT), intent(in) :: scalar
    obj%values( index ) = scalar
  end subroutine setElement_realVector

  
  elemental function magnitude_realVector( obj )
    class(RealVectorType), intent(in) :: obj
    real(FLOAT) :: magnitude_realVector
    magnitude_realVector = sqrt(sum(obj%values**2))
  end function magnitude_realVector

  
  elemental subroutine normalise_realVector( obj )
    class(RealVectorType), intent(inout) :: obj
    obj%values = obj%values / sqrt(sum(obj%values**2))
  end subroutine normalise_realVector


  pure subroutine applyMask_realVector( obj, mask )
    class(RealVectorType), intent(inout) :: obj
    class(LogicalVectorType), intent(in) :: mask
    obj%values( .not. mask%getValues() ) = 0._FLOAT
  end subroutine applyMask_realVector

  
  elemental function dotProduct_realVector( obj, vector )
    class(RealVectorType), intent(in) :: obj
    class(RealVectorType), intent(in) :: vector
    real(FLOAT) :: dotProduct_realVector
    dotProduct_realVector = dot_product( obj%values, vector%values )
  end function dotProduct_realVector

  
  elemental function anyNaN_realVector( obj )
    class(RealVectorType), intent(in) :: obj
    logical :: anyNaN_realVector
    anyNaN_realVector = any(ieee_is_nan(obj%values))
  end function anyNaN_realVector

  
  pure function getValues_realVector( obj )
    class(RealVectorType), intent(in) :: obj
    real(FLOAT), dimension(NDIM) :: getValues_realVector
    getValues_realVector = obj%values
  end function getValues_realVector

  
  pure function getValue_realVector( obj, index )
    class(RealVectorType), intent(in) :: obj
    integer, intent(in) :: index
    real(FLOAT) :: getValue_realVector
    getValue_realVector = obj%values(index)
  end function getValue_realVector

  
  pure function realVector2str( vector ) 
    class(RealVectorType), intent(in) :: vector
    character( 1 + NDIM*REALSP ) :: realVector2str
    realVector2str = str( vector%values )
  end function realVector2str
  }})
  
  
  !-------------------------------------------------------------------
  !- RealTensor procedures/methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_REALTENSOR}, {{
  pure function realTensor( vectors )
    type(RealVectorType), dimension(NDIM), intent(in) :: vectors
    type(RealTensorType) :: realTensor
    integer :: i
    do i = 1, NDIM
       realTensor%values(i, :) = vectors(i)%values
    end do
  end function realTensor


  elemental subroutine assign_realTensor_realTensor( tgt, src )
    type(RealTensorType), intent(out) :: tgt
    type(RealTensorType), intent(in) :: src
    tgt%values = src%values
  end subroutine assign_realTensor_realTensor

    
  elemental subroutine assign_realTensor_scalar( tgt, src )
    type(RealTensorType), intent(out) :: tgt
    real(FLOAT), intent(in) :: src
    tgt%values = src
  end subroutine assign_realTensor_scalar

  
  pure subroutine assign_realTensor_realDim2( tgt, src )
    type(RealTensorType), intent(out) :: tgt
    real(FLOAT), dimension(NDIM, NDIM), intent(in) :: src
    tgt%values = src
  end subroutine assign_realTensor_realDim2
  
  
  pure subroutine setElement_realTensor( obj, row, column, scalar )
    class(RealTensorType), intent(inout) :: obj
    integer, intent(in) :: row, column
    real(FLOAT), intent(in) :: scalar
    obj%values( row, column ) = scalar
  end subroutine setElement_realTensor
    

  pure subroutine setRow_realTensor_scalar( obj, row, scalar )
    class(RealTensorType), intent(inout) :: obj
    integer, intent(in) :: row
    real(FLOAT), intent(in) :: scalar
    obj%values( row, : ) = scalar
  end subroutine setRow_realTensor_scalar
  

  pure subroutine setRow_realTensor_realVector( obj, row, vector )
    class(RealTensorType), intent(inout) :: obj
    integer, intent(in) :: row
    type(RealVectorType), intent(in) :: vector
    obj%values( row, : ) = vector%values
  end subroutine setRow_realTensor_realVector


  pure subroutine setRow_realTensor_realTensor( obj, row, tensor )
    class(RealTensorType), intent(inout) :: obj
    integer, intent(in) :: row
    type(RealTensorType), intent(in) :: tensor
    obj%values( :, row ) = tensor%values( :, row )
  end subroutine setRow_realTensor_realTensor

  
  pure subroutine setColumn_realTensor_scalar( obj, column, scalar )
    class(RealTensorType), intent(inout) :: obj
    integer, intent(in) :: column
    real(FLOAT), intent(in) :: scalar
    obj%values( :, column ) = scalar
  end subroutine setColumn_realTensor_scalar


  pure subroutine setColumn_realTensor_realVector( obj, column, vector )
    class(RealTensorType), intent(inout) :: obj
    integer, intent(in) :: column
    type(RealVectorType), intent(in) :: vector
    obj%values( :, column ) = vector%values
  end subroutine setColumn_realTensor_realVector


  pure subroutine setColumn_realTensor_realTensor( obj, column, tensor )
    class(RealTensorType), intent(inout) :: obj
    integer, intent(in) :: column
    type(RealTensorType), intent(in) :: tensor
    obj%values( :, column ) = tensor%values( :, column )
  end subroutine setColumn_realTensor_realTensor


!!$  subroutine set_realTensor_realTensor( obj, tensor, row, column )
!!$    class(RealTensorType), intent(inout) :: obj
!!$    type(RealTensorType), intent(in) :: tensor
!!$    integer, intent(in), optional :: row, column
!!$    if ( present(row) .and. present(column) ) then
!!$       obj%values( row, column ) = tensor%values( row, column )
!!$    else if ( present(row) ) then
!!$       obj%values( row, : ) = tensor%values( row, : )
!!$    else if ( present(column) ) then
!!$       obj%values( :, column ) = tensor%values( :, column )
!!$    else
!!$       obj%values = tensor%values
!!$    end if
!!$  end subroutine set_realTensor_realTensor

  
  pure function getRow_realTensor( obj, row )
    class(RealTensorType), intent(in) :: obj
    integer, intent(in) :: row
    type(RealVectorType) :: getRow_realTensor
    getRow_realTensor = obj%values( row, : )
  end function getRow_realTensor
  
  
  pure function getColumn_realTensor( obj, column )
    class(RealTensorType), intent(in) :: obj
    integer, intent(in) :: column
    type(RealVectorType) :: getColumn_realTensor
    getColumn_realTensor = obj%values( :, column )
  end function getColumn_realTensor
  }})
  

end module Global_Base
