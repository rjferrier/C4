module PointMomentFields_DomainDim_2
  
  use LogModule
  use Global
  
  implicit none
  private
  
  
  character(*), parameter, private :: MOD_NAME = &
       'PointMomentFields_DomainDim'
  
  
  type, public :: PointScalarMomentGrid_NDPartsType
     private
     ! dimensions correspond to (1, 2) polytopes and (3) points on
     ! each polytope
     real(FLOAT), dimension(:, :, :), allocatable :: values
   contains
     procedure :: init_ScalarGrid_NDParts_args
     procedure :: init_ScalarGrid_NDParts_copy
     generic :: init => init_ScalarGrid_NDParts_args, &
          init_ScalarGrid_NDParts_copy
     procedure :: deinit => deinit_ScalarGrid_NDParts
     procedure :: setField_scalar => setField_scalar_Scalar
     procedure :: setField_array => setField_array_Scalar
     generic :: setField => setField_scalar, setField_array
     procedure :: getValue => getValue_Scalar
     procedure :: getValues => getValues_Scalar
  end type PointScalarMomentGrid_NDPartsType
  

  type, public :: PointVectorMomentGrid_NDPartsType
     private
     ! dimensions correspond to (1, 2) polytopes and (3) points on
     ! each polytope
     type(RealVectorType), dimension(:, :, :), allocatable :: values
   contains
     procedure :: init_VectorGrid_NDParts_args
     procedure :: init_VectorGrid_NDParts_copy
     generic :: init => init_VectorGrid_NDParts_args, &
          init_VectorGrid_NDParts_copy
     procedure :: deinit => deinit_VectorGrid_NDParts
     procedure :: setField_scalar => setField_scalar_Vector
     procedure :: setField_array => setField_array_Vector
     generic :: setField => setField_scalar, setField_array
     procedure :: getValue => getValue_Vector
     procedure :: getValues => getValues_Vector
  end type PointVectorMomentGrid_NDPartsType

  
  
contains

  
  !-------------------------------------------------------------------
  !- Scalar variant
  !-------------------------------------------------------------------
  
  subroutine init_ScalarGrid_NDParts_args( obj, gridSize, &
       nPointsPerPolytope, log )
    class(PointScalarMomentGrid_NDPartsType), intent(inout) :: obj
    class(SizeVectorType), intent(in) :: gridSize
    integer, intent(in) :: nPointsPerPolytope
    class(LogType), intent(inout), optional :: log
    integer, dimension(NDIM) :: n
    integer :: allocStat
    call beginSub( MOD_NAME, 'init_ScalarGrid_NDParts', log )
    
    n = gridSize%getValues()
    
    ! use pointArrangement and polytopeGrid to help set up the grid
    ! of values
    allocate( obj%values(n(1), n(2), nPointsPerPolytope), stat=&
         allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &obj%values(n1, n2).  STAT='//int2str(allocStat), log )

    call endSub( log )
  end subroutine init_ScalarGrid_NDParts_args

  
  subroutine init_ScalarGrid_NDParts_copy( obj, src, log )
    class(PointScalarMomentGrid_NDPartsType), intent(out) :: obj
    class(PointScalarMomentGrid_NDPartsType), intent(in) :: src
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'init_ScalarGrid_NDParts_copy', log )

    allocate( obj%values(size(src%values, 1), size(src%values, 2), &
         size(src%values, 3)), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%values(&
         &size(src%values, 1), size(src%values, 2), size(src%values, &
         &3)).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%values = src%values

    call endSub( log )
  end subroutine init_ScalarGrid_NDParts_copy

  
  subroutine deinit_ScalarGrid_NDParts( obj, log )
    class(PointScalarMomentGrid_NDPartsType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_ScalarGrid_NDParts', &
         log )
    
    deallocate( obj%values, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
         &obj%values.  STAT='//int2str(deallocStat), log )
    
    call endSub( log )
  end subroutine deinit_ScalarGrid_NDParts

  
  pure subroutine setField_scalar_Scalar( obj, src )
    class(PointScalarMomentGrid_NDPartsType), intent(inout) :: obj
    real(FLOAT), intent(in) :: src
    obj%values = src
  end subroutine setField_scalar_Scalar
  
  
  pure subroutine setField_array_Scalar( obj, src )
    class(PointScalarMomentGrid_NDPartsType), intent(inout) :: obj
    real(FLOAT), dimension(:), intent(in) :: src
    obj%values = reshape( src, [size(obj%values, 1), size(&
         obj%values, 2), size(obj%values, 3)] )
  end subroutine setField_array_Scalar


  pure subroutine getValue_Scalar( obj, value, gridParameters, &
       polytopeIndex, pointIndex )
    class(PointScalarMomentGrid_NDPartsType), intent(in) :: obj
    real(FLOAT), intent(out) :: value
    type(GridParametersType), intent(in) :: gridParameters
    integer, intent(in) :: polytopeIndex, pointIndex
    type(IntVectorType) :: a
    integer, dimension(NDIM) :: i
    
    a = gridParameters%size%index2address( polytopeIndex )
    i = a%getValues()
    value = obj%values( i(1), i(2), pointIndex )
  end subroutine getValue_Scalar


  pure subroutine getValues_Scalar( obj, values, gridParameters, &
       polytopeIndex )
    class(PointScalarMomentGrid_NDPartsType), intent(in) :: obj
    real(FLOAT), dimension(:), intent(out) :: values
    type(GridParametersType), intent(in) :: gridParameters
    integer, intent(in) :: polytopeIndex
    type(IntVectorType) :: a
    integer, dimension(NDIM) :: i

    a = gridParameters%size%index2address( polytopeIndex )
    i = a%getValues()
    values = obj%values( i(1), i(2), : )
  end subroutine getValues_Scalar

  
  !-------------------------------------------------------------------
  !- Vector variant
  !-------------------------------------------------------------------
  
  subroutine init_VectorGrid_NDParts_args( obj, gridSize, &
       nPointsPerPolytope, log )
    class(PointVectorMomentGrid_NDPartsType), intent(inout) :: obj
    class(SizeVectorType), intent(in) :: gridSize
    integer, intent(in) :: nPointsPerPolytope
    class(LogType), intent(inout), optional :: log
    integer, dimension(NDIM) :: n
    integer :: allocStat
    call beginSub( MOD_NAME, 'init_VectorGrid_NDParts_args', log )
    
    n = gridSize%getValues()
    
    ! use pointArrangement and polytopeGrid to help set up the grid
    ! of values
    allocate( obj%values(n(1), n(2), nPointsPerPolytope), stat=&
         allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating &
         &obj%values(n1, n2).  STAT='//int2str(allocStat), log )

    call endSub( log )
  end subroutine init_VectorGrid_NDParts_args

  
  subroutine init_VectorGrid_NDParts_copy( obj, src, log )
    class(PointVectorMomentGrid_NDPartsType), intent(out) :: obj
    class(PointVectorMomentGrid_NDPartsType), intent(in) :: src
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'init_VectorGrid_NDParts_copy', log )

    allocate( obj%values(size(src%values, 1), size(src%values, 2), &
         size(src%values, 3)), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%values(&
         &size(src%values, 1), size(src%values, 2), size(src%values, &
         &3)).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%values = src%values

    call endSub( log )
  end subroutine init_VectorGrid_NDParts_copy

  
  subroutine deinit_VectorGrid_NDParts( obj, log )
    class(PointVectorMomentGrid_NDPartsType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_VectorGrid_NDParts', &
         log )
    
    deallocate( obj%values, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
         &obj%values.  STAT='//int2str(deallocStat), log )
    
    call endSub( log )
  end subroutine deinit_VectorGrid_NDParts

  
  pure subroutine setField_scalar_Vector( obj, src )
    class(PointVectorMomentGrid_NDPartsType), intent(inout) :: obj
    type(RealVectorType), intent(in) :: src
    obj%values = src
  end subroutine setField_scalar_Vector
  
  
  pure subroutine setField_array_Vector( obj, src )
    class(PointVectorMomentGrid_NDPartsType), intent(inout) :: obj
    type(RealVectorType), dimension(:), intent(in) :: src
    obj%values = reshape( src, [size(obj%values, 1), size(&
         obj%values, 2), size(obj%values, 3)] )
  end subroutine setField_array_Vector


  pure subroutine getValue_Vector( obj, value, gridParameters, &
       polytopeIndex, pointIndex )
    class(PointVectorMomentGrid_NDPartsType), intent(in) :: obj
    type(RealVectorType), intent(out) :: value
    type(GridParametersType), intent(in) :: gridParameters
    integer, intent(in) :: polytopeIndex, pointIndex
    type(IntVectorType) :: a
    integer, dimension(NDIM) :: i
    
    a = gridParameters%size%index2address( polytopeIndex )
    i = a%getValues()
    value = obj%values( i(1), i(2), pointIndex )
  end subroutine getValue_Vector


  pure subroutine getValues_Vector( obj, values, gridParameters, &
       polytopeIndex )
    class(PointVectorMomentGrid_NDPartsType), intent(in) :: obj
    type(RealVectorType), dimension(:), intent(out) :: values
    type(GridParametersType), intent(in) :: gridParameters
    integer, intent(in) :: polytopeIndex
    type(IntVectorType) :: a
    integer, dimension(NDIM) :: i

    a = gridParameters%size%index2address( polytopeIndex )
    i = a%getValues()
    values = obj%values( i(1), i(2), : )
  end subroutine getValues_Vector

  
  
end module PointMomentFields_DomainDim_2
