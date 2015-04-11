HEADER()
module PointArrangementsModule

  use PointArrangements_DomainDim
  
  USE_MACROS({PointArrangements})
  use LogModule
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule
  
  implicit none
  private

  public :: createArrayPointArrangement, createGridPointArrangement
  
  character(*), parameter :: MOD_NAME = 'PointArrangementsModule'

  ! in the proceeding code, Gaussian coordinates and weights will be
  ! defined.  We can start by defining the universal weight for degree-1
  ! polynomials.
  real(FLOAT), dimension(1), parameter :: &
       GAUSSIANWEIGHTS_ANY_DEG1 = [1._FLOAT]

  
  !-------------------------------------------------------------------
  !- ArrayPointArrangement type definition and Gaussian point details
  !-------------------------------------------------------------------
  
  ! corresponds to simplicial polytopes.
  EXPAND({TYPEDEF_SUBPOINTARRANGEMENT({Array},
     {! the following is a matrix of barycentric coordinates.  Each row
     ! corresponds to a vertex on the polytope; each column to an arrival
     ! point.
     real(FLOAT), dimension(:, :), allocatable :: pointPattern

     ! Gaussian quadrature weights are directly linked to the point
     ! pattern, so we might as well reference these as well.
     real(FLOAT), dimension(:), allocatable :: gaussianWeights},
     
     {procedure, private :: checkPoints => checkPoints_Array})})

  !-----------------------------------------------------------------------
  real(FLOAT), dimension(1,1), parameter :: POINTPATTERN_POINT = &
       reshape( [1._FLOAT], [1, 1] )
  
  !-----------------------------------------------------------------------
  real(FLOAT), dimension(2,1), parameter :: &
       POINTPATTERN_LINE_DEG1 = reshape( [0.5_FLOAT, 0.5_FLOAT], [2, 1] )

  real(FLOAT), dimension(2,2), parameter :: &
       POINTPATTERN_LINE_DEG3 = reshape( [&
       (1._FLOAT + 1._FLOAT/sqrt(3._FLOAT))/2._FLOAT, &
       (1._FLOAT - 1._FLOAT/sqrt(3._FLOAT))/2._FLOAT, &
       (1._FLOAT - 1._FLOAT/sqrt(3._FLOAT))/2._FLOAT, &
       (1._FLOAT + 1._FLOAT/sqrt(3._FLOAT))/2._FLOAT ], [2, 2] )
  real(FLOAT), dimension(2), parameter :: &
       GAUSSIANWEIGHTS_LINE_DEG3 = [0.5_FLOAT, 0.5_FLOAT]

  !-----------------------------------------------------------------------
  real(FLOAT), dimension(3,1), parameter :: &
       POINTPATTERN_TRIANGLE_DEG1 = reshape( [&
       [1._FLOAT, 1._FLOAT, 1._FLOAT]/3._FLOAT ], [3, 1] )

  real(FLOAT), dimension(3,3), parameter :: &
       POINTPATTERN_TRIANGLE_DEG2 = reshape( [&
       4._FLOAT, 1._FLOAT, 1._FLOAT, &
       1._FLOAT, 4._FLOAT, 1._FLOAT, &
       1._FLOAT, 1._FLOAT, 4._FLOAT ]/6._FLOAT, [3, 3] )
  real(FLOAT), dimension(3), parameter :: &
       GAUSSIANWEIGHTS_TRIANGLE_DEG2 = [1._FLOAT, 1._FLOAT, 1._FLOAT]/&
       3._FLOAT

  real(FLOAT), dimension(3,4), parameter :: &
       POINTPATTERN_TRIANGLE_DEG3 = reshape( [[&
       3._FLOAT, 1._FLOAT, 1._FLOAT, &
       1._FLOAT, 3._FLOAT, 1._FLOAT, &
       1._FLOAT, 1._FLOAT, 3._FLOAT]/5._FLOAT, &
       [1._FLOAT,  1._FLOAT,  1._FLOAT]/3._FLOAT ], [3, 4] )
  real(FLOAT), dimension(4), parameter :: &
       GAUSSIANWEIGHTS_TRIANGLE_DEG3 = [[1._FLOAT,  1._FLOAT,  1._FLOAT]*&
       25._FLOAT/48._FLOAT, -9._FLOAT/16._FLOAT]
  

  !-------------------------------------------------------------------
  !- GridPointArrangement type definition and Gaussian point details
  !-------------------------------------------------------------------

  ! corresponds to orthotopic polytopes.
  EXPAND({TYPEDEF_SUBPOINTARRANGEMENT({Grid},
     {! the following is an array of unit-normalised Cartesian ordinates.
     ! Permutations of these (repeat ordinates allowed) make up the
     ! unit-normalised coordinates of the Gaussian points.  Therefore, if
     ! N is the dimension of the associated orthotope, there are
     ! size(pointPattern)**N points.
     real(FLOAT), dimension(:), allocatable :: pointPattern

     ! 1D Gaussian weights are then extended to multidimensions by simply
     ! multiplying corresponding permutations of them together.
     real(FLOAT), dimension(:), allocatable :: gaussianWeights},
     
     {procedure, private :: checkPoints => checkPoints_Grid})})

  !-----------------------------------------------------------------------
  real(FLOAT), dimension(1), parameter :: POINTPATTERN_ORTH_DEG1 = &
       [0.5_FLOAT]
  
  !-----------------------------------------------------------------------
  real(FLOAT), dimension(2), parameter :: POINTPATTERN_ORTH_DEG3 = &
       [ (1._FLOAT + 1._FLOAT/sqrt(3._FLOAT))/2._FLOAT, &
       (1._FLOAT - 1._FLOAT/sqrt(3._FLOAT))/2._FLOAT ]
  real(FLOAT), dimension(2), parameter :: &
       GAUSSIANWEIGHTS_ORTH_DEG3 = [0.5_FLOAT, 0.5_FLOAT]
  
contains
 
  
  !-------------------------------------------------------------------
  !- ArrayPointArrangement methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_SUBPOINTARRANGEMENT({Array})})
  
  function convertToPolytopeValues_Array( obj, &
       valuesMatrix ) result ( valuesArray )
    class(ArrayPointArrangementType), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: valuesMatrix
    real(FLOAT), dimension(size(valuesMatrix, 2)) :: valuesArray
    integer :: i, n

    forall ( i = 1:size(valuesArray) )
       valuesArray(i) = sum( valuesMatrix(:, i) * obj%gaussianWeights )
    end forall
  end function convertToPolytopeValues_Array

  
  pure subroutine createPointPattern_Array( obj, pointPattern, log )
    class(ArrayPointArrangementType), intent(in) :: obj
    real(FLOAT), dimension(:, :), allocatable, intent(inout) :: &
         pointPattern
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'createPointPattern_Array', log )
    
    select case ( obj%polytopeDim )
    case (0)
       call clone( pointPattern, POINTPATTERN_POINT, log )
    case (1)
       select case ( obj%degree )
       case (:1)
          call clone( pointPattern, POINTPATTERN_LINE_DEG1, log )
       case (2:3)
          call clone( pointPattern, POINTPATTERN_LINE_DEG3, log )
       end select
    case (2)
       select case ( obj%degree )
       case (:1)
          call clone( pointPattern, POINTPATTERN_TRIANGLE_DEG1, log )
       case (2)
          call clone( pointPattern, POINTPATTERN_TRIANGLE_DEG2, log )
       case (3)
          call clone( pointPattern, POINTPATTERN_TRIANGLE_DEG3, log )
       end select
    end select
    
    call addEvent( .not. allocated(pointPattern), FATAL, 'Point pattern &
         &was not allocated.', log )
    
    call endSub( log )
  end subroutine createPointPattern_Array
 

  pure subroutine createGaussianWeights_Array( obj, gaussianWeights, &
       log )
    class(ArrayPointArrangementType), intent(in) :: obj
    real(FLOAT), dimension(:), allocatable, intent(inout) :: &
         gaussianWeights
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'createGaussianWeights_Array', log )
    
    if ( (obj%polytopeDim == 0) .or. (obj%degree <= 1) ) then
       call clone( gaussianWeights, GAUSSIANWEIGHTS_ANY_DEG1, log )
       return
    end if

    select case ( obj%polytopeDim )
    case (1)
       select case ( obj%degree )
       case (2:3)
          call clone( gaussianWeights, GAUSSIANWEIGHTS_LINE_DEG3, log )
       end select
    case (2)
       select case ( obj%degree )
       case (2)
          call clone( gaussianWeights, GAUSSIANWEIGHTS_TRIANGLE_DEG2, log )
       case (3)
          call clone( gaussianWeights, GAUSSIANWEIGHTS_TRIANGLE_DEG3, log )
       end select
    end select

    call addEvent( .not. allocated(gaussianWeights), FATAL, 'Gaussian &
         &weights were not allocated.', log )
    
    call endSub( log )
  end subroutine createGaussianWeights_Array
  
  
  pure subroutine getPositions_Array( obj, pointCoords, polytopeIndex, &
       pointIndex, log )
    class(ArrayPointArrangementType), intent(in) :: obj
    type(RealVectorType), dimension(:), intent(out) :: pointCoords
    integer, intent(in) :: polytopeIndex
    integer, intent(in), optional :: pointIndex
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerSetType) :: pps
    type(PolytopePointerSetIteratorType) :: iterator
    type(PolytopePointerType), pointer :: pp
    type(RealVectorType) :: vertexCoords
    integer :: i, j1, j2, nv, np
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, 'getPositions_Array', log )

    ! if a pointIndex is given, limit column indicies to the
    ! corresponding column in pointPattern. 
    if ( present(pointIndex) ) then
       j1 = pointIndex
       j2 = pointIndex
    else
       j1 = 1
       j2 = size(pointCoords)
    end if

    ! check whether we have a valid set of arguments
    call obj%checkPoints( j2, present(pointIndex), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! collect polytope verticies.  Remember to add the host polytope.
    call obj%polytopeArray%collectSubPolytopes( polytopeIndex, pps, &
         log )
    call pp%init(obj%polytopeArray, polytopeIndex)
    call pps%append( pp, log )
    call pps%filter( 0, log )

    ! check that the size of pps matches pointPattern
    nv = pps%size()
    np = size(obj%pointPattern, 1)
    call addEvent( nv/=np, FATAL, 'The number of collected verticies ('//&
         str(nv)//') does not match the size of pointPattern ('//str(&
         np)//').', log )
    if ( checkSub(FATAL, log) ) then
       call addEvent( ADVICE, 'Listing verticies...', log )
       call iterator%init( pps )
       call iterator%first( pp )
       do
          if ( iterator%isDone() ) exit
          call addEvent( ADVICE, '  '//pp%describe(), log )
          call iterator%next( pp )
       end do
       call endSub( log )
       return
    end if
    
    ! for each vertex i, multiply the vertex's Cartesian coordinates
    ! with the ith barycentric coordinate of each point in the pattern
    ! and add to the respective points' Cartesian coordinates
    pointCoords = 0._FLOAT
    call iterator%init( pps )
    call iterator%first( pp )
    do i = 1, nv
       if ( iterator%isDone() ) exit
       call pp%getCentroid( vertexCoords )
       pointCoords = pointCoords + vertexCoords*obj%pointPattern(i, &
            j1:j2)
       if ( present(log) ) call addEvent( DEBUG_MODE, ADVICE, &
            'vertex #'//str(i)//' coords = '//str(vertexCoords%&
            getValues()), log )
       call iterator%next( pp )
    end do
    
    if ( DEBUG_MODE ) then
       do i = 1, size(pointCoords)
          call addEvent( DEBUG_MODE, ADVICE, 'point #'//str(i)//' &
               &coords = '//str(pointCoords(i)%getValues()), log )
       end do
    end if

    ! clean up
    call pps%deinit( log )

    call endSub( log )
  end subroutine getPositions_Array


  pure subroutine checkPoints_Array( obj, intValue, isPointIndex, log )
    class(ArrayPointArrangementType), intent(in) :: obj
    integer, intent(in) :: intValue
    logical, intent(in) :: isPointIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'checkPoints_Array', log )
    
    if ( isPointIndex ) then
       call addEvent( intValue > size(obj%pointPattern, 2), &
            FATAL, 'Client requested computation of point '//str(&
            intValue)//', but only '//str(size(obj%pointPattern, &
            2))//' points exist in the pattern.', log )
    else
       call addEvent( intValue /= size(obj%pointPattern, 2), &
            FATAL, 'Expected to compute '//str(size(&
            obj%pointPattern, 2))//' sets of point coordinates, but &
            &client supplied output array of size '//str(&
            intValue)//'.', log )
    end if
    
    call endSub( log )
  end subroutine checkPoints_Array

  
  pure function getNumPointsPerPolytope_Array( obj )
    class(ArrayPointArrangementType), intent(in) :: obj
    integer :: getNumPointsPerPolytope_Array
    getNumPointsPerPolytope_Array = size(obj%pointPattern, 2)
  end function getNumPointsPerPolytope_Array

  
  !-------------------------------------------------------------------
  !- GridPointArrangement methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_SUBPOINTARRANGEMENT({Grid})})

  
  function convertToPolytopeValues_Grid( obj, &
       valuesMatrix ) result ( valuesArray )
    class(GridPointArrangementType), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: valuesMatrix
    real(FLOAT), dimension(size(valuesMatrix, 2)) :: valuesArray
    integer :: i

    ! a forall block here causes a memory leak for some reason
    do i = 1, size(valuesArray)
       valuesArray(i) = sum( valuesMatrix(:, i) * &
            expandWeights(obj%polytopeGrid%getCartesianCode(), obj%&
            gaussianWeights) )
    end do
  end function convertToPolytopeValues_Grid

  
  pure subroutine createPointPattern_Grid( obj, pointPattern, log )
    class(GridPointArrangementType), intent(in) :: obj
    real(FLOAT), dimension(:), allocatable, intent(inout) :: &
         pointPattern
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'createPointPattern_Grid', log )
    
    select case ( obj%degree )
    case (:1)
       call clone( pointPattern, POINTPATTERN_ORTH_DEG1, log )
    case (2:3)
       call clone( pointPattern, POINTPATTERN_ORTH_DEG3, log )
    end select
    call addEvent( .not. allocated(pointPattern), FATAL, 'Point pattern &
         &was not allocated.', log )
    
    call endSub( log )
  end subroutine createPointPattern_Grid
 

  pure subroutine createGaussianWeights_Grid( obj, gaussianWeights, &
       log )
    class(GridPointArrangementType), intent(in) :: obj
    real(FLOAT), dimension(:), allocatable, intent(inout) :: &
         gaussianWeights
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'createGaussianWeights_Grid', log )

    select case ( obj%degree )
    case (:1)
       call clone( gaussianWeights, GAUSSIANWEIGHTS_ANY_DEG1, log )
    case (2:3)
       call clone( gaussianWeights, GAUSSIANWEIGHTS_ORTH_DEG3, log )
    end select
    call addEvent( .not. allocated(gaussianWeights), FATAL, 'Gaussian &
         &weights were not allocated.', log )
    
    call endSub( log )
  end subroutine createGaussianWeights_Grid


  pure subroutine getPositions_Grid( obj, pointCoords, polytopeIndex, &
       pointIndex, log )
    class(GridPointArrangementType), intent(in) :: obj
    integer, intent(in) :: polytopeIndex
    integer, intent(in), optional :: pointIndex
    type(RealVectorType), dimension(:), intent(out) :: pointCoords
    class(LogType), intent(inout), optional :: log
    type(RealVectorType), dimension(size(obj%pointPattern)**NDIM) :: &
         work
    integer :: i, j1, j2
    call beginSub( MOD_NAME, 'getPositions_Grid', log )

    ! if a pointIndex is given, limit column indicies to the
    ! corresponding column in pointPattern. 
    if ( present(pointIndex) ) then
       j1 = pointIndex
       j2 = pointIndex
    else
       j1 = 1
       j2 = size(pointCoords)
    end if

    ! check whether we have a valid set of arguments
    call obj%checkPoints( obj%polytopeGrid%getPolytopeDim(), j2, present(&
         pointIndex), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    pointCoords = expandPointCoords( obj%polytopeGrid%&
         getCartesianCode(), obj%pointPattern, obj%polytopeGrid%&
         getGridParameters(), obj%polytopeGrid%convertToGridAddress(&
         polytopeIndex) )
    
    call endSub( log )
  end subroutine getPositions_Grid


  pure subroutine checkPoints_Grid( obj, polytopeDim, intValue, &
       isPointIndex, log )
    class(GridPointArrangementType), intent(in) :: obj
    integer, intent(in) :: polytopeDim, intValue
    logical, intent(in) :: isPointIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'checkPoints_Grid', log )
    
    if ( isPointIndex ) then
       call addEvent( intValue > size(obj%pointPattern)**polytopeDim, &
            FATAL, 'Client requested computation of point '//str(&
            intValue)//', but only '//str(size(obj%pointPattern))//'^'//&
            str(polytopeDim)//' points exist in the pattern.', log )
    else
       call addEvent( intValue /= size(obj%pointPattern)**polytopeDim, &
            FATAL, 'Expected to compute '//str(size(obj%&
            pointPattern))//'^'//str(polytopeDim)//' sets of point &
            &coordinates, but client supplied output array of size '//&
            str(intValue)//'.', log )
    end if
    
    call endSub( log )
  end subroutine checkPoints_Grid

  
  pure function getNumPointsPerPolytope_Grid( obj )
    class(GridPointArrangementType), intent(in) :: obj
    integer :: getNumPointsPerPolytope_Grid
    
    getNumPointsPerPolytope_Grid = size(obj%pointPattern)**obj%&
         polytopeDim
  end function getNumPointsPerPolytope_Grid

  
  
end module PointArrangementsModule

