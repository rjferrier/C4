module FakePolytopeArrays_TriangleWithBoundary

  use TestUtilities
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FiniteVolume_Stubs
  use Domain_Stubs
  use DirectionsModule
  use FlowFieldModule

  implicit none
  private
  public :: interiorPointArray, boundaryPointArray, interiorLineArray, &
       boundaryLineArray, triangleArray, setup_FakePolytopeArrays, &
       teardown_FakePolytopeArrays, FakeBoundaryRegionType
  
  character(*), parameter :: MOD_NAME = &
       'FakePolytopeArrays_TriangleWithBoundary'
  
  ! These fake classes emulate the same geometry as
  ! FakePolytopeArrays_RightAngleTriangle, but this time the sloping
  ! face and its verticies are separate so that they can be treated
  ! as a boundary.
  !
  !      |    
  !    4 +   +--
  !      |   |  \---
  !      +   |      \---
  !      |   |          \---
  !    2 +   |              \---
  !      |   |                  \---
  !    1 +   +-----------------------+ 
  !      |
  !   ---+---+---+---+---+---+---+---+--
  !      |   1       3               7
  !
  ! The normal vector of the sloping face is [1, 2]/sqrt(5).
  ! We will go anticlockwise when ordering the various elements.
  
  ! --- verticies ---

  type, extends(PolytopeArrayStub), abstract :: FakePointArrayType
     type(RealVectorType), dimension(:), allocatable :: coords
   contains
     procedure :: initPointArray
     procedure :: deinitPointArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_PointArray
     procedure :: getCentroid => getCentroid_PointArray
  end type FakePointArrayType
  
  type, extends(FakePointArrayType) :: FakeInteriorPointArrayType
   contains
     procedure :: init => init_InteriorPointArray
     procedure :: deinit => deinit_InteriorPointArray
  end type FakeInteriorPointArrayType
  
  type, extends(FakePointArrayType) :: FakeBoundaryPointArrayType
   contains
     procedure :: init => init_BoundaryPointArray
     procedure :: deinit => deinit_BoundaryPointArray
  end type FakeBoundaryPointArrayType
  

  ! --- lines ---
  
  type, extends(PolytopeArrayStub) :: FakeInteriorLineArrayType
   contains
     type(FakeInteriorPointArrayType), pointer :: interiorPointArray
     type(FakeBoundaryPointArrayType), pointer :: boundaryPointArray
   contains
     procedure :: init => init_InteriorLineArray
     procedure :: deinit => deinit_InteriorLineArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_InteriorLineArray
     procedure :: getCentroid => getCentroid_InteriorLineArray
  end type FakeInteriorLineArrayType
  
  type, extends(PolytopeArrayStub) :: FakeBoundaryLineArrayType
   contains
     type(FakeBoundaryPointArrayType), pointer :: boundaryPointArray
   contains
     procedure :: init => init_BoundaryLineArray
     procedure :: deinit => deinit_BoundaryLineArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_BoundaryLineArray
     procedure :: getCentroid => getCentroid_BoundaryLineArray
  end type FakeBoundaryLineArrayType

  
  ! --- triangle ---
  
  type, extends(PolytopeArrayStub) :: FakeTriangleArrayType
   contains
     type(FakeInteriorPointArrayType), pointer :: interiorPointArray
     type(FakeBoundaryPointArrayType), pointer :: boundaryPointArray
     type(FakeInteriorLineArrayType), pointer :: interiorLineArray
     type(FakeBoundaryLineArrayType), pointer :: boundaryLineArray
   contains
     procedure :: init => init_TriangleArray
     procedure :: deinit => deinit_TriangleArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_TriangleArray
     procedure :: getCentroid => getCentroid_TriangleArray
  end type FakeTriangleArrayType

  
  ! --- boundary region ---
  
  type, extends(BoundaryRegionStub) :: FakeBoundaryRegionType
     private 
     class(DirectionInterface), allocatable :: faceNormal
   contains
     procedure :: init => init_FakeBoundaryRegion
     procedure :: deinit => deinit_FakeBoundaryRegion
     procedure :: getFaceNormals => getFaceNormals_FakeBoundaryRegion
  end type FakeBoundaryRegionType


  type(FakeInteriorPointArrayType) :: interiorPointArray
  type(FakeBoundaryPointArrayType) :: boundaryPointArray
  type(FakeInteriorLineArrayType) :: interiorLineArray
  type(FakeBoundaryLineArrayType) :: boundaryLineArray
  type(FakeTriangleArrayType) :: triangleArray


contains

  subroutine setup_FakePolytopeArrays( log )
    class(TestLogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'setup_FakePolytopeArrays', log )
    
    call interiorPointArray%init( log )
    call boundaryPointArray%init( log )
    if ( log%test(FATAL) ) return
    
    call interiorLineArray%init( interiorPointArray, &
         boundaryPointArray, log )
    call boundaryLineArray%init( boundaryPointArray, log )
    if ( log%test(FATAL) ) return
    
    call triangleArray%init( interiorPointArray, &
         boundaryPointArray, interiorLineArray, &
         boundaryLineArray, log )
    if ( log%test(FATAL) ) return

    call endSub( log )
  end subroutine setup_FakePolytopeArrays


  subroutine teardown_FakePolytopeArrays( log )
    class(TestLogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'teardown_FakePolytopeArrays', log )

    call interiorPointArray%deinit( log )
    call boundaryPointArray%deinit( log )
    call interiorLineArray%deinit( log )
    call boundaryLineArray%deinit( log )
    call triangleArray%deinit( log )
    
    call endSub( log )
  end subroutine teardown_FakePolytopeArrays

  
  !-------------------------------------------------------------------
  !- FakePointArrayType methods
  !-------------------------------------------------------------------
  
  subroutine initPointArray( obj, coords, log )
    class(FakePointArrayType), intent(inout) :: obj
    type(RealVectorType), dimension(:), intent(in) :: coords
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'initPointArray', log )
    
    allocate( obj%coords(size(coords)), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%&
         &coords(size(coords)).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%coords = coords
    
    call obj%initPolytopeArray( size(coords), 0, log )
    
    call endSub( log )
  end subroutine initPointArray

  
  subroutine deinitPointArray( obj, log )
    class(FakePointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, stat
    call beginSub( MOD_NAME, 'deinitPointArray', log )
    
    call obj%deinitPolytopeArray( log )

    deallocate( obj%coords, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating &
         &obj%coords.  STAT='//int2str(stat), log )
    
    call endSub( log )
  end subroutine deinitPointArray

  
  pure recursive subroutine collectSubPolytopes_PointArray( obj, &
       index, pps, log, depth )
    class(FakePointArrayType), target, intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    type(PolytopePointerType) :: pp
    call beginSub( MOD_NAME, 'collectSubPolytopes_PointArray', log )

    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this fake.', log )

    call pp%init( obj, index )
    call addEvent( ADVICE, 'pp = '//pp%describe(), log )
    call pps%init( [pp] )

    call endSub( log )
  end subroutine collectSubPolytopes_PointArray
  
  
  pure subroutine getCentroid_PointArray( obj, index, coords, log )
    class(FakePointArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCentroid_PointArray', log )

    coords = obj%coords( index )
    call addEvent( ADVICE, 'coords = '//str(coords%getValues()), log )
    
    call endSub( log )
  end subroutine getCentroid_PointArray
  

  
  !-------------------------------------------------------------------
  !- FakeInteriorPointArrayType methods
  !-------------------------------------------------------------------
  
  subroutine init_InteriorPointArray( obj, log )
    class(FakeInteriorPointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_InteriorPointArray', log )
    
    call obj%initPointArray( [vector([1._FLOAT, 1._FLOAT])], log )
    
    call endSub( log )
  end subroutine init_InteriorPointArray

  
  subroutine deinit_InteriorPointArray( obj, log )
    class(FakeInteriorPointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'deinit_InteriorPointArray', log )

    call obj%deinitPointArray( log )
    
    call endSub( log )
  end subroutine deinit_InteriorPointArray


  !-------------------------------------------------------------------
  !- FakeBoundaryPointArrayType methods
  !-------------------------------------------------------------------
  
  subroutine init_BoundaryPointArray( obj, log )
    class(FakeBoundaryPointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_BoundaryPointArray', log )
    
    call obj%initPointArray( [vector([7._FLOAT, 1._FLOAT]), vector([&
         1._FLOAT, 4._FLOAT])], log )
    
    call endSub( log )
  end subroutine init_BoundaryPointArray

  
  subroutine deinit_BoundaryPointArray( obj, log )
    class(FakeBoundaryPointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'deinit_BoundaryPointArray', log )

    call obj%deinitPointArray( log )
    
    call endSub( log )
  end subroutine deinit_BoundaryPointArray


  
  !-------------------------------------------------------------------
  !- FakeInteriorLineArrayType methods
  !-------------------------------------------------------------------
  
  subroutine init_InteriorLineArray( obj, interiorPointArray, &
       boundaryPointArray, log )
    class(FakeInteriorLineArrayType), intent(inout) :: obj
    type(FakeInteriorPointArrayType), target, intent(in) :: &
         interiorPointArray
    type(FakeBoundaryPointArrayType), target, intent(in) :: &
         boundaryPointArray
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'init_InteriorLineArray', log )

    obj%interiorPointArray => interiorPointArray
    obj%boundaryPointArray => boundaryPointArray
    
    call obj%initPolytopeArray( 2, 1, log )
    
    call endSub( log )
  end subroutine init_InteriorLineArray

  
  subroutine deinit_InteriorLineArray( obj, log )
    class(FakeInteriorLineArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'deinit_InteriorLineArray', log )

    call obj%deinitPolytopeArray( log )
    nullify(obj%interiorPointArray)
    nullify(obj%boundaryPointArray)
    
    call endSub( log )
  end subroutine deinit_InteriorLineArray

  
  pure recursive subroutine collectSubPolytopes_InteriorLineArray( obj, &
       index, pps, log, depth )
    class(FakeInteriorLineArrayType), target, intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    type(PolytopePointerType) :: pp1, pp2
    call beginSub( MOD_NAME, 'collectSubPolytopes_InteriorLineArray', log )

    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this fake.', log )

    call pp1%init( obj%interiorPointArray, 1 )
    call addEvent( ADVICE, 'pp1 = '//pp1%describe(), log )
    call pp2%init( obj%boundaryPointArray, index )
    call addEvent( ADVICE, 'pp2 = '//pp2%describe(), log )
    
    call pps%init( [pp1, pp2], log )

    call endSub( log )
  end subroutine collectSubPolytopes_InteriorLineArray
  
  
  pure subroutine getCentroid_InteriorLineArray( obj, index, coords, log )
    class(FakeInteriorLineArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    integer, dimension(2) :: indexPair
    call beginSub( MOD_NAME, 'getCentroid_InteriorLineArray', log )

    coords = ( obj%interiorPointArray%coords(1) + &
         obj%boundaryPointArray%coords(index) ) / 2._FLOAT
    
    call endSub( log )
  end subroutine getCentroid_InteriorLineArray


  
  !-------------------------------------------------------------------
  !- FakeBoundaryLineArrayType methods
  !-------------------------------------------------------------------
  
  subroutine init_BoundaryLineArray( obj, boundaryPointArray, log )
    class(FakeBoundaryLineArrayType), intent(inout) :: obj
    type(FakeBoundaryPointArrayType), target, intent(in) :: &
         boundaryPointArray
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'init_BoundaryLineArray', log )

    obj%boundaryPointArray => boundaryPointArray
    
    call obj%initPolytopeArray( 1, 1, log )
    
    call endSub( log )
  end subroutine init_BoundaryLineArray

  
  subroutine deinit_BoundaryLineArray( obj, log )
    class(FakeBoundaryLineArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'deinit_BoundaryLineArray', log )

    call obj%deinitPolytopeArray( log )
    nullify(obj%boundaryPointArray)
    
    call endSub( log )
  end subroutine deinit_BoundaryLineArray

  
  pure recursive subroutine collectSubPolytopes_BoundaryLineArray( obj, &
       index, pps, log, depth )
    class(FakeBoundaryLineArrayType), target, intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    type(PolytopePointerType) :: pp1, pp2
    call beginSub( MOD_NAME, 'collectSubPolytopes_BoundaryLineArray', log )

    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this fake.', log )
    call addEvent( index /= 1, WARNING, 'Input index is not &
         &applicable.  Should be 1.', log )

    call pp1%init( obj%boundaryPointArray, 1 )
    call addEvent( ADVICE, 'pp1 = '//pp1%describe(), log )
    call pp2%init( obj%boundaryPointArray, 2 )
    call addEvent( ADVICE, 'pp2 = '//pp2%describe(), log )
    
    call pps%init( [pp1, pp2], log )

    call endSub( log )
  end subroutine collectSubPolytopes_BoundaryLineArray
  
  
  pure subroutine getCentroid_BoundaryLineArray( obj, index, coords, log )
    class(FakeBoundaryLineArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    integer, dimension(2) :: indexPair
    call beginSub( MOD_NAME, 'getCentroid_BoundaryLineArray', log )

    call addEvent( index /= 1, WARNING, 'Input index is not &
         &applicable.  Should be 1.', log )
    
    coords = ( obj%boundaryPointArray%coords(1)%getValues() + obj%&
         boundaryPointArray%coords(2)%getValues() ) / 2._FLOAT
    
    call endSub( log )
  end subroutine getCentroid_BoundaryLineArray



  !-------------------------------------------------------------------
  !- FakeTriangleArrayType methods
  !-------------------------------------------------------------------
  
  subroutine init_TriangleArray( obj, interiorPointArray, &
       boundaryPointArray, interiorLineArray, boundaryLineArray, log )
    class(FakeTriangleArrayType), intent(inout) :: obj
    type(FakeInteriorPointArrayType), target, intent(in) :: &
         interiorPointArray
    type(FakeBoundaryPointArrayType), target, intent(in) :: &
         boundaryPointArray
    type(FakeInteriorLineArrayType), target, intent(in) :: &
         interiorLineArray
    type(FakeBoundaryLineArrayType), target, intent(in) :: &
         boundaryLineArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_TriangleArray', log )

    obj%interiorPointArray => interiorPointArray
    obj%boundaryPointArray => boundaryPointArray
    obj%interiorLineArray => interiorLineArray
    obj%boundaryLineArray => boundaryLineArray
    
    call obj%initPolytopeArray( 1, 2, log )
    
    call endSub( log )
  end subroutine init_TriangleArray

  
  subroutine deinit_TriangleArray( obj, log )
    class(FakeTriangleArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_TriangleArray', log )

    call obj%deinitPolytopeArray( log )
    nullify(obj%interiorPointArray)
    nullify(obj%boundaryPointArray)
    nullify(obj%interiorLineArray)
    nullify(obj%boundaryLineArray)
    
    call endSub( log )
  end subroutine deinit_TriangleArray

  
  pure recursive subroutine collectSubPolytopes_TriangleArray( obj, &
       index, pps, log, depth )
    class(FakeTriangleArrayType), target, intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    integer, dimension(2) :: indexPair
    type(PolytopePointerType), dimension(6) :: pp 
    call beginSub( MOD_NAME, 'collectSubPolytopes_TriangleArray', &
         log )
    
    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this fake.  All subs will be returned.', log )
    
    call pp(1)%init( obj%interiorPointArray, 1 )
    call pp(2)%init( obj%boundaryPointArray, 1 )
    call pp(3)%init( obj%boundaryPointArray, 2 )
    
    call pp(4)%init( obj%interiorLineArray, 1 )
    call pp(5)%init( obj%interiorLineArray, 2 )
    call pp(6)%init( obj%boundaryLineArray, 1 )
    
    call pps%init( pp, log )
    
    call endSub( log )
  end subroutine collectSubPolytopes_TriangleArray
  
  
  pure subroutine getCentroid_TriangleArray( obj, index, coords, log )
    class(FakeTriangleArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerSetType) :: pps
    call beginSub( MOD_NAME, 'getCentroid_TriangleArray', log )

    coords = ( obj%interiorPointArray%coords(1) + obj%&
         boundaryPointArray%coords(1) + obj%boundaryPointArray%&
         coords(2) ) / 3._FLOAT
    
    call endSub( log )
  end subroutine getCentroid_TriangleArray

  
  !-------------------------------------------------------------------
  !- FakeBoundaryRegion methods
  !-------------------------------------------------------------------
  
  subroutine init_FakeBoundaryRegion( obj, name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, log )
    class(FakeBoundaryRegionType), intent(inout) :: obj
    character(*), intent(in) :: name
    class(PointVectorBoundaryConditionInterface), allocatable, &
         intent(inout) :: pointVelocityBoundaryCondition
    type(PointScalarBoundaryConditionListType), intent(inout) :: &
         pointScalarBoundaryConditionList
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_FakeBoundaryRegion', log )

    ! create custom components - in this case, the face normal 
    call createObliqueDirection( obj%faceNormal, &
         unitVector([1._FLOAT, 2._FLOAT]), log )

    ! initialise superclass components
    call obj%initBoundaryRegion( name, &
       pointVelocityBoundaryCondition, &
       pointScalarBoundaryConditionList, log )
    
    call endSub( log )
  end subroutine init_FakeBoundaryRegion

    
  subroutine deinit_FakeBoundaryRegion( obj, log )
    class(FakeBoundaryRegionType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_FakeBoundaryRegion', log )

    call obj%deinitBoundaryRegion( log )

    call destroy( obj%faceNormal, log )

    call endSub( log )
  end subroutine deinit_FakeBoundaryRegion


  subroutine getFaceNormals_FakeBoundaryRegion( obj, directions, &
       boundaryStaticPointGroupPointer, log )
    class(FakeBoundaryRegionType), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(&
         out) :: directions
    class(BoundaryStaticPointGroupPointerType), intent(in) :: &
         boundaryStaticPointGroupPointer
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormals_FakeBoundaryRegion', &
         log )

    call obj%faceNormal%clone( directions, &
         boundaryStaticPointGroupPointer%getNumPoints(), log )
    
    call endSub( log )
  end subroutine getFaceNormals_FakeBoundaryRegion


end module FakePolytopeArrays_TriangleWithBoundary
