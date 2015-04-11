module FakePolytopeArrays_QuadWithBoundary

  use TestUtilities
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use DomainModule
  use FiniteVolume_Stubs
  use Domain_Stubs
  use DirectionsModule
  use FlowFieldModule

  implicit none

  character(*), parameter, private :: MOD_NAME = &
       'FakePolytopeArrays_QuadWithBoundary'
  

  ! These mock classes emulate the following geometry:
  !
  !      |
  !    5 +   d---E---c
  !      |   |      / \   
  !      +   | II  /   \   <  This side is a boundary.
  !      |   |    /     \     Face normal = [2, 1]/sqrt(5)
  !    3 +   C   B       D
  !      |   |  /    I    \
  !      +   | /           \
  !      |   |/             \
  !    1 +   a-------A-------b
  !      |
  !   ---+---+---+---+---+---+---+-
  !      |   1       3       5    
  !
  type, extends(PolytopeArrayStub) :: FakePointArrayType
     type(RealVectorType), dimension(4) :: coords
   contains
     procedure :: init => init_PointArray
     procedure :: deinit => deinit_PointArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_PointArray
     procedure :: getCentroid => getCentroid_PointArray
     procedure :: isAssociatedWithRegion => &
          isAssociatedWithRegion_PointArray
     
  end type FakePointArrayType
  
  
  type, extends(PolytopeArrayStub) :: FakeLineArrayType
     type(FakePointArrayType), pointer :: pointArray
     integer, dimension(2, 5) :: vertexIndicies
     class(FaceArrayInterface), allocatable :: faceExtension
   contains
     procedure :: init => init_LineArray
     procedure :: deinit => deinit_LineArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_LineArray
     procedure :: getCentroid => getCentroid_LineArray
     procedure :: isAssociatedWithRegion => &
          isAssociatedWithRegion_LineArray
     procedure :: attachToArray_Faces => &
          attachToArray_Faces_LineArray
     
  end type FakeLineArrayType

  
!!$  type, extends(PolytopeArrayStub) :: FakeInteriorLineArrayType
!!$     type(PolytopePointerType), dimension(2, 4) :: points
!!$   contains
!!$     procedure :: init => init_ILineArray
!!$     procedure :: deinit => deinit_ILineArray
!!$     procedure :: collectSubPolytopes => &
!!$          collectSubPolytopes_ILineArray
!!$     procedure :: getCentroid => getCentroid_ILineArray
!!$  end type FakeLineArrayType
!!$
!!$  
!!$  type, extends(PolytopeArrayStub) :: FakeBoundaryLineArrayType
!!$     type(PolytopePointerType), dimension(2, 1) :: points
!!$   contains
!!$     procedure :: init => init_BLineArray
!!$     procedure :: deinit => deinit_BLineArray
!!$     procedure :: collectSubPolytopes => &
!!$          collectSubPolytopes_BLineArray
!!$     procedure :: getCentroid => getCentroid_BLineArray
!!$  end type FakeLineArrayType

  
  type, extends(PolytopeArrayStub) :: FakeTriangleArrayType
     type(FakePointArrayType), pointer :: pointArray
     integer, dimension(3, 2) :: vertexIndicies
   contains
     procedure :: init => init_TriangleArray
     procedure :: deinit => deinit_TriangleArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_TriangleArray
     procedure :: getCentroid => getCentroid_TriangleArray
  end type FakeTriangleArrayType

  
  type, extends(FaceArrayStub) :: FakeFaceArrayType
   contains
     procedure :: init => init_FaceArray
     procedure :: deinit => deinit_FaceArray
     procedure :: getFaceNormal => getFaceNormal_FaceArray
     procedure :: findSide => findSide_FaceArray
  end type FakeFaceArrayType

  
  type, extends(BoundaryRegionStub) :: FakeBoundaryRegionType
     class(DirectionInterface), allocatable :: faceNormal
   contains
     procedure :: init => init_FakeBoundaryRegion
     procedure :: deinit => deinit_FakeBoundaryRegion
     procedure :: getFaceNormals => getFaceNormals_FakeBoundaryRegion
  end type FakeBoundaryRegionType
  
  type(FakePointArrayType) :: pointArray
  type(FakeLineArrayType) :: lineArray
  type(FakeTriangleArrayType) :: triangleArray
  
  type(InteriorRegionStub) :: interiorRegion
  type(FakeBoundaryRegionType) :: boundaryRegion

contains

  subroutine setup_FakePolytopeArrays( log )
    class(TestLogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'setup_FakePolytopeArrays', log )

    call pointArray%init( log )
    if ( log%test(FATAL) ) return

    call lineArray%init( pointArray, log )
    if ( log%test(FATAL) ) return

    call triangleArray%init( pointArray, log )
    if ( log%test(FATAL) ) return

    ! the boundary region is not initialised here because it requires
    ! parameters that are yet to be calculated.  The same goes for the
    ! interior region.

    call endSub( log )
  end subroutine setup_FakePolytopeArrays


  subroutine teardown_FakePolytopeArrays( log )
    class(TestLogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'teardown_FakePolytopeArrays', log )

    call triangleArray%deinit( log )
    call lineArray%deinit( log )
    call pointArray%deinit( log )

    call endSub( log )
  end subroutine teardown_FakePolytopeArrays


  !-------------------------------------------------------------------
  !- FakePointArrayType methods
  !-------------------------------------------------------------------

  subroutine init_PointArray( obj, log )
    class(FakePointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_PointArray', log )

    call obj%initPolytopeArray( 4, 0, log )
    obj%coords(1) = [1._FLOAT, 1._FLOAT]
    obj%coords(2) = [5._FLOAT, 1._FLOAT]
    obj%coords(3) = [3._FLOAT, 5._FLOAT]
    obj%coords(4) = [1._FLOAT, 5._FLOAT]

    call endSub( log )
  end subroutine init_PointArray


  subroutine deinit_PointArray( obj, log )
    class(FakePointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'deinit_PointArray', log )

    obj%coords = 0._FLOAT
    call obj%deinitPolytopeArray( log )

    call endSub( log )
  end subroutine deinit_PointArray


  pure recursive subroutine collectSubPolytopes_PointArray( obj, &
       index, pps, log, depth )
    class(FakePointArrayType), target, intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    type(PolytopePointerType), dimension(1) :: ppa
    call beginSub( MOD_NAME, 'collectSubPolytopes_PointArray', log )

    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this mock.', log )

    call ppa(1)%init( obj, index )
    call addEvent( ADVICE, 'ppa(1) = '//ppa(1)%describe(), log )
    call pps%init( ppa )

    call endSub( log )
  end subroutine collectSubPolytopes_PointArray


  pure subroutine getCentroid_PointArray( obj, index, coords, log )
    class(FakePointArrayType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getCentroid_PointArray', log )

    coords = obj%coords( index )
    call addEvent( ADVICE, 'coords = '//str(coords%getValues()), log )

    call endSub( log )
  end subroutine getCentroid_PointArray
  

  pure function isAssociatedWithRegion_PointArray( obj, region )
    class(FakePointArrayType), intent(in) :: obj
    class(RegionInterface), intent(in) :: region
    logical :: isAssociatedWithRegion_PointArray
    
    isAssociatedWithRegion_PointArray = .false.
    
  end function isAssociatedWithRegion_PointArray

  
  !-------------------------------------------------------------------
  !- FakeLineArrayType methods
  !-------------------------------------------------------------------

  subroutine init_LineArray( obj, pointArray, log )
    class(FakeLineArrayType), intent(inout) :: obj
    type(FakePointArrayType), target, intent(in) :: pointArray
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'init_LineArray', log )

    call obj%initPolytopeArray( 5, 1, log )
    obj%pointArray => pointArray
    obj%vertexIndicies = reshape( [1, 2, 1, 3, 1, 4, 2, 3, 3, 4], &
         [2, 5])
    
    call endSub( log )
  end subroutine init_LineArray


  subroutine deinit_LineArray( obj, log )
    class(FakeLineArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'deinit_LineArray', log )

    nullify(obj%pointArray)
    obj%vertexIndicies = 0
    call obj%deinitPolytopeArray( log )

    call endSub( log )
  end subroutine deinit_LineArray


  pure recursive subroutine collectSubPolytopes_LineArray( obj, &
       index, pps, log, depth )
    class(FakeLineArrayType), target, intent(inout) :: obj
    integer, intent(in) :: index
    class(PolytopePointerSetType), intent(inout) :: pps
    class(LogType), intent(inout), optional :: log
    integer, intent(in), optional :: depth
    integer, dimension(2) :: indexPair
    type(PolytopePointerType), dimension(2) :: ppa
    call beginSub( MOD_NAME, 'collectSubPolytopes_LineArray', log )

    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this mock.', log )

    indexPair = mod([index-1, index], 3) + 1

    call ppa(1)%init( obj%pointArray, indexPair(1) )
    call addEvent( ADVICE, 'ppa(1) = '//ppa(1)%describe(), log )
    call ppa(2)%init( obj%pointArray, indexPair(2) )
    call addEvent( ADVICE, 'ppa(2) = '//ppa(2)%describe(), log )
    call pps%init( ppa, log )

    call endSub( log )
  end subroutine collectSubPolytopes_LineArray


  pure subroutine getCentroid_LineArray( obj, index, coords, log )
    class(FakeLineArrayType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    integer, dimension(2) :: indexPair
    call beginSub( MOD_NAME, 'getCentroid_LineArray', log )

    indexPair = mod([index-1, index], 3) + 1

    coords = ( obj%pointArray%coords(indexPair(1)) + &
         obj%pointArray%coords(indexPair(2)) ) / 2._FLOAT

    call endSub( log )
  end subroutine getCentroid_LineArray

  
  pure function isAssociatedWithRegion_LineArray( obj, region )
    class(FakeLineArrayType), intent(in) :: obj
    class(RegionInterface), intent(in) :: region
    logical :: isAssociatedWithRegion_LineArray
    
    isAssociatedWithRegion_LineArray = .false.
    
  end function isAssociatedWithRegion_LineArray

  
  subroutine attachToArray_Faces_LineArray( obj, ptr, log )
    class(FakeLineArrayType), intent(inout), target :: obj
    class(FaceArrayInterface), pointer, intent(inout) :: ptr
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'attachToArray_Faces_SimplexArray', &
         log )
    
    if ( .not. allocated(obj%faceExtension) ) then
       allocate( FakeFaceArrayType :: obj%faceExtension, stat=&
            allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &FakeFaceArrayType :: obj%faceExtension.  STAT='//&
            int2str(allocStat), log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
    end if
    
    ptr => obj%faceExtension
    call endSub( log )
  end subroutine attachToArray_Faces_LineArray




  !-------------------------------------------------------------------
  !- FakeTriangleArrayType methods
  !-------------------------------------------------------------------

  subroutine init_TriangleArray( obj, pointArray, log )
    class(FakeTriangleArrayType), intent(inout) :: obj
    type(FakePointArrayType), target, intent(in) :: pointArray
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_TriangleArray', log )

    call obj%initPolytopeArray( 2, 1, log )
    obj%pointArray => pointArray
    obj%vertexIndicies = reshape( [1, 2, 3, 1, 3, 4], [3, 2] )
    
    call endSub( log )
  end subroutine init_TriangleArray


  subroutine deinit_TriangleArray( obj, log )
    class(FakeTriangleArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_TriangleArray', log )

    nullify(obj%pointArray)
    obj%vertexIndicies = 0
    call obj%deinitPolytopeArray( log )

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
    type(PolytopePointerType), dimension(3) :: pp
    call beginSub( MOD_NAME, 'collectSubPolytopes_TriangleArray', &
         log )

    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this mock.', log )

    call pp(1)%init( obj%pointArray, 1 )
    call pp(2)%init( obj%pointArray, 2 )
    call pp(3)%init( obj%pointArray, 3 )
    call pps%init( pp, log )

    call endSub( log )
  end subroutine collectSubPolytopes_TriangleArray


  pure subroutine getCentroid_TriangleArray( obj, index, coords, log )
    class(FakeTriangleArrayType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out)  :: coords
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerSetType) :: pps
    call beginSub( MOD_NAME, 'getCentroid_TriangleArray', log )

    coords = ( obj%pointArray%coords(1) + obj%pointArray%coords(&
         2) + obj%pointArray%coords(3) ) / 3._FLOAT

    call endSub( log )
  end subroutine getCentroid_TriangleArray

  
  pure function isAssociatedWithRegion_TriangleArray( obj, region )
    class(FakeTriangleArrayType), intent(in) :: obj
    class(RegionInterface), intent(in) :: region
    logical :: isAssociatedWithRegion_TriangleArray
    
    isAssociatedWithRegion_TriangleArray = .false.
    
  end function isAssociatedWithRegion_TriangleArray

  
  !-------------------------------------------------------------------
  !- FakeFaceArray methods
  !-------------------------------------------------------------------
  
  subroutine init_FaceArray( obj, pa, log )
    class(FakeFaceArrayType), intent(out) :: obj
    class(PolytopeArrayInterface), intent(in), target :: pa
    class(LogType), intent(inout), optional :: log
    integer :: i, allocStat
    call beginSub( MOD_NAME, 'init_FaceArray', log )

    ! init extension superclass
    call obj%initExtension( pa, log )

    call endSub( log )
  end subroutine init_FaceArray
  

  subroutine deinit_FaceArray( obj, log )
    class(FakeFaceArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_FaceArray', log )

    ! deinit extension superclass
    call obj%deinitExtension( log )
    
    call endSub( log )
  end subroutine deinit_FaceArray

  
  pure subroutine getFaceNormal_FaceArray( obj, index, vector, log )
    class(FakeFaceArrayType), intent(in) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(out) :: vector
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormal_FaceArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    vector = 0._FLOAT
    
    call endSub( log )
  end subroutine getFaceNormal_FaceArray

  
  elemental subroutine findSide_FaceArray( obj, index, coords, side, &
       log )
    class(FakeFaceArrayType), intent(inout) :: obj
    integer, intent(in) :: index
    type(RealVectorType), intent(in) :: coords
    logical, intent(out) :: side
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'findSide_FaceArray', log )
    call addEvent( WARNING, 'This stub method should be overridden.' )

    side = .false.

    call endSub( log )
  end subroutine findSide_FaceArray


  
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
         unitVector([2._FLOAT, 1._FLOAT]), log )

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


  pure subroutine getFaceNormals_FakeBoundaryRegion( obj, directions, &
       boundaryStaticPointCollection, polytopeIndex, log )
    class(FakeBoundaryRegionType), intent(in) :: obj
    class(DirectionInterface), dimension(:), allocatable, intent(&
         out) :: directions
    class(BoundaryStaticPointCollectionInterface), intent(in) :: &
         boundaryStaticPointCollection
    integer, intent(in) :: polytopeIndex
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'getFaceNormals_FakeBoundaryRegion', &
         log )

    call obj%faceNormal%clone( directions, &
         boundaryStaticPointCollection%getNumPointsPerPolytope(), log )
    
    call endSub( log )
  end subroutine getFaceNormals_FakeBoundaryRegion

!!$  subroutine getFaceNormals_FakeBoundaryRegion( obj, directions, &
!!$       boundaryStaticPointGroupPointer, log )
!!$    class(FakeBoundaryRegionType), intent(in) :: obj
!!$    class(DirectionInterface), dimension(:), allocatable, intent(&
!!$         out) :: directions
!!$    class(BoundaryStaticPointGroupPointerType), intent(in) :: &
!!$         boundaryStaticPointGroupPointer
!!$    class(LogType), intent(inout), optional :: log
!!$    call beginSub( MOD_NAME, 'getFaceNormals_FakeBoundaryRegion', &
!!$         log )
!!$
!!$    call obj%faceNormal%clone( directions, &
!!$         boundaryStaticPointGroupPointer%getNumPoints(), log )
!!$    
!!$    call endSub( log )
!!$  end subroutine getFaceNormals_FakeBoundaryRegion


end module FakePolytopeArrays_QuadWithBoundary
