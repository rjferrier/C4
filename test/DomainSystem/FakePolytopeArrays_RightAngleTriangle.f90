module FakePolytopeArrays_RightAngleTriangle

  use TestUtilities
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FiniteVolume_Stubs
  
  implicit none
  private
  public :: pointArray, lineArray, triangleArray, &
       setup_FakePolytopeArrays, teardown_FakePolytopeArrays
  
  character(*), parameter, private :: MOD_NAME = &
       'FakePolytopeArrays_RightAngleTriangle'
  
  ! These fake classes emulate the following geometry:
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
  !

  type, extends(PolytopeArrayStub) :: FakePointArrayType
     type(RealVectorType), dimension(3) :: coords
   contains
     procedure :: init => init_PointArray
     procedure :: deinit => deinit_PointArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_PointArray
     procedure :: getCentroid => getCentroid_PointArray
  end type FakePointArrayType
  
  type, extends(PolytopeArrayStub) :: FakeLineArrayType
     type(FakePointArrayType), pointer :: pointArray
   contains
     procedure :: init => init_LineArray
     procedure :: deinit => deinit_LineArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_LineArray
     procedure :: getCentroid => getCentroid_LineArray
  end type FakeLineArrayType
  
  type, extends(PolytopeArrayStub) :: FakeTriangleArrayType
     type(FakePointArrayType), pointer :: pointArray
   contains
     procedure :: init => init_TriangleArray
     procedure :: deinit => deinit_TriangleArray
     procedure :: collectSubPolytopes => &
          collectSubPolytopes_TriangleArray
     procedure :: getCentroid => getCentroid_TriangleArray
  end type FakeTriangleArrayType

  
  type(FakePointArrayType), target :: pointArray
  type(FakeLineArrayType), target :: lineArray
  type(FakeTriangleArrayType), target :: triangleArray
  
contains

  subroutine setup_FakePolytopeArrays( log )
    class(TestLogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'setup_FakePolytopeArrays', log )

    call pointArray%init( log )
    if ( log%test(FATAL) ) return
    
    call lineArray%init( log )
    if ( log%test(FATAL) ) return
    
    call triangleArray%init( log )
    if ( log%test(FATAL) ) return

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
    
    call obj%initPolytopeArray( 3, 0, log )
    obj%coords(1) = [1._FLOAT, 1._FLOAT]
    obj%coords(2) = [7._FLOAT, 1._FLOAT]
    obj%coords(3) = [1._FLOAT, 4._FLOAT]
    
    call endSub( log )
  end subroutine init_PointArray

  
  subroutine deinit_PointArray( obj, log )
    class(FakePointArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'deinit_PointArray', log )

    do i = 1, 3
       obj%coords(i) = 0._FLOAT
    end do
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
    type(PolytopePointerType) :: pp
    call beginSub( MOD_NAME, 'collectSubPolytopes_PointArray', log )

    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this fake.', log )

    ! nothing to add !
!!$    call pp%init( obj, index )
!!$    call addEvent( ADVICE, 'pp = '//pp%describe(), log )
!!$    call pps%init( [pp] )

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


  
  !-------------------------------------------------------------------
  !- FakeLineArrayType methods
  !-------------------------------------------------------------------
  
  subroutine init_LineArray( obj, log )
    class(FakeLineArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'init_LineArray', log )
    
    call obj%initPolytopeArray( 3, 1, log )
    obj%pointArray => pointArray
    
    call endSub( log )
  end subroutine init_LineArray

  
  subroutine deinit_LineArray( obj, log )
    class(FakeLineArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'deinit_LineArray', log )

    nullify(obj%pointArray)
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
    type(PolytopePointerType), dimension(2) :: pp
    call beginSub( MOD_NAME, 'collectSubPolytopes_LineArray', log )

    call addEvent( present(depth), WARNING, 'depth arg is not &
         &applicable in this fake.', log )

    indexPair = mod([index-1, index], 3) + 1

    call pp(1)%init( obj%pointArray, indexPair(1) )
    call addEvent( ADVICE, 'pp1 = '//pp(1)%describe(), log )
    call pp(2)%init( obj%pointArray, indexPair(2) )
    call addEvent( ADVICE, 'pp2 = '//pp(2)%describe(), log )
    call pps%init( pp, log )

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



  !-------------------------------------------------------------------
  !- FakeTriangleArrayType methods
  !-------------------------------------------------------------------
  
  subroutine init_TriangleArray( obj, log )
    class(FakeTriangleArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_TriangleArray', log )
    
    call obj%initPolytopeArray( 1, 2, log )
    obj%pointArray => pointArray
    
    call endSub( log )
  end subroutine init_TriangleArray

  
  subroutine deinit_TriangleArray( obj, log )
    class(FakeTriangleArrayType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_TriangleArray', log )

    nullify(obj%pointArray)
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
         &applicable in this fake.  Only points will be returned.', &
         log )
    
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
  

end module FakePolytopeArrays_RightAngleTriangle
