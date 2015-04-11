module CellTriangleArrayTests

  use TestUtilities
  use Global
  use FiniteVolumeModule
  use FlowFieldModule

  ! dynamic types to be tested
  use FaceCollectionsModule
  use CellCollectionsModule
  use PolytopeCollectionsModule

  implicit none

  character(*), parameter :: MOD_NAME = &
       'CellTriangleArrayTests'
  
  class(PolytopeArrayInterface), allocatable :: pointArray, &
       lineArray, triangleArray
  
  type(PolytopePointerSetType) :: expectedPPS, testPPS
  type(PolytopePointerType) :: pp1, pp2, pp3
  type(TestLogType) :: log
  
  class(InteriorStaticPointCollectionInterface), allocatable :: igpc
  class(IntAveVectorMomentFieldInterface), allocatable :: viamf
  type(IntAveScalarMomentFieldListType) :: siamfList
  

contains

  !
  !      |
  !    4 +   o--
  !      |   |  \---
  !      +   |      \---
  !      |   o          \o--
  !    2 +   |       o      \---
  !      |   |                  \---
  !    1 +   o-----------o-----------o
  !      |
  !   ---+---+---+---+---+---+---+---+--
  !      |   1       3               7
  !
  subroutine setUp
    call log%init( MOD_NAME, 'setup' )

    ! make points
    call createPointArray( pointArray, [1._FLOAT, 1._FLOAT, 1._FLOAT, &
         4._FLOAT, 7._FLOAT, 1._FLOAT], log )
    if ( log%test(FATAL) ) return
    
    pp1 = ptr( pointArray, 1 )
    pp2 = ptr( pointArray, 2 )
    pp3 = ptr( pointArray, 3 )

    ! make lines
    call createSimplexArray( lineArray, 1, [pp1, pp2, pp2, pp3, pp3, &
         pp1], pointArray, log )
    if ( log%test(FATAL) ) return
    
    
    ! make a triangle
    call createSimplexArray( triangleArray, 2, [&
         ptr(lineArray, 1), &
         ptr(lineArray, 2), &
         ptr(lineArray, 3)], lineArray, log )
    if ( log%test(FATAL) ) return
    
    ! add cell and face extensions (question to self: is the sequence
    ! of attachment calls critical?)
    call attachInteriorFaceArray( lineArray, igpc, log )
    call attachCellArray( triangleArray, igpc, log )
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( triangleArray, log )
    call destroy( lineArray, log )
    call destroy( pointArray, log )

    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testCellTriangleArrayTestSetup
    if ( log%test(MOD_NAME, 'testCellTriangleArrayTestSetup', FATAL) ) &
         return
  end subroutine testCellTriangleArrayTestSetup




end module CellTriangleArrayTests
