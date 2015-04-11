module FaceLineArrayTests

  use TestUtilities
  use Global
  use FiniteVolumeModule
  use ieee_arithmetic
  use FiniteVolume_Stubs
  use FlowField_Stubs
  use FlowFieldModule

  ! dynamic types to be tested
  use PolytopeCollectionsModule
  use FaceCollectionsModule


  implicit none

  !-------------------------------------------------------------------
  !- test variables
  !-------------------------------------------------------------------

  class(PolytopeArrayInterface), allocatable :: pointArray, lineArray
  class(FaceArrayInterface), pointer :: faceExtension
  
  type(PolytopePointerType) :: pp1, pp2, pp3, pp4, pp5, pp6
  type(TestLogType) :: log
  integer, parameter :: N_LINES = 5
  
  character(*), parameter, private :: MOD_NAME = &
       'FaceLineArrayCreation'

  character(33), dimension(5), parameter :: DESCRIPTIONS = [ &
       'diagonal; normal points southeast', &
       'horizontal; normal points south  ', &
       'diagonal; normal points southwest', &
       'diagonal; normal points northwest', &
       'vertical; normal points east     ' ]

  ! dummy component
  class(InteriorStaticPointCollectionInterface), allocatable :: ispc
  
  
contains


  !-------------------------------------------------------------------
  !- tests
  !-------------------------------------------------------------------

  ! create a bunch of lines to be made into faces
  !
  !       |                                 | Expect the unit normal
  !     4 F          -B                     | vectors to point to the
  !       |        -/                       | right hand side as we
  !       +      -/                         | travel from the first
  !       |    -/                           | vertex to the second.
  !     2 +   A---------------C             |
  !       |-/  \                            | In 3D, the right hand 
  !       E     \-                          | will probably hold as
  !       |       \-                        | we travel round the 
  !       +---+---+-\-+---+---+--           | vertices in the order 
  !       |   1      \-   4   5             | that the edges and
  !       +            \-                   | verticies on those 
  !       |              \                  | edges were specified.
  !    -2 +               D                 
  !
  ! Previously went by another convention, but it probably needs more
  ! logic and is also at the mercy of machine zero error, so it was
  ! scrapped.  The convention went like this: Line A-B has an
  ! ambiguous unit vector; we want to point our vectors such that
  ! dot(n, [1. 1.]) > 0 but here is a marginal case where the vector
  ! could point either up-left or down-right.  We introduce the
  ! further (arbitrary) convention that the first component should be
  ! positive if possible.  i.e. n_AB should point down-right.
  !
  subroutine setup
    call log%init( MOD_NAME, 'setup' )

    ! make some points
    call createPointArray( pointArray, [ &
         1._FLOAT,  2._FLOAT, &
         3._FLOAT,  4._FLOAT, &
         5._FLOAT,  2._FLOAT, &
         4._FLOAT, -2._FLOAT, &
         0._FLOAT,  1._FLOAT, &
         0._FLOAT,  4._FLOAT ], log )
    if ( log%test(FATAL) ) return

    pp1 = ptr( pointArray, 1 )
    pp2 = ptr( pointArray, 2 )
    pp3 = ptr( pointArray, 3 )
    pp4 = ptr( pointArray, 4 )
    pp5 = ptr( pointArray, 5 )
    pp6 = ptr( pointArray, 6 )

    ! join these to make lines
    call createSimplexArray( lineArray, 1, [ pp1, pp2, pp1, pp3, pp1, &
         pp4, pp1, pp5, pp5, pp6 ], pointArray, log )
    if ( log%test(FATAL) ) return

    ! create a fake PointCollection
    call createInteriorStaticPointCollectionStub( ispc, log )

    ! make and get face extension
    call attachInteriorFaceArray( lineArray, ispc, log )
    call lineArray%getArrayExtension( faceExtension, log )
    if ( log%test(FATAL) ) return
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( lineArray, log )
    call pp1%deinit()
    call pp2%deinit()
    call pp3%deinit()
    call pp4%deinit()
    call pp5%deinit()
    call destroy( pointArray, log )

    call log%report()
    call log%deinit()
  end subroutine tearDown

  
  subroutine testGetFaceNormal_disallowNaNs
    type(RealVectorType) :: vector
    integer :: i
    if ( log%test(MOD_NAME, 'testGetFaceNormal_disallowNaNs', FATAL) ) &
         return
    
    do i = 1, N_LINES
       call faceExtension%getFaceNormal(i, vector)
       call assertFalse( vector%anyNaN(), 'Face ('//str(i)//') &
            &failed.' )
    end do
  end subroutine testGetFaceNormal_disallowNaNs


  subroutine testGetFaceNormal
    real(FLOAT), dimension(NDIM) :: expected
    type(RealVectorType) :: vector
    integer :: i
    if ( log%test(MOD_NAME, 'testGetFaceNormal', FATAL) ) return
    
    do i = 1, N_LINES
       call faceExtension%getFaceNormal(i, vector)

       ! note how lines A-B and A-E are parallel, but we expect them
       ! to have opposing vectors because of the ordering of their
       ! verticies. 
       select case ( i )
       case (1)
          expected = [sqrt(0.5_FLOAT), -sqrt(0.5_FLOAT)] 
       case (2)
          expected = [0._FLOAT, -1._FLOAT]
       case (3)
          expected = [-0.8_FLOAT, -0.6_FLOAT] 
       case (4)
          expected = [-sqrt(0.5_FLOAT), sqrt(0.5_FLOAT)] 
       case (5)
          ! this one is also salient because there may be problems
          ! with faces that pass through the origin
          expected = [1._FLOAT, 0._FLOAT]
       end select

       call assertEqual( expected, vector%getValues(), TOL, 'Face '//&
            str(i)//' ('//trim(DESCRIPTIONS(i))//' failed.' )
    end do
  end subroutine testGetFaceNormal


  subroutine testFindSide
    type(RealVectorType) :: coords
    logical :: side
    integer :: i
    character(80) :: description
    logical, dimension(5), parameter :: EXPECTED = &
         [.true., .false., .false., .false., .true.]
    if ( log%test(MOD_NAME, 'testFindSide', FATAL) ) return
    
    coords = [3._FLOAT, 3._FLOAT]
    do i = 1, 5
       call faceExtension%findSide( i, coords, side )
       call assertTrue( side == EXPECTED(i), 'Failed w.r.t. face '//&
            int2str(i)//' ('//trim(DESCRIPTIONS(i))//').' )
    end do
  end subroutine testFindSide

  
  subroutine testFindSide_Marginal
    type(RealVectorType) :: coords
    logical :: side
    real(FLOAT), parameter :: MARGIN = 1.E-7
    if ( log%test(MOD_NAME, 'testFindSide', FATAL) ) return
    
    coords = [2._FLOAT + MARGIN, 3._FLOAT]
    call faceExtension%findSide(1, coords, side)
    call assertTrue( side, 'Failed marginally positive test.' )
    
    coords = [2._FLOAT - MARGIN, 3._FLOAT]
    call faceExtension%findSide(1, coords, side)
    call assertFalse( side, 'Failed marginally negative test.' )
  end subroutine testFindSide_Marginal

  
end module FaceLineArrayTests
