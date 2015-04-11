module TriangleAndSubsetTests

  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule

  implicit none

  character(*), parameter :: MOD_NAME = 'TriangleAndSubsetTests'

  class(PolytopeArrayInterface), allocatable :: pointArray, &
       lineArray, triangleArray
  type(PolytopePointerSetType) :: expectedPPS, testPPS
  type(PolytopePointerType) :: pp1, pp2, pp3
  type(TestLogType) :: log

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
    call log%init( MOD_NAME, 'setUp' )

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

    ! create a set from this polytope's boundaries
    call testPPS%deinit( log )
    call triangleArray%collectSubPolytopes( 1, testPPS, log )
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( triangleArray, log )
    call destroy( lineArray, log )
    call destroy( pointArray, log )

    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testFilterPoints
    type(PolytopePointerType), dimension(3) :: pp
    if ( log%test(MOD_NAME, 'testFilterPoints', FATAL) ) return
    
    ! filter polytopes that are not points
    call testPPS%filter( 0, log )

    ! create the expected set.  Jumble the polytopes for good measure.
    pp = [pp2, pp1, pp3]
    call expectedPPS%init( pp, log )
    if ( log%test(FATAL) ) return
    
    ! verify
    call assertTrue( testPPS%sameAs(expectedPPS), 'Filtered set &
         &should match expected set of points.' )

    call expectedPPS%deinit( log )
    call testPPS%deinit( log )
  end subroutine testFilterPoints


  subroutine testFilterLines
    type(PolytopePointerType), dimension(3) :: pp
    if ( log%test(MOD_NAME, 'testFilterLines', FATAL) ) return

    ! filter polytopes that are not lines
    call testPPS%filter( 1, log )

    ! create the expected set.  Jumble the polytopes for good measure.
    pp = [ptr(lineArray, 3), ptr(lineArray, 1), ptr(lineArray, 2)]
    call expectedPPS%init( pp, log )
    if ( log%test(FATAL) ) return

    ! verify
    call assertTrue( testPPS%sameAs(expectedPPS), 'Filtered set &
         &should match expected set of lines.' )

    call expectedPPS%deinit( log )
    call testPPS%deinit( log )
  end subroutine testFilterLines


  subroutine testPointCentroids
    type(RealVectorType) :: coords
    real(FLOAT), dimension(NDIM) :: expected
    integer :: i

    do i = 1, 3
       call pointArray%getCentroid( i, coords )
       select case ( i )
       case (1)
          expected = [1., 1.]
       case (2)
          expected = [1., 4.]
       case (3)
          expected = [7., 1.]
       end select
       call assertEqual( expected, coords%getValues(), TOL, &
            'Point ('//str(i)//') failed.' )
    end do
  end subroutine testPointCentroids
  

  subroutine testLineCentroids
    type(RealVectorType) :: coords
    real(FLOAT), dimension(NDIM) :: expected
    integer :: i
    if ( log%test(MOD_NAME, 'testLineCentroids', FATAL) ) &
         return

    do i = 1, 3
       call lineArray%getCentroid( i, coords, log )
       select case ( i )
       case (1)
          expected = [1., 2.5]
       case (2)
          expected = [4., 2.5]
       case (3)
          expected = [4., 1. ]
       end select
       call assertEqual( expected, coords%getValues(), TOL, &
            'Line ('//str(i)//') failed.' )
    end do
  end subroutine testLineCentroids

  
  subroutine testTriangleCentroid
    type(RealVectorType) :: coords
    if ( log%test(MOD_NAME, 'testTriangleCentroid', FATAL) ) &
         return

    call triangleArray%getCentroid( 1, coords, log )

    call assertEqual( [3._FLOAT, 2._FLOAT], coords%getValues(), TOL, &
         'Triangle failed.' )
  end subroutine testTriangleCentroid

  
  subroutine testTriangleArea
    real(FLOAT) :: area
    if ( log%test(MOD_NAME, 'testTriangleArea', FATAL) ) &
         return

    call triangleArray%getSize( 1, area, log )

    call assertEqual( 9._FLOAT, area, TOL, 'Triangle failed.' )
  end subroutine testTriangleArea
  
end module TriangleAndSubsetTests
