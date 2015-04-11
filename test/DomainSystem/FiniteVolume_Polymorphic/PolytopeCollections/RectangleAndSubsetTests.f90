module RectangleAndSubsetTests

  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule

  implicit none

  character(*), parameter :: MOD_NAME = 'RectangleAndSubsetTests'

  class(PolytopeGridInterface), allocatable :: pointGrid, &
       xLineGrid, yLineGrid, rectangleGrid
  type(TestLogType) :: log

contains

  !
  !      |         
  !    2 +   o-------o
  !      |   |       |   
  !    1 +   o-------o
  !      |
  !   ---+---+---+---+---
  !      |   1       3   
  !
  subroutine setUp
    type(PolytopeGridPointerType) :: nullGridPtr
    call log%init( MOD_NAME, 'setUp' )

    ! make points
    call createPointGrid( pointGrid, vector([2, 2]), &
         vector([1.0_FLOAT, 1.0_FLOAT]), &
         vector([3.0_FLOAT, 2.0_FLOAT]), log )
    if ( log%test(FATAL) ) return
    
    ! make lines
    call createOrthotopeGrid( xLineGrid, [ptr(pointGrid), &
         nullGridPtr], log )
    if ( log%test(FATAL) ) return
    
    call createOrthotopeGrid( yLineGrid, [nullGridPtr, ptr(&
         pointGrid)], log )
    if ( log%test(FATAL) ) return

    ! make a rectangle.  Notice that the order of line grids in the
    ! constituents argument is reversed, because want to 'extrude'
    ! xLines in the y-direction and yLines in the x-direction.
    call createOrthotopeGrid( rectangleGrid, [ptr(yLineGrid), ptr(&
         xLineGrid)], log )
    if ( log%test(FATAL) ) return
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'tearDown' )

    call destroy( rectangleGrid, log )
    call destroy( yLineGrid, log )
    call destroy( xLineGrid, log )
    call destroy( pointGrid, log )

    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testGetExtents_SingleRectangle
    type(GridParametersType) :: gp
    if ( log%test(MOD_NAME, 'testGetExtents_SingleRectangle', &
         FATAL) ) return
    
    gp = rectangleGrid%getGridParameters()
    call assertEqual( 1._FLOAT, gp%extents(1)%getValue(1), &
         TOL, 'Failed to return correct xMin.' )
    call assertEqual( 1._FLOAT, gp%extents(1)%getValue(2), &
         TOL, 'Failed to return correct yMin.' )
    call assertEqual( 3._FLOAT, gp%extents(2)%getValue(1), &
         TOL, 'Failed to return correct xMax.' )
    call assertEqual( 2._FLOAT, gp%extents(2)%getValue(2), &
         TOL, 'Failed to return correct yMax.' )
  end subroutine testGetExtents_SingleRectangle

  
  subroutine testGetSpacing_SingleRectangle
    type(GridParametersType) :: gp
    if ( log%test(MOD_NAME, 'testGetSpacing_SingleRectangle', &
         FATAL) ) return
    
    gp = rectangleGrid%getGridParameters()
    call assertEqual( 2._FLOAT, gp%spacing%getValue(1), &
         TOL, 'Failed to return correct dx.' )
    call assertEqual( 1._FLOAT, gp%spacing%getValue(2), &
         TOL, 'Failed to return correct dy.' )
  end subroutine testGetSpacing_SingleRectangle


  subroutine testCentroid_SingleRectangle
    type(RealVectorType) :: r
    if ( log%test(MOD_NAME, 'testCentroid_SingleRectangle', &
         FATAL) ) return
        
    call rectangleGrid%getCentroid( 1, r, log )
    if ( log%test(FATAL) ) return
    
    call assertEqual( [2._FLOAT, 1.5_FLOAT], r%getValues(), 'Wrong &
         &centroid coordinates.' )
  end subroutine testCentroid_SingleRectangle


  subroutine testSize_SingleRectangle
    real(FLOAT) :: a
    if ( log%test(MOD_NAME, 'testSize_SingleRectangle', &
         FATAL) ) return
        
    call rectangleGrid%getSize( 1, a, log )
    if ( log%test(FATAL) ) return
    
    call assertEqual( 2._FLOAT, a, 'Wrong cell size.' )
  end subroutine testSize_SingleRectangle


  subroutine testCentroid_XLines
    type(RealVectorType) :: r
    if ( log%test(MOD_NAME, 'testCentroid_XLines', FATAL) ) return
    
    call xLineGrid%getCentroid( 1, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [2._FLOAT, 1._FLOAT], r%getValues(), 'Wrong &
         &centroid coordinates.' )

    call xLineGrid%getCentroid( 2, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [2._FLOAT, 2._FLOAT], r%getValues(), 'Wrong &
         &centroid coordinates.' )
  end subroutine testCentroid_XLines


  subroutine testSize_XLines
    real(FLOAT) :: a
    if ( log%test(MOD_NAME, 'testSize_XLines', &
         FATAL) ) return
        
    call xLineGrid%getSize( 1, a, log )
    if ( log%test(FATAL) ) return
    call assertEqual( 2._FLOAT, a, 'Wrong cell size.' )
  end subroutine testSize_XLines


  subroutine testCentroid_YLines
    type(RealVectorType) :: r
    if ( log%test(MOD_NAME, 'testCentroid_YLines', &
         FATAL) ) return
        
    call yLineGrid%getCentroid( 1, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [1._FLOAT, 1.5_FLOAT], r%getValues(), 'Wrong &
         &centroid coordinates.' )
    
    call yLineGrid%getCentroid( 2, r, log )
    if ( log%test(FATAL) ) return
    call assertEqual( [3._FLOAT, 1.5_FLOAT], r%getValues(), 'Wrong &
         &centroid coordinates.' )
  end subroutine testCentroid_YLines


  subroutine testSize_YLines
    real(FLOAT) :: a
    if ( log%test(MOD_NAME, 'testSize_YLines', &
         FATAL) ) return
        
    call yLineGrid%getSize( 1, a, log )
    if ( log%test(FATAL) ) return
    call assertEqual( 1._FLOAT, a, 'Wrong cell size.' )
  end subroutine testSize_YLines
    
  

  
end module RectangleAndSubsetTests
