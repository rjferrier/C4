module ArrayPointArrangementTests

  use TestUtilities
  use Global
  use FiniteVolumeModule
  use SemiLagrangianModule
  use FlowFieldModule
  use FakePolytopeArrays_RightAngleTriangle
  
  ! dynamic module under test
  use PointArrangementsModule

  implicit none
  
  character(*), parameter, private :: MOD_NAME = &
       'ArrayPointArrangementTests'

  type(TestLogType) :: log
  
  class(PointArrangementInterface), allocatable :: pointArrangement

  
contains


  subroutine setup
    call log%init( MOD_NAME, 'setup' )
    
    call setup_FakePolytopeArrays( log )
    if ( log%test(FATAL) ) return
  end subroutine setup


  subroutine teardown
    call log%reinit( MOD_NAME, 'teardown' )
    
    call teardown_FakePolytopeArrays( log )

    call log%report()
    call log%deinit()
  end subroutine teardown


  subroutine testPositionOnVertex
    type(RealVectorType), dimension(1) :: pointCoords
    if ( log%test(MOD_NAME, 'testPositionOnVertex', FATAL) ) return
    
    call createArrayPointArrangement( pointArrangement, pointArray, 1, &
         log )
    if ( log%test(FATAL) ) return
    
    call pointArrangement%getPositions( pointCoords, 2, log=log ) 

    call assertEqual( [7._FLOAT, 1._FLOAT], pointCoords(1)&
         %getValues(), TOL, 'Expected point to be coincident with second &
         &vertex.' )
    
    call destroy( pointArrangement, log )
  end subroutine testPositionOnVertex

  
  subroutine testOnePointPositionOnEdge
    type(RealVectorType), dimension(1) :: pointCoords
    if ( log%test(MOD_NAME, 'testOnePointPositionOnEdge', FATAL) ) &
         return
    
    call createArrayPointArrangement( pointArrangement, lineArray, 1, &
         log )
    if ( log%test(FATAL) ) return
    
    call pointArrangement%getPositions( pointCoords, 2, log=log )
    
    call assertEqual( [4._FLOAT, 2.5_FLOAT], pointCoords(1)&
         %getValues(), TOL, 'Expected point to be halfway along edge.' )

    call destroy( pointArrangement, log )
  end subroutine testOnePointPositionOnEdge

  
  subroutine testTooManyOutputCoordinates
    ! note the two elements in the following declaration
    type(RealVectorType), dimension(2) :: pointCoords
    if ( log%test(MOD_NAME, 'testTooManyOutputCoordinates', &
         FATAL) ) return
    
    call createArrayPointArrangement( pointArrangement, lineArray, &
         1, log )
    if ( log%test(FATAL) ) return

    call pointArrangement%getPositions( pointCoords, 2, log=log )
    
    call assertEqual( FATAL, log%getSeverity(), 'Too many output &
         &coordinates should trigger a fatal error in the log object.' )
    call log%reinit( MOD_NAME, 'testTooManyOutputCoordinates' )

    call destroy( pointArrangement, log )
  end subroutine testTooManyOutputCoordinates

  
  subroutine testTwoPointPositionsOnEdge
    type(RealVectorType), dimension(2) :: vertexCoords, &
         expectedCoords, actualCoords
    if ( log%test(MOD_NAME, 'testTwoPointPositionsOnEdge', &
         FATAL) ) return
    
    call createArrayPointArrangement( pointArrangement, lineArray, &
         2, log )
    if ( log%test(FATAL) ) return

    call pointArrangement%getPositions( actualCoords, 2, log=log )

    vertexCoords(1) = [7._FLOAT, 1._FLOAT]
    vertexCoords(2) = [1._FLOAT, 4._FLOAT]
    expectedCoords(1) = vertexCoords(1) + ( vertexCoords(2) - &
         vertexCoords(1) )*( 1._FLOAT - 1._FLOAT/sqrt(3._FLOAT) )/&
         2._FLOAT
    expectedCoords(2) = vertexCoords(1) + ( vertexCoords(2) - &
         vertexCoords(1) )*( 1._FLOAT + 1._FLOAT/sqrt(3._FLOAT) )/&
         2._FLOAT
    
    call assertEqual( expectedCoords(1)%getValues(), actualCoords(1)&
         %getValues(), TOL, 'Actual does not match expected coords &
         &for lower-right hand point.' )
    call assertEqual( expectedCoords(2)%getValues(), actualCoords(2)&
         %getValues(), TOL, 'Actual does not match expected coords &
         &for upper-left hand point.' )
    
    call destroy( pointArrangement, log )
  end subroutine testTwoPointPositionsOnEdge

  
  subroutine testNotEnoughOutputCoordinates
    type(RealVectorType), dimension(1) :: actualCoords
    if ( log%test(MOD_NAME, 'testNotEnoughOutputCoordinates', &
         FATAL) ) return
    
    call createArrayPointArrangement( pointArrangement, lineArray, 2, &
         log )
    if ( log%test(FATAL) ) return

    call pointArrangement%getPositions( actualCoords, 2, log=log )

    call assertEqual( FATAL, log%getSeverity(), 'Too few output &
         &coordinates should trigger a fatal error in the log &
         &object.' )
    call log%reinit( MOD_NAME, 'testNotEnoughOutputCoordinates' )

    call destroy( pointArrangement, log )
  end subroutine testNotEnoughOutputCoordinates


  subroutine testFourPointPositionsOnTriangle
    type(RealVectorType), dimension(3) :: vertexCoords
    type(RealVectorType), dimension(4) :: expectedCoords, actualCoords
    real(FLOAT) :: c1, c2
    integer :: i, j
    if ( log%test(MOD_NAME, 'testFourPointPositionsOnTriangle', &
         FATAL) ) return
    
    call createArrayPointArrangement( pointArrangement, triangleArray, &
         3, log )
    if ( log%test(FATAL) ) return

    call pointArrangement%getPositions( actualCoords, 1, log=log )

    vertexCoords(1) = [1._FLOAT, 1._FLOAT]
    vertexCoords(2) = [7._FLOAT, 1._FLOAT]
    vertexCoords(3) = [1._FLOAT, 4._FLOAT]

    c1 = 0.6_FLOAT
    c2 = 0.2_FLOAT
    expectedCoords(4) = 0._FLOAT
    do i = 1, 3
       expectedCoords(i) = vertexCoords(i)*c1
       do j = 1, 3
          if ( i /= j ) expectedCoords(i) = &
               expectedCoords(i) + vertexCoords(j)*c2
       end do
       expectedCoords(4) = &
            expectedCoords(4) + vertexCoords(i)/3._FLOAT
    end do
    
    do i = 1, 4
       call assertEqual( expectedCoords(i)%getValues(), actualCoords(&
            i)%getValues(), TOL, 'Actual does not match expected &
            &coords for point #'//str(i))
    end do
    
    call destroy( pointArrangement, log )
  end subroutine testFourPointPositionsOnTriangle
  
  
end module ArrayPointArrangementTests
