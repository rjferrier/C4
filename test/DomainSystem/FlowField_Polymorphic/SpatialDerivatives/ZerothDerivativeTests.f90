module ZerothDerivativeTests
  
  use TestUtilities
  use Global
  use FlowFieldModule

  ! system under test
  use SpatialDerivativesModule
  
  ! helpers
  use DirectionsModule

  implicit none
  
  character(*), parameter :: MOD_NAME = 'ZerothDerivativeTests'
  
  ! system under test
  class(DirectedSpatialDerivativeInterface), allocatable :: &
       directedSpatialDerivative
  
  type(TestLogType) :: log
  
  integer, dimension(2, 6), parameter :: undiffPowers = reshape( [&
       0, 1, 2, 3, 4, 5, &
       5, 4, 3, 2, 1, 0], [2, 6], order=[2,1] )
  real(FLOAT), dimension(NDIM, 1), parameter :: testCoords = &
       reshape( [2._FLOAT, 1.5_FLOAT], [NDIM, 1] )
  
contains
  
  subroutine setUp
    integer :: allocStat
    call log%init( MOD_NAME, 'setup' )

    call createZerothSpatialDerivative( directedSpatialDerivative, &
         log )
    
  end subroutine setUp
  
  
  subroutine tearDown
    integer :: deallocStat
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( directedSpatialDerivative, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown
  
  
  subroutine testSpreadPowers_OnePoint
    integer, dimension(NDIM, 1, 6) :: expected, actual
    integer :: i, j
    if ( log%test(MOD_NAME, 'testRowComponents_OnePoint', FATAL) ) return
    
    actual = spreadPowers( undiffPowers, 1 )
    
    do j = 1, 6
       do i = 1, 2
          expected(i, 1, j) = undiffPowers(i, j)
       end do
    end do
       
    call assertEqual( expected(1, 1, :), actual(1, 1, :), 'Elements &
         &on x-row failed.')
    call assertEqual( expected(2, 1, :), actual(2, 1, :), 'Elements &
         &on y-row failed.')
  end subroutine testSpreadPowers_OnePoint
  
  
  subroutine testSpreadCoords_OnePoint
    real(FLOAT), dimension(NDIM, 1, 6) :: expected, actual
    integer :: i
    if ( log%test(MOD_NAME, 'testRowComponents_OnePoint', FATAL) ) return
    
    actual = spreadCoords( testCoords, 6 )
    
    do i = 1, 2
       expected(i, 1, :) = testCoords(i, 1)
    end do
       
    do i = 1, 6
       call assertEqual( expected(1, 1, i), actual(1, 1, i), TOL, 'Element '//&
            str(i)//' on x-row failed.')
    end do
    do i = 1, 6
       call assertEqual( expected(2, 1, i), actual(2, 1, i), TOL, 'Element '//&
            str(i)//' on y-row failed.')
    end do
  end subroutine testSpreadCoords_OnePoint
  
  
  subroutine testRowComponents_OnePoint
    real(FLOAT), dimension(NDIM, 1, 6) :: expected, actual
    integer :: i
    if ( log%test(MOD_NAME, 'testRowComponents_OnePoint', FATAL) ) return
    
    actual = directedSpatialDerivative%&
         computeSubRowComponentsFromRawCoords( testCoords, undiffPowers )
    
    ! matlab>> x = meshgrid([2, 1.5], 1:6).'; p = [0:5; 5:-1:0]; x.^p;
    ! fprintf('%.5f_FLOAT, ', ans); fprintf('\n');
    expected = reshape([1.00000_FLOAT, 7.59375_FLOAT, 2.00000_FLOAT, &
         5.06250_FLOAT, 4.00000_FLOAT, 3.37500_FLOAT, 8.00000_FLOAT, &
         2.25000_FLOAT, 16.00000_FLOAT, 1.50000_FLOAT, &
         32.00000_FLOAT, 1.00000_FLOAT], [2,1,6] )

    do i = 1, 6
       call assertEqual( expected(1, 1, i), actual(1, 1, i), TOL, 'Element '//&
            str(i)//' on x-row failed.')
    end do
    
    do i = 1, 6
       call assertEqual( expected(2, 1, i), actual(2, 1, i), TOL, 'Element '//&
            str(i)//' on y-row failed.')
    end do
  end subroutine testRowComponents_OnePoint
  
  
  subroutine testRow_OnePoint
    real(FLOAT), dimension(1, 6) :: expected, actual
    real(FLOAT), dimension(NDIM, 1, 6) :: rowComponents
    integer :: i
    if ( log%test(MOD_NAME, 'testRow_OnePoint', FATAL) ) return
    
    rowComponents = directedSpatialDerivative%&
         computeSubRowComponentsFromRawCoords( testCoords, undiffPowers )
    actual = directedSpatialDerivative%computeSubRows( rowComponents )
    
    ! matlab>> x = meshgrid([2, 1.5], 1:6).'; p = [0:5; 5:-1:0]; x.^p;
    ! prod(ans); fprintf('%.5f_FLOAT, ', ans); fprintf('\n');
    expected = reshape([7.59375_FLOAT, 10.12500_FLOAT, &
         13.50000_FLOAT, 18.00000_FLOAT, 24.00000_FLOAT, &
         32.00000_FLOAT], [1, 6])
    
    do i = 1, 6
       call assertEqual( expected(1, i), actual(1, i), TOL, 'Element '//&
            str(i)//' failed.')
    end do
  end subroutine testRow_OnePoint

  
end module ZerothDerivativeTests
