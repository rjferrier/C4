module ObliqueGradientTests
  
  use TestUtilities
  use Global
  use FlowFieldModule

  ! system under test
  use SpatialDerivativesModule
  
  ! helpers
  use DirectionsModule

  implicit none
  
  character(*), parameter :: MOD_NAME = 'ObliqueGradientTests'
  
  ! system under test
  class(DirectedSpatialDerivativeInterface), allocatable :: &
       directedSpatialDerivative
  
  type(TestLogType) :: log
  
  integer, dimension(2, 6), parameter :: undiffPowers = reshape( [&
       0, 1, 2, 3, 4, 5, &
       5, 4, 3, 2, 1, 0], [2, 6], order=[2, 1] )
  real(FLOAT), dimension(NDIM, 1), parameter :: testCoords = &
       reshape( [2._FLOAT, 1.5_FLOAT], [NDIM, 1] )
  
contains
  
  subroutine setUp
    integer :: allocStat
    call log%init( MOD_NAME, 'setup' )

    call createFirstSpatialDerivative( directedSpatialDerivative, &
         unitVector([2._FLOAT, 1._FLOAT]), log )
    
  end subroutine setUp
  
  
  subroutine tearDown
    integer :: deallocStat
    call log%reinit( MOD_NAME, 'teardown' )
    
    call destroy( directedSpatialDerivative, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown
  
  
  subroutine testRowComponents_OnePoint
    real(FLOAT), dimension(6) :: expected
    real(FLOAT), dimension(NDIM, 1, 6) :: actual
    integer :: i
    if ( log%test(MOD_NAME, 'testRowComponents_OnePoint', FATAL) ) return
    
    actual = directedSpatialDerivative%&
         computeSubRowComponentsFromRawCoords( testCoords, undiffPowers )
    
    ! matlab>> x = meshgrid([2 1.5], 1:6).'; p = [0:5; 5:-1:0];
    ! p(1,:).*x(1,:).^max(0, p(1,:)-1) .* x(2,:).^p(2,:);
    ! fprintf('%.5f_FLOAT, ', ans); fprintf('\n');
    expected = [0.00000_FLOAT, 5.06250_FLOAT, 13.50000_FLOAT, &
         27.00000_FLOAT, 48.00000_FLOAT, 80.00000_FLOAT]
    do i = 1, 6
       call assertEqual( expected(i), actual(1, 1, i), TOL, 'Element '//&
            str(i)//' on x-row failed.')
    end do

    ! >> x(1,:).^p(1,:) .* p(2,:).*x(2,:).^max(0, p(2,:)-1);
    ! fprintf('%.5f_FLOAT, ', ans); fprintf('\n');
    expected = [25.31250_FLOAT, 27.00000_FLOAT, 27.00000_FLOAT, &
         24.00000_FLOAT, 16.00000_FLOAT, 0.00000_FLOAT]
    do i = 1, 6
       call assertEqual( expected(i), actual(2, 1, i), TOL, 'Element '//&
            str(i)//' on y-row failed.')
    end do
  end subroutine testRowComponents_OnePoint

  
  
  subroutine testRow_OnePoint
    real(FLOAT), dimension(6) :: expected
    real(FLOAT), dimension(1, 6) :: actual
    real(FLOAT), dimension(NDIM, 1, 6) :: rowComponents
    integer :: i
    if ( log%test(MOD_NAME, 'testRow_OnePoint', FATAL) ) return
    
    rowComponents = directedSpatialDerivative%&
         computeSubRowComponentsFromRawCoords( testCoords, undiffPowers )
    actual = directedSpatialDerivative%computeSubRows( rowComponents )
    
    ! matlab>> x = meshgrid([2 1.5], 1:6).'; p = [0:5; 5:-1:0];
    ! p(1,:).*x(1,:).^max(0, p(1,:)-1) .* x(2,:).^p(2,:);
    ! fx = p(1,:).*x(1,:).^max(0, p(1,:)-1) .* x(2,:).^p(2,:);
    ! fy = x(1,:).^p(1,:) .* p(2,:).*x(2,:).^max(0, p(2,:)-1);
    ! 2/sqrt(5)*fx + 1/sqrt(5)*fy; fprintf('%.14f_FLOAT, &\n', ans);
    ! fprintf('\n');
    expected = [&
         11.32009413609268_FLOAT, &
         16.60280473293594_FLOAT, &
         24.14953415699773_FLOAT, &
         34.88266044899672_FLOAT, &
         50.08792269599529_FLOAT, &
         71.55417527999327_FLOAT]
    do i = 1, 6
       call assertEqual( expected(i), actual(1, i), TOL, &
            'Element '//str(i)//' failed.')
    end do
  end subroutine testRow_OnePoint

  
end module ObliqueGradientTests
