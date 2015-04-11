module ObliqueDirectionTests

  ! this also tests the unbound dot_product functions in
  ! FlowFieldModule.
  
  use TestUtilities
  use Global
  use FlowFieldModule
  
  use DirectionsModule
  
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'ObliqueDirectionTests'
  
  ! system under test
  class(DirectionInterface), allocatable :: direction
  
  type(TestLogType) :: log
  
contains

  subroutine setUp
    integer :: allocStat
    call log%init( MOD_NAME, 'setup' )
    
    call createObliqueDirection( direction, vector([2._FLOAT, &
         1._FLOAT]), log )
    
  end subroutine setUp
  
  
  subroutine tearDown
    integer :: deallocStat
    call log%reinit( MOD_NAME, 'teardown' )
    
    call destroy( direction, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown
  
  
  subroutine testIsNormalised
    type(RealVectorType) :: r
    if ( log%test(MOD_NAME, 'testIsNormalised', FATAL) ) return
    
    r = direction%convert()
    call assertEqual( [2._FLOAT, 1._FLOAT]/sqrt(5._FLOAT), r%&
         getValues(), TOL, 'Sum of squares should equal 1.')
  end subroutine testIsNormalised
  
  
  subroutine testDotProduct
    real(FLOAT), dimension(2, 4) :: x
    real(FLOAT), dimension(4) :: y
    if ( log%test(MOD_NAME, 'testDotProduct', FATAL) ) return

    x = reshape( [0._FLOAT, 1._FLOAT, 0._FLOAT, 1._FLOAT, 0._FLOAT, &
         0._FLOAT, 1._FLOAT, 1._FLOAT], [2, 4], order=[2, 1] )
    y = direction%dotProduct(x)
    
    call assertEqual( [0._FLOAT, 2._FLOAT, 1._FLOAT, 3._FLOAT]/sqrt(&
         5._FLOAT), y, TOL, 'Result vector should be [0, nx, ny, nx+ny].')
  end subroutine testDotProduct
  
  
  subroutine testClone
    class(DirectionInterface), dimension(:), allocatable :: dd
    type(RealVectorType) :: r
    if ( log%test(MOD_NAME, 'testClone', FATAL) ) return

    call direction%clone( dd, 3, log )
    
    r = dd(1)%convert()
    call assertEqual( [2._FLOAT, 1._FLOAT]/sqrt(5._FLOAT), &
         r%getValues(), TOL, 'First element of result does not match &
         &input.')
    r = dd(3)%convert()
    call assertEqual( [2._FLOAT, 1._FLOAT]/sqrt(5._FLOAT), &
         r%getValues(), TOL, 'Third element of result does not match &
         &input.')

    call destroy( dd, log )
  end subroutine testClone
  
  
  subroutine testUnboundDotProductSingle
    real(FLOAT), dimension(2, 3, 4) :: x
    real(FLOAT), dimension(3, 4) :: y
    if ( log%test(MOD_NAME, 'testUnboundDotProductSingle', &
         FATAL) ) return

    ! the array shapes for the expanded, unbound case are a bit
    ! tricky.  The indices correspond to (1) physical dimensions, (2)
    ! points, and (3) monomials.  It is the physical dimensions that
    ! collapse to yield the result.
    
    x = reshape( [&
         0._FLOAT, 1._FLOAT, 0._FLOAT, 1._FLOAT, 0._FLOAT, 0._FLOAT, &
         1._FLOAT, 1._FLOAT, &
         
         0._FLOAT, 2._FLOAT, 0._FLOAT, 2._FLOAT, 0._FLOAT, 0._FLOAT, &
         2._FLOAT, 2._FLOAT, &
         
         0._FLOAT, 3._FLOAT, 0._FLOAT, 3._FLOAT, 0._FLOAT, 0._FLOAT, &
         3._FLOAT, 3._FLOAT  &
         ], [2, 3, 4], order=[3, 1, 2] )
    
    y = dot_product( direction, x )
    
    call assertEqual( [0._FLOAT, 2._FLOAT, 1._FLOAT, 3._FLOAT]/sqrt(&
         5._FLOAT), y(1, :), TOL, 'Result vector (1) should be [0, nx, &
         &ny, nx+ny].')
    call assertEqual( [0._FLOAT, 6._FLOAT, 3._FLOAT, 9._FLOAT]/sqrt(&
         5._FLOAT), y(3, :), TOL, 'Result vector (3) should be 3*[0, nx, &
         &ny, nx+ny].')
    
  end subroutine testUnboundDotProductSingle

  
  
end module ObliqueDirectionTests
