module PointArrayCreationTests

  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule
  
  implicit none

  class(PolytopeArrayInterface), allocatable :: pointArray
  type(TestLogType) :: log
  
  character(*), parameter, private :: MOD_NAME = &
       'PointArrayCreationTests'
  
contains

  subroutine setUp
    call log%init( MOD_NAME, 'setUp' )
  end subroutine setUp

  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
    
    call destroy( pointArray, log )        

    if ( log%test(FATAL) ) return
    call log%report()
    call log%deinit()  
  end subroutine tearDown 
    
  
  subroutine testGoodPointArrayDefinition
    if ( log%test(MOD_NAME, 'testGoodPointArrayDefinition', &
         FATAL) ) return
    
    call createPointArray( pointArray, real((/1., 2., 3., 4., 5., &
         6./), FLOAT), log )
    if ( log%test(FATAL) ) return
    
    call assertEqual( 3, pointArray%getNumPolytopes(), 'PointArray &
         &is wrong size.' )
  end subroutine testGoodPointArrayDefinition

  
  subroutine testBadPointArrayDefinition
    call log%init( MOD_NAME, 'testBadPointArrayDefinition' )
    
    call createPointArray( pointArray, real((/1., 2., 3., 4., 5. /), &
         FLOAT), log )
    ! hold off the conditional return - we are expecting a fatal
    
    call assertEqual( FATAL, log%getSeverity(), 'Odd number of &
         &Cartesian positions specified for coordinate list.  Bad &
         &geometry definition should be reported.' )
    
    call log%reinit( MOD_NAME, 'testBadPointArrayDefinition' )
  end subroutine testBadPointArrayDefinition

  
end module PointArrayCreationTests
