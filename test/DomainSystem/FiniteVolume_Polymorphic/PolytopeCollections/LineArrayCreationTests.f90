module LineArrayCreationTests
  
  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule

  implicit none

  class(PolytopeArrayInterface), allocatable :: pointArray, lineArray
  type(PolytopePointerType) :: pp1, pp2, pp3
  type(TestLogType) :: log

  character(*), parameter, private :: MOD_NAME = &
       'LineArrayCreationTests'

  
contains
  
  subroutine setUp
    call log%init( MOD_NAME, 'setUp' )

    call createPointArray( pointArray, real((/1., 2., 3., 4., 5., &
         6./), FLOAT), log )
    if ( log%test(FATAL) ) return
    
    pp1 = ptr( pointArray, 1 )
    pp2 = ptr( pointArray, 2 )
    pp3 = ptr( pointArray, 3 )
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )

    call destroy( pointArray, log )  
    call pp1%deinit()
    call pp2%deinit()
    call pp3%deinit()
    call destroy( lineArray, log )

    call log%deinit( )
  end subroutine tearDown

    
  subroutine testGoodLineArrayDefinition
    if ( log%test(MOD_NAME, 'testGoodLineArrayDefinition', FATAL) ) &
         return
    
    call createSimplexArray( lineArray, 1, (/ pp1, pp2, pp1, pp3, pp2, &
         pp3 /), pointArray, log )
    if ( log%test(FATAL) ) return
    
    call assertEqual( 3, lineArray%getNumPolytopes(), 'LineArray is &
         &wrong size.' )
  end subroutine testGoodLineArrayDefinition

  
  subroutine testBadLineArrayDefinition
    if ( log%test(MOD_NAME, 'testBadLineArrayDefinition', FATAL) ) &
         return
    
    call createSimplexArray( lineArray, 1, (/ pp1, pp2, pp1, pp3, &
         pp2 /), pointArray, log )
    ! hold off the conditional return - we are expecting a fatal
    
    call assertEqual( FATAL, log%getSeverity(), 'Odd number of &
         &Cartesian positions specified for coordinate list, so this &
         &should be a bad geometry definition (but sound &
         &allocation).  A non-fatal error should be triggered.' )
    
    call log%reinit( MOD_NAME, 'testBadPointArrayDefinition' )
  end subroutine testBadLineArrayDefinition

  
end module LineArrayCreationTests
