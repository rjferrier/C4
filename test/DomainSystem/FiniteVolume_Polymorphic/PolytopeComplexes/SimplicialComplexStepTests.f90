module SimplicialComplexStepTests

  use pFUnit
  use Global
  use TestUtilities

  ! We are going to shortcut the creation of mock polytopes, so this
  ! is not technically a unit test module yet.
  use FiniteVolumeModule
  use PolytopeComplexesModule

  ! the following module defines the test geometry, which relies upon
  ! Polytope classes.
  use SmallStepGrid

  implicit none

  character(*), parameter, private :: MOD_NAME = &
       'SimplicialComplexStepTests'

  class(PolytopeComplexInterface), allocatable :: sc
  type(PolytopePointerSetType) :: expectedPtps, actualPtps
  type(TestLogType) :: log
  
contains
  

  subroutine setUp
    call log%init( MOD_NAME, 'setup' )
    
    ! set up the geometry
    call setUp_SmallStepGrid( log )
    if ( log%test(FATAL) ) return
    
    ! triangleArray contains exactly the contents of our test
    ! simplicial complex.  Use for the definition.
    call createUnmixedSimplicialComplex( sc, triangleArray, [1:4], &
         log )
    if ( log%test(FATAL) ) return
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
        
    call destroy( sc, log )
    call expectedPtps%deinit( log )
    call actualPtps%deinit( log )
    
    ! tear down the geometry
    call tearDown_SmallStepGrid( log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown
  

  subroutine testGetSimpliciesAtDepth0
    type(PolytopePointerType), dimension(4) :: pp
    integer :: i
    if ( log%test(MOD_NAME, 'testGetSimpliciesAtDepth0', FATAL) ) return

    do i = 1, 4
       pp(i) = ptr(triangleArray, i)
    end do
    
    call expectedPtps%init( pp, log )
    if ( log%test(FATAL) ) return
    
    call sc%getPolytopes( 0, actualPtps, log )
    if ( log%test(FATAL) ) return

    call assertTrue( actualPtps%sameAs(expectedPtps), 'Returned set &
         &of Polytopes does not match expected set.' )
  end subroutine testGetSimpliciesAtDepth0

  
  subroutine testGetSimpliciesAtDepth1
    type(PolytopePointerType), dimension(4) :: pp
    integer :: i
    if ( log%test(MOD_NAME, 'testGetSimpliciesAtDepth1', FATAL) ) return

    do i = 1, 4
       pp(i) = ptr(freeLineArray, i)
    end do
    
    call expectedPtps%init( pp, log )
    if ( log%test(FATAL) ) return

    call sc%getPolytopes( 1, actualPtps, log )
    if ( log%test(FATAL) ) return
    
    call assertTrue( actualPtps%sameAs(expectedPtps), 'Returned set &
         &of Polytopes does not match expected set.' )
  end subroutine testGetSimpliciesAtDepth1
    

  subroutine testGetSimpliciesAtDepth2
    type(PolytopePointerType), dimension(1) :: pp
    if ( log%test(MOD_NAME, 'testGetSimpliciesAtDepth2', FATAL) ) return

    pp(1) = ptr(freePointArray, 1)
    
    call expectedPtps%init( pp, log )
    if ( log%test(FATAL) ) return

    call sc%getPolytopes( 2, actualPtps, log )
    if ( log%test(FATAL) ) return

    call assertTrue( actualPtps%sameAs(expectedPtps), 'Returned set &
         &of Polytopes does not match expected set.' )
  end subroutine testGetSimpliciesAtDepth2


    
end module SimplicialComplexStepTests
 
