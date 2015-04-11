module PolytopeConnectionTests

  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule

  ! this module defines the test geometry
  use SmallStepGrid

  implicit none
  
  character(*), parameter, private :: MOD_NAME = &
       'PolytopeConnectionTests'
  
  type(PolytopePointerType), pointer :: pp
  type(PolytopePointerSetType) :: g_boundaries, D_boundaries
  type(TestLogType) :: log
    
contains


  !-------------------------------------------------------------------
  !- fixture setup/teardown
  !-------------------------------------------------------------------

  subroutine setUp
    call log%init( MOD_NAME, 'setUp' )

    ! set up the geometry
    call setUp_SmallStepGrid( log )
    if ( log%test(FATAL) ) return

    ! boundaries to compare with in the tests
    call triangleArray%collectSubPolytopes( 4, D_boundaries, &
         log, depth=1 )
    call freeLineArray%collectSubPolytopes( 5, g_boundaries, &
         log, depth=1 )
  end subroutine setUp


  subroutine tearDown
    call log%reinit( MOD_NAME, 'tearDown' )

    call g_boundaries%deinit( log )
    call D_boundaries%deinit( log )

    call tearDown_SmallStepGrid( log )

    call log%report()
    call log%deinit()
  end subroutine tearDown


  !-------------------------------------------------------------------
  !- unit tests
  !-------------------------------------------------------------------

  subroutine testBoundaryCollection
    type(PolytopePointerType), dimension(3) :: pp
    type(PolytopePointerSetType) :: pps
    if ( log%test(MOD_NAME, 'testBoundaryCollection', FATAL) ) return

    pp = [ ptr(xLineArray, 1), ptr(freeLineArray, 5), ptr(&
         yLineGrid, [3,1]) ]
    call pps%init( pp, log )
    if ( log%test(FATAL) ) return
    
    ! this should be failing 
    call assertTrue( pps%sameAs( D_boundaries, log ), 'Error in &
         &triangle D''s collection of immediate boundaries.' )
    
    call pps%deinit( log )
  end subroutine testBoundaryCollection
    
  
  subroutine testPointComparisons
    type(PolytopePointerType) :: pp1, pp2
    if ( log%test(MOD_NAME, 'testPointComparisons', FATAL) ) return
    
    pp1 = ptr( xPointArray, 1 )
    pp2 = ptr( pointGrid, (/3, 2/) )
        
    call g_boundaries%find(pp, 1)
    call assertTrue( pp%sameAs(pp1), 'Element in point array should &
         &test same as that defining first point of free line no.5 (&
         &g) in test fixture.' )
    
    call g_boundaries%find(pp, 2)
    call assertTrue( pp%sameAs(pp2), 'Element in point grid &
         &should test same as that defining second point of free &
         &line no.5 (g) in test fixture.' )

    call pp1%deinit()
    call pp2%deinit()
  end subroutine testPointComparisons


  subroutine testPointInverseComparisons
    type(PolytopePointerType) :: pp1, pp2
    if ( log%test(MOD_NAME, 'testPointInverseComparisons', FATAL) ) &
         return

    pp1 = ptr( xPointArray, 1 )
    pp2 = ptr( pointGrid, (/3, 2/) )    
    
    call g_boundaries%find(pp, 1)
    call assertTrue( pp1%sameAs(pp), &
         'First point of free line no.5 (g) in test fixture should &
         &test same as element in point grid.' )
    
    call g_boundaries%find(pp, 2)
    call assertTrue( pp2%sameAs(pp), &
         'Second point of free line no.5 (g) in test fixture should &
         &test same as element in point array.' )

    call pp1%deinit()
    call pp2%deinit()
  end subroutine testPointInverseComparisons
    
 
  subroutine testLineComparisons
    type(PolytopePointerType) :: pp1, pp2
    if ( log%test(MOD_NAME, 'testLineComparisons', FATAL) ) return

    pp1 = ptr( freeLineArray, 5 )
    pp2 = ptr( yLineGrid, (/3, 1/) )
    
    call D_boundaries%find(pp, 1)
    call assertTrue( pp%sameAs(pp1), 'Element in line array should &
         &test same as that defining first point of triangle no.4 (&
         &D) in test fixture.' )
    
    call D_boundaries%find(pp, 2)
    call assertTrue( pp%sameAs(pp2), 'Element in line grid should &
         &test same as that defining first point of triangle no.4 (&
         &D) in test fixture.' )

    call pp1%deinit()
    call pp2%deinit()
  end subroutine testLineComparisons
  

  subroutine testWrongElementComparisons
    type(PolytopePointerType) :: ptPtr1, lnPtr2
    if ( log%test(MOD_NAME, 'testWrongElementComparisons', FATAL) ) return
    
    ptPtr1 = ptr( xPointArray, 1 )
    lnPtr2 = ptr( yLineGrid, (/3, 1/) )

    call g_boundaries%find(pp, 2)
    call assertFalse( ptPtr1%sameAs(pp), &
         'Pointing to the wrong element of the right point &
         &array.  Should fail sameAs(.) test.' )

    call D_boundaries%find(pp, 1)
    call assertFalse( lnPtr2%sameAs(pp), &
         'Pointing to the wrong element of the right line &
         &grid.  Should fail sameAs(.) test.' )    
  end subroutine testWrongElementComparisons

  
  subroutine testImposterComparisons
    type(PolytopePointerType) :: ptPtr1, ptPtr2, lnPtr2
    class(PolytopeArrayInterface), allocatable :: xPointArrayImposter
    class(PolytopeGridInterface), allocatable :: pointGridImposter, &
         yLineGridImposter
    type(RealVectorType) :: minCoords, maxCoords
    type(IntVectorType) :: gridSize
    type(PolytopeGridPointerType) :: nullGridPtr
    if ( log%test(MOD_NAME, 'testImposterComparisons', FATAL) ) return

    gridSize  = vector([3, 2])
    minCoords = vector([0.0_FLOAT, 0.0_FLOAT])
    maxCoords = vector([1.5_FLOAT, 0.8_FLOAT])

    call createPointGrid( pointGridImposter, gridSize+1, &
         minCoords, maxCoords, log )
    call createPointArray( xPointArrayImposter, &
         (/1.2_FLOAT, 0.0_FLOAT/), log )
    call createOrthotopeGrid( yLineGridImposter, [nullGridPtr, ptr(&
         pointGrid)], log )
    if ( log%test(FATAL) ) return

    ptPtr1 = ptr( xPointArrayImposter, 1 )
    ptPtr2 = ptr( pointGridImposter, (/3, 2/) )
    lnPtr2 = ptr( yLineGridImposter, (/3, 1/) )

    call g_boundaries%find(pp, 1)
    call assertFalse( ptPtr1%sameAs(pp), 'Pointing to the right &
         &element of an imposter point array.  Should fail sameAs(.) &
         &test.' )
    
    call g_boundaries%find(pp, 2)
    call assertFalse( ptPtr2%sameAs(pp), &
         'Pointing to the right element of an imposter point grid.  &
         &Should fail sameAs(.) test.' )

    call D_boundaries%find(pp, 1)
    call assertFalse( lnPtr2%sameAs(pp), &
         'Pointing to the right element of an imposter line grid.  &
         &Should fail sameAs(.) test.' )

    call destroy( pointGridImposter, log )
    call destroy( xPointArrayImposter, log )
    call destroy( yLineGridImposter, log )
  end subroutine testImposterComparisons


  
  subroutine testCollectSubPolytopesA
    type(PolytopePointerType), dimension(6) :: pp
    type(PolytopePointerSetType) :: expectedSubPtps
    type(PolytopePointerType) :: triangleA
    type(PolytopePointerSetType) :: actualSubPtps
    if ( log%test(MOD_NAME, 'testCollectSubPolytopesA', FATAL) ) return

    pp = [ptr(freeLineArray, 2), &
         ptr(freeLineArray, 3), &
         ptr(yLineArray, 1), &
         ptr(yPointArray, 1), &
         ptr(freePointArray, 1), &
         ptr(pointGrid, (/4, 2/))]
    call expectedSubPtps%init( pp, log )
    if ( log%test(FATAL) ) return
    
    triangleA = ptr( triangleArray, 1 )
    call triangleA%collectSubPolytopes( actualSubPtps, log )
    if ( log%test(FATAL) ) return
    
    call assertTrue( actualSubPtps%sameAs(expectedSubPtps), 'Set of &
         &subdimensional boundaries is wrong.' )
    
    call expectedSubPtps%deinit( log )
    call actualSubPtps%deinit( log )
  end subroutine testCollectSubPolytopesA

  
  subroutine testCollectSubPolytopesD
    type(PolytopePointerSetType) :: expectedSubPtps
    type(PolytopePointerType) :: triangleD
    type(PolytopePointerSetType) :: actualSubPtps
    type(PolytopePointerType), dimension(6) :: pp
    if ( log%test(MOD_NAME, 'testCollectSubPolytopesD', FATAL) ) return

    pp = [ptr(freeLineArray, 5), &
         ptr(yLineGrid, (/3, 1/)), &
         ptr(xLineArray, 1), &
         ptr(xPointArray, 1), &
         ptr(pointGrid, (/3, 1/)), &
         ptr(pointGrid, (/3, 2/))]
    call expectedSubPtps%init( pp, log )
    if ( log%test(FATAL) ) return

    triangleD = ptr( triangleArray, 4 )
    call triangleD%collectSubPolytopes( actualSubPtps, log )
    if ( log%test(FATAL) ) return

    call assertTrue( actualSubPtps%sameAs(expectedSubPtps), 'Set of &
         &subdimensional boundaries is wrong.' )
    
    call expectedSubPtps%deinit( log )
    call actualSubPtps%deinit( log )
  end subroutine testCollectSubPolytopesD
  

  ! to come
!!$  subroutine testCollectSuperPolytopes_i
!!$    type(PolytopePointerSetType) :: expectedSuperPtps
!!$    type(PolytopePointerType) :: vertex_i
!!$    type(PolytopePointerSetType) :: actualSuperPtps
!!$    if ( log%test(MOD_NAME, 'testCollectSuperPolytopes_i', FATAL) ) return
!!$    
!!$    call expectedSuperPtps%init( (/&
!!$         ptr(freeLineArray, 1), &
!!$         ptr(freeLineArray, 5), &
!!$         ptr(xLineArray, 1), &
!!$         ptr(triangleArray, 3), &
!!$         ptr(triangleArray, 4) /), log )
!!$    if ( log%test(FATAL) ) return
!!$
!!$    vertex_i = ptr( xPointArray, 0 )
!!$    call vertex_i%collectSuperPolytopes( actualSuperPtps, log )
!!$    if ( log%test(FATAL) ) return
!!$
!!$    call assertTrue( actualSuperPtps%sameAs(expectedSuperPtps), 'Set &
!!$         &of superdimensional incident polytopes is wrong.' )
!!$    
!!$    call expectedSuperPtps%deinit( log )
!!$    call actualSuperPtps%deinit( log )
!!$  end subroutine testCollectSuperPolytopes_i



end module PolytopeConnectionTests
