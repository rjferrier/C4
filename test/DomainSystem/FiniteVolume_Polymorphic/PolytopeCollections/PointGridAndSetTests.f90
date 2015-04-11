! See PointArrayAndSetTests for more thorough tests involving PPSets.
module PointGridAndSetTests

  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule
  
  implicit none

  character(*), parameter :: MOD_NAME = &
       'PointGridAndSetTests'
  
  class(PolytopeGridInterface), allocatable :: pointGrid
  type(PolytopePointerSetType) :: pps
  type(PolytopePointerType) :: pp1, pp2, pp3, pp4
  type(TestLogType) :: log

contains
   

  subroutine setUp
    type(PolytopePointerType), dimension(4) :: pp
    call log%init( MOD_NAME, 'setup' )
    
    call createPointGrid( pointGrid, vector([2, 2]), &
         vector([0.0_FLOAT, 0.0_FLOAT]), &
         vector([1.0_FLOAT, 1.0_FLOAT]), log )
    if ( log%test(FATAL) ) return

    pp1 = ptr( pointGrid, [1, 1] )
    pp2 = ptr( pointGrid, [2, 1] )
    pp3 = ptr( pointGrid, [1, 2] )
    pp4 = ptr( pointGrid, [2, 2] )
    
    pp = [pp1, pp2, pp3, pp4]
    call pps%init( pp, log )
  end subroutine setUp

  
  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
     
    call pps%deinit( log )
    call pp1%deinit()
    call pp2%deinit()
    call pp3%deinit()
    call pp4%deinit()
    call destroy( pointGrid, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown

  

  subroutine testGoodMatchWithPolytopePointer
    if ( log%test(MOD_NAME, 'testGoodMatchWithPolytopePointer', &
         FATAL) ) return
    
    call assertEqual( pointGrid%convertToIndex( vector([2, 1]) ), &
         pp2%match( pointGrid ), "Should have matched pointArray and &
         &returned an index corresponding to address [2, 1]." )
  end subroutine testGoodMatchWithPolytopePointer

  

  subroutine testGoodQueryWithPolytopePointerSet
    integer, dimension(:), allocatable :: indices
    integer :: deallocStat
    if ( log%test(MOD_NAME, 'testGoodQueryWithPolytopePointerSet', &
         FATAL) ) return
    
    call pps%query( pointGrid, indices, log )
    if ( log%test(FATAL) ) return

    ! keep in mind that the order of the indices is arbitrary, so take
    ! care to match the original spec of pps in the expected indices
    call assertEqual( [pointGrid%convertToIndex(vector([2, 1])), &
         pointGrid%convertToIndex(vector([1, 1])), &
         pointGrid%convertToIndex(vector([2, 2])), &
         pointGrid%convertToIndex(vector([1, 2]))], indices, "Should &
         &have matched." )
    
    deallocate( indices, stat=deallocStat )
    call log%addEvent( deallocStat/=0, WARNING, 'Problem &
         &deallocating indices.  STAT='//int2str(deallocStat) )
  end subroutine testGoodQueryWithPolytopePointerSet
  

end module PointGridAndSetTests
