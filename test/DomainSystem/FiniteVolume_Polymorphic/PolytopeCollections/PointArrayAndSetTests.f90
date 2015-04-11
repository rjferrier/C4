module PointArrayAndSetTests
  
  use TestUtilities
  use Global
  use FiniteVolumeModule

  ! dynamic module under test
  use PolytopeCollectionsModule
  
  implicit none

  character(*), parameter :: MOD_NAME = &
       'PointArrayAndSetTests'
  
  class(PolytopeArrayInterface), allocatable :: pointArray
  type(PolytopePointerSetType) :: pps
  type(PolytopePointerType) :: pp1, pp2, pp3
  type(TestLogType) :: log

contains
   

  subroutine setUp
    type(PolytopePointerType), dimension(3) :: pp
    call log%init( MOD_NAME, 'setUp' )
    
    call createPointArray( pointArray, real([1., 2., 3., 4., &
         5., 6., 7., 8.], FLOAT), log )
    if ( log%test(FATAL) ) return

    pp1 = ptr( pointArray, 1 )
    pp2 = ptr( pointArray, 2 )
    pp3 = ptr( pointArray, 3 )

    pp = [pp2, pp1, pp3]
    call pps%init( pp, log )
    if ( log%test(FATAL) ) return
    
    call log%report()
    ! don't deinitialise the error object just yet - we may need to
    ! exit early from the tested procedure if fatal errors exist
  end subroutine setUp

  
  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )
    
    call destroy( pointArray, log )
    call pps%deinit( log )

    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testGoodMatchWithPolytopePointer
    if ( log%test(MOD_NAME, 'testGoodMatchWithPolytopePointer', &
         FATAL) ) return
    
    call assertEqual( 2, pp2%match( pointArray ), "Should have &
         &matched pointArray and returned index of 2." )
  end subroutine testGoodMatchWithPolytopePointer
  

  subroutine testBadMatchWithPolytopePointer
    class(PolytopeArrayInterface), allocatable :: imposterPointArray
    if ( log%test(MOD_NAME, 'testBadMatchWithPolytopePointer', &
         FATAL) ) return
    
    call createPointArray( imposterPointArray, real([1., 2., 3., 4., &
         5., 6., 7., 8.], FLOAT), log )
    if ( log%test(FATAL) ) return
    
    call assertEqual( 0, pp2%match( imposterPointArray ), "Should &
         &not have matched imposterPointArray." )

    call destroy( imposterPointArray, log )
  end subroutine testBadMatchWithPolytopePointer


  subroutine testGoodQueryWithPolytopePointerSet
    integer, dimension(:), allocatable :: indices
    integer :: deallocStat
    if ( log%test(MOD_NAME, 'testGoodQueryWithPolytopePointerSet', &
         FATAL) ) return
    
    call pps%query( pointArray, indices, log )
    if ( log%test(FATAL) ) return
    
    ! keep in mind that the order of the indices is arbitrary, so take
    ! care to match the original spec of pps in the expected indices
    call assertEqual( [2, 1, 3], indices, "Should have matched." )
    
    deallocate( indices, stat=deallocStat )
    call log%addEvent( deallocStat/=0, WARNING, 'Problem &
         &deallocating indices.  STAT='//int2str(deallocStat) )
  end subroutine testGoodQueryWithPolytopePointerSet
  

  subroutine testBadQueryWithPolytopePointerSet
    class(PolytopeArrayInterface), allocatable :: imposterPointArray
    integer, dimension(:), allocatable :: indices
    integer, dimension(0) :: empty
    if ( log%test(MOD_NAME, 'testBadQueryWithPolytopePointerSet', &
         FATAL) ) return
    
    call createPointArray( imposterPointArray, real([1., 2., 3., 4., &
         5., 6., 7., 8.], FLOAT), log )
    if ( log%test(FATAL) ) return
    
    call pps%query( imposterPointArray, indices, log )
    if ( log%test(FATAL) ) return
    
    call assertEqual( empty, indices, "Should not have &
         &matched, so empty array expected." )
    
    call destroy( imposterPointArray, log )
  end subroutine testBadQueryWithPolytopePointerSet

  

end module PointArrayAndSetTests
