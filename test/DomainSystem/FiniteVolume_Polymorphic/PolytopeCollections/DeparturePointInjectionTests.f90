module DeparturePointInjectionTests

  use TestUtilities
  use Global
  use FlowFieldModule
  use FiniteVolumeModule
  use SemiLagrangianModule
  use SemiLagrangian_Stubs

  use PolytopeCollectionsModule

  implicit none
  
  character(*), parameter, private :: MOD_NAME = &
       'DeparturePointInjectionTests'
  
  class(PolytopeArrayInterface), allocatable :: pointArray
  class(DeparturePointCollectionInterface), allocatable :: departurePoints
  
  type(TestLogType) :: log
  

contains
  
  subroutine setUp
    integer :: allocStat
    call log%init( MOD_NAME, 'setup' )

    call createPointArray( pointArray, [0._FLOAT, 0._FLOAT], log )

    ! to do: create DeparturePointCollectionStubs in FlowField_Stubs
    call createDeparturePointCollectionStub( departurePoints, log )
    if ( log%test(FATAL) ) return
  end subroutine setUp


  subroutine tearDown
    integer :: deallocStat
    call log%reinit( MOD_NAME, 'teardown' )
    
    call destroy( pointArray, log )
    call destroy( departurePoints, log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testGoodInjectAndExtractDeparturePoints
    if ( log%test(MOD_NAME, 'testGoodInjectAndExtractDeparturePoints', &
         FATAL) ) return
    
    call pointArray%injectDeparturePoints( departurePoints, log )
    if ( log%test(FATAL) ) return

    call pointArray%extractDeparturePoints( departurePoints, log )
    if ( log%test(FATAL) ) return
  end subroutine testGoodInjectAndExtractDeparturePoints


  subroutine testInjectEmptyDeparturePoints
    class(DeparturePointCollectionInterface), allocatable :: &
         departurePoints2
    integer :: allocStat
    if ( log%test(MOD_NAME, 'testInjectEmptyDeparturePoints', FATAL) ) &
         return
    
    call pointArray%injectDeparturePoints( departurePoints2, log )
    call assertEqual( WARNING, log%getSeverity(), 'Trying to inject &
         &an empty object should trigger a warning.' )
  end subroutine testInjectEmptyDeparturePoints


  subroutine testExtractEmptyDeparturePoints
    class(DeparturePointCollectionInterface), allocatable :: &
         departurePoints2
    integer :: allocStat
    if ( log%test(MOD_NAME, 'testExtractEmptyDeparturePoints', FATAL) ) &
         return
    
    call pointArray%extractDeparturePoints( departurePoints2, log )
    call assertEqual( WARNING, log%getSeverity(), 'Trying to extract &
         &an empty component should trigger a warning.' )
  end subroutine testExtractEmptyDeparturePoints



  subroutine testInjectTwiceDeparturePoints
    class(DeparturePointCollectionInterface), allocatable :: &
         departurePoints2
    integer :: allocStat
    if ( log%test(MOD_NAME, 'testInjectTwiceDeparturePoints', FATAL) ) &
         return
    
    call createDeparturePointCollectionStub( departurePoints2, log )
    if ( log%test(FATAL) ) return
    
    call pointArray%injectDeparturePoints( departurePoints, log )
    if ( log%test(FATAL) ) return
    
    call pointArray%injectDeparturePoints( departurePoints2, log )
    call assertEqual( FATAL, log%getSeverity(), 'Trying to inject &
         &twice should trigger a fatal error.' )
    call log%reinit( MOD_NAME, 'testInjectTwiceDeparturePoints' )

    call pointArray%extractDeparturePoints( departurePoints, log )
    if ( log%test(FATAL) ) return

    call destroy( departurePoints2, log )
    if ( log%test(FATAL) ) return
  end subroutine testInjectTwiceDeparturePoints

  
  subroutine testExtractToFullDeparturePoints
    class(DeparturePointCollectionInterface), allocatable :: &
         departurePoints2
    integer :: allocStat
    if ( log%test(MOD_NAME, 'testExtractToFullDeparturePoints', FATAL) ) &
         return
    
    call createDeparturePointCollectionStub( departurePoints2, log )
    if ( log%test(FATAL) ) return
    
    call pointArray%injectDeparturePoints( departurePoints, log )
    if ( log%test(FATAL) ) return

    call pointArray%extractDeparturePoints( departurePoints2, log )
    call assertEqual( FATAL, log%getSeverity(), 'Trying to extract &
         &to an already-allocated object should trigger a fatal &
         &error.' )
    call log%reinit( MOD_NAME, 'testExtractToFullDeparturePoints' )

    call pointArray%extractDeparturePoints( departurePoints, log )
    if ( log%test(FATAL) ) return
    
    call destroy( departurePoints2, log )
    if ( log%test(FATAL) ) return
  end subroutine testExtractToFullDeparturePoints


end module DeparturePointInjectionTests
