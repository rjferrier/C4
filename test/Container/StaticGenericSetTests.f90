
module StaticGenericSetTests
  
  use TestUtilities
  use Global
  use StaticGenericModule
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'StaticGenericSetTests'

  type(UniqueGenericType) :: genA, genB, genC
  
  type(TestLogType) :: log
  
contains
  

  subroutine setUp
    call log%init( MOD_NAME, 'setup' )

    ! initialise generics
    call genA%init( 'A', log )
    call genB%init( 'B', log )
    call genC%init( 'C', log )
    if ( log%test(FATAL) ) return
  end subroutine setUp
  
  
  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )

    call genA%deinit( log )
    call genB%deinit( log )
    call genC%deinit( log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testGoodMatch
    type(UniqueGenericSetType) :: set
    if ( log%test(MOD_NAME, 'testAB', FATAL) ) &
         return

    call set%append( genC, log )
    call set%append( genA, log )
    call set%append( genB, log )

    call assertEqual( 1, set%match(genC), 'Match should have passed.' )
    call assertEqual( 3, set%match(genB), 'Match should have passed.' )

    call set%deinit( log )
  end subroutine testGoodMatch

  
  subroutine testBadMatch
    type(UniqueGenericSetType) :: set
    if ( log%test(MOD_NAME, 'testAB', FATAL) ) &
         return

    call set%append( genC, log )
    call set%append( genA, log )

    call assertEqual( 0, set%match(genB), 'Match should have failed.' )

    call set%deinit( log )
  end subroutine testBadMatch


  subroutine testGoodSameAs
    type(UniqueGenericSetType) :: set1, set2
    if ( log%test(MOD_NAME, 'testGoodSameAs', FATAL) ) &
         return

    call set1%append( genC, log )
    call set1%append( genA, log )
    call set1%append( genB, log )

    call set2%append( genA, log )
    call set2%append( genB, log )
    call set2%append( genC, log )

    call assertTrue( set1%sameAs(set2, log), 'set2 should test the &
         &same as set1.' )

    if ( .not. set1%sameAs(set2, log) ) call set2%diagnostics( '2', log )

    call set1%deinit( log )
    call set2%deinit( log )
  end subroutine testGoodSameAs


  subroutine testBadSameAs
    type(UniqueGenericSetType) :: set1, set2
    if ( log%test(MOD_NAME, 'testBadSameAs', FATAL) ) &
         return

    call set1%append( genA, log )
    call set1%append( genB, log )

    call set2%append( genA, log )
    call set2%append( genB, log )
    call set2%append( genC, log )

    call assertFalse( set1%sameAs(set2, log), 'set2 should test &
         &different to set1.' )

    if ( set1%sameAs(set2, log) ) call set2%diagnostics( '2', log )

    call set1%deinit( log )
    call set2%deinit( log )
  end subroutine testBadSameAs


  subroutine testAppendWithoutDuplicates
    type(UniqueGenericSetType) :: set1, set2
    type(UniqueGenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testAppendWithoutDuplicates', &
         FATAL) ) return
    
    call set1%append( genA, log )
    call set1%append( genB, log )

    call set2%append( genB, log )
    call set2%append( genA, log )
    call set2%append( genB, log )
    call set2%append( genB, log )
    call set2%append( genA, log )
    call set2%append( genA, log )

    call assertEqual( 2, set2%size(), 'set2 has wrong number of &
         &elements.' )
    call assertTrue( set1%sameAs(set2, log), 'set2 should test the &
         &same as set1.' )

    if ( .not. set1%sameAs(set2, log) ) call set2%diagnostics( '2', log )

    call set1%deinit( log )
    call set2%deinit( log )
  end subroutine testAppendWithoutDuplicates

  
  
end module StaticGenericSetTests
 
