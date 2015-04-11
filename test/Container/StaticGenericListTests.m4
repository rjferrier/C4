module StaticGenericListTests
  
  use TestUtilities
  use Global
  use StaticGenericModule
  
  implicit none
  
  character(*), parameter :: MOD_NAME = 'StaticGenericListTests'

  type(GenericType) :: genA, genB, genC
  type(TestLogType) :: log
  
  ! systems under test
  type(GenericListType) :: list
  type(GenericListIteratorType) :: iterator
  
contains

  subroutine setUp
    type(GenericType) :: gen
    call log%init( MOD_NAME, 'setup' )
    
    ! create a list with nodes AAB
    call gen%init( 'A', log )
    call list%append( gen, log )
    if ( log%test(FATAL) ) return
    call list%append( gen, log )
    if ( log%test(FATAL) ) return
    
    call gen%init( 'B', log )
    call list%append( gen, log )
    if ( log%test(FATAL) ) return
        
    call iterator%init( list )
  end subroutine setUp
  
  
  subroutine tearDown
    call log%reinit( MOD_NAME, 'teardown' )

    call list%deinit( log )
    
    call log%report()
    call log%deinit()
  end subroutine tearDown


  subroutine testListSize
    if ( log%test(MOD_NAME, 'testListSize', FATAL) ) &
         return

    call assertEqual( 3, list%size(), "Failed to give correct number &
         &of elements" )
  end subroutine testListSize


  subroutine testDeinit
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testDeinit', FATAL) ) &
         return

    call list%deinit( log )
    call assertEqual( 0, list%size(), "Deinitialised list should &
         &have no elements" )
    
    call iterator%first( ptr )
    call assertFalse( associated(ptr), "Expected first element to be &
         &nonexistent." )
    
    call iterator%last( ptr )
    call assertFalse( associated(ptr), "Expected last element to be &
         &nonexistent." )
  end subroutine testDeinit


  subroutine testSequentialAccess
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testSequentialAccess', FATAL) ) &
         return

    call iterator%first( ptr )
    call assertEqual( "A", ptr%describe(), "Expected first element to &
         &be type A." )
    
    call iterator%next( ptr )
    call assertEqual( "A", ptr%describe(), "Expected next (2nd) element to &
         &be type A." )
    
    call iterator%next( ptr )
    call assertEqual( "B", ptr%describe(), "Expected next (3rd) element to &
         &be type B." )
    
    call iterator%next( ptr )
    call assertFalse( associated(ptr), "Expected next element to be &
         &nonexistent." )
  end subroutine testSequentialAccess

  
  subroutine testReverseSequentialAccess
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testReverseSequentialAccess', FATAL) ) &
         return

    call iterator%last( ptr )
    call assertEqual( "B", ptr%describe(), "Expected last (1st) element to &
         &be type B." )
    
    call iterator%prev( ptr )
    call assertEqual( "A", ptr%describe(), "Expected previous (2nd) &
         &element to be type A." )
    
    call iterator%prev( ptr )
    call assertEqual( "A", ptr%describe(), "Expected previous (1st) &
         &element to be type A." )

    call iterator%prev( ptr )
    call assertFalse( associated(ptr), "Expected previous element to be &
         &nonexistent." )
  end subroutine testReverseSequentialAccess

  
  subroutine testRandomAccess
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testRandomAccess', FATAL) ) return
    
    call list%find( ptr, 1 )
    call assertEqual( "A", ptr%describe(), "Expected first element to &
         &be type A." )
    
    call list%find( ptr, 2 )
    call assertEqual( "A", ptr%describe(), "Expected second element to &
         &be type A." )
    
    call list%find( ptr, 4, log )
    call assertEqual( WARNING, log%getSeverity(), "Expected a &
         &warning in the log for exceeding the list length." )
    call assertFalse( associated(ptr), "Expected 4th element to be &
         &nonexistent." )
    
  end subroutine testRandomAccess

  
  subroutine testInsertBeforeFirst
    type(GenericType) :: gen
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testInsertBeforeFirst', FATAL) ) return

    call iterator%first()
    call gen%init( "B", log )
    call iterator%insert( gen, log )

    call iterator%current( ptr )
    call assertEqual( "B", ptr%describe(), "Expected current element to &
         &be the newly inserted type B." )

    call checkFourElementList( "B", "A", "A", "B", log )
  end subroutine testInsertBeforeFirst

  
  subroutine testInsertMid
    type(GenericType) :: gen
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testInsertMid', FATAL) ) return

    call iterator%first()
    call iterator%next()
    call gen%init( "B", log )
    call iterator%insert( gen, log )

    call iterator%current( ptr )
    call assertEqual( "B", ptr%describe(), "Expected current element to &
         &be the newly inserted type B." )

    call checkFourElementList( "A", "B", "A", "B", log )
  end subroutine testInsertMid


  subroutine testInsertBeforeLast
    type(GenericType) :: gen
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testInsertBeforeFirst', FATAL) ) return

    call iterator%last()
    call gen%init( 'A', log )
    call iterator%insert( gen, log )

    call iterator%current( ptr )
    call assertEqual( "A", ptr%describe(), "Expected current element to &
         &be the newly inserted type A." )

    call checkFourElementList( "A", "A", "A", "B", log )
  end subroutine testInsertBeforeLast

  
  subroutine testExtractFirst
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testExtractFirst', FATAL) ) return

    call iterator%first()
    call iterator%extract( log )
    
    call iterator%current( ptr )
    call assertEqual( "A", ptr%describe(), "Expected current element &
         &after extraction to be type A." )
    
    call checkTwoElementList( "A", "B" )
  end subroutine testExtractFirst


  subroutine testExtractMid
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testExtractMid', FATAL) ) return

    call iterator%first()
    call iterator%next()
    call iterator%extract( log )

    call iterator%current( ptr )
    call assertEqual( "B", ptr%describe(), "Expected current element &
         &after extraction to be type B." )
    
    call checkTwoElementList( "A", "B", log )
  end subroutine testExtractMid
  

  subroutine testExtractLast
    type(GenericType), pointer :: ptr
    if ( log%test(MOD_NAME, 'testExtractLast', FATAL) ) return

    call iterator%last()
    call iterator%extract( log )
    
    call iterator%current( ptr )
    call assertFalse( associated(ptr), "Expected current element to &
         &now be nonexistent." )
    
    call checkTwoElementList( "A", "A", log )
  end subroutine testExtractLast

  
  subroutine testTakeNodes
    type(GenericType) :: gen
    type(GenericListType) :: newList
    type(GenericType), pointer :: ptr
    integer :: i
    if ( log%test(MOD_NAME, 'testTakeNodes', FATAL) ) return
    
    ! create a new list with nodes AB
    call gen%init( 'A', log )
    call newList%append( gen, log )
    if ( log%test(FATAL) ) return
    call gen%init( 'B', log )
    call newList%append( gen, log )
    if ( log%test(FATAL) ) return

    ! add the contents of the old list
    call newList%takeNodes( list, log )
    if ( log%test(FATAL) ) return

    call assertEqual( 5, newList%size(), "Should now be 2+3 nodes" )

    call newList%find( ptr, 2 )
    call assertEqual( "B", ptr%describe(), "Expected newList's second &
         &element to still be type B." )
    
    call newList%find( ptr, 3 )
    call assertTrue( associated(ptr), "Expected newList's third node to &
         &exist" )
    if ( associated(ptr) ) then
       call assertEqual( "A", ptr%describe(), "Expected newList's third &
            &element to be type A." )
    end if

    ! move the nodes back again for deinitialisation
    call list%takeNodes( newList, log )
  end subroutine testTakeNodes

  
  !-------------------------------------------------------------------
  !- helpers
  !-------------------------------------------------------------------
  
  subroutine checkFourElementList( str1, str2, str3, str4, log )
    character(1), intent(in) :: str1, str2, str3, str4
    class(LogType), intent(inout), optional :: log
    type(GenericType), pointer :: ptr
    call beginSub( MOD_NAME, 'checkFourElementList', log )
    
    call iterator%first( ptr )
    call addEvent( .not. associated(ptr), FATAL, 'ptr to first &
         &element is dissociated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call assertEqual( str1, ptr%describe(), "Expected first element &
         &of modified list to be type "//str1//"." )
    
    call iterator%next( ptr )
    call addEvent( .not. associated(ptr), FATAL, 'ptr to second &
         &element is dissociated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call assertEqual( str2, ptr%describe(), "Expected second element &
         &of modified list to be type "//str2//"." )

    call iterator%last( ptr )
    call addEvent( .not. associated(ptr), FATAL, 'ptr to last &
         &element is dissociated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call assertEqual( str4, ptr%describe(), "Expected last element &
         &of modified list to be type "//str4//"." )
    
    call iterator%prev( ptr )
    call addEvent( .not. associated(ptr), FATAL, 'ptr to penultimate &
         &element is dissociated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call assertEqual( str3, ptr%describe(), "Expected penultimate &
         &element of modified list to be type "//str3//"." )

    call endSub( log )
  end subroutine checkFourElementList

  
  subroutine checkTwoElementList( str1, str2, log )
    character(1), intent(in) :: str1, str2
    class(LogType), intent(inout), optional :: log
    type(GenericType), pointer :: ptr
    call beginSub( MOD_NAME, 'checkTwoElementList', log )
    
    call iterator%first( ptr )
    call addEvent( .not. associated(ptr), FATAL, 'ptr to first &
         &element is dissociated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call assertEqual( str1, ptr%describe(), "Expected first element &
         &of modified list to be type "//str1//"." )
    
    call iterator%next( ptr )
    call addEvent( .not. associated(ptr), FATAL, 'ptr to second &
         &element is dissociated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call assertEqual( str2, ptr%describe(), "Expected second element &
         &of modified list to be type "//str2//" (i.e. the last &
         &element)." )

    call iterator%last( ptr )
    call addEvent( .not. associated(ptr), FATAL, 'ptr to last &
         &element is dissociated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call assertEqual( str2, ptr%describe(), "Expected last element &
         &of modified list to be type "//str2//"." )
    
    call iterator%prev( ptr )
    call addEvent( .not. associated(ptr), FATAL, 'ptr to penultimate &
         &element is dissociated.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call assertEqual( str1, ptr%describe(), "Expected penultimate &
         &element of modified list to be type "//str1//" (i.e. the &
         &first element)." )
    
    call endSub( log )
  end subroutine checkTwoElementList
    
  
end module StaticGenericListTests
