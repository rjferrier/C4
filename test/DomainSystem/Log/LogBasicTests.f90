module LogBasicTests

  use LogModule
  use pFUnit
  use Echo
  
  implicit none
  
  type(LogType) :: log

  character(*), parameter :: MOD_NAME = 'LogBasicTests'
  
contains

  subroutine setUp
  end subroutine setUp

  subroutine tearDown
    call log%report()
    call log%deinit()
  end subroutine tearDown
  

  subroutine testSingleEvent
    call log%init( MOD_NAME, 'testSingleEvent' )
    
    call log%addEvent( .true., ADVICE, 'Test event with severity '//&
         trim(describeSeverity(ADVICE)) )
    call assertEqual( ADVICE, log%getSeverity(), 'Log &
         &severity should be  '//trim(describeSeverity(ADVICE)) )
  end subroutine testSingleEvent
    

  subroutine testMultiEvent
    call log%init( MOD_NAME, 'testMultiEvent' )
    call log%addEvent( .true., ADVICE, 'Test event with severity '//&
         trim(describeSeverity(ADVICE)) )
    call log%addEvent( .true., WARNING, 'Test event with &
         &severity  '//trim(describeSeverity(WARNING)) )
    call assertEqual( WARNING, log%getSeverity(), 'Log &
         &severity should be  '//trim(describeSeverity(WARNING)) )
  end subroutine testMultiEvent
  
  
  subroutine testDeepTree
    call log%init( MOD_NAME, 'testDeepTree' )
    call add1Level( log )
    call add2Levels( log )
    call assertEqual( FATAL, log%getSeverity(), 'Log &
         &severity should be '//trim(describeSeverity(FATAL)) )
    call addEvent( WARNING, 'Test event with severity  '//trim(&
         describeSeverity(WARNING)), log )
  end subroutine testDeepTree

  
  subroutine testTriggeredEvent
    call log%init( MOD_NAME, 'testTriggeredEvent' )
    call log%addEvent( .true., ADVICE, 'Test event with severity '//&
         trim(describeSeverity(ADVICE)) )
    call add1Level3( log, .true. )
    call assertEqual( FATAL, log%getSeverity(), 'Log &
         &severity should be '//trim(describeSeverity(FATAL)) )
  end subroutine testTriggeredEvent

  
  subroutine testUntriggeredEvent
    call log%init( MOD_NAME, 'testUntriggeredEvent' )
    call log%addEvent( .true., ADVICE, 'Test event with severity '//&
         trim(describeSeverity(ADVICE)) )
    call add1Level3( log, .false. )
    call assertEqual( ADVICE, log%getSeverity(), 'Log &
         &severity should be '//trim(describeSeverity(ADVICE)) )
  end subroutine testUntriggeredEvent


  
  subroutine testNumberOfLeavesInDeepTree
    call log%init( MOD_NAME, 'testNumberOfLeavesInDeepTree' )
    call add1Level( log )
    call add2Levels( log )
    call assertEqual( 4, log%numberOfLeaves(), 'Number of leaves &
         &is wrong.' )
  end subroutine testNumberOfLeavesInDeepTree
  

  subroutine testNumberOfLeavesInFlatTree
    call log%init( MOD_NAME, 'testNumberOfLeavesInFlatTree' )
    call add1Level( log )
    call log%addEvent( .true., ADVICE, 'Test event with severity '//&
         trim(describeSeverity(WARNING)) )
    call add1Level2( log )
    call assertEqual( 3, log%numberOfLeaves(), 'Number of leaves &
         &is wrong.' )
  end subroutine testNumberOfLeavesInFlatTree


  
  !-------------------------------------------------------------------
  !- helper subroutines
  !-------------------------------------------------------------------

  subroutine add1Level( log )
    class(LogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'add1Level', log )
    
    call addEvent( ADVICE, 'Test event with severity '//trim(&
         describeSeverity(ADVICE)), log )
    
    call endSub( log )
  end subroutine add1Level
  
    
  subroutine add2Levels( log )
    class(LogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'add2Levels', log )
    
    call addEvent( WARNING, 'Test event with severity  '//trim(&
         describeSeverity(WARNING)), log )
    call add1Level2( log )
    call addEvent( FATAL, 'Test event with severity '//trim(&
         describeSeverity(FATAL)), log )
    
    call endSub( log )
  end subroutine add2Levels

  
  subroutine add1Level2( log )
    class(LogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'add1Level2', log )
    
    call addEvent( ADVICE, 'Test event with severity '//trim(&
         describeSeverity(ADVICE)), log )
    
    call endSub( log )
  end subroutine add1Level2

  
  subroutine add1Level3( log, trigger )
    class(LogType), intent(inout) :: log
    logical, intent(in) :: trigger
    call beginSub( MOD_NAME, 'add1Level3', log ) 
    
    call addEvent( trigger, FATAL, 'Test event with severity '//trim(&
         describeSeverity(FATAL)), log )
    
    call endSub( log )
  end subroutine add1Level3

  
  !-------------------------------------------------------------------
  !- unbound and common procedures
  !-------------------------------------------------------------------

  function describeSeverity( level )
    integer, intent(in) :: level
    character(80) :: describeSeverity
    describeSeverity = int2str(level)//' ('//trim(WARNINGS(&
         level+1))//').'
  end function describeSeverity

  pure function nDigits( x )
    integer, intent(in) :: x
    integer :: nDigits
    nDigits = floor(log10(max(real(x), 1.)) + 1)
  end function nDigits

  
  pure function int2str( x ) result( str )
  integer, intent(in) :: x
    character( len=nDigits(x) ) :: str
    integer :: n, i, j, y, y0
    y0 = ichar('0')
    n = nDigits(x)
    do i = 1, n
       y = x/10**(i-1)    
       y = modulo(y, 10)  
       j = n + 1 - i      
       str(j:j) = char(y0 + y)
    end do
  end function int2str
    
    
end module LogBasicTests
  
