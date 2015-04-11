module LogModule
  use Echo
  
  implicit none

  private
  public :: beginSub, endSub, addEvent, checkSub, checkInjection, &
       checkExtraction

  
  interface addEvent
     module procedure addEventViaTriggerAndSpec
     module procedure addEventViaSpec
  end interface

  
  type, abstract :: LogInterface
     private
     integer :: severity = 0
     logical :: errorsExist = .false.
     class(LogInterface), pointer :: nextSibling => null()     
   contains
     procedure :: getSeverity
     procedure :: check
     procedure(l_p), deferred :: print
     procedure(l_d), deferred :: deinit
     procedure(l_pal), deferred :: pushActiveLog
  end type LogInterface
  

  type  :: LogStackType
     class(LogInterface), pointer :: log => null()
     type(LogStackType), pointer :: previous => null()
   contains
     procedure :: propagateError 
  end type LogStackType
  
  
  type, extends(LogInterface), public :: LogType
     private
     character(60) :: moduleName
     character(60) :: procedureName
     class(LogInterface), pointer :: firstChild => null(), &
          lastChild => null()
     
     type(LogStackType), pointer, public  :: activeLogStack => null()
   contains
     procedure, private :: initLogViaSource
     procedure, private :: initLogViaSpec
     procedure :: init => initLogViaMinimalSpec
     procedure, private :: allocNextChildAsLog
     procedure, private :: allocNextChildAsEvent
     procedure :: addLog => addLogViaSpec_Log
     procedure  :: addEventViaSpec_Log
     procedure  :: addEventViaTriggerAndSpec_Log
     generic  :: addEvent => addEventViaSpec_Log, &
          addEventViaTriggerAndSpec_Log
     procedure  :: add => addViaSource_Log
     procedure :: print => printLog
     procedure :: deinit => deinitLog
     procedure :: report
     procedure :: isEmpty
     procedure :: numberOfLeaves
     
     ! methods for internal sub-log
     procedure :: beginSub => beginSub_Log
     procedure :: endSub => endSub_Log
     procedure :: checkSub => checkSub_Log
     procedure :: addEventToActiveViaSpec
     procedure :: addEventToActiveViaTriggerAndSpec
     generic :: addEventToActive => addEventToActiveViaSpec, &
          addEventToActiveViaTriggerAndSpec
     
     procedure, private :: pushActiveLog => pushActiveLog_Log
     procedure, private :: popActiveLog => popActiveLog_Log
  end type LogType
  
  
  type, extends(LogInterface) :: EventType
     private
     character(300) :: message = ' '
     character(80), dimension(:), allocatable :: messageExtension
   contains
     procedure :: initEventViaSource
     procedure :: initEventViaSpec     
     procedure :: print => printEvent
     procedure :: deinit => deinitEvent
     procedure, private :: pushActiveLog => pushActiveLog_Event
  end type EventType
  


  integer, parameter :: TAB_WIDTH = 3

  character(14), dimension(3), parameter, public :: WARNINGS = &
       (/ 'Advice        ', 'Warning(s)    ', 'Fatal error(s)' /)
  
  ! enumerations
  integer, parameter, public :: ADVICE = 0, WARNING = 1, FATAL = 2

  ! print empty nodes (for e.g. debugging)
  logical, parameter :: PRINT_ALL = .false.

  
  abstract interface
     subroutine l_p( obj, level )
       import LogInterface
       class(LogInterface), intent(in) :: obj
       integer, intent(in) :: level
     end subroutine l_p

     pure subroutine l_d( obj )
       import LogInterface
       class(LogInterface), intent(inout) :: obj
     end subroutine l_d

     pure subroutine l_pal( obj, activeLog )
       import LogInterface, LogStackType
       class(LogInterface), intent(inout) :: obj
       class(LogInterface), target, intent(inout) :: activeLog
       class(LogStackType), pointer :: ptr
       integer :: allocStat
     end subroutine l_pal
  end interface
  
  
contains


  !-------------------------------------------------------------------
  !- unbound and common procedures
  !-------------------------------------------------------------------

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


  integer function getSeverity( obj )
    class(LogInterface), intent(in) :: obj
    getSeverity = obj%severity
  end function getSeverity


  pure function check( obj, severity )
    class(LogInterface), intent(in) :: obj
    integer, intent(in) :: severity
    logical :: check
    check = ( obj%getSeverity() >= severity )
  end function check


  !-------------------------------------------------------------------
  !- flexible public methods
  !-------------------------------------------------------------------

  ! in the following methods, the 'log' argument is left optional so
  ! that clients can pass in the object without having to check its
  ! existence first.

  pure subroutine beginSub( moduleName, procedureName, log )
    character(*), intent(in) :: moduleName, procedureName
    class(LogType), intent(inout), optional :: log
    if ( present(log) ) call log%beginSub( moduleName, procedureName )
  end subroutine beginSub

  
  pure subroutine endSub( log )
    class(LogType), intent(inout), optional :: log
    if ( present(log) ) call log%endSub()
  end subroutine endSub

  
  pure function checkSub( severity, log )
    integer, intent(in), optional :: severity
    class(LogType), intent(in), optional :: log
    logical :: checkSub
    if ( present(log) ) then
       checkSub = log%checkSub( severity )
    else
       checkSub = .false. 
    end if
  end function checkSub

  
  pure subroutine addEventViaTriggerAndSpec( trigger, severity, &
       message, log )
    logical, intent(in) :: trigger
    integer, intent(in) :: severity
    character(*), intent(in) :: message
    class(LogType), intent(inout), optional :: log
    if ( present(log) ) call log%addEventToActive( trigger, &
         severity, message )
  end subroutine addEventViaTriggerAndSpec

  
  pure subroutine addEventViaSpec( severity, message, log )
    integer, intent(in) :: severity
    character(*), intent(in) :: message
    class(LogType), intent(inout), optional :: log
    if ( present(log) ) call log%addEventToActive( .true., severity, &
         message )
  end subroutine addEventViaSpec

  
  pure subroutine checkInjection( argumentIsAllocated, &
       componentIsAllocated, log )
    logical, intent(in) :: argumentIsAllocated, componentIsAllocated
    class(LogType), intent(inout), optional :: log
    call addEvent( .not. argumentIsAllocated, WARNING, 'Argument to be &
         &injected has not been allocated.', log )
    call addEvent( componentIsAllocated, FATAL, 'Component to &
         &receive injection is already allocated.', log )
  end subroutine checkInjection

  
  pure subroutine checkExtraction( componentIsAllocated, &
       argumentIsAllocated, log )
    logical, intent(in) :: componentIsAllocated, argumentIsAllocated
    class(LogType), intent(inout), optional :: log
    call addEvent( .not. componentIsAllocated, WARNING, &
         'Component to be extracted has not been allocated.', log )
    call addEvent( argumentIsAllocated, FATAL, 'Argument to &
         &receive extraction is already allocated.', log )
  end subroutine checkExtraction

  
   
  !-------------------------------------------------------------------
  !- LogType methods
  !-------------------------------------------------------------------
  
  pure recursive subroutine initLogViaSource( obj, src )
    class(LogType), intent(out) :: obj
    class(LogType), intent(inout) :: src
    call obj%initLogViaSpec( src%severity, src%moduleName, &
         src%procedureName, src%firstChild )
  end subroutine initLogViaSource

    
  pure recursive subroutine initLogViaSpec( obj, &
       severity, moduleName, procedureName, firstChild )
    class(LogType), intent(out), target :: obj
    integer, intent(in) :: severity
    character(*), intent(in) :: moduleName, procedureName
    class(LogInterface), pointer, intent(inout), optional :: &
         firstChild
    class(LogInterface), pointer :: ptr
    obj%severity = severity
    obj%moduleName = moduleName
    obj%procedureName = procedureName
    nullify(obj%firstChild, obj%lastChild, obj%nextSibling)
    
    if ( present(firstChild) ) then
       if ( .not. associated(firstChild) ) return
       ptr => firstChild
       do
          call obj%add( ptr )
          if ( .not. associated(ptr%nextSibling) ) exit
          ptr => ptr%nextSibling
       end do
    end if
  end subroutine initLogViaSpec

  
  pure subroutine initLogViaMinimalSpec( obj, moduleName, &
       procedureName )
    class(LogType), intent(inout) :: obj
    character(*), intent(in) :: moduleName, procedureName
    call obj%initLogViaSpec( 0, moduleName, procedureName )
  end subroutine initLogViaMinimalSpec

  
  pure subroutine initLogViaTriggerAndMinimalSpec( obj, &
       trigger, moduleName, procedureName )
    class(LogType), intent(inout) :: obj
    logical, intent(in) :: trigger
    character(*), intent(in) :: moduleName, procedureName
    if ( trigger ) call obj%initLogViaSpec( 0, moduleName, &
         procedureName )
  end subroutine initLogViaTriggerAndMinimalSpec

    
  pure subroutine allocNextChildAsLog( obj )
    class(LogType), intent(inout) :: obj
    if ( .not. associated(obj%firstChild) ) then
       allocate( LogType :: obj%firstChild )
       obj%lastChild => obj%firstChild
    else
       ! ! test
       ! if ( .not. associated(obj%lastChild) ) return
        allocate( LogType :: obj%lastChild%nextSibling )
       obj%lastChild => obj%lastChild%nextSibling
    end if
  end subroutine allocNextChildAsLog
    
  
  pure subroutine allocNextChildAsEvent( obj )
    class(LogType), intent(inout) :: obj
    if ( .not. associated(obj%firstChild) ) then
       allocate( EventType :: obj%firstChild )
       obj%lastChild => obj%firstChild
    else
       allocate( EventType :: obj%lastChild%nextSibling )
       obj%lastChild => obj%lastChild%nextSibling
    end if
  end subroutine allocNextChildAsEvent

  
  pure recursive subroutine addLogViaSpec_Log( obj, &
       severity, moduleName, procedureName, firstChild, head )
    class(LogType), intent(inout), target :: obj
    integer, intent(in) :: severity
    character(*), intent(in) :: moduleName, procedureName
    class(LogInterface), pointer, intent(inout), optional :: &
         firstChild
    class(LogInterface), intent(inout), optional :: head
    
    call obj%allocNextChildAsLog()
    select type ( child => obj%lastChild )
       class is (LogType)
       call child%initLogViaSpec( severity, moduleName, &
            procedureName, firstChild )
       ! update the overall severity and the existence of any leaves
       obj%severity = max( obj%severity, child%severity )
       obj%errorsExist = obj%errorsExist .or. child%errorsExist
    end select
    
    ! important: record the new addition as the active branch
    ! n.b. This did not work when the activeLog%log was LogType
    ! and placed within the above block.  That is why we have made
    ! activeLog%log and pushActiveLog polymorphic.
    if ( present(head) ) then
       call head%pushActiveLog( obj%lastChild )
    end if
  end subroutine addLogViaSpec_Log
  
  
  pure subroutine addEventViaSpec_Log( obj, severity, message )
    class(LogType), intent(inout) :: obj
    integer, intent(in) :: severity
    character(*), intent(in) :: message
    call obj%allocNextChildAsEvent()
    select type ( child => obj%lastChild )
    class is (EventType)
       call child%initEventViaSpec( severity, message )
       ! update the overall severity and the existence of this event
       obj%severity = max( obj%severity, child%severity )
       obj%errorsExist = .true.
    end select
  end subroutine addEventViaSpec_Log

  
  pure subroutine addEventViaTriggerAndSpec_Log( obj, trigger, &
       severity, message )
    class(LogType), intent(inout) :: obj
    logical, intent(in) :: trigger
    integer, intent(in) :: severity
    character(*), intent(in) :: message
    if ( trigger ) call obj%addEventViaSpec_Log( severity, message )
  end subroutine addEventViaTriggerAndSpec_Log

  
  ! note: it is assumed that after adding a generic log to a
  ! concrete one, the former is no longer needed.  Accordingly it
  ! is deinitialised here.  This saves the user from having to clean
  ! up after the call.
  pure recursive subroutine addViaSource_Log( obj, src )
    class(LogType), intent(inout) :: obj
    class(LogInterface), intent(inout) :: src
    select type (src)       
    class is (LogType)
       call obj%addLog( src%severity, src%moduleName, &
            src%procedureName, src%firstChild )
       
    class is (EventType)
       call obj%addEventViaSpec_Log( src%severity, src%message )
    end select
    ! clean up
    call src%deinit()
  end subroutine addViaSource_Log

  
  recursive subroutine printLog( obj, level )
    class(LogType), intent(in) :: obj
    integer, intent(in) :: level
    class(LogInterface), pointer :: ptr
    if ( obj%errorsExist .or. PRINT_ALL ) then
       ! print header-style material at this level before descending
       write( echoUnit(), '(t'//int2str(level*TAB_WIDTH+1)//', &
            &4a)' ) 'In ', trim(obj%moduleName), '::', trim(&
            obj%procedureName)
       
       if ( .not. associated(obj%firstChild) ) return
       ptr => obj%firstChild
       do
          call ptr%print( level+1 )
          if ( .not. associated(ptr%nextSibling) ) exit
          ptr => ptr%nextSibling
       end do
    end if
  end subroutine printLog

  
  pure recursive subroutine deinitLog( obj )
    class(LogType), intent(inout) :: obj
    class(LogInterface), pointer :: ptr
        
    if ( .not. associated(obj%firstChild) ) return
    ptr => obj%firstChild
    ! the following block will obliterate all children
    do
       ! point to the next sibling before destroying the first
       if ( .not. associated(ptr%nextSibling) ) exit
       ptr => ptr%nextSibling
       call obj%firstChild%deinit()
       deallocate( obj%firstChild )
       ! update first child 
       obj%firstChild => ptr
    end do
    ! when the first/last child has no more siblings, destroy it
    call obj%lastChild%deinit()
    deallocate( obj%lastChild )
    nullify(obj%firstchild, obj%lastChild)    
    obj%severity = 0
    obj%errorsExist = .false.

    ! obliterate the stack of log pointers
    do
       if ( .not. associated(obj%activeLogStack) ) exit
       call obj%popActiveLog()
    end do

    ! finally, wipe the text 
    obj%moduleName = ' '
    obj%procedureName = ' '
  end subroutine deinitLog

  
  subroutine report( obj, trigger )
    class(LogType), intent(inout) :: obj
    logical, intent(in), optional :: trigger
    if ( present(trigger) ) then
       if (.not. trigger) return
    end if
    if ( obj%errorsExist ) then
       write( echoUnit(), '(2a)' ) trim( WARNINGS(obj%getSeverity()+&
            1) ), ' reported'
       call obj%print( 0 )
       write( echoUnit(), '(a)' ) ' '
    end if
  end subroutine report
  

  logical function isEmpty( obj )
    class(LogType), intent(in) :: obj
    isEmpty = .not. associated( obj%firstChild )
  end function isEmpty

  
  recursive integer function numberOfLeaves( obj )
    class(LogType), intent(inout) :: obj
    class(LogInterface), pointer :: ptr
    numberOfLeaves = 0
    if ( .not. associated(obj%firstChild) ) return
    ptr => obj%firstChild
    do
       select type (ptr)
       class is (LogType)
          numberOfLeaves = numberOfLeaves + ptr%numberOfLeaves()
       class default
          numberOfLeaves = numberOfLeaves + 1
       end select
       if ( .not. associated(ptr%nextSibling) ) exit
       ptr => ptr%nextSibling
    end do
  end function numberOfLeaves


  pure subroutine beginSub_Log( obj, moduleName, procedureName )
    class(LogType), intent(inout) :: obj
    character(*), intent(in) :: moduleName, procedureName
    integer :: allocStat
    class(LogInterface), pointer :: log
    
    if ( associated(obj%activeLogStack) ) then
       select type ( log => obj%activeLogStack%log )
       type is (LogType)
          call log%addLog( 0, moduleName, procedureName, head=obj )
       end select
    else
       call obj%addLog( 0, moduleName, procedureName, head=obj )
    end if
  end subroutine beginSub_Log
  
  
  pure subroutine endSub_Log( obj )
    class(LogType), intent(inout) :: obj
    call obj%popActiveLog()
  end subroutine endSub_Log


  pure function checkSub_Log( obj, severity )
    class(LogType), intent(in) :: obj
    integer, intent(in) :: severity
    logical :: checkSub_Log
    if ( associated(obj%activeLogStack) ) then
       checkSub_Log = obj%activeLogStack%log%check(severity)
    else
       checkSub_Log = obj%check(severity)
    end if
  end function checkSub_Log
  
  
  pure subroutine addEventToActiveViaSpec( obj, severity, &
       message )
    class(LogType), intent(inout) :: obj
    integer, intent(in) :: severity
    character(*), intent(in) :: message
    
    if ( associated(obj%activeLogStack) ) then
       select type ( log => obj%activeLogStack%log )
       type is (LogType)
          call log%addEvent( severity, message )
       end select
       ! update the overall severity and the existence of this
       ! event
       call obj%activeLogStack%propagateError( severity, obj )
    else
       call obj%addEvent( severity, message )
    end if
  end subroutine addEventToActiveViaSpec

  
  pure subroutine addEventToActiveViaTriggerAndSpec( obj, trigger, &
       severity, message )
    class(LogType), intent(inout) :: obj
    logical, intent(in) :: trigger
    integer, intent(in) :: severity
    character(*), intent(in) :: message
    if ( trigger ) &
         call obj%addEventToActiveViaSpec( severity, message )
  end subroutine addEventToActiveViaTriggerAndSpec



  pure subroutine pushActiveLog_Log( obj, activeLog )
    class(LogType), intent(inout) :: obj
    class(LogInterface), target, intent(inout) :: activeLog
    class(LogStackType), pointer :: ptr
    integer :: allocStat

    nullify(ptr)
    if ( associated(obj%activeLogStack) ) then
       ptr => obj%activeLogStack
       nullify(obj%activeLogStack)
    end if
    
    allocate( obj%activeLogStack, stat=allocStat )
    obj%activeLogStack%log => activeLog
    
    if ( associated(ptr) ) then
       obj%activeLogStack%previous => ptr
    end if
  end subroutine pushActiveLog_Log

  
  pure subroutine popActiveLog_Log( obj )
    class(LogType), intent(inout) :: obj
    class(LogStackType), pointer :: ptr
    class(LogInterface), pointer :: child, parent
    
    if ( .not. associated(obj%activeLogStack) ) then
       call obj%addEvent( FATAL, 'Corrupt activeLogStack.  Look for &
            &a log%endSub() or endSub(log) call where there isn''t a &
            &matching beginSub call, for instance in test subroutines.' )
       return
    end if
    
    if ( associated(obj%activeLogStack%previous) ) then
       ptr => obj%activeLogStack%previous
    else
       nullify(ptr)
    end if
    
    deallocate( obj%activeLogStack )
    obj%activeLogStack => ptr
  end subroutine popActiveLog_Log

  
  !-------------------------------------------------------------------
  !- LogStackType methods
  !-------------------------------------------------------------------
  
  pure recursive subroutine propagateError( obj, severity, head )
    class(LogStackType), intent(inout) :: obj
    integer, intent(in) :: severity
    class(LogType), intent(inout) :: head
    obj%log%errorsExist = .true. 
    obj%log%severity = max( obj%log%severity, severity )
    if ( associated(obj%previous) ) then
       call obj%previous%propagateError( severity, head )
    else
       head%errorsExist = .true. 
       head%severity = max( head%severity, severity )
    end if
  end subroutine propagateError

  
  !-------------------------------------------------------------------
  !- EventType methods
  !-------------------------------------------------------------------
  
  pure subroutine initEventViaSource( obj, src )
    class(EventType), intent(out) :: obj
    class(EventType), intent(in) :: src
    call obj%initEventViaSpec( src%severity, src%message )
  end subroutine initEventViaSource

  
  pure subroutine initEventViaSpec( obj, severity, message )
    class(EventType), intent(out) :: obj
    integer, intent(in) :: severity
    character(*), intent(in) :: message
    obj%errorsExist = .true. 
    obj%severity = severity
    obj%message = message
    nullify(obj%nextSibling)
  end subroutine initEventViaSpec

  
  subroutine printEvent( obj, level )
    class(EventType), intent(in) :: obj
    integer, intent(in) :: level  
    ! print message
    write( echoUnit(), '(t'//int2str(level*TAB_WIDTH+1)//', 3a)' ) &
         trim(killSubstring(WARNINGS(obj%severity+1), '(&
         &s)')), ': ', trim(obj%message)
  end subroutine printEvent


  function killSubstring( str, substr )
    character(*), intent(in) :: str, substr
    character( len(str) ) :: killSubstring
    integer :: i
    i = index(str, substr)    
    if (i > 0) then
       killSubstring = str(1:i-1)
    else
       killSubstring = str
    end if
  end function killSubstring
  
  

  pure subroutine deinitEvent( obj )
    class(EventType), intent(inout) :: obj
    obj%severity = 0
    obj%message = ' '
  end subroutine deinitEvent
  
  
  pure subroutine pushActiveLog_Event( obj, activeLog )
    class(EventType), intent(inout) :: obj
    class(LogInterface), target, intent(inout) :: activeLog
    class(LogStackType), pointer :: ptr
    integer :: allocStat
  end subroutine pushActiveLog_Event

  
end module LogModule
