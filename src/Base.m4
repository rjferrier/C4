m4_changequote({,})

m4_define({HEADER},
{! This file has been generated using the M4 preprocessor. Edits to the
! source code must be done in the file:
! m4___file__

! {$0} @ m4___line__})

!# the USE_MACROS macro is used to redefine the m4 "define" construct.
!# The keyword "USE_MACROS" is also used by our emacs code to
!# figure out dependencies, so don't change it!
m4_define({USE_MACROS}, 
{! Dependent on macros defined in $1_Macros.m4})

!# the MACRO macro is used to redefine the m4 "define" construct.
!# Specifically, on expansion, it decorates the usual macro definition
!# with a statement describing the location of the macro definition.
!# This file, when expanded, then acts as a sort of ad-hoc tags file.
!# Note that use of the word "macro" by itself (without arguments) is
!# protected.
m4_define({MACRO}, {m4_ifelse({$1}, {}, {$0}, 
{m4___file__ : m4___line__ : $1
m4_define($1, $2)})})
 
!# the EXPAND macro wraps macro expansion in the source code.
!# Specifically, on expansion, it decorates the usual macro expansion
!# with macro name, <<< >>> delimiters, and line number.  Note
!# that use of the word "expand" by itself (without arguments) is
!# protected.
m4_define({EXPAND}, {m4_ifelse({$1}, {}, {$0},
{! m4_regexp({$1}, {\([A-Z0-9_]*\)\(.*\)}, {{\1}}) @ m4___line__ <<< $1! >>> })})

!# MACRO_HELPER and EXPAND_HELPER are less-functional versions of the above,
!# designed to be usable within the same file as the main expansions.
m4_define({MACRO_HELPER}, {m4_ifelse({$1}, {}, {$0}, 
{m4_define($1, $2) m4_dnl})})
m4_define({EXPAND_HELPER}, {m4_ifelse({$1}, {}, {},
{! @ m4___line__ <<< $1 ! >>> })})

m4_define({UPCASE}, {m4_translit({$*}, {a-z}, {A-Z})})
m4_define({DOWNCASE}, {m4_translit({$*}, {A-Z}, {a-z})})
m4_define({LWR}, {DOWNCASE(m4_substr({$1}, 0, 1)){}m4_substr({$1}, 1)})
m4_define({UPR}, {UPCASE(m4_substr({$1}, 0, 1)){}m4_substr({$1}, 1)})
m4_define({ABB}, {m4_translit({DOWNCASE($*)}, {a-z}, {})})  
m4_define({TRIMCOMMA}, {m4_patsubst({$1}, {\(\(w\), *\)*, }, {\1})})
m4_define({DEFAULT}, {m4_ifelse({$2}, {}, {{$0}}, {m4_ifelse({$1}, {}, {$2}, {$1})})})


! lifted from http://mbreen.com/m4.html#toc19
m4_define({FOREACH}, {m4_ifelse(eval($#>2), 1,
  {m4_pushdef({$1}, {$3})$2{}m4_popdef({$1})m4_dnl{}m4_ifelse(m4_eval($#>3), 1,
  {$0({$1},{$2},m4_shift(m4_shift(m4_shift($@))))})})})


MACRO({PROCEDURE_CREATE}, {{
  ! This subroutine name should appear in the public entity list at
  ! the top of this module
  subroutine create$1( LWR($2), $3 log )
    class($2Interface), allocatable, intent(inout) :: LWR($2)
    $4
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'create$1', log )
    
    ! allocate
    if ( allocated(LWR($2)) ) then
       call addEvent( FATAL, 'LWR($2) already allocated.', log )
    else
       allocate( $1Type :: LWR($2), stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &$1Type :: LWR($2).  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise
    select type ( LWR($2) )
    class is ($1Type)
       call LWR($2)%init( $3 log )
    end select
    call endSub( log )
  end subroutine create$1
  }})


MACRO({PROCEDURES_CREATE_ARRAY}, {{
  ! This subroutine name should appear in the public entity list at
  ! the top of this module
  pure subroutine alloc$1Array( LWR($2), nElements, log )
    class($2Interface), dimension(:), allocatable, intent(inout) :: &
         LWR($2)
    integer, intent(in) :: nElements
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'alloc$1Array', log )
    
    ! allocate
    if ( allocated(LWR($2)) ) then
       call addEvent( FATAL, 'LWR($2) already allocated.', log )
    else
       allocate( $1Type :: LWR($2){}(nElements), stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating $1Type :: &
            &LWR($2){}(nElements).  STAT='//int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    call endSub( log )
  end subroutine alloc$1Array

  
  ! This subroutine name should appear in the public entity list at
  ! the top of this module
  subroutine init$1ArrayElement( LWR($2), iElement, $3 log )
    class($2Interface), dimension(:), intent(inout) :: LWR($2)
    integer, intent(in) :: iElement
    $4
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init$1ArrayElement', log )
    
    ! initialise
    select type ( LWR($2) )
    class is ($1Type)
       call LWR($2)(iElement)%init( $3 log )
    end select
    
    call endSub( log )
  end subroutine init$1ArrayElement
  }})

  
MACRO({PROCEDURE_ATTACH}, {{
  ! This is like a «create» procedure, except the result is not a new
  ! allocated object but a modified, existing host object that holds
  ! the allocated object as an extension.  Why not cut out this
  ! unbound procedure and delegate completely to the host object with
  ! setExtension( <initialising args> )?  Because that would couple
  ! the host to the concrete extension.  Here we are using dependency 
  ! injection to remove the coupling.
  ! This subroutine should appear in the public entity list at the 
  ! top of this module.
  subroutine attach$1( LWR($3), $4 log )
    class($3Interface), intent(inout) :: LWR($3)
    $5
    class(LogType), intent(inout), optional :: log
    class($2Interface), allocatable ::  LWR($2)
    integer :: allocStat
    call beginSub( MOD_NAME, 'attach$1', log )
    
    ! allocate
    if ( allocated(LWR($2)) ) then
       call addEvent( FATAL, 'LWR($2) already allocated.', log )
    else
       allocate( $1Type :: LWR($2), stat=allocStat )
       call addEvent( allocStat/=0, FATAL, 'Problem allocating &
            &$1Type :: LWR($2).  STAT='&
	    //int2str(allocStat), log )
    end if
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! initialise.  Note that this creates an associative link from the
    ! extension object to the host.
    select type ( LWR($2) )
    class is ($1Type)
       call LWR($2)%init( LWR($3), $4 log )
    end select

    ! extend the host.  This creates a compositional link from the
    ! host to the extension object.  IF THE COMPILER COMPLAINS THAT
    ! THE SETEXTENSION METHOD IS MISSING, then this needs to be
    ! defined for the host object.  Our attachment mechanism just
    ! assumes that such an interface exists.
    call LWR($3)%setExtension( LWR($2), log )
    $6
    
    call endSub( log )
  end subroutine attach$1
  }})

  
MACRO({PROCEDURE_DESTROY}, {{
  ! This subroutine name should appear in a public «destroy» interface
  ! at the top of this module
  $2 subroutine destroy$1( LWR($1), log )
    class($1Interface), allocatable, intent(inout) :: &
         LWR($1)
    class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'destroy$1', log )
    
    if ( .not. allocated(LWR($1)) ) then
       call addEvent( WARNING, 'LWR($1) already deallocated.', log )
    else
       call LWR($1)%deinit( log )
       deallocate( LWR($1), stat=deallocStat )
       call addEvent( deallocStat/=0, FATAL, 'Problem deallocating &
            LWR($1).  STAT='//int2str(deallocStat), log )
    end if
    
    call endSub( log )
  end subroutine destroy$1
  }})

  
MACRO({PROCEDURE_DESTROY_ARRAY}, {{
  ! This subroutine name may also be included in the public «destroy»
  ! interface if it is needed
  $3 subroutine destroy$1Array( LWR($1), log )
    m4_ifelse($2, {Type}, {type}, {class})($1$2), dimension(:), allocatable, intent(inout) :: &
         LWR($1)
    class(LogType), intent(inout), optional :: log
    integer :: i, deallocStat
    call beginSub( MOD_NAME, 'destroy$1Array', log )
    
    if ( .not. allocated(LWR($1)) ) then
       call addEvent( WARNING, 'LWR($1) already deallocated.', log )
    else
       do i = 1, size(LWR($1))
          call LWR($1)(i)%deinit( log )
       end do
       deallocate( LWR($1), stat=deallocStat )
       call addEvent( deallocStat/=0, FATAL, 'Problem deallocating &
            LWR($1).  STAT='//int2str(deallocStat), log )
    end if
    
    call endSub( log )
  end subroutine destroy$1Array
  }})


  MACRO({PROCEDURE_INJECT}, {{
  subroutine inject_$1( src, tgt, log )
    class($1Interface), allocatable, intent(inout) :: src, tgt
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'inject_$1', log )

    ! check
    call addEvent( .not. allocated(src), WARNING, 'Argument to be &
         &injected has not been allocated.', log )
    call addEvent( allocated(tgt), FATAL, 'Component to receive &
         &injection is already allocated.', log )

    ! copy the source to the target, then destroy the source.  If the
    ! compiler complains that the CLONE or DESTROY interfaces are missing,
    ! then they need to be defined for $1Interface.  Our injection
    ! mechanism just assumes that such interfaces exists.
    call src%clone( tgt, log )
    call destroy( src, log )
        
    call endSub( log )
  end subroutine inject_$1
  }})
  
  
  MACRO({INJECT}, {{
    call checkInjection( allocated($1), &
         allocated(obj%$1), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( $1, &
         obj%$1 )
    }})

  
  MACRO({EXTRACT}, {{
    call checkExtraction( allocated(obj%$1), allocated($1), &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call move_alloc( obj%$1, &
         $1 )
    }})
    
  MACRO({DEALLOC}, {{
    deallocate( $1, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating &
         &$1.  STAT='//int2str(stat), log )
    }})

