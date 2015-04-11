
MACRO({TYPEDEFS_CONTAINER}, {{
  type, public :: $1$3Type
     private
     integer :: nNodes = 0
     type($1$3NodeType), pointer :: head => null()
     type($1$3NodeType), pointer :: tail => null()
     ! custom attributes
     $4
   contains
     procedure :: deinit => deinit_$1$3
     procedure :: size => size_$1$3
     
     ! modification
     procedure :: append => append_$1$3
     procedure :: remove => remove_$1$3
     procedure :: takeNodes => takeNodes_$1$3
     procedure, private :: createNode => createNode_$1$3
     procedure, private :: destroyNode => destroyNode_$1$3

     procedure :: find => find_$1$3
     m4_ifelse($3, {List}, {}, {! set-specific methods
     procedure :: match => match_$1$3
     procedure :: sameAs => sameAs_$1$3

     })! custom methods
     $5
  end type $1$3Type
  
  
  type :: $1$3NodeType
     m4_ifelse($2, {Type}, {type($1$2)}, {class($1$2), allocatable}) :: contents
     type($1$3NodeType), pointer :: next => null(), prev => null()
  end type $1$3NodeType


  type, public :: $1$3IteratorType
     ! this pointer can be used to traverse the container gradually as
     ! well as insert/extract nodes (immediately before it).
     type($1$3NodeType), pointer :: ptr => null()
     type($1$3Type), pointer :: target$3 => null()
     ! custom attributes
     $6
   contains
     procedure :: init => init_$1$3Iterator
     procedure :: first => first_$1$3Iterator
     procedure :: prev => prev_$1$3Iterator
     procedure :: next => next_$1$3Iterator
     procedure :: last => last_$1$3Iterator
     procedure :: isDone => isDone_$1$3Iterator
     procedure :: current => current_$1$3Iterator
     procedure :: insert => insert_$1$3Iterator
     procedure :: extract => extract_$1$3Iterator
     ! custom methods
     $7
  end type $1$3IteratorType
  }})
  
  
MACRO({METHODDEFS_CONTAINER}, {{
  $5 subroutine deinit_$1$3( obj, log )
    class($1$3Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    m4_ifelse($2, {Type},
    {type($1$2) :: contents},
    {class($1$2), allocatable :: contents})
    integer :: deallocStat
    call beginSub( MOD_NAME, 'deinit_$1$3', log )
   
    do
       ! exit when empty
       if ( obj%nNodes == 0 ) exit
       
       ! remove the end node.  The extraction potentially triggers a
       ! fatal error; check for this.  This operation decrements
       ! nNodes by 1.
       call obj%remove( m4_ifelse($2, {Type}, {}, {contents, })log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       m4_ifelse($2, {Type}, {},
       {! destroy the contents.  IF THE COMPILER COMPLAINS THAT THE
       ! %DEINIT INTERFACE IS MISSING, then this needs to be
       ! defined in the content's class.  Our container mechanism just
       ! assumes that such a signature exists.
       m4_ifelse($3, {Set}, {! It is further required that the interface
       ! be pure.})
       call contents%deinit( log )
       deallocate( contents, stat=deallocStat )
       call addEvent( deallocStat/=0, WARNING, 'Problem deallocating &
            &contents.  STAT='//int2str(deallocStat), log )})
    end do
    call addEvent( obj%nNodes > 0, WARNING, 'obj%nNodes should be 0; &
         & actual number is '//str(obj%nNodes), log )

    ! custom deinitialisations
    $4
    
    call endSub( log )
  end subroutine deinit_$1$3


  pure function size_$1$3( obj )
    class($1$3Type), intent(in) :: obj
    integer :: size_$1$3
    size_$1$3 = obj%nNodes
  end function size_$1$3

  
  $5 subroutine append_$1$3( obj, contents, log )
    class($1$3Type), intent(inout) :: obj
    m4_ifelse($2, {Type},
    {type($1$2), intent(inout) :: contents},
    {class($1$2), allocatable, intent(inout) :: contents})
    class(LogType), intent(inout), optional :: log
    type($1$3NodeType), pointer :: newNode
    integer :: allocStat
    call beginSub( MOD_NAME, 'append_$1$3', log )
    
    m4_ifelse($3, {Set}, {
    ! if the contents already exist in this set, return
    if ( obj%match(contents) /= 0 ) then
       call endSub( log )
       return
    end if
    
    })! create and fill new node
    call obj%createNode( newNode, contents, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! update links
    if ( .not. associated(obj%head) ) then
       obj%head => newNode
       obj%tail => newNode
    else
       obj%tail%next => newNode
       newNode%prev => obj%tail
       obj%tail => newNode
    end if

    call endSub( log )
  end subroutine append_$1$3

  
  $5 subroutine remove_$1$3( obj, m4_ifelse($2, {Type}, {}, {contents, })log )
    class($1$3Type), intent(inout) :: obj
    m4_ifelse($2, {Type}, {},
    {class($1$2), allocatable, intent(inout) :: contents})
    class(LogType), intent(inout), optional :: log
    type($1$3NodeType), pointer :: doomedNode
    call beginSub( MOD_NAME, 'remove_$1$3Iterator', log )

    ! check container status
    if ( .not. associated(obj%tail) ) then
       call addEvent( WARNING, '$3 is empty.  No extraction &
            &performed.', log )
       call endSub( log )
       return
    end if
    
    ! update links, isolating the tail node.  Also, if the 'current'
    ! node is being removed, take care to properly nullify it
    doomedNode => obj%tail
    if ( associated(obj%tail%prev) ) then
       obj%tail => obj%tail%prev
       nullify(obj%tail%next)
    else
       nullify(obj%tail)
       nullify(obj%head)
    end if
        
    call obj%destroyNode( doomedNode, m4_ifelse($2, {Type}, {}, {contents, })log )
    
    call endSub( log )
  end subroutine remove_$1$3
  

  $5 subroutine takeNodes_$1$3( obj, src, log )
    class($1$3Type), intent(inout) :: obj
    class($1$3Type), intent(inout) :: src
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'takeNodes_$1$3', log )

    ! check the existence of source nodes
    if ( .not. associated(src%head) ) then
       return
    end if

    ! link source nodes to target container and nodes
    if ( .not. associated(obj%head) ) then
       obj%head => src%head
    else
       obj%tail%next => src%head
       src%head%prev => obj%tail
    end if
    obj%tail => src%tail

    ! sever source container links
    nullify(src%head)
    nullify(src%tail)

    ! update nNodes in either case
    obj%nNodes = obj%nNodes + src%nNodes
    src%nNodes = 0
    
    call endSub( log )
  end subroutine takeNodes_$1$3
  

  $5 subroutine createNode_$1$3( obj, node, contents, log )
    class($1$3Type), intent(inout) :: obj
    type($1$3NodeType), intent(inout), pointer :: node
    m4_ifelse($2, {Type},
    {type($1$2), intent(inout) :: contents},
    {class($1$2), allocatable, intent(inout) :: contents})
    class(LogType), intent(inout), optional :: log
    integer :: allocStat
    call beginSub( MOD_NAME, 'createNode_$1$3', log )

    ! allocate
    allocate( node, stat=allocStat )
    call addEvent( allocStat/=0, FATAL, 'Problem allocating node.  &
         &STAT='//int2str(allocStat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
        
    m4_ifelse($2, {Type},
    {! assign node contents.  IF THE COMPILER COMPLAINS THAT THE (=)
    ! ASSIGNMENT INTERFACE IS MISSING, then this needs to be defined
    ! (overloaded) for the derived type.  Our container mechanism just
    ! assumes that the assignment interface exists.
    m4_ifelse($5, {pure}, {! UPDATE (2012-11-14): for pure variations,
    ! changed the (=) operator to an init call to admit pure procedures
    ! with intent(inout) pointer arguments.
    call node%contents%init( contents )}, {node%contents = contents})
    },
    {! inject source argument into the node
    call checkInjection( allocated(contents), allocated(&
         node%contents), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call contents%clone( node%contents, log )
    call destroy( contents, log )})
    
    obj%nNodes = obj%nNodes + 1

    call endSub( log )
  end subroutine createNode_$1$3


  m4_ifelse($2, {Type}, {}, {! beware that this does not destroy the contents but rather moves
  ! the contents back into the argument.})
  $5 subroutine destroyNode_$1$3( obj, node, m4_ifelse($2, {Type}, {}, {contents, })log )
    class($1$3Type), intent(inout) :: obj
    type($1$3NodeType), pointer :: node
    m4_ifelse($2, {Type},
    {},
    {class($1$2), allocatable, intent(inout) :: contents}
    )class(LogType), intent(inout), optional :: log
    integer :: deallocStat
    call beginSub( MOD_NAME, 'destroyNode_$1$3', log )
        
    m4_ifelse($2, {Type},
    {! deinit contents.  IF THE COMPILER COMPLAINS THAT THE %DEINIT
    ! INTERFACE IS MISSING, then this needs to be defined in the
    ! content's class.  Our container mechanism just assumes that such
    ! a signature exists.
    call node%contents%deinit( log )},
    {! extract node contents to argument 
    call checkExtraction( allocated(node%contents), allocated(&
         contents), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call node%contents%clone( contents, log )
    call destroy( node%contents, log )})
    
    obj%nNodes = obj%nNodes - 1

    ! deallocate
    deallocate( node, stat=deallocStat )
    call addEvent( deallocStat/=0, WARNING, 'Problem deallocating node.  &
         &STAT='//int2str(deallocStat), log )

    call endSub( log )
  end subroutine destroyNode_$1$3

  
  ! uses an iterator to return the contents of the node at index i.
  $5 subroutine find_$1$3( obj, contents, index, log )
    class($1$3Type), intent(inout) :: obj
    m4_ifelse($2, {Type}, {type}, {class})($1$2), intent(out), pointer, optional :: contents
    integer, intent(in) :: index
    class(LogType), intent(inout), optional :: log
    type($1$3IteratorType) :: iterator
    integer :: i
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, 'find_$1$3Iterator', log )

    call iterator%init( obj )
    do i = 1, index
       call iterator%next()
       if ( iterator%isDone() ) then
          call addEvent( WARNING, 'Ran past the end of the $3.', &
               log )
          exit
       end if
    end do
    
    ! return the current contents if they exist
    call iterator%current( contents )

    call endSub( log )
  end subroutine find_$1$3

  m4_ifelse($3, {List}, {}, {
  ! Returns the index of contents within obj, if it is a member.  If
  ! it is not, 0 is returned.  Optional logical array skip specifies
  ! nodes to skip over in the match.
  function match_$1$3( obj, contents, skip ) result ( index )
    class($1$3Type), intent(inout), target :: obj
    m4_ifelse($2, {Type}, {type}, {class})($1$2), intent(in) :: contents
    logical, dimension(:), intent(in), optional :: skip
    integer :: index
    type($1$3IteratorType) :: iterator
    m4_ifelse($2, {Type}, {type}, {class})($1$2), pointer :: nodeContents
    call iterator%init( obj )
    call iterator%first( nodeContents )
    do index = 1, obj%size()
       if ( iterator%isDone() ) exit
       
       if ( present(skip) ) then
          if ( skip(index) ) then
             call iterator%next( nodeContents )
             cycle
          end if
       end if
       
       ! IF THE COMPILER COMPLAINS THAT THE %SAMEAS INTERFACE IS
       ! MISSING, then this needs to be defined in the content's
       ! class.  Our container mechanism just assumes that such a
       ! signature exists.
       if ( contents%sameAs( nodeContents ) ) return
       
       call iterator%next( nodeContents )
    end do
    ! if we are here then the match has failed
    index = 0 
  end function match_$1$3
  
  
  function sameAs_$1$3( obj, tgt, log )
    class($1$3Type), intent(inout), target :: obj, tgt
    class(LogType), intent(inout), optional :: log
    logical :: sameAs_$1$3    
    logical, dimension( obj%nNodes ) :: objChecklist, tgtChecklist
    integer :: i, j
    type($1$3IteratorType) :: iterator
    m4_ifelse($2, {Type}, {type}, {class})($1$2), pointer :: nodeContents
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, 'sameAs_$1$3', log )
    
    ! default to fail
    sameAs_$1$3 = .false.

    ! check sizes (i.e. number of nodes) match
    if ( obj%size() /= tgt%size() ) then
       call endSub( log )
       return
    end if
    
    tgtChecklist = .false.
    objChecklist = .false.
    call iterator%init( obj )
    call iterator%first( nodeContents )

    ! check elements match.  Order does not matter.  To do this, cycle
    ! through the elements on one side checking if they match on the
    ! other.  We keep a record of matches on the latter side so as not
    ! to duplicate matches.
    do j = 1, obj%size()
       if ( iterator%isDone() ) exit
       
       i = tgt%match( nodeContents, tgtChecklist )
       if ( i > 0 ) then
          objChecklist(j) = .true.
          tgtChecklist(i) = .true.
       end if
       
       call iterator%next( nodeContents )
    end do
    call addEvent( DEBUG_MODE, ADVICE, 'objChecklist = '//str(&
         objChecklist), log )
    call addEvent( DEBUG_MODE, ADVICE, 'tgtChecklist  = '//str(&
         tgtChecklist), log )

    sameAs_$1$3 = all( objChecklist )

    call endSub( log )
  end function sameAs_$1$3
  })
  
  $5 subroutine init_$1$3Iterator( obj, target$3 )
    class($1$3IteratorType), intent(out) :: obj
    type($1$3Type), target, intent(m4_ifelse($5, {pure}, {inout}, {in})) :: target$3
    obj%target$3 => target$3
  end subroutine init_$1$3Iterator
  
  
  ! sets the internal node pointer to the head node, optionally
  ! providing a pointer to the contents
  $5 subroutine first_$1$3Iterator( obj, contents )
    class($1$3IteratorType), intent(inout) :: obj
    m4_ifelse($2, {Type}, {type}, {class})($1$2), intent(out), pointer, optional :: contents

    if ( .not. associated(obj%target$3%head) ) then
       nullify(obj%ptr)
       if ( present (contents) ) nullify(contents)
    else
       obj%ptr => obj%target$3%head
       if ( present (contents) ) contents => obj%ptr%contents
    end if
  end subroutine first_$1$3Iterator

  
  ! advances the internal node pointer, optionally providing a pointer
  ! to the contents
  $5 subroutine next_$1$3Iterator( obj, contents )
    class($1$3IteratorType), intent(inout) :: obj
    m4_ifelse($2, {Type}, {type}, {class})($1$2), intent(out), pointer, optional :: contents

    ! if current is null, start from the beginning
    if ( obj%isDone() ) then
       call obj%first(contents)
       return
    end if

    if ( .not. associated(obj%ptr%next) ) then
       if ( present (contents) ) nullify(contents)
       nullify(obj%ptr)
    else
       obj%ptr => obj%ptr%next
       if ( present (contents) ) contents => obj%ptr%contents
    end if
  end subroutine next_$1$3Iterator

  
  ! retreats the internal node pointer, optionally providing a pointer
  ! to the contents
  $5 subroutine prev_$1$3Iterator( obj, contents )
    class($1$3IteratorType), intent(inout) :: obj
    m4_ifelse($2, {Type}, {type}, {class})($1$2), intent(out), pointer, optional :: contents
    
    ! if current is null, start from the end 
    if ( obj%isDone() ) then
       call obj%last(contents)
       return
    end if

    if ( .not. associated(obj%ptr%prev) ) then
       if ( present (contents) ) nullify(contents)
       nullify(obj%ptr)
    else
       obj%ptr => obj%ptr%prev
       if ( present (contents) ) contents => obj%ptr%contents
    end if
  end subroutine prev_$1$3Iterator
    

  ! fast-forwards the internal node pointer to the tail node, optionally
  ! providing a pointer to the contents
  $5 subroutine last_$1$3Iterator( obj, contents )
    class($1$3IteratorType), intent(inout) :: obj
    m4_ifelse($2, {Type}, {type}, {class})($1$2), intent(out), pointer, optional :: contents
    
    if ( .not. associated(obj%target$3%tail) ) then
       nullify(obj%ptr)
       if ( present (contents) ) nullify(contents)
    else
       obj%ptr => obj%target$3%tail
       if ( present (contents) ) contents => obj%ptr%contents
    end if
  end subroutine last_$1$3Iterator
  

  ! BEWARE OF COMPILER BUG WHEN CALLING THIS FUNCTION.  If bug appears,
  ! query .not. associated(iterator%ptr) directly.
  pure function isDone_$1$3Iterator( obj )
    class($1$3IteratorType), intent(in) :: obj
    logical :: isDone_$1$3Iterator
    isDone_$1$3Iterator = .not. associated(obj%ptr)
  end function isDone_$1$3Iterator

  
  ! provides a pointer to the current node's contents
  $5 subroutine current_$1$3Iterator( obj, contents )
    class($1$3IteratorType), intent(inout) :: obj
    m4_ifelse($2, {Type}, {type}, {class})($1$2), intent(out), pointer, optional :: contents

    if ( obj%isDone() ) then
       if ( present (contents) ) nullify(contents)
    else
       if ( present (contents) ) contents => obj%ptr%contents
    end if
  end subroutine current_$1$3Iterator

  
  $5 subroutine insert_$1$3Iterator( obj, contents, log )
    class($1$3IteratorType), intent(inout) :: obj
    m4_ifelse($2, {Type},
    {type($1$2), intent(inout) :: contents},
    {class($1$2), allocatable, intent(inout) :: contents})
    class(LogType), intent(inout), optional :: log
    type($1$3NodeType), pointer :: newNode
    integer :: allocStat
    call beginSub( MOD_NAME, 'insert_$1$3Iterator', log )

    ! if this is the end of the container, pass control to append
    if ( obj%isDone() ) then
       call obj%target$3%append( contents, log )
       call endSub( log )
       return
    end if
    m4_ifelse($3, {Set}, {
    ! if the contents already exist in this set, return
    if ( obj%target$3%match(contents) /= 0 ) then
       call endSub( log )
       return
    end if

    })! create and fill new node
    call obj%target$3%createNode( newNode, contents, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! update links.  newNode becomes the new 'current' node.
    if ( .not. associated(obj%ptr%prev) ) then
       obj%target$3%head%prev => newNode
       newNode%next => obj%target$3%head
       obj%target$3%head => newNode
    else
       newNode%next => obj%ptr
       newNode%prev => obj%ptr%prev
       obj%ptr%prev%next => newNode
       obj%ptr%prev => newNode
    end if
    obj%ptr => newNode

    call endSub( log )
  end subroutine insert_$1$3Iterator

  
  ! extracts the node targeted by the current pointer 
  $5 subroutine extract_$1$3Iterator( obj, m4_ifelse($2, {Type}, {}, {contents, })log )
    class($1$3IteratorType), intent(inout) :: obj
    m4_ifelse($2, {Type}, {},
    {class($1$2), allocatable, intent(inout) :: contents
    })class(LogType), intent(inout), optional :: log
    type($1$3NodeType), pointer :: doomedNode
    logical :: eol
    call beginSub( MOD_NAME, 'extract_$1$3', log )
    
    ! if this is the end of the container, nullify the current node
    ! pointer and pass control to remove.  By end of the container we
    ! mean that the current node is either null or last in the
    ! container.
    eol = .false. 
    if ( obj%isDone() ) then
       eol = .true.
    else
       if ( .not. associated(obj%ptr%next) ) then
          eol = .true.
       end if
    end if
    if ( eol ) then
       nullify(obj%ptr)
       call obj%target$3%remove( m4_ifelse($2, {Type}, {}, {contents, })log )
       call endSub( log )
       return
    end if

    ! update links, isolating the current node
    doomedNode => obj%ptr
    if ( associated(obj%ptr%prev) ) then
       obj%ptr%next%prev => obj%ptr%prev
       obj%ptr%prev%next => obj%ptr%next
    else
       nullify(obj%ptr%next%prev)
       obj%target$3%head => obj%ptr%next
    end if
    obj%ptr => obj%ptr%next
    
    call obj%target$3%destroyNode( doomedNode, m4_ifelse($2, {Type}, {}, {contents, })log )
    
    call endSub( log )
  end subroutine extract_$1$3Iterator
  }})

