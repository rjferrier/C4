! This module provides useful constants and subroutines that are
! available to all C4 modules.   The module should be kept lean.
module Global
   
  USE_MACROS({Container})
  use LogModule
  use Global_DomainDim
  
  implicit none  
  public

  private :: getUnit_new

  character(*), parameter, private  :: MOD_NAME = 'Global'
  
  ! read/write 
  integer, parameter :: STDIN = 5      !< standard input unit
  integer, parameter :: STDOUT = 6     !< standard output unit

  ! error numbers
  integer, parameter :: ERROR_ALREADY_ALLOCATED = 151
  integer, parameter :: ERROR_NOT_ALLOCATED = 153
  
  ! constants
  real(FLOAT), parameter :: PI = 3.1415927
  real(FLOAT), parameter :: GRAVITY = 9.80665

  
  interface clone
     module procedure cloneArray1
     module procedure cloneArray2
  end interface

  
  interface destroy
     module procedure destroyArray1
     module procedure destroyArray2
  end interface

  ! don't know why but the name 'openFile' can cause conflicts in client
  ! modules, so use the 'open' interface instead
  interface open
     module procedure openFile
  end interface

  interface close
     module procedure closeFile
  end interface


  interface assignment(=)
     module procedure assign_String
  end interface
  
  
  !-------------------------------------------------------------------
  !- ID management
  !-------------------------------------------------------------------

  ! would type-parameterise N_IDS but parameterized derived types not
  ! supported yet
  integer, parameter :: N_IDS = 100
  
  
  type, public :: IDManagerType
     private
     logical, dimension(N_IDS) :: IDArray = .false.
   contains
     procedure :: requestID => requestID_IDManager
     procedure :: returnID => returnID_IDManager
  end type IDManagerType
  
  
  ! immediately declare an instance to keep track of file IDs.  But make
  ! it private and instead let clients have access to public procedures
  ! requestFileID and returnFileID.  These filter the IDs to prevent STDIN
  ! and STDOUT being used.
  type(IDManagerType), private :: fileIDManager


  !-------------------------------------------------------------------
  !- GridParameters
  !-------------------------------------------------------------------

  
  type, public :: GridParametersType
     type(SizeVectorType) :: size
     type(RealVectorType), dimension(2) :: extents
     type(RealVectorType) :: spacing
   contains
     procedure :: init_GridParameters_args
     procedure :: init_GridParameters_src
     generic :: init => init_GridParameters_args, init_GridParameters_src
     procedure :: deinit => deinit_GridParameters
  end type GridParametersType

  
  interface assignment(=)
     module procedure assign_gridParams_gridParams
  end interface


  
  !-------------------------------------------------------------------
  !- String and StringList
  !-------------------------------------------------------------------

  type, public :: StringType
     character(80) :: data
   contains
     procedure :: init => init_String  
     procedure :: deinit => deinit_String  
  end type StringType

  EXPAND({TYPEDEFS_CONTAINER(String, Type, List, {}, {
     procedure :: writeContents => writeContents_StringList})})

  
contains
  
  !-------------------------------------------------------------------
  !- IDManager procedures
  !-------------------------------------------------------------------
 
  pure subroutine requestID_IDManager( obj, i, log )
    class(IDManagerType), intent(inout) :: obj
    integer, intent(out) :: i
    class(LogType), intent(inout), optional :: log
    logical, parameter :: DEBUG_MODE = .false. 
    call beginSub( MOD_NAME, 'requestID_IDManager', log )
    
    do i = 1, N_IDS
       if ( obj%IDArray(i) ) cycle
       obj%IDArray(i) = .true.
       call endSub( log )
       return
    end do
    call addEvent( FATAL, 'Out of IDs.  Recompile with N_IDS set &
         &higher.', log )
    call addEvent( DEBUG_MODE, ADVICE, 'ID counter = '//str(i), log )
    
    call endSub( log )
  end subroutine requestID_IDManager

  
  pure subroutine returnID_IDManager( obj, i, log )
    class(IDManagerType), intent(inout) :: obj
    integer, intent(inout) :: i
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'returnID_IDManager', log )
    
    if ( i < 1 .or. i > N_IDS ) then
       call addEvent( WARNING, 'Expected 0 <= i <= N_IDS; found i = '//&
            str(i)//'.', log )
       call endSub( log )
       return
    end if
    
    obj%IDArray(i) = .false.
    i = 0
    
    call endSub( log )
  end subroutine returnID_IDManager

  
  !-------------------------------------------------------------------
  !- GridParameters methods
  !-------------------------------------------------------------------
  
  pure subroutine init_GridParameters_args( obj, size, extents, spacing )
    class(GridParametersType), intent(inout) :: obj
    type(SizeVectorType), intent(in) :: size
    type(RealVectorType), dimension(2), intent(in) :: extents
    type(RealVectorType), intent(in) :: spacing
    obj%size = size
    obj%extents = extents
    obj%spacing = spacing
  end subroutine init_GridParameters_args

  
  pure subroutine init_GridParameters_src( obj, src )
    class(GridParametersType), intent(inout) :: obj
    type(GridParametersType), intent(in) :: src
    obj%size = src%size
    obj%extents = src%extents
    obj%spacing = src%spacing
  end subroutine init_GridParameters_src

  
  pure subroutine deinit_GridParameters( obj )
    class(GridParametersType), intent(inout) :: obj
    obj%size = 0
    obj%extents = 0._FLOAT
    obj%spacing = 0._FLOAT
  end subroutine deinit_GridParameters


  elemental subroutine assign_gridParams_gridParams( tgt, src )
    type(GridParametersType), intent(out) :: tgt
    type(GridParametersType), intent(in) :: src
    tgt%size = src%size
    tgt%extents = src%extents
    tgt%spacing = src%spacing
  end subroutine assign_gridParams_gridParams

  
  !-------------------------------------------------------------------
  !- String and StringList methods
  !-------------------------------------------------------------------

  elemental subroutine assign_String( tgt, src )
    type(StringType), intent(out) :: tgt
    type(StringType), intent(in) :: src
    tgt%data = src%data
  end subroutine assign_String
    
  
  pure subroutine init_String( obj, src )
    class(StringType), intent(out) :: obj
    class(StringType), intent(inout) :: src
    obj%data = src%data
  end subroutine init_String
    
  
  pure subroutine deinit_String( obj, log )
    class(StringType), intent(inout) :: obj
    type(LogType), intent(inout) :: log
    call beginSub( MOD_NAME, 'deinit_String', log )

    obj%data(:) = ' '

    call endSub( log )
  end subroutine deinit_String

  
  EXPAND({METHODDEFS_CONTAINER(String, Type, List)})

  subroutine writeContents_StringList( obj, fileID, log )
    class(StringListType), intent(inout) :: obj
    integer, intent(in) :: fileID
    class(LogType), intent(inout), optional :: log
    type(StringListIteratorType) :: sli
    type(StringType), pointer :: s
    integer :: iostat
    call beginSub( MOD_NAME, 'writeContents_StringList', log )

    call sli%init( obj )
    call sli%first( s )
    do
       if ( sli%isDone() ) exit

       write ( unit=fileID, fmt='(a)', iostat=iostat ) trim(s%data)
       
       call addEvent( iostat/=0, FATAL, 'Tried to write '''//trim(s%&
            data)//''' but statement returned IOSTAT='//str(iostat), &
            log )
       
       call sli%next( s )
    end do

    call endSub( log )
  end subroutine writeContents_StringList
  
  !-------------------------------------------------------------------
  !- Global procedures
  !-------------------------------------------------------------------

  subroutine requestFileID( i, log )
    integer, intent(out) :: i
    class(LogType), intent(inout), optional :: log
    logical :: ok
    call beginSub( MOD_NAME, 'requestFileID', log )

    do 
       ok = .true.
       call fileIDManager%requestID( i, log )
       
       if ( i == STDIN ) ok = .false.
       if ( i == STDOUT ) ok = .false.
       if ( ok ) exit
    end do
        
    call endSub( log )
  end subroutine requestFileID

  
  subroutine returnFileID( i, log )
    integer, intent(inout) :: i
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'returnFileID', log )

    call fileIDManager%returnID( i, log )
    
    call endSub( log )
  end subroutine returnFileID

  
  pure recursive function det( matrix, rank )
    integer, intent(in) :: rank
    real(FLOAT), dimension(rank, rank), intent(in) :: matrix
    logical, dimension(rank, rank) :: mask
    real(FLOAT) :: det, sign
    integer :: i
    if ( rank > 1 ) then
       det = 0._FLOAT
       do i = 1, rank
          mask = .true.
          mask(1, :) = .false.
          mask(:, i) = .false.
          sign = real( 2*mod(i, 2) - 1, FLOAT )
          det = det + &
               sign*matrix(1, i)*det( reshape(pack(matrix, mask), &
               [rank-1, rank-1]), rank-1 )
       end do
    else
       det = matrix(1, 1)
    end if
  end function det

  
  
  pure subroutine cloneArray1( tgt, src, log )
    real(FLOAT), dimension(:), allocatable, intent(inout) :: tgt
    real(FLOAT), dimension(:), intent(in) :: src
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'cloneArray1', log )

    call addEvent( allocated(tgt), FATAL, 'tgt already allocated.', log )
    
    allocate( tgt(size(src, 1)), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating tgt(size(src, &
         &1)).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    tgt = src

    call endSub( log )
  end subroutine cloneArray1

  
  pure subroutine cloneArray2( tgt, src, log )
    real(FLOAT), dimension(:, :), allocatable, intent(inout) :: tgt
    real(FLOAT), dimension(:, :), intent(in) :: src
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'cloneArray2', log )

    call addEvent( allocated(tgt), FATAL, 'tgt already allocated.', log )

    allocate( tgt(size(src, 1), size(src, 2)), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating tgt(size(src, 1), &
         &size(src, 2)).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    tgt = src

    call endSub( log )
  end subroutine cloneArray2

  
  subroutine destroyArray1( tgt, log )
    real(FLOAT), dimension(:), allocatable, intent(inout) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'destroyArray1', log )

    call addEvent( .not. allocated(tgt), FATAL, 'tgt already &
         &deallocated.', log )

    deallocate( tgt, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating tgt.  STAT='//&
         int2str(stat), log )
  end subroutine destroyArray1

  
  subroutine destroyArray2( tgt, log )
    real(FLOAT), dimension(:, :), allocatable, intent(inout) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'destroyArray2', log )
    
    call addEvent( .not. allocated(tgt), FATAL, 'tgt already &
         &deallocated.', log )

    deallocate( tgt, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating tgt.  STAT='//&
         int2str(stat), log )
  end subroutine destroyArray2
  

  ! converts fraction to real with FLOAT precision
  elemental function frac( numerator, denominator )
    integer, intent(in) :: numerator, denominator
    real(FLOAT) :: frac
    frac = real(numerator, FLOAT)/real(denominator, FLOAT)
  end function frac
  
  
  ! LOWERCASE
  !> Converts any string to lowercase.  This function is pure [F95].
  pure function lowerCase( str )
    character(*), intent(in) :: str
    character( len(str) ) :: lowerCase

    character(len=26), parameter :: &
         UPPERSET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
         LOWERSET = 'abcdefghijklmnopqrstuvwxyz'   
    integer :: i, j

    ! copy str to the result
    lowerCase = str
    
    ! loop over the characters in STR
    do i = 1, len(str)
       ! find the current character in UPPERSET
       j = index( UPPERSET, str(i:i) )
       ! if a match was made, replace current character with the
       !  corresponding LOWERSET character
       if ( j > 0 ) then
          lowerCase(i:i) = LOWERSET(j:j)
       end if
    end do

  end function lowerCase
  

  ! UPPERCASE
  !> Converts any string to uppercase.  This function is pure [F95].
  pure function upperCase( str )    
    character(*), intent(in) :: str
    character( len(str) ) :: upperCase

    character(len=26), parameter :: &
         UPPERSET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
         LOWERSET = 'abcdefghijklmnopqrstuvwxyz'   
    integer :: i, j

    ! copy str to the result
    upperCase = str
    
    ! loop over the characters in STR
    do i = 1, len(str)
       ! find the current character in LOWERSET
       j = index( LOWERSET, str(i:i) )
       ! if a match was made, replace current character with the
       !  corresponding UPPERSET character
       if ( j > 0 ) then
          upperCase(i:i) = UPPERSET(j:j)
       end if
    end do

  end function upperCase

  
  !> strips \a filename of \a extension.
  subroutine stripFilename( filename, extension )
    character(*), intent(inout) :: filename
    character(*), intent(in) :: extension
    integer :: i    
    i = index(filename, '.'//extension)
    if (i > 0) then
       filename(i:) = ' '
    end if
  end subroutine stripFilename


  !> Gets a logical unit number for its I/O as default
  !
  !> [This code was lifted, with modification to the exception part, from
  !> IO_Utilities_mod.F90, written by Tom Clune as part of NASA's pFUnit
  !> testing facility.]
  !
  !! @return getUnit_new new logical unit number
  !
  !! @note The Fortran standard apparently does not dictate the legitimate
  !! range for logical unit numbers used for I/O aside from the requirement
  !! that they not be negative.  The following limits are generally safe
  !! for known compilers.  If an application needs more than 90 files to be
  !! opened simultaneously (shudder), then MAX_UNIT_NUMBER could be
  !! increased on a per-compiler basis.
  integer function getUnit_new()
    integer :: unit
    logical :: isOpen
    
    integer, parameter :: MIN_UNIT_NUMBER = 10
    integer, parameter :: MAX_UNIT_NUMBER = 99
    
    do unit = MIN_UNIT_NUMBER, MAX_UNIT_NUMBER
       inquire(unit = unit, opened = isOpen)
       if (.not. isOpen) then
          getUnit_new = unit
          return
       end if
    end do
    
  end function getUnit_new

  
  
  !> Routine for opening a file.
  !
  !> \a fileID is returned to the caller who uses this to close the file
  !> later.  \a readOrWrite is a single character: 'r' for read or 'w' for
  !> write.  If \a filename and \a readOrWrite are omitted, the file is
  !> assumed to be a scratch file.
  subroutine openFile( fileID, filename, readOrWrite, log )
    integer, intent(out) :: fileID
    character(*), intent(in), optional :: filename
    character(1), intent(in), optional :: readOrWrite
    class(LogType), intent(inout), optional :: log
    integer :: iostat
    call beginSub( MOD_NAME, 'openFile', log )

    fileID = getUnit_new()
    
    ! if arguments are missing, just open a scratch file
    if ( .not. present( filename ) ) then
       open( unit=fileID, status='scratch', action='readwrite', iostat=&
            iostat )
       return

    else
       ! open the file with attributes depending on readORWrite
       if ( readOrWrite == 'r' ) then
          open( unit=fileID, file=trim(filename), status='old', action=&
               'read', iostat=iostat )
       elseif ( readOrWrite == 'w' ) then
          open( unit=fileID, file=trim(filename), status='replace', &
               action='write', iostat=iostat )
       end if
    end if
    call addEvent( iostat/=0, FATAL, 'Problem opening file.  IOSTAT='//&
         str(iostat), log )

    call endSub( log )
  end subroutine openFile


  !> Routine for closing a file.  \a fileID is provided to the user from  
  !> \b openFile.
  subroutine closeFile( fileID, log )
    integer, intent(in) :: fileID
    class(LogType), intent(inout), optional :: log
    integer :: iostat
    call beginSub( MOD_NAME, 'closeFile', log )

    ! attempt to close
    close( unit=fileID, iostat=iostat )

    call endSub( log )
  end subroutine closeFile

  
end module Global
