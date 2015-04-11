!> Utilities for outputting variables and other information to a
!> selected echo area.
!
!> Part of this module helps things run silently, re-routing all
!> outputs to a file called 'echo' when openEchoFile() is called.
!> closeEchoFile() routes outputs back to the screen again.  Mainly
!> this is done so that unit tests can run in an uncluttered command
!> window.  Unfortunately, it also means that legacy code must be
!> modified such that print*,... and write(*, ...) statements are
!> changed to write(echoUnit(), ...), and that 'use Echo' is inserted
!> at the top of each module.  It is quite easy to reverse these
!> changes by replacing all instances of echoUnit() with *.
!
!> The remainder of the module contains routines for outputting values
!> of different variables under the writeVariable interface.
module Echo
  
  implicit none

  private  
  public :: echoUnit, openEchoFile, resetEchoFile, closeEchoFile, &
       writeVariable, printVariable, timestamp

  !> Public I/O unit for the echo file.  This is assumed to be unique.
  integer, public, parameter :: ECHO_FILE_UNIT = 9

  ! The main setting.  If set to .true., output is always directed to
  ! screen.
  logical, parameter :: FORCE_ECHO_TO_STDOUT = .false.

  ! standard output
  integer, parameter :: STDOUT = 6
  
  ! precision of real numbers
  integer, parameter :: FLOAT = selected_real_kind(8, 30)
  real(FLOAT), parameter :: EPS = 1.e-7

  ! formatting parameters
  integer, parameter :: REALSP = 10              ! spacing between reals
  integer, parameter :: INTSP = 8                ! spacing between ints
  character(*), parameter :: REALFMT = '(f10.4)' ! print format for rls
  character(*), parameter :: INTFMT = '(i8)'     ! print format for ints
  integer, parameter :: NREAL = 8          ! max num of reals per line
  integer, parameter :: NINT = 10          ! "    "   "  ints  "  "
  ! when printing arrays, how many elements to include at each end
  ! before truncating
  integer, parameter :: NTRUNC = 2
  ! more formatting parameters
  character(*), parameter :: VARINDENT = '   '
  character(*), parameter, private :: TABFMT = '(t32)'
  integer, parameter, private :: CRITNAMELENGTH = 28
  character(*), parameter, private :: BORDERLINE = '===================&
       &==================================================='
  
  interface writeVariable
     module procedure writeVariable_string
     module procedure writeVariable_logical_scalar
     module procedure writeVariable_int_scalar
     module procedure writeVariable_int_vector
     module procedure writeVariable_real_scalar
     module procedure writeVariable_real_vector
     module procedure writeVariable_real_matrix
     module procedure writeVariable_real_array3
     module procedure writeVariable_real_array4
  end interface
  
  interface printVariable
     module procedure printVariable_string
     module procedure printVariable_logical_scalar
     module procedure printVariable_int_scalar
     module procedure printVariable_int_vector
     module procedure printVariable_real_scalar
     module procedure printVariable_real_vector
     module procedure printVariable_real_matrix
     module procedure printVariable_real_array3
     module procedure printVariable_real_array4
  end interface
  
contains

  

  !---------------------------------------------------------------------
  ! Helper functions
  !---------------------------------------------------------------------


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

  
  !> finds the number of digits in \a x.
  !> This function is pure [F95].
  pure function nDigits( x )
    integer, intent(in) :: x
    integer :: nDigits
    nDigits = floor(log10(max(real(x), 1.)) + 1)
  end function nDigits
    

  !> Turns positive integer X into a character string STR.  
  !> This function is pure [F95].
  pure function int2str( x ) result( str )

    ! arguments and local variables
    integer, intent(in) :: x
    character( len=nDigits(x) ) :: str
    integer :: n, i, j, y, y0

    ! find '0' in the ASCII character set
    y0 = ichar('0')
    
    ! loop over each digit in X
    n = nDigits(x)
    do i = 1, n
       y = x/10**(i-1)     ! truncate digits to the right
       y = modulo(y, 10)   ! truncate digits to the left
       j = n + 1 - i       ! get the corresponding index to STR
       ! look up result in the ASCII character set and store
       str(j:j) = char(y0 + y)
    end do

  end function int2str


  !> Turns character string \a str into an integer \a x.
  !> This function is pure [F95].
  pure function str2int( str ) result( x )

    ! arguments and local variables
    character(*), intent(in) :: str
    integer :: x
    integer :: n, i, j, y, y0

    ! find '0' in the ASCII character set
    y0 = ichar('0')

    ! initialise x, get number of digits
    x = 0
    n = len_trim( str )

    ! loop over digits adding up multiples of powers of 10
    do i = 1, n
       j = n + 1 - i
       y = ichar( str(i:i) ) - y0
       x = x + y*(10**j)
    end do

  end function str2int


  !---------------------------------------------------------------------
  ! Manipulation of the echo area
  !---------------------------------------------------------------------

  !> Returns the I/O unit of the echo area (which may or may not be
  !> standard output).  This can be used freely within write statements.
  function echoUnit()
    integer :: echoUnit
    logical :: isOpen
    character(1) :: wr    
    if ( .not. FORCE_ECHO_TO_STDOUT ) then
       
       inquire( ECHO_FILE_UNIT, opened=isOpen, write=wr )
       if ( isOpen .and. wr == 'Y') then
          echoUnit = ECHO_FILE_UNIT
       else
          echoUnit = STDOUT
       end if
       
    else
       echoUnit = STDOUT
    end if
    
  end function echoUnit 
    
  
  !> Changes echo behaviour from standard output to a text file.
  !> closeEchoFile is needed to close the file.
  subroutine openEchoFile()
    integer :: ioStat
    logical :: isExisting
    if ( .not. FORCE_ECHO_TO_STDOUT ) then       

       inquire( file='echo', exist=isExisting )
       if ( isExisting ) then
          open( ECHO_FILE_UNIT, file='echo', iostat=ioStat , &
               status='old', action='write', position='append' )
       else
          open( ECHO_FILE_UNIT, file='echo', iostat=ioStat , &
               status='new', action='write')
       end if
       
       if ( ioStat /= 0 ) then
          ! no choice but to print error to STDOUT if there is a problem
          write (STDOUT, '(a)') 'Non-fatal error in Echo::&
               &openEchoFile - problem opening the echo file.'
       end if
       
    end if
  end subroutine openEchoFile

  
  subroutine timestamp()
    character(8) :: date
    character(10) :: time
    call date_and_time( date, time )
    write( echoUnit(), '(13a)') '=== ', &
         date(1:4), '-', date(5:6), '-', date(7:8), ' ', &
         time(1:2), ':', time(3:4), ':', time(5:6), ' ==='
    write( echoUnit(), '(a)') ' '
  end subroutine timestamp
  
    
  !> Effectively wipes the echo file to receive fresh output.
  subroutine resetEchoFile()
    integer :: ioStat
    if ( .not. FORCE_ECHO_TO_STDOUT ) then

       rewind( ECHO_FILE_UNIT, iostat=ioStat )

       if ( ioStat /= 0 ) then
          ! no choice but to print error to STDOUT if there is a problem
          write (STDOUT, '(a)') 'Non-fatal error in Echo::&
               &closeEchoFile - problem rewinding the echo file.'     
       end if
       
    end if
  end subroutine resetEchoFile
  
    
  !> Changes echo behaviour back to standard output.  Must be called before
  !> the program terminates if openEchoFile was called.
  subroutine closeEchoFile()
    integer :: ioStat
    if ( .not. FORCE_ECHO_TO_STDOUT ) then

       write( echoUnit(), '(a)') ' '
       close( ECHO_FILE_UNIT, iostat=ioStat )
       
       if ( ioStat /= 0 ) then
          ! no choice but to print error to STDOUT if there is a problem
          write (STDOUT, '(a)') 'Non-fatal error in Echo::&
               &closeEchoFile: problem closing the echo file.'
       end if
       
    end if
  end subroutine closeEchoFile


  !---------------------------------------------------------------------
  ! writeVariable subroutines
  !---------------------------------------------------------------------
  
  !> Writes a header.
  subroutine writeHeader( ioUnit, headerText )
    integer, intent(in) :: ioUnit
    character(*), intent(in), optional :: headerText

    write ( ioUnit, '(a)' ) ' '
    write ( ioUnit, '(a)' ) BORDERLINE
    if ( present(headerText) ) then
       write ( ioUnit, '(a)' ) upperCase(headerText)
       write ( ioUnit, '(a)' ) BORDERLINE
    end if
    write ( ioUnit, '(a)' ) ' '
  end subroutine writeHeader


  !> Writes a subheader.
  subroutine writeSubheader( ioUnit, subheaderText )
    integer, intent(in) :: ioUnit
    character(*), intent(in), optional :: subheaderText
    character(70) :: truncLineOfDashes

    write ( ioUnit, '(a)' ) ' '
    if ( present(subheaderText) ) then
       truncLineOfDashes = BORDERLINE
       truncLineOfDashes( len_trim(subheaderText)+1: ) = ' '
       write ( ioUnit, '(2a)' ) VARINDENT, truncLineOfDashes
       write ( ioUnit, '(2a)' ) VARINDENT, subheaderText
    else
       truncLineOfDashes = '==='
    end if
    write ( ioUnit, '(2a)' ) VARINDENT, truncLineOfDashes
    write ( ioUnit, '(a)' ) ' '
  end subroutine writeSubheader
  
  
!!$  !> Writes a footer.
!!$  subroutine writeFooter( ioUnit, footerText )
!!$    integer, intent(in) :: ioUnit
!!$    character(*), intent(in), optional :: footerText
!!$
!!$    write ( ioUnit, '(a)' ) ' '
!!$    write ( ioUnit, '(a)' ) BORDERLINE
!!$    if ( present(footerText) ) then
!!$       write ( ioUnit, '(a)' ) upperCase(footerText)
!!$       write ( ioUnit, '(a)' ) BORDERLINE
!!$    end if
!!$  end subroutine writeFooter
  
 
  !> This is a private subroutine to be called at the beginning of each
  !> writeVariable subroutine that handles an array.  
  !> 
  subroutine writeArrayPreamble( ioUnit, arrayName, arraySizeVector, &
       extentLim, exponent, isTooBig )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: arrayName
    integer, dimension(:), intent(in) :: arraySizeVector
    integer, intent(in) :: extentLim, exponent
    logical, intent(out) :: isTooBig

    ! local variables and parameters
    integer :: maxExtent
    integer :: rank
    character(16) :: multiExtentFmt
    character(4) :: localIntFmt
    
    ! find the rank and maximum extent, and check to see if the array 
    ! is too big to print whole
    rank = count( arraySizeVector > 0 )
    maxExtent = maxval( arraySizeVector )
    isTooBig = (maxExtent > extentLim) .or. (rank > 2)

!!$    ! print the variable name.  If oversized, include limits
!!$    if ( isTooBig ) then
    ! revised - always include limits

    ! write a format descriptor to display extents compactly
    localIntFmt(:) = ' '
    localIntFmt = 'i'//int2str( nDigits(maxExtent) )
    
    ! write a format descriptor to display "size1 x size2 x ..." 
    multiExtentFmt(:) = ' '
    if ( rank > 1 ) then
       multiExtentFmt = int2str(rank-1)//'('' x '', '//&
            localIntFmt//'), '
    end if

    write ( ioUnit, '(3a, '//localIntFmt//', '//multiExtentFmt//' &
         &2a)' ) VARINDENT, arrayName, ' [', arraySizeVector, '] = ', &
         exp2str(exponent)

!!$    else
!!$       write ( ioUnit, '(3a)' ) VARINDENT, arrayName, ' = '
!!$    end if

  end subroutine writeArrayPreamble

  
  !> Returns an exponent to be displayed when printing out array
  !> values that are too big or too small for REALFMT.
  !>
  !> \a arrayMaxAbs is the maximum absolute value of the array.
  !> Within a certain range, the REALFMT can display values effectively
  !> and so \a commonExponent will be 0.
  function commonExponent( arrayMaxAbs )
    real(FLOAT), intent(in) :: arrayMaxAbs
    integer :: commonExponent
    integer, parameter :: THRESHOLDBIG = 3, THRESHOLDSMALL = -2
    if ( arrayMaxAbs < EPS ) then
       ! forget the calculation if values are close to zero.
       commonExponent = 0
    else
       ! this calculates the exponent
       commonExponent = int(floor(log10(arrayMaxAbs)))
       ! this determines whether exponent=0 is acceptable for
       ! printing the values in REALFMT
       if ( (THRESHOLDSMALL < commonExponent) .and. &
            (commonExponent < THRESHOLDBIG) ) then
          commonExponent = 0
       end if
    end if
  end function commonExponent

  !> Converts an exponent into a real amplitude.  e.g. amp(2) = 100.
  function amp( exponent )
    integer, intent(in) :: exponent
    real(FLOAT) :: amp
    amp = real(10., FLOAT)**exponent
  end function amp

  !> If \a exponent is an integer ±### (2 digits max), then exp2str is
  !> '1.0e±### *'.  But blank if \a exponent = 0.
  function exp2str( exponent )
    integer, intent(in) :: exponent
    character(11) :: exp2str
    exp2str(:) = ' '
    if ( exponent /= 0 ) then
       write ( exp2str, '(a, i2, a)') '1.0e', exponent, ' *'
!!$       exp2str = '1.0e'//int2str(exponent)//' *'
    end if
    
  end function exp2str


  !> Writes the string \a scalarName and logical \a scalar to \a ioUnit.
  subroutine writeVariable_logical_scalar( ioUnit, scalarName, scalar )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: scalarName
    logical, intent(in) :: scalar
    
    ! spread across two lines if variable name too long
    if ( len_trim(scalarName) > CRITNAMELENGTH ) then
       write ( ioUnit, '(3a)' ) VARINDENT, scalarName, ' = '
       write ( ioUnit, '('//TABFMT//', a)' ) scalar
       write ( ioUnit, '(a)' ) ' '
    else
       write ( ioUnit, '(3a, '//TABFMT//', l10)' ) &
            VARINDENT, scalarName, ' = ', scalar
    end if
  end subroutine writeVariable_logical_scalar

  
  !> Writes the strings \a stringName and \a string to \a ioUnit. 
  subroutine writeVariable_string( ioUnit, stringName, string )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: stringName
    character(*), intent(in) :: string
    
    ! spread across two lines if variable name too long
    if ( len_trim(stringName) > CRITNAMELENGTH ) then
       write ( ioUnit, '(3a)' ) VARINDENT, stringName, ' = '
       write ( ioUnit, '('//TABFMT//', a)' ) string
       write ( ioUnit, '(a)' ) ' '
    else
       write ( ioUnit, '(3a, '//TABFMT//', a)' ) VARINDENT, &
            stringName, ' = ', string
    end if
  end subroutine writeVariable_string

  !> Writes the string \a scalarName and integer values \a scalar to \a
  !> ioUnit.
  subroutine writeVariable_int_scalar( ioUnit, scalarName, scalar )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: scalarName
    integer, intent(in) :: scalar
        
    ! spread across two lines if variable name too long
    if ( len_trim(scalarName) > CRITNAMELENGTH ) then
       write ( ioUnit, '(3a)' ) VARINDENT, scalarName, ' = '
       write ( ioUnit, '('//TABFMT//', '//INTFMT//')' ) scalar
       write ( ioUnit, '(a)' ) ' '
    else
       write ( ioUnit, '(3a, '//TABFMT//', '//INTFMT//')' ) &
            VARINDENT, scalarName, ' = ', scalar
    end if
  end subroutine writeVariable_int_scalar
  

  !> Writes the string \a vectorName and integer values \a vector to \a
  !> ioUnit.  If \a vector is too big to print, only the bounds are
  !> displayed.
  subroutine writeVariable_int_vector( ioUnit, vectorName, vector )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: vectorName
    integer, dimension(:), intent(in) :: vector
    logical :: isTooBig
    call writeArrayPreamble(ioUnit, vectorName, ubound(vector), &
         NINT, 0, isTooBig)
    if ( isTooBig ) then
       write ( ioUnit, '(a, '//int2str(NTRUNC)//INTFMT//', a, '//&
            int2str(NTRUNC)//INTFMT//')' ) &
            VARINDENT, vector( 1 : NTRUNC ), ' ... ', &
            vector( size(vector)-NTRUNC+1 : size(vector) )
    else
       write ( ioUnit, '(a, '//int2str(NINT)//INTFMT//')' ) VARINDENT, &
            vector
    end if
    write ( ioUnit, '(a)' ) ' '
  end subroutine writeVariable_int_vector


  !> Writes the string \a scalarName and real values \a scalar to \a
  !> ioUnit.
  subroutine writeVariable_real_scalar( ioUnit, scalarName, scalar )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: scalarName
    real(FLOAT), intent(in) :: scalar
    integer :: e
    
    e = commonExponent( abs(scalar) )
    
    ! spread across two lines if variable name too long
    if ( len_trim(scalarName) > CRITNAMELENGTH ) then
       write ( ioUnit, '(4a)' ) VARINDENT, scalarName, ' = ', exp2str(e)
       write ( ioUnit, '('//TABFMT//', '//REALFMT//')' ) scalar/amp(e)
       write ( ioUnit, '(a)' ) ' '
    else
       write ( ioUnit, '(4a, '//TABFMT//', '//REALFMT//')' ) &
            VARINDENT, scalarName, ' = ', exp2str(e), scalar/amp(e)
    end if
  end subroutine writeVariable_real_scalar


  !> Writes the string \a vectorName and real values \a vector to \a
  !> ioUnit.  If \a vector is too big to print, only the bounds are
  !> displayed.
  subroutine writeVariable_real_vector( ioUnit, vectorName, vector )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: vectorName
    real(FLOAT), dimension(:), intent(in) :: vector
    logical :: isTooBig
    integer :: e
    
    e = commonExponent( maxval( abs(vector) ) )
    call writeArrayPreamble(ioUnit, vectorName, ubound(vector), NREAL, &
         e, isTooBig)
    
    if ( isTooBig ) then
       write ( ioUnit, '(a, '//int2str(NTRUNC)//REALFMT//', a, '//&
            int2str(NTRUNC)//REALFMT//')' ) &
            VARINDENT, vector( 1 : NTRUNC ), ' ... ', &
            vector( size(vector)-NTRUNC+1 : size(vector) )/amp(e)
    else
       write ( ioUnit, '(a, '//int2str(NREAL)//REALFMT//')' ) &
            VARINDENT, vector/amp(e)
    end if
    write ( ioUnit, '(a)' ) ' '
  end subroutine writeVariable_real_vector


  !> Writes the string \a matrixName and real values \a matrix to \a
  !> ioUnit.  If \a matrix is too big to print, only the bounds are
  !> displayed.
  !
  !> if \a isCartesian is present and switched on, the matrix is 
  !> output as a Cartesian array with the RIGHT and UP
  !> directions representing X and Y respectively.
  !> This is in contrast to the row/column system that would otherwise
  !> have X and Y going DOWN and RIGHT respectively.
  subroutine writeVariable_real_matrix( ioUnit, matrixName, matrix, &
       isCartesian )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: matrixName
    real(FLOAT), dimension(:, :), intent(in) :: matrix
    logical, intent(in), optional :: isCartesian
    logical :: isTooBig
    integer :: i, j, e
    
    e = commonExponent( maxval( abs(matrix) ) )
    call writeArrayPreamble(ioUnit, matrixName, ubound(matrix), &
         NREAL, e, isTooBig)

    ! check row ordering
    if ( (.not. present(isCartesian)) .or. isCartesian ) then 
       ! Cartesian format. Loop backwards over columns 
       ! (i.e. loop over y-direction).
       do j = size(matrix, 2), 1, -1
          call writeLine_real_matrix( ioUnit, matrix, .true., &
               isTooBig, j, e )
       end do

    else
       ! row/column format.  Loop over rows.
       do i = size(matrix, 1), 1, -1
          call writeLine_real_matrix( ioUnit, matrix, .false., &
               isTooBig, i, e )
       end do
    end if
    write ( ioUnit, '(a)' ) ' '
    
  end subroutine writeVariable_real_matrix


  ! private subroutine for writeVariable_real_matrix 
  subroutine writeLine_real_matrix( ioUnit, matrix, isCartesian, &
       isTooBig, iLine, exponent )
    integer, intent(in) :: ioUnit
    real(FLOAT), dimension(:, :), intent(in), target :: matrix
    logical, intent(in) :: isCartesian, isTooBig
    integer, intent(in) :: iLine, exponent
    real(FLOAT), dimension(:), pointer :: vector
    integer :: nElemPerLine, nLine
    character(1), dimension(NTRUNC) :: vertFiller
    logical, parameter :: DEBUGMODE = .false.

    ! get the current line and key numbers orientated correctly
    if ( isCartesian ) then
       vector => matrix(:, iLine)
       nElemPerLine = size(matrix, 1)
       nLine =        size(matrix, 2)
    else
       vector => matrix(iLine, :)
       nElemPerLine = size(matrix, 2)
       nLine =        size(matrix, 1)
    end if

!!$    if ( DEBUGMODE ) then
!!$       print REALFMT, vector
!!$       print INTFMT,  nElemPerLine
!!$       print INTFMT,  nLine
!!$    end if

    if ( DEBUGMODE ) then
       print *, ' '
       print *, 'In Global/writeLine_real_matrix:'
       print *, 'amp(exponent) = ', amp(exponent)
       print *, ' '
    end if
    
    ! check size
    if ( isTooBig ) then

       ! check the current line.  If it is at the start or end, 
       ! we may print it
       if ( iLine <= NTRUNC .or. iLine >= nLine-NTRUNC+1 ) then
          
          ! print a line with the middle section replaced by an ellipsis
          ! to denote filler.
          write ( ioUnit, '(a, '//int2str(NTRUNC)//REALFMT//', a, '//&
               int2str(NTRUNC)//REALFMT//')' ) &
               VARINDENT, vector( 1 : NTRUNC ), ' ... ', &
               vector( nElemPerLine-NTRUNC+1 : nElemPerLine )/amp(&
               exponent)

       elseif ( iLine == NTRUNC + 1 ) then

          ! if current line is just after the start, print vertical 
          ! filler symbols
          vertFiller(:) = ':'
          write ( ioUnit, '(a, '//&
               int2str(NTRUNC)//'('//int2str(REALSP-1)//&
               '('' ''), a), a, '//int2str(NTRUNC)//'('//int2str(&
               REALSP-1)//'('' ''), a))' ) VARINDENT, &
               vertFiller, '     ', vertFiller
          
       end if

    else
       ! if not too big, print the whole line
       write ( ioUnit, '(a, '//int2str(NREAL)//REALFMT//')' ) &
            VARINDENT, vector/amp(exponent)
    end if

  end subroutine writeLine_real_matrix

  
  !> calls writeLine_real_matrix for each layer in the 3rd dimension
  !> of \a array.  We assume that arrays are always Cartesian.
  subroutine writeVariable_real_array3( ioUnit, arrayName, array )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: arrayName
    real(FLOAT), dimension(:, :, :), intent(in) :: array
    integer :: i
    logical, parameter :: ISCARTESIAN = .true. 
    
    do i = 1, size(array, 3)
       call writeVariable_real_matrix( ioUnit, &
            arrayName//'(:, :, '//int2str(i)//')', array(:, :, i), &
            ISCARTESIAN )
    end do
    write ( ioUnit, '(a)' ) ' '
    
  end subroutine writeVariable_real_array3

  
  !> calls writeLine_real_matrix for each layer in the 3rd and 4th
  !> dimensions of \a array.
  subroutine writeVariable_real_array4( ioUnit, arrayName, array )
    integer, intent(in) :: ioUnit
    character(*), intent(in) :: arrayName
    real(FLOAT), dimension(:, :, :, :), intent(in) :: array
    integer :: i, j
    logical, parameter :: ISCARTESIAN = .true. 
    
    do j = 1, size(array, 4)
       do i = 1, size(array, 3)
          call writeVariable_real_matrix( ioUnit, &
               arrayName//'(:, :, '//int2str(i)//')', &
               array(:, :, i, j), ISCARTESIAN )
       end do
    end do
    write ( ioUnit, '(a)' ) ' '
    
  end subroutine writeVariable_real_array4


  
  !---------------------------------------------------------------------
  ! printVariable subroutines
  !---------------------------------------------------------------------

  !> Writes a header to the echo area.
  subroutine printHeader( headerText )
    character(*), intent(in), optional :: headerText
    call writeHeader( echoUnit(), headerText )
  end subroutine printHeader
  

  !> Writes a subheader to the echo area.
  subroutine printSubheader( subheaderText )
    character(*), intent(in), optional :: subheaderText
    call writeSubheader( echoUnit(), subheaderText )
  end subroutine printSubheader

  
  !> Writes the string \a scalarName and logical \a scalar to the echo
  !> area.
  subroutine printVariable_logical_scalar( scalarName, scalar )
    character(*), intent(in) :: scalarName
    logical, intent(in) :: scalar
    call writeVariable_logical_scalar( echoUnit(), scalarName, scalar )
  end subroutine printVariable_logical_scalar
  
    
  !> Writes the strings \a stringName and \a string to the echo area.
  subroutine printVariable_string( stringName, string )
    character(*), intent(in) :: stringName
    character(*), intent(in) :: string
    call writeVariable_string( echoUnit(), stringName, string )
  end subroutine printVariable_string
    

  !> Writes the string \a scalarName and integer values \a scalar to the
  !> echo area
  subroutine printVariable_int_scalar( scalarName, scalar )
    character(*), intent(in) :: scalarName
    integer, intent(in) :: scalar
    call writeVariable_int_scalar( echoUnit(), scalarName, scalar )
  end subroutine printVariable_int_scalar

  
  !> Writes the string \a vectorName and integer values \a vector to the
  !> echo area.  If \a vector is too big to print, only the bounds are
  !> displayed.
  subroutine printVariable_int_vector( vectorName, vector )
    character(*), intent(in) :: vectorName
    integer, dimension(:), intent(in) :: vector
    call writeVariable_int_vector( echoUnit(), vectorName, vector )
  end subroutine printVariable_int_vector


  !> Writes the string \a scalarName and real values \a scalar to the
  !> echo area.
  subroutine printVariable_real_scalar( scalarName, scalar )
    character(*), intent(in) :: scalarName
    real(FLOAT), intent(in) :: scalar
    call writeVariable_real_scalar( echoUnit(), scalarName, scalar )
  end subroutine printVariable_real_scalar


  !> Writes the string \a vectorName and real values \a vector to the
  !> echo area.  If \a vector is too big to print, only the bounds are
  !> displayed.
  subroutine printVariable_real_vector( vectorName, vector )
    character(*), intent(in) :: vectorName
    real(FLOAT), dimension(:), intent(in) :: vector
    call writeVariable_real_vector( echoUnit(), vectorName, vector )
  end subroutine printVariable_real_vector


  !> Writes the string \a matrixName and real values \a matrix to the
  !> echo area.  If \a matrix is too big to print, only the bounds are
  !> displayed.
  !
  !> if \a isCartesian is present and switched on, the matrix is 
  !> output as a Cartesian array with the RIGHT and UP
  !> directions representing X and Y respectively.
  !> This is in contrast to the row/column system that would otherwise
  !> have X and Y going DOWN and RIGHT respectively.
  subroutine printVariable_real_matrix( matrixName, matrix, &
       isCartesian )
    character(*), intent(in) :: matrixName
    real(FLOAT), dimension(:, :), intent(in) :: matrix
    logical, intent(in), optional :: isCartesian
    if ( present(isCartesian) ) then
       call writeVariable_real_matrix( echoUnit(), matrixName, matrix, &
            isCartesian )
    else
       call writeVariable_real_matrix( echoUnit(), matrixName, matrix )
    end if
  end subroutine printVariable_real_matrix
   
  
  !> calls writeLine_real_matrix for each layer in the 3rd dimension of
  !> \a array.  We assume that arrays are always Cartesian.  Output is
  !> to the echo area.
  subroutine printVariable_real_array3( arrayName, array )
    character(*), intent(in) :: arrayName
    real(FLOAT), dimension(:, :, :), intent(in) :: array
    call writeVariable_real_array3( echoUnit(), arrayName, array )
  end subroutine printVariable_real_array3

  
  !> calls printLine_real_matrix for each layer in the 3rd and 4th
  !> dimensions of \a array.  Output is to the echo area.
  subroutine printVariable_real_array4( arrayName, array )
    character(*), intent(in) :: arrayName
    real(FLOAT), dimension(:, :, :, :), intent(in) :: array
    call writeVariable_real_array4( echoUnit(), arrayName, array )
  end subroutine printVariable_real_array4
  
    
end module Echo
