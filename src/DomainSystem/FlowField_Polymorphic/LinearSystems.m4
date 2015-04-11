HEADER()
module LinearSystemsModule
      
  USE_MACROS({FlowField})
  USE_MACROS({LinearSystems})
  use LogModule
  use Global
  use FlowFieldModule

  ! coupling to concrete class module
  use PureLinearOperationsModule

  use mkl95_lapack
  use mkl95_blas
  use mkl95_precision

  implicit none
  
  private
  public :: createScalarMultiRowMatrix, createScalarSingleRowMatrix, &
       createScalarRHSColumn, createScalarLinearSystem
  
    
  character(*), parameter :: MOD_NAME = 'LinearSystemsModule'

  logical, parameter :: USE_PIVOTING = .true.
  
  
  !-------------------------------------------------------------------
  !- Matricies
  !-------------------------------------------------------------------

  ! Strategy pattern
  type, abstract :: ScalarMatrix_OperandBehaviourInterface
   contains
     procedure(sm_ob_a), deferred :: acceptAsOperand1
     procedure(sm_ob_a), deferred :: acceptAsOperand2
     procedure(sm_ob_a), deferred :: acceptAsOperand3
  end type ScalarMatrix_OperandBehaviourInterface

  
  type, extends(ScalarMatrixInterface), abstract :: ScalarMatrixType
     private
     real(FLOAT), dimension(:, :), allocatable :: rows
     real(FLOAT), dimension(:, :), allocatable :: Q2
     integer, dimension(:), allocatable :: jPivot
     real(FLOAT), dimension(:), allocatable :: tau 
     logical :: transposed 
     integer :: rowCounter, columnCounter
     logical :: factorised
     class(ScalarMatrix_OperandBehaviourInterface), allocatable :: &
          operandBehaviour
   contains
     procedure :: initAbstract => initAbstract_ScalarMatrix
     procedure :: deinitAbstract => deinitAbstract_ScalarMatrix
     procedure :: factorise => factorise_ScalarMatrix
     procedure :: performQRDecomposition => &
          performQRDecomposition_ScalarMatrix
     procedure :: buildNullVectors => buildNullVectors_ScalarMatrix
     procedure :: solve => solve_ScalarMatrix
     procedure :: permutate => permutate_ScalarMatrix
     procedure :: getNumRows => getNumRows_ScalarMatrix
     procedure :: getNumColumns => getNumColumns_ScalarMatrix
     procedure :: prepareToReset => prepareToReset_ScalarMatrix
  end type ScalarMatrixType

  
  type, extends(ScalarMatrixType) :: ScalarMultiRowMatrixType
     private
     logical :: blockDiagonal 
   contains
     EXPAND({METHODLIST_LINEAROPERAND_CONCRETE({ScalarMultiRowMatrix})})
     procedure :: init => init_ScalarMultiRowMatrix
     procedure :: deinit => deinit_ScalarMultiRowMatrix
     procedure :: setRows_real => setRows_real_ScalarMultiRowMatrix
     procedure :: setRows_LinOp => setRows_LinOp_ScalarMultiRowMatrix
  end type ScalarMultiRowMatrixType
  

  type, extends(ScalarMatrixType) :: ScalarSingleRowMatrixType
     private
     logical :: segmented 
   contains
     EXPAND({METHODLIST_LINEAROPERAND_CONCRETE({ScalarSingleRowMatrix})})
     procedure :: init => init_ScalarSingleRowMatrix
     procedure :: deinit => deinit_ScalarSingleRowMatrix
     procedure :: setRows_real => setRows_real_ScalarSingleRowMatrix
     procedure :: setRows_LinOp => setRows_LinOp_ScalarSingleRowMatrix
  end type ScalarSingleRowMatrixType

  
  type, extends(ScalarMatrix_OperandBehaviourInterface) :: ScalarMatrix_SequentialSectionBehaviourType
     contains
     EXPAND({METHODLIST_LINEAROPERAND_CONCRETE(
     {ScalarMatrix_SequentialSectionBehaviour})})
  end type ScalarMatrix_SequentialSectionBehaviourType

  
  type, extends(ScalarMatrix_OperandBehaviourInterface) :: ScalarMatrix_WholeArrayBehaviourType
     contains
     EXPAND({METHODLIST_LINEAROPERAND_CONCRETE(
     {ScalarMatrix_WholeArrayBehaviour})})
  end type ScalarMatrix_WholeArrayBehaviourType

  
  abstract interface
     subroutine sm_ob_a( obj, scalarLinearOperation, scalarMatrix, from )
       import ScalarMatrix_OperandBehaviourInterface, ScalarMatrixType, &
            ScalarLinearOperationInterface
       class(ScalarMatrix_OperandBehaviourInterface), intent(in) :: obj
       class(ScalarLinearOperationInterface), intent(inout) :: &
            scalarLinearOperation
       class(ScalarMatrixType), target, intent(inout) :: scalarMatrix
       integer, dimension(2), intent(in) :: from
     end subroutine sm_ob_a
  end interface

  
  
  !-------------------------------------------------------------------
  !- RHSColumns
  !-------------------------------------------------------------------

  ! Strategy pattern
  type, abstract :: ScalarRHSColumn_OperandBehaviourInterface
   contains
     procedure(srhsc_ob_a), deferred :: acceptAsOperand1
     procedure(srhsc_ob_a), deferred :: acceptAsOperand2
     procedure(srhsc_ob_a), deferred :: acceptAsOperand3
  end type ScalarRHSColumn_OperandBehaviourInterface


  type, extends(ScalarRHSColumnInterface) :: ScalarRHSColumnType
     private
     ! each RHSGenerator yields an arbitrary number of real RHS elements.
     ! The 'from' vector is therefore needed to map each RHSGenerator to
     ! its target first element in the realValues array.
     class(ScalarRHSGeneratorInterface), dimension(:), allocatable :: &
          scalarRHSGenerators
     integer, dimension(:), allocatable :: from
     
     ! a pre-generator can be used to initialise the RHSColumn before an
     ! iterative update, e.g. when computing b0 - sum( Ak*xk, k=1, n ),
     ! the pre-generator can compute b0 while the other generators handle
     ! the summation.
     class(ScalarRHSGeneratorInterface), allocatable :: &
          scalarRHSPreGenerator
     integer :: nPre
     
     ! the realValues and permutation arrays are nElements long
     real(FLOAT), dimension(:, :), allocatable :: realValues
     
     integer :: elementCounter
     logical :: counterIsFrozen
     class(ScalarRHSColumn_OperandBehaviourInterface), allocatable :: &
          operandBehaviour
   contains
     EXPAND({METHODLIST_LINEAROPERAND_CONCRETE({ScalarRHSColumn})})
     procedure :: init => init_ScalarRHSColumn
     procedure :: deinit => deinit_ScalarRHSColumn
     procedure :: setElements_real => setElements_real_ScalarRHSColumn
     procedure :: setElements_RHSGen => setElements_RHSGen_ScalarRHSColumn
     procedure :: getNumElements => getNumElements_ScalarRHSColumn
     procedure :: refresh => refresh_ScalarRHSColumn
     procedure :: prepareToReset => prepareToReset_ScalarRHSColumn
  end type ScalarRHSColumnType

  
  type, extends(ScalarRHSColumn_OperandBehaviourInterface) :: ScalarRHSColumn_SequentialSectionBehaviourType
     contains
     EXPAND({METHODLIST_LINEAROPERAND_CONCRETE(
     {ScalarRHSColumn_SequentialSectionBehaviour})})
  end type ScalarRHSColumn_SequentialSectionBehaviourType

  
  type, extends(ScalarRHSColumn_OperandBehaviourInterface) :: ScalarRHSColumn_WholeArrayBehaviourType
     contains
     EXPAND({METHODLIST_LINEAROPERAND_CONCRETE(
     {ScalarRHSColumn_WholeArrayBehaviour})})
  end type ScalarRHSColumn_WholeArrayBehaviourType

  abstract interface
     subroutine srhsc_ob_a( obj, scalarLinearOperation, scalarRHSColumn, &
          from  )
       import ScalarRHSColumn_OperandBehaviourInterface, &
            ScalarRHSColumnType, ScalarLinearOperationInterface
       class(ScalarRHSColumn_OperandBehaviourInterface), intent(in) :: obj
       class(ScalarLinearOperationInterface), intent(inout) :: &
            scalarLinearOperation
       class(ScalarRHSColumnType), target, intent(inout) :: scalarRHSColumn
       integer, dimension(2), intent(in) :: from
     end subroutine srhsc_ob_a
  end interface

  
  
  !-------------------------------------------------------------------
  !- LinearSystems
  !-------------------------------------------------------------------
  
  type, extends(ScalarLinearSystemInterface) :: ScalarLinearSystemType
     private
     ! although linear system components are readily available as concrete
     ! types (existing in the same module), we will declare abstract
     ! classes so that we force use of the public interfaces and therefore
     ! do not inadvertently introduce coupling between the concrete
     ! subclasses.
     class(ScalarMatrixInterface), allocatable :: scalarMatrix
     class(ScalarRHSColumnInterface), allocatable :: scalarRHSColumn 
     real(FLOAT), dimension(:, :), allocatable :: solution
     integer :: visitationCounter, solutionElementCounter
     logical :: factorised
     class(PureScalarLinearOperationInterface), allocatable :: &
          completingOperation
   contains
     EXPAND({METHODLIST_LINEAROPERAND_CONCRETE({ScalarLinearSystem})})
     procedure :: init => init_ScalarLinearSystem
     procedure :: deinit => deinit_ScalarLinearSystem
     procedure :: setMatrixRows_real => &
          setMatrixRows_real_ScalarLinearSystem
     procedure :: setMatrixRows_LinOp => &
          setMatrixRows_LinOp_ScalarLinearSystem
     procedure :: setRHSElements_RHSGen => &
          setRHSElements_RHSGen_ScalarLinearSystem
     procedure :: setRHSElements_real => &
          setRHSElements_real_ScalarLinearSystem
     procedure :: setSolutionElements_real => &
          setSolutionElements_real_ScalarLinearSystem
     procedure :: setSolutionElements_LinOp => &
          setSolutionElements_LinOp_ScalarLinearSystem
     procedure :: factorise => factorise_ScalarLinearSystem
     procedure :: solve => solve_ScalarLinearSystem
     procedure :: permutateSolution => permutateSolution_ScalarLinearSystem
     procedure :: getNumEquations => getNumEquations_ScalarLinearSystem
     procedure :: prepareToReset => prepareToReset_ScalarLinearSystem
     procedure :: prepareToBeVisited => &
          prepareToBeVisited_ScalarLinearSystem
  end type ScalarLinearSystemType
  

contains
  
  !-------------------------------------------------------------------
  !- ScalarMatrix methods
  !-------------------------------------------------------------------
  
  subroutine initAbstract_ScalarMatrix( obj, nRows, nColumns, &
       transposed, behaveAsWholeArray, log )
    class(ScalarMatrixType), intent(inout) :: obj
    integer, intent(in) :: nRows, nColumns
    logical, intent(in), optional :: transposed, behaveAsWholeArray
    class(LogType), intent(inout), optional :: log
    integer :: m, n, stat
    logical :: tp, bwa, flag
    call beginSub( MOD_NAME, 'initAbstract_ScalarMatrix', log )

    ! default transposition argument
    if ( present(transposed) ) then
       tp = transposed
    else
       tp = .false. 
    end if

    ! default behaveAsWholeArray argument
    if ( present(behaveAsWholeArray) ) then
       bwa = behaveAsWholeArray
    else
       bwa = .false. 
    end if

    ! the matrix may be stored transposed.  The motivation for this
    ! functionality was to keep QR factorisation of the transposed
    ! underdetermined system compact.  We could have opted for LQ
    ! factorisation instead, which makes the transposition unnecessary.
    ! But it seems that LAPACK has a pivoting QR algorithm which is more
    ! sophisticated than the LQ algorithm.
    if ( tp ) then
       m = nColumns
       n = nRows
    else
       m = nRows
       n = nColumns
    end if
    
    allocate( obj%rows(m, n), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%rows(m, n).  &
         &STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%rows = 0._FLOAT

    obj%transposed = tp
    obj%rowCounter = 0
    obj%columnCounter = 0
    obj%factorised = .false.
    
    if ( bwa ) then
       allocate( ScalarMatrix_WholeArrayBehaviourType :: obj%&
            operandBehaviour, stat=stat )
    else
       allocate( ScalarMatrix_SequentialSectionBehaviourType :: obj%&
            operandBehaviour, stat=stat )
    end if
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%&
         &operandBehaviour.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    call endSub( log )
  end subroutine initAbstract_ScalarMatrix

  
  pure subroutine deinitAbstract_ScalarMatrix( obj, log )
    class(ScalarMatrixType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinitAbstract_ScalarMatrix', log )

    obj%transposed = .false. 
    obj%rowCounter = 0
    obj%columnCounter = 0
    obj%factorised = .false.
    
    deallocate( obj%rows, obj%operandBehaviour, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating obj%rows, obj%&
         &operandBehaviour.  STAT='//int2str(stat), log )
    
    ! the other components may not have been allocated, so don't worry if
    ! we get STAT=ERROR_NOT_ALLOCATED.
    deallocate( obj%jPivot, obj%tau, stat=stat )
    call addEvent( stat/=0 .and. stat==ERROR_NOT_ALLOCATED, WARNING, &
         'Problem deallocating obj%jPivot, obj%tau.  STAT='//int2str(&
         stat), log )
    deallocate( obj%Q2, stat=stat )
    call addEvent( stat/=0 .and. stat==ERROR_NOT_ALLOCATED, WARNING, &
         'Problem deallocating obj%Q2.  STAT='//int2str(stat), log )
    
    call endSub( log )
  end subroutine deinitAbstract_ScalarMatrix
  
  
  
  subroutine factorise_ScalarMatrix( obj, log )
    class(ScalarMatrixType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log 
    call beginSub( MOD_NAME, 'factorise_ScalarMatrix', log )

    call addEvent( obj%factorised, WARNING, 'Matrix already &
         &factorised.  Method aborted.', log )
    call addEvent( obj%rowCounter==0, WARNING, 'Empty or unallocated &
         &matrix.  Method aborted.', log )
    if ( obj%factorised .or. obj%rowCounter==0 ) then
       call endSub( log )
       return
    end if
        
    call obj%performQRDecomposition( log )
    call obj%buildNullVectors( log )
    obj%factorised = .true.
    
    call endSub( log )
  end subroutine factorise_ScalarMatrix

  
  subroutine performQRDecomposition_ScalarMatrix( obj, log )
    class(ScalarMatrixType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, n, stat, info
    call beginSub( MOD_NAME, 'performQRDecomposition_ScalarMatrix', log )
    
    ! create helper components
    n = size( obj%rows, 2 )
    allocate( obj%jPivot(n), obj%tau(n), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%jPivot(n), &
         &obj%tau(n).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    if ( USE_PIVOTING ) then
       obj%jPivot = 0
    else
       obj%jPivot = [(i, i=1, n)]
    end if
    obj%tau = 0._FLOAT
    
    ! LAPACK routine: turns the stored, transposed Vandermonde matrix into
    ! QR factorisation information.  This will be needed for computing a
    ! particular solution when the RHS vector is known.  A permutation
    ! vector for the original matrix is also returned.
    call geqp3( obj%rows, obj%jPivot, obj%tau, info )
    call addEvent( info/=0, WARNING, 'Problem reported by geqp3.  &
         &INFO='//int2str(info), log )
    
    call endSub( log )
  end subroutine performQRDecomposition_ScalarMatrix
  

  subroutine buildNullVectors_ScalarMatrix( obj, log )
    class(ScalarMatrixType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat, m, n, i, j, info
    call beginSub( MOD_NAME, 'buildNullVectors_ScalarMatrix', log )
    
    n = obj%getNumRows()
    m = obj%getNumColumns()
    if ( n >= m ) then
       ! this method may not be applicable
       call endSub( log )
       return
    end if
    
    ! The last (n-r) columns of Q represent the null space and are needed
    ! for deferring the final solution of the primitive profile.  Allocate
    ! the columns.
    allocate( obj%Q2(m, m - n), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%Q2(&
         &m, m - n).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! We need to initialise the last (n-r) columns of an identity matrix
    ! and ask the LAPACK routine ORMQR to multiply it by Q.  There is
    ! alternative routine, ORGQR, that simply generates the matrix Q, but
    ! it is 'economy size' and does not include the null vectors needed.
    obj%Q2 = 0._FLOAT
    forall ( j = 1:(m - n) ) obj%Q2(j + n, j) = 1._FLOAT
    
    ! "... obj%Qnull(m - n + j, j) = 1._FLOAT" was a mistake in
    ! the previous revision(s) that weirdly passed the test at the time
    
    ! LAPACK routine: multiplication by Q.
    call ormqr( obj%rows, obj%tau, obj%Q2, side='L', trans='N', info=&
         info )
    call addEvent( info/=0, WARNING, 'Problem reported by ormqr.  &
         &INFO='//int2str(info), log )
    
    call endSub( log )
  end subroutine buildNullVectors_ScalarMatrix

  
  pure subroutine solve_ScalarMatrix( obj, columnVector, log )
    class(ScalarMatrixType), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(inout) :: columnVector
    class(LogType), intent(inout), optional :: log
    integer :: n, info
    call beginSub( MOD_NAME, 'solve_ScalarMatrix', log )

    if ( .not. obj%factorised ) then
       call addEvent( WARNING, 'Matrix not factorised.  Method &
            &aborted.', log )
       call endSub( log )
       return
    end if

    n = size(obj%rows, 2)

    if ( obj%getNumRows() > obj%getNumColumns() ) then
       ! overdetermined.

       ! LAPACK routine (to follow QR factorisation): solve
       ! transpose(Q)*y1 = b for y1.  y1 overwrites y1.
       call ormqr( obj%rows, obj%tau, columnVector, side='L', trans='T' )
       
       ! LAPACK routine: solve R*x1 = y1 for x1.  x1 overwrites y1.
       call trsm( obj%rows(1:n, 1:n), columnVector(1:n, 1:1), side='L', &
            uplo='U', transa='N', diag='N', alpha=1._FLOAT )

    else
       ! when underdetermined, we need to permutate the RHS.  Be warned,
       ! this is more a pattern than a rule.  Well-determined systems have
       ! not been tested yet, but it is presumed they fall into this
       ! category.
       columnVector(1:n, 1:1) = columnVector( obj%jPivot, 1:1 )
       
       ! LAPACK routine (to follow QR factorisation): solve
       ! transpose(R)*y1 = b for y1.  y1 overwrites b.
       n = obj%getNumRows()
       call trsm( obj%rows(1:n, 1:n), columnVector(1:n, 1:1), side='L', &
            uplo='U', transa='T', diag='N', alpha=1._FLOAT )

       ! LAPACK routine: solve Q*x1 = y1 for x1.  x1 overwrites y1.
       call ormqr( obj%rows, obj%tau, columnVector, side='L', trans='N' )
    end if
    
    call endSub( log )
  end subroutine solve_ScalarMatrix


  subroutine permutate_ScalarMatrix( obj, columnVector, log )
    class(ScalarMatrixType), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(inout) :: columnVector
    class(LogType), intent(inout), optional :: log 
    call beginSub( MOD_NAME, 'permutate_ScalarMatrix', log )

    if ( .not. obj%factorised ) then
       call addEvent( WARNING, 'Matrix not factorised.  Method &
            &aborted.', log )
       call endSub( log )
       return
    end if

    columnVector( 1:size(obj%jPivot), 1:1 ) = &
         columnVector( obj%jPivot, 1:1 )
    
    call endSub( log )
  end subroutine permutate_ScalarMatrix


  pure function getNumRows_ScalarMatrix( obj )
    class(ScalarMatrixType), intent(in) :: obj
    integer :: getNumRows_ScalarMatrix
    if ( obj%transposed ) then
       getNumRows_ScalarMatrix = size(obj%rows, 2)
    else
       getNumRows_ScalarMatrix = size(obj%rows, 1)
    end if
  end function getNumRows_ScalarMatrix


  pure function getNumColumns_ScalarMatrix( obj )
    class(ScalarMatrixType), intent(in) :: obj
    integer :: getNumColumns_ScalarMatrix
    if ( obj%transposed ) then
       getNumColumns_ScalarMatrix = size(obj%rows, 1)
    else
       getNumColumns_ScalarMatrix = size(obj%rows, 2)
    end if
  end function getNumColumns_ScalarMatrix


  subroutine prepareToReset_ScalarMatrix( obj, log )
    class(ScalarMatrixType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'prepareToReset_ScalarMatrix', log )
    
    ! these components may not have been allocated, so don't worry if
    ! we get STAT=ERROR_NOT_ALLOCATED.
    deallocate( obj%Q2, obj%jPivot, obj%tau, stat=stat )
    call addEvent( stat/=0 .and. stat/=ERROR_NOT_ALLOCATED, WARNING, &
         'Problem deallocating obj%Q2, obj%jPivot, obj%tau.  STAT='//&
         int2str(stat), log )
    obj%factorised = .false.

    obj%rowCounter = 0
    obj%columnCounter = 0

    call endSub( log )
  end subroutine prepareToReset_ScalarMatrix
  

  !-------------------------------------------------------------------
  !- ScalarMultiRowMatrix methods
  !-------------------------------------------------------------------
    
  EXPAND({PROCEDURE_CREATE({ScalarMultiRowMatrix}, {ScalarMatrix}, {&
       nRows, nColumns, transposed, behaveAsWholeArray, &
       blockDiagonal, },
  {integer, intent(in) :: nRows, nColumns
    logical, intent(in), optional :: transposed, behaveAsWholeArray, &
         blockDiagonal})})
    
  subroutine init_ScalarMultiRowMatrix( obj, nRows, nColumns, &
       transposed, behaveAsWholeArray, blockDiagonal, log )
    class(ScalarMultiRowMatrixType), intent(inout) :: obj
    integer, intent(in) :: nRows, nColumns
    logical, intent(in), optional :: transposed, behaveAsWholeArray, &
         blockDiagonal
    class(LogType), intent(inout), optional :: log
    integer :: m, n, stat
    call beginSub( MOD_NAME, 'init_ScalarMultiRowMatrix', log )

    call obj%initAbstract( nRows, nColumns, transposed, &
         behaveAsWholeArray, log )
    if ( present(blockDiagonal) ) then
       obj%blockDiagonal = blockDiagonal
    else
       obj%blockDiagonal = .false.
    end if
    
    call endSub( log )
  end subroutine init_ScalarMultiRowMatrix

  
  subroutine deinit_ScalarMultiRowMatrix( obj, log )
    class(ScalarMultiRowMatrixType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log 
    call beginSub( MOD_NAME, 'deinit_ScalarMultiRowMatrix', log )
    
    obj%blockDiagonal = .false. 
    call obj%deinitAbstract( log )
    
    call endSub( log )
  end subroutine deinit_ScalarMultiRowMatrix

  
  pure subroutine setRows_real_ScalarMultiRowMatrix( obj, rows, log )
    class(ScalarMultiRowMatrixType), intent(inout) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: rows
        class(LogType), intent(inout), optional :: log
    integer :: i1, i2, j1, j2 
    call beginSub( MOD_NAME, 'setRows_ScalarMultiRowMatrix', log )
    
    ! identify the rows
    i1 = obj%rowCounter + 1
    i2 = obj%rowCounter + size(rows, 1)

    call addEvent( i2 > obj%getNumRows(), FATAL, 'Target end row ('//str(&
         i2)//') is beyond the extent of the matrix ('//str(obj%&
         getNumRows())//').', log )
    if ( i2 > obj%getNumRows() ) then
       call endSub( log )
       return
    end if

    ! limit the columns if this is a block diagonal matrix
    if ( obj%blockDiagonal ) then
       j1 = obj%columnCounter + 1
       j2 = obj%columnCounter + size(rows, 2)
    else
       j1 = 1
       j2 = size(rows, 2)
    end if
    
    ! copy (with transposition if necessary)
    if ( obj%transposed ) then
       obj%rows(j1:j2, i1:i2) = transpose(rows)
    else
       obj%rows(i1:i2, j1:j2) = rows
    end if
    
    ! bump counter(s)
    obj%rowCounter = i2
    if ( obj%blockDiagonal ) obj%columnCounter = j2
    
    
    call endSub( log )
  end subroutine setRows_real_ScalarMultiRowMatrix

  
  pure subroutine setRows_LinOp_ScalarMultiRowMatrix( obj, &
       scalarLinearOperation, log )
    class(ScalarMultiRowMatrixType), intent(inout) :: obj
    class(PureScalarLinearOperationInterface), intent(in) :: &
         scalarLinearOperation
    class(LogType), intent(inout), optional :: log
    integer :: i1, i2, j1, j2
    logical, dimension(3) :: vacancies
    call beginSub( MOD_NAME, 'setRows_LinOp_ScalarMultiRowMatrix', log )

    ! check operands
    call scalarLinearOperation%check( vacancies, log )
    if ( any(vacancies) ) then
       call endSub( log )
       return
    end if
    
    ! identify the rows
    i1 = obj%rowCounter + 1
    i2 = obj%rowCounter + scalarLinearOperation%getNumRows()

    call addEvent( i2 > obj%getNumRows(), FATAL, 'Target end row ('//str(&
         i2)//') is beyond the extent of the matrix ('//str(obj%&
         getNumRows())//').', log )
    if ( i2 > obj%getNumRows() ) then
       call endSub( log )
       return
    end if
    
    ! limit the columns if this is a block diagonal matrix
    if ( obj%blockDiagonal ) then
       j1 = obj%columnCounter + 1
       j2 = obj%columnCounter + scalarLinearOperation%getNumColumns()
    else
       j1 = 1
       j2 = scalarLinearOperation%getNumColumns()
    end if
    
    ! copy (with transposition if necessary)
    call scalarLinearOperation%perform( obj%rows(i1:i2, j1:j2), &
         obj%transposed )
    
    ! bump counter(s)
    obj%rowCounter = i2
    if ( obj%blockDiagonal ) obj%columnCounter = j2
    
    call endSub( log )
  end subroutine setRows_LinOp_ScalarMultiRowMatrix
  
  
  EXPAND({METHODDEFS_MULTIROWMATRIX_ACCEPT({Scalar})})

  
  !-------------------------------------------------------------------
  !- ScalarSingleRowMatrix methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_CREATE({ScalarSingleRowMatrix}, {ScalarMatrix}, {&
       nColumns, transposed, behaveAsWholeArray, segmented, },
    {integer, intent(in) :: nColumns
    logical, intent(in), optional :: transposed, behaveAsWholeArray, &
         segmented})})
    
  subroutine init_ScalarSingleRowMatrix( obj, nColumns, transposed, &
       behaveAsWholeArray, segmented, log )
    class(ScalarSingleRowMatrixType), intent(inout) :: obj
    integer, intent(in) :: nColumns
    logical, intent(in), optional :: transposed, behaveAsWholeArray, &
         segmented
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_ScalarSingleRowMatrix', log )
    
    call obj%initAbstract( 1, nColumns, transposed, behaveAsWholeArray, &
         log )
    if ( present(segmented) ) then
       obj%segmented = segmented
    else
       obj%segmented = .false.
    end if
    
    call endSub( log )
  end subroutine init_ScalarSingleRowMatrix

  
  subroutine deinit_ScalarSingleRowMatrix( obj, log )
    class(ScalarSingleRowMatrixType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log 
    call beginSub( MOD_NAME, 'deinit_ScalarSingleRowMatrix', log )
    
    obj%segmented = .false. 
    call obj%deinitAbstract( log )
    
    call endSub( log )
  end subroutine deinit_ScalarSingleRowMatrix

  
  pure subroutine setRows_real_ScalarSingleRowMatrix( obj, rows, log )
    class(ScalarSingleRowMatrixType), intent(inout) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: rows
        class(LogType), intent(inout), optional :: log
    integer :: j1, j2 
    call beginSub( MOD_NAME, 'setRows_real_ScalarSingleRowMatrix', log )

    call addEvent( obj%rowCounter > obj%getNumRows(), FATAL, 'Target row &
         &is beyond the extent of the matrix.', log )
    if ( obj%rowCounter > obj%getNumRows() ) then
       call endSub( log )
       return
    end if
    
    ! limit the columns if this is a segmented matrix
    if ( obj%segmented ) then
       j1 = obj%columnCounter + 1
       j2 = obj%columnCounter + size(rows, 2)
    else
       j1 = 1
       j2 = size(rows, 2)
    end if
    
    ! copy (with transposition if necessary)
    if ( obj%transposed ) then
       obj%rows(j1:j2, 1:1) = transpose(rows(1:1, :))
    else
       obj%rows(1:1, j1:j2) = rows(1:1, :)
    end if
    
    ! bump counter(s)
    obj%rowCounter = 1
    if ( obj%segmented ) then
       obj%columnCounter = j2
    end if
    
    call endSub( log )
  end subroutine setRows_real_ScalarSingleRowMatrix

  
  pure subroutine setRows_LinOp_ScalarSingleRowMatrix( obj, &
       scalarLinearOperation, log )
    class(ScalarSingleRowMatrixType), intent(inout) :: obj
    class(PureScalarLinearOperationInterface), intent(in) :: &
         scalarLinearOperation
    class(LogType), intent(inout), optional :: log
    integer :: j1, j2 
    logical, dimension(3) :: vacancies
    call beginSub( MOD_NAME, 'setRows_LinOp_ScalarSingleRowMatrix', log )

    ! check operands
    call scalarLinearOperation%check( vacancies, log )
    if ( any(vacancies) ) then
       call endSub( log )
       return
    end if

    call addEvent( obj%rowCounter > obj%getNumRows(), FATAL, 'Target row &
         &is beyond the extent of the matrix.', log )
    if ( obj%rowCounter > obj%getNumRows() ) then
       call endSub( log )
       return
    end if
    
    ! limit the columns if this is a segmented matrix
    if ( obj%segmented ) then
       j1 = obj%columnCounter + 1
       j2 = obj%columnCounter + scalarLinearOperation%getNumColumns()
    else
       j1 = 1
       j2 = scalarLinearOperation%getNumColumns()
    end if
    
    ! copy (with transposition if necessary)
    call scalarLinearOperation%perform( obj%rows(j1:j2, 1:1), obj%&
         transposed )
    
    ! bump counter(s)
    obj%rowCounter = 1
    if ( obj%segmented ) then
       obj%columnCounter = j2
    end if
    
    call endSub( log )
  end subroutine setRows_LinOp_ScalarSingleRowMatrix
  

  EXPAND({METHODDEFS_SINGLEROWMATRIX_ACCEPT({Scalar})})


  !-------------------------------------------------------------------
  !- ScalarMatrix OperandBehaviour methods
  !-------------------------------------------------------------------
  
  METHODDEFS_MATRIX_OPERANDBEHAVIOUR({Scalar})

  
  !-------------------------------------------------------------------
  !- ScalarRHSColumn methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_CREATE({ScalarRHSColumn},
    {ScalarRHSColumn}, {nElements, nRHSGenerators, &
         rhsGeneratorSeed, behaveAsWholeArray, &
         counterIsFrozen, scalarRHSPreGenerator, },
    {integer, intent(in) :: nElements, nRHSGenerators
    class(ScalarRHSGeneratorInterface), intent(in) :: rhsGeneratorSeed
    logical, intent(in), optional :: behaveAsWholeArray, counterIsFrozen
    class(ScalarRHSGeneratorInterface), allocatable, intent(inout), &
         optional :: scalarRHSPreGenerator
    })})
  
  subroutine init_ScalarRHSColumn( obj, nElements, nRHSGenerators, &
       rhsGeneratorSeed, behaveAsWholeArray, counterIsFrozen, &
       scalarRHSPreGenerator, log )
    class(ScalarRHSColumnType), intent(inout) :: obj
    integer, intent(in) :: nElements, nRHSGenerators
    class(ScalarRHSGeneratorInterface), intent(in) :: rhsGeneratorSeed
    logical, intent(in), optional :: behaveAsWholeArray, counterIsFrozen
    class(ScalarRHSGeneratorInterface), allocatable, intent(inout), &
         optional :: scalarRHSPreGenerator
    class(LogType), intent(inout), optional :: log
    integer :: i, stat
    logical :: bwa, fc
    call beginSub( MOD_NAME, 'init_ScalarRHSColumn', log )

    ! to allocate the correct type of RHSGenerator, use the passed-in
    ! 'seed'.
    call rhsGeneratorSeed%allocArray( obj%scalarRHSGenerators, &
         nRHSGenerators, log )
    
    ! the obj%from array needs an extra element to make iteration more
    ! convenient
    allocate( obj%from(nRHSGenerators + 1), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%from(&
         &nRHSGenerators + 1).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%from = 0
    obj%from(1) = 1
    
    allocate( obj%realValues(nElements, 1), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%realValues(&
         &nElements, 1).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%realValues = 0._FLOAT
    
    ! default behaveAsWholeArray argument
    if ( present(behaveAsWholeArray) ) then
       bwa = behaveAsWholeArray
    else
       bwa = .false. 
    end if

    if ( bwa ) then
       allocate( ScalarRHSColumn_WholeArrayBehaviourType :: obj%&
            operandBehaviour, stat=stat )
    else
       allocate( ScalarRHSColumn_SequentialSectionBehaviourType :: obj%&
            operandBehaviour, stat=stat )
    end if
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%&
         &behaveAsWholeArray.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    ! default counterIsFrozen argument
    if ( present(counterIsFrozen) ) then
       fc = counterIsFrozen
    else
       fc = .false. 
    end if

    obj%elementCounter = 0
    
    obj%counterIsFrozen = fc

    if ( present(scalarRHSPreGenerator) ) then
       EXPAND({INJECT({scalarRHSPreGenerator})})
       obj%nPre = obj%scalarRHSPreGenerator%getNumElements()
    else
       obj%nPre = 0
    end if
    
    call endSub( log )
  end subroutine init_ScalarRHSColumn

  
  subroutine deinit_ScalarRHSColumn( obj, log )
    class(ScalarRHSColumnType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_ScalarRHSColumn', log )
    
    call destroy( obj%scalarRHSGenerators, log )

    deallocate( obj%realValues, obj%from, obj%&
         operandBehaviour, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating obj%&
         &realValues, obj%from, obj%operandBehaviour.  &
         &STAT='//int2str(stat), log )
    
    obj%elementCounter = 0
    obj%counterIsFrozen = .false.

    deallocate( obj%scalarRHSPreGenerator, stat=stat )
    call addEvent( stat/=0 .and. stat/=ERROR_NOT_ALLOCATED, WARNING, &
         'Problem deallocating obj%scalarRHSPreGenerator.  STAT='//&
         int2str(stat), log )
    obj%nPre = 0
    
    call endSub( log )
  end subroutine deinit_ScalarRHSColumn
  
  
  ! CAUTION - the RHS elements can be updated in two ways.  One is with
  ! RHSGenerator objects; the other is with real values set directly.
  ! Both ways use the same element counter (though they shouldn't
  ! overlap).
  
  pure subroutine setElements_real_ScalarRHSColumn( obj, realValues, log )
    class(ScalarRHSColumnType), intent(inout) :: obj
    real(FLOAT), dimension(:), intent(in) :: realValues
    class(LogType), intent(inout), optional :: log
    integer :: i1, i2, i
    call beginSub( MOD_NAME, 'setElements_real_ScalarRHSColumn', log )
    
    i1 = obj%elementCounter + 1
    i2 = obj%elementCounter + size(realValues)

    call addEvent( i2 > obj%getNumElements(), FATAL, 'Target end &
         &element ('//str(i2)//') is beyond the extent of the matrix ('//&
         str(obj%getNumElements())//').', log )
    if ( i2 > obj%getNumElements() ) then
       call endSub( log )
       return
    end if

    obj%realValues(i1:i2, 1) = realValues

    if ( .not. obj%counterIsFrozen ) obj%elementCounter = i2
    
    call endSub( log )
  end subroutine setElements_real_ScalarRHSColumn

  
  pure subroutine setElements_RHSGen_ScalarRHSColumn( obj, &
       scalarRHSGenerator, log )
    class(ScalarRHSColumnType), intent(inout) :: obj
    class(ScalarRHSGeneratorInterface), intent(inout) :: scalarRHSGenerator
    class(LogType), intent(inout), optional :: log
    integer :: i1, i2, j
    call beginSub( MOD_NAME, 'setElements_RHSGen_ScalarRHSColumn', log )
    
    i1 = obj%elementCounter + 1
    i2 = obj%elementCounter + scalarRHSGenerator%getNumElements()
    
    call addEvent( i2 > obj%getNumElements(), FATAL, 'Target end &
         &element ('//str(i2)//') is beyond the extent of the real-&
         &valued RHS column vector ('//str(obj%getNumElements())//').', &
         log )
    if ( i2 > obj%getNumElements() ) then
       call endSub( log )
       return
    end if
    
    ! the index of the last non-zero element of the 'from' array gives us
    ! the index of the next generator
    j = count( obj%from /= 0 )
    
    ! set the next generator
    call obj%scalarRHSGenerators(j)%init( scalarRHSGenerator )

    ! update the real element counter and the 'from' array if appropriate.
    ! Warning: when the counter is frozen, the from array cannot be used
    ! (because the upper bound for i = from(i+1) - 1 = 0) except to count
    ! the number of initialised generators.
    if ( obj%counterIsFrozen ) then
       obj%from(j + 1) = 1
    else
       obj%elementCounter = i2
       obj%from(j + 1) = i2 + 1
    end if
    
    call endSub( log )
  end subroutine setElements_RHSGen_ScalarRHSColumn


  pure function getNumElements_ScalarRHSColumn( obj, generatorIndex )
    class(ScalarRHSColumnType), intent(in) :: obj
    integer, intent(in), optional :: generatorIndex
    integer :: getNumElements_ScalarRHSColumn
    if ( present(generatorIndex) ) then
       ! return the number of elements for this generator only.  Do the
       ! necessary checks.  Return 0 in case of errors
       getNumElements_ScalarRHSColumn = 0
       if ( .not. allocated(obj%scalarRHSGenerators) ) return
       if ( generatorIndex <= 0 .or. generatorIndex > size(obj%&
            scalarRHSGenerators) ) return
       if ( .not. obj%scalarRHSGenerators(generatorIndex)%&
            isInitialised() ) return
       
       getNumElements_ScalarRHSColumn = &
            obj%scalarRHSGenerators(generatorIndex)%getNumElements()
    else
       getNumElements_ScalarRHSColumn = size(obj%realValues)
    end if
  end function getNumElements_ScalarRHSColumn


  pure subroutine refresh_ScalarRHSColumn( obj, values )
    class(ScalarRHSColumnType), intent(inout) :: obj
    real(FLOAT), dimension(:, :), intent(out), optional :: values
    integer :: j, i1, i2

    if ( allocated(obj%scalarRHSPreGenerator) ) then
       i1 = 1
       i2 = obj%nPre
       call obj%scalarRHSPreGenerator%getValues( &
            obj%realValues(i1:i2, 1:1) )
    end if

    if ( obj%counterIsFrozen ) then
       i1 = 1
       i2 = size(obj%realValues, 1)
       
       ! wanted this to be a forall block, but the compiler complained
       do j = 1, count(obj%scalarRHSGenerators%isInitialised())
          call obj%scalarRHSGenerators(j)%getValues( obj%realValues(i1:&
               i2, 1:1) )
       end do
       
    else
       ! wanted this to be a forall block, but the compiler complained
       do j = 1, count(obj%scalarRHSGenerators%isInitialised())
          i1 = obj%from(j)
          i2 = obj%from(j + 1) - 1
          
          call obj%scalarRHSGenerators(j)%getValues( obj%realValues(&
               i1:i2, 1:1) )
       end do
    end if
    ! pad the remaining elements with zeroes.  Not doing so may affect
    ! the particular solution and upset the unit tests.
    obj%realValues(i2+1:size(obj%realValues, 1), 1:1) = 0._FLOAT
    
    if ( present(values) ) then
       i1 = 1
       i2 = obj%getNumElements()
       values( i1:i2, 1:1 ) = obj%realValues
       values( i2+1:size(values), 1:1 ) = 0._FLOAT
    end if
  end subroutine refresh_ScalarRHSColumn

  
  subroutine prepareToReset_ScalarRHSColumn( obj, log )
    class(ScalarRHSColumnType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, &
         'prepareToResetElements_ScalarRHSColumn', log )
    
    ! flush the RHSGenerators
    do i = 1, size(obj%scalarRHSGenerators)
       call obj%scalarRHSGenerators(i)%deinit( log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
    end do
    
    obj%from = 0
    obj%from(1) = 1
    obj%elementCounter = 0

    call endSub( log )
  end subroutine prepareToReset_ScalarRHSColumn
  
  
  EXPAND({METHODDEFS_CONCRETERHSCOLUMN_ACCEPT({Scalar})})

  
  !-------------------------------------------------------------------
  !- RHSColumn OperandBehaviour methods
  !-------------------------------------------------------------------
  
  METHODDEFS_RHSCOLUMN_OPERANDBEHAVIOUR({Scalar})

  
  !-------------------------------------------------------------------
  !- ScalarLinearSystem methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_CREATE({ScalarLinearSystem}, {ScalarLinearSystem},
  {&
       scalarMatrix, scalarRHSColumn, }, {
    class(ScalarMatrixInterface), allocatable, intent(inout) :: &
         scalarMatrix
    class(ScalarRHSColumnInterface), allocatable, intent(inout) :: &
         scalarRHSColumn})})
    
  subroutine init_ScalarLinearSystem( obj, scalarMatrix, &
       scalarRHSColumn, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(ScalarMatrixInterface), allocatable, intent(inout) :: &
         scalarMatrix
    class(ScalarRHSColumnInterface), allocatable, intent(inout) :: &
         scalarRHSColumn
    class(LogType), intent(inout), optional :: log
    integer :: n, stat
    call beginSub( MOD_NAME, 'init_ScalarLinearSystem', log )
    
    call addEvent( scalarMatrix%getNumRows() /= scalarRHSColumn%&
         getNumElements(), FATAL, 'Number of matrix rows does not equal &
         &number of RHS elements.', log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    EXPAND({INJECT({scalarMatrix})})
    
    EXPAND({INJECT({scalarRHSColumn})})

    ! strictly speaking the solution vector will have as many elements as
    ! matrix columns.  But the linear algebra routines often require us to
    ! store the RHS in the solution vector.  So we need to make the vector
    ! big enough.
    n = max( obj%scalarMatrix%getNumColumns(), obj%scalarMatrix%&
         getNumRows() )
    
    allocate( obj%solution(n, 1), stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating obj%solution(n, &
         &1).  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%solution = 0
    obj%visitationCounter = 1
    obj%factorised = .false.
    
    call endSub( log )
  end subroutine init_ScalarLinearSystem

  
  subroutine deinit_ScalarLinearSystem( obj, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log 
    integer :: stat
    call beginSub( MOD_NAME, 'deinit_ScalarLinearSystem', log )

    obj%visitationCounter = 1
    obj%factorised = .false.
    
    deallocate( obj%solution, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating obj%&
         &solution.  STAT='//int2str(stat), log )
    call destroy( obj%scalarMatrix, log )
    call destroy( obj%scalarRHSColumn, log )
    
    call endSub( log )
  end subroutine deinit_ScalarLinearSystem
  

  pure subroutine setMatrixRows_real_ScalarLinearSystem( obj, rows, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    real(FLOAT), dimension(:, :), intent(in) :: rows
        class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'setMatrixRows_real_ScalarLinearSystem', &
         log )
    
    call obj%scalarMatrix%setRows( rows, log )
    
    call endSub( log )
  end subroutine setMatrixRows_real_ScalarLinearSystem
  

  pure subroutine setMatrixRows_LinOp_ScalarLinearSystem( obj, &
       scalarLinearOperation, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(PureScalarLinearOperationInterface), intent(in) :: &
         scalarLinearOperation
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'setMatrixRows_LinOp_ScalarLinearSystem', &
         log )
    
    call obj%scalarMatrix%setRows( scalarLinearOperation, log )
    
    call endSub( log )
  end subroutine setMatrixRows_LinOp_ScalarLinearSystem
  

  pure subroutine setRHSElements_real_ScalarLinearSystem( obj, &
       realValues, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    real(FLOAT), dimension(:), intent(in) :: realValues
    class(LogType), intent(inout), optional :: log 
    call beginSub( MOD_NAME, 'setRHSElements_real_ScalarLinearSystem', &
         log )
    
    call obj%scalarRHSColumn%setElements( realValues, log )
    
    call endSub( log )
  end subroutine setRHSElements_real_ScalarLinearSystem

     
  pure subroutine setRHSElements_RHSGen_ScalarLinearSystem( obj, &
       scalarRHSGenerator, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(ScalarRHSGeneratorInterface), intent(inout) :: scalarRHSGenerator
    class(LogType), intent(inout), optional :: log 
    call beginSub( MOD_NAME, 'setRHSElements_RHSGen_ScalarLinearSystem', &
         log )
    
    call obj%scalarRHSColumn%setElements( scalarRHSGenerator, log )
    
    call endSub( log )
  end subroutine setRHSElements_RHSGen_ScalarLinearSystem
  

  pure subroutine setSolutionElements_real_ScalarLinearSystem( obj, &
       realValues, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    real(FLOAT), dimension(:), intent(in) :: realValues
    class(LogType), intent(inout), optional :: log 
    call beginSub( MOD_NAME, &
         'setSolutionElements_real_ScalarLinearSystem', log )

    obj%solution(:, 1) = realValues
    
    call endSub( log )
  end subroutine setSolutionElements_real_ScalarLinearSystem

  
  pure subroutine setSolutionElements_LinOp_ScalarLinearSystem( obj, &
       scalarLinearOperation, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(PureScalarLinearOperationInterface), intent(in) :: &
         scalarLinearOperation
    class(LogType), intent(inout), optional :: log
    logical, dimension(3) :: vacancies
    call beginSub( MOD_NAME, &
         'setSolutionElements_LinOp_ScalarLinearSystem', log )

    ! check operands
    call scalarLinearOperation%check( vacancies, log )
    if ( any(vacancies) ) then
       call endSub( log )
       return
    end if
    
    call scalarLinearOperation%perform( obj%solution )
    
    call endSub( log )
  end subroutine setSolutionElements_LinOp_ScalarLinearSystem


  subroutine factorise_ScalarLinearSystem( obj, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log 
    call beginSub( MOD_NAME, 'factorise_ScalarLinearSystem', log )

    call addEvent( obj%factorised, ADVICE, 'System already factorised.', &
         log )
    if ( obj%factorised ) then
       call endSub( log )
       return
    end if
    
    call obj%scalarMatrix%factorise( log )
    obj%factorised = .true. 
        
    call endSub( log )
  end subroutine factorise_ScalarLinearSystem
    
  
  pure subroutine solve_ScalarLinearSystem( obj, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'solve_ScalarLinearSystem', log )

    ! the solution vector may be computed as a particular (incomplete)
    ! solution.  Refresh the RHS and then pass it to the matrix's solve
    ! method.  It is important that clients do not refresh the RHS
    ! themselves, because the RHS elements may be recursively altered when
    ! LinearOperation-based.
    call obj%scalarRHSColumn%refresh( obj%solution )
    
    ! continue only if factorisation has been carried out
    if ( obj%factorised ) then
       call obj%scalarMatrix%solve( obj%solution, log )
    end if

    call endSub( log )
  end subroutine solve_ScalarLinearSystem


  subroutine permutateSolution_ScalarLinearSystem( obj, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'permutateSolution_ScalarLinearSystem', &
         log )
    
    call obj%scalarMatrix%permutate( obj%solution, log )

    call endSub( log )
  end subroutine permutateSolution_ScalarLinearSystem

  
  pure function getNumEquations_ScalarLinearSystem( obj ) result ( &
       nEquations )
    class(ScalarLinearSystemType), intent(in) :: obj
    integer :: nEquations
    nEquations = obj%scalarMatrix%getNumRows()
  end function getNumEquations_ScalarLinearSystem


  subroutine prepareToReset_ScalarLinearSystem( obj, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'prepareToReset_ScalarLinearSystem', log )

    call obj%scalarMatrix%prepareToReset( log )
    call obj%scalarRHSColumn%prepareToReset( log )

    call endSub( log )
  end subroutine prepareToReset_ScalarLinearSystem


  subroutine prepareToBeVisited_ScalarLinearSystem( obj, log )
    class(ScalarLinearSystemType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'prepareToBeVisited_ScalarLinearSystem', log )

    obj%visitationCounter = 0
    obj%solutionElementCounter = 0

    call endSub( log )
  end subroutine prepareToBeVisited_ScalarLinearSystem

  
  EXPAND({METHODDEF_CONCRETELINEARSYSTEM_ACCEPT({Scalar}, {1})})
  
  EXPAND({METHODDEF_CONCRETELINEARSYSTEM_ACCEPT({Scalar}, {2})})
  
  EXPAND({METHODDEF_CONCRETELINEARSYSTEM_ACCEPT({Scalar}, {3})})

    
end module LinearSystemsModule
