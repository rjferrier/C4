
  !-------------------------------------------------------------------
  !- types
  !-------------------------------------------------------------------
  
  MACRO({METHODLIST_LINEAROPERATION_CONCRETE}, {{
     procedure :: deinit => deinit_$1
     m4_ifelse($2, {}, {}, {procedure :: visit$2ForOperand1 => &
          visit$2ForOperand1_$1})
     m4_ifelse($3, {}, {}, {procedure :: visit$3ForOperand2 => &
          visit$3ForOperand2_$1})
     m4_ifelse($4, {}, {}, {procedure :: visit$4ForOperand3 => &
          visit$4ForOperand3_$1})
     procedure :: getNumRows => getNumRows_$1
     procedure :: getNumColumns => getNumColumns_$1
     procedure :: check => check_$1
     procedure :: perform => perform_$1
     procedure :: clone => clone_$1
     }})

  
  MACRO({TYPEDEF_COPY_A}, {{
  type, extends(Pure$1LinearOperationInterface) :: Copy_A_$1Type
     real(FLOAT), dimension(:, :), pointer :: operand1 => null()
   contains
     EXPAND({METHODLIST_LINEAROPERATION_CONCRETE(
     {Copy_A_$1}, {Matrix})})
  end type Copy_A_$1Type
  }})


  MACRO({TYPEDEF_COPY_B}, {{
  type, extends(Pure$1LinearOperationInterface) :: Copy_B_$1Type
     real(FLOAT), dimension(:, :), pointer :: operand1 => null()
   contains
     EXPAND({METHODLIST_LINEAROPERATION_CONCRETE(
     {Copy_B_$1}, {RHSColumn})})
  end type Copy_B_$1Type
  }})

  
  MACRO({TYPEDEF_A_TIMES_Q2}, {{
  type, extends(Pure$1LinearOperationInterface) :: A_Times_Q2_$1Type
     real(FLOAT), dimension(:, :), pointer :: operand1 => null()
     real(FLOAT), dimension(:, :), pointer :: operand2 => null()
   contains
     EXPAND({METHODLIST_LINEAROPERATION_CONCRETE(
     {A_Times_Q2_$1},
     {Matrix}, {NullSpace})})
  end type A_Times_Q2_$1Type
  }})


  MACRO({TYPEDEF_B_MINUS_A_TIMES_X}, {{
  type, extends(Pure$1LinearOperationInterface) :: B_Minus_A_Times_X_$1Type
     private
     real(FLOAT), dimension(:, :), pointer :: operand1 => null()
     real(FLOAT), dimension(:, :), pointer :: operand2 => null()
     real(FLOAT), dimension(:, :), pointer :: operand3 => null()
     integer, dimension(2) :: from1 = 1
   contains
     EXPAND({METHODLIST_LINEAROPERATION_CONCRETE(
     {B_Minus_A_Times_X_$1},
     {RHSColumn}, {Matrix}, {SolutionColumn})})
  end type B_Minus_A_Times_X_$1Type
  }})


  MACRO({TYPEDEF_X_PLUS_Q2_TIMES_X}, {{
  type, extends(Pure$1LinearOperationInterface) :: X_Plus_Q2_Times_X_$1Type
     private
     real(FLOAT), dimension(:, :), pointer :: operand1 => null()
     real(FLOAT), dimension(:, :), pointer :: operand2 => null()
     real(FLOAT), dimension(:, :), pointer :: operand3 => null()
     integer, dimension(2) :: from3 = 1
   contains
     EXPAND({METHODLIST_LINEAROPERATION_CONCRETE(
     {X_Plus_Q2_Times_X_$1},
     {SolutionColumn}, {NullSpace}, {SolutionColumn})})
  end type X_Plus_Q2_Times_X_$1Type
  }})

  
  !-------------------------------------------------------------------
  !- General methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEF_LINEAROPERATION_VISIT_ARRAY}, {{
  subroutine visit$2ForOperand$3_$1( obj, array, from )
    class($1Type), intent(inout) :: obj
    real(FLOAT), dimension(:, :), target, intent(inout) :: array
    integer, dimension(2), intent(in), optional :: from
    obj%operand$3 => array $4
  end subroutine visit$2ForOperand$3_$1
  }})
  
  MACRO({METHODDEF_LINEAROPERATION_CHECK}, {{
  pure subroutine check_$1( obj, vacancies, log )
    class($1Type), intent(in) :: obj
    logical, dimension(:), intent(out), optional :: vacancies
    class(LogType), intent(inout), optional :: log
    integer :: i
    call beginSub( MOD_NAME, 'check_$1', log )

    if ( present(vacancies) ) then
       vacancies = .false.
       m4_ifelse($2, {}, {},
       {vacancies(1) = .not. associated(obj%operand1)
       m4_ifelse($2, {1}, {}, 
       {vacancies(2) = .not. associated(obj%operand2)
       m4_ifelse($2, {2}, {}, 
       {vacancies(3) = .not. associated(obj%operand3)
    })})}){}end if
    
    m4_ifelse($2, {}, {},
    {call addEvent( .not. associated(obj%operand1), WARNING, &
         'Operand 1 has not been set.', log )
    m4_ifelse($2, {1}, {}, 
    {call addEvent( .not. associated(obj%operand2), WARNING, &
         'Operand 2 has not been set.', log )
    m4_ifelse($2, {2}, {}, 
    {call addEvent( .not. associated(obj%operand3), WARNING, &
         'Operand 3 has not been set.', log )
    })})})
    call endSub( log )
  end subroutine check_$1
  }})
  
  MACRO({METHODDEF_LINEAROPERATION_CLONE}, {{
  pure subroutine clone_$2_$1( obj, tgt, log )
    class($2_$1Type), intent(inout) :: obj
    class(Pure$1LinearOperationInterface), allocatable, &
         intent(inout) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_$2_$1', log )
    
    allocate( $2_$1Type :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating $2_$1Type :: &
         &tgt.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    call tgt%setScaleFactor( obj%getScaleFactor() )

    select type ( tgt )
    type is ($2_$1Type)
       $3
    end select

    call endSub( log )
  end subroutine clone_$2_$1
  }})

  
  MACRO({PROCEDURE_CREATE_LINEAROPERATION_CONCRETE}, {{
  subroutine createLinearOperation_$1( pure$1LinearOperation, &
       operationName, log )
    class(Pure$1LinearOperationInterface), allocatable, &
         intent(inout) :: pure$1LinearOperation
    character(*), intent(in) :: operationName
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'createLinearOperation_$1', log )
    
    select case ( operationName )
    case ('A_Times_Q2')
       allocate( A_Times_Q2_$1Type :: pure$1LinearOperation, &
            stat=stat )
    case ('B_Minus_A_Times_X')
       allocate( B_Minus_A_Times_X_$1Type :: pure$1LinearOperation, &
            stat=stat )
    case ('X_Plus_Q2_Times_X')
       allocate( X_Plus_Q2_Times_X_$1Type :: pure$1LinearOperation, &
            stat=stat )
       
    case default
       call addEvent( FATAL, 'operationName not recognised.  Can have &
            &''A_Times_Q2'', ''B_Minus_A_Times_X'', or &
            &''X_Plus_Q2_Times_X''; argument was '''//trim(&
            operationName)//'''.', log )
    end select
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &pure$1LinearOperation.  STAT='//int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    call endSub( log )
  end subroutine createLinearOperation_$1
  }})


  !-------------------------------------------------------------------
  !- Copy_A methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_COPY_A}, {{
  pure subroutine deinit_Copy_A_$1( obj, log )
    class(Copy_A_$1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    nullify(obj%operand1)
  end subroutine deinit_Copy_A_$1

  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY({Copy_A_$1},
  {Matrix}, {1})})
  
  pure function getNumRows_Copy_A_$1( obj )
    class(Copy_A_$1Type), intent(in) :: obj
    integer :: getNumRows_Copy_A_$1
    getNumRows_Copy_A_$1 = size(obj%operand1, 1)
  end function getNumRows_Copy_A_$1


  pure function getNumColumns_Copy_A_$1( obj )
    class(Copy_A_$1Type), intent(in) :: obj
    integer :: getNumColumns_Copy_A_$1
    getNumColumns_Copy_A_$1 = size(obj%operand1, 2)
  end function getNumColumns_Copy_A_$1
  
  EXPAND({METHODDEF_LINEAROPERATION_CHECK({Copy_A_$1}, {1})})
  
  pure subroutine perform_Copy_A_$1( obj, values, transposed )
    class(Copy_A_$1Type), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(inout) :: values
    logical, intent(in), optional :: transposed
    ! intermediate matricies seem to be needed, otherwise the operation
    ! does not proceed properly
    real(FLOAT), dimension(size(obj%operand1, 1), &
         size(obj%operand1, 2)) :: work1
    work1 = obj%operand1
    values = obj%getScaleFactor() * work1
  end subroutine perform_Copy_A_$1

  
  EXPAND({METHODDEF_LINEAROPERATION_CLONE($1, {Copy_A},
       {
       tgt%operand1 => obj%operand1})})
  }})
  


  !-------------------------------------------------------------------
  !- Copy_B methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_COPY_B}, {{
  pure subroutine deinit_Copy_B_$1( obj, log )
    class(Copy_B_$1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    nullify(obj%operand1)
  end subroutine deinit_Copy_B_$1

  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY({Copy_B_$1},
  {RHSColumn}, {1})})
  
  pure function getNumRows_Copy_B_$1( obj )
    class(Copy_B_$1Type), intent(in) :: obj
    integer :: getNumRows_Copy_B_$1
    getNumRows_Copy_B_$1 = size(obj%operand1, 1)
  end function getNumRows_Copy_B_$1


  pure function getNumColumns_Copy_B_$1( obj )
    class(Copy_B_$1Type), intent(in) :: obj
    integer :: getNumColumns_Copy_B_$1
    getNumColumns_Copy_B_$1 = size(obj%operand1, 2)
  end function getNumColumns_Copy_B_$1
  
  EXPAND({METHODDEF_LINEAROPERATION_CHECK({Copy_B_$1}, {1})})
  
  pure subroutine perform_Copy_B_$1( obj, values, transposed )
    class(Copy_B_$1Type), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(inout) :: values
    logical, intent(in), optional :: transposed
    ! intermediate matricies seem to be needed, otherwise the operation
    ! does not proceed properly
    real(FLOAT), dimension(size(obj%operand1, 1), &
         size(obj%operand1, 2)) :: work1
    work1 = obj%operand1
    values = obj%getScaleFactor() * work1
  end subroutine perform_Copy_B_$1

  
  EXPAND({METHODDEF_LINEAROPERATION_CLONE($1, {Copy_B},
       {
       tgt%operand1 => obj%operand1})})
  }})
  

  !-------------------------------------------------------------------
  !- A_Times_Q2 methods
  !-------------------------------------------------------------------
  
  MACRO({METHODDEFS_A_TIMES_Q2}, {{
  pure subroutine deinit_A_Times_Q2_$1( obj, log )
    class(A_Times_Q2_$1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    nullify(obj%operand1)
    nullify(obj%operand2)
  end subroutine deinit_A_Times_Q2_$1

  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY({A_Times_Q2_$1},
  {Matrix}, {1})})
  
  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY({A_Times_Q2_$1},
  {NullSpace}, {2})})
  
  pure function getNumRows_A_Times_Q2_$1( obj )
    class(A_Times_Q2_$1Type), intent(in) :: obj
    integer :: getNumRows_A_Times_Q2_$1
    getNumRows_A_Times_Q2_$1 = size(obj%operand1, 1)
  end function getNumRows_A_Times_Q2_$1


  pure function getNumColumns_A_Times_Q2_$1( obj )
    class(A_Times_Q2_$1Type), intent(in) :: obj
    integer :: getNumColumns_A_Times_Q2_$1
    getNumColumns_A_Times_Q2_$1 = size(obj%operand2, 2)
  end function getNumColumns_A_Times_Q2_$1
  
  EXPAND({METHODDEF_LINEAROPERATION_CHECK({A_Times_Q2_$1}, {2})})
  
  pure subroutine perform_A_Times_Q2_$1( obj, values, transposed )
    class(A_Times_Q2_$1Type), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(inout) :: values
    logical, intent(in), optional :: transposed
    ! intermediate matricies seem to be needed, otherwise the operation
    ! does not proceed properly
    real(FLOAT), dimension(size(obj%operand1, 1), &
         size(obj%operand1, 2)) :: work1
    real(FLOAT), dimension(size(obj%operand2, 1), &
         size(obj%operand2, 2)) :: work2
    work1 = obj%operand1
    work2 = obj%operand2
    if ( present(transposed) ) then
       if ( transposed ) then
          values = transpose( obj%getScaleFactor()*matmul(work1, work2) )
       else
          values = obj%getScaleFactor()*matmul(work1, work2)
       end if
    else
       values = obj%getScaleFactor()*matmul(work1, work2)
    end if
  end subroutine perform_A_Times_Q2_$1

  
  EXPAND({METHODDEF_LINEAROPERATION_CLONE($1, {A_Times_Q2},
       {
       tgt%operand1 => obj%operand1
       tgt%operand2 => obj%operand2})})
  }})
  
  !-------------------------------------------------------------------
  !- B_Minus_A_Times_X methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_B_MINUS_A_TIMES_X}, {{
  pure subroutine deinit_B_Minus_A_Times_X_$1( obj, log )
    class(B_Minus_A_Times_X_$1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    nullify(obj%operand1)
    nullify(obj%operand2)
    nullify(obj%operand3)
    obj%from1 = 0
  end subroutine deinit_B_Minus_A_Times_X_$1
  
  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY(
  {B_Minus_A_Times_X_$1}, {RHSColumn}, {1}, {
    if ( present(from) ) then
       obj%from1 = from
    end if})})
  
  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY(
  {B_Minus_A_Times_X_$1}, {Matrix}, {2})})
  
  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY(
  {B_Minus_A_Times_X_$1}, {SolutionColumn}, {3})})
  
  pure function getNumRows_B_Minus_A_Times_X_$1( obj )
    class(B_Minus_A_Times_X_$1Type), intent(in) :: obj
    integer :: getNumRows_B_Minus_A_Times_X_$1
    getNumRows_B_Minus_A_Times_X_$1 = size(obj%operand2, 1)
  end function getNumRows_B_Minus_A_Times_X_$1
  

  pure function getNumColumns_B_Minus_A_Times_X_$1( obj )
    class(B_Minus_A_Times_X_$1Type), intent(in) :: obj
    integer :: getNumColumns_B_Minus_A_Times_X_$1
    getNumColumns_B_Minus_A_Times_X_$1 = size(obj%operand3, 2)
  end function getNumColumns_B_Minus_A_Times_X_$1

  EXPAND({METHODDEF_LINEAROPERATION_CHECK({B_Minus_A_Times_X_$1}, {3})})
  
  pure subroutine perform_B_Minus_A_Times_X_$1( obj, values, transposed )
    class(B_Minus_A_Times_X_$1Type), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(inout) :: values
    logical, intent(in), optional :: transposed
    integer, dimension(2) :: to1
    ! intermediate matricies seem to be needed, otherwise the operation
    ! does not proceed properly
    real(FLOAT), dimension(size(obj%operand2, 1), &
         size(obj%operand3, 2)) :: work1
    real(FLOAT), dimension(size(obj%operand2, 1), &
         size(obj%operand2, 2)) :: work2
    real(FLOAT), dimension(size(obj%operand3, 1), &
         size(obj%operand3, 2)) :: work3

    ! for the first operand, a section must be taken 
    to1 = obj%from1 - 1 + [obj%getNumRows(), obj%getNumColumns()]
    work1 = obj%operand1( obj%from1(1):to1(1), obj%from1(2):to1(2) )
    work2 = obj%operand2
    work3 = obj%operand3
    if ( present(transposed) ) then
       if ( transposed ) then
          values = work1 - transpose( obj%getScaleFactor()*&
               matmul(work2, work3) )
       else
          values = work1 - obj%getScaleFactor()*matmul(work2, work3)
       end if
    else
       values = work1 - obj%getScaleFactor()*matmul(work2, work3)
    end if
  end subroutine perform_B_Minus_A_Times_X_$1

  
  EXPAND({METHODDEF_LINEAROPERATION_CLONE($1, {B_Minus_A_Times_X},
       {
       tgt%operand1 => obj%operand1
       tgt%operand2 => obj%operand2
       tgt%operand3 => obj%operand3
       tgt%from1 = obj%from1})})
  }})

  
  !-------------------------------------------------------------------
  !- X_Plus_Q2_Times_X methods
  !-------------------------------------------------------------------

  MACRO({METHODDEFS_X_PLUS_Q2_TIMES_X}, {{
  pure subroutine deinit_X_Plus_Q2_Times_X_$1( obj, log )
    class(X_Plus_Q2_Times_X_$1Type), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    nullify(obj%operand1)
    nullify(obj%operand2)
    nullify(obj%operand3)
    obj%from3 = 0
  end subroutine deinit_X_Plus_Q2_Times_X_$1
  
  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY({X_Plus_Q2_Times_X_$1},
  {SolutionColumn}, {1})})
  
  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY({X_Plus_Q2_Times_X_$1},
  {NullSpace}, {2})})
  
  EXPAND({METHODDEF_LINEAROPERATION_VISIT_ARRAY({X_Plus_Q2_Times_X_$1},
  {SolutionColumn}, {3}, {
    if ( present(from) ) then
       obj%from3 = from
    end if})})
  
  pure function getNumRows_X_Plus_Q2_Times_X_$1( obj )
    class(X_Plus_Q2_Times_X_$1Type), intent(in) :: obj
    integer :: getNumRows_X_Plus_Q2_Times_X_$1
    getNumRows_X_Plus_Q2_Times_X_$1 = size(obj%operand1, 1)
  end function getNumRows_X_Plus_Q2_Times_X_$1


  pure function getNumColumns_X_Plus_Q2_Times_X_$1( obj )
    class(X_Plus_Q2_Times_X_$1Type), intent(in) :: obj
    integer :: getNumColumns_X_Plus_Q2_Times_X_$1
    getNumColumns_X_Plus_Q2_Times_X_$1 = size(obj%operand1, 2)
  end function getNumColumns_X_Plus_Q2_Times_X_$1

  EXPAND({METHODDEF_LINEAROPERATION_CHECK({X_Plus_Q2_Times_X_$1}, {3})})
  
  pure subroutine perform_X_Plus_Q2_Times_X_$1( obj, values, transposed )
    class(X_Plus_Q2_Times_X_$1Type), intent(in) :: obj
    real(FLOAT), dimension(:, :), intent(inout) :: values
    logical, intent(in), optional :: transposed
    integer, dimension(2) :: to3
    ! intermediate matricies seem to be needed, otherwise the operation
    ! does not proceed properly
    real(FLOAT), dimension(size(obj%operand1, 1), &
         size(obj%operand1, 2)) :: work1
    real(FLOAT), dimension(size(obj%operand2, 1), &
         size(obj%operand2, 2)) :: work2
    real(FLOAT), dimension(size(obj%operand2, 2), &
         size(obj%operand1, 2)) :: work3
    
    ! for the third operand, a section must be taken 
    to3 = obj%from3 - 1 + [size(work2, 2), size(work1, 2)]
    work1 = obj%operand1
    work2 = obj%operand2
    work3 = obj%operand3( obj%from3(1):to3(1), obj%from3(2):to3(2) )
    if ( present(transposed) ) then
       if ( transposed ) then
          values = transpose( work1 + obj%getScaleFactor()*&
               matmul(work2, work3) )
       else
          values = work1 + obj%getScaleFactor()*matmul(work2, work3)
       end if
    else
       values = work1 + obj%getScaleFactor()*matmul(work2, work3)
    end if
  end subroutine perform_X_Plus_Q2_Times_X_$1
  
  
  EXPAND({METHODDEF_LINEAROPERATION_CLONE($1, {X_Plus_Q2_Times_X},
       {
       tgt%operand1 => obj%operand1
       tgt%operand2 => obj%operand2
       tgt%operand3 => obj%operand3
       tgt%from3 = obj%from3})})
  }})
   
