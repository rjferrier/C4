
  
  !-------------------------------------------------------------------
  !- SimplexProfiles
  !-------------------------------------------------------------------

  MACRO({TYPEDEF_SIMPLEXPROFILE}, {{
  type, extends($1ProfileInterface) :: $1SimplexProfileType
     private
     ! initialisation data 
     class($1FlowVariableInterface), pointer :: LWR($1)FlowVariable
     type($1MomentGroupListType) :: LWR($1)MomentGroupList
     class(IntegrableElementPointerInterface), allocatable :: &
          integrableSimplexPointer
     class(DirectedSpatialDerivativeInterface), pointer :: &
          polytopeIntAveSpatialDerivative
     ! processed data
     class($1LinearSystemInterface), allocatable :: &
          exactlyConstrainedSystem
     class($1MatrixInterface), allocatable :: leastSquaresMatrix
     class($1RHSColumnInterface), allocatable :: leastSquaresRHSColumn
     class($1MatrixInterface), allocatable :: polytopeIntAveRow

     type(FinaliseSimplexProfile_ScalarType) :: finalisation
   contains
     procedure :: init => init_$1SimplexProfile
     procedure :: deinit => deinit_$1SimplexProfile
     procedure :: getPolytopeSize => getPolytopeSize_$1SimplexProfile
     procedure :: buildSystems => buildSystems_$1SimplexProfile
     procedure :: factoriseSystems => factoriseSystems_$1SimplexProfile
     procedure :: solveIncomplete => solveIncomplete_$1SimplexProfile
     procedure :: finalise => finalise_$1SimplexProfile

     ! the following visit method allows a client to pass a linear operand
     ! to the profile.  The profile may then use the operand, for example,
     ! to help set up its finalisation operation.
     procedure :: getNumEquations => getNumEquations_$1SimplexProfile
     procedure :: acceptAsOperand1 => acceptAsOperand1_$1SimplexProfile
     procedure :: acceptAsOperand2 => acceptAsOperand2_$1SimplexProfile
     procedure :: acceptAsOperand3 => acceptAsOperand3_$1SimplexProfile
     procedure :: visit => visit_$1SimplexProfile
  end type $1SimplexProfileType
  }})

  
  !-------------------------------------------------------------------
  !- ComplexProfiles
  !-------------------------------------------------------------------

  MACRO({TYPEDEF_COMPLEXPROFILE}, {{
  type, extends($1ProfileInterface) :: $1ComplexProfileType
     private
     ! initialisation data 
     class($1ProfileInterface), dimension(:), pointer :: &
          LWR($1)SubProfileArray
     integer, dimension(:), allocatable :: LWR($1)SubProfileIndicies
     
     ! processed data.  The free variables needed to complete the simplex
     ! profiles' solutions will be located in the solution vector of
     ! polytopeIntAveSystem.  Strictly speaking the leastSquaresSystem
     ! does not need a solution vector and therefore should be decomposed
     ! into a standalone matrix and RHS, but a LinearSystem is needed to
     ! initialise a ProfileOperation later on.
     class($1LinearSystemInterface), allocatable :: leastSquaresSystem
     class($1LinearSystemInterface), allocatable :: polytopeIntAveSystem
     class($1LinearSystemInterface), allocatable :: finalSystem

     type(FinaliseComplexProfile_ScalarType) :: finalisation
   contains
     procedure :: init => init_$1ComplexProfile
     procedure :: deinit => deinit_$1ComplexProfile
     procedure :: getPolytopeSize => getPolytopeSize_$1ComplexProfile
     procedure :: buildSystems => buildSystems_$1ComplexProfile
     procedure :: factoriseSystems => factoriseSystems_$1ComplexProfile
     procedure :: solveIncomplete => solveIncomplete_$1ComplexProfile
     procedure :: finalise => finalise_$1ComplexProfile
     
     procedure :: getNumEquations => getNumEquations_$1ComplexProfile
     procedure :: acceptAsOperand1 => acceptAsOperand1_$1ComplexProfile
     procedure :: acceptAsOperand2 => acceptAsOperand2_$1ComplexProfile
     procedure :: acceptAsOperand3 => acceptAsOperand3_$1ComplexProfile
     procedure :: visit => visit_$1ComplexProfile
  end type $1ComplexProfileType
  }})
  

  !-------------------------------------------------------------------
  !- SimplexProfile methods
  !- ------------------------------------------------------------------
  
  MACRO({COMBINATION_CREATEINIT_SIMPLEXPROFILE}, {{
  m4_pushdef({ARGLINE}, {LWR($1)FlowVariable, &
       integrableSimplexPointer, polytopeIntAveSpatialDerivative, &
       nProfileCoefs $2, }) m4_dnl
  m4_pushdef({ARGLIST}, {
    class($1FlowVariableInterface), target, intent(inout) :: &
        LWR($1)FlowVariable
    class(IntegrableElementPointerInterface), intent(inout) :: &
         integrableSimplexPointer
    class(DirectedSpatialDerivativeInterface), target, intent(in) :: &
         polytopeIntAveSpatialDerivative
    integer, intent(in) :: nProfileCoefs $3}) m4_dnl

  EXPAND({PROCEDURE_CREATE({$1SimplexProfile},
  {$1Profile}, {ARGLINE}, {ARGLIST})})

  EXPAND({PROCEDURES_CREATE_ARRAY({$1SimplexProfile},
  {$1Profile}, {ARGLINE}, {ARGLIST})})

  subroutine init_$1SimplexProfile( obj, ARGLINE log )
    class($1SimplexProfileType), intent(out) :: obj ARGLIST
    class(LogType), intent(inout), optional :: log
    type(PolytopePointerType) :: pp
    type(PolytopePointerSetType) :: pps
    type(PolytopePointerSetIteratorType) :: ppsi
    type(PolytopePointerType), pointer :: ppPtr
    class($1MatrixInterface), allocatable :: A
    class($1RHSColumnInterface), allocatable :: b
    type($1MomentGroupListIteratorType) :: ABB($1)mgli
    class($1MomentGroupInterface), pointer :: ABB($1)mg
    type($1MomGpBasedRHSGeneratorType) :: rhsGeneratorSeed
    integer :: m, stat, nMomGp
    call beginSub( MOD_NAME, 'init_$1SimplexProfile', log )

    obj%LWR($1)FlowVariable => LWR($1)FlowVariable
    
    ! we will need to make a copy of the simplex pointer so that we can
    ! make a row for constraining the integrated average.
    call integrableSimplexPointer%clone( obj%integrableSimplexPointer, &
         log )
    
    ! the integrableSimplexPointer has all the geometric info we need.
    ! Ask it for its associated polytope and sub-polytopes.
    call integrableSimplexPointer%getOwner( pp )
    call pp%collectSubPolytopes( pps, log )
    call pps%append( pp, log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    
    ! loop over these polytopes appending relevant LWR($1) point moments
    ! to the embedded list.  
    call obj%LWR($1)MomentGroupList%setFlowVariable( &
         LWR($1)FlowVariable )
    call ppsi%init( pps )
    call ppsi%first( ppPtr )
    do 
       if ( ppsi%isDone() ) exit

       call ppPtr%append$1Moments( obj%LWR($1)MomentGroupList, log )

       call ppsi%next( ppPtr )
    end do

    call pps%deinit( log )
    
    ! also create a link to the spatial derivative of the integrated
    ! average
    obj%polytopeIntAveSpatialDerivative => polytopeIntAveSpatialDerivative

    ! create the linear algebraic components
    m = obj%LWR($1)MomentGroupList%countMoments( &
         exactlyConstrained=.true. )
    nMomGp = obj%LWR($1)MomentGroupList%countNodes( &
         exactlyConstrained=.true. )
    call create$1MultiRowMatrix( A, m, nProfileCoefs, &
         transposed=.true., log=log )
    call create$1RHSColumn( b, m, nMomGp, rhsGeneratorSeed, log=log )
    call create$1LinearSystem( obj%exactlyConstrainedSystem, A, b, log )
    
    m = obj%LWR($1)MomentGroupList%countMoments( &
         exactlyConstrained=.false. )
    nMomGp = obj%LWR($1)MomentGroupList%countNodes( &
         exactlyConstrained=.false. )
    ! 'behaveAsWholeArray' means that the least squares system components
    ! will be operated on as a whole during LinearOperation methods,
    ! rather than passing subsections in sequence
    call create$1MultiRowMatrix( obj%leastSquaresMatrix, m, &
         nProfileCoefs, transposed=.false., behaveAsWholeArray=.true., &
         log=log )
    call create$1RHSColumn( obj%leastSquaresRHSColumn, m, nMomGp, &
         rhsGeneratorSeed, behaveAsWholeArray=.true., log=log )
    
    call create$1SingleRowMatrix( obj%polytopeIntAveRow, &
         nProfileCoefs, transposed=.false., log=log )

    ! finally, initialise the finalisation operation.  This operation
    ! targets the solution vector in exactlyConstrainedSystem.  We cannot
    ! set the operands, however, until (i) exactlyConstrainedSystem has
    ! been factorised and its Q-null space come into existence, and (ii)
    ! the client supplies a column vector of free variables as the third
    ! operand.
    call obj%finalisation%init( obj%exactlyConstrainedSystem )
    $4
    call endSub( log )
  end subroutine init_$1SimplexProfile
  m4_popdef({ARGLINE}, {ARGLIST}) m4_dnl
  }})

  
  MACRO({METHODDEF_SIMPLEXPROFILE_DEINIT}, {{
  subroutine deinit_$1SimplexProfile( obj, log )
    class($1SimplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat $3
    call beginSub( MOD_NAME, 'deinit_$1SimplexProfile', log )
    $4
    call obj%finalisation%deinit( log )
    
    call destroy( obj%exactlyConstrainedSystem, log )
    call destroy( obj%leastSquaresMatrix, log )
    call destroy( obj%leastSquaresRHSColumn, log )
    call destroy( obj%polytopeIntAveRow, log )
    
    nullify(obj%LWR($1)FlowVariable)
    call obj%LWR($1)MomentGroupList%deinit( log )
    call destroy( obj%integrableSimplexPointer, log )
    nullify(obj%polytopeIntAveSpatialDerivative)

    call endSub( log )
  end subroutine deinit_$1SimplexProfile
  }})

  
  MACRO({METHODDEF_SIMPLEXPROFILE_GETPOLYTOPESIZE}, {{
  pure subroutine getPolytopeSize_$1SimplexProfile( obj, polytopeSize )
    class($1SimplexProfileType), intent(inout) :: obj
    real(FLOAT), intent(out) :: polytopeSize
    type(PolytopePointerType) :: pp
    call obj%integrableSimplexPointer%getOwner( pp )
    call pp%getSize( polytopeSize )
  end subroutine getPolytopeSize_$1SimplexProfile
  }})

  
  MACRO({METHODDEF_SIMPLEXPROFILE_BUILDSYSTEMS}, {{
  subroutine buildSystems_$1SimplexProfile( obj, log )
    class($1SimplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    type($1MomentGroupListIteratorType) :: ABB($1)mgli
    class($1MomentGroupInterface), pointer :: ABB($1)mg $3
    call beginSub( MOD_NAME, 'buildSystems_$1SimplexProfile', log )

    call obj%exactlyConstrainedSystem%prepareToReset( log )
    call obj%leastSquaresMatrix%prepareToReset( log )
    call obj%polytopeIntAveRow%prepareToReset( log )

    ! append rows to the exactlyContrained and leastSquares systems
    call ABB($1)mgli%init( obj%LWR($1)MomentGroupList )
    call ABB($1)mgli%first( ABB($1)mg )
    do
       if ( ABB($1)mgli%isDone() ) exit

       if ( ABB($1)mg%getNumMoments(exactlyConstrained=.true.) > 0 ) then
          call ABB($1)mg%appendEquations( obj%exactlyConstrainedSystem, &
               log )
       else
          call ABB($1)mg%appendRows( obj%leastSquaresMatrix, log )
          call ABB($1)mg%appendElements( obj%leastSquaresRHSColumn, log )
       end if
       
       call ABB($1)mgli%next( ABB($1)mg )
    end do
    
    ! for the poytopeIntAveRow, only one moment is expected, so building
    ! the matrix does not require iteration.  Delegation to the list of
    ! moment groups is unnecessary since we can compute the required row
    ! more directly.
    call obj%polytopeIntAveRow%setRows( obj%integrableSimplexPointer%&
         computeSubRow( obj%polytopeIntAveSpatialDerivative ) )
    $4
    
    call endSub( log )
  end subroutine buildSystems_$1SimplexProfile
  }})

  
  MACRO({METHODDEF_SIMPLEXPROFILE_FACTORISESYSTEMS}, {{
  subroutine factoriseSystems_$1SimplexProfile( obj, log )
    class($1SimplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'factoriseSystems_$1SimplexProfile', log )

    ! only the exactlyConstrainedSystem shall be factorised
    call obj%exactlyConstrainedSystem%factorise( log )
        
    ! We can now PARTIALLY set the operands of the finalisation operation.
    ! This wraps the linear operation xE + Q2E*xF, where E and F denote
    ! the simplex's exactlyConstrainedSystem and some client's system
    ! holding a solution vector of free variables.  We will have to defer
    ! the latter to the 'visit' method.
    call obj%acceptAsOperand1( obj%finalisation )
    call obj%acceptAsOperand2( obj%finalisation )
    
    call endSub( log )
  end subroutine factoriseSystems_$1SimplexProfile
  }})

  
  MACRO({METHODDEF_SIMPLEXPROFILE_SOLVE}, {{
  pure subroutine solveIncomplete_$1SimplexProfile( obj, log )
    class($1SimplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, n
    call beginSub( MOD_NAME, 'solveIncomplete_SimplexProfile', log )

    ! solve only the exactly constrained system; update RHS elsewhere.
    call obj%leastSquaresRHSColumn%refresh()
    call obj%exactlyConstrainedSystem%solve( log )

    call endSub( log )
  end subroutine solveIncomplete_$1SimplexProfile

  
  subroutine finalise_$1SimplexProfile( obj, &
       log )
    class($1SimplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, n
    call beginSub( MOD_NAME, 'finalise_SimplexProfile', log )

    ! convert the final particular solution to a complete solution by
    ! performing xP + Q2P*xF.  The result gets stored in xP.
    call obj%finalisation%perform( log )

    call endSub( log )
  end subroutine finalise_$1SimplexProfile
  }})
  
  
  MACRO({METHODDEF_SIMPLEXPROFILE_GETNUMEQUATIONS}, {{
  pure function getNumEquations_$1SimplexProfile( obj, selector ) &
       result ( nEquations )
    class($1SimplexProfileType), intent(in) :: obj
    character(1), intent(in) :: selector
    integer :: nEquations
    select case ( selector )
    case ( 'E' )
       nEquations = obj%exactlyConstrainedSystem%getNumEquations()
    case ( 'L' )
       nEquations = obj%leastSquaresMatrix%getNumRows()
    case default
       nEquations = 0
    end select
  end function getNumEquations_$1SimplexProfile
  }})

  
  MACRO({METHODDEF_SIMPLEXPROFILE_ACCEPT}, {{
  subroutine acceptAsOperand$2_$1SimplexProfile( obj, &
       LWR($1ProfileOperation) )
    class($1SimplexProfileType), intent(inout) :: obj
    class($1ProfileOperationInterface), intent(inout) :: &
         LWR($1ProfileOperation) $3
    ! descend through subsystems
    call LWR($1ProfileOperation)%visitExactlyConstrForOperand$2( &
         obj%exactlyConstrainedSystem )
    call LWR($1ProfileOperation)%visitLeastSquaresForOperand$2( &
         obj%leastSquaresMatrix )
    call LWR($1ProfileOperation)%visitLeastSquaresForOperand$2( &
         obj%leastSquaresRHSColumn )
    call LWR($1ProfileOperation)%visitPolytpIntAveForOperand$2( &
         obj%polytopeIntAveRow )    $4
  end subroutine acceptAsOperand$2_$1SimplexProfile
  }})

  
  MACRO({METHODDEF_SIMPLEXPROFILE_VISIT}, {{
  subroutine visit_$1SimplexProfile( obj, &
       LWR($1)LinearOperand )
    class($1SimplexProfileType), intent(inout) :: obj
    class($1LinearOperandInterface), intent(inout) :: &
         LWR($1)LinearOperand

    ! in this instance we will set the passed-in system as operand 3
    ! in the finalisation operation xE + Q2E*xP.
    call obj%finalisation%visitPolytpIntAveForOperand3( &
         LWR($1)LinearOperand )
  end subroutine visit_$1SimplexProfile
  }})


  !-------------------------------------------------------------------
  !- ComplexProfile methods
  !-------------------------------------------------------------------
  
  MACRO({COMBINATION_CREATEINIT_COMPLEXPROFILE}, {{
  m4_pushdef({ARGLINE}, {&
       LWR($1)SubProfileArray, LWR($1)SubProfileIndicies, &
       sharedIntAve$1Moment $2, }) m4_dnl
  m4_pushdef({ARGLIST}, {
    class($1ProfileInterface), dimension(:), target, &
         intent(in) :: LWR($1)SubProfileArray
    integer, dimension(:), intent(in) :: &
         LWR($1)SubProfileIndicies
    class($1MomentGroupInterface), allocatable, intent(inout) :: &
         sharedIntAve$1Moment $3}) m4_dnl
    
  EXPAND({PROCEDURE_CREATE({$1ComplexProfile},
  {$1Profile}, {ARGLINE}, {ARGLIST})})

  EXPAND({PROCEDURES_CREATE_ARRAY({$1ComplexProfile},
  {$1Profile}, {ARGLINE}, {ARGLIST})})

  subroutine init_$1ComplexProfile( obj, ARGLINE log )
    class($1ComplexProfileType), intent(out) :: obj ARGLIST
    class(LogType), intent(inout), optional :: log
    class($1MatrixInterface), allocatable :: A
    class($1RHSColumnInterface), allocatable :: b
    integer :: nSubs, stat, i, ii, nEquations
    type($1LinOpBasedRHSGeneratorType) :: rhsg_lo
    class($1RHSGeneratorInterface), allocatable :: rhsg
    type(AppendFinalRHS_$1Type) :: profOp_final
    class(Pure$1LinearOperationInterface), allocatable :: linOp
    call beginSub( MOD_NAME, 'init_$1ComplexProfile', log )

    obj%LWR($1)SubProfileArray => LWR($1)SubProfileArray
    
    nSubs = size(LWR($1)SubProfileIndicies)
    allocate( obj%LWR($1)SubProfileIndicies(nSubs), stat=&
         stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &obj%LWR($1)SubProfileIndicies(nSubs).  STAT='//&
         int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    obj%LWR($1)SubProfileIndicies = LWR($1)SubProfileIndicies

    ! count the number of equations we have to solve in the least squares
    ! sense, then create the appropriate linear systems 
    nEquations = 0
    do i = 1, nSubs
       ii = obj%LWR($1)SubProfileIndicies(i)
       nEquations = nEquations + LWR($1)SubProfileArray(ii)%&
            getNumEquations('L')
    end do

    ! create the linear systems
    call create$1MultiRowMatrix( A, nEquations, nEquations, &
         blockDiagonal=.true., log=log )
    call create$1RHSColumn( b, nEquations, nSubs, rhsg_lo, log=log )
    call create$1LinearSystem( obj%leastSquaresSystem, A, b, log )

    call create$1MomGpBasedRHSGenerator( rhsg, sharedIntAve$1Moment, &
         log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if
    call create$1SingleRowMatrix( A, nEquations, transposed=.true., &
         segmented=.true., log=log )
    call create$1RHSColumn( b, 1, nSubs, rhsg_lo, counterIsFrozen=&
         .true., scalarRHSPreGenerator=rhsg, log=log )
    call create$1LinearSystem( obj%polytopeIntAveSystem, A, b, log )

    call create$1MultiRowMatrix( A, nEquations, nEquations - 1, log=log )
    call create$1RHSColumn( b, nEquations, 1, rhsg_lo, log=log )
    call create$1LinearSystem( obj%finalSystem, A, b, log )
    
    ! we can set the RHS generator of the final system right away: it
    ! invariably performs bL - AL*xP, where bL and AL are the RHS and
    ! matrix of leastSquaresSystem, and xP is the solution vector of
    ! polytopeIntAveSystem.  This is encapsulated in profOp_final.
    call profOp_final%init( obj%finalSystem )
    call obj%accept( profOp_final )
    call profOp_final%convert( linOp )
    call obj%finalSystem%setRHSElements( linOp, log )
    call destroy( linOp, log )
    call profOp_final%deinit( log )

    ! finally, initialise the finalisation operation.  This operation
    ! targets the solution vector in polytopeIntAveSystem.  We cannot set
    ! the operands, however, until polytopeIntAveSystem has been
    ! factorised and its Q-null space come into existence.
    call obj%finalisation%init( obj%polytopeIntAveSystem )
    
    call endSub( log )
  end subroutine init_$1ComplexProfile
  m4_popdef({ARGLINE}, {ARGLIST}) m4_dnl
  }})

  
  MACRO({METHODDEF_COMPLEXPROFILE_DEINIT}, {{
  subroutine deinit_$1ComplexProfile( obj, log )
    class($1ComplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: stat $3
    call beginSub( MOD_NAME, 'deinit_$1ComplexProfile', log )
    $4
    call obj%finalisation%deinit( log )

    call destroy( obj%leastSquaresSystem, log )
    call destroy( obj%polytopeIntAveSystem, log )
    call destroy( obj%finalSystem, log )
    
    nullify(obj%LWR($1)SubProfileArray)
    deallocate( obj%LWR($1)SubProfileIndicies, stat=stat )
    call addEvent( stat/=0, WARNING, 'Problem deallocating obj%LWR($1)&
         &SubProfileIndicies.  STAT='//int2str(stat), log )
    
    call endSub( log )
  end subroutine deinit_$1ComplexProfile
  }})
  
  
  MACRO({METHODDEF_COMPLEXPROFILE_GETPOLYTOPESIZE}, {{
  pure subroutine getPolytopeSize_$1ComplexProfile( obj, polytopeSize )
    class($1ComplexProfileType), intent(inout) :: obj
    real(FLOAT), intent(out) :: polytopeSize
    integer :: i, ii
    real(FLOAT) :: work

    ! add the polytope sizes from each simplex profile
    polytopeSize = 0._FLOAT
    do i = 1, size(obj%LWR($1)SubProfileIndicies)
       ii = obj%LWR($1)SubProfileIndicies(i)
       call obj%LWR($1)SubProfileArray(ii)%getPolytopeSize( work )
       polytopeSize = polytopeSize + work
    end do
  end subroutine getPolytopeSize_$1ComplexProfile
  }})
  
  
  MACRO({METHODDEF_COMPLEXPROFILE_BUILDSYSTEMS}, {{
  subroutine buildSystems_$1ComplexProfile( obj, log )
    class($1ComplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    type(AppendPolytpIntAveMatrix_ScalarType) :: profOp_pia_mat
    type(AppendPolytpIntAveRHS_ScalarType) :: profOp_pia_rhs
    type(AppendLeastSquaresMatrix_ScalarType) :: profOp_ls_mat
    type(AppendLeastSquaresRHS_ScalarType) :: profOp_ls_rhs
    type(FinaliseSimplexProfile_ScalarType) :: profOp_fsp
    class(Pure$1LinearOperationInterface), allocatable :: linOp
    type($1LinOpBasedRHSGeneratorType) :: rhsg
    integer :: stat, i, ii
    real(FLOAT) :: volSimplex, volComplex
    call beginSub( MOD_NAME, 'buildSystems_$1ComplexProfile', log )
    
    ! to build each linear system, we need to supply it with a number of
    ! linear operations, one for every sub-profile, that it uses to set
    ! matrix rows and update RHS values.  The type and states of linear
    ! operations depend on the profile operations required.  At this level
    ! we know about the latter but not the former.  To complicate things
    ! further, the linear operations must become embedded within the
    ! RHSColumn, whereas the profile operations are disposable (being
    ! needed only to create the former).  We will delegate the creation of
    ! linear operations to the profile operations.
    
    ! start by initialising the profile operations.  This can be done
    ! outside the loop over simplicies because the target linear systems
    ! are invariant.
    call profOp_ls_mat%init( obj%leastSquaresSystem )
    call profOp_ls_rhs%init( obj%leastSquaresSystem )
    call profOp_pia_mat%init( obj%polytopeIntAveSystem )
    call profOp_pia_rhs%init( obj%polytopeIntAveSystem )
    
    ! need this to calculate the simplex-to-complex volume fraction
    call obj%getPolytopeSize( volComplex )
    
    ! loop over simplicies.  Each simplex profile MUST be factorised
    ! before anything further is done.  We do not do this here because the
    ! simplex profiles exist independently of the complex profile (if they
    ! were embedded, we would be more confident in taking control of
    ! them).  For each profile operation: (i) use the simplex profile's
    ! 'accept' method to finalise the state of the profile operation; (ii)
    ! convert the fully-initialised profile operation into a linear
    ! operation; and (iii) set the Matrix rows or RHSColumn elements
    ! according to the linear operation.  The linear operation, being
    ! dynamically allocated in (ii), must be destroyed after each step.
    do i = 1, size(obj%LWR($1)SubProfileIndicies)
       ii = obj%LWR($1)SubProfileIndicies(i)
    
       call obj%LWR($1)SubProfileArray(ii)%accept( profOp_ls_mat )
       call profOp_ls_mat%convert( linOp, log )
       call obj%leastSquaresSystem%setMatrixRows( linOp, log )
       call destroy( linOp, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       
       call obj%LWR($1)SubProfileArray(ii)%accept( profOp_ls_rhs )
       call profOp_ls_rhs%convert( linOp, log )
       call obj%leastSquaresSystem%setRHSElements( linOp, log )
       call destroy( linOp, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       
       ! need this to calculate the simplex-to-complex volume fraction
       call obj%LWR($1)SubProfileArray(ii)%getPolytopeSize( &
            volSimplex )
       
       call profOp_pia_mat%setScaleFactor( volSimplex/volComplex )
       call obj%LWR($1)SubProfileArray(ii)%accept( profOp_pia_mat )
       call profOp_pia_mat%convert( linOp, log )
       call obj%polytopeIntAveSystem%setMatrixRows( linOp, log )
       call destroy( linOp, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
       
       call profOp_pia_rhs%setScaleFactor( volSimplex/volComplex )
       call obj%LWR($1)SubProfileArray(ii)%accept( profOp_pia_rhs )
       call profOp_pia_rhs%convert( linOp, log )
       call obj%polytopeIntAveSystem%setRHSElements( linOp, log )
       call destroy( linOp, log )
       if ( checkSub(FATAL, log) ) then
          call endSub( log )
          return
       end if
    end do

    ! no real need to deinitialise these lightweight objects, but do so
    ! for the sake of good habit
    call profOp_ls_mat%deinit( log )
    call profOp_ls_rhs%deinit( log )
    call profOp_pia_mat%deinit( log )
    call profOp_pia_rhs%deinit( log )
    
    ! now that polytopeIntAveSystem's RHS generators have been
    ! implemented, we can supply the system as the third operand in each
    ! sub-profile's finalisation operation.  Its solution vector contains
    ! the crucial free variables needed to compute the profile
    ! coefficients.  The linear system will keep track of the indicies of
    ! the vector each set of free variables start from.  However, the
    ! system's counters become muddled if we have multiple visit/accept
    ! calls in the same iteration block.  This is why the below iteration
    ! block MUST be kept separate from the block above.  In future we may
    ! want to implement a more stable alternative.

    ! prepareToBeVisited() is needed to initialise polytopeIntAveSystem's
    ! internal counters
    call obj%polytopeIntAveSystem%prepareToBeVisited( log )
    do i = 1, size(obj%LWR($1)SubProfileIndicies)
       ii = obj%LWR($1)SubProfileIndicies(i)
       
       call obj%LWR($1)SubProfileArray(ii)%visit( obj%&
            polytopeIntAveSystem )
    end do
    
    $4
    call endSub( log )
  end subroutine buildSystems_$1ComplexProfile
  }})

  
  MACRO({METHODDEF_COMPLEXPROFILE_FACTORISESYSTEMS}, {{
  subroutine factoriseSystems_$1ComplexProfile( obj, log )
    class($1ComplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    type(A_Times_Q2_$1Type) :: A_times_Q2
    call beginSub( MOD_NAME, 'factoriseSystems_$1ComplexProfile', log )

    ! factorising the transposed polytopeIntAveSystem yields a null space,
    ! Q2cia
    call obj%polytopeIntAveSystem%factorise( log )
    
    ! We can now set the operands of the finalisation operation.  This
    ! wraps the linear operation xP + Q2P*xF, where P and F denote the
    ! polytopeIntAveSystem and finalSystem, respectively.
    call obj%accept( obj%finalisation )
    
    ! the 'final' matrix is Alsq*Q2cia
    call obj%leastSquaresSystem%acceptAsOperand1( A_times_Q2 )
    call obj%polytopeIntAveSystem%acceptAsOperand2( A_times_Q2 )
    call obj%finalSystem%setMatrixRows( A_times_Q2, log )
    
    call obj%finalSystem%factorise( log )

    call endSub( log )
  end subroutine factoriseSystems_$1ComplexProfile
  }})

  
  MACRO({METHODDEF_COMPLEXPROFILE_SOLVE}, {{
  pure subroutine solveIncomplete_$1ComplexProfile( obj, log )
    class($1ComplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, n
    call beginSub( MOD_NAME, 'solve_$1ComplexProfile', log )
    
    ! the least squares system does not get factorised and solved, so
    ! calling solve() simply updates the RHS.  All wiring to the various
    ! sub-profiles will have been done in each linear system, so no
    ! looping is necessary.
    call obj%leastSquaresSystem%solve( log=log )
    
    ! solve the other systems to get particular solutions.  The
    ! constrainingSystem argument is not used.
    call obj%polytopeIntAveSystem%solve( log=log )
    call obj%finalSystem%solve( log=log )

    call endSub( log )
  end subroutine solveIncomplete_$1ComplexProfile

  
  subroutine finalise_$1ComplexProfile( obj, log )
    class($1ComplexProfileType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    integer :: i, n
    call beginSub( MOD_NAME, 'finalise_$1ComplexProfile', log )
    
    ! convert the final particular solution to a complete solution by
    ! performing xP + Q2P*xF(iF).  We need to permutate xF first.  The
    ! result gets stored in xP.
    call obj%finalSystem%permutateSolution()
    call obj%finalisation%perform( log )
    
    call endSub( log )
  end subroutine finalise_$1ComplexProfile
  }})

  
  MACRO({METHODDEF_COMPLEXPROFILE_GETNUMEQUATIONS}, {{
  pure function getNumEquations_$1ComplexProfile( obj, selector ) &
       result ( nEquations )
    class($1ComplexProfileType), intent(in) :: obj
    character(1), intent(in) :: selector
    integer :: nEquations
    select case ( selector )
    case ( 'L' )
       nEquations = obj%leastSquaresSystem%getNumEquations()
    case ( 'P' )
       nEquations = obj%polytopeIntAveSystem%getNumEquations()
    case default
       nEquations = 0
    end select
  end function getNumEquations_$1ComplexProfile
  }})
  

  MACRO({METHODDEF_COMPLEXPROFILE_ACCEPT}, {{
  subroutine acceptAsOperand$2_$1ComplexProfile( obj, &
       LWR($1ProfileOperation) )
    class($1ComplexProfileType), intent(inout) :: obj
    class($1ProfileOperationInterface), intent(inout) :: &
         LWR($1ProfileOperation) $3
    ! descend through subsystems
    call LWR($1ProfileOperation)%visitLeastSquaresForOperand$2( &
         obj%leastSquaresSystem )
    call LWR($1ProfileOperation)%visitPolytpIntAveForOperand$2( &
         obj%polytopeIntAveSystem )
    call LWR($1ProfileOperation)%visitFinalForOperand$2( &
         obj%finalSystem )
    $4
  end subroutine acceptAsOperand$2_$1ComplexProfile
  }})

  
  MACRO({METHODDEF_COMPLEXPROFILE_VISIT}, {{
  subroutine visit_$1ComplexProfile( obj, LWR($1)LinearOperand )
    class($1ComplexProfileType), intent(inout) :: obj
    class($1LinearOperandInterface), intent(inout) :: &
         LWR($1)LinearOperand

    ! nothing to do
    
  end subroutine visit_$1ComplexProfile
  }})
