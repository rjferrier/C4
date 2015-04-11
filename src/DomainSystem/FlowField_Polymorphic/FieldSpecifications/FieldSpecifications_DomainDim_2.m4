module FieldSpecifications_DomainDim_2
  
  use LogModule
  use Global
  use FlowFieldModule
  
  
  implicit none
  private
  public :: createUniformScalarFieldSpecification, &
       createTopHatStripSpecification
  
  character(*), parameter :: MOD_NAME = 'FieldSpecifications_DomainDim_2'
  
  type, extends(ScalarFieldSpecificationInterface) :: UniformScalarFieldSpecificationType
     private
     real(FLOAT) :: value
   contains
     procedure :: init => init_UniformScalarField
     procedure :: deinit => deinit_UniformScalarField
     procedure :: clone => clone_UniformScalarField
     procedure :: getValue => getValue_UniformScalarField
  end type UniformScalarFieldSpecificationType

  
  type, extends(ScalarFieldSpecificationInterface) :: TopHatStripSpecificationType
     private
     type(RealVectorType) :: normalDirection
     type(RealVectorType) :: startPosition
     real(FLOAT) :: width
   contains
     procedure :: init => init_TopHatStrip
     procedure :: deinit => deinit_TopHatStrip
     procedure :: clone => clone_TopHatStrip
     procedure :: getValue => getValue_TopHatStrip
  end type TopHatStripSpecificationType


  
contains

  !-------------------------------------------------------------------
  !- UniformField methods
  !-------------------------------------------------------------------

  EXPAND({PROCEDURE_CREATE({UniformScalarFieldSpecification},
  {ScalarFieldSpecification},  {scalarFlowVariable, &
       directedSpatialDerivative, value, }, {
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    real(FLOAT), intent(in) :: value})})
  
  subroutine init_UniformScalarField( obj, scalarFlowVariable, &
       directedSpatialDerivative, value, log )
    class(UniformScalarFieldSpecificationType), intent(out) :: obj
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    real(FLOAT), intent(in) :: value
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_UniformScalarField', log )

    call obj%initScalarFieldSpecification( scalarFlowVariable, &
         directedSpatialDerivative, log )
    obj%value = value    
    
    call endSub( log )
  end subroutine init_UniformScalarField

  
  subroutine deinit_UniformScalarField( obj, log )
    class(UniformScalarFieldSpecificationType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_UniformScalarField', log )

    obj%value = 0._FLOAT
    call obj%deinitScalarFieldSpecification

    call endSub( log )
  end subroutine deinit_UniformScalarField

  
  subroutine clone_UniformScalarField( obj, tgt, log )
    class(UniformScalarFieldSpecificationType), intent(in) :: obj
    class(ScalarFieldSpecificationInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_UniformScalarField', log )

    allocate( UniformScalarFieldSpecificationType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &UniformScalarFieldSpecificationType :: tgt.  STAT='//&
         int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    select type ( tgt )
    type is (UniformScalarFieldSpecificationType)
       tgt%value = obj%value
    end select
    
    call endSub( log )
  end subroutine clone_UniformScalarField


  elemental function getValue_UniformScalarField( obj, position )
    class(UniformScalarFieldSpecificationType), intent(in) :: obj
    type(RealVectorType), intent(in) :: position
    real(FLOAT) :: getValue_UniformScalarField
    getValue_UniformScalarField = obj%value
  end function getValue_UniformScalarField


  
  !-------------------------------------------------------------------
  !- TopHatStripSpecification methods
  !-------------------------------------------------------------------
  
  
  EXPAND({PROCEDURE_CREATE({TopHatStripSpecification}, {ScalarFieldSpecification},
  {scalarFlowVariable, &
       directedSpatialDerivative, normalDirection, &
       startPosition, width, }, {
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    type(RealVectorType), intent(in) :: normalDirection
    type(RealVectorType), intent(in) :: startPosition
    real(FLOAT), intent(in) :: width})})
  
  subroutine init_TopHatStrip( obj, scalarFlowVariable, &
       directedSpatialDerivative, normalDirection, startPosition, &
       width, log )
    class(TopHatStripSpecificationType), intent(out) :: obj
    class(ScalarFlowVariableInterface), target, intent(in) :: &
         scalarFlowVariable
    class(DirectedSpatialDerivativeInterface), allocatable, intent(&
         inout) :: directedSpatialDerivative
    type(RealVectorType), intent(in) :: normalDirection
    type(RealVectorType), intent(in) :: startPosition
    real(FLOAT), intent(in) :: width
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'init_TopHatStrip', log )

    call obj%initScalarFieldSpecification( scalarFlowVariable, &
         directedSpatialDerivative, log )

    obj%normalDirection = normalDirection
    call obj%normalDirection%normalise()
    
    obj%startPosition = startPosition
    obj%width = width
    
    call endSub( log )
  end subroutine init_TopHatStrip

  
  subroutine deinit_TopHatStrip( obj, log )
    class(TopHatStripSpecificationType), intent(inout) :: obj
    class(LogType), intent(inout), optional :: log
    call beginSub( MOD_NAME, 'deinit_TopHatStrip', log )

    obj%normalDirection = 0._FLOAT
    obj%startPosition = 0._FLOAT
    obj%width = 0._FLOAT
    
    call obj%deinitScalarFieldSpecification( log )

    call endSub( log )
  end subroutine deinit_TopHatStrip

  
  subroutine clone_TopHatStrip( obj, tgt, log )
    class(TopHatStripSpecificationType), intent(in) :: obj
    class(ScalarFieldSpecificationInterface), allocatable, intent(&
         out) :: tgt
    class(LogType), intent(inout), optional :: log
    integer :: stat
    call beginSub( MOD_NAME, 'clone_TopHatStrip', log )

    allocate( TopHatStripSpecificationType :: tgt, stat=stat )
    call addEvent( stat/=0, FATAL, 'Problem allocating &
         &TopHatStripSpecificationType :: tgt.  STAT='//&
         int2str(stat), log )
    if ( checkSub(FATAL, log) ) then
       call endSub( log )
       return
    end if

    select type ( tgt )
    type is (TopHatStripSpecificationType)
       tgt%normalDirection = obj%normalDirection
       tgt%startPosition = obj%startPosition
       tgt%width = obj%width
    end select
    
    call endSub( log )
  end subroutine clone_TopHatStrip


  elemental function getValue_TopHatStrip( obj, position )
    class(TopHatStripSpecificationType), intent(in) :: obj
    type(RealVectorType), intent(in) :: position
    real(FLOAT) :: getValue_TopHatStrip
    logical :: l1, l2
    
    l1 = obj%normalDirection%dotProduct( position - obj%&
         startPosition ) >= 0
    l2 = obj%normalDirection%dotProduct( position - obj%startPosition - &
         obj%normalDirection*obj%width ) <= 0
    
    if ( l1 .and. l2 ) then
       getValue_TopHatStrip = 1._FLOAT
    else
       getValue_TopHatStrip = 0._FLOAT
    end if
  end function getValue_TopHatStrip
  
end module FieldSpecifications_DomainDim_2
