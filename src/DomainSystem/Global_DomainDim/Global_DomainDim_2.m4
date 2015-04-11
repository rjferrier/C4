module Global_DomainDim_2
  use ieee_arithmetic

  USE_MACROS({Global_DomainDim})

  EXPAND({PREAMBLE_GLOBAL({2})})
  
  EXPAND({TYPEDEFS_LOGICALVECTOR})

  EXPAND({TYPEDEFS_INTVECTOR({
     procedure :: setGridElement_intDim2
     generic :: setGridElement => setGridElement_intDim2
     
     procedure :: getGridElement_intDim2
     generic :: getGridElement => getGridElement_intDim2},
     {     
     procedure :: allocGrid_intDim2
     generic :: allocGrid => allocGrid_intDim2
     })})
  
  EXPAND({TYPEDEFS_REALVECTOR})

  EXPAND({TYPEDEFS_REALTENSOR})

  
contains

  
  EXPAND({PROCEDURES_GLOBAL({2})})
  
  !-------------------------------------------------------------------
  !- LogicalVector procedures
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_LOGICALVECTOR})

  
  !-------------------------------------------------------------------
  !- IntVector procedures
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_INTVECTOR})
  
  subroutine setGridElement_intDim2( obj, grid, scalar )
    class(IntVectorType), intent(in) :: obj
    integer, dimension(:, :), intent(inout) :: grid
    integer, intent(in) :: scalar
    grid( obj%values(1), obj%values(2) ) = scalar
  end subroutine setGridElement_intDim2
  
  
  pure function getGridElement_intDim2( obj, grid  ) result ( &
       scalar )
    class(IntVectorType), intent(in) :: obj
    integer, dimension(:, :), intent(in) :: grid
    integer :: scalar
    scalar = grid( obj%values(1), obj%values(2) )
  end function getGridElement_intDim2
  
  
  subroutine allocGrid_intDim2( obj, grid, allocStat )
    class(SizeVectorType), intent(in) :: obj
    integer, dimension(:, :), intent(inout), allocatable :: grid
    integer, intent(out) :: allocStat
    allocate( grid( obj%values(1), obj%values(2) ), stat=allocStat )
  end subroutine allocGrid_intDim2
  
  
  !-------------------------------------------------------------------
  !- RealVector procedures
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_REALVECTOR})

  elemental function crossProductMagnitude_realVector( obj, vector )
    class(RealVectorType), intent(in) :: obj
    class(RealVectorType), intent(in) :: vector
    real(FLOAT) :: crossProductMagnitude_realVector
    associate( a => obj%values, b => vector%values )
!!$      select case ( NDIM )
!!$      case ( 2 )
         crossProductMagnitude_realVector = abs( a(1)*b(2) - a(2)*b(1) )
!!$      case ( 3 )
!!$         crossProductMagnitude_realVector = sqrt( &
!!$              (a(2)*b(3) - a(3)*b(2))**2 + &
!!$              (a(3)*b(1) - a(1)*b(3))**2 + &
!!$              (a(1)*b(2) - a(2)*b(1))**2 )
!!$      end select
    end associate
  end function crossProductMagnitude_realVector

  
  !-------------------------------------------------------------------
  !- RealTensor procedures/methods
  !-------------------------------------------------------------------

  EXPAND({METHODDEFS_REALTENSOR})
  
  pure function det_realTensor( obj )
    class(RealTensorType), intent(in)  :: obj
    real(FLOAT) :: det_realTensor
    associate( A => obj%values )
!!$      select case ( NDIM )
!!$      case (2)
         det_realTensor = A(1,1)*A(2,2) - A(1,2)*A(2,1)
!!$      case (3)
!!$         det_realTensor = &
!!$              A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2) + &
!!$              A(1,2)*A(2,3)*A(3,1) - A(1,2)*A(2,1)*A(3,3) + &
!!$              A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1)
!!$      end select
    end associate
  end function det_realTensor



end module Global_DomainDim_2
