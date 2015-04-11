module FlowField_AdvectionProfile_Cubic

  use LogModule
  use Global
  
  implicit none
  
  character(*), parameter, private :: MOD_NAME = &
       'FlowField_AdvectionProfile_Cubic'
  
  integer, parameter :: DEGREE_ADV = 3
  integer, parameter :: NCOEFS_ADV = (NDIM + 1)*(NDIM + 2)*(NDIM + 3)/6
  
  
  ! this module needs to be compatible with any number of spatial
  ! dimensions.  So when listing powers of x, y, etc., first cycle
  ! powers of x up to 3, then cycle y as far as possible for each
  ! power of x, etc.  
  integer, dimension(3, 20), parameter, private :: &
       ALL_POWERS = reshape( [&
       0, 1, 2, 3, 0, 1, 2, 0, 1, 0, 0, 1, 2, 0, 1, 0, 0, 1, 0, 0, &
       0, 0, 0, 0, 1, 1, 1, 2, 2, 3, 0, 0, 0, 1, 1, 2, 0, 0, 1, 0, &
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3], [&
       3, 20], order=[2, 1] )
  
  integer, dimension(NDIM, NCOEFS_ADV), parameter :: POWERS_ADV = &
       ALL_POWERS(1:NDIM, 1:NCOEFS_ADV)
 
  
end module FlowField_AdvectionProfile_Cubic
