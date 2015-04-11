module PointArrangements_DomainDim_2
  
  use LogModule
  use Global
  
  implicit none
  
  character(*), parameter, private :: MOD_NAME = &
       'PointArrangments_DomainDim_2'
  
contains

  
  pure function expandPointCoords( cartesianCode, pointPattern, &
       gridParameters, polytopeAddress ) result ( values )
    type(LogicalVectorType), intent(in) :: cartesianCode
    real(FLOAT), dimension(:), intent(in) :: pointPattern
    type(GridParametersType), intent(in) :: gridParameters
    type(IntVectorType), intent(in) :: polytopeAddress
    type(RealVectorType), dimension( size(pointPattern)**count(&
         cartesianCode) ) :: values
    real(FLOAT), dimension( NDIM, size(pointPattern), size(&
         pointPattern) ) :: work
    real(FLOAT), dimension( NDIM, size(pointPattern)**count(&
         cartesianCode) ) :: packed
    logical, dimension( size(pointPattern), size(pointPattern) ) :: mask
    logical, dimension(size(pointPattern)) :: m
    type(RealVectorType) :: temp
    integer :: i, j, n

    ! local initialisations
    n = size(pointPattern)
    work = spread( &
         spread(gridParameters%spacing%getValues(), 2, n), &
         3, n )
    mask = .true. 
    m = .false.
    m(1) = .true.
    
    ! if cartesianCode = [T F], require [p1*dx, 0], [p2*dx, 0], ...,
    ! [pn*dx, 0].  If [F T], require [0, p1*dy], ..., [0, pn*dy].  If [T
    ! T], require [p1*dx, p1*dy], [p2*dx, p1*dy], ..., [pn*dx, pn*dy].
    ! Then an offset is applied.
    do i = 1, NDIM
       if ( cartesianCode%getValue(i) ) then
          work(i, :, :) = spread(pointPattern, i, n) * work(i, :, :)
       else
          work(i, :, :) = 0._FLOAT
          ! if this dimension is not applicable, mask all corresponding
          ! entries in the work matrix
          mask = mask .and. spread(m, i, n)
       end if
    end do

    do i = 1, NDIM
       packed(i, :) = pack(work(i, :, :), mask)
    end do
    
    do j = 1, size(pointPattern)**count(cartesianCode)
       temp = real(polytopeAddress - 1)
       temp = temp*gridParameters%spacing
       temp = gridParameters%extents(1) + temp
       values(j) = vector(packed(:, j)) + temp
       
       values(j) = vector(packed(:, j)) + gridParameters%extents(1) + &
            real(polytopeAddress - 1)*gridParameters%spacing
    end do
  end function expandPointCoords

  
  pure function expandWeights( cartesianCode, gaussianWeights ) &
       result ( values )
    type(LogicalVectorType), intent(in) :: cartesianCode
    real(FLOAT), dimension(:), intent(in) :: gaussianWeights
    real(FLOAT), dimension( size(gaussianWeights)**NDIM ) :: values
    real(FLOAT), dimension( size(gaussianWeights), size(&
         gaussianWeights) ) :: work
    logical, dimension( size(gaussianWeights), size(&
         gaussianWeights) ) :: mask
    logical, dimension(size(gaussianWeights)) :: m
    integer :: i, n

    ! local initialisations
    n = size(gaussianWeights)
    work = 1._FLOAT
    mask = .true. 
    m = .false.
    m(1) = .true.
    
    ! if polytopeDim = 1, require w1, w2, ..., wn.  If polytopeDim = 2,
    ! require w1*w1, w2*w1, w3*w1, ..., wn*wn.
    do i = 1, NDIM
       if ( cartesianCode%getValue(i) ) then
          work = work * spread(gaussianWeights, i, n)
       else
          ! if this dimension is not applicable, mask all corresponding
          ! entries in the work matrix
          mask = mask .and. spread(m, i, n)
       end if
    end do
    values = pack(work, mask)
    
  end function expandWeights
  
end module PointArrangements_DomainDim_2
