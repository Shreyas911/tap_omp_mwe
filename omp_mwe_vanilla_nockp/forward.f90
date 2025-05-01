module forward

implicit none

contains

subroutine forward_problem(xx,V)

!$ use omp_lib

implicit none

integer :: i
real(8), intent(in), dimension(10) :: xx
real(8), dimension(10) :: vi
real(8), intent(out) :: V

!$omp parallel do default(shared) private(i) schedule(static)
do i=1, 10
    vi(i) = xx(i) + 1.0
end do
!$omp end parallel do       

V = SUM(vi)

end subroutine forward_problem

end module forward
