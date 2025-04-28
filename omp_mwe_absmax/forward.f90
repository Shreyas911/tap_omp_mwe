module forward

implicit none

contains

subroutine forward_problem(xx,V)

!$ use omp_lib

implicit none

integer :: i
real(8), intent(in), dimension(10000) :: xx
real(8), dimension(10000) :: vi
real(8), intent(out) :: V

!$omp parallel do default(shared) private(i) schedule(static)
do i=1, 10000
    if (xx(i)**2 + xx(i)**3 .ge. 0) then

        ! sin function
        vi(i) = sin(xx(i))

        ! abs function
        vi(i) = vi(i) + abs(xx(i))

        ! max function
        vi(i) = vi(i) + max(xx(i), 1.0)

        ! abs and max together
        vi(i) = vi(i) + abs(xx(i))/max(abs(xx(i)), 1.0)

    end if
end do
!$omp end parallel do       

V = SUM(vi)

end subroutine forward_problem

end module forward
