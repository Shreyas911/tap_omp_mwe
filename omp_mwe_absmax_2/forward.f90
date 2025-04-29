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
real(8) :: tap_temp_abs1, tap_temp_abs2, tap_temp_abs3, tap_temp_max1, tap_temp_max2

!$omp parallel do default(shared) private(i, tap_temp_abs1, tap_temp_abs2, tap_temp_abs3, tap_temp_max1, tap_temp_max2) schedule(static)
do i=1, 10000
    if (xx(i)**2 + xx(i)**3 .ge. 0) then

        ! sin function
        vi(i) = sin(xx(i))

        ! abs function
        tap_temp_abs1 = abs(xx(i))
        vi(i) = vi(i) + tap_temp_abs1

        ! max function
        tap_temp_max1 = max(xx(i), 1.0)
        vi(i) = vi(i) + tap_temp_max1

        ! abs and max together
        tap_temp_abs2 = abs(xx(i))
        tap_temp_abs3 = abs(xx(i))
        tap_temp_max2 = max(tap_temp_abs3, 1.0)
        vi(i) = vi(i) + tap_temp_abs2/tap_temp_max2

    end if
end do
!$omp end parallel do       

V = SUM(vi)

end subroutine forward_problem

end module forward
