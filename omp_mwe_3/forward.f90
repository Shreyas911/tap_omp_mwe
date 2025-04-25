module forward

implicit none

contains

	subroutine forward_problem(xx,V)
!$ use omp_lib
		implicit none

		integer :: i
		real(8), intent(in), dimension(10000000) :: xx
        real(8), dimension(10000000) :: vi
		real(8), intent(out) :: V

!$omp parallel do default(shared) private(i)
!!$omp parallel do default(none) private(i) &
!!$omp& shared(vi, xx)
        do i=1, 10000000
            if (xx(i)**2 + sin(xx(i)) .ge. 0) then
                vi(i) = 1.0 + xx(i)*dummy_func(xx(i))
                vi(i) = abs(vi(i)) / sin(vi(i))
            end if
        end do
!$omp end parallel do       

        V = SUM(vi)

	end subroutine forward_problem

    function dummy_func(x)

implicit none
real(8)             :: dummy_func
real(8), intent(in) :: x

dummy_func = x + 1.0

    end function dummy_func
end module forward
