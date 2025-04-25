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

!!$omp parallel do default(shared) private(i)
!$omp parallel do default(none) &
!$omp& private(i) &
!$omp& shared(vi, xx)
        do i=1, 10000000
            if (xx(i)**2 + sin(xx(i)) .ge. 0) then
                vi(i) = xx(i) + 1.0
            end if
        end do
!$omp end parallel do       

        V = SUM(vi)

	end subroutine forward_problem

end module forward
