module forward

implicit none

contains

subroutine forward_problem(xx,V)

!$ use omp_lib

implicit none

integer :: i
real(8), intent(in), dimension(10000) :: xx
real(8), dimension(10000) :: vi
real(8) :: temp
real(8), intent(out) :: V

!$omp parallel do default(shared) private(i, temp) schedule(static)
do i=1, 10000
    if (xx(i)**2 + xx(i)**3 .ge. 0) then

        ! Inline function call
        vi(i) = 1.0 + xx(i)*dummy_func(xx(i))

        ! Not inline function call
        temp = dummy_func(xx(i))
        vi(i) = vi(i) + 1.0 + xx(i)*temp

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
