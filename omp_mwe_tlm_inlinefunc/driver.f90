program driver

use forward_diff, fp => forward_problem
use forward_tgt, fp_useless => forward_problem 

implicit none 

real(8), dimension(10000000) :: xx=0., xx_tlm =0., xxb =0.
real(8) :: V=0., Vb = 1., Vd = 0.

xx_tlm(:) = 1.
call forward_problem_d(xx,xx_tlm,V,Vd)
print *, Vd

xx(:) = 0.
call forward_problem_b(xx,xxb,V,Vb)
print *, SUM(xxb)

end program driver

