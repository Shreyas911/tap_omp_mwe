!        Generated by TAPENADE     (INRIA, Ecuador team)
!  Tapenade 3.16 (develop) - 14 Mar 2025 11:54
!
MODULE FORWARD_DIFF
  IMPLICIT NONE

CONTAINS
!  Differentiation of forward_problem in reverse (adjoint) mode (with options split(dummy_func) OpenMP):
!   gradient     of useful results: v
!   with respect to varying inputs: v xx
!   RW status of diff variables: v:in-zero xx:out
  SUBROUTINE FORWARD_PROBLEM_B(xx, xxb, v, vb)
    IMPLICIT NONE
    INTEGER :: i
    REAL*8, DIMENSION(10), INTENT(IN) :: xx
    REAL*8, DIMENSION(10) :: xxb
    REAL*8, DIMENSION(10) :: vi
    REAL*8, DIMENSION(10) :: vib
    REAL*8 :: temp
    REAL*8 :: tempb
    REAL*8 :: v
    REAL*8 :: vb
    INTRINSIC SUM
    REAL*8 :: result1
    REAL*8 :: result1b
    INTEGER*4 :: branch
    INTEGER :: chunk_start
    INTEGER :: chunk_end
!$OMP PARALLEL DEFAULT(shared), PRIVATE(i, temp), PRIVATE(result1, chunk_start, chunk_end)
    CALL GETSTATICSCHEDULE(1, 10, 1, chunk_start, chunk_end)
    DO i=chunk_start,chunk_end
      IF (xx(i)**2 + xx(i)**3 .GE. 0) THEN
! Inline function call
        CALL PUSHREAL8(result1)
        result1 = DUMMY_FUNC_FWD(xx(i))
        vi(i) = 1.0 + xx(i)*result1
! Not inline function call
        CALL PUSHREAL8(temp)
        temp = DUMMY_FUNC_FWD(xx(i))
        vi(i) = vi(i) + 1.0 + xx(i)*temp
        CALL PUSHCONTROL1B(1)
      ELSE
        CALL PUSHCONTROL1B(0)
      END IF
    END DO
    CALL PUSHREAL8(result1)
    CALL PUSHREAL8(temp)
!$OMP END PARALLEL
    vib = 0.0_8
    vib = vb
    xxb = 0.0_8
!$OMP PARALLEL DEFAULT(shared), PRIVATE(i, temp), PRIVATE(tempb), PRIVATE(result1, branch, chunk_end, chunk_start)
    CALL POPREAL8(temp)
    CALL POPREAL8(result1)
    tempb = 0.0_8
    xxb = 0.0_8
    CALL GETSTATICSCHEDULE(1, 10, 1, chunk_start, chunk_end)
    DO i=chunk_end,chunk_start,-1
      CALL POPCONTROL1B(branch)
      IF (branch .NE. 0) THEN
        xxb(i) = xxb(i) + temp*vib(i)
        tempb = xx(i)*vib(i)
        CALL DUMMY_FUNC_BWD(xx(i), xxb(i), tempb)
        CALL POPREAL8(temp)
        xxb(i) = xxb(i) + result1*vib(i)
        result1b = xx(i)*vib(i)
        vib(i) = 0.0_8
        CALL DUMMY_FUNC_BWD(xx(i), xxb(i), result1b)
        CALL POPREAL8(result1)
      END IF
    END DO
!$OMP END PARALLEL
    vb = 0.0_8
  END SUBROUTINE FORWARD_PROBLEM_B

  SUBROUTINE FORWARD_PROBLEM(xx, v)
    IMPLICIT NONE
    INTEGER :: i
    REAL*8, DIMENSION(10), INTENT(IN) :: xx
    REAL*8, DIMENSION(10) :: vi
    REAL*8 :: temp
    REAL*8, INTENT(OUT) :: v
    INTRINSIC SUM
    REAL*8 :: result1
!$OMP PARALLEL DO DEFAULT(shared), PRIVATE(i, temp), SCHEDULE(static)
    DO i=1,10
      IF (xx(i)**2 + xx(i)**3 .GE. 0) THEN
! Inline function call
        result1 = DUMMY_FUNC(xx(i))
        vi(i) = 1.0 + xx(i)*result1
! Not inline function call
        temp = DUMMY_FUNC(xx(i))
        vi(i) = vi(i) + 1.0 + xx(i)*temp
      END IF
    END DO
    v = SUM(vi)
  END SUBROUTINE FORWARD_PROBLEM

!  Differentiation of dummy_func in reverse (adjoint) mode, forward sweep (with options split(dummy_func) OpenMP):
!   gradient     of useful results: x dummy_func
!   with respect to varying inputs: x
  FUNCTION DUMMY_FUNC_FWD(x) RESULT (dummy_func)
    IMPLICIT NONE
    REAL*8 :: dummy_func
    REAL*8, INTENT(IN) :: x
    dummy_func = x + 1.0
  END FUNCTION DUMMY_FUNC_FWD

!  Differentiation of dummy_func in reverse (adjoint) mode, backward sweep (with options split(dummy_func) OpenMP):
!   gradient     of useful results: x dummy_func
!   with respect to varying inputs: x
  SUBROUTINE DUMMY_FUNC_BWD(x, xb, dummy_funcb)
    IMPLICIT NONE
    REAL*8 :: dummy_func
    REAL*8 :: dummy_funcb
    REAL*8, INTENT(IN) :: x
    REAL*8 :: xb
!$OMP ATOMIC update
    xb = xb + dummy_funcb
  END SUBROUTINE DUMMY_FUNC_BWD

  FUNCTION DUMMY_FUNC(x)
    IMPLICIT NONE
    REAL*8 :: dummy_func
    REAL*8, INTENT(IN) :: x
    dummy_func = x + 1.0
  END FUNCTION DUMMY_FUNC

END MODULE FORWARD_DIFF

