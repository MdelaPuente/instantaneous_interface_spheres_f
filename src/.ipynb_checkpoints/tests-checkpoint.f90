PROGRAM test_fib
  USE fibonacci
  IMPLICIT NONE
  REAL(dp) :: x, y
  REAL(dp) :: result(2)

  x = 0.3_dp
  y = 0.7_dp
  result = cartesian_to_polar(x, y)
  PRINT *, "Polar coords:", result

  PRINT *, "Golden ratio:", phi_gold
END PROGRAM test_fib