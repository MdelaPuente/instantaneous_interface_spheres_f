!!!!!! It works
! PROGRAM test_fib
!   USE fibonacci
!   IMPLICIT NONE
!   REAL(dp) :: x, y
!   REAL(dp) :: res(2), com(3)
!   REAL(dp), ALLOCATABLE :: sphere(:,:)
!   INTEGER :: ouput_file_unit=31, i
! 
!   x = 0.3_dp
!   y = 0.7_dp
!   res = cartesian_to_polar(x, y)
!   PRINT *, "Polar coords:", res

!   PRINT *, "Golden ratio:", phi_gold

!   com(:) = 31.5
!   sphere = fc_get_spherical_grid(com, 5.0_dp, 1.0_dp)

!   OPEN(UNIT=ouput_file_unit, FILE = "test-sphere-recentered.xyz")
!   WRITE(ouput_file_unit,'(I0)') SIZE(sphere,DIM=2)
!   WRITE(ouput_file_unit,'(A9,F0.4)') "# Test on a Fibonacci sphere of radius =", 5.0_dp
!   DO i=1,SIZE(sphere,DIM=2)
!       WRITE(ouput_file_unit, '(A1,1X,F0.4,1X,F0.4,1X,F0.4)') "X", sphere(1,i), sphere(2,i), sphere(3,i)
!   END DO
!   CLOSE(ouput_file_unit)
  
! END PROGRAM test_fib


!!!!!! It works 
! PROGRAM test_input
!     USE input
!     IMPLICIT NONE
!     CHARACTER(LEN=64) :: log_file="out-test.log"
!     CHARACTER(LEN=100) :: input_file
!     INTEGER :: log_file_unit=30
!     LOGICAL :: bool

!     OPEN(UNIT=log_file_unit, FILE=log_file)

!     CALL GET_COMMAND_ARGUMENT(1, input_file)
!     input_file = TRIM(input_file)
!     INQUIRE(FILE=input_file,EXIST=bool)
!     IF ( .NOT.(bool)  )  THEN
!         WRITE(log_file_unit,*) "The input file doesn't exist. Exiting..."
!         STOP
!     END IF
    
!     CALL read_input(input_file,log_file_unit)
!     CLOSE(UNIT=log_file_unit)

!     PRINT *, "File with COM positions: ", file_com
    
! END PROGRAM test_input



