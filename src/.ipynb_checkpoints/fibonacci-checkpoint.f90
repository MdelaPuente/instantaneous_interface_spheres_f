! Author: Miguel de la Puente
! Date: 04/2025
! License: GNU AGPLv3
! UTF-8, LF, Fortran2008

MODULE fibonacci
    USE, INTRINSIC :: ISO_Fortran_env, ONLY: dp => REAL64

    IMPLICIT NONE
    ! -----------------------------------------------Set Double precision
    REAL(dp), PARAMETER         :: c_pi=4.0_dp*DATAN(1.0_dp)
    REAL(dp), PARAMETER         :: phi_gold= 0.5_dp*(1.0_dp + sqrt(5.0_dp))

    CONTAINS

    ! Number of points separated by d in a sphere of radius r 
    PURE FUNCTION number_of_points(r, d) RESULT(Np)
        REAL(dp), INTENT(IN) :: r, d
        INTEGER :: Np

        Np = FLOOR(4*c_pi*(r**2)/(d**2))
    END FUNCTION

    ! Polar transformation of cartesian coordinates
    PURE FUNCTION cartesian_to_polar(x, y) RESULT(polar)
        REAL(dp), INTENT(IN) :: x, y
        REAL(dp) :: polar(2)

        polar(1) = ACOS( 1 - 2*x)
        polar(2) = 2*c_pi*y
    END FUNCTION

    ! From polar coordinates give (x,y,z) of point in sphere
    PURE FUNCTION polar_to_sphere(polar, r) RESULT(cartesian)
        REAL(dp), INTENT(IN) :: polar(2), r
        REAL(dp) :: cartesian(3)

        cartesian(1) = r*COS(polar(2))*SIN(polar(1))
        cartesian(2) = r*SIN(polar(2))*SIN(polar(1))
        cartesian(3) = r*COS(polar(1))
    END FUNCTION

    ! Create a Fibonacci sphere of radius r with N points
    PURE FUNCTION fibonacci_sphere(N, r) RESULT(sphere)
        REAL(dp), INTENT(IN) :: r
        INTEGER, INTENT(IN) :: N
        REAL(dp) :: sphere(3,N), polar(2)
        INTEGER :: i
        
        DO i = 1,N
            polar = cartesian_to_polar( 1.0_dp*i/N , 1.0_dp*i/phi_gold)
            sphere(:,i) = polar_to_sphere( polar, r )
        END DO
    END FUNCTION
    
    ! Spawn a spherical grid using the Fibonacci sphere centered around COM:
    PURE FUNCTION fc_get_spherical_grid(com_mat, R_expected, d_grid) RESULT(grid)
        REAL(dp), INTENT(IN) :: com_mat(3), R_expected, d_grid
        INTEGER :: Npoints
        REAL(dp), ALLOCATABLE :: grid(:,:)

        Npoints = number_of_points(R_expected, d_grid)
        ALLOCATE(grid(3,Npoints))
        grid = fibonacci_sphere(Npoints, R_expected)
        grid(1,:) = grid(1,:) + com_mat(1)
        grid(2,:) = grid(2,:) + com_mat(2)
        grid(3,:) = grid(3,:) + com_mat(3)
    END FUNCTION
        

END MODULE fibonacci