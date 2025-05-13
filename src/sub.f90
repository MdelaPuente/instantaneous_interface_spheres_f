! Original Author: Rolf David
! Modified by: Miguel de la Puente
! Date: 06/2022
! License: GNU AGPLv3
! UTF-8, LF, Fortran2008

MODULE sub
    USE, INTRINSIC :: ISO_Fortran_env, ONLY: dp => REAL64

    IMPLICIT NONE
    ! -----------------------------------------------Set Double precision
    REAL(dp), PARAMETER         :: c_pi=4.0_dp*DATAN(1.0_dp)

    CONTAINS

    FUNCTION sb_count_nb_line(sb_file_is) RESULT(nb_of_lines)
        CHARACTER(LEN=64), INTENT(IN) :: sb_file_is
        !-- Procedure only
        INTEGER :: nb_of_lines
        INTEGER :: sb_iostatus

        OPEN(UNIT=21, FILE=sb_file_is, STATUS='old', FORM='formatted', ACTION='READ')
        DO
            READ(21, *, IOSTAT=sb_iostatus)
            IF ( sb_iostatus .NE. 0 ) THEN
                EXIT
            ELSE
                nb_of_lines = nb_of_lines + 1
            END IF
        END DO
        CLOSE(UNIT=21)
    END FUNCTION sb_count_nb_line

    PURE FUNCTION fc_trim_ext(string_in) RESULT(string_out)
        CHARACTER(*), INTENT(IN) :: string_in
        CHARACTER(:), ALLOCATABLE :: string_out

        string_out = TRIM( string_in(1:SCAN( TRIM( string_in ),".", .TRUE. )-1) )
    END FUNCTION

    PURE FUNCTION fc_ij_vector(pos_i,pos_j,box_dim) RESULT(ij_vector)
        REAL(dp), INTENT(IN) :: pos_i(:), pos_j(:)
        REAL(dp), INTENT(IN), OPTIONAL :: box_dim(:)
        REAL(dp)  :: ij_vector(3)

        ij_vector = pos_j - pos_i
        IF ( PRESENT(box_dim))&
         ij_vector = ij_vector - box_dim * ANINT( ij_vector / box_dim )
    END FUNCTION fc_ij_vector

    PURE FUNCTION fc_phi(r,xi) RESULT(phi_r_xi)
        REAL(dp), INTENT(IN) :: r,xi
        REAL(dp) :: phi_r_xi

        phi_r_xi=(2 * c_pi * (xi**2))**(-1.5_dp) * EXP(-(r**2)/(2*xi**2))
    END FUNCTION

    PURE FUNCTION fc_rho_norm_diff_at_r(r,mat_xyz_atm,xi,box_dim,rho0) RESULT(rho_norm_diff_at_r)
        REAL(dp) :: rho, rho_norm_diff_at_r
        REAL(dp), INTENT(IN) :: r(:),xi,rho0
        REAL(dp), INTENT(IN) :: mat_xyz_atm(:,:), box_dim(:)
        INTEGER :: i

        rho=0.0_dp
        DO i=1,SIZE(mat_xyz_atm(:,:),DIM=2)
            rho=rho+fc_phi(NORM2(fc_ij_vector(mat_xyz_atm(:,i),r,box_dim)),xi)
        END DO
        rho_norm_diff_at_r = (rho/rho0 - 1.0_dp )
    END FUNCTION

    PURE FUNCTION fc_root_density_is_between_sphere(a,b,unit_vec,atoms,xi,increment,com,box_dim,rho0,direction)&
     RESULT(root_density_bounds_low_up)
        REAL(dp), INTENT(IN) :: a,b,x_i,x_j,xi,increment,z_orig,box_dim(:),rho0,direction
        REAL(dp), INTENT(IN):: atoms(:,:), unit_vec(3), com(3)
        REAL(dp):: root_density_bounds_low_up(2), low_bound,hi_bound,l_i, l_h

        IF ( a .GT. b) THEN
            root_density_bounds_low_up=[0.0_dp,0.0_dp]
            RETURN
        END IF
        low_bound = fc_rho_norm_diff_at_r( a*unit_vec + com, atoms,xi,box_dim,rho0)
        hi_bound  = fc_rho_norm_diff_at_r( b*unit_vec + com, atoms,xi,box_dim,rho0)
        l_i = 0.0_dp
        l_h = 0.0_dp

        ! Density at a should be above/below rho0 if system is droplet/cavity
        IF (low_bound*direction .LE. 0.0 ) THEN
            counts1 = 1
            DO WHILE (fc_rho_norm_diff_at_r( (a-l_i)*unit_vec + com , atoms,xi,box_dim,rho0)*direction .LE. 0.0)
                l_i=l_i+increment
            END DO
        END IF
        IF (hi_bound*direction .GE. 0.0 ) THEN
            DO WHILE (fc_rho_norm_diff_at_r( (b+l_h)*unit_vec + com ,atoms,xi,box_dim,rho0)*direction .GE. 0.0)
                l_h=l_h+increment
            END DO
        END IF
        root_density_bounds_low_up=[a-l_i,b+l_h]
    END FUNCTION

    SUBROUTINE sb_ridders_root_sphere(a,b,unit_vec,x_accuracy,f_accuracy,atoms,xi,com,box,rho0,result)
        REAL(dp), INTENT(IN) :: a,b,unit_vec(3),x_accuracy,f_accuracy,xi,com(3),box(3),rho0
        REAL(dp), INTENT(IN) :: atoms(:,:)
        REAL(dp), INTENT(OUT) :: result
        INTEGER :: MAXITER,counts
        REAL(dp) :: x_low,x_high,f_low,f_high,x_m,f_m,s,x_new,root,f_new

        MAXITER=100
        x_low=a
        x_high=b
        f_low=fc_rho_norm_diff_at_r( x_low*unit_vec + com ,atoms,xi,box,rho0)
        f_high=fc_rho_norm_diff_at_r( x_high*unit_vec + com ,atoms,xi,box,rho0)

        IF (ABS(f_low) .LT. f_accuracy ) THEN
            result = x_low*unit_vec + com
            RETURN
        END IF
        IF (ABS(f_high) .LT. f_accuracy ) THEN
            root = x_high*unit_vec + com
            RETURN
        END IF

        root = 0
        DO counts=1,MAXITER
            x_m = 0.5*(x_low + x_high)
            f_m = fc_rho_norm_diff_at_r(x_m*unit_vec + com,atoms,xi,box,rho0)
            s = SQRT(f_m**2 - f_low*f_high)

            IF ( s .LT. 1.0e-9) THEN
                PRINT'(A100)', "Noway. Exiting..."
                STOP
            END IF

            x_new = x_m + (x_m - x_low) * (SIGN(1.0_dp,(f_low-f_high)) * f_m / s)

            IF ( ABS(x_new - root) .LT. x_accuracy) THEN
                result = x_new*unit_vec + com
                RETURN
            END IF

            root = x_new
            f_new = fc_rho_norm_diff_at_r([x_i,x_j,direction*root+z_orig],atoms,xi,box,rho0)
            IF ( ABS(f_new) < f_accuracy ) THEN
                result = root*unit_vec + com
                RETURN
            END IF

            IF ( SIGN(1.0_dp,f_m) .NE. SIGN(1.0_dp,f_new) ) THEN
                x_low = x_m
                f_low = f_m
                x_high = root
                f_high = f_new
            ELSE IF ( SIGN(1.0_dp, f_low) .NE. SIGN(1.0_dp,f_new) ) THEN
                x_high=root
                f_high = f_new
            ELSE IF ( SIGN(1.0_dp, f_high) .NE. SIGN(1.0_dp,f_new) ) THEN
                x_low=root
                f_low=f_new
            END IF
            IF (ABS(x_high - x_low) .LE. x_accuracy) THEN
                result = root*unit_vec + com
                RETURN
            END IF

        END DO
        PRINT'(A100)', "NOTCONV Noway. Exiting..."
        WRITE(*,*) x_high,x_low,f_high,f_low
        STOP

    END SUBROUTINE

END MODULE sub