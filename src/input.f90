! Author: Rolf David
! Date: 06/2022
! License: GNU AGPLv3
! UTF-8, LF, Fortran2008

MODULE input
    USE, INTRINSIC :: ISO_Fortran_env, ONLY: dp => REAL64

    IMPLICIT NONE
    ! -----------------------------------------------Reading related variables
    CHARACTER(LEN=100)          :: label, value
    INTEGER                     :: iostatus=0, line_c=0, delim
    INTEGER, PARAMETER          :: input_file_unit=99
    ! -----------------------------------------------Variables
    CHARACTER(LEN=64)           :: file_coord='0', file_index='NONE'
    INTEGER                     :: n_atoms=-1, n_frames=-1
    REAL(dp)                    :: box_dim(3), box_bounds(2,3)
    INTEGER                     :: first_O_atom=-1, last_O_atom=-1
    REAL(dp)                    :: rho0=0.016_dp, xi=2.4_dp
    REAL(dp)                    :: default_l_bound=10.0_dp
    REAL(dp)                    :: default_h_bound=25.0_dp
    REAL(dp)                    :: d_grid=1.0_dp, incr=1.0_dp
    REAL(dp)                    :: x_ACC=0.01_dp, f_ACC=6.25e-4_dp
    REAL(dp)                    :: fixed_com(3), R_expected=10.0
    CHARACTER(LEN=64)           :: file_com="NONE"

    CONTAINS

    SUBROUTINE read_input(input_file,log_file_unit)
        CHARACTER(LEN=100)      :: input_file
        INTEGER                 :: log_file_unit

        box_bounds(:,:) = 0.0_dp
        box_dim(:) = 0.0_dp
        fixed_com(:) = 0.0_dp

        OPEN(input_file_unit, file=input_file)
        RL:DO WHILE ( iostatus == 0 )
            READ(input_file_unit, '(A)', IOSTAT=iostatus) value
            IF ( iostatus == 0 )then
                line_c=line_c + 1
                delim=SCAN( value, '    ' )
                label=value(1:delim)
                value=value(delim + 1:)
                IF ( label(1:1) == '!' )THEN
                    CYCLE RL
                END IF
                SELECT CASE (label)
                    CASE ('file_coord')
                        READ(value, * , IOSTAT=iostatus) file_coord
                    CASE ('file_index')
                        READ(value, * , IOSTAT=iostatus) file_index
                    CASE ('n_atoms')
                        READ(value, * , IOSTAT=iostatus) n_atoms
                    CASE ('n_frames')
                        READ(value, * , IOSTAT=iostatus) n_frames
                    CASE ('xlo')
                        READ(value, * , IOSTAT=iostatus) box_bounds(1,1)
                    CASE ('xhi')
                        READ(value, * , IOSTAT=iostatus) box_bounds(2,1)
                    CASE ('ylo')
                        READ(value, * , IOSTAT=iostatus) box_bounds(1,2)
                    CASE ('yhi')
                        READ(value, * , IOSTAT=iostatus) box_bounds(2,2)
                    CASE ('zlo')
                        READ(value, * , IOSTAT=iostatus) box_bounds(1,3)
                    CASE ('zhi')
                        READ(value, * , IOSTAT=iostatus) box_bounds(2,3)
                    CASE ('rho0')
                        READ(value, * , IOSTAT=iostatus) rho0
                    CASE ('xi')
                        READ(value, * , IOSTAT=iostatus) xi
                    CASE ('f_ACC')
                        READ(value, * , IOSTAT=iostatus) f_ACC
                    CASE ('x_ACC')
                        READ(value, * , IOSTAT=iostatus) x_ACC
                    CASE ('default_l_bound')
                        READ(value, * , IOSTAT=iostatus) default_l_bound
                    CASE ('default_h_bound')
                        READ(value, * , IOSTAT=iostatus) default_h_bound
                    CASE ('d_grid')
                        READ(value, * , IOSTAT=iostatus) d_grid
                    CASE ('incr')
                        READ(value, * , IOSTAT=iostatus) incr
                    CASE ('first_O_atom')
                        READ(value, * , IOSTAT=iostatus) first_O_atom
                    CASE ('last_O_atom')
                        READ(value, * , IOSTAT=iostatus) last_O_atom
                    CASE ('R0')
                        READ(value, * , IOSTAT=iostatus) R_expected
                    CASE ('xcom')
                        READ(value, * , IOSTAT=iostatus) fixed_com(1)
                    CASE ('ycom')
                        READ(value, * , IOSTAT=iostatus) fixed_com(2)
                    CASE ('zcom')
                        READ(value, * , IOSTAT=iostatus) fixed_com(3)
                    CASE ('file_com')
                        READ(value, * , IOSTAT=iostatus) file_com
                    CASE DEFAULT
                        PRINT'(A65,I64)', 'Invalid label, line:', line_c
                END SELECT
            END IF
        END DO RL
        WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
        &INPUT PARAMETERS-------------------------------------------')
        WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
        &-----------------------------------------------------------')
        WRITE(log_file_unit,'(A,A)') 'Atomic coordinates file: ', ADJUSTL(TRIM(file_coord))
        WRITE(log_file_unit,'(A,I0)') 'Number of Atoms: ', n_atoms
        WRITE(log_file_unit,'(A)') '[Overridden/Optionnal if DCD]'
        WRITE(log_file_unit,'(A,I0)') 'Number of Frames: ', n_frames
        WRITE(log_file_unit,'(A)') '[if DCD and omitted/-1, use all frames]'
        WRITE(log_file_unit,'(A)') 'Box dimensions: '
        WRITE(log_file_unit,'(A1,1X,F30.10,1X,F30.10)') 'X',box_bounds(1,1),box_bounds(2,1)
        WRITE(log_file_unit,'(A1,1X,F30.10,1X,F30.10)') 'Y',box_bounds(1,2),box_bounds(2,2)
        WRITE(log_file_unit,'(A1,1X,F30.10,1X,F30.10)') 'Z',box_bounds(1,3),box_bounds(2,3)
        
        IF (file_com == "NONE") THEN
            WRITE(log_file_unit,'(A)') 'Center of mass (COM) position: '
            WRITE(log_file_unit,'(A1,1X,F30.10)') 'X', fixed_com(1)
            WRITE(log_file_unit,'(A1,1X,F30.10)') 'Y', fixed_com(2)
            WRITE(log_file_unit,'(A1,1X,F30.10)') 'Z', fixed_com(3)
        ELSE
            WRITE(log_file_unit,'(A,A)') 'Center of mass (COM) coordinates file: ',&
            ADJUSTL(TRIM(file_com))
        END IF
        
        WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
        &-----------------------------------------------------------')
        WRITE(log_file_unit,'(A,A)') 'Selection index file: ', ADJUSTL(TRIM(file_index))
        WRITE(log_file_unit,'(A)') 'Index for oxygen atom selection Start,End: '
        WRITE(log_file_unit,'(A1,I0,A1,I0,A1)') '',first_O_atom,',',last_O_atom,''
        WRITE(log_file_unit,'(A)') '[-1,-1] (start from first, end at last)]'
        WRITE(log_file_unit,'(A,F0.3)') 'ρ0 [0.016 Å^-3]: ', rho0
        WRITE(log_file_unit,'(A,F0.3)') 'ξ [2.4 Å]: ', xi
        WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
        &-----------------------------------------------------------')
        WRITE(log_file_unit,'(A)') 'Root search will start betwen COM+lbound and COM+hbound'
        WRITE(log_file_unit,'(A,F0.3)') 'Starting value for lbound [10 Å]: ', default_l_bound
        WRITE(log_file_unit,'(A,F0.3)') 'Starting value for hbound [25 Å]: ', default_h_bound
        WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
        &-----------------------------------------------------------')
        WRITE(log_file_unit,'(A,F0.4)') 'Expected spherical grid radius R0 = ', R_expected
        WRITE(log_file_unit,'(A,F0.3)') 'Spacing for the grid [1 Å]: ', d_grid
!        WRITE(log_file_unit,'(A)') '(or FLOOR(box_dim) gridpoints evenly spaced)]'
        WRITE(log_file_unit,'(A,F0.3)') 'Increment for the bound search [1 Å]: ', incr
        WRITE(log_file_unit,'(A,E20.5)') "x Accuracy for Ridders' root [0.01]: ", x_ACC
        WRITE(log_file_unit,'(A,E20.5)') "f Accuracy for Ridders' root [6.25e-4]: ", f_ACC
        WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
        &-----------------------------------------------------------')
        WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
        &END OF INPUT PARAMETERS------------------------------------')
        WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
        &-----------------------------------------------------------')
        WRITE(log_file_unit,'(A1)') ' '
    END SUBROUTINE read_input

END MODULE input
