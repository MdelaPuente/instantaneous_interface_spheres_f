! Original Author: Rolf David
! Modified by: Miguel de la Puente
! Date: 04/2025
! License: GNU AGPLv3
! UTF-8, LF, Fortran2008

PROGRAM main

USE, INTRINSIC :: ISO_Fortran_env, ONLY: dp => REAL64
USE OMP_LIB

USE input

USE fibonacci, ONLY: fc_get_spherical_grid

USE sub, ONLY: fc_ij_vector,fc_get_center_of_slab,&
&fc_root_density_is_between,fc_trim_ext,sb_ridders_root

USE read_dcd, ONLY:sb_read_dcd_pdb
USE read_xyz, ONLY:sb_read_xyz

IMPLICIT NONE

!   ----------------------------------------------- Timings
REAL(dp)                        :: start,finish
!   -------------------------------------------------
CHARACTER(LEN=100)              :: input_file, log_file
CHARACTER(LEN=200)              :: step_name
INTEGER                         :: log_file_unit=30, ouput_file_unit=31
!   -------------------------------------------------
REAL(dp), ALLOCATABLE           :: atoms_mat(:,:,:), atoms_mat_i(:,:), grid(:,:,:), com_traj(:,:,:)
CHARACTER(LEN=3), ALLOCATABLE   :: atoms_types(:,:), com_types(:,:)
LOGICAL                         :: bool
REAL(dp)                        :: lbound_up(2), lbound_down(2), unit_vect(3)
REAL(dp), ALLOCATABLE           :: wc_interface_sphere(:,:,:)
!   -------------------------------------------------
INTEGER                         :: s, i, j, nb_o, k

! ----------------------------------------------------------------------------------------------
!   ----------------------------------------------- Get arguments (filenames, choices)
! ----------------------------------------------------------------------------------------------
IF ( COMMAND_ARGUMENT_COUNT() .LE. 0 ) THEN
    PRINT'(A100)', "No argument provided. Exiting..."
    STOP
ELSE IF ( COMMAND_ARGUMENT_COUNT() .GT. 2 ) THEN
    PRINT'(A100)', "Too many arguments provided. Exiting..."
    STOP
END IF

IF ( COMMAND_ARGUMENT_COUNT() .EQ. 1) THEN
    log_file='out.log'
ELSE
    CALL GET_COMMAND_ARGUMENT(2, log_file)
END IF

OPEN(UNIT=log_file_unit, FILE = log_file)

CALL GET_COMMAND_ARGUMENT(1, input_file)
input_file=TRIM(input_file)

INQUIRE(FILE=input_file,EXIST=bool)
IF ( .NOT.(bool)  )  THEN
    WRITE(log_file_unit,*) "The input file doesn't exist. Exiting..."
    STOP
END IF

CALL read_input(input_file,log_file_unit)

DO i=1,3
    box_dim(i) = box_bounds(2,i) - box_bounds(1,i)
    IF (box_dim(i) .LE. 0.1 ) THEN
        WRITE(log_file_unit,*) "The box seems very small. Please check. Exiting..."
        STOP
    END IF
END DO

WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&STARTING---------------------------------------------------')
WRITE(log_file_unit,'(A,I0)') 'Number of threads available: ', omp_get_max_threads()

! ----------------------------------------------------------------------------------------------
! -----------------------------------------------Read atomic positions file
! ----------------------------------------------------------------------------------------------
start = OMP_get_wtime()
step_name='reading atomic coordinate file'
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
WRITE(log_file_unit,'(A)') 'Start '//TRIM(step_name)//' ...'

file_coord=TRIM(file_coord)

IF (file_coord(SCAN( TRIM( file_coord ),".", .TRUE. )+1:) .EQ. "xyz") THEN
    IF ( (n_atoms .LE. 0 ) .OR. (n_frames .LE. 0) ) THEN
        WRITE(log_file_unit,*) "Number of Atoms/Frames must be defined for XYZ files. Exiting..."
        STOP
    END IF
    ALLOCATE(atoms_mat(5,n_atoms,n_frames))
    ALLOCATE(atoms_types(n_atoms,n_frames))
    atoms_mat(:,:,:) = 0.0_dp
    CALL sb_read_xyz(file_coord,n_atoms,n_frames,atoms_mat,atoms_types)
ELSE IF (file_coord(SCAN( TRIM( file_coord ),".", .TRUE. )+1:) .EQ. "dcd") THEN
    CALL sb_read_dcd_pdb(file_coord,n_atoms,n_frames,atoms_mat,atoms_types,log_file_unit)
ELSE
    WRITE(log_file_unit,*) "Can only read DCD or XYZ files. Exiting..."
    STOP
END IF

WRITE(log_file_unit,'(A,I0)') 'Final number of Atoms: ', n_atoms
WRITE(log_file_unit,'(A,I0)') 'Final number of Frames: ', n_frames

finish = OMP_get_wtime()
WRITE(log_file_unit,'(A,F10.2,A10)') TRIM("Done with "//step_name)//" :", finish-start, " seconds."
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
! ----------------------------------------------------------------------------------------------
! -----------------------------------------------Spawn the grid as a Fibonacci sphere
! ----------------------------------------------------------------------------------------------
start = OMP_get_wtime()
step_name='defining the grid (X,Y,Z) from the box size'
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
WRITE(log_file_unit,'(A)') 'Start '//TRIM(step_name)//' ...'

! Only one grid for all the calculation (ie. fixed COM)
IF ( file_com == "NONE" ) THEN
    grid = fc_get_spherical_grid(fixed_com, R_expected, d_grid)
ELSE
    IF ( (n_frames .LE. 0) ) THEN
        WRITE(log_file_unit,*) "Number of Atoms/Frames must be defined for XYZ COM files. Exiting..."
        STOP
    END IF

    ALLOCATE(com_traj(5,1,n_frames))
    ALLOCATE(com_types(1,n_frames))
    com_traj(:,:,:) = 0.0_dp
    CALL sb_read_xyz(file_com,1,n_frames,com_traj,com_types)
    grid = fc_get_spherical_grid( com_traj(3:5,1,1), R_expected, d_grid )
END IF

finish = OMP_get_wtime()
WRITE(log_file_unit,'(A,F10.2,A10)') TRIM("Done with "//step_name)//" :", finish-start, " seconds."
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
! ----------------------------------------------------------------------------------------------
! -----------------------------------------------Calculate spherical instantaneous interfaces (OpenMP by step)
! ----------------------------------------------------------------------------------------------
start = OMP_get_wtime()
step_name='calculating spherical instantaneous interface'
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
WRITE(log_file_unit,'(A)') 'Start '//TRIM(step_name)//' ...'

ALLOCATE(wc_interface_sphere(3,SIZE(grid,DIM=2),n_frames))
wc_interface_sphere=0.0_dp

IF ( file_coord .NE. '0') THEN
    IF ( first_O_atom .LE. 0 ) first_O_atom=1
    IF ( last_O_atom .EQ. -1 ) last_O_atom=n_atoms
    IF ( last_O_atom .GT. n_atoms ) last_O_atom=n_atoms
    IF ( first_O_atom .GE. last_O_atom ) THEN
        WRITE(log_file_unit,*) "Index of first oxygen can't .GE. than the last one. Exiting..."
        STOP
    END IF
ELSE
    WRITE(log_file_unit,*) "Not implemented, yet... Exiting..."
    STOP
END IF

!$OMP PARALLEL DO DEFAULT(NONE) SHARED(atoms_mat,com_traj,wc_interface_sphere)&
!$OMP SHARED(default_l_bound,default_h_bound,grid,xi,incr,box_dim,rho0,x_acc,f_acc)&
!$OMP SHARED(n_frames,n_atoms,first_O_atom,last_O_atom,log_file_unit)&
!$OMP PRIVATE(s, j, i,nb_o,k)&
!$OMP PRIVATE(atoms_mat_i,lbound_up,lbound_down,unit_vect)

DO s=1,n_frames
    ! Count number of O
    nb_o=COUNT(atoms_mat(2,first_O_atom:last_O_atom,s) .EQ. 16)
    ALLOCATE(atoms_mat_i(5,nb_o))
    ! Populate the matrix
    k=0
    DO i=first_O_atom,last_O_atom !n_atoms
        IF ( atoms_mat(2,i,s) .EQ. 16 ) THEN
            k=k+1
            atoms_mat_i(:,k) = atoms_mat(:,i,s)
        END IF
    END DO

    DO i=1,SIZE(grid,DIM=2) ! Loop over grid directions

        ! Get the bounds

        IF ( file_com == "NONE" ) THEN
            unit_vect = (grid(:,i,s) - fixed_com(:))/R_expected
        ELSE
            unit_vect = (grid(:,i,s) - com_traj(3:5,1,s))/R_expected
        END


        lbound_up = fc_root_density_is_between(default_l_bound,default_h_bound,grid(1,i,j),grid(2,i,j)&
        ,atoms_mat_i(3:5,:),xi,incr,z_bounds(2),box_dim,rho0,1.0_dp)

        IF ( (lbound_up(1) .EQ. 0.0_dp) .AND. (lbound_up(2) .EQ. 0.0_dp)) THEN
            WRITE(log_file_unit,*) "Error within the search of roots. Exiting..."
            STOP
        END IF

        CALL sb_ridders_root(lbound_up(1),lbound_up(2),grid(1,i,j),grid(2,i,j)&
        ,1,x_acc,f_acc,atoms_mat_i(3:5,:),xi,z_bounds(2),box_dim,rho0,wc_interface_upper(3,i,j,s))

        wc_interface_upper(1,i,j,s)=grid(1,i,j)
        wc_interface_upper(2,i,j,s)=grid(2,i,j)

        lbound_down = fc_root_density_is_between(default_l_bound,default_h_bound,grid(1,i,j),grid(2,i,j)&
        ,atoms_mat_i(3:5,:),xi,incr,z_bounds(2),box_dim,rho0,-1.0_dp)

        IF ( (lbound_down(1) .EQ. 0.0_dp) .AND. (lbound_down(2) .EQ. 0.0_dp)) THEN
            WRITE(log_file_unit,*) "Error within the search of roots. Exiting..."
            STOP
        END IF

        CALL sb_ridders_root(lbound_down(1),lbound_down(2),grid(1,i,j),grid(2,i,j)&
        ,-1,x_acc,f_acc,atoms_mat_i(3:5,:),xi,z_bounds(2),box_dim,rho0,wc_interface_lower(3,i,j,s))

        wc_interface_lower(1,i,j,s)=grid(1,i,j)
        wc_interface_lower(2,i,j,s)=grid(2,i,j)

    END DO
    DEALLOCATE(atoms_mat_i)
END DO
!$OMP END PARALLEL DO

finish = OMP_get_wtime()
WRITE(log_file_unit,'(A,F10.2,A10)') TRIM("Done with "//step_name)//" :", finish-start, " seconds."
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
! ----------------------------------------------------------------------------------------------
! -----------------------------------------------Write XYZ interfaces
! ----------------------------------------------------------------------------------------------
start = OMP_get_wtime()
step_name='writing coordinates for the upper interface (XU)'
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
WRITE(log_file_unit,'(A)') 'Start '//TRIM(step_name)//' ...'

OPEN(UNIT=ouput_file_unit, FILE = fc_trim_ext(file_coord)//"-surface-upper.xyz")
DO s=1,n_frames
    WRITE(ouput_file_unit,'(I0)') SIZE(grid,DIM=2)*SIZE(grid,DIM=3)
    WRITE(ouput_file_unit,'(A9,I0)') "Frame: ", s
    DO i=1,SIZE(grid,DIM=2)
        DO j=1,SIZE(grid,DIM=3)
            WRITE(ouput_file_unit,'(A2,1X,F0.4,1X,F0.4,1X,F0.4)') ADJUSTL( "XU" )&
            , wc_interface_upper(1,i,j,s), wc_interface_upper(2,i,j,s), wc_interface_upper(3,i,j,s)
        END DO
    END DO
END DO
CLOSE(UNIT=ouput_file_unit)
finish = OMP_get_wtime()
WRITE(log_file_unit,'(A,F10.2,A10)') TRIM("Done with "//step_name)//" :", finish-start, " seconds."
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
! ----------------------------------------------------------------------------------------------
start = OMP_get_wtime()
step_name='writing coordinates for the lower interface (XL)'
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
WRITE(log_file_unit,'(A)') 'Start '//TRIM(step_name)//' ...'

OPEN(UNIT=ouput_file_unit, FILE = fc_trim_ext(file_coord)//"-surface-lower.xyz")
DO s=1,n_frames
    WRITE(ouput_file_unit,'(I0)') SIZE(grid,DIM=2)*SIZE(grid,DIM=3)
    WRITE(ouput_file_unit,'(A9,I0)') "Frame: ", s
    DO i=1,SIZE(grid,DIM=2)
        DO j=1,SIZE(grid,DIM=3)
            WRITE(ouput_file_unit,'(A2,1X,F0.4,1X,F0.4,1X,F0.4)') ADJUSTL( "XL" )&
            , wc_interface_lower(1,i,j,s), wc_interface_lower(2,i,j,s), wc_interface_lower(3,i,j,s)
        END DO
    END DO
END DO
CLOSE(UNIT=ouput_file_unit)
finish = OMP_get_wtime()
WRITE(log_file_unit,'(A,F10.2,A10)') TRIM("Done with "//step_name)//" :", finish-start, " seconds."
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&-----------------------------------------------------------')
WRITE(log_file_unit,'(A100)') TRIM('-----------------------------------------&
&END--------------------------------------------------------')
! ----------------------------------------------------------------------------------------------
END PROGRAM main