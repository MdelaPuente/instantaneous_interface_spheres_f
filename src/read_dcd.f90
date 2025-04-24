! Author: Rolf David
! Date: 06/2022
! License: GNU AGPLv3
! UTF-8, LF, Fortran2008

MODULE read_dcd
    USE, INTRINSIC :: ISO_Fortran_env, ONLY: sp => REAL32, dp => REAL64

    IMPLICIT NONE

    CONTAINS

    SUBROUTINE sb_read_dcd_pdb(dcd_file,n_atoms,n_frames,atoms_mat,atoms_types,log_file_unit)
        CHARACTER(LEN=64), INTENT(IN)                :: dcd_file
        INTEGER, INTENT(OUT)                         :: n_atoms
        INTEGER, INTENT(INOUT)                       :: n_frames
        REAL(dp), ALLOCATABLE, INTENT(INOUT)         :: atoms_mat(:,:,:)
        CHARACTER(LEN=3), ALLOCATABLE, INTENT(INOUT) :: atoms_types(:,:)
        INTEGER, INTENT(IN)                          :: log_file_unit

        !-- Procedure only
        INTEGER                                      :: s,i
        CHARACTER(LEN=64)                            :: pdb_file
        CHARACTER(LEN=5)                             :: temp_char
        REAL(dp)                                     :: d(6)
        REAL(dp)                                     :: dummyr
        REAL(sp), ALLOCATABLE                        :: x(:),y(:),z(:)
        INTEGER                                      :: n_frames_dcd,dummyi
        INTEGER                                      :: iostat_ok
        CHARACTER(LEN=4)                             :: dummyc
        LOGICAL                                      :: file_status
        INTEGER                                      :: dcd_file_unit=90,pdb_file_unit=90

        OPEN(dcd_file_unit, FILE=TRIM(dcd_file), STATUS='old', FORM='unformatted', ACTION='READ')
        READ(dcd_file_unit, IOSTAT=iostat_ok) dummyc, n_frames_dcd, (dummyi,i=1,8), dummyr, (dummyi,i=1,9)
        IF (iostat_ok .NE. 0) THEN
            WRITE(log_file_unit,'(A)') "Error reading DCD. Exiting..."
            STOP
        END IF

        READ(dcd_file_unit) dummyi, dummyr
        READ(dcd_file_unit) n_atoms

        IF (n_frames .LE. 0) THEN
            n_frames = n_frames_dcd
        ELSE IF (n_frames .GT. n_frames_dcd) THEN
            n_frames = n_frames_dcd
        END IF

        ALLOCATE(x(n_atoms),y(n_atoms),z(n_atoms))
        ALLOCATE(atoms_mat(5,n_atoms,n_frames))
        ALLOCATE(atoms_types(n_atoms,n_frames))
        x(:) = 0.0_dp
        y(:) = 0.0_dp
        z(:) = 0.0_dp
        atoms_mat(:,:,:) = 0.0_dp
        atoms_types(:,:) = 'UNK'

        DO s=1,n_frames
           READ(dcd_file_unit) (d(i),i=1,6) ! Cell size !
           READ(dcd_file_unit) (x(i),i=1,n_atoms)
           READ(dcd_file_unit) (y(i),i=1,n_atoms)
           READ(dcd_file_unit) (z(i),i=1,n_atoms)
           atoms_mat(3,:,s)=x(:)
           atoms_mat(4,:,s)=y(:)
           atoms_mat(5,:,s)=z(:)
        END DO
        DO s=1,n_frames
            DO i=1,n_atoms
                atoms_mat(1,i,s) = i
            END DO
        END DO
        DEALLOCATE(x,y,z)
        CLOSE(UNIT=dcd_file_unit)

        pdb_file=TRIM(dcd_file(1:SCAN(TRIM( dcd_file ),".",.TRUE.)-1) )//".pdb"
        INQUIRE(FILE=pdb_file,EXIST=file_status)
        IF ( .NOT.(file_status) ) THEN
            WRITE(log_file_unit,*) "No PDB associated with the DCD file. Exiting..."
            STOP
        END IF
        OPEN(UNIT=pdb_file_unit,FILE=pdb_file,STATUS='old',FORM='formatted',ACTION='READ')
        temp_char = 'X'
        DO WHILE (temp_char .NE. "ATOM")
            READ(pdb_file_unit,'(A)') temp_char
        END DO
        BACKSPACE(pdb_file_unit)
        DO i = 1, n_atoms
            READ(pdb_file_unit, *) dummyc, dummyi, atoms_types(i,1), dummyc
            IF ( atoms_types(i,1) .EQ. "C" ) THEN
                atoms_mat(2,i,1) = 12
            ELSE IF ( atoms_types(i,1) .EQ. "O" ) THEN
                atoms_mat(2,i,1) = 16
            ELSE IF ( atoms_types(i,1) .EQ. "N" ) THEN
                atoms_mat(2,i,1) = 14
            ELSE IF ( atoms_types(i,1) .EQ. "H" ) THEN
                atoms_mat(2,i,1) = 1
            ELSE
                atoms_mat(2,i,1) = -1
            END IF
        END DO
        DO s = 2, n_frames
            atoms_types(:,s) = atoms_types(:,1)
            atoms_mat(2,:,s) =  atoms_mat(2,:,1)
        END DO
        CLOSE(UNIT=pdb_file_unit)

    END SUBROUTINE sb_read_dcd_pdb

END MODULE read_dcd