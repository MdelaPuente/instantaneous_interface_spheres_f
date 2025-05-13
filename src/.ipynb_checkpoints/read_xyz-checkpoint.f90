! Author: Rolf David
! Date: 06/2022
! License: GNU AGPLv3
! UTF-8, LF, Fortran2008

MODULE read_xyz
    USE, INTRINSIC :: ISO_Fortran_env, ONLY: dp => REAL64

    IMPLICIT NONE

    CONTAINS

    SUBROUTINE sb_read_xyz(xyz_file,n_atoms,n_frames,atoms_mat,atoms_types)
        CHARACTER(LEN=64), INTENT(IN)                :: xyz_file
        INTEGER, INTENT(IN)                          :: n_atoms, n_frames
        REAL(dp), INTENT(INOUT)                      :: atoms_mat(:,:,:)
        CHARACTER(LEN=3), ALLOCATABLE, INTENT(INOUT) :: atoms_types(:,:)
        !-- Procedure only
        INTEGER             :: xyz_file_unit=90
        INTEGER             :: s, i
        CHARACTER(LEN=1)    :: temp_char

        OPEN(UNIT=xyz_file_unit,FILE=xyz_file,STATUS='old',FORM='formatted',ACTION='READ')
        DO s = 1, n_frames
            temp_char = '#'
            DO WHILE (temp_char .eq. "#")
                READ(xyz_file_unit,'(A)') temp_char
            END DO
            BACKSPACE(xyz_file_unit)
            READ(xyz_file_unit, *)
            READ(xyz_file_unit, *)
            DO i = 1, n_atoms
                atoms_mat(2,i,s) = -1
                READ(xyz_file_unit, *) atoms_types(i,s), atoms_mat(3,i,s), atoms_mat(4,i,s), atoms_mat(5,i,s)
                atoms_mat(1,i,s) = i
                IF ( atoms_types(i,s) .EQ. "C" ) THEN
                    atoms_mat(2,i,s) = 12
                ELSE IF ( atoms_types(i,s) .EQ. "O" ) THEN
                    atoms_mat(2,i,s) = 16
                ELSE IF ( atoms_types(i,s) .EQ. "N" ) THEN
                    atoms_mat(2,i,s) = 14
                ELSE IF ( atoms_types(i,s) .EQ. "H" ) THEN
                    atoms_mat(2,i,s) = 1
                ELSE IF ( atoms_types(i,s) .EQ. "XL" ) THEN
                    atoms_mat(2,i,s) = 200
                ELSE IF ( atoms_types(i,s) .EQ. "XU" ) THEN
                    atoms_mat(2,i,s) = 201
                ELSE IF ( atoms_types(i,s) .EQ. "COM" ) THEN
                    atoms_mat(2,i,s) = 300
                END IF
            END DO
        END DO
        CLOSE(UNIT=xyz_file_unit)
    END SUBROUTINE sb_read_xyz

END MODULE read_xyz