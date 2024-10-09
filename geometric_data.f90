! -------------------------------------------------------------------------------------------------------
! @file geometric_data.f90
! @brief Module for handling scalar and vector data associated with points or cells.
! @author ofgn
! @date 2024-07-01
! -------------------------------------------------------------------------------------------------------
module geometric_data
    
    use iso_fortran_env, only: int32, int64, real32, real64
    use utility

    implicit none

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Type definition for handling scalar and vector data in various 
    ! formats (integer, real32, real64) associated with points or cells.
    ! ---------------------------------------------------------------------------------------------------
    type :: GeometricData
        integer(int32) :: n = 0                                     ! Number of points or cells
        integer, allocatable :: scalar_integer(:, :)                ! Integer scalar data
        real(real32), allocatable :: scalar_real32(:, :)            ! real(real32) scalar data
        real(real64), allocatable :: scalar_real64(:, :)            ! real(real64) scalar data
        character(len=32), allocatable :: scalar_integer_labels(:)  ! Labels for integer scalar data
        character(len=32), allocatable :: scalar_real32_labels(:)   ! Labels for real(real32) scalar data
        character(len=32), allocatable :: scalar_real64_labels(:)   ! Labels for real(real64) scalar data
        integer, allocatable :: scalar_integer_components(:)        ! Components of integer scalar data
        integer, allocatable :: scalar_real32_components(:)         ! Components of real(real32) scalar data
        integer, allocatable :: scalar_real64_components(:)         ! Components of real(real64) scalar data
        integer, allocatable :: vector_integer(:, :)                ! Integer vector data
        real(real32), allocatable :: vector_real32(:, :)            ! real(real32) vector data
        real(real64), allocatable :: vector_real64(:, :)            ! real(real64) vector data
        character(len=32), allocatable :: vector_integer_labels(:)  ! Labels for integer vector data
        character(len=32), allocatable :: vector_real32_labels(:)   ! Labels for real(real32) vector data
        character(len=32), allocatable :: vector_real64_labels(:)   ! Labels for real(real64) vector data
    contains
        procedure :: info                                           ! Print data structure info
        procedure :: add_scalar_real64                              ! Add scalar data in real(real64) format
    end type GeometricData

contains

    ! ===================================================================================================
    ! SECTION: Data Array (vtk_data) Procedures
    ! ===================================================================================================

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Report the contents of the data structure.
    ! @param[in] self The data structure to report.
    ! ---------------------------------------------------------------------------------------------------
    subroutine info(self)
        implicit none
        class(GeometricData), intent(in) :: self                     ! Data structure to be reported
        integer(int32) :: i                                     ! Loop index

        call report("------------------------[      MESHF INFO      ]------------------------"//new_line('A'))
        call report("- Number of points/cells:       "//trim(itoa(self%n)))

        ! Report integer scalar data if allocated
        if (allocated(self%scalar_integer)) then
            call report("- Scalar fields (integer):      "//trim(itoa(size(self%scalar_integer_components, 1))))
            do i = 1, size(self%scalar_integer_labels)
                call report("  * Label:                      "//trim(self%scalar_integer_labels(i)))
                call report("  * Components:                 "//trim(itoa(self%scalar_integer_components(i))))
            end do
        else
            call report("- Scalar fields (integer):      "//"0 (unallocated)")
        end if

        ! Report real32 scalar data if allocated
        if (allocated(self%scalar_real32)) then
            call report("- Scalar fields (real32):       "//trim(itoa(size(self%scalar_real32_components, 1))))
            do i = 1, size(self%scalar_real32_labels)
                call report("  * Label:                      "//trim(self%scalar_real32_labels(i)))
                call report("  * Components:                 "//trim(itoa(self%scalar_real32_components(i))))
            end do
        else
            call report("- Scalar fields (real32):       "//"0 (unallocated)")
        end if

        ! Report real64 scalar data if allocated
        if (allocated(self%scalar_real64)) then
            call report("- Scalar fields (real64):       "//trim(itoa(size(self%scalar_real64_components, 1))))
            do i = 1, size(self%scalar_real64_labels)
                call report("  * Label:                      "//trim(self%scalar_real64_labels(i)))
                call report("  * Components:                 "//trim(itoa(self%scalar_real64_components(i))))
            end do
        else
            call report("- Scalar fields (real64):       "//"0 (unallocated)")
        end if

        ! Report integer vector data if allocated
        if (allocated(self%vector_integer)) then
            call report("- Vector fields (integer):      "//trim(itoa(size(self%vector_integer_labels, 1))))
            do i = 1, size(self%vector_integer_labels)
                call report("  * Label:                      "//trim(self%vector_integer_labels(i)))
            end do
        else
            call report("- Vector fields (integer):      "//"0 (unallocated)")
        end if

        ! Report real32 vector data if allocated
        if (allocated(self%vector_real32)) then
            call report("- Vector fields (real32):       "//trim(itoa(size(self%vector_real32_labels, 1))))
            do i = 1, size(self%vector_real32_labels)
                call report("  * Label:                      "//trim(self%vector_real32_labels(i)))
            end do
        else
            call report("- Vector fields (real32):       "//"0 (unallocated)")
        end if

        ! Report real64 vector data if allocated
        if (allocated(self%vector_real64)) then
            call report("- Vector fields (real64):       "//trim(itoa(size(self%vector_real64_labels, 1))))
            do i = 1, size(self%vector_real64_labels)
                call report("  * Label:                      "//trim(self%vector_real64_labels(i)))
            end do
        else
            call report("- Vector fields (real64):       "//"0 (unallocated)")
        end if

        call report("------------------------------------------------------------------------")
    end subroutine info

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Add new real(real64) scalar data to the data structure.
    ! @param[inout] self The data structure containing real(real64) scalar data.
    ! @param[in] new_scalars_real64 The new real(real64) scalar data to append.
    ! @param[in, optional] new_scalars_real64_labels Labels for the real(real64) scalar data.
    ! @param[in, optional] new_scalars_real64_components Number of components for the new scalar.
    ! ---------------------------------------------------------------------------------------------------
    subroutine add_scalar_real64(self, new_scalars_real64, new_scalars_real64_labels, &
                                new_scalars_real64_components)
        implicit none
        class(GeometricData), intent(inout) :: self                                  ! Data structure containing real(real64) scalar data
        real(real64), intent(in) :: new_scalars_real64(:, :)                         ! New real(real64) scalar data to append
        character(len=*), intent(in), optional :: new_scalars_real64_labels     ! Labels for the scalar data
        integer, intent(in), optional :: new_scalars_real64_components          ! Number of components for the scalar data

        integer(int32) :: i
        integer(int32) :: n, n_new_scalars                                             ! Number of elements, number of new scalars
        integer, allocatable :: temp_components(:)                              ! Temporary array for components
        real(real64), allocatable :: temp_scalars(:, :)                              ! Temporary array for scalar data
        character(len=32), allocatable :: temp_labels(:)                        ! Temporary array for scalar labels

        logical :: unallocated                                                  ! Flag to check allocation state of arrays

        n_new_scalars = size(new_scalars_real64, 1)
        n = size(new_scalars_real64, 2)

        unallocated = .not. (allocated(self%scalar_real64) .and. allocated(self%scalar_real64_labels) &
                            .and. allocated(self%scalar_real64_components))

        if (unallocated) then
            self%scalar_real64 = new_scalars_real64

            if (present(new_scalars_real64_components)) then
                self%scalar_real64_components = new_scalars_real64_components
            else
                allocate (self%scalar_real64_components(n_new_scalars))
                self%scalar_real64_components = 1
            end if

            if (present(new_scalars_real64_labels)) then
                self%scalar_real64_labels = new_scalars_real64_labels
            else
                allocate (self%scalar_real64_labels(size(self%scalar_real64_components)))
                do i = 1, size(self%scalar_real64_components)
                    write (self%scalar_real64_labels(i), "(A, I0, A)") "Scalar ", i, " [double]"
                end do
            end if
        else
            allocate (temp_scalars(size(self%scalar_real64, 1), n + n_new_scalars))
            temp_scalars(:, 1:n) = self%scalar_real64
            temp_scalars(:, n + 1:n + n_new_scalars) = new_scalars_real64
            deallocate (self%scalar_real64)
            self%scalar_real64 = temp_scalars

            allocate (temp_components(size(self%scalar_real64_components) + n_new_scalars))
            temp_components(1:size(self%scalar_real64_components)) = self%scalar_real64_components
            temp_components(size(self%scalar_real64_components) + 1:size(self%scalar_real64_components) + n_new_scalars) = 1
            deallocate (self%scalar_real64_components)
            self%scalar_real64_components = temp_components

            allocate (temp_labels(size(self%scalar_real64_labels) + n_new_scalars))
            temp_labels(1:size(self%scalar_real64_labels)) = self%scalar_real64_labels
            if (present(new_scalars_real64_labels)) then
                temp_labels(size(self%scalar_real64_labels) + 1:size(self%scalar_real64_labels) + n_new_scalars) = &
                    new_scalars_real64_labels
            else
                do i = 1, n_new_scalars
                    write (temp_labels(size(self%scalar_real64_labels) + i), "(A, I0, A)") "Scalar ", &
                        size(self%scalar_real64_labels) + i, " [double]"
                end do
            end if
            deallocate (self%scalar_real64_labels)
            self%scalar_real64_labels = temp_labels
        end if
    end subroutine add_scalar_real64

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Read scalar data from a file and add it to the cell data.
    ! @param[in] file_path Path to the scalar data file.
    ! @param[inout] geometric_data The data structure where the scalar data is stored.
    ! ---------------------------------------------------------------------------------------------------
    subroutine read_scalar_real64(file_path, geometric_data)
        implicit none
        character(len=*) :: file_path                                           ! Path to scalar data file
        type(GeometricData), intent(inout) :: geometric_data                         ! Data structure for storing scalar data
        integer(int32) :: i, unit, io_status, n, n_scalars                             ! Loop index, unit ID, I/O status, etc.
        real(real64), allocatable :: scalar_real64(:, :)                             ! Array for real(real64) scalar data
        real(real64) :: start_time, end_time                                         ! Timing variables

        call cpu_time(start_time)

        open (newunit=unit, file=file_path, status="old", action="read", iostat=io_status)
        if (io_status .ne. 0) then
            call report("ERROR: Failed to open file: "//trim(file_path), is_error=.true.)
            stop
        end if

        call report("------------------------[    MESHF FILE READ   ]------------------------"//new_line('A'))
        call report("- File:                         "//trim(file_path))
        call report("- Description:                  "//"Scalar data (real64)") 
        ! call report("- Status:                       "//"Started")

        read (unit, *, iostat=io_status) n, n_scalars
        if (io_status .ne. 0) then
            call report("ERROR: Error parsing file header.", is_error=.true.)
            stop
        end if

        if (n .ne. geometric_data%n) then
            call report("ERROR: Mismatched number of points/cells.", is_error=.true.)
            stop
        end if

        allocate (scalar_real64(n_scalars, n))
        read (unit, *, iostat=io_status) (scalar_real64(1:n_scalars, i), i=1, n)
        if (io_status .ne. 0) then
            call report("ERROR: Error parsing file data.", is_error=.true.)
            stop
        end if

        close (unit)    
        call geometric_data%add_scalar_real64(scalar_real64)
        deallocate (scalar_real64)

        call cpu_time(end_time)
        call report("- Status:                       Completed in "&
            //trim(rtoa(end_time - start_time, decimal_places=4))//" seconds")
        call report("------------------------------------------------------------------------")
    end subroutine read_scalar_real64
end module geometric_data