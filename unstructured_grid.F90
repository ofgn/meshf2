! -------------------------------------------------------------------------------------------------------------------------------------------------------------
! @file unstructured_grid.f90
! @brief Module for handling unstructured grids.
! @author ofgn
! @date 2024-07-01
! -------------------------------------------------------------------------------------------------------------------------------------------------------------
module unstructured_grid
    use global
    use utility
    use geometric_data

    implicit none

    type :: UnstructuredGrid
        integer(kind=custom_int) :: n_points                                    ! Number of points in the grid
        integer(kind=custom_int) :: n_cells                                     ! Number of cells in the grid
        real(kind=custom_real), allocatable :: points(:, :)                     ! 3D coordinates of points
        integer(int16), allocatable :: points_per_cell(:)                       ! Number of points in each cell
        integer(kind=custom_int), allocatable :: cell_connectivity(:)           ! Point indices for each cell
        integer(int16), allocatable :: cell_types(:)                            ! VTK cell types for each cell
    contains
        procedure :: read_node_file                                             ! Read TetGen node file
        procedure :: read_tetgen_ele                                            ! Read TetGen element file
        procedure :: write_vtk_legacy                                           ! Write legacy VTK file in binary or ASCII
        procedure :: blank_on_point_mask                                        ! Blank points and cells based on a point mask
        procedure :: blank_on_cell_mask                                         ! Blank points and cells based on a cell mask
        procedure :: clear => u_grid_clear                                      ! Clear the unstructured grid
    end type UnstructuredGrid

contains

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Read a TetGen/Triangle node file and extract the node coordinates.
    ! @param[in] file_path Path to the TetGen node file.
    ! @param[out] point_data Data structure to store point information (optional).
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine read_node_file(self, file_path, point_data)
        implicit none

        class(UnstructuredGrid) :: self                                         ! The unstructured grid to store node data
        character(len=*) :: file_path                                           ! Path to the TetGen node file
        type(GeometricData), optional :: point_data                             ! Data structure for storing point info (optional)
        integer(kind=custom_int) :: n_dimensions, n_scalars, id_flag            ! Dimensions, scalars, and boundary ID flag
        integer(kind=custom_int) :: point_index                                 ! Index for the point data
        integer(kind=custom_int) :: i                                           ! Loop index
        integer(int16) :: unit, io_status                                       ! File unit and I/O status
        real(real64) :: start_time, end_time                                    ! Timing variables

        call cpu_time(start_time)
        call meshf_title("NODE FILE READ")
        call meshf_now()

        open (newunit=unit, file=file_path, status="old", action="read", &
            iostat=io_status)
        if (io_status .ne. 0) then
            call meshf_error("Failed to open file " // trim(file_path))
            stop
        end if

        call meshf_status("Filepath", trim(file_path))

        ! Read header
        read (unit, *, iostat=io_status) self%n_points, n_dimensions, &
            n_scalars, id_flag

        if (io_status .ne. 0) then
            call meshf_error("Failed to parse node file header")
            stop
        end if

        ! Validate dimensions and report
        if (n_dimensions .eq. 2) then
            call meshf_status("Description", "2D points (Triangle)")
        else if (n_dimensions .eq. 3) then
            call meshf_status("Description", "3D points (TetGen)")
        else
            call meshf_error("Invalid number of dimensions")
            stop
        end if

        ! Allocate memory for points
        allocate(self%points(3, self%n_points))

        ! Allocate memory for scalars and boundary ID if present
        if (present(point_data) .and. n_scalars .gt. 0) then
            allocate(point_data%scalar_real64(n_scalars, self%n_points))
            allocate(point_data%scalar_real64_labels(n_scalars))
            allocate(point_data%scalar_real64_components(n_scalars))
            do i = 1, n_scalars
                write(point_data%scalar_real64_labels(i), &
                    "(A, I0, A)") "Scalar ", i, "[double]"
            end do
            point_data%scalar_real64_components = 1
        end if

        if (present(point_data) .and. id_flag .gt. 0) then
            allocate(point_data%scalar_integer(1, self%n_points))
            allocate(point_data%scalar_integer_labels(1))
            point_data%scalar_integer_labels(1) = "Boundary Id"
            point_data%scalar_integer_components = 1
        end if

        ! Read point data directly into the appropriate arrays
        do i = 1, self%n_points
            if (present(point_data) .and. (n_scalars .gt. 0) .and. (id_flag .gt. 0)) then
                read (unit, *, iostat=io_status) point_index, &
                    self%points(1:n_dimensions, i), &
                    point_data%scalar_real64(:, i), &
                    point_data%scalar_integer(1, i)
                    point_data%n = self%n_points
            else if (present(point_data) .and. (n_scalars .gt. 0)) then
                read (unit, *, iostat=io_status) point_index, &
                    self%points(1:n_dimensions, i), &
                    point_data%scalar_real64(:, i)
                    point_data%n = self%n_points
            else if (present(point_data) .and. (id_flag .gt. 0)) then
                read (unit, *, iostat=io_status) point_index, &
                    self%points(1:n_dimensions, i), &
                    point_data%scalar_integer(1, i)
                    point_data%n = self%n_points
            else
                read (unit, *, iostat=io_status) point_index, &
                    self%points(1:n_dimensions, i)
            end if

            ! If 2D points, set the z-coordinate to 0
            if (n_dimensions .eq. 2) then
                self%points(3, i) = 0.0d0
            end if

            if (io_status .ne. 0) then
                call meshf_error("Failed to parse node file data")
            stop
            end if
        end do

        close (unit)

        call cpu_time(end_time)
        call meshf_runtime(end_time - start_time)
    end subroutine read_node_file

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Read a TetGen element file (.ele) and extract connectivity.
    ! @param[in] file_path Path to the TetGen element file.
    ! @param[out] cell_data Data structure to store cell information (optional).
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine read_tetgen_ele(self, file_path, cell_data)
        implicit none

        class(UnstructuredGrid) :: self                                         ! The unstructured grid to store element data
        character(len=*), intent(in) :: file_path                               ! Path to the TetGen element file
        type(GeometricData), optional :: cell_data                              ! Data structure for storing cell data (optional)
        integer(int16) :: unit, io_status                                       ! File unit and I/O status
        integer(kind=custom_int) :: i                                           ! Loop index
        integer(kind=custom_int) ::points_per_cell, id_flag                     ! Points per cell, boundary ID flag
        integer(kind=custom_int) :: cell_i, dummy                               ! Element index
        real(real64) :: start_time, end_time                                    ! Timing variables

        if (.not. allocated(self%points)) then
            call meshf_report("Error:              " &
                // "Points array not allocated.", is_error=.true.)
            stop
        end if

        call cpu_time(start_time)
        open (newunit=unit, file=file_path, status="old", action="read", &
            iostat=io_status)
        if (io_status .ne. 0) then
            call meshf_error("Failed to open file " // trim(file_path))
            stop
        end if

        call meshf_title("ELE FILE READ")
        call meshf_now()
        call meshf_status("Filepath", trim(file_path))

        read (unit, *, iostat=io_status) self%n_cells, points_per_cell, id_flag
        if (io_status .ne. 0) then
            call meshf_error("Failed to parse element file header")
            stop
        end if

        allocate (self%cell_connectivity(self%n_cells * points_per_cell))
        allocate (self%points_per_cell(self%n_cells))
        allocate (self%cell_types(self%n_cells))

        if (points_per_cell .eq. 3) then
            self%cell_types = 5
            call meshf_report("- Description:      " &
                //"Unstructured grid (Triangle)")
        else if (points_per_cell .eq. 4) then
            self%cell_types = 10
            call meshf_report("- Description:      " &
                //"Unstructured grid (TetGen, linear)")
        else if (points_per_cell .eq. 10) then
            self%cell_types = 24
            call meshf_report("- Description:      " &
                //"Unstructured grid (TetGen, quadratic)")
        else
            call meshf_error("Invalid number of points per cell")
            stop
        end if

        self%points_per_cell = points_per_cell

        if (present(cell_data) .and. id_flag > 0) then
            allocate (cell_data%scalar_integer(id_flag, self%n_cells))
            allocate (cell_data%scalar_integer_labels(id_flag))
            allocate (cell_data%scalar_integer_components(id_flag))
            cell_data%scalar_integer_labels(1) = "Id"
            cell_data%scalar_integer_components = 1
            cell_data%n = self%n_cells
        end if

        if (id_flag > 0) then
            if (present(cell_data)) then
                read (unit, *, iostat=io_status) &
                    (cell_i, self%cell_connectivity((i - 1) &
                    * points_per_cell + 1:i * points_per_cell), &
                    cell_data%scalar_integer(:, i), i=1, self%n_cells)
            else 
                read (unit, *, iostat=io_status) &
                    (cell_i, self%cell_connectivity((i - 1) &
                    * points_per_cell + 1:i * points_per_cell), &
                    dummy, i=1, self%n_cells)
            end if
        else
            read (unit, *, iostat=io_status) &
                (cell_i, self%cell_connectivity((i - 1) * &
                points_per_cell + 1:i * points_per_cell), i=1, self%n_cells)
        end if

        if (io_status .ne. 0) then
            call meshf_error("Failed to parse element file data")
            stop
        end if

        self%cell_connectivity = self%cell_connectivity - 1
        close (unit)

        call cpu_time(end_time)
        call meshf_runtime(end_time - start_time)
    end subroutine read_tetgen_ele

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Write an unstructured grid to a VTK legacy file in either binary or ASCII format.
    ! @param[in] file_path Path to the output VTK file.
    ! @param[in] point_data Data associated with points (optional).
    ! @param[in] cell_data Data associated with cells (optional).
    ! @param[in] ascii Logical flag to write in ASCII format (default: false).
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine write_vtk_legacy(self, file_path, point_data, cell_data, ascii)
        implicit none

        class(UnstructuredGrid), intent(in) :: self                             ! Unstructured grid data structure
        character(len=*), intent(in) :: file_path                               ! Path to the output VTK file
        type(GeometricData), optional, intent(in) :: point_data                 ! Data structure for point data (optional)
        type(GeometricData), optional, intent(in) :: cell_data                  ! Data structure for cell data (optional)
        logical, intent(in), optional :: ascii                                  ! Write in ASCII format (default: false)
        integer(int16) unit, io_status                                          ! File unit and I/O status
        integer(kind=custom_int), allocatable :: full_connectivity(:)           ! Complete cell connectivity array
        integer(kind=custom_int) :: i, j                                        ! Loop variables
        real(real64) :: start_time, end_time                                    ! Timing variables
        logical :: binary                                                       ! Flag to determine binary or ASCII format

        call cpu_time(start_time)
        
        if (present(ascii)) then
            binary = .not. ascii
        else
            binary = .true.
        end if

        if (binary) then
            open (newunit=unit, file=file_path, status="replace", &
            access="stream", action="write", form="unformatted", &
            convert="big_endian", iostat=io_status)
        else
            open (newunit=unit, file=file_path, status="replace", &
            access="stream", action="write", form="formatted", &
            iostat=io_status)
        end if

        if (io_status .ne. 0) then
            call meshf_error("Failed to open file " // trim(file_path))
            stop
        end if

        call meshf_title("VTK FILE WRITE")
        call meshf_now()
        call meshf_status("Filepath", trim(file_path))

        ! Determine file format (ASCII or binary)
        if (binary) then
            call meshf_status("Format", "Binary")
        else
            call meshf_status("Format", "ASCII")
        end if

        call meshf_status("Type", "Unstructured Grid")

        ! Allocate the full connectivity array
        allocate (full_connectivity(size(self%cell_connectivity) &
            + self%n_cells))
        j = 1
        do i = 1, self%n_cells
            full_connectivity(j) = self%points_per_cell(i)
            full_connectivity(j + 1:j + self%points_per_cell(i)) = &
                self%cell_connectivity((i - 1) * self%points_per_cell(i) &
                + 1:i * self%points_per_cell(i))
            j = j + self%points_per_cell(i) + 1
        end do

        if (binary) then
            write (unit) "# vtk DataFile Version 3.0"//lf
            write (unit) "vtk"//lf
            write (unit) "BINARY"//lf
            write (unit) "DATASET UNSTRUCTURED_GRID"//lf
        else
            write (unit, "(a)") "# vtk DataFile Version 3.0"
            write (unit, "(a)") "vtk"
            write (unit, "(a)") "ASCII"
            write (unit, "(a)") "DATASET UNSTRUCTURED_GRID"
        end if

        if (binary) then
            write (unit) "POINTS "//trim(itoa(self%n_points))//" double"//lf
            write (unit) self%points
            write (unit) lf
        else
            write (unit, "(A, I0, A)") "POINTS ", self%n_points, " double"
            do i = 1, self%n_points
                write (unit, "(3E20.8)") self%points(:, i)
            end do
        end if

        if (binary) then
            write (unit) "CELLS " // trim(itoa(self%n_cells)) // " " &
                // trim(itoa(size(full_connectivity))) // lf
            write (unit) full_connectivity
            write (unit) lf
        else
            write (unit, "(A, I0, A, I0)") "CELLS ", self%n_cells  , " ", &
                size(full_connectivity)
            do i = 1, size(full_connectivity)
                write (unit, "(I0)") full_connectivity(i)
            end do
        end if

        if (binary) then
            write (unit) "CELL_TYPES " // trim(itoa(self%n_cells)) // lf
            write (unit) int(self%cell_types, kind=custom_int)
            write (unit) lf
        else
            write (unit, "(A, I0)") "CELL_TYPES ", self%n_cells
            do i = 1, self%n_cells
                write (unit, "(I0)") self%cell_types(i)
            end do
        end if

        call cpu_time(end_time)
        close (unit)

        call meshf_runtime(end_time - start_time)
    end subroutine write_vtk_legacy

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Mask and remove points from the grid based on the given point mask.
    ! @param[inout] self The unstructured grid to modify.
    ! @param[in] point_mask Logical mask to determine which points to keep.
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine blank_on_point_mask(self, point_mask)
        implicit none

        class(UnstructuredGrid), intent(inout) :: self                          ! The unstructured grid to modify
        logical, intent(in) :: point_mask(:)                                    ! Logical mask for filtering points
        integer(kind=custom_int) :: i, j, k, new_n_points, new_n_cells          ! Counters for points and cells
        integer(kind=custom_int), allocatable :: new_cell_connectivity(:)       ! New cell connectivity array
        integer(int16), allocatable :: new_points_per_cell(:)                   ! New points-per-cell array
        integer(int16), allocatable :: new_cell_types(:)                        ! New cell types array
        integer(kind=custom_int), allocatable :: point_map(:)                   ! Mapping from old points to new points
        real(kind=custom_real), allocatable :: new_points(:, :)                 ! New point coordinates array
        real(real64) :: start_time, end_time                                    ! Timing variables
        logical, allocatable :: cell_mask(:)                                    ! Masks for cells and connectivity
        logical, allocatable :: connectivity_mask(:)                            ! Masks for connectivity

        call cpu_time(start_time)
        call meshf_title("OPERATION")
        call meshf_report("- Description:      " &
            // "Blanking grid based on a point mask") 

        if (size(point_mask) .ne. self%n_points) then
            call meshf_report("Error:              " &
            // "Mask size does not match number of points.", is_error=.true.)
            
        end if

        allocate(cell_mask(self%n_cells), &
            connectivity_mask(size(self%cell_connectivity)))
        cell_mask = .true.
        connectivity_mask = .true.

        j = 1
        do i = 1, self%n_cells
            do k = 0, self%points_per_cell(i) - 1
                if (.not. point_mask(self%cell_connectivity(j + k) + 1)) then
                    cell_mask(i) = .false.
                    connectivity_mask(j:j + self%points_per_cell(i) - 1) &
                        = .false.
                    exit
                end if
            end do
            j = j + self%points_per_cell(i)
        end do

        allocate(point_map(self%n_points))
        point_map = -1
        j = 1
        do i = 1, self%n_points
            if (point_mask(i)) then
                point_map(i) = j
                j = j + 1
            end if
        end do

        new_n_points = count(point_mask)
        new_n_cells = count(cell_mask)

        allocate(new_points(3, new_n_points))
        new_points = self%points(:, pack([(i, i=1, self%n_points)], point_mask))

        new_cell_connectivity = pack(self%cell_connectivity, connectivity_mask)
        new_points_per_cell = pack(self%points_per_cell, cell_mask)
        new_cell_types = pack(self%cell_types, cell_mask)

        new_cell_connectivity = point_map(new_cell_connectivity + 1) - 1
        deallocate(cell_mask, connectivity_mask)

        self%points = new_points
        self%cell_connectivity = new_cell_connectivity
        self%points_per_cell = new_points_per_cell
        self%cell_types = new_cell_types

        call meshf_report("- Points prior:     "//trim(itoa(self%n_points)))
        self%n_points = new_n_points
        call meshf_report("- Points after:     "//trim(itoa(new_n_points)))
        call meshf_report("- Cells prior:      "//trim(itoa(self%n_cells)))
        self%n_cells = new_n_cells
        call meshf_report("- Cells after:      "//trim(itoa(new_n_cells)))
        call cpu_time(end_time)
        call meshf_report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
    end subroutine blank_on_point_mask

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Mask and remove cells from the grid based on the given cell mask.
    ! @param[inout] self The unstructured grid to modify.
    ! @param[in] cell_mask Logical mask to determine which cells to keep.
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine blank_on_cell_mask(self, cell_mask)
        implicit none

        class(UnstructuredGrid), intent(inout) :: self                          ! The unstructured grid to modify
        logical, intent(in) :: cell_mask(:)                                     ! Logical mask for filtering cells
        integer(kind=custom_int) :: i, j, new_n_points, new_n_cells             ! Counters for points and cells
        integer(kind=custom_int), allocatable :: new_cell_connectivity(:)       ! New cell connectivity array
        integer(int16), allocatable :: new_points_per_cell(:)                   ! New points-per-cell array
        integer(int16), allocatable :: new_cell_types(:)                        ! New cell types array
        integer(kind=custom_int), allocatable :: point_map(:)                   ! Mapping from old points to new points
        real(kind=custom_real), allocatable :: new_points(:, :)                 ! New point coordinates array
        real(real64) :: start_time, end_time                                    ! Timing variables
        logical, allocatable :: point_mask(:)                                   ! Masks for points
        logical, allocatable :: connectivity_mask(:)                            ! Masks for connectivity

        call cpu_time(start_time)
        call meshf_title("OPERATION")
        call meshf_report("- Description:      " &
            // "Blanking grid based on a cell mask") 
        ! call report("- Status:                       "//"Started")

        if (size(cell_mask) .ne. self%n_cells) then
            call meshf_report("Error:              " &
            // "Mask size does not match number of cells.", is_error=.true.)
            stop
        end if

        allocate(point_mask(self%n_points), connectivity_mask(size(self%cell_connectivity)))
        point_mask = .false.
        connectivity_mask = .false.

        j = 1
        do i = 1, self%n_cells
            if (cell_mask(i)) then
                point_mask(self%cell_connectivity(j:j + self%points_per_cell(i) - 1) &
                    + 1) = .true.
                connectivity_mask(j:j + self%points_per_cell(i) - 1) = .true.
            end if
            j = j + self%points_per_cell(i)
        end do

        allocate(point_map(self%n_points))
        point_map = -1
        j = 1
        do i = 1, self%n_points
            if (point_mask(i)) then
                point_map(i) = j
                j = j + 1
            end if
        end do

        new_n_points = count(point_mask)
        new_n_cells = count(cell_mask)

        allocate(new_points(3, new_n_points))
        new_points = self%points(:, pack([(i, i=1, self%n_points)], point_mask))

        new_cell_connectivity = pack(self%cell_connectivity, connectivity_mask)
        new_points_per_cell = pack(self%points_per_cell, cell_mask)
        new_cell_types = pack(self%cell_types, cell_mask)

        new_cell_connectivity = point_map(new_cell_connectivity + 1) - 1
        deallocate(point_mask, connectivity_mask)

        self%points = new_points
        self%cell_connectivity = new_cell_connectivity
        self%points_per_cell = new_points_per_cell
        self%cell_types = new_cell_types

        call meshf_report("- Points prior:     "//trim(itoa(self%n_points)))
        self%n_points = new_n_points
        call meshf_report("- Points after:     "//trim(itoa(new_n_points)))
        call meshf_report("- Cells prior:      "//trim(itoa(self%n_cells)))
        self%n_cells = new_n_cells
        call meshf_report("- Cells after:      "//trim(itoa(new_n_cells)))
        call cpu_time(end_time)
        call meshf_report("- Completed:        "// &
                    trim(rtoa(end_time - start_time, decimal_places=4))// &
                    "s" // lf)
    end subroutine blank_on_cell_mask

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Reset the unstructured grid by deallocating all arrays.
    ! @param[inout] self The unstructured grid to reset.
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine u_grid_clear(self)
        implicit none

        class(UnstructuredGrid), intent(inout) :: self                      !    The unstructured grid to clear


        deallocate(self%points)
        deallocate(self%points_per_cell)
        deallocate(self%cell_connectivity)
        deallocate(self%cell_types)

        self%n_points = 0
        self%n_cells = 0

    end subroutine u_grid_clear
end module unstructured_grid