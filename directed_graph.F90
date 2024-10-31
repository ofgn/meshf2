! -------------------------------------------------------------------------------------------------------
! @file DirectedGraph.f90
! @brief DirectedGraph module implemented using Intel MKL's Sparse BLAS (CSR format).
! @note Uses ILP64 interface (64-bit integers).
! @date 2024-04-27
! -------------------------------------------------------------------------------------------------------
include 'mkl_spblas.f90'
module DirectedGraphModule
    use mkl_spblas        ! MKL Sparse BLAS module
    use iso_c_binding
    implicit none



    ! Derived type definition
    type :: DirectedGraph
        integer(C_INT) :: num_nodes = 0                ! Number of nodes
        integer(C_INT), allocatable :: row_ptr(:)      ! CSR row pointers
        integer(C_INT), allocatable :: col_ind(:)      ! CSR column indices
        real(kind=8), allocatable :: values(:)             ! CSR values (optional for weighted graphs)
        integer(C_INT) :: nnz = 0                       ! Number of non-zero entries (edges)
        logical :: finalized = .false.                    ! Flag indicating CSR is finalized
        type(sparse_matrix_t) :: mkl_handle                ! MKL Sparse Matrix handle
    contains
        procedure :: initialize_graph
        procedure :: add_edge
        procedure :: finalize_graph
        procedure :: is_acyclic
        procedure :: topological_sort
    end type DirectedGraph

    ! Make public after type declaration
    public :: DirectedGraph, initialize_graph, add_edge, finalize_graph
    public :: is_acyclic, topological_sort

contains

    ! ---------------------------------------------------
    ! Subroutine: initialize_graph
    ! Purpose  : Initialize the DirectedGraph with a given number of nodes
    ! ---------------------------------------------------
    subroutine initialize_graph(self, n)
        class(DirectedGraph), intent(inout) :: self
        integer(C_INT), intent(in) :: n

        self%num_nodes = n
        self%nnz = 0
        allocate(self%row_ptr(n + 1))
        self%row_ptr = 0
        allocate(self%col_ind(0))
        allocate(self%values(0))
        self%finalized = .false.
    end subroutine initialize_graph

    ! ---------------------------------------------------
    ! Subroutine: add_edge
    ! Purpose  : Add a directed edge from node 'from' to node 'to'
    ! ---------------------------------------------------
    subroutine add_edge(self, from, to, weight)
        class(DirectedGraph), intent(inout) :: self
        integer(C_INT), intent(in) :: from, to
        real(kind=8), intent(in), optional :: weight

        if (self%finalized) then
            print *, "Error: Cannot add edges after finalizing the graph."
            stop 1
        end if

        if (from < 1 .or. from > self%num_nodes .or. &
            to < 1 .or. to > self%num_nodes) then
            print *, "Error: Node indices out of bounds in add_edge."
            stop 1
        end if

        self%nnz = self%nnz + 1

        ! Resize col_ind and values arrays
        if (.not. allocated(self%col_ind)) then
            allocate(self%col_ind(1))
            if (present(weight)) then
                allocate(self%values(1))
                self%values(1) = weight
            else
                allocate(self%values(1))
                self%values(1) = 1.0d0  ! Default weight
            end if
        else
            call move_alloc(self%col_ind, self%col_ind)  ! Ensures allocation status
            allocate(self%col_ind(self%nnz))
            if (present(weight)) then
                call move_alloc(self%values, self%values)
                allocate(self%values(self%nnz))
                self%values(self%nnz) = weight
            else
                call move_alloc(self%values, self%values)
                allocate(self%values(self%nnz))
                self%values(self%nnz) = 1.0d0  ! Default weight
            end if
        end if

        self%col_ind(self%nnz) = to

        ! Update row_ptr for the 'from' node
        self%row_ptr(from + 1) = self%row_ptr(from + 1) + 1
    end subroutine add_edge

    ! ---------------------------------------------------
    ! Subroutine: finalize_graph
    ! Purpose  : Finalize the CSR arrays by computing cumulative row_ptr and create MKL Sparse Matrix handle
    ! ---------------------------------------------------
    subroutine finalize_graph(self)
        class(DirectedGraph), intent(inout) :: self
        integer(C_INT) :: i
        integer(C_INT), allocatable :: csr_row_ptr(:), csr_col_ind(:)
        real(kind=8), allocatable :: csr_values(:)
        integer(C_INT) :: status

        if (self%finalized) return

        ! Convert row_ptr to cumulative sum
        do i = 1, self%num_nodes
            self%row_ptr(i + 1) = self%row_ptr(i) + self%row_ptr(i + 1)
        end do

        ! Allocate CSR arrays
        allocate(csr_row_ptr(self%num_nodes + 1))
        csr_row_ptr = self%row_ptr

        allocate(csr_col_ind(self%nnz))
        csr_col_ind = self%col_ind

        if (allocated(self%values)) then
            allocate(csr_values(self%nnz))
            csr_values = self%values
        else
            allocate(csr_values(self%nnz))
            csr_values = 1.0d0  ! Default weight for unweighted graphs
        end if

        ! Create MKL Sparse Matrix handle
        status = mkl_sparse_d_create_csr(self%mkl_handle, SPARSE_INDEX_BASE_ZERO, &
                                         self%num_nodes, self%num_nodes, &
                                         csr_row_ptr, csr_row_ptr(2:), csr_col_ind, csr_values)

        if (status /= SPARSE_STATUS_SUCCESS) then
            print *, "Error: Failed to create MKL Sparse Matrix handle. Status = ", status
            stop 1
        end if

        self%finalized = .true.
    end subroutine finalize_graph

    ! ---------------------------------------------------
    ! Function: is_acyclic
    ! Purpose  : Check if the directed graph is acyclic using DFS
    ! ---------------------------------------------------
    logical function is_acyclic(self)
        class(DirectedGraph), intent(in) :: self
        integer(C_INT) :: i
        logical, allocatable :: visited(:), rec_stack(:)
        logical :: cycle_found

        if (.not. self%finalized) then
            print *, "Error: Graph must be finalized before cycle detection."
            cycle_found = .true.
            is_acyclic = .false.
            return
        end if

        allocate(visited(self%num_nodes))
        allocate(rec_stack(self%num_nodes))
        visited = .false.
        rec_stack = .false.

        cycle_found = .false.

        do i = 1, self%num_nodes
            if (.not. visited(i)) then
                call dfs_cycle_detect(self, i, visited, rec_stack, cycle_found)
                if (cycle_found) exit
            end if
        end do

        is_acyclic = .not. cycle_found

        deallocate(visited, rec_stack)
    end function is_acyclic

    ! ---------------------------------------------------
    ! Recursive Subroutine: dfs_cycle_detect
    ! Purpose            : Recursive DFS for cycle detection
    ! ---------------------------------------------------
    recursive subroutine dfs_cycle_detect(self, node, visited, rec_stack, cycle_found)
        class(DirectedGraph), intent(in) :: self
        integer(C_INT), intent(in) :: node
        logical, intent(inout) :: visited(:), rec_stack(:)
        logical, intent(inout) :: cycle_found
        integer(C_INT) :: i, neighbor

        if (cycle_found) return

        visited(node) = .true.
        rec_stack(node) = .true.

        ! Iterate through neighbors
        do i = self%row_ptr(node), self%row_ptr(node + 1) - 1
            neighbor = self%col_ind(i)
            if (.not. visited(neighbor)) then
                call dfs_cycle_detect(self, neighbor, visited, rec_stack, cycle_found)
                if (cycle_found) exit
            else if (rec_stack(neighbor)) then
                cycle_found = .true.
                return
            end if
        end do

        rec_stack(node) = .false.
    end subroutine dfs_cycle_detect

    ! ---------------------------------------------------
    ! Subroutine: topological_sort
    ! Purpose  : Perform topological sorting using Kahn's Algorithm
    ! ---------------------------------------------------
    subroutine topological_sort(self, sorted_nodes, success)
        class(DirectedGraph), intent(in) :: self
        integer(C_INT), allocatable, intent(out) :: sorted_nodes(:)
        logical, intent(out) :: success
        integer(C_INT), allocatable :: in_degree(:)
        integer(C_INT) :: i, j, count, neighbor
        integer(C_INT), allocatable :: queue(:)
        integer(C_INT) :: front, rear

        if (.not. self%finalized) then
            print *, "Error: Graph must be finalized before topological sorting."
            success = .false.
            return
        end if

        allocate(sorted_nodes(self%num_nodes))
        allocate(queue(self%num_nodes))
        allocate(in_degree(self%num_nodes))
        sorted_nodes = 0
        queue = 0
        in_degree = 0
        front = 1
        rear = 0
        count = 0

        ! Compute in-degrees
        do i = 1, self%num_nodes
            do j = self%row_ptr(i), self%row_ptr(i + 1) - 1
                neighbor = self%col_ind(j)
                in_degree(neighbor) = in_degree(neighbor) + 1
            end do
        end do

        ! Enqueue nodes with in-degree 0
        do i = 1, self%num_nodes
            if (in_degree(i) == 0) then
                rear = rear + 1
                queue(rear) = i
            end if
        end do

        ! Kahn's Algorithm
        do while (front <= rear)
            i = queue(front)
            front = front + 1
            count = count + 1
            sorted_nodes(count) = i

            do j = self%row_ptr(i), self%row_ptr(i + 1) - 1
                neighbor = self%col_ind(j)
                in_degree(neighbor) = in_degree(neighbor) - 1
                if (in_degree(neighbor) == 0) then
                    rear = rear + 1
                    queue(rear) = neighbor
                end if
            end do
        end do

        if (count == self%num_nodes) then
            success = .true.
            sorted_nodes = sorted_nodes(1:count)
        else
            success = .false.
            deallocate(sorted_nodes)
        end if

        deallocate(in_degree, queue)
    end subroutine topological_sort

end module DirectedGraphModule
