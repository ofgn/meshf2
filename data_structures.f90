! -------------------------------------------------------------------------------------------------------
! @file data_structures.f90
! @brief Implementation of simple data structures in Fortran with support for hash map values as integer arrays.
! @note All values in the hash map are integer arrays of fixed length n.
! @date 2024-04-27
! -------------------------------------------------------------------------------------------------------
module data_structures
    use global
    use utility

    implicit none

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines the linked list.
    ! ---------------------------------------------------------------------------------------------------
    type :: LinkedList
        type(ListNode), pointer :: head => null()               ! Pointer to the head of the list
        integer(kind=custom_int) :: size = 0                    ! Number of elements in the list
    contains
        procedure :: prepend => linked_list_prepend             ! Insert an element at the start
        procedure :: append => linked_list_append               ! Insert an element at the end
        procedure :: remove => linked_list_remove               ! Remove an element from the list
        procedure :: contains => linked_list_contains           ! Check if an element exists in the list
        procedure :: clear => linked_list_clear                 ! Clear the list
        procedure :: print => linked_list_print                 ! Print the elements of the list
    end type LinkedList

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines a node in the linked list.
    ! ---------------------------------------------------------------------------------------------------
    type :: ListNode
        integer(kind=custom_int) :: value               ! Value of the node
        type(ListNode), pointer :: next => null()       ! Pointer to the next node
    end type ListNode

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines a hash map, containing an array of hash map buckets.
    ! ---------------------------------------------------------------------------------------------------
    type :: HashMap
        integer(kind=custom_int) :: count = 0                   ! Number of inserted key-value pairs
        integer(kind=custom_int) :: size = 0                    ! Current maximum number of key-value pairs
        type(HashMapBucket), allocatable :: buckets(:)          ! Array of hash_map_bucket types
    contains
        procedure :: initialise => hash_map_initialise          ! Initialise the hash map
        procedure :: insert => hash_map_insert                  ! Insert a key-value pair
        procedure :: delete => hash_map_delete                  ! Delete a key-value pair
        procedure :: find => hash_map_find                      ! Find a value by key
        procedure :: clear => hash_map_clear                    ! Clear the hash map
        procedure :: resize => hash_map_resize                  ! Resize the hash map
    end type HashMap

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines a hash map node, containing a key-value pair.
    ! ---------------------------------------------------------------------------------------------------
    type :: HashMapNode
        integer(kind=custom_int), allocatable :: key(:)         ! The key, 1D integer array
        integer(kind=custom_int), allocatable :: value(:)       ! The value associated with the key (variable-length integer array)
        type(HashMapNode), pointer :: next => null()            ! Pointer to the next node in the bucket
    end type HashMapNode

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines a hash map bucket, containing a pointer to the head node.
    ! ---------------------------------------------------------------------------------------------------
    type :: HashMapBucket
        type(HashMapNode), pointer :: head => null()            ! Pointer to the head node of the bucket
    end type HashMapBucket

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines a Set of sorted, unique integers.
    ! ---------------------------------------------------------------------------------------------------
    type :: Set
        integer(kind=custom_int), allocatable :: elements(:)        ! Array of unique elements
        integer(kind=custom_int) :: size = 0                        ! Number of elements in the set
    contains
        procedure :: initialise => set_initialise                   ! Initialise the set
        procedure :: insert => set_insert                           ! Insert an element into the set
        procedure :: contains => set_contains                       ! Check if an element is in the set
        procedure :: remove => set_remove                           ! Remove an element from the set
        procedure :: clear => set_clear                             ! Clear the set
    end type Set

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines a Directed Acyclic Graph (DAG).
    ! ---------------------------------------------------------------------------------------------------
    type :: DirectedAcyclicGraph
        type(HashMap) :: adjacency_list                         ! Adjacency list represented as a hash map
        type(Set) :: nodes                                       ! Set of nodes in the DAG
    contains
        procedure :: initialise => dag_initialise
        procedure :: add_node => dag_add_node
        procedure :: add_edge => dag_add_edge
        procedure :: remove_node => dag_remove_node
        procedure :: remove_edge => dag_remove_edge
        procedure :: contains_node => dag_contains_node
        procedure :: contains_edge => dag_contains_edge
        procedure :: topological_sort => dag_topological_sort
        procedure :: clear => dag_clear
        procedure :: print => dag_print
    end type DirectedAcyclicGraph

contains

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Prepend an element at the start of the linked list.
    ! @param[inout] self The linked list.
    ! @param[in] value The value to insert.
    ! ---------------------------------------------------------------------------------------------------
    subroutine linked_list_prepend(self, value)
        implicit none
        class(LinkedList), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: value
        type(ListNode), pointer :: new_node
        integer :: alloc_stat

        ! Allocate a new node for the value
        allocate(new_node, stat=alloc_stat)
        if (alloc_stat /= 0) then
            call report("Error: Failed to allocate new node in list_prepend.", is_error=.true.)
            return
        end if

        ! Set the new node's value and make it point to the current head
        new_node%value = value
        new_node%next => self%head

        ! Set the new node as the head of the list
        self%head => new_node

        ! Increment the size of the linked list
        self%size = self%size + 1
    end subroutine linked_list_prepend


    ! ---------------------------------------------------------------------------------------------------
    ! @brief Insert an element at the end of the linked list.
    ! @param[inout] self The linked list.
    ! @param[in] value The value to insert.
    ! ---------------------------------------------------------------------------------------------------
    subroutine linked_list_append(self, value)
        implicit none
        class(LinkedList), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: value
        type(ListNode), pointer :: new_node, current
        integer :: alloc_stat

        ! Allocate a new node for the value
        allocate(new_node, stat=alloc_stat)
        if (alloc_stat /= 0) then
            call report("Error: Failed to allocate new node in list_insert.", is_error=.true.)
            return
        end if

        new_node%value = value
        new_node%next => null()

        ! If the list is empty, set the new node as the head
        if (.not. associated(self%head)) then
            self%head => new_node
        else
            ! Traverse the list to find the last node
            current => self%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => new_node
        end if

        self%size = self%size + 1
    end subroutine linked_list_append

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Remove an element from the linked list.
    ! @param[inout] self The linked list.
    ! @param[in] value The value to remove.
    ! ---------------------------------------------------------------------------------------------------
    subroutine linked_list_remove(self, value)
        implicit none
        class(LinkedList), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: value
        type(ListNode), pointer :: current, previous

        if (.not. associated(self%head)) return ! List is empty, nothing to remove

        ! Special case: removing the head node
        if (self%head%value == value) then
            current => self%head
            self%head => current%next
            deallocate(current)
            self%size = self%size - 1
            return
        end if

        ! Traverse the list to find the node to remove
        previous => self%head
        current => self%head%next
        do while (associated(current))
            if (current%value == value) then
                previous%next => current%next
                deallocate(current)
                self%size = self%size - 1
                return
            end if
            previous => current
            current => current%next
        end do
    end subroutine linked_list_remove

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Check if the list contains a specific element.
    ! @param[in] self The linked list.
    ! @param[in] value The value to check.
    ! @return contains Flag indicating whether the value is in the list.
    ! ---------------------------------------------------------------------------------------------------
    logical function linked_list_contains(self, value)
        implicit none
        class(LinkedList), intent(in) :: self
        integer(kind=custom_int), intent(in) :: value
        type(ListNode), pointer :: current

        linked_list_contains = .false.
        current => self%head
        do while (associated(current))
            if (current%value == value) then
                linked_list_contains = .true.
                return
            end if
            current => current%next
        end do
    end function linked_list_contains

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Clear the linked list, deallocating all nodes.
    ! @param[inout] self The linked list.
    ! ---------------------------------------------------------------------------------------------------
    subroutine linked_list_clear(self)
        implicit none
        class(LinkedList), intent(inout) :: self
        type(ListNode), pointer :: current, next_node

        current => self%head
        do while (associated(current))
            next_node => current%next
            deallocate(current)
            current => next_node
        end do
        self%head => null()
        self%size = 0
    end subroutine linked_list_clear

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Print the elements of the linked list.
    ! @param[in] self The linked list.
    ! ---------------------------------------------------------------------------------------------------
    subroutine linked_list_print(self)
        implicit none
        class(LinkedList), intent(in) :: self
        type(ListNode), pointer :: current

        if (.not. associated(self%head)) then
            print *, "List is empty."
            return
        end if

        current => self%head
        do while (associated(current))
            print *, current%value
            current => current%next
        end do
    end subroutine linked_list_print

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Initialise the hash map with a given size and value length.
    ! @param[inout] self The hash map.
    ! @param[in] map_size The initial size of the hash map.
    ! ---------------------------------------------------------------------------------------------------
    subroutine hash_map_initialise(self, map_size)
        implicit none

        class(HashMap), intent(inout) :: self                   ! The hash map type
        integer(kind=custom_int), intent(in) :: map_size                  ! The initial size of the hash map
        integer(kind=custom_int) :: alloc_stat                   ! Allocation status

        if (map_size .lt. 0) then
            call report("Error (hash_map): Size must be a positive integer.", is_error=.true.)
            return
        end if

        allocate (self%buckets(map_size), stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call report("Error (hash_map): Failed to initialise hash map.", is_error=.true.)
            return
        end if

        self%size = map_size
        self%count = 0

        ! Initialise all bucket heads to null
        self%buckets = HashMapBucket()
    end subroutine hash_map_initialise

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Insert a key-value pair into the hash map.
    ! @param[inout] self The hash map.
    ! @param[in] key The key as an integer array.
    ! @param[in] value The associated value as an integer array.
    ! ---------------------------------------------------------------------------------------------------
    subroutine hash_map_insert(self, key, value)
        implicit none

        class(HashMap), intent(inout) :: self                   ! The hash map
        integer(kind=custom_int), intent(in) :: key(:)          ! The key to insert (1D integer array)
        integer(kind=custom_int), intent(in) :: value(:)        ! The value to insert (1D integer array of variable length)
        integer(kind=custom_int) :: index                       ! Index into buckets
        integer :: alloc_stat                                   ! Allocation status
        real(real64) :: load_factor                             ! Load factor
        type(HashMapNode), pointer :: new_node                  ! New node to insert

        ! Compute hash index
        index = map_hash(key, self%size)
        if (index .eq. -1) then
            call report("Error (hash_map): Invalid hash index.", is_error=.true.)
            return
        end if

        ! Allocate a new node
        allocate (new_node, stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call report("Error (hash_map): Failed to allocate new node.", is_error=.true.)
            return
        end if

        ! Allocate and assign key
        allocate (new_node%key(size(key)), stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call report("Error (hash_map): Failed to allocate key array.", is_error=.true.)
            deallocate (new_node)
            return
        end if
        new_node%key = key

        ! Allocate and assign value (variable length)
        allocate (new_node%value(size(value)), stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call report("Error (hash_map): Failed to allocate value array.", is_error=.true.)
            deallocate (new_node%key)
            deallocate (new_node)
            return
        end if
        new_node%value = value

        ! Insert the node at the beginning of the bucket's linked list
        new_node%next => self%buckets(index)%head
        self%buckets(index)%head => new_node

        self%count = self%count + 1

        ! Calculate load factor
        load_factor = real(self%count, real64)/real(self%size, real64)

        ! Resize if load factor exceeds threshold (e.g., 0.7)
        if (load_factor > 0.7) then
            call hash_map_resize(self, self%size*2)
        end if
    end subroutine hash_map_insert

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Delete a key-value pair from the hash map.
    ! @param[inout] self The hash map.
    ! @param[in] key The key to delete.
    ! ---------------------------------------------------------------------------------------------------
    subroutine hash_map_delete(self, key)
        implicit none

        class(HashMap), intent(inout) :: self                   ! The hash map
        integer(kind=custom_int), intent(in) :: key(:)           ! The key to delete
        integer(kind=custom_int) :: index                        ! Index into buckets
        type(HashMapNode), pointer :: current_node              ! Current node
        type(HashMapNode), pointer :: prev_node                 ! Previous node

        index = map_hash(key, self%size)
        if (index .eq. -1) then
            call report("Error (hash_map): Invalid hash index.", is_error=.true.)
            return
        end if

        prev_node => null()
        current_node => self%buckets(index)%head

        do while (associated(current_node))
            if (all(current_node%key == key)) then
                if (associated(prev_node)) then
                    prev_node%next => current_node%next
                else
                    self%buckets(index)%head => current_node%next
                end if
                deallocate (current_node%key)
                deallocate (current_node%value)
                deallocate (current_node)
                self%count = self%count - 1
                return
            end if
            prev_node => current_node
            current_node => current_node%next
        end do
    end subroutine hash_map_delete

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Find a key-value pair in the hash map.
    ! @param[in] self The hash map.
    ! @param[in] key The key to search for.
    ! @param[out, optional] value The value associated with the key.
    ! @return found Flag indicating if the key was found.
    ! ---------------------------------------------------------------------------------------------------
    function hash_map_find(self, key, value) result(found)
        implicit none

        class(HashMap), intent(in) :: self                          ! The hash map
        integer(kind=custom_int), intent(in) :: key(:)              ! The key to search for
        integer(kind=custom_int), allocatable, optional :: value(:) ! Result value array (optional)
        logical :: found                                            ! Flag indicating if the key was found
        integer(kind=custom_int) :: index                           ! Index into buckets
        type(HashMapNode), pointer :: current_node                  ! Current node
        integer :: alloc_stat

        found = .false.  ! Initialise as not found
        index = map_hash(key, self%size)

        if (index .eq. -1) then
            call report("Error (hash_map): Invalid hash index.", is_error=.true.)
            return
        end if

        current_node => self%buckets(index)%head
        do while (associated(current_node))
            if (all(current_node%key == key)) then
                found = .true.  ! Key was found

                ! Only allocate and assign value if present
                if (present(value)) then
                    if (.not. allocated(value)) then
                        allocate (value(size(current_node%value)), stat=alloc_stat)
                        if (alloc_stat /= 0) then
                            call report("Error (hash_map_find): Failed to allocate value array.", is_error=.true.)
                            return
                        end if
                    end if
                    value = current_node%value
                end if

                return  ! Exit the function after finding the key
            end if
            current_node => current_node%next
        end do
    end function hash_map_find

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Clear the hash map, deallocating all nodes.
    ! @param[inout] self The hash map.
    ! ---------------------------------------------------------------------------------------------------
    subroutine hash_map_clear(self)
        implicit none

        class(HashMap), intent(inout) :: self                   ! The hash map
        integer(kind=custom_int) :: i                                     ! Loop index
        type(HashMapNode), pointer :: current_node              ! Current and next nodes
        type(HashMapNode), pointer :: next_node                 ! Next node

        do i = 1, self%size
            current_node => self%buckets(i)%head
            do while (associated(current_node))
                next_node => current_node%next
                deallocate (current_node%key)
                deallocate (current_node%value)
                deallocate (current_node)
                current_node => next_node
            end do
            self%buckets(i)%head => null()
        end do

        self%count = 0
    end subroutine hash_map_clear

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Resize the hash map to a new size.
    ! @param[inout] self The hash map.
    ! @param[in] new_size The new size of the hash map.
    ! ---------------------------------------------------------------------------------------------------
    subroutine hash_map_resize(self, new_size)
        implicit none

        class(HashMap), intent(inout) :: self                   ! The hash map
        integer(kind=custom_int), intent(in) :: new_size        ! New size of the hash map
        type(HashMapBucket), allocatable :: new_buckets(:)      ! New array of buckets
        type(HashMapNode), pointer :: current_node              ! Current and next nodes
        type(HashMapNode), pointer :: next_node                 ! Next node
        integer(kind=custom_int) :: i, index                     ! Loop index and hash index
        integer :: alloc_stat                                   ! Allocation status

        if (new_size .lt. 0) then
            call report("Error (hash_map): New size must be positive.", is_error=.true.)
            return
        end if

        allocate (new_buckets(new_size), stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call report("Error (hash_map): Failed to allocate new buckets.", is_error=.true.)
            return
        end if

        ! Initialise new buckets to null
        new_buckets = HashMapBucket()

        do i = 1, self%size
            current_node => self%buckets(i)%head
            do while (associated(current_node))
                next_node => current_node%next
                index = map_hash(current_node%key, new_size)
                if (index .eq. -1) then
                    call report("Error (hash_map): Invalid hash index during resize.", is_error=.true.)
                    return
                end if
                ! Insert the node into the new bucket
                current_node%next => new_buckets(index)%head
                new_buckets(index)%head => current_node
                current_node => next_node
            end do
        end do

        deallocate (self%buckets)
        self%buckets = new_buckets
        self%size = new_size
    end subroutine hash_map_resize

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Compute the hash index for a given key.
    ! @param[in] key The key as an integer array.
    ! @param[in] bucket_size The size of the bucket array.
    ! @return index The computed hash index.
    ! ---------------------------------------------------------------------------------------------------
    integer(kind=custom_int) function map_hash(key, bucket_size)
        implicit none

        integer(kind=custom_int), intent(in) :: key(:)                    ! The key to hash
        integer(kind=custom_int), intent(in) :: bucket_size               ! The size of the bucket array
        integer(kind=custom_int) :: i                                     ! Loop index

        if (bucket_size .lt. 0) then
            call report("Error (hash_map): Bucket size must be a positive integer.", is_error=.true.)
            map_hash = -1
            return
        end if

        map_hash = 0
        do i = 1, size(key)
            map_hash = mod(map_hash + key(i), bucket_size)
        end do
        map_hash = map_hash + 1
    end function map_hash

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Initialise the set.
    ! @param[inout] self The set to initialise.
    ! @param[in] initial_size Initial size for allocation (optional).
    ! ---------------------------------------------------------------------------------------------------
    subroutine set_initialise(self, initial_size)
        implicit none
        class(Set), intent(inout) :: self
        integer(kind=custom_int), intent(in), optional :: initial_size
        integer :: alloc_stat

        if (present(initial_size)) then
            allocate (self%elements(initial_size), stat=alloc_stat)
        else
            allocate (self%elements(0), stat=alloc_stat)
        end if

        if (alloc_stat .ne. 0) then
            call report("Error (set): Failed to initialise set.", is_error=.true.)
            return
        end if

        self%size = 0
    end subroutine set_initialise

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Insert a unique element into the set in sorted order.
    ! @param[inout] self The set to insert into.
    ! @param[in] element The element to insert.
    ! ---------------------------------------------------------------------------------------------------
    subroutine set_insert(self, element)
        implicit none
        class(Set), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: element
        integer :: i, alloc_stat

        ! Check if the element already exists
        if (set_contains(self, element)) return

        ! Resize if necessary
        if (self%size == size(self%elements)) then
            call array_resize(self%elements, self%size + 1)
        end if

        ! Insert in sorted order
        do i = 1, self%size
            if (self%elements(i) > element) exit
        end do

        ! Shift elements to make room for the new one
        self%elements(i + 1:self%size + 1) = self%elements(i:self%size)
        self%elements(i) = element
        self%size = self%size + 1
    end subroutine set_insert

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Check if the set contains an element.
    ! @param[in] self The set to check.
    ! @param[in] element The element to check.
    ! @return contains Flag indicating if the element is in the set.
    ! ---------------------------------------------------------------------------------------------------
    logical function set_contains(self, element)
        implicit none
        class(Set), intent(in) :: self
        integer(kind=custom_int), intent(in) :: element
        integer :: i

        set_contains = .false.
        do i = 1, self%size
            if (self%elements(i) == element) then
                set_contains = .true.
                return
            end if
        end do
    end function set_contains

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Remove an element from the set.
    ! @param[inout] self The set to remove from.
    ! @param[in] element The element to remove.
    ! ---------------------------------------------------------------------------------------------------
    subroutine set_remove(self, element)
        implicit none
        class(Set), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: element
        integer :: i

        do i = 1, self%size
            if (self%elements(i) == element) then
                self%elements(i:self%size - 1) = self%elements(i + 1:self%size)
                self%size = self%size - 1
                return
            end if
        end do
    end subroutine set_remove

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Clear the set, deallocating all elements.
    ! @param[inout] self The set to clear.
    ! ---------------------------------------------------------------------------------------------------
    subroutine set_clear(self)
        implicit none
        class(Set), intent(inout) :: self

        if (allocated(self%elements)) then
            deallocate (self%elements)
        end if
        self%size = 0
    end subroutine set_clear

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Initialise the DAG.
    ! @param[inout] self The DAG.
    ! @param[in] initial_size The initial size of the adjacency list hash map.
    ! ---------------------------------------------------------------------------------------------------
    subroutine dag_initialise(self, initial_size)
        implicit none
        class(DirectedAcyclicGraph), intent(inout) :: self
        integer(kind=custom_int), intent(in), optional :: initial_size
        integer(kind=custom_int) :: size_to_use

        if (present(initial_size)) then
            size_to_use = initial_size
        else
            size_to_use = 16  ! Default initial size
        end if

        call self%adjacency_list%initialise(size_to_use)
        call self%nodes%initialise()
    end subroutine dag_initialise

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Add a node to the DAG.
    ! @param[inout] self The DAG.
    ! @param[in] node The node identifier to add.
    ! ---------------------------------------------------------------------------------------------------
    subroutine dag_add_node(self, node)
        implicit none
        class(DirectedAcyclicGraph), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: node
        integer(kind=custom_int), allocatable :: key(:)
        integer(kind=custom_int), allocatable :: empty_value(:)

        ! Check if node already exists
        if (self%nodes%contains(node)) return

        ! Insert node into the set
        call self%nodes%insert(node)

        ! Initialize adjacency list for the node with an empty list
        allocate(key(1))
        key(1) = node
        allocate(empty_value(0))
        call self%adjacency_list%insert(key, empty_value)
        deallocate(key)
        deallocate(empty_value)
    end subroutine dag_add_node

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Remove a node from the DAG.
    ! @param[inout] self The DAG.
    ! @param[in] node The node identifier to remove.
    ! ---------------------------------------------------------------------------------------------------
    subroutine dag_remove_node(self, node)
        implicit none
        class(DirectedAcyclicGraph), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: node
        integer(kind=custom_int), allocatable :: key(:), value(:)
        integer(kind=custom_int) :: i, j
        logical :: found

        ! Remove node from the set
        call self%nodes%remove(node)

        ! Remove the node's adjacency list
        allocate(key(1))
        key(1) = node
        call self%adjacency_list%delete(key)
        deallocate(key)

        ! Remove all edges pointing to this node
        do i = 1, self%adjacency_list%size
            allocate(key(1))
            key(1) = i  ! Assuming keys are sequential; adjust if keys are not sequential
            found = self%adjacency_list%find(key, value)
            if (found) then
                ! Remove 'node' from the adjacency list
                do j = 1, size(value)
                    if (value(j) == node) then
                        value(j) = value(size(value))
                        call array_resize(value, size(value) - 1)
                        exit
                    end if
                end do
                call self%adjacency_list%insert(key, value)
                deallocate(value)
            end if
            deallocate(key)
        end do
    end subroutine dag_remove_node

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Add a directed edge to the DAG.
    ! @param[inout] self The DAG.
    ! @param[in] from_node The source node identifier.
    ! @param[in] to_node The destination node identifier.
    ! @return success Flag indicating if the edge was added successfully.
    ! ---------------------------------------------------------------------------------------------------
    logical function dag_add_edge(self, from_node, to_node)
    implicit none
    class(DirectedAcyclicGraph), intent(inout) :: self
    integer(kind=custom_int), intent(in) :: from_node, to_node
    integer(kind=custom_int), allocatable :: key(:), value(:)
    logical :: found, cycle_detected
    integer(kind=custom_int) :: alloc_stat

    dag_add_edge = .false.

    ! Ensure both nodes exist
    if (.not. self%nodes%contains(from_node)) then
        call report("Error (dag_add_edge): 'from_node' does not exist.", is_error=.true.)
        return
    end if
    if (.not. self%nodes%contains(to_node)) then
        call report("Error (dag_add_edge): 'to_node' does not exist.", is_error=.true.)
        return
    end if

    ! Check if the edge already exists
    allocate(key(1))
    key(1) = from_node
    found = self%adjacency_list%find(key, value)
    if (found) then
        if (any(value == to_node)) then
            ! Edge already exists
            dag_add_edge = .true.
            deallocate(key)
            deallocate(value)
            return
        end if
    else
        ! Initialize adjacency list for 'from_node' if not found
        allocate(value(0))
    end if
    deallocate(key)

    ! Temporarily add the edge
    allocate(key(1))
    key(1) = from_node
    if (found) then
        call self%adjacency_list%find(key, value)
        call array_resize(value, size(value) + 1)
        value(size(value)) = to_node
    else
        allocate(value(1))
        value(1) = to_node
    end if
    call self%adjacency_list%insert(key, value)
    deallocate(key)
    deallocate(value)

    ! Check for cycles
    cycle_detected = dag_detect_cycle(self)
    if (cycle_detected) then
        ! Revert the addition
        allocate(key(1))
        key(1) = from_node
        call self%adjacency_list%find(key, value)
        value = [to_node]  ! Simplistic removal; adjust as needed
        call self%adjacency_list%insert(key, value)
        deallocate(key)
        deallocate(value)
        call report("Error (dag_add_edge): Adding this edge creates a cycle.", is_error=.true.)
        return
    end if

    dag_add_edge = .true.
    end function dag_add_edge






    ! ---------------------------------------------------------------------------------------------------
    ! @brief Resize a 1D array.
    ! @param[inout] arr The array to resize.
    ! @param[in] new_size The new size of the array.
    ! ---------------------------------------------------------------------------------------------------
    subroutine array_resize(arr, new_size)
        implicit none

        integer(kind=custom_int), allocatable, intent(inout) :: arr(:)  ! 1D array
        integer(kind=custom_int), intent(in) :: new_size               ! New size of the array
        integer(kind=custom_int), allocatable :: temp_arr(:)           ! Temporary array for data
        integer(kind=custom_int) :: alloc_stat

        ! Store old data if array is already allocated
        if (allocated(arr)) then
            allocate (temp_arr(size(arr)), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call report("Error: Failed to allocate temporary array in array_resize.", is_error=.true.)
                return
            end if
            temp_arr = arr
            deallocate (arr)
        end if

        ! Allocate new array with new size
        allocate (arr(new_size), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call report("Error: Failed to allocate new array in array_resize.", is_error=.true.)
            if (allocated(temp_arr)) then
                allocate (arr(size(temp_arr)), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call report("Error: Failed to reallocate original array in array_resize.", is_error=.true.)
                    deallocate (temp_arr)
                    return
                end if
                arr = temp_arr
                deallocate (temp_arr)
            end if
            return
        end if

        ! Initialise the new array to zero
        arr = 0

        ! Copy old data back into the resized array
        if (allocated(temp_arr)) then
            arr(1:min(size(temp_arr), new_size)) = temp_arr(1:min(size(temp_arr), new_size))
            deallocate (temp_arr)
        end if
    end subroutine array_resize

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Resize a 2D array.
    ! @param[inout] arr The array to resize.
    ! @param[in] new_rows The new number of rows.
    ! @param[in] new_cols The new number of columns.
    ! ---------------------------------------------------------------------------------------------------
    subroutine array2d_resize(arr, new_rows, new_cols)
        implicit none

        integer(kind=custom_int), allocatable, dimension(:, :), intent(inout) :: arr ! 2D array
        integer(kind=custom_int), intent(in) :: new_rows, new_cols                   ! New dimensions
        integer(kind=custom_int), allocatable, dimension(:, :) :: temp_arr           ! Temporary array for data
        integer(kind=custom_int) :: min_rows, min_cols, alloc_stat

        ! Determine minimum dimensions for copying
        min_rows = min(size(arr, dim=1), new_rows)
        min_cols = min(size(arr, dim=2), new_cols)

        ! Store old data if array is already allocated
        if (allocated(arr)) then
            allocate (temp_arr(min_rows, min_cols), stat=alloc_stat)
            if (alloc_stat /= 0) then
                call report("Error: Failed to allocate temporary array in array2d_resize.", is_error=.true.)
                return
            end if
            temp_arr = arr(1:min_rows, 1:min_cols)
            deallocate (arr)
        end if

        ! Allocate new array with new dimensions
        allocate (arr(new_rows, new_cols), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call report("Error: Failed to allocate new array in array2d_resize.", is_error=.true.)
            if (allocated(temp_arr)) then
                allocate (arr(size(temp_arr, dim=1), size(temp_arr, dim=2)), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call report("Error: Failed to reallocate original array in array2d_resize.", is_error=.true.)
                    deallocate (temp_arr)
                    return
                end if
                arr = temp_arr
                deallocate (temp_arr)
            end if
            return
        end if

        ! Initialise the new array to zero
        arr = 0

        ! Copy old data back into the resized array
        if (allocated(temp_arr)) then
            arr(1:min_rows, 1:min_cols) = temp_arr
            deallocate (temp_arr)
        end if
    end subroutine array2d_resize

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Randomly permute the rows or columns of a 2D array along a specified dimension.
    ! @detail The subroutine uses the Fisher-Yates shuffle algorithm to randomly permute
    !         the rows or columns of the input 2D array `arr` in-place along the specified dimension.
    ! @param[inout] arr The 2D array to be permuted.
    ! @param[in] dim The dimension along which to perform the permutation (1 for rows, 2 for columns).
    ! ---------------------------------------------------------------------------------------------------
    subroutine array2d_permute(arr, dim)
        implicit none

        real(real64), intent(inout), dimension(:, :) :: arr     ! 2D array
        integer(kind=custom_int), intent(in) :: dim                       ! Dimension to shuffle
        integer(kind=custom_int) :: n, i, j                               ! Loop indices and swap index
        real(real64) :: rand_num                                ! Random number
        real(real64), allocatable :: temp_row(:), temp_col(:)   ! Temporary arrays for swapping
        integer :: alloc_stat

        ! Determine the size of the array
        if (dim .eq. 1) then
            n = size(arr, dim=1)
        else if (dim .eq. 2) then
            n = size(arr, dim=2)
        else
            call report("Error: Invalid dimension specified. Please use 1 or 2.", is_error=.true.)
            return
        end if

        ! Initialise random seed
        call random_seed()

        if (dim .eq. 1) then
            ! Shuffle along the first dimension (rows)
            allocate (temp_row(size(arr, dim=2)), stat=alloc_stat)
            if (alloc_stat .ne. 0) then
                call report("Error: Failed to allocate temp_row in permute_array2d.", is_error=.true.)
                return
            end if
            do i = n, 2, -1
                call random_number(rand_num)
                j = int(rand_num*i) + 1

                ! Swap rows i and j
                temp_row = arr(i, :)
                arr(i, :) = arr(j, :)
                arr(j, :) = temp_row
            end do
            deallocate (temp_row)

        else if (dim .eq. 2) then
            ! Shuffle along the second dimension (columns)
            allocate (temp_col(size(arr, dim=1)), stat=alloc_stat)
            if (alloc_stat .ne. 0) then
                call report("Error: Failed to allocate temp_col in permute_array2d.", is_error=.true.)
                return
            end if
            do i = n, 2, -1
                call random_number(rand_num)
                j = int(rand_num*i) + 1

                ! Swap columns i and j
                temp_col = arr(:, i)
                arr(:, i) = arr(:, j)
                arr(:, j) = temp_col
            end do
            deallocate (temp_col)
        end if
    end subroutine array2d_permute

end module data_structures
