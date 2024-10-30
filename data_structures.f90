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
        procedure :: get => linked_list_get                     ! Retrieve the elements as an integer array
    end type LinkedList

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines a node in the linked list.
    ! ---------------------------------------------------------------------------------------------------
    type :: CircularLinkedList
        type(ListNode), pointer :: head => null()               ! Pointer to the head of the list
        type(ListNode), pointer :: tail => null()               ! Pointer to the tail of the list
        integer(kind=custom_int) :: size = 0                    ! Number of elements in the list
    contains
        procedure :: prepend => circular_linked_list_prepend             ! Insert an element at the start
        procedure :: append => circular_linked_list_append               ! Insert an element at the end
        procedure :: remove => circular_linked_list_remove               ! Remove an element from the list
        procedure :: contains => circular_linked_list_contains           ! Check if an element exists in the list
        procedure :: clear => circular_linked_list_clear                 ! Clear the list
        procedure :: print => circular_linked_list_print                 ! Print the elements of the list
    end type CircularLinkedList

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
        procedure :: get => hash_map_get                        ! Find a value by key
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
    ! @brief Defines a min-heap (priority queue).
    ! ---------------------------------------------------------------------------------------------------
    type :: MinHeap
        type(MinHeapElement), allocatable :: elements(:)            ! Array of elements (priority and value)
        integer :: count                                            ! Current number of elements in the heap
        integer :: size                                             ! Maximum capacity of the heap
    contains
        procedure :: initialise => min_heap_initialise
        procedure :: insert => min_heap_insert
        procedure :: pop => min_heap_pop
        procedure :: is_empty => min_heap_is_empty
    end type MinHeap

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Defines an element in the min-heap, consisting of a priority and a variable-sized integer array.
    ! ---------------------------------------------------------------------------------------------------
    type :: MinHeapElement
        real(kind=custom_real) :: Priority          ! Priority of the element
        integer, allocatable :: value(:)       ! Associated variable-sized integer array
    end type MinHeapElement

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
        allocate (new_node, stat=alloc_stat)
        if (alloc_stat /= 0) then
            call meshf_report("Error: Failed to allocate new node in list_prepend.", is_error=.true.)
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
        allocate (new_node, stat=alloc_stat)
        if (alloc_stat /= 0) then
            call meshf_report("Error: Failed to allocate new node in list_insert.", is_error=.true.)
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
            deallocate (current)
            self%size = self%size - 1
            return
        end if

        ! Traverse the list to find the node to remove
        previous => self%head
        current => self%head%next
        do while (associated(current))
            if (current%value == value) then
                previous%next => current%next
                deallocate (current)
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
            deallocate (current)
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
    ! @brief Retrieve the elements of the linked list as an integer array.
    ! @param[in] self The linked list.
    ! @param[out] array An integer array containing all elements in the linked list.
    ! ---------------------------------------------------------------------------------------------------
    function linked_list_get(self) result(array)
        implicit none
        class(LinkedList), intent(in) :: self
        integer(kind=custom_int), allocatable :: array(:)
        type(ListNode), pointer :: current
        integer(kind=custom_int) :: i

        ! Check if the list is empty
        if (self%size == 0) then
            allocate(array(0))  ! Return an empty array
            return
        end if

        ! Allocate the array with the size of the linked list
        allocate(array(self%size))

        ! Traverse the linked list and copy values into the array
        current => self%head
        do i = 1, self%size
            array(i) = current%value
            current => current%next
        end do
    end function linked_list_get


    ! ---------------------------------------------------------------------------------------------------
    ! @brief Insert an element at the end of the circular linked list.
    ! @param[inout] self The circular linked list.
    ! @param[in] value The value to insert.
    ! ---------------------------------------------------------------------------------------------------
    subroutine circular_linked_list_append(self, value)
        implicit none
        class(CircularLinkedList), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: value
        type(ListNode), pointer :: new_node
        integer :: alloc_stat

        ! Allocate a new node
        allocate (new_node, stat=alloc_stat)
        if (alloc_stat /= 0) then
            call meshf_report("Error: Failed to allocate new node in circular_linked_list_append.", is_error=.true.)
            return
        end if

        new_node%value = value

        if (.not. associated(self%head)) then
            ! List is empty
            self%head => new_node
            self%tail => new_node
            new_node%next => new_node  ! Point to itself to make it cyclic
        else
            ! Insert at the end
            new_node%next => self%head
            self%tail%next => new_node
            self%tail => new_node
        end if

        self%size = self%size + 1
    end subroutine circular_linked_list_append

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Insert an element at the start of the circular linked list.
    ! @param[inout] self The circular linked list.
    ! @param[in] value The value to insert.
    ! ---------------------------------------------------------------------------------------------------
    subroutine circular_linked_list_prepend(self, value)
        implicit none
        class(CircularLinkedList), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: value
        type(ListNode), pointer :: new_node
        integer :: alloc_stat

        ! Allocate a new node
        allocate (new_node, stat=alloc_stat)
        if (alloc_stat /= 0) then
            call meshf_report("Error: Failed to allocate new node in circular_linked_list_prepend.", is_error=.true.)
            return
        end if

        new_node%value = value

        if (.not. associated(self%head)) then
            ! List is empty
            self%head => new_node
            self%tail => new_node
            new_node%next => new_node  ! Point to itself
        else
            ! Insert at the beginning
            new_node%next => self%head
            self%tail%next => new_node
            self%head => new_node
        end if

        self%size = self%size + 1
    end subroutine circular_linked_list_prepend

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Remove an element from the circular linked list.
    ! @param[inout] self The circular linked list.
    ! @param[in] value The value to remove.
    ! ---------------------------------------------------------------------------------------------------
    subroutine circular_linked_list_remove(self, value)
        implicit none
        class(CircularLinkedList), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: value
        type(ListNode), pointer :: current, previous
        logical :: found

        if (.not. associated(self%head)) return  ! List is empty

        found = .false.
        previous => self%tail
        current => self%head

        do
            if (current%value == value) then
                found = .true.
                exit
            end if
            previous => current
            current => current%next
            if (associated(current, self%head)) exit  ! Completed a full cycle
        end do

        if (found) then
            if (associated(current, self%head)) then
                ! Removing the head
                self%head => current%next
                self%tail%next => self%head
            else if (associated(current, self%tail)) then
                ! Removing the tail
                self%tail => previous
                self%tail%next => self%head
            else
                previous%next => current%next
            end if
            deallocate (current)
            self%size = self%size - 1

            ! If the list becomes empty
            if (self%size == 0) then
                self%head => null()
                self%tail => null()
            end if
        end if
    end subroutine circular_linked_list_remove

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Check if the circular linked list contains a specific element.
    ! @param[in] self The circular linked list.
    ! @param[in] value The value to check.
    ! @return contains Flag indicating whether the value is in the list.
    ! ---------------------------------------------------------------------------------------------------
    logical function circular_linked_list_contains(self, value)
        implicit none
        class(CircularLinkedList), intent(in) :: self
        integer(kind=custom_int), intent(in) :: value
        type(ListNode), pointer :: current

        circular_linked_list_contains = .false.

        if (.not. associated(self%head)) return

        current => self%head
        do
            if (current%value == value) then
                circular_linked_list_contains = .true.
                return
            end if
            current => current%next
            if (associated(current, self%head)) exit  ! Completed a full cycle
        end do
    end function circular_linked_list_contains

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Print the elements of the circular linked list.
    ! @param[in] self The circular linked list.
    ! ---------------------------------------------------------------------------------------------------
    subroutine circular_linked_list_print(self)
        implicit none
        class(CircularLinkedList), intent(in) :: self
        type(ListNode), pointer :: current

        if (.not. associated(self%head)) then
            print *, "List is empty."
            return
        end if

        current => self%head
        do
            print *, current%value
            current => current%next
            if (associated(current, self%head)) exit  ! Completed a full cycle
        end do
    end subroutine circular_linked_list_print

    subroutine circular_linked_list_clear(self)
        implicit none
        class(CircularLinkedList), intent(inout) :: self
        type(ListNode), pointer :: current, next_node

        if (.not. associated(self%head)) return

        ! Break the cycle
        self%tail%next => null()

        current => self%head
        do while (associated(current))
            next_node => current%next
            deallocate (current)
            current => next_node
        end do

        self%head => null()
        self%tail => null()
        self%size = 0
    end subroutine circular_linked_list_clear

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
            call meshf_report("Error (hash_map): Size must be a positive integer.", is_error=.true.)
            return
        end if

        allocate (self%buckets(map_size), stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call meshf_report("Error (hash_map): Failed to initialise hash map.", is_error=.true.)
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
            call meshf_report("Error (hash_map): Invalid hash index.", is_error=.true.)
            return
        end if

        ! Allocate a new node
        allocate (new_node, stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call meshf_report("Error (hash_map): Failed to allocate new node.", is_error=.true.)
            return
        end if

        ! Allocate and assign key
        allocate (new_node%key(size(key)), stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call meshf_report("Error (hash_map): Failed to allocate key array.", is_error=.true.)
            deallocate (new_node)
            return
        end if
        new_node%key = key

        ! Allocate and assign value (variable length)
        allocate (new_node%value(size(value)), stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call meshf_report("Error (hash_map): Failed to allocate value array.", is_error=.true.)
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
            call meshf_report("Error (hash_map): Invalid hash index.", is_error=.true.)
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
    function hash_map_get(self, key, value) result(found)
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
            call meshf_report("Error (hash_map): Invalid hash index.", is_error=.true.)
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
                            call meshf_report("Error (hash_map_find): Failed to allocate value array.", is_error=.true.)
                            return
                        end if
                    end if
                    value = current_node%value
                end if

                return  ! Exit the function after finding the key
            end if
            current_node => current_node%next
        end do
    end function hash_map_get

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
            call meshf_report("Error (hash_map): New size must be positive.", is_error=.true.)
            return
        end if

        allocate (new_buckets(new_size), stat=alloc_stat)
        if (alloc_stat .ne. 0) then
            call meshf_report("Error (hash_map): Failed to allocate new buckets.", is_error=.true.)
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
                    call meshf_report("Error (hash_map): Invalid hash index during resize.", is_error=.true.)
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
            call meshf_report("Error (hash_map): Bucket size must be a positive integer.", is_error=.true.)
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
            call meshf_report("Error (set): Failed to initialise set.", is_error=.true.)
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
    ! @brief Initialise the min-heap.
    ! @param[inout] self The min-heap to initialise.
    ! @param[in] capacity The initial capacity of the heap.
    ! ---------------------------------------------------------------------------------------------------
    subroutine min_heap_initialise(self, capacity)
        implicit none
        class(MinHeap), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: capacity

        self%size = capacity
        self%count = 0
        allocate (self%elements(capacity))
    end subroutine min_heap_initialise

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Check if the min-heap is empty.
    ! @param[in] self The min-heap.
    ! @return is_empty Flag indicating if the heap is empty.
    ! ---------------------------------------------------------------------------------------------------
    logical function min_heap_is_empty(self)
        implicit none
        class(MinHeap), intent(in) :: self
        min_heap_is_empty = (self%count == 0)
    end function min_heap_is_empty

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Insert an element into the min-heap.
    ! @param[inout] self The min-heap.
    ! @param[in] priority The priority of the element.
    ! @param[in] value The integer(kind=custom_int) array to insert.
    ! ---------------------------------------------------------------------------------------------------
    subroutine min_heap_insert(self, value, priority)
        implicit none
        class(MinHeap), intent(inout) :: self
        real(kind=custom_real), intent(in) :: priority
        integer(kind=custom_int), intent(in) :: value(:)
        integer(kind=custom_int) :: i, alloc_stat

        if (self%count >= self%size) then
            print *, 'Error: MinHeap is full!'
            return
        end if

        ! Insert at the end
        self%count = self%count + 1
        i = self%count
        self%elements(i)%priority = priority
        allocate (self%elements(i)%value(size(value)), stat=alloc_stat)
        if (alloc_stat /= 0) then
            print *, 'Error: Failed to allocate memory for value array!'
            self%count = self%count - 1
            return
        end if
        self%elements(i)%value = value

        ! Bubble up to maintain heap property
        call min_heap_bubble_up(self, i)
    end subroutine min_heap_insert

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Extract the minimum element from the min-heap.
    ! @param[inout] self The min-heap.
    ! @param[out] priority The minimum priority.
    ! @param[out] value The value associated with the minimum priority.
    ! ---------------------------------------------------------------------------------------------------
    subroutine min_heap_pop(self, value, priority)
        implicit none
        class(MinHeap), intent(inout) :: self
        real(kind=custom_real), intent(out) :: priority
        integer(kind=custom_int), allocatable, intent(out) :: value(:)
        integer(kind=custom_int) :: last, alloc_stat
        type(MinHeapElement) :: temp_element

        if (self%count == 0) then
            print *, 'Error: MinHeap is empty!'
            return
        end if

        ! The minimum element is the root (the first element)
        priority = self%elements(1)%priority
        allocate (value(size(self%elements(1)%value)), stat=alloc_stat)
        if (alloc_stat /= 0) then
            print *, 'Error: Failed to allocate memory for extracted value!'
            return
        end if
        value = self%elements(1)%value

        if (self%count == 1) then
            self%count = 0
            return
        end if

        ! Swap the root with the last element
        last = self%count
        temp_element = self%elements(1)
        self%elements(1) = self%elements(last)
        self%elements(last) = temp_element

        ! Deallocate the value of the last element to prevent memory leaks
        deallocate (self%elements(last)%value)

        self%count = self%count - 1

        ! Bubble down to maintain heap property
        call min_heap_bubble_down(self, 1)
    end subroutine min_heap_pop

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Helper function to "bubble up" an element at a given index.
    ! @param[inout] self The min-heap.
    ! @param[in] start_i The starting index of the element to bubble up.
    ! ---------------------------------------------------------------------------------------------------
    subroutine min_heap_bubble_up(self, start_i)
        implicit none
        class(MinHeap), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: start_i
        integer(kind=custom_int) :: current_i, parent
        type(MinHeapElement) :: temp_element

        current_i = start_i
        parent = current_i/2

        ! Swap with parent until heap property is restored
        do while (current_i > 1 .and. self%elements(current_i)%priority < self%elements(parent)%priority)
            ! Swap elements
            temp_element = self%elements(current_i)
            self%elements(current_i) = self%elements(parent)
            self%elements(parent) = temp_element

            ! Update current and parent indices
            current_i = parent
            parent = current_i/2
        end do
    end subroutine min_heap_bubble_up

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Helper function to "bubble down" an element at a given index.
    ! @param[inout] self The min-heap.
    ! @param[in] start_i The starting index of the element to bubble down.
    ! ---------------------------------------------------------------------------------------------------
    subroutine min_heap_bubble_down(self, start_i)
        implicit none
        class(MinHeap), intent(inout) :: self
        integer(kind=custom_int), intent(in) :: start_i
        integer(kind=custom_int) :: current_i, left, right, smallest
        type(MinHeapElement) :: temp_element

        current_i = start_i

        do while (2*current_i <= self%count)
            left = 2*current_i
            right = left + 1
            smallest = current_i

            ! Find the smallest child
            if (self%elements(left)%priority < self%elements(smallest)%priority) then
                smallest = left
            end if
            if (right <= self%count .and. self%elements(right)%priority < self%elements(smallest)%priority) then
                smallest = right
            end if

            ! If the smallest child is smaller than the current node, swap
            if (smallest /= current_i) then
                ! Swap elements
                temp_element = self%elements(current_i)
                self%elements(current_i) = self%elements(smallest)
                self%elements(smallest) = temp_element

                ! Update current index to the smallest child
                current_i = smallest
            else
                exit
            end if
        end do
    end subroutine min_heap_bubble_down

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
                call meshf_report("Error: Failed to allocate temporary array in array_resize.", is_error=.true.)
                return
            end if
            temp_arr = arr
            deallocate (arr)
        end if

        ! Allocate new array with new size
        allocate (arr(new_size), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call meshf_report("Error: Failed to allocate new array in array_resize.", is_error=.true.)
            if (allocated(temp_arr)) then
                allocate (arr(size(temp_arr)), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call meshf_report("Error: Failed to reallocate original array in array_resize.", is_error=.true.)
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
                call meshf_report("Error: Failed to allocate temporary array in array2d_resize.", is_error=.true.)
                return
            end if
            temp_arr = arr(1:min_rows, 1:min_cols)
            deallocate (arr)
        end if

        ! Allocate new array with new dimensions
        allocate (arr(new_rows, new_cols), stat=alloc_stat)
        if (alloc_stat /= 0) then
            call meshf_report("Error: Failed to allocate new array in array2d_resize.", is_error=.true.)
            if (allocated(temp_arr)) then
                allocate (arr(size(temp_arr, dim=1), size(temp_arr, dim=2)), stat=alloc_stat)
                if (alloc_stat /= 0) then
                    call meshf_report("Error: Failed to reallocate original array in array2d_resize.", is_error=.true.)
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
    !         the rows or columns of the input 2D array  in-place along the specified dimension.
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
            call meshf_report("Error: Invalid dimension specified. Please use 1 or 2.", is_error=.true.)
            return
        end if

        ! Initialise random seed
        call random_seed()

        if (dim .eq. 1) then
            ! Shuffle along the first dimension (rows)
            allocate (temp_row(size(arr, dim=2)), stat=alloc_stat)
            if (alloc_stat .ne. 0) then
                call meshf_report("Error: Failed to allocate temp_row in permute_array2d.", is_error=.true.)
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
                call meshf_report("Error: Failed to allocate temp_col in permute_array2d.", is_error=.true.)
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
