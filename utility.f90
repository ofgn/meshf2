! -------------------------------------------------------------------------------------------------------
! @file utility.f90
! @brief Utility module for common functions and procedures.
! @author ofgn
! @date 2024-07-01
! -------------------------------------------------------------------------------------------------------
module utility
    use global

    implicit none
contains

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Log a message to stdout or stderr.
    ! @param[in] message The message to log.
    ! @param[in, optional] is_error Flag to log to stderr if present and true.
    ! ---------------------------------------------------------------------------------------------------
    subroutine report(message, is_error)
        use iso_fortran_env, only: output_unit, error_unit
        implicit none

        character(len=*), intent(in) :: message                         ! Message to log
        logical, intent(in), optional :: is_error                       ! Flag for logging to stderr (optional)

        if (present(is_error)) then
            if (is_error) then
                write (error_unit, "(a)") trim(message)                 ! Log to stderr
            else
                write (output_unit, "(a)") trim(message)                ! Log to stdout
            end if
        else
            write (output_unit, "(a)") trim(message)                    ! Default logging to stdout
        end if
    end subroutine report

    subroutine report2(message1, message2, is_error)
        use iso_fortran_env, only: output_unit, error_unit
        implicit none

        character(len=*), intent(in) :: message1, message2                      ! Message to log
        logical, intent(in), optional :: is_error                               ! Flag for logging to stderr (optional)
        integer(int32) :: hspace                                                ! Horizontal space
        
        if (len_trim(message1) > 32) then
            call report(message2, is_error)
        end if

        hspace = 32 - len_trim(message1)
        if (present(is_error)) then
            if (is_error) then
                write (error_unit, "(a)") trim(message1) // &                   ! Log to stderr            
                    repeat(' ', hspace) // &
                    trim(message2)                                         
            else
                write (output_unit, "(a)") trim(message1) // &                  ! Log to stdout
                    repeat(' ', hspace) // &
                    trim(message2)                                         
            end if
        else
            write (output_unit, "(a)") trim(message1) // &                  ! Log to stdout
                repeat(' ', hspace) // &
                trim(message2)                                         
        end if
    end subroutine report2

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Convert an integer to a character string.
    ! @param[in] value The integer value to convert.
    ! @return The character string representation of the integer.
    ! ---------------------------------------------------------------------------------------------------
    function itoa(value) result(str)
        implicit none
        integer, intent(in) :: value                          ! Integer value to be converted
        character(len=32) :: str                              ! Resulting string (maximum length of 32 characters)
    
        ! Convert the integer to a string
        write (str, "(I0)") value                             ! 'I0' ensures no leading spaces in the result
    end function itoa

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Convert a real number to a character string.
    ! @param[in] value The real value to convert.
    ! @param[in, optional] decimal_places Number of decimal places to include in the output (default is 2).
    ! @return The character string representation of the real number.
    ! ---------------------------------------------------------------------------------------------------
    function rtoa(value, decimal_places) result(str)
        implicit none
        real(real64), intent(in) :: value                           ! Real value to be converted
        integer, intent(in), optional :: decimal_places             ! Number of decimal places (optional, default is 2)
        character(len=64) :: str                                    ! Resulting string (maximum length of 64 characters)
        integer(int32) :: prec                                        ! Final decimal places for the format string

        ! Set default decimal places to 2 if not provided
        prec = 2
        if (present(decimal_places)) prec = decimal_places

        ! Ensure that the format string uses the specified number of decimal places
        write (str, "(F0.2)") value                          ! Default format with 2 decimal places
        if (prec > 0) then
            if (abs(value) .ge. 1.0d0) then
                write (str, "(F0." // trim(itoa(prec)) // ")") value
            else
                write (str, "(A, F0." // trim(itoa(prec)) // ")") "0", value
            end if
        end if
    end function rtoa

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Replace space characters from a string by replacing spaces with underscores.
    ! @param[in] input_string The input string to modify.
    ! @return The modified string with spaces replaced by underscores.
    ! ---------------------------------------------------------------------------------------------------
    function strip(input_string) result(output_string)
        implicit none
        character(len=*), intent(in) :: input_string                    ! Input string
        character(len=len(input_string)) :: output_string               ! Output string with spaces replaced

        integer(int32) :: i                                                    ! Loop index

        output_string = input_string                                    ! Initialise the output string

        do i = 1, len(output_string)
            if (output_string(i:i) .eq. " ") then
                output_string(i:i) = "_"                                ! Replace spaces with underscores
            end if
        end do
    end function strip

    function banner(input_string) result(output_string)
        implicit none
        character(len=*), intent(in) :: input_string      ! Input string
        character(len=97) :: output_string                ! Output string with newline
        integer(int16) :: length, a, b                    ! Length of the input string and padding
    
        length = len_trim(input_string)
    
        if (mod(24 - length - 2, 2) == 0) then
            a = (24 - length - 2) / 2
            b = a
        else
            a = (24 - length - 2) / 2
            b = a + 1
        end if
    
        output_string = repeat('-', 28) // &
        '[' // repeat(' ', a) // trim(input_string) // repeat(' ', b) // ']' &
        // repeat('-', 28) // lf
    
    end function banner
    
end module utility