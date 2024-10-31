! -------------------------------------------------------------------------------------------------------------------------------------------------------------
! @file utility.f90
! @brief Utility module for common functions and procedures.
! @author ofgn
! @date 2024-07-01
! -------------------------------------------------------------------------------------------------------------------------------------------------------------
module utility
    use global

    implicit none

    integer, parameter :: max_title_width = 28

contains

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Log a title to stdout.
    ! @param[in] title The title to log.
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine meshf_title(title)
        use iso_fortran_env, only: output_unit

        implicit none

        character(len=*), intent(in) :: title                                   ! Title for the output  
        integer(kind=int16) :: title_length                                     ! Length of the input string
        integer(kind=int16) :: left_padding, right_padding                      ! Padding for the title              
        character(len=255) :: output                                             ! Output string with newline

        title_length = len_trim(title)
        if (mod(max_title_width - title_length - 2, 2) == 0) then
            left_padding = (max_title_width - title_length - 2) / 2
            right_padding = left_padding
        else
            left_padding = (max_title_width - title_length - 2) / 2
            right_padding = left_padding + 1
        end if
    
        output = repeat('-', (80 - (max_title_width + 2)) / 2) &
        // '[' // repeat(' ', left_padding) // trim(title) &
        // repeat(' ', right_padding) // ']' &
        // repeat('-', (80 - (max_title_width + 2)) / 2)

        write (output_unit, "(a)") lf // trim(output) // lf
    end subroutine meshf_title

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Log a message to stdout.
    ! @param[in] label The label for the message.
    ! @param[in] status The body of the message.
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine meshf_status(label, status)
        use iso_fortran_env, only: output_unit

        implicit none

        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: status
        integer(kind=int16) :: hspace
        character(len=255) :: output

        hspace = 16 - len_trim(label)
        output = "- " // label // ": " // repeat(' ', hspace) // status
        write (output_unit, "(a)") trim(output)
    end subroutine meshf_status

    subroutine meshf_now()
        use iso_fortran_env, only: output_unit

        implicit none

        integer :: date(8)
        character(len=23) :: timestamp
        character(len=255) :: output

        call date_and_time(values=date)
        write(timestamp, '(I4.4, "-", I2.2, "-", I2.2, " ", I2.2, ":", I2.2, ":", I2.2, ".", I3.3)') &
            date(1), date(2), date(3), date(5), date(6), date(7), date(8)

        output = "- Started: " // repeat(' ', 9) // trim(timestamp)
        write (output_unit, "(a)") trim(output)
    end subroutine meshf_now

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Log the time taken to complete a task.
    ! @param[in] time The time taken to complete the task.
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine meshf_runtime(time)
        use iso_fortran_env, only: output_unit

        implicit none

        real(kind=real64), intent(in) :: time
        integer(kind=int16) :: hspace
        character(len=255) :: output
        
        hspace = 9
        output = "- Runtime: " // repeat(' ', hspace) &
            // trim(rtoa(time, 4)) // "s"
        write (output_unit, "(a)") trim(output)
    end subroutine meshf_runtime

    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    ! @brief Log an error message to stderr.
    ! @param[in] error The error message to log.
    ! ---------------------------------------------------------------------------------------------------------------------------------------------------------
    subroutine meshf_error(error)
        use iso_fortran_env, only: error_unit

        implicit none

        character(len=*), intent(in) :: error
        integer(kind=int16) :: hspace
        character(len=255) :: output

        hspace = 11
        output = "- ERROR: " // repeat(' ', hspace) // error
        write (error_unit, "(a)") trim(output)
    end subroutine meshf_error

    ! ---------------------------------------------------------------------------------------------------
    ! @brief Log a message to stdout or stderr.
    ! @param[in] message The message to log.
    ! @param[in, optional] is_error Flag to log to stderr if present and true.
    ! ---------------------------------------------------------------------------------------------------
    subroutine meshf_report(message, is_error)
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
    end subroutine meshf_report

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