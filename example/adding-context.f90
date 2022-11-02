module processing_mod
    use error_handling, only: error_t, wrap_error, fail
    implicit none

contains

    pure subroutine process_array(arr, res, error)
        integer, intent(inout) :: arr(:)
        integer, intent(out) :: res
        class(error_t), allocatable, intent(inout) :: error

        integer :: i
        character(len=20) :: i_value

        ! Here we use a labelled block to separate multiple fallible procedure calls
        ! from the code that handles any errors
        res = 0
        fallible: block
            do i = 1, size(arr)
                call check_and_accumulate(arr(i), res, error)
                if (allocated(error)) exit fallible
            end do
            ! Return for subroutine on success, code below is only for
            ! error handling so no allocated(error) check is needed there.
            return
        end block fallible
        ! Provide some context with error
        write(i_value, *) i
        call wrap_error(error, 'Processing of array failed at element '    &
            // trim(adjustl(i_value)))
    end subroutine


    pure subroutine check_and_accumulate(i, res, error)
        integer, intent(in) :: i
        integer, intent(inout) :: res
        class(error_t), allocatable, intent(inout) :: error

        if (res > 50) then
            error = fail('Magic limit reached')
            return
        end if
        res = res + i
    end subroutine

end module


program basic_example
    use error_handling, only: error_t, wrap_error
    use processing_mod, only: process_array
    implicit none

    integer :: res
    integer, allocatable :: arr(:)
    class(error_t), allocatable :: error

    arr = [1, 2, 3, 5, 8, 12, 11, 20, 5, 2, 4, 6]
    call process_array(arr, res, error)
    if (allocated(error)) then
        call wrap_error(error, 'Example failed (but that was the intent...)')
        write(*,'(a,a)') 'Error: ', error%to_chars()
    else
        write(*,*)  'Got back: ', res
    end if
end program