module bounded_mod
    use error_handling, only: error_t
    implicit none

contains

    pure subroutine add_bounded(i, j, error)
        integer, intent(inout) :: i
        integer, intent(in) :: j
        type(error_t), allocatable, intent(inout) :: error

        if (i > 25) then
            error = error_t('i is too large')
            return
        end if
        i = i + j
    end subroutine


    pure subroutine multiply_bounded(i, j, error)
        integer, intent(inout) :: i
        integer, intent(in) :: j
        type(error_t), allocatable, intent(inout) :: error

        if (i > 25) then
            error = error_t('i is too large')
            return
        end if
        i = i * j
    end subroutine

end module


module some_mod
    use bounded_mod, only: add_bounded, multiply_bounded
    use error_handling, only: error_t
    implicit none

contains

    pure subroutine do_something(i, error)
        integer, intent(inout) :: i
        type(error_t), allocatable, intent(inout) :: error

        integer :: j
        character(len=20) :: i_value, j_value

        ! Here we are using a block to separate multiple fallible procedure calls
        ! from the code that handles any error
        fallible: block
            do j = 1, 5
                call add_bounded(i, j + 2, error)
                if (allocated(error)) exit fallible
                call multiply_bounded(i, j, error)
                if (allocated(error)) exit fallible
            end do
            ! Return for subroutine on success, code below is only for
            ! error handling so no allocated(error) check is needed there.
            return
        end block fallible
        ! Provide some context with error
        write(i_value, *) i
        write(j_value, *) j
        call error%with_cause('Could not do some thing with i = '    &
            // trim(adjustl(i_value)) // ' and j = ' // trim(adjustl(j_value)))
    end subroutine
end module


program basic_example
    use error_handling, only: error_t
    use some_mod, only: do_something
    implicit none
    integer :: i
    type(error_t), allocatable :: error

    i = 10
    call do_something(i, error)
    if (allocated(error)) then
        call error%with_cause('Example failed (but that was the intent...)')
        write(*,'(a)') error%display()
    else
        write(*,*)  'Got back: ', i
    end if
end program