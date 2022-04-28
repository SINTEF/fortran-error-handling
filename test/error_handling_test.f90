module error_handling_test
    use error_handling, only: &
        error_t, &
        fail_reason_t, &
        error_stop

    implicit none
    private
    public :: test_error_handling


    type, extends(fail_reason_t) :: my_fail_reason_t
    contains
        procedure :: describe
    end type

contains


    subroutine test_error_handling()
        write(*,*) 'test_error_handling...'
        call pure_sub_with_error_works
        call impure_sub_with_error_works
        call pure_func_with_error_works
        call with_cause_should_be_visible_in_output
        call cause_check_should_match
        call include_stacktrace_false_should_only_output_error
        write(*,*) 'test_error_handling [Ok]'
    end subroutine


    subroutine pure_sub_with_error_works
        type(error_t), allocatable :: error

        call pure_sub_with_error(error)
        if (.not. allocated(error)) then
            call error_stop('expected error')
        end if
    end subroutine


    subroutine impure_sub_with_error_works
        type(error_t), allocatable :: error

        call impure_sub_with_error(error)
        if (.not. allocated(error)) then
            call error_stop('expected error')
        end if
    end subroutine


    subroutine pure_func_with_error_works
        type(error_t), allocatable :: error

        error = pure_func_with_error()
        if (.not. allocated(error)) then
            call error_stop('expected error')
        end if
    end subroutine


    subroutine with_cause_should_be_visible_in_output
        type(error_t), allocatable :: error
        character(len=:), allocatable :: chars
        integer :: i1, i2, i3, i4

        error = error_t('This failed')
        call error%with_cause(my_fail_reason_t())
        call error%with_cause('here are some context' // new_line('c') &
                // 'that spans' // new_line('c') &
                // 'multiple' // new_line('c') &
                // 'lines')
        call error%with_cause('This is the final context which will become the' // new_line('c') &
                // '"title" of the displayed error')

        chars = error%display()
        i1 = index(chars, 'Error: This is the final context')
        i2 = index(chars, '  - here are some context')
        i3 = index(chars, '  - my_fail_reason_t')
        i4 = index(chars, '  - This failed')
        if (any([i1, i2, i3, i4] == 0)) then
            call error_stop('Expected context in error: ' // new_line('c') // chars)
        end if
        if (.not. (i1 < i2 .and. i2 < i3 .and. i3 < i4)) then
            call error_stop('Unexpected order in error: ' // new_line('c') // chars)
        end if
    end subroutine


    subroutine include_stacktrace_false_should_only_output_error
        type(error_t), allocatable :: error
        character(len=:), allocatable :: chars

        error = error_t('message')

        chars = error%display()

        if (index(chars, 'Error: message') /= 1) then
            call error_stop('Unexpected output: "' // chars // '"')
        end if
    end subroutine


    subroutine cause_check_should_match
        type(error_t), allocatable :: error
        logical :: matched

        error = error_t(my_fail_reason_t())

        matched = .false.
        select type (reason => error%root_cause)
            type is (my_fail_reason_t)
                matched = .true.
        end select
        if (.not. matched) call error_stop('Expected to match for cause')
    end subroutine


    pure function pure_func_with_error() result(error)
        type(error_t), allocatable :: error

        error = error_t('func failed')
    end function


    pure subroutine pure_sub_with_error(error)
        type(error_t), allocatable, intent(inout) :: error

        error = error_t('pure sub failed')
    end subroutine


    subroutine impure_sub_with_error(error)
        type(error_t), allocatable, intent(out) :: error

        error = error_t('impure sub failed')
    end subroutine


    pure function describe(this)
        class(my_fail_reason_t), intent(in) :: this
        character(len=:), allocatable :: describe

        describe = 'my_fail_reason_t'
        associate(dummy => this); end associate
    end function

end module
