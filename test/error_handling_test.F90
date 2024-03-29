module error_handling_test
    use error_handling, only: &
        error_t, &
        fail, &
        wrap_error, &
        error_report_t, &
        error_chain_t, &
        error_stop, &
        error_handler_t, &
        error_hook_t, &
        set_error_hook, &
        remove_error_hook

    implicit none
    private
    public :: test_error_handling


    type, extends(error_t) :: my_error_t
    contains
        procedure :: to_chars => my_error_to_chars
    end type


    type, extends(error_hook_t) :: custom_error_hook_t
    contains
        procedure :: create_handler
    end type


    character(len=29), parameter :: CUSTOM_MESSAGE = 'Formatted with custom handler'

    type, extends(error_handler_t) :: custom_error_handler_t
    contains
        procedure :: format_error
    end type

contains


    subroutine test_error_handling()
        write(*,*) 'test_error_handling...'
        call pure_sub_with_error_works
        call impure_sub_with_error_works
#       ifndef __GFORTRAN__
            ! gfortran 13.2.0 segfaults on this. TODO: Report in gcc bug tracker
            call pure_func_with_error_works
#       endif
        call fail_and_wrap_error_should_produce_error_report
        call wrap_error_should_be_visible_in_output
        call uninitialized_error_report_should_not_carsh
        call fail_on_error_report_should_not_add_new_layer
        call fail_should_use_custom_handler
#       ifndef __GFORTRAN__
            ! gfortran 13.2.0 give
            !     malloc(): unaligned tcache chunk detected
            ! on this test. Code bug? Compiler bug?
            call wrap_error_should_use_cusom_handler
#       endif
        write(*,*) 'test_error_handling [Ok]'
    end subroutine


    subroutine pure_sub_with_error_works
        class(error_t), allocatable :: error

        call pure_sub_with_error(error)
        if (.not. allocated(error)) then
            call error_stop('expected error')
        end if
    end subroutine


    subroutine impure_sub_with_error_works
        class(error_t), allocatable :: error

        call impure_sub_with_error(error)
        if (.not. allocated(error)) then
            call error_stop('expected error')
        end if
    end subroutine


    subroutine pure_func_with_error_works
        class(error_t), allocatable :: error

        error = pure_func_with_error()
        if (.not. allocated(error)) then
            call error_stop('expected error')
        end if
    end subroutine


    subroutine fail_and_wrap_error_should_produce_error_report
        class(error_t), allocatable :: error
        type(error_report_t) :: report

        error = fail('foo')
        if (.not. same_type_as(error, report)) call error_stop('Unexpected error type')
        error = fail(my_error_t())
        if (.not. same_type_as(error, report)) call error_stop('Unexpected error type')
        error = my_error_t()
        call wrap_error(error, 'bar')
        if (.not. same_type_as(error, report)) call error_stop('Unexpected error type')
        error = fail('foo')
        call wrap_error(error, 'bar')
        if (.not. same_type_as(error, report)) call error_stop('Unexpected error type')
    end subroutine


    subroutine wrap_error_should_be_visible_in_output
        class(error_t), allocatable :: error
        character(len=:), allocatable :: chars
        integer :: i1, i2, i3, i4

        error = fail('This failed')
        call wrap_error(error, my_error_t())
        call wrap_error(error, 'here are some context' // new_line('c') &
                // 'that spans' // new_line('c') &
                // 'multiple' // new_line('c') &
                // 'lines')
        call wrap_error(error, 'This is the final context which will become the' // new_line('c') &
                // '"title" of the displayed error')

        chars = error%to_chars()
        i1 = index(chars, 'This is the final context')
        i2 = index(chars, '  - here are some context')
        i3 = index(chars, '  - my_error_t')
        i4 = index(chars, '  - This failed')
        ! Library does not give sensible output when compiled with NVidia Fortran yet
#ifndef __NVCOMPILER
        if (any([i1, i2, i3, i4] == 0)) then
            write(*,*) i1, i2, i3, i4
            call error_stop('Expected context in error: ' // new_line('c') // chars)
        end if
        if (.not. (i1 < i2 .and. i2 < i3 .and. i3 < i4)) then
            call error_stop('Unexpected order in error: ' // new_line('c') // chars)
        end if
#endif
    end subroutine


    subroutine uninitialized_error_report_should_not_carsh
        type(error_report_t) :: error
        character(len=:), allocatable :: chars

        chars = error%to_chars()
        if (chars /= '<UNKNOWN ERROR>') call error_stop('Unexpected chars: ' // chars)
        ! Partly initialized
        allocate(error%chain)
        chars = error%to_chars()
        if (chars /= '<UNKNOWN ERROR>') call error_stop('Unexpected chars: ' // chars)
    end subroutine


    subroutine fail_on_error_report_should_not_add_new_layer
        class(error_t), allocatable :: error1, error2

        error1 = fail('foo')
        error2 = fail(error1)
        if (error1%to_chars() /= error2%to_chars()) call error_stop('Not expected')
    end subroutine


    subroutine fail_should_use_custom_handler
        class(error_t), allocatable :: error

        call set_error_hook(custom_error_hook_t())
        error = fail('foo')
        select type(error)
            class is (error_report_t)
                if (.not. same_type_as(error%handler, custom_error_handler_t())) error stop 'Unexpected handler'
            class default
                error stop 'Unexpected error'
        end select

        call remove_error_hook()
        error = fail('bar')
        select type(error)
            class is (error_report_t)
                if (same_type_as(error%handler, custom_error_handler_t())) error stop 'Unexpected handler'
            class default
                error stop 'Unexpected error'
        end select
    end subroutine


    subroutine wrap_error_should_use_cusom_handler
        class(error_t), allocatable :: error

        call set_error_hook(custom_error_hook_t())
        error = my_error_t()
        call wrap_error(error, 'foo')
        select type(error)
            class is (error_report_t)
                if (.not. same_type_as(error%handler, custom_error_handler_t())) error stop 'Unexpected handler'
            class default
                error stop 'Unexpected error'
        end select
        call remove_error_hook()
    end subroutine

    pure function create_handler(this, error) result(handler)
        class(custom_error_hook_t), intent(in) :: this
        class(error_t), intent(in) :: error
        class(error_handler_t), allocatable :: handler

        handler = custom_error_handler_t()
    end function


    pure function format_error(this, chain) result(chars)
        class(custom_error_handler_t), intent(in) :: this
        type(error_chain_t), intent(in) :: chain
        character(len=:), allocatable :: chars

        chars = 'Custom error handler'
    end function


    pure function pure_func_with_error() result(error)
        class(error_t), allocatable :: error

        error = fail('func failed')
    end function


    pure subroutine pure_sub_with_error(error)
        class(error_t), allocatable, intent(inout) :: error

        error = fail('pure sub failed')
    end subroutine


    subroutine impure_sub_with_error(error)
        class(error_t), allocatable, intent(out) :: error

        error = fail('impure sub failed')
    end subroutine


    pure function my_error_to_chars(this) result(chars)
        class(my_error_t), intent(in) :: this
        character(len=:), allocatable :: chars

        chars = 'my_error_t'
        associate(dummy => this); end associate
    end function

end module
