! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT

submodule(error_handling) error_handling_impl
    use error_mod, only: error_t
    implicit none

    type, extends(error_t) :: message_error_t
        character(len=:), allocatable :: message
    contains
        procedure :: to_chars => message_to_chars
    end type


    type, extends(error_handler_t) :: default_handler_t
    contains
        procedure :: format_error => default_format_error
    end type


    class(error_hook_t), allocatable :: global_hook


contains


    module subroutine set_error_hook(hook)
        class(error_hook_t), intent(in) :: hook

        global_hook = hook
    end subroutine


    module subroutine remove_error_hook()
        if (allocated(global_hook)) then
            deallocate(global_hook)
        end if
    end subroutine

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! IMPORTANT:
    ! Care is taken here to make sure that all code paths from the
    ! public API (fail and wrap_error procedures) down to make_error_report has
    ! the same number of stack frames. This is crucial for being able to generate
    ! reliable stack traces from custom error handlers.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    pure module function fail_error(cause) result(error)
        class(error_t), intent(in) :: cause
        type(error_report_t) :: error

        ! If incoming error is an error_report_t just return it, otherwise
        ! wrap it inside one
        select type (cause)
            class is (error_report_t)
                error = cause
            class default
                error = do_fail(cause)
        end select
    end function


    pure module function fail_message(message) result(error)
        character(len=*), intent(in) :: message
        type(error_report_t) :: error

        type(message_error_t) :: the_error

        the_error%message = message
        error = do_fail(the_error)
        ! error = do_fail(message_error_t(message))
    end function


    pure module subroutine wrap_error_message(error, message)
        class(error_t), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: message

        call do_wrap_error(error, message_error_t(message))
    end subroutine


    pure module subroutine wrap_error_error(error, other_error)
        class(error_t), allocatable, intent(inout) :: error
        class(error_t), intent(in) :: other_error

        call do_wrap_error(error, other_error)
    end subroutine


    pure function do_fail(error) result(report)
        class(error_t), intent(in) :: error
        type(error_report_t) :: report


        report = make_error_report(error)
    end function


    pure subroutine do_wrap_error(error, other_error)
        class(error_t), allocatable, intent(inout) :: error
        class(error_t), intent(in) :: other_error

        logical :: handled

        if (.not. allocated(error)) return

        handled = .false.
        select type (error)
            type is (error_report_t)
                call push(error%chain, other_error)
                handled = .true.
        end select
        if (.not. handled) then
            ! Intel has problems manipulating `error` in a `class default` statement,
            ! hence we keep track of this information through the `handled` variable.
            error = make_error_report(error)
        end if
    end subroutine


    pure function make_error_report(error) result(report)
        class(error_t), intent(in) :: error
        type(error_report_t) :: report

        call push(report%chain, error)
        if (allocated(global_hook)) then
            report%handler = global_hook%create_handler(error)
        else
            report%handler = default_handler_t()
        end if
    end function


    pure subroutine push(chain, error)
        type(error_chain_t), allocatable, intent(inout) :: chain
        class(error_t), intent(in) :: error

        type(error_chain_t), allocatable :: tmp

        if (.not. allocated(chain)) then
            allocate(chain)
            chain%error = error
        else
            call move_alloc(chain, tmp)
            allocate(chain)
            chain%error = error
#ifndef __NVCOMPILER
            call move_alloc(tmp, chain%cause)
#endif
        end if
    end subroutine


    pure module function report_to_chars(this) result(chars)
        class(error_report_t), intent(in) :: this
        character(len=:), allocatable :: chars

        if (.not. allocated(this%chain)) then
            ! This will not happen unless sombody is using an uninitialized type
            chars = '<UNKNOWN ERROR>'
            return
        end if
        if (.not. allocated(this%chain%error)) then
            ! This will not happen unless sombody is manipulating the type themselves
            ! (which they should not...)
            chars = '<UNKNOWN ERROR>'
            return
        end if
        if (.not. allocated(this%handler)) then
            ! This will not happen unless a custom error hook is misbehaving
            block
                type(default_handler_t) :: handler
                chars = handler%format_error(this%chain)
            end block
        else
            chars = this%handler%format_error(this%chain)
        end if
    end function


    pure function default_format_error(this, chain) result(chars)
        class(default_handler_t), intent(in) :: this
        type(error_chain_t), intent(in) :: chain
        character(len=:), allocatable :: chars

        ! Just to avoid unused variable warning (it is intentional)
        associate(dummy => this)
        end associate

        chars = chain%error%to_chars()
#ifndef __NVCOMPILER
        if (allocated(chain%cause)) then
            chars = chars // new_line('c') &
                // new_line('c') &
                // 'Caused by:' // new_line('c') &
                // chain_to_chars(chain%cause)
        end if
#endif
    end function


    pure recursive function chain_to_chars(chain) result(chars)
        type(error_chain_t), intent(in) :: chain
        character(len=:), allocatable :: chars

        chars = '  - ' // indent_newlines(chain%error%to_chars(), 4)
#ifndef __NVCOMPILER
        if (.not. allocated(chain%cause)) then
            return
        end if
        chars = chars // new_line('c') &
            // chain_to_chars(chain%cause)
#endif
    end function


    recursive pure function indent_newlines(chars, n) result(indented)
        character(len=*), intent(in) :: chars
        integer, intent(in) :: n
        character(len=:), allocatable :: indented

        integer :: idx

        idx = index(chars, new_line('c'))
        if (idx <= 0) then
            indented = chars
        else
            indented = chars(1:idx)    &
                // repeat(' ', n) // indent_newlines(chars(idx + 1:), n)
        end if
    end function


    pure function message_to_chars(this) result(chars)
        class(message_error_t), intent(in) :: this
        character(len=:), allocatable :: chars

        if (allocated(this%message)) then
            chars = this%message
        else
            chars = ''
        end if
    end function

end submodule