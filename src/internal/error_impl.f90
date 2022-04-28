! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT
submodule(error_handling_error) error_impl
    use iso_fortran_env, only: error_unit
    implicit none


    ! Globally installed error hook
    class(error_hook_t), allocatable :: installed_hook


    ! Type for storing ad-hoc messages as fail_reason_t
    type, extends(fail_reason_t) :: message_fail_reason_t
        character(len=:), allocatable :: message
    contains
        procedure :: describe
    end type


    type, extends(error_hook_t) :: default_error_hook_t
    contains
        procedure :: create_handler
    end type


    type, extends(error_handler_t) :: default_error_handler_t
    contains
        procedure :: display => display_default
    end type

contains


    module subroutine set_error_hook(hook)
        class(error_hook_t), intent(in) :: hook

        installed_hook = hook
    end subroutine


    ! IMPORTANT: For reliable stacktrace generation the number of procedure invokations
    !            from error_t initializer to hook%create_handler must be the same
    !            for all code paths!
    pure function new_error(reason) result(this)
        class(fail_reason_t), intent(in) :: reason
        type(error_t) :: this

        if (allocated(installed_hook)) then
            call installed_hook%create_handler(reason, this%handler)
        else
            block
                type(default_error_hook_t):: hook

                hook = default_error_hook_t()
                call hook%create_handler(reason, this%handler)
            end block
        end if
        allocate(this%chain(0))
        this%root_cause = reason
    end function


    pure module function new_from_message(message) result(this)
        character(len=*), intent(in) :: message
        type(error_t) :: this

        this = new_error(message_fail_reason_t(message))
    end function


    pure module function new_from_reason(reason) result(this)
        class(fail_reason_t), intent(in) :: reason
        type(error_t) :: this

        this = new_error(reason)
    end function


    pure module subroutine with_cause_message(this, message)
        class(error_t), intent(inout) :: this
        character(len=*), intent(in) :: message

        call this%with_cause(message_fail_reason_t(message))
    end subroutine


    pure module subroutine with_cause_reason(this, reason)
        class(error_t), intent(inout) :: this
        class(fail_reason_t), intent(in) :: reason

        integer :: cap
        integer :: i
        type(fail_reason_ctr_t), allocatable :: tmp(:)

        cap = size(this%chain)
        ! call move_alloc(this%chain, tmp)
        ! This is either premature optimization or slightly more efficient than
        ! move_alloc on the array since items don't have to be copied.
        allocate(tmp(cap))
        do i = 1, cap
            call move_alloc(this%chain(i)%reason, tmp(i)%reason)
        end do
        ! No growth factor, just increment the size each time. The advantage
        ! of this is that the array always have the correct size
        deallocate(this%chain)
        allocate(this%chain(cap + 1))
        do i = 1, cap
            call move_alloc(tmp(i)%reason, this%chain(i)%reason)
        end do
        this%chain(cap + 1)%reason = reason
    end subroutine


    pure module function display(this) result(chars)
        class(error_t), intent(in) :: this
        character(len=:), allocatable :: chars

        chars = this%handler%display(this%root_cause, this%chain)
    end function


    !
    ! message_fail_reason_t procedures
    !
    pure function describe(this)
        class(message_fail_reason_t), intent(in) :: this
        character(len=:), allocatable :: describe

        describe = this%message
    end function


    !
    ! default_hook_t procedures
    !
    pure subroutine create_handler(this, root_cause, handler)
        class(default_error_hook_t), intent(in) :: this
        class(fail_reason_t), intent(in) :: root_cause
        class(error_handler_t), allocatable, intent(inout) :: handler

        handler = default_error_handler_t()
        ! Avoid unused argument warnings
        associate(dummy => root_cause); end associate
        associate(dummy => this); end associate
    end subroutine


    !
    ! default_handler_t procedures
    !
    pure function display_default(this, root_cause, chain) result(chars)
        class(default_error_handler_t), intent(in) :: this
        class(fail_reason_t), intent(in) :: root_cause
        type(fail_reason_ctr_t), intent(in) :: chain(:)
        character(len=:), allocatable :: chars

        integer :: idx
        integer :: n

        n = size(chain)
        if (n == 0) then
            chars = 'Error: ' // indent_newlines(root_cause%describe(), 7)
        else
            chars = 'Error: ' // indent_newlines(chain(n)%reason%describe(), 7)
            chars = chars // new_line('c') &
                // 'Caused by: '
            do idx = n - 1, 1, -1
                chars = chars // new_line('c') &
                    // '  - ' // indent_newlines(chain(idx)%reason%describe(), 4)
            end do
                chars = chars // new_line('c') &
                    // '  - ' // indent_newlines(root_cause%describe(), 4)
        end if
        ! Avoid unused argument warning
        associate(dummy => this); end associate
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

end submodule