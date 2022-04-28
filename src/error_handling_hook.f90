! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT
module error_handling_hook
    use error_handling_fail_reason, only: fail_reason_t, fail_reason_ctr_t
    implicit none

    private
    public error_handler_t
    public error_hook_t


    type, abstract :: error_handler_t
        !> An error_handler_t can be used to extend or customize error handling
        !! for an application
    contains
        procedure(display_i), deferred :: display
    end type


    abstract interface
        pure function display_i(this, root_cause, chain) result(chars)
            import error_handler_t, fail_reason_ctr_t, fail_reason_t
            class(error_handler_t), intent(in) :: this
            class(fail_reason_t), intent(in) :: root_cause
            type(fail_reason_ctr_t), intent(in) :: chain(:)
            character(len=:), allocatable :: chars
        end function
    end interface


    type, abstract :: error_hook_t
    contains
        procedure(create_handler_i), deferred :: create_handler
    end type


    abstract interface
        pure subroutine create_handler_i(this, root_cause, handler)
            import error_hook_t, error_handler_t, fail_reason_t
            class(error_hook_t), intent(in) :: this
            class(fail_reason_t), intent(in) :: root_cause
            class(error_handler_t), allocatable, intent(inout) :: handler
        end subroutine
    end interface

end module