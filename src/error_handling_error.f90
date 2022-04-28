! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT
module error_handling_error
    use error_handling_fail_reason, only: fail_reason_t, fail_reason_ctr_t
    use error_handling_hook, only: error_handler_t, error_hook_t
    implicit none

    public error_t


    type :: error_t
        !> Error type used to indicate the sucsess or failure of a procedure. Use for
        !! example as an dummy argument in a subroutine:
        !!     type(error_t), allocatable, intent(out) :: error
        !! Note that for pure and elemental subroutines the Fortran standard requires
        !! the intent to be inout:
        !!     type(error_t), allocatable, intent(inout) :: error
        !! When an error occurs, the argument can then be assigned to an error:
        !!     error = error_t('Some description of error here')
        !! In the calling procedure, check for any error with
        !!     if (allocated(error)) !...
        !! afterwards.

        !> The fail_reason_t which the error was created with
        class(fail_reason_t), allocatable :: root_cause
        !> Array of subsequent fail_reason_t for the error, ending with the reason
        !! from the most recent call to with_cause
        type(fail_reason_ctr_t), allocatable :: chain(:)
        !> Handler for this error
        class(error_handler_t), allocatable :: handler
    contains
        generic :: with_cause => with_cause_message, with_cause_reason
        procedure :: display

        procedure, private :: with_cause_message
        procedure, private :: with_cause_reason
    end type

    interface error_t
        module procedure new_from_message
        module procedure new_from_reason
    end interface


    interface
        !> Create an error_t type with a message describing the reason for the error
        pure module function new_from_message(message) result(this)
            character(len=*), intent(in) :: message
            type(error_t) :: this
        end function


        !> Create an error_t type with a custom fail_reason_t
        pure module function new_from_reason(reason) result(this)
            class(fail_reason_t), intent(in) :: reason
            type(error_t) :: this
        end function


        !> Provide contextual information about the error in the form of text
        pure module subroutine with_cause_message(this, message)
            class(error_t), intent(inout) :: this
            !> Descriptive text
            character(len=*), intent(in) :: message
        end subroutine


        !> Provide contextual information about the error in the form of text
        pure module subroutine with_cause_reason(this, reason)
            class(error_t), intent(inout) :: this
            !> Reason for failure
            class(fail_reason_t), intent(in) :: reason
        end subroutine


        !> Return a character string with a human readable description of the error using
        !! the default error formatter
        pure module function display(this) result(chars)
            class(error_t), intent(in) :: this
            character(len=:), allocatable :: chars
        end function


        !> Install an error_hook_t for custom error handling
        module subroutine set_error_hook(hook)
            class(error_hook_t), intent(in) :: hook
        end subroutine
    end interface

end module