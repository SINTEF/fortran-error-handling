! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT

module error_handling
    use error_mod, only: error_t
    implicit none

    private
    ! Re-export of base error_t type for convenience
    public error_t
    ! fail is used to generate an error report when something went wrong
    public fail
    ! wrap_error wraps an exsiting error with additional information about the
    ! cause of the error
    public wrap_error
    ! error_stop stops the execution imediately with an error report as message
    public error_stop
    ! error_report_t is the type created by `fail` and `wrap_error`. It is usually
    ! not neccesary to create variables of this type instead of a class(error_t).
    public error_report_t
    ! error_chain_t describes a chain of causalities down to a root cause error_t
    public error_chain_t
    ! error_handler_t can be extended to customize error handling from this library
    public error_handler_t
    ! error_hook_t is used to create custom error_handler_t objects
    public error_hook_t
    ! set_error_hook sets the globally used error_hook_t
    public set_error_hook
    ! remove_error_hook removes any global error_hook_t
    public remove_error_hook


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Types used for error reporting by this module
    !
    ! For conventional use, it will usually not be neccesary to use these
    ! types. They are however made public to enable accessing its data, e.g.
    ! if the regular `to_chars` function on `error_t` does not provide sufficient
    ! flexibility when presenting the error to the user.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    !> The error type that `fail` and `wrap_error` produce. It has an
    !! error_handler_t which could be a custom type provided by the
    !! `set_error_hook` subroutine which typically will be used to store a
    !! stacktrace.
    type, extends(error_t) :: error_report_t
        !> Handler for this error report
        class(error_handler_t), allocatable :: handler
        !> error_chain_t or errors descriing the cause. Last error in chain is
        !! the first error the report was created with.
        type(error_chain_t), allocatable :: chain
    contains
        procedure :: to_chars => report_to_chars
    end type


    interface
        !> Returns a human readable description of the error
        pure module function report_to_chars(this) result(chars)
            class(error_report_t), intent(in) :: this
            character(len=:), allocatable :: chars
        end function
    end interface


    !> An error_chain_t contains an error and the cause of this error. The
    !! cause is itself an error_chain_t so the type can be used to recursively
    !! describe a chain of context down to the root cause.
    type :: error_chain_t
        !> The error
        class(error_t), allocatable :: error
        !> Cause of the error
        type(error_chain_t), allocatable :: cause
    end type


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Main API
    !
    ! The `fail`, `wrap_error` and `error_stop` procedures are the main API of
    ! this module and those most users will regularly use.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    interface fail
        !> Generate an error report with the given message as root cause.
        !! Use this to create general application errors which don't need to be
        !! identified programatically.
        pure module function fail_message(message) result(error)
            character(len=*), intent(in) :: message
            type(error_report_t) :: error
        end function


        !> Generate an error report with the given error as root cause.
        !! Use this to create general application errors which don't need to be
        !! identified programatically.
        pure module function fail_error(cause) result(error)
            class(error_t), intent(in) :: cause
            type(error_report_t) :: error
        end function
    end interface


    interface wrap_error
        !> Wrap an existing error with a message providing more context about
        !! the cause of the error.
        pure module subroutine wrap_error_message(error, message)
            class(error_t), allocatable, intent(inout) :: error
            character(len=*), intent(in) :: message
        end subroutine


        !> Wrap an existing error with another `error_t` providing more context
        !! about the cause of the error.
        pure module subroutine wrap_error_error(error, other_error)
            class(error_t), allocatable, intent(inout) :: error
            class(error_t), intent(in) :: other_error
        end subroutine
    end interface


    interface error_stop
        !> Error stop execution of the application with an `error_report_t`
        !! created with `message`.
        pure module subroutine error_stop_message(message)
            !> Message to stop execution with
            character(len=*), intent(in) :: message
        end subroutine


        !> Error stop execution of the application with an error_t
        pure module subroutine error_stop_error(error)
            !> Error to stop execution with
            class(error_t), intent(in) :: error
        end subroutine
    end interface


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Customization API
    !
    ! The types and procedures below are provided to enable customization of
    ! this modules behaviour. Typically, it could be used to generate and attach
    ! a stacktrace to an `error_report_t` created by `fail`, `wrap_error`, or
    ! `error_stop`.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    !> An error_handler_t can be used to extend or customize error handling
    !! for an application. When an `error_report_t` is created, a custom
    !! `error_hook_t` (see below) is asked to create a handler.
    !! This means that a `error_handler_t` could load and keep e.g. a
    !! stacktrace to know where the error ocurred.
    type, abstract :: error_handler_t
    contains
        procedure(format_error_i), deferred :: format_error
    end type


    abstract interface
        !> Generate a character representation of the error which this handler is
        !! attached to, optionally using additional data from the handler itself.
        pure function format_error_i(this, chain) result(chars)
            import error_handler_t, error_chain_t
            class(error_handler_t), intent(in) :: this
            !> Chain of errors for the error
            type(error_chain_t), intent(in) :: chain
            character(len=:), allocatable :: chars
        end function
    end interface


    !> The type responsible for creating handlers for error reports.
    type, abstract :: error_hook_t
    contains
        procedure(create_handler_i), deferred :: create_handler
    end type


    abstract interface
        !> Create a handler which will be attached to an error report
        pure function create_handler_i(this, error) result(handler)
            import error_hook_t, error_handler_t, error_t
            class(error_hook_t), intent(in) :: this
            !> Root cause error
            class(error_t), intent(in) :: error
            class(error_handler_t), allocatable :: handler
        end function
    end interface


    interface
        !> Install an error_hook_t for custom error handling
        module subroutine set_error_hook(hook)
            class(error_hook_t), intent(in) :: hook
        end subroutine

        !> Remove any existing error_hook_t for custom error handling
        module subroutine remove_error_hook()
        end subroutine
    end interface

end module