! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT

!> Top level module for error handling library. It is preferred to use types
!! and procedures from this module. See README.adoc and the modules use-d below
!! for documentaion.
module error_handling
    use error_handling_error, only: error_t, set_error_hook
    use error_handling_fail_reason, only: fail_reason_t, fail_reason_ctr_t
    use error_handling_hook, only: error_hook_t, error_handler_t
    use error_handling_error_stop, only: error_stop
    implicit none

    private
    ! `error_t` is the type that can be used to indicate that a procedure failed.
    ! See error_handling_error.f90
    public error_t

    ! `fail_reason_t` can be used to programatically handle specific errors.
    ! See error_handling_fail_reason.f90
    public fail_reason_t
    public fail_reason_ctr_t

    ! This subroutine can be used to set a hook for customizing error handling,
    ! e.g. for adding stacktrace generation.
    ! See error_handling_error.f90
    public set_error_hook

    ! Abstract types for customizing error handling.
    ! See error_handling_hook.f90
    public error_hook_t
    public error_handler_t

    ! `error_stop` is a subroutine that wraps a Fortran `error stop` that can be used
    ! to stop the application with an `error_t` error.
    ! See error_handling_error_stop.f90
    public error_stop
end module