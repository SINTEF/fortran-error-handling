module better_sqrt_inplace_mod
    use error_handling, only: error_t, fail_reason_t
    implicit none

    private
    public sqrt_inplace
    public negative_value_failure_t

    type, extends(fail_reason_t) :: negative_value_failure_t
    contains
        procedure :: describe
    end type

contains


    pure function describe(this)
        class(negative_value_failure_t), intent(in) :: this
        character(len=:), allocatable :: describe

        describe = 'x is negative'
        ! The unused argument `this` is intentional, the trick below
        ! avoids compilers to emit a warning about it.
        associate(dummy => this); end associate
    end function


    pure subroutine sqrt_inplace(x, error)
        real, intent(inout) :: x
        type(error_t), allocatable, intent(inout) :: error

        if (x <= 0.0) then
            error = error_t(negative_value_failure_t())
            return
        end if
        x = sqrt(x)
    end subroutine

end module


program custom_error_cause
    use error_handling, only: error_t, error_stop
    use better_sqrt_inplace_mod, only: sqrt_inplace, negative_value_failure_t
    implicit none

    real :: x
    type(error_t), allocatable :: error

    fallible: block
        x = -20.0
        call sqrt_inplace(x, error)
        ! If an error occurred, go to error handling code
        if (allocated(error)) exit fallible
        ! Success -> write result
        write(*,*) 'sqrt = ', x
        ! In a procedure we would usually have a return here instead of stop
        stop
    end block fallible
    ! If we're here then an error has happened!
    select type (reason => error%root_cause)
        ! Check if it's an error we know how to handle
        type is (negative_value_failure_t)
            ! We know better!
            block
                ! Notice that the variable `error` here masks error from the parent
                ! scope. This is intentional as `error`in the parent scope is
                ! already assigned to a value.
                type(error_t), allocatable :: error

                x = - x
                call sqrt_inplace(x, error)
                ! If this fails we're out of luck...
                if (allocated(error)) call error_stop(error)
                write(*,*) 'sqrt = ', x, ' * i'
            end block
        class default
            ! Unknown error that we don't know how to handle
            call error_stop(error)
    end select
end program
