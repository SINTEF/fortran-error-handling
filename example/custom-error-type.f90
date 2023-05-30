module better_sqrt_inplace_mod
    use error_handling, only: error_t
    implicit none

    private
    public sqrt_inplace
    public negative_value_error_t

    type, extends(error_t) :: negative_value_error_t
    contains
        procedure :: to_chars
    end type

contains


    pure function to_chars(this) result(chars)
        class(negative_value_error_t), intent(in) :: this
        character(len=:), allocatable :: chars

        chars = 'x is negative'
        ! The unused argument `this` is intentional, the trick below
        ! avoid compilers emitting a warning about it.
        associate(dummy => this); end associate
    end function


    pure subroutine sqrt_inplace(x, error)
        real, intent(inout) :: x
        class(error_t), allocatable, intent(inout) :: error

        if (x <= 0.0) then
            error = negative_value_error_t()
            return
        end if
        x = sqrt(x)
    end subroutine

end module


program custom_error_type
    use error_handling, only: error_t, error_stop
    use better_sqrt_inplace_mod, only: sqrt_inplace, negative_value_error_t
    implicit none

    real :: x
    class(error_t), allocatable :: error

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
    select type (error)
        ! Check if it's an error we know how to handle
        type is (negative_value_error_t)
            ! We know better!
            block
                class(error_t), allocatable :: error2

                x = - x
                call sqrt_inplace(x, error2)
                ! If this fails we're out of luck...
                if (allocated(error2)) call error_stop(error2)
                write(*,*) 'sqrt = ', x, ' * i'
            end block
        class default
            ! Unknown error that we don't know how to handle
            call error_stop(error)
    end select
end program
