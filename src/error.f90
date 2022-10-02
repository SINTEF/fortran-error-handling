!> Error handling base error type defintion
!!
!! This module contains the definition of the basic error type.
module error_mod
    implicit none

    private
    public error_t

    !> Error type used to indicate the sucsess or failure of a procedure. Use for
    !! example as an dummy argument in a subroutine:
    !!     class(error_t), allocatable, intent(out) :: error
    !! Note that for pure and elemental subroutines the Fortran standard requires
    !! the intent to be inout:
    !!     class(error_t), allocatable, intent(inout) :: error
    !! When an error occurs, the argument can then be assigned to an error:
    !!     error = my_error_t()
    !! where my_error_t is a type which extends error_t.
    !! In the calling procedure, check for any error with
    !!     if (allocated(error)) !...
    !! afterwards.
    type, abstract :: error_t
    contains
        procedure(to_chars), deferred :: to_chars
    end type

    abstract interface
        !> Returns a human readable description of the error
        pure function to_chars(this) result(chars)
            import error_t
            class(error_t), intent(in) :: this
            character(len=:), allocatable :: chars
        end function
    end interface

end module