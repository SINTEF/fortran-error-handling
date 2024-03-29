! tag::usage[]
module sqrt_inplace_mod
    use error_handling, only: error_t, fail
    implicit none

    private
    public sqrt_inplace

contains

    pure subroutine sqrt_inplace(x, error)
        real, intent(inout) :: x
        class(error_t), allocatable, intent(inout) :: error

        if (x <= 0.0) then
            error = fail('x is negative')
            return
        end if
        x = sqrt(x)
    end subroutine

end module
! end::usage[]


module basic_example
    implicit none

    private
    public run

contains

    subroutine run
! tag::run[]
        use error_handling, only: error_t
        use sqrt_inplace_mod, only: sqrt_inplace
        implicit none

        real :: x
        class(error_t), allocatable :: error

        ! Here we use a labelled block to separate multiple fallible
        ! procedure calls from the code that handles any errors
        fallible: block
            write(*,*) 'computing square root...'
            x = 20.0
            call sqrt_inplace(x, error)
            ! If an error occurred, go to error handling code
            if (allocated(error)) exit fallible
            ! Success -> write result
            write(*,*) ' - sqrt = ', x
            write(*,*) 'computing square root...'
            x = - 20.0
            call sqrt_inplace(x, error)
            if (allocated(error)) exit fallible
            write(*,*) ' - sqrt = ', x
            ! Return from subroutine on success, code below is only for
            ! error handling so no allocated(error) check is needed there.
            return
        end block fallible
        ! If we're here then an error has happened!
        write(*, '(a,a)') 'Error: ', error%to_chars()
! end::run[]
    end subroutine
end module


program basic
    use basic_example, only: run

    call run
end program

