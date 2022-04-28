! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT
submodule(error_handling_error_stop) error_stop_impl
    implicit none

contains


    pure module subroutine error_stop_message(message)
        character(len=*), intent(in) :: message

        call error_stop(error_t(message))
    end subroutine


    pure module subroutine error_stop_error(error)
        type(error_t), intent(in) :: error

        character(len=:), allocatable :: chars

        chars = error%display()
        error stop chars
    end subroutine

end submodule