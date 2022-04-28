! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT
module error_handling_error_stop
    use error_handling_error, only: error_t
    implicit none

    private
    public error_stop


    interface error_stop
        module procedure error_stop_message
        module procedure error_stop_error
    end interface


    interface
        !> Error stop execution of the application with an error_t created
        !! from `message`
        pure module subroutine error_stop_message(message)
            !> Message to stop execution with
            character(len=*), intent(in) :: message
        end subroutine


        !> Error stop execution of the application with an error_t
        pure module subroutine error_stop_error(error)
            !> Error to stop execution with
            type(error_t), intent(in) :: error
        end subroutine
    end interface

end module