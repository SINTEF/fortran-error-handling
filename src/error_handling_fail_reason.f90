! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT
module error_handling_fail_reason
    implicit none

    public fail_reason_t
    public fail_reason_ctr_t


    type, abstract :: fail_reason_t
    !> A type that can be extended to let library users check for and handle
    !! specific error scenarios
    contains
        procedure(describe_i), deferred :: describe
    end type

    abstract interface
        !> Returns a human readable description of the error
        pure function describe_i(this) result(chars)
            import fail_reason_t
            class(fail_reason_t), intent(in) :: this
            character(len=:), allocatable :: chars
        end function
    end interface


    type :: fail_reason_ctr_t
        !> Container type for fail_reason_t used to keep an array of different
        !! fail_reason_t implementations
        class(fail_reason_t), allocatable :: reason
    end type

end module
