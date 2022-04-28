! SPDX-FileCopyrightText: 2022 SINTEF Ocean
! SPDX-License-Identifier: MIT

!! Types extending result_t for (some) primitive types
module error_handling_experimental_result
    use iso_fortran_env, only: sp => real32, dp => real64
    use error_handling_error, only: error_t
    implicit none


    private
    public result_chars_t
    public result_real_sp_t
    public result_real_sp_rank1_t
    public result_real_sp_rank2_t
    public result_real_sp_rank3_t
    public result_real_sp_rank4_t
    public result_real_dp_t
    public result_real_dp_rank1_t
    public result_real_dp_rank2_t
    public result_real_dp_rank3_t
    public result_real_dp_rank4_t
    public result_complex_sp_t
    public result_complex_sp_rank1_t
    public result_complex_sp_rank2_t
    public result_complex_sp_rank3_t
    public result_complex_sp_rank4_t
    public result_complex_dp_t
    public result_complex_dp_rank1_t
    public result_complex_dp_rank2_t
    public result_complex_dp_rank3_t
    public result_complex_dp_rank4_t
    public result_integer_t
    public result_integer_rank1_t
    public result_integer_rank2_t
    public result_integer_rank3_t
    public result_integer_rank4_t
    public result_logical_t
    public result_logical_rank1_t
    public result_logical_rank2_t
    public result_logical_rank3_t
    public result_logical_rank4_t


    type :: result_chars_t
        character(len=:), allocatable :: value
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_chars, assign_chars_error
        procedure :: is_value => is_value_chars
        procedure :: is_error => is_error_chars

        procedure, private :: assign_chars
        procedure, private :: assign_chars_error
    end type


    type :: result_real_sp_t
        real(sp), allocatable :: value
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_sp, assign_error_real_sp
        procedure :: is_value => is_value_real_sp
        procedure :: is_error => is_error_real_sp

        procedure, private :: assign_real_sp
        procedure, private :: assign_error_real_sp
    end type


    type :: result_real_sp_rank1_t
        real(sp), allocatable :: value(:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_sp_rank1, assign_error_real_sp_rank1
        procedure :: is_value => is_value_real_sp_rank1
        procedure :: is_error => is_error_real_sp_rank1

        procedure, private :: assign_real_sp_rank1
        procedure, private :: assign_error_real_sp_rank1
    end type


    type :: result_real_sp_rank2_t
        real(sp), allocatable :: value(:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_sp_rank2, assign_error_real_sp_rank2
        procedure :: is_value => is_value_real_sp_rank2
        procedure :: is_error => is_error_real_sp_rank2

        procedure, private :: assign_real_sp_rank2
        procedure, private :: assign_error_real_sp_rank2
    end type


    type :: result_real_sp_rank3_t
        real(sp), allocatable :: value(:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_sp_rank3, assign_error_real_sp_rank3
        procedure :: is_value => is_value_real_sp_rank3
        procedure :: is_error => is_error_real_sp_rank3

        procedure, private :: assign_real_sp_rank3
        procedure, private :: assign_error_real_sp_rank3
    end type


    type :: result_real_sp_rank4_t
        real(sp), allocatable :: value(:,:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_sp_rank4, assign_error_real_sp_rank4
        procedure :: is_value => is_value_real_sp_rank4
        procedure :: is_error => is_error_real_sp_rank4

        procedure, private :: assign_real_sp_rank4
        procedure, private :: assign_error_real_sp_rank4
    end type


    type :: result_real_dp_t
        real(dp), allocatable :: value
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_dp, assign_error_real_dp
        procedure :: is_value => is_value_real_dp
        procedure :: is_error => is_error_real_dp

        procedure, private :: assign_real_dp
        procedure, private :: assign_error_real_dp
    end type


    type :: result_real_dp_rank1_t
        real(dp), allocatable :: value(:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_dp_rank1, assign_error_real_dp_rank1
        procedure :: is_value => is_value_real_dp_rank1
        procedure :: is_error => is_error_real_dp_rank1

        procedure, private :: assign_real_dp_rank1
        procedure, private :: assign_error_real_dp_rank1
    end type


    type :: result_real_dp_rank2_t
        real(dp), allocatable :: value(:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_dp_rank2, assign_error_real_dp_rank2
        procedure :: is_value => is_value_real_dp_rank2
        procedure :: is_error => is_error_real_dp_rank2

        procedure, private :: assign_real_dp_rank2
        procedure, private :: assign_error_real_dp_rank2
    end type


    type :: result_real_dp_rank3_t
        real(dp), allocatable :: value(:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_dp_rank3, assign_error_real_dp_rank3
        procedure :: is_value => is_value_real_dp_rank3
        procedure :: is_error => is_error_real_dp_rank3

        procedure, private :: assign_real_dp_rank3
        procedure, private :: assign_error_real_dp_rank3
    end type


    type :: result_real_dp_rank4_t
        real(dp), allocatable :: value(:,:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_real_dp_rank4, assign_error_real_dp_rank4
        procedure :: is_value => is_value_real_dp_rank4
        procedure :: is_error => is_error_real_dp_rank4

        procedure, private :: assign_real_dp_rank4
        procedure, private :: assign_error_real_dp_rank4
    end type


    type :: result_complex_sp_t
        complex(sp), allocatable :: value
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_sp, assign_error_complex_sp
        procedure :: is_value => is_value_complex_sp
        procedure :: is_error => is_error_complex_sp

        procedure, private :: assign_complex_sp
        procedure, private :: assign_error_complex_sp
    end type


    type :: result_complex_sp_rank1_t
        complex(sp), allocatable :: value(:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_sp_rank1, assign_error_complex_sp_rank1
        procedure :: is_value => is_value_complex_sp_rank1
        procedure :: is_error => is_error_complex_sp_rank1

        procedure, private :: assign_complex_sp_rank1
        procedure, private :: assign_error_complex_sp_rank1
    end type


    type :: result_complex_sp_rank2_t
        complex(sp), allocatable :: value(:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_sp_rank2, assign_error_complex_sp_rank2
        procedure :: is_value => is_value_complex_sp_rank2
        procedure :: is_error => is_error_complex_sp_rank2

        procedure, private :: assign_complex_sp_rank2
        procedure, private :: assign_error_complex_sp_rank2
    end type


    type :: result_complex_sp_rank3_t
        complex(sp), allocatable :: value(:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_sp_rank3, assign_error_complex_sp_rank3
        procedure :: is_value => is_value_complex_sp_rank3
        procedure :: is_error => is_error_complex_sp_rank3

        procedure, private :: assign_complex_sp_rank3
        procedure, private :: assign_error_complex_sp_rank3
    end type


    type :: result_complex_sp_rank4_t
        complex(sp), allocatable :: value(:,:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_sp_rank4, assign_error_complex_sp_rank4
        procedure :: is_value => is_value_complex_sp_rank4
        procedure :: is_error => is_error_complex_sp_rank4

        procedure, private :: assign_complex_sp_rank4
        procedure, private :: assign_error_complex_sp_rank4
    end type


    type :: result_complex_dp_t
        complex(dp), allocatable :: value
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_dp, assign_error_complex_dp
        procedure :: is_value => is_value_complex_dp
        procedure :: is_error => is_error_complex_dp

        procedure, private :: assign_complex_dp
        procedure, private :: assign_error_complex_dp
    end type


    type :: result_complex_dp_rank1_t
        complex(dp), allocatable :: value(:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_dp_rank1, assign_error_complex_dp_rank1
        procedure :: is_value => is_value_complex_dp_rank1
        procedure :: is_error => is_error_complex_dp_rank1

        procedure, private :: assign_complex_dp_rank1
        procedure, private :: assign_error_complex_dp_rank1
    end type


    type :: result_complex_dp_rank2_t
        complex(dp), allocatable :: value(:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_dp_rank2, assign_error_complex_dp_rank2
        procedure :: is_value => is_value_complex_dp_rank2
        procedure :: is_error => is_error_complex_dp_rank2

        procedure, private :: assign_complex_dp_rank2
        procedure, private :: assign_error_complex_dp_rank2
    end type


    type :: result_complex_dp_rank3_t
        complex(dp), allocatable :: value(:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_dp_rank3, assign_error_complex_dp_rank3
        procedure :: is_value => is_value_complex_dp_rank3
        procedure :: is_error => is_error_complex_dp_rank3

        procedure, private :: assign_complex_dp_rank3
        procedure, private :: assign_error_complex_dp_rank3
    end type


    type :: result_complex_dp_rank4_t
        complex(dp), allocatable :: value(:,:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_complex_dp_rank4, assign_error_complex_dp_rank4
        procedure :: is_value => is_value_complex_dp_rank4
        procedure :: is_error => is_error_complex_dp_rank4

        procedure, private :: assign_complex_dp_rank4
        procedure, private :: assign_error_complex_dp_rank4
    end type


    type :: result_integer_t
        integer, allocatable :: value
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_integer, assign_error_integer
        procedure :: is_value => is_value_integer
        procedure :: is_error => is_error_integer

        procedure, private :: assign_integer
        procedure, private :: assign_error_integer
    end type


    type :: result_integer_rank1_t
        integer, allocatable :: value(:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_integer_rank1, assign_error_integer_rank1
        procedure :: is_value => is_value_integer_rank1
        procedure :: is_error => is_error_integer_rank1

        procedure, private :: assign_integer_rank1
        procedure, private :: assign_error_integer_rank1
    end type


    type :: result_integer_rank2_t
        integer, allocatable :: value(:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_integer_rank2, assign_error_integer_rank2
        procedure :: is_value => is_value_integer_rank2
        procedure :: is_error => is_error_integer_rank2

        procedure, private :: assign_integer_rank2
        procedure, private :: assign_error_integer_rank2
    end type


    type :: result_integer_rank3_t
        integer, allocatable :: value(:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_integer_rank3, assign_error_integer_rank3
        procedure :: is_value => is_value_integer_rank3
        procedure :: is_error => is_error_integer_rank3

        procedure, private :: assign_integer_rank3
        procedure, private :: assign_error_integer_rank3
    end type


    type :: result_integer_rank4_t
        integer, allocatable :: value(:,:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_integer_rank4, assign_error_integer_rank4
        procedure :: is_value => is_value_integer_rank4
        procedure :: is_error => is_error_integer_rank4

        procedure, private :: assign_integer_rank4
        procedure, private :: assign_error_integer_rank4
    end type


    type :: result_logical_t
        logical, allocatable :: value
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_logical, assign_error_logical
        procedure :: is_value => is_value_logical
        procedure :: is_error => is_error_logical

        procedure, private :: assign_logical
        procedure, private :: assign_error_logical
    end type


    type :: result_logical_rank1_t
        logical, allocatable :: value(:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_logical_rank1, assign_error_logical_rank1
        procedure :: is_value => is_value_logical_rank1
        procedure :: is_error => is_error_logical_rank1

        procedure, private :: assign_logical_rank1
        procedure, private :: assign_error_logical_rank1
    end type


    type :: result_logical_rank2_t
        logical, allocatable :: value(:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_logical_rank2, assign_error_logical_rank2
        procedure :: is_value => is_value_logical_rank2
        procedure :: is_error => is_error_logical_rank2

        procedure, private :: assign_logical_rank2
        procedure, private :: assign_error_logical_rank2
    end type


    type :: result_logical_rank3_t
        logical, allocatable :: value(:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_logical_rank3, assign_error_logical_rank3
        procedure :: is_value => is_value_logical_rank3
        procedure :: is_error => is_error_logical_rank3

        procedure, private :: assign_logical_rank3
        procedure, private :: assign_error_logical_rank3
    end type


    type :: result_logical_rank4_t
        logical, allocatable :: value(:,:,:,:)
        type(error_t), allocatable :: error
    contains
        generic :: assignment(=) => assign_logical_rank4, assign_error_logical_rank4
        procedure :: is_value => is_value_logical_rank4
        procedure :: is_error => is_error_logical_rank4

        procedure, private :: assign_logical_rank4
        procedure, private :: assign_error_logical_rank4
    end type


contains


    pure subroutine assign_chars(lhs, rhs)
        class(result_chars_t), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs

        lhs%value = rhs
    end subroutine


    pure subroutine assign_chars_error(lhs, rhs)
        class(result_chars_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_chars(this) result(is_value)
        class(result_chars_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_chars(this) result(is_error)
        class(result_chars_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_sp(lhs, rhs)
        class(result_real_sp_t), intent(inout) :: lhs
        real(sp), intent(in) :: rhs

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_sp(lhs, rhs)
        class(result_real_sp_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_sp(this) result(is_value)
        class(result_real_sp_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_sp(this) result(is_error)
        class(result_real_sp_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_sp_rank1(lhs, rhs)
        class(result_real_sp_rank1_t), intent(inout) :: lhs
        real(sp), intent(in) :: rhs(:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_sp_rank1(lhs, rhs)
        class(result_real_sp_rank1_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_sp_rank1(this) result(is_value)
        class(result_real_sp_rank1_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_sp_rank1(this) result(is_error)
        class(result_real_sp_rank1_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_sp_rank2(lhs, rhs)
        class(result_real_sp_rank2_t), intent(inout) :: lhs
        real(sp), intent(in) :: rhs(:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_sp_rank2(lhs, rhs)
        class(result_real_sp_rank2_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_sp_rank2(this) result(is_value)
        class(result_real_sp_rank2_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_sp_rank2(this) result(is_error)
        class(result_real_sp_rank2_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_sp_rank3(lhs, rhs)
        class(result_real_sp_rank3_t), intent(inout) :: lhs
        real(sp), intent(in) :: rhs(:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_sp_rank3(lhs, rhs)
        class(result_real_sp_rank3_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_sp_rank3(this) result(is_value)
        class(result_real_sp_rank3_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_sp_rank3(this) result(is_error)
        class(result_real_sp_rank3_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_sp_rank4(lhs, rhs)
        class(result_real_sp_rank4_t), intent(inout) :: lhs
        real(sp), intent(in) :: rhs(:,:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_sp_rank4(lhs, rhs)
        class(result_real_sp_rank4_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_sp_rank4(this) result(is_value)
        class(result_real_sp_rank4_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_sp_rank4(this) result(is_error)
        class(result_real_sp_rank4_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_dp(lhs, rhs)
        class(result_real_dp_t), intent(inout) :: lhs
        real(dp), intent(in) :: rhs

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_dp(lhs, rhs)
        class(result_real_dp_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_dp(this) result(is_value)
        class(result_real_dp_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_dp(this) result(is_error)
        class(result_real_dp_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_dp_rank1(lhs, rhs)
        class(result_real_dp_rank1_t), intent(inout) :: lhs
        real(dp), intent(in) :: rhs(:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_dp_rank1(lhs, rhs)
        class(result_real_dp_rank1_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_dp_rank1(this) result(is_value)
        class(result_real_dp_rank1_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_dp_rank1(this) result(is_error)
        class(result_real_dp_rank1_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_dp_rank2(lhs, rhs)
        class(result_real_dp_rank2_t), intent(inout) :: lhs
        real(dp), intent(in) :: rhs(:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_dp_rank2(lhs, rhs)
        class(result_real_dp_rank2_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_dp_rank2(this) result(is_value)
        class(result_real_dp_rank2_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_dp_rank2(this) result(is_error)
        class(result_real_dp_rank2_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_dp_rank3(lhs, rhs)
        class(result_real_dp_rank3_t), intent(inout) :: lhs
        real(dp), intent(in) :: rhs(:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_dp_rank3(lhs, rhs)
        class(result_real_dp_rank3_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_dp_rank3(this) result(is_value)
        class(result_real_dp_rank3_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_dp_rank3(this) result(is_error)
        class(result_real_dp_rank3_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_real_dp_rank4(lhs, rhs)
        class(result_real_dp_rank4_t), intent(inout) :: lhs
        real(dp), intent(in) :: rhs(:,:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_real_dp_rank4(lhs, rhs)
        class(result_real_dp_rank4_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_real_dp_rank4(this) result(is_value)
        class(result_real_dp_rank4_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_real_dp_rank4(this) result(is_error)
        class(result_real_dp_rank4_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_sp(lhs, rhs)
        class(result_complex_sp_t), intent(inout) :: lhs
        complex(sp), intent(in) :: rhs

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_sp(lhs, rhs)
        class(result_complex_sp_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_sp(this) result(is_value)
        class(result_complex_sp_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_sp(this) result(is_error)
        class(result_complex_sp_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_sp_rank1(lhs, rhs)
        class(result_complex_sp_rank1_t), intent(inout) :: lhs
        complex(sp), intent(in) :: rhs(:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_sp_rank1(lhs, rhs)
        class(result_complex_sp_rank1_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_sp_rank1(this) result(is_value)
        class(result_complex_sp_rank1_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_sp_rank1(this) result(is_error)
        class(result_complex_sp_rank1_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_sp_rank2(lhs, rhs)
        class(result_complex_sp_rank2_t), intent(inout) :: lhs
        complex(sp), intent(in) :: rhs(:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_sp_rank2(lhs, rhs)
        class(result_complex_sp_rank2_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_sp_rank2(this) result(is_value)
        class(result_complex_sp_rank2_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_sp_rank2(this) result(is_error)
        class(result_complex_sp_rank2_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_sp_rank3(lhs, rhs)
        class(result_complex_sp_rank3_t), intent(inout) :: lhs
        complex(sp), intent(in) :: rhs(:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_sp_rank3(lhs, rhs)
        class(result_complex_sp_rank3_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_sp_rank3(this) result(is_value)
        class(result_complex_sp_rank3_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_sp_rank3(this) result(is_error)
        class(result_complex_sp_rank3_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_sp_rank4(lhs, rhs)
        class(result_complex_sp_rank4_t), intent(inout) :: lhs
        complex(sp), intent(in) :: rhs(:,:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_sp_rank4(lhs, rhs)
        class(result_complex_sp_rank4_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_sp_rank4(this) result(is_value)
        class(result_complex_sp_rank4_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_sp_rank4(this) result(is_error)
        class(result_complex_sp_rank4_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_dp(lhs, rhs)
        class(result_complex_dp_t), intent(inout) :: lhs
        complex(dp), intent(in) :: rhs

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_dp(lhs, rhs)
        class(result_complex_dp_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_dp(this) result(is_value)
        class(result_complex_dp_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_dp(this) result(is_error)
        class(result_complex_dp_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_dp_rank1(lhs, rhs)
        class(result_complex_dp_rank1_t), intent(inout) :: lhs
        complex(dp), intent(in) :: rhs(:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_dp_rank1(lhs, rhs)
        class(result_complex_dp_rank1_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_dp_rank1(this) result(is_value)
        class(result_complex_dp_rank1_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_dp_rank1(this) result(is_error)
        class(result_complex_dp_rank1_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_dp_rank2(lhs, rhs)
        class(result_complex_dp_rank2_t), intent(inout) :: lhs
        complex(dp), intent(in) :: rhs(:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_dp_rank2(lhs, rhs)
        class(result_complex_dp_rank2_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_dp_rank2(this) result(is_value)
        class(result_complex_dp_rank2_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_dp_rank2(this) result(is_error)
        class(result_complex_dp_rank2_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_dp_rank3(lhs, rhs)
        class(result_complex_dp_rank3_t), intent(inout) :: lhs
        complex(dp), intent(in) :: rhs(:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_dp_rank3(lhs, rhs)
        class(result_complex_dp_rank3_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_dp_rank3(this) result(is_value)
        class(result_complex_dp_rank3_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_dp_rank3(this) result(is_error)
        class(result_complex_dp_rank3_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_complex_dp_rank4(lhs, rhs)
        class(result_complex_dp_rank4_t), intent(inout) :: lhs
        complex(dp), intent(in) :: rhs(:,:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_complex_dp_rank4(lhs, rhs)
        class(result_complex_dp_rank4_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_complex_dp_rank4(this) result(is_value)
        class(result_complex_dp_rank4_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_complex_dp_rank4(this) result(is_error)
        class(result_complex_dp_rank4_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_integer(lhs, rhs)
        class(result_integer_t), intent(inout) :: lhs
        integer, intent(in) :: rhs

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_integer(lhs, rhs)
        class(result_integer_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_integer(this) result(is_value)
        class(result_integer_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_integer(this) result(is_error)
        class(result_integer_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_integer_rank1(lhs, rhs)
        class(result_integer_rank1_t), intent(inout) :: lhs
        integer, intent(in) :: rhs(:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_integer_rank1(lhs, rhs)
        class(result_integer_rank1_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_integer_rank1(this) result(is_value)
        class(result_integer_rank1_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_integer_rank1(this) result(is_error)
        class(result_integer_rank1_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_integer_rank2(lhs, rhs)
        class(result_integer_rank2_t), intent(inout) :: lhs
        integer, intent(in) :: rhs(:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_integer_rank2(lhs, rhs)
        class(result_integer_rank2_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_integer_rank2(this) result(is_value)
        class(result_integer_rank2_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_integer_rank2(this) result(is_error)
        class(result_integer_rank2_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_integer_rank3(lhs, rhs)
        class(result_integer_rank3_t), intent(inout) :: lhs
        integer, intent(in) :: rhs(:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_integer_rank3(lhs, rhs)
        class(result_integer_rank3_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_integer_rank3(this) result(is_value)
        class(result_integer_rank3_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_integer_rank3(this) result(is_error)
        class(result_integer_rank3_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_integer_rank4(lhs, rhs)
        class(result_integer_rank4_t), intent(inout) :: lhs
        integer, intent(in) :: rhs(:,:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_integer_rank4(lhs, rhs)
        class(result_integer_rank4_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_integer_rank4(this) result(is_value)
        class(result_integer_rank4_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_integer_rank4(this) result(is_error)
        class(result_integer_rank4_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_logical(lhs, rhs)
        class(result_logical_t), intent(inout) :: lhs
        logical, intent(in) :: rhs

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_logical(lhs, rhs)
        class(result_logical_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_logical(this) result(is_value)
        class(result_logical_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_logical(this) result(is_error)
        class(result_logical_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_logical_rank1(lhs, rhs)
        class(result_logical_rank1_t), intent(inout) :: lhs
        logical, intent(in) :: rhs(:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_logical_rank1(lhs, rhs)
        class(result_logical_rank1_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_logical_rank1(this) result(is_value)
        class(result_logical_rank1_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_logical_rank1(this) result(is_error)
        class(result_logical_rank1_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_logical_rank2(lhs, rhs)
        class(result_logical_rank2_t), intent(inout) :: lhs
        logical, intent(in) :: rhs(:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_logical_rank2(lhs, rhs)
        class(result_logical_rank2_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_logical_rank2(this) result(is_value)
        class(result_logical_rank2_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_logical_rank2(this) result(is_error)
        class(result_logical_rank2_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_logical_rank3(lhs, rhs)
        class(result_logical_rank3_t), intent(inout) :: lhs
        logical, intent(in) :: rhs(:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_logical_rank3(lhs, rhs)
        class(result_logical_rank3_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_logical_rank3(this) result(is_value)
        class(result_logical_rank3_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_logical_rank3(this) result(is_error)
        class(result_logical_rank3_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


    pure subroutine assign_logical_rank4(lhs, rhs)
        class(result_logical_rank4_t), intent(inout) :: lhs
        logical, intent(in) :: rhs(:,:,:,:)

        lhs%value = rhs
    end subroutine


    pure subroutine assign_error_logical_rank4(lhs, rhs)
        class(result_logical_rank4_t), intent(inout) :: lhs
        type(error_t), intent(in) :: rhs

        lhs%error = rhs
    end subroutine


    logical pure function is_value_logical_rank4(this) result(is_value)
        class(result_logical_rank4_t), intent(in) :: this

        is_value = .not. allocated(this%error)
    end function


    logical pure function is_error_logical_rank4(this) result(is_error)
        class(result_logical_rank4_t), intent(in) :: this

        is_error = allocated(this%error)
    end function


end module