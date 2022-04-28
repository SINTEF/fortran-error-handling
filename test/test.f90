program test
    use error_handling_test, only: test_error_handling
    use result_test, only: test_result
    implicit none

    call test_error_handling
    call test_result
end program