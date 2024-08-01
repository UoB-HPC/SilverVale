module second_module
    implicit none
    integer, parameter :: value = 64
contains

    real(kind = 8) function bar()
        bar = value! comment should not appear
    end function
    ! comment should not appear
end module second_module