#define MACRO 128 + 1

module main_module
    implicit none
contains

    function foo() result (value)
        character(len = :), allocatable :: value
        value = "bar"
    end function foo

    real(kind = 8) function baz(x)
        real (kind = 8) :: x
        baz = min(1.0, x)
    end function baz

    real(kind = 8) function deadcode()
        deadcode = baz(42d0)
    end function deadcode

#ifdef NOT_DEFINED
! comment should not appear
  real(kind=8) function guarded()
    guarded = deadcode()
  end function guarded
#endif

end module main_module
! comment should not appear
program main
    use main_module
    use second_module
    implicit none

    print *, "> ", foo(), baz(43d0), bar(), MACRO

    if (baz(-1d0) < 0d0) then ! comment should not appear
        print *, "OK" ! comment should not appear
    else ! comment should not appear
        print *, "FAIL"
    end if

#ifdef NOT_DEFINED
  print *, 'nope ', guarded()
#endif

end program main
