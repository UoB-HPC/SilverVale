module consume_module
    use, intrinsic :: ISO_C_binding, only : c_ptr, c_loc, c_associated
    implicit none
    private
    public :: consume
contains

    subroutine consume(ptr)
        type(c_ptr), intent(in) :: ptr
        if (c_associated(ptr)) then
            write(*, '(A, Z8.8)') 'Consumed ', ptr
        endif
    end subroutine consume

end module consume_module