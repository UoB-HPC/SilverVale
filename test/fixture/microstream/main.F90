#define DATA_KIND REAL64
#define DATA_KIND_NAME "REAL64"

module stream
    use, intrinsic :: ISO_Fortran_env, only : REAL64, REAL32, INT64, INT32
    implicit none

    real(kind = DATA_KIND), parameter :: startA = 0.1
    real(kind = DATA_KIND), parameter :: startB = 0.2
    real(kind = DATA_KIND), parameter :: startC = 0.0
    real(kind = DATA_KIND), parameter :: startScalar = 0.4
contains

    function time() result(t)
        implicit none
        real(kind = REAL64) :: t
        integer :: c, r
        call system_clock(count = c, count_rate = r)
        t = real(c, REAL64) / real(r, REAL64)
    end function time

    subroutine execute(times, init, read, copy, mul, add, triad, dot, timings)
        implicit none
        integer, intent(in) :: times
        real(kind = REAL64), dimension(:), intent(out) :: timings(:, :)
        external :: init, read, copy, mul, add, triad, dot
        integer :: k
        real(kind = REAL64) :: t1, t2

        call init()

        do k = 1, times
            t1 = time()
            call copy()
            t2 = time()
            timings(1, k) = t2 - t1

            t1 = time()
            call mul()
            t2 = time()
            timings(2, k) = t2 - t1

            t1 = time()
            call add()
            t2 = time()
            timings(3, k) = t2 - t1

            t1 = time()
            call triad()
            t2 = time()
            timings(4, k) = t2 - t1

            t1 = time()
            call dot()
            t2 = time()
            timings(5, k) = t2 - t1
        end do

        call read()
    end subroutine execute

    subroutine runAll(a, b, c, h_a, h_b, h_c, initA, initB, initC, scalar, dotSum, arrSize, times, timings)
        implicit none
        real(kind = DATA_KIND), dimension(:), intent(inout) :: a, b, c
        real(kind = DATA_KIND), dimension(:), intent(out) :: h_a, h_b, h_c
        real(kind = DATA_KIND), intent(in) :: initA, initB, initC, scalar
        real(kind = DATA_KIND), intent(inout) :: dotSum
        integer, intent(in) :: arrSize, times
        real(kind = REAL64), dimension(:, :), allocatable, intent(out) :: timings
        allocate(timings(5, times))
        call execute(times, init, read, copy, mul, add, triad, dot, timings)
    contains
        subroutine init()
            integer :: i
#ifdef USE_OMP
            !$omp parallel do
#endif
            do i = 1, arrSize
                a(i) = initA
                b(i) = initB
                c(i) = initC
            end do
        end subroutine init

        subroutine read()
            integer :: i
#ifdef USE_OMP
            !$omp parallel do
#endif
            do i = 1, arrSize
                h_a(i) = a(i)
                h_b(i) = b(i)
                h_c(i) = c(i)
            end do
        end subroutine read

        subroutine copy()
            integer :: i
#ifdef USE_OMP
            !$omp parallel do
#endif
            do i = 1, arrSize
                c(i) = a(i)
            end do
        end subroutine copy

        subroutine mul()
            integer :: i
#ifdef USE_OMP
            !$omp parallel do
#endif
            do i = 1, arrSize
                b(i) = scalar * c(i)
            end do
        end subroutine mul

        subroutine add()
            integer :: i
#ifdef USE_OMP
            !$omp parallel do
#endif
            do i = 1, arrSize
                c(i) = a(i) + b(i)
            end do
        end subroutine add

        subroutine triad()
            integer :: i
#ifdef USE_OMP
            !$omp parallel do
#endif
            do i = 1, arrSize
                a(i) = b(i) + scalar * c(i)
            end do
        end subroutine triad

        subroutine dot()
            integer :: i
            dotSum = 0.0
#ifdef USE_OMP
            !$omp parallel do reduction(+:dotSum)
#endif
            do i = 1, arrSize
                dotSum = dotSum + a(i) * b(i)
            end do
        end subroutine dot
    end subroutine runAll

    subroutine run(arrSize, times)
        implicit none
        integer, intent(in) :: arrSize, times
        real(kind = REAL64) :: bytes
        real(kind = DATA_KIND) :: dotSum
        real(kind = DATA_KIND), dimension(:), allocatable :: h_a, h_b, h_c, a, b, c
        real(kind = REAL64), dimension(:, :), allocatable :: timings

        allocate(a(arrSize), b(arrSize), c(arrSize))
        allocate(h_a(arrSize), h_b(arrSize), h_c(arrSize))

        bytes = arrSize * 4.0

        write(*, "(a,i0,a)") "Running kernels ", times, " times"
        write(*, "(a,i0)") "Number of elements: ", arrSize
        write(*, "(a,a)") "Precision: ", DATA_KIND_NAME
        write(*, "(a,f8.1,a,f8.1,a)") "Array size: ", bytes * 1.0E-6, " MB (=", bytes * 1.0E-9, " GB)"
        write(*, "(a,f8.1,a,f8.1,a)")  "Total size: ", 3.0 * bytes * 1.0E-6, " MB (=", 3.0 * bytes * 1.0E-9, " GB)"

        call runAll(a, b, c, h_a, h_b, h_c, startA, startB, startC, startScalar, dotSum, arrSize, times, timings)
        block
            character(20) :: buffer(8)
            integer, parameter :: sizes(5) = [2, 2, 3, 3, 2]
            character(5), parameter :: labels(5) = ["Copy ", "Mul  ", "Add  ", "Triad", "Dot  "]
            integer :: kindSize = storage_size(real(0, kind = DATA_KIND)) / 8
            integer :: i
            real(kind = REAL64) :: tmin, tmax, tavg
            write(*, "(a)")   "Function    Mbytes/s    Min (sec)   Max         Average"
            do i = 1, 5
                tmin = MINVAL(timings(i, 2:times))
                tmax = MAXVAL(timings(i, 2:times))
                tavg = SUM(timings(i, 2:times)) / (times - 1)
                write(buffer(1), '(a)')     labels(i)
                write(buffer(2), '(f12.3)') 1.0d-6 * (kindSize * REAL(arrSize, kind = REAL64) * sizes(i)) / tmin
                write(buffer(3), '(f12.5)') tmin
                write(buffer(4), '(f12.5)') tmax
                write(buffer(5), '(f12.5)') tavg
                write(*, '(5a12)') ADJUSTL(buffer(1:5))
            enddo
        end block

        block
            real(kind = REAL64) :: epsi, goldA, goldB, goldC, goldSum, errSum
            integer :: i
            goldA = startA
            goldB = startB
            goldC = startC
            do i = 1, times
                goldC = goldA
                goldB = startScalar * goldC
                goldC = goldA + goldB
                goldA = goldB + startScalar * goldC
            end do
            goldSum = goldA * goldB * arrSize
            epsi = EPSILON(REAL(1.0, kind = DATA_KIND)) * 100.0
            errSum = ABS((dotSum - goldSum) / goldSum)
            if (errSum > 1.0E-8) then
                write (*, '(a,f12.5)')  "Validation failed on sum. Error ", errSum
                write (*, '(a,f12.5,a,f12.5)')  "Sum was ", dotSum, " but should be ", goldSum
            end if
            call checkErr(h_a, goldA, epsi, "a")
            call checkErr(h_b, goldB, epsi, "b")
            call checkErr(h_c, goldC, epsi, "c")
        end block
    end subroutine run

    subroutine checkErr(xs, gold, epsi, name)
        implicit none
        real(kind = DATA_KIND), dimension(:), intent(in) :: xs
        real(kind = REAL64), intent(in) :: gold, epsi
        real(kind = REAL64) :: acc, err
        character(*), intent(in) :: name
        integer :: i
        acc = 0.0
        do i = 1, SIZE(xs)
            acc = acc + ABS(xs(i) - gold)
        end do
        if ((acc / SIZE(xs)) > epsi) then
            write (*, '(a,a,f12.5)') "Validation failed on ", name, ". Average error ", acc
        end if
    end subroutine checkErr

end module stream

program main
    use iso_fortran_env
    use stream
    implicit none
    call run(33554432, 100)
end program main
