!
! This program reads in a text file containing two integer parameters,
! n and k, and computes the Fibonacci-based result using the recurrence
! relation:
!
!     F(n) = F(n-1) + k*F(n-2).
!
! In this program, we're using a simple do loop to solve the problem.
!
! Compile using your favourite compiler. I like gfortran:
!
! gfortran -O3 -funroll-loops -march=native -ftree-vectorize MN_fortran_loop.f90 -o MN_fortran_loop
!
! This will take MN_fortran_loop.f90, and create an executable binary
! named "MN_fortran_loop". This binary is then run using the following
! command:
!
! ./MN_fortran_loop datfile.txt
!
! where "datfile.txt" is the data file containing the n and k parameters
! that are used to calculate the Fibonacci-based result.
!
! Output is the (integer) result of F(n).
!
program fib
! Get 64-bit integer type from the Fortran ISO module (required, because
! the numbers can get extremely large).
use iso_fortran_env, only: int64

! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
integer :: iunit, i
integer(kind=int64) :: n, k
integer(kind=int64), dimension(40) :: F

! Step 1: Dynamically get the file-name and read the data
!
!  Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
!  If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
!  Open the file connection
open(newunit=iunit, file=filename)
!  Read the two input parameters, n and k
read(iunit, *) n, k

! Step 2: Compute the Fibonacci result and write it to the screen
!  Initialise F(1) = F(2) = 1.
F(1) = 1
F(2) = 1
!  Calculate the Fibonacci sequence from 3:n
do i=3,n
  F(i) = F(i-1) + k*F(i-2)
end do
!  Write the result to the screen
write(*,*) F(n)

stop

end program fib
