!
! This program reads in a text file containing two integer parameters,
! n and k, and computes the Fibonacci-based result using the recurrence
! relation:
!
!     F(n,k) = F(n-1,k) + k*F(n-2,k),
!
! where F(1,k) = F(2,k) = 1.
!
! In this program, we're using a do loop (without arrays) to solve the
! problem. Should have a very small overhead, since we only need to keep
! track of two variables throughout the entire loop.
!
! Compile using your favourite compiler. I like gfortran:
!
! gfortran -O3 -funroll-loops -march=native -ftree-vectorize MN_fortran_scalar.f90 -o MN_fortran_scalar
!
! This will take MN_fortran_scalar.f90, and create an executable binary
! named "MN_fortran_scalar". This binary is then run using the following
! command:
!
! ./MN_fortran_scalar datfile.txt
!
! where "datfile.txt" is the data file containing the n and k parameters
! that are used to calculate the Fibonacci-based result.
!
! Output is the (integer) result of F(n,k).
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
integer(kind=int64) :: F1, F2

! Step 1: Dynamically get the file-name and read the input parameters
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
!  Make sure that the parameters are valid -- stop if they are not
if( (n.GT.40) .OR. (k.GT.5) ) then
  write(*,*) "Invalid input parameters!"
  write(*,*) "n cannot exceed 40, and k cannot exceed 5"
  stop
end if

! Step 2: Compute the Fibonacci result and write it to the screen
!  Initialise F1 and F2.
!  N.B. F1 = 1 if n is even, 0 otherwise. F2 always equals 1.
F1 = mod(n+1,2)
F2 = 1
!  Calculate the Fibonacci iterations (iterate in pairs, since we are 
!  only using two variables)
do i=1,(n-1-F1)/2
  F1 = F2 + k*F1
  F2 = F1 + k*F2
end do
!  Write the result to the screen
write(*,*) F2

stop

end program fib
