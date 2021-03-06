!
! This program reads in a text file containing two integer parameters,
! n and k, and computes the Fibonacci-based result using the recurrence
! relation:
!
!     F(n,k) = F(n-1,k) + k*F(n-2,k),
!
! where F(1,k) = F(2,k) = 1.
!
! In this program, we're using a recursive function with memoization
! to solve the problem. Here, we use a global array (contained in a
! module) to store all previously-computed values, which avoids
! repeated calculations.
!
! Compile using your favourite compiler. I like gfortran:
!
! gfortran -O3 -funroll-loops -march=native -ftree-vectorize MN_fortran_memoize.f90 -o MN_fortran_memoize
!
! This will take MN_fortran_memoize.f90, and create an executable binary
! named "MN_fortran_recursive". This binary is then run using the following
! command:
!
! ./MN_fortran_memoize datfile.txt
!
! where "datfile.txt" is the data file containing the n and k parameters
! that are used to calculate the Fibonacci-based result.
!
! Output is the (integer) result of F(n,k).
!

! Create a module containing the look-up array, Fval
module vals_mod
  use iso_fortran_env, only: int64
  integer(kind=int64), dimension(40) :: Fval
end module vals_mod

program fib
! Get 64-bit integer type from the Fortran ISO module (required, because
! the numbers can get extremely large).
use vals_mod

! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
integer :: iunit
integer(kind=int64) :: n, k

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
!
! Initialise Fval(1) = Fval(2) = 1, and all other elements of Fval to 0.
Fval = 0
Fval(1:2) = 1
! Calculate F(n,k) and write the result to the screen
write(*,*) F(n,k)

stop

contains

! Declare a recursive function that computes the Fibonacci sequence
! with k offspring pairs at each iteration
recursive function F(n,k) result(r)
implicit none
integer(kind=int64), value, intent(in) :: n, k
integer(kind=int64) :: r

if(Fval(n).EQ.0) Fval(n) = F(n-1,k) + k*F(n-2,k)
r = Fval(n)

end function F

end program fib
