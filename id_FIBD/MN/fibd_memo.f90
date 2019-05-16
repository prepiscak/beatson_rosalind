!
! This program reads in a text file containing two integer parameters,
! n and m, and computes the Fibonacci-based result using the recurrence
! relation:
!
!     F(n) = F(n-1) + F(n-2) - F(n-m-1),
!
! where:
!
!     F(i) = 0, (i<0),
!     F(0) = F(1) = F(2) = 1.
!
! In this program, we're using a recursive function with memoisation to
! solve the problem.
!
! Compile using your favourite compiler. I like gfortran:
!
! gfortran -O3 -funroll-loops -march=native -ftree-vectorize fibd_memo.f90 -o fibd_memo
!
! This will take fibd_memo.f90, and create an executable binary
! named "fibd_memo". This binary is then run using the following
! command:
!
! ./fibd_memo datfile.txt
!
! where "datfile.txt" is the data file containing the n and m parameters
! that are used to calculate the Fibonacci-based result.
!
! Output is the (integer) result of F(n).
!

module memo_mod
! Declare a new data-type for storing integers comprising up to 100
! digits (could make integers arbitrarily large using this approach,
! just choose the dimension of v to be as large as the largest number
! of digits you want to represent).
!
! Data type has two components:
!   v (an integer array, where each element contains each digit of a number),
!   n (an integer denoting the number of digits that comprise the number.
!
! Each element of v is initialised to 0, whilst n is initialised to 1.
  type bigtype
      integer, dimension(100) :: v = 0
      integer :: n = 1
  end type bigtype
! Declare Fm, the memoisation array for storing results (avoids repeated calculation).
  type(bigtype), dimension(:), allocatable :: Fm
end module memo_mod

program fibd

use memo_mod
! Ensure that we don't have any undeclared variables
implicit none

   
! Declare variables
character(len=64) :: filename
integer :: iunit, i
integer :: n, m
type(bigtype) :: res

! Step 1: Dynamically get the file-name and read the input parameters
call getarg(1,filename)
!  If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
!  Open the file connection
open(newunit=iunit, file=filename)
!  Read the two input parameters, n and m
read(iunit, *) n, m

! Step 2: make sure that the parameters are valid -- stop if they are not
if( (n.GT.100) .OR. (m.GT.20) ) then
  write(*,*) "Invalid input parameters!"
  write(*,*) "n cannot exceed 100, and m cannot exceed 20"
  stop
end if

! Step 3: Initialisation
!   Allocate memory for Fm
allocate(Fm( (-m):n ))
!   Initialise F(0) = F(1) = F(2) = 1
Fm(0:2)%v(1) = 1
!   Initialise F(3:n) = -1, to signify that we've yet to calculate their values
Fm(3:n)%v(1) = -1

! Step 4: Call the recursive function to perform the calculation
res = F(n,m)

! Step 5: write the result to the screen (need to write array elements in
!         reverse-order).
write(*,'(1000(i1))') (res%v(i), i=res%n,1,-1)

stop

contains

! Declare a recursive function that computes the Fibonacci-based sequence,
! where rabbits die after m months. This function uses memoisation to speed
! things up (considerably!!).
recursive function F(n,m) result(r)
implicit none
integer, value, intent(in) :: n, m
type(bigtype) :: r

! If Fm(n) hasn't been calculated already, calculate it using our addition
! and subtraction functions
if(Fm(n)%v(1).EQ.-1) Fm(n) = subtract( add(F(n-1,m),F(n-2,m)), F(n-m-1,m) )
! Grab the appropriate result from the memoisation array
r = Fm(n)

end function F

!
! Function for addition -- just standard high-school arithmetic
! r = x1 + x2
function add(x1, x2) result(r)
implicit none
type(bigtype) :: x1, x2, r
integer       :: maxn, i
integer       :: carry

! Initialise the carry-over to zero for the first iteration
carry = 0
! Calculate the maximum number of elements
maxn = max(x1%n,x2%n)
! Loop through the pairs of digits
do i=1,maxn
  r%v(i) = x1%v(i) + x2%v(i) + carry
  if(r%v(i).GE.10) then
    ! Value exceeds 10, so adjust and carry 1 to the next digit
    r%v(i) = r%v(i) - 10
    carry = 1
  else
    carry = 0
  end if
end do
! Calculate the number of digits in the result
if(carry.EQ.0) then
  r%n = maxn
else
  r%n = maxn + 1
  r%v(r%n) = 1
end if

end function add

!
! Function for subtraction -- again, just standard high-school arithmetic
!  r = x1 - x2
function subtract(x1, x2) result(r)
implicit none
type(bigtype) :: x1, x2, r
integer       :: i
integer       :: borrow

! Initialise borrow to zero for the first iteration
borrow = 0
! Loop through the pairs of digits
do i=1,x1%n
  if( (x1%v(i)-borrow) .LT. x2%v(i) ) then
    ! Borrow from the next digit
    r%v(i) = 10 + (x1%v(i)-borrow) - x2%v(i)
    ! Note the borrow for the next iteration
    borrow = 1
  else
    ! No need to borrow from the next digit
    r%v(i) = (x1%v(i)-borrow) - x2%v(i)
    ! Ensure that borrow is zero
    borrow = 0
  end if
end do
! Calculate the number of digits in the result
if(r%v(x1%n).EQ.0) then
  r%n = x1%n - 1
else
  r%n = x1%n
end if

end function subtract

end program fibd
