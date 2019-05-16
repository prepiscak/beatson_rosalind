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
! In this program, we're using a simple do loop to solve the problem.
!
! Compile using your favourite compiler. I like gfortran:
!
! gfortran -O3 -funroll-loops -march=native -ftree-vectorize fibd_iterative.f90 -o fibd_iterative
!
! This will take fibd_iterative.f90, and create an executable binary
! named "fibd_iterative". This binary is then run using the following
! command:
!
! ./fibd_iterative datfile.txt
!
! where "datfile.txt" is the data file containing the n and m parameters
! that are used to calculate the Fibonacci-based result.
!
! Output is the (integer) result of F(n).
!
program fibd

! Ensure that we don't have any undeclared variables
implicit none

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
   
! Declare variables
character(len=64) :: filename
integer :: iunit, i
integer :: n, m
type(bigtype), dimension(:), allocatable :: F

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

! Step 3: allocate memory for F, which will contain our Fibonacci-based sequence.
! N.B. Could have hard-coded -20 and 100 for the lower and upper bounds at the
!      time of declaration, since the spec states limits for m and n, but doing
!      it dynamically feels a bit clearer to me.
allocate(F((-m):n))

! Step 4: initialise F(0) = F(1) = F(2) = 1.
F(0:2)%v(1) = 1

! Step 5: calculate the Fibonacci-based sequence from 3:n, using our addition
!         and subtraction functions.
!   F(i) = F(i-1) + F(i-2) - F(i-m-1)
do i=3,n
  F(i) = subtract( add(F(i-1),F(i-2)), F(i-m-1) )
end do

! Step 6: write the result to the screen (need to write array elements in
!         reverse-order).
write(*,'(1000(i1))') (F(n)%v(i), i=F(n)%n,1,-1)

stop

contains

!
! Function for addition -- just standard high-school arithmetic
! res = x1 + x2
function add(x1, x2) result(res)
implicit none
type(bigtype) :: x1, x2, res
integer       :: maxn, i
integer       :: carry

! Initialise the carry-over to zero for the first iteration
carry = 0
! Calculate the maximum number of elements
maxn = max(x1%n,x2%n)
! Loop through the pairs of digits
do i=1,maxn
  res%v(i) = x1%v(i) + x2%v(i) + carry
  if(res%v(i).GE.10) then
    ! Value exceeds 10, so adjust and carry 1 to the next digit
    res%v(i) = res%v(i) - 10
    carry = 1
  else
    carry = 0
  end if
end do
! Calculate the number of digits in the result
if(carry.EQ.0) then
  res%n = maxn
else
  res%n = maxn + 1
  res%v(res%n) = 1
end if

end function add

!
! Function for subtraction -- again, just standard high-school arithmetic
!  res = x1 - x2
function subtract(x1, x2) result(res)
implicit none
type(bigtype) :: x1, x2, res
integer       :: i
integer       :: borrow

! Initialise borrow to zero for the first iteration
borrow = 0
! Loop through the pairs of digits
do i=1,x1%n
  if( (x1%v(i)-borrow) .LT. x2%v(i) ) then
    ! Borrow from the next digit
    res%v(i) = 10 + (x1%v(i)-borrow) - x2%v(i)
    ! Note the borrow for the next iteration
    borrow = 1
  else
    ! No need to borrow from the next digit
    res%v(i) = (x1%v(i)-borrow) - x2%v(i)
    ! Ensure that borrow is zero
    borrow = 0
  end if
end do
! Calculate the number of digits in the result
if(res%v(x1%n).EQ.0) then
  res%n = x1%n - 1
else
  res%n = x1%n
end if

end function subtract

end program fibd
