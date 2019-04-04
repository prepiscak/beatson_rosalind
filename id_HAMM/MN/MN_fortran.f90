!
! This program reads in a text file containing two strings (one on each line),
! and calculates the Hamming distance between the two strings (that is, the
! number of corresponding characters that are not equal to each other).
!
! Compile using your favourite compiler. I like gfortran:
!
! gfortran -O3 -funroll-loops -march=native -ftree-vectorize MN_fortran.f90 -o MN_fortran
!
! This will take MN_fortran.f90, and create an executable binary
! named "MN_fortran". This binary is then run using the following
! command:
!
! ./MN_fortran datfile.txt
!
! where "datfile.txt" is the data file containing the two strings for
! which we wish to calculate the Hamming distance.
!
! Output is the Hamming distance (an integer).

program ham
! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=5000) :: line1,line2
integer :: iunit, io, i, len, hamm

! Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
! If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
! Open the file connection (read unformatted, because it's faster)
open(newunit=iunit, file=filename)
! Read the first and second lines
read(iunit, *, iostat=io) line1
read(iunit, *, iostat=io) line2
! Get the length of the first line (assume that it is equal to the length of the second line -- could easily check this)
len = LEN_TRIM(line1)

! Initialise ham to zero
hamm = 0
! Count the number of characters that are the not equal to each other
do i=1,len
  if ( line1(i:i) .NE. line2(i:i) ) hamm = hamm + 1
end do

! Write ham to the screen
write(*,*) hamm

stop

end program
