!
! This program reads in a two-line text file containing two strings (one on
! each line), and calculates the location(s) of the substring on the second
! line within the string on the first line.
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
! where "datfile.txt" is the data file containing the two strings.
!
! Output is the locations of t (the substring on the second line of the input
! file) within s (the string on the first line of the input file).

program subs
! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=10000) :: s, t
integer :: iunit, io, i, s_len, t_len, pos, n, lo
integer, dimension(10000) :: locations

! Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
! If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
! Open the file connection
open(newunit=iunit, file=filename)
! Read s (the first line of text)
read(iunit, *, iostat=io) s
! Read t (the second line of text)
read(iunit, *, iostat=io) t
! Get the length of s
s_len = LEN_TRIM(s)
! Get the length of t
t_len = LEN_TRIM(t)

! Initialise n (the number of locations that have been detected) to zero
n = 0
! Initialise the lower search-index of s to 1
lo = 1
! Loop until we have found every location of t within s
do
  ! Find the first location of t within s(lo:s_len)
  pos = INDEX(s(lo:s_len), t(1:t_len))
  ! Exit the loop if no more locations exist
  if ( pos .EQ. 0 ) exit
  ! Add the newly-found location to our locations output array
  n = n + 1
  locations(n) = lo + pos - 1
  ! Update the lower search-index of s
  lo = lo + pos
end do

! Write the locations to the screen (separate each location with a single space)
write(*,'(10000(1x,i0))') (locations(i), i=1,n)

stop

end program
