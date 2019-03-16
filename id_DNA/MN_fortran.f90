!
! This program reads in a text file containing only the characters
! A, C, G and T, and counts the number of occurrences of each one.
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
! where "datfile.txt" is the data file containing the string we
! want to analyse.
!
! Output is four integer counts, corresponding to A, C, G and T,
! respectively.
!

! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=:),allocatable :: str
integer :: iunit, istat, filesize, i, A, C, G, T

! Step 1: Dynamically get the file-name and read the data into
!         a character variable (this is the annoying bit!)
!
! Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
! If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
! Open the file connection (read unformatted, because it's faster)
open(newunit=iunit, file=filename, status='OLD', &
     form='UNFORMATTED', access='STREAM', iostat=istat)
! If the open command threw an error, stop
if (istat .NE. 0) then
  write(*,*) 'Error opening file.'
  stop
end if
! Get the file size
inquire(file=filename, size=filesize)
! Allocate memory for the character variable
allocate( character(len=filesize) :: str )
! Read the data into the character variable
read(iunit, pos=1, iostat=istat) str
! Remove the trailing termination character
do i=filesize,1,-1
  if ( (str(i:i) .EQ. "A") .OR. &
       (str(i:i) .EQ. "C") .OR. &
       (str(i:i) .EQ. "G") .OR. &
       (str(i:i) .EQ. "T") ) exit
end do
filesize = i

! Step 2: Count the occurrences of each character
!         (this is the easy/fun bit!)
!
! Initialise the counts for each character
A = 0
C = 0
G = 0
T = 0
! Cycle through each character and increment each count as required
do i=1,filesize
  if ( str(i:i) .EQ. "A" ) then
    A = A + 1
  else if ( str(i:i) .EQ. "C" ) then
    C = C + 1
  else if ( str(i:i) .EQ. "G" ) then
    G = G + 1
  else
    T = T + 1
  end if
end do
! Write the output to the screen
! (4 lots of integers, each followed by exactly one space)
write(*,'(4(I0,X))') A, C, G, T

stop
end
