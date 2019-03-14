!
! This program reads in a text file comprising a string, and replaces
! every instance of "T" with "U".
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
! Output is the string, with all instances of "T" replaced with "U".
!

! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=:),allocatable :: str
integer :: iunit, istat, filesize, i

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

! Step 2: Replace all instances of "T" with "U" (the easy/fun bit!)
!
! Cycle through each character and increment each count as required
do i=1,filesize
  if ( str(i:i) .EQ. "T" ) str(i:i) = "U"
end do
! Write the output to the screen (unformatted, since it's a single string)
write(*,*) str

stop
end
