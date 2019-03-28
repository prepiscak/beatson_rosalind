!
! This program reads in a text file containing only the characters
! A, C, G and T, and calculates its reverse-complement.
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
! where "datfile.txt" is the data file containing the string whose
! reverse-complement we want to calculate.
!
! Output is a string containing the reverse-complement of the string
! contained in "datfile.txt".
!

! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=:),allocatable :: str_in, str_out
integer :: iunit, istat, filesize, i, j

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
! Allocate memory for the input character variable
allocate( character(len=filesize) :: str_in )
! Read the data into the input character variable
read(iunit, pos=1, iostat=istat) str_in
! Remove the trailing termination character
do i=filesize,1,-1
  if ( (str_in(i:i) .EQ. "A") .OR. &
       (str_in(i:i) .EQ. "C") .OR. &
       (str_in(i:i) .EQ. "G") .OR. &
       (str_in(i:i) .EQ. "T") ) exit
end do
filesize = i

! Step 2: Compute the reverse-complement of str_in
!
! Allocate memory for the output character variable
allocate( character(len=filesize) :: str_out )
! Cycle through each character and build-up the reverse-complement
do i=1,filesize
  j = filesize+1-i
  if ( str_in(i:i) .EQ. "A" ) then
    str_out(j:j) = "T"
  else if ( str_in(i:i) .EQ. "C" ) then
    str_out(j:j) = "G"
  else if ( str_in(i:i) .EQ. "G" ) then
    str_out(j:j) = "C"
  else
    str_out(j:j) = "A"
  end if
end do
! Write the output to the screen
write(*,'(A)') str_out

stop
end
