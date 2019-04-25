!
! This program reads in a one-line text file containing an RNA string, and encodes
! it into a protein string using a look-up table.
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
! where "datfile.txt" is the data file containing the RNA string.
!
! Output is the encoded protein string.

program prot
! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=100000) :: line, out
character(len=1) :: x
integer :: iunit, io, i, len, n, lo, hi, f

! Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
! If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
! Open the file connection (read unformatted, because it's faster)
open(newunit=iunit, file=filename)
! Read the line of text
read(iunit, *, iostat=io) line
! Get the length of the line
len = LEN_TRIM(line)

! Initialise n (the length of the protein string) to zero
n = 0
! Loop through each triplet of RNA characters
do i=1,(len/3)
  hi = 3*i
  lo = hi-2
  ! encode the current triplet of RNA characters, giving a single protein character
  call encode_rna(line(lo:hi),x,f)
  if ( f .EQ. 0 ) then
    ! if the stopping-flag is zero, add the encoded protein character to our output string
    n = n + 1
    out(n:n) = x
  else
    ! if the stopping-flag is non-zero, stop
    exit
  end if
end do

! Write the encoded protein string to the screen
write(*,*) out(1:n)

stop

contains

! Subroutine containing the look-up table for encoding three RNA characters
! as a protein character:
!    c : the three RNA characters we wish to encode
!  out : the encoded protein character
!    f : stopping flag, for whether or not we need to stop (1 => stop; 0 => continue)
subroutine encode_rna(c,out,f)
implicit none
integer :: f
character(len=3) :: c
character(len=1) :: out

! initialise stopping flag to zero
f = 0

! Perform the encoding by checking each of the three RNA characters
if ( c(1:1) .EQ. "U" ) then
  if ( c(2:2) .EQ. "U" ) then
    if ( (c(3:3) .EQ. "U") .OR. (c(3:3) .EQ. "C") ) then
      out = "F"
    else
      ! c(3:3) = "A" or c(3:3) = "G"
      out = "L"
    end if
  else if ( c(2:2) .EQ. "C" ) then
    out = "S"
  else if ( c(2:2) .EQ. "A" ) then
    if ( (c(3:3) .EQ. "U") .OR. (c(3:3) .EQ. "C") ) then
      out = "Y"
    else
      ! c(3:3) = "A" or c(3:3) = "G"
      f = 1
    end if
  else
    ! c(2:2) = "G"
    if ( (c(3:3) .EQ. "U") .OR. (c(3:3) .EQ. "C") ) then
      out = "C"
    else if ( c(3:3) .EQ. "A" ) then
      f = 1
    else
      ! c(3:3) = "G"
      out = "W"
    end if
  end if
else if ( c(1:1) .EQ. "C" ) then
  if ( c(2:2) .EQ. "U" ) then
    out = "L"
  else if ( c(2:2) .EQ. "C" ) then
    out = "P"
  else if ( c(2:2) .EQ. "A" ) then
    if ( (c(3:3) .EQ. "U") .OR. (c(3:3) .EQ. "C") ) then
      out = "H"
    else
      ! c(3:3) = "A" or c(3:3) = "G"
      out = "Q"
    end if
  else
    ! c(2:2) = "G"
    out = "R"
  end if
else if ( c(1:1) .EQ. "A" ) then
  if ( c(2:2) .EQ. "U" ) then
    if ( c(3:3) .EQ. "G" ) then
      out = "M"
    else
      ! c(3:3) = "U" or c(3:3) = "C" or c(3:3) = "A"
      out = "I"
    end if
  else if ( c(2:2) .EQ. "C" ) then
    out = "T"
  else if ( c(2:2) .EQ. "A" ) then
    if ( (c(3:3) .EQ. "U") .OR. (c(3:3) .EQ. "C") ) then
      out = "N"
    else
      ! c(3:3) = "A" or c(3:3) = "G"
      out = "K"
    end if
  else
    ! c(2:2) = "G"
    if ( (c(3:3) .EQ. "U") .OR. (c(3:3) .EQ. "C") ) then
      out = "S"
    else
      ! c(3:3) = "A" or c(3:3) = "G"
      out = "R"
    end if
  end if
else
  ! c(1:1) = "G"
  if ( c(2:2) .EQ. "U" ) then
    out = "V"
  else if ( c(2:2) .EQ. "C" ) then
    out = "A"
  else if ( c(2:2) .EQ. "A" ) then
    if ( (c(3:3) .EQ. "U") .OR. (c(3:3) .EQ. "C") ) then
      out = "D"
    else
      ! c(3:3) = "A" or c(3:3) = "G"
      out = "E"
    end if
  else
    ! c(2:2) = "G"
    out = "G"
  end if
end if

end subroutine

end program
