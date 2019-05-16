!
! This program reads in a text file containing DNA strings in Rosalind's
! FASTA format, calculates the profile matrix and consensus string, and
! then writes the consensus string followed by the profile matrix.
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
! where "datfile.txt" is the data file containing the DNA strings whose
! consensus string and profile matrix we wish to calculate.
!
! Output is the consensus string and profile matrix for the collection.
!

program cons
! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=4), parameter :: letters = "ACGT"
character(len=5000) :: line, consensus
integer :: iunit, io, i, j, l, cur
integer, dimension(4,10000) :: counts = 0
integer, dimension(10000) :: maxes

! Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
! If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
! Open the file connection (read unformatted, because it's faster)
open(newunit=iunit, file=filename)

! Cycle through each line of the input file
do
  ! Read the current line into the 'line' variable
  read(iunit, *, iostat=io) line
  if (io .NE. 0) then
    ! End of file: terminate the loop
    exit
  else if (line(1:1) .EQ. ">") then
    ! New DNA string: reset cur
    cur = 0
  else
    ! Get the length of the current line
    l = LEN_TRIM(line)
    ! For each letter, add one to the appropriate row of counts
    do i=1,l
      cur = cur + 1
      if ( line(i:i) .EQ. letters(1:1) ) then
        counts(1,cur) = counts(1,cur) + 1
      else if ( line(i:i) .EQ. letters(2:2) ) then
        counts(2,cur) = counts(2,cur) + 1
      else if ( line(i:i) .EQ. letters(3:3) ) then
        counts(3,cur) = counts(3,cur) + 1
      else
        counts(4,cur) = counts(4,cur) + 1
      end if
    end do
  end if
end do

! Calculate the consensus string
maxes(1:cur) = MAXLOC(counts(1:4,1:cur),1)
do i=1,cur
  consensus(i:i) = letters(maxes(i):maxes(i))
end do

! Write the consensus string to the screen
write(*,'(A)') consensus(1:cur)
! Write the profile matrix
do j=1,4
  write(*,'(A1,A1,10000(1x,i0))') letters(j:j),":",(counts(j,i),i=1,cur)
end do

stop

end program
