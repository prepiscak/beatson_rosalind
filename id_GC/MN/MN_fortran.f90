!
! This program reads in a text file containing DNA strings in Rosalind's
! FASTA format, calculates the GC-content of each DNA string, and returns
! the 13-character Rosalind id and GC-content of the DNA string with the
! highest GC-content.
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
! GC-contents we wish to calculate.
!
! Output is the 13-character Rosalind id of the DNA string with the
! highest GC-content (on the first line) and its associated GC-content
! as a percentage (on the second line).
!
! Note that, as we read-in the input file, we only keep track of the
! Rosalind id and the GC content of the DNA string with the highest
! GC content. Once a DNA string has been read in, we compare its GC
! content with that of the previous maximum and update accordingly.
! This is more efficient than reading everything in and then 
! selecting the DNA string with the maximum GC content.

program gc_cont
! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=13) :: id_cur, id_max
character(len=5000) :: line
integer :: iunit, io, i, l, gc, dna_len
real ( kind = 8 ) :: gc_cur, gc_max

! Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
! If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
! Open the file connection (read unformatted, because it's faster)
open(newunit=iunit, file=filename)

! Initialise gc_max, gc and dna_len for the first iteration
gc_max = -1D0
gc = 0
dna_len = 1
! Cycle through each line of the input file
do
  ! Read the current line into the 'line' variable
  read(iunit, *, iostat=io) line
  if (io .NE. 0) then
    ! End of file: update gc_max and id_max, then terminate the loop
    gc_cur = DBLE(gc)/dna_len
    if ( gc_cur .GT. gc_max ) then
      gc_max = gc_cur
      id_max = id_cur
    end if
    exit
  else if (line(1:1) .EQ. ">") then
    ! New DNA string: update gc_max and id_max
    gc_cur = DBLE(gc)/dna_len
    if ( gc_cur .GT. gc_max ) then
      gc_max = gc_cur
      id_max = id_cur
    end if
    ! Read the 13-character id, and reinitialise gc and dna_len
    id_cur = line(2:14)
    gc = 0
    dna_len = 0
  else
    ! Get the length of the current line
    l = LEN_TRIM(line)
    ! Count the number of Cs and Gs
    do i=1,l
      if ( (line(i:i) .EQ. "C") .OR. (line(i:i) .EQ. "G") ) gc = gc + 1
    end do
    ! Update the length of the current DNA string
    dna_len = dna_len + l
  end if
end do

! Write the id and the GC content (as a percentage) of the DNA string
! with the largest GC content
write(*,'(A13)') id_max
write(*,'(F9.6)') 100*gc_max

stop

end program
