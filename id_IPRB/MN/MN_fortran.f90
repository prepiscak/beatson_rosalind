!
! This program reads in a text file containing three integers: k, m and n,which
! correspond to homozygous dominant, heterozygous, and homozygous recessive
! organisms respectively, and calculates the probability that two randomly
! selected mating organisms will produce an individual possessing a dominant
! allele. It is assumed that any two organisms can mate.
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
! where "datfile.txt" is the data file containing the three integers.
!
! Output is the probability that two randomly selected mating organisms will
! produce an individual possessing a dominant allele.

program iprb
! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
integer :: iunit, io, k, m, n, tot
real(kind=8) :: p1, p2, p3, prob

! Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
! If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
! Open the file connection (read unformatted, because it's faster)
open(newunit=iunit, file=filename)
! Read the three integers
read(iunit, *, iostat=io) k, m, n

! Calculate total
 tot = k + m + n

! Case 1: k chosen first (doesn't matter what's chosen second)
 p1 = DBLE(k)/tot

! Case 2: m chosen first
!  (i) k chosen second
 p2 = DBLE(k)/(tot-1)
! (ii) n chosen second
 p2 = p2 + n*0.5D0/(tot-1)
!(iii) m chosen second
 p2 = p2 + (m-1)*0.75D0/(tot-1)
! Multiply p2 by probability of m being chosen first
 p2 = m*p2/tot

! Case 3: n chosen first
!  (i) k chosen second
 p3 = DBLE(k)/(tot-1)
! (ii) m chosen second
 p3 = p3 + m*0.5D0/(tot-1)
!(iii) probability is zero if n is chosen second
! Multiply p3 by probability of n being chosen first
 p3 = n*p3/tot

! Total probability is the sum of the three sub-probabilities
 prob = p1 + p2 + p3

! Write ham to the screen
write(*,*) prob

stop

end program
