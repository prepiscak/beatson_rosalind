!
! This program reads in a text file containing at most 100 DNA strings (each
! of at most 1000 kbp) in Rosalind's FASTA format, and returns the longest
! substring that is common to all DNA strings.
!
! This is a parallel implementation that makes use of OpenMP for multi-threading.
!
! Compile using your favourite compiler. I like gfortran:
!
! gfortran -O3 -funroll-loops -march=native -ftree-vectorize -fopenmp lcsm_mp.f90 -o lcsm_mp
!
! This will take lcsm.f90, and create an executable binary named "clsm".
! This binary is then run using the following command:
!
! ./lcsm datfile.txt
!
! where "datfile.txt" is the data file containing the DNA strings for which
! we wish to find the longest common substring.
!
! Output is the longest common substring.
!
! The basic algorithm is as follows:
!
! (1) We first find the shortest DNA string (since it's impossible for the
!     longest common substring to be longer than the shortest DNA string).
! (2) First loop: We then create a window of characters (initially equal to
!     the number of characters in the shortest DNA string, and decrementing
!     by 1 with each iteration)...
! (3) Second loop: Starting from the first character of the shortest DNA
!     string, we shift the window one place to the right with each iteration
!     until the right-hand edge of the window reaches the final character of
!     the shortest DNA string. The characters encompassed by the window are
!     our test substring.
! (4) We then check this test substring against each DNA string. If any DNA
!     string does *not* contain the test substring, then the substring isn't
!     common to all DNA strings -- so move on to the next iteration of the
!     second loop. If all DNA strings contain the test substring, then we
!     have found a longest common substring.
!
! Here's an example of the moving window, assuming that the shortest DNA
! string comprises eight characters...
!
! First iteration  (w=8, p=1):
!    [ x x x x x x x x ]
!
! Second iteration (w=7, p=1):
!    [ x x x x x x x - ]
! Third iteration  (w=7, p=2):
!    [ - x x x x x x x ]
!
! Fourth iteration (w=6, p=1):
!    [ x x x x x x - - ]
! Fifth iteration  (w=6, p=2):
!    [ - x x x x x x - ]
! Sixth iteration  (w=6, p=3):
!    [ - - x x x x x x ]
!
! ...etc
!

program lcsm
! Use the openMP library for multi-threading
use omp_lib
! Ensure that we don't have any undeclared variables
implicit none

! Declare variables
character(len=64) :: filename
character(len=5000) :: line, substring
character(len=5000), dimension(100) :: strings
integer :: iunit, io, n, l, n_short, i, j, s, pos
logical :: found = .FALSE.
integer, dimension(100) :: lengths = 0

! Get the input filename (the first argument passed from the command line)
call getarg(1,filename)
! If the number of input arguments is less than 1, stop with an error
if ( iargc() .NE. 1 ) then
  write(*,*) "No input file!"
  stop
end if
! Open the file connection
open(newunit=iunit, file=filename)

! Initialise the number of strings to 0 (since we haven't read any yet!)
n = 0
! Cycle through each line of the input file
do
  ! Read the current line into the 'line' variable
  read(iunit, *, iostat=io) line
  if (io .NE. 0) then
    ! End of file: terminate the loop
    exit
  else if (line(1:1) .EQ. ">") then
    ! New DNA string: increment n by 1
    n = n+1
  else
    ! Get the length of the current line
    l = LEN_TRIM(line)
    ! Append the current line to the current string
    strings(n)( (lengths(n)+1):(lengths(n)+l) ) = line(1:l)
    ! Update the vector of lengths for the nth element
    lengths(n) = lengths(n) + l
  end if
end do

! Close the file connection
close(iunit)

! Get the shortest string
n_short = MINLOC(lengths(1:n),1)
! Get the length of the shortest string
l = lengths(n_short)

! Find the longest common substring
! width of moving window is (l+1-i)
width: do i=1,l
  ! Starting position (left-hand side of the window, runs from 1 to i)
  !$OMP PARALLEL DO SHARED(n,n_short,strings,lengths,i,l,substring,found), DEFAULT(PRIVATE)
  start: do j=1,i
    ! If any thread has found a common substring, repeatedly cycle this loop (essentially the same as exiting)
    if ( found ) cycle start
    ! Cycle through each string
    string: do s=1,n
      if ( found ) exit string
      ! Skip the iteration when s is the shortest string (the substring is obviously present in the shortest string!)
      if ( s .EQ. n_short ) cycle string
      ! Find the first location of the substring within s
      pos = INDEX(strings(s)(1:lengths(s)),strings(n_short)( j:(l+j-i) ))
      ! If the substring isn't present, then it isn't a common substring -- so proceed to the next iteration of the 'start' loop
      if ( pos .EQ. 0 ) cycle start
    end do string
    ! We can only reach this point when the substring is common to all strings, so store the substring and exit the 'width' loop
    ! If another thread has found a common substring, cycle
    if ( found ) cycle start
    ! Mark that a common substring has been found (to notify other threads) and store it
    found = .TRUE.
    ! Use critical construct to avoid a race condition (i.e. multiple threads writing to the same string simultaneously)
    !$OMP CRITICAL
    substring(1:(l+1-i)) = strings(n_short)( j:(l+j-i) )
    !$OMP END CRITICAL
  end do start
  !$OMP END PARALLEL DO
  if ( found ) exit width
end do width

! Write the longest common substring to the screen
write(*,'(A)') substring(1:(l+1-i))

stop

end program lcsm
