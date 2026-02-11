module funcs

contains

function int2str(k) result (out)
!   "Convert an integer to string."
   integer :: k
   character(len=20) :: out
   character(len=8) :: fmt ! format descriptor
   !fmt = '(I5.5)'
   !write (out, fmt) k
   write (out, *) k
   out = adjustl(out)
end function int2str

!
function real2str(r) result (out)
!   "Convert a real to string."
   real :: r
   character(len=20) :: out
   character(len=8) :: fmt ! format descriptor
   write (out,'(f13.3)') r
   out = adjustl(out)
end function real2str

!
function str2num(text) result (out)
   character(len=*) ::text
   real :: out

   if (len_trim(text)==0) then
      out=0.
   else
      !read(text,'(f16.6)') out
      read(text,*) out
   endif
end function

!
function str2num_2(text) result (out)
   character(len=*) ::text
   double precision :: out
   read(text,*) out
end function str2num_2



  
!----------------------------------------------------------------------------------------------

subroutine findstr(char_array,nrow,char_str,ifindstr,nfindstr)

implicit none

character(len=*),dimension(*) :: char_array
character(len=*) :: char_str
integer,dimension(*) :: ifindstr
integer :: nrow,nfindstr
integer :: i

nfindstr=0
do i=1,nrow
   if ( trim(char_array(i)) == trim(char_str) ) then
      nfindstr=nfindstr+1
      ifindstr(nfindstr)=i
   endif
enddo

end subroutine

!----------------------------------------------------------------------------------------------

subroutine unique_num(char_array,nrow,nunique)

implicit none

character(len=*),dimension(*) :: char_array
integer :: nrow,nunique
integer :: i,j,num
logical,dimension(:),allocatable :: mask
integer,dimension(:),allocatable :: indexSos 

num=nrow

ALLOCATE(mask(num))
mask = .TRUE.

DO i=num,2,-1  
   mask(i)=.NOT.(ANY(char_array(:i-1)==char_array(i)))  
END DO 

!print *,mask
nunique=size(pack([(i,i=1,num)],mask))

end subroutine

!----------------------------------------------------------------------------------------------

!----------------------------------------------------------------------------------------------

function myround(rinp,inum) result(rout)

implicit none

real :: rinp,rnum
integer :: inum
real :: rout

rnum = real(inum)
rout = int(rinp/rnum + 0.5)*rnum   ! 160415 지금까지 int를 빼먹었음.

end function

!----------------------------------------------------------------------------------------------


RECURSIVE SUBROUTINE quick_sort(list, order)

! Quick sort routine from:
! Brainerd, W.S., Goldberg, C.H. & Adams, J.C. (1990) "Programmer's Guide to
! Fortran 90", McGraw-Hill  ISBN 0-07-000248-7, pages 149-150.
! Modified by Alan Miller to include an associated integer array which gives
! the positions of the elements in the original order.

IMPLICIT NONE
REAL, DIMENSION (:), INTENT(IN OUT)  :: list
INTEGER, DIMENSION (:), INTENT(OUT)  :: order

! Local variable
INTEGER :: i

DO i = 1, SIZE(list)
  order(i) = i
END DO

CALL quick_sort_1(1, SIZE(list))

CONTAINS

RECURSIVE SUBROUTINE quick_sort_1(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j, itemp
REAL                :: reference, temp
INTEGER, PARAMETER  :: max_simple_sort_size = 6

IF (right_end < left_end + max_simple_sort_size) THEN
  ! Use interchange sort for small lists
  CALL interchange_sort(left_end, right_end)

ELSE
  ! Use partition ("quick") sort
  reference = list((left_end + right_end)/2)
  i = left_end - 1; j = right_end + 1

  DO
    ! Scan list from left end until element >= reference is found
    DO
      i = i + 1
      IF (list(i) >= reference) EXIT
    END DO
    ! Scan list from right end until element <= reference is found
    DO
      j = j - 1
      IF (list(j) <= reference) EXIT
    END DO


    IF (i < j) THEN
      ! Swap two out-of-order elements
      temp = list(i); list(i) = list(j); list(j) = temp
      itemp = order(i); order(i) = order(j); order(j) = itemp
    ELSE IF (i == j) THEN
      i = i + 1
      EXIT
    ELSE
      EXIT
    END IF
  END DO

  IF (left_end < j) CALL quick_sort_1(left_end, j)
  IF (i < right_end) CALL quick_sort_1(i, right_end)
END IF

END SUBROUTINE quick_sort_1


SUBROUTINE interchange_sort(left_end, right_end)

INTEGER, INTENT(IN) :: left_end, right_end

!     Local variables
INTEGER             :: i, j, itemp
REAL                :: temp

DO i = left_end, right_end - 1
  DO j = i+1, right_end
    IF (list(i) > list(j)) THEN
      temp = list(i); list(i) = list(j); list(j) = temp
      itemp = order(i); order(i) = order(j); order(j) = itemp
    END IF
  END DO
END DO

END SUBROUTINE interchange_sort

END SUBROUTINE quick_sort



end module

