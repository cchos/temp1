
module globy
implicit none

integer,parameter :: MAXLINE = 4096
integer,parameter :: MAXBRAN = 256
integer,parameter :: MAXCHILD = 24
integer,parameter :: NATTR = 50,NPOS = 6
character(len=:),allocatable :: sheet_read(:,:)
character(len=:),allocatable :: PDMS_info1(:,:),PDMS_info2(:,:)
character(len=:),allocatable :: dsheet(:,:)
character(len=:),allocatable :: spref_read(:,:),spref_VDAMP(:,:),spref_FDAMP(:,:),spref_FANEQ(:,:),spref_VENTU(:,:),spref_ROOMU(:,:),spref_MISCL(:,:),spref_PIPED(:,:),spref_ETC(:,:)
integer :: nrow_ETC,ncol_ETC
integer :: nrow_VDAMP,ncol_VDAMP
integer :: nrow_FDAMP,ncol_FDAMP
integer :: nrow_FANEQ,ncol_FANEQ
integer :: nrow_VENTU,ncol_VENTU
integer :: nrow_ROOMU,ncol_ROOMU
integer :: nrow_MISCL,ncol_MISCL
integer :: nrow_PIPED,ncol_PIPED
integer :: nrow_sheet_read,ncol_sheet_read
integer :: nnode_tree,iroot_tree,isupp_tree,iextr_tree,nnode_supp_tree,nnode_extr_tree
integer,parameter :: IDXF1=60,IDXF2=61
character(len=512) :: AMserver_path,AMlocaltemp_path,INIT_MASTER_file,AMlibrary_path,VDAMP_file,FDAMP_file,FANEQ_file,VENTU_file,ROOMU_file,MISCL_file,PIPED_file,ETC_file
integer,parameter :: IMAX_SPREF=1000, JMAX_SPREF=30

integer :: twork_nChild(MAXLINE)
integer :: twork_iChilds(MAXLINE,MAXCHILD)
integer :: twork_iBrothers(MAXLINE,MAXCHILD-1)
integer :: twork_nBrother(MAXLINE)
integer :: twork_iParent(MAXLINE)
integer :: twork_iFan,twork_iFanInlet,twork_iFanOutlet
integer :: twork_nMaxLevel
integer :: twork_yDiffuser(MAXLINE)
integer :: twork_ID(MAXLINE)
integer :: twork_FlowDir(MAXLINE)
integer :: twork_LevelS2(MAXLINE)
integer :: twork_Level(MAXLINE)
integer :: twork_BranID(MAXLINE)
integer :: twork_BLevel(MAXLINE)
integer :: twork_BelemID(MAXLINE)
integer :: nbran_tree

integer :: bwork_nChild(MAXBRAN)
integer :: bwork_FlowDir(MAXBRAN)
integer :: bwork_bChilds(MAXBRAN,MAXCHILD)
integer :: bwork_bParent(MAXBRAN)
integer :: bwork_iParent(MAXBRAN)
integer :: bwork_iSta(MAXBRAN)
integer :: bwork_iEnd(MAXBRAN)
integer :: bwork_NumElem(MAXBRAN)
integer :: bwork_Level(MAXBRAN)

integer :: idx_sort_supp(MAXLINE)
integer :: idx_sort_extr(MAXLINE)
integer :: idx_sort_all(MAXLINE)

end module

!----------------------------------------------------------------------------------------------
    
program temp000
use globy
implicit none

integer :: i,j,k

call init()
call init_spref()
call import_csv_PDMS()
call trim_csv_PDMS()
call arrange_PDMS()

call trim_PDMS_info2()

! 150716 sort tree
call trim_twork_iChilds()
!print *,'trim_twork_iChilds end'
call sort_tree()
!print *,'sort_tree end'
call make_bwork()
!print *,'make_bwork end'


call export_tree()
!print *,'export_tree end'

call make_dsheet()
!print *,'make_dsheet end'

call export_dsheet()
!print *,'export_dsheet end'

call export_tree_expand()

write(*,*)
write(*,*) 'Successful End.' 

!write(*,*) 'Press Enter to continue' 
!read(*,*)

!pause

end program

!----------------------------------------------------------------------------------------------

subroutine init()
use globy
implicit none

integer :: fid=100,fid2=200
integer :: i,io_status,io
character(len=512) :: cline_read

write(*,*) '------------------------------------------'
write(*,*) '    HHI Duct Analysis Pre-Processor      '
!write(*,*) '        by  KSOE Dynamics   Research Dept.  '
write(*,*) '        by HHI Naval Research Dept.  '
write(*,*) '        Version v1.00_191205  '
write(*,*) '------------------------------------------'
write(*,*)

!write(*,*) 'Initialization ...'

INIT_MASTER_file = 'C:\temp\hhiducta\hhiducta.ini'
!-- File Exist Test
open(unit=fid,file=INIT_MASTER_file,iostat=io_status,form='FORMATTED')
if (io_status /= 0) then
   write (*, *) 'File Open Error'
   stop
endif
close(fid)



!--- AM path 1
AMlocaltemp_path=''
!AMlocaltemp_path = 'C:\temp\hhiducta'
open(unit=fid,file=INIT_MASTER_file,iostat=io_status,form='FORMATTED')
do i=1,1000
   read(fid,"(a)",iostat=io) cline_read
   if (io > 0 .or. io < 0) then
      exit
   else
      if (trim(cline_read)=='[LOCALTEMPPATH]') then
         read(fid,"(a)") AMlocaltemp_path
         !write(*,"(a)") ' LocalTemp : '//trim(AMlocaltemp_path)
         exit
      endif
   endif
end do
close(fid)





!--- AM path 2
AMserver_path=''
open(unit=fid,file=INIT_MASTER_file,iostat=io_status,form='FORMATTED')
do i=1,1000
   read(fid,"(a)",iostat=io) cline_read
   if (io > 0 .or. io < 0) then
      exit
   else
      if (trim(cline_read)=='[SERVERPATH]') then
         read(fid,"(a)") AMserver_path
         !write(*,"(a)") ' ServerExe : '//trim(AMserver_path)
         exit
      endif
   endif
end do
close(fid)

!--- AM path 3
AMlibrary_path=''
open(unit=fid,file=INIT_MASTER_file,iostat=io_status,form='FORMATTED')
do i=1,1000
   read(fid,"(a)",iostat=io) cline_read
   if (io > 0 .or. io < 0) then
      exit
   else
      if (trim(cline_read)=='[LIBRARYPATH]') then
         read(fid,"(a)") AMlibrary_path
         !write(*,"(a)") ' ServerLib : '//trim(AMlibrary_path)
         exit
      endif
   endif
end do
close(fid)





!--- LIBRARY path
if (trim(AMlibrary_path) .ne. '') then
   ETC_file=trim(AMlocaltemp_path)//'\spref_etc.dat'
   ! VDAMP_file=trim(AMlocaltemp_path)//'\spref_vdamp.csv'
   !FDAMP_file=trim(AMlocaltemp_path)//'\spref_fdamp.csv'
   !FANEQ_file=trim(AMlocaltemp_path)//'\spref_faneq.csv'
   !VENTU_file=trim(AMlocaltemp_path)//'\spref_ventu.csv'
   !ROOMU_file=trim(AMlocaltemp_path)//'\spref_roomu.csv'
   !MISCL_file=trim(AMlocaltemp_path)//'\spref_miscl.csv'
   !PIPED_file=trim(AMlocaltemp_path)//'\spref_piped.csv'
else
    ETC_file=''
   !VDAMP_file=''
   !FDAMP_file=''
   !FANEQ_file=''
   !VENTU_file=''
   !ROOMU_file=''
   !MISCL_file=''
   !PIPED_file=''
endif

!write(*,"(a)") ' VDAMP_file   : '//trim(VDAMP_file)
!write(*,"(a)") ' FDAMP_file   : '//trim(FDAMP_file)
!write(*,"(a)") ' FANEQ_file   : '//trim(FANEQ_file)


!-- Initialization
!allocate(character(len=128) :: sheet_read(MAXLINE,3+50+6+4))   ! 3은 기본, 50은 user, 6은 position, 4는 flow
allocate(character(len=128) :: sheet_read(MAXLINE,3+50+6+6))   ! 3은 기본, 50은 user, 6은 position, 6은 flow
allocate(character(len=64) :: PDMS_info1(MAXLINE,12+MAXCHILD))
!allocate(character(len=64) :: PDMS_info2(MAXLINE,50+6+4))        ! 50은 user, 6은 position, 4는 flow
allocate(character(len=64) :: PDMS_info2(MAXLINE,50+6+6))        ! 50은 user, 6은 position, 6은 flow
allocate(character(len=64) :: dsheet(MAXLINE,2+MAXCHILD+50+6))   ! 개수 수정해야 할듯... MAXCHILD와 무관 
allocate(character(len=64) :: spref_read(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427
allocate(character(len=64) :: spref_ETC(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427
!allocate(character(len=64) :: spref_VDAMP(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427
!allocate(character(len=64) :: spref_FDAMP(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427
!allocate(character(len=64) :: spref_FANEQ(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427
!allocate(character(len=64) :: spref_VENTU(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427
!allocate(character(len=64) :: spref_ROOMU(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427
!allocate(character(len=64) :: spref_MISCL(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427
!allocate(character(len=64) :: spref_PIPED(IMAX_SPREF,JMAX_SPREF))   ! 개수 적당히 잡았음. 160427

sheet_read(:,:)=''
PDMS_info1(:,:)=''
PDMS_info2(:,:)=''
dsheet(:,:)=''

iroot_tree=0
nnode_tree=0

end subroutine


!----------------------------------------------------------------------------------------------

subroutine init_spref()
use globy
use strings
implicit none

integer :: i,j
integer :: nrow_TEMP,ncol_TEMP
character(len=64) :: spref_TEMP(IMAX_SPREF,JMAX_SPREF)

call read_spref(ETC_file,spref_ETC,nrow_ETC,ncol_ETC)
!call read_spref(VDAMP_file,spref_VDAMP,nrow_VDAMP,ncol_VDAMP)
!call read_spref(FDAMP_file,spref_FDAMP,nrow_FDAMP,ncol_FDAMP)
!call read_spref(FANEQ_file,spref_FANEQ,nrow_FANEQ,ncol_FANEQ)
!call read_spref(VENTU_file,spref_VENTU,nrow_VENTU,ncol_VENTU)
!call read_spref(ROOMU_file,spref_ROOMU,nrow_ROOMU,ncol_ROOMU)
!call read_spref(MISCL_file,spref_MISCL,nrow_MISCL,ncol_MISCL)
!call read_spref(PIPED_file,spref_PIPED,nrow_PIPED,ncol_PIPED)



!if (.false.) then
!    
!print *, trim(PIPED_file),nrow_PIPED
!spref_TEMP=spref_PIPED
!ncol_TEMP=ncol_PIPED
!do i=1,ncol_TEMP
!   print *, i,trim(spref_TEMP(i,1)),', ',trim(spref_TEMP(i,2)),', ',trim(spref_TEMP(i,3)),', ',trim(spref_TEMP(i,4)),', ',trim(spref_TEMP(i,5)),', ',trim(spref_TEMP(i,6))
!enddo
!
!    
!print *, trim(VDAMP_file),nrow_VDAMP
!spref_TEMP=spref_VDAMP
!nrow_TEMP=nrow_VDAMP
!do i=1,nrow_TEMP
!   print *, i,trim(spref_TEMP(i,1)),', ',trim(spref_TEMP(i,2)),', ',trim(spref_TEMP(i,3)),', ',trim(spref_TEMP(i,4)),', ',trim(spref_TEMP(i,5)),', ',trim(spref_TEMP(i,6))
!enddo
!
!endif

end subroutine

!----------------------------------------------------------------------------------------------

subroutine read_spref(TEMP_file,spref_TEMP,nrow_TEMP,ncol_TEMP)
use globy
use strings
implicit none

integer :: fid=110
integer :: i,j
integer :: io_status,io
integer :: nrow,ncol,icount
character(len=20) :: cSTOP1,cSTOP2
character(len=512) :: cline_read
character(len=128) :: tokens(70)
integer :: ntokens
character(len=512) :: TEMP_file
integer :: nrow_TEMP,ncol_TEMP
character(len=64) :: spref_TEMP(IMAX_SPREF,JMAX_SPREF)

cSTOP1 = 'EOF';
cSTOP2 = 'END OF LIST';


!--- TEMP Read -----------------------------------------------------------
open(unit=fid,file=trim(TEMP_file),iostat=io_status,form='FORMATTED')
if (io_status /= 0) then
   write (*, *) 'File Open Error: '//trim(TEMP_file)
   stop
endif

! header
read(fid,"(a)") cline_read

icount=0;
do while (.not. EOF(fid))
   read(fid,"(a)") cline_read

   if ( cline_read(1:3) == 'EOF' .or. cline_read(1:3) == 'END' ) then
      exit
   endif
   if ( cline_read(1:1) == ',' .or. cline_read(1:1) == ' ' ) then
      cycle
   endif

   icount=icount+1

   !c parse the whole line
   call parse(cline_read, ',', tokens, ntokens)
   do j=1,ntokens
      spref_read(icount,j)=trim(tokens(j))
   enddo

enddo
nrow=icount
ncol=ntokens

! globy : sheet_read, nrow_sheet_read, ncol_sheet_read
do i=1,nrow
do j=1,ncol
   spref_TEMP(i,j)=spref_read(i,j)
enddo
enddo

nrow_TEMP=nrow
ncol_TEMP=ncol

close(fid)

end subroutine

!----------------------------------------------------------------------------------------------

!subroutine sort_tree_modify()
!use globy
!implicit none
!
!integer :: i,ic,icount,nnode
!integer :: j,nChild
!integer :: iChilds(MAXCHILD)
!integer :: idum(MAXLINE),isupp_sort(MAXLINE),iextr_sort(MAXLINE)
!!integer :: mypreordop(MAXLINE)
!integer :: iroot,isupp,iextr
!integer,dimension(MAXCHILD) :: isupp_ic,iextr_ic
!integer :: isupp_ic_no,iextr_ic_no
!integer :: len_supp,len_extr
!
!nnode=nnode_tree
!
!idum(:)=0
!isupp_sort(:)=0
!iextr_sort(:)=0
!
!iroot=iroot_tree
!isupp=0
!iextr=0
!isupp_ic(:)=0   ! iroot의 supply  childs
!iextr_ic(:)=0   ! iroot의 extract childs
!isupp_ic_no=0   ! iroot의 supply  child 개수
!iextr_ic_no=0   ! iroot의 extract child 개수
!
!nChild=twork_nChild(iroot)
!if (nChild == 0) then
!   write(*,*) 'Error in sort_tree'
!else
!   do j=1,nChild
!      ic=twork_iChilds(iroot,j)
!      if ( trim(PDMS_info1(ic, 6)) == 'SUPPLY' ) then
!         isupp_ic_no=isupp_ic_no+1 
!         isupp_ic(isupp_ic_no)=ic
!      elseif ( trim(PDMS_info1(ic, 6)) == 'EXTRACT' ) then
!         iextr_ic_no=iextr_ic_no+1 
!         iextr_ic(iextr_ic_no)=ic
!      endif
!   enddo
!endif
!isupp_tree=isupp
!iextr_tree=iextr
!
!idum(:)=0
!if (isupp .ne. 0) call mypreordop(isupp,idum,isupp_sort)
!idum(:)=0
!if (iextr .ne. 0) call mypreordop(iextr,idum,iextr_sort)
!
!icount=0
!do i=1,nnode
!   if (isupp_sort(i) .ne. 0) then
!      icount=icount+1
!   else
!      exit
!   endif
!enddo
!len_supp=icount
!nnode_supp_tree=len_supp
!
!icount=0
!do i=1,nnode
!   if (iextr_sort(i) .ne. 0) then
!      icount=icount+1
!   else
!      exit
!   endif
!enddo
!len_extr=icount
!nnode_extr_tree=len_extr
!
!!--------------- twork_LevelS2 --------------- 151222
!do i=1,nnode_supp_tree
!   twork_LevelS2(isupp_sort(i)) = i
!enddo
!do i=1,nnode_extr_tree
!   twork_LevelS2(iextr_sort(i)) = -i
!enddo
!   twork_LevelS2(iroot) = 0
!!---------------------------------------------
!
!!
!icount=0
!do i=len_extr,1,-1
!   icount=icount+1
!   idx_sort_all(icount)=iextr_sort(i)
!   idx_sort_extr(i)=iextr_sort(i)
!   if (idx_sort_extr(i)==0) then
!      print *,'Error in idx_sort_extr',i,idx_sort_extr(i)
!      stop
!   endif
!enddo
!   icount=icount+1
!   idx_sort_all(icount)=iroot
!do i=1,len_supp
!   icount=icount+1
!   idx_sort_all(icount)=isupp_sort(i)
!   idx_sort_supp(i)=isupp_sort(i)
!   if (idx_sort_supp(i)==0) then
!      print *,'Error in idx_sort_supp',i,idx_sort_supp(i)
!      stop
!   endif
!enddo
!
!write(*,*) iroot,isupp,iextr
!if (len_extr+1+len_supp == nnode) then
!   write(*,*) len_extr,'+',1,'+',len_supp,' =',nnode
!else
!   write(*,*) 'Error in Tree'
!   write(*,*) len_extr,'+',1,'+',len_supp,'/=',nnode
!   !stop
!endif
!   
!end subroutine
!
!----------------------------------------------------------------------------------------------

subroutine sort_tree()
use globy
implicit none

integer :: i,ic,icount,nnode
integer :: j,nChild
integer :: iChilds(MAXCHILD)
integer :: idum(MAXLINE),isupp_sort(MAXLINE),iextr_sort(MAXLINE)
!integer :: mypreordop(MAXLINE)
integer :: iroot,isupp,iextr
integer :: len_supp,len_extr
integer :: icount_supp,icount_extr,ierror

nnode=nnode_tree

idum(:)=0
isupp_sort(:)=0
iextr_sort(:)=0

iroot=iroot_tree
isupp=0
iextr=0
icount_supp=0
icount_extr=0

nChild=twork_nChild(iroot)
if (nChild == 0) then
   write(*,*) 'Error in sort_tree'
else
   do j=1,nChild
      ic=twork_iChilds(iroot,j)
      if ( trim(PDMS_info1(ic, 6)) == 'SUPPLY' ) then
         icount_supp = icount_supp + 1
         isupp=ic
      elseif ( trim(PDMS_info1(ic, 6)) == 'EXTRACT' ) then
         icount_extr = icount_extr + 1
         iextr=ic
      endif
   enddo
endif

! 160113  Fan inlet, outlet 개수 체크 --> 위에서 체크하게 하였음. 이부분 나중에 지울것.
!if (icount_extr >= 2) then
!   print *,'Fan Inlet should be connected to single element.', icount_supp
!   ierror = 160113
!endif
!if (icount_supp >= 2) then
!   print *,'Fan Outlet should be connected to single element.', icount_extr
!   ierror = 160113
!endif
!if (ierror == 160113) then
!   !call export_error(ierror)
!endif


! 다시 정상 루틴
isupp_tree=isupp
iextr_tree=iextr

idum(:)=0
if (isupp .ne. 0) call mypreordop(isupp,idum,isupp_sort)
idum(:)=0
if (iextr .ne. 0) call mypreordop(iextr,idum,iextr_sort)

icount=0
do i=1,nnode
   if (isupp_sort(i) .ne. 0) then
      icount=icount+1
   else
      exit
   endif
enddo
len_supp=icount
nnode_supp_tree=len_supp

icount=0
do i=1,nnode
   if (iextr_sort(i) .ne. 0) then
      icount=icount+1
   else
      exit
   endif
enddo
len_extr=icount
nnode_extr_tree=len_extr

!--------------- twork_LevelS2 --------------- 151222
do i=1,nnode_supp_tree
   twork_LevelS2(isupp_sort(i)) = i
enddo
do i=1,nnode_extr_tree
   twork_LevelS2(iextr_sort(i)) = -i
enddo
   twork_LevelS2(iroot) = 0
!---------------------------------------------

!
icount=0
do i=len_extr,1,-1
   icount=icount+1
   idx_sort_all(icount)=iextr_sort(i)
   idx_sort_extr(i)=iextr_sort(i)
   if (idx_sort_extr(i)==0) then
      print *,'Error in idx_sort_extr',i,idx_sort_extr(i)
      stop
   endif
enddo
   icount=icount+1
   idx_sort_all(icount)=iroot
do i=1,len_supp
   icount=icount+1
   idx_sort_all(icount)=isupp_sort(i)
   idx_sort_supp(i)=isupp_sort(i)
   if (idx_sort_supp(i)==0) then
      print *,'Error in idx_sort_supp',i,idx_sort_supp(i)
      stop
   endif
enddo

!write(*,*) iroot,isupp,iextr
!if (len_extr+1+len_supp == nnode) then
!   write(*,*) len_extr,'+',1,'+',len_supp,' =',nnode
!else
!   write(*,*) 'Error in Tree'
!   write(*,*) len_extr,'+',1,'+',len_supp,'/=',nnode
!   !stop
!endif
!   
end subroutine

!----------------------------------------------------------------------------------------------

subroutine make_bwork()
use globy
implicit none

integer :: i,ic,icount,nnode,isort,icount_bran
integer :: j,nChild,itmp
integer :: iChilds(MAXCHILD),nBrother,iParent
integer :: idum(MAXLINE),isupp_sort(MAXLINE),iextr_sort(MAXLINE)
integer :: iroot,isupp,iextr
integer :: len_supp,len_extr

nnode=nnode_tree

twork_BLevel(iroot_tree)=0
twork_BranID(iroot_tree)=1
twork_BelemID(iroot_tree)=1
bwork_Level(1)=0
bwork_FlowDir(1)=0
bwork_iSta(1)=1
bwork_iEnd(1)=1
bwork_bParent(1)=0
bwork_iParent(1)=0
bwork_NumElem(1)=1

icount_bran=1
do isort=1,nnode_supp_tree
   i=idx_sort_supp(isort)
   nBrother=twork_nBrother(i)
   iParent=twork_iParent(i)
   if (nBrother > 0 .or. i==isupp_tree) then
      twork_BLevel(i)=twork_BLevel(iParent)+1
      icount_bran=icount_bran+1
      twork_BranID(i)=icount_bran
      twork_BelemID(i)=1
      bwork_Level(icount_bran)=twork_BLevel(i)
      bwork_bParent(icount_bran)=twork_BranID(iParent)
      bwork_iParent(icount_bran)=iParent
      bwork_FlowDir(icount_bran)=twork_FlowDir(i)
      bwork_iSta(icount_bran)=i
      bwork_iEnd(icount_bran)=i
      bwork_NumElem(icount_bran)=twork_BelemID(i)
   else
      twork_BLevel(i)=twork_BLevel(iParent)
      twork_BranID(i)=twork_BranID(iParent)
      twork_BelemID(i)=twork_BelemID(iParent)+1
      bwork_iEnd(icount_bran)=i
      bwork_NumElem(icount_bran)=twork_BelemID(i)
   endif
enddo

do isort=1,nnode_extr_tree
   i=idx_sort_extr(isort)
   nBrother=twork_nBrother(i)
   iParent=twork_iParent(i)
   if (nBrother > 0 .or. i==iextr_tree) then
      twork_BLevel(i)=twork_BLevel(iParent)+1
      icount_bran=icount_bran+1
      twork_BranID(i)=icount_bran
      twork_BelemID(i)=1
      bwork_Level(icount_bran)=twork_BLevel(i)
      bwork_bParent(icount_bran)=twork_BranID(iParent)
      bwork_iParent(icount_bran)=iParent
      bwork_FlowDir(icount_bran)=twork_FlowDir(i)
      bwork_iSta(icount_bran)=i
      bwork_iEnd(icount_bran)=i
      bwork_NumElem(icount_bran)=twork_BelemID(i)
   else
      twork_BLevel(i)=twork_BLevel(iParent)
      twork_BranID(i)=twork_BranID(iParent)
      twork_BelemID(i)=twork_BelemID(iParent)+1
      bwork_iEnd(icount_bran)=i
      bwork_NumElem(icount_bran)=twork_BelemID(i)
   endif
enddo

nbran_tree=icount_bran   ! 0~nbran_tree

!do i=1,nbran_tree
!   print *,i,bwork_bParent(i),bwork_Level(i),bwork_NumElem(i),bwork_iParent(i)
!enddo
!
!stop

end subroutine

!----------------------------------------------------------------------------------------------

!subroutine name_tree_3(cinp,cout,ielem)
!use globy
!implicit none
!
!character(len=64) :: cinp
!character(len=64+MAXBRAN*6) :: cout
!integer :: ielem
!character(len=MAXBRAN*6) :: cpre,cpst
!character(len=6) :: ctmp
!integer :: BLevel,BranID,BranList,FlowDir,ParentBranID
!integer :: i,iSta,iEnd
!integer :: iCurrent,iParent,iBranParent
!
!cpre=''
!cpst=''
!ctmp=''
!
!BLevel=twork_BLevel(ielem)
!FlowDir=twork_FlowDir(ielem)
!
!iCurrent=ielem
!do i=1,BLevel
!   
!   iParent=twork_iParent(iCurrent)
!   BranID=twork_BranID(iCurrent)
!   ParentBranID=bwork_bParent(BranID)
!
!   iSta=bwork_iSta(BranID)
!   iEnd=bwork_iEnd(BranID)
!   
!   iBranParent=twork_iParent(iSta)
!   BranList=twork_BelemID(iCurrent)
!   
!   if (twork_iChilds(iBranParent,twork_nChild(iBranParent)) == iSta .or. twork_BLevel(iCurrent)==1 ) then
!      if (BranList==1 .and. i==1) then   ! 150826
!      !if (i==BLevel) then   ! OLD
!         if (FlowDir>0) then
!            ctmp='└─'
!         else
!            ctmp='┌─'
!         endif
!      else
!         ctmp='　　'
!      endif
!   else 
!      !if (BranList==1) then   ! 160304 이전.
!      if (BranList==1 .and. i==1) then   ! 160304 이후. 왜 되는지는 파악 못했음... 위에거를 참고.
!         ctmp='├─'
!      else
!         ctmp='│　'
!      endif
!   endif
!
!   cpre=trim(ctmp)//trim(cpre)
!   iCurrent=iBranParent
!
!enddo
!
!!do i=1,MAXBRAN-BLevel
!   !cpst=trim(cpst)//','
!!enddo
!
!cout=trim(cpre)//trim(cinp)//trim(cpst)
!
!end subroutine
!
!----------------------------------------------------------------------------------------------

subroutine name_tree_3s(cinp,cout,ielem)
use globy
implicit none

character(len=64) :: cinp
character(len=64+MAXBRAN*6) :: cout
integer :: ielem
character(len=MAXBRAN*6) :: cpre,cpst
character(len=3) :: ctmp
integer :: BLevel,BranID,BranList,FlowDir,ParentBranID
integer :: i,iSta,iEnd
integer :: iCurrent,iParent,iBranParent

cpre=''
cpst=''
ctmp=''

BLevel=twork_BLevel(ielem)
FlowDir=twork_FlowDir(ielem)

iCurrent=ielem
do i=1,BLevel
   
   iParent=twork_iParent(iCurrent)
   BranID=twork_BranID(iCurrent)
   ParentBranID=bwork_bParent(BranID)

   iSta=bwork_iSta(BranID)
   iEnd=bwork_iEnd(BranID)
   
   iBranParent=twork_iParent(iSta)
   BranList=twork_BelemID(iCurrent)
   
   if (twork_iChilds(iBranParent,twork_nChild(iBranParent)) == iSta .or. twork_BLevel(iCurrent)==1 ) then
      if (BranList==1 .and. i==1) then   ! 150826
      !if (i==BLevel) then   ! OLD
         if (FlowDir>0) then
            ctmp='└ '
         else
            ctmp='┌ '
         endif
      else
         ctmp='　 '
      endif
   else 
      !if (BranList==1) then   ! 160304 이전.
      if (BranList==1 .and. i==1) then   ! 160304 이후. 왜 되는지는 파악 못했음... 위에거를 참고.
         ctmp='├ '
      else
         ctmp='│ '
      endif
   endif

   !cpre=trim(ctmp)//trim(cpre)
   cpre=ctmp//trim(cpre)//' '
   iCurrent=iBranParent

enddo

!do i=1,MAXBRAN-BLevel
   !cpst=trim(cpst)//','
!enddo

if (trim(cpre) == '') then
   cout = trim(cinp)//trim(cpst)
else
   cout = trim(cpre)//' '//trim(cinp)//trim(cpst)
endif

end subroutine

!----------------------------------------------------------------------------------------------

recursive subroutine mypreordop(node,x,y)
use globy
implicit none

integer :: node
integer :: icount
integer :: i,j,nChild
integer :: x(MAXLINE)
integer :: y(MAXLINE)

! 전위순회

icount=0
if (x(1) == 0) then
   y(1)=node
else
   do i=1,MAXLINE
      icount=icount+1
      if (x(i) .ne. 0) then
         y(i)=x(i)
      else
         exit
      endif
   enddo
   y(icount)=node
endif

nChild=twork_nChild(node)
do j=1,nChild
   i=twork_iChilds(node,j)
   if (i==node) then
      write(*,*) 'Error in Preorder Op.'
   else
      x(:)=y(:)
      call mypreordop(i,x,y);
   endif
enddo
   
end subroutine

!----------------------------------------------------------------------------------------------

subroutine export_tree()
use globy
implicit none

integer :: fid=201
integer :: io_status
integer :: i,nnode

nnode=nnode_tree

!open(unit=fid,file='C:\temp_tree.txt',iostat=io_status,form='FORMATTED')
open(unit=fid,file=trim(AMlocaltemp_path)//'\tmp\temp_tree.dat',iostat=io_status,form='FORMATTED')
if (io_status /= 0) then
   write (*, *) 'File Open Error'
   stop
end if
do i=1,nnode
   write(fid,'(40(a,","),a)') &
      trim(PDMS_info1(i, 1)),trim(PDMS_info1(i, 3)),trim(PDMS_info1(i,11)),trim(PDMS_info1(i, 2)), &   ! 1,2,3,4
      trim(PDMS_info1(i,12)),trim(PDMS_info1(i,13)),trim(PDMS_info1(i,14)),trim(PDMS_info1(i,15)), &   ! 5,6,7,8
      trim(PDMS_info1(i,16)),trim(PDMS_info1(i,17)),trim(PDMS_info1(i,18)),trim(PDMS_info1(i,19)),trim(PDMS_info1(i,20)), &   ! 9,10,11,12,13
      trim(PDMS_info1(i, 4)),trim(PDMS_info1(i, 5)),trim(PDMS_info1(i, 6)),trim(PDMS_info1(i,10)), &   ! 14,15,16,17
      '','','', &   ! 18,19,20
      '','','','','', &   ! 21,22,23,24,25
      '','','','','', &   ! 26,27,28,29,30
      '','','','',''      ! 31,32,33,34,35
enddo
close(fid)
!write (*, *) 'Saved temp_tree.txt.'

end subroutine


!----------------------------------------------------------------------------------------------

subroutine export_tree_expand()
use globy
use funcs
implicit none

integer :: fid=202
integer :: io_status
integer :: i,nnode
integer :: nchild,ic
character(len=64) :: ChildS1,ChildS2,ChildS3,ChildS4,ChildS5,ChildS6,ChildS7,ChildS8

nnode=nnode_tree

!open(unit=fid,file='C:\temp_tree.txt',iostat=io_status,form='FORMATTED')
open(unit=fid,file=trim(AMlocaltemp_path)//'\tmp\temp_tree.dat',action="write",status="replace",iostat=io_status,form='FORMATTED')
!if (io_status /= 0) then
!   write (*, *) 'File Open Error'
!   stop
!end if
do i=1,nnode

if (1==0) then
   nchild = int(str2num(trim(PDMS_info1(i,12))))
   ChildS1=''
   ChildS2=''
   ChildS3=''
   ChildS4=''
   ChildS5=''
   ChildS6=''
   ChildS7=''
   ChildS8=''
   do ic=1,min(nchild,MAXCHILD)
      if (ic <= 5) then
         ChildS1=trim(ChildS1)//trim(PDMS_info1(i,13-1+ic))
      elseif (ic <= 10) then
         ChildS2=trim(ChildS2)//trim(PDMS_info1(i,13-1+ic))
      elseif (ic <= 10) then
         ChildS3=trim(ChildS3)//trim(PDMS_info1(i,13-1+ic))
      elseif (ic <= 10) then
         ChildS4=trim(ChildS4)//trim(PDMS_info1(i,13-1+ic))
      elseif (ic <= 10) then
         ChildS5=trim(ChildS5)//trim(PDMS_info1(i,13-1+ic))
      elseif (ic <= 10) then
         ChildS6=trim(ChildS6)//trim(PDMS_info1(i,13-1+ic))
      elseif (ic <= 10) then
         ChildS7=trim(ChildS7)//trim(PDMS_info1(i,13-1+ic))
      elseif (ic <= 10) then
         ChildS8=trim(ChildS8)//trim(PDMS_info1(i,13-1+ic))
      endif
   enddo
endif

   write(fid,'(40(a,","),a)') &
      trim(PDMS_info1(i, 1)),trim(PDMS_info1(i, 3)),trim(PDMS_info1(i,11)),trim(PDMS_info1(i, 2)), &   ! 1,2,3,4
      trim(PDMS_info1(i,12)),trim(PDMS_info1(i,13)),trim(PDMS_info1(i,14)),trim(PDMS_info1(i,15)), &   ! 5,6,7,8
      trim(PDMS_info1(i,16)),trim(PDMS_info1(i,17)),trim(PDMS_info1(i,18)),trim(PDMS_info1(i,19)),trim(PDMS_info1(i,20)), &   ! 9,10,11,12,13
      trim(PDMS_info1(i, 4)),trim(PDMS_info1(i, 5)),trim(PDMS_info1(i, 6)),trim(PDMS_info1(i,10)), &   ! 14,15,16,17
      '','','', &   ! 18,19,20
      trim(dsheet(i,19)),trim(dsheet(i,20)),trim(dsheet(i,21)),trim(dsheet(i,22)),trim(dsheet(i,23)), &   ! 21,22,23,24,25
      trim(dsheet(i,24)),trim(dsheet(i,25)),trim(dsheet(i,26)),trim(dsheet(i,27)),trim(dsheet(i,28)), &   ! 26,27,28,29,30
      trim(dsheet(i,13)),trim(dsheet(i,14)),trim(dsheet(i,15)),trim(dsheet(i,16)),trim(dsheet(i,17))      ! 31,32,33,34,35
enddo
close(fid)
!write (*, *) 'Saved temp_tree.txt.'

end subroutine

!----------------------------------------------------------------------------------------------

subroutine make_dsheet()
use globy
use ISO_VARYING_STRING
use strings
use funcs
implicit none

integer :: fid=200
integer :: i,nnode,i2,ib,ic,ic2,ic3,icomp
character(len=64) :: cType,cType2,SecType,FlowDir,cTypeParent,cTypeBrother,cNameParent,cName,cTypeChild,cType2Child,SecTypeChild
character(len=64) :: cSPREF,cSPREF1,cSPREF2
integer :: nChild,nBrother,iParent,iBrother1,iBrother2,iChild1,iChild2
integer :: iChilds(MAXCHILD),iBrothers(MAXCHILD-1)
real,dimension(MAXCHILD) :: w0_Childs,h0_Childs
integer :: itmp,j
real,dimension(3) :: pos1,pos2,dir0,dirc1,dirc2,dirc3
real :: angleC1,angleC2,angleC3,rtmp,angleS(6),angleS_max,angleS1,angleS2
real :: ang1,rad1,H_W,r_W,r_H,Para1,Para2,Para3
real :: w0,h0,d0,w1,h1,d1
real :: w0_ic,h0_ic
real :: angleBend,radiBend
real :: pi
real :: maxarea
integer :: ic_strt

character(len=64),dimension(MAXLINE) :: d_type1,d_type2,d_refno,d_parent,d_direction,d_name,d_sectype,d_sectype1,d_sectypeb
real,dimension(MAXLINE) :: d_length,d_flowrate,d_w0,d_h0,d_d0,d_w1,d_h1,d_d1,d_wb,d_hb,d_db,d_angleS
real,dimension(MAXLINE) :: d_angleBend,d_radiBend,d_filleti,d_filleto,d_woff,d_hoff,d_lenext
real,dimension(MAXLINE,3) :: d_dirn

character(len=256) :: cline_temp
character(len=64) :: tokens(10)
integer :: ntokens

logical :: yes_matchf,yes_match0,yes_match1,yes_match2
logical :: yes_found

nnode=nnode_tree

pi=acos(-1.)

do i=1,nnode

   d_refno(i)     =trim(PDMS_info1(i, 1))     ! refno
   d_parent(i)    =trim(PDMS_info1(i, 2))     ! parent refno

   ! 150716 A 대신 홑따옴표 char(39)로. --> Spacing으로.
   d_refno(i) =' '//trim(d_refno(i))     ! refno
   if (len_trim(d_parent(i))>0) then
      d_parent(i)=' '//trim(d_parent(i))     ! parent refno
   endif

   d_name(i)      =trim(PDMS_info1(i, 3))     ! name
   d_flowrate(i)  =str2num(trim(PDMS_info1(i, 5)))     ! flowrate
   d_direction(i) =trim(PDMS_info1(i, 6))     ! direction

   d_type1(i)     =trim(PDMS_info2(i, 1))  ! type1
   d_type2(i)     =trim(PDMS_info2(i, 2))  ! type2
   d_sectype(i)   =trim(PDMS_info2(i, 3))  ! section type

   ! 김태호 사원 발견 오류
   ! --> trim_PDMS_info2() 로 이동   

   if (trim(PDMS_info2(i, 5))=='DIAM') then
      d_sectype(i) = 'CIRC'
      d_d1(i) = str2num(trim(PDMS_info2(i, 4)))
      d_w1(i) = 0.
      d_h1(i) = 0.
   elseif (trim(PDMS_info2(i, 4))=='9999' .or. trim(PDMS_info2(i, 5))=='9999') then
      d_sectype(i) = 'ETC'
      d_d1(i) = 9999.
      d_w1(i) = 9999.
      d_h1(i) = 9999.
   else
      d_sectype(i) = 'RECT'
      d_d1(i) = 0.
      d_w1(i) = str2num(trim(PDMS_info2(i, 4)))
      d_h1(i) = str2num(trim(PDMS_info2(i, 5)))
   endif

   if (trim(PDMS_info2(i, 7))=='DIAM') then
       !if (trim(PDMS_info2(i, 5))=='DIAM') then
            d_sectype(i) = 'CIRC'
       !else
       !    d_sectype(i) = 'TRAN'
       !endif       
      d_d0(i) = str2num(trim(PDMS_info2(i, 6)))
      d_w0(i) = 0.
      d_h0(i) = 0.
      
   elseif (trim(PDMS_info2(i, 6))=='9999' .or. trim(PDMS_info2(i, 7))=='9999') then
      d_sectype(i) = 'ETC'
      d_d0(i) = 9999.
      d_w0(i) = 9999.
      d_h0(i) = 9999.
   else
             ! if (trim(PDMS_info2(i, 7))=='DIAM') then
      !     d_sectype(i) = 'TRAN'
      ! else           
            d_sectypeb(i) = 'RECT'
      !endif

      d_d0(i) = 0.
      d_w0(i) = str2num(trim(PDMS_info2(i, 6)))
      d_h0(i) = str2num(trim(PDMS_info2(i, 7)))
   endif
   
   if (trim(PDMS_info2(i, 9))=='DIAM') then
      d_sectypeb(i) = 'CIRC'
      d_db(i) = str2num(trim(PDMS_info2(i, 8)))
      d_wb(i) = 0.
      d_hb(i) = 0.
   elseif (trim(PDMS_info2(i, 8))=='9999' .or. trim(PDMS_info2(i, 9))=='9999') then
      d_sectypeb(i) = 'ETC'
      d_db(i) = 9999.
      d_wb(i) = 9999.
      d_hb(i) = 9999.
   else
      d_sectype(i) = 'RECT'
      d_db(i) = 0.
      d_wb(i) = str2num(trim(PDMS_info2(i, 8)))
      d_hb(i) = str2num(trim(PDMS_info2(i, 9)))
   endif   
   
   d_length(i)    =str2num(trim(PDMS_info2(i,10)))  ! length 
   d_lenext(i)    =str2num(trim(PDMS_info2(i,11)))  ! length extension
   d_angleBend(i) =str2num(trim(PDMS_info2(i,12)))
   d_radiBend(i)  =str2num(trim(PDMS_info2(i,13)))
   d_filleti(i)   =str2num(trim(PDMS_info2(i,14)))
   d_filleto(i)   =str2num(trim(PDMS_info2(i,15)))
   d_woff(i)      =str2num(trim(PDMS_info2(i,16)))
   d_hoff(i)      =str2num(trim(PDMS_info2(i,17)))

   pos1 = (/ str2num(trim(PDMS_info2(i,51))),str2num(trim(PDMS_info2(i,52))),str2num(trim(PDMS_info2(i,53))) /)
   pos2 = (/ str2num(trim(PDMS_info2(i,54))),str2num(trim(PDMS_info2(i,55))),str2num(trim(PDMS_info2(i,56))) /)
   rtmp = sqrt((pos2(1) - pos1(1))**2 + (pos2(2) - pos1(2))**2 + (pos2(3) - pos1(3))**2) + 1e-7

   dir0 = (pos2 - pos1)/rtmp
   d_dirn(i,1)=dir0(1)
   d_dirn(i,2)=dir0(2)
   d_dirn(i,3)=dir0(3)

enddo

do i=1,nnode 
    
   dsheet(i, 1)=int2str(i)
   dsheet(i, 2)=d_refno(i)      ! refno
   dsheet(i, 3)=d_parent(i)     ! parent refno
   dsheet(i, 4)=d_direction(i)     ! direction
   dsheet(i, 5)=d_name(i)     ! name
   dsheet(i, 6)=d_sectype(i)  ! section type
   dsheet(i, 7)=trim(PDMS_info1(i, 5))     ! flowrate
   dsheet(i, 8)=trim(PDMS_info2(i,10))  ! length
   dsheet(i, 9)=trim(real2str(d_w0(i)))  ! W
   dsheet(i,10)=trim(real2str(d_h0(i)))  ! H
   dsheet(i,11)=trim(real2str(d_d0(i)))  ! D
   
   ! BRCO W,H,D 160609 추가.
    cType   =trim(d_type1(i))
   if (trim(cType)=='BRCO') then
      dsheet(i, 6)=d_sectypeb(i)  ! section type
      dsheet(i, 9)=trim(real2str(d_wb(i)))  ! W
      dsheet(i,10)=trim(real2str(d_hb(i)))  ! H
      dsheet(i,11)=trim(real2str(d_db(i)))  ! D
   endif
   
   dsheet(i,12)=trim(PDMS_info2(i,24))  ! Roughness
   dsheet(i,18)=trim(PDMS_info2(i,50))  ! Memo
   dsheet(i,40)=trim(PDMS_info1(i, 4))  ! Tree Level
enddo

! Roughness
do i=1,nnode
   ! 임시. Roughness = 0.09로 일괄 통일. 160617 지울거면 바로 아래만 지우면 됨.
   PDMS_info2(i,24) = '0.09'

   ! 정식..
   if (trim(PDMS_info2(i,24)) /= '') then
      PDMS_info2(i,24) = REPLACE(PDMS_info2(i,24),'EX','E',.TRUE.)
   else
      PDMS_info2(i,24) = '0.09'
   endif
   dsheet(i,12)=trim(PDMS_info2(i,24))  ! Roughness

enddo

do i=1,nnode
    
    if (twork_iParent(i)/=0) then

        nChild=twork_nChild(i)
        do j=1,nChild
            iChilds(j)=twork_iChilds(i,j)
        enddo
   
        SecType=trim(dsheet(i, 6))    
        cType  =trim(PDMS_info2(i, 1))
        cType2 =trim(PDMS_info2(i, 2))
        FlowDir=trim(dsheet(i, 4))

        angleS(1)=str2num(trim(PDMS_info2(i,18)))
        angleS(2)=str2num(trim(PDMS_info2(i,19)))
        angleS(3)=str2num(trim(PDMS_info2(i,20)))
        angleS(4)=str2num(trim(PDMS_info2(i,21)))
        angleS(5)=str2num(trim(PDMS_info2(i,22)))
        angleS(6)=str2num(trim(PDMS_info2(i,23)))
        angleS_max = 0.
  
        angleS1=max(abs(angleS(2)),abs(angleS(3)))
        angleS2=max(abs(angleS(5)),abs(angleS(6)))
        angleS_max=max(angleS1,angleS2)

        w0 = d_w0(i)
        h0 = d_h0(i)
        d0 = d_d0(i)
        w1 = d_w1(i)
        h1 = d_h1(i)
        d1 = d_d1(i)

        ang1 = d_angleBend(i)
        rad1 = d_radiBend(i)
   
        if (nChild > 1) then
        
            if ( trim(SecType)=='RECT') then

                !1) Find S
                ic_strt=0
                do j=1,nChild
                
                    ic=iChilds(j)
                    
                    pos1 = (/ str2num(trim(PDMS_info2(ic,51))),str2num(trim(PDMS_info2(ic,52))),str2num(trim(PDMS_info2(ic,53))) /)
                    pos2 = (/ str2num(trim(PDMS_info2(i,54))) ,str2num(trim(PDMS_info2(i,55))) ,str2num(trim(PDMS_info2(i,56)))  /)
                                                          
                    if ( pos1(1)==pos2(1) .and. pos1(2)==pos2(2) .and. pos1(3)==pos2(3)) then                        
                        ic_strt = ic                    
                    endif                    
                    
                enddo
                
                maxarea=0
                if (ic_strt==0) then                
                    
                    print *,'Cannot Find STRT Elem. AT BRANCH',i,trim(dsheet(i, 2))                                    
                                   
                    do j=1,nChild
                
                        ic=iChilds(j)
                
                        w0 = d_w0(i)
                        h0 = d_h0(i)
                        w0_ic = d_w0(ic)
                        h0_ic = d_h0(ic)                
                                        
                        !itmp = i
                        !dir0 = (/ d_dirn(itmp,1),d_dirn(itmp,2),d_dirn(itmp,3) /)
                        !itmp = ic
                        !dirc1 = (/ d_dirn(itmp,1),d_dirn(itmp,2),d_dirn(itmp,3) /)
                        !angleC1 = acos(dot_product(dir0,dirc1))*180./pi                    
                        !! 각도 구해놓고 이용은 안했음
                    
                        if ( (w0==w0_ic .and. h0==h0_ic) .or. (w0==h0_ic .and. h0==w0_ic) ) then
                            maxarea = w0_ic*h0_ic
                            ic_strt = ic
                        endif
                    
                        if ( w0_ic*h0_ic >= maxarea ) then
                            maxarea = w0_ic*h0_ic
                            ic_strt = ic
                        endif
                        
                    enddo               
            
                    ! nChild > 3 이상일 경우 확인 필요               
                    cTypeChild   =trim(d_type1(ic_strt))
                    if (trim(cTypeChild)=='BRCO' .or. trim(cTypeChild)=='BEND' .or. trim(cTypeChild)=='ELBO') then
                        do j=1,nChild                        
                            ic=iChilds(j)
                            cTypeChild=trim(d_type1(ic))
                            if (trim(cTypeChild)/='BRCO' .and. trim(cTypeChild)/='BEND' .and. trim(cTypeChild)/='ELBO') ic_strt=ic
                        enddo             
                    endif
                
                endif
            
                !2) Matching
                do j=1,nChild
                
                    ic=iChilds(j)
                
                    if (ic/=ic_strt) then
                    
                        cTypeChild   =trim(d_type1(ic))
                        cType2Child  =trim(d_type2(ic))
                        SecTypeChild =trim(d_sectype(ic))
                        
                        if (trim(cTypeChild)=='BRCO') then
                                
                            ! 151203
                            if (d_db(ic) == 0) then
                                dsheet(ic, 6)='RECT'
                            elseif (d_db(ic) == 9999) then
                                dsheet(ic, 6)='ETC'
                            else
                                dsheet(ic, 6)='CIRC'
                            endif
                                
                            dsheet(ic, 9)=trim(real2str(d_wb(ic)))  ! W
                            dsheet(ic,10)=trim(real2str(d_hb(ic)))  ! H
                            dsheet(ic,11)=trim(real2str(d_db(ic)))  ! D
                                
                            if (trim(cType2Child)=='BOOT' .or. trim(cType2Child)=='TABR') then
                     
                                if (trim(FlowDir)=='SUPPLY') then
                                    dsheet(ic,13)='SR5-13';
                                    dsheet(ic,14)='B'                                                
                                    !dsheet(ic,19)=''
                                    !dsheet(ic,20)=''
                                    !dsheet(ic,21)=''
                                    !dsheet(ic,22)=''
                                    
                                    dsheet(ic_strt,13)='SR5-13';
                                    dsheet(ic_strt,14)='S'
                                else
                                    dsheet(ic,13)='ER5-3';
                                    dsheet(ic,14)='B'
                                    !dsheet(ic,19)=''
                                    !dsheet(ic,20)=''
                                    !dsheet(ic,21)=''
                                    !dsheet(ic,22)=''
                                    
                                    dsheet(ic_strt,13)='ER5-3';
                                    dsheet(ic_strt,14)='S'
                                endif
                                    
                            else !if (trim(cType2Child)=='SBRA') then
                                    
                                if (trim(dsheet(ic, 6))=='RECT') then
                                        
                                    if ( trim(FlowDir)=='SUPPLY') then
                                        dsheet(ic,13)='SR5-5';
                                        dsheet(ic,14)='B'
                                        dsheet(ic_strt,13)='SR5-5';
                                        dsheet(ic_strt,14)='S'
                                    else
                                        !dsheet(ic,13)='ER5-2';
                                        dsheet(ic,13)='ER5-3';
                                        dsheet(ic,14)='B'
                                        !dsheet(ic_strt,13)='ER5-2';
                                        dsheet(ic_strt,13)='ER5-3';
                                        dsheet(ic_strt,14)='S'
                                    endif
                                        
                                elseif (trim(dsheet(ic, 6))=='CIRC') then
                                        
                                    if ( trim(FlowDir)=='SUPPLY') then
                                        dsheet(ic,13)='SR5-11';
                                        dsheet(ic,14)='B'
                                        dsheet(ic_strt,13)='SR5-11';
                                        dsheet(ic_strt,14)='S'
                                    else
                                        dsheet(ic,13)='ER5-2';
                                        dsheet(ic,14)='B'
                                        dsheet(ic_strt,13)='ER5-2';
                                        dsheet(ic_strt,14)='S'
                                    endif
                                        
                                endif   
                                
                            endif   
                     
                        elseif (trim(cTypeChild)=='TAPE') then
                            
                            !if (trim(d_type1(ic_strt))=='BEND' .or. trim(d_type1(ic_strt))=='ELBO') then
                            !    
                            !    if ( trim(FlowDir)=='SUPPLY') then
                            !        dsheet(ic,13)='SR5-1';
                            !        dsheet(ic,14)='S'
                            !        dsheet(ic_strt,13)='SR5-1';
                            !        dsheet(ic_strt,14)='B'
                            !    else
                            !        dsheet(ic,13)='ER5-1';
                            !        dsheet(ic,14)='S'
                            !        dsheet(ic_strt,13)='ER5-1';
                            !        dsheet(ic_strt,14)='B'
                            !    endif                                 
                            !    
                            !else                                
                      
                                if ( trim(FlowDir)=='SUPPLY') then
                                    dsheet(ic,13)='SR5-13';
                                    dsheet(ic,14)='B'
                                    !dsheet(ic,19)=''
                                    !dsheet(ic,20)=''
                                    !dsheet(ic,21)=''
                                    !dsheet(ic,22)=''
                                    
                                    dsheet(ic_strt,13)='SR5-13';
                                    dsheet(ic_strt,14)='S'
                                else
                                    dsheet(ic,13)='ER5-3';
                                    dsheet(ic,14)='B'
                                    !dsheet(ic,19)=''
                                    !dsheet(ic,20)=''
                                    !dsheet(ic,21)=''
                                    !dsheet(ic,22)=''
                                    
                                    dsheet(ic_strt,13)='ER5-3';
                                    dsheet(ic_strt,14)='S'
                                endif 
                                
                            !endif                            

                        elseif (trim(cTypeChild)=='BEND' .or. trim(cTypeChild)=='ELBO') then
                                           
                            if (trim(d_type1(ic_strt))=='BEND' .or. trim(d_type1(ic_strt))=='ELBO') then
                            
                                if ( trim(FlowDir)=='SUPPLY') then
                                    dsheet(ic,13)='SR5-14';
                                    dsheet(ic,14)='B'
                                    !dsheet(ic,19)=''
                                    !dsheet(ic,20)=''
                                    !dsheet(ic,21)=''
                                    !dsheet(ic,22)=''
                                    
                                    dsheet(ic_strt,13)='SR5-14';
                                    dsheet(ic_strt,14)='S'
                                    !dsheet(ic_strt,19)=''
                                    !dsheet(ic_strt,20)=''
                                    !dsheet(ic_strt,21)=''
                                    !dsheet(ic_strt,22)=''
                                    
                                else
                                    dsheet(ic,13)='ER5-4';
                                    dsheet(ic,14)='B'
                                    !dsheet(ic,19)=''
                                    !dsheet(ic,20)=''
                                    !dsheet(ic,21)=''
                                    !dsheet(ic,22)=''
                                    
                                    dsheet(ic_strt,13)='ER5-4';
                                    dsheet(ic_strt,14)='S'
                                    !dsheet(ic_strt,19)=''
                                    !dsheet(ic_strt,20)=''
                                    !dsheet(ic_strt,21)=''
                                    !dsheet(ic_strt,22)=''
                                endif
                                           
                            else
                    
                                if ( trim(FlowDir)=='SUPPLY') then
                                    dsheet(ic,13)='SR5-1';
                                    dsheet(ic,14)='B'
                                    !dsheet(ic,19)=''
                                    !dsheet(ic,20)=''
                                    !dsheet(ic,21)=''
                                    !dsheet(ic,22)=''
                                    
                                    dsheet(ic_strt,13)='SR5-1';
                                    dsheet(ic_strt,14)='S'
                                else
                                    dsheet(ic,13)='ER5-1';
                                    dsheet(ic,14)='B'
                                    !dsheet(ic,19)=''
                                    !dsheet(ic,20)=''
                                    !dsheet(ic,21)=''
                                    !dsheet(ic,22)=''
                                    
                                    dsheet(ic_strt,13)='ER5-1';
                                    dsheet(ic_strt,14)='S'
                                endif
                                          
                            endif                            
                        
                        !elseif (trim(cTypeChild)=='TRNS') then
                        
                        else    
                                                        
                            !if (trim(d_type1(ic_strt))=='BEND' .or. trim(d_type1(ic_strt))=='ELBO') then
                            !    
                            !    if ( trim(FlowDir)=='SUPPLY') then
                            !        dsheet(ic,13)='SR5-1';
                            !        dsheet(ic,14)='S'
                            !        dsheet(ic_strt,13)='SR5-1';
                            !        dsheet(ic_strt,14)='B'
                            !    else
                            !        dsheet(ic,13)='ER5-1';
                            !        dsheet(ic,14)='S'
                            !        dsheet(ic_strt,13)='ER5-1';
                            !        dsheet(ic_strt,14)='B'
                            !    endif                                 
                            !    
                            !else

                                if ( trim(FlowDir)=='SUPPLY') then
                                    dsheet(ic,13)='SR5-5';
                                    dsheet(ic,14)='B'
                                    dsheet(ic_strt,13)='SR5-5';
                                    dsheet(ic_strt,14)='S'
                                else
                                    dsheet(ic,13)='ER5-3';
                                    dsheet(ic,14)='B'
                                    dsheet(ic_strt,13)='ER5-3';
                                    dsheet(ic_strt,14)='S'
                                endif
                                
                            !endif                            

                        endif
                        
                    endif
                    
                enddo
            
            elseif ( trim(SecType)=='CIRC') then
                                         
                ! 수정필요
                itmp = i
                dir0 = (/ d_dirn(itmp,1),d_dirn(itmp,2),d_dirn(itmp,3) /)
                itmp = iChilds(1)
                dirc1 = (/ d_dirn(itmp,1),d_dirn(itmp,2),d_dirn(itmp,3) /)
                itmp = iChilds(2)
                dirc2 = (/ d_dirn(itmp,1),d_dirn(itmp,2),d_dirn(itmp,3) /)
                angleC1 = acos(dot_product(dir0,dirc1))*180./pi
                angleC2 = acos(dot_product(dir0,dirc2))*180./pi
                angleBend = myround(d_angleBend(i),5)
         
                if ( trim(FlowDir)=='SUPPLY') then
                dsheet(iChilds(1),13)='SD5-9';
                dsheet(iChilds(2),13)='SD5-9';

                if (angleBend == 30) then
                dsheet(iChilds(1),13)='SD5-1(30)';
                dsheet(iChilds(2),13)='SD5-1(30)';
                elseif (angleBend == 45) then
                dsheet(iChilds(1),13)='SD5-1';
                dsheet(iChilds(2),13)='SD5-1';
                endif
         
                else
                dsheet(iChilds(1),13)='ED5-3';
                dsheet(iChilds(2),13)='ED5-3';
            
                if (angleBend == 30) then
                dsheet(iChilds(1),13)='ED5-1';
                dsheet(iChilds(2),13)='ED5-1';
                elseif (angleBend == 45) then
                dsheet(iChilds(1),13)='ED5-2';
                dsheet(iChilds(2),13)='ED5-2';
                endif

                endif

                if (angleC1 < angleC2) then
                dsheet(iChilds(1),14)='S'
                dsheet(iChilds(2),14)='B'
                else
                dsheet(iChilds(1),14)='B'
                dsheet(iChilds(2),14)='S'
                endif
            
            endif 
            
        endif
        
    endif
    
enddo

!% SPREF
do i=1,nnode
   
   SecType=trim(dsheet(i, 6))
   cType  =trim(PDMS_info2(i, 1))
   cType2 =trim(PDMS_info2(i, 2))
   cSPREF = trim(PDMS_info2(i,30))
   cSPREF1 = ''
   cSPREF2 = ''
   FlowDir=trim(dsheet(i, 4))
   
   w0 = d_w0(i)
   h0 = d_h0(i)
   d0 = d_d0(i)
   w1 = d_w1(i)
   h1 = d_h1(i)
   d1 = d_d1(i)

   nChild=twork_nChild(i)
   do j=1,nChild
      iChilds(j)=twork_iChilds(i,j)
   enddo

   call parse(cSPREF, '/', tokens, ntokens)
   if (ntokens >= 2) cSPREF1 = tokens(2)
   if (ntokens >= 3) cSPREF2 = tokens(3)
         
   select case (trim(cSPREF1))

   case ('SNSD-HVAC-SPEC')

      ! Y-BRANCH
      if (trim(cSPREF2) == 'SNSDY') then
         if (nChild > 1) then
             if ( trim(FlowDir)=='SUPPLY') then
                dsheet(iChilds(1),13)='SR5-1'
                dsheet(iChilds(2),13)='SR5-1'
             else
                dsheet(iChilds(1),13)='ER5-1'
                dsheet(iChilds(2),13)='ER5-1'
             endif
         endif
      endif
       

      ! Dovetail
      if (trim(cSPREF2) == 'SNSD_RTTHRE') then
         if (nChild > 1) then
             if ( trim(FlowDir)=='SUPPLY') then
                dsheet(iChilds(1),13)='SR5-14'
                dsheet(iChilds(2),13)='SR5-14'
             else
                dsheet(iChilds(1),13)='ER5-4'
                dsheet(iChilds(2),13)='ER5-4'
             endif
         endif
      endif      
      
      !! DAMPER
      !if (trim(cSPREF2) == 'SNSD_RSBLDAMP') then         
      !       
      !  dsheet(i,19)='CR9-1'
      !  dsheet(i,20)='0'
      !  dsheet(i,21)=''
      !  dsheet(i,22)=''       
      !   
      !endif      
      
   case ('HHI-HVAC-SPEC')

      ! Dovetail
      if (trim(cSPREF2) == 'RTTHRE') then
         if (nChild > 1) then
             if ( trim(FlowDir)=='SUPPLY') then
                dsheet(iChilds(1),13)='SR5-14'
                dsheet(iChilds(2),13)='SR5-14'
             else
                dsheet(iChilds(1),13)='ER5-4'
                dsheet(iChilds(2),13)='ER5-4'
             endif
         endif
      endif   
      
   endselect
   
enddo

!% 6) ARBITRARY BRANCH
do i=1,nnode
    
    if (twork_iParent(i)/=0) then

        nChild=twork_nChild(i)
        do j=1,nChild
            iChilds(j)=twork_iChilds(i,j)
        enddo
   
        SecType=trim(dsheet(i, 6))    
        cType  =trim(PDMS_info2(i, 1))
        cType2 =trim(PDMS_info2(i, 2))
        FlowDir=trim(dsheet(i, 4))

        angleS(1)=str2num(trim(PDMS_info2(i,18)))
        angleS(2)=str2num(trim(PDMS_info2(i,19)))
        angleS(3)=str2num(trim(PDMS_info2(i,20)))
        angleS(4)=str2num(trim(PDMS_info2(i,21)))
        angleS(5)=str2num(trim(PDMS_info2(i,22)))
        angleS(6)=str2num(trim(PDMS_info2(i,23)))
        angleS_max = 0.
  
        angleS1=max(abs(angleS(2)),abs(angleS(3)))
        angleS2=max(abs(angleS(5)),abs(angleS(6)))
        angleS_max=max(angleS1,angleS2)

        w0 = d_w0(i)
        h0 = d_h0(i)
        d0 = d_d0(i)
        w1 = d_w1(i)
        h1 = d_h1(i)
        d1 = d_d1(i)

        ang1 = d_angleBend(i)
        rad1 = d_radiBend(i)
   
        if (nChild < 2) then
        
        !TAPER
            if (trim(cType)=='TAPE') then
                
                if ((trim(dsheet(i,13))=='SR5-13' .or. trim(dsheet(i,13))=='ER5-3') .and. trim(dsheet(i,14))=='B') then
                    
                else                  
            
                    if (trim(SecType)=='RECT') then
                
                        if (trim(cType2)=='TAPR') then
                
                            Para1=angleS_max*2.
                            Para2=(w0*h0)/(w1*h1)
                            Para3=0
                
                            if (trim(FlowDir)=='SUPPLY') then                    
                    
                                if (w0==w1 .or. w0==h1 .or. h0==w1 .or. h0==h1) then
                                    dsheet(i,19)='SR4-1';
                                else
                                    dsheet(i,19)='SR4-2';
                                endif
                    
                            else                    
                
                                if (w0==w1 .or. w0==h1 .or. h0==w1 .or. h0==h1) then
                                    dsheet(i,19)='ER4-1';
                                else
                                    dsheet(i,19)='ER4-2';
                                endif
                    
                            endif                            
                
                            dsheet(i,20)=real2str(Para1)
                            dsheet(i,21)=real2str(Para2)
                            dsheet(i,22)=real2str(Para3)
                    
                        endif                
            
                    elseif (trim(SecType)=='CIRC') then
                
                        if (trim(cType2)=='REDU') then
                
                            Para1=angleS_max*2.
                            Para2=(d0/d1)**2
                            Para3=0
                
                            if (trim(FlowDir)=='SUPPLY') then
                                dsheet(i,19)='SD4-1';
                            else
                                dsheet(i,19)='ED4-1';
                            endif
                
                            dsheet(i,20)=real2str(Para1)
                            dsheet(i,21)=real2str(Para2)
                            dsheet(i,22)=''
                
                        endif
            
                    endif
                
                endif            
            
            !BEND
            elseif (trim(cType)=='BEND') then                                
                                
                if (((trim(dsheet(i,13))=='SR5-1' .or. trim(dsheet(i,13))=='ER5-1') .and. trim(dsheet(i,14))=='B') &
                    .or. (trim(dsheet(i,13))=='SR5-14' .or. trim(dsheet(i,13))=='ER5-4')) then
                    
                else
            
                    if (trim(SecType)=='RECT') then
                
                        if (trim(cType2)=='RBEN') then
                    
                            Para1=ang1
                            Para2=h0/w0
                            Para3=rad1/w0+0.5
                    
                            if (w0 > 350) then
                                dsheet(i,19)='CR3-4'
                            elseif (w0 > 250) then
                                dsheet(i,19)='CR3-3'
                            else
                                dsheet(i,19)='CR3-1'
                            endif
                    
                            dsheet(i,20)=real2str(Para1)
                            dsheet(i,21)=real2str(Para2)
                            dsheet(i,22)=real2str(Para3)
                    
                        else
                                            
                            Para1=(w0*h0)/(w1*h1)
                            Para2=h0/w0
                            Para3=0
                    
                            if (trim(FlowDir)=='SUPPLY') then
                                !dsheet(i,19)='SR3-1';
                                dsheet(i,19)='CR3-1';
                            else
                                !dsheet(i,19)='ER3-1';
                                dsheet(i,19)='CR3-1';
                            endif
                    
                            dsheet(i,20)=real2str(Para1)
                            dsheet(i,21)=real2str(Para2)
                            dsheet(i,22)=''
                    
                        endif
                
                    elseif (trim(SecType)=='CIRC') then
                
                        if (trim(cType2)=='RBEN') then                            
                    
                            Para1=ang1
                            Para2=rad1/d0+0.5
                            Para3=0                    
                            dsheet(i,20)=real2str(Para1)
                            dsheet(i,21)=real2str(Para2)
                            dsheet(i,22)=''
                    
                        endif
                
                    endif 
                
                endif
            
            
            !ELBO
            elseif (trim(cType)=='ELBO') then
                
                !if (trim(dsheet(i,14))/='B') then
            
                    if (trim(SecType)=='RECT') then
                
                        if (trim(cType2)=='ELBO') then
                    
                            Para1=ang1
                            Para2=h0/w0
                            Para3=0
                            dsheet(i,19)='CR3-6'
                            dsheet(i,20)=real2str(Para1)
                            dsheet(i,21)=real2str(Para2)
                            dsheet(i,22)=''
                    
                        endif
                
                    endif
                
                !endif  
                
        
            !CAP
            elseif (trim(cType)=='CAP') then
            
                dsheet(i,19)='CAP'
                dsheet(i,20)='9999999.'
                dsheet(i,21)=''
                dsheet(i,22)=''
            
                dsheet(i,24)=''
                dsheet(i,25)=''
                dsheet(i,26)=''
                dsheet(i,27)=''
            
            
            !FLEX
            elseif (trim(cType)=='FLEX') then
            
                PDMS_info2(i,24) = '0.03'
            
                if (trim(SecType)=='CIRC') then
                
                    if (trim(cType2)=='FLXB') then
                    
                        Para1=ang1
                        Para2=rad1/d0+0.5
                        Para3=0                    
                        dsheet(i,20)=real2str(Para1)
                        dsheet(i,21)=real2str(Para2)
                        dsheet(i,22)=''
                    
                    endif
                
                endif            
            
        
            !MESH GRIL
            elseif (trim(cType)=='MESH' .or. trim(cType)=='GRIL') then
            
                if (trim(SecType)=='RECT') then
                
                    if (trim(cType2)=='MESH' .or. trim(cType2)=='RGRR') then
                    
                        dsheet(i,19)='CR6-1'
                        dsheet(i,20)='0.95'
                        dsheet(i,21)='1.0'
                        dsheet(i,22)=''
                    
                    endif
                
                elseif (trim(SecType)=='CIRC') then
                
                    if (trim(cType2)=='MESH') then
                    
                        dsheet(i,19)='CD6-1'
                        dsheet(i,20)='0.95'
                        dsheet(i,21)='1.0'
                        dsheet(i,22)=''
                    
                    endif
                
                endif
            
            
            !IDAM DAMP
            elseif (trim(cType)=='IDAM' .or. trim(cType)=='DAMP') then
            
                if (trim(SecType)=='RECT') then
                    
                    dsheet(i,19)='A-15G'
                    dsheet(i,20)='0'
                    dsheet(i,21)=''
                    dsheet(i,22)=''
                
                elseif (trim(SecType)=='CIRC') then 
                    
                    dsheet(i,19)='A-15G'
                    dsheet(i,20)='0'
                    dsheet(i,21)=''
                    dsheet(i,22)=''
                
                endif
            
            
            !TRNS
            elseif (trim(cType)=='TRNS') then
                
                !d_sectype(i)='TRAN'
                !dsheet(i, 6)=d_sectype(i)  ! section type
                
                Para1=angleS_max*2.
        
                iChilds(1)=twork_iChilds(i,1)
                ic=iChilds(1)
                SecTypeChild =trim(d_sectype(ic))
            
                if (SecTypeChild=='RECT') then             
                
                    if (d0/=0) then
                    Para2=(0.25*pi*d0**2)/(w1*h1)
                    elseif (d1/=0) then
                Para2=(0.25*pi*d1**2)/(w0*h0)
                    else
                        Para2=1.d0
                    endif
                    
                    if ( trim(FlowDir)=='SUPPLY') then
                        dsheet(i,19)='SR4-3'
                    else
                        dsheet(i,19)='ED4-2'
                    endif
                
                elseif (SecTypeChild=='CIRC') then
                
                    if (d0/=0) then
                    Para2=(w1*h1)/(0.25*pi*d0**2)
                    elseif (d1/=0) then
                             Para2=(w0*h0)/(0.25*pi*d1**2)
                    else
                        Para2=1.d0
                    endif
                    
                
                    if ( trim(FlowDir)=='SUPPLY') then
                        dsheet(i,19)='SD4-2'
                    else
                        dsheet(i,19)='ER4-3'
                    endif
                
                endif
            
                dsheet(i,20)=real2str(Para1)
                dsheet(i,21)=real2str(Para2)
                dsheet(i,22)=''            
        
            endif           
            
        endif 
            
    endif    
    
enddo

!% 6-1) Offset
do i=1,nnode
!print *,i,nnode,'AAA'  
   SecType=trim(dsheet(i, 6))
   cType  =trim(PDMS_info2(i, 1))
   cType2 =trim(PDMS_info2(i, 2))
   FlowDir=trim(dsheet(i, 4))
!print *,i,nnode,'BBB',trim(PDMS_info2(i,18)),' ',trim(PDMS_info2(i,19)),' ',trim(PDMS_info2(i,20)),' ',trim(PDMS_info2(i,21)),' ',trim(PDMS_info2(i,22)),' ',trim(PDMS_info2(i,23))
   ! angle
   angleS(1)=str2num(trim(PDMS_info2(i,18)))
   angleS(2)=str2num(trim(PDMS_info2(i,19)))
   angleS(3)=str2num(trim(PDMS_info2(i,20)))
   angleS(4)=str2num(trim(PDMS_info2(i,21)))
   angleS(5)=str2num(trim(PDMS_info2(i,22)))
   angleS(6)=str2num(trim(PDMS_info2(i,23)))
   angleS_max = 0.
  
   angleS1=max(abs(angleS(2)),abs(angleS(3)))
   angleS2=max(abs(angleS(5)),abs(angleS(6)))
   angleS_max=max(angleS1,angleS2)
!print *,i,nnode,'CCC'  
   ! section
   w0 = d_w0(i)
   h0 = d_h0(i)
   d0 = d_d0(i)
   w1 = d_w1(i)
   h1 = d_h1(i)
   d1 = d_d1(i)

   ang1 = d_angleBend(i)
   rad1 = d_radiBend(i)
!print *,i,nnode,'DDD'   
   if (trim(cType)=='OFST') then

      if (trim(SecType)=='RECT' .and. trim(cType2)=='MOFF') then
         dsheet(i,19)='CR3-6_OFST'
         dsheet(i,20)=real2str(angleS_max)
         dsheet(i,21)=''
         dsheet(i,22)=''
      elseif (trim(SecType)=='RECT' .and. trim(cType2)=='SWAN') then
         dsheet(i,19)='CR3-6_OFST'
         dsheet(i,20)=real2str(angleS_max)
         dsheet(i,21)=''
         dsheet(i,22)=''
      elseif (trim(SecType)=='CIRC' .and. trim(cType2)=='MOFF') then
         dsheet(i,19)='CR3-6_OFST'
         dsheet(i,20)=real2str(angleS_max)
         dsheet(i,21)=''
         dsheet(i,22)=''
      elseif (trim(SecType)=='CIRC' .and. trim(cType2)=='SWAN') then
         dsheet(i,19)='CR3-6_OFST'
         dsheet(i,20)=real2str(angleS_max)
         dsheet(i,21)=''
         dsheet(i,22)=''
      elseif (trim(SecType)=='CIRC' .and. trim(cType2)=='OFFS') then
         dsheet(i,19)='CR3-6_OFST'
         dsheet(i,20)=real2str(angleS_max)
         dsheet(i,21)=''
         dsheet(i,22)=''
      endif  
      
   elseif (trim(cType)=='ELBO') then
      !PRINT *,trim(SecType),trim(cType2)
      if ((trim(SecType)=='CIRC' .or. trim(SecType)=='ETC').and. trim(cType2)=='') then
         Para1=90.   ! PIPE element 임. 정보 없음. 90도로 가정
         Para2=1.    ! PIPE element 임. 정보 없음. 1로 가정
         Para3=0
         dsheet(i,19)='ELBOW(R'//trim(int2str(int(Para1)))//')'
         dsheet(i,20)=real2str(Para1)
         dsheet(i,21)=real2str(Para2)
         dsheet(i,22)=''
      endif

       if (trim(SecType)=='CIRC' .and. trim(cType2)=='ELBO') then
         Para1=ang1
         Para2=rad1/d0+0.5
         Para3=0
         !dsheet(i,19)='ELBOW'   ! 160415 Default 를 Mitered 로.
         dsheet(i,19)='ELBOW(M'//trim(int2str(int(Para1)))//')'
         dsheet(i,20)=real2str(Para1)
         dsheet(i,21)=real2str(Para2)
         dsheet(i,22)=''
      endif

      if (trim(SecType)=='CIRC' .and. trim(cType2)=='BEND') then
         Para1=ang1
         Para2=rad1/d0+0.5
         Para3=0
         dsheet(i,19)='ELBOW'
         dsheet(i,20)=real2str(Para1)
         dsheet(i,21)=real2str(Para2)
         dsheet(i,22)=''
      endif      

   endif
   
enddo

!EQUI
do i=1,nnode
    
    !SecType=trim(dsheet(i, 6))
    cType  =trim(PDMS_info2(i, 1))
    cSPREF =trim(PDMS_info2(i,30))
    yes_found = .false.
   
    if ( trim(cType) == 'EQUI' ) then

        if (.not. yes_found) then
            
            do icomp=1,nrow_ETC
                
                call match_spref(cSPREF,spref_ETC(icomp,2),spref_ETC(icomp,3),yes_matchf)
                
                if ( yes_matchf ) then                    
                    
                    if (trim(spref_ETC(icomp, 7))/='') dsheet(i, 9) = trim(spref_ETC(icomp, 7)) !W        
                    if (trim(spref_ETC(icomp, 8))/='') dsheet(i,10) = trim(spref_ETC(icomp, 8)) !H        
                    if (trim(spref_ETC(icomp, 9))/='') dsheet(i,11) = trim(spref_ETC(icomp, 9)) !D        
                    if (trim(spref_ETC(icomp, 5))/='') dsheet(i,18) = trim(spref_ETC(icomp, 5)) !NAME             
                    if (trim(spref_ETC(icomp,11))/='') dsheet(i,19) = trim(spref_ETC(icomp,11)) !FITTING CODE              
                    if (trim(spref_ETC(icomp,12))/='') dsheet(i,20) = trim(spref_ETC(icomp,12)) !PARA1
                    if (trim(spref_ETC(icomp,13))/='') dsheet(i,21) = trim(spref_ETC(icomp,13)) !PARA2
                    if (trim(spref_ETC(icomp,14))/='') dsheet(i,22) = trim(spref_ETC(icomp,14)) !PARA3
                    
                    exit
                    
                endif
                
            enddo
        
        endif
        
    endif
    
enddo


!% User Input
do i=1,nnode

   if (len_trim(PDMS_info2(i,31)) > 0) then
      dsheet(i,19)=trim(PDMS_info2(i,31))
      dsheet(i,20)=trim(PDMS_info2(i,32))
      dsheet(i,21)=trim(PDMS_info2(i,33))
      dsheet(i,22)=trim(PDMS_info2(i,34))
      dsheet(i,23)=trim(PDMS_info2(i,35))
   endif
   ! correction
   if (len_trim(PDMS_info2(i,35)) > 0) then
      dsheet(i,23)=trim(PDMS_info2(i,35))
   else
      !dsheet(i,23)='1'
   endif
   
   if (len_trim(PDMS_info2(i,36)) > 0) then
      dsheet(i,24)=trim(PDMS_info2(i,36))
      dsheet(i,25)=trim(PDMS_info2(i,37))
      dsheet(i,26)=trim(PDMS_info2(i,38))
      dsheet(i,27)=trim(PDMS_info2(i,39))
      dsheet(i,28)=trim(PDMS_info2(i,40))
   endif
   ! correction
   if (len_trim(PDMS_info2(i,40)) > 0) then
      dsheet(i,28)=trim(PDMS_info2(i,40))
   else
      !dsheet(i,28)='1'
   endif
   
   if (len_trim(PDMS_info2(i,41)) > 0) then
      dsheet(i,13)=trim(PDMS_info2(i,41))
      dsheet(i,14)=trim(PDMS_info2(i,42))
      dsheet(i,15)=trim(PDMS_info2(i,43))
      dsheet(i,16)=trim(PDMS_info2(i,44))
      dsheet(i,17)=trim(PDMS_info2(i,45))
   endif
   ! correction
   if (len_trim(PDMS_info2(i,45)) > 0) then
      dsheet(i,17)=trim(PDMS_info2(i,45))
   else
      !dsheet(i,17)='1'
   endif
   
enddo

!% User Input PQN
do i=1,nnode

   j = 19
   if (trim(dsheet(i, j))=='PQN') then
      if (trim(dsheet(i, j+2)) =='' ) then
         dsheet(i,j+2)=trim(real2str(d_flowrate(i)))
      endif
      if (trim(dsheet(i, j+3)) =='' ) then
         dsheet(i,j+3)='2'
      endif
   endif
      
   j = 24
   if (trim(dsheet(i, j))=='PQN') then
      if (trim(dsheet(i, j+2)) =='' ) then
         dsheet(i,j+2)=trim(real2str(d_flowrate(i)))
      endif
      if (trim(dsheet(i, j+3)) =='' ) then
         dsheet(i,j+3)='2'
      endif
   endif

enddo

!Diffuser
do i=1,nnode

    SecType=trim(dsheet(i, 6))
    cType  =trim(PDMS_info2(i, 1))
    cType2 =trim(PDMS_info2(i, 2))
    FlowDir=trim(dsheet(i, 4))

    if (abs(twork_yDiffuser(i))==1) then
        
        if ( trim(FlowDir)=='SUPPLY') then
            !dsheet(i,18)='TERMINAL'
            dsheet(i,24)='DISCHRG'
            dsheet(i,25)='1.0'
            dsheet(i,26)=''
            dsheet(i,27)=''
        else
            !dsheet(i,18)='TERMINAL'
            dsheet(i,24)='SUCTION'
            dsheet(i,25)='0.5'
            dsheet(i,26)=''
            dsheet(i,27)=''
        endif
      
    endif
   
enddo

! Memo
do i=1,nnode
   if (len_trim(PDMS_info2(i,50)) > 0) then
      dsheet(i,18)=trim(PDMS_info2(i,50))
   endif
enddo

end subroutine

!----------------------------------------------------------------------------------------------

subroutine export_dsheet()
use globy
use funcs
implicit none

integer :: fid=200
integer :: io_status
integer :: i,i2,nnode,ipos
character(len=64) :: cipos
character(len=64) :: cinp
character(len=64+MAXBRAN*6) :: cout

nnode=nnode_tree

!open(unit=fid,file='C:\temp_dsheet.dexp',iostat=io_status,form='FORMATTED')
open(unit=fid,file=trim(AMlocaltemp_path)//'\tmp\temp_dsheet.dexp',iostat=io_status,form='FORMATTED')
if (io_status /= 0) then
   write (*, *) 'File Open Error'
   stop
end if
write(fid,'(53(a,","),a)') 'Memo','Name','Parent','Dir','Desc','Sectype','TargetQ','Length','SecA','SecB','SecD','Rough', &
   'DBNAME','Ttype','Order','P1','P2','Correction', &
   'S1type','P1','P2','P3','Correction', &
   'S2type','P1','P2','P3','Correction', &
   'S3type','P1','P2','P3','Correction', &
   'S4type','P1','P2','P3','Correction', &
   'Blank','TreeID','TreeLevel','BranLevel','BranID','ElemID','Name2', &
   'Blank','Blank','Spref', &
   'RoomName','UnitNo','RoomPres', &
    'X(Pos1)','Y(Pos1)','Z(Pos1)'

do i2=1,nnode
   i=idx_sort_all(i2)

      !print *,i2,i,nnode,'a'


   !i=i2

   !print *,i,i2
   !if (i==iroot_tree) then
   !   print *,'Root',i
   !endif

   if (i==0) then
      print *,'Error :',trim(dsheet(i2, 2)),i2,nnode
   endif

   
   !ipos = i
   if (i == twork_iFan) then
      ipos = 0
   elseif (trim(dsheet(i, 4)) == 'EXTRACT') then
      ipos = -i
   else
      ipos = i
   endif
   
   cinp=trim(dsheet(i, 5))
   !call name_tree_2(cinp,cout,i)
   !call name_tree_3(cinp,cout,i)
   call name_tree_3s(cinp,cout,i)
   !cout=trim(cinp)
   write(*,'(a)') trim(cout(1:79))

   write(fid,'(53(a,","),a)') &
      trim(int2str(twork_LevelS2(i))),trim(dsheet(i, 2)),trim(dsheet(i, 3)),trim(dsheet(i, 4)), &             !trim(dsheet(i, 1)) 대신 trim(int2str(ipos)) 사용. 151222
      trim(dsheet(i, 5)),trim(dsheet(i, 6)),trim(dsheet(i, 7)),trim(dsheet(i, 8)), &
      trim(dsheet(i, 9)),trim(dsheet(i,10)),trim(dsheet(i,11)),trim(dsheet(i,12)), &
      trim(dsheet(i,18)),trim(dsheet(i,13)),trim(dsheet(i,14)),trim(dsheet(i,15)),trim(dsheet(i,16)), &
      trim(dsheet(i,17)),trim(dsheet(i,19)),trim(dsheet(i,20)), &
      trim(dsheet(i,21)),trim(dsheet(i,22)),trim(dsheet(i,23)),trim(dsheet(i,24)), &
      trim(dsheet(i,25)),trim(dsheet(i,26)),trim(dsheet(i,27)),trim(dsheet(i,28)), &
      trim(dsheet(i,29)),trim(dsheet(i,30)),trim(dsheet(i,31)),trim(dsheet(i,32)), &
      trim(dsheet(i,33)),trim(dsheet(i,34)),trim(dsheet(i,35)),trim(dsheet(i,36)), &
      trim(dsheet(i,37)),trim(dsheet(i,38)), &
      '', &
      trim(int2str(twork_ID(i))),trim(int2str(twork_Level(i))),trim(int2str(twork_BLevel(i))),trim(int2str(twork_BranID(i))),trim(int2str(twork_BelemID(i))),trim(cout), &
      '','',trim(PDMS_info2(i,30)), &
      trim(PDMS_info2(i,59)),trim(PDMS_info2(i,61)),trim(PDMS_info2(i,62)), &    !'RoomName','UnitNo','RoomPres'
    trim(PDMS_info2(i,51)),trim(PDMS_info2(i,52)),trim(PDMS_info2(i,53))
   
   !print *,i,PDMS_info2(i,59)

   !print *,i2,i,nnode,'b'
enddo
close(fid)

end subroutine

!----------------------------------------------------------------------------------------------

subroutine import_csv_PDMS()
! Objectives : 
!    1) read sheet_read 
!    2) trim sheet_read 
use globy
use strings
implicit none

integer :: i,j
integer :: nrow,ncol,icount
integer :: fid=110
integer :: io_status
character(len=6) :: cSTOP1
character(len=512) :: cline_read
character(len=128) :: tokens(70)
integer :: ntokens

! Read
!open(unit=fid,file='C:\temp.txt',iostat=io_status,form='FORMATTED')
open(unit=fid,file=trim(AMlocaltemp_path)//'\tmp\temp.dat',iostat=io_status,form='FORMATTED',status='old')
if (io_status /= 0) then
   write (*, *) 'File Open Error:'
   write (*, *) '  '//trim(AMlocaltemp_path)//'\tmp\temp.dat'
   stop
end if

cSTOP1 = 'EOF';

icount=0;
do while (.not. EOF(fid))
   read(fid,"(a)") cline_read

   if (len(trim(cline_read)) <= 3) then
      ! for AM 12.1 (151207)
      cycle
   endif
   if ( trim(cline_read) == trim(cSTOP1) ) then
      exit
   endif
   icount=icount+1
   !print *,icount
   !c parse the whole line
   call parse(cline_read, ',', tokens, ntokens)
   do j=1,ntokens
      sheet_read(icount,j)=trim(tokens(j))
   enddo

enddo
nrow=icount
ncol=ntokens

! globy : sheet_read, nrow_sheet_read, ncol_sheet_read
! do ~ enddo 삭제 가능??? 160427
do i=1,nrow
   do j=1,ntokens
      tokens(j)=sheet_read(i,j)
   enddo
   !print *, trim(tokens(1)),',',trim(tokens(2)),',',trim(tokens(3))
enddo
nrow_sheet_read=nrow
ncol_sheet_read=ncol

close(fid)

end subroutine

!----------------------------------------------------------------------------------------------

subroutine trim_csv_PDMS()
! Objectives : trim sheet_read 
!    1) trim fullname
use globy
use ISO_VARYING_STRING
use strings
use funcs
implicit none

integer :: i,j
integer :: nrow,ncol,ii,icount
character(len=256) :: cline_temp
character(len=128) :: tokens(10)
integer :: ntokens

nrow=nrow_sheet_read
ncol=ncol_sheet_read

! COL3 : trim fullname : 원래 이름 유지.(151215)
do i=1,nrow

if (1==0) then
   cline_temp = REPLACE(sheet_read(i, 3),' of ','~',.TRUE.)
   call parse(cline_temp, '~', tokens, ntokens)
   if (ntokens>=2) then
      if (len(trim(tokens(2)))>0) then
         tokens(2) = REPLACE(tokens(2),'BRANCH ','',.TRUE.)
         tokens(2)=' ('//trim(tokens(2))//')'
      endif
   endif
   sheet_read(i, 3) = trim(tokens(1))//trim(tokens(2))
   
elseif (1==1) then   
   !cline_temp = REPLACE(sheet_read(i, 3),'BRANCH ','BRAN ',.TRUE.)
   cline_temp = trim(sheet_read(i, 3))
   sheet_read(i, 3) = trim(cline_temp)
endif

enddo

! COL1, COL2 : A 지우기
!do i=1,nrow
   !sheet_read(i, 1) = REPLACE(sheet_read(i, 1),'A','',.FALSE.)
   !sheet_read(i, 2) = REPLACE(sheet_read(i, 2),'A','',.FALSE.)
!enddo

! COL2 : 유량 추출, Discharge/Suction
do i=1,nrow
   cline_temp = REPLACE(sheet_read(i, 2),')','',.TRUE.)
   cline_temp = REPLACE(cline_temp,' (','~',.TRUE.)
   call parse(cline_temp, '~', tokens, ntokens)
   
   if (ntokens>=2) then
      if (is_digit(trim(tokens(1)))) then
         sheet_read(i, 2) = 'END'
         sheet_read(i,IDXF1) = tokens(1)
         sheet_read(i,IDXF2) = tokens(2)
      else
         sheet_read(i, 2) = 'END'
         sheet_read(i,IDXF1) = '0'
         sheet_read(i,IDXF2) = tokens(2)
      endif
   else
      ! 아무 작업 안하는게 나음.
      ! NOP
      sheet_read(i,IDXF1) = '-1'
      !sheet_read(i,IDXF2) = ''
   endif
   
enddo

end subroutine


!----------------------------------------------------------------------------------------------

subroutine arrange_PDMS()

use globy
use funcs
use ISO_VARYING_STRING
use strings
implicit none

integer :: i,j,ir,icopy,ir_add,ic,nchild,nroot,ilevel,ip,i2
integer :: nnode_read,i_nonreg,nnode_nonreg,nnode_read2
integer :: ifound(16),nfound
integer :: icount1,icount2

character(len=32),dimension(MAXLINE) :: col_idmy_read,col_idch_read,col_type_read
character(len=32),dimension(MAXLINE) :: col_idmy,col_idpr
integer,dimension(MAXLINE) :: col_i_pr
integer,dimension(MAXLINE) :: col_i_nchild,col_i_nparnt
integer,dimension(MAXLINE,MAXCHILD) :: col_i_ch
integer,dimension(MAXLINE) :: col_i_level
real,dimension(MAXLINE) :: col_i_flow
integer,dimension(MAXLINE) :: col_i_dirn
integer,dimension(MAXLINE) :: i_ir,ic_ir,ir_i
integer :: nnode
integer :: max_level
integer :: iParent,nBrother,ib
character(len=64) :: c_temp

nnode_read=nrow_sheet_read
do ir=1,nnode_read
   col_idmy_read(ir)=sheet_read(ir, 1)
   col_idch_read(ir)=sheet_read(ir, 2)
   col_type_read(ir)=sheet_read(ir, 4)
enddo



! 자식이 자기 list 속에 없을 때, 자식을 등록 <----------- 일단 무시하고 진행
! Find non-resgistered child (Attribute가 없음)
i_nonreg=0
if (1) then  ! 무시하였음.
do ir=1,nnode_read
   call findstr(col_idmy_read,nnode_read,col_idch_read(ir),ifound,nfound)
   if (nfound==0 .and. len_trim(col_idch_read(ir))>0  .and. trim(col_idch_read(ir))/='END') then
      i_nonreg=i_nonreg+1;
      ir_add=nnode_read+i_nonreg;
      
      sheet_read(ir_add, 1)=col_idch_read(ir)
      sheet_read(ir_add, 2)=''
      sheet_read(ir_add, 3)='UNKNOWN (W/O ATTRIBUTE)'
      !sheet_read(ir_add, 3)='UNKNOWN TERMINAL'
      sheet_read(ir_add, 4)='UNKNOWN'

      !print *,trim(col_idmy_read(ir)),trim(col_idch_read(ir)),ir,ir_add
      
      col_idmy_read(ir_add)=sheet_read(ir_add, 1)   ! 자식을 등록
      col_idch_read(ir_add)=''
      col_type_read(ir_add)=sheet_read(ir_add, 4)
   endif
enddo
endif


nnode_nonreg=i_nonreg;
nnode_read2=nnode_read+nnode_nonreg;   !%%%%%%%%%%%%%%

!% Find nnode
call unique_num(col_idmy_read,nnode_read2,nnode)

!print *, nnode_read, nnode_read2, nnode

!print *,' '
!print *,'nnode_read   = ', nnode_read
!print *,'nnode_nonreg = ', nnode_nonreg
!print *,'nnode_read2  = ', nnode_read2
!print *,'nnode        = ', nnode
!print *,' '

if (0) then
do i=1,nnode_read2
   print *, trim(sheet_read(i, 1)),',',trim(sheet_read(i, 2)),',',trim(sheet_read(i, 3)),',',trim(sheet_read(i, 4)),',',trim(sheet_read(i,IDXF1)),',',trim(sheet_read(i,IDXF2))
enddo
endif


!% make i_ir
i=0
do ir=1,nnode_read2
   call findstr(col_idmy,nnode,col_idmy_read(ir),ifound,nfound)
   if (nfound==0) then
      i=i+1;
      i_ir(ir)=i;
      col_idmy(i)=col_idmy_read(ir)
      !print *, '1', i_ir(ir)
   else
      i_ir(ir)=ifound(1)
      !print *, '2', i_ir(ir)
   endif
enddo

!% make ir_i
ir_i(:)=0
do i=1,nnode
   call findstr(col_idmy_read,nnode_read2,col_idmy(i),ifound,nfound)
   if (nfound>=1) then
      ir_i(i)=ifound(1)
      !print *, ir_i(i)
   else
      print *,'Error : ir_i'
   endif
enddo


!% make ic_ir
do ir=1,nnode_read2
   call findstr(col_idmy,nnode,col_idch_read(ir),ifound,nfound)
   if (nfound==1) then
      ic_ir(ir)=ifound(1);
   else
      !% No child
   endif
enddo

!% make col_i_pr
col_i_pr(:)=0
do ir=1,nnode_read2
   i=i_ir(ir)
   ic=ic_ir(ir)
   if (ic /= 0) then
      col_i_pr(ic)=i
      col_idpr(ic)=col_idmy(i)
   endif
enddo

!% Make 'col_i_ch' : 줄 속에서 내 자식의 위치 찾기. 자식3명 초과는 처리 루틴에서 에러 뜰 듯...
do i=1,nnode
   call findstr(col_idpr,nnode,col_idmy(i),ifound,nfound)
   !nchild=nfound;   ! 임시
   nchild=min(nfound,MAXCHILD);
   col_i_nchild(i)=nchild;
   if (nchild > MAXCHILD) then
      print *,'Warning : No. of Child is over 24.'
   endif
   do j=1,nchild
      !!!col_i_ch(i,j)=ifound(j)
      col_i_ch(i,j)=ifound(nchild+1-j)         ! 150720A twork_iChilds 순서가 뒤집혀있는 이유.(151222)
   enddo
enddo

!% Make 'col_i_nparent' : 부모 유무
do i=1,nnode
   if (col_i_pr(i)==0) then
      col_i_nparnt(i)=0
   else
      col_i_nparnt(i)=1
   endif
enddo

!% Make 'col_i_level' : Level
max_level=0
do i=1,nnode
   ip = col_i_pr(i)
   ilevel = 0
   j=0
   do while (ip > 0)
      ilevel = ilevel + 1
      ip = col_i_pr(ip)
      j=j+1
      if (j==100000) exit
   enddo
   col_i_level(i)=ilevel+1
   max_level=max(max_level,col_i_level(i))
enddo


!% Make 'col_i_flow' : flow
if (1) then
do ilevel=max_level,1,-1
   do i=1,nnode
      if (col_i_level(i)==ilevel) then
         ir=ir_i(i)
         col_i_flow(i)=0
         if (trim(sheet_read(ir, 2))=='END') then
            col_i_flow(i)=str2num(sheet_read(ir,IDXF1))
            if (trim(sheet_read(ir,IDXF2))=='Discharge') then
               col_i_dirn(i)=1
            elseif (trim(sheet_read(ir,IDXF2))=='Suction') then
               col_i_dirn(i)=-1
            elseif (trim(sheet_read(ir,IDXF2))=='Capped') then
               !col_i_dirn(i)=99
               col_i_dirn(i)=0
            else
               col_i_dirn(i)=0
            endif
         elseif (ilevel == 1) then
            col_i_flow(i)=0
            col_i_dirn(i)=0
            ! 150720 new, 150722 debug
            if (col_i_nchild(i)==1) then
               ic=col_i_ch(i,1)
               col_i_flow(i)=col_i_flow(ic)
            elseif (col_i_nchild(i)==2) then
               col_i_flow(i)=0.5*(col_i_flow(col_i_ch(i,1))+col_i_flow(col_i_ch(i,2)))
            else
               print *,'Error in col_i_flow(i) (root nchild>2)', i,sheet_read(ir_i(i), 3)
            endif
         else
            do i2=1,col_i_nchild(i)
               ic=col_i_ch(i,i2)
               col_i_flow(i)=col_i_flow(i)+col_i_flow(ic)
               if (col_i_dirn(ic) .ne. 99 .and. col_i_dirn(ic) .ne. 0) then
                   ! 151120 Cap 때문에 모두 오염되는 것 방지.
                   col_i_dirn(i)=col_i_dirn(ic)
               endif
            enddo
         endif
      endif
   enddo
enddo
endif



!% Flow Dir 정리
do ilevel=2,max_level
   do i=1,nnode
      if (col_i_level(i)==ilevel) then
         ir=ir_i(i)

         do i2=1,col_i_nchild(i)
            ic=col_i_ch(i,i2)
            col_i_dirn(ic) = col_i_dirn(i)
         enddo
      endif
   enddo
enddo



if (0) then
write(*,'(a)') '   I     PARENT   CH1     CH2     CH3'
write(*,'(a)') '-------!-------!-------!-------!-------!'
do i=1,nnode
   write(*,'(5i8)') i, col_i_pr(i), col_i_ch(i,1), col_i_ch(i,2), col_i_ch(i,3)
enddo
endif




nnode_tree=nnode
do i=1,nnode
   do j=1,50+6+6               
      PDMS_info2(i,j)=sheet_read(ir_i(i), 3+j)
      !print *,i,trim(PDMS_info2(i,59))

      !comma 들어간 것들을 ?로 대체 - 160407 --> 안됨. 참고로 남겨 놓음. 지워야 함.
      !c_temp = REPLACE(trim(PDMS_info2(i,j)),',','?',.TRUE.)
      !PDMS_info2(i,j) = trim(c_temp)
   enddo

   
   PDMS_info1(i, 1)=sheet_read(ir_i(i), 1)
   PDMS_info1(i, 3)=sheet_read(ir_i(i), 3)      ! 11: name
   PDMS_info1(i,11)=int2str(col_i_nparnt(i))    ! 12: nparent
   PDMS_info1(i,12)=int2str(col_i_nchild(i))    ! 13: nchild
   PDMS_info1(i, 4)=int2str(col_i_level(i))     ! 14: level
   PDMS_info1(i, 5)=real2str(col_i_flow(i))     ! 15: flowrate
   if (col_i_dirn(i) == 1) then
      PDMS_info1(i, 6)='SUPPLY'
   elseif (col_i_dirn(i)  == -1) then
      PDMS_info1(i, 6)='EXTRACT'
   elseif (col_i_dirn(i)  == 99) then
      PDMS_info1(i, 6)='CAPPED'
   else
      PDMS_info1(i, 6)='ETC'
   endif
   
   if (col_i_pr(i)>0) then
      PDMS_info1(i, 2)=sheet_read(ir_i(col_i_pr(i)), 1)
   endif
   ! 3~10
   do ic=1,MAXCHILD
      if (col_i_ch(i,ic)>0) then
         PDMS_info1(i,12+ic)=sheet_read(ir_i(col_i_ch(i,ic)), 1)
      endif
   enddo
enddo

!% Check
nroot=0
do i=1,nnode
   ir=ir_i(i)
   if (col_i_nparnt(i)==0) then
      if (col_i_nchild(i)>0) then
         if (nroot==0) then
            nroot=nroot+1
            PDMS_info1(i,10)='ROOT'
            iroot_tree=i
         else
            nroot=nroot+1
            PDMS_info1(i,10)='ROOT ('//trim(int2str(nroot))//') <-- ERROR'
         endif
      else
         PDMS_info1(i,10)='ORPHAN <-- WARNING'
      endif
   else
       if (col_i_nchild(i)==0) then
         if (trim(sheet_read(ir, 2))=='END' .and. str2num(sheet_read(ir,IDXF1)) >= 0) then
             PDMS_info1(i,10)=trim(sheet_read(ir,IDXF1))//' ('//trim(sheet_read(ir,IDXF2))//')'
         else
             PDMS_info1(i,10)='WRONG END <-- ERROR'
         endif
      endif
   endif
enddo

!% Check
do i=1,nnode
   !c   if (i==col_i_nparnt(i)) then     <------- 16-03-03 까지 버그
   if (i==col_i_pr(i)) then
       PDMS_info1(i,10)='SELF-REFER <-- ERROR'   ! 민동이 연구원 문의 150527
   endif
enddo

!% Check
do i=1,nnode
   if (len_trim(PDMS_info1(i, 1))==0) then
       PDMS_info1(i,10)='BLANK <-- ERROR'
   endif
enddo

!% Check (Fan inlet, outlet)
i=iroot_tree
nchild=col_i_nchild(i)
icount1 = 0
icount2 = 0
if (nchild == 0) then
   PDMS_info1(i,10)='NO CHILD <-- ERROR'
else
   do j=1,nchild
      ic=col_i_ch(i,j)
      if ( trim(PDMS_info1(ic, 6)) == 'SUPPLY' ) then
         icount1 = icount1 + 1
      elseif ( trim(PDMS_info1(ic, 6)) == 'EXTRACT' ) then
         icount2 = icount2 + 1
      endif
   enddo
endif
if (icount1 > 1) then
   PDMS_info1(i,10)='NO OF ROOT OUTLET > 1 <-- ERROR'
   print *,'Fan Outlet should be connected to single element.', icount1
endif
if (icount2 > 1) then
   PDMS_info1(i,10)='NO OF ROOT INLET > 1 <-- ERROR'
   print *,'Fan Inlet  should be connected to single element.', icount2
endif



!
if (0) then
write(*,'(a)') '   I     PARENT   CH1     CH2     CH3'
write(*,'(a)') '-------!-------!-------!-------!-------!'
do i=1,nnode
    write(*,'(9(a,","),a)') &
      trim(PDMS_info1(i, 1)),trim(PDMS_info1(i, 2)),trim(PDMS_info1(i,13)),trim(PDMS_info1(i,14)),trim(PDMS_info1(i,15)), &
      trim(PDMS_info1(i,16)),trim(PDMS_info1(i,17)),trim(PDMS_info1(i,18)),trim(PDMS_info1(i,19)),trim(PDMS_info1(i,20))
enddo
endif

!% twork
do i=1,nnode

   ! twork_FlowDir
   if (col_i_dirn(i) == 1) then
      twork_FlowDir(i) = 1
   elseif (col_i_dirn(i) == -1) then
      twork_FlowDir(i) = -1
   elseif (col_i_dirn(i) == 99) then
      twork_FlowDir(i) = 0
   else
      twork_FlowDir(i) = 0
   endif
   
   ! twork_yDiffuser
   if (col_i_nchild(i)==0) then
      twork_yDiffuser(i) = twork_FlowDir(i)
   else
      twork_yDiffuser(i) = 0
   endif

   ! twork_iParent
   twork_iParent(i) = col_i_pr(i)

   if (twork_iParent(i)==0) then
      twork_iFan = i
   endif
   
   ! twork_nChild
   twork_nChild(i) = col_i_nchild(i)

   ! twork_iChilds
   do j=1,twork_nChild(i)
      twork_iChilds(i,j) = col_i_ch(i,j)
   enddo
   
   ! twork_Level
   twork_Level(i)=col_i_level(i)
   
enddo

!% Find Brothers
do i=1,nnode
   iParent=twork_iParent(i)
   nChild=twork_nChild(i)
   
   if (iParent > 0) then

      nBrother=0
      do j=1,twork_nChild(iParent)
         ib=twork_iChilds(iParent,j)
         if (i /= ib) then
            nBrother=nBrother+1
            twork_iBrothers(i,nBrother)=ib
         endif
      enddo
      twork_nBrother(i) = nBrother
      !print *, nBrother
   endif
enddo

!% twork_ID
do i=1,nnode
   if (twork_FlowDir(i) == -1) then
    twork_ID(i) = -9
      if (twork_nChild(i)==0) twork_ID(i) = -1
      if (twork_nChild(i)>1) twork_ID(i) = -2
      if (twork_nBrother(i)>=1) twork_ID(i) = -3
   elseif (twork_FlowDir(i) == 1) then
    twork_ID(i) = +9 
      if (twork_nChild(i)==0) twork_ID(i) = +1
      if (twork_nChild(i)>1) twork_ID(i) = +2
      if (twork_nBrother(i)>=1) twork_ID(i) = +3
   elseif (twork_FlowDir(i) == 99) then
      twork_ID(i) = 0
   else
      twork_ID(i) = 0
   endif
enddo

twork_nMaxLevel = max_level

end subroutine

!----------------------------------------------------------------------------------------------

subroutine trim_twork_iChilds()
use globy
use ISO_VARYING_STRING
use strings
use funcs
implicit none

integer :: fid=200
integer :: i,nnode,i2,ib,ic
character(len=32) :: cRefno,cType,cType2,SecType,cTypeChild,cRefnoChild,TfitOrder
integer :: nChild,nBrother,iParent,iBrother1,iBrother2,iChild1,iChild2
integer :: iChilds(MAXCHILD),iBrothers(MAXCHILD-1)
integer :: itmp,j,j1,jcount
real :: rOrder(MAXCHILD),rOrderSort(MAXCHILD)
integer :: iOrderSort(MAXCHILD)

nnode=nnode_tree

do i=1,nnode

! 150720A (Child 순서 뒤집은 상태. 바로 뒤에 나오는 녀석이 STRT일 가능성이 높다. STRT를 맨 뒤로 빼야 PLOT에서 이해가 잘 감.)
! AM에서 NEXT Connection 을 연결하고 NEXT 요소가 바로 잡히는 녀석들은 순서를 다시 뒤집어야 함. (BRCO, C_TEE, C_THRE)
if (twork_iParent(i)/=0) then

   cRefno =trim(PDMS_info1(i, 1))
   cType  =trim(PDMS_info2(i, 1))
   cType2 =trim(PDMS_info2(i, 2))
   SecType=trim(PDMS_info2(i, 3))

   nChild=twork_nChild(i)
   do j=1,nChild
      iChilds(j)=twork_iChilds(i,j)
   enddo
   do j=1,nChild
      ic=twork_iChilds(i,j)
      cTypeChild=trim(PDMS_info2(ic, 1))
      if (trim(cTypeChild)=='BRCO') then
         do j1=1,nChild
            !print *,j1, nChild
            twork_iChilds(i,j1)=iChilds(nChild+1-j1)
         enddo
         exit
      endif
   enddo

   !151222 C_TEE 처리...
   do j=1,nChild
      ic=twork_iChilds(i,j)
      cRefnoChild=trim(PDMS_info1(ic, 1))
      if (cRefnoChild(1:2)=='C=') then
         do j1=1,nChild
            twork_iChilds(i,j1)=iChilds(nChild+1-j1)
         enddo
         exit
      endif
   enddo

!   if (nChild > 1) then; 
!   do j=1,nChild
!          print *,trim(PDMS_info2(twork_iChilds(i,j), 1)),' ',trim(PDMS_info1(twork_iChilds(i,j), 3))
!   enddo
!   print *,'a'
!   endif

   
endif
enddo



! 2. 'S','B' 순서를 조정한다.
do i=1,nnode
   nChild=twork_nChild(i)

if (twork_iParent(i)/=0 .and. nChild > 1) then

   cRefno =trim(PDMS_info1(i, 1))
   cType  =trim(PDMS_info2(i, 1))
   cType2 =trim(PDMS_info2(i, 2))
   SecType=trim(PDMS_info2(i, 3))

   do j=1,nChild
      iChilds(j)=twork_iChilds(i,j)
   enddo
   
   rOrder(:)=9999999.
   do j=1,nChild
      ic=twork_iChilds(i,j)
      TfitOrder=trim(PDMS_info2(ic,42))
      
      select case (trim(TfitOrder))
      case ('B1','S1');  rOrder(j) = 1.
      case ('B2');  rOrder(j) = 2.
      case ('B3');  rOrder(j) = 3.
      case ('B4');  rOrder(j) = 4.
      case ('B5');  rOrder(j) = 5.
      case ('B6');  rOrder(j) = 6.
      case ('B7');  rOrder(j) = 7.
      case ('B8');  rOrder(j) = 8.
      case ('B9');  rOrder(j) = 9.
      case ('B10'); rOrder(j) = 10.
      case ('B11'); rOrder(j) = 11.
      case ('B12'); rOrder(j) = 12.
      case ('B13'); rOrder(j) = 13.
      case ('B14'); rOrder(j) = 14.
      case ('B15'); rOrder(j) = 15.
      case ('B16'); rOrder(j) = 16.
      case ('B17'); rOrder(j) = 17.
      case ('B18'); rOrder(j) = 18.
      case ('B19'); rOrder(j) = 19.
      case ('B20'); rOrder(j) = 20.
      case ('B');   rOrder(j) = 990.
      case ('S');   rOrder(j) = 999.
      case default; rOrder(j) = 990.
      end select
      
      rOrder(j) = rOrder(j) + 0.0001*j    ! 값이 같을 때 sort 에서 순서가 뒤집히는 경우 있음... 임시방편.
      
!      case ('B1' ,'S1' ); rOrder(j) = 1.
!      case ('B2' ,'S2' ); rOrder(j) = 2.
!      case ('B3' ,'S3' ); rOrder(j) = 3.
!      case ('B4' ,'S4' ); rOrder(j) = 4.
!      case ('B5' ,'S5' ); rOrder(j) = 5.
!      case ('B6' ,'S6' ); rOrder(j) = 6.
!      case ('B7' ,'S7' ); rOrder(j) = 7.
!      case ('B8' ,'S8' ); rOrder(j) = 8.
!      case ('B9' ,'S9' ); rOrder(j) = 9.
!      case ('B10','S10'); rOrder(j) = 10.
!      case ('B11','S11'); rOrder(j) = 11.
!      case ('B12','S12'); rOrder(j) = 12.
!      case ('B13','S13'); rOrder(j) = 13.
!      case ('B14','S14'); rOrder(j) = 14.
!      case ('B15','S15'); rOrder(j) = 15.
!      case ('B16','S16'); rOrder(j) = 16.
!      case ('B17','S17'); rOrder(j) = 17.
!      case ('B18','S18'); rOrder(j) = 18.
!      case ('B19','S19'); rOrder(j) = 19.
!      case ('B20','S20'); rOrder(j) = 20.
!      case ('B99','S99'); rOrder(j) = 99.
!      case ('B');   rOrder(j) = 990.
!      case ('S');   rOrder(j) = 999.
!      case default; rOrder(j) = 990.      
      
   enddo

   rOrderSort = rOrder
   call quick_sort(rOrderSort,iOrderSort)

   !PRINT *,rOrder(1:nChild)
   !PRINT *,iOrderSort(1:nChild)

   !print *,iOrderSort(1:nChild),'aa'
   
   !print *,twork_iChilds(i,1:nChild),'a'
   do j=1,nChild
      twork_iChilds(i,j) = iChilds(iOrderSort(j))
   enddo
   !print *,twork_iChilds(i,1:nChild),'b'

endif
enddo

end subroutine

!----------------------------------------------------------------------------------------------

subroutine mystrfind(str1, str2, yes_found)

use globy
use ISO_VARYING_STRING
use strings
use funcs
implicit none

character(len=64) :: str1, str2
character(len=64) :: cline_temp
character(len=64) :: tokens(2)
integer :: ntokens
logical :: yes_found

   yes_found = .FALSE.
   cline_temp = REPLACE(trim(str1),trim(str2),'~',.TRUE.)
   call parse(cline_temp,'~', tokens, ntokens)
   if (ntokens>=2) then
      yes_found = .TRUE.
   endif

end subroutine

!----------------------------------------------------------------------------------------------

subroutine match_spref(cSPREF_CE,cSPREF_DB,cMatch_type,yes_matchf)

use globy
use ISO_VARYING_STRING
use strings
use funcs
implicit none

character(len=64) :: cSPREF_CE,cSPREF_DB,cMatch_type
logical :: yes_matchf,yes_match0,yes_match1,yes_match2

   yes_match0 = .false.
   yes_match1 = .false.
   yes_match2 = .false.

   if (trim(cMatch_type) == '0') then
      yes_match0 = trim(cSPREF_CE) == trim(cSPREF_DB)
   elseif (trim(cMatch_type) == '1') then
      call mystrfind(cSPREF_CE,cSPREF_DB,yes_match1)
   elseif (trim(cMatch_type) == '2') then
      call mystrfind(cSPREF_CE,cSPREF_DB,yes_match2)
   else
      !stop
   endif

   yes_matchf = yes_match0 .or. yes_match1 .or.yes_match2

end subroutine

!----------------------------------------------------------------------------------------------


subroutine trim_PDMS_info2()

use globy
use ISO_VARYING_STRING
use strings
use funcs
implicit none

character(len=64) :: cType,cSPREF
logical :: yes_matchf,yes_match0,yes_match1,yes_match2,yes_found
integer :: i,j,nnode
integer :: icomp
integer :: iParent,iChilds(MAXCHILD),nChild
real,dimension(3) :: pos1,pos2,dir0,dirc1,dirc2
real :: angleC1,angleC2,rtmp,angleS(6),angleS_max,angleS1,angleS2

nnode=nnode_tree

!------------- 사이즈, 속성, 좌표 사전 체크 -------------------------


! 김태호 사원 발견 오류: 숫자에 mm와 같은 단위가 붙는 경우가 있음. PCOMP, COUPLING 등에서.
do i=1,nnode
   
   PDMS_info2(i, 4) = REPLACE(trim(PDMS_info2(i, 4)),'m','',.TRUE.)
   PDMS_info2(i, 5) = REPLACE(trim(PDMS_info2(i, 5)),'m','',.TRUE.)
   PDMS_info2(i, 6) = REPLACE(trim(PDMS_info2(i, 6)),'m','',.TRUE.)
   PDMS_info2(i, 7) = REPLACE(trim(PDMS_info2(i, 7)),'m','',.TRUE.)
   PDMS_info2(i, 8) = REPLACE(trim(PDMS_info2(i, 8)),'m','',.TRUE.)
   PDMS_info2(i, 9) = REPLACE(trim(PDMS_info2(i, 9)),'m','',.TRUE.)
   PDMS_info2(i,10) = REPLACE(trim(PDMS_info2(i,10)),'m','',.TRUE.)
   PDMS_info2(i,11) = REPLACE(trim(PDMS_info2(i,11)),'m','',.TRUE.)
   PDMS_info2(i,12) = REPLACE(trim(PDMS_info2(i,12)),'m','',.TRUE.)
   PDMS_info2(i,13) = REPLACE(trim(PDMS_info2(i,13)),'m','',.TRUE.)
   PDMS_info2(i,14) = REPLACE(trim(PDMS_info2(i,14)),'m','',.TRUE.)
   PDMS_info2(i,15) = REPLACE(trim(PDMS_info2(i,15)),'m','',.TRUE.)
   PDMS_info2(i,16) = REPLACE(trim(PDMS_info2(i,16)),'m','',.TRUE.)
   PDMS_info2(i,17) = REPLACE(trim(PDMS_info2(i,17)),'m','',.TRUE.)

enddo




! SPREF에서 Section Dimension 가져오기
!EQUI
do i=1,nnode
   cType  =trim(PDMS_info2(i, 1))
   cSPREF = trim(PDMS_info2(i,30))
   yes_found = .false.
   
   if ( trim(cType) == 'EQUI' ) then

      if (.not. yes_found) then !--s
      do icomp=1,nrow_FANEQ
         call match_spref(cSPREF,spref_FANEQ(icomp,2),spref_FANEQ(icomp,3),yes_matchf)
         if ( yes_matchf ) then
            if ( len_trim(spref_FANEQ(icomp,6)) > 0 .and. len_trim(spref_FANEQ(icomp,7)) > 0  ) then
               PDMS_info2(i, 6) = trim(spref_FANEQ(icomp,6))
               PDMS_info2(i, 7) = trim(spref_FANEQ(icomp,7))
               yes_found = .true.
            endif
            exit
         endif
      enddo
      endif ! (.not. yes_found) ---e
      
      if (.not. yes_found) then !--s
      do icomp=1,nrow_VENTU
         call match_spref(cSPREF,spref_VENTU(icomp,2),spref_VENTU(icomp,3),yes_matchf)
         if ( yes_matchf ) then
            if ( len_trim(spref_VENTU(icomp,6)) > 0 .and. len_trim(spref_VENTU(icomp,7)) > 0  ) then
               PDMS_info2(i, 6) = trim(spref_VENTU(icomp,6))
               PDMS_info2(i, 7) = trim(spref_VENTU(icomp,7))
               yes_found = .true.
            endif
            exit
         endif
      enddo      
      endif ! (.not. yes_found) ---e
      
      if (.not. yes_found) then !--s
      do icomp=1,nrow_ROOMU
         call match_spref(cSPREF,spref_ROOMU(icomp,2),spref_ROOMU(icomp,3),yes_matchf)
         if ( yes_matchf ) then
            if ( len_trim(spref_ROOMU(icomp,6)) > 0 .and. len_trim(spref_ROOMU(icomp,7)) > 0  ) then
               PDMS_info2(i, 6) = trim(spref_ROOMU(icomp,6))
               PDMS_info2(i, 7) = trim(spref_ROOMU(icomp,7))
               yes_found = .true.
            endif
            exit
         endif
      enddo      
      endif ! (.not. yes_found) ---e
      
      if (.not. yes_found) then !--s
      do icomp=1,nrow_MISCL
         call match_spref(cSPREF,spref_MISCL(icomp,2),spref_MISCL(icomp,3),yes_matchf)
         if ( yes_matchf ) then
            if ( len_trim(spref_MISCL(icomp,6)) > 0 .and. len_trim(spref_MISCL(icomp,7)) > 0  ) then
               PDMS_info2(i, 6) = trim(spref_MISCL(icomp,6))
               PDMS_info2(i, 7) = trim(spref_MISCL(icomp,7))
               yes_found = .true.
            endif
            exit
         endif
      enddo      
      endif ! (.not. yes_found) ---e
      

   elseif ( trim(cType) == 'ELBO' .or. trim(cType) == 'COUP' .or. trim(cType) == 'VALV'  .or. trim(cType) == 'FTUB') then
   
      if (.not. yes_found) then !--s
      do icomp=1,nrow_PIPED
         call match_spref(cSPREF,spref_PIPED(icomp,2),spref_PIPED(icomp,3),yes_matchf)
         if ( yes_matchf ) then
            if ( len_trim(spref_PIPED(icomp,6)) > 0 .and. len_trim(spref_PIPED(icomp,7)) > 0  ) then
               PDMS_info2(i, 6) = trim(spref_PIPED(icomp,6))
               PDMS_info2(i, 7) = trim(spref_PIPED(icomp,7))
               yes_found = .true.
            endif
            exit
         endif
      enddo      
      endif ! (.not. yes_found) ---e
      
      
   endif
enddo






! Section Dimension 정리.
!c Attr3: SecType
!c Attr4: W1, !c Attr5: H1
!c Attr6: W0, !c Attr7: H0
!c Attr8: WB, !c Attr9: HB

do i=1,nnode

   !--- W1, H1
   if (trim(PDMS_info2(i, 4)) /= '') then
      if (trim(PDMS_info2(i, 5)) == 'DIAM') then
         ! CIRC 정상
         ! NOP
      elseif (trim(PDMS_info2(i, 5)) == '0' .or. trim(PDMS_info2(i, 5)) == '') then
         ! CIRC 예외 허용
         PDMS_info2(i, 5) ='DIAM'
      else
         ! RECT 정상
         ! NOP
      endif
   else
      if (trim(PDMS_info2(i, 5)) /= '') then
         ! ETC 예외 
         PDMS_info2(i, 4) ='9999'
         PDMS_info2(i, 5) ='9999'
      else
         ! ETC 예외
         PDMS_info2(i, 4) ='9999'
         PDMS_info2(i, 5) ='9999'
      endif
   endif

   
   !--- W0, H0
   if (trim(PDMS_info2(i, 6)) /= '') then
      if (trim(PDMS_info2(i, 7)) == 'DIAM') then
         ! CIRC 정상
         PDMS_info2(i, 3) = 'CIRC'
      elseif (trim(PDMS_info2(i, 7)) == '0' .or. trim(PDMS_info2(i, 7)) == '') then
         ! CIRC 예외 허용
         PDMS_info2(i, 7) ='DIAM'
         PDMS_info2(i, 3) = 'CIRC'
      else
         ! RECT 정상
         PDMS_info2(i, 3) = 'RECT'
      endif
   else
      if (trim(PDMS_info2(i, 7)) /= '') then
         ! ETC 예외 
         PDMS_info2(i, 6) ='9999'
         PDMS_info2(i, 7) ='9999'
         PDMS_info2(i, 3) = 'ETC'
      else
         ! ETC 예외
         PDMS_info2(i, 6) ='9999'
         PDMS_info2(i, 7) ='9999'
         PDMS_info2(i, 3) = 'ETC'
      endif
   endif

   
   !--- W1, H1
   if (trim(PDMS_info2(i, 8)) /= '') then
      if (trim(PDMS_info2(i, 9)) == 'DIAM') then
         ! CIRC 정상
         ! NOP
      elseif (trim(PDMS_info2(i, 9)) == '0' .or. trim(PDMS_info2(i, 9)) == '') then
         ! CIRC 예외 허용
         PDMS_info2(i, 9) ='DIAM'
      else
         ! RECT 정상
         ! NOP
      endif
   else
      if (trim(PDMS_info2(i, 9)) /= '') then
         ! ETC 예외 
         PDMS_info2(i, 8) ='9999'
         PDMS_info2(i, 9) ='9999'
      else
         ! ETC 예외
         PDMS_info2(i, 8) ='9999'
         PDMS_info2(i, 9) ='9999'
      endif
   endif

enddo





! 연결 관계를 통해 예외 정보를 유추하기. DIMENSION, POSITION
!twork_iParent(i)/=0

do i=1,nnode
   if (twork_iParent(i) /= 0) then
      iParent = twork_iParent(i)
   else
      iParent = i
   endif

   nChild = twork_nChild(i)
   if (nChild == 0) iChilds(1) = i
   do j=1,nChild
      iChilds(j) = twork_iChilds(i,j)
   enddo 
   
enddo


end subroutine

!----------------------------------------------------------------------------------------------



