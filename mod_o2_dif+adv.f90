module o2_diffusion

use Globalvariables
implicit none

contains

! *********************************************************

SubRoutine Output_O2txtImg()

Use GlobalVariables
Implicit None
integer(kind=4) :: X, Y
Character*21 numtemp
real(kind=8) :: txtimg(N_Col)

write(numtemp,'(i10.1)') Time

OPEN(unit = File_txtImg, File = trim(adjustl(today))//'/o2/O2data-'          &
    //trim(adjustl(numtemp))//'.txt', status = 'unknown')
DO Y = 1, N_row
    txtimg = 0 
    DO X = 1, N_col
        txtimg(X) = O2(y,x)%oxygen
    END Do
    write(File_txtImg, *) (txtimg(x), x = 1, n_col)
END Do
Close(File_txtimg)

OPEN(unit = File_txtImg, File = trim(adjustl(today))//'/o2/O2_use_data-'          &
    //trim(adjustl(numtemp))//'.txt', status = 'unknown')
DO Y = 1, N_row
    txtimg = 0 
    DO X = 1, N_col
        txtimg(X) = O2(y,x)%oxygen_use
    END Do
    write(File_txtImg, *) (txtimg(x), x = 1, n_col)
END Do
Close(File_txtimg)

OPEN(unit = File_txtImg, File = trim(adjustl(today))//'/o2/O2_chan_data-'          &
    //trim(adjustl(numtemp))//'.txt', status = 'unknown')
DO Y = 1, N_row
    txtimg = 0 
    DO X = 1, N_col
        txtimg(X) = O2(y,x)%value_pre
    END Do
    write(File_txtImg, *) (txtimg(x), x = 1, n_col)
END Do
Close(File_txtimg)

OPEN(unit = File_txtImg, File = trim(adjustl(today))//'/o2/O2_dif-'          &
    //trim(adjustl(numtemp))//'.txt', status = 'unknown')
DO Y = 1, N_row
    txtimg = 0 
    DO X = 1, N_col
        txtimg(X) = edif(y,x)
    END Do
    write(File_txtImg, *) (txtimg(x), x = 1, n_col)
END Do
Close(File_txtimg)

End Subroutine Output_O2txtimg

! *********************************************************

subroutine O2i_setup()

use globalvariables
implicit none
integer(kind=4) :: x, y
real(kind=8) :: porosity 
logical :: O2_sat_initial
real(kind=8) :: oxup 

oxup = pal

if (time==0)  then 
    print'(A)','        +++++++++++++++++++'
    print'(A,F4.2,A)','        O2 LEVEL = ',oxup, ' PAL'
    print'(A)','        +++++++++++++++++++'
    print*
endif 

O2(:,:)%Oxygen = 0.  ! oxgen conc. 
O2(:,:)%Oxygen_pre = 0. ! 1 if connected; 0 otherwise 
O2(:,:)%Oxygen_use = 0. ! pseud-1st order constant for oxygen consumption 

O2(1,:)%Oxygen = oxup
O2(1,:)%Oxygen_pre = 1.

O2_sat_initial = .true.
! O2_sat_initial = .false.

if (O2_sat_initial) then 
    do y = 1, n_row
        do x = 1, N_Col
            if (matrix(y,x)%class==p) cycle
            O2(y,x)%Oxygen = oxup
            O2(y,x)%Oxygen_pre = 1.
            O2(y,x)%Oxygen_use = 0.
        end do
    end do
end if

end subroutine O2i_setup

! *********************************************************

subroutine O2pre_setup()

use globalvariables
implicit none
integer(kind=4) :: x, y, i, cnt
integer(kind=4) :: up(n_row,n_col), down(n_row,n_col)

up = 0
down = 0

O2%value_pre = 0

up(1,:) = 1
down(n_row,:) = 1
cnt = 100
do while (cnt > 0) 
    cnt = 0 
    do y = 2, n_row
        do x = 1, N_Col
            if (matrix(y,x)%class == p ) cycle
            if (up(y,x) == 1) cycle 
            if (up(y-1,x)/=0) up(y,x) = 1
            if (x == 1) then
                if (up(y,2)/=0) up(y,x) = 1
                if (up(y,n_col)/=0) up(y,x) = 1
            else if (x==n_col) then  
                if (up(y,1)/=0) up(y,x) = 1
                if (up(y,n_col-1)/=0) up(y,x) = 1
            else
                if (up(y,x+1)/=0) up(y,x) = 1
                if (up(y,x-1)/=0) up(y,x) = 1
            end if 
            if (up(y,x) == 1) cnt = cnt + 1
        end do

        do x = n_col, 1, -1
            if (matrix(y,x)%class == p ) cycle
            if (up(y,x) == 1) cycle 
            if (up(y-1,x)/=0) up(y,x) = 1
            if (x == 1) then
                if (up(y,2)/=0) up(y,x) = 1
                if (up(y,n_col)/=0) up(y,x) = 1
            else if (x==n_col) then  
                if (up(y,1)/=0) up(y,x) = 1
                if (up(y,n_col-1)/=0) up(y,x) = 1
            else
                if (up(y,x+1)/=0) up(y,x) = 1
                if (up(y,x-1)/=0) up(y,x) = 1
            end if 
            if (up(y,x) == 1) cnt = cnt + 1
        end do
    end do

    do y = n_row-1, 1, -1
        do x = 1, N_Col
            if (matrix(y,x)%class == p ) cycle
            if (up(y,x) == 1) cycle 
            if (up(y+1,x)/=0) up(y,x) = 1
            if (x == 1) then
                if (up(y,2)/=0) up(y,x) = 1
                if (up(y,n_col)/=0) up(y,x) = 1
            else if (x==n_col) then  
                if (up(y,1)/=0) up(y,x) = 1
                if (up(y,n_col-1)/=0) up(y,x) = 1
            else
                if (up(y,x+1)/=0) up(y,x) = 1
                if (up(y,x-1)/=0) up(y,x) = 1
            end if 
            if (up(y,x) == 1) cnt = cnt + 1
        end do

        do x = n_col, 1, -1
            if (matrix(y,x)%class == p ) cycle
            if (up(y,x) == 1) cycle 
            if (up(y+1,x)/=0) up(y,x) = 1
            if (x == 1) then
                if (up(y,2)/=0) up(y,x) = 1
                if (up(y,n_col)/=0) up(y,x) = 1
            else if (x==n_col) then  
                if (up(y,1)/=0) up(y,x) = 1
                if (up(y,n_col-1)/=0) up(y,x) = 1
            else
                if (up(y,x+1)/=0) up(y,x) = 1
                if (up(y,x-1)/=0) up(y,x) = 1
            end if 
            if (up(y,x) == 1) cnt = cnt + 1
        end do
    end do

    do y = n_row-1, 1, -1
        do x = 1, N_Col
            if (matrix(y,x)%class == p ) cycle
            if (down(y,x) == 1) cycle 
            if (down(y+1,x)/=0) down(y,x) = 1
            if (x == 1) then
                if (down(y,2)/=0) down(y,x) = 1
                if (down(y,n_col)/=0) down(y,x) = 1
            else if (x==n_col) then  
                if (down(y,1)/=0) down(y,x) = 1
                if (down(y,n_col-1)/=0) down(y,x) = 1
            else
                if (down(y,x+1)/=0) down(y,x) = 1
                if (down(y,x-1)/=0) down(y,x) = 1
            end if 
            if (down(y,x) == 1) cnt = cnt + 1
        end do

        do x = n_col, 1, -1
            if (matrix(y,x)%class == p ) cycle
            if (down(y,x) == 1) cycle 
            if (down(y+1,x)/=0) down(y,x) = 1
            if (x == 1) then
                if (down(y,2)/=0) down(y,x) = 1
                if (down(y,n_col)/=0) down(y,x) = 1
            else if (x==n_col) then  
                if (down(y,1)/=0) down(y,x) = 1
                if (down(y,n_col-1)/=0) down(y,x) = 1
            else
                if (down(y,x+1)/=0) down(y,x) = 1
                if (down(y,x-1)/=0) down(y,x) = 1
            end if 
            if (down(y,x) == 1) cnt = cnt + 1
        end do
    end do

    do y = 2, n_row
        do x = 1, N_Col
            if (matrix(y,x)%class == p ) cycle
            if (down(y,x) == 1) cycle 
            if (down(y-1,x)/=0) down(y,x) = 1
            if (x == 1) then
                if (down(y,2)/=0) down(y,x) = 1
                if (down(y,n_col)/=0) down(y,x) = 1
            else if (x==n_col) then  
                if (down(y,1)/=0) down(y,x) = 1
                if (down(y,n_col-1)/=0) down(y,x) = 1
            else
                if (down(y,x+1)/=0) down(y,x) = 1
                if (down(y,x-1)/=0) down(y,x) = 1
            end if 
            if (down(y,x) == 1) cnt = cnt + 1
        end do

        do x = n_col, 1, -1
            if (matrix(y,x)%class == p ) cycle
            if (down(y,x) == 1) cycle 
            if (down(y-1,x)/=0) down(y,x) = 1
            if (x == 1) then
                if (down(y,2)/=0) down(y,x) = 1
                if (down(y,n_col)/=0) down(y,x) = 1
            else if (x==n_col) then  
                if (down(y,1)/=0) down(y,x) = 1
                if (down(y,n_col-1)/=0) down(y,x) = 1
            else
                if (down(y,x+1)/=0) down(y,x) = 1
                if (down(y,x-1)/=0) down(y,x) = 1
            end if 
            if (down(y,x) == 1) cnt = cnt + 1
        end do
    end do

enddo 

where ((up/=0).or.(down/=0)) O2%value_pre = 1

end subroutine O2pre_setup

! *********************************************************

subroutine oxygen_profile()

!!!!
!! solving   - [c(x,t)-c(x,t-1)]/dt + <D[c(x+1,t)-c(x,t)]/dx - D[c(x,t)-c(x-1,t)]/dx>/dx - v[c(x,t)-c(x-1,t)]/dx (if v >0) - R = 0
!!  
!!  (using 1st order upwind scheme for advection, 2nd order central differenciation for diffusion term. Time is explicit, 1st order)

use GlobalVariables
implicit none

integer(kind=4) :: n, nnz
integer(kind=4), allocatable :: ai( : )  ! row number where /=0
integer(kind=4), allocatable :: ap(:)   ! number of non-zero component at each column
real(kind=8), allocatable :: ax( : )   !  component where /=0
real(kind=8), allocatable :: b( :) 
real (kind=8) :: control(20)
integer(kind=4) :: filenum
integer(kind=4) :: i
real(kind=8) :: info(90)
integer(kind=8) :: numeric
integer(kind=4) :: status
integer(kind=8) :: symbolic
integer (kind=4) :: sys
real(kind=8), allocatable  :: x(:)
integer(kind=4) :: n_row_calc       !  number of row used for the calculation
integer(kind=4) :: y_fin       !  bottom y layer
real(kind=8) :: org_dif_fact = 1.  !  porosity of particle
integer(kind=4) :: j, k, cnt, cnt2, xx, yy, cnt3, cnt4, cnt5
logical :: mtx_chk
real(kind=8) :: ox_l, ox_r, ox_u, ox_d, ox_c
real(kind=8) :: po_l, po_r, po_u, po_d, po_c, po_pre
real(kind=8) :: dif_l, dif_r, dif_u, dif_d, dif_c
real(kind=8) :: rxn
real(kind=8) :: dt, dx
integer(kind=4) :: cnt_rec(n_row,n_col)
real(kind=8), parameter :: imp = 1.0d0
real(kind=8), parameter :: epl = 0d0
real(kind=8), parameter :: cntgrad = 0.0d0
integer(kind=4) :: step, totstep
real(kind=8) :: tmpU,tmpV
real(kind=8), allocatable :: tmpO2(:,:)
real(kind=8) :: oxup 
real(kind=8) :: maxedif
logical :: divide = .false.
! logical :: divide = .true.
! logical :: only_sed
! real(kind=8), parameter :: rx_ex = 0.0  ! 1.0 if reaction is explicitly reflected
! real(kind=8), parameter :: rx_imp = 1.0 - rx_ex  ! 1.0 if reaction is implicitly reflected
real(kind=8) :: rx_ex = 0.0d0  ! 1.0 if reaction is explicitly reflected
real(kind=8) :: rx_imp = 1.0d0 !  - rx_ex  ! 1.0 if reaction is implicitly reflected
real(kind=8) ::  shear, visc
integer(kind=4) :: sedloc
real(kind=8) :: fact_law1, fact_law2 
real(kind=8) :: merge2
real(kind=8) :: do2dt
real(kind=8) :: TotOrgDecay_tmp, TotResp_tmp, TotO2Dif_tmp
real(kind=8) :: TotAbio_tmp, TotO2Adv_tmp, do2dt_tmp, resO2_tmp
logical :: chk_details = .false.
logical :: flg_stop = .false.
integer(kind=4):: col,row, xp,xg, yp,yg, ppp,ai_tmp(5),ai_sort(5)
real(kind=8) :: ax_tmp(5)
! logical :: calc_form = .true.
real(kind=8) :: form_btm, form_top, ec_top, ec_btm, ec_w
real(kind=8) :: ep_top = 200d0, ep_btm = 100d0
real(kind=8) :: sigma = 4d-2 ! ohm-1 cm-1; electrical conductivity of seawater, cf., Tyler et al. 2007
real(kind=8) :: elecp(n_row,n_col)
Character*21 numtemp
real(kind=8) :: txtimg(N_Col)
oxup = pal

! if (time==0) print*,oxup,pal

write(numtemp,'(i10.1)') Time

if (allocated(tmpo2)) deallocate(tmpo2) 
allocate(tmpo2(N_row,N_col))
tmpo2 = O2(:,:)%oxygen

! print*,maxval(tmpo2(:,:)-O2(:,:)%oxygen),minval(tmpo2(:,:)-O2(:,:)%oxygen)

! pause

select case (trim(adjustl(O2ratelaw)))
    case('linear','LINEAR','Linear')
        fact_law1 = 0d0
        fact_law2 = 1d0
    case('zero','ZERO','Zero')
        fact_law1 = 0d0
        fact_law2 = 0d0
    case('monod','MONOD','Monod')
        fact_law1 = 1d0
        fact_law2 = 0d0
end select 


y_fin = n_row   
if (only_sed) then 
    do yy = 1, n_row
        if (any(matrix(yy,:)%class==p)) exit
    end do 
    y_int = yy - 3
else 
    y_int  = 1       !  when upper water column is boudary 
end if 

y_int = 1

n_row_calc = y_fin - (y_int - 1)

if (n_row_calc /= y_fin - y_int + 1) write(*,*) "fatal error"

cnt = 0    ! count the number of water 
cnt2 = 0    ! count necessary non-zero matrix components
cnt_rec = 0
do yy = y_int  + 1, y_fin
    do xx = 1, n_col
        if (matrix(yy,xx)%class /=p) then
            cnt = cnt + 1	
            cnt2 = cnt2 + 1	  
            cnt_rec(yy,xx) = cnt	   
            if (yy == y_int+1) then 
                if (xx == 1) then 
                    if (matrix(yy,n_col)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,2)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy+1,1)%class /= p) cnt2 = cnt2 + 1
                else if (xx == n_col) then 
                    if (matrix(yy,1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,n_col-1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy+1,n_col)%class /= p) cnt2 = cnt2 + 1
                else 
                    if (matrix(yy,xx-1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,xx+1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy+1,xx)%class /= p) cnt2 = cnt2 + 1
                end if 
            else if (yy == y_fin) then 
                if (xx == 1) then 
                    if (matrix(yy,n_col)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,2)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy-1,1)%class /= p) cnt2 = cnt2 + 1
                else if (xx == n_col) then 
                    if (matrix(yy,1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,n_col-1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy-1,n_col)%class /= p) cnt2 = cnt2 + 1
                else 
                    if (matrix(yy,xx-1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,xx+1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy-1,xx)%class /= p) cnt2 = cnt2 + 1
                end if 
            else 
                if (xx == 1) then 
                    if (matrix(yy,n_col)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,2)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy+1,1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy-1,1)%class /= p) cnt2 = cnt2 + 1
                else if (xx == n_col) then 
                    if (matrix(yy,1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,n_col-1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy+1,n_col)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy-1,n_col)%class /= p) cnt2 = cnt2 + 1
                else 
                    if (matrix(yy,xx-1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy,xx+1)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy+1,xx)%class /= p) cnt2 = cnt2 + 1
                    if (matrix(yy-1,xx)%class /= p) cnt2 = cnt2 + 1
                end if 
            end if
        end if
    end do 
end do


edif = dif_0
! shear = 3.171e-3*60.*60.*24.*365. ! cm/yr (compare 1e-5 m/s; Volkenborn et al. 2012)
shear = 3.171d-2*60d0*60d0*24d0*365d0 ! cm/yr (compare 1e-5 m/s; Volkenborn et al. 2012)
shear = shear*shearfact 
visc =  4.79d5   ! kinematic viscosity in cm^2/yr

!! lateral change in diffusitivity allowed 

do xx = 1, n_col  
    sedloc = 1
    do yy = 1, n_row
        if (matrix(yy,xx)%class == p) then 
            sedloc = yy
            exit 
        end if 
    end do 

    do yy = sedloc, 1, -1
        edif(yy,xx) = edif(yy,xx) + 0.4d0*visc/363.d0*((sedloc - yy)*pixelsize*shear/visc)**3
    end do 
end do

maxedif = maxval(edif)	

n = cnt
nnz = cnt2

Allocate (ai(nnz),ap(n+1),ax(nnz),b(n),x(n))

totstep = 1

if (divide) then 
    do i = 1,N_ind
        totstep = max(totstep, 5*ceiling(sqrt(maxval(Vb(:,i))**2+maxval(Vb(:,i))**2)/org(i)%width))
        print *, totstep, org(i)%width, ceiling(sqrt(maxval(Vb(:,i))**2+maxval(Vb(:,i))**2))
    end do
end if

if (.not. calc_form) then 

do step = 1, totstep

ai = 0
ap = 0
ax = 0.
b = 0.

dt = TImescale/365./real(totstep)  ! yr
dx = PixelSIze  ! cm

ap(1) = 0
do yy = y_int  + 1, y_fin

    do xx = 1, n_col

        if (matrix(yy,xx)%class == p) cycle
        
        ai_tmp = 0
        ax_tmp = 0d0
        
        row = cnt_rec(yy,xx)
        cnt2 = 1
        ai_tmp( 1 ) = row 
        ax_tmp( 1 ) = ax_tmp( 1 ) - o2(yy,xx)%oxygen_use
        ax_tmp( 1 ) = ax_tmp( 1 ) - (1d0-0d0)/dt   
        b(row)      = b(row)       - (0d0-tmpo2(yy,xx))/dt  

        if (yy == y_int  + 1) then
        
            ax_tmp( 1 ) = ax_tmp( 1 ) + (-0.5d0*(edif(yy,xx)+edif(yy-1,xx))*(1.0d0  - 0d0)/dx)/dx 
            b(row)      = b(row)      + (-0.5d0*(edif(yy,xx)+edif(yy-1,xx))*(0d0 - oxup)/dx)/dx 
                
            ax_tmp( 1 ) = ax_tmp( 1 ) - ((Vom(xx,yy)+abs(Vom(xx,yy)))*0.5d0*(1d0-0d0) )/dx
            b(row)      = b(row)       - ((Vom(xx,yy)+abs(Vom(xx,yy)))*0.5d0*(0d0-oxup) )/dx 
            
            xp = xx + 1
            xg = xx - 1
            yp = yy + 1
            
            if (xp > n_col) xp = 1
            if (xg < 1) xg = n_col
                
            if (matrix(yy,xp)%class /=p) then 
                col = cnt_rec(yy,xp)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (0.5d0*(edif(yy,xx)+edif(yy,xp))*(0d0 - 1d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (-0.5d0*(edif(yy,xp)+edif(yy,xx))*(0d0 - 1d0)/dx)/dx 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Uom(xx+1,yy)-abs(Uom(xx+1,yy)))*0.5d0*(0d0-1d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Uom(xp,yy)+abs(Uom(xp,yy)))*0.5d0*(0d0-1d0) )/dx 
            endif 
                
            if (matrix(yy,xg)%class /=p) then 
                col = cnt_rec(yy,xg)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (-0.5d0*(edif(yy,xx)+edif(yy,xg))*(1d0 - 0d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (0.5d0*(edif(yy,xx)+edif(yy,xg))*(1d0 - 0d0)/dx)/dx  
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Uom(xx,yy)+abs(Uom(xx,yy)))*0.5d0*(1d0-0d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Uom(xg+1,yy)-abs(Uom(xg+1,yy)))*0.5d0*(1d0-0d0) )/dx 
            endif 
                
            if (matrix(yp,xx)%class /=p) then 
                col = cnt_rec(yp,xx)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (0.5d0*(edif(yy,xx)+edif(yp,xx))*(0d0 - 1d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (-0.5d0*(edif(yy,xx)+edif(yp,xx))*(0d0 - 1d0)/dx)/dx  
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Vom(xx,yy+1)-abs(Vom(xx,yy+1)))*0.5d0*(0d0-1d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Vom(xx,yp)+abs(Vom(xx,yp)))*0.5d0*(0d0-1d0) )/dx 
            endif 

        ELSE IF (yy == y_fin) THEN
        
            xp = xx + 1
            xg = xx - 1
            yg = yy - 1
            
            if (xp > n_col) xp = 1
            if (xg < 1) xg = n_col
                
            if (matrix(yy,xp)%class /=p) then 
                col = cnt_rec(yy,xp)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (0.5d0*(edif(yy,xx)+edif(yy,xp))*(0d0 - 1d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (-0.5d0*(edif(yy,xp)+edif(yy,xx))*(0d0 - 1d0)/dx)/dx 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Uom(xx+1,yy)-abs(Uom(xx+1,yy)))*0.5d0*(0d0-1d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Uom(xp,yy)+abs(Uom(xp,yy)))*0.5d0*(0d0-1d0) )/dx 
            endif 
                
            if (matrix(yy,xg)%class /=p) then 
                col = cnt_rec(yy,xg)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (-0.5d0*(edif(yy,xx)+edif(yy,xg))*(1d0 - 0d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (0.5d0*(edif(yy,xx)+edif(yy,xg))*(1d0 - 0d0)/dx)/dx  
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Uom(xx,yy)+abs(Uom(xx,yy)))*0.5d0*(1d0-0d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Uom(xg+1,yy)-abs(Uom(xg+1,yy)))*0.5d0*(1d0-0d0) )/dx 
            endif 
                
            if (matrix(yg,xx)%class /=p) then 
                col = cnt_rec(yg,xx)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (-0.5d0*(edif(yy,xx)+edif(yg,xx))*(1d0 - 0d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (0.5d0*(edif(yy,xx)+edif(yg,xx))*(1d0 - 0d0)/dx)/dx 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Vom(xx,yy)+abs(Vom(xx,yy)))*0.5d0*(1d0-0d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Vom(xx,yg+1)-abs(Vom(xx,yg+1)))*0.5d0*(1d0-0d0) )/dx 
            endif 

        ELSE 
        
            xp = xx + 1
            xg = xx - 1
            yp = yy + 1
            yg = yy - 1
            
            if (xp > n_col) xp = 1
            if (xg < 1) xg = n_col
                
            if (matrix(yy,xp)%class /=p) then 
                col = cnt_rec(yy,xp)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (0.5d0*(edif(yy,xx)+edif(yy,xp))*(0d0 - 1d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (-0.5d0*(edif(yy,xp)+edif(yy,xx))*(0d0 - 1d0)/dx)/dx
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Uom(xx+1,yy)-abs(Uom(xx+1,yy)))*0.5d0*(0d0-1d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Uom(xp,yy)+abs(Uom(xp,yy)))*0.5d0*(0d0-1d0) )/dx 
            endif 
                
            if (matrix(yy,xg)%class /=p) then 
                col = cnt_rec(yy,xg)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (-0.5d0*(edif(yy,xx)+edif(yy,xg))*(1d0 - 0d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (0.5d0*(edif(yy,xx)+edif(yy,xg))*(1d0 - 0d0)/dx)/dx  
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Uom(xx,yy)+abs(Uom(xx,yy)))*0.5d0*(1d0-0d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Uom(xg+1,yy)-abs(Uom(xg+1,yy)))*0.5d0*(1d0-0d0) )/dx 
            endif 
                
            if (matrix(yg,xx)%class /=p) then 
                col = cnt_rec(yg,xx)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (-0.5d0*(edif(yy,xx)+edif(yg,xx))*(1d0 - 0d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (0.5d0*(edif(yy,xx)+edif(yg,xx))*(1d0 - 0d0)/dx)/dx   
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Vom(xx,yy)+abs(Vom(xx,yy)))*0.5d0*(1d0-0d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Vom(xx,yg+1)-abs(Vom(xx,yg+1)))*0.5d0*(1d0-0d0) )/dx 
            endif 
                
            if (matrix(yp,xx)%class /=p) then 
                col = cnt_rec(yp,xx)
                cnt2 = cnt2+ 1
                ai_tmp(cnt2) = col 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) + (0.5d0*(edif(yy,xx)+edif(yp,xx))*(0d0 - 1d0)/dx)/dx  
                ax_tmp(cnt2) = ax_tmp(cnt2) + (-0.5d0*(edif(yy,xx)+edif(yp,xx))*(0d0 - 1d0)/dx)/dx 
                
                ax_tmp( 1  ) = ax_tmp( 1  ) - ((Vom(xx,yy+1)-abs(Vom(xx,yy+1)))*0.5d0*(0d0-1d0) )/dx   
                ax_tmp(cnt2) = ax_tmp(cnt2) - ((Vom(xx,yp)+abs(Vom(xx,yp)))*0.5d0*(0d0-1d0) )/dx 
            endif 

        END IF
        
        ap(row+1) = ap(row) + cnt2 
        ai_sort = 0
        call heapsort2(cnt2,ai_tmp(1:cnt2),ai_sort(1:cnt2))
        if (cnt2==1) ai_sort = 1
		do ppp = 1, cnt2
			ai(ap(row)+ppp) = ai_tmp(ppp) - 1  !! matrix index must start with 0 in UMFPACK
			ax(ap(row)+ppp) = ax_tmp(ai_sort(ppp))
		end do 
        
    end do 

end do

b = - b

! b = b/maxedif/oxup
! ax = ax/maxedif/oxup


mtx_chk = .FALSE.
! mtx_chk = .TRUE.

if (mtx_chk) then
    open(500, file="chck_ap.txt", status = 'unknown')
    do j = 1, n+1
        write(500,*) ap(j)
    end do 
    close(500)
    open(500, file="chck_ai.txt", status = 'unknown')
    do j = 1, nnz
        write(500,*) ai(j)
    end do 
    close(500)
    if (any(isnan(ax))) then 
        open(500, file="chck_ax.txt", status = 'unknown')
        do j = 1, nnz
            write(500,*) ax(j)
        end do 
        close(500)
    endif 
    if (any(isnan(b))) then 
        open(500, file="chck_b.txt", status = 'unknown')
        do j = 1, n
            write(500,*) b(j)
        end do 
        close(500)
    endif 
end if

if (mtx_chk) stop

!
!  Set the default control parameters.
!
call umf4def( control )
!
!  From the matrix data, create the symbolic factorization information.
!
call umf4sym ( n, n, ap, ai, ax, symbolic, control, info )

if ( info(1) < 0.0D+00 ) then
    write ( *, * ) ''
    write ( *, *) 'UMFPACK_SIMPLE - Fatal error!'
    write ( *, * ) '  UMF4SYM returns INFO(1) = ', info(1)
    stop 1
end if
!
!  From the symbolic factorization information, carry out the numeric factorization.
!
call umf4num ( ap, ai, ax, symbolic, numeric, control, info )

if ( info(1) < 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'UMFPACK_SIMPLE - Fatal error!'
    write ( *, '(a,g14.6)' ) '  UMF4NUM returns INFO(1) = ', info(1)
    stop 1
end if
!
!  Free the memory associated with the symbolic factorization.
!
call umf4fsym ( symbolic )
!
!  Solve the linear system.
!
sys = 0
call umf4sol ( sys, x, b, numeric, control, info )

if ( info(1) < 0.0D+00 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'UMFPACK_SIMPLE - Fatal error!'
    write ( *, '(a,g14.6)' ) '  UMF4SOL returns INFO(1) = ', info(1)
    stop 1
end if
!
!  Free the memory associated with the numeric factorization.
!
call umf4fnum ( numeric )
!
!  Print the solution.
!

print '(A22,I0)','O2 time-step: ',Time

call O2pre_setup()

do yy = y_int  + 1, y_fin
    do xx = 1, n_col
        if (matrix(yy,xx)%class == p ) cycle

        j = cnt_rec(yy,xx)
        O2(yy,xx)%oxygen = x(j)

        if (O2(yy,xx)%oxygen < 0) then 
            if (O2(yy,xx)%value_pre /=1) then 
                O2(yy,xx)%oxygen = 0.0
            else 
                if (chk_details) then 
                    print *, "================="
                    print *, "=negative oxygen=" , O2(yy,xx)%oxygen, xx,yy, O2(yy,xx)%value_pre
                    print *, "================="
                endif 
                O2(yy,xx)%oxygen = 0.0
                flg_stop = .true. 
                ! stop
            end if
        end if 

        if (O2(yy,xx)%oxygen > 1.) then
            if (chk_details) then 
                print *, "%%%%%%%%%%%%%%%%%%%%"
                print *, "=oxygen production?=" , O2(yy,xx)%oxygen, xx,yy, O2(yy,xx)%value_pre
                print *, "%%%%%%%%%%%%%%%%%%%%"
            endif 
        end if 

        if (isnan(O2(yy,xx)%oxygen)) then
            if (chk_details) then
                print *,"//////////////////"
                print *,"//  NAN at",yy,xx,"///"
                print *,"//////////////////"
            endif
            if (matrix(yy,xx)%class == p) O2(yy,xx)%oxygen = 0.0

            ! stop
            flg_stop = .true.

        end if
        
    end do
end do 

if (flg_stop) then 
    print *, 'ERROR in o2 calc (NAN or NEGATIVE)'
    stop
endif 

end do

! calculating fluxes

TotOrgDecay =0d0 
TotResp = 0d0 
TotO2Dif = 0d0
TotAbio = 0d0
TotO2Adv = 0d0
do2dt = 0d0
resO2 = 0d0

do yy = y_int  + 1, y_fin

    do xx = 1, n_col

        if (matrix(yy,xx)%class == p) cycle
        
        do2dt      = do2dt       - (o2(yy,xx)%oxygen-tmpo2(yy,xx))/dt  
        totorgdecay = totorgdecay - o2(yy,xx)%oxygen_use*o2(yy,xx)%oxygen

        if (yy == y_int  + 1) then
            
            toto2dif = toto2dif + (-0.5d0*(edif(yy,xx)+edif(yy-1,xx))*(o2(yy,xx)%oxygen  - oxup)/dx)/dx 
            
            toto2adv = toto2adv - ((Vom(xx,yy)+abs(Vom(xx,yy)))*0.5d0*(o2(yy,xx)%oxygen  - oxup) )/dx
            
            
            xp = xx + 1
            xg = xx - 1
            yp = yy + 1
            
            if (xp > n_col) xp = 1
            if (xg < 1) xg = n_col
                
            if (matrix(yy,xp)%class /=p) then 
            
                toto2dif = toto2dif + (0.5d0*(edif(yy,xx)+edif(yy,xp))*(o2(yy,xp)%oxygen - o2(yy,xx)%oxygen)/dx)/dx
                
                toto2adv = toto2adv - ((Uom(xx+1,yy)-abs(Uom(xx+1,yy)))*0.5d0*(o2(yy,xp)%oxygen - o2(yy,xx)%oxygen) )/dx 
            endif 
                
            if (matrix(yy,xg)%class /=p) then 
            
                toto2dif = toto2dif + (-0.5d0*(edif(yy,xx)+edif(yy,xg))*(o2(yy,xx)%oxygen - o2(yy,xg)%oxygen)/dx)/dx  
                
                toto2adv = toto2adv - ((Uom(xx,yy)+abs(Uom(xx,yy)))*0.5d0*(o2(yy,xx)%oxygen-o2(yy,xg)%oxygen) )/dx  
            endif 
                
            if (matrix(yp,xx)%class /=p) then 
            
                toto2dif = toto2dif + (0.5d0*(edif(yy,xx)+edif(yp,xx))*(o2(yp,xx)%oxygen - o2(yy,xx)%oxygen)/dx)/dx  
                
                toto2adv = toto2adv - ((Vom(xx,yy+1)-abs(Vom(xx,yy+1)))*0.5d0*(o2(yp,xx)%oxygen-o2(yy,xx)%oxygen) )/dx 
            endif 

        ELSE IF (yy == y_fin) THEN
        
            xp = xx + 1
            xg = xx - 1
            yg = yy - 1
            
            if (xp > n_col) xp = 1
            if (xg < 1) xg = n_col
                
            if (matrix(yy,xp)%class /=p) then 
            
                toto2dif = toto2dif + (0.5d0*(edif(yy,xx)+edif(yy,xp))*(o2(yy,xp)%oxygen - o2(yy,xx)%oxygen)/dx)/dx
                
                toto2adv = toto2adv - ((Uom(xx+1,yy)-abs(Uom(xx+1,yy)))*0.5d0*(o2(yy,xp)%oxygen - o2(yy,xx)%oxygen) )/dx 
            endif 
                
            if (matrix(yy,xg)%class /=p) then 
            
                toto2dif = toto2dif + (-0.5d0*(edif(yy,xx)+edif(yy,xg))*(o2(yy,xx)%oxygen - o2(yy,xg)%oxygen)/dx)/dx  
                
                toto2adv = toto2adv - ((Uom(xx,yy)+abs(Uom(xx,yy)))*0.5d0*(o2(yy,xx)%oxygen-o2(yy,xg)%oxygen) )/dx  
            endif 
                
            if (matrix(yg,xx)%class /=p) then 
            
                toto2dif = toto2dif + (-0.5d0*(edif(yy,xx)+edif(yg,xx))*(o2(yy,xx)%oxygen - o2(yg,xx)%oxygen)/dx)/dx 
                    
                toto2adv = toto2adv - ((Vom(xx,yy)+abs(Vom(xx,yy)))*0.5d0*(o2(yy,xx)%oxygen-o2(yg,xx)%oxygen) )/dx  
            endif 

        ELSE 
        
            xp = xx + 1
            xg = xx - 1
            yp = yy + 1
            yg = yy - 1
            
            if (xp > n_col) xp = 1
            if (xg < 1) xg = n_col
                
            if (matrix(yy,xp)%class /=p) then 
            
                toto2dif = toto2dif + (0.5d0*(edif(yy,xx)+edif(yy,xp))*(o2(yy,xp)%oxygen - o2(yy,xx)%oxygen)/dx)/dx
                
                toto2adv = toto2adv - ((Uom(xx+1,yy)-abs(Uom(xx+1,yy)))*0.5d0*(o2(yy,xp)%oxygen - o2(yy,xx)%oxygen) )/dx 
            endif 
                
            if (matrix(yy,xg)%class /=p) then 
            
                toto2dif = toto2dif + (-0.5d0*(edif(yy,xx)+edif(yy,xg))*(o2(yy,xx)%oxygen - o2(yy,xg)%oxygen)/dx)/dx  
                
                toto2adv = toto2adv - ((Uom(xx,yy)+abs(Uom(xx,yy)))*0.5d0*(o2(yy,xx)%oxygen-o2(yy,xg)%oxygen) )/dx  
            endif 
                
            if (matrix(yg,xx)%class /=p) then 
            
                toto2dif = toto2dif + (-0.5d0*(edif(yy,xx)+edif(yg,xx))*(o2(yy,xx)%oxygen - o2(yg,xx)%oxygen)/dx)/dx 
                    
                toto2adv = toto2adv - ((Vom(xx,yy)+abs(Vom(xx,yy)))*0.5d0*(o2(yy,xx)%oxygen-o2(yg,xx)%oxygen) )/dx  
            endif 
                
            if (matrix(yp,xx)%class /=p) then 
            
                toto2dif = toto2dif + 0.5d0*(edif(yy,xx)+edif(yp,xx))*(o2(yp,xx)%oxygen - o2(yy,xx)%oxygen)/dx/dx 
                
                toto2adv = toto2adv - ((Vom(xx,yy+1)-abs(Vom(xx,yy+1)))*0.5d0*(o2(yp,xx)%oxygen-o2(yy,xx)%oxygen) )/dx 
            endif 

        END IF
    end do 

end do


if (resp_ON) then 
    do i = 1, N_ind
        do k = 1, Org(i)%headSize
            if (matrix(Org_loc(k,i)%Y,Org_loc(k,i)%X)%class/=i) print *, "error in calc. flux"
            TotResp = TotResp +  &   
                O2(Org_loc(k,i)%Y,Org_loc(k,i)%X)%oxygen_use*merge2(1.,O2(Org_loc(k,i)%Y,Org_loc(k,i)%X)%oxygen   &
                ,(fact_law1 == 0d0 .and. fact_law2 == 0d0).or.   &
                (fact_law1==1d0 .and. O2(Org_loc(k,i)%Y,Org_loc(k,i)%X)%oxygen > mo2/iox))  
        end do 
    enddo 
endif 

Totresp = -Totresp

TotO2dif = TotO2dif*iox*1e-3*width_3d*(pixelSize)*(PixelSize)/(n_col*pixelsize*width_3d)
TotO2adv = TotO2adv*iox*1e-3*width_3d*(pixelSize)*(PixelSize)/(n_col*pixelsize*width_3d)
TotOrgdecay = Totorgdecay*iox*1e-3*width_3d*(pixelSize)*(PixelSize)/(n_col*pixelsize*width_3d)
Totresp = Totresp*iox*1e-3*width_3d*(pixelSize)*(PixelSize)/(n_col*pixelsize*width_3d)
do2dt = do2dt*iox*1e-3*width_3d*(pixelSize)*(PixelSize)/(n_col*pixelsize*width_3d)
totAbio = -(abs(totOrgDecay) - abs(Totresp) )
reso2 = do2dt + toto2dif + toto2adv + totorgdecay 

! stop  !!!

write(File_flux,*) Time*Timescale, TotO2dif,TotO2Adv, TotAbio, TotResp, TotOrgDecay, do2dt, reso2
print '(A19,7A11)','','DIFFUSION','ADVECTION','DECAY','RESP','ORGTOT','dO2/dT','RESIDUAL'
print '(A19,7E11.3)', 'O2 fluxes: ', TotO2dif,TotO2Adv,TotAbio, TotResp, TotOrgDecay, do2dt, reso2     
print '(A23)','[mol cm-2 yr-1]'
print *

if (chk_details) then 
    if (abs(reso2) > minval(abs((/TotO2dif,TotO2Adv,TotAbio, TotResp, TotOrgDecay, do2dt/)))) then 
        print *, 'too large error in flx: '
        stop
    endif 

    ! if (abs(reso2)/minval(abs((/TotO2dif,TotO2Adv,TotAbio, TotResp, TotOrgDecay, do2dt/)))>1e-2  &
        ! .and. maxval(abs((/TotO2dif,TotO2Adv,TotAbio, TotResp, TotOrgDecay, do2dt,reso2/)))>1e-14   &
        ! ) then 
    if (abs(TotO2Adv) > 1d-12) then 
        if (abs(reso2)/abs(TotO2Adv) >1d-2 &
            .and. maxval(abs((/TotO2dif,TotO2Adv,TotAbio, TotResp, TotOrgDecay, do2dt,reso2/)))>1e-12   &
            ) then 
            print *, 'too large error in flx wrt adv: ',abs(reso2)/abs(TotO2Adv)
            pause
        endif 
    endif 
    if (abs(TotO2dif) > 1d-12) then 
        if (abs(reso2)/abs(TotO2dif) >1d-2 &
            .and. maxval(abs((/TotO2dif,TotO2Adv,TotAbio, TotResp, TotOrgDecay, do2dt,reso2/)))>1e-12   &
            ) then 
            print *, 'too large error in flx wrt dif: ',abs(reso2)/abs(TotO2dif)
            pause
        endif 
    endif 
endif 

endif 

if (calc_form) then 
    form_top  = 0d0
    form_btm  = 0d0
    
    edif = sigma

    ai = 0
    ap = 0
    ax = 0.
    b = 0.

    dx = PixelSIze  ! cm

    ap(1) = 0
    do yy = y_int  + 1, y_fin

        do xx = 1, n_col

            if (matrix(yy,xx)%class == p) cycle
            
            ai_tmp = 0
            ax_tmp = 0d0
            
            row = cnt_rec(yy,xx)
            cnt2 = 1
            ai_tmp( 1 ) = row 

            if (yy == y_int  + 1) then
            
                ax_tmp( 1 ) = ax_tmp( 1 ) - edif(yy,xx)*(1.0d0  - 0d0)/dx/dx 
                b(row)      = b(row)       - edif(yy,xx)*(0d0 - ep_top)/dx/dx 
                
                xp = xx + 1
                xg = xx - 1
                yp = yy + 1
                
                if (xp > n_col) xp = 1
                if (xg < 1) xg = n_col
                    
                if (matrix(yy,xp)%class /=p) then 
                    col = cnt_rec(yy,xp)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx  
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yy,xp)*(1d0 - 0d0)/dx/dx  
                endif 
                    
                if (matrix(yy,xg)%class /=p) then 
                    col = cnt_rec(yy,xg)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx  
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yy,xg)*(1d0 - 0d0)/dx/dx  
                endif 
                    
                if (matrix(yp,xx)%class /=p) then 
                    col = cnt_rec(yp,xx)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx  
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yp,xx)*(1d0 - 0d0)/dx/dx  
                endif 

            ELSE IF (yy == y_fin) THEN
            
                ax_tmp( 1 ) = ax_tmp( 1 ) - edif(yy,xx)*(1.0d0  - 0d0)/dx/dx 
                b(row)      = b(row)       - edif(yy,xx)*(0d0 - ep_btm)/dx/dx 
            
                xp = xx + 1
                xg = xx - 1
                yg = yy - 1
                
                if (xp > n_col) xp = 1
                if (xg < 1) xg = n_col
                    
                if (matrix(yy,xp)%class /=p) then 
                    col = cnt_rec(yy,xp)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx  
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yy,xp)*(1d0 - 0d0)/dx/dx  
                endif 
                    
                if (matrix(yy,xg)%class /=p) then 
                    col = cnt_rec(yy,xg)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx 
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yy,xg)*(1d0 - 0d0)/dx/dx  
                endif 
                    
                if (matrix(yg,xx)%class /=p) then 
                    col = cnt_rec(yg,xx)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx  
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yg,xx)*(1d0 - 0d0)/dx/dx  
                endif 

            ELSE 
            
                xp = xx + 1
                xg = xx - 1
                yp = yy + 1
                yg = yy - 1
                
                if (xp > n_col) xp = 1
                if (xg < 1) xg = n_col
                    
                if (matrix(yy,xp)%class /=p) then 
                    col = cnt_rec(yy,xp)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx  
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yy,xp)*(1d0 - 0d0)/dx/dx  
                endif 
                    
                if (matrix(yy,xg)%class /=p) then 
                    col = cnt_rec(yy,xg)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx 
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yy,xg)*(1d0 - 0d0)/dx/dx  
                endif 
                    
                if (matrix(yg,xx)%class /=p) then 
                    col = cnt_rec(yg,xx)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx  
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yg,xx)*(1d0 - 0d0)/dx/dx  
                endif 
                    
                if (matrix(yp,xx)%class /=p) then 
                    col = cnt_rec(yp,xx)
                    cnt2 = cnt2+ 1
                    ai_tmp(cnt2) = col 
                    
                    ax_tmp( 1  ) = ax_tmp( 1  ) + edif(yy,xx)*(0d0 - 1d0)/dx/dx  
                    ax_tmp(cnt2) = ax_tmp(cnt2) + edif(yp,xx)*(1d0 - 0d0)/dx/dx  
                endif 

            END IF
            
            ap(row+1) = ap(row) + cnt2 
            ai_sort = 0
            call heapsort2(cnt2,ai_tmp(1:cnt2),ai_sort(1:cnt2))
            if (cnt2==1) ai_sort = 1
            do ppp = 1, cnt2
                ai(ap(row)+ppp) = ai_tmp(ppp) - 1  !! matrix index must start with 0 in UMFPACK
                ax(ap(row)+ppp) = ax_tmp(ai_sort(ppp))
            end do 
            
        end do 

    end do

    b = - b
    
    !
    !  Set the default control parameters.
    !
    call umf4def( control )
    !
    !  From the matrix data, create the symbolic factorization information.
    !
    call umf4sym ( n, n, ap, ai, ax, symbolic, control, info )

    if ( info(1) < 0.0D+00 ) then
        write ( *, * ) ''
        write ( *, *) 'UMFPACK_SIMPLE - Fatal error!'
        write ( *, * ) '  UMF4SYM returns INFO(1) = ', info(1)
        stop 1
    end if
    !
    !  From the symbolic factorization information, carry out the numeric factorization.
    !
    call umf4num ( ap, ai, ax, symbolic, numeric, control, info )

    if ( info(1) < 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'UMFPACK_SIMPLE - Fatal error!'
        write ( *, '(a,g14.6)' ) '  UMF4NUM returns INFO(1) = ', info(1)
        stop 1
    end if
    !
    !  Free the memory associated with the symbolic factorization.
    !
    call umf4fsym ( symbolic )
    !
    !  Solve the linear system.
    !
    sys = 0
    call umf4sol ( sys, x, b, numeric, control, info )

    if ( info(1) < 0.0D+00 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'UMFPACK_SIMPLE - Fatal error!'
        write ( *, '(a,g14.6)' ) '  UMF4SOL returns INFO(1) = ', info(1)
        stop 1
    end if
    !
    !  Free the memory associated with the numeric factorization.
    !
    call umf4fnum ( numeric )

    elecp = 0d0
    elecp(1:y_int,:) = ep_top
    do yy = y_int  + 1, y_fin
        do xx = 1, n_col
            if (matrix(yy,xx)%class == p ) cycle

            j = cnt_rec(yy,xx)
            elecp(yy,xx) = x(j)
        enddo 
    enddo
    ec_top = 0d0 
    ec_btm = 0d0
    ec_w   = 0d0
    do xx = 1, n_col
        ec_w = ec_w - sigma*(ep_btm-ep_top)/(n_row*dx)
        yy = y_int  + 1
        if (matrix(yy,xx)%class /= p) ec_top = ec_top - sigma*(elecp(yy,xx)-ep_top)/dx
        yy = y_fin
        if (matrix(yy,xx)%class /= p) ec_btm = ec_btm - sigma*(ep_btm-elecp(yy,xx))/dx
    enddo 
    
    ! form_top = -sigma*(ep_btm - ep_top)/(n_row*dx)/(ec_top/(n_col))
    ! form_btm = -sigma*(ep_btm - ep_top)/(n_row*dx)/(ec_btm/(n_col))
    form_top = ec_w/ec_top
    form_btm = ec_w/ec_btm
    
    ! print *, -sigma*(ep_btm - ep_top)/(n_row*dx), (ec_btm/(n_col))
    
    print '(A)','        *****************************'
    print '(A15,2A11)','','UP','DOWN'
    print '(A8,A7,2E11.3)', '','F [-]: ',form_top,form_btm
    print '(A)','        *****************************'
    print *
    
    open(unit = File_txtImg, File = trim(adjustl(today))//'/geo/ep'          &
        //trim(adjustl(numtemp))//'.txt', status = 'unknown')
    do yy = 1, N_row
        txtimg = 0 
        DO xx = 1, N_col
            txtimg(xx) = elecp(yy,xx)
        END Do
        write(File_txtImg, *) (txtimg(xx), xx = 1, n_col)
    END Do
    Close(File_txtimg)
    
    write(File_Form,*) Time*Timescale, form_top,form_btm
    
endif 

end subroutine oxygen_profile

! *********************************************************

! ***************************

function merge2(a,b,n)

implicit none

logical :: n 
real(kind=8) :: a,b,merge2

if (n) then
merge2 = a
else 
merge2 = b
endif

end function merge2

! *************************
! ====================================================
subroutine heapsort2(n,array,turn)
!!!  from http://slpr.sakura.ne.jp/qp/sortf90/
implicit none
integer(kind=4),intent(in)::n
integer(kind=4),intent(out)::turn(1:n)
integer(kind=4),intent(inout)::array(1:n)

integer(kind=4)::i,k,j,l,m
integer(kind=4)::t

if(n.le.0)then
    write(6,*)"Error, at heapsort"; stop
endif
if(n.eq.1)return

do i=1,N
    turn(i)=i
enddo

l=n/2+1
k=n
do while(k.ne.1)
    if(l.gt.1)then
        l=l-1
        t=array(l)
        m=turn(l)
    else
        t=array(k)
        m=turn(k)
        array(k)=array(1)
        turn(k)=turn(1)
        k=k-1
        if(k.eq.1) then
            array(1)=t
            turn(1)=m
            exit
        endif
    endif
    i=l
    j=l+l
    do while(j.le.k)
        if(j.lt.k)then
            if(array(j).lt.array(j+1))j=j+1
        endif
        if (t.lt.array(j))then
            array(i)=array(j)
            turn(i)=turn(j)
            i=j
            j=j+j
        else
            j=k+1
        endif
    enddo
    array(i)=t
    turn(i)=m
enddo

return
end subroutine heapsort2
! =====================================================

end module
