


program ens_rembo

    use ensembles 
    use coordinates
    use ncio 
    use nml 

    implicit none 

    character(len=512) :: ens_fldr, filename
    character(len=512), allocatable :: fldrs(:) 
    
    double precision, allocatable :: time_in(:), time_out(:), x(:), y(:), m(:)
    character(len=256) :: tname, tunits, xname, xunits, yname, yunits, mname, munits

    character(len=512), allocatable :: names(:), units(:)

    integer :: nsim, nt, nx, ny, nm, nvar, k 
    double precision :: t0, t1, dt 

    ens_fldr = "output"

    allocate(names(3),units(3)) 
    names = ["var1","var2","var3"]
    units = ["m","m","m"]

    tname  = "time"
    tunits = "ka BP"

    xname  = "xc"
    xunits = "km"

    yname  = "yc"
    yunits = "km" 

    mname  = "month"
    munits = "" 

    nm = 13 
    allocate(m(nm))
    do k = 1, nm 
        m(k) = dble(k)
    end do

    ! ### Test writing sico 1D data ###

    ! Define folders
    nsim = 5 
    allocate(fldrs(nsim))
    do k = 1, nsim 
        write(fldrs(k),"(a,i1)") "data/test_sico",k
    end do 

    ! Define output times 
    t0 = 1700
    t1 = 1720 
    dt = 1

    nt = (t1-t0)/dt + 1
    if (allocated(time_out)) deallocate(time_out)
    allocate(time_out(nt))
    do k = 1, nt 
        time_out(k) = t0 + dt*(k-1)
    end do 

    ! Write some fields
    filename = "sico.1d.nc"
    call ens_init(ens_fldr,filename,nsim,t=time_out,tname=tname,tunits=tunits)

    call ens_1D(ens_fldr,fldrs,filename,name="Vtot",time=time_out,tname="time",prec="float",interp="align")
    call ens_1D(ens_fldr,fldrs,filename,name="Aib", time=time_out,tname="time",prec="float")



end program ens_rembo 