


program ens_rembo

    use ensembles 
    use coordinates
    use ncio 
    use nml 

    implicit none 

    character(len=512) :: ens_fldr, filename 
    
    double precision, allocatable :: time_in(:), time_out(:), x(:), y(:), m(:)
    character(len=256) :: tname, tunits, xname, xunits, yname, yunits, mname, munits

    character(len=512), allocatable :: names(:), units(:)

    integer :: nsim, nt, nx, ny, nm
    integer :: nvar  

    integer :: k 

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

    nsim = 5 

    nt = 10 
    allocate(time_out(nt))
    do k = 1, nt 
        time_out(k) = dble(k)*2.d0
    end do 

    nx = 12 
    allocate(x(nx))
    do k = 1, nx 
        x(k) = dble(k)*2.d0
    end do 

    ny = 24 
    allocate(y(ny))
    do k = 1, ny 
        y(k) = dble(k)*2.d0
    end do

    nm = 13 
    allocate(m(nm))
    do k = 1, nm 
        m(k) = dble(k)
    end do

    ! 1D ensemble file
    filename = "sico.nc"
    call ens_init(ens_fldr,filename,nsim,t=time_out,tname=tname,tunits=tunits)

    ! 2D ensemble file 
    filename = "sico2D.nc"
    call ens_init(ens_fldr,filename,nsim,x=x,xname=xname,xunits=xunits, &
                  t=time_out,tname=tname,tunits=tunits)

    ! 2D ensemble file: rembo[month,time]
    filename = "rembo.nc"
    call ens_init(ens_fldr,filename,nsim,x=m,xname=mname,xunits=munits, &
                  t=time_out,tname=tname,tunits=tunits)





end program ens_rembo 