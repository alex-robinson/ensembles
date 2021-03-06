


program ens_rembo

    use ensembles 
    use coordinates
    use ncio 
    use nml 

    implicit none 

    character(len=512) :: ens_fldr, filename
    character(len=512), allocatable :: fldrs(:) 
    
    double precision, allocatable :: time_out(:)
    character(len=256) :: tname, tunits

    integer :: nsim, nt, k
    double precision :: t0, t1, dt

    ens_fldr = "output"

    tname  = "time"
    tunits = "ka BP"

    ! ### Test writing sico 1D data ###

    ! Define folders
    nsim = 5 
    allocate(fldrs(nsim))
    do k = 1, nsim 
        write(fldrs(k),"(a,i1)") "data/test_data",k
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

    ! Test parameter writing 
    filename = "options_rembo"
    call ens_write_par(ens_fldr,fldrs,filename,fmt="options", &
                       names=["dT_factor","itm_c    ","ppfac    "])
    
    ! Test 1D writing
    !filename = "sico.1d.nc"
    !call ens_init(ens_fldr,fldrs,filename,names=["time"],t=time_out,tname=tname,tunits=tunits)

    !call ens_write(ens_fldr,fldrs,filename,"Vtot",method="align")
    !call ens_write(ens_fldr,fldrs,filename,"Aib",prec="double")

    ! Test 2D writing
    filename = "rembo.gis.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["month","time "],t=time_out,tname=tname,tunits=tunits)

    call ens_write(ens_fldr,fldrs,filename,"tt",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"pp")

    ! Test 3D writing
    !filename = "sico.2d.nc"
    !call ens_init(ens_fldr,fldrs,filename,names=["x   ","y   ","time"], &
    !    t=time_out,tname=tname,tunits=tunits)

    !call ens_write(ens_fldr,fldrs,filename,"zs",method="align")
    !call ens_write(ens_fldr,fldrs,filename,"zb")

    

end program ens_rembo 
