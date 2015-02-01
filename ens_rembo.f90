


program ens_rembo

    use ensembles 
    use coordinates
    use ncio 
    use nml 

    implicit none 

    character(len=512) :: ens_fldr, filename 
    character(len=512), allocatable :: names(:), units(:)

    double precision, allocatable :: time_in(:), time_out(:)
    character(len=256) :: tunits

    integer :: nsim, nt 

    integer :: k 

    ens_fldr = "output"
    filename = "sico.nc"

    allocate(names(3),units(3)) 
    names = ["var1","var2","var3"]
    units = ["m","m","m"]

    nt = 10 
    allocate(time_out(nt))
    do k = 1, nt 
        time_out = dble(k)*2.d0
    end do 

    nsim = 5 

    call ens_init_1D(ens_fldr,filename,names,units,time_out,tunits="ka BP",nsim=nsim)






end program ens_rembo 