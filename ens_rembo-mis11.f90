


program ens_rembo

    use ensembles 
    use coordinates
    use ncio 
    use nml 

    implicit none 

    character(len=512) :: ens_fldr, filename
    character(len=512), allocatable :: fldrs(:) 
    
    double precision, allocatable :: time1D(:), time2D(:)
    character(len=256) :: tname, tunits

    character(len=256) :: cores(7)
    integer :: q 

    ! =============================================
    ! 1. Define output folder and get ensemble
    !    input folders 
    ! =============================================
    ens_fldr = "output/mis11y_m11"
    call ens_folders(fldrs,path=ens_fldr)

    ! =============================================
    ! 2. Define custom output times 
    ! =============================================
    tname  = "time"
    tunits = "years BP"
    call ens_times(time1D,par=[-550.d3,-430.d3,2.d3, &
                               -430.d3,-390.d3,0.1d3, &
                               -390.d3,-350.d3,0.5d3 ] )
    call ens_times(time2D,par=[-411.d3,-400.d3,0.5d3] )

    ! =============================================
    ! 4. Write ensemble files 
    ! =============================================

    ! ## Parameters ##
    call ens_write_par(ens_fldr,fldrs,filename="options_rembo",fmt="options", &
                       names=["dT_factor    ","dT_width     ","itm_c        ","ppfac        ", &
                              "paleo_frac_dT"])

    ! ## SICO 1D (time) ##
    filename = "sico.1d.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["time"],t=time1D,tname=tname,tunits=tunits)

    call ens_write(ens_fldr,fldrs,filename,"Vtot")
    call ens_write(ens_fldr,fldrs,filename,"Aib")
    call ens_write(ens_fldr,fldrs,filename,"dVdt")
    call ens_write(ens_fldr,fldrs,filename,"zs_max")
    call ens_write(ens_fldr,fldrs,filename,"z_sle")
    call ens_write(ens_fldr,fldrs,filename,"z_sl")

    call ens_write(ens_fldr,fldrs,filename,"V_south")
    call ens_write(ens_fldr,fldrs,filename,"A_south")

    ! ## SICO 2D (2D+time) ##
    filename = "sico.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["x   ","y   ","time"],static=["lon","lat"], &
                  t=time2D,tname=tname,tunits=tunits)

    ! Write fields with integer precision (rounds to the meter) to save space
    call ens_write(ens_fldr,fldrs,filename,"mask",method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"zs",  method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"zb",  method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"H",   method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"H_t", method="align",prec="int")

    ! ## SICO CORES (time) ##
    cores = ["cc     ", "dye3   ", "gisp2  ", "grip   ", "neem   ", "neem_up", "ngrip  "]
    do q = 1, size(cores)

        filename = "core_"//trim(cores(q))//".nc"
        call ens_init(ens_fldr,fldrs,filename,names=["z   ","time"], &
                      t=time1D,tname=tname,tunits=tunits)

        call ens_write(ens_fldr,fldrs,filename,"delta_ts")
        call ens_write(ens_fldr,fldrs,filename,"zs")
        call ens_write(ens_fldr,fldrs,filename,"H")
        call ens_write(ens_fldr,fldrs,filename,"temp")
        call ens_write(ens_fldr,fldrs,filename,"Rb")
        call ens_write(ens_fldr,fldrs,filename,"tts")
        call ens_write(ens_fldr,fldrs,filename,"tt")
        call ens_write(ens_fldr,fldrs,filename,"tdjf")
        call ens_write(ens_fldr,fldrs,filename,"tjja")
        call ens_write(ens_fldr,fldrs,filename,"tjan")
        call ens_write(ens_fldr,fldrs,filename,"tjul")
        call ens_write(ens_fldr,fldrs,filename,"ttp")
        call ens_write(ens_fldr,fldrs,filename,"pp")
        call ens_write(ens_fldr,fldrs,filename,"snow")
        call ens_write(ens_fldr,fldrs,filename,"Q_bm")
        call ens_write(ens_fldr,fldrs,filename,"zc")
        call ens_write(ens_fldr,fldrs,filename,"temp_p")
        call ens_write(ens_fldr,fldrs,filename,"age_p")
        
    end do 

    ! ## REMBO 2D (month+time) ##
    filename = "rembo.gis.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["month","time "],t=time1D,tname=tname,tunits=tunits)

    call ens_write(ens_fldr,fldrs,filename,"tt")
    call ens_write(ens_fldr,fldrs,filename,"tte")
    call ens_write(ens_fldr,fldrs,filename,"pp")
    call ens_write(ens_fldr,fldrs,filename,"snow")
    call ens_write(ens_fldr,fldrs,filename,"runoff")
    call ens_write(ens_fldr,fldrs,filename,"smb")
    call ens_write(ens_fldr,fldrs,filename,"dT")
    call ens_write(ens_fldr,fldrs,filename,"dTb")
    call ens_write(ens_fldr,fldrs,filename,"S")
    ! call ens_write(ens_fldr,fldrs,filename,"S65")
    call ens_write(ens_fldr,fldrs,filename,"aco2")

    ! ## REMBO 3D (2D+time) ##
    filename = "clima.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["x   ","y   ","time"],static=["lon","lat"], &
                  t=time2D,tname=tname,tunits=tunits)

    call ens_write(ens_fldr,fldrs,filename,"mask",method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"zs",  method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"tjja",method="align")
    call ens_write(ens_fldr,fldrs,filename,"tann",method="align")
    call ens_write(ens_fldr,fldrs,filename,"tjan",method="align")
    call ens_write(ens_fldr,fldrs,filename,"tjul",method="align")
    call ens_write(ens_fldr,fldrs,filename,"pp",  method="align")
    call ens_write(ens_fldr,fldrs,filename,"pdds",method="align")

    write(*,*)
    write(*,*) "Ensemble generation completed."
    write(*,*)

end program ens_rembo 

