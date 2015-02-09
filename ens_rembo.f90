


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

    ! =============================================
    ! 1. Define output folder and get ensemble
    !    input folders 
    ! =============================================
    ens_fldr = "output"
    call ens_folders(fldrs,path="data")

    ! =============================================
    ! 2. Define custom output times 
    ! =============================================
    call ens_times(time_out,par=[1700.d0,1720.d0,1.d0])
    tname  = "time"
    tunits = "ka BP"

    ! =============================================
    ! 4. Write ensemble files 
    ! =============================================

    ! ## Parameters ##
    call ens_write_par(ens_fldr,fldrs,filename="options_rembo",fmt="options", &
                       names=["dT_factor","itm_c    ","ppfac    "])

    call ens_write_par(ens_fldr,fldrs,filename="options_sico",fmt="options", &
                       names=["Q_GEO_0  ","C_SLIDE_0"])

    ! ## SICO 1D (time) ##
    filename = "sico.1d.nc"
!     call ens_init(ens_fldr,fldrs,filename,names=["time"],t=time_out,tname=tname,tunits=tunits)
    call ens_init(ens_fldr,fldrs,filename,names=["time"])

    call ens_write(ens_fldr,fldrs,filename,"Vtot",method="align")
    call ens_write(ens_fldr,fldrs,filename,"Aib",prec="double")

    ! ## SICO 2D (2D+time) ##
    filename = "sico.2d.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["x   ","y   ","time"], &
        t=time_out,tname=tname,tunits=tunits)

    call ens_write(ens_fldr,fldrs,filename,"zs",method="align")
    call ens_write(ens_fldr,fldrs,filename,"zb")

    ! ## REMBO 2D (month+time) ##
    filename = "rembo.gis.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["month","time "],t=time_out,tname=tname,tunits=tunits)

    call ens_write(ens_fldr,fldrs,filename,"tt")
    call ens_write(ens_fldr,fldrs,filename,"pp")

    ! ## REMBO 3D (2D+time) ##
    filename = "clima.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["x   ","y   ","time"])

    call ens_write(ens_fldr,fldrs,filename,"tt")
    call ens_write(ens_fldr,fldrs,filename,"pp")

    write(*,*)
    write(*,*) "Ensemble generation completed."
    write(*,*)

end program ens_rembo 

