


program ens_grisli

    use ensembles 
    use coordinates
    use ncio 
    use nml 

    implicit none 

    character(len=512) :: ens_fldr, filename
    character(len=512), allocatable :: fldrs(:) 
    
    double precision, allocatable :: time1D(:), time2D(:)
    character(len=256) :: tname, tunits

    ! =============================================
    ! 1. Define output folder and get ensemble
    !    input folders 
    ! =============================================
    ens_fldr = "GRISLI/output/iso1"
    call ens_folders(fldrs,path=ens_fldr)

    ! =============================================
    ! 2. Define custom output times 
    ! =============================================
    tname  = "time"
    tunits = "years BP"
    call ens_times(time1D,par=[-250.d3,-140.d3,2.0d3, &
                               -140.d3,-110.d3,0.1d3, &
                               -110.d3, -25.d3,0.5d3, &
                                -25.d3, -16.d3,0.2d3, &
                                -16.d3,   0.d3,0.1d3 ] )
    call ens_times(time2D,par=[-130.d3,-110.d3,2.0d3], &
            times=[-22.d3,-16.d3,-12.d3,-8.d3,-4.d3,0.d3] )

    ! =============================================
    ! 4. Write ensemble files 
    ! =============================================

    ! ## Parameters ##
    call ens_write_par(ens_fldr,fldrs,filename="grisli_par_Greenland.nml",fmt="nml", &
                       names=["kappa_grz ","kappa_shlf"])

    stop 

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

    ! ## REMBO 2D (month+time) ##
    filename = "rembo.gis.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["point","month","time "],t=time1D,tname=tname,tunits=tunits)

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

    ! Forcing too 
    call ens_write(ens_fldr,fldrs,filename,"dT_jja")
!     call ens_write(ens_fldr,fldrs,filename,"dT_amp")

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

end program ens_grisli 

