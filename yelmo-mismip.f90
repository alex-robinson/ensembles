


program ens_yelmo

    use ensembles 
    use coordinates
    use ncio 
    use nml 

    implicit none 

    character(len=512) :: ens_fldr, filename
    character(len=512) :: ens_fldr_out 
    character(len=512), allocatable :: fldrs(:) 
    
    double precision, allocatable :: time1D(:), time2D(:)
    character(len=256) :: tname, tunits

    integer :: narg 

    ! =============================================
    ! 1. Define output folder and get ensemble
    !    input folders 
    ! =============================================

    narg = command_argument_count()
    if (narg .gt. 0) then 
        write(*,*) "Load ensemble folders from command line arguments ..."
        CALL get_command_argument(1,ens_fldr)

        if (narg .eq. 2) then 
            CALL get_command_argument(2,ens_fldr_out)
        else 
            ens_fldr_out = ens_fldr 
        end if 

    else 
        ! Hard coded folders 

        ens_fldr     = "/home/itabone/grisli-ucm/output/v0.32/iso/new/ens1/"
        ens_fldr_out = "grisli-ucm/output/iso-ilaria-1/ens1/"

    end if 

    write(*,*) "ens_fldr:     "//trim(ens_fldr)
    write(*,*) "ens_fldr_out: "//trim(ens_fldr_out) 

    call ens_folders(fldrs,path=ens_fldr,path_out=ens_fldr_out)

    ! =============================================
    ! 2. Define custom output times 
    ! =============================================
    tname  = "time"
    tunits = "years"
    call ens_times(time1D,par=[  0.d3,150.d3,0.5d3] )
    call ens_times(time2D,times=[0.d3,20.d3,150.d3] )

    ! =============================================
    ! 4. Write ensemble files 
    ! =============================================

    ! ! ## Parameters ##
    ! call ens_write_par(ens_fldr_out,fldrs,filename="grisli_Greenland.nml",fmt="nml", &
    !     names=["bmelt_par:kappa_grz ","isos_par:tau        ","clim_forcing:f_eem  "])

    ! ## yelmo2D ##
    filename = "yelmo2D.nc"
    call ens_init(ens_fldr_out,fldrs,filename,names=["xc  ","yc  ","time"],t=time1D,tname=tname,tunits=tunits)

    call ens_write(ens_fldr_out,fldrs,filename,"x_rf",method="align")
    call ens_write(ens_fldr_out,fldrs,filename,"x_gl",method="align")
    
    ! ! ## GRISLI 2D ##
    ! filename = "grisli2D.nc"
    ! call ens_init(ens_fldr_out,fldrs,filename,names=["xc  ","yc  ","time"],static=["lon","lat"], &
    !               t=time2D,tname=tname,tunits=tunits)

    ! ! Write fields with integer precision (rounds to the meter) to save space
    ! call ens_write(ens_fldr_out,fldrs,filename,"S",     method="align",prec="int")
    ! call ens_write(ens_fldr_out,fldrs,filename,"H",     method="align",prec="int")
    ! call ens_write(ens_fldr_out,fldrs,filename,"Bsoc",  method="align",prec="int")
    ! call ens_write(ens_fldr_out,fldrs,filename,"smb",   method="align",prec="real")
    ! call ens_write(ens_fldr_out,fldrs,filename,"Bmelt", method="align",prec="real")
    ! call ens_write(ens_fldr_out,fldrs,filename,"dTshlf",method="align",prec="real")
    ! call ens_write(ens_fldr_out,fldrs,filename,"Tjja",  method="align",prec="real")
    ! call ens_write(ens_fldr_out,fldrs,filename,"Tb0",   method="align",prec="real")
    ! call ens_write(ens_fldr_out,fldrs,filename,"H_water",  method="align",prec="real")
    ! call ens_write(ens_fldr_out,fldrs,filename,"U",        method="align",prec="real")
    ! call ens_write(ens_fldr_out,fldrs,filename,"Ub_tot",   method="align",prec="real")
    ! call ens_write(ens_fldr_out,fldrs,filename,"gl_frac",  method="align",prec="real")

    write(*,*)
    write(*,*) "Ensemble generation completed."
    write(*,*)

end program ens_yelmo 

