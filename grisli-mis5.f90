


program ens_grisli

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
    tunits = "years BP"
    call ens_times(time1D,par=[-250.d3,-140.d3,2.0d3, &
                               -140.d3,-110.d3,0.1d3, &
                               -110.d3, -25.d3,0.5d3, &
                                -25.d3, -16.d3,0.2d3, &
                                -16.d3,   0.d3,0.2d3 ] )
    call ens_times(time2D,par=[-132.d3,-129.d3,0.2d3, &
                               -129.d3,-122.d3,0.1d3, &
                               -122.d3,-118.d3,0.2d3], &
            times=[-22.d3,-16.d3,-12.d3,-8.d3,-4.d3,0.d3] )

    ! =============================================
    ! 4. Write ensemble files 
    ! =============================================

    ! ## Parameters ##
    call ens_write_par(ens_fldr_out,fldrs,filename="grisli_Greenland.nml",fmt="nml", &
        names=["bmelt_par:kappa_grz ","isos_par:tau        ","clim_forcing:f_eem  "])

    ! ## GRISLI 1D ##
    filename = "grisli1D.nc"
    call ens_init(ens_fldr_out,fldrs,filename,names=["time"],t=time1D,tname=tname,tunits=tunits)

    call ens_write(ens_fldr_out,fldrs,filename,"isvol",method="linear")
    call ens_write(ens_fldr_out,fldrs,filename,"isvolf",method="linear")
!     call ens_write(ens_fldr_out,fldrs,filename,"hmean_",method="linear")
!     call ens_write(ens_fldr_out,fldrs,filename,"Hmax_",method="linear")
    call ens_write(ens_fldr_out,fldrs,filename,"idx_at",method="linear")
    call ens_write(ens_fldr_out,fldrs,filename,"idx_ap",method="linear")
    call ens_write(ens_fldr_out,fldrs,filename,"idx_ao",method="linear")
!     call ens_write(ens_fldr_out,fldrs,filename,"idx_bt",method="linear")
!     call ens_write(ens_fldr_out,fldrs,filename,"idx_bp",method="linear")
!     call ens_write(ens_fldr_out,fldrs,filename,"idx_bo",method="linear")
    call ens_write(ens_fldr_out,fldrs,filename,"idx_sl",method="linear")
    
    ! ## GRISLI 2D ##
    filename = "grisli2D.nc"
    call ens_init(ens_fldr_out,fldrs,filename,names=["xc  ","yc  ","time"],static=["lon","lat"], &
                  t=time2D,tname=tname,tunits=tunits)

    ! Write fields with integer precision (rounds to the meter) to save space
    call ens_write(ens_fldr_out,fldrs,filename,"S",     method="align",prec="int")
    call ens_write(ens_fldr_out,fldrs,filename,"H",     method="align",prec="int")
    call ens_write(ens_fldr_out,fldrs,filename,"Bsoc",  method="align",prec="int")
    call ens_write(ens_fldr_out,fldrs,filename,"smb",   method="align",prec="real")
    call ens_write(ens_fldr_out,fldrs,filename,"Bmelt", method="align",prec="real")
    call ens_write(ens_fldr_out,fldrs,filename,"dTshlf",method="align",prec="real")
    call ens_write(ens_fldr_out,fldrs,filename,"Tjja",  method="align",prec="real")
    call ens_write(ens_fldr_out,fldrs,filename,"Tb0",   method="align",prec="real")
    call ens_write(ens_fldr_out,fldrs,filename,"H_water",  method="align",prec="real")
    call ens_write(ens_fldr_out,fldrs,filename,"U",        method="align",prec="real")
    call ens_write(ens_fldr_out,fldrs,filename,"Ub_tot",   method="align",prec="real")
    call ens_write(ens_fldr_out,fldrs,filename,"gl_frac",  method="align",prec="real")
!     call ens_write(ens_fldr_out,fldrs,filename,"mask_bed", method="align",prec="int")
!     call ens_write(ens_fldr_out,fldrs,filename,"mask_border", method="align",prec="int")

    write(*,*)
    write(*,*) "Ensemble generation completed."
    write(*,*)

end program ens_grisli 

