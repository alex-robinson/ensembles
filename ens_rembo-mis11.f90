


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

    ! =============================================
    ! 1. Define output folder and get ensemble
    !    input folders 
    ! =============================================
    ens_fldr = "output/mis11s_m11"
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

!     ! ## Parameters ##
!     call ens_write_par(ens_fldr,fldrs,filename="options_rembo",fmt="options", &
!                        names=["dT_factor","itm_c    ","ppfac    "])

!     ! ## SICO 1D (time) ##
!     filename = "sico.1d.nc"
!     call ens_init(ens_fldr,fldrs,filename,names=["time"],t=time1D,tname=tname,tunits=tunits)

!     call ens_write(ens_fldr,fldrs,filename,"Vtot")
!     call ens_write(ens_fldr,fldrs,filename,"Aib")
!     call ens_write(ens_fldr,fldrs,filename,"dVdt")
!     call ens_write(ens_fldr,fldrs,filename,"zs_max")
!     call ens_write(ens_fldr,fldrs,filename,"z_sle")
!     call ens_write(ens_fldr,fldrs,filename,"z_sl")

!     ! ## SICO 2D (2D+time) ##
    filename = "sico.nc"
    call ens_init(ens_fldr,fldrs,filename,names=["x   ","y   ","time"],static=["lon","lat"])

    ! Write fields with integer precision (rounds to the meter) to save space
    call ens_write(ens_fldr,fldrs,filename,"mask",method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"zs",  method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"zb",  method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"H",   method="align",prec="int")
    call ens_write(ens_fldr,fldrs,filename,"H_t", method="align",prec="int")

!     call ens_write(ens_fldr,fldrs,filename,"zb")

!     ! ## REMBO 2D (month+time) ##
!     filename = "rembo.gis.nc"
!     call ens_init(ens_fldr,fldrs,filename,names=["month","time "],t=time_out,tname=tname,tunits=tunits)

!     call ens_write(ens_fldr,fldrs,filename,"tt")
!     call ens_write(ens_fldr,fldrs,filename,"pp")

!     ! ## REMBO 3D (2D+time) ##
!     filename = "clima.nc"
!     call ens_init(ens_fldr,fldrs,filename,names=["x   ","y   ","time"])

!     call ens_write(ens_fldr,fldrs,filename,"tt")
!     call ens_write(ens_fldr,fldrs,filename,"pp")

    write(*,*)
    write(*,*) "Ensemble generation completed."
    write(*,*)

end program ens_rembo 

