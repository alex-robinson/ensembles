


module ensembles 

    use interp1D 
    use ncio 

    implicit none 


    double precision, parameter :: mv = -9999.d0




    private 
    public :: ens_init, ens_1D

contains 




    subroutine ens_1D(ens_fldr,fldrs,filename,name,units,time,tname,prec)

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, fldrs(:), filename 
        character(len=*), intent(IN) :: name, units, tname
        double precision, intent(IN) :: time(:) 
        character(len=*), intent(IN), optional :: prec 
        character(len=32) :: precision

        character(len=512)  :: filename_out
        character(len=1024) :: path_in, path_out  

        integer :: nfldr, nsim, nt, q 

        double precision, allocatable :: time0(:), var_in(:), var_out(:)
        integer :: nt0, k, k0, k1, l0, l1

        ! Define output ensemble filename 
        filename_out = "ens_"//trim(filename)
        path_out     = trim(ens_fldr)//"/"//trim(filename_out)

        ! Determine output precision (int,float,double)
        precision = "float"
        if (present(prec)) precision = trim(prec)

        ! Determine number of simulations based on folders 
        nfldr = size(fldrs) 
        nsim  = nc_size(path_out,"sim")
        if (nfldr .ne. nsim) then 
            write(*,*) "ens_1D:: error: number of folders does not match number of sims in the file."
            write(*,*) "ensemble file: ",trim(path_out)
            write(*,*) "nfolders = ",nfldr 
            write(*,*) "nsims    = ",nsim
        end if 

        ! Allocate the output variable, set to missing values 
        nt = size(time) 
        allocate(var_out(nt))
        var_out = mv 

        ! Loop over folders and write variable to ensemble file
        do q = 1, nsim  

            ! Reset output variable data to missing data
            var_out = mv 

            ! Define file for current simulation 
            path_in = trim(fldrs(q))//"/"//trim(filename)

            ! Get original time length and dimension
            path_in = trim(fldrs(1))//"/"//trim(filename)
            nt0 = nc_size(path_in,tname)
            if (allocated(time0)) deallocate(time0)
            if (allocated(var_in)) deallocate(var_in)
            allocate(time0(nt0),var_in(nt0))
            call nc_read(path_in,tname,time0,missing_value=mv)

            ! Read in variable 
            call nc_read(path_in,name,var_in,missing_value=mv)

            ! ## Interpolate to new resolution ##
            do k = 1, nt0
                k0 = k  
                if (var_in(k0) .ne. mv) exit 
            end do 

            do k = nt0, k0, -1 
                k1 = k 
                if (var_in(k1) .ne. mv) exit 
            end do 

            if (k1-k0 .le. 1) then 
                write(*,*) "ens_1D:: error: not enough data points are available."
                write(*,*) "filename name: ",trim(path_in), trim(name) 
                stop 
            end if 

            l0 = minloc(abs(time-time0(k0)),dim=1)
            l1 = minloc(abs(time-time0(k1)),dim=1)
            
            if (l1-l0 .le. 1) then 
                write(*,*) "ens_1D:: error: problem with time indices."
                write(*,*) "filename name: ",trim(path_in), trim(name) 
                write(*,*) "filename_out : ",trim(path_out)
                stop 
            end if 
            
            var_out(l0:l1) = interp_spline(time0(k0:k1),var_in(k0:k1),xout=time(l0:l1)) 

            ! Write to ensemble file 
            select case(trim(precision))
                case("int")
                    call nc_write(path_out,name,int(var_out),units=units,dim1="sim", &
                                  dim2=tname,start=[q,1],count=[1,nt],missing_value=int(mv))
                case("float")
                    call nc_write(path_out,name,real(var_out),units=units,dim1="sim", &
                                  dim2=tname,start=[q,1],count=[1,nt],missing_value=real(mv))
                case("double")
                    call nc_write(path_out,name,var_out,units=units,dim1="sim", &
                                  dim2=tname,start=[q,1],count=[1,nt],missing_value=mv)
                case DEFAULT 
                    write(*,*) "ens_1D:: error: output precision must be one of: "// &
                               "int, real, double. Specified: "//trim(precision)
                    stop 
            end select 

            write(*,"(a,a,2x,a,i3)") "ens_1D:: written: ",trim(path_out), trim(name), q 
        end do 

        return 

    end subroutine ens_1D


    subroutine ens_init(ens_fldr,filename,nsim,x,xname,xunits, &
                        y,yname,yunits,z,zname,zunits,t,tname,tunits)

        ! This subroutine will initialize an ensemble file of up to four dimensions
        ! (3 dimensions + 1 time dimension)

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, filename
        integer :: nsim

        double precision, intent(IN), optional :: x(:) 
        character(len=*), intent(IN), optional :: xname, xunits 
        
        double precision, intent(IN), optional :: y(:) 
        character(len=*), intent(IN), optional :: yname, yunits 
            
        double precision, intent(IN), optional :: z(:) 
        character(len=*), intent(IN), optional :: zname, zunits 
        
        double precision, intent(IN), optional :: t(:) 
        character(len=*), intent(IN), optional :: tname, tunits 
        
        character(len=512) :: filename_out, path_out 
        integer :: nvar, q 


        ! Define output ensemble filename 
        filename_out = "ens_"//trim(filename)
        path_out     = trim(ens_fldr)//"/"//trim(filename_out)

        call nc_create(path_out,description="Generated by ensembles module.")
        call nc_write_dim(path_out,"sim",x=1,dx=1,nx=nsim)

        if (present(x)) then 
            if (.not. present(xname) .or. .not. present(xunits)) then 
                write(*,*) "ens_init_temporal:: error: To define x dimension, name and units are needed."
                stop 
            end if 

            call nc_write_dim(path_out,xname,x=x,units=xunits)

        end if 

        if (present(y)) then 
            if (.not. present(yname) .or. .not. present(yunits)) then 
                write(*,*) "ens_init_temporal:: error: To define y dimension, name and units are needed."
                stop 
            end if 

            call nc_write_dim(path_out,yname,x=y,units=yunits)

        end if 

        if (present(z)) then 
            if (.not. present(zname) .or. .not. present(zunits)) then 
                write(*,*) "ens_init_temporal:: error: To define z dimension, name and units are needed."
                stop 
            end if 

            call nc_write_dim(path_out,zname,x=z,units=zunits)

        end if 

        if (present(t)) then 
            if (.not. present(tname) .or. .not. present(tunits)) then 
                write(*,*) "ens_init_temporal:: error: To define time dimension, name and units are needed."
                stop 
            end if 

            call nc_write_dim(path_out,tname,x=t,units=tunits,calendar="360_day",unlimited=.TRUE.)

        end if 

        return 

    end subroutine ens_init

!         do q = 1, nvar 
!             call nc_write(path_out,names(q),mv,dim1="sim",dim2=xname,dim3="time", &
!                           units=units(q),missing_value=mv,start=[1,1,1],count=[1,1,1])
!         end do 


    subroutine ens_folders()

        implicit none 

        return 

    end subroutine ens_folders

end module ensembles 