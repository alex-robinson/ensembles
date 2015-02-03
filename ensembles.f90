


module ensembles 

    use interp1D 
    use ncio 

    implicit none 


    double precision, parameter :: mv = -9999.d0




    private 
    public :: ens_init, ens_write, ens_1D

contains 


    subroutine ens_write(ens_fldr,fldrs,filename,name,tname,time,prec,units,method)
        ! This routine will prepare a file for writing ensemble data,
        ! assuming the dimension variables already exist. It determines
        ! the shape of the current variable from the first file, and 
        ! can potentially interpolate one dimension (ie, time) using 
        ! a spline or linear approximation. 

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, fldrs(:), filename 
        character(len=*), intent(IN) :: name, tname
        double precision, intent(IN) :: time(:) 
        character(len=*), intent(IN), optional :: prec, units, method 
        character(len=32) :: precision
        character(len=256) :: var_units

        character(len=512)  :: filename_out
        character(len=1024) :: path_in, path_out  
        
        character(len=32), allocatable :: names(:)
        integer, allocatable :: dims(:)

        integer :: nfldr, nsim, ndim, nt, q 

        double precision, allocatable :: tin1D(:), vout1D(:)
        double precision, allocatable :: vin1D(:), vin2D(:,:), vin3D(:,:) 
        integer :: nt0, k, k0, k1, l0, l1, i, j  

        ! Define output ensemble filename 
        filename_out = "ens_"//trim(filename)
        path_out     = trim(ens_fldr)//"/"//trim(filename_out)

        ! Define first source file for loading units, etc.
        path_in = trim(fldrs(1))//"/"//trim(filename)

        ! Determine output precision (int,real,double)
        precision = "real"
        if (present(prec)) precision = trim(prec)

        ! Determine units 
        if (present(units)) then 
            var_units = trim(units)
        else 
            call nc_read_attr(path_in,varname=name,name="units",value=var_units)
        end if 

        ! Get dim names and lengths
        call nc_dims(path_in,name=name,names=names,dims=dims)
        ndim = size(dims)

        write(*,"(a12,a3,4a12,a3)") trim(name)
        write(*,*) "Writing precision: ",trim(precision)
        write(*,*) "Original dimensions: ", ndim
        write(*,*) names, dims

        ! Determine number of simulations based on folders 
        nfldr = size(fldrs) 
        nsim  = nc_size(path_out,"sim")
        if (nfldr .ne. nsim) then 
            write(*,*) "ens_write:: error: number of folders does not match number of sims in the file."
            write(*,*) "ensemble file: ",trim(path_out)
            write(*,*) "nfolders = ",nfldr 
            write(*,*) "nsims    = ",nsim
        end if 

        ! Allocate input variable 

        ! Allocate the output variable
        nt = size(time) 
        allocate(vout1D(nt)) 

        ! Loop over folders and write variable to ensemble file
        do q = 1, nsim  

            ! Reset output variable data to missing data
            vout1D = mv 

            ! Define file for current simulation 
            path_in = trim(fldrs(q))//"/"//trim(filename)

            ! Get original time length and dimension, if exists
            if (names(size(names)) == tname) then 
                nt0 = nc_size(path_in,tname)
                if (allocated(tin1D)) deallocate(tin1D)
                if (allocated(vin1D)) deallocate(vin1D)
                allocate(tin1D(nt0),vin1D(nt0))
                call nc_read(path_in,tname,tin1D,missing_value=mv)
            end if 


            select case(ndim)

            case(1)

                ! Read in variable 
                call nc_read(path_in,name,vin1D,missing_value=mv,start=[1],count=[dims(1)])

                ! Interpolate input data 
                vout1D = ens_interp(tin1D,vin1D,time,missing_value=mv,method=method)

                ! Write to ensemble file 
                select case(trim(precision))
                    case("int")
                        call nc_write(path_out,name,int(vout1D),units=var_units,dim1="sim", &
                                      dim2=tname,start=[q,1],count=[1,nt],missing_value=int(mv))
                    case("real")
                        call nc_write(path_out,name,real(vout1D),units=var_units,dim1="sim", &
                                      dim2=tname,start=[q,1],count=[1,nt],missing_value=real(mv))
                    case("double")
                        call nc_write(path_out,name,vout1D,units=var_units,dim1="sim", &
                                      dim2=tname,start=[q,1],count=[1,nt],missing_value=mv)
                    case DEFAULT 
                        write(*,*) "ens_write:: error: output precision must be one of: "// &
                                   "int, real, double. Specified: "//trim(precision)
                        stop 
                end select 

                write(*,"(a,a,2x,a,i3)") "ens_write:: 1D field written: ",trim(path_out), trim(name), q 
                
            case(2)

                ! Loop over dimensions
                do i = 1,dims(1)
                    ! Read in variable 
                    call nc_read(path_in,name,vin1D,missing_value=mv,start=[i,1],count=[1,dims(1)])

                    ! Interpolate input data 
                    vout1D = ens_interp(tin1D,vin1D,time,missing_value=mv,method=method)

                    ! Write to ensemble file 
                    write(*,*) "case= ",trim(precision)
                    select case(trim(precision))
                        case("int")
                            call nc_write(path_out,name,int(vout1D),units=var_units,dim1="sim", &
                                          dim2=names(1),dim3=tname,start=[q,i,1],count=[1,1,nt],missing_value=int(mv))
                            write(*,*) "Wrote int 2D!"
                        case("real")
                            call nc_write(path_out,name,real(vout1D),units=var_units,dim1="sim", &
                                          dim2=names(1),dim3=tname,start=[q,i,1],count=[1,1,nt],missing_value=real(mv))
                            write(*,*) "Wrote real 2D!"
                        case("double")
                            call nc_write(path_out,name,vout1D,units=var_units,dim1="sim", &
                                          dim2=names(1),dim3=tname,start=[q,i,1],count=[1,1,nt],missing_value=mv)
                            write(*,*) "Wrote double 2D!"
                        case DEFAULT 
                            write(*,*) "ens_write:: error: output precision must be one of: "// &
                                       "int, real, double. Specified: "//trim(precision)
                            stop 
                    end select 
                end do 

                write(*,"(a,a,2x,a,i3)") "ens_write:: 2D field written: ",trim(path_out), trim(name), q 

            case DEFAULT
                write(*,*) "ens_write:: error: number of dimensions not handled."
                write(*,*) "path, name, ndim: ", trim(path_in), trim(name), ndim 
                stop 

            end select

        end do  

        return 

    end subroutine ens_write

    function ens_interp(x,y,xout,missing_value,method) result(yout)

        implicit none 

        double precision :: x(:), y(:), xout(:)
        double precision :: yout(size(x))
        double precision :: missing_value 
        character(len=*), optional :: method 
        character(len=32) :: interp_method 
        integer :: nx, k, k0, k1, l0, l1

        ! ## Interpolate to new resolution ##

        ! Determine interpolation option 
        interp_method = "spline"
        if (present(method)) interp_method = trim(method)

        ! Length of original series
        nx = size(x) 

        ! Check for starting and ending indices of non-missing data 
        ! (assumes a contiguous datasets!!)
        do k = 1, nx
            k0 = k  
            if (y(k0) .ne. missing_value) exit 
        end do 

        do k = nx, k0, -1 
            k1 = k 
            if (y(k1) .ne. missing_value) exit 
        end do 

        if (k1-k0 .le. 1) then 
            write(*,*) "ens_interp:: error: not enough data points are available."
            write(*,*) "Range x:    ", minval(x,mask=x .ne. missing_value), &
                                       maxval(x,mask=x .ne. missing_value)
            write(*,*) "Range xout: ", minval(xout), maxval(x)                                           
            stop 
        end if 

        ! Find the corresponding indices in the output vector
        l0 = minloc(abs(xout-x(k0)),dim=1)
        l1 = minloc(abs(xout-x(k1)),dim=1)
        
        if (l1-l0 .le. 1) then 
            write(*,*) "ens_interp:: error: problem with time indices."
            stop 
        end if 
        
        ! Apply missing values to output vector and then interpolate as desired
        yout = missing_value

        select case(trim(interp_method))
            case("spline")
                yout(l0:l1) = interp_spline(x(k0:k1),y(k0:k1),xout=xout(l0:l1)) 
            case("linear")
                yout(l0:l1) = interp_linear(x(k0:k1),y(k0:k1),xout=xout(l0:l1)) 
            case("align")
                yout(l0:l1) = interp_align(x(k0:k1),y(k0:k1),xout=xout(l0:l1)) 
            case DEFAULT 
                write(*,*) "ens_interp:: error: interpolation method must be one of: spline, linear, align."
                stop 
        end select

        return 

    end function ens_interp 

    subroutine ens_1D(ens_fldr,fldrs,filename,name,time,tname,prec,units,interp)

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, fldrs(:), filename 
        character(len=*), intent(IN) :: name, tname
        double precision, intent(IN) :: time(:) 
        character(len=*), intent(IN), optional :: prec, units, interp 
        character(len=32) :: precision
        character(len=256) :: var_units, interpolation 

        character(len=512)  :: filename_out
        character(len=1024) :: path_in, path_out  
         
        integer :: nfldr, nsim, nt, q 

        double precision, allocatable :: time0(:), var_in(:), var_out(:)
        integer :: nt0, k, k0, k1, l0, l1

        ! Define output ensemble filename 
        filename_out = "ens_"//trim(filename)
        path_out     = trim(ens_fldr)//"/"//trim(filename_out)

        ! Define first source file for loading units, etc
        path_in = trim(fldrs(1))//"/"//trim(filename)

        ! Determine output precision (int,real,double)
        precision = "real"
        if (present(prec)) precision = trim(prec)

        ! Determine units 
        if (present(units)) then 
            var_units = trim(units)
        else 
            call nc_read_attr(path_in,varname=name,name="units",value=var_units)
        end if 

        ! Determine interpolation option 
        interpolation = "spline"
        if (present(interp)) interpolation = trim(interp)

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
            
            select case(trim(interpolation))
                case("spline")
                    var_out(l0:l1) = interp_spline(time0(k0:k1),var_in(k0:k1),xout=time(l0:l1)) 
                case("linear")
                    var_out(l0:l1) = interp_linear(time0(k0:k1),var_in(k0:k1),xout=time(l0:l1)) 
                case("align")
                    var_out(l0:l1) = interp_align(time0(k0:k1),var_in(k0:k1),xout=time(l0:l1)) 
                case DEFAULT 
                    write(*,*) "ens_1D:: error: interpolation method must be one of: spline, linear, align."
                    stop 
            end select

            ! Write to ensemble file 
            select case(trim(precision))
                case("int")
                    call nc_write(path_out,name,int(var_out),units=var_units,dim1="sim", &
                                  dim2=tname,start=[q,1],count=[1,nt],missing_value=int(mv))
                case("real")
                    call nc_write(path_out,name,real(var_out),units=var_units,dim1="sim", &
                                  dim2=tname,start=[q,1],count=[1,nt],missing_value=real(mv))
                case("double")
                    call nc_write(path_out,name,var_out,units=var_units,dim1="sim", &
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