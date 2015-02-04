


module ensembles 

    use interp1D 
    use ncio 

    implicit none 


    double precision, parameter :: mv = -9999.d0


    interface ens_write
        module procedure ens_write_static 
!         module procedure ens_write_temporal
    end interface

    private 
    public :: ens_init, ens_write

contains 

    subroutine ens_write_static(ens_fldr,fldrs,filename,name,prec,units,method)
        ! This routine will prepare a file for writing ensemble data,
        ! assuming the dimension variables already exist. It determines
        ! the shape of the current variable from the first file, and 
        ! can potentially interpolate one dimension (ie, time) using 
        ! a spline or linear approximation. 

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, fldrs(:), filename 
        character(len=*), intent(IN) :: name
        character(len=*), intent(IN), optional :: prec, units, method 
        character(len=32) :: precision
        character(len=256) :: var_units

        character(len=512)  :: filename_out
        character(len=1024) :: path_in, path_out  
        
        character(len=32), allocatable :: names(:), names1(:)
        integer, allocatable :: dims(:), dims1(:)
        integer, allocatable :: start(:), count(:)

        integer :: nfldr, nsim, ndim, ndim1

        double precision, allocatable :: vin1D(:), vin2D(:,:), vin3D(:,:,:) 
        integer :: q, i, j, k

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
        ndim  = size(dims)

        ndim1 = ndim+1 
        allocate(names1(ndim1),dims1(ndim1))
        names1(1)       = "sim"
        names1(2:ndim1) = names 
        do k = 1, ndim1
            dims1(k) = nc_size(path_out,names1(k))
        end do 

        write(*,"(a,2x,a,1x,a1,1x,a)") "ens_write:: "//trim(path_out), trim(name), ":", trim(precision)
        do k = 1, ndim1 
            write(*,"(a12,i6)") names1(k), dims1(k)
        end do 

        ! Determine number of simulations based on folders 
        nfldr = size(fldrs) 
        nsim  = nc_size(path_out,"sim")
        if (nfldr .ne. nsim) then 
            write(*,*) "ens_write:: error: number of folders does not match number of sims in the file."
            write(*,*) "ensemble file: ",trim(path_out)
            write(*,*) "nfolders = ",nfldr 
            write(*,*) "nsims    = ",nsim
        end if 

        ! Get start and count vectors ready
        allocate(start(ndim1),count(ndim1))
        
        ! Write ensemble data to file based on dimensions
        select case(ndim)

            case(1)

                ! Loop over folders and write variable to ensemble file
                do q = 1, nsim  

                    ! Define file for current simulation 
                    path_in = trim(fldrs(q))//"/"//trim(filename)

                    ! Allocate variable
                    if (allocated(vin1D)) deallocate(vin1D)
                    allocate(vin1D(dims(1)))

                    ! Read in variable 
                    call nc_read(path_in,name,vin1D,missing_value=mv)

                    ! Define current start and count
                    start = 1
                    start(1) = q 
                    count = dims1 
                    count(1) = 1 

                    write(*,"(a12,4i6)") "start: ", start 
                    write(*,"(a12,4i6)") "count: ", count 
                    
                    ! Write to ensemble file 
                    select case(trim(precision))
                        case("int")
                            call nc_write(path_out,name,int(vin1D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=int(mv))
                        case("real")
                            call nc_write(path_out,name,real(vin1D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=real(mv))
                        case("double")
                            call nc_write(path_out,name,vin1D,units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=mv)
                        case DEFAULT 
                            write(*,*) "ens_write:: error: output precision must be one of: "// &
                                       "int, real, double. Specified: "//trim(precision)
                            stop 
                    end select 

                    write(*,"(a,a,2x,a,i3)") "ens_write:: 1D field written: ", &
                                             trim(path_out), trim(name), q 
                    
                end do 

            case(2)

                ! Loop over folders and write variable to ensemble file
                do q = 1, nsim  

                    ! Define file for current simulation 
                    path_in = trim(fldrs(q))//"/"//trim(filename)

                    ! Allocate variable
                    if (allocated(vin2D)) deallocate(vin2D)
                    allocate(vin2D(dims(1),dims(2)))

                    ! Read in variable 
                    call nc_read(path_in,name,vin2D,missing_value=mv)

                    ! Define current start and count
                    start = 1
                    start(1) = q 
                    count = dims1 
                    count(1) = 1 

                    write(*,"(a12,4i6)") "start: ", start 
                    write(*,"(a12,4i6)") "count: ", count 
                    
                    ! Write to ensemble file 
                    select case(trim(precision))
                        case("int")
                            call nc_write(path_out,name,int(vin2D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=int(mv))
                        case("real")
                            call nc_write(path_out,name,real(vin2D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=real(mv))
                        case("double")
                            call nc_write(path_out,name,vin2D,units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=mv)
                        case DEFAULT 
                            write(*,*) "ens_write:: error: output precision must be one of: "// &
                                       "int, real, double. Specified: "//trim(precision)
                            stop 
                    end select 
 
                    write(*,"(a,a,2x,a,i3)") "ens_write:: 2D field written: ", &
                                             trim(path_out), trim(name), q 
                end do 

            case(3)

                ! Loop over folders and write variable to ensemble file
                do q = 1, nsim  

                    ! Define file for current simulation 
                    path_in = trim(fldrs(q))//"/"//trim(filename)

                    ! Allocate variable
                    if (allocated(vin3D)) deallocate(vin3D)
                    allocate(vin3D(dims(1),dims(2),dims(3)))

                    ! Read in variable 
                    call nc_read(path_in,name,vin3D,missing_value=mv)

                    ! Define current start and count
                    start = 1
                    start(1) = q 
                    count = dims1 
                    count(1) = 1 

                    write(*,"(a12,4i6)") "start: ", start 
                    write(*,"(a12,4i6)") "count: ", count 
                    
                    ! Write to ensemble file 
                    select case(trim(precision))
                        case("int")
                            call nc_write(path_out,name,int(vin3D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=int(mv))
                        case("real")
                            call nc_write(path_out,name,real(vin3D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=real(mv))
                        case("double")
                            call nc_write(path_out,name,vin3D,units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=mv)
                        case DEFAULT 
                            write(*,*) "ens_write:: error: output precision must be one of: "// &
                                       "int, real, double. Specified: "//trim(precision)
                            stop 
                    end select  
 
                    write(*,"(a,a,2x,a,i3)") "ens_write:: 3D field written: ", &
                                             trim(path_out), trim(name), q 

                end do 

            case DEFAULT
                write(*,*) "ens_write:: error: number of dimensions not handled."
                write(*,*) "path, name, ndim: ", trim(path_in), trim(name), ndim 
                stop 

        end select 

        return 

    end subroutine ens_write_static

    subroutine ens_write_temporal(ens_fldr,fldrs,filename,name,tname,method,prec,units)
        ! This routine will prepare a file for writing ensemble data,
        ! assuming the dimension variables already exist. It determines
        ! the shape of the current variable from the first file, and 
        ! can potentially interpolate one dimension (ie, time) using 
        ! a spline or linear approximation. 

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, fldrs(:), filename 
        character(len=*), intent(IN) :: name, tname, method
        character(len=*), intent(IN), optional :: prec, units
        character(len=32) :: precision
        character(len=256) :: var_units

        character(len=512)  :: filename_out
        character(len=1024) :: path_in, path_out  
        
        character(len=32), allocatable :: names(:), names1(:)
        integer, allocatable :: dims(:), dims1(:)

        integer :: nfldr, nsim, ndim, ndim1, nt

        double precision, allocatable :: tin1D(:), tout1D(:), vout1D(:)
        double precision, allocatable :: vin1D(:), vin2D(:,:), vin3D(:,:) 
        integer :: q, k, k0, k1, l0, l1, i, j  

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
        ndim  = size(dims)

        ndim1 = ndim+1 
        allocate(names1(ndim1),dims1(ndim1))
        names1(1)       = "sim"
        names1(2:ndim1) = names 
        names1(ndim1)   = tname 
        do k = 1, ndim1
            dims1(k) = nc_size(path_out,names1(k))
        end do 

        write(*,"(a,2x,a,1x,a1,1x,a)") "ens_write:: "//trim(path_out), trim(name), ":", trim(precision)
        do k = 1, ndim1 
            write(*,"(a12,i6)") names1(k), dims1(k)
        end do 

        ! Determine number of simulations based on folders 
        nfldr = size(fldrs) 
        nsim  = nc_size(path_out,"sim")
        if (nfldr .ne. nsim) then 
            write(*,*) "ens_write:: error: number of folders does not match number of sims in the file."
            write(*,*) "ensemble file: ",trim(path_out)
            write(*,*) "nfolders = ",nfldr 
            write(*,*) "nsims    = ",nsim
        end if 

        ! Allocate the output variable, read time axis from ensemble file
        nt = nc_size(path_out,tname)
        allocate(tout1D(nt),vout1D(nt))
        call nc_read(path_out,tname,tout1D)

        ! Loop over folders and write variable to ensemble file
        do q = 1, nsim  

            ! Reset output variable data to missing data
            vout1D = mv 

            ! Define file for current simulation 
            path_in = trim(fldrs(q))//"/"//trim(filename)

            ! Get original time length and dimension
            nt = nc_size(path_in,tname)
            if (allocated(tin1D)) deallocate(tin1D)
            if (allocated(vin1D)) deallocate(vin1D)
            allocate(tin1D(nt),vin1D(nt))
            call nc_read(path_in,tname,tin1D,missing_value=mv)

            select case(ndim)

                case(1)

                    ! Read in variable 
                    call nc_read(path_in,name,vin1D,missing_value=mv,start=[1],count=[dims(1)])

                    ! Interpolate input data 
                    vout1D = ens_interp(tin1D,vin1D,tout1D,missing_value=mv,method=method)

                    ! Write to ensemble file 
                    select case(trim(precision))
                        case("int")
                            call nc_write(path_out,name,int(vout1D),units=var_units, &
                                          dim1="sim",dim2=names1(2), &
                                          start=[q,1],count=[1,dims1(2)],missing_value=int(mv))
                        case("real")
                            call nc_write(path_out,name,real(vout1D),units=var_units, &
                                          dim1="sim",dim2=names1(2), &
                                          start=[q,1],count=[1,dims1(2)],missing_value=real(mv))
                        case("double")
                            call nc_write(path_out,name,vout1D,units=var_units, &
                                          dim1="sim",dim2=names1(2), &
                                          start=[q,1],count=[1,dims1(2)],missing_value=mv)
                        case DEFAULT 
                            write(*,*) "ens_write:: error: output precision must be one of: "// &
                                       "int, real, double. Specified: "//trim(precision)
                            stop 
                    end select 

                    write(*,"(a,a,2x,a,i3)") "ens_write:: 1D field written: ", &
                                             trim(path_out), trim(name), q 
                    
                case(2)

                    ! Loop over dimensions
                    do i = 1,dims(1)
                        ! Read in variable 
                        call nc_read(path_in,name,vin1D,missing_value=mv, &
                                     start=[i,1],count=[1,dims(2)])

                        ! Interpolate input data 
                        vout1D = ens_interp(tin1D,vin1D,tout1D,missing_value=mv,method=method)

                        ! Write to ensemble file 
                        select case(trim(precision))
                            case("int")
                                call nc_write(path_out,name,int(vout1D),units=var_units, &
                                              dim1="sim",dim2=names(1),dim3=names1(3), &
                                              start=[q,i,1],count=[1,1,dims1(3)],missing_value=int(mv))
                            case("real")
                                call nc_write(path_out,name,real(vout1D),units=var_units, &
                                              dim1="sim",dim2=names(1),dim3=names1(3), &
                                              start=[q,i,1],count=[1,1,dims1(3)],missing_value=real(mv))
                            case("double")
                                call nc_write(path_out,name,vout1D,units=var_units, &
                                              dim1="sim",dim2=names(1),dim3=names1(3), &
                                              start=[q,i,1],count=[1,1,dims1(3)],missing_value=mv)
                            case DEFAULT 
                                write(*,*) "ens_write:: error: output precision must be one of: "// &
                                           "int, real, double. Specified: "//trim(precision)
                                stop 
                        end select 
                    end do 

                    write(*,"(a,a,2x,a,i3)") "ens_write:: 2D field written: ", &
                                             trim(path_out), trim(name), q 

                case(3)

                    ! Loop over dimensions
                    do i = 1,dims(1)
                    do j = 1,dims(2)
                        ! Read in variable 
                        call nc_read(path_in,name,vin1D,missing_value=mv, &
                                     start=[i,j,1],count=[1,1,dims(3)])

                        ! Interpolate input data 
                        vout1D = ens_interp(tin1D,vin1D,tout1D,missing_value=mv,method="align")

                        ! Write to ensemble file 
                        select case(trim(precision))
                            case("int")
                                call nc_write(path_out,name,int(vout1D),units=var_units, &
                                              dim1="sim",dim2=names(1),dim3=names(2),dim4=names1(4), &
                                              start=[q,i,j,1],count=[1,1,1,dims1(4)],missing_value=int(mv))
                            case("real")
                                call nc_write(path_out,name,real(vout1D),units=var_units, &
                                              dim1="sim",dim2=names(1),dim3=names(2),dim4=names1(4), &
                                              start=[q,i,j,1],count=[1,1,1,dims1(4)],missing_value=real(mv))
                            case("double")
                                call nc_write(path_out,name,vout1D,units=var_units, &
                                              dim1="sim",dim2=names(1),dim3=names(2),dim4=names1(4), &
                                              start=[q,i,j,1],count=[1,1,1,dims1(4)],missing_value=mv)
                            case DEFAULT 
                                write(*,*) "ens_write:: error: output precision must be one of: "// &
                                           "int, real, double. Specified: "//trim(precision)
                                stop 
                        end select 
                    end do 
                    end do

                    write(*,"(a,a,2x,a,i3)") "ens_write:: 3D field written: ", &
                                             trim(path_out), trim(name), q 

                case DEFAULT
                    write(*,*) "ens_write:: error: number of dimensions not handled."
                    write(*,*) "path, name, ndim: ", trim(path_in), trim(name), ndim 
                    stop 

            end select

        end do  

        return 

    end subroutine ens_write_temporal

    subroutine ens_init(ens_fldr,fldrs,filename,names,t,tname,tunits)

        ! This subroutine will initialize an ensemble file with the
        ! dimensions `names` + a new dimension 'sim'
        ! time dimension (t,tname,tunits) handled specially

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, fldrs(:), filename
        character(len=*), intent(IN) :: names(:)
        double precision, intent(IN), optional  :: t(:)  
        character(len=32), intent(IN), optional :: tname, tunits

        double precision, allocatable :: x(:) 
        character(len=512) :: xname, xunits 
        
        character(len=512) :: filename_out, path_out, path_in
        integer :: q, nsim, ndim, nd, nx 
        logical :: unlim 

        ! Define output ensemble filename 
        filename_out = "ens_"//trim(filename)
        path_out     = trim(ens_fldr)//"/"//trim(filename_out)
        path_in      = trim(fldrs(1))//"/"//trim(filename)

        ! How many simulations and dims?
        nsim = size(fldrs)
        ndim = size(names)

        call nc_create(path_out,description="Generated by ensembles module.")
        call nc_write_dim(path_out,"sim",x=1,dx=1,nx=nsim)

        do q = 1, ndim
            xname = names(q) 
            nd = nc_size(path_in,xname)
            if (allocated(x)) deallocate(x)
            allocate(x(nd))
            call nc_read(path_in,xname,x)
            call nc_read_attr(path_in,xname,"units",value=xunits)
            unlim = .FALSE.

            if (trim(names(q)) .eq. "time") then 
                 unlim = .TRUE. 
                if (present(t)) then
                    if (.not. present(tname) .or. .not. present(tunits)) then 
                        write(*,*) "ens_init:: error: name and units of time dimension must be specified."
                        stop 
                    end if 
                    deallocate(x)
                    allocate(x(size(t)))
                    x = t 
                    xname    = trim(tname)
                    xunits   = trim(tunits)
                end if 
            end if 

!             write(*,"(i2,1x,a,1x,a)") q, trim(xname),trim(xunits)  
   
            call nc_write_dim(path_out,xname,x=x,units=xunits,unlimited=unlim)

        end do 

        return 

    end subroutine ens_init

    function ens_interp(x,y,xout,missing_value,method) result(yout)

        implicit none 

        double precision :: x(:), y(:), xout(:)
        double precision :: yout(size(xout))
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

    subroutine ens_folders()

        implicit none 

        return 

    end subroutine ens_folders

end module ensembles 