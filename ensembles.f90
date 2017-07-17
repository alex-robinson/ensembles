


module ensembles 

    use interp1D 
    use ncio 
    use nml 
    use parameters

    implicit none 

    ! Default missing value
    double precision, parameter :: mv = -9999.d0

    ! private interface to ensemble interpolation routines
    interface ens_interp 
        module procedure ens_interp_1D
        module procedure ens_interp_2D
        module procedure ens_interp_3D
    end interface

    private 
    public :: ens_init, ens_write, ens_write_par
    public :: ens_folders, ens_times

contains 

    subroutine ens_write_par(ens_fldr,fldrs,filename,fmt,names)
        ! This routine will prepare a file for writing ensemble data,
        ! assuming the dimension variables already exist. It determines
        ! the shape of the current variable from the first file, and 
        ! can potentially interpolate one dimension (ie, time) using 
        ! a spline or linear approximation. 

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, fldrs(:), filename, fmt
        character(len=*) :: names(:)

        character(len=512)  :: filename_out, groups(size(names))
        character(len=1024) :: path_in, path_out  
        double precision :: values(size(names))
        integer :: nfldr, nsim, np, q, i, n 
        integer :: ncid  

        ! Check if file format is supported
        if (trim(fmt) .ne. "nml" .and. trim(fmt) .ne. "options") then 
            write(*,*) "ens_write_par:: error: parameter file format not supported: "//trim(fmt)
            stop
        end if 

        ! Define output ensemble filename 
        filename_out = "ens_"//trim(filename)
        path_out     = trim(ens_fldr)//"/"//trim(filename_out)//".nc"

        ! Define first source file for loading units, etc.
        path_in = trim(fldrs(1))//"/"//trim(filename)

        ! Determine number of simulations based on folders 
        nfldr = size(fldrs) 
        nsim  = nfldr 
        np    = size(names)

        ! Separate out group names if namelist parameters 
        if (trim(fmt) .eq. "nml") then 
            do i = 1, np 
                q = index(names(i),":")
                if (q .le. 0) then 
                    write(*,*) "ens_write_par:: error: &
                        &nml parameters must be given as 'group:name'."
                    write(*,*) names(i) 
                    stop 
                end if 
                n = len(names)
                groups(i) = names(i)(1:q-1)
                names(i)  = names(i)(q+1:n)

                write(*,*) trim(groups(i))//" : "//trim(names(i))
            end do 
        end if 

        ! Create output file 
        call nc_create(path_out)
        call nc_write_dim(path_out,"sim",x=1,dx=1,nx=nsim)

        ! Loop over folders and write variable to ensemble file
        do q = 1, nsim  

            ! Define file for current simulation
            path_in = trim(fldrs(q))//"/"//trim(filename)

            if (trim(fmt) .eq. "options") then 
                ! Get the parameter choices from the options file,
                ! and save these values to the global parameter array
                call get_params(trim(path_in))
                
                ! Now load the parameters of interest 
                do i = 1, np
                    values(i) = param(names(i))
                end do 

            else 

                ! Load the parameters of interest from namelist file 
                do i = 1, np 
                    call nml_read(path_in,groups(i),names(i),values(i))
                end do 
 
            end if 

            ! Open the output file and write the parameters for this simulation
            call nc_open(path_out,ncid,writable=.TRUE.)
            
            do i = 1, np 
!                 write(*,*) "ens_write_par:: ",trim(path_out)," ",trim(names(i))," ",values(i)
                call nc_write(path_out,names(i),values(i),dim1="sim", &
                              start=[q],count=[1],ncid=ncid)
                if (trim(fmt)=="nml") call nc_write_attr(path_out,varname=names(i),name="group",value=groups(i))
            end do 

            call nc_close(ncid)

        end do 

        return 

    end subroutine ens_write_par 

    subroutine ens_write(ens_fldr,fldrs,filename,name,prec,units,method)
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
        
        character(len=32), allocatable :: names1(:)
        character(len=32), allocatable :: names(:)
        integer, allocatable :: dims(:), dims1(:)
        integer, allocatable :: start(:), count(:)

        integer :: nfldr, nsim, ndim, ndim1

        double precision, allocatable :: vin1D(:), vin2D(:,:), vin3D(:,:,:) 
        double precision, allocatable :: vout1D(:), vout2D(:,:), vout3D(:,:,:)
        double precision, allocatable :: tin(:), tout(:)
        character(len=256) :: tname_in, tname_out 
        integer :: q, i, j, k

        character(len=512) :: interp_txt, txt  

        ! Define output ensemble filename 
        filename_out = "ens_"//trim(filename)
        path_out     = trim(ens_fldr)//"/"//trim(filename_out)

        ! Define first source file for loading units, etc.
        path_in = trim(fldrs(1))//"/"//trim(filename)

        ! Determine output precision (int,real,double)
        precision = "real"
        if (present(prec)) precision = trim(prec)

        interp_txt = " -- interp. times" 
        if (present(method)) interp_txt = trim(interp_txt)//", "//trim(method)

        ! Determine units 
        if (present(units)) then 
            var_units = trim(units)
        else 
            call nc_read_attr(path_in,varname=name,name="units",value=var_units)
        end if 

        ! Get dim names and lengths in input file
        call nc_dims(path_in,name=name,names=names,dims=dims)
        ndim  = size(dims)

        write(*,*) "debugging..."
        write(*,*) "ndim = ", ndim, size(dims), size(names)
        write(*,*) "dims = ", dims 
        write(*,*) "names is allocated? ", allocated(names)
        write(*,*) "names = ", names

        ! Get dim names and lengths in output file
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

        ! Make additional error check 
        if (trim(names(ndim)) .eq. "time" .and. trim(names(ndim)) .ne. trim(names1(ndim1)) ) then 
            write(*,*) "ens_write:: error: currently only input and output time dimensions with&
                        & the same name are supported."
            write(*,*) trim(names(ndim)), " ", trim(names1(ndim1))
            stop 
        end if 

        ! Determine number of simulations based on folders 
        nfldr = size(fldrs) 
        nsim  = nc_size(path_out,"sim")
        if (nfldr .ne. nsim) then 
            write(*,*) "ens_write:: error: number of folders does not&
                       & match number of sims in the file."
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

                    ! Get input dimensions again (in case they changed)
                    call nc_dims(path_in,name=name,names=names,dims=dims)
                    ndim  = size(dims)

                    ! Check if interpolation is needed 
                    if (dims1(ndim1) .ne. dims(ndim)) then 

                        ! Get input times
                        if (allocated(tin)) deallocate(tin)
                        allocate(tin(dims(ndim)))
                        call nc_read(path_in,names(ndim),tin,missing_value=mv)

                        ! Get output times 
                        if (allocated(tout)) deallocate(tout)
                        allocate(tout(dims1(ndim1)))
                        call nc_read(path_out,names1(ndim1),tout,missing_value=mv)

                        txt = trim(interp_txt)
                    else 
                        txt = ""
                    end if

                    ! Allocate variable
                    if (allocated(vin1D)) deallocate(vin1D)
                    allocate(vin1D(dims(1)))

                    ! Read in variable 
                    call nc_read(path_in,name,vin1D,missing_value=mv)

                    ! Allocate output variable
                    if (allocated(vout1D)) deallocate(vout1D)
                    allocate(vout1D(dims1(2)))

                    ! Check if interpolation is needed 
                    if (allocated(tout)) then 

                        ! Interpolate to output times
                        vout1D = ens_interp(tin,vin1D,tout,missing_value=mv,method=method)

                    else 
                        ! Pass input variable to output variable
                        vout1D = vin1D

                    end if

                    ! Define current start and count
                    start    = 1
                    start(1) = q 
                    count    = dims1 
                    count(1) = 1 

                    ! Write to ensemble file 
                    select case(trim(precision))
                        case("int")
                            call nc_write(path_out,name,int(vout1D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=int(mv))
                        case("real")
                            call nc_write(path_out,name,real(vout1D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=real(mv))
                        case("double")
                            call nc_write(path_out,name,vout1D,units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=mv)
                        case DEFAULT 
                            write(*,*) "ens_write:: error: output precision must be one of: "// &
                                       "int, real, double. Specified: "//trim(precision)
                            stop 
                    end select 

                    write(*,"(a,a,2x,a,i5,1x,a)") "ens_write:: 1D field written: ", &
                                             trim(path_out), trim(name), q, trim(txt) 
                    
                end do 

            case(2)

                ! Loop over folders and write variable to ensemble file
                do q = 1, nsim  

                    ! Define file for current simulation 
                    path_in = trim(fldrs(q))//"/"//trim(filename)

                    ! Get input dimensions again (in case they changed)
                    call nc_dims(path_in,name=name,names=names,dims=dims)
                    ndim  = size(dims)

                    ! Check if interpolation is needed 
                    if (dims1(ndim1) .ne. dims(ndim)) then 

                        ! Get input times
                        if (allocated(tin)) deallocate(tin)
                        allocate(tin(dims(ndim)))
                        call nc_read(path_in,names(ndim),tin,missing_value=mv)

                        ! Get output times 
                        if (allocated(tout)) deallocate(tout)
                        allocate(tout(dims1(ndim1)))
                        call nc_read(path_out,names1(ndim1),tout,missing_value=mv)

                        txt = trim(interp_txt)
                    else 
                        txt = ""
                    end if

                    ! Allocate variable
                    if (allocated(vin2D)) deallocate(vin2D)
                    allocate(vin2D(dims(1),dims(2)))

                    ! Read in variable 
                    call nc_read(path_in,name,vin2D,missing_value=mv)

                    ! Allocate output variable
                    if (allocated(vout2D)) deallocate(vout2D)
                    allocate(vout2D(dims1(2),dims1(3)))

                    ! Check if interpolation is needed 
                    if (allocated(tout)) then 

                        ! Interpolate to output times
                        vout2D = ens_interp(tin,vin2D,tout,missing_value=mv,method=method)

                    else 
                        ! Pass input variable to output variable
                        vout2D = vin2D

                    end if

                    ! Define current start and count
                    start    = 1
                    start(1) = q 
                    count    = dims1 
                    count(1) = 1 

                    ! Write to ensemble file 
                    select case(trim(precision))
                        case("int")
                            call nc_write(path_out,name,int(vout2D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=int(mv))
                        case("real")
                            call nc_write(path_out,name,real(vout2D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=real(mv))
                        case("double")
                            call nc_write(path_out,name,vout2D,units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=mv)
                        case DEFAULT 
                            write(*,*) "ens_write:: error: output precision must be one of: "// &
                                       "int, real, double. Specified: "//trim(precision)
                            stop 
                    end select 
 
                    write(*,"(a,a,2x,a,i5,1x,a)") "ens_write:: 2D field written: ", &
                                             trim(path_out), trim(name), q, trim(txt) 
                end do 

            case(3)

                ! Loop over folders and write variable to ensemble file
                do q = 1, nsim  

                    ! Define file for current simulation 
                    path_in = trim(fldrs(q))//"/"//trim(filename)

                    ! Get input dimensions again (in case they changed)
                    call nc_dims(path_in,name=name,names=names,dims=dims)
                    ndim  = size(dims)

                    ! Check if interpolation is needed 
                    if (dims1(ndim1) .ne. dims(ndim)) then 

                        ! Get input times
                        if (allocated(tin)) deallocate(tin)
                        allocate(tin(dims(ndim)))
                        call nc_read(path_in,names(ndim),tin,missing_value=mv)

                        ! Get output times 
                        if (allocated(tout)) deallocate(tout)
                        allocate(tout(dims1(ndim1)))
                        call nc_read(path_out,names1(ndim1),tout,missing_value=mv)

                        txt = trim(interp_txt)
                    else 
                        txt = ""
                    end if

                    ! Allocate variable
                    if (allocated(vin3D)) deallocate(vin3D)
                    allocate(vin3D(dims(1),dims(2),dims(3)))

                    ! Read in variable 
                    call nc_read(path_in,name,vin3D,missing_value=mv)

                    ! Allocate output variable
                    if (allocated(vout3D)) deallocate(vout3D)
                    allocate(vout3D(dims1(2),dims1(3),dims1(4)))

                    ! Check if interpolation is needed 
                    if (allocated(tout)) then 

                        ! Interpolate to output times
                        vout3D = ens_interp(tin,vin3D,tout,missing_value=mv,method=method)

                    else 
                        ! Pass input variable to output variable
                        vout3D = vin3D

                    end if

                    ! Define current start and count
                    start    = 1
                    start(1) = q 
                    count    = dims1 
                    count(1) = 1 

                    ! Write to ensemble file 
                    select case(trim(precision))
                        case("int")
                            call nc_write(path_out,name,int(vout3D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=int(mv))
                        case("real")
                            call nc_write(path_out,name,real(vout3D),units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=real(mv))
                        case("double")
                            call nc_write(path_out,name,vout3D,units=var_units,dims=names1, &
                                          start=start,count=count,missing_value=mv)
                        case DEFAULT 
                            write(*,*) "ens_write:: error: output precision must be one of: "// &
                                       "int, real, double. Specified: "//trim(precision)
                            stop 
                    end select  
 
                    write(*,"(a,a,2x,a,i5,1x,a)") "ens_write:: 3D field written: ", &
                                             trim(path_out), trim(name), q, trim(txt) 

                end do 

            case DEFAULT
                write(*,*) "ens_write:: error: number of dimensions not handled."
                write(*,*) "path, name, ndim: ", trim(path_in), trim(name), ndim 
                stop 

        end select 

        return 

    end subroutine ens_write

    subroutine ens_init(ens_fldr,fldrs,filename,names,t,tname,tunits,static)

        ! This subroutine will initialize an ensemble file with the
        ! dimensions `names` + a new dimension 'sim'
        ! time dimension (t,tname,tunits) handled specially

        implicit none 

        character(len=*), intent(IN) :: ens_fldr, fldrs(:), filename
        character(len=*), intent(IN) :: names(:)
        double precision, intent(IN), optional  :: t(:)  
        character(len=32), intent(IN), optional :: tname, tunits
        character(len=*), intent(IN), optional :: static(:)

        double precision, allocatable :: x(:), v1D(:), v2D(:,:), v3D(:,:,:)
        character(len=512) :: xname, xunits 
        
        character(len=512) :: filename_out, path_out, path_in
        integer :: q, nsim, ndim, nd, nx, nd1, nd2, nd3 
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
                else 
                    if (present(tname))  xname  = trim(tname)
                    if (present(tunits)) xunits = trim(tunits)
                end if 
            end if 

!             write(*,"(i2,1x,a,1x,a)") q, trim(xname),trim(xunits)  
   
            call nc_write_dim(path_out,xname,x=x,units=xunits,unlimited=unlim)

        end do 

        ! Add static variables to ensemble file if desired 
        if (present(static)) then 

            ! Drop the last dimension in case time is involved
            if (trim(names(ndim)) .eq. "time") ndim = ndim-1
            
            do q = 1, size(static)

                if (allocated(v1D)) deallocate(v1D)
                if (allocated(v2D)) deallocate(v2D)
                if (allocated(v3D)) deallocate(v3D)

                select case(ndim)
                    case(1)
                        nd1 = nc_size(path_in,names(1))
                        allocate(v1D(nd1))
                        call nc_read(path_in,static(q),v1D)
                        call nc_write(path_out,static(q),v1D,dims=names(1:ndim))

                    case(2)
                        nd1 = nc_size(path_in,names(1))
                        nd2 = nc_size(path_in,names(2))
                        allocate(v2D(nd1,nd2))
                        call nc_read(path_in,static(q),v2D)
                        call nc_write(path_out,static(q),v2D,dims=names(1:ndim))

                    case(3)
                        nd1 = nc_size(path_in,names(1))
                        nd2 = nc_size(path_in,names(2))
                        nd3 = nc_size(path_in,names(3))
                        allocate(v3D(nd1,nd2,nd3))
                        call nc_read(path_in,static(q),v3D)
                        call nc_write(path_out,static(q),v3D,dims=names(1:ndim))

                    case DEFAULT 
                        ! Nothing to do here 

                end select 

                        
            end do 

        end if 

        return 

    end subroutine ens_init

    function ens_interp_1D(x,y,xout,missing_value,method) result(yout)

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

!         ! Eliminate redundant index (GRISLI bug writing two 0 time values)
!         if (x(nx) .eq. x(nx-1)) nx = nx-1 

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

        if (k1-k0 .lt. 1) then 
            write(*,*) "ens_interp:: error: not enough data points are available."
            write(*,*) "Range x:    ", minval(x,mask=x .ne. missing_value), &
                                       maxval(x,mask=x .ne. missing_value)
            write(*,*) "Range xout: ", minval(xout), maxval(xout)                                           
            stop 
        end if 

        ! Find the corresponding indices in the output vector
        l0 = minloc(abs(xout-x(k0)),dim=1)
        l1 = minloc(abs(xout-x(k1)),dim=1)
        
        ! Apply missing values to output vector and then interpolate as desired
        yout = missing_value

        if (l1-l0 .lt. 1) then
            write(*,*) "ens_interp:: warning: simulation has no points to interpolate in range."
            write(*,*) "interp range: ", minval(xout), maxval(xout)
            write(*,*) "sim range:    ", x(k0), x(k1)
            write(*,*) "Indices: l0,l1: ", l0, l1 

        else 
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

        end if 
        
        return 

    end function ens_interp_1D

    function ens_interp_2D(x,y,xout,missing_value,method) result(yout)

        implicit none 

        double precision :: x(:), y(:,:), xout(:)
        double precision :: yout(size(y,1),size(xout,1))
        double precision :: missing_value 
        character(len=*), optional :: method 
        integer :: i

        ! Loop over other dimensions and call 1D interpolation
        do i = 1, size(y,1) 
            yout(i,:) = ens_interp_1D(x,y(i,:),xout,missing_value,method)
        end do 

        return 

    end function ens_interp_2D

    function ens_interp_3D(x,y,xout,missing_value,method) result(yout)

        implicit none 

        double precision :: x(:), y(:,:,:), xout(:)
        double precision :: yout(size(y,1),size(y,2),size(xout,1))
        double precision :: missing_value 
        character(len=*), optional :: method 
        integer :: i, j

        ! Loop over other dimensions and call 1D interpolation
        do i = 1, size(y,1)
        do j = 1, size(y,2) 
            yout(i,j,:) = ens_interp_1D(x,y(i,j,:),xout,missing_value,method)
        end do 
        end do

        return 

    end function ens_interp_3D

    subroutine ens_folders(fldrs,path)
        ! Generate an array of simulation folder names given a base path

        implicit none 

        character(len=*), allocatable :: fldrs(:)
        character(len=*) :: path 
        integer :: k, q, nfldr0, nfldr, iostat
        logical :: dir_e
        character(len=512) :: tmpfldrs(10000) 

        if (allocated(fldrs)) deallocate(fldrs)

        ! Check if the base directory exists
        call system("touch "//trim(path)//"/batch.txt")
        inquire( file=trim(path)//"/batch.txt", exist=dir_e )
        if ( .not. dir_e ) then
            write(*,*) "ens_folders:: error: base path cannot be found: "//trim(path)
            stop 
        end if 

        ! Either use batch file (copy to batch.txt to be safe)
        ! or generate batch.txt from directory listing 
        inquire( file=trim(path)//"/batch", exist=dir_e )
        if ( dir_e ) then
            call system("cp "//trim(path)//"/batch "//trim(path)//"/batch.txt")
        else
!             call system("ls -d "//trim(path)//"/*/ > "//trim(path)//"/batch.txt")
            call system("ls -F "//trim(path)//" | grep / > "//trim(path)//"/batch.txt")
        end if 


        ! Load files from batch.txt file 
        inquire( file=trim(path)//"/batch.txt", exist=dir_e )
        if ( .not. dir_e ) then 
            write(*,*) "ens_folders:: error: batch file not found."
            write(*,*) trim(path)//"/batch.txt"
            stop
        end if 

        ! Open batch.txt file and read folders
        open(unit=22,file=trim(path)//"/batch.txt",status='old',action='read')
        do k = 1, 10000
            read(22,"(a)",iostat=iostat) tmpfldrs(k) 
            if (iostat == -1) exit
        end do 
        close(22)

        ! Store filenames in fldrs vector
        nfldr0 = k-1 
        allocate(fldrs(nfldr0))
        do k = 1, nfldr0
            fldrs(k) = trim(tmpfldrs(k))
        end do 

        ! Eliminate white space folders
        q = 0  
        do k = 1, nfldr0 
            if (trim(tmpfldrs(k)) .ne. "") then ! .and. tmpfldrs(1:1) .ne. "#") then 
                q = q+1 
                fldrs(q) = trim(tmpfldrs(k))
            end if 
        end do 
        nfldr = q 

        ! Eliminate extra path coordinates, add actual path 
        do k = 1, nfldr
            ! Remove the last slash if present
            q = len(trim(fldrs(k)))
            if (fldrs(k)(q:q)=="/") fldrs(k) = trim(fldrs(k)(1:q-1))

            ! Remove previous directory information if present
            q = index(fldrs(k),"/",back=.TRUE.)
            if (q .gt. 0) fldrs(k) = trim(fldrs(k)(q+1:len(fldrs(k))))
            fldrs(k) = trim(path)//"/"//trim(fldrs(k))
        end do 

        ! Reallocate fldrs to match those actually found in the file
        do k = 1, nfldr 
            tmpfldrs(k) = trim(fldrs(k))
        end do 
        deallocate(fldrs)
        allocate(fldrs(nfldr))
        do k = 1, nfldr 
            fldrs(k) = trim(tmpfldrs(k))
        end do 

        ! Print out folders to be loaded
        write(*,"(a)") "ens_folders:: folders to be loaded: "
        do k = 1, nfldr
            write(*,"(a)") trim(fldrs(k))
        end do 
        write(*,*) 

        return 

    end subroutine ens_folders

    subroutine ens_times(time,par,times)

        implicit none 

        double precision, allocatable, intent(INOUT) :: time(:)
        double precision, intent(IN), optional :: par(:) 
        double precision, intent(IN), optional :: times(:)
        double precision, allocatable :: tmp(:)
        integer :: n, ntot, ntot_par, nt, q, know, k, jnow  
        double precision :: pnow(3)

        ! Make sure time vector is not allocated yet
        if (allocated(time)) deallocate(time)

        if (present(par)) then 
            ! Generate times via parameter options

            ! Make sure input parameters have right length
            n = size(par)/3 
            if (mod(size(par),3) /= 0) then 
                write(*,*) "ens_times:: error: time parameters must be a set of three: "
                write(*,*) "start_time, end_time, dt"
                write(*,"(a,30f12.3)") "par: ", par
                stop 
            end if 

            ! Determine how long the time vector will be based
            ! on input parameters
            ntot_par = 0 
            do q = 1, n 
                pnow = par(1+3*(q-1):3*q)
                ntot_par = ntot_par + (pnow(2)-pnow(1))/pnow(3) + 1
            end do 

        else 
            ! No times from par 

            ntot_par = 0 

        end if 

        ! Add room for specific output times too
        ntot = 0 
        if (present(par))   ntot = ntot + ntot_par 
        if (present(times)) ntot = ntot + size(times)
        
        ! Check to make sure output is not excessive!
        if ( ntot .gt. 5000 ) then
            write(*,*) "ens_times:: Too much output desired!! Try again."
            write(*,"(a,i12)")     "ntot: ", ntot
            write(*,"(a,30f12.3)") " par: ", par
            stop
        end if

        if ( ntot .eq. 0 ) then 
            write(*,*) "ens_times:: No output times returned for the current parameters!! Try again."
            write(*,*) "ens_times:: Too much output desired!! Try again."
            write(*,"(a,i12)")     "ntot: ", ntot
            write(*,"(a,30f12.3)") " par: ", par
            stop
        end if

        ! Allocate time vector
        allocate(time(ntot))

        ! Add in specific times if available
        know = 0 
        if (present(times)) then 
            nt = size(times)
            time(1:nt) = times 
            know = nt
        end if 
        
        if (present(par)) then 
            ! Loop over each section and generate additional times
            jnow = 1
            do q = 1, n 
                pnow = par(1+3*(q-1):3*q)

                nt = (pnow(2)-pnow(1))/pnow(3) + 1
                do k = 1, nt 
                    know = know+1
                    time(know) = pnow(1) + pnow(3)*(k-1) 
                end do 
            end do 

        end if 

        ! Eliminate duplicates
        call unique(time)

        ! Sort the times to make sure they are in the right order
!         call quicksort(time)
        call QsortC(time)

        write(*,*) "Output times:"
        write(*,"(500f12.2)") time 
        write(*,*) 

        return 

    end subroutine ens_times 

    subroutine unique(x)

        implicit none 

        double precision, allocatable, intent(INOUT) :: x(:)
        double precision, allocatable :: tmp(:)
        integer :: n0, n, k, j  
        logical :: add 
        double precision, parameter :: tol = 1d-5

        n0 = size(x)
        allocate(tmp(n0))
        tmp = -99999999.d0 

        ! Store original data in temporary array avoiding duplicates 
        tmp(1) = x(1)
        n = 1 
        do k = 2, n0
            add = .TRUE. 
            do j = 1, n
                if (abs(x(k)-tmp(j)) .le. tol) add = .FALSE.
            end do 
            if (add) then 
                n = n+1
                tmp(n) = x(k)
            end if 
        end do 

        ! Reallocate and save data to output array 
        deallocate(x)
        allocate(x(n))
        x = tmp(1:n)

        return 

    end subroutine unique 

    subroutine quicksort(x)
        ! Custom routine, should use a real sorting algorithm

        implicit none 

        double precision, intent(INOUT) :: x(:)
        double precision, allocatable :: tmp(:)
        integer :: n, k, j  

        n = size(x)
        allocate(tmp(n))
        tmp = -99999999.d0 

        write(*,*) "ensembles:: IMPLEMENT SORT!"
!         stop 

!         do k = 1,n 
!             tmp(k) = 

!         return 

    end subroutine quicksort 


    recursive subroutine QsortC(A)
        ! Modified from source found here: http://www.fortran.com/qsort_c.f95
        ! Recursive Fortran 95 quicksort routine
        ! sorts real numbers into ascending numerical order
        ! Author: Juli Rew, SCD Consulting (juliana@ucar.edu), 9/03
        ! Based on algorithm from Cormen et al., Introduction to Algorithms,
        ! 1997 printing

        ! Made F conformant by Walt Brainerd

        double precision, intent(in out), dimension(:) :: A
        integer :: iq

        if(size(A) > 1) then
            call Partition(A, iq)
            call QsortC(A(:iq-1))
            call QsortC(A(iq:))
        endif

        return 

    end subroutine QsortC

subroutine Partition(A, marker)
  double precision, intent(in out), dimension(:) :: A
  integer, intent(out) :: marker
  integer :: i, j
  double precision :: temp
  double precision :: x      ! pivot point
  x = A(1)
  i= 0
  j= size(A) + 1

  do
     j = j-1
     do
        if (A(j) <= x) exit
        j = j-1
     end do
     i = i+1
     do
        if (A(i) >= x) exit
        i = i+1
     end do
     if (i < j) then
        ! exchange A(i) and A(j)
        temp = A(i)
        A(i) = A(j)
        A(j) = temp
     elseif (i == j) then
        marker = i+1
        return
     else
        marker = i
        return
     endif
  end do

end subroutine Partition

end module ensembles 


