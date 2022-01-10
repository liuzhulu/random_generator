! date:2021.11.14
! developer:guojiandi
! introduction:a random number generator contains two methods:
! the first is multiply congruence method;
! the second is square the middle method.

program main
    use calculate_delta_module
    use calculate_e_module
    implicit none 
    integer :: meth !the figure of method
    integer :: num  !the number of random numbers
    integer :: x_0  !users input number
    real(8),dimension(:),allocatable :: x , y

    write(*,*)"Developer:guojiandi"
    write(*,*)"Introduction:a random number generator contains two methods:"
    write(*,*)"The first is multiply congruence method, the second is square the middle method."
    write(*,*)"Please choose method:1/2?"

    read(*,*)meth 
    if(meth == 1) then !choose the first method
        write(*,*)"please input the first random figure"
        read(*,*)x_0
        open(3,file = "data1.txt")
        do num=100,10000,100
           allocate(x(0:num))
           allocate(y(0:num))
           call multiply_congruence(num,x_0,x,y)   !调用乘同余法
           deallocate(x)
           deallocate(y)
        enddo
        close(3)

    elseif (meth == 2)then !choose the second method
        write(*,*)"please input the first random eight-digit number: "
        read(*,*)x_0
        open(4,file = "data2.txt")
        do num=100,10000,100
           allocate(x(0:num))
           allocate(y(0:num))
           call square_the_middle(num,x_0,x,y)    !调用平方取中法
           deallocate(x)
           deallocate(y)
        enddo
        close(4)
    else
        write(*,*)"error:please choose the right method."
    endif
    
contains
!乘同余法生成随机数
    subroutine multiply_congruence(u,v,arry_1,arry_2)
        integer::u,v,i
        real(8)::arry_1(0:u)
        real(8)::arry_2(0:u)
        real :: delta  !均匀性检验结果
        real :: e   !独立性检验结果

        arry_1(0) = v
        arry_2(0) = arry_1(0) / (2.**31-1)
         do i=1,u
            arry_1(i) = mod (1103515245*arry_1(i-1)+12345, (2.**31-1))
            arry_2(i) = arry_1(i) / (2.0**31-1)
         enddo
         if(u==10000)then
            open(1,file='random_1.txt')
            do i=1,u
                write(1,*)arry_2(i)
            enddo
            close(1)
         endif
        call calculate_delta(u,arry_2,delta)  !调用均匀性检验函数
        call calculate_e(u,arry_2,e)   !调用独立性检验函数
        write(3,*)u,delta,e
        write(*,*)u,delta,e
    endsubroutine
!平方取中法生成随机数
    subroutine square_the_middle(u,v,arry_1,arry_2)
        integer::u,v,i
        real(8)::arry_1(0:u)
        real(8)::arry_2(0:u)
        real :: std
        real :: delta  !均匀性检验结果
        real :: e  !独立性检验结果

        arry_1(0) = v
        arry_2(0) = arry_1(0) / 1e8
         do i=1,u
            arry_1(i) = mod (real(int(arry_1(i-1)))*arry_1(i-1)/1e4,1.0e8)
            arry_2(i) = arry_1(i) / (1e8)
            ! write(*,*)arry_2(i)
         enddo
         if(u==10000)then
            open(2,file='random_2.txt')
            do i=1,u
                write(2,*)arry_2(i)
            enddo
            close(2)
         endif
 
         call calculate_delta(u,arry_2,delta)     !调用均匀性检验函数
         call calculate_e(u,arry_2,e)       !调用独立性检验函数
         write(4,*)u,delta,e
         write(*,*)u,delta,e
    endsubroutine

end program