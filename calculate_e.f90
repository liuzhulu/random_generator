!独立性检验模块
module calculate_e_module 
    implicit none
    public
    contains
    subroutine calculate_e(u,x,e)
        integer::u,i,j,k
        real :: n_x  !奇数位随机数小于x的组数
        real :: n_y  !偶数位随机数小于y的组数
        real :: n_xy !奇数位随机数小于x，偶数位随机数小于y的组数
        real(8)::x(0:u)
        real :: std
        real :: e,e_0 !独立性检验结果

        e_0 = 0
          do j = 1,1000
            do k = 1,1000
                n_x = 0
                n_y = 0
                n_xy = 0
                do i = 0,u-1,2
                    if (x(i)<(real(j)/1000))then
                        n_x = n_x+1
                    endif

                    if(x(i+1)<(real(k)/1000))then
                        n_y = n_y+1
                    endif

                    if(x(i+1)<(real(k)/1000).and.x(i)<(real(j)/1000))then
                        n_xy = n_xy+1
                    endif

                enddo
                std = abs(2*n_xy/real(u)-2*n_x/real(u)*2*n_y/real(u))
                if(std>e_0) e_0 = std
            enddo
          enddo
          e = e_0
    endsubroutine
endmodule