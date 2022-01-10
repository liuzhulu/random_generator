!均匀性检验模块
module calculate_delta_module
    implicit none
    public
    contains
    subroutine calculate_delta(u,x,delta)
        integer::u,v,i,j
        real :: n    !随机数小于x的个数
        real(8)::x(0:u)
        real :: std
        real :: delta,delta_0  !均匀性检验结果

        delta_0 = 0
          do j=1,1000
            n = 0
            do i = 1,u
              if(x(i)<(real(j)/1000))then
                n = n+1
              endif
            enddo
            std = abs((n/real(u))-(real(j)/1000))
            if (std>delta_0) delta_0 = std
          enddo
      delta = delta_0
    endsubroutine
endmodule