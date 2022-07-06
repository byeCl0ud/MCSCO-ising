      real*8 FUNCTION random()
cccccc______ Generates random numbers 0<r<1 ____________cccccc 
cc  multipl=524287 => Period=max=2**31-2 , rinit_int=any int.

        real*8 scope, multipl, scopinv, rinit_int
        parameter(   scope=2.0d0**31-1.0d0,
     2               scopinv=1.0/scope ,
     3               multipl=524287.0d0 ,
     4               rinit_int=1321.0d0  
     6                                 )
c        real*8 r_int/rinit_int/
        real*8 r_int
        common /rand/r_int
c        SAVE r_int        
        
      if (r_int.eq.0.d0) then r_int = rinit_int
      r_int=dmod(r_int*multipl, scope)
      random=r_int*scopinv
      return
      end
