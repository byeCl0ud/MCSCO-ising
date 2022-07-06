      integer function nexttype(ConcInit)
       real*8 ConcInit, random
       if(ConcInit .le. random() )then
         nexttype=-1
       else
         nexttype=1
       endif
       return
      end
