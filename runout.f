      subroutine runout(
     1   output,
     1   lattype, L, V2, V1, erko,
     2   Tinit, Tfinal, nTsteps, 
     4   nMC, ntimes, q, 
     3   ConcInit )
     
c
c    Calculates everything and writes
c    output into output file.
c     
       character output*128

       integer lattype, L
       real*8  V2, V1, erko
       real*8  Tinit, Tfinal, dT
       integer nTsteps, q, iTstep
       real*8  ConcInit
       
       integer L_p, nTsteps_p, nlattyp_p, ntime_p
       parameter( 
     1   L_p=128,
     2   nTsteps_p=500,
     3   nlattyp_p=8,
     4   ntime_p=2000)   
       
       real*8  Einst( nTsteps_p, ntime_p )
       integer Minst( nTsteps_p, ntime_p )
       real*8  rMinst( ntime_p )
       real*8  Et( ntime_p )
       integer Mt( ntime_p )
       real*8  Eav( nTsteps_p ), dEav( nTsteps_p )
       real*8  E2av( nTsteps_p ), CvT( nTsteps_p )
       real*8  avM( nTsteps_p ), dMav( nTsteps_p ) 
       real*8  Eavg, avgM, dEavg, avgdM, E2avg, Cv

c Hysteresis-leading variables
       real*8 ni, ntt, leadT 
c  Internal variables:       
       integer itime, M, i,j
       real*8  T, E
cc nncoords:
       integer nnnumber, nnnum_p
       parameter( nnnum_p=8 ) 
       integer idx(nnnum_p), jdy(nnnum_p) 
cc latinit:
       integer ijatom(0:L_p+1,0:L_p+1) 
c  functions:
       real*8  Energy, dEnergy            
       
c  MEMORY CHECK
       if(L       .gt. L_p)  then
           write(*,*) "  L gt L_MAX: reset L .le.", L_p
       endif
       if(nTsteps .gt. nTsteps_p) then
           write(*,*) "  nTsteps gt nTsteps_p: reset .le.", nTsteps_p
       endif

       
       call nncoords(
     1       lattype, 
     2       nnnumber, nnnum_p,
     3       idx, jdy )
       
       call latinit(
     1       lattype, L, L_p,
     2       ConcInit,
     1       ijatom  )  
       
       E = Energy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1,      
     5       ijatom,
     6       M  )       

c       dirty implementation of hysteresis 
c       idk what I'm doing
c
c       ni is number of T intervals in one way
        ni=nint(float(nTsteps-q)/float(q))
c       
c       dT is temperature differential        
        dT=(Tfinal-Tinit)/ni

c       T is starting temperature
        T=Tinit-dT

c       temperature loop filter
        if (q.eq.1) then
          ntt=1
        else
          ntt= 2 - nint(float(nTsteps)/2)*2 + nTsteps
        endif

c       hacks with leadT for leading hysteresis
c       **really cool stuff, took me 2 hours to
c       figure it out from sco_curve**
        leadT = dabs((Tfinal-Tinit)+dT+dT*ntt/2)


       open(UNIT=35, FILE='output', status='UNKNOWN')
       
       WRITE(*,*) 'E=',E,' V1=',V1,' V2=',V2
       WRITE(*,*) 'Here we go, running the calculation...' 
       WRITE(*,*) 'To preview progress of the calculation,'
       WRITE(*,*) 'run "tail -f output" in another terminal'
c
c    Loop over Tempetature
c       
      do 100 iTstep=1, nTsteps
       leadT = leadT - dabs(dT)
       T = T + dT*(nint(leadT/dabs(leadT-1.d-3)))**(3-q)

c       this needs explanation, like seriously
c
c       if its one-directional SCO, dT is always positive
c       thanks to ^2
c       if its bi-directional SCO, dT is negative after
c       half of the steps 
c       if nTsteps is odd, it crosses Tfinal once and 
c       then go backwards, else we reach Tfinal twice,
c       thanks to 1.d-3, we avoid division by zero

       call MonteCar(
     1       lattype, L, L_p,  
     2       nnnumber, nnnum_p,
     3       idx, jdy,
     4       V2, V1, erko,
     5       T,  
     1       E, M,
     2       Et, Mt,
     2       ijatom ,
     6       nMC, ntimes, ntime_p )  
                    
       Eavg=0
       E2avg=0
       avgM=0
       Cv=0
       do 111 itime=1,ntimes
         Einst(iTstep,itime)=Et(itime)
         Minst(iTstep,itime)=Mt(itime) 
         rMinst(itime)=dfloat(Mt(itime))/dfloat(L*L)       
         Eavg=Eavg+Et(itime)
         E2avg=E2avg+Et(itime)*Et(itime)
         avgM=avgM+rMinst(itime)
111    continue 
       Eavg = Eavg/dfloat(ntimes)
       E2avg = E2avg/dfloat(ntimes)
       avgM = avgM/dfloat(ntimes)
       avM( iTstep ) = avgM         
       
       dEavg=0
       avgdM=0
       do 112 itime=1,ntimes
         dEavg = dEavg + (Et(itime)-Eavg)**2
         avgdM = avgdM + (rMinst(itime)-avgM)**2
112    continue 
       dEavg = dEavg/dfloat(ntimes*(ntimes-1))
       avgdM = avgdM/dfloat(ntimes*(ntimes-1))
       dEavg = sqrt( dEavg ) 
       avgdM = sqrt( avgdM )
       dMav( iTstep ) = avgdM
       Eav( iTstep ) = Eavg/dfloat(L*L)  
       E2av( iTstep ) = E2avg/dfloat(L*L*L*L)    
       dEav( iTstep ) = dEavg/dfloat(L*L) 
       Cv = ((E2avg - Eavg*Eavg)/(T*T))/dfloat(L*L*L*L)
       CvT(iTstep)=(E2av(iTstep)- Eav(iTstep)**2)/(T*T)
       
       write(35,1135) T, Eav(iTstep), avM( iTstep )
1135   format(1X,F15.9,';',F18.9,'; ',F12.9)
           
           
100   continue       

      close(35)

      return
      end
