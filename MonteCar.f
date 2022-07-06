      subroutine MonteCar(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, 
     5       T, 
     1       E, M, 
     2       Et, Mt,    
     2       ijatom ,
     6       nMC, ntimes, ntime_p ) 
           
       integer lattype, L, L_p, nnnumber, nnnum_p
       integer idx(nnnum_p), jdy(nnnum_p)       
       real*8  V2, V1, erko, T, E, dE, Et( ntime_p )
       integer M, Mt( ntime_p )
       integer ijatom(0:L_p+1,0:L_p+1)
       integer nMC, ntimes, ntime_p 
       integer iMC, itime
      
       integer i,j
       real*8  r, beta
       real*8  random, dEnergy
       integer nreject,nacrais,naclower
       
       nattemp=0
       nreject=0
       nacrais=0
       naclower=0
       beta=1.0/T
       
       do 101 itime=1,ntimes
        do 102 iMC=1, nMC 
        
cc      Periodic Boundary: ijatom
c    (1,1)=(1,L+1)=(L+1,1)=(L+1,L+1) 

         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,     
     5       ijatom,1,1  )   
         if(dE .le. 0)then
             ijatom(1,1)=-ijatom(1,1)
             ijatom(1,L+1)=-ijatom(1,L+1)
             ijatom(L+1,1)=-ijatom(L+1,1)
             ijatom(L+1,L+1)=-ijatom(L+1,L+1)
             M=M+2*ijatom(1,1) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(1,1)=-ijatom(1,1)
             ijatom(1,L+1)=-ijatom(1,L+1)
             ijatom(L+1,1)=-ijatom(L+1,1)
             ijatom(L+1,L+1)=-ijatom(L+1,L+1)             
             M=M+2*ijatom(1,1) 
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
 
c    (1,L)=(1,0)=(L+1,0)=(L+1,L)   

         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,     
     5       ijatom,1,L  )   
         if(dE .le. 0)then
             ijatom(1,L)=-ijatom(1,L)
             ijatom(1,0)=-ijatom(1,0)
             ijatom(L+1,0)=-ijatom(L+1,0)
             ijatom(L+1,L)=-ijatom(L+1,L)
             M=M+2*ijatom(1,L) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(1,L)=-ijatom(1,L)
             ijatom(1,0)=-ijatom(1,0)
             ijatom(L+1,0)=-ijatom(L+1,0)
             ijatom(L+1,L)=-ijatom(L+1,L)             
             M=M+2*ijatom(1,L) 
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
         
c    (L,1)=(0,1)=(0,L+1)=(L,L+1) 

         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,      
     5       ijatom,L,1  )   
         if(dE .le. 0)then
             ijatom(L,1)=-ijatom(L,1)
             ijatom(0,1)=-ijatom(0,1)
             ijatom(0,L+1)=-ijatom(0,L+1)
             ijatom(L,L+1)=-ijatom(L,L+1)
             M=M+2*ijatom(L,1) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(L,1)=-ijatom(L,1)
             ijatom(0,1)=-ijatom(0,1)
             ijatom(0,L+1)=-ijatom(0,L+1)
             ijatom(L,L+1)=-ijatom(L,L+1)
             M=M+2*ijatom(L,1) 
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
    
c    (L,L)=(0,0)=(0,L)=(L,0)

         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,      
     5       ijatom,L,L  )   
         if(dE .le. 0)then
             ijatom(L,L)=-ijatom(L,L)
             ijatom(0,0)=-ijatom(0,0)
             ijatom(0,L)=-ijatom(0,L)
             ijatom(L,0)=-ijatom(L,0)
             M=M+2*ijatom(L,L) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(L,L)=-ijatom(L,L)
             ijatom(0,0)=-ijatom(0,0)
             ijatom(0,L)=-ijatom(0,L)
             ijatom(L,0)=-ijatom(L,0)
             M=M+2*ijatom(L,L) 
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
   
c    (1,j)=(L+1,j)
       do 5 j=2,L-1 

         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,      
     5       ijatom,1,j  )   
         if(dE .le. 0)then
             ijatom(1,j)=-ijatom(1,j)
             ijatom(L+1,j)=-ijatom(L+1,j)             
             M=M+2*ijatom(L+1,j) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(1,j)=-ijatom(1,j)
             ijatom(L+1,j)=-ijatom(L+1,j)             
             M=M+2*ijatom(L+1,j) 
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
              
5      continue
c    (L,j)=(0,j)         
       do 6 j=2,L-1 

         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,     
     5       ijatom,L,j  )   
         if(dE .le. 0)then
             ijatom(L,j)=-ijatom(L,j)
             ijatom(0,j)=-ijatom(0,j)             
             M=M+2*ijatom(L,j) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(L,j)=-ijatom(L,j)
             ijatom(0,j)=-ijatom(0,j)             
             M=M+2*ijatom(L,j)             
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
        
6      continue 
c    (i,1)=(i,L+1)
       do 7 i=2,L-1 
        
         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,     
     5       ijatom,i,1  )   
         if(dE .le. 0)then
             ijatom(i,1)=-ijatom(i,1)
             ijatom(i,L+1)=-ijatom(i,L+1)             
             M=M+2*ijatom(i,1) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(i,1)=-ijatom(i,1)
             ijatom(i,L+1)=-ijatom(i,L+1)             
             M=M+2*ijatom(i,1) 
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
                
7      continue 
c    (i,L)=(i,0)
       do 8 i=2,L-1 

         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,      
     5       ijatom,i,L  )   
         if(dE .le. 0)then
             ijatom(i,L)=-ijatom(i,L)
             ijatom(i,0)=-ijatom(i,0)             
             M=M+2*ijatom(i,L) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(i,L)=-ijatom(i,L)
             ijatom(i,0)=-ijatom(i,0)             
             M=M+2*ijatom(i,L) 
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
               
8      continue 
cc   (i,j):   ijatom(i,j) = 1 or -1
       do 91 i=2,L-1
        do 92 j=2,L-1 

         dE=dEnergy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1, erko, T,     
     5       ijatom,i,j  )   
         if(dE .le. 0)then
             ijatom(i,j)=-ijatom(i,j)                      
             M=M+2*ijatom(i,j) 
             E=E+dE
             naclower=naclower+1
         else if( random() .le. exp(-beta*dE) )then
             ijatom(i,j)=-ijatom(i,j)                      
             M=M+2*ijatom(i,j)               
             E=E+dE
             nacrais=nacrais+1 
         else     
             nreject=nreject+1     
         endif         
                   
92      continue
91     continue
                
102    continue

       Et(itime)=E
       Mt(itime)=M

101   continue 
     
c       write(34,1134) T,nacrais,naclower,nreject
c 1134   format(1X, F18.9, 3I12)       
      
c      WRITE(*,*) 'dEnergy for this cycle is',dE
      end
