      subroutine latinit(
     1       lattype, L, L_p,
     2       ConcInit,
     1       ijatom  ) 
c
c    Initialises lattice and buffer layer.
c      ijatom(i,j) = 1 or -1
c
       implicit none 
       integer lattype, L, L_p
       real*8  ConcInit      
       integer ijatom(0:L_p+1,0:L_p+1)
       integer itype 
       integer i,j 
       integer nexttype
       
cc   Periodic Boundary: ijatom
c    (1,1)=(1,L+1)=(L+1,1)=(L+1,L+1)           
       itype=nexttype(ConcInit)
       ijatom(1,1)=itype
       ijatom(1,L+1)=itype
       ijatom(L+1,1)=itype
       ijatom(L+1,L+1)=itype
c    (1,L)=(1,0)=(L+1,0)=(L+1,L)       
       itype=nexttype(ConcInit)    
       ijatom(1,L)=itype
       ijatom(1,0)=itype
       ijatom(L+1,0)=itype
       ijatom(L+1,L)=itype
c    (L,1)=(0,1)=(0,L+1)=(L,L+1)
       itype=nexttype(ConcInit)      
       ijatom(L,1)=itype
       ijatom(0,1)=itype
       ijatom(0,L+1)=itype
       ijatom(L,L+1)=itype
c    (L,L)=(0,0)=(0,L)=(L,0)
       itype=nexttype(ConcInit)
       ijatom(L,L)=itype
       ijatom(0,0)=itype
       ijatom(0,L)=itype
       ijatom(L,0)=itype
c    (1,j)=(L+1,j)
       do 5 j=2,L-1 
         itype=nexttype(ConcInit)
         ijatom(1,j)=itype
         ijatom(L+1,j)=itype
5      continue
c    (L,j)=(0,j)         
       do 6 j=2,L-1 
         itype=nexttype(ConcInit)
         ijatom(L,j)=itype
         ijatom(0,j)=itype
6      continue 
c    (i,1)=(i,L+1)
       do 7 i=2,L-1 
         itype=nexttype(ConcInit)
         ijatom(i,1)=itype
         ijatom(i,L+1)=itype
7      continue 
c    (i,L)=(i,0)
       do 8 i=2,L-1 
         itype=nexttype(ConcInit)
         ijatom(i,L)=itype
         ijatom(i,0)=itype
8      continue 
cc   (i,j):   ijatom(i,j) = 1 or -1
       do 91 i=2,L-1
        do 92 j=2,L-1
         ijatom(i,j)=nexttype(ConcInit)          
92      continue
91     continue
       return
      end
