      real*8 function Energy(
     1       lattype, L, L_p,       
     2       nnnumber, nnnum_p,       
     3       idx, jdy,
     4       V2, V1,      
     5       ijatom,
     6       M  ) 
       integer lattype, L, L_p,nnnumber, nnnum_p
       integer idx(nnnum_p), jdy(nnnum_p) 
       real*8  V2, V1, E2, E1
       integer iE1, iE2, inE2
       integer ijatom(0:L_p+1,0:L_p+1), iatom
       integer M
       integer i,j,k,i2,j2
       
       
       iE1=0       
         do 21 i=1,L
          do 22 j=1,L     
           iE1=iE1+ijatom(i,j)
22        continue
21       continue  
       M=iE1       
       E1=V1*iE1
       
       iE2=0
       do 31 i=1,L
        do 32 j=1,L
         inE2=0 
         do 33 k=1,nnnumber
           i2=i+idx(k)
           j2=j+jdy(k)
           inE2=inE2+ijatom(i2,j2)
33       continue           
         iE2=iE2+ijatom(i,j)*inE2 
32      continue
31     continue  
       E2=V2*iE2
       Energy=E1+0.5*E2
       WRITE(*,*) 'Energy is',Energy
      end
