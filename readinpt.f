      subroutine readinpt(  
     1         IN1FILE,
     2         lattype, L, V2, V1, erko,
     3         Tinit, Tfinal, nTsteps,
     4         nMC, ntimes, q )
c
c    Reads input from file IN1FILE 
c       
       character IN1FILE*128       
       integer L,lattype 
       real*8  V2, V1, erko
       integer nTsteps, q
       real*8  Tinit, Tfinal
                    
       open(UNIT=11, FILE=IN1FILE, 
     ^      ACCESS='SEQUENTIAL',
     ^      STATUS='OLD' )  
         
         read(11,*) lattype, L, V2, V1, erko
         read(11,*) Tinit, Tfinal, nTsteps
         read(11,*) ntimes,iseed
         read(11,*) q
       close(11) 
      
      erko=log(erko)

        call random_initial(iseed)
       nMC=L+2            ! no. of steps before sampling.
c       ntimes=125         ! no. of times sampled for avg.
      return
      end
