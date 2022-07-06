      program MC2DIM
c
c Core part of Monte Carlo simulation of Ising-like SCO model
c *** M. Blasko et al., 2022 ***
c

       implicit none


       character IN1FILE*128
       character output*128

       parameter(
     1   IN1FILE='in.2d' ,
     1   output='output.txt' )


       integer lattype, L
       real*8  V2, V1, erko
       real*8  Tinit, Tfinal
       integer nTsteps, q, nMC, ntimes
       real*8  ConcInit
       parameter( ConcInit=0.5 )

       WRITE(*,*) 'Welcome to SCO modeling with 2-dim Monte Carlo'

       call readinpt(
     1         IN1FILE,
     2         lattype, L, V2, V1, erko,
     3         Tinit, Tfinal, nTsteps,
     4         nMC, ntimes, q  )

       WRITE(*,*) 'Input is okay, or at least I hope...'

       call runout(
     1   output,
     1   lattype, L, V2, V1, erko,
     2   Tinit, Tfinal, nTsteps,
     4   nMC, ntimes, q,
     3   ConcInit  )

       WRITE(*,*) 'Enjoy your results, I am off now...'

      end
