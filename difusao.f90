PROGRAM difusao

! Este programa resolve a equacao de difusao unidimensional pelo metodo
! avancado tempo e centrado no espaco: 01/04/2014

   IMPLICIT NONE
   
   INTEGER, PARAMETER         :: nstop=500, imax=15
   INTEGER                    :: i,j,m,jj
   REAL, DIMENSION(15)        :: t,told
   REAL                       :: coef,alpha,pi,dt,dx

   OPEN(1,FILE='saida_ftcs.dat',STATUS='unknown') 

  m=1
  dx = 2.0
  dt = 0.2
  pi = 4.*DATAN(1.D0)
  alpha = 0.5*pi**2.0
  coef = alpha*dt/dx**2.0
  WRITE(*,*)coef


  DO i=1,imax

     IF(i.LE.imax/2.+1)THEN
        told(i)=i
     ELSE
        told(i)=i-2*m
        m=m+1
     ENDIF
    
  END DO
  WRITE(1,*)(told(jj),jj=1,imax)

  DO j=1,nstop


    DO i = 2,imax-1
       t(i) = told(i) + coef*(told(i+1)-2.0*told(i)+told(i-1))
    END DO

    t(1)=t(14)
    t(15)=t(2)

 
   DO i = 1,15
      told(i) = t(i)
   END DO
   WRITE(1,*)(t(i),i=1,15)


 END DO


END PROGRAM difusao
