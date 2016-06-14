      
C.**************************************************************************
C     SUBROUTINE FOR THE MOVEMENTS OF THE PARTICLES IN THE CRYSTAL
C.**************************************************************************
      SUBROUTINE CRYST(IS,x,xp,y,yp,PC,Length)
c
C     Simple tranport protons in crystal 2
C-----------------------------------------------------------C
C      J -   number of element                              C
C      S - longitudinal coordinate      
C      IS -   number of substance 1-4: Si,W,C,Ge(110)       C
C      x,xp,y,yp - coordinates at input of crystal          C
C      PC -   momentum of particle*c [GeV]                  C
C      W  -   weigth of particle                            C
C-----------------------------------------------------------C
C
C
      IMPLICIT none
c     
C
      double precision Rcurv,Length,C_xmax,C_ymax !crystal geometrical parameters
                                                  ! [m],[m],[m],[m],[rad]
      double precision ymax,ymin       !crystal geometrical parameters 
      double precision s_length             !element length along s
      double precision Alayer               !amorphous layer [m]
      double precision C_orient                      !crystal orientation 
      integer IS                            !index of the material
c      integer counter 
      double precision  DLRI(4),DLYI(4),AI(4),DES(4)!cry parameters:see line~270
      integer NAM,ZN                        !switch on/off the nuclear 
                                            !interaction (NAM) and the MCS (ZN)
      double precision x,xp,y,yp,PC         !coordinates of the particle 
                                            ![m],[rad],[m],[rad],[GeV]
      double precision x0,y0                !coordinates of the particle [m]
      double precision s                    !long coordinates of the particle [m]
      double precision a_eq,b_eq,c_eq,Delta !second order equation param.
      double precision Ang_rms, Ang_avr     !Volume reflection mean angle [rad]
      double precision c_v1                 !fitting coefficient 
      double precision c_v2                 !fitting coefficient 
      double precision Dechan               !probability for dechanneling 
      double precision Lrefl, Srefl         !distance of the reflection point [m] 
      double precision Vcapt                !volume capture probability
      double precision Chann                !channeling probability
      double precision N_atom               !probability for entering 
                                            !channeling near atomic planes
      double precision Dxp                  !variation in angle
      double precision xpcrit               !critical angle for curved crystal[rad]
      double precision xpcrit0              !critical angle for str. crystal [rad]
      double precision Rcrit                !critical curvature radius [m]
      double precision ratio                !x=Rcurv/Rcrit
      double precision Cry_length
      double precision TLdech2              !tipical dechanneling length(1) [m]
      double precision TLdech1              !tipical dechanneling length(2) [m]
      double precision tdech, Ldech,Sdech   !angle, lenght, and S coordinate 
                                            !of dechanneling point  
      double precision Rlength, Red_S       !reduced length/s coordinate 
                                            !(in case of dechanneling)
      double precision Am_length            !Amorphous length 
      double precision Length_xs, Length_ys !Amorphous length 
      double precision miscut               !miscut angle in rad
      double precision L_chan, tchan
      double precision xp_rel               !xp-miscut angle in mrad
      REAL*4 RNDM                           !random numbers
      REAL*4      rndm4
      REAL*4      RAN_GAUSS
      double precision eUm(4)                !maximum potential
      CHARACTER*50 PROC        !string that contains the physical process 
      common /Par_Cry1/ Cry_length, Rcurv,C_xmax,C_ymax,Alayer,C_orient
      common /miscut/ miscut
      common /Proc2/PROC
      COMMON/NPC/     NAM,ZN                 
      COMMON/CRYS/    DLRI,DLYI,AI,DES       
      COMMON/eUc/     eUm  !
c      common/utils/ counter
C
c
      NAM=1 !switch on/off the nuclear interaction (NAM) and the MCS (ZN)
      ZN=1
c      miscut=0.001000
c 
c      write(*,*)"last miscut angle =",miscut
c      write(*,*) 'enter crystal subroutine'
c      write(*,*) 'particle energy Gev :', PC
c      write(*,*) 'x_initial :', x
c      write(*,*) 'Length [m]:', Length
c      write(*,*) 'Random:', rndm4()
c      write(*,*)'xp',xp,'x',x , 's', s
      s=0
      s_length=Rcurv*(sin(length/Rcurv)) !
      L_chan=length

      if ( miscut .lt. 0 
     &     .and. x .gt. 0 !should be useless 
     &     .and. x .lt. -length*tan(miscut)) then
            L_chan=-x/sin(miscut)
      endif
      tchan=L_chan/Rcurv
      xp_rel=xp-miscut
c  FIRST CASE: p don't interact with crystal
      ymin = - C_ymax/2 
      ymax =  C_ymax/2
      IF (y.LT.ymin .or. y.GT.ymax .or. x.gt.C_xmax) THEN
        x = x+xp*s_length
        y = y+yp*s_length
        PROC='out'
        GOTO 111
c SECOND CASE: p hits the amorphous layer        
      ELSEIF ( (x.LT.Alayer) .or.  ((y-ymin).LT.Alayer) .or. 
     1  ((ymax-y).lt.Alayer)  ) THEN
        x0=x
        y0=y
        a_eq=(1+(xp)**2)
        b_eq=(2*(x)*(xp)-2*(xp)*Rcurv)
        c_eq=(x)**2-2*(x)*Rcurv
        Delta=b_eq**2-4*a_eq*c_eq
        s=((-b_eq+sqrt(Delta))/(2*a_eq)) 
        if (s .ge. s_length) s=s_length
        x=(xp)*s+x0
        Length_xs=sqrt((x-x0)**2+s**2)
        if ( (yp .ge.0 .and. (y+yp*s).le.ymax)) then
          Length_ys = yp*Length_xs
        elseif (yp.lt.0 .and. (y+yp*s).ge. ymin) then
          Length_ys = yp*Length_xs
        else
          s=(ymax-y)/yp
          Length_ys = sqrt((ymax-y)**2+s**2)
          x=x0+xp*s
          Length_xs=sqrt((x-x0)**2+s**2)
        endif
        Am_length   = sqrt(Length_xs**2+Length_ys**2)       
        s=s/2
        x=x0+xp*s
        y=y0+yp*s
        PROC='AM'
        CALL MOVE_AM_(IS,NAM,Am_Length,DES(IS),DLYi(IS),DLRi(IS),xp,yp
     + ,PC)
        x=x+xp*(s_length-s)
        y=y+yp*(s_length-s)
        GOTO 111
      ELSEIF ((x.GT.(C_xmax-Alayer)) .and. x.LT.(C_xmax)  ) THEN
        PROC='AM'
        CALL MOVE_AM_(IS,NAM,s_length,DES(IS),DLYi(IS),DLRi(IS), xp,yp
     + ,PC)
        WRITE(*,*)'Fix here!'
        GOTO 111
      END IF
c
c THIRD CASE: the p interacts with the crystal.      
C. Define typical angles/probabilities for orientation 110
c
      xpcrit0 = (2.e-9*eUm(IS)/PC)**0.5       ! critical angle (rad) for 
                                              ! straight crystals
      Rcrit  = PC/(2.e-6*eUm(IS))*AI(IS)      ! critical curvature radius [m] 
                                              ! if R>Rcritical=>no channeling is
                                              ! possible (ratio<1)
      ratio = Rcurv/Rcrit                     ! parameter Rcry/Rcritical
c      write(*,*) "Critical Radius: ",Rcrit
      xpcrit = xpcrit0*(Rcurv-Rcrit)/Rcurv    ! critical angle for curved crystal
                                              ! NB: if ratio<1 => xpcrit<0
      c_v1 = 1.7                              ! fitting coefficient ??!
      c_v2 = -1.5                             ! fitting coefficient ???
      if (ratio .le. 1.) then                 ! case 1:no possibile channeling
        Ang_rms = c_v1*0.42*xpcrit0*sin(1.4*ratio)  ! rms scattering
        Ang_avr = c_v2*xpcrit0*0.05*ratio           ! average angle reflection
        Vcapt = 0.0                                 ! probability of VC
        elseif (ratio .le. 3) then              ! case 2: strongly bent xstal
          Ang_rms = c_v1*0.42*xpcrit0*sin(1.571*0.3*ratio+0.85)! rms scattering
          Ang_avr = c_v2*xpcrit0*(0.1972*ratio-0.1472)  ! avg angle reflection
c          Vcapt   = 0.01*(ratio-0.7)/(PC**2)
          Vcapt   = 0.0007*(ratio-0.7)/PC**0.2 !correction by sasha drozdin/armen    
          !K=0.00070 is taken based on simulations using CATCH.f (V.Biryukov)   
        else                                       ! case 3: Rcry >> Rcrit
          Ang_rms = c_v1*xpcrit0*(1./ratio)        !   
          Ang_avr = c_v2*xpcrit0*(1.-1.6667/ratio) ! average angle for VR
c          Vcapt = 0.01*(ratio-0.7)/(PC**2)        ! probability for VC  
          Vcapt = 0.0007*(ratio-0.7)/PC**0.2  !correction by sasha drozdin/armen
          ! K=0.0007 is taken based on simulations using CATCH.f (V.Biryukov)
      endif
cc----------------valentina approx-----------   
c      Ang_avr=-(xpcrit+xpcrit0) 
c      Ang_rms=(xpcrit0-xpcrit)/2 
cc-----------end valentina approx--------------

c      write(*,*) "Rcrit" , Rcrit,"Rcurv",Rcurv,
c     c "Ratio: ",ratio,"average VR angle:", ang_avr*1e6,"+-",           
c     c ang_rms*1e6, "ang crit:", xpcrit0*1e6,xpcrit*1e6
c      
      if(C_orient .eq. 2.) then
        Ang_avr = Ang_avr * 0.93                     ! for (111)
        Ang_rms = Ang_rms * 1.05
        xpcrit  = xpcrit * 0.98
      endif 
c
C. case 3-1: channeling
      IF (abs(xp_rel) .lt. xpcrit) THEN              ! if R' < R'c (ok CH) (1)
        Chann  = (xpcrit**2-xp_rel**2)**0.5/xpcrit0  ! probability of CH/VC
        N_atom = 0.1                                 ! probability of entering
                                                     ! close to atomic planes
        IF (rndm4() .le. Chann) then      ! if they can channel: 2 options
                                          ! option 1:channeling
c          TLdech1= 0.00054*PC*(1.-1./ratio)**2 ! calculate dechanneling length
          TLdech1= 0.0005*PC*(1.-1./ratio)**2 !calculate tipical dech. length(m)
          IF (rndm4() .le. N_atom) then
c            TLdech1= 0.000004*PC*(1.-1./ratio)**2! calculate tipical dechanneling
                                                  !length near atomic planes(m)
           !next line new from sasha
            TLdech1= 0.000002*PC*(1.-1./ratio)**2  ! dechanneling length (m) 
                               !for short crystal for high amplitude particles 
          ENDIF  

c          TLdech1=TLdech1/100 !!!!CHECK
          
          Dechan = -log(rndm4())                 ! probability of dechanneling
          Ldech  = TLdech1*Dechan                ! actual dechan. length
                     ! careful: the dechanneling lentgh is along the trajectory 
                     ! of the particle -not along the longitudinal coordinate...   
          if(Ldech .LT. L_chan) THEN    
           PROC='DC'                  !                               
            Dxp= Ldech/Rcurv             ! change angle from channeling [mrad]
            Sdech=Ldech*cos(miscut+0.5*Dxp)

            x  = x+ Ldech*(sin(0.5*Dxp+miscut))   ! trajectory at channeling exit
            xp = xp + Dxp + 2.0*(rndm4()-0.5)*xpcrit
            y= y + yp * Sdech 
            
            x = x + 0.5*(s_length-Sdech)*xp
            y = y + 0.5*(s_length-Sdech)*yp
            CALL 
     +MOVE_AM_(IS,NAM,s_length-Sdech,DES(IS),DLYi(IS),DLRi(IS),xp,yp,PC)
           !next line new from sasha
            PC = PC - 0.5*DES(IS)*y          ! energy loss to ionization [GeV]
            x = x + 0.5*(s_length-Sdech)*xp
            y = y + 0.5*(s_length-Sdech)*yp
          else                     
            PROC='CH'
            Dxp= L_chan/Rcurv + 0.5*RAN_GAUSS(1.)*xpcrit ! change angle[rad]
            xp = Dxp
            !next line new from sasha
            x  = x+ L_chan*(sin(0.5*Dxp+miscut)) ! trajectory at channeling exit
c            xp = xp + Dxp + 2.0*(rndm4()-0.5)*xpcrit  
            y = y + s_length * yp
c            !next line new from sasha
            PC = PC - 0.5*DES(IS)*Length       ! energy loss to ionization [GeV]
          endif                     
        ELSE                                   !option 2: VR
                                               ! good for channeling
                                               ! but don't channel         (1-2)
          PROC='VR'                            !volume reflection at the surface 
c          Dxp=0.5*(xp_rel/xpcrit+1)*Ang_avr
c          xp=xp+Dxp+Ang_rms*RAN_GAUSS(1.)
            !next line new from sasha
          xp=xp+0.45*(xp/xpcrit+1)*Ang_avr
          x = x + 0.5*s_length * xp
          y = y + 0.5*s_length * yp 
          CALL MOVE_AM_(IS,NAM,s_length,DES(IS),DLYi(IS),DLRi(IS), 
     +      xp ,yp,PC)
          x = x + 0.5*s_length * xp
          y = y + 0.5*s_length * yp 
        ENDIF                                    !           
c case 3-2: no good for channeling. check if the  can VR
      ELSE                                       
        Lrefl =  (xp_rel)*Rcurv                  ! distance of refl. point [m]
c        Srefl = sin(xp) * Lrefl
        Srefl = sin(xp_rel/2+miscut) * Lrefl
        if(Lrefl .gt. 0. .and. Lrefl .lt. Length) then  
                ! VR point inside
                !2 options: volume capture and volume reflection
          IF (rndm4() .gt. Vcapt .or. ZN. eq. 0.) THEN   !opt. 1: VR
           PROC='VR'
            x = x + xp * Srefl
            y = y + yp * Srefl
            Dxp= Ang_avr 
            xp = xp + Dxp + Ang_rms*RAN_GAUSS(1.)
            x = x + 0.5* xp * (s_length - Srefl)
            y = y + 0.5* yp * (s_length - Srefl)     !       
            CALL MOVE_AM_(IS,NAM,s_length-Srefl,DES(IS),DLYi(IS),
     +          DLRi(IS),xp ,yp,PC)
            x = x + 0.5 * xp * (s_length - Srefl)
            y = y + 0.5 * yp * (s_length - Srefl)    
          ELSE                                      !opt 2: VC
            x = x + xp * Srefl
            y = y + yp * Srefl
c            TLdech2= 0.00011*PC**0.25*(1.-1./ratio)**2 ! dechanneling length(m)
c            Dechan = log(1.-rndm4())
c            Ldech  = -TLdech2*Dechan
           !next 2 lines new from sasha - different dechanneling
           !probability
            TLdech2= 0.01*PC*(1.-1./ratio)**2   ! typical dechanneling length(m)
            Ldech  = 0.005*TLdech2*(sqrt(0.01-log(rndm4())) -0.1)**2 ! DC length
            tdech=Ldech/Rcurv
            Sdech=Ldech*cos(xp+0.5*tdech)
            IF(Ldech .LT. (Length-Lrefl)) then    
              PROC='DC'
              Dxp= Ldech/Rcurv + 0.5*ran_gauss(1)*xpcrit
              x  = x+ Ldech*(sin(0.5*Dxp+xp))   ! trajectory at channeling exit
              y = y + Sdech * yp 
              xp =  Dxp
              Red_S = s_length-Srefl -Sdech        
              x = x + 0.5 * xp * Red_S
              y = y + 0.5 * yp * Red_S                     
              CALL MOVE_AM_(IS,NAM,Red_S,DES(IS),DLYi(IS),DLRi(IS),
     +          xp,yp,PC)
              x = x + 0.5 * xp * Red_S
              y = y + 0.5 * yp * Red_S                   
            else                               
              PROC='VC'
              Rlength = Length-Lrefl
              tchan = Rlength / Rcurv 
              Red_S=Rlength*cos(xp+0.5*tchan)
              Dxp = (Length-Lrefl)/Rcurv 
              x  = x+ sin(0.5*Dxp+xp)*Rlength     ! trajectory at channeling exit
              y = y + red_S * yp
              xp =  Length/Rcurv + 0.5*ran_gauss(1)*xpcrit ! [mrad]
            endif                       
          ENDIF                        
C.  case 3-3: move in amorphous substance (big input angles)---------------
        else                          
           PROC='AM'
           x = x + 0.5 * s_length * xp
           y = y + 0.5 * s_length * yp
          if(ZN .gt. 0) then                        
           CALL MOVE_AM_(IS,NAM,s_length,DES(IS),DLYi(IS),DLRi(IS), 
     +          xp,yp,PC)
          endif
          x = x + 0.5 * s_length * xp
          y = y + 0.5 * s_length * yp            
        endif                                 
       ENDIF                                 
c      if (counter .eq. 0) then
111   write(833,*)'crystal parameters:\n Length:',Length, '\n Rcurv:'
     + , Rcurv ,'\n Critical Radius:', Rcrit, 'ratio',ratio             +
     +, '\n Critical angle for straight:',                              +
     + xpcrit0,'\n critical angle for curved crystal:', xpcrit,' \n Leng+
     +th:', Length, '\n xmax:', C_xmax, ' ymax:', ymax, '  C_orient: '  +
     +, C_orient                                                        +
     +, '\n Avg angle reflection:', Ang_avr, '\n full channeling angle: +
     +',(Length/Rcurv) 
c      counter=1
c      endif
c      
c      WRITE(*,*)'Crystal process: ',PROC
c      write(*,*)'xp_final :', xp
c      WRITE(*,*)'Crystal process: ',PROC,'Chann Angle',Ch_angle/1000,   +
c     1'Critical angle: ', xpcrit/1000
c     2 DLRI(IS),DLYI(IS),AI(IS),DES(IS),eUm(IS),IS,ZN,NAM,C_orient     !,W
      END


C.**************************************************************************
C     subroutine for the movement in the amorphous 
C.**************************************************************************
      SUBROUTINE MOVE_AM_(IS,NAM,DZ,DEI,DLY,DLr, XP,YP,PC)
C. Moving in amorphous substance...........................
      IMPLICIT none
      integer IS,NAM
      double precision DZ,DEI,DLY,DLr, XP,YP,PC
      double precision DLAI(4),SAI(4)
      double precision DLRI(4),DLYI(4),AI(4),DES(4)
      double precision AM(30),QP(30),NPAI
      double precision Dc(4),eUm(4)
      double precision DYA,W_p
      REAL*4 RNDM4
         REAL*4      RAN_GAUSS
      CHARACTER*50 PROC              !string that contains the physical process 
      COMMON /ALAST/DLAI,SAI
      COMMON/CRYS/ DLRI,DLYI,AI,DES 
      common /Proc2/PROC
c      write(*,*)'***********amorphous*************'
      xp=xp*1000
      yp=yp*1000
c      write(*,*)'xp initial:', xp, 'yp initial', yp
C. DEI - dE/dx stoping energy
      PC    = PC - DEI*DZ    ! energy lost beacause of ionization process[GeV]
C. Coulomb scattering
C. DYA - rms of coloumb scattering
      DYA = (14.0/PC)*SQRT(DZ/DLr)             !MCS (mrad)
c      write(*,*)'dya=',dya
      XP    = XP+DYA*RAN_GAUSS(1.)
      YP    = YP+DYA*RAN_GAUSS(1.)
      if(NAM .eq. 0) return
C.  Elastic scattering
      IF (rndm4() .LE. DZ/DLAI(IS)) THEN
        PROC = 'mcs'
c        write(*,*)'***********MCS*************'     !daniele
c         write(*,*)'case 1'
c        XP    = XP+SAI(IS)*RAN_GAUSS(1.)/PC 
c        YP    = YP+SAI(IS)*RAN_GAUSS(1.)/PC
        xp    = xp+196.*RAN_GAUSS(1.)/PC ! angle elast. scattering in R plane
        yp    = yp+196.*RAN_GAUSS(1.)/PC
      ENDIF
C.  Diffraction interaction
      IF (rndm4() .LE. DZ/(DLY*6.143)) THEN
        PROC = 'diff'
c        write(*,*)'***********diff*************'     !daniele
c         write(*,*)'case 2'
        XP    = XP+ 257.0*RAN_GAUSS(1.)/PC ! angle elast. scattering in R plane
        YP    = YP+ 257.0*RAN_GAUSS(1.)/PC
        W_p = rndm4()
        PC = PC -0.5*(0.3*PC)**W_p             ! m0*c = 1 GeV/c for particle
      ENDIF
C.  Inelastic interaction
      IF (rndm4() .LE. DZ/DLY) THEN
c       write(*,*)'***********inelastic*************'     !daniele
c        PC = 0.
        PROC = 'absorbed'
      ENDIF
c      write(*,*)'xp final:', xp, 'yp final', yp
      xp=xp/1000
      yp=yp/1000
      RETURN
      END
C      
      BLOCK DATA
      implicit none
      double precision DLAI(4),SAI(4)
      double precision DLRI(4),DLYI(4),AI(4),DES(4)
      double precision eUm(4)
      COMMON /ALAST/DLAI,SAI
      COMMON/CRYS/ DLRI,DLYI,AI,DES 
      COMMON/eUc/  eUm
C-----4 substances: Si(110),W(110),C,Ge----------------------------
      DATA DLRI/0.09336,.0035,0.188,.023/    ! radiation  length(m)
     +    ,DLYI/.455, .096, .400, .162/      ! nuclear length(m)
     +    ,AI /0.96E-7, 0.56E-7, 0.63E-7, 1.E-7/  !Si110 1/2 interplan. dist. mm
     +    ,DES/0.56,  3.0,  0.6, 1./         ! energy deposition in subst(GeV/m)
     +    ,DLAI/1.6,  0.57, 2.2, 1.0/        ! elastic length(m)
     +    ,SAI /42.,  140.,  42., 50./       ! elastic scat. r.m.s(mr)
     +    ,eUm/21.34,  21.,   21.,   21./    ! only for Si(110) potent. [eV]
      END





      

C                        'MATH.F'   10.07.01
C1  RANNOR - Gauss distribution simulation procedure.
C2  RF RNDM- ­  [0,1]        *
C3  RNDMST - .
C4  ...
C.**************************************************************************
C     subroutine for the generation of random numbers with a gaussian
C     distribution
C.**************************************************************************
C.**************************************************************************
      SUBROUTINE RANNOR(A,B) !gaussian with mean 0 and sigma 1
C
C1    Gauss distribution simulation procedure
C
      Y = RNDM(1.)
      IF (Y .EQ. 0.) Y = RNDM(1.)
      Z = RNDM(1.)
      X = 6.2831853*Z
      A1= SQRT(-2.0*ALOG(Y))
      A = A1*SIN(X)
      B = A1*COS(X)
C
      RETURN
      END
C
      real*4 function RNDM(rdummy)
      REAL*4          u,c,cd,cm, rrdummy
      COMMON/RANDOM/  u(97),c,cd,cm,i,j
C
      rrdummy = rdummy
C
      RNDM = u(i)-u(j)
      if (RNDM .LT. 0.) RNDM = RNDM+1.
      U(i) = RNDM
      i = i-1
      if ( i .EQ. 0 ) i = 97
      j = j-1
      if ( j .EQ. 0 ) j = 97
      c = c-cd
      if ( C .LT. 0.) c = c+cm
      RNDM = RNDM - c
      if ( RNDM .LT. 0. ) RNDM = RNDM+1.
C
      return
      END
C.**************************************************************************
C
C.**************************************************************************
      subroutine RNDMST(NA1,NA2,NA3,NB1)
      REAL*4 u,c,cd,cm
      COMMON/RANDOM/ u(97),c,cd,cm,i,j
C
      ma1 = na1
      ma2 = na2
      ma3 = na3
      mb1 = nb1
      i   = 97
      j   = 33
C
      do ii2 = 1,97
        s = 0.0
        t = 0.5
        do ii1 = 1,24
          mat  = MOD(MOD(ma1*ma2,179)*ma3,179)
          ma1  = ma2
          ma2  = ma3
          ma3  = mat
          mb1  = MOD(53*mb1+1,169)
          if (MOD(MB1*MAT,64) .GE. 32 ) s = s+t
          t = 0.5*t
        end do
        u(ii2) = s
      end do
C
      c  =   362436./16777216.
      cd =  7654321./16777216.
      cm = 16777213./16777216.
C
      return
      END
C.**************************************************************************
C
C.**************************************************************************
      subroutine RNDMIN(uin,cin,cdin,cmin,iin,jin)
      REAL*4 uin(97),cin,cdin,cmin
      REAL*4 u,c,cd,cm
      COMMON/RANDOM/ u(97),c,cd,cm,i,j
C
      do kkk = 1,97
        u(kkk) = uin(kkk)
      end do
C
      c  = cin
      cd = cdin
      cm = cmin
      i  = iin
      j  = jin
C
      return
      END
C.**************************************************************************
C
C.**************************************************************************
      subroutine RNDMOU(UOUT,COUT,CDOUT,CMOUT,IOUT,JOUT)
      REAL*4 uout(97),cout,cdout,cmout
      REAL*4 u,c,cd,cm
      COMMON/RANDOM/ u(97),c,cd,cm,i,j
C
      do kkk = 1,97
        uout(kkk) = u(kkk)
      end do
C
      COUT  = C
      CDOUT = CD
      CMOUT = CM
      IOUT  = I
      JOUT  = J
C
      return
      END
C.**************************************************************************
C
C.**************************************************************************
      subroutine RNDMTE(IO)
C
C.    *******************************************************************
C.    *  SUBROUTINE RNDMTE(IO)                                          
C.    *******************************************************************
C
      REAL*4 uu(97)
      REAL*4 u(6),x(6),d(6)
      DATA u / 6533892.0 , 14220222.0 ,  7275067.0 ,
     +         6172232.0 ,  8354498.0 , 10633180.0 /
C
      call RNDMOU(UU,CC,CCD,CCM,II,JJ)
      call RNDMST(12,34,56,78)
C
      do ii1 = 1,20000
        xx = rndm4()
      end do
C
      sd = 0.0
      do ii2 = 1,6
        x(II2)  = 4096.*(4096.*rndm4())
        d(II2)  = x(II2)-u(II2)
        sd = sd+d(II2)
      end do
C
      call RNDMIN(uu,cc,ccd,ccm,ii,jj)
      if (IO.EQ.1 .OR. SD .NE. 0.) write(6,10) (u(I),x(I),d(I),i=1,6)
C
 10   format('  === TEST OF THE RANDOM-GENERATOR ===',/,
     +       '    EXPECTED VALUE    CALCULATED VALUE     DIFFERENCE',/,
     +       6(F17.1,F20.1,F15.3,/),
     +       '  === END OF TEST ;',
     +       '  GENERATOR HAS THE SAME STATUS AS BEFORE CALLING RNDMTE')
      return
      END


