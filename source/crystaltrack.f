cut 
c=============================================================== 
c 
      SUBROUTINE collimate_cry(name_coll,C_MATERIAL, C_LENGTH,
     1                   C_ROTATION, 
     1                   C_APERTURE, C_OFFSET, C_TILT, 
     1                   X_IN, XP_IN, Y_IN,  
     2                   YP_IN, P_IN, S_IN, NP, ENOM, LHIT, 
     3                   PART_ABS, IMPACT, INDIV, LINT,
     4                   BX,BY,AX,
     5                   AY,EMITX0,EMITY0,
     6                   name,flagsec,dowrite_impact)!
c
c++  Based on routines by JBJ-R.assmann... Re-written for the crystal
c case by V.previtali in september 2008
c
c++  - Deleted all HBOOK stuff.
c++  - Deleted optics routine and all parser routines.
c++  - Replaced RANMAR call by RANLUX call
c++  - Included RANLUX code from CERNLIB into source
c++  - Changed dimensions from CGen(100,nmat) to CGen(200,nmat)
c++  - Replaced FUNPRE with FUNLXP
c++  - Replaced FUNRAN with FUNLUX
c++  - Included all CERNLIB code into source: RANLUX, FUNLXP, FUNLUX,
c++	                                      FUNPCT, FUNLZ, RADAPT,
c++                                           RGS56P
c++	 with additional entries:             RLUXIN, RLUXUT, RLUXAT,
c++                                           RLUXGO
c++                                           
c++  - Changed program so that Nev is total number of particles
c++    (scattered and not-scattered)
c++  - Added debug comments
c++  - Put real dp/dx
c
c
c   !!!!!!!!   KNOWN ISSUES:  !!!!!!!
c      1- the pencil beam is not working in the 6track version (I did not change
c      the colltrack version yet)
c      
      IMPLICIT NONE
c
      integer     MAX_NCOLL
      PARAMETER     (MAX_NCOLL = 99)
c
         integer         mat
         integer         Nev
         integer         j
         integer         nabs
         integer         NHIT
         integer         MAX_NPART 
         integer         NP
         PARAMETER         (MAX_NPART=20000)
c
         double precision            p0
         double precision            xp_pencil0(MAX_NCOLL)
         double precision            yp_pencil0(MAX_NCOLL)
         double precision            x_pencil(MAX_NCOLL)
         double precision            y_pencil(MAX_NCOLL)
         double precision            zlm
         double precision            x,xp
         double precision            shift
         double precision            x_shift, xp_shift,s_shift !coordinates after shift/rotation
         double precision            x_rot, xp_rot,s_rot
         double precision            x_temp, xp_temp,s_temp !!all the _temp variables are used when you hit the cry from below
         double precision            tilt_int, x_int,xp_int,s_int       !all the _int variables are used when you hit the cry from below (int=interaction point)
         double precision            x00
         double precision            z
         double precision            z00
         double precision            zp
         double precision            p
         double precision            s 
         double precision            a_eq,b_eq,c_eq,Delta
         double precision            ENOM
c         
         double precision            x_PRINT,xp_PRINT,y_PRINT,yp_PRINT 
         double precision            AX,BX,AY,BY
         double precision            X_NORM,XP_NORM,Y_NORM,YP_NORM
         double precision            EMITX0               
         double precision            EMITY0              
c
         integer         LHIT(MAX_NPART)
         integer         PART_ABS(MAX_NPART)
c
c
         double precision            x_in(MAX_NPART)
         double precision            xp_in(MAX_NPART)
         double precision            y_in(MAX_NPART)
         double precision            yp_in(MAX_NPART)
         double precision            p_in(MAX_NPART)    !be careful: [Gev]
         double precision            s_in(MAX_NPART)
c        adding variables for the pencil beam. Variables in the absolute reference frame. 
         double precision            x_in0(MAX_NPART)
         double precision            xp_in0(MAX_NPART)
         double precision            y_in0(MAX_NPART)
         double precision            yp_in0(MAX_NPART)
         double precision            p_in0(MAX_NPART)    !be careful: [Gev]
         double precision            s_in0(MAX_NPART)
         double precision            s_impact
         integer                     flagsec(MAX_NPART)
         logical                     dowrite_impact
         
c
         double precision            IMPACT(MAX_NPART)
         double precision            INDIV(MAX_NPART)
         double precision            LINT(MAX_NPART)
         integer                     name(MAX_NPART)
c
         double precision            x_out(MAX_NPART)
         double precision            xp_out(MAX_NPART)
         double precision            y_out(MAX_NPART)
         double precision            yp_out(MAX_NPART)
         double precision            p_out(MAX_NPART)
         double precision            s_out(MAX_NPART)
c
         double precision            fracab
         double precision            drift_length 
         double precision            mirror 
         double precision            tiltangle
         double precision            tiltangle2
c
         CHARACTER*6 C_MATERIAL     !Material 
         double precision      C_LENGTH       !Length in m 
         double precision      C_ROTATION     !Rotation angle vs vertical in radian 
         double precision      C_APERTURE     !Aperture in m 
         double precision      C_OFFSET       !Offset in m 
         double precision      C_TILT(2)      !Tilt in radian 
         double precision      C_TILT0(2)      !Tilt in radian 
         double precision      PENCIL_DX(MAX_NCOLL)
         double precision      PENCIL_SPREAD(MAX_NCOLL)
         double precision      cry_bend
         
c
         LOGICAL           CHANGED_TILT1(MAX_NCOLL)
         LOGICAL           CHANGED_TILT2(MAX_NCOLL)
c
         COMMON /TILT/ CHANGED_TILT1, CHANGED_TILT2
c
         REAL*4      rndm4
         REAL*4      RAN_GAUSS
c
         common/materia/mat
         common/nommom/p0
c
        integer ie,iturn,nabs_total
        COMMON  /INFO/ IE, ITURN, nabs_total
c
        integer   IPENCIL
        integer   ICOLL
        common  /icoll/  icoll
        
        common  /pencil/  xp_pencil0,yp_pencil0,pencil_dx,ipencil
c
        COMMON  /PENCIL2/ X_PENCIL, Y_PENCIL
c
c
        double precision        AMPLZ
c 
      double precision XP_tangent
c
      double precision Rcurv,C_xmax,C_ymax           !crystal geometrical parameters  - be careful! are in [m]
      double precision Alayer                           !amorphous layer [mm]
      integer C_orient                           !crystal orientation [0-1] 
      double precision Cry_tilt_part                    !crystal tilt [rad]
      double precision Cry_tilt                         !crystal tilt [rad]
      double precision Cry_tilt0                        !tilt of the crystal for having channeling (determined by divergence of the beam) [rad]
      double precision Cry_length                       !original length (from the db) [m]
      double precision miscut
c                                           !instead of this parameter, I use (mat-7) 
c
      
      integer bool_proc(MAX_NPART)           
      integer bool_proc_old(MAX_NPART)           
      integer n_chan
      integer n_VR
      integer n_amorphous
      character*50 name_coll
      CHARACTER*50 PROC                     !string that contains the physical process
c                                            !=' ' if the particle does not pass by crystal, ='*' if there is interaction
      logical  bool_create
      common /miscut/ miscut
      common /Par_Cry1/ Cry_length, Rcurv,C_xmax,C_ymax,Alayer,C_orient 
      
      common /Par_Cry2/ Cry_tilt,Cry_tilt0
      common /Process/ bool_proc,bool_create
      common /Process_old/ bool_proc_old
      common/Proc2/PROC
      logical write_c_out, write_SPS_out, write_elens_out
     &   , write_TM_QUAD_out
      common /outputs/ write_c_out, write_SPS_out, write_elens_out
     &   , write_TM_QUAD_out

      integer idx_proc                !daniele
      integer   samplenumber                !daniele
      character*4 smpl                !daniele
      character*80 pfile                !daniele
      common /samplenumber/ pfile,smpl,samplenumber                !daniele

c
  666 FORMAT(A,1x,e20.10,1x,A,1x,e20.10,A,1x,e20.10,1x,A,1x,e20.10)
c=======================================================================
c
c      write(*,*) 'enter collimate_cry routine, rotation ',C_ROTATION

c      open(unit=9999,file='debug.dat')
      IF (C_MATERIAL.eq.'CRY-Si')THEN 
           mat = 8 
      ELSEIF (C_MATERIAL.eq.'CRY-W')THEN 
           mat = 9 
      ELSEIF (C_MATERIAL.eq.'CRY-C')THEN 
           mat = 10 
      ELSEIF (C_MATERIAL.eq.'CRY-Ge')THEN 
           mat = 11
      ELSE 
           WRITE(*,*) 'ERR>', C_MATERIAL, ' Material not found. STOP' 
           STOP 
      ENDIF 
c

c          write(*,*) "debug - length  bent" , C_LENGTH   
c          write(*,*) "debug - length  unbent" , CRY_LENGTH   
      Cry_bend =  Cry_length/Rcurv !cry_length longitudinal estension of the straight crystal
c          write(*,*) "debug - bend angle" , CRY_BEND   
c          write(*,*) "debug - C_xmax" , C_xmax   
c          write(*,*) "debug - miscut angle" , miscut  
      if (c_length .gt. 0.) then
       NEV = NP 
       P0  = ENOM 
       C_TILT0(1) = C_TILT(1)
       C_TILT0(2) = C_TILT(2)
       tiltangle=C_TILT0(1)
c
c++  Initialize scattering processes
c
       call scatin(p0)

* EVENT LOOP,  initial distribution is here a flat distribution with
* xmin=x-, xmax=x+, etc. from the input file
*
      nhit    = 0
      fracab  = 0.
      n_chan  = 0          !valentina :initialize to zero the counters for crystal effects 
      n_VR    = 0          !
      n_amorphous = 0      !
c      
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c        WRITE(*,*) ITURN,ICOLL  
      do j = 1, nev 
c 
        impact(j) = -1.
        lint(j)   = -1.
        indiv(j)  = -1.

        idx_proc = name(j)-100*samplenumber



        if (ITURN .eq. 1) then
                bool_proc_old(idx_proc)=-1
        else 
               if (bool_proc(idx_proc).ne.-1)  
     &                       bool_proc_old(idx_proc)=bool_proc(idx_proc)
        endif
        PROC='out' !the default process is 'out'
        bool_proc(idx_proc)=-1    
c
        nabs = 0
c
        s   = 0
        x   = x_in(j)
        xp  = xp_in(j)
        z   = y_in(j)
        zp  = yp_in(j)
        p   = p_in(j)
c        
        x_temp=0.
        x_int=0.
        x_rot=0
        x_shift=0
        s_temp=0.
        s_int=0.
        s_rot=0
        s_shift=0
        xp_int=0.
        xp_temp=0.
        xp_rot=0
        xp_shift=0
        shift=0.
        tilt_int=0.
c        Cry_tilt_part=Cry_tilt                         !crystal tilt [rad]
C             write(*,*)'collimator',mat,'particle',j,'x',x,
C     1       'in sigma', x/sqrt(bx)/sqrt(EMITX0),'sig_sk',
C     2       C_APERTURE/2
c 
c++  transform particle coordinates to get into collimator coordinate 
c++  system 
c 
c++  first check whether particle was lost before 
c 
        if (x.lt.99.0*1d-3 .and. z.lt.99.0*1d-3) then
c /
c++  first do rotation into collimator frame 
c 
              
c          write(*,*) "debug - rotazione" ,c_rotation   
          x  = x_in(j)*cos(c_rotation) +sin(c_rotation)*y_in(j) 
          z  = y_in(j)*cos(c_rotation) -sin(c_rotation)*x_in(j) 
          xp = xp_in(j)*cos(c_rotation)+sin(c_rotation)*yp_in(j) 
          zp = yp_in(j)*cos(c_rotation)-sin(c_rotation)*xp_in(j) 
c
c++  for one-sided collimators consider only positive x. for negative
c++  x jump to the next particle

          if ((name_coll(1:11) .eq. "CRY.SPS.EXP")) then
          mirror=-1
c             write(*,*) "valentina crsitallo SPS riconosciuto"     
             if (x .gt. 0) then
                goto 777
             endif
          else
             mirror=1
             if  (x.lt.0 ) then
                 goto 777
             endif
          endif
          x  = mirror * x
          xp = mirror * xp
 
              
          
c++Shift with opening and offset 
c 
          X  = X - C_APERTURE/2 - C_OFFSET 
c
c++  Include collimator tilt
c
c          write(*,*) "debug - collimator tilt" ,tiltangle   
          IF (tiltangle.GT.0.) THEN
            XP = XP - tiltangle
          ELSEIF (tiltangle.LT.0.) THEN
            X  = X + SIN(tiltangle) * C_LENGTH
            XP = XP - tiltangle
          ENDIF
c
c++  For selected collimator, first turn reset particle distribution
c++  to simple pencil beam
c
          IF (ipencil .eq. 0) bool_create=.true.
!            
c! pencil beam stuffffffffffffffffffffffffffffffffff  (~80 lines)           
cc ----------------------------pencil beam generatio at the crystal--------------------------------------
c          IF ( (ICOLL.EQ.IPENCIL 
c     1           .AND. ITURN.EQ.1)    
c     2           ) THEN
c            X    = pencil_dx(ICOLL)
c            XP = cry_tilt0 !valentina (if the beam is generated @ crystal, I want it to have the natural beam divergence)
c            bool_create=.true.
cc
c            AMPLZ = RAN_GAUSS(3.)!*pencil_spread(ICOLL)
c            Z     = AMPLZ
c            ZP   = 0.
cc
c            if (write_c_out) then
cc++             I want to write the original coordinates in a file... I have to transform back all coordinates....Include collimator tilt
cc 
c              IF (tiltangle.GT.0.) THEN
c                x_print  = X  + tiltangle*C_LENGTH
c                XP_print = XP + tiltangle
c              ELSEIF (tiltangle.LT.0.) THEN
c                x_print  = X + tiltangle*C_LENGTH
c                XP_print = XP + tiltangle
c                x_print  = X - SIN(tiltangle) * C_LENGTH
c              ELSE
c                x_print = x
c                xp_print = xp
c              ENDIF
c
cc++  Transform back to particle coordinates with opening and offset 
cc
c              x_print = x_print + C_APERTURE/2 + MIRROR*C_OFFSET 
cc++  Now mirror at the horizontal axis for negative X offset 
cc 
c              x_print    = MIRROR * x_print 
c              XP_print   = MIRROR * xp_print
c
cc++  Last do rotation into collimator frame 
cc 
c              Y_print  = Z  *COS(-1.*C_ROTATION) - 
c     1                     X_print  *SIN(-1.*C_ROTATION) 
c              x_print  = x_print  *COS(-1.*C_ROTATION) + 
c     1                     Z  *SIN(-1.*C_ROTATION) 
c              YP_print = ZP *COS(-1.*C_ROTATION) - 
c     1                     XP_print *SIN(-1.*C_ROTATION) 
c              XP_print = XP_print *COS(-1.*C_ROTATION) + 
c     1                     ZP *SIN(-1.*C_ROTATION) 
cc              WRITE(*,'(i4,2x,i4,2x,a,2x,5(f15.8,2x))') 
cc     2          ITURN,ICOLL,C_MATERIAL,x_print,XP_print,Y_print,YP_print
cc     3          ,P
cC
cc++ Then drift forward of half lenght
cc       
c              x_print= x_print+xp_print*C_length/2        !valentina drift removed                               
c              y_print= y_print+yp_print*C_length/2                   
c              WRITE(881,'(i4,2x,i4,2x,a,2x,5(f15.8,2x))') 
c     2        ITURN,ICOLL,C_MATERIAL,x_print,XP_print,Y_print,YP_print
c     3        ,P
c
c              X_NORM=x_print/ SQRT(bx)/sqrt(EMITX0)
c              XP_NORM=(x_print*ax+xp_print*bx)/SQRT(bx)/sqrt(EMITX0)
c              Y_NORM=y_print/ SQRT(by)/sqrt(EMITY0)
c              YP_NORM=(y_print*AY+yp_print*by)/SQRT(by)/sqrt(EMITY0)
cc
c              WRITE(883,'(i4,2x,i4,2x,a,2x,7(f15.8,2x))')            
c     2          ITURN,ICOLL,C_MATERIAL,X_NORM,XP_NORM,Y_NORM,YP_NORM,   
c     3          SQRT(X_NORM**2+XP_NORM**2),
c     4          SQRT(Y_NORM**2+YP_NORM**2),P
cc
cc
c            endif
c          ENDIF
cc.--------------------end of pencil beam stuff-------------------------------


c-valentina for the first impact file        
        s_in0(j)   = s_in(j) 
        x_in0(j)   = x
        xp_in0(j)  = xp
        y_in0(j)   = z
        yp_in0(j)  = zp
        p_in0(j)   = p
        

c        write(*,*) "debug - coll RFS" , s_in(j),x,xp,z,zp   


          
            
c-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o
c
c crystal!!! transform in the crystal rerference system
c    1st transformation: shift of the center of my reference frame  
c          write(*,*) "debug - cry tilt" , Cry_tilt  
c          write(*,*) "debug - cry tilt 0" , Cry_tilt0  

         if (Cry_tilt .lt. 0) then
           S_shift=S
c           write(*,*) j,'- s=',s
           shift=Rcurv*(1-cos(Cry_tilt))
           if (Cry_tilt .lt. (-Cry_bend) ) then
                shift= ( Rcurv *
     &          ( cos ( - Cry_tilt)
     &          - cos( Cry_bend - Cry_tilt) ) )
           endif
           X_shift=X-shift
         else
           S_shift=S
           X_shift=X
         endif
c          write(*,*) "debug - S shift" ,  S_shift 
c          write(*,*) "debug - X shift" ,  X_shift 
c         
c    2nd transformation: rotation
         S_rot =X_shift*sin(Cry_tilt)+S_shift*cos(Cry_tilt)
         X_rot = X_shift*cos(Cry_tilt)-S_shift*sin(Cry_tilt)
         XP_rot= XP - Cry_tilt      
c          write(*,*) "debug - S rot" ,  S_rot 
c          write(*,*) "debug - X rot" ,  X_rot 
c          write(*,*) "debug - XP rot" ,  XP_rot 
c    3rd transformation: drift to the new coordinate s=0         
         XP=XP_rot
         X= X_rot - XP_rot*S_rot
         Z= Z - ZP*S_rot
         S=0     
c          write(*,*) "debug - S cryRF" ,  S_rot 
c          write(*,*) "debug - X cryRF" ,  X_rot 
c          write(*,*) "debug - XP cryRF" ,  XP_rot 
c
c  NOW CHECK IF THE PARTICLE HIT the crystal
c        
         if (x .ge. 0) then
           s_impact=s_in0(j) !(for the first impact)      
c           write(*,*)'hit the cry entrance face'
c           write(*,*)'impact at s,x = ', s_impact,x_in0(j)
c           write(*,*)'with angle xp = ',xp
c           write(*,*)'s before', s
           CALL CRYST(mat-7,X,XP,Z,ZP,p,cry_length)
           s=Rcurv*sin(cry_bend)
           zlm=Rcurv*sin(cry_bend)
c           write(*,*) 'process:',PROC
c           write(*,*)'s after', s
c           write(*,*)'hit the crystal'
           if (PROC(1:3).ne.'out')then
             NHIT = NHIT + 1
             LHIT(j) = 100000000*ie + ITURN
             IMPACT(j) = X_in0(j)            
             INDIV(j) = XP_in0(j)
           endif
         else 
           XP_tangent=sqrt((-2*X*Rcurv+X**2)/(Rcurv**2))
c           write(*,*)j,'-','tangent',xp_tangent,'angle',xp
c           write(*,*)'s tan',Rcurv*sin(XP_tangent)
c           write(*,*) 's tot', c_length,Rcurv*sin(cry_bend) 
           if ( XP .ge. XP_tangent  ) then

c if it hits the crystal, calculate in which point and apply the
c transformation and drift to that point
             a_eq=(1.+xp**2)
             b_eq=2.*xp*(x-Rcurv)
             c_eq=-2.*x*Rcurv+x**2
             Delta=b_eq**2-4.*(a_eq*c_eq)
             S_int=(-b_eq-sqrt(Delta))/(2.*a_eq)
c             write(*,*)'s int',S_int
             if (S_int .lt. Rcurv*sin(cry_bend)) then
c  transform to a new ref system:shift and rotate
               X_int=XP*S_int+X
               XP_int=XP
               Z=Z+ZP*S_int
               X=0
               S=0
c               tilt_int=2*X_int/S_int
               tilt_int=S_int/Rcurv
               XP=XP-tilt_int
c               write(*,*)'hit the cry from below!!!'
c               write(*,*)'tilt int',tilt_int,'over',cry_bend
c               write(*,*)'tilt bending',Cry_length/Rcurv,
c     &         'total tilt', cry_tilt-cry_tilt0,         
c     &         'int. tilt', tilt_int         
c               s_impact=Rcurv*(sin(Cry_length/Rcurv)
c     &           -sin(Cry_length/Rcurv-tilt_int))!(for the first impact)      
c               x_in0(j)=Rcurv*(1-cos(Cry_length/Rcurv-tilt_int))
c               write(*,*)'impact at s,x = ', s_impact,x_in0(j)
c               write(*,*)'with angle xp = ',xp
c               write(*,*) "debug - S minicry" ,  S 
c               write(*,*) "debug - X minicry" ,  X 
c               write(*,*) "debug - XP minicry" ,  XP 
c call cry routine             
              CALL 
     &        CRYST(mat-7,X,XP,Z,ZP,p,(cry_length-(tilt_int*Rcurv)))
              s=Rcurv*sin(cry_bend-tilt_int) 
              zlm=Rcurv*sin(cry_bend-tilt_int)
c              write(*,*) 'process:',PROC
c              write(*,*) "debug - S minicry 2" ,  S 
c              write(*,*) "debug - X minicry 2" ,  X
c              write(*,*) "debug - XP minicry 2" ,  XP 
              if (PROC(1:3).ne.'out')then
                 X_rot=X_int
                 S_rot=S_int
                 XP_rot=XP_int             
                 S_shift=S_rot*cos(-Cry_tilt)+X_rot*sin(-Cry_tilt)
                 X_shift=-S_rot*sin(-Cry_tilt)+X_rot*cos(-Cry_tilt)
                 XP_shift=XP_rot + Cry_tilt 
                 if (Cry_tilt .lt. 0) then
                   S_impact=S_shift       
                   X_in0(j)=X_shift+shift                
                   XP_in0(j)=XP_shift
                 else
                   X_in0(j)=X_shift
                   S_impact=S_shift
                   XP_in0(j)=XP_shift
                 endif
                 NHIT = NHIT + 1
                 LHIT(j) = 100000000*ie + ITURN
                 IMPACT(j) = X_in0(j)            
                 INDIV(j) = XP_in0(j)
               endif
c           write(*,*)'s after', s
c un-rotate
               X_temp=X
               S_temp=S
               XP_temp=XP              
               S=S_temp*cos(-tilt_int)+X_temp*sin(-tilt_int)
               X=-S_temp*sin(-tilt_int)+X_temp*cos(-tilt_int)
               XP=XP_temp + tilt_int
c     2nd: shift back the 2 axis
               X=X+X_int                
               S=S+S_int
c               write(*,*)'s after', s
             else
c               write(*,*)'treat the drift'
               S=Rcurv*sin(cry_length/Rcurv)
               X=X+S*XP
               Z=Z+S*ZP
             endif
           else
c              write(*,*) 'just the drift'
             S=Rcurv*sin(cry_length/Rcurv)
             X=X+S*XP
             Z=Z+S*ZP
           endif
         endif
c               WRITE(*,*)'X1_cry',X,'Z1_Cry',Z,'XP1_Cry',XP,'ZP1_Cry',ZP
c     1         ,'s',s, Cry_tilt
c               
c trasform back from the crystal to the collimator reference system
c    1st: un-rotate the coordinates
               X_rot=X
               S_rot=S
               XP_rot=XP              
c               write(*,*) "debug - S cryRF 2" ,  S_rot 
c               write(*,*) "debug - X cryRF 2" ,  X_rot 
c               write(*,*) "debug - XP cryRF 2" ,  XP_rot 
               S_shift=S_rot*cos(-Cry_tilt)+X_rot*sin(-Cry_tilt)
               X_shift=-S_rot*sin(-Cry_tilt)+X_rot*cos(-Cry_tilt)
               XP_shift=XP_rot + Cry_tilt 
c     2nd: shift back the reference frame
               if (Cry_tilt .lt. 0) then
                 S=S_shift       
                 X=X_shift+shift               
                 XP=XP_shift
               else
                 X=X_shift
                 S=S_shift
                 XP=XP_shift
               endif
c     3rd: shift to new S=Length position               
               X=XP*(c_length-S)+X
               Z=ZP*(c_length-S)+Z
               S=c_length
               
c               write(*,*) "debug - S Coll RF 2" ,  S_rot 
c               write(*,*) "debug - X Coll RF 2" ,  X_rot 
c               write(*,*) "debug - XP Coll RF 2" ,  XP_rot 
c
c          WRITE(*,*)'X1_coll',X,'Z1_coll',Z,'XP1_coll',XP,'ZP1_coll',ZP
c     1         ,'s' ,s
               NABS=0
               if (PROC(1:2).eq.'AM')then 
                 bool_proc(idx_proc)=1
                 n_amorphous = n_amorphous + 1 
               elseif (PROC(1:2).eq.'VR') then 
                 bool_proc(idx_proc)=2
                 n_VR = n_VR + 1
               elseif (PROC(1:2).eq.'CH')then
                 bool_proc(idx_proc) = 3
                 n_chan = n_Chan + 1
               elseif (PROC(1:2).eq.'VC') then 
                 bool_proc(idx_proc)=3
                 n_chan = n_Chan + 1
               elseif (PROC(1:3).eq.'out')then
                 bool_proc(idx_proc)=-1
               elseif (PROC(1:8).eq.'absorbed') then 
                 bool_proc(idx_proc)=5
                 NABS=1
               elseif (PROC(1:2).eq.'DC')then
                 bool_proc(idx_proc)=6
               elseif (PROC(1:3).eq.'mcs')then        !daniele 
                 bool_proc(idx_proc)=7
               elseif (PROC(1:4).eq.'diff')then       !daniele                                                                                                                           
                 bool_proc(idx_proc)=8
               else  
                write(*,*)'???????????????????',PROC(1:2) 
                stop
               endif
c=========================== 
c++  Transform back to particle coordinates with opening and offset 
c 
       IF (PART_ABS(j).eq.0) THEN 
c 
c++  Include collimator tilt
c
         IF (tiltangle.GT.0.) THEN
           X  = X  + tiltangle*C_LENGTH
           XP = XP + tiltangle
         ELSEIF (tiltangle.LT.0.) THEN
           X  = X + tiltangle*C_LENGTH
           XP = XP + tiltangle
c
           X  = X - SIN(tiltangle) * C_LENGTH
         ENDIF
c
c++  Transform back to particle coordinates with opening and offset 
c
         Z00 = Z
         X00 = X + MIRROR*C_OFFSET 
         X = X + C_APERTURE/2 + MIRROR*C_OFFSET 
c 
c++  Now mirror at the horizontal axis for negative X offset 
c 
         X    = MIRROR * X 
         XP   = MIRROR * XP 

c 
c++  Last do rotation into collimator frame 
c 
         X_IN(J)  = X  *COS(-1.*C_ROTATION) + 
     1                 Z  *SIN(-1.*C_ROTATION) 
         Y_IN(J)  = Z  *COS(-1.*C_ROTATION) - 
     1                 X  *SIN(-1.*C_ROTATION) 
         XP_IN(J) = XP *COS(-1.*C_ROTATION) + 
     1                 ZP *SIN(-1.*C_ROTATION) 
         YP_IN(J) = ZP *COS(-1.*C_ROTATION) - 
     1                 XP *SIN(-1.*C_ROTATION) 

c----- other pencil beam stuff-------
c         IF ( ICOLL.EQ.IPENCIL) then 
c           X00  = MIRROR * X00 
c           X_IN(J)  = X00  *COS(-1.*C_ROTATION) + 
c     1                    Z00  *SIN(-1.*C_ROTATION) 
c           Y_IN(J)  = Z00  *COS(-1.*C_ROTATION) - 
c     1                    X00  *SIN(-1.*C_ROTATION) 
cc
c           XP_IN(J) = XP_IN(J) + MIRROR*XP_PENCIL0(ICOLL)
c           YP_IN(J) = YP_IN(J) + MIRROR*YP_PENCIL0(ICOLL)
c           X_IN(J) = X_IN(J) + MIRROR*X_PENCIL(ICOLL)
c           Y_IN(J) = Y_IN(J) + MIRROR*Y_PENCIL(ICOLL)
c
c           IF (.NOT. CHANGED_TILT1(ICOLL) .AND. MIRROR.GT.0.) THEN
c                   WRITE (*,*) 'NEVER!!!'
c                   C_TILT(1) = XP_PENCIL0(ICOLL)*COS(C_ROTATION)+
c     1                         SIN(C_ROTATION)*YP_PENCIL0(ICOLL)
c                   WRITE(*,*) 'INFO> Changed tilt1  ICOLL  to  ANGLE  ',
c     1                     ICOLL, C_TILT(1)
cc
c                   CHANGED_TILT1(ICOLL) = .true.
c           ELSEIF (.NOT. CHANGED_TILT2(ICOLL) 
c     1                                   .AND. MIRROR.LT.0.) THEN
c                   C_TILT(2) = -1.*(XP_PENCIL0(ICOLL)*COS(C_ROTATION)+
c     1                       SIN(C_ROTATION)*YP_PENCIL0(ICOLL))
c                   WRITE(*,*) 'INFO> Changed tilt2  ICOLL  to  ANGLE  ',
c     1                   ICOLL, C_TILT(2)
cc                
c                   CHANGED_TILT2(ICOLL) = .true.
c           ENDIF
c         ELSE 
c           C_TILT(1) = 0.
c           C_TILT(2) = 0.
c           CHANGED_TILT1(ICOLL) = .true.
c           CHANGED_TILT2(ICOLL) = .true.
c         ENDIF 
c------------------- end pencil beam stuff-----------------
c         
         p_in(J) = p 
         s_in(J) = s_in(J) + S
c 
          if (nabs.eq.1) then
             fracab = fracab + 1
             part_abs(j) = 100000000*ie + iturn
             lint(j) = zlm
             if (dowrite_impact) then
               write(48,'(i4,(1x,f6.3),(1x,f8.6),4(1x,e19.10),i2,
     &         2(1x,i7))')                                              
     &         icoll,c_rotation,                                       
     &         s,                 
     &         x_in(j)*1d3, xp_in(j)*1d3, y_in(j)*1d3, yp_in(j)*1d3,  
     &         nabs,name(j),iturn
             endif
             x = 99.99d-3
             z = 99.99d-3
          endif
       ENDIF 
c-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o
c valentina First impact file
c
c                write(9999,*) "dowrite impact value", dowrite_impact
             if(flagsec(j).eq.0 .and. PROC(1:3) .ne. 'out') then
               flagsec(j)=1 
               if (dowrite_impact) then
               write(39,'(i5,1x,i7,1x,i2,1x,i1,2(1x,f7.6),8(1x,e17.9))')
     &               name(j),iturn,icoll,nabs,                          
     &               s_impact,          
     &               s, 
     &               x_in0(j),xp_in0(j),y_in0(j),yp_in0(j),
     &               x_in(j),xp_in(j),y_in(j),yp_in(j)
               endif
             endif
c
c++  End of check for particles not being lost before   (see @330)
c
        ENDIF 
c++  End of loop over all particles 
c 
 777  END DO
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
c
c        if (nhit .gt. 0.) then
c          WRITE(*,*) 'Collimator:                    ',ICOLL 
c          WRITE(*,*) 'Number of particles:           ', Nev
c          WRITE(*,*) 'Number of particle hits:       ', Nhit
c          WRITE(*,*) 'Number of absorped particles:   ', fracab 
c          WRITE(*,*) 'Number of escaped particles:    ', Nhit-fracab 
c          WRITE(*,*) 'Fraction of absorbed particles:', 100.*fracab/Nhit
c            WRITE(*,*)'Fraction of channeled particles:',100*n_chan/Nhit
c            WRITE(*,*)'Fraction of VR particles:       ',100*n_VR/Nhit
c            WRITE(*,*)'Fraction of amorphous process:  ',100*n_amorphous
c     1/Nhit
c        endif 
c
      endif  !collimator with length = 0
      return
      end

