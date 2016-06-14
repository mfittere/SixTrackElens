!******************************************************************************
!
! VALENTINA PREVITALI - 2012
! JCSMITH
!
!
! subroutine collimate_elense (elense_r_min, elense_curr,
!               length, voltage, r2_ov_r1, center_x, center_y, 
!               op_mode, tune, Delta_tune, step_tune, step_turns,
!               resonant_turns, elens_jitter, elens_radial,
!               x_in, xp_in, y_in, yp_in, 
!               p_in, s_in, np,enom, lhit, part_abs, impact, indiv, lint, name)
! Calculates radial kick of beam passing through electron lense. Assumes a
! radially symmetric lense and no slicing. Does not address fringe field. Applies
! kick at center of element, it's a thin lense, no actual tracking is performed.
!
! from: V. Shiltsev, et al. "LHC Particle Colimation by Hollow Electron Beams"
!  EPAC08 MOPC098
!
! Parameters:
!  npart           -- total number of particles := 64
!  maxn            -- 1000000
!                  
! Input:        
!  elense_radius   -- double precision: electron lens inner radius (m)      
!  elense_curr     -- double precision: electron lens total current (A) 
!  length          -- double precision: electron lens lenght (m)
!  voltage         -- double precision: electron lens Voltage (kV)      
!  r2_ov_r1        -- double precision: ratio between external and
!                     internal cathode radius
!  center_x        -- double precision: center x coordinate
!  center_y        -- double precision: center y coordinate
!  op_mode         -- integer: electron lens operation mode
!                     1- random mode
!                     2- AC mode
!                     3- resonant mode
!                     otherwise the electron lens is operated in DC mode
!  tune            -- double precision: (for AC mode only) elens operational tune
!  Delta_tune      -- double precision: (for AC mode only) maximum variation in tune
!  step_tune       -- double precision: (for AC mode only) tune variation per step
!  step_turns      -- integer: (for AC mode only) number of turns per step
!  resonant_turns  -- integer: (for resonant mode only) turn period for resonant mode      
!  elens_jitter    -- logical: switch on/off a 2% jitter in electron  lens current
!  elens_radial    -- logical: switch on/off a realistic radial profile  i(r)
!  x_in(npart)     -- double precision: incoming particle horizontal position (meters?)
!  xp_in(npart)    -- double precision: incoming particle horizontal geometric angle (radians?)
!  y_in(npart)     -- double precision: incoming particle vertical position (meters?)
!  yp_in(npart)    -- double precision: incoming particle vertical geometric angle (radians)
!  p_in(npart)     -- double precision: incoming particle momentum (GeV)
!  s_in(npart)     -- double precision: incoming particle longitudinal position (meters)
!                      NOTE: all particles start at same longitudinal position!
!  np              -- integer: total number of tracked particles (.le. npart)
!  enom            -- double precision: reference energy (MeV)
! Output:
!  x_in(npart)     -- double precision: outgoing particle horizontal position (meters?)
!  xp_in(npart)    -- double precision: outgoing particle horizontal geometric angle (radians?)
!  y_in(npart)     -- double precision: outgoing particle vertical position (meters?)
!  yp_in(npart)    -- double precision: outgoing particle vertical geometric angle (radians)
!  p_in(npart)     -- double precision: outgoing particle momentum (Gev)
!  s_in(npart)     -- double precision: outgoing particle longitudinal position (meters?)
!  /info/   ! do not modify:
!       ie: element number
!       iturn: turn number
!       nabs_total: not used
!
!******************************************************************************

      subroutine collimate_elense (elense_r_min, elense_curr,
     &  length, voltage, r2_ov_r1, center_x, center_y, 
     &  op_mode, tune, mult_tune,Delta_tune, step_tune, step_turns,
     &  resonant_turns, elens_jitter, elens_radial,
     &  x_in, xp_in, y_in, yp_in, 
     &  p_in, s_in, np,enom, lhit, part_abs, impact, indiv, lint, name)
    
       implicit none
     
       ! Parameters defined yet again...
       integer, parameter :: npart = 64
     
       double precision elense_r_min, elense_curr,curr,
     &                  length, voltage, r2_ov_r1, center_x,
     &                  center_y, tune,mult_tune,Delta_tune, step_tune
       logical         elens_jitter, elens_radial 
       integer          op_mode , step_turns,resonant_turns

       double precision x_in(npart), xp_in(npart),    
     &                  y_in(npart), yp_in(npart), p_in(npart),         &
     &                  s_in(npart), enom, impact(npart), indiv(npart), &
     &                  lint(npart),sigma

       logical    switch 
       real      rndm4
c     
       double precision p_radius, p_angle
       double precision, target :: coord(npart,6)
       double precision, pointer :: icoord(:)
     

       integer np, name(npart), lhit(npart), this_elense
       integer part_abs(npart)
       integer i_part
     
       ! p mass in GeV, defined yet again...
       
        
      !Common blocks
       
       integer ie,iturn,nabs_total, icoll

       common  /info/ ie,iturn,nabs_total
       
       common /icoll/  icoll

       !Big ugly GRD common block! WHY WHY WHY?
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
     &,physical,diffusive
      integer nloop,rnd_seed,c_offsettilt_seed,ibeam,jobnumber,         &
     &do_thisdis,n_slices,pencil_distr
      double precision myenom,mynex,mdex,myney,mdey,                    &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo,            &
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,ndr,                            &
     &driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,            &
     &sigsecut3,sigsecut2,enerror,bunchlength
      character*80 filename_dis
      character*24 name_sel
      character*80 coll_db
      character*16 castordir
      double precision, PARAMETER :: Pi = 3.14159265
      common /grd/ myenom,mynex,mdex,myney,mdey,                        &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo,            &
!
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
!
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,                                &
!
     &ndr,driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,        &
     &physical,diffusive,
     &sigsecut3,sigsecut2,enerror,                                      &
     &bunchlength,coll_db,name_sel,                                     &
     &castordir,filename_dis,nloop,rnd_seed,c_offsettilt_seed,          &
     &ibeam,jobnumber,do_thisdis,n_slices,pencil_distr,                 &
     &do_coll,                                                          &
!
     &do_select,do_nominal,dowrite_dist,do_oneside,dowrite_impact,      &
     &dowrite_secondary,dowrite_amplitude,radial,systilt_antisymm,      &
     &dowritetracks,cern,do_nsig,do_mingap
       ! Use c_aperture as inner diameter

       
       switch=.false.
       curr=elense_curr
       if (elens_jitter) then 
c               write(*,*)"elens jitter ON"
                curr=elense_curr+elense_curr*(rndm4()/100*2)
        endif

       if (rndm4() .le. 0.5) switch=.true.
 

       ! Switch to elense frame
       coord(:np,1) = x_in(:np)-center_x
       coord(:np,2) = xp_in(:np)
       coord(:np,3) = y_in(:np)-center_y
       coord(:np,4) = yp_in(:np)
       coord(:np,5) = s_in(:np) ! absolute s poisition!
       coord(:np,6) = p_in(:np) ! absolute energy in Gev!
     
c       call coord_transformation (.true., coord)
       
       ! main loop over all particles
       do i_part = 1, np
         ! don't loop over absorbed particles
         if (part_abs(i_part) .ne. 0) cycle
         ! initialize parameters
         impact(i_part) = -1d0
         lint(i_part)   = -1d0
         indiv(i_part)  = -1d0
     
         icoord => coord(i_part,:)

         !consider the particle coordinates at the middle of the elens 
         icoord(1) = icoord(1) + 0.5*length*icoord(2)
         icoord(3) = icoord(3) + 0.5*length*icoord(4)
       
         ! find if particle hits e lense field
         p_radius = sqrt(icoord(1)**2 + icoord(3)**2)
         p_angle = atan(icoord(3)/icoord(1))
         if (icoord(1).lt.0 ) then
               p_angle=p_angle+4*atan(1.)
         endif
       
         if (p_radius .ge. elense_r_min ) then
            call elense_kick(icoord,elense_r_min,p_radius,p_angle,
     &                         length,voltage,
     &                         r2_ov_r1, curr, op_mode, tune,mult_tune,
     &                         Delta_tune, step_tune, step_turns, 
     &                         resonant_turns,elens_radial, switch
     &                         )
            lhit(i_part)  =  100000000*ie + iturn
            indiv(i_part) = sqrt(icoord(2)**2 + icoord(4)**2)
            lint(i_part)  = length
            part_abs(i_part) = 0 ! particles never absorbed
            impact(i_part) = p_radius - elense_r_min
         endif
         ! In order to work with track6d routine have to first track to end of
         ! collimator. This is silly!
         icoord(1) = icoord(1) + 0.5*length*icoord(2)
         icoord(3) = icoord(3) + 0.5*length*icoord(4)
     
       enddo
       
       ! Switch back to accelerator frame
c       call coord_transformation (.false., coord)
       
       !transfer working coords to final coords
       x_in(:np)  = coord(:np,1) + center_x
       xp_in(:np) = coord(:np,2)
       y_in(:np)  = coord(:np,3) + center_y
       yp_in(:np) = coord(:np,4)
       s_in(:np)  = coord(:np,5)
       p_in(:np)  = coord(:np,6)
     
      end subroutine collimate_elense


!     ***
       subroutine elense_kick(coord, elense_r_min,p_radius, p_angle,
     &         hit_length,
     &         V, r2_ov_r1, curr,  
     &         op_mode, m_tune, mult_tune,
     &         Delta_tune, step, period, repetition,
     &         elens_radial,switch)
     
       implicit none
     
       double precision coord(6), r_kick, max_kick, Brho, hit_length
       double precision, intent(in) :: p_radius, p_angle,elense_r_min
       double precision gamma_pr, beta_pr, beta_el
       double precision coord2_temp, coord4_temp
       double precision phi
       double precision V,p_el, r2_ov_r1, curr     !parameters for 
                        !electron lens simplified. cathode voltage [KV], electron beam momentum [KeV], 
                        !ratio R1/R2, current [A]
       double precision f !shape function for elense kick

c     RADIAL PROFILE 
       logical :: elens_radial
       double precision r0,x0,y0 
       double precision r1,x1,y1 
       double precision r2,x2,y2 
       double precision r3,x3,y3 
       double precision r4,x4,y4 
       double precision n0,n1,n2,n3,ntot

c       OPERATION MODE 
!       LHC tune with octupoles H= .311->.312 ; 
!       LHC tune no octupoles H= .31->.3105 ; V=.32 ->.3205
!       SPS tune no octupoles H= .13 ->     ; V=.18 ->
c       
       integer op_mode

       logical :: AC 
       double precision  m_tune,mult_tune,Delta_tune,nstep, step
       double precision min_tune,op_tune
       integer period
c       
       logical :: random 
       logical :: switch
c
       logical :: resonant 
       integer  repetition 
c       
       logical write_c_out, write_SPS_out, write_elens_out
     &   , write_TM_QUAD_out
       integer ie,iturn,nabs_total, icoll

       double precision, PARAMETER :: Pi = 3.14159265
       double precision, PARAMETER :: e0=.00000000000885418781762
       double precision, PARAMETER :: c=299792458. 
       double precision, parameter :: pmap_gev = 0.938271998d0

       common  /info/ ie,iturn,nabs_total
       common /outputs/ write_c_out, write_SPS_out, write_elens_out
     &   , write_TM_QUAD_out

       p_el=V
       Brho = 3.33564095 * sqrt(coord(6)**2 - pmap_gev**2)
       gamma_pr = (coord(6)) / pmap_gev !convert momentum to GeV
       beta_pr = sqrt(1 - (1/gamma_pr**2))
       beta_el = sqrt(1 - 1/((511.+p_el)/511.)**2)
       max_kick = (2*hit_length*curr*(1+beta_el*beta_pr))
     &        /(4*(4*atan(1.))*e0*p_radius*Brho*beta_el*beta_pr*c**2)

       AC=.false.
       random=.false.
       resonant=.false.
       if (op_mode .eq. 1) then
c               write(*,*) "random"
               random=.true.
       elseif (op_mode .eq. 2) then
c               write(*,*) "AC"
               AC=.true.
       elseif (op_mode .eq.3) then
               write(*,*) "resonant"
               resonant=.true.
c               write(*,*) "DC"

       endif
       
       ! three regions
       if (p_radius .lt. elense_r_min) then ! hits elense
             f=0
             write(*,*) "elense warning! this should not happen..."     
       elseif (p_radius .le. (r2_ov_r1*elense_r_min)) then
             f=(p_radius**2-elense_r_min**2)/
     &             ( (r2_ov_r1*elense_r_min)**2-elense_r_min**2)
       else 
             f=1
       endif
       
       if (elens_radial) then
c               write(*,*)"elens radial!!"
         r0=445./2.
         x0=1*elense_r_min 
         y0=0.
         r1=505./2
         x1=r1/r0*elense_r_min 
         y1=917.
         r2=574./2
         x2=r2/r0*elense_r_min 
         y2=397.
         r3=729./2
         x3=r3/r0*elense_r_min 
         y3=228.
         r4=853./2
         x4=r4/r0*elense_r_min 
         y4=0.
          n0=((y1-y0)/(x1-x0)/3)*x1**3+(y0 -x0*(y1-y0)/(x1-x0))*x1**2/2 
     &   -(((y1-y0)/(x1-x0)/3)*x0**3+(y0 - x0*(y1-y0)/(x1-x0))*x0**2/2)
          n1=(y2-y1)/(x2-x1)*x2**3/3+(y1 - x1 *(y2-y1)/(x2-x1))*x2**2/2
     &    -((y2-y1)/(x2-x1)*x1**3/3+(y1 - x1 *(y2-y1)/(x2-x1))*x1**2/2)
          n2=(y3-y2)/(x3-x2)*x3**3/3+(y2 - x2 *(y3-y2)/(x3-x2))*x3**2/2
     &    -((y3-y2)/(x3-x2)*x2**3/3+(y2 - x2 *(y3-y2)/(x3-x2))*x2**2/2)
          n3=(y4-y3)/(x4-x3)*x4**3/3+(y3 - x3 *(y4-y3)/(x4-x3))*x4**2/2
     &    -((y4-y3)/(x4-x3)*x3**3/3+(y3 - x3 *(y4-y3)/(x4-x3))*x3**2/2)
          ntot=n0+n1+n2+n3
          if (p_radius.lt.x0) then
                 f=0
                 write(*,*) "elense warning! this should not happen..."     
         elseif (p_radius.lt.x1) then
                 f= (((y1-y0)/(x1-x0)/3)*p_radius**3+(y0 - x0 *(y1-y0)/
     &          (x1-x0))*p_radius**2/2 -(((y1-y0)/(x1-x0)/3)*x0**3+(y0 
     &          - x0 *(y1-y0)/(x1-x0))*x0**2/2))/ntot 

         elseif (p_radius.lt.x2) then
                 f= (n0 + (y2-y1)/(x2-x1)*p_radius**3/3+(y1 - x1
     &            *(y2-y1)/(x2-x1))*p_radius**2/2 - ((y2-y1)/(x2-x1)*
     &           x1**3/3+(y1 - x1*(y2-y1)/(x2-x1))*x1**2/2))/ntot 

         elseif (p_radius.lt.x3) then
                f=(n0+n1+(y3-y2)/(x3-x2)*p_radius**3/3+(y2 - x2
     &           *(y3-y2)/(x3-x2))*p_radius**2/2 - ((y3-y2)/(x3-x2)
     &           *x2**3/3+(y2 - x2*(y3-y2)/(x3-x2))*x2**2/2))/ntot
         elseif (p_radius.lt.x4) then
                f=(n0+n1+n2+(y4-y3)/(x4-x3)*p_radius**3/3+(y3 - x3
     &           *(y4-y3)/(x4-x3))*p_radius**2/2 - ((y4-y3)/(x4-x3)*
     &           x3**3/3+(y3 - x3*(y4-y3)/(x4-x3))*x3**2/2))/ntot    
         else 
                 f=1
         endif
      endif


       r_kick =  - f*max_kick !minus is because the kick in inward

       if (AC) then 
               if (step.ne.0 .and. Delta_tune .ne. 0) then
                        min_tune=m_tune-Delta_tune
                        nstep=(2*Delta_tune/step)+1
                        op_tune=
     &                    (min_tune+mod(floor(DBLE(iturn)/
     &                    DBLE(period)),(nstep))*step)
                else
                        op_tune=m_tune
               endif
               phi=mult_tune*(iturn*op_tune*2*(4*atan(1.)))
               r_kick=r_kick*0.5*(1+cos(phi))





       elseif  (switch .and. random) then 
               r_kick = 0
       elseif (resonant .and. mod(iturn,repetition).gt.0) then
               r_kick = 0
       endif

      

     
       coord2_temp = coord(2)
       coord4_temp = coord(4)
       ! Apply the kick in the appropriate ratios
       coord(2) = coord(2) + r_kick*cos(p_angle)
       coord(4) = coord(4) + r_kick*sin(p_angle)
       
       
       
       end subroutine elense_kick
     
     
     
     
