!******************************************************************************
!
! VALENTINA PREVITALI - 2012
!
!
! subroutine 
! Parameters:
!  npart           -- total number of particles := 64
!  maxn            -- 20000
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
!  lhit(npart)     -- integer: element number*10000 + turn number for particle hit
!  part_abs(npart) -- integer: element number*10000 + turn number for particle absorption
!  impact(npart)   -- double precision: impact parameter if particle hits collimator (meters?)
!  indiv(npart)    -- double precision: angle of impact on collimator (radians?)
!  lint(npart)     -- double precision: length of collimator particle travelled through (meters?)
!  name(npart)     -- integer: particle id ?
!
! Output:
!  x_in(npart)     -- double precision: outgoing particle horizontal position (meters?)
!  xp_in(npart)    -- double precision: outgoing particle horizontal geometric angle (radians?)
!  y_in(npart)     -- double precision: outgoing particle vertical position (meters?)
!  yp_in(npart)    -- double precision: outgoing particle vertical geometric angle (radians)
!  p_in(npart)     -- double precision: outgoing particle momentum (Gev)
!  s_in(npart)     -- double precision: outgoing particle longitudinal position (meters?)
!  lhit(npart)     -- integer: element number*10000 + turn number for particle hit
!  part_abs(npart) -- integer: element number*10000 + turn number for particle absorption
!  impact(npart)   -- double precision: impact parameter if particle hits collimator (meters?)
!  indiv(npart)    -- double precision: angle of impact on collimator (radians?)
!  lint(npart)     -- double precision: length of collimator particle travelled through (meters?)
!
! Common blocks used:
!  /pencil/ ! I'll figure out the pencil beam stuff later
!  /pencile2/ !who two pencil common blocks
!  /icoll/  ! yet another common block used with pencil beam somehow
!  /info/   ! do not modify:
!       ie: element number
!       iturn: turn number
!       nabs_total: not used
!
!******************************************************************************

      subroutine collimate_tm ( gradient,
     &  length, orientation, center_x, center_y, 
     &  tune, mult_tune,Delta_tune, step_tune, step_turns,
     &  db_tm_switch, element_type,
     &  x_in, xp_in, y_in, yp_in, 
     &  p_in, s_in, np,enom) 
    
       implicit none
       integer np
     
       ! Parameters defined yet again...
       integer, parameter :: npart = 64

       double precision gradient,length, orientation,
     &                  center_x, center_y ,
     &                  tune,mult_tune,Delta_tune, step_tune
       integer          step_turns, element_type
       logical db_tm_switch

       double precision g_kick

       double precision x_in(npart), xp_in(npart),    
     &                  y_in(npart), yp_in(npart), p_in(npart),         &
     &                  s_in(npart), enom, impact(npart), indiv(npart), &
     &                  lint(npart),sigma

       double precision U,V,UP,VP
       real      rndm4
c     
       double precision, target :: coord(npart,6)
       double precision, pointer :: icoord(:)
     

       integer i_part
     
       ! p mass in GeV, defined yet again...
       
        
      !Common blocks
       
       integer ie,iturn,nabs_total, icoll

       common  /info/ ie,iturn,nabs_total
       
       common /icoll/  icoll

c       write(*,*) "tune modulation TM_QUAD, np = ", np
        g_kick=gradient*length
c       write(*,*) "TM_QUAD gradient ", gradient
c       write(*,*) "TM_QUAD lenght ", length
c       write(*,*) "TM_QUAD kick at 1 mm [urad] ", g_kick

       ! Switch to quad frame
       coord(:np,1) = x_in(:np)-center_x
       coord(:np,2) = xp_in(:np)
       coord(:np,3) = y_in(:np)-center_y
       coord(:np,4) = yp_in(:np)
       coord(:np,5) = s_in(:np) ! absolute s poisition!
       coord(:np,6) = p_in(:np) ! absolute energy in Gev!
     
       
       ! main loop over all particles
       do i_part = 1, np
         ! don't loop over absorbed particles
!         if (part_abs(i_part) .ne. 0) cycle
         ! initialize parameters
         impact(i_part) = -1d0
         lint(i_part)   = -1d0
         indiv(i_part)  = -1d0
     
         icoord => coord(i_part,:)

         !consider the particle coordinates at the middle of the elens 
         icoord(1) = icoord(1) + 0.5*length*icoord(2)
         icoord(3) = icoord(3) + 0.5*length*icoord(4)

         U=icoord(1) *cos(orientation)+icoord(3)*sin(orientation)
         V=icoord(1) * sin(orientation)+icoord(3)*cos(orientation)
         UP=icoord(2) *cos(orientation)+icoord(4)*sin(orientation)
         VP=icoord(2) *sin(orientation)+icoord(4)*cos(orientation)
       
         call TM_QUAD_KICK(  U,V,UP,VP,g_kick,element_type,
     &                         tune,mult_tune,
     &                         Delta_tune,step_tune,
     &                          step_turns,db_tm_switch
     &                         ,iturn)
         ! In order to work with track6d routine have to first track to end of
         ! collimator. This is silly!
         icoord(1)=U *cos(-orientation)+V*sin(-orientation)
         icoord(3)=V *cos(-orientation)+U*sin(-orientation)
         icoord(2)=UP*cos(-orientation)+VP*sin(-orientation)
         icoord(4)=VP*cos(-orientation)+UP*sin(-orientation)
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
     
      end subroutine collimate_tm


!     ***
       subroutine TM_QUAD_KICK(  U,V,UP,VP,g_kick, element_type,
     &                         m_tune,mult_tune,
     &                         Delta_tune, step, period, 
     &                         db_tm_switch,iturn)
     
       implicit none
     
       double precision U,V,UP,VP
       double precision g_kick
       double precision U_kick,V_kick
       double precision phi

       double precision  m_tune,mult_tune,Delta_tune,nstep, step
       double precision min_tune,op_tune
       integer period, iturn, element_type
       logical db_tm_switch
c       
c       integer ie,iturn,nabs_total, icoll

c       double precision, PARAMETER :: Pi = 3.14159265
c       double precision, PARAMETER :: e0=.00000000000885418781762
c       double precision, PARAMETER :: c=299792458. 
c       double precision, parameter :: pmap_gev = 0.938271998d0

c       Brho = 3.33564095 * sqrt(coord(6)**2 - pmap_gev**2)
c       gamma_pr = (coord(6)) / pmap_gev !convert momentum to GeV
c       beta_pr = sqrt(1 - (1/gamma_pr**2))
c       beta_el = sqrt(1 - 1/((511.+p_el)/511.)**2)
c       max_kick = (2*hit_length*curr*(1+beta_el*beta_pr))
c     &        /(4*(4*atan(1.))*e0*p_radius*Brho*beta_el*beta_pr*c**2)


       if ( element_type .eq. 2 ) then
                U_kick=g_kick*U
                V_kick=-g_kick*V
       elseif ( element_type .eq. 1 ) then
                U_kick=g_kick
                V_kick=0
        else
                write(*,*) "Only dipoles and quads are set for tune mod"
                stop
       endif             


       if (step.ne.0 .and. Delta_tune .ne. 0) then
                    min_tune=m_tune-Delta_tune
                    nstep=(2*Delta_tune/step)+1
                    op_tune=
     &                (min_tune+mod(floor(DBLE(iturn)/
     &                DBLE(period)),(nstep))*step)
            else
                    op_tune=m_tune
       endif
c       if ( mod(DBLE(iturn),DBLE(period)) .eq. 0 ) 
c     &      write(*,*)"turn and operation tune:",
c     &           iturn, op_tune*mult_tune
       phi=mult_tune*(iturn*op_tune*2*(4*atan(1.)))
       if (element_type .eq. 1) then
                U_kick=U_kick*(cos(phi))
       elseif ( element_type .eq. 2) then 
                if (db_tm_switch) then
                        U_kick=U_kick*(cos(phi))
                        V_kick=V_kick*(cos(phi))
                else
                        U_kick=U_kick*0.5*(1+cos(phi))
                        V_kick=V_kick*0.5*(1+cos(phi))
                endif
        else
                write(*,*) "Only dipoles and quads are set for tune mod"
                stop
        endif
     
       UP=UP+U_kick
       VP=VP+V_kick
       
       end subroutine TM_QUAD_KICK
     
     
     
     
