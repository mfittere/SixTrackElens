!******************************************************************************
!
! JCSMITH SEPT2008
!
! Suborutine collimate_elense (elense_thickness, elense_j_e, c_length,  &
!   c_rotation, c_aperture, c_offset, c_tilt, x_in,                     &
!   xp_in, y_in, yp_in, p_in, s_in, np, enom, lhit, part_abs, impact,   &
!   indiv, lint, name)
!
! Calculates radial kick of beam passing through electron lense. Assumes a
! radially symmetric lense and no slicing. Does not address fringe field. Applies
! kick at center of element, it's a thin lense, no actual tracking is performed.
!
! c_rotation doesn't realy change the e lense other than change how the tilt is
! aplied.
!
! from: V. Shiltsev, et al. "LHC Particle Colimation by Hollow Electron Beams"
!  EPAC08 MOPC098
!
! Parameters:
!  npart           -- total number of particles := 64
!  maxn            -- 20000
!                  
! Input:           
!  c_length        -- double precision: Total length of Collimator (meters)
!  c_rotation      -- double precision: rotation of colimator from x-axis (radians)
!  C_aperture      -- double precision: Collimation full aperture (meters)
!  c_offset        -- double precision: Collimator offset (meters)
!  c_tilt(2)       -- double precision: Tilt of collimator from x-axis (radians)
!  x_in(npart)     -- double precision: incoming particle horizontal position (meters?)
!  xp_in(npart)    -- double precision: incoming particle horizontal geometric angle (radians?)
!  y_in(npart)     -- double precision: incoming particle vertical position (meters?)
!  yp_in(npart)    -- double precision: incoming particle vertical geometric angle (radians)
!  p_in(npart)     -- double precision: incoming particle momentum (GeV)
!  s_in(npart)     -- double precision: incoming particle longitudinal position (meters)
!                      NOTE: all particles start at same longitudinal position!
!  np              -- integer: total number of tracked particles (.le. npart)
!  enom            -- double precision: reference energy (MeV)
!  name(npart)     -- integer: name of collimator?
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

      subroutine collimate_elense (elense_thickness, elense_j_e,        &
     &  c_length, c_rotation, c_aperture,                               &
     &  c_offset, c_tilt, x_in, xp_in, y_in, yp_in, p_in, s_in, np,     &
     &  enom, lhit, part_abs, impact, indiv, lint, name)
     
       implicit none
     
       ! Parameters defined yet again...
       integer, parameter :: npart = 64
     
       double precision elense_thickness, elense_j_e,                   &
     &                  c_length, c_rotation, c_aperture, c_offset,     &
     &                  c_tilt(2), x_in(npart), xp_in(npart),           &
     &                  y_in(npart), yp_in(npart), p_in(npart),         &
     &                  s_in(npart), enom, impact(npart), indiv(npart), &
     &                  lint(npart)
     
       double precision p_radius, p_angle, elense_radius
       double precision, target :: coord(npart,6)
       double precision, pointer :: icoord(:)
     

       integer np, name(npart), lhit(npart), this_elense
       integer part_abs(npart)
       integer i_part
     
       ! p mass in GeV, defined yet again...
       double precision, parameter :: pmap_gev = 0.938271998d0
       
       double precision elense_r_min, elense_r_max

      !Common blocks
       
       integer ie,iturn,nabs_total, icoll

       common  /info/ ie,iturn,nabs_total
       
       common /icoll/  icoll

       !Big ugly GRD common block! WHY WHY WHY?
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
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
      REAL, PARAMETER :: Pi = 3.14159265
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
     &sigsecut3,sigsecut2,enerror,                                      &
     &bunchlength,coll_db,name_sel,                                     &
     &castordir,filename_dis,nloop,rnd_seed,c_offsettilt_seed,          &
     &ibeam,jobnumber,do_thisdis,n_slices,pencil_distr,                 &
     &do_coll,                                                          &
!
     &do_select,do_nominal,dowrite_dist,do_oneside,dowrite_impact,      &
     &dowrite_secondary,dowrite_amplitude,radial,systilt_antisymm,      &
     &dowritetracks,cern,do_nsig,do_mingap
c       write(*,*) "elense routine"
         icoord(1) = icoord(1) + c_length*icoord(2)
         icoord(3) = icoord(3) + c_length*icoord(4)
     
      end subroutine collimate_elense
     
