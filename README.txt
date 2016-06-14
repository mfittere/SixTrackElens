######## May 17, 2013
added quadrupole for tune modulation. 

#new format for TM_QUAD entries in the collimator database

1: STRING					NAME UPPERCASE 
2: STRING					name (lowercase)
3: DOUBLE					lenght [m]
4: DOUBLE					(quadrupole) angle of the focusing  plane [rad] 0=HOR
						(dipole) angle of the active plane [rad] 0=HOR
5: DOUBLE DOUBLE				center_x[m] center_y[m]
6: DOUBLE					(quadrupole) gradient kick per length unit (rad/m^2) 
						(dipole) kick per length unit (rad/m^2) 
7: DOUBLE DOUBLE    DOUBLE	DOUBLE 	INT 	operation tune, multiplication factor 
						Delta tune, tune increment per step
8: LOGIC				 	if true, the quad polarity can be inverted
8: DOUBLE					database beta x
9: DOUBLE 					database beta y
 
# change in fort.3 to activate detailed output for tune modulation quadrupole
line 10: LOGICAL    LOGICAL      INT        LOGICAL        STRING     LOGICAL    LOGICAL        LOGICAL           LOGICAL           LOGICAL         LOGICAL
	do_select   do_nominal   rnd_seed   dowrite_dist   name_sel   do_oneside dowrite_impact dowrite_secondary dowrite_amplitude write_elens_out write_TM_quad_out

##apr 23, 2013
funny results for 0 elens Voltage!
fixed by imposing a check on elens current & voltage before calling the elens routine

#Trying to Fix the bug regarding abs+surv particles are not equal to total particles

#mar 12, 2013
fixed the crystal energy treatment
working with D. Mirarchi

# Electron lens and tune modulation elements are treated as collimators by the code and are recognized by name and should be declared in collDB.
# Electron lens should have a name starting with ELENS
# Tubne modulation elements have a name starting with TM (to be double checked)
#new format for electron lens entry in colldb
1: STRING					NAME UPPERCASE 
2: STRING					name (lowercase)
3: DOUBLE					half aperture [sigma]
4: DOUBLE					electron lens lenght [m] (active length of straight section) 
5: DOUBLE					angle of the primary collimation plane [rad]. Only used to determine the sigma of the half opening in THIS plane
6: DOUBLE DOUBLE				center_x[m] center_y[m]
7: DOUBLE DOUBLE        			current[A], accelrating voltage of the electrons [kV] (used in code to calculate electron energy)
8: DOBLE					R2/R1 ratio
9: INT	  DOUBLE DOUBLE    DOUBLE	DOUBLE 	INT INT	special operation mode (0=DC, 1=random on/off on every turn, 2=AC (presently a sin[2w], but found that a square wave with freq. 2w is more efficient, w being the tune frequency), 
						operation tune w (middle value of tune range to be scanned for AC mode), 
						multiplication factor = multiple of w, fixed to 2 in latest version,
						Delta tune (whole range (or half?) of tunes to be scanned in AC mode), tune increment per step (for AC mode), turns per step (how many turns do you stay at each tune step in the tune range to be scanned for AC mode), 
10: LOGIC  LOGIC				2% current jitter on/off (random variation on the current on every turn sampled homogeneously in the range 0.98--1.02), radial profile on/off (use the perfect lens FALSE or the measured profile scaled to the relevant current TRUE)
11:DOUBLE					database beta x
12:DOUBLE 					database beta y


# change in fort.3 to accomodate the diffusion process

line 10: LOGICAL    LOGICAL      INT        LOGICAL        STRING     LOGICAL    LOGICAL        LOGICAL           LOGICAL           LOGICAL
	do_select   do_nominal   rnd_seed   dowrite_dist   name_sel   do_oneside dowrite_impact dowrite_secondary dowrite_amplitude write_elens_out

write_elens_out: activate debugging output to binary file elens.bin and elens.norm.bin. Contain all coordinates beofre and after the kick and the kick itself.

line 14: DOUBLE    DOUBLE   DOUBLE   LOGICAL           LOGICAL    LOGICAL
        driftsx   driftsy  cut_input systilt_antisymm  physical   diffusive

the new parameters are:
physical:    if true, the values of driftsx and driftsy are amplitude increase per turn in m. If false, the vales of driftsi are given in units of sigma
diffusive:   if true, the amplitude change per turn is ran[-1:1]*driftsx  and ran[-1:1]*driftsy (or could be Gaussian - to be checked). If false, a static deterministic increase of the radial amplitude is applied on every turn

#I have verified that the number of particles which are still running + the number of particles in the FLUKA_impacts is **NOT***
#the initial number of particles. I thought it was an error introduced by elens but then I verified it happen with the 
#previous database Sixtrack version. I supsect it comes from the eleminitation of the particles at high amplitude nut not absorbed. 
#It is clear when I run the code with only one collimator in.


#included triangular distribution between 2 extremes - meaning triangular in the amplitude space. 
#associated to distribution n. 5. 
#on momentum particles.
