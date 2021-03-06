all:    SixTrack-last 
SixTrack-last:	track.o sixve.o sixvefox.o lielib.o dabnews.o plotdumy.o flushdum.o elense.o crystaltrack.o crystal.o tune_mod.o 
	lf95 -g -Wa,--static -m32 -pedant track.o \
	sixve.o \
	sixvefox.o     \
	lielib.o     \
	dabnews.o    \
	plotdumy.o      \
	flushdum.o      \
	elense.o	\
	crystaltrack.o	\
	crystal.o	\
	tune_mod.o 	\
	-o SixTrack-last
track.o:	 source/track.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/track.f
sixve.o:  	source/sixve.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/sixve.f
sixvefox.o:  	 source/sixvefox.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/sixvefox.f
lielib.o:  	 source/lielib.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/lielib.f
dabnews.o:  	 source/dabnews.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/dabnews.f
plotdumy.o:  	 source/plotdumy.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/plotdumy.f
flushdum.o:  	 source/flushdum.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/flushdum.f
elense.o:  	 source/elense.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/elense.f
tune_mod.o:  	 source/tune_mod.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/tune_mod.f
crystaltrack.o:  	 source/crystaltrack.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/crystaltrack.f
crystal.o:  	 source/crystal.f
	/afs/cern.ch/sw/fortran/lahey/lf9562/bin/ -g -c  -Wa,--32    source/crystal.f
clean: 
	./clean.sh

