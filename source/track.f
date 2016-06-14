      subroutine trauthin(nthinerr)
!--------------------------------------------------------------------------
!
!  TRACK THIN LENS PART
!
!
!  F. SCHMIDT
!
!
!  CHANGES FOR COLLIMATION MADE BY G. ROBERT-DEMOLAIZE, October 29th, 2004
!--------------------------------------------------------------------------
      implicit none
      integer i,ix,j,jb,jj,jx,kpz,kzz,napx0,nbeaux,nmz,nthinerr
      integer n
      double precision benkcc,cbxb,cbzb,cikveb,crkveb,crxb,crzb,r0,r000,&
     &r0a,r2b,rb,rho2b,rkb,tkb,xbb,xrb,zbb,zrb
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      double precision cc,xlim,ylim
      parameter(cc = 1.12837916709551d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),    &
     &r2b(npart),rb(npart),rkb(npart),                                  &
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),          &
     &crzb(npart),cbxb(npart),cbzb(npart)
      dimension nbeaux(nbb)
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
      integer   mynp
      common /mynp/ mynp
!
      logical cut_input
      common /cut/ cut_input
!
!++ Vectors of coordinates
      double precision myemitx,mygammax,myemity,mygammay,xsigmax,ysigmay
!
      real rndm4
!
      character*80 dummy
!
      double precision remitxn,remityn,remitx,remity
      common  /remit/ remitxn, remityn, remitx, remity
!
      double precision mux(nblz),muy(nblz)
      common /mu/ mux,muy
!
      double precision ielem,iclr,grd
      character*80 ch
      character*160 ch1
      logical flag
!
      integer k,np0,rnd_lux,rnd_k1,rnd_k2
!
      double precision ax0,ay0,bx0,by0,mux0,muy0,nspx,nspy
!
      double precision xbob(nblz),ybob(nblz),xpbob(nblz),ypbob(nblz),   &
     &xineff(npart),yineff(npart),xpineff(npart),ypineff(npart)
!
      common /xcheck/ xbob,ybob,xpbob,ypbob,xineff,yineff,xpineff,      &
     &ypineff
!
      integer   mclock_liar
!
      character*160 cmd
      character*160 cmd2
      character*1 ch0
      character*2 ch00
      character*3 ch000
      character*4 ch0000
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!GRD
!GRD THIS BLOC IS COMMON TO MAINCR, DATEN, TRAUTHIN AND THIN6D
!GRD
!APRIL2005
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
     &,relative, diffusive
!
      integer nloop,rnd_seed,c_offsettilt_seed,ibeam,jobnumber,         &
     &do_thisdis,n_slices,pencil_distr
      double precision myenom,mynex,mdex,myney,mdey,                    &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
!SEPT2005 add these lines for the slicing procedure
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
!SEPT2005,OCT2006 added offset
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,ndr,                            &
     &driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,            &
     &sigsecut3,sigsecut2,enerror,bunchlength
!
      character*24 name_sel
      character*80 coll_db
      character*16 castordir
!JUNE2005
      character*80 filename_dis
!JUNE2005
      common /grd/ myenom,mynex,mdex,myney,mdey,                        &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
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
     & relative,diffusive,
     &sigsecut3,sigsecut2,enerror,                                      &
     &bunchlength,coll_db,name_sel,                                     &
     &castordir,filename_dis,nloop,rnd_seed,c_offsettilt_seed,          &
     &ibeam,jobnumber,do_thisdis,n_slices,pencil_distr,                 &
     &do_coll,                                                          &
!
     &do_select,do_nominal,dowrite_dist,do_oneside,dowrite_impact,      &
     &dowrite_secondary,dowrite_amplitude,radial,systilt_antisymm,      &
     &dowritetracks,cern,do_nsig,do_mingap
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
! THIS BLOCK IS COMMON TO BOTH THIN6D AND TRAUTHIN SUBROUTINES
!
      integer ieff
!
      double precision myemitx0,myemity0,myalphay,mybetay,myalphax,     &
     &mybetax,rselect
      common /ralph/ myemitx0,myemity0,myalphax,myalphay,mybetax,       &
     &mybetay,rselect
!
      integer absorbed(npart),counted(npart,numeff)
      double precision neff(numeff),rsig(numeff)
      common  /eff/ neff,rsig,counted,absorbed
!
      integer  nimpact(50)
      double precision sumimpact(50),sqsumimpact(50)
      common  /rimpact/ sumimpact,sqsumimpact,nimpact
!
      integer  nampl(nblz)
      character*16  ename(nblz)
      double precision sum_ax(nblz),sqsum_ax(nblz),sum_ay(nblz),        &
     &sqsum_ay(nblz),sampl(nblz)
      common  /ampl_rev/ sum_ax,sqsum_ax,sum_ay,sqsum_ay,sampl,ename,   &
     &nampl
!
      double precision neffx(numeff),neffy(numeff)
      common /efficiency/ neffx,neffy
!
      integer part_hit(maxn),part_abs(maxn),n_tot_absorbed,n_absorbed   &
     &,part_select(maxn)
      double precision part_impact(maxn)
      common /stats/ part_impact,part_hit,part_abs
      common /n_tot_absorbed/ n_tot_absorbed,n_absorbed
      common /part_select/ part_select
!
      double precision x00(maxn),xp00(maxn),y00(maxn),yp00(maxn)
      common   /beam00/ x00,xp00,y00,yp00
!
      logical firstrun
      common /firstrun/ firstrun
!
      integer nsurvive_end,num_selhit,n_impact
      integer nsurvive(10000000)
      common /outcoll/ nsurvive,num_selhit,n_impact,nsurvive_end
!
      integer napx00
      common /napx00/ napx00
!
      integer  icoll
      common  /icoll/  icoll


!
      integer db_ncoll
!
      character*16 db_name1(max_ncoll),db_name2(max_ncoll)
      character*6 db_material(max_ncoll)
      double precision db_nsig(max_ncoll),db_length(max_ncoll),         &
     &db_offset(max_ncoll),db_rotation(max_ncoll),                      &
     &db_bx(max_ncoll),db_by(max_ncoll),db_tilt(max_ncoll,2),           &
     &db_elense_thickness(max_ncoll),db_elense_j_e(max_ncoll)
     &,db_cry_rcurv(max_ncoll),db_cry_rmax(max_ncoll),                  &
     &db_cry_zmax(max_ncoll),db_cry_alayer(max_ncoll),                  &
     &db_cry_orient(max_ncoll),db_cry_tilt(max_ncoll)
     &,db_miscut(max_ncoll)
     &,db_elens_center_x(max_ncoll),db_elens_center_y(max_ncoll),
     &db_elens_curr(max_ncoll),db_elens_voltage(max_ncoll),
     &db_elens_r2_ov_r1(max_ncoll),
     &db_elens_tune(max_ncoll),
     &db_elens_mult_tune(max_ncoll),db_elens_delta_tune(max_ncoll),
     &db_elens_step_tune(max_ncoll)
     &,db_tm_center_x(max_ncoll), db_tm_center_y(max_ncoll)
     &,db_tm_kick(max_ncoll),db_tm_tune(max_ncoll)
     &,db_tm_mult_tune(max_ncoll),db_tm_delta_tune(max_ncoll)
     &,db_tm_step_tune(max_ncoll)
    
      integer db_elens_op_mode(max_ncoll),
     & db_elens_step_turns(max_ncoll),
     & db_elens_resonant_turns(max_ncoll)
     & , db_tm_step_turns(max_ncoll)

      logical  db_elens_jitter(max_ncoll),db_elens_radial(max_ncoll)
     & , db_tm_switch(max_ncoll)

      common /colldatabase/ db_nsig,db_length,db_rotation,db_offset,    &
     &db_bx,db_by,db_tilt,db_name1,db_name2,db_material,db_ncoll,       &
     &db_elense_thickness,db_elense_j_e
     &,db_cry_rcurv,db_cry_rmax,db_cry_zmax,db_cry_alayer,db_cry_orient,&
     &db_cry_tilt,db_miscut 
     &,db_elens_center_x,db_elens_center_y,        
     & db_elens_curr,db_elens_voltage,db_elens_r2_ov_r1,
     &db_elens_op_mode,db_elens_tune,
     &db_elens_mult_tune,db_elens_delta_tune,
     &db_elens_step_tune,db_elens_step_turns,db_elens_resonant_turns,
     &db_elens_jitter,db_elens_radial
     &,db_tm_center_x, db_tm_center_y
     &,db_tm_kick,db_tm_tune
     &,db_tm_mult_tune,db_tm_delta_tune
     &,db_tm_step_tune, db_tm_step_turns
     & , db_tm_switch
!
      integer cn_impact(max_ncoll),cn_absorbed(max_ncoll)
      double precision caverage(max_ncoll),csigma(max_ncoll)
      common /collsummary/ caverage,csigma,cn_impact,cn_absorbed
!
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn)
      common /coord/ myx,myxp,myy,myyp,myp,mys
!
      integer counted_r(maxn,numeff),counted_x(maxn,numeff),            &
     &counted_y(maxn,numeff),                                           &
     &ieffmax_r(npart),ieffmax_x(npart),ieffmax_y(npart)
      common /counting/ counted_r,counted_x,counted_y,ieffmax_r,        &
     &ieffmax_x, ieffmax_y
!
      integer secondary(maxn),tertiary(maxn),other(maxn),               &
     &part_hit_before(maxn)
      double precision part_indiv(maxn),part_linteract(maxn)
!
      integer   samplenumber
      character*4 smpl
      character*80 pfile
      common /samplenumber/ pfile,smpl,samplenumber
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
! THIS BLOCK IS COMMON TO WRITELIN,LINOPT,TRAUTHIN,THIN6D AND MAINCR
!
      double precision tbetax(nblz),tbetay(nblz),talphax(nblz),         &
     &talphay(nblz),torbx(nblz),torbxp(nblz),torby(nblz),torbyp(nblz),  &
     &tdispx(nblz),tdispy(nblz)
!
      common /rtwiss/ tbetax,tbetay,talphax,talphay,torbx,torbxp,       &
     &torby,torbyp,tdispx,tdispy
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
! THIS BLOCK IS COMMON TO THIN6D, TRAUTHIN, COLLIMATE32 AND MAINCR
!
      integer ipencil
      double precision xp_pencil0,yp_pencil0,x_pencil(max_ncoll),       &
     &y_pencil(max_ncoll),pencil_dx(max_ncoll)
      common  /pencil/  xp_pencil0,yp_pencil0,pencil_dx,ipencil
      common  /pencil2/ x_pencil, y_pencil
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      integer ie,iturn,nabs_total
      common  /info/ ie,iturn,nabs_total
!
!
! SEPT2008: valentina add flags for debug output
!
      logical write_c_out, write_SPS_out, write_elens_out
     &     ,write_TM_out  
      common /outputs/ write_c_out, write_SPS_out, write_elens_out
     &     ,write_TM_out  


      double precision xdebug(nblz),xdebugN(nblz),xpdebug(nblz),
     & xpdebugN(nblz),
     & ydebug(nblz),ydebugN(nblz),ypdebug(nblz),ypdebugN(nblz) 
      common /debugvale/xdebug,xdebugN,xpdebug,xpdebugN,
     &ydebug,ydebugN,ypdebug,ypdebugN 
      
      save
!-----------------------------------------------------------------------
      write_c_out= .false. !valentina
c
c
c     
      do 5 i=1,npart
        nlostp(i)=i
   5  continue
      do 10 i=1,nblz
        ktrack(i)=0
        strack(i)=zero
        strackc(i)=zero
        stracks(i)=zero
   10 continue
!--beam-beam element
      if(nbeam.ge.1) then
        do 15 i=1,nbb
          nbeaux(i)=0
   15   continue
        do i=1,iu
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(kz(ix).eq.20.and.parbe(ix,2).eq.0) then
!--round beam
              if(sigman(1,imbb(i)).eq.sigman(2,imbb(i))) then
                if(nbeaux(imbb(i)).eq.2.or.nbeaux(imbb(i)).eq.3) then
                  call prror(89)
                else
                  nbeaux(imbb(i))=1
                  sigman2(1,imbb(i))=sigman(1,imbb(i))**2
                endif
              endif
!--elliptic beam x>z
              if(sigman(1,imbb(i)).gt.sigman(2,imbb(i))) then
                if(nbeaux(imbb(i)).eq.1.or.nbeaux(imbb(i)).eq.3) then
                  call prror(89)
                else
                  nbeaux(imbb(i))=2
                  sigman2(1,imbb(i))=sigman(1,imbb(i))**2
                  sigman2(2,imbb(i))=sigman(2,imbb(i))**2
                  sigmanq(1,imbb(i))=sigman(1,imbb(i))/sigman(2,imbb(i))
                  sigmanq(2,imbb(i))=sigman(2,imbb(i))/sigman(1,imbb(i))
                endif
              endif
!--elliptic beam z>x
              if(sigman(1,imbb(i)).lt.sigman(2,imbb(i))) then
                if(nbeaux(imbb(i)).eq.1.or.nbeaux(imbb(i)).eq.2) then
                  call prror(89)
                else
                  nbeaux(imbb(i))=3
                  sigman2(1,imbb(i))=sigman(1,imbb(i))**2
                  sigman2(2,imbb(i))=sigman(2,imbb(i))**2
                  sigmanq(1,imbb(i))=sigman(1,imbb(i))/sigman(2,imbb(i))
                  sigmanq(2,imbb(i))=sigman(2,imbb(i))/sigman(1,imbb(i))
                endif
              endif
            endif
          endif
        enddo
      endif
      
      do 290 i=1,iu
        if(mout2.eq.1.and.i.eq.1) call write4
        ix=ic(i)
        if(ix.gt.nblo) goto 30
        ktrack(i)=1
        do 20 jb=1,mel(ix)
          jx=mtyp(ix,jb)
          strack(i)=strack(i)+el(jx)
   20   continue
        if(abs(strack(i)).le.pieni) ktrack(i)=31
        goto 290
   30   ix=ix-nblo
        kpz=abs(kp(ix))
        if(kpz.eq.6) then
          ktrack(i)=2
          goto 290
        endif
   40   kzz=kz(ix)
        if(kzz.eq.0) then
          ktrack(i)=31
          goto 290
        endif
!--beam-beam element
        if(kzz.eq.20.and.nbeam.ge.1.and.parbe(ix,2).eq.0) then
          strack(i)=crad*ptnfac(ix)
          if(abs(strack(i)).le.pieni) then
            ktrack(i)=31
            goto 290
          endif
          if(nbeaux(imbb(i)).eq.1) then
            ktrack(i)=41
            if(ibeco.eq.1) then
              do 42 j=1,napx
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)                                       &
     &goto 42
            tkb(j)=rho2b(j)/(two*sigman2(1,imbb(i)))
                beamoff(4,imbb(i))=strack(i)*crkveb(j)/rho2b(j)*        &
     &(one-exp(-tkb(j)))
                beamoff(5,imbb(i))=strack(i)*cikveb(j)/rho2b(j)*        &
     &(one-exp(-tkb(j)))
   42         continue
            endif
          endif
          if(nbeaux(imbb(i)).eq.2) then
            ktrack(i)=42
            if(ibeco.eq.1) then
            if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              beamoff(4,imbb(i))=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))* &
     &sign(one,crkveb(j))
              beamoff(5,imbb(i))=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))* &
     &sign(one,cikveb(j))
            enddo
            else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do j=1,napx
              beamoff(4,imbb(i))=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))* &
     &sign(one,crkveb(j))
              beamoff(5,imbb(i))=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))* &
     &sign(one,cikveb(j))
            enddo
            endif
            endif
          endif
          if(nbeaux(imbb(i)).eq.3) then
            ktrack(i)=43
            if(ibeco.eq.1) then
            if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              beamoff(4,imbb(i))=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))* &
     &sign(one,crkveb(j))
              beamoff(5,imbb(i))=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))* &
     &sign(one,cikveb(j))
            enddo
            else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do j=1,napx
              beamoff(4,imbb(i))=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))* &
     &sign(one,crkveb(j))
              beamoff(5,imbb(i))=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))* &
     &sign(one,cikveb(j))
            enddo
            endif
            endif
          endif
          goto 290
!--Hirata's 6D beam-beam kick
        else if(kzz.eq.20.and.parbe(ix,2).gt.0) then
          ktrack(i)=44
          parbe(ix,4)=-crad*ptnfac(ix)*half*c1m6
          if(ibeco.eq.1) then
            track6d(1,1)=ed(ix)*c1m3
            track6d(2,1)=zero
            track6d(3,1)=ek(ix)*c1m3
            track6d(4,1)=zero
            track6d(5,1)=zero
            track6d(6,1)=zero
            napx0=napx
            napx=1
            call beamint(napx,track6d,parbe,sigz,bbcu,imbb(i),ix,ibtyp, &
     &ibbc)
            beamoff(1,imbb(i))=track6d(1,1)*c1e3
            beamoff(2,imbb(i))=track6d(3,1)*c1e3
            beamoff(4,imbb(i))=track6d(2,1)*c1e3
            beamoff(5,imbb(i))=track6d(4,1)*c1e3
            beamoff(6,imbb(i))=track6d(6,1)
            napx=napx0
          endif
          goto 290
        endif
        if(kzz.eq.15) then
          ktrack(i)=45
          goto 290
        endif
        if(kzz.eq.16) then
          ktrack(i)=51
          goto 290
        else if(kzz.eq.-16) then
          ktrack(i)=52
          goto 290
        endif
        if(kzz.eq.22) then
          ktrack(i)=3
          goto 290
        endif
        if(mout2.eq.1.and.icextal(i).ne.0) then
          write(27,'(a16,2x,1p,2d14.6,d17.9)') bez(ix),extalign(i,1),   &
     &extalign(i,2),extalign(i,3)
        endif
        if(kzz.lt.0) goto 180
        goto(50,60,70,80,90,100,110,120,130,140,150),kzz
        ktrack(i)=31
        goto 290
   50   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=11
        strack(i)=smiv(1,i)*c1e3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   60   if(abs(smiv(1,i)).le.pieni.and.abs(ramp(ix)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=12
        strack(i)=smiv(1,i)
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   70   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=13
        strack(i)=smiv(1,i)*c1m3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   80   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=14
        strack(i)=smiv(1,i)*c1m6
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   90   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=15
        strack(i)=smiv(1,i)*c1m9
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  100   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=16
        strack(i)=smiv(1,i)*c1m12
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  110   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=17
        strack(i)=smiv(1,i)*c1m15
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  120   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=18
        strack(i)=smiv(1,i)*c1m18
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  130   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=19
        strack(i)=smiv(1,i)*c1m21
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  140   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=20
        strack(i)=smiv(1,i)*c1m24
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  150   r0=ek(ix)
        nmz=nmu(ix)
        if(abs(r0).le.pieni.or.nmz.eq.0) then
          if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).le.pieni) then
            ktrack(i)=31
          else if(abs(dki(ix,1)).gt.pieni.and.abs(dki(ix,2)).le.pieni)  &
     &then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=33
              strack(i)=dki(ix,1)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=35
              strack(i)=dki(ix,1)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          else if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).gt.pieni)  &
     &then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=37
              strack(i)=dki(ix,2)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=39
              strack(i)=dki(ix,2)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          endif
        else
          if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).le.pieni) then
            ktrack(i)=32
          else if(abs(dki(ix,1)).gt.pieni.and.abs(dki(ix,2)).le.pieni)  &
     &then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=34
              strack(i)=dki(ix,1)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=36
              strack(i)=dki(ix,1)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          else if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).gt.pieni)  &
     &then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=38
              strack(i)=dki(ix,2)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=40
              strack(i)=dki(ix,2)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          endif
        endif
        if(abs(r0).le.pieni.or.nmz.eq.0) goto 290
        if(mout2.eq.1) then
          benkcc=ed(ix)*benkc(irm(ix))
          r0a=one
          r000=r0*r00(irm(ix))
          do 160 j=1,mmul
            fake(1,j)=bbiv(j,1,i)*r0a/benkcc
            fake(2,j)=aaiv(j,1,i)*r0a/benkcc
  160     r0a=r0a*r000
          write(9,'(a16)') bez(ix)
          write(9,'(1p,3d23.15)') (fake(1,j), j=1,3)
          write(9,'(1p,3d23.15)') (fake(1,j), j=4,6)
          write(9,'(1p,3d23.15)') (fake(1,j), j=7,9)
          write(9,'(1p,3d23.15)') (fake(1,j), j=10,12)
          write(9,'(1p,3d23.15)') (fake(1,j), j=13,15)
          write(9,'(1p,3d23.15)') (fake(1,j), j=16,18)
          write(9,'(1p,2d23.15)') (fake(1,j), j=19,20)
          write(9,'(1p,3d23.15)') (fake(2,j), j=1,3)
          write(9,'(1p,3d23.15)') (fake(2,j), j=4,6)
          write(9,'(1p,3d23.15)') (fake(2,j), j=7,9)
          write(9,'(1p,3d23.15)') (fake(2,j), j=10,12)
          write(9,'(1p,3d23.15)') (fake(2,j), j=13,15)
          write(9,'(1p,3d23.15)') (fake(2,j), j=16,18)
          write(9,'(1p,2d23.15)') (fake(2,j), j=19,20)
          do 170 j=1,20
            fake(1,j)=zero
  170     fake(2,j)=zero
        endif
        goto 290
  180   kzz=-kzz
        goto(190,200,210,220,230,240,250,260,270,280),kzz
        ktrack(i)=31
        goto 290
  190   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=21
        strack(i)=smiv(1,i)*c1e3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  200   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=22
        strack(i)=smiv(1,i)
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  210   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=23
        strack(i)=smiv(1,i)*c1m3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  220   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=24
        strack(i)=smiv(1,i)*c1m6
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  230   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=25
        strack(i)=smiv(1,i)*c1m9
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  240   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=26
        strack(i)=smiv(1,i)*c1m12
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  250   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=27
        strack(i)=smiv(1,i)*c1m15
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  260   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=28
        strack(i)=smiv(1,i)*c1m18
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  270   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=29
        strack(i)=smiv(1,i)*c1m21
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  280   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=30
        strack(i)=smiv(1,i)*c1m24
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
  290 continue

        
      do 300 j=1,napx
        dpsv1(j)=dpsv(j)*c1e3/(one+dpsv(j))
  300 continue
      nwri=nwr(3)
      if(nwri.eq.0) nwri=numl+numlr+1
      if(idp.eq.0.or.ition.eq.0) then
        call thin4d(nthinerr)
      else
        hsy(3)=c1m3*hsy(3)*ition
        do 310 jj=1,nele
          if(kz(jj).eq.12) hsyc(jj)=c1m3*hsyc(jj)*itionc(jj)
  310   continue
        if(abs(phas).ge.pieni) then
          call thin6dua(nthinerr)
        else
      open(unit=outlun, file='colltrack.out')
!
      write(*,*)
      write(*,*) '         -------------------------------'
      write(*,*)
      write(*,*) '          Program      C O L L T R A C K '
      write(*,*)
      write(*,*) '            R. Assmann           -    AB/ABP'
      write(*,*) '            C. Bracco            -    AB/ABP'
      write(*,*) '            V. Previtali         -    AB/ABP'
      write(*,*) '            S. Redaelli          -    AB/OP'
      write(*,*) '            G. Robert-Demolaize  -    AB/ABP'
      write(*,*) '            T. Weiler            -    AB/ABP'
      write(*,*)
      write(*,*) '                 CERN 2001 - 2007'
      write(*,*)
      write(*,*) '         -------------------------------'
      write(*,*)
      write(*,*)
      write(outlun,*)
      write(outlun,*)
      write(outlun,*) '       -------------------------------'
      write(outlun,*)
      write(outlun,*) '       Program      C O L L T R A C K '
      write(outlun,*)
      write(outlun,*) '          R. Assmann             -    AB/ABP'
      write(outlun,*) '          C. Bracco              -    AB/ABP'
      write(outlun,*) '          V. Previtali           -    AB/ABP'
      write(outlun,*) '          S. Redaelli            -    AB/OP'
      write(outlun,*) '          G. Robert-Demolaize    -    AB/ABP'
      write(outlun,*) '          T. Weiler              -    AB/ABP'
      write(outlun,*)
      write(outlun,*) '                 CERN 2001 - 2007'
      write(outlun,*)
      write(outlun,*) '         -------------------------------'
      write(outlun,*)
      write(outlun,*)
!
      write(*,*)
      write(*,*) 'Collimation version of Sixtrack running... 10/2005'
      write(*,*)
      write(*,*) '                     R. Assmann, F. Schmidt, CERN'
      write(*,*) '                           S. Redaelli,      CERN'
      write(*,*) '                       G. Robert-Demolaize,  CERN'
      write(*,*)
      write(*,*) 'Generating particle distribution at FIRST element!'
      write(*,*) 'Optical functions obtained from Sixtrack internal!'
      write(*,*) 'Emittance and energy obtained from Sixtrack input!'
      write(*,*)
      write(*,*)
      write(*,*) 'Info: Betax0   [m]    ', tbetax(1)
      write(*,*) 'Info: Betay0   [m]    ', tbetay(1)
      write(*,*) 'Info: Alphax0         ', talphax(1)
      write(*,*) 'Info: Alphay0         ', talphay(1)
      write(*,*) 'Info: Orbitx0  [mm]   ', torbx(1)
      write(*,*) 'Info: Orbitxp0 [mrad] ', torbxp(1)
      write(*,*) 'Info: Orbity0  [mm]   ', torby(1)
      write(*,*) 'Info: Orbitpy0 [mrad] ', torbyp(1)
      write(*,*) 'Info: Emitx0   [um]   ', remitx
      write(*,*) 'Info: Emity0   [um]   ', remity
      write(*,*) 'Info: E0       [MeV]  ', e0
      write(*,*)
      write(*,*)  'MYENOM' ,myenom
!
      myemitx0 = remitx*1d-6
      myemity0 = remity*1d-6
      myalphax = talphax(1)
      myalphay = talphay(1)
      mybetax  = tbetax(1)
      mybetay  = tbetay(1)
!      myenom   = e0
!
      if (myemitx0.le.0. .or. myemity0.le.0.) then
        write(*,*)                                                      &
     &'ERR> Please use BEAM command to define emittances!'
        stop
      endif
!
!++  Calculate the gammas
!
      mygammax = (1d0+myalphax**2)/mybetax
      mygammay = (1d0+myalphay**2)/mybetay
!
!++  Number of points and generate distribution
!
!GRD SEMI-AUTOMATIC INPUT
!      NLOOP=10
!      MYNEX=6.003
!      MYDEX=0.0015
!      MYNEY=6.003
!      MYDEY=0.0015
!      DO_COLL=1
!      NSIG_PRIM=5.
!      NSIG_SEC=6.
      rselect=64
!
      write(*,*) 'INFO>  NLOOP     = ', nloop
      write(*,*) 'INFO>  DO_THISDIS     = ', do_thisdis
      write(*,*) 'INFO>  MYNEX     = ', mynex
      write(*,*) 'INFO>  MYDEX     = ', mdex
      write(*,*) 'INFO>  MYNEY     = ', myney
      write(*,*) 'INFO>  MYDEY     = ', mdey
      write(*,*) 'INFO>  FILENAME_DIS     = ', filename_dis
      write(*,*) 'INFO>  ENERROR     = ', enerror
      write(*,*) 'INFO>  BUNCHLENGTH     = ', bunchlength
      write(*,*) 'INFO>  RSELECT   = ', int(rselect)
      write(*,*) 'INFO>  DO_COLL   = ', do_coll
      write(*,*) 'INFO>  DO_NSIG   = ', do_nsig
      write(*,*) 'INFO>  NSIG_TCP3    = ', nsig_tcp3
      write(*,*) 'INFO>  NSIG_TCSG3   = ', nsig_tcsg3
      write(*,*) 'INFO>  NSIG_TCSM3   = ', nsig_tcsm3
      write(*,*) 'INFO>  NSIG_TCLA3   = ', nsig_tcla3
      write(*,*) 'INFO>  NSIG_TCP7    = ', nsig_tcp7
      write(*,*) 'INFO>  NSIG_TCSG7   = ', nsig_tcsg7
      write(*,*) 'INFO>  NSIG_TCSM7   = ', nsig_tcsm7
      write(*,*) 'INFO>  NSIG_TCLA7   = ', nsig_tcla7
      write(*,*) 'INFO>  NSIG_TCLP    = ', nsig_tclp
      write(*,*) 'INFO>  NSIG_TCLI    = ', nsig_tcli
!      write(*,*) 'INFO>  NSIG_TCTH    = ', nsig_tcth
!      write(*,*) 'INFO>  NSIG_TCTV    = ', nsig_tctv
      write(*,*) 'INFO>  NSIG_TCTH1   = ', nsig_tcth1
      write(*,*) 'INFO>  NSIG_TCTV1   = ', nsig_tctv1
      write(*,*) 'INFO>  NSIG_TCTH2   = ', nsig_tcth2
      write(*,*) 'INFO>  NSIG_TCTV2   = ', nsig_tctv2
      write(*,*) 'INFO>  NSIG_TCTH5   = ', nsig_tcth5
      write(*,*) 'INFO>  NSIG_TCTV5   = ', nsig_tctv5
      write(*,*) 'INFO>  NSIG_TCTH8   = ', nsig_tcth8
      write(*,*) 'INFO>  NSIG_TCTV8   = ', nsig_tctv8
!
      write(*,*) 'INFO>  NSIG_TCDQ    = ', nsig_tcdq
      write(*,*) 'INFO>  NSIG_TCSTCDQ = ', nsig_tcstcdq
      write(*,*) 'INFO>  NSIG_TDI     = ', nsig_tdi
      write(*,*) 'INFO>  NSIG_TCXRP   = ', nsig_tcxrp
      write(*,*) 'INFO>  NSIG_TCRYP   = ', nsig_tcryo
      write(*,*) 'INFO>  NSIG_CRY   = ', nsig_cry
!
      write(*,*)
      write(*,*) 'INFO> INPUT PARAMETERS FOR THE SLICING:'
      write(*,*)
      write(*,*) 'INFO>  N_SLICES    = ', n_slices
      write(*,*) 'INFO>  SMIN_SLICES = ',smin_slices
      write(*,*) 'INFO>  SMAX_SLICES = ',smax_slices
      write(*,*) 'INFO>  RECENTER1   = ',recenter1
      write(*,*) 'INFO>  RECENTER2   = ',recenter2
      write(*,*)
      write(*,*) 'INFO>  FIT1_1   = ',fit1_1
      write(*,*) 'INFO>  FIT1_2   = ',fit1_2
      write(*,*) 'INFO>  FIT1_3   = ',fit1_3
      write(*,*) 'INFO>  FIT1_4   = ',fit1_4
      write(*,*) 'INFO>  FIT1_5   = ',fit1_5
      write(*,*) 'INFO>  FIT1_6   = ',fit1_6
      write(*,*) 'INFO>  SCALING1 = ',ssf1
      write(*,*)
      write(*,*) 'INFO>  FIT2_1   = ',fit2_1
      write(*,*) 'INFO>  FIT2_2   = ',fit2_2
      write(*,*) 'INFO>  FIT2_3   = ',fit2_3
      write(*,*) 'INFO>  FIT2_4   = ',fit2_4
      write(*,*) 'INFO>  FIT2_5   = ',fit2_5
      write(*,*) 'INFO>  FIT2_6   = ',fit2_6
      write(*,*) 'INFO>  SCALING2 = ',ssf2
      write(*,*)
!
! HERE WE CHECK IF THE NEW INPUT IS READ CORRECTLY
!
      write(*,*) 'INFO>  EMITX0            = ', emitx0
      write(*,*) 'INFO>  EMITY0            = ', emity0
      write(*,*)
      write(*,*) 'INFO>  DO_SELECT         = ', do_select
      write(*,*) 'INFO>  DO_NOMINAL        = ', do_nominal
      write(*,*) 'INFO>  RND_SEED          = ', rnd_seed
      write(*,*) 'INFO>  DOWRITE_DIST      = ', dowrite_dist
      write(*,*) 'INFO>  NAME_SEL          = ', name_sel
      write(*,*) 'INFO>  DO_ONESIDE        = ', do_oneside
      write(*,*) 'INFO>  DOWRITE_IMPACT    = ', dowrite_impact
      write(*,*) 'INFO>  DOWRITE_SECONDARY = ', dowrite_secondary
      write(*,*) 'INFO>  DOWRITE_AMPLITUDE = ', dowrite_amplitude
      write(*,*)
      write(*,*) 'INFO>  XBEAT             = ', xbeat
      write(*,*) 'INFO>  XBEATPHASE        = ', xbeatphase
      write(*,*) 'INFO>  YBEAT             = ', ybeat
      write(*,*) 'INFO>  YBEATPHASE        = ', ybeatphase
      write(*,*)
      write(*,*) 'INFO>  C_RMSTILT_PRIM     = ', c_rmstilt_prim
      write(*,*) 'INFO>  C_RMSTILT_SEC      = ', c_rmstilt_sec
      write(*,*) 'INFO>  C_SYSTILT_PRIM     = ', c_systilt_prim
      write(*,*) 'INFO>  C_SYSTILT_SEC      = ', c_systilt_sec
      write(*,*) 'INFO>  C_RMSOFFSET_PRIM   = ', c_rmsoffset_prim
      write(*,*) 'INFO>  C_SYSOFFSET_PRIM   = ', c_sysoffset_prim
      write(*,*) 'INFO>  C_RMSOFFSET_SEC    = ', c_rmsoffset_sec
      write(*,*) 'INFO>  C_SYSOFFSET_SEC    = ', c_sysoffset_sec
      write(*,*) 'INFO>  C_OFFSETTITLT_SEED = ', c_offsettilt_seed
      write(*,*) 'INFO>  C_RMSERROR_GAP     = ', c_rmserror_gap
      write(*,*) 'INFO>  DO_MINGAP          = ', do_mingap
      write(*,*)
      write(*,*) 'INFO>  RADIAL            = ', radial
      write(*,*) 'INFO>  NR                = ', nr
      write(*,*) 'INFO>  NDR               = ', ndr
      write(*,*)
      write(*,*) 'INFO>  DRIFTSX           = ', driftsx
      write(*,*) 'INFO>  DRIFTSY           = ', driftsy
      write(*,*) 'INFO>  CUT_INPUT         = ', cut_input
      write(*,*) 'INFO>  SYSTILT_ANTISYMM  = ', systilt_antisymm
      write(*,*)
      write(*,*) 'INFO>  IPENCIL           = ', ipencil
      write(*,*) 'INFO>  PENCIL_OFFSET     = ', pencil_offset
      write(*,*) 'INFO>  PENCIL_RMSX       = ', pencil_rmsx
      write(*,*) 'INFO>  PENCIL_RMSY       = ', pencil_rmsy
      write(*,*) 'INFO>  PENCIL_DISTR      = ', pencil_distr
      write(*,*)
      write(*,*) 'INFO>  COLL_DB           = ', coll_db
      write(*,*) 'INFO>  IBEAM             = ', ibeam
      write(*,*)
      write(*,*) 'INFO>  DOWRITETRACKS     = ', dowritetracks
      write(*,*)
      write(*,*) 'INFO>  CERN              = ', cern
      write(*,*)
      write(*,*) 'INFO>  CASTORDIR     = ', castordir
      write(*,*)
      write(*,*) 'INFO>  JOBNUMBER     = ', jobnumber
      write(*,*)
      write(*,*) 'INFO>  CUTS     = ', sigsecut2, sigsecut3
      write(*,*)
!
      mynp = nloop*napx
!
      napx00 = napx
!
      write(*,*) 'INFO>  NAPX     = ', napx, mynp
      write(*,*) 'INFO>  Sigma_x0 = ', sqrt(mybetax*myemitx0)
      write(*,*) 'INFO>  Sigma_y0 = ', sqrt(mybetay*myemity0)
!
! HERE WE SET THE MARKER FOR INITIALIZATION:
!
      firstrun = .true.
!
! ...and here is implemented colltrack's beam distribution:
!
!
!++  Initialize random number generator
!
        if (rnd_seed.eq.0) rnd_seed = mclock_liar()
        if (rnd_seed.lt.0) rnd_seed = abs(rnd_seed)
        rnd_lux = 3
        rnd_k1  = 0
        rnd_k2  = 0
        call rluxgo(rnd_lux, rnd_seed, rnd_k1, rnd_k2)
        CALL RNDMST(12,34,56,78)
        write(*,*)
        write(outlun,*) 'INFO>  rnd_seed: ', rnd_seed
!Call distribution routines only if collimation block is in fort.3, otherwise
!the standard sixtrack would be prevented by the 'stop' command
      if(do_coll) then
      if (radial) then
         call   makedis_radial(mynp, myalphax, myalphay, mybetax,
     &        mybetay, myemitx0, myemity0, myenom, nr, ndr,
     &        myx, myxp, myy, myyp, myp, mys)
      else
         if (do_thisdis.eq.1) then
            call makedis(mynp, myalphax, myalphay, mybetax, mybetay,
     &           myemitx0, myemity0, myenom, mynex, mdex, myney, mdey,
     &           myx, myxp, myy, myyp, myp, mys)
         elseif(do_thisdis.eq.2) then
            call makedis_st(mynp, myalphax, myalphay, mybetax, mybetay,
     &           myemitx0, myemity0, myenom, mynex, mdex, myney, mdey,
     &           myx, myxp, myy, myyp, myp, mys)
         elseif(do_thisdis.eq.3) then
            call makedis_de(mynp, myalphax, myalphay, mybetax, mybetay,
     &           myemitx0, myemity0, myenom, mynex, mdex, myney, mdey,
     &           myx, myxp, myy, myyp, myp, mys,enerror,bunchlength)
         elseif(do_thisdis.eq.4) then
            call  readdis(filename_dis,
     &           mynp, myx, myxp, myy, myyp, myp, mys)                  
         elseif(do_thisdis.eq.5) then
            call makedis_tr(mynp, myalphax, myalphay, mybetax, mybetay,
     &           myemitx0, myemity0, myenom, mynex, mdex, myney, mdey,
     &           myx, myxp, myy, myyp, myp, mys)
         else
            write(*,*) 'INFO> review your distribution parameters !!'
            stop
         endif
!     
      endif
!     
      endif
!++  Reset distribution for pencil beam
!
      if (ipencil.gt.0) then
         write(*,*) 'WARN>  Distributions reset to pencil beam!'    
         write(*,*)
         write(outlun,*) 'WARN>  Distributions reset to pencil beam!'    
         do j = 1, mynp
            myx(j)  = 0d0
            myxp(j) = 0d0
            myy(j)  = 0d0
            myyp(j) = 0d0
         end do
      endif
!
!++  Optionally write the generated particle distribution
!
      open(unit=52,file='dist0.dat')
      if (dowrite_dist) then
        write(52,*)"1=x 2=xp 3=y 4=yp 5=s 6=p 7=x_norm 8=xp_norm
     1  9=y_norm 10=yp_norm 11=Ax 12=Ay 13=iname"        
        do j = 1, mynp
         write(52,*) myx(j), myxp(j), myy(j), myyp(j),
     &     mys(j), myp(j), (myx(j)/sqrt(myemitx0*mybetax)),
     &     (myx(j)*myalphax+myxp(j)*mybetax)/sqrt(myemitx0*mybetax),
     &     (myy(j)/sqrt(myemity0*mybetay)),
     &     (myy(j)*myalphay+myyp(j)*mybetay)/sqrt(myemity0*mybetay),
     &     sqrt(  ((myx(j)/sqrt(myemitx0*mybetax)))**2 +
     & ( (myx(j)*myalphax+myxp(j)*mybetax)/sqrt(myemitx0*mybetax))**2 ),
     &     sqrt( ((myy(j)/sqrt(myemity0*mybetay))  )**2+
     & ( (myy(j)*myalphay+myyp(j)*mybetay)/sqrt(myemity0*mybetay) )**2),
     &  int((j-1)/napx00)+1, j-int((j-1)/napx00)*napx00
        end do
      endif
      close(52)
!
!++  Initialize efficiency array
!
      do i = 1, mynp
        part_hit(i)           = 0
        part_abs(i)           = 0
        part_select(i)        = 1
        part_indiv(i)         = -1d-6
        part_linteract(i)     = 0d0
        part_hit_before(i)    = 0
        tertiary(i)           = 0
        secondary(i)          = 0
        other(i)              = 0
        x00(i)      = myx(i)
        xp00(i)     = myxp(i)
        y00(i)      = myy(i)
        yp00(i)     = myyp(i)
      end do
!
      do i=1,iu
      sum_ax(i)   = 0d0
      sqsum_ax(i) = 0d0
      sum_ay(i)   = 0d0
      sqsum_ay(i) = 0d0
      nampl(i)    = 0d0
      sampl(i)    = 0d0
      end do
!
      nspx = 0d0
      nspy = 0d0

      np0  = mynp
!
      ax0  = myalphax
      bx0  = mybetax
      mux0 = mux(1)
      ay0  = myalphay
      by0  = mybetay
      muy0 = muy(1)
      iturn = 1
      ie    = 1
      n_tot_absorbed = 0
!
!===============================================================================
!Ralph make loop over 1e6/napx, a read xv(1,j) etc
!Du solltest zur Sicherheit dies resetten bevor Du in thin6d gehst
!Im Falle von Teilchenverluste werden n mlich pstop und nnumxv umgesetzt
!      do 80 i=1,npart
!        pstop(i)=.false.
!        nnumxv(i)=numl
!   80 numxv(i)=numl
!===============================================================================


      open(unit=42, file='beta_beat.dat')
      write(42,*)                                                       &
     &'# 1=s 2=bx/bx0 3=by/by0 4=sigx0 5=sigy0 6=crot 7=acalc'
      write(42,*) j

      open(unit=43, file='collgaps.dat')
      
      open(unit=44, file='survival.dat')
      write(44,*)                                                       &
     &'# 1=turn 2=n_particle 3=sample_number'
!
      open(unit=40, file='collimator-temp.db')
      write(43,*)       
     &'# ID name  angle[rad]  betax[m]  betay[m] ',                     &
     &'halfgap[m]  Material  Length[m]  sigx[m]  sigy[m] ',             &
     &'tilt1[rad] tilt2[rad] nsig'

      open(unit=55, file='collsettings.dat')
      write(55,*)        
     &'# name  slicenumber  halfgap[m]  gap_offset[m] ',                &
     &'tilt jaw1[rad]  tilt jaw2[rad] length[m] material'               &

      open(unit=990, file='distn.dat')
      write(990,*)                                                     
     &'# 1=x 2=xp 3=y 4=yp 5=xn 6=xpn 7=yn 8=ypn 9=Ax 10=Ay 11=ipart'

      if (dowrite_impact) then
        open(unit=49,file='impact.dat')
        write(49,*)'# 1=impact 2=divergence'
      endif

!SEPT 2007 valentina : open (if set write_c_out) special outputs for crystal
      if (write_c_out)  then
        OPEN(UNIT=881,FILE='cry_entrance.dat')
        WRITE(881,'(a)') 
     1  '# ipart nturn last_proc icoll coll_mat   x[m]         xp[rad] 
     2       y[m]             yp[rad]       p[GeV]'
c
        OPEN(UNIT=882,FILE='cry_exit.dat')
        WRITE(882,'(a)') 
     1  '#ipart nturn last_proc proc  icoll   coll_mat     x[m]         
     2xp[rad]        y[m]           yp[rad]       p[GeV]'
c
        OPEN(UNIT=883,FILE='cry_entrance_norm.dat')
        WRITE(883,'(a)') 
     1  '#ipart nturn last_proc icoll coll_mat   x[m]         xp[rad]   
     2      y[m]         yp[rad]          n_ampl-X[sig]    n_ampl-Y[sig]
     3  p[GeV]'

        OPEN(UNIT=884,FILE='cry_exit_norm.dat')
        WRITE(884,'(a)') 
     1  '#ipart interaction? last_proc proc nturn icoll coll_mat    x[m]
     2       xp[rad]       y[m]           yp[rad]          n_ampl-X[sig]
     3    n_ampl-Y[sig]      p[GeV]'
!
        OPEN(UNIT=885,FILE='kick.dat')
        write(885,'(a)')
     & '#1=ipart 2=nturn 3=last_proc 4=proc 5=icoll 6=coll_mat  7=x[m] 
     & 8=xp[rad]  9=y[m] 10=yp[rad] 11=kickx[rad] 12=kicky[rad] 
     & 13=Deltap[GeV] 14=aperture 15=tilt '
!
        OPEN(UNIT=833,FILE='cr_par_check.dat')        
        open(unit=866,file='cr_process.dat')

      endif

      if(write_elens_out) then
         open(UNIT=887,FORM='UNFORMATTED', file="elens.bin")
c          write(887, *) "# 1=npart 2=nturn 3=x0 4=xp0 5=y0 6=yp0 7=x1 8=&
c     &xp1 9=y1 10=yp1 11=rkick 12=xkick 13=ykick 14=E0 15=E1" 
c          write(887, *) "# 1=npart 2=nturn 3=x0 4=xp0 5=y0 6=yp0 7=rkick 
c     &8=Ax 9=Ay" 
         open(UNIT=888,FORM='UNFORMATTED',file="elens.norm.bin")
c         write(888,*) "# 1=sample 2=npart 3=nturn 4=x0 5=xp0 6=y0 7=yp0
c     &8=x1 9=xp1 10=y1 11=yp1 12=Ax0 13=Ay0 14=phiX0 15=phiy0 16=Ax1 17=
c     &=Ay1 18=phyX1 19=phyY1 20=DAx 21=DAy" 
       endif
       if (write_TM_out) then
         open(UNIT=889,FORM='UNFORMATTED', file="tm.bin")
         open(UNIT=890,FORM='UNFORMATTED',file="tm.norm.bin")
        endif

       if(do_select) then
         open(unit=45, FORM='UNFORMATTED',file='coll_ellipse.bin')
c         open(unit=45, file='coll_ellipse.dat')

c           write(45,'(a)')                                              &
c     &'# 1=sample  2=part  3=x  4=y  5=xp  6=yp  7=E  8=s  9=turn    10=&
c     &xnorm  11=ynorm  12=xpnorm  13=xpnorm  14=ampl_x  15=ampl_y'  
c           open(unit=9999,file='ellipse.dat')
c           write(9999,*)                                                &
c     &          '# 1=npart 2=turn 3=x 4=y 5=xp 6=yp 7=xnorm 8=xpnorm 9=y&
c     &norm 10=ypnorm 11=ampl_x 12=ampl_y'  
       endif


      open(unit=9997, file='pencilbeam_distr.dat')
      write(9997,*) 'x    xp    y      yp'      
      if(dowrite_impact) then
        open(unit=46, file='all_impacts.dat')
        open(unit=47, file='all_absorptions.dat')
        open(unit=48, file='FLUKA_impacts.dat')
        open(unit=39, file='FirstImpacts.dat')
        if (firstrun) then
          write(46,'(a)') '# 1=name 2=turn 3=s'
          write(47,'(a)') '# 1=name 2=turn 3=s'
          write(48,'(a)')                                               &
     &'# 1=icoll 2=c_rotation 3=s 4=x 5=xp 6=y 7=yp 8=nabs 9=np 10=turn'
          write(39,*)                                                   &
     &     '%1=name,2=iturn, 3=icoll, 4=nabs, 5=s_imp[m], 6=s_out[m], ',&
     &     '7=x_in(b!)[m], 8=xp_in, 9=y_in, 10=yp_in, ',                &
     &     '11=x_out [m], 12=xp_out, 13=y_out, 14=yp_out'
          write(866,'(a)')                                               &
     &     '%1=name,2=iturn, 3=icoll, 4=cr_process'
       endif
      endif
      if(name_sel(1:3).eq.'COL') then
      open(unit=555, file='RHIClosses.dat')
      write(555,'(a)')                                     
     &'# 1=name 2=turn 3=s 4=x 5=xp 6=y 7=yp 8=dp/p 9=type'
      endif

      do n=1, numl
        nsurvive(n)=0
      enddo
c#########################################################################
C   beginning of the loop on the particle samples (closes @~1820)
c########################################################################      
      
      do j = 1, int(mynp/napx00)
!
            write(*,*) 'Sample number ', j, int(mynp/napx00)
            samplenumber=j
!
      if (dowritetracks) then
c              
       if (cern) then
        pfile(1:8) = 'tracks2.'
c        
        if(samplenumber.le.9) then
           pfile(9:9) = smpl
           pfile(10:13) = '.dat'
        elseif(samplenumber.gt.9.and.samplenumber.le.99) then
           pfile(9:10) = smpl
           pfile(11:14) = '.dat'
        elseif(samplenumber.gt.99.and.                                  &
     &samplenumber.le.int(mynp/napx00)) then
           pfile(9:11) = smpl
           pfile(12:15) = '.dat'
        endif
c         
        if(samplenumber.le.9)                                           &
     &open(unit=38,file=pfile(1:13))
        if(samplenumber.gt.9.and.samplenumber.le.99)                    &
     &open(unit=38,file=pfile(1:14))
c         
        if(samplenumber.gt.99.and.                                      &
     &samplenumber.le.int(mynp/napx00))                                 &
     &open(unit=38,file=pfile(1:15))
        else
        open(unit=38,file='tracks2.dat')
!
        endif !close if(cern)
c         
        if(firstrun) write(38,*)                                        &
     &'# 1=name 2=turn 3=s 4=x 5=xp 6=y 7=yp 8=DE/E 9=type'
c         
       endif   !close if(dowritetracks)
       
c         
!++  Copy new particles to tracking arrays. Also add the orbit offset at
!++  start of ring!
!
            do i = 1, napx00
              xv(1,i)  = 1e3*myx(i+(j-1)*napx00)  +torbx(1)
              yv(1,i)  = 1e3*myxp(i+(j-1)*napx00) +torbxp(1)
              xv(2,i)  = 1e3*myy(i+(j-1)*napx00)  +torby(1)
              yv(2,i)  = 1e3*myyp(i+(j-1)*napx00) +torbyp(1)
              x00(i)  = xv(1,i)
              xp00(i) = yv(1,i)
              y00(i)  = xv(2,i)
              yp00(i) = yv(2,i)
!JULY2005 assignation of the proper bunch length
              sigmv(i) = mys(i+(j-1)*napx00)
              ejv(i)   = myp(i+(j-1)*napx00)
!
!GRD FOR NOT FAST TRACKING ONLY
              ejfv(i)=sqrt(ejv(i)*ejv(i)-pma*pma)
              rvv(j)=(ejv(i)*e0f)/(e0*ejfv(i))
              dpsv(i)=(ejfv(i)-e0f)/e0f
              oidpsv(i)=one/(one+dpsv(i))
              dpsv1(i)=dpsv(i)*c1e3*oidpsv(i)
!GRD
!              dpsv(i)  = 0d0
              absorbed(i) = 0
              do ieff =1, numeff
                 counted_r(i,ieff) = 0
                 counted_x(i,ieff) = 0
                 counted_y(i,ieff) = 0
              end do
!GRD INITIALIZE MAX COUNTERS
              ieffmax_r(i) = 0
              ieffmax_x(i) = 0
              ieffmax_y(i) = 0
            end do
!
!
!++  Thin lens tracking
!
!
          call thin6d(nthinerr)
!
!
      if(dowritetracks) then
       if(cern) close(38)
      endif
!------------------------------------------------------------------------
!++  Write the number of absorbed particles
!
      write(outlun,*) 'INFO>  Number of impacts             : ',        &
!     &N_TOT_ABSORBED+NSURVIVE
     &n_tot_absorbed+nsurvive_end
      write(outlun,*) 'INFO>  Number of impacts at selected : ',        &
     &num_selhit
      write(outlun,*) 'INFO>  Number of surviving particles : ',        &
!     &NSURVIVE
     &nsurvive_end
      write(outlun,*) 'INFO>  Number of absorbed particles  : ',        &
     &n_tot_absorbed
!
      write(outlun,*)
!GRD UPGRADE JANUARY 2005
      if(n_tot_absorbed.ne.0d0) then
!
      write(outlun,*) ' INFO>  Eff_r @  8 sigma    [e-4] : ',           &
     &neff(5)/dble(n_tot_absorbed)/1d-4
      write(outlun,*) ' INFO>  Eff_r @ 10 sigma    [e-4] : ',           &
     &neff(9)/dble(n_tot_absorbed)/1d-4
      write(outlun,*) ' INFO>  Eff_r @ 10-20 sigma [e-4] : ',           &
     &(neff(9)-neff(19))/(dble(n_tot_absorbed))/1d-4
!
      write(outlun,*)
      write(outlun,*) neff(5)/dble(n_tot_absorbed),                     &
     &neff(9)/dble(n_tot_absorbed),                                     &
     &(neff(9)-neff(19))/(dble(n_tot_absorbed)), ' !eff'
      write(outlun,*)
!
!UPGRADE JANUARY 2005
      else
          write(*,*) 'NO PARTICLE ABSORBED'
      endif
!
!----
      write(*,*)
      write(*,*) 'INFO>  Number of impacts             : ',             &
     &n_tot_absorbed+nsurvive_end
      write(*,*) 'INFO>  Number of impacts at selected : ',             &
     &num_selhit
      write(*,*) 'INFO>  Number of surviving particles : ',             &
     &nsurvive_end
      write(*,*) 'INFO>  Number of absorbed particles  : ',             &
     &n_tot_absorbed
      write(*,*)
      if(n_tot_absorbed.ne.0d0) then
      write(*,*) ' INFO>  Eff_r @  8 sigma    [e-4] : ',                &
     &neff(5)/dble(n_tot_absorbed)/1d-4
      write(*,*) ' INFO>  Eff_r @ 10 sigma    [e-4] : ',                &
     &neff(9)/dble(n_tot_absorbed)/1d-4
      write(*,*) ' INFO>  Eff_r @ 10-20 sigma [e-4] : ',                &
     &(neff(9)-neff(19))/(dble(n_tot_absorbed))/1d-4
      write(*,*)
      else
          write(*,*) 'NO PARTICLE ABSORBED'
      endif
!
!********************************************************************
! THIS IS THE END OF THE 'DO' LOOP OVER THE thin6d SUBROUTINE  !!!!!
!   this is after the last turn of the last sample      
!********************************************************************
      end do
!
!------------------------------------------------------------------------
!++  Write efficiency file
!
      do n=1, numl
       write(44,*) n, nsurvive(n)
      enddo

      open(unit=991, file='efficiency.dat')
      if(n_tot_absorbed.ne.0d0) then
      write(99,*)                                                       &
     &'# 1=rad_sigma 2=frac_x 3=frac_y 4=frac_r'
      do k=1,numeff
        write(99,'(7(1x,e15.7),1x,I5)') rsig(k),                        &
     &neffx(k)/dble(n_tot_absorbed),                                    &
     &neffy(k)/dble(n_tot_absorbed),                                    &
     &neff(k)/dble(n_tot_absorbed),                                     &
     &neffx(k),                                                         &
     &neffy(k),                                                         &
     &neff(k), n_tot_absorbed
      end do
      else
          write(*,*) 'NO PARTICLE ABSORBED'
      endif
      close(991)
!------------------------------------------------------------------------
!++  Write collimation summary file
!
      open(unit=50, file='coll_summary.dat')
      
      write(50,*)                                                       &
     &'# 1=icoll 2=nimp 3=nabs 4=imp_av 5=imp_sig 6=length'
      do icoll = 1, db_ncoll
        if(db_length(icoll).gt.0d0) then
        write(50,'(i4,1x,a,2(1x,i8),2(1x,e15.7),3x,f13.10)')            &
     &icoll, db_name1(icoll),cn_impact(icoll), cn_absorbed(icoll),      &
     &caverage(icoll), csigma(icoll),db_length(icoll)
      endif
      end do
      close(50)
!-------------------------------------------------------------------------
!GRD
      close(outlun)
      close(40)
      close(42)
      close(43)
      close(44)
      close(10000)
      close(10001)
!SEPT2008 valentina: close special cry outputs
      if (write_c_out) then
         CLOSE(881)   !valentina
         CLOSE(882)   !valentina
         CLOSE(883)   !valentina
         CLOSE(884)   !valentina
         CLOSE(885)   !valentina
         close(833)
      endif

      if(write_elens_out) then
          close(887)
          close(888)
      endif
      if (write_TM_out) then
          close(889)
          close (890)
      endif
      close(990)


      if(dowritetracks) then
      if(.not. cern) close(38)
      if(name_sel(1:3).eq.'COL') close(555)

      endif

      if(do_select) then
         close(45)
c         close(9999)
      endif
      if(dowrite_impact) then
        close(46)
        close(46)
        close(47)
        close(48)
        close(49)
        close(39)
        
        close(866)
      endif
!
!
!++  End of Ralph's own little loop
!
!=============================================================================
        endif
      endif
!
      open(unit=56, file='amplitude.dat')
      open(unit=51, file='amplitude2.dat')
      open(unit=57, file='betafunctions.dat')
      
      if(dowrite_amplitude) then
      write(56,*)                                                       &
     &'# 1=ielem 2=name 3=s 4=AX_AV 5=AX_RMS 6=AY_AV 7=AY_RMS',         &
     &'8=alphax 9=alphay 10=betax 11=betay 12=orbitx',                  &
     &'13=orbity 14=tdispx 15=tdispy',                                  &
     &'16=xbob 17=ybob 18=xpbob 19=ypbob'
      do i=1,iu
        write(56,'(i4, (1x,a16), 17(1x,e20.13))')                       &
     &i, ename(i), sampl(i),                                            &
     &sum_ax(i)/max(nampl(i),1),                                        &
     &sqrt(abs((sqsum_ax(i)/max(nampl(i),1))-                           &
     &(sum_ax(i)/max(nampl(i),1))**2)),                                 &
     &sum_ay(i)/max(nampl(i),1),                                        &
     &sqrt(abs((sqsum_ay(i)/max(nampl(i),1))-                           &
     &(sum_ay(i)/max(nampl(i),1))**2)),                                 &
     &talphax(i), talphay(i),                                           &
     &tbetax(i), tbetay(i), torbx(i), torby(i),                         &
     &tdispx(i), tdispy(i),                                             &
     &xbob(i),ybob(i),xpbob(i),ypbob(i)
      end do
      write(51,*)                                                       &
     &'# 1=ielem 2=name 3=s 4=ORBITX 5=orbity 6=orbxp 7=orbyp 8=tdispx 9
     &=tdispy 10=x_norm 11=y_norm 12=xp_norm 13=yp_norm 14=nx 15=ny'
      
      do i=1,iu
        write(51,*)                         
     &i, ename(i), sampl(i),                                            &
     &torbx(i), torby(i),        
     &torbxp(i),torbyp(i),
     &tdispx(i), tdispy(i),                                             &
     &xdebugN(i),ydebugN(i),xpdebugN(i),ypdebugN(i),
     &sqrt(xdebugN(i)**2+xpdebugN(i)**2),
     &sqrt(ydebugN(i)**2+ypdebugN(i)**2)
        
      end do
      
      write(57,*)                                                       &
     &'# 1=ielem 2=name 3=s 4=TBETAX 5=TBETAY'
      do i=1,iu
        write(57,'(i4, (1x,a16), 3(1x,e15.7))')                         &
     &i, ename(i), sampl(i),                                            &
     &tbetax(i), tbetay(i)
      end do
      endif
      close(56)
      close(51)
      close(57)
      open(unit=992, file='orbitchecking.dat')
      write(992,*) '# 1=s 2=torbitx 3=torbity'
      do j=1,iu
      write(99,'(i4, 3(1x,e15.7))')                                     &
     &j, sampl(j),torbx(j), torby(j)
      end do
      close(992)
      return
      end
      subroutine thin4d(nthinerr)
!-----------------------------------------------------------------------
!
!  TRACK THIN LENS 4D
!
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
      implicit none
      integer i,irrtr,ix,j,k,kpz,n,nmz,nthinerr
      double precision cbxb,cbzb,cccc,cikve,cikveb,crkve,crkveb,crkveuk,&
     &crxb,crzb,dpsv3,pux,r0,r2b,rb,rho2b,rkb,stracki,tkb,xbb,xlvj,xrb, &
     &yv1j,yv2j,zbb,zlvj,zrb
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer ireturn, xory, nac, nfree, nramp1,nplato, nramp2
      double precision e0fo,e0o,xv1j,xv2j
      double precision acdipamp, qd, acphase, acdipamp2,                &
     &acdipamp1
      double precision l,cur,dx,dy,tx,ty,embl,leff,rx,ry,lin,chi,xi,yi
      logical llost
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      double precision cc,xlim,ylim
      parameter(cc = 1.12837916709551d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),    &
     &r2b(npart),rb(npart),rkb(npart),                                  &
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),          &
     &crzb(npart),cbxb(npart),cbzb(npart)
      dimension dpsv3(npart)
      save
!-----------------------------------------------------------------------
      nthinerr=0
      do 640 n=1,numl
        numx=n-1
        if(irip.eq.1) call ripple(n)
        if(mod(numx,nwri).eq.0) call writebin(nthinerr)
        if(nthinerr.ne.0) return
        do 630 i=1,iu
          ix=ic(i)-nblo
!---------count:43
          goto(10,630,740,630,630,630,630,630,630,630,30,50,70,90,110,  &
     &130,150,170,190,210,420,440,460,480,500,520,540,560,580,600,      &
     &620,390,230,250,270,290,310,330,350,370,680,700,720,630,748,      &
     &630,630,630,630,630,745,746),ktrack(i)
          goto 630
   10     stracki=strack(i)
          do 20 j=1,napx
            xv(1,j)=xv(1,j)+stracki*yv(1,j)
            xv(2,j)=xv(2,j)+stracki*yv(2,j)
   20     continue
          goto 630
!--HORIZONTAL DIPOLE
   30     do 40 j=1,napx
            yv(1,j)=yv(1,j)+strackc(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+stracks(i)*oidpsv(j)
   40     continue
          goto 620
!--NORMAL QUADRUPOLE
   50     do 60 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
   60     continue
          goto 620
!--NORMAL SEXTUPOLE
   70     do 80 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
   80     continue
          goto 620
!--NORMAL OCTUPOLE
   90     do 100 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  100     continue
          goto 620
!--NORMAL DECAPOLE
  110     do 120 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  120     continue
          goto 620
!--NORMAL DODECAPOLE
  130     do 140 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  140     continue
          goto 620
!--NORMAL 14-POLE
  150     do 160 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  160     continue
          goto 620
!--NORMAL 16-POLE
  170     do 180 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  180     continue
          goto 620
!--NORMAL 18-POLE
  190     do 200 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  200     continue
          goto 620
!--NORMAL 20-POLE
  210     do 220 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  220     continue
          goto 620
  230     continue
          do 240 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  240     continue
          goto 620
  250     continue
          do 260 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  260     continue
          goto 390
  270     continue
          do 280 j=1,napx
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  280     continue
          goto 620
  290     continue
          do 300 j=1,napx
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  300     continue
          goto 390
  310     continue
          do 320 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  320     continue
          goto 620
  330     continue
          do 340 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  340     continue
          goto 390
  350     continue
          do 360 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  360     continue
          goto 620
  370     continue
          do 380 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  380     continue
  390     r0=ek(ix)
          nmz=nmu(ix)
          if(nmz.ge.2) then
            do 410 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
              yv1j=bbiv(1,1,i)+bbiv(2,1,i)*xlvj+aaiv(2,1,i)*zlvj
              yv2j=aaiv(1,1,i)-bbiv(2,1,i)*zlvj+aaiv(2,1,i)*xlvj
              crkve=xlvj
              cikve=zlvj
                do 400 k=3,nmz
                  crkveuk=crkve*xlvj-cikve*zlvj
                  cikve=crkve*zlvj+cikve*xlvj
                  crkve=crkveuk
                  yv1j=yv1j+bbiv(k,1,i)*crkve+aaiv(k,1,i)*cikve
                  yv2j=yv2j-bbiv(k,1,i)*cikve+aaiv(k,1,i)*crkve
  400           continue
              yv(1,j)=yv(1,j)+(tiltc(i)*yv1j-tilts(i)*yv2j)*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*yv2j+tilts(i)*yv1j)*oidpsv(j)
  410       continue
          else
            do 415 j=1,napx
              yv(1,j)=yv(1,j)+(tiltc(i)*bbiv(1,1,i)-                    &
     &tilts(i)*aaiv(1,1,i))*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*aaiv(1,1,i)+                    &
     &tilts(i)*bbiv(1,1,i))*oidpsv(j)
  415       continue
          endif
          goto 620
!--SKEW ELEMENTS
!--VERTICAL DIPOLE
  420     do 430 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+strackc(i)*oidpsv(j)
  430     continue
          goto 620
!--SKEW QUADRUPOLE
  440     do 450 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  450     continue
          goto 620
!--SKEW SEXTUPOLE
  460     do 470 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  470     continue
          goto 620
!--SKEW OCTUPOLE
  480     do 490 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  490     continue
          goto 620
!--SKEW DECAPOLE
  500     do 510 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  510     continue
          goto 620
!--SKEW DODECAPOLE
  520     do 530 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  530     continue
          goto 620
!--SKEW 14-POLE
  540     do 550 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  550     continue
          goto 620
!--SKEW 16-POLE
  560     do 570 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  570     continue
          goto 620
!--SKEW 18-POLE
  580     do 590 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  590     continue
          goto 620
!--SKEW 20-POLE
  600     do 610 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  610     continue
          goto 620
  680     continue
          do 690 j=1,napx
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)                                       &
     &goto 690
            tkb(j)=rho2b(j)/(two*sigman2(1,imbb(i)))
            if(ibbc.eq.0) then
              yv(1,j)=yv(1,j)+oidpsv(j)*(strack(i)*crkveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))
              yv(2,j)=yv(2,j)+oidpsv(j)*(strack(i)*cikveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))
            else
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),11)-          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
              yv(1,j)=yv(1,j)+oidpsv(j)*cccc
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),12)+          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
              yv(2,j)=yv(2,j)+oidpsv(j)*cccc
            endif
  690     continue
          goto 620
  700     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 620
  720     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 620
  740     continue
          irrtr=imtr(ix)
          do j=1,napx
            pux=xv(1,j)
            dpsv3(j)=dpsv(j)*c1e3
            xv(1,j)=cotr(irrtr,1)+rrtr(irrtr,1,1)*pux+                  &
     &rrtr(irrtr,1,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,1,6)
            yv(1,j)=cotr(irrtr,2)+rrtr(irrtr,2,1)*pux+                  &
     &rrtr(irrtr,2,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,2,6)
            pux=xv(2,j)
            xv(2,j)=cotr(irrtr,3)+rrtr(irrtr,3,3)*pux+                  &
     &rrtr(irrtr,3,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,3,6)
            yv(2,j)=cotr(irrtr,4)+rrtr(irrtr,4,3)*pux+                  &
     &rrtr(irrtr,4,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,4,6)
          enddo
 
!----------------------------------------------------------------------
 
! Wire.
 
          goto 620
  745     continue
          xory=1
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 620
  746     continue
          xory=2
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 620
 
!----------------------------
 
! Wire.
 
  748     continue
!     magnetic rigidity
      chi = sqrt(e0*e0-pmap*pmap)*c1e6/clight
 
      ix = ixcav
      tx = xrms(ix)
      ty = zrms(ix)
      dx = xpl(ix)
      dy = zpl(ix)
      embl = ek(ix)
      l = wirel(ix)
      cur = ed(ix)
 
      leff = embl/cos(tx)/cos(ty)
      rx = dx *cos(tx)-embl*sin(tx)/2
      lin= dx *sin(tx)+embl*cos(tx)/2
      ry = dy *cos(ty)-lin *sin(ty)
      lin= lin*cos(ty)+dy  *sin(ty)
 
      do 750 j=1, napx
 
      xv(1,j) = xv(1,j) * c1m3
      xv(2,j) = xv(2,j) * c1m3
      yv(1,j) = yv(1,j) * c1m3
      yv(2,j) = yv(2,j) * c1m3
 
!      print *, 'Start: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
!     call tilt(tx,ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(tx)*yv(2,j)/sqrt((1+dpsv(j))**2-    &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-tx)
      xv(1,j) = xv(1,j)*(cos(tx)-sin(tx)*tan(atan(yv(1,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(ty)*yv(1,j)/sqrt((1+dpsv(j))**2-    &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-ty)
      xv(2,j) = xv(2,j)*(cos(ty)-sin(ty)*tan(atan(yv(2,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty)
 
!     call drift(lin)
 
      xv(1,j) = xv(1,j) + lin*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) + lin*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
 
!      call kick(l,cur,lin,rx,ry,chi)
 
      xi = xv(1,j)-rx
      yi = xv(2,j)-ry
      yv(1,j) = yv(1,j)-1.0d-7*cur/chi*xi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
!GRD FOR CONSISTENSY
!      yv(2,j) = yv(2,j)-1e-7*cur/chi*yi/(xi**2+yi**2)*                  &
      yv(2,j) = yv(2,j)-1.0d-7*cur/chi*yi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
 
!     call drift(leff-lin)
 
      xv(1,j) = xv(1,j) + (leff-lin)*yv(1,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
      xv(2,j) = xv(2,j) + (leff-lin)*yv(2,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
 
!     call invtilt(tx,ty)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(-ty)*yv(1,j)/sqrt((1+dpsv(j))**2-   &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+ty)
      xv(2,j) = xv(2,j)*(cos(-ty)-sin(-ty)*tan(atan(yv(2,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(-tx)*yv(2,j)/sqrt((1+dpsv(j))**2-   &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+tx)
      xv(1,j) = xv(1,j)*(cos(-tx)-sin(-tx)*tan(atan(yv(1,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx)
 
!     call shift(-embl*tan(tx),-embl*tan(ty)/cos(tx))
 
      xv(1,j) = xv(1,j) + embl*tan(tx)
      xv(2,j) = xv(2,j) + embl*tan(ty)/cos(tx)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
      xv(1,j) = xv(1,j) * c1e3
      xv(2,j) = xv(2,j) * c1e3
      yv(1,j) = yv(1,j) * c1e3
      yv(2,j) = yv(2,j) * c1e3
 
!      print *, 'End: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!-----------------------------------------------------------------------
 
  750     continue
          goto 620
 
!----------------------------
 
  620     continue
          llost=.false.
          do j=1,napx
             llost=llost.or.                                            &
     &abs(xv(1,j)).gt.aper(1).or.abs(xv(2,j)).gt.aper(2)
          enddo
          if (llost) then
             kpz=abs(kp(ix))
             if(kpz.eq.2) then
                call lostpar3(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             elseif(kpz.eq.3) then
                call lostpar4(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             else
                call lostpar2(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             endif
          endif
  630   continue
        call lostpart(nthinerr)
        if(nthinerr.ne.0) return
        if(ntwin.ne.2) call dist1
        if(mod(n,nwr(4)).eq.0) call write6(n)
  640 continue
      return
      end
      subroutine thin6d(nthinerr)
!-----------------------------------------------------------------------
!
!  TRACK THIN LENS 6D
!
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
      implicit none
      integer i,irrtr,ix,j,k,kpz,n,nmz,nthinerr
      double precision c5m4,cbxb,cbzb,cccc,cikve,cikveb,crkve,crkveb,   &
     &crkveuk,crxb,crzb,dpsv3,pux,r0,r2b,rb,rho2b,rkb,stracki,tkb,xbb,  &
     &xlvj,xrb,yv1j,yv2j,zbb,zlvj,zrb
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer ireturn, xory, nac, nfree, nramp1,nplato, nramp2
      double precision e0fo,e0o,xv1j,xv2j
      double precision acdipamp, qd, acphase,acdipamp2,acdipamp1
      double precision l,cur,dx,dy,tx,ty,embl,leff,rx,ry,lin,chi,xi,yi
      logical llost
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      double precision cc,xlim,ylim
      parameter(cc = 1.12837916709551d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),    &
     &r2b(npart),rb(npart),rkb(npart),                                  &
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),          &
     &crzb(npart),cbxb(npart),cbzb(npart)
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
! THIS BLOCK IS COMMON TO BOTH THIN6D AND TRAUTHIN SUBROUTINES
!
      integer ieff
!
      double precision myemitx0,myemity0,myalphay,mybetay,myalphax,     &
     &mybetax,rselect
      common /ralph/ myemitx0,myemity0,myalphax,myalphay,mybetax,       &
     &mybetay,rselect
!
      integer absorbed(npart),counted(npart,numeff)
      double precision neff(numeff),rsig(numeff)
      common  /eff/ neff,rsig,counted,absorbed
!
      integer  nimpact(50)
      double precision sumimpact(50),sqsumimpact(50)
      common  /rimpact/ sumimpact,sqsumimpact,nimpact
!
      integer  nampl(nblz)
      character*16  ename(nblz)
      double precision sum_ax(nblz),sqsum_ax(nblz),sum_ay(nblz),        &
     &sqsum_ay(nblz),sampl(nblz)
      common  /ampl_rev/ sum_ax,sqsum_ax,sum_ay,sqsum_ay,sampl,ename,   &
     &nampl
!
      double precision neffx(numeff),neffy(numeff)
      common /efficiency/ neffx,neffy
!
      integer part_hit(maxn),part_abs(maxn),n_tot_absorbed,n_absorbed   &
     &,part_select(maxn)
      double precision part_impact(maxn)
      common /stats/ part_impact,part_hit,part_abs
      common /n_tot_absorbed/ n_tot_absorbed,n_absorbed
      common /part_select/ part_select
!
      double precision x00(maxn),xp00(maxn),y00(maxn),yp00(maxn)
      common   /beam00/ x00,xp00,y00,yp00
!
      logical firstrun
      common /firstrun/ firstrun
!
      integer nsurvive_end,num_selhit,n_impact
      integer nsurvive(10000000)
      common /outcoll/ nsurvive,num_selhit,n_impact,nsurvive_end
!
      integer napx00
      common /napx00/ napx00
!
      integer  icoll
      common  /icoll/  icoll
!
!UPGRADE January 2005
!     INTEGER DB_NCOLL
      integer db_ncoll
!
! For re-initializtion of random generator
      integer   mclock_liar
!
      character*16 db_name1(max_ncoll),db_name2(max_ncoll)
      character*6 db_material(max_ncoll)
      double precision db_nsig(max_ncoll),db_length(max_ncoll),         &
     &db_offset(max_ncoll),db_rotation(max_ncoll),                      &
     &db_bx(max_ncoll),db_by(max_ncoll),db_tilt(max_ncoll,2),           &
     &db_elense_thickness(max_ncoll),db_elense_j_e(max_ncoll)
     &,db_cry_rcurv(max_ncoll),db_cry_rmax(max_ncoll),                  &
     &db_cry_zmax(max_ncoll),db_cry_alayer(max_ncoll),                  &
     &db_cry_orient(max_ncoll),db_cry_tilt(max_ncoll) 
     &,db_miscut(max_ncoll)
     &,db_elens_center_x(max_ncoll),db_elens_center_y(max_ncoll),
     &db_elens_curr(max_ncoll),db_elens_voltage(max_ncoll),
     &db_elens_r2_ov_r1(max_ncoll),
     &db_elens_tune(max_ncoll),
     &db_elens_mult_tune(max_ncoll),db_elens_delta_tune(max_ncoll),
     &db_elens_step_tune(max_ncoll)
     &,db_tm_center_x(max_ncoll), db_tm_center_y(max_ncoll)
     &,db_tm_kick(max_ncoll),db_tm_tune(max_ncoll)
     &,db_tm_mult_tune(max_ncoll),db_tm_delta_tune(max_ncoll)
     &,db_tm_step_tune(max_ncoll)

      integer db_elens_op_mode(max_ncoll),
     & db_elens_step_turns(max_ncoll),
     & db_elens_resonant_turns(max_ncoll) 
     & , db_tm_step_turns(max_ncoll)

      logical  db_elens_jitter(max_ncoll),db_elens_radial(max_ncoll)
     & , db_tm_switch(max_ncoll)

      common /colldatabase/ db_nsig,db_length,db_rotation,db_offset,    &
     &db_bx,db_by,db_tilt,db_name1,db_name2,db_material,db_ncoll,       &
     &db_elense_thickness,db_elense_j_e
     &,db_cry_rcurv,db_cry_rmax,db_cry_zmax,db_cry_alayer,db_cry_orient,&
     &db_cry_tilt,db_miscut
     &,db_elens_center_x,db_elens_center_y,        
     & db_elens_curr,db_elens_voltage,db_elens_r2_ov_r1,
     &db_elens_op_mode,db_elens_tune,
     &db_elens_mult_tune,db_elens_delta_tune,
     &db_elens_step_tune,db_elens_step_turns,db_elens_resonant_turns,
     &db_elens_jitter,db_elens_radial
     &,db_tm_center_x, db_tm_center_y
     &,db_tm_kick,db_tm_tune
     &,db_tm_mult_tune,db_tm_delta_tune
     &,db_tm_step_tune, db_tm_step_turns
     & , db_tm_switch
!
      integer cn_impact(max_ncoll),cn_absorbed(max_ncoll)
      double precision caverage(max_ncoll),csigma(max_ncoll)
      common /collsummary/ caverage,csigma,cn_impact,cn_absorbed
!
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn)
      common /coord/ myx,myxp,myy,myyp,myp,mys
!
      integer counted_r(maxn,numeff),counted_x(maxn,numeff),            &
     &counted_y(maxn,numeff),                                           &
     &ieffmax_r(npart),ieffmax_x(npart),ieffmax_y(npart)
      common /counting/ counted_r,counted_x,counted_y,ieffmax_r,        &
     &ieffmax_x, ieffmax_y
!
      integer secondary(maxn),tertiary(maxn),other(maxn),               &
     &part_hit_before(maxn)
      double precision part_indiv(maxn),part_linteract(maxn)
!
      integer   samplenumber
      character*4 smpl
      character*80 pfile
      common /samplenumber/ pfile,smpl,samplenumber
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
!
      logical cut_input,firstcoll,found,onesided
!
      integer myktrack,n_gt72,n_gt80,n_gt90,nx_gt72,nx_gt80,            &
     &ny_gt72,ny_gt80,rnd_lux,rnd_k1,rnd_k2,ios,num_surhit,numbin,ibin, &
     &num_selabs,iturn_last_hit,iturn_absorbed,iturn_survive,imov,      &
     &ipart(npart),totalelem,selelem,unitnumber,distnumber,turnnumber,  &
     &jb,myix,                                                          &
     &flukaname(npart)
      integer jjj, ijk
!
      double precision  ran_gauss, myran_gauss
      real rndm5,zbv
!
      double precision c_length    !length in m
      double precision c_rotation  !rotation angle vs vertical in radian
      double precision c_aperture  !aperture in m
      double precision c_offset    !offset in m
      double precision c_tilt(2)   !tilt in radian
      double precision cx(npart),cxp(npart),cy(npart),cyp(npart),       &
     &cp(npart),cs(npart),rcx(npart),rcxp(npart),rcy(npart),rcyp(npart),&
     &rcp(npart),rcs(npart),rcx0(npart),rcxp0(npart),rcy0(npart),       &
     &rcyp0(npart),rcp0(npart),enom_gev,totals,betax,betay,xmax,ymax,   &
     &nsig,calc_aperture,gammax,gammay,gammax0,gammay0,gammax1,gammay1, &
     &xj,xpj,yj,ypj,pj,arcdx,arcbetax,xdisp,nspx,nspy,rxjco,ryjco,      &
     &rxpjco,rypjco,dummy,mux(nblz),muy(nblz),mux0,muy0,c_rmstilt,      &
     &c_systilt,scale_bx,scale_by,scale_bx0,scale_by0,xkick,            &
     &ykick,bx_dist,by_dist,xmax_pencil,ymax_pencil,xmax_nom,ymax_nom,  &
     &nom_aperture,pencil_aperture,xp_pencil(max_ncoll),                &
     &yp_pencil(max_ncoll),x_pencil0,y_pencil0,sum,sqsum,               &
     &csum(max_ncoll),csqsum(max_ncoll),average,sigma,sigsecut,nspxd,   &
     &xndisp,xgrd(npart),xpgrd(npart),ygrd(npart),ypgrd(npart),         &
     &pgrd(npart),ejfvgrd(npart),sigmvgrd(npart),rvvgrd(npart),         &
     &dpsvgrd(npart),oidpsvgrd(npart),dpsv1grd(npart),                  &
     &ax0,ay0,bx0,by0,dnormx,dnormy,driftx,drifty,
     &xnorm,xpnorm,xangle,ynorm,ypnorm,yangle,xbob(nblz),ybob(nblz),    &
     &xpbob(nblz),ypbob(nblz),xineff(npart),yineff(npart),              &
     &xpineff(npart),ypineff(npart),grdpiover2,grdpiover4,grd3piover4
      double precision x_sl(100),x1_sl(100),x2_sl(100),                 &
     &     y1_sl(100), y2_sl(100),                                      &
     &     angle1(100), angle2(100),                                    &
     &     max_tmp,                                                     &
     &     a_tmp1, a_tmp2
!
      character*6 c_material     !material
!
      common /cut/ cut_input
      common /mu/ mux, muy
      common /xcheck/ xbob,ybob,xpbob,ypbob,xineff,yineff,xpineff,      &
     &ypineff
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!GRD
!GRD THIS BLOC IS COMMON TO MAINCR, DATEN, TRAUTHIN AND THIN6D
!GRD
!APRIL2005
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
     &,relative, diffusive
      integer nloop,rnd_seed,c_offsettilt_seed,ibeam,jobnumber,         &
     &do_thisdis,n_slices,pencil_distr
      double precision myenom,mynex,mdex,myney,mdey,                    &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
!SEPT2005 add these lines for the slicing procedure
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
!SEPT2005,OCT2006 added offset
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,ndr,                            &
     &driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,            &
     &sigsecut3,sigsecut2,enerror,bunchlength
!
      character*16 name_coll
      character*24 name_sel
      character*80 coll_db
      character*16 castordir
      character*80 filename_dis
!
      common /grd/ myenom,mynex,mdex,myney,mdey,                        &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
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
     & relative,diffusive,
     &sigsecut3,sigsecut2,enerror,                                      &
     &bunchlength,coll_db,name_sel,                                     &
     &castordir,filename_dis,nloop,rnd_seed,c_offsettilt_seed,          &
     &ibeam,jobnumber,do_thisdis,n_slices,pencil_distr,                 &
     &do_coll,                                                          &
!
     &do_select,do_nominal,dowrite_dist,do_oneside,dowrite_impact,      &
     &dowrite_secondary,dowrite_amplitude,radial,systilt_antisymm,      &
     &dowritetracks,cern,do_nsig,do_mingap
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
! THIS BLOCK IS COMMON TO WRITELIN,LINOPT,TRAUTHIN,THIN6D AND MAINCR
!
      double precision tbetax(nblz),tbetay(nblz),talphax(nblz),         &
     &talphay(nblz),torbx(nblz),torbxp(nblz),torby(nblz),torbyp(nblz),  &
     &tdispx(nblz),tdispy(nblz)
!
      common /rtwiss/ tbetax,tbetay,talphax,talphay,torbx,torbxp,       &
     &torby,torbyp,tdispx,tdispy
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
! Variables for finding the collimator with the smallest gap
! and defining, stroring the gap rms error
!
      character*16 coll_mingap1, coll_mingap2
      double precision gap_rms_error(max_ncoll), nsig_err, sig_offset
      double precision mingap,gap_h1,gap_h2,gap_h3,gap_h4
      integer coll_mingap_id
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
! THIS BLOCK IS COMMON TO THIN6D, TRAUTHIN, COLLIMATE32 AND MAINCR
!
      integer ipencil
      double precision xp_pencil0,yp_pencil0,x_pencil(max_ncoll),       &
     &y_pencil(max_ncoll),pencil_dx(max_ncoll)
      common  /pencil/  xp_pencil0,yp_pencil0,pencil_dx,ipencil
      common  /pencil2/ x_pencil, y_pencil
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      integer ie,iturn,nabs_total
      common  /info/ ie,iturn,nabs_total
!--September 2006 -- TW common to readcollimator and collimate2
!      logical           changed_tilt1(max_ncoll)
!      logical           changed_tilt2(max_ncoll)
!      common /tilt/ changed_tilt1, changed_tilt2
!--September 2006
!
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
! SEPT2008 JCSMITH
! electron lense parameters
c       integer this_elense ! which elense we are currently looking at
c       integer, parameter :: n_elense = 10 
c       double precision elense_r_min(n_elense)
c       double precision elense_r_max(n_elense)
c       double precision elense_j_e(n_elense)
c       double precision elense_length(n_elense)
     
c       common /elense/ elense_r_min, elense_r_max, elense_j_e,          &
c     &                elense_length
! SEPT2008
      integer nprim
!
      dimension dpsv3(npart)
!-----------------------------------------------------------------------
!SEPT 2008 valentina
! crystal parameters
! Cry_length crystal length [m] 
! Rcurv curvature radius [m]
! C_xmax thickness of the crystal [m] 
! C_ymax height of the crystal [m]
! Alayer thickness of amorphous layer [m]
! C_orient crystalline planes orientation
! Cry_tilt0 beam natural divergence at the crystal position
! Cry_tilt total tilt of the crystal (orientation + c_tilt0)
!      
! bool_proc(j) is set, for each particle, after the last passage from a
! crystal. bool_proc(j)=1: amorphous ,  bool_proc(j)=2: volume reflection,
! bool_proc(j)=3: channeling, bool_proc(j)=4: dechanneling
!  bool_proc(j)=5: absorbed ,bool_proc(j)=6: volume capture
!
! bool_create is just to check that the distribution of particles has
! alredy been created
!
! write_c_out, write_SPS_out are flags to activate more output files for
! the crystal
!
      double precision  Cry_length, Rcurv,C_xmax,C_ymax,Alayer,C_orient
      double precision  Cry_Bending
      double precision miscut  !miscut angle in rad
      common/miscut/ miscut
      common /Par_Cry1/ Cry_length, Rcurv,C_xmax,C_ymax,Alayer,C_orient
      double precision Cry_tilt,Cry_tilt0
      common /Par_Cry2/ Cry_tilt,Cry_tilt0 
      integer  bool_proc(maxn)
      integer  bool_proc_old(maxn)
      logical bool_create
      logical write_c_out, write_SPS_out, write_elens_out
     &     ,write_TM_out  
      common /outputs/ write_c_out, write_SPS_out, write_elens_out
     &     ,write_TM_out  
      common /Process/ bool_proc,bool_create
      common /Process_old/ bool_proc_old
!
      double precision  X_NORM0,XP_NORM0,Y_NORM0,YP_NORM0,PHI_X0,PHI_Y0   
      double precision  X_NORM,XP_NORM,Y_NORM,YP_NORM, PHI_X,PHI_Y
!

      double precision xdebug(nblz),xdebugN(nblz),xpdebug(nblz),
     & xpdebugN(nblz),
     & ydebug(nblz),ydebugN(nblz),ypdebug(nblz),ypdebugN(nblz) 
      common /debugvale/xdebug,xdebugN,xpdebug,xpdebugN,
     &ydebug,ydebugN,ypdebug,ypdebugN 

!
      double precision totals_vale

!---------------------------------------------------------------------------
      save
      
      c5m4=5.0d-4
      nthinerr=0
!++  Some initialization
!
      do i = 1, numeff
        rsig(i) = dble(i)/2d0 - 0.5d0 + 6d0
      enddo
      n_gt72 = 0
      n_gt80 = 0
      n_gt90 = 0
      nx_gt72 = 0
      nx_gt80 = 0
      ny_gt72 = 0
      ny_gt80 = 0
      firstcoll = .true.
      napx = napx00
      do j = 1, napx
         part_hit(j)    = 0
         part_abs(j)    = 0
         part_impact(j) = 0
         tertiary(j)=0
         secondary(j)=0
         other(j)=0
         ipart(j) = j
c         flukaname(j) = 0
         flukaname(j) = ipart(j)+100*samplenumber
      enddo
!     
!++   This we only do once, for the first call to this routine. Numbers
!++   are saved in memory to use exactly the same info for each sample.
!++   COMMON block to decide for first usage and to save coll info.
!     
!--------------------------------------------------------------------
!++   Read collimator database
      if (firstrun) then
!     
         write(*,*)"read collimator..."
         call readcollimator
         write(*,*)"read collimator done"
!     
         write(*,*) 'number of collimators', db_ncoll
         do icoll = 1, db_ncoll
            write(*,*) 'COLLIMATOR', icoll, ' ', db_name1(icoll)
            write(*,*) 'collimator', icoll, ' ', db_name2(icoll)
         end do
!******write settings for alignment error in colltrack.out file
!
      write(outlun,*) ' '
      write(outlun,*) 'Alignment errors settings (tilt, offset,...)'
      write(outlun,*) ' '
      write(outlun,*) 'SETTING> c_rmstilt_prim   : ', c_rmstilt_prim
      write(outlun,*) 'SETTING> c_rmstilt_sec    : ', c_rmstilt_sec
      write(outlun,*) 'SETTING> c_systilt_prim   : ', c_systilt_prim
      write(outlun,*) 'SETTING> c_systilt_sec    : ', c_systilt_sec
      write(outlun,*) 'SETTING> c_rmsoffset_prim : ', c_rmsoffset_prim
      write(outlun,*) 'SETTING> c_rmsoffset_sec  : ', c_rmsoffset_sec
      write(outlun,*) 'SETTING> c_sysoffset_prim : ', c_sysoffset_prim
      write(outlun,*) 'SETTING> c_sysoffset_sec  : ', c_sysoffset_sec
      write(outlun,*) 'SETTING> c_offsettilt seed: ', c_offsettilt_seed
      write(outlun,*) 'SETTING> c_rmserror_gap   : ', c_rmserror_gap
      write(outlun,*) 'SETTING> do_mingap        : ', do_mingap
      write(outlun,*) ' '
!     added offset and random_seed for tilt and offset
!*****intialize random generator with offset_seed
      c_offsettilt_seed = abs(c_offsettilt_seed)
      rnd_lux = 3
      rnd_k1  = 0
      rnd_k2  = 0
      call rluxgo(rnd_lux, c_offsettilt_seed, rnd_k1, rnd_k2)         
!      write(outlun,*) 'INFO>  c_offsettilt seed: ', c_offsettilt_seed
!
! reset counter to assure starting at the same position in case of
! using rndm5 somewhere else in the code before
! 
      zbv = rndm5(1)
!
!++  Generate random tilts (Gaussian distribution plus systematic)
!++  Do this only for the first call of this routine (first sample)
!++  Keep all collimator database info and errors in memeory (COMMON
!++  block) in order to re-use exactly the same information for every
!++  sample.
!
         if (c_rmstilt_prim.gt.0. .or. c_rmstilt_sec.gt.0. .or.         &
     &        c_systilt_prim.ne.0. .or. c_systilt_sec.ne.0.) then
            do icoll = 1, db_ncoll
               if (db_name1(icoll)(1:3).eq.'TCP') then
                  c_rmstilt = c_rmstilt_prim
                  c_systilt = c_systilt_prim
               else
                  c_rmstilt = c_rmstilt_sec
                  c_systilt = c_systilt_sec
               endif
               db_tilt(icoll,1) = c_systilt+c_rmstilt*myran_gauss(3d0)
               if (systilt_antisymm) then
                  db_tilt(icoll,2) =                                    &
     &                 -1d0*c_systilt+c_rmstilt*myran_gauss(3d0)
               else
                  db_tilt(icoll,2) =                                    &
     &                 c_systilt+c_rmstilt*myran_gauss(3d0)
               endif
               write(outlun,*) 'INFO>  Collimator ', db_name1(icoll),   &
     &              ' jaw 1 has tilt [rad]: ', db_tilt(icoll,1)
               write(outlun,*) 'INFO>  Collimator ', db_name1(icoll),   &
     &              ' jaw 2 has tilt [rad]: ', db_tilt(icoll,2)
            end do
         endif
!++  Generate random offsets (Gaussian distribution plus systematic)
!++  Do this only for the first call of this routine (first sample)
!++  Keep all collimator database info and errors in memeory (COMMON
!++  block) in order to re-use exactly the same information for every
!++  sample and throughout a all run.
         if (c_sysoffset_prim.ne.0. .or. c_sysoffset_sec.ne.0. .or.     &
     &        c_rmsoffset_prim.gt.0. .or. c_rmsoffset_sec.gt.0.) then
            do icoll = 1, db_ncoll 
               if (db_name1(icoll)(1:3).eq.'TCP') then
                  db_offset(icoll) = c_sysoffset_prim +                 &
     &                 c_rmsoffset_prim*myran_gauss(3d0)
               else
                  db_offset(icoll) = c_sysoffset_sec +                  &
     &                 c_rmsoffset_sec*myran_gauss(3d0)
               endif
               write(outlun,*) 'INFO>  offset: ', db_name1(icoll),      &
     &              db_offset(icoll)
            end do
         endif
!++  Generate random offsets (Gaussian distribution)
!++  Do this only for the first call of this routine (first sample)
!++  Keep all collimator database info and errors in memeory (COMMON
!++  block) in order to re-use exactly the same information for every
!++  sample and throughout a all run.
            do icoll = 1, db_ncoll 
               gap_rms_error(icoll) = c_rmserror_gap * myran_gauss(3d0)
               write(outlun,*) 'INFO>  gap_rms_error: ',                &
     &              db_name1(icoll),gap_rms_error(icoll)
            end do
!
!---- creating a file with beta-functions at TCP/TCS 
         open(unit=10000, file='twisslike.out')
         open(unit=10001, file='sigmasettings.out')
         mingap = 20

!*************************************************
!        Firstrun: cycle over elements
!************************************************         
         do j=1,iu
! this transformation gives the right marker/name to the corresponding 
! beta-dunctions or vice versa ;)
            if(ic(j).le.nblo) then
               do jb=1,mel(ic(j))
                  myix=mtyp(ic(j),jb)
               enddo
            else
               myix=ic(j)-nblo
            endif
! Using same code-block as below to evalute the collimator opening
! for each collimator, this is needed to get the smallest collimator gap
! in principal only looking for primary and secondary should be enough
! JULY 2008 added changes (V6.503) for names in TCTV -> TCTVA amd TCTVB 
! both namings before and after V6.503 can be used 
            if ( bez(myix)(1:2).eq.'TC'                                 &
     &           .or. bez(myix)(1:2).eq.'tc'                            &
     &           .or. bez(myix)(1:2).eq.'TD'                            &
     &           .or. bez(myix)(1:2).eq.'td'                            &
     &           .or. bez(myix)(1:3).eq.'COL'                           &
     &           .or. bez(myix)(1:3).eq.'col'
     &           .or. bez(myix)(1:5).eq.'ELENS'       
     &           .or. bez(myix)(1:5).eq.'elens'       
     &           .or. bez(myix)(1:2).eq.'TM'       
     &           .or. bez(myix)(1:2).eq.'tm'       
     &           .or. bez(myix)(1:3).eq.'CRY'       !valentina add crystal 
     &           .or. bez(myix)(1:3).eq.'cry') then
               if(bez(myix)(1:3).eq.'TCP' .or.                          &
     &              bez(myix)(1:3).eq.'tcp') then
                  if(bez(myix)(7:9).eq.'3.B' .or.                       &
     &                 bez(myix)(7:9).eq.'3.b') then
                     nsig = nsig_tcp3
                  else
                     nsig = nsig_tcp7
                  endif
               elseif(bez(myix)(1:4).eq.'TCSG' .or.                     &
     &                 bez(myix)(1:4).eq.'tcsg' .or.
     &                 bez(myix)(1:4).eq.'TCSP' .or.
     &                 bez(myix)(1:4).eq.'tcsp'  ) then
                  if(bez(myix)(8:10).eq.'3.B' .or.                      &
     &                 bez(myix)(8:10).eq.'3.b' .or.                    &
     &                 bez(myix)(9:11).eq.'3.B' .or.                    &
     &                 bez(myix)(9:11).eq.'3.b') then
                     nsig = nsig_tcsg3
                  else
                     nsig = nsig_tcsg7
                  endif
                  if((bez(myix)(5:6).eq.'.4'.and.bez(myix)(8:9).eq.'6.')&
     &                 ) then
                     nsig = nsig_tcstcdq
                  endif
               elseif(bez(myix)(1:4).eq.'TCSM' .or.                     &
     &                 bez(myix)(1:4).eq.'tcsm') then
                  if(bez(myix)(8:10).eq.'3.B' .or.                      &
     &                 bez(myix)(8:10).eq.'3.b' .or.                    &
     &                 bez(myix)(9:11).eq.'3.B' .or.                    &
     &                 bez(myix)(9:11).eq.'3.b') then
                     nsig = nsig_tcsm3
                  else
                     nsig = nsig_tcsm7
                  endif
               elseif(bez(myix)(1:4).eq.'TCLA' .or.                     &
     &                 bez(myix)(1:4).eq.'tcla') then
                  if(bez(myix)(9:11).eq.'7.B' .or.                      &
     &                 bez(myix)(9:11).eq.'7.b') then
                     nsig = nsig_tcla7
                  else
                     nsig = nsig_tcla3
                  endif
               elseif(bez(myix)(1:4).eq.'TCDQ' .or.                     &
     &                 bez(myix)(1:4).eq.'tcdq') then
                  nsig = nsig_tcdq
               elseif(bez(myix)(1:4).eq.'TCTH' .or.                     &
     &                 bez(myix)(1:4).eq.'tcth' ) then                  &
                  if(bez(myix)(8:10).eq.'1.B' .or.                      &
     &                 bez(myix)(8:10).eq.'1.b') then
                     nsig = nsig_tcth1
                  elseif(bez(myix)(8:10).eq.'2.B' .or.                  &
     &                    bez(myix)(8:10).eq.'2.b') then
                     nsig = nsig_tcth2
                  elseif(bez(myix)(8:10).eq.'5.B' .or.                  &
     &                    bez(myix)(8:10).eq.'5.b') then
                     nsig = nsig_tcth5
                  elseif(bez(myix)(8:10).eq.'8.B' .or.                  &
     &                    bez(myix)(8:10).eq.'8.b') then
                     nsig = nsig_tcth8
                  endif
               elseif(bez(myix)(1:4).eq.'TCTV' .or.                     &
     &                 bez(myix)(1:4).eq.'tctv' ) then
                  if(bez(myix)(8:10).eq.'1.B' .or.                      &
     &                 bez(myix)(8:10).eq.'1.b' .or.                    &
     &                 bez(myix)(9:11).eq.'1.B' .or.                    &
     &                 bez(myix)(9:11).eq.'1.b' ) then
                     nsig = nsig_tctv1
                  elseif(bez(myix)(8:10).eq.'2.B' .or.                  &
     &                    bez(myix)(8:10).eq.'2.b' .or.                 &
     &                    bez(myix)(9:11).eq.'2.B' .or.                 &
     &                    bez(myix)(9:11).eq.'2.b' ) then
                     nsig = nsig_tctv2
                  elseif(bez(myix)(8:10).eq.'5.B' .or.                  &
     &                    bez(myix)(8:10).eq.'5.b' .or.                 &
     &                    bez(myix)(9:11).eq.'5.B' .or.                 &
     &                    bez(myix)(9:11).eq.'5.b') then
                     nsig = nsig_tctv5
                  elseif(bez(myix)(8:10).eq.'8.B' .or.                  &
     &                    bez(myix)(8:10).eq.'8.b' .or.                 &
     &                    bez(myix)(9:11).eq.'8.B' .or.                 &
     &                    bez(myix)(9:11).eq.'8.b') then
                     nsig = nsig_tctv8
                  endif
               elseif(bez(myix)(1:3).eq.'TDI' .or.                      &
     &                 bez(myix)(1:3).eq.'tdi') then
                  nsig = nsig_tdi
               elseif(bez(myix)(1:4).eq.'TCLP' .or.                     &
     &                 bez(myix)(1:4).eq.'tclp' .or.                    &
     &                 bez(myix)(1:4).eq.'TCL.' .or.                    &
     &                 bez(myix)(1:4).eq.'tcl.') then
                  nsig = nsig_tclp
               elseif(bez(myix)(1:4).eq.'TCLI' .or.                     &
     &                 bez(myix)(1:4).eq.'tcli') then
                  nsig = nsig_tcli
               elseif(bez(myix)(1:4).eq.'TCXR' .or.                     &
     &                 bez(myix)(1:4).eq.'tcxr') then
                  nsig = nsig_tcxrp
!     TW 04/2008 ---- start adding TCRYO
               elseif(bez(myix)(1:5).eq.'TCRYO' .or.                     &
     &                 bez(myix)(1:5).eq.'tcryo') then
                  nsig = nsig_tcryo
!     TW 04/2008 ---- end adding TCRYO
!     valentina SEPT2008 ---- start adding CRY
               elseif(bez(myix)(1:3).eq.'CRY' .or.                     
     &                 bez(myix)(1:3).eq.'cry') then
                  nsig = nsig_cry
!     valentina SEPT2008 ---- end adding CRY
               elseif(bez(myix)(1:5).eq.'ELENS' .or.
     &                 bez(myix)(1:5).eq.'elens') then
                        nsig=3
               elseif(bez(myix)(1:2).eq.'TM' .or.
     &                 bez(myix)(1:2).eq.'tm') then
                        nsig=100
               elseif(bez(myix)(1:3).eq.'COL' .or.                      &
     &                 bez(myix)(1:3).eq.'col') then
                  if(bez(myix)(1:4).eq.'COLM' .or.                      &
     &                 bez(myix)(1:4).eq.'colm' .or.                    &
     &                 bez(myix)(1:5).eq.'COLH0' .or.                   &
     &                 bez(myix)(1:5).eq.'colh0') then
                     nsig = nsig_tcth1
                  elseif(bez(myix)(1:5).eq.'COLV0' .or.                 &
     &                    bez(myix)(1:5).eq.'colv0') then
                     nsig = nsig_tcth2
                  elseif(bez(myix)(1:5).eq.'COLH1' .or.                 &
     &                    bez(myix)(1:5).eq.'colh1') then
!     JUNE2005   HERE WE USE NSIG_TCTH2 AS THE OPENING IN THE VERTICAL
!     JUNE2005   PLANE FOR THE PRIMARY COLLIMATOR OF RHIC; NSIG_TCTH5 STANDS
!     JUNE2005   FOR THE OPENING OF THE FIRST SECONDARY COLLIMATOR OF RHIC
                     nsig = nsig_tcth5
                  elseif(bez(myix)(1:5).eq.'COLV1' .or.                 &
     &                    bez(myix)(1:5).eq.'colv1') then
                     nsig = nsig_tcth8
                  elseif(bez(myix)(1:5).eq.'COLH2' .or.                 &
     &                    bez(myix)(1:5).eq.'colh2') then
                     nsig = nsig_tctv1
                  endif
!     JUNE2005   END OF DEDICATED TREATMENT OF RHIC OPENINGS
               endif
!     FEBRUAR2007
               do i = 1, db_ncoll
! start searching minimum gap 
                  if ((db_name1(i)(1:11).eq.bez(myix)(1:11)) .or.       &
     &                 (db_name2(i)(1:11).eq.bez(myix)(1:11))) then
                     if ( db_length(i) .gt. 0d0 ) then
                        nsig_err = nsig + gap_rms_error(i)
! jaw 1 on positive side x-axis
                        gap_h1 = nsig_err - sin(db_tilt(i,1))*          &
     &                       db_length(i)/2
                        gap_h2 = nsig_err + sin(db_tilt(i,1))*          &
     &                       db_length(i)/2
! jaw 2 on negative side of x-axis (see change of sign comapred 
! to above code lines, alos have a look to setting of tilt angle)
                        gap_h3 = nsig_err + sin(db_tilt(i,2))*          &
     &                       db_length(i)/2
                        gap_h4 = nsig_err - sin(db_tilt(i,2))*          &
     &                       db_length(i)/2
! find minumum halfgap
! --- searching for smallest halfgap
                        if (do_nominal) then                            
                           bx_dist = db_bx(icoll) 
                           by_dist = db_by(icoll)
                        else
                           bx_dist = tbetax(j)
                           by_dist = tbetay(j)
                        endif
                        sig_offset = db_offset(i) /                     &
     &                       (sqrt(bx_dist**2 * cos(db_rotation(i))**2  &
     &                       + by_dist**2 * sin(db_rotation(i))**2 ))
                        write(10000,*) bez(myix),tbetax(j),tbetay(j),   &
     &                       torbx(j),torby(j), nsig, gap_rms_error(i)
                        write(10001,*) bez(myix), gap_h1, gap_h2,       & 
     &                       gap_h3, gap_h4, sig_offset, db_offset(i),  &
     &                       nsig, gap_rms_error(i)
                        if ((gap_h1 + sig_offset) .le. mingap) then
                           mingap = gap_h1 + sig_offset
                           coll_mingap_id = i
                           coll_mingap1 = db_name1(i)
                           coll_mingap2 = db_name2(i) 
                        elseif ((gap_h2 + sig_offset) .le. mingap) then
                           mingap = gap_h2 + sig_offset
                           coll_mingap_id = i
                           coll_mingap1 = db_name1(i)
                           coll_mingap2 = db_name2(i) 
                        elseif ((gap_h3 - sig_offset) .le. mingap) then
                           mingap = gap_h3 - sig_offset
                           coll_mingap_id = i
                           coll_mingap1 = db_name1(i)
                           coll_mingap2 = db_name2(i) 
                        elseif ((gap_h4 - sig_offset) .le. mingap) then
                           mingap = gap_h4 - sig_offset
                           coll_mingap_id = i
                           coll_mingap1 = db_name1(i)
                           coll_mingap2 = db_name2(i)
                        endif
                     endif
                  endif
               enddo
!     
! could be done more elegant the above code to search the minimum gap
! and should also consider the jaw tilt
!
            endif

         enddo
c***************************************************************
c         END cyrcle over elements @first run         
c***************************************************************
c 
         write(10000,*) 'minimum gap collimator:',coll_mingap_id,
     &    coll_mingap1,coll_mingap2, mingap
         write(10000,*) 'INFO> IPENCIL initial ',ipencil
! if pencil beam is used and on collimator with smallest gap the
! distribution should be generated, set ipencil to coll_mingap_id    
         if (ipencil.gt.0 .and. do_mingap) then
            ipencil = coll_mingap_id
         endif
         write(10000,*) 'INFO> IPENCIL new (if do_mingap) ',ipencil
! ---
         write(10001,*) coll_mingap_id,coll_mingap1,coll_mingap2,       &
     &        mingap
! if pencil beam is used and on collimator with smallest gap the
! distribution should be generated, set ipencil to coll_mingap_id    
         write(10001,*) 'INFO> IPENCIL new (if do_mingap) ',ipencil
         write(10001,*) 'INFO> rnd_seed is (before reinit)',rnd_seed
!
!
!****** re-intialize random generator with rnd_seed 
!       reinit with initial value used in first call  
         rnd_lux = 3
         rnd_k1  = 0
         rnd_k2  = 0
         call rluxgo(rnd_lux, rnd_seed, rnd_k1, rnd_k2)
!
!GRD
!GRD INITIALIZE LOCAL ADDITIVE PARAMETERS, ie THE ONE WE DON'T WANT
!GRD TO KEEP OVER EACH LOOP
!GRD
          do k = 1, numeff
            neff(k)  = 0d0
            neffx(k) = 0d0
            neffy(k) = 0d0
          enddo
!
!Mars 2005
          do j=1,max_ncoll
            cn_impact(j) = 0
            cn_absorbed(j) = 0
            csum(j) = 0d0
            csqsum(j) = 0d0
          enddo
!Mars 2005
c***************************************************************
!++ End of first call stuff (end of first run)
c***************************************************************
      endif
!
c      open(unit=99,file='betatron.dat')



c***************************************************************
c     begin of cycle over number of turns (main part of thin6D routine)   
c***************************************************************
      do 660 n=1,numl
       iturn=n
       numx=n-1
        if(irip.eq.1) call ripple(n)
        if(mod(numx,nwri).eq.0) call writebin(nthinerr)
        if(nthinerr.ne.0) return
        totals=0d0
        totals_vale=0d0
c***************************************************************
c     begin of cycle over number of element (for each turn)   
c***************************************************************
      do 650 i=1,iu
      ie=i
!
!++  For absorbed particles set all coordinates to zero. Also
!++  include very large offsets, let's say above 100mm or
!++  100mrad.
!
          do j = 1, napx
            if (part_abs(j).gt.0 .or.                                   &
     &xv(1,j).gt.100d0 .or.                                             &
     &yv(1,j).gt.100d0 .or.                                             &
     &xv(2,j).gt.100d0 .or.                                             &
     &yv(2,j).gt.100d0) then
              xv(1,j) = 0d0
              yv(1,j) = 0d0
              xv(2,j) = 0d0
              yv(2,j) = 0d0
              ejv(j)  = myenom
              sigmv(j)= 0d0
              part_abs(j) = 100000000*ie + iturn
              secondary(j) = 0
              tertiary(j)  = 0
              other(j) = 0
            endif
          end do
!GRD
!GRD SAVE COORDINATES OF PARTICLE 1 TO CHECK ORBIT
!GRD
         if(firstrun) then
        xbob(ie)=xv(1,1) 
       !xbob [mm] is the transverse coordinate of the first particle 
       !of the first bunch at the (last?) turn  
        ybob(ie)=xv(2,1)
        xpbob(ie)=yv(1,1)
        ypbob(ie)=yv(2,1)
        endif
!++  Here comes sixtrack stuff
!
        if(ic(i).le.nblo) then
              do jb=1,mel(ic(i))
                 myix=mtyp(ic(i),jb)
              enddo
        else
              myix=ic(i)-nblo
        endif
          ix=ic(i)-nblo
!++  Make sure we go into collimation routine for any definition
!++  of collimator element, relying on element name instead.
!
          if (                                                          &
!GRD HERE ARE SOME CHANGES TO MAKE RHIC TRAKING AVAILABLE
     &   (bez(myix)(1:3).eq.'TCP'.or.bez(myix)(1:3).eq.'tcp') .or.      &
     &   (bez(myix)(1:3).eq.'TCS'.or.bez(myix)(1:3).eq.'tcs') .or.      &
     &   (bez(myix)(1:3).eq.'TCL'.or.bez(myix)(1:3).eq.'tcl') .or.      &
     &   (bez(myix)(1:3).eq.'TCT'.or.bez(myix)(1:3).eq.'tct') .or.      &
     &   (bez(myix)(1:3).eq.'TCD'.or.bez(myix)(1:3).eq.'tcd') .or.      &
     &   (bez(myix)(1:3).eq.'TDI'.or.bez(myix)(1:3).eq.'tdi') .or.      &
     &   (bez(myix)(1:3).eq.'TCX'.or.bez(myix)(1:3).eq.'tcx') .or.      &
     &   (bez(myix)(1:3).eq.'TCR'.or.bez(myix)(1:3).eq.'tcr') .or.      &
     &   (bez(myix)(1:3).eq.'COL'.or.bez(myix)(1:3).eq.'col') .or.      &
     &   (bez(myix)(1:5).eq.'ELENS'.or.bez(myix)(1:5).eq.'elens') .or.  &
     &   (bez(myix)(1:2).eq.'TM'.or.
     &   bez(myix)(1:2).eq.'tm').or.
     &   (bez(myix)(1:3).eq.'CRY'.or.bez(myix)(1:3).eq.'cry') ) then
            myktrack = 1
         else
            myktrack = ktrack(i)
         endif
         if (myktrack.eq.1) then 
          totals_vale=totals_vale+strack(i)
          if (strack(i).lt.0) write(*,*)"WARN:lenght <0!!!!,el=",ie
     &    ,bez(myix),myktrack 
         endif
          goto(10,30,740,650,650,650,650,650,650,650,50,70,90,110,130,  &
     &150,170,190,210,230,440,460,480,500,520,540,560,580,600,620,      &
     &640,410,250,270,290,310,330,350,370,390,680,700,720,730,748,      &
     &650,650,650,650,650,745,746),myktrack
          goto 650
   10     stracki=strack(i)
!==========================================
!Ralph drift length is stracki
!bez(ix) is name of drift
          totals=totals+stracki
!________________________________________________________________________
!++  If we have a collimator then...
!
!Feb2006
!GRD (June 2005) 'COL' option is for RHIC collimators
!
!     SR (17-01-2006): Special assignment to the TCS.TCDQ for B1 and B4,
!     using the new naming as in V6.500.
!     Note that this must be in the loop "if TCSG"!!
!
!     SR, 17-01-2006: Review the TCT assignments because the MADX names
!     have changes (TCTH.L -> TCTH.4L)
!
! JULY 2008 added changes (V6.503) for names in TCTV -> TCTVA amd TCTVB 
! both namings before and after V6.503 can be used 
! 
!SEPT2008 JCSMITH
! Added electorn lense collimator
!
!SEPT2008 valentina
! Added crystal collimator
          if (do_coll .and.
     &    (bez(myix)(1:2).eq.'TC'                                  
     &    .or. bez(myix)(1:2).eq.'tc'                              
     &    .or. bez(myix)(1:2).eq.'TD'                              
     &    .or. bez(myix)(1:2).eq.'td'                              
     &    .or. bez(myix)(1:5).eq.'ELENS'                           
     &    .or. bez(myix)(1:5).eq.'elens'                           
     &    .or. bez(myix)(1:2).eq.'TM'                           
     &    .or. bez(myix)(1:2).eq.'tm'                           
     &    .or. bez(myix)(1:3).eq.'CRY'                             
     &    .or. bez(myix)(1:3).eq.'cry'                             
     &    .or. bez(myix)(1:3).eq.'COL'                             
     &    .or. bez(myix)(1:3).eq.'col')) then
                if(bez(myix)(1:3).eq.'TCP' .or.                         &
     &          bez(myix)(1:3).eq.'tcp') then
                        if(bez(myix)(7:9).eq.'3.B' .or.                 &
     &                  bez(myix)(7:9).eq.'3.b') then
                                nsig = nsig_tcp3
                        else
                                nsig = nsig_tcp7
                        endif
                elseif(bez(myix)(1:4).eq.'TCSG' .or.                    &
     &                  bez(myix)(1:4).eq.'tcsg' .or.
     &                  bez(myix)(1:4).eq.'TCSP' .or.
     &                  bez(myix)(1:4).eq.'tcsp'  ) then
                        if(bez(myix)(8:10).eq.'3.B' .or.                &
     &                  bez(myix)(8:10).eq.'3.b' .or.                   &
     &                  bez(myix)(9:11).eq.'3.B' .or.                   &
     &                  bez(myix)(9:11).eq.'3.b') then
                                nsig = nsig_tcsg3
                        else
                                nsig = nsig_tcsg7
                        endif
                        if((bez(myix)(5:6).eq.'.4'.and.
     &                  bez(myix)(8:9).eq.'6.')    
     &                  ) then
                                nsig = nsig_tcstcdq
                        endif
                elseif(bez(myix)(1:4).eq.'TCSM' .or.                    &
     &          bez(myix)(1:4).eq.'tcsm') then
                        if(bez(myix)(8:10).eq.'3.B' .or.                &
     &                  bez(myix)(8:10).eq.'3.b' .or.                   &
     &                  bez(myix)(9:11).eq.'3.B' .or.                   &
     &                  bez(myix)(9:11).eq.'3.b') then
                                nsig = nsig_tcsm3
                        else
                                nsig = nsig_tcsm7
                        endif
                 elseif(bez(myix)(1:4).eq.'TCLA' .or.                   &
     &           bez(myix)(1:4).eq.'tcla') then
                        if(bez(myix)(9:11).eq.'7.B' .or.                &
     &                  bez(myix)(9:11).eq.'7.b') then
                                nsig = nsig_tcla7
                        else
                                nsig = nsig_tcla3
                        endif
                elseif(bez(myix)(1:4).eq.'TCDQ' .or.                    &
     &          bez(myix)(1:4).eq.'tcdq') then
                        nsig = nsig_tcdq
                elseif(bez(myix)(1:4).eq.'TCTH' .or.                    &
     &          bez(myix)(1:4).eq.'tcth' ) then                         &
                        if(bez(myix)(8:10).eq.'1.B' .or.                &
     &                  bez(myix)(8:10).eq.'1.b') then
                                nsig = nsig_tcth1
                        elseif(bez(myix)(8:10).eq.'2.B' .or.            &
     &                  bez(myix)(8:10).eq.'2.b') then
                                nsig = nsig_tcth2
                        elseif(bez(myix)(8:10).eq.'5.B' .or.            &
     &                  bez(myix)(8:10).eq.'5.b') then
                                nsig = nsig_tcth5
                        elseif(bez(myix)(8:10).eq.'8.B' .or.            &
     &                  bez(myix)(8:10).eq.'8.b') then
                                nsig = nsig_tcth8
                        endif
                elseif(bez(myix)(1:4).eq.'TCTV' .or.                    &
     &          bez(myix)(1:4).eq.'tctv' ) then
                        if(bez(myix)(8:10).eq.'1.B' .or.                &
     &                  bez(myix)(8:10).eq.'1.b' .or.                   &
     &                  bez(myix)(9:11).eq.'1.B' .or.                   &
     &                  bez(myix)(9:11).eq.'1.b' ) then
                                nsig = nsig_tctv1
                        elseif(bez(myix)(8:10).eq.'2.B' .or.            &
     &                  bez(myix)(8:10).eq.'2.b' .or.                   &
     &                  bez(myix)(9:11).eq.'2.B' .or.                   &
     &                  bez(myix)(9:11).eq.'2.b' ) then
                                nsig = nsig_tctv2
                        elseif(bez(myix)(8:10).eq.'5.B' .or.            &
     &                  bez(myix)(8:10).eq.'5.b' .or.                   &
     &                  bez(myix)(9:11).eq.'5.B' .or.                   &
     &                  bez(myix)(9:11).eq.'5.b') then
                                nsig = nsig_tctv5
                        elseif(bez(myix)(8:10).eq.'8.B' .or.            &
     &                  bez(myix)(8:10).eq.'8.b' .or.                   &
     &                  bez(myix)(9:11).eq.'8.B' .or.                   &
     &                  bez(myix)(9:11).eq.'8.b') then
                                nsig = nsig_tctv8
                        endif
                elseif(bez(myix)(1:3).eq.'TDI' .or.                     &
     &          bez(myix)(1:3).eq.'tdi') then
                        nsig = nsig_tdi
                elseif(bez(myix)(1:4).eq.'TCLP' .or.                    &
     &          bez(myix)(1:4).eq.'tclp' .or.                           &
     &          bez(myix)(1:4).eq.'TCL.' .or.                           &
     &          bez(myix)(1:4).eq.'tcl.') then
                        nsig = nsig_tclp
                elseif(bez(myix)(1:4).eq.'TCLI' .or.                    &
     &          bez(myix)(1:4).eq.'tcli') then
                        nsig = nsig_tcli
                elseif(bez(myix)(1:4).eq.'TCXR' .or.                    &
     &          bez(myix)(1:4).eq.'tcxr') then
                        nsig = nsig_tcxrp
                elseif(bez(myix)(1:5).eq.'TCRYO' .or.                   &
     &          bez(myix)(1:5).eq.'tcryo') then
                        nsig = nsig_tcryo
                elseif(bez(myix)(1:3).eq.'CRY' .or.                     &
     &          bez(myix)(1:3).eq.'cry') then
                        nsig = nsig_cry
                elseif(bez(myix)(1:5).eq.'ELENS' .or.                     &
     &          bez(myix)(1:5).eq.'elens') then
                        nsig = nsig_cry
                elseif(bez(myix)(1:3).eq.'COL' .or.                     &
     &          bez(myix)(1:3).eq.'col') then
                        if(bez(myix)(1:4).eq.'COLM' .or.                &
     &                  bez(myix)(1:4).eq.'colm' .or.                   &
     &                  bez(myix)(1:5).eq.'COLH0' .or.                  &
     &                  bez(myix)(1:5).eq.'colh0') then
                                nsig = nsig_tcth1
                        elseif(bez(myix)(1:5).eq.'COLV0' .or.           &
     &                  bez(myix)(1:5).eq.'colv0') then
                                nsig = nsig_tcth2
                        elseif(bez(myix)(1:5).eq.'COLH1' .or.           &
     &                  bez(myix)(1:5).eq.'colh1') then
!     JUNE2005   HERE WE USE NSIG_TCTH2 AS THE OPENING IN THE VERTICAL
!     JUNE2005   PLANE FOR THE PRIMARY COLLIMATOR OF RHIC; NSIG_TCTH5 STANDS
!     JUNE2005   FOR THE OPENING OF THE FIRST SECONDARY COLLIMATOR OF RHIC
                                nsig = nsig_tcth5
                        elseif(bez(myix)(1:5).eq.'COLV1' .or.           &
     &                  bez(myix)(1:5).eq.'colv1') then
                                nsig = nsig_tcth8
                        elseif(bez(myix)(1:5).eq.'COLH2' .or.           &
     &                  bez(myix)(1:5).eq.'colh2') then
                        nsig = nsig_tctv1
                        endif
                endif

!
!++  Write trajectory for any selected particle
!
          c_length = 0d0
!
!Feb2006
!     SR, 23-11-2005: To avoid binary entries in 'amplitude.dat'
       if ( firstrun ) then
!
                if (rselect.gt.0 .and. rselect.lt.65) then
!                        
                        do j = 1, napx
!
                                xj     = (xv(1,j)-torbx(ie))/1d3
                                xpj    = (yv(1,j)-torbxp(ie))/1d3
                                yj     = (xv(2,j)-torby(ie))/1d3
                                ypj    = (yv(2,j)-torbyp(ie))/1d3
                                pj     = ejv(j)/1d3
                                if (iturn.eq.1.and.j.eq.1) then
                                        sum_ax(ie)=0d0
                                        sum_ay(ie)=0d0
                                endif
                                if (stracki.eq.0.) then
                                        xj  = xj + 0.5d0*c_length*xpj
                                        yj  = yj + 0.5d0*c_length*ypj
                                endif
                                gammax = (1d0 + talphax(ie)**2)/
     &                          tbetax(ie)
                                gammay = (1d0 + talphay(ie)**2)/
     &                          tbetay(ie)
                                if (part_abs(j).eq.0) then
                                        xdebug(ie)=xj
                                        xpdebug(ie)=xpj
                                        ydebug(ie)=yj
                                        ypdebug(ie)=ypj
                                        xdebugN(ie)= xdebug(ie)
     &                                  /sqrt(myemitx0*tbetax(ie))
                                        xpdebugN(ie)=(xdebug(ie)*
     &                                  talphax(ie)+xpdebug(ie)*
     &                                  tbetax(ie))
     &                                  /sqrt(myemitx0*tbetax(ie))
                                        ydebugN(ie)=ydebug(ie)
     &                                  /sqrt(myemity0*tbetay(ie))
                                        ypdebugN(ie)=(ydebug(ie)*
     &                                  talphay(ie)+ypdebug(ie)*
     &                                  tbetay(ie))
     &                                  /sqrt(myemity0*tbetay(ie))
                                        nspx    = sqrt(                 &
     &                                  abs( gammax*(xj)**2 +           &
     &                                  2d0*talphax(ie)*xj*xpj +        &
     &                                  tbetax(ie)*xpj**2 )/myemitx0    &
     &                                  )
                                        nspy    = sqrt(                 &
     &                                  abs( gammay*(yj)**2 +           &
     &                                  2d0*talphay(ie)*yj*ypj +        &
     &                                  tbetay(ie)*ypj**2 )/myemity0    &
     &                                  )
                                        sum_ax(ie)   = sum_ax(ie) + nspx
                                       sqsum_ax(ie)=sqsum_ax(ie)+nspx**2
                                        sum_ay(ie)   = sum_ay(ie) + nspy
                                       sqsum_ay(ie)=sqsum_ay(ie)+nspy**2
                                        nampl(ie)    = nampl(ie) + 1
                                else
                                        nspx = 0d0
                                        nspy = 0d0
                                endif
                                sampl(ie)    = totals
                                ename(ie)    = bez(myix)(1:16)
                        end do
                endif
        endif
!GRD------------------------------------------------------------------------
!GRD HERE WE LOOK FOR ADEQUATE DATABASE INFORMATION
!GRD------------------------------------------------------------------------
        found = .false.
        do j = 1, db_ncoll
                if ((db_name1(j)(1:11).eq.bez(myix)(1:11)) .or.         &
     &          (db_name2(j)(1:11).eq.bez(myix)(1:11))) then
                        if ( db_length(j) .gt. 0d0 ) then
                                found = .true.
                                icoll = j
c                                write(*,*)"database: coll " ,bez(myix)
c                                write(*,*)"database: coll " ,j, myix
                        endif
                endif
        end do
c        if (.not. found .and. firstrun) then
c                write(*,*) 'ERR>  Collimator not found: ', bez(myix)
c        endif

!
!++ For known collimators
!
        if (found) then
!-----------------------------------------------------------------------
!GRD
!GRD NEW COLLIMATION PARAMETERS
!GRD
!-----------------------------------------------------------------------
!++  Get the aperture from the beta functions and emittance
!++  A simple estimate of beta beating can be included that
!++  has twice the betatron phase advance
!
!Mars 2005
                if(.not. do_nsig) nsig = db_nsig(icoll)
!Mars 2005
                scale_bx = (1d0 + xbeat*sin(4*pi*mux(ie)+               &
     &          xbeatphase)  )
                scale_by = (1d0 + ybeat*sin(4*pi*muy(ie)+               &
     &          ybeatphase)  )
!
                if (firstcoll) then
                        scale_bx0 = scale_bx
                        scale_by0 = scale_by
                        firstcoll = .false.
                endif
!
!-------------------------------------------------------------------
!++  Assign nominal OR design beta functions for later
!
 
                if (do_nominal) then
                        bx_dist = db_bx(icoll) * scale_bx / scale_bx0
                        by_dist = db_by(icoll) * scale_by / scale_by0
                else
                        bx_dist = tbetax(ie) * scale_bx / scale_bx0
                        by_dist = tbetay(ie) * scale_by / scale_by0
                endif
!
!-------------------------------------------------------------------
!++  Write beam ellipse at selected collimator
! ---- changed name_sel(1:11) name_sel(1:12) to be checked if feasible!!
                if (                                                    &
     &         ((db_name1(icoll)(1:11) .eq.name_sel(1:11))              &
     &         .or.(db_name2(icoll)(1:11) .eq.name_sel(1:11)))          &
     &         .and. dowrite_dist) then
                        do j = 1, napx
c                       write(45,'(I8,I8,6(1X,E15.7),1X,I8,6(1X,E15.7))')
                       write(45)
     &                          samplenumber, ipart(j), 
     &                          xv(1,j), xv(2,j), yv(1,j), yv(2,j), 
     &                          ejv(j), mys(j),iturn,
     &                          xv(1,j)/1000/sqrt(tbetax(ie)*myemitx0),  
     &                          xv(2,j)/1000/sqrt(tbetay(ie)*myemity0), 
     &                          (xv(1,j)/1000*talphax(ie)+yv(1,j)/1000*
     &                          tbetax(ie))/sqrt(tbetax(ie)*myemitx0),
     &                          (xv(2,j)/1000*talphay(ie)+ yv(2,j)/1000*
     &                          tbetay(ie))/sqrt(tbetay(ie)*myemity0),
     &                          sqrt((xv(1,j)/1000/sqrt(tbetax(ie)*
     &                          myemitx0))**2+((xv(1,j)/1000*talphax(ie)
     &                          +yv(1,j)/1000*tbetax(ie))/
     &                          sqrt(tbetax(ie)*myemitx0))**2),
     &                          sqrt((xv(2,j)/1000/sqrt(tbetay(ie)*
     &                          myemity0))**2+((xv(2,j)/1000*
     &                          talphay(ie)+yv(2,j)/1000*tbetay(ie))/
     &                          sqrt(tbetay(ie)*myemity0))**2)

                       

c                        write(9999,*) 100*samplenumber+ipart(j), iturn,
c     &                  xv(1,j), xv(2,j), yv(1,j), yv(2,j),
c     &
c     &                          xv(1,j)/1000/sqrt(tbetax(ie)*
c     &                          myemitx0),((xv(1,j)/1000*talphax(ie)
c     &                          +yv(1,j)/1000*tbetax(ie))/
c     &                          sqrt(tbetax(ie)*myemitx0)) ,
c     &
c     &                          (xv(2,j)/1000/sqrt(tbetay(ie)*
c     &                          myemity0)),((xv(2,j)/1000*talphay(ie)
c     &                          +yv(2,j)/1000*tbetay(ie))/
c     &                          sqrt(tbetay(ie)*myemity0)),
c     &
c     &                          sqrt((xv(1,j)/1000/sqrt(tbetax(ie)*
c     &                          myemitx0))**2+((xv(1,j)/1000*talphax(ie)
c     &                          +yv(1,j)/1000*tbetax(ie))/
c     &                          sqrt(tbetax(ie)*myemitx0))**2) ,
c     &                          sqrt((xv(2,j)/1000/sqrt(tbetay(ie)*
c     &                          myemity0))**2+((xv(2,j)/1000*talphay(ie)
c     &                          +yv(2,j)/1000*tbetay(ie))/
c     &                          sqrt(tbetay(ie)*myemity0))**2) 
                        end do
                endif

!
!-------------------------------------------------------------------
!++  Output to temporary database and screen
!
                if (iturn.eq.1.and.firstrun) then
                        write(40,*) '# '
                        write(40,*) db_name1(icoll)
                        write(40,*) db_material(icoll)
                        write(40,*) db_length(icoll)
                        write(40,*) db_rotation(icoll)
                        if (db_name1(icoll)(1:5).eq.'ELENS' .or. 
     &                  db_name2(icoll)(1:5).eq.'elens') then
                                write (40,*) db_elens_center_x(icoll),
     &                          db_elens_center_y(icoll)
                                write (40,*) db_elens_curr(icoll),
     &                           db_elens_voltage(icoll)
                                write (40,*)db_elens_r2_ov_r1(icoll)
                                write(40,*)db_elens_op_mode(icoll),
     &                           db_elens_tune(icoll),
     &                           db_elens_delta_tune(icoll),
     &                           db_elens_step_tune(icoll),
     &                           db_elens_step_turns(icoll),
     &                           db_elens_resonant_turns(icoll)
                                write (40,*) db_elens_jitter(icoll),
     &                           db_elens_radial(icoll)
                        elseif (db_name1(icoll)(1:2).eq.'TM' .or.
     &                        db_name2(icoll)(1:2).eq.'tm') then
                                write (40,*) db_tm_center_x(icoll),
     &                          db_tm_center_y(icoll)
                                write (40,*)db_tm_kick(icoll)
                                write(40,*)
     &                           db_tm_tune(icoll),
     &                           db_tm_delta_tune(icoll),
     &                           db_tm_step_tune(icoll),
     &                           db_tm_step_turns(icoll)
                                 write(40,*) db_tm_switch(icoll)
                                
                        else
                                write(40,*) db_offset(icoll)
                        endif
                        write(40,*) tbetax(ie)
                        write(40,*) tbetay(ie)
!
                        write(outlun,*) ' '
                        write(outlun,*)   'Collimator information: '
                        write(outlun,*) ' '
                        write(outlun,*) 'Name:                '         &
     &                  , db_name1(icoll)(1:11)
                        write(outlun,*) 'Material:            '         &
     &                  , db_material(icoll)
                        write(outlun,*) 'Length [m]:          '         &
     &                  , db_length(icoll)
                         write(outlun,*) 'Rotation [rad]:     '         &
     &                  , db_rotation(icoll) 
                        write(outlun,*) 'Offset [m]:          '         &
     &                  ,db_offset(icoll)
                        write(outlun,*) 'Design beta x [m]:   '         &
     &                  ,db_bx(icoll)
                        write(outlun,*) 'Design beta y [m]:   '         &
     &                  ,db_by(icoll)
                        write(outlun,*) 'Optics beta x [m]:   '         &
     &                  ,tbetax(ie)
                        write(outlun,*) 'Optics beta y [m]:   '         &
     &                  ,tbetay(ie)
                endif
!
!-------------------------------------------------------------------
!++  Calculate aperture of collimator
!
                if(db_name1(icoll)(1:4).ne.'COLM') then
                        nsig = nsig + gap_rms_error(icoll)
                        xmax = nsig*sqrt(bx_dist*myemitx0)
                        ymax = nsig*sqrt(by_dist*myemity0)
                        xmax_pencil = (nsig+pencil_offset)*             &
     &                  sqrt(bx_dist*myemitx0)
                        ymax_pencil = (nsig+pencil_offset)*             &
     &                  sqrt(by_dist*myemity0)
                        xmax_nom = db_nsig(icoll)*sqrt(db_bx(icoll)
     &                  *myemitx0)
                        ymax_nom = db_nsig(icoll)*sqrt(db_by(icoll)
     &                  *myemity0)
                        c_rotation = db_rotation(icoll)
                        c_length   = db_length(icoll)
                        c_material = db_material(icoll)
                        c_offset   = db_offset(icoll)
                        c_tilt(1)  = db_tilt(icoll,1)
                        c_tilt(2)  = db_tilt(icoll,2)
c----- valentina ---------------------------------------------      
c Orient the crystal with the beam divergence          
                        if (DB_NAME1(icoll)(1:3).EQ.'CRY') then         !aligning the crystal with the divergence of the beam in that point  
                                if (DB_ROTATION(ICOLL).eq.0) then
                                        Cry_tilt0= -1.*sqrt(myemitx0
     &                                  /tbetax(ie))*talphaX(ie)*nsig
                                elseif (DB_ROTATION(ICOLL).GT.1.5) then !for the moment I have just hor and vertical crystals
                                        Cry_tilt0 =-1.*sqrt(myemity0/
     &                          tbetay(ie))*talphay(ie)* nsig
                                write(*,*) 'vertical crystal'
                                endif
                        
                                Cry_length=db_length(icoll)     
                                C_xmax=db_cry_rmax(icoll)
                                C_ymax=db_cry_zmax(icoll)
                                C_orient=db_cry_orient(icoll)
                                Alayer=db_cry_alayer(icoll)
                                miscut=db_miscut(icoll)
c                                write(*,*)"from db miscut",miscut
                                Cry_tilt = DB_CRY_TILT(ICOLL)+Cry_tilt0 ! the total alignment of the crystal
                                Cry_bending=DB_LENGTH(ICOLL)
     &                          /DB_CRY_RCURV(ICOLL)
                                Rcurv    = DB_CRY_RCURV(ICOLL)
                                if (Cry_tilt .ge. -Cry_bending ) then
                               C_LENGTH=Rcurv*(SIN(Cry_bending+Cry_tilt)
     &                  -       SIN(Cry_tilt))
                                else
                               C_LENGTH=Rcurv*(SIN(Cry_bending-Cry_tilt)
     &                  +       SIN(Cry_tilt))
                                endif
c                                IF(ITURN.eq.1)write(*,*)'div. @ cry: ',
c     1                          Cry_tilt0,'tilt DB:',DB_CRY_TILT(ICOLL),
c     2                          "total tilt Cry_tilt",Cry_tilt
c                                write(*,*)"debug track.f; c_length",
c     &                          C_LENGTH 
                               
c                               write(*,*)"crystal bending = ", Cry_bending
c                               write(*,*)"crystal lenght = ", C_LENGTH
c                               write(*,*)"cry tilt = ", Cry_tilt,"=",DB_CRY_TILT(ICOLL),
c     1                         "+",Cry_tilt0
                        endif
c----------------------------------------------------------------------

          
                        calc_aperture =sqrt( xmax**2 *cos(c_rotation)**2
     &                    + ymax**2 * sin(c_rotation)**2 )
                        nom_aperture=sqrt(xmax_nom**2*cos(c_rotation)**2
     &                   + ymax_nom**2 * sin(c_rotation)**2 )
!
                        pencil_aperture =                               
     &                       sqrt( xmax_pencil**2 * cos(c_rotation)**2  
     &                       + ymax_pencil**2 * sin(c_rotation)**2 )
!
!++  Get x and y offsets at collimator center point
!
                        x_pencil(icoll) = xmax_pencil *(cos(c_rotation))
                        y_pencil(icoll) = ymax_pencil *(sin(c_rotation))
!
!++  Get corresponding beam angles (uses xp_max)
!
                        xp_pencil(icoll) =              
     &                   -1d0 * sqrt(myemitx0/tbetax(ie))*talphax(ie)   
     &                   * xmax / sqrt(myemitx0*tbetax(ie))
!     
                        yp_pencil(icoll) =                              
     &                    -1d0 * sqrt(myemity0/tbetay(ie))*talphay(ie)  
     &                   * ymax / sqrt(myemity0*tbetay(ie))
!
                        xp_pencil0 = xp_pencil(icoll)
                        yp_pencil0 = yp_pencil(icoll)
!
                        pencil_dx(icoll)  =                             
     &                      sqrt( xmax_pencil**2 * cos(c_rotation)**2   
     &                      + ymax_pencil**2 * sin(c_rotation)**2 )     
     &                      - calc_aperture
!++ TW -- tilt for of jaw for pencil beam
!++ as in Ralphs orig routine, but not in collimate subroutine itself
!               nprim = 3
!               if ( (icoll.eq.ipencil) &
!     &           icoll.le.nprim .and. (j.ge.(icoll-1)*nev/nprim)        &
!     &           .and. (j.le.(icoll)*nev/nprim))) then
! this is done for every bunch (64 particle bucket)
! important: Sixtrack calculates in "mm" and collimate2 in "m"
! therefore 1E-3 is used to  
                        if ((icoll.eq.ipencil).and.(iturn.eq.1)) then
!!                      write(*,*) " ************************************** "
!!                      write(*,*) " * INFO> seting tilt for pencil beam  * "
!!                      write(*,*) " ************************************** "
                                c_tilt(1) =c_tilt(1)+(xp_pencil0*cos(
     &                          c_rotation)+sin(c_rotation)*yp_pencil0)
                                write(*,*)
     &                          "INFO> Changed tilt1 ICOLL to  ANGLE: ",
     &                          icoll, c_tilt(1)
!
!! respects if the tilt symmetric or not, for systilt_antiymm c_tilt is 
!! -systilt + rmstilt otherwise +systilt + rmstilt
!!               if (systilt_antisymm) then
!! to align the jaw/pencil to the beam always use the minus regardless which 
!! orientation of the jaws was used (symmetric/antisymmetric) 
                                c_tilt(2) = c_tilt(2) -1.*(xp_pencil0
     &                          *cos(c_rotation)+ sin(c_rotation)*
     &                          yp_pencil0)
                                write(*,*)
     &                          "INFO> Changed tilt2 ICOLL to  ANGLE: ",
     &                          icoll, c_tilt(2)
                        endif
!++ TW -- tilt angle changed (added to genetated on if spec. in fort.3) 
!JUNE2005   HERE IS THE SPECIAL TREATMENT...
                elseif(db_name1(icoll)(1:4).eq.'COLM') then
!
                        xmax = nsig_tcth1*sqrt(bx_dist*myemitx0)        
!     
                        c_rotation = db_rotation(icoll)
                        c_length   = db_length(icoll)
                        c_material = db_material(icoll)
                        c_offset   = db_offset(icoll)
                        c_tilt(1)  = db_tilt(icoll,1)
                        c_tilt(2)  = db_tilt(icoll,2)
                        calc_aperture = xmax
                        nom_aperture = ymax
!
                endif
!
!-------------------------------------------------------------------
!++  Further output
!
                if(firstrun) then
                        if (iturn.eq.1) then
                                write(outlun,*) xp_pencil(icoll), 
     &                          yp_pencil(icoll),pencil_dx(icoll)
                                write(outlun,'(a,i4)') 
     &                          'Collimator number:   ',icoll
                                write(outlun,*) 
     &                          'Beam size x [m]:     ',sqrt(tbetax(ie)*
     &                          myemitx0)
                                write(outlun,*) 'Beam size y [m]:     ' 
     &                          ,sqrt(tbetay(ie)*myemity0)
                                write(outlun,*) 
     &                          'Divergence x [urad]:     ',
     &                          1d6*xp_pencil(icoll)
                                write(outlun,*) 
     &                          'Divergence y [urad]:     ',
     &                          1d6*yp_pencil(icoll)
                                write(outlun,*) 'Aperture (nom) [m]:  ' 
     &                          ,nom_aperture
                                write(outlun,*) 'Aperture (cal) [m]:  '
     &                          ,calc_aperture
                                write(outlun,*) 
     &                          'Collimator halfgap [sigma]:  ',nsig
                                write(outlun,*) 
     &                          'RMS error on halfgap [sigma]:  '
     &                          ,gap_rms_error(icoll)
                                write(outlun,*) ' '
                                write(43,'(i7.5,1x,a,4(1x,e13.5),1x,a,
     &                          6(1x,e13.5))')icoll,
     &                          db_name1(icoll)(1:12),db_rotation(icoll)
     &                          ,tbetax(ie), tbetay(ie), calc_aperture, 
     &                          db_material(icoll),db_length(icoll),    
     &                          sqrt(tbetax(ie)*myemitx0), 
     &                          sqrt(tbetay(ie)*myemity0),   
     &                          db_tilt(icoll,1),db_tilt(icoll,2),  nsig
                                if ( n_slices.le.1 .or. 
     &                          db_name1(icoll)(1:3) .eq. 'CRY' ) then
                                write(55,'(a,1x,i7.5,5(1x,e13.5),1x,a)')
     &                                  db_name1(icoll),
     &                                  1,calc_aperture,                
     &                                  db_offset(icoll),
     &                                  db_tilt(icoll,1),
     &                                  db_tilt(icoll,2), 
     &                                  db_length(icoll),
     &                                  db_material(icoll)
                                endif

                        endif

                endif
!
!++  Assign aperture which we define as the FULL width (factor 2)!!!
!
!JUNE2005 AGAIN, SOME SPECIFIC STUFF FOR RHIC
                 if(db_name1(icoll)(1:4).eq.'COLM') then
                        nom_aperture = 2d0*nom_aperture
                endif
                c_aperture = 2d0*calc_aperture
!
!GRD-------------------------------------------------------------------
c      if(firstrun.and.iturn.eq.1.and.icoll.eq.7) then
c      open(unit=99,file='distsec')
c      do j=1,napx
c      write(99,'(4(1X,E15.7))') xv(1,j),yv(1,j),xv(2,j),yv(2,j)
c      enddo
c      close(99)
c      endif
!GRD-------------------------------------------------------------------
!++  Copy particle data to 1-dim array and go back to meters
!
                do j = 1, napx
                        rcx(j)  = (xv(1,j)-torbx(ie))/1d3
                        rcxp(j) = (yv(1,j)-torbxp(ie))/1d3
                        rcy(j)  = (xv(2,j)-torby(ie))/1d3
                        rcyp(j) = (yv(2,j)-torbyp(ie))/1d3
                        rcp(j)  = ejv(j)/1d3
                        rcs(j)  = 0d0
                        part_hit_before(j) = part_hit(j)
                        rcx0(j)  = rcx(j)
                        rcxp0(j) = rcxp(j)
                        rcy0(j)  = rcy(j)
                        rcyp0(j) = rcyp(j)
                        rcp0(j)  = rcp(j)
                        ejf0v(j) = ejfv(j)
!
!++  For zero length element track back half collimator length
!
                        if (stracki.eq.0.) then
                                rcx(j) = rcx(j) - 0.5d0*c_length*rcxp(j)
                                rcy(j)= rcy(j) - 0.5d0*c_length*rcyp(j)
                        else
                                Write(*,*) 
     &                          "ERROR: Non-zero length collimator!"
                                STOP
                        endif
c                        flukaname(j) = ipart(j)+100*samplenumber
!
                end do
!
!++  Do the collimation tracking
!
                enom_gev = myenom*1d-3
!
!++  Allow primaries to be one-sided, if requested
!
                if (((db_name1(icoll)(1:3).eq.'TCP' .or.            
     &          db_name1(icoll)(1:3).eq.'COL' .or.
     &          db_name1(icoll)(1:4).eq.'TCSP'   )                      
     &          .and. do_oneside)
     &          .or. (db_name1(icoll)(1:3).eq.'CRY')) then
!SEPT2008 valentina: cry is always one sided
                        onesided = .true.
                else
                        onesided = .false.
                endif
!
!Force the treatment of the TCDQ equipment as a onsided collimator.
!Both for Beam 1 and Beam 2, the TCDQ is at positive x side.
!              if(db_name1(icoll)(1:4).eq.'TCDQ' ) onesided = .true.
! to treat all collimators onesided 
! -> only for worst case TCDQ studies
                if(db_name1(icoll)(1:5).eq.'TCXRP') onesided = .true.
                if(db_name1(icoll)(1:11).eq.'TCP.1MM.EXP')
     1              onesided = .true.    !scraper for the UA9 experiment
                if(db_name1(icoll)(1:4).eq.'TCDQ') onesided = .true.
!
                if (found) then
!
                        if(db_name1(icoll)(1:4).eq.'COLM') then
!
                                        call collimaterhic(c_material,  
     &                                  c_length, c_rotation,           
     &                                  c_aperture, nom_aperture,       
     &                                  c_offset, c_tilt,               
     &                                  rcx, rcxp, rcy, rcyp, rcp, rcs, 
     &                                  napx,enom_gev,part_hit,part_abs,
     &                                  part_impact, part_indiv, 
     &                                  part_linteract, onesided,       
     &                                  flukaname)
                                        
!-------     valentina   SEPT 2008------------------------------
! add crystal collimation routine
                                        
                        elseif (db_name1(icoll)(1:3).eq.'CRY') then
                                call collimate_cry ( db_name1(icoll) , 
     &                           C_MATERIAL, C_LENGTH, C_ROTATION,
     1                          C_APERTURE, C_OFFSET, C_TILT,
     1                          rcx, rcxp, rcy, rcyp, rcp, rcs,
     2                          napx,enom_gev, part_hit, 
     3                          PART_ABS, part_impact, part_indiv, 
     &                          part_linteract,tbetax(ie),talphax(ie),
     &                          tbetay(ie),talphay(ie),EMITX0,EMITY0,
     6                          flukaname, secondary,dowrite_impact)   

c||!|!|!|!|!|!|!||! Tune modulation quadrupole element TM_QUAD  !|!|!|!|!|!||!|
                        elseif (db_name1(icoll)(1:7).eq.'TM_QUAD') then
                                call collimate_TM (db_tm_kick(icoll),
     &                          c_length, db_rotation(icoll),
     &                          db_tm_center_x(icoll), 
     &                          db_tm_center_y(icoll), 
     &                          db_tm_tune(icoll),
     &                          db_tm_mult_tune(icoll),
     &                          db_tm_delta_tune(icoll),
     &                          db_tm_step_tune(icoll),
     &                          db_tm_step_turns(icoll),
     &                          db_tm_switch(icoll),2,
     &                          rcx, rcxp, rcy, rcyp, 
     &                          rcp, rcs, napx,enom_gev) 
c||!|!|!|!|!|!|!||! Tune modulation quadrupole element TM_DIPOLE  !|!|!|!|!|!||!|
                        elseif (db_name1(icoll)(1:6).eq.'TM_DIP') then
                                call collimate_TM (db_tm_kick(icoll),
     &                          c_length, db_rotation(icoll),
     &                          db_tm_center_x(icoll), 
     &                          db_tm_center_y(icoll), 
     &                          db_tm_tune(icoll),
     &                          db_tm_mult_tune(icoll),
     &                          db_tm_delta_tune(icoll),
     &                          db_tm_step_tune(icoll),
     &                          db_tm_step_turns(icoll),
     &                          db_tm_switch(icoll),1,
     &                          rcx, rcxp, rcy, rcyp, 
     &                          rcp, rcs, napx,enom_gev) 
c------------- ----------- ------------------------- ----------
!SEPT2008 JCSMITH
! add in electron lense collimator
c------------- ----------- ------------------------- ----------
!SEPT2008 JCSMITH
! add in electron lense collimator
                        elseif (db_name1(icoll)(1:5).eq.'ELENS') then
                                if (db_elens_curr(icoll).gt.0 
     &                            .and. (db_elens_voltage(icoll).gt.0.))
     &                           call collimate_elense ( c_aperture/2,
     &                          db_elens_curr(icoll), 
     &                          c_length, 
     &                          db_elens_voltage(icoll),
     &                          db_elens_r2_ov_r1(icoll),
     &                          db_elens_center_x(icoll),
     &                          db_elens_center_y(icoll),
     &                          db_elens_op_mode(icoll),
     &                          db_elens_tune(icoll),
     &                          db_elens_mult_tune(icoll), 
     &                          db_elens_delta_tune(icoll),
     &                          db_elens_step_tune(icoll),
     &                          db_elens_step_turns(icoll),
     &                          db_elens_resonant_turns(icoll),
     &                          db_elens_jitter(icoll),
     &                          db_elens_radial(icoll),
     &                          rcx, rcxp,
     &                          rcy, rcyp,rcp, rcs, napx, enom_gev, 
     &                          part_hit, part_abs, part_impact, 
     &                          part_indiv, part_linteract, flukaname)
!
!     SR, 29-08-2005: Slice the collimator jaws in 'n_slices' pieces
!     using two 4th-order polynomial fits. For each slices, the new
!     gaps and centre are calculates
!     It is assumed that the jaw point closer to the beam defines the
!     nominal aperture.
!
!     SR, 01-09-2005: new official version - input assigned through
!     the 'fort.3' file.
!CB
                        elseif (n_slices.gt.1d0 .and.
     &                  (db_name1(icoll)(1:4).eq.'TCSG'
     &                  .or. db_name1(icoll)(1:3).eq.'TCP' 
     &                  .or. db_name1(icoll)(1:4).eq.'TCLA'
     &                  .or. db_name1(icoll)(1:3).eq.'TCT' 
     &                  .or. db_name1(icoll)(1:4).eq.'TCLI' 
     &                  .or. db_name1(icoll)(1:4).eq.'TCSP' 
     &                  .or. db_name1(icoll)(1:4).eq.'TCL.')) then      
                                if (firstrun) then
                                        write(*,*) 
     &                                  'INFO> slice - Collimator ',
     &                                  db_name1(icoll), ' sliced in ',
     &                                  n_slices,' pieces!'
                                endif
!!     In this preliminary try, all secondary collimators are sliced.
!!     Slice only collimators with finite length!!
!!     Slice the primaries, to have more statistics faster!
!!
!!     Calculate longitudinal positions of slices and corresponding heights
!!     and angles from the fit parameters.
!!     -> MY NOTATION: y1_sl: jaw at x > 0; y2_sl: jaw at x < 0;
!!     Note: here, take (n_slices+1) points in order to calculate the
!!           tilt angle of the last slice!!
!     CB:10-2007 deformation of the jaws scaled with length
                                do jjj=1,n_slices+1
                                        x_sl(jjj) = (jjj-1) * c_length/
     &                                  dble(n_slices)
                                        y1_sl(jjj) =  fit1_1 +
     &                                  fit1_2*x_sl(jjj) +
     &                                  fit1_3/c_length*(x_sl(jjj)**2)+
     &                                  fit1_4*(x_sl(jjj)**3) +
     &                                  fit1_5*(x_sl(jjj)**4) +
     &                                  fit1_6*(x_sl(jjj)**5)
!     
                                        y2_sl(jjj) = -1d0 * (fit2_1 +
     &                                  fit2_2*x_sl(jjj) +
     &                                  fit2_3/c_length*(x_sl(jjj)**2)+
     &                                  fit2_4*(x_sl(jjj)**3) +
     &                                  fit2_5*(x_sl(jjj)**4) +
     &                                  fit2_6*(x_sl(jjj)**5))
                                enddo
!     Apply the slicing scaling factors (ssf's):
!     
!                               do jjj=1,n_slices+1
!                                        y1_sl(jjj) = ssf1 * y1_sl(jjj)
!                                        y2_sl(jjj) = ssf2 * y2_sl(jjj)
!                               enddo
!
!     CB:10-2007 coordinates rotated of the tilt 
                                do jjj=1,n_slices+1
                                        y1_sl(jjj) = ssf1 * y1_sl(jjj)
                                        y2_sl(jjj) = ssf2 * y2_sl(jjj)
! CB code
                                        x1_sl(jjj)=x_sl(jjj)*
     &                                  cos(db_tilt(icoll,1))- 
     &                                  y1_sl(jjj)*sin(db_tilt(icoll,1))
                                        x2_sl(jjj)=x_sl(jjj)*
     &                                  cos(db_tilt(icoll,2))- 
     &                                  y2_sl(jjj)*sin(db_tilt(icoll,2))
                                        y1_sl(jjj) = y1_sl(jjj)*
     &                                  cos(db_tilt(icoll,1))+ 
     &                                  x_sl(jjj)*sin(db_tilt(icoll,1))
                                        y2_sl(jjj) = y2_sl(jjj)*
     &                                  cos(db_tilt(icoll,2))+ 
     &                                  x_sl(jjj)*sin(db_tilt(icoll,2))
                                enddo
!     Sign of the angle defined differently for the two jaws!
                                do jjj=1,n_slices
                                        angle1(jjj) = ((y1_sl(jjj+1)-
     &                                  y1_sl(jjj))
     &                                  /( x1_sl(jjj+1)-x1_sl(jjj) ))
                                        angle2(jjj) =(( y2_sl(jjj+1)-
     &                                  y2_sl(jjj))
     &                                  /( x2_sl(jjj+1)-x2_sl(jjj) ))
                                enddo
!
!     Sign of the angle defined differently for the two jaws!
!     For both jaws, look for the 'deepest' point (closest point to beam)
!     Then, shift the vectors such that this closest point defines
!     the nominal aperture
!     Index here must go up to (n_slices+1) in case the last point is the
!     closest (and also for the later calculation of 'a_tmp1' and 'a_tmp2')
!
!     SR, 01-09-2005: add the recentring flag, as given in 'fort.3' to
!     choose whether recentre the deepest point or not
                                max_tmp = 1e6
                                do jjj=1, n_slices+1
                                        if ( y1_sl(jjj).lt.max_tmp )then
                                                max_tmp = y1_sl(jjj)
                                        endif
                                enddo
                                do jjj=1, n_slices+1
                                        y1_sl(jjj) = y1_sl(jjj) -max_tmp
     &                                  * recenter1+ 0.5 *c_aperture
                                enddo
                                max_tmp = -1e6
                                do jjj=1, n_slices+1
                                        if ( y2_sl(jjj).gt.max_tmp )then
                                                max_tmp = y2_sl(jjj)
                                        endif
                                enddo
                                do jjj=1, n_slices+1
                                        y2_sl(jjj) = y2_sl(jjj) -max_tmp
     &                                  * recenter2- 0.5 *c_aperture
                                enddo
!
!!     Check the collimator jaw surfaces (beam frame, before taking into
!!     account the azimuthal angle of the collimator)
                                if (firstrun)  write(*,*) 
     &                          'Slicing collimator ',db_name1(icoll)
                                
!
!     Now, loop over the number of slices and call collimate2 each time!
!     For each slice, the corresponding offset and angle are to be used.
                                do jjj=1,n_slices
!
!     First calculate aperture and centre of the slice
!     Note that:
!     (1)due to our notation for the angle sign,
!     the rotation point of the slice (index j or j+1)
!     DEPENDS on the angle value!!
!     (2) New version of 'collimate2' is required: one must pass
!     the slice number in order the calculate correctly the 's'
!     coordinate in the impact files.
!
!     Here, 'a_tmp1' and 'a_tmp2' are, for each slice, the closest
!     corners to the beam
                                        if ( angle1(jjj).gt.0d0 ) then
                                                a_tmp1 = y1_sl(jjj)
                                        else
                                                a_tmp1 = y1_sl(jjj+1)
                                        endif
                                        if ( angle2(jjj).lt.0d0 ) then
                                                a_tmp2 = y2_sl(jjj)
                                        else
                                                a_tmp2 = y2_sl(jjj+1)
                                        endif
!!     Write down the information on slice centre and offset
!!
!     Be careful! the initial tilt must be added!
!     We leave it like this for the moment (no initial tilt)
!                                       c_tilt(1) = c_tilt(1) + angle1(jjj)
!                                       c_tilt(2) = c_tilt(2) + angle2(jjj)
                                        c_tilt(1) = angle1(jjj)
                                        c_tilt(2) = angle2(jjj)
!     New version of 'collimate2' is required: one must pass the
!     slice number in order the calculate correctly the 's'
!     coordinate in the impact files.
!     +                                 a_tmp1 - a_tmp2,
!     +                                 0.5 * ( a_tmp1 + a_tmp2 ),
! -- TW SEP07 added compatility for tilt, gap and ofset errors to slicing
! -- TW gaprms error is already included in the c_aperture used above  
! -- TW tilt error is added to y1_sl and y2_sl therfore included in 
! -- TW angle1 and angle2 no additinal changes needed 
! -- TW offset error directly added to call of collimate2
                                        if (firstrun) then
                                                write(55,'(a,1x,i7.5,
     &                                          5(1x,e13.5),1x ,a)')
     &                                          db_name1(icoll)(1:12), 
     &                                          jjj,(a_tmp1 - a_tmp2)   
     &                                          /2d0,0.5*(a_tmp1+a_tmp2)
     &                                          +c_offset, c_tilt(1),
     &                                          c_tilt(2),         
     &                                          c_length/dble(n_slices),
     &                                          db_material(icoll)
                                        endif
                                        name_coll=db_name1(icoll)
                                        call collimate2(name_coll,
     &                                  c_material,
     &                                  c_length/dble(n_slices),
     &                                  c_rotation, a_tmp1 - a_tmp2, 
     &                                  0.5 * ( a_tmp1 + a_tmp2 ) 
     &                                  + c_offset,   c_tilt, rcx, rcxp,
     &                                  rcy, rcyp,rcp, rcs, napx, 
     &                                  enom_gev,part_hit, part_abs, 
     &                                  part_impact, part_indiv,  
     &                                  part_linteract, onesided, 
     &                                  flukaname,secondary, jjj)
                                enddo
                        else
!     Treatment of non-sliced collimators!
                                name_coll=db_name1(icoll)
                                call collimate2(name_coll,
     &                          c_material, c_length, c_rotation,
     &                          c_aperture, c_offset, c_tilt, rcx, rcxp,
     &                          rcy, rcyp,rcp, rcs, napx, enom_gev, 
     &                          part_hit, part_abs, part_impact, 
     &                          part_indiv, part_linteract,         
     &                          onesided, flukaname, secondary, 1)
                         endif
! end of check for 'found'
                endif
!++  Output information:

!valentina: increase turn number. hit/abs flag is set to 100 000 000 *element# +
! turn#)
!++  PART_HIT(MAX_NPART)     Hit flag for last hit (10000*element# + turn#)
!++  PART_ABS(MAX_NPART)     Abs flag (10000*element# + turn#)
!++  PART_IMPACT(MAX_NPART)  Impact parameter (0 for inner face)
!++  PART_INDIV(MAX_NPART)   Divergence of impacting particles
!------------------------------------------------------------------------------
!++  Calculate average impact parameter and save info for all
!++  collimators. Copy information back and do negative drift.
                n_impact = 0
                n_absorbed = 0
                sum      = 0d0
                sqsum    = 0d0
!++  Copy particle data back and do path length stuff; check for absorption
!++  Add orbit offset back.
                do j = 1, napx
!APRIL2005 IN ORDER TO GET RID OF NUMERICAL ERRORS, JUST DO THE TREATMENT FOR
!APRIL2005 IMPACTING PARTICLES...
                        if ( (part_hit(j).eq.(100000000*ie+iturn))
     &                  .or. (DB_NAME1(ICOLL)(1:2).eq."TM") ) then
!++  For zero length element track back half collimator length
                                if (stracki.eq.0.) then
                                        rcx(j)  = rcx(j) - 
     &                                  0.5d0*c_length*rcxp(j)
                                        rcy(j)  = rcy(j) - 
     &                                  0.5d0*c_length*rcyp(j)
                                endif
c------------------write initial-final coordinates for crystal -----------------------
!SEPT2008 valentina: write special output files for cry
                                if (write_c_out  .and.
     1                          DB_NAME1(ICOLL)(1:3).EQ.'CRY'
     1                          ) THEN  
c-                                      initial coordinates
                                        WRITE(881,'(i7,i4,2x,i4,2x,i4,
     &                                  2x,a,2x,5(f15.8,2x))')
     1                                  ipart(j)+100*samplenumber,ITURN,
     &                                  bool_proc_old(j),ICOLL,
     2                                  DB_MATERIAL(ICOLL),rcx0(J),
     &                                 rcxp0(J),rcy0(j),rcyp0(J),rcP0(J)!write down real and noormalized coordinates
                                                                       !before and after the kick (always at the middle of the element)
                                        X_NORM0=rcx0(J)/SQRT(tbetax(ie))
     &                                  /sqrt(myEMITX0)
                                        XP_NORM0=(rcx0(J)*talphax(IE) + 
     &                                  rcxp0(J)*tbetax(IE))/
     1                                  SQRT(tbetax(ie))/ sqrt(myEMITX0)
                                        Y_NORM0=rcY0(J)/sqrt(tbetay(ie))
     &                                  /sqrt(myEMITY0)
                                        YP_NORM0=(rcY0(J)*talphay(IE) + 
     &                                  rcyp0(J)*tbetay(IE))/
     1                                  SQRT(tbetay(ie))/ sqrt(myEMITY0)

                                        WRITE(883,'(i7,i4,2x,i4,2x,i4,2x
     &                                  ,a,2x,7(f15.8,2x))') 
     2                                  ipart(j)+100*samplenumber,ITURN,
     &                                  bool_proc_old(j),ICOLL,
     3                                  DB_MATERIAL(ICOLL),X_NORM0,
     &                                  XP_NORM0,Y_NORM0,YP_NORM0,
     &                                  SQRT(X_NORM0**2+XP_NORM0**2),
     5                                  SQRT(Y_NORM0**2+YP_NORM0**2),
     &                                  rcP0(j)
                                        if (part_abs(j) .eq. 0) then 
                                          WRITE(882,'(i7,4(i4,2x)
     &                                    ,a,2x,5(f15.8,2x))')
     1                                    ipart(j)+100*samplenumber,
     2                                    ITURN,bool_proc_old(j), 
     &                                    bool_proc(j),ICOLL,
     2                                    DB_MATERIAL(ICOLL),rcX(J),
     2                                    rcXP(J),rcY(J),rcYP(J),rcP(J)
c
                                          X_NORM =rcX(J)/SQRT(tbetax(ie)
     &                                     )/sqrt(myEMITX0)
                                          XP_NORM = (rcX(J)*talphax(IE)+
     &                                     rcXP(J)*tbetax(IE))/SQRT(
     1                                     tbetax(ie))/ sqrt(myEMITX0)
                                          Y_NORM =rcY(J)/SQRT(tbetay(ie)
     &                                     )/sqrt(myEMITY0)
                                          YP_NORM = (rcY(J)*talphay(IE)+ 
     &                                    rcYP(J)*tbetay(IE))/SQRT(
     1                                     tbetay(ie))/ sqrt(myEMITY0)
            
                                          WRITE(884,'(i7,4(i4,2x),a,2x,
     &                                    7(f15.8,2x))') ipart(j)+100*
     &                                    samplenumber, ITURN , 
     &                                    bool_proc_old(j),bool_proc(j),
     3                                    ICOLL,DB_MATERIAL(ICOLL)
     3                                    ,X_NORM,XP_NORM, Y_NORM,     
     3                                    YP_NORM,SQRT(X_NORM**2+
     &                                    XP_NORM**2),SQRT(Y_NORM**2+
     4                                    YP_NORM**2), rcP(j)

                                          write(885,*)
     1                                    ipart(j)+100*samplenumber,
     2                                    ITURN,bool_proc_old(j), 
     &                                    bool_proc(j),ICOLL,
     2                                    DB_MATERIAL(ICOLL),
     &                                    rcx0(J),rcxp0(J),
     &                                    rcy0(J),rcyp0(J),!write down real and noormalized coordinates
     &                                    rcXP(J)-rcXP0(J),
     &                                    rcYP(J)-rcYP0(J),
     &                                    rcp0(J)-rcp(j),
     &                                    c_aperture,cry_tilt0
                                        endif
                                endif
c----------------------------------o----------------------------------------
c-------------------------------- initial-final normalized amplitudes for elense--
c
                                if (write_elens_out  .and.
     1                          DB_NAME1(ICOLL)(1:5).EQ.'ELENS'
     1                          ) THEN    
c                                       note that, since the drift back has already
C                                       been applied, must be rcx0==rcx and rcy==rcy0         
c                                        write(887,*) ipart(j)+100*
c     &                                  samplenumber,iturn,
c     &                                  rcx0(J),rcxp0(J),      
c     &                                  rcY0(J),rcyp0(J), 
c     &                                  rcX(J),rcXP(J),           
c     &                                  rcY(J),rcYP(J), 
c     &                                  sqrt((rcXP(J)-rcXP0(J))**2+
c     &                                       (rcYP(J)-rcYP0(J))**2),
c     &                                  rcXP(J)-rcXP0(J), 
c     &                                  rcYP(J)-rcYP0(J),
c     &                                  rcp0(J),rcp(J)

                                        !calculate norm coord before the kick
                                        X_NORM0=(rcx0(J))
     &                                  /SQRT(tbetax(ie))
     &                                  /sqrt(myEMITX0)
                                        XP_NORM0=(rcx0(J)*talphax(IE) + 
     &                                  rcxp0(J)*tbetax(IE))/
     1                                  SQRT(tbetax(ie))/ sqrt(myEMITX0)
                                        PHI_X0 = ATAN(XP_NORM0/X_NORM0)
                                   IF (X_NORM0.lt.0)
     &                                       PHI_X0=PHI_X0+4d0*atan(1d0)
                                        Y_NORM0=rcY0(J)/sqrt(tbetay(ie))
     &                                  /sqrt(myEMITY0)
                                        YP_NORM0=(rcY0(J)*talphay(IE) + 
     &                                  rcyp0(J)*tbetay(IE))/
     1                                  SQRT(tbetay(ie))/ sqrt(myEMITY0)
                                        PHI_Y0 = ATAN(YP_NORM0/Y_NORM0)
                                   IF (Y_NORM0.lt.0) 
     &                                       PHI_Y0=PHI_Y0+4d0*atan(1d0)
                                        !calculate norm coord after  the kick
                                        X_NORM =rcX(J)
     &                                    /SQRT(tbetax(ie)
     &                                    )/sqrt(myEMITX0)
                                        XP_NORM = (rcX0(J)*talphax(IE)+
     &                                    rcXP(J)*tbetax(IE))/SQRT(
     1                                    tbetax(ie))/ sqrt(myEMITX0)
                                        PHI_X = ATAN(XP_NORM/X_NORM)
                                   IF (X_NORM.lt.0)
     &                                       PHI_X=PHI_X+4d0*atan(1d0)
                                        Y_NORM =rcY0(J)/SQRT(tbetay(ie)
     &                                    )/sqrt(myEMITY0)
                                        YP_NORM = (rcY(J)*talphay(IE)+ 
     &                                   rcYP(J)*tbetay(IE))/SQRT(
     1                                   tbetay(ie))/ sqrt(myEMITY0)
                                        PHI_Y = ATAN(YP_NORM/Y_NORM)
                                   IF (Y_NORM.lt.0) 
     &                                       PHI_Y=PHI_Y+4d0*atan(1d0)
                                        WRITE(888) 
     2                                  samplenumber,ipart(j),ITURN,
     3                                  X_NORM0,
     &                                  XP_NORM0,Y_NORM0,YP_NORM0,
c     3                                  X_NORM,
c     &                                  XP_NORM,Y_NORM,YP_NORM,
c     &                                  SQRT(X_NORM0**2+XP_NORM0**2),
c     &                                  SQRT(Y_NORM0**2+YP_NORM0**2),
c     &                                  PHI_X0,PHI_Y0,
c     &                                  SQRT(X_NORM**2+XP_NORM**2),
c     &                                  SQRT(Y_NORM**2+YP_NORM**2),
c     &                                  PHI_X , PHI_Y,
     &                                  SQRT(X_NORM**2+XP_NORM**2)-
     *                                     SQRT(X_NORM0**2+XP_NORM0**2),
     &                                  SQRT(Y_NORM**2+YP_NORM**2)-
     &                                     SQRT(Y_NORM0**2+YP_NORM0**2),
     &                                  SQRT(X_NORM**2+XP_NORM**2),
     &                                  SQRT(Y_NORM**2+YP_NORM**2)
                                        
                                        write(887)
     &                                  samplenumber,ipart(j),iturn,
     &                                  rcx0(J),rcxp0(J),      
     &                                  rcY0(J),rcyp0(J), 
c     &                                  rcX(J),rcXP(J),           
c     &                                  rcY(J),rcYP(J), 
     &                                  rcXP(J)-rcXP0(J), 
     &                                  rcYP(J)-rcYP0(J),
     &                                  sqrt((rcXP(J)-rcXP0(J))**2+
     &                                       (rcYP(J)-rcYP0(J))**2)
c     &                                  SQRT(X_NORM**2+XP_NORM**2),
c     &                                  SQRT(Y_NORM**2+YP_NORM**2)
                                endif
cc-------------------------------- initial-final normalized amplitudes for tune modulation TM--
c
                                if ( 
     &                          write_TM_out  .and.
     1                          (DB_NAME1(ICOLL)(1:2).EQ.'TM').and.
     &                          (part_abs(j) .eq. 0)
     1                          ) THEN    
c                                       note that, since the drift back has already
c                                       been applied, must be rcx0==rcx and rcy==rcy0         
                                        !calculate norm coord before the kick
                                        X_NORM0=(rcx0(J))
     &                                  /SQRT(tbetax(ie))
     &                                  /sqrt(myEMITX0)
                                        XP_NORM0=(rcx0(J)*talphax(IE) + 
     &                                  rcxp0(J)*tbetax(IE))/
     1                                  SQRT(tbetax(ie))/ sqrt(myEMITX0)
                                        PHI_X0 = ATAN(XP_NORM0/X_NORM0)
                                   IF (X_NORM0.lt.0)
     &                                       PHI_X0=PHI_X0+4d0*atan(1d0)
                                        Y_NORM0=rcY0(J)/sqrt(tbetay(ie))
     &                                  /sqrt(myEMITY0)
                                        YP_NORM0=(rcY0(J)*talphay(IE) + 
     &                                  rcyp0(J)*tbetay(IE))/
     1                                  SQRT(tbetay(ie))/ sqrt(myEMITY0)
                                        PHI_Y0 = ATAN(YP_NORM0/Y_NORM0)
                                   IF (Y_NORM0.lt.0) 
     &                                       PHI_Y0=PHI_Y0+4d0*atan(1d0)
                                        !calculate norm coord after  the kick
                                        X_NORM =rcX(J)
     &                                    /SQRT(tbetax(ie)
     &                                    )/sqrt(myEMITX0)
                                        XP_NORM = (rcX0(J)*talphax(IE)+
     &                                    rcXP(J)*tbetax(IE))/SQRT(
     1                                    tbetax(ie))/ sqrt(myEMITX0)
                                        PHI_X = ATAN(XP_NORM/X_NORM)
                                   IF (X_NORM.lt.0)
     &                                       PHI_X=PHI_X+4d0*atan(1d0)
                                        Y_NORM =rcY0(J)/SQRT(tbetay(ie)
     &                                    )/sqrt(myEMITY0)
                                        YP_NORM = (rcY(J)*talphay(IE)+ 
     &                                   rcYP(J)*tbetay(IE))/SQRT(
     1                                   tbetay(ie))/ sqrt(myEMITY0)
                                        PHI_Y = ATAN(YP_NORM/Y_NORM)
                                   IF (Y_NORM.lt.0) 
     &                                       PHI_Y=PHI_Y+4d0*atan(1d0)
                                        WRITE(890) 
     i                                  icoll,        
     &                                  samplenumber*1000000+ipart(j),
     2                                  ITURN,
     3                                  X_NORM0,
     &                                  XP_NORM0,Y_NORM0,YP_NORM0,
c     3                                  X_NORM,
c     &                                  XP_NORM,Y_NORM,YP_NORM,
c     &                                  SQRT(X_NORM0**2+XP_NORM0**2),
c     &                                  SQRT(Y_NORM0**2+YP_NORM0**2),
c     &                                  PHI_X0,PHI_Y0,
c     &                                  SQRT(X_NORM**2+XP_NORM**2),
c     &                                  SQRT(Y_NORM**2+YP_NORM**2),
c     &                                  PHI_X , PHI_Y,
     &                                  SQRT(X_NORM**2+XP_NORM**2)-
     *                                     SQRT(X_NORM0**2+XP_NORM0**2),
     &                                  SQRT(Y_NORM**2+YP_NORM**2)-
     &                                     SQRT(Y_NORM0**2+YP_NORM0**2),
     &                                  SQRT(X_NORM**2+XP_NORM**2),
     &                                  SQRT(Y_NORM**2+YP_NORM**2)
c                                        
                                       write(889)
     i                                  icoll,        
     &                                  samplenumber*1000000+ipart(j),
     &                                  iturn,
     &                                  rcx0(J),rcxp0(J),      
     &                                  rcY0(J),rcyp0(J), 
c     &                                  rcX(J),rcXP(J),           
c     &                                  rcY(J),rcYP(J), 
     &                                  rcXP(J)-rcXP0(J), 
     &                                  rcYP(J)-rcYP0(J),
     &                                  sqrt((rcXP(J)-rcXP0(J))**2+
     &                                       (rcYP(J)-rcYP0(J))**2)
c     &                                  SQRT(X_NORM**2+XP_NORM**2),
c     &                                  SQRT(Y_NORM**2+YP_NORM**2)
                                endif

!++  Now copy data back to original verctor
 
                                xv(1,j) = rcx(j)*1d3  +torbx(ie)
                                yv(1,j) = rcxp(j)*1d3 +torbxp(ie)
                                xv(2,j) = rcy(j)*1d3  +torby(ie)
                                yv(2,j) = rcyp(j)*1d3 +torbyp(ie)
                                ejv(j) = rcp(j)*1d3
!
!
!++  Energy update, as recommended by Frank
!
                                ejfv(j)=sqrt(ejv(j)*ejv(j)-pma*pma)
                                rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
                                dpsv(j)=(ejfv(j)-e0f)/e0f
                                oidpsv(j)=one/(one+dpsv(j))
                                dpsv1(j)=dpsv(j)*c1e3*oidpsv(j)
                                yv(1,j)=ejf0v(j)/ejfv(j)*yv(1,j)
                                yv(2,j)=ejf0v(j)/ejfv(j)*yv(2,j)
!if the particle does not hit the collimator, take the original coordinates
!(before drifting back) 
                        else
                                xv(1,j) = rcx0(j)*1d3+torbx(ie)
                                yv(1,j) = rcxp0(j)*1d3+torbxp(ie)
                                xv(2,j) = rcy0(j)*1d3+torby(ie)
                                yv(2,j) = rcyp0(j)*1d3+torbyp(ie)
                                ejv(j) = rcp0(j)*1d3
                        endif
! 
!++  Write trajectory for any selected particle
!
                        if (firstrun) then
                        if (rselect.gt.0 .and. rselect.lt.65) then
                                xj     = (xv(1,j)-torbx(ie))/1d3
                                xpj    = (yv(1,j)-torbxp(ie))/1d3
                                yj     = (xv(2,j)-torby(ie))/1d3
                                ypj    = (yv(2,j)-torbyp(ie))/1d3
                                pj     = ejv(j)/1d3
                                        if (iturn.eq.1.and.j.eq.1) then
                                                sum_ax(ie)=0d0
                                                sum_ay(ie)=0d0
                                        endif
                                gammax=(1d0 + talphax(ie)**2)/tbetax(ie)
                                gammay=(1d0 + talphay(ie)**2)/tbetay(ie)
                                if (part_abs(j).eq.0) then
                                        xdebug(ie)=xj
                                        xpdebug(ie)=xpj
                                        ydebug(ie)=yj
                                        ypdebug(ie)=ypj
                                        xdebugN(ie)= xdebug(ie)
     &                                  /sqrt(myemitx0*tbetax(ie))
                                        xpdebugN(ie)=(xdebug(ie)
     &                              *talphax(ie)+xpdebug(ie)*tbetax(ie))
     &                                  /sqrt(myemitx0*tbetax(ie))
                                        ydebugN(ie)=ydebug(ie)/sqrt(
     &                                  myemity0*tbetay(ie))
                                        ypdebugN(ie)=(ydebug(ie)*
     &                                  talphay(ie)+ypdebug(ie)*
     &                                  tbetay(ie))
     &                                  /sqrt(myemity0*tbetay(ie))
          
                                        nspx    = sqrt(
     &                                  abs( gammax*(xj)**2 +  
     &                                  2d0*talphax(ie)*xj*xpj +
     &                                  tbetax(ie)*xpj**2 )/myemitx0)  
                                        nspy    = sqrt(
     &                                  abs( gammay*(yj)**2 +  
     &                                  2d0*talphay(ie)*yj*ypj +
     &                                  tbetay(ie)*ypj**2 )/myemity0) 
                                        sum_ax(ie)   = sum_ax(ie) + nspx
                                       sqsum_ax(ie)=sqsum_ax(ie)+nspx**2
                                       sum_ay(ie)   = sum_ay(ie) + nspy
                                       sqsum_ay(ie)=sqsum_ay(ie)+nspy**2
                                        nampl(ie)    = nampl(ie) + 1
                                else
                                        nspx = 0d0
                                        nspy = 0d0
                                endif
                                sampl(ie)    = totals
                                ename(ie)    = bez(myix)(1:16)
                        endif
                        endif
!++  First check for particle interaction at this collimator and this turn
                        if (part_hit(j).eq. (100000000*ie+iturn)) then
!++  Fill the change in particle angle into histogram
                                if(dowrite_impact) then
                                  write(46,'(i8,1x,i4,1x,f8.2)')ipart(j)
     &                            +100*samplenumber,iturn,sampl(ie)
                                endif
                                if(part_abs(j).ne.0) then
                                  if(dowrite_impact) 
     &                            write(47,'(i8,1x,i4,1x,f8.2)')        
     &                            ipart(j)+100*samplenumber,iturn,
     &                            sampl(ie)
                                  write(38,'(1x,i8,1x,i4,1x,f8.2,
     &                            5(1x,e11.3),1x,i4)')
     &                            ipart(j)+100*samplenumber,iturn,
     &                            sampl(ie)-0.5*c_length,           
     &                            (rcx0(j)*1d3+torbx(ie))-0.5*c_length*
     &                            (rcxp0(j)*1d3+torbxp(ie)),   
     &                            rcxp0(j)*1d3+torbxp(ie),
     &                            (rcy0(j)*1d3+torby(ie))-0.5*c_length*
     &                            (rcyp0(j)*1d3+torbyp(ie)),   
     &                            rcyp0(j)*1d3+torbyp(ie),
     &                            (ejv(j)-myenom)/myenom,secondary(j)+
     &                            tertiary(j)+other(j)
                                endif
                                if (part_abs(j).eq.0) then
                                  xkick = rcxp(j) - rcxp0(j)
                                  ykick = rcyp(j) - rcyp0(j)
!
                                  if (db_name1(icoll)(1:3).eq.'TCP'.or. 
     &                            db_name1(icoll)(1:4).eq.'COLM'.or.    
     &                            db_name1(icoll)(1:5).eq.'COLH0'.or.   
     &                            db_name1(icoll)(1:5).eq.'COLV0'.or.
     &                            db_name1(icoll)(1:3).eq.'CRY') then
                                        secondary(j) = 1
                                  elseif(db_name1(icoll)(1:3).eq.'TCS'
     &                           .or.db_name1(icoll)(1:4).eq.'COLH1'.or. 
     &                            db_name1(icoll)(1:4).eq.'COLV1'.or.   
     &                            db_name1(icoll)(1:4).eq.'COLH2') then
                                        tertiary(j)  = 2
                                  elseif((db_name1(icoll)(1:3).eq.'TCL')
     &                           .or.(db_name1(icoll)(1:3).eq.'TCT').or. 
     &                            (db_name1(icoll)(1:3).eq.'TCD').or.   
     &                            (db_name1(icoll)(1:3).eq.'TDI')) then
                                        other(j)     = 4
                                  endif
                                endif
!
!GRD THIS LOOP MUST NOT BE WRITTEN INTO THE "IF(FIRSTRUN)" LOOP !!!!!
                                if (dowritetracks) then
                                  if(part_abs(j).eq.0) then
                                        if ((secondary(j).eq.1.or.
     &                                  tertiary(j).eq.2.or.other(j)
     &                                  .eq.4) .and.
     &                                  (xv(1,j).lt.99d0 .and. xv(2,j)
     &                                  .lt.99d0)
     &                                  .and. 
!GRD HERE WE APPLY THE SAME KIND OF CUT THAN THE SIGSECUT PARAMETER
     &                                  (            
     &                                  ((               
     &                                  (xv(1,j)*1d-3)**2     
     &                                  /(tbetax(ie)*myemitx0)    
     &                                  ).ge.dble(sigsecut2)).or.   
     &                                  ((                            
     &                                  (xv(2,j)*1d-3)**2             
     &                                  /(tbetay(ie)*myemity0)          
     &                                  ).ge.dble(sigsecut2)).or.       
     &                                  (((xv(1,j)*1d-3)**2/(tbetax(ie)*
     &                                  myemitx0))+((xv(2,j)*1d-3)**2/
     &                                  (tbetay(ie)*myemity0))
     &                                  .ge.sigsecut3)  
     &                                  ) ) then
                                             xj=(xv(1,j)-torbx(ie))/1d3
                                             xpj=(yv(1,j)-torbxp(ie))
     &                                       /1d3
                                             yj=(xv(2,j)-torby(ie))/1d3
                                             ypj=(yv(2,j)-torbyp(ie))
     &                                       /1d3
                                             write(38,'(1x,i8,1x,i4,1x,
     &                                       f8.2,5(1x,e11.3),1x,i4)')
     &                                       ipart(j)+100*samplenumber,
     &                                       iturn,sampl(ie)-0.5*
     &                                       c_length,
     &                                       (rcx0(j)*1d3+torbx(ie))-0.5
     &                                       *c_length*(rcxp0(j)*1d3+
     &                                       torbxp(ie)),   
     &                                       rcxp0(j)*1d3+torbxp(ie),   
     &                                       (rcy0(j)*1d3+torby(ie))-0.5
     &                                       *c_length*(rcyp0(j)*1d3+
     &                                       torbyp(ie)),rcyp0(j)*1d3+
     &                                       torbyp(ie),                
     &                                       (ejv(j)-myenom)/myenom,
     &                                       secondary(j)+tertiary(j)+
     &                                       other(j)
                                             write(38,'(1x,i8,1x,i4,1x,
     &                                       f8.2,5(1x,e11.3),1x,i4)')  
     &                                       ipart(j)+100*samplenumber,
     &                                       iturn,sampl(ie)+0.5*
     &                                       c_length,xv(1,j)+0.5
     &                                       *c_length*yv(1,j),yv(1,j), 
     &                                       xv(2,j)+0.5*c_length*
     &                                       yv(2,j),yv(2,j),
     &                                       (ejv(j)-myenom)/myenom,
     &                                       secondary(j)+tertiary(j)
     &                                       +other(j)
                                       endif
                                  endif
                                endif
!++  Calculate impact observables, fill histograms, save collimator info, ...
! OCT2008 JCSMITH
! There's something wrong here I'll try to fix it...
                                if (abs(part_impact(j)) .lt. 0.9) then
                                  n_impact = n_impact + 1
                                  sum = sum + abs(part_impact(j))
                                  sqsum = sqsum + abs(part_impact(j))**2
                                  cn_impact(icoll) = cn_impact(icoll)+1
                                  csum(icoll) = csum(icoll) + 
     &                            abs(part_impact(j))
                                  csqsum(icoll) = csqsum(icoll) +
     &                            abs(part_impact(j))**2
                                endif
!++  If the interacting particle was lost, add-up counters for absorption
!++  Note: a particle with x/y >= 99. never hits anything any more in
!++        the logic of this program. Be careful to always fulfill this!
!
                                if (part_abs(j).ne.0) then
                                  n_absorbed = n_absorbed + 1
                                 cn_absorbed(icoll)=cn_absorbed(icoll)+1
                                  n_tot_absorbed = n_tot_absorbed + 1
                                  iturn_last_hit = part_hit_before(j)- 
     &                             int(part_hit_before(j)/100000000)*100000000
                                  iturn_absorbed = part_hit(j)- 
     &                             int(part_hit(j)/100000000)*100000000
                                  if (iturn_last_hit.eq.0) 
     &                            iturn_last_hit =iturn_absorbed
                                  iturn_survive  = iturn_absorbed - 
     &                             iturn_last_hit
                                endif
!++  End of check for hit this turn and element
                        endif
!++  Now copy the new particle momenta
                end do
!++  Calculate statistical observables and save into files...
                if (n_impact.gt.0) then
                        average = sum/n_impact
                        if (sqsum/n_impact.ge.average**2) then
                                sigma =sqrt(sqsum/n_impact - average**2)
                        else
                                sigma = 0d0
                        endif
                else
                        average = 0d0
                        sigma   = 0d0
                endif
                if (cn_impact(icoll).gt.0) then
                        caverage(icoll) = csum(icoll)/cn_impact(icoll)
                        if ((caverage(icoll)**2).gt.                    
     &                  (csqsum(icoll)/cn_impact(icoll))) then
                                csigma(icoll) = 0
                        else
                                csigma(icoll) = sqrt(csqsum(icoll)/     
     &                          cn_impact(icoll) - caverage(icoll)**2)
                        endif
                endif
!
!-----------------------------------------------------------------
!++  For a  S E L E C T E D  collimator only consider particles that
!++  were scattered on this selected collimator at the first turn. All
!++  other particles are discarded.
!++  - This is switched on with the DO_SELECT flag in the input file.
!++  - Note that the part_select(j) flag defaults to 1 for all particles.
!
! should name_sel(1:11) extended to allow longer names as done for 
! coll the coll_ellipse.dat file !!!!!!!!
                if (((db_name1(icoll)(1:10).eq.name_sel(1:10) )
     &          .or.(db_name2(icoll)(1:10).eq.name_sel(1:10) ) )       
     &          .and. iturn.eq.1  ) then
                        num_selhit = 0
                        num_surhit = 0
                        num_selabs = 0
                        do j = 1, napx
                                if(part_hit(j).eq.
     &                                  (100000000*ie+iturn))then
                                        num_selhit = num_selhit+1
                                        if (part_abs(j).eq.0) then
                                               num_surhit = num_surhit+1
                                        else
                                        num_selabs = num_selabs + 1
                                        endif
!++  If we want to select only partciles interacting at the specified
!++  collimator then remove all other particles and reset the number
!++  of the absorbed particles to the selected collimator.
                                endif
                        end do
!++  Calculate average impact parameter and save distribution into file
!++  only for selected collimator
                        n_impact = 0
                        sum      = 0d0
                        sqsum    = 0d0
                        do j = 1, napx
                                if(part_hit(j).eq.
     &                            (100000000*ie+iturn))then
                                  if (part_impact(j).lt.-0.5d0) then
                                        write(*,*) 
     &                                  'ERR> Invalid impact parameter!'
     &                                  , part_impact(j)
                                        write(outlun,*) 
     &                                  'ERR> Invalid impact parameter!'
     &                                  , part_impact(j)
                                        stop
                                  endif
                                  n_impact = n_impact + 1
                                  sum = sum + part_impact(j)
                                  sqsum = sqsum + part_impact(j)**2
                                  if (part_hit(j).gt.0 
     &                            .and. dowrite_impact) write(49,*)
     &                            part_impact(j), part_indiv(j)
                                endif
                        end do
                        if (n_impact.gt.0) then
                                average = sum/n_impact
                                if(sqsum/n_impact.ge.average**2) then
                                  sigma=sqrt(sqsum/n_impact- average**2)
                                else
                                  sigma = 0d0
                                endif
                        endif
!++  Some information
                        write(*,*) 
     &                  'INFO>  Selected collimator had N hits. N: ', 
     &                  num_selhit
                        write(*,*) 
     &                  'INFO>  Number of impacts                : ',
     &                  n_impact
                        write(*,*)
     &                  'INFO>  Number of escaped protons        : ',   
     &                  num_surhit
                        write(*,*)                                     
     &                  'INFO>  Average impact parameter [m]     : ',  
     &                  average
                        write(*,*)                                     
     &'                 INFO>  Sigma impact parameter [m]       : ',  
     &                  sigma
!
!++  End of    S E L E C T E D   collimator
                endif
!---------------------------------------------------------
!++  End of check for known collimator
        endif
!------------------------------------------------------------------
!++  Here leave the known collimator IF loop...
!_______________________________________________________________________
!++  If it is just a drift...
        else
!
                do 23 j=1,napx
                        xv(1,j)=xv(1,j)+stracki*yv(1,j)
                        xv(2,j)=xv(2,j)+stracki*yv(2,j)
                        sigmv(j)=sigmv(j)+stracki*(c1e3-rvv(j)*         &
     &                  (c1e3+(yv(1,j)*yv(1,j)+yv(2,j)*yv(2,j))*c5m4))
                        xj     = (xv(1,j)-torbx(ie))/1d3
                        xpj    = (yv(1,j)-torbxp(ie))/1d3
                        yj     = (xv(2,j)-torby(ie))/1d3
                        ypj    = (yv(2,j)-torbyp(ie))/1d3
                        pj     = ejv(j)/1.e3
                        if(firstrun) then
                                 if (iturn.eq.1.and.j.eq.1) then
                                        sum_ax(ie)=0d0
                                        sum_ay(ie)=0d0
                                 endif
                        endif
                        gammax = (1d0 + talphax(ie)**2)/tbetax(ie)
                        gammay = (1d0 + talphay(ie)**2)/tbetay(ie)
                        if (part_abs(j).eq.0) then
          
                                xdebug(ie)=xj
                                xpdebug(ie)=xpj
                                ydebug(ie)=yj
                                ypdebug(ie)=ypj
                                xdebugN(ie)= xdebug(ie)/sqrt(myemitx0*
     &                          tbetax(ie))
                                xpdebugN(ie)=(xdebug(ie)*talphax(ie)+
     &                          xpdebug(ie)*tbetax(ie))
     &                          /sqrt(myemitx0*tbetax(ie))
                                ydebugN(ie)=ydebug(ie)/sqrt(myemity0*
     &                          tbetay(ie))
                                ypdebugN(ie)=(ydebug(ie)*talphay(ie)+
     &                          ypdebug(ie)*tbetay(ie))
     &                          /sqrt(myemity0*tbetay(ie))
          
                                nspx    = sqrt(                         &
     &                          abs( gammax*(xj)**2 +                   &
     &                          2d0*talphax(ie)*xj*xpj +                &
     &                          tbetax(ie)*xpj**2 )/myemitx0            &
     &                          )
                                nspy    = sqrt(                         &
     &                          abs( gammay*(yj)**2 +                   &
     &                          2d0*talphay(ie)*yj*ypj +                &
     &                          tbetay(ie)*ypj**2 )/myemity0            
     &                          )
                                sum_ax(ie)   = sum_ax(ie) + nspx
                                sqsum_ax(ie) = sqsum_ax(ie) + nspx**2
                                sum_ay(ie)   = sum_ay(ie) + nspy
                                sqsum_ay(ie) = sqsum_ay(ie) + nspy**2
                                nampl(ie)    = nampl(ie) + 1
                        else
                                nspx = 0d0
                                nspy = 0d0
                        endif
                                sampl(ie)    = totals
                                ename(ie)    = bez(myix)(1:16)
 23             continue
          endif
          goto 650
!GRD END OF THE CHANGES FOR COLLIMATION STUDIES, BACK TO NORMAL SIXTRACK STUFF
   30     do 40 j=1,napx
            ejf0v(j)=ejfv(j)
            if(abs(dppoff).gt.pieni) sigmv(j)=sigmv(j)-sigmoff(i)
            if(kz(ix).eq.12) then
              ejv(j)=ejv(j)+ed(ix)*sin(hsyc(ix)*sigmv(j)+               &
     &phasc(ix))
            else
              ejv(j)=ejv(j)+hsy(1)*sin(hsy(3)*sigmv(j))
            endif
            ejfv(j)=sqrt(ejv(j)*ejv(j)-pma*pma)
            rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
            dpsv(j)=(ejfv(j)-e0f)/e0f
            oidpsv(j)=one/(one+dpsv(j))
            dpsv1(j)=dpsv(j)*c1e3*oidpsv(j)
            yv(1,j)=ejf0v(j)/ejfv(j)*yv(1,j)
            yv(2,j)=ejf0v(j)/ejfv(j)*yv(2,j)
 40       continue
          if(n.eq.1) write(98,'(1p,6(2x,e25.18))')                      &
     &(xv(1,j),yv(1,j),xv(2,j),yv(2,j),sigmv(j),dpsv(j),j=1,napx)
          goto 640
!--HORIZONTAL DIPOLE
   50     do 60 j=1,napx
            yv(1,j)=yv(1,j)+strackc(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+stracks(i)*oidpsv(j)
   60     continue
          goto 640
!--NORMAL QUADRUPOLE
   70     do 80 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
   80     continue
          goto 640
!--NORMAL SEXTUPOLE
   90     do 100 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  100     continue
          goto 640
!--NORMAL OCTUPOLE
  110     do 120 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  120     continue
          goto 640
!--NORMAL DECAPOLE
  130     do 140 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  140     continue
          goto 640
!--NORMAL DODECAPOLE
  150     do 160 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  160     continue
          goto 640
!--NORMAL 14-POLE
  170     do 180 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  180     continue
          goto 640
!--NORMAL 16-POLE
  190     do 200 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  200     continue
          goto 640
!--NORMAL 18-POLE
  210     do 220 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  220     continue
          goto 640
!--NORMAL 20-POLE
  230     do 240 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  240     continue
          goto 640
  250     continue
          do 260 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  260     continue
          goto 640
  270     continue
          do 280 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  280     continue
          goto 410
  290     continue
          do 300 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  300     continue
          goto 640
  310     continue
          do 320 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  320     continue
          goto 410
  330     continue
          do 340 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  340     continue
          goto 640
  350     continue
          do 360 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  360     continue
          goto 410
  370     continue
          do 380 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  380     continue
          goto 640
  390     continue
          do 400 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  400     continue
  410     r0=ek(ix)
          nmz=nmu(ix)
          if(nmz.ge.2) then
            do 430 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
              yv1j=bbiv(1,1,i)+bbiv(2,1,i)*xlvj+aaiv(2,1,i)*zlvj
              yv2j=aaiv(1,1,i)-bbiv(2,1,i)*zlvj+aaiv(2,1,i)*xlvj
              crkve=xlvj
              cikve=zlvj
                do 420 k=3,nmz
                  crkveuk=crkve*xlvj-cikve*zlvj
                  cikve=crkve*zlvj+cikve*xlvj
                  crkve=crkveuk
                  yv1j=yv1j+bbiv(k,1,i)*crkve+aaiv(k,1,i)*cikve
                  yv2j=yv2j-bbiv(k,1,i)*cikve+aaiv(k,1,i)*crkve
  420           continue
              yv(1,j)=yv(1,j)+(tiltc(i)*yv1j-tilts(i)*yv2j)*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*yv2j+tilts(i)*yv1j)*oidpsv(j)
  430       continue
          else
            do 435 j=1,napx
              yv(1,j)=yv(1,j)+(tiltc(i)*bbiv(1,1,i)-                    &
     &tilts(i)*aaiv(1,1,i))*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*aaiv(1,1,i)+                    &
     &tilts(i)*bbiv(1,1,i))*oidpsv(j)
  435       continue
          endif
          goto 640
!--SKEW ELEMENTS
!--VERTICAL DIPOLE
  440     do 450 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+strackc(i)*oidpsv(j)
  450     continue
          goto 640
!--SKEW QUADRUPOLE
  460     do 470 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  470     continue
          goto 640
!--SKEW SEXTUPOLE
  480     do 490 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  490     continue
          goto 640
!--SKEW OCTUPOLE
  500     do 510 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  510     continue
          goto 640
!--SKEW DECAPOLE
  520     do 530 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  530     continue
          goto 640
!--SKEW DODECAPOLE
  540     do 550 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  550     continue
          goto 640
!--SKEW 14-POLE
  560     do 570 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  570     continue
          goto 640
!--SKEW 16-POLE
  580     do 590 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  590     continue
          goto 640
!--SKEW 18-POLE
  600     do 610 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  610     continue
          goto 640
!--SKEW 20-POLE
  620     do 630 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  630     continue
          goto 640
  680     continue
          do 690 j=1,napx
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)                                       &
     &goto 690
            tkb(j)=rho2b(j)/(two*sigman2(1,imbb(i)))
            if(ibbc.eq.0) then
              yv(1,j)=yv(1,j)+oidpsv(j)*(strack(i)*crkveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))
              yv(2,j)=yv(2,j)+oidpsv(j)*(strack(i)*cikveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))
            else
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),11)-          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
              yv(1,j)=yv(1,j)+oidpsv(j)*cccc
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),12)+          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
              yv(2,j)=yv(2,j)+oidpsv(j)*cccc
            endif
  690     continue
          goto 640
  700     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 640
  720     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 640
  730     continue
!--Hirata's 6D beam-beam kick
            do j=1,napx
              track6d(1,j)=(xv(1,j)+ed(ix)-clobeam(1,imbb(i)))*c1m3
              track6d(2,j)=(yv(1,j)/oidpsv(j)-clobeam(4,imbb(i)))*c1m3
              track6d(3,j)=(xv(2,j)+ek(ix)-clobeam(2,imbb(i)))*c1m3
              track6d(4,j)=(yv(2,j)/oidpsv(j)-clobeam(5,imbb(i)))*c1m3
              track6d(5,j)=(sigmv(j)-clobeam(3,imbb(i)))*c1m3
              track6d(6,j)=dpsv(j)-clobeam(6,imbb(i))
            enddo
            call beamint(napx,track6d,parbe,sigz,bbcu,imbb(i),ix,ibtyp, &
     &ibbc)
            do j=1,napx
              xv(1,j)=track6d(1,j)*c1e3+clobeam(1,imbb(i))-             &
     &beamoff(1,imbb(i))
              xv(2,j)=track6d(3,j)*c1e3+clobeam(2,imbb(i))-             &
     &beamoff(2,imbb(i))
              dpsv(j)=track6d(6,j)+clobeam(6,imbb(i))-beamoff(6,imbb(i))
              oidpsv(j)=one/(one+dpsv(j))
              yv(1,j)=(track6d(2,j)*c1e3+clobeam(4,imbb(i))-            &
     &beamoff(4,imbb(i)))*oidpsv(j)
              yv(2,j)=(track6d(4,j)*c1e3+clobeam(5,imbb(i))-            &
     &beamoff(5,imbb(i)))*oidpsv(j)
              ejfv(j)=dpsv(j)*e0f+e0f
              ejv(j)=sqrt(ejfv(j)*ejfv(j)+pma*pma)
              rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
              if(ithick.eq.1) call envarsv(dpsv,oidpsv,rvv,ekv)
            enddo
          goto 640
  740     continue
          irrtr=imtr(ix)
          do j=1,napx
            sigmv(j)=sigmv(j)+cotr(irrtr,5)+rrtr(irrtr,5,1)*xv(1,j)+    &
     &rrtr(irrtr,5,2)*yv(1,j)+rrtr(irrtr,5,3)*xv(2,j)+                  &
     &rrtr(irrtr,5,4)*yv(2,j)
            pux=xv(1,j)
            dpsv3(j)=dpsv(j)*c1e3
            xv(1,j)=cotr(irrtr,1)+rrtr(irrtr,1,1)*pux+                  &
     &rrtr(irrtr,1,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,1,6)
            yv(1,j)=cotr(irrtr,2)+rrtr(irrtr,2,1)*pux+                  &
     &rrtr(irrtr,2,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,2,6)
            pux=xv(2,j)
            xv(2,j)=cotr(irrtr,3)+rrtr(irrtr,3,3)*pux+                  &
     &rrtr(irrtr,3,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,3,6)
            yv(2,j)=cotr(irrtr,4)+rrtr(irrtr,4,3)*pux+                  &
     &rrtr(irrtr,4,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,4,6)
          enddo
 
!----------------------------------------------------------------------
 
! Wire.
 
          goto 640
  745     continue
          xory=1
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 640
  746     continue
          xory=2
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 640
 
!----------------------------
 
! Wire.
 
  748     continue
!     magnetic rigidity
      chi = sqrt(e0*e0-pmap*pmap)*c1e6/clight
 
      ix = ixcav
      tx = xrms(ix)
      ty = zrms(ix)
      dx = xpl(ix)
      dy = zpl(ix)
      embl = ek(ix)
      l = wirel(ix)
      cur = ed(ix)
 
      leff = embl/cos(tx)/cos(ty)
      rx = dx *cos(tx)-embl*sin(tx)/2
      lin= dx *sin(tx)+embl*cos(tx)/2
      ry = dy *cos(ty)-lin *sin(ty)
      lin= lin*cos(ty)+dy  *sin(ty)
 
      do 750 j=1, napx
 
      xv(1,j) = xv(1,j) * c1m3
      xv(2,j) = xv(2,j) * c1m3
      yv(1,j) = yv(1,j) * c1m3
      yv(2,j) = yv(2,j) * c1m3
 
 
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(tx)*yv(2,j)/sqrt((1+dpsv(j))**2-    &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-tx)
      xv(1,j) = xv(1,j)*(cos(tx)-sin(tx)*tan(atan(yv(1,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(ty)*yv(1,j)/sqrt((1+dpsv(j))**2-    &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-ty)
      xv(2,j) = xv(2,j)*(cos(ty)-sin(ty)*tan(atan(yv(2,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty)
 
      xv(1,j) = xv(1,j) + lin*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) + lin*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
 
      xi = xv(1,j)-rx
      yi = xv(2,j)-ry
      yv(1,j) = yv(1,j)-1.0d-7*cur/chi*xi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
!GRD FOR CONSISTENSY
      yv(2,j) = yv(2,j)-1.0d-7*cur/chi*yi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
 
      xv(1,j) = xv(1,j) + (leff-lin)*yv(1,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
      xv(2,j) = xv(2,j) + (leff-lin)*yv(2,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(-ty)*yv(1,j)/sqrt((1+dpsv(j))**2-   &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+ty)
      xv(2,j) = xv(2,j)*(cos(-ty)-sin(-ty)*tan(atan(yv(2,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(-tx)*yv(2,j)/sqrt((1+dpsv(j))**2-   &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+tx)
      xv(1,j) = xv(1,j)*(cos(-tx)-sin(-tx)*tan(atan(yv(1,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx)
 
 
      xv(1,j) = xv(1,j) + embl*tan(tx)
      xv(2,j) = xv(2,j) + embl*tan(ty)/cos(tx)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
      xv(1,j) = xv(1,j) * c1e3
      xv(2,j) = xv(2,j) * c1e3
      yv(1,j) = yv(1,j) * c1e3
      yv(2,j) = yv(2,j) * c1e3
 
!-----------------------------------------------------------------------
 
  750     continue
          goto 640
 
!----------------------------
 
  640     continue
!GRD
!GRD UPGRADE JANUARY 2005
!GRD
      if (firstrun) then
        if (rselect.gt.0 .and. rselect.lt.65) then
          do j = 1, napx
!
            xj     = (xv(1,j)-torbx(ie))/1d3
            xpj    = (yv(1,j)-torbxp(ie))/1d3
            yj     = (xv(2,j)-torby(ie))/1d3
            ypj    = (yv(2,j)-torbyp(ie))/1d3
            pj     = ejv(j)/1d3
!GRD
            if (iturn.eq.1.and.j.eq.1) then
              sum_ax(ie)=0d0
              sum_ay(ie)=0d0
            endif
!GRD
!
            if (tbetax(ie).gt.0.) then
              gammax = (1d0 + talphax(ie)**2)/tbetax(ie)
              gammay = (1d0 + talphay(ie)**2)/tbetay(ie)
            else
              gammax = (1d0 + talphax(ie-1)**2)/tbetax(ie-1)
              gammay = (1d0 + talphay(ie-1)**2)/tbetay(ie-1)
            endif
!
            if (part_abs(j).eq.0) then
              if(tbetax(ie).gt.0.) then
                nspx    = sqrt(                                         &
     &               abs( gammax*(xj)**2 +                              &
     &               2d0*talphax(ie)*xj*xpj +                           &
     &               tbetax(ie)*xpj**2 )/myemitx0                       &
     &               )
                nspy    = sqrt(                                         &
     &               abs( gammay*(yj)**2 +                              &
     &               2d0*talphay(ie)*yj*ypj +                           &
     &               tbetay(ie)*ypj**2 )/myemity0                       &
     &               )
              else
                nspx    = sqrt(                                         &
     &               abs( gammax*(xj)**2 +                              &
     &               2d0*talphax(ie-1)*xj*xpj +                         &
     &               tbetax(ie-1)*xpj**2 )/myemitx0                     &
     &               )
                nspy    = sqrt(                                         &
     &               abs( gammay*(yj)**2 +                              &
     &               2d0*talphay(ie-1)*yj*ypj +                         &
     &               tbetay(ie-1)*ypj**2 )/myemity0                     &
     &               )
              endif
          
          xdebug(ie)=xj
          xpdebug(ie)=xpj
          ydebug(ie)=yj
          ypdebug(ie)=ypj
          xdebugN(ie)= xdebug(ie)/sqrt(myemitx0*tbetax(ie))
          xpdebugN(ie)=(xdebug(ie)*talphax(ie)+xpdebug(ie)*tbetax(ie))
     &    /sqrt(myemitx0*tbetax(ie))
          ydebugN(ie)=ydebug(ie)/sqrt(myemity0*tbetay(ie))
          ypdebugN(ie)=(ydebug(ie)*talphay(ie)+ypdebug(ie)*tbetay(ie))
     &    /sqrt(myemity0*tbetay(ie))
          
!
              sum_ax(ie)   = sum_ax(ie) + nspx
              sqsum_ax(ie) = sqsum_ax(ie) + nspx**2
              sum_ay(ie)   = sum_ay(ie) + nspy
              sqsum_ay(ie) = sqsum_ay(ie) + nspy**2
              nampl(ie)    = nampl(ie) + 1
            else
              nspx = 0d0
              nspy = 0d0
            endif
              sampl(ie)    = totals
              ename(ie)    = bez(myix)(1:16)
          end do
        endif
      endif
!GRD
!GRD THIS LOOP MUST NOT BE WRITTEN INTO THE "IF(FIRSTRUN)" LOOP !!!!
!GRD
          if (dowritetracks) then
            do j = 1, napx
              xj     = (xv(1,j)-torbx(ie))/1d3
              xpj    = (yv(1,j)-torbxp(ie))/1d3
              yj     = (xv(2,j)-torby(ie))/1d3
              ypj    = (yv(2,j)-torbyp(ie))/1d3
!
              arcdx = 2.5d0
              arcbetax = 180d0
!
                if (xj.le.0.) then
                  xdisp = xj + (pj-myenom)/myenom * arcdx               &
     &* sqrt(tbetax(ie)/arcbetax)
                else
                  xdisp = xj - (pj-myenom)/myenom * arcdx               &
     &* sqrt(tbetax(ie)/arcbetax)
                endif
                xndisp = xj
                nspxd   = sqrt(                                         &
     &abs(gammax*xdisp**2 + 2d0*talphax(ie)*xdisp*xpj                   &
     &+ tbetax(ie)*xpj**2)/myemitx0                                     &
     &)
                nspx    = sqrt(                                         &
     &abs( gammax*xndisp**2 + 2d0*talphax(ie)*xndisp*                   &
     &xpj + tbetax(ie)*xpj**2 )/myemitx0                                &
     &)
                nspy    = sqrt(                                         &
     &abs( gammay*yj**2 + 2d0*talphay(ie)*yj                            &
     &*ypj + tbetay(ie)*ypj**2 )/myemity0                               &
     &)
!
!
!
         if(part_abs(j).eq.0) then
         if ((secondary(j).eq.1.or.tertiary(j).eq.2.or.other(j).eq.4)   &
     & .and.(xv(1,j).lt.99d0 .and. xv(2,j).lt.99d0) .and.               &
!GRD
!GRD HERE WE APPLY THE SAME KIND OF CUT THAN THE SIGSECUT PARAMETER
!GRD                                                                    &
     &(                                                                 &
     &((                                                                &
     &(xv(1,j)*1d-3)**2                                                 &
     &/                                                                 &
     &(tbetax(ie)*myemitx0)                                             &
!     &).ge.sigsecut2).and.                                              &
     &).ge.dble(sigsecut2)).or.                                         &
     &((                                                                &
     &(xv(2,j)*1d-3)**2                                                 &
     &/                                                                 &
     &(tbetay(ie)*myemity0)                                             &
     &).ge.dble(sigsecut2)).or.                                         &
     &(((xv(1,j)*1d-3)**2/(tbetax(ie)*myemitx0))+                       &
     &((xv(2,j)*1d-3)**2/(tbetay(ie)*myemity0))                         &
     &.ge.sigsecut3)                                                    &
     &) ) then
                xj     = (xv(1,j)-torbx(ie))/1d3
                xpj    = (yv(1,j)-torbxp(ie))/1d3
                yj     = (xv(2,j)-torby(ie))/1d3
                ypj    = (yv(2,j)-torbyp(ie))/1d3
          write(38,'(1x,i8,1x,i4,1x,f8.2,5(1x,e11.3),1x,i4)')           &
     &ipart(j)+100*samplenumber,iturn,sampl(ie),                        &
     &xv(1,j),yv(1,j),                                                  &
     &xv(2,j),yv(2,j),(ejv(j)-myenom)/myenom,                           &
     &secondary(j)+tertiary(j)+other(j)
              endif
         endif
            end do
!!GRD+KAD here we dump the location within RHIC where any one transvere
!!GRD+KAD dimension of the beam gets bigger than 4 cm => kind of like a
!!GRD+KAD raw aperture check to obtain loss maps...
!!GRD+KAD then we just delete the particle from the tracking, so as not to have
!!GRD+KAD strange values for the impact parameter and have losses at other crazy
!!GRD+KAD locations
!!AUGUST2005 comment that out for LHC studies
!!JUNE2005 here I close the "if(dowritetracks)" outside of the firstrun flag
      endif
 
!GRD END OF UPGRADE
          kpz=abs(kp(ix))
          if(kpz.eq.0) goto 650
          if(kpz.eq.1) goto 650
  650   continue
!GRD
!UPGRADE JANUARY 2005
!__________________________________________________________________
!++  Now do analysis at selected elements...
!
!++  Save twiss functions of present element
!
        
c        write(*,*) "element ",ie
        ax0  = talphax(ie)
        bx0  = tbetax(ie)
        mux0 = mux(ie)
        ay0  = talphay(ie)
        by0  = tbetay(ie)
        muy0 = muy(ie)
!GRD GET THE COORDINATES OF THE PARTICLES AT THE IEth ELEMENT:
        do j = 1,napx
              xgrd(j)  = xv(1,j)
              xpgrd(j) = yv(1,j)
              ygrd(j)  = xv(2,j)
              ypgrd(j) = yv(2,j)
!
              xineff(j)  = xv(1,j)                                      &
     &        - torbx(ie)
              xpineff(j) = yv(1,j)                                      &
     &        - torbxp(ie)
              yineff(j)  = xv(2,j)                                      &
     &        - torby(ie)
              ypineff(j) = yv(2,j)                                      &
     &        - torbyp(ie)
!
              pgrd(j)  = ejv(j)
              ejfvgrd(j) = ejfv(j)
              sigmvgrd(j) = sigmv(j)
              rvvgrd(j) = rvv(j)
              dpsvgrd(j) = dpsv(j)
              oidpsvgrd(j) = oidpsv(j)
              dpsv1grd(j) = dpsv1(j)
!GRD IMPORTANT: ALL PARTICLES ABSORBED ARE CONSIDERED TO BE LOST,
!GRD SO WE GIVE THEM A LARGE OFFSET
             if (part_abs(j).ne.0) then
                xgrd(j)  = 99.5d0
                ygrd(j)  = 99.5d0
             endif
        end do
!
!++  For LAST ELEMENT in the ring calculate the number of surviving
!++  particles and save into file versus turn number
!
        if (ie.eq.iu) then
             do j = 1, napx
c                if (xgrd(j).lt.99d0 .and. ygrd(j).lt.99d0) then
             !valentina!!!
                if (part_abs(j).eq.0) then
                        nsurvive(n) = nsurvive(n) + 1
                endif
             end do
             if (iturn.eq.numl) then
                nsurvive_end = nsurvive_end + nsurvive(n)
             endif
        endif
!
!=======================================================================
!++  Do collimation analysis at element 20 ("zero" turn) or LAST
!++  ring element.
!
!++  If selecting, look at number of scattered particles at selected
!++  collimator. For the "zero" turn consider the information at element
!++  20 (before collimation), otherwise take information at last ring
!++  element.
!
        if (do_coll .and.                                               &
     &  (  (iturn.eq.1 .and. ie.eq.20) .or.                             &
     &  (ie.eq.iu)  )    ) then
!
!++  Calculate gammas
!------------------------------------------------------------------------
!
          gammax = (1 + talphax(ie)**2)/tbetax(ie)
          gammay = (1 + talphay(ie)**2)/tbetay(ie)
!
!________________________________________________________________________
!++  Loop over all particles.
!
          do j = 1, napx
!
!------------------------------------------------------------------------
!++  Save initial distribution of particles that were scattered on
!++  the first turn at the selected primary collimator
!
!------------------------------------------------------------------------
!++  Do the binning in amplitude, only considering particles that were
!++  not absorbed before.
!
            if (xgrd(j).lt.99d0 .and. ygrd(j) .lt.99d0 .and.            &
     &      (part_select(j).eq.1 .or. ie.eq.20)) then
!
!++  Normalized amplitudes are calculated
!
!++  Allow to apply some dispersive offset. Take arc dispersion (2m) and
!++  normalize with arc beta_x function (180m).
!
              arcdx    = 2.5d0
              arcbetax = 180d0
              xdisp = abs(xgrd(j)*1d-3) +                               &
     &        abs((pgrd(j)-myenom)/myenom)*arcdx                        &
     &        * sqrt(tbetax(ie)/arcbetax)
              nspx    = sqrt(                                           &
     &        abs(gammax*xdisp**2 +                                     &
     &        2d0*talphax(ie)*xdisp*(xpgrd(j)*1d-3)+                    &
     &        tbetax(ie)*(xpgrd(j)*1d-3)**2 )/myemitx0                  &
     &        )
              nspy    = sqrt(                                           &
     &        abs( gammay*(ygrd(j)*1d-3)**2 +                           &
     &        2d0*talphay(ie)*(ygrd(j)*1d-3*ypgrd(j)*1d-3)              &
     &        + tbetay(ie)*(ypgrd(j)*1d-3)**2 )/myemity0                &
     &        )
!
!++  Populate the efficiency arrays at the end of each turn...
!
              if (ie.eq.iu) then
                do ieff = 1, numeff
                  if (counted_r(j,ieff).eq.0 .and.                      &
     &sqrt(                                                             &
     &((xineff(j)*1d-3)**2                                              &
     &/                                                                 &
     &(tbetax(ie)*myemitx0))                                            &
     &+                                                                 &
     &((yineff(j)*1d-3)**2                                              &
     &/                                                                 &
     &(tbetay(ie)*myemity0))                                            &
     &).ge.rsig(ieff)) then
                    neff(ieff) = neff(ieff)+1d0
                    counted_r(j,ieff)=1
                  endif
                  if (counted_x(j,ieff).eq.0 .and.                      &
     &sqrt(                                                             &
     &((xineff(j)*1d-3)**2                                              &
     &/                                                                 &
     &(tbetax(ie)*myemitx0))                                            &
     &).ge.rsig(ieff)) then
                    neffx(ieff) = neffx(ieff) + 1d0
                    counted_x(j,ieff)=1
                  endif
                  if (counted_y(j,ieff).eq.0 .and.
     &sqrt(                                                             &
     &((yineff(j)*1d-3)**2                                              &
     &/                                                                 &
     &(tbetay(ie)*myemity0))                                            &
     &).ge.rsig(ieff)) then
                    neffy(ieff) = neffy(ieff) + 1d0
                    counted_y(j,ieff)=1
                  endif
!
                end do
              endif
!
!++  Do an emittance drift
!
            if (ie.eq.iu) then
              xnorm  = ((xgrd(j)-torbx(ie))*1d-3) 
     &          / sqrt(tbetax(ie)*myemitx0)
              xpnorm = (talphax(ie)*
     &          ( xgrd(j)-torbx(ie))*1d-3+                     
     &          tbetax(ie)* (xpgrd(j)-torbxp(ie))*1d-3) /      
     &          sqrt(tbetax(ie)*myemitx0)


              dnormx = driftsx
              if (relative) dnormx=dnormx/myemitx0
              if (diffusive) then
                      dnormx=dnormx*ran_gauss(1d0)
C     &                  *sqrt(xnorm**2+xpnorm**2)
              endif

              if((xnorm.ne.0d0).and.(xpnorm.ne.0d0)) then
                    xangle = atan2(xnorm,xpnorm)
              else
                    xangle=0
              endif     
                xnorm  = xnorm  + dnormx*sin(xangle)
                xpnorm = xpnorm + dnormx*cos(xangle)
                xgrd(j)   = 1000d0*(xnorm * sqrt(tbetax(ie)*myemitx0))
     &            +torbx(ie)           
                xpgrd(j)  = 1000d0*((xpnorm*sqrt(tbetax(ie)*myemitx0)
     &            -talphax(ie)*xgrd(j)*1d-3)/tbetax(ie))
     &            +torbxp(ie)           
!
 
                ynorm  = ((ygrd(j)-torby(ie))*1d-3)  
     &            / sqrt(tbetay(ie)*myemity0)
                ypnorm = (talphay(ie)*(
     &            (ygrd(j)-torby(ie))*1d-3)+                   
     &            tbetay(ie)*((ypgrd(j)-torbyp(ie))*1d-3)) / 
     &            sqrt(tbetay(ie)*myemity0)
              dnormy = driftsy
              if (relative) dnormy=dnormy/myemity0
              if (diffusive) then
                      dnormy=dnormy*ran_gauss(1d0)
c     &                  *sqrt(ynorm**2+ypnorm**2)
              endif

              if((ynorm.ne.0d0).and.(ypnorm.ne.0d0)) then
                    yangle = atan2(ynorm,ypnorm)
              else
                    yangle=0
              endif     
              ynorm  = ynorm  + dnormy*sin(yangle)
              ypnorm = ypnorm + dnormy*cos(yangle)
              ygrd(j)   = 1000d0*(ynorm * sqrt(tbetay(ie)*myemity0))
     &          + torby(ie)
              ypgrd(j)  = 1000d0*((ypnorm*sqrt(tbetay(ie)*myemity0)   
     &          -talphay(ie)*ygrd(j)*1d-3)/tbetay(ie))
     &          + torbyp(ie)
            endif
!
!------------------------------------------------------------------------
!++  End of check for selection flag and absorption
!
            endif
!
!++  End of do loop over particles
!
          end do
!
!_________________________________________________________________
!
!++  End of collimation efficiency analysis for selected particles
!
        end if
!------------------------------------------------------------------
!++  For LAST ELEMENT in the ring compact the arrays by moving all
!++  lost particles to the end of the array.
!
        if (ie.eq.iu) then
          imov = 0
          do j = 1, napx
            if (xgrd(j).lt.99d0 .and. ygrd(j).lt.99d0) then
              imov = imov + 1
              xgrd(imov)           = xgrd(j)
              ygrd(imov)           = ygrd(j)
              xpgrd(imov)          = xpgrd(j)
              ypgrd(imov)          = ypgrd(j)
              pgrd(imov)           = pgrd(j)
              ejfvgrd(imov)        = ejfvgrd(j)
              sigmvgrd(imov)       = sigmvgrd(j)
              rvvgrd(imov)         = rvvgrd(j)
              dpsvgrd(imov)        = dpsvgrd(j)
              oidpsvgrd(imov)      = oidpsvgrd(j)
              dpsv1grd(imov)       = dpsv1grd(j)
              part_hit(imov)    = part_hit(j)
              part_abs(imov)    = part_abs(j)
              part_select(imov) = part_select(j)
              part_impact(imov) = part_impact(j)
              part_indiv(imov)  = part_indiv(j)
              part_linteract(imov)  = part_linteract(j)
              part_hit_before(imov) = part_hit_before(j)
              secondary(imov) = secondary(j)
              tertiary(imov) = tertiary(j)
!GRD HERE WE ADD A MARKER FOR THE PARTICLE FORMER NAME
              ipart(imov) = ipart(j)
              flukaname(imov) = flukaname(j)
              do ieff = 1, numeff
                counted_r(imov,ieff) = counted_r(j,ieff)
                counted_x(imov,ieff) = counted_x(j,ieff)
                counted_y(imov,ieff) = counted_y(j,ieff)
              end do
            endif
          end do
          if ((mod(iturn,100).eq.0).or.(iturn.eq.1))
     &    write(*,*)'INFO>  Compacted the particle distributions, turn '&
     &,iturn, ":", napx, ' -->  ', imov
          napx = imov
        endif
!GRD
!
!------------------------------------------------------------------------
!
!++  Write final distribution
!
      if (dowrite_dist.and.(ie.eq.iu).and.(n.eq.numl)) then
        do j = 1, napx
              xnorm  = ((xgrd(j)-torbx(ie))*1d-3) 
     &          / sqrt(tbetax(ie)*myemitx0)
              xpnorm = (talphax(ie)*
     &          ( xgrd(j)-torbx(ie))*1d-3+                     
     &          tbetax(ie)* (xpgrd(j)-torbxp(ie))*1d-3) /      
     &          sqrt(tbetax(ie)*myemitx0)
              ynorm  = ((ygrd(j)-torby(ie))*1d-3)  
     &            / sqrt(tbetay(ie)*myemity0)
              ypnorm = (talphay(ie)*(
     &            (ygrd(j)-torby(ie))*1d-3)+                   
     &            tbetay(ie)*((ypgrd(j)-torbyp(ie))*1d-3)) / 
     &            sqrt(tbetay(ie)*myemity0)
c              write(*,*)"betavale", tbetax(ie),tbetay(ie)
c              write(*,*)"alphavale",talphax(ie),talphay(ie)
c              write(*,*)"orbitvale",torbx(ie),torbxp(ie),
c     &                  torby(ie), torbyp(ie)
              write(990,*) xgrd(j), xpgrd(j),              
     & ygrd(j), ypgrd(j), xnorm, xpnorm, ynorm,ypnorm, 
     & (xnorm**2+xpnorm**2)**.5,(ynorm**2+ypnorm**2)**.5,
     & flukaname(j)
!     2             , S(J)
        end do
      endif
!
!GRD
!GRD NOW ONE HAS TO COPY BACK THE NEW DISTRIBUTION TO ITS "ORIGINAL NAME"
!GRD AT THE END OF EACH TURN
!GRD
      if (ie.eq.iu) then
         do j = 1,napx
            xv(1,j) = xgrd(j)
            yv(1,j) = xpgrd(j)
            xv(2,j) = ygrd(j)
            yv(2,j) = ypgrd(j)
            ejv(j)  = pgrd(j)
            ejfv(j)   = ejfvgrd(j)
            sigmv(j)  = sigmvgrd(j)
            rvv(j)    = rvvgrd(j)
            dpsv(j)   = dpsvgrd(j)
            oidpsv(j) = oidpsvgrd(j)
            dpsv1(j)  = dpsv1grd(j)
         end do
      endif
         if (firstrun) then
       if (rselect.gt.0 .and. rselect.lt.65) then
            do j = 1, napx
!
              xj     = (xv(1,j)-torbx(ie))/1d3
              xpj    = (yv(1,j)-torbxp(ie))/1d3
              yj     = (xv(2,j)-torby(ie))/1d3
              ypj    = (yv(2,j)-torbyp(ie))/1d3
              pj     = ejv(j)/1d3
              if (iturn.eq.1.and.j.eq.1) then
              sum_ax(ie)=0d0
              sum_ay(ie)=0d0
              endif
              if (tbetax(ie).gt.0.) then
          gammax = (1d0 + talphax(ie)**2)/tbetax(ie)
                gammay = (1d0 + talphay(ie)**2)/tbetay(ie)
              else
          gammax = (1d0 + talphax(ie-1)**2)/tbetax(ie-1)
          gammay = (1d0 + talphay(ie-1)**2)/tbetay(ie-1)
              endif
!
              if (part_abs(j).eq.0) then
                if(tbetax(ie).gt.0.) then
          nspx    = sqrt(                                               &
     &abs( gammax*(xj)**2 +                                             &
     &2d0*talphax(ie)*xj*xpj +                                          &
     &tbetax(ie)*xpj**2 )/myemitx0                                      &
     &)
                nspy    = sqrt(                                         &
     &abs( gammay*(yj)**2 +                                             &
     &2d0*talphay(ie)*yj*ypj +                                          &
     &tbetay(ie)*ypj**2 )/myemity0                                      &
     &)
                else
          nspx    = sqrt(                                               &
     &abs( gammax*(xj)**2 +                                             &
     &2d0*talphax(ie-1)*xj*xpj +                                        &
     &tbetax(ie-1)*xpj**2 )/myemitx0                                    &
     &)
                nspy    = sqrt(                                         &
     &abs( gammay*(yj)**2 +                                             &
     &2d0*talphay(ie-1)*yj*ypj +                                        &
     &tbetay(ie-1)*ypj**2 )/myemity0                                    &
     &)
                endif
!
          
          xdebug(ie)=xj
          xpdebug(ie)=xpj
          ydebug(ie)=yj
          ypdebug(ie)=ypj
          xdebugN(ie)= xdebug(ie)/sqrt(myemitx0*tbetax(ie))
          xpdebugN(ie)=(xdebug(ie)*talphax(ie)+xpdebug(ie)*tbetax(ie))
     &    /sqrt(myemitx0*tbetax(ie))
          ydebugN(ie)=ydebug(ie)/sqrt(myemity0*tbetay(ie))
          ypdebugN(ie)=(ydebug(ie)*talphay(ie)+ypdebug(ie)*tbetay(ie))
     &    /sqrt(myemity0*tbetay(ie))
          
                sum_ax(ie)   = sum_ax(ie) + nspx
                sqsum_ax(ie) = sqsum_ax(ie) + nspx**2
                sum_ay(ie)   = sum_ay(ie) + nspy
                sqsum_ay(ie) = sqsum_ay(ie) + nspy**2
                nampl(ie)    = nampl(ie) + 1
                sampl(ie)    = totals
                ename(ie)    = bez(myix)(1:16)
              else
                nspx = 0d0
                nspy = 0d0
              endif
            end do
          endif
         endif
!GRD
!GRD THIS LOOP MUST NOT BE WRITTEN INTO THE "IF(FIRSTRUN)" LOOP !!!!
!GRD
          if (dowritetracks) then
            do j = 1, napx
              xj     = (xv(1,j)-torbx(ie))/1d3
              xpj    = (yv(1,j)-torbxp(ie))/1d3
              yj     = (xv(2,j)-torby(ie))/1d3
              ypj    = (yv(2,j)-torbyp(ie))/1d3
!
              arcdx = 2.5d0
              arcbetax = 180d0
!
                if (xj.le.0.) then
                  xdisp = xj + (pj-myenom)/myenom * arcdx               &
     &* sqrt(tbetax(ie)/arcbetax)
                else
                  xdisp = xj - (pj-myenom)/myenom * arcdx               &
     &* sqrt(tbetax(ie)/arcbetax)
                endif
                xndisp = xj
                nspxd   = sqrt(                                         &
     &abs(gammax*xdisp**2 + 2d0*talphax(ie)*xdisp*xpj                   &
     &+ tbetax(ie)*xpj**2)/myemitx0                                     &
     &)
                nspx    = sqrt(                                         &
     &abs( gammax*xndisp**2 + 2d0*talphax(ie)*xndisp*                   &
     &xpj + tbetax(ie)*xpj**2 )/myemitx0                                &
     &)
                nspy    = sqrt(                                         &
     &abs( gammay*yj**2 + 2d0*talphay(ie)*yj                            &
     &*ypj + tbetay(ie)*ypj**2 )/myemity0                               &
     &)
!
!
!
         if(part_abs(j).eq.0) then
        if ((secondary(j).eq.1.or.tertiary(j).eq.2.or.other(j).eq.4)    &
     &.and.(xv(1,j).lt.99d0 .and. xv(2,j).lt.99d0) .and.                &
!GRD
!GRD HERE WE APPLY THE SAME KIND OF CUT THAN THE SIGSECUT PARAMETER
!GRD                                                                    &
     &(                                                                 &
     &((                                                                &
     &(xv(1,j)*1d-3)**2                                                 &
     &/                                                                 &
     &(tbetax(ie)*myemitx0)                                             &
     &).ge.dble(sigsecut2)).or.                                         &
     &((                                                                &
     &(xv(2,j)*1d-3)**2                                                 &
     &/                                                                 &
     &(tbetay(ie)*myemity0)                                             &
     &).ge.dble(sigsecut2)).or.                                         &
     &(((xv(1,j)*1d-3)**2/(tbetax(ie)*myemitx0))+                       &
     &((xv(2,j)*1d-3)**2/(tbetay(ie)*myemity0))                         &
     &.ge.sigsecut3)                                                    &
     &) ) then
                xj     = (xv(1,j)-torbx(ie))/1d3
                xpj    = (yv(1,j)-torbxp(ie))/1d3
                yj     = (xv(2,j)-torby(ie))/1d3
                ypj    = (yv(2,j)-torbyp(ie))/1d3
          write(38,'(1x,i8,1x,i4,1x,f8.2,5(1x,e11.3),1x,i4)')           &
     &ipart(j)+100*samplenumber,iturn,sampl(ie),                        &
     &xv(1,j),yv(1,j),                                                  &
     &xv(2,j),yv(2,j),(ejv(j)-myenom)/myenom,                           &
     &secondary(j)+tertiary(j)+other(j)
              endif
         endif
            end do
          endif
!=======================================================================
!GRD END OF UPGRADE
  660 continue
c      close(99)
      close(53)
!GRD HERE WE SET THE FLAG FOR INITIALIZATION TO FALSE AFTER TURN 1
      firstrun = .false.
      return
      end
!
!==============================================================================
!
      subroutine thin6dua(nthinerr)
!-----------------------------------------------------------------------
!
!  TRACK THIN LENS 6D WITH ACCELERATION
!
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
      implicit none
      integer i,irrtr,ix,j,k,kpz,n,nmz,nthinerr
      double precision c5m4,cbxb,cbzb,cccc,cikve,cikveb,crkve,crkveb,   &
     &crkveuk,crxb,crzb,dpsv3,pux,e0fo,e0o,r0,r2b,rb,rho2b,rkb,stracki, &
     &tkb,xbb,xlvj,xrb,yv1j,yv2j,zbb,zlvj,zrb
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer ireturn, xory, nac, nfree, nramp1,nplato, nramp2
      double precision xv1j,xv2j
      double precision acdipamp, qd, acphase,acdipamp2,                 &
     &acdipamp1
      double precision l,cur,dx,dy,tx,ty,embl,leff,rx,ry,lin,chi,xi,yi
      logical llost
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      double precision cc,xlim,ylim
      parameter(cc = 1.12837916709551d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),    &
     &r2b(npart),rb(npart),rkb(npart),                                  &
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),          &
     &crzb(npart),cbxb(npart),cbzb(npart)
      dimension dpsv3(npart)
      save
!-----------------------------------------------------------------------
      c5m4=5.0d-4
      nthinerr=0
      do 660 n=1,numl
        numx=n-1
        if(irip.eq.1) call ripple(n)
        if(n.le.nde(1)) nwri=nwr(1)
        if(n.gt.nde(1).and.n.le.nde(2)) nwri=nwr(2)
        if(n.gt.nde(2)) nwri=nwr(3)
        if(nwri.eq.0) nwri=numl+numlr+1
        if(mod(numx,nwri).eq.0) call writebin(nthinerr)
        if(nthinerr.ne.0) return
        do 650 i=1,iu
          ix=ic(i)-nblo
!--------count44
          goto(10,30,740,650,650,650,650,650,650,650,50,70,90,110,130,  &
     &150,170,190,210,230,440,460,480,500,520,540,560,580,600,620,      &
     &640,410,250,270,290,310,330,350,370,390,680,700,720,730,748,      &
     &650,650,650,650,650,745,746),ktrack(i)
          goto 650
   10     stracki=strack(i)
          do 20 j=1,napx
            xv(1,j)=xv(1,j)+stracki*yv(1,j)
            xv(2,j)=xv(2,j)+stracki*yv(2,j)
            sigmv(j)=sigmv(j)+stracki*(c1e3-rvv(j)*(c1e3+(yv(1,j)       &
     &*yv(1,j)+yv(2,j)*yv(2,j))*c5m4))
   20     continue
          goto 650
   30     e0o=e0
          e0fo=e0f
          call adia(n,e0f)
          do 40 j=1,napx
            ejf0v(j)=ejfv(j)
            if(abs(dppoff).gt.pieni) sigmv(j)=sigmv(j)-sigmoff(i)
            if(sigmv(j).lt.zero) sigmv(j)=e0f*e0o/(e0fo*e0)*sigmv(j)
            if(kz(ix).eq.12) then
              ejv(j)=ejv(j)+ed(ix)*sin(hsyc(ix)*sigmv(j)+phas+          &
     &phasc(ix))
            else
              ejv(j)=ejv(j)+hsy(1)*sin(hsy(3)*sigmv(j)+phas)
            endif
            ejfv(j)=sqrt(ejv(j)*ejv(j)-pma*pma)
            rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
            dpsv(j)=(ejfv(j)-e0f)/e0f
            oidpsv(j)=one/(one+dpsv(j))
            dpsv1(j)=dpsv(j)*c1e3*oidpsv(j)
            if(sigmv(j).gt.zero) sigmv(j)=e0f*e0o/(e0fo*e0)*sigmv(j)
            yv(1,j)=ejf0v(j)/ejfv(j)*yv(1,j)
   40     yv(2,j)=ejf0v(j)/ejfv(j)*yv(2,j)
          if(n.eq.1) write(98,'(1p,6(2x,e25.18))')                      &
     &(xv(1,j),yv(1,j),xv(2,j),yv(2,j),sigmv(j),dpsv(j),j=1,napx)
          goto 640
!--HORIZONTAL DIPOLE
   50     do 60 j=1,napx
            yv(1,j)=yv(1,j)+strackc(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+stracks(i)*oidpsv(j)
   60     continue
          goto 640
!--NORMAL QUADRUPOLE
   70     do 80 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
   80     continue
          goto 640
!--NORMAL SEXTUPOLE
   90     do 100 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  100     continue
          goto 640
!--NORMAL OCTUPOLE
  110     do 120 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  120     continue
          goto 640
!--NORMAL DECAPOLE
  130     do 140 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  140     continue
          goto 640
!--NORMAL DODECAPOLE
  150     do 160 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  160     continue
          goto 640
!--NORMAL 14-POLE
  170     do 180 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  180     continue
          goto 640
!--NORMAL 16-POLE
  190     do 200 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  200     continue
          goto 640
!--NORMAL 18-POLE
  210     do 220 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  220     continue
          goto 640
!--NORMAL 20-POLE
  230     do 240 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  240     continue
          goto 640
  250     continue
          do 260 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  260     continue
          goto 640
  270     continue
          do 280 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  280     continue
          goto 410
  290     continue
          do 300 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  300     continue
          goto 640
  310     continue
          do 320 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  320     continue
          goto 410
  330     continue
          do 340 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  340     continue
          goto 640
  350     continue
          do 360 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  360     continue
          goto 410
  370     continue
          do 380 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  380     continue
          goto 640
  390     continue
          do 400 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  400     continue
  410     r0=ek(ix)
          nmz=nmu(ix)
          if(nmz.ge.2) then
            do 430 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
              yv1j=bbiv(1,1,i)+bbiv(2,1,i)*xlvj+aaiv(2,1,i)*zlvj
              yv2j=aaiv(1,1,i)-bbiv(2,1,i)*zlvj+aaiv(2,1,i)*xlvj
              crkve=xlvj
              cikve=zlvj
                do 420 k=3,nmz
                  crkveuk=crkve*xlvj-cikve*zlvj
                  cikve=crkve*zlvj+cikve*xlvj
                  crkve=crkveuk
                  yv1j=yv1j+bbiv(k,1,i)*crkve+aaiv(k,1,i)*cikve
                  yv2j=yv2j-bbiv(k,1,i)*cikve+aaiv(k,1,i)*crkve
  420           continue
              yv(1,j)=yv(1,j)+(tiltc(i)*yv1j-tilts(i)*yv2j)*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*yv2j+tilts(i)*yv1j)*oidpsv(j)
  430       continue
          else
            do 435 j=1,napx
              yv(1,j)=yv(1,j)+(tiltc(i)*bbiv(1,1,i)-                    &
     &tilts(i)*aaiv(1,1,i))*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*aaiv(1,1,i)+                    &
     &tilts(i)*bbiv(1,1,i))*oidpsv(j)
  435       continue
          endif
          goto 640
!--SKEW ELEMENTS
!--VERTICAL DIPOLE
  440     do 450 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+strackc(i)*oidpsv(j)
  450     continue
          goto 640
!--SKEW QUADRUPOLE
  460     do 470 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  470     continue
          goto 640
!--SKEW SEXTUPOLE
  480     do 490 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  490     continue
          goto 640
!--SKEW OCTUPOLE
  500     do 510 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  510     continue
          goto 640
!--SKEW DECAPOLE
  520     do 530 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  530     continue
          goto 640
!--SKEW DODECAPOLE
  540     do 550 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  550     continue
          goto 640
!--SKEW 14-POLE
  560     do 570 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  570     continue
          goto 640
!--SKEW 16-POLE
  580     do 590 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  590     continue
          goto 640
!--SKEW 18-POLE
  600     do 610 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  610     continue
          goto 640
!--SKEW 20-POLE
  620     do 630 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  630     continue
          goto 640
  680     continue
          do 690 j=1,napx
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)                                       &
     &goto 690
            tkb(j)=rho2b(j)/(two*sigman2(1,imbb(i)))
            if(ibbc.eq.0) then
              yv(1,j)=yv(1,j)+oidpsv(j)*(strack(i)*crkveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))
              yv(2,j)=yv(2,j)+oidpsv(j)*(strack(i)*cikveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))
            else
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),11)-          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
              yv(1,j)=yv(1,j)+oidpsv(j)*cccc
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),12)+          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
              yv(2,j)=yv(2,j)+oidpsv(j)*cccc
            endif
  690     continue
          goto 640
  700     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 640
  720     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 640
  730     continue
!--Hirata's 6D beam-beam kick
            do j=1,napx
              track6d(1,j)=(xv(1,j)+ed(ix)-clobeam(1,imbb(i)))*c1m3
              track6d(2,j)=(yv(1,j)/oidpsv(j)-clobeam(4,imbb(i)))*c1m3
              track6d(3,j)=(xv(2,j)+ek(ix)-clobeam(2,imbb(i)))*c1m3
              track6d(4,j)=(yv(2,j)/oidpsv(j)-clobeam(5,imbb(i)))*c1m3
              track6d(5,j)=(sigmv(j)-clobeam(3,imbb(i)))*c1m3
              track6d(6,j)=dpsv(j)-clobeam(6,imbb(i))
            enddo
            call beamint(napx,track6d,parbe,sigz,bbcu,imbb(i),ix,ibtyp, &
     &ibbc)
            do j=1,napx
              xv(1,j)=track6d(1,j)*c1e3+clobeam(1,imbb(i))-             &
     &beamoff(1,imbb(i))
              xv(2,j)=track6d(3,j)*c1e3+clobeam(2,imbb(i))-             &
     &beamoff(2,imbb(i))
              dpsv(j)=track6d(6,j)+clobeam(6,imbb(i))-beamoff(6,imbb(i))
              oidpsv(j)=one/(one+dpsv(j))
              yv(1,j)=(track6d(2,j)*c1e3+clobeam(4,imbb(i))-            &
     &beamoff(4,imbb(i)))*oidpsv(j)
              yv(2,j)=(track6d(4,j)*c1e3+clobeam(5,imbb(i))-            &
     &beamoff(5,imbb(i)))*oidpsv(j)
              ejfv(j)=dpsv(j)*e0f+e0f
              ejv(j)=sqrt(ejfv(j)*ejfv(j)+pma*pma)
              rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
              if(ithick.eq.1) call envarsv(dpsv,oidpsv,rvv,ekv)
            enddo
          goto 640
  740     continue
          irrtr=imtr(ix)
          do j=1,napx
            sigmv(j)=sigmv(j)+cotr(irrtr,5)+rrtr(irrtr,5,1)*xv(1,j)+    &
     &rrtr(irrtr,5,2)*yv(1,j)+rrtr(irrtr,5,3)*xv(2,j)+                  &
     &rrtr(irrtr,5,4)*yv(2,j)
            pux=xv(1,j)
            dpsv3(j)=dpsv(j)*c1e3
            xv(1,j)=cotr(irrtr,1)+rrtr(irrtr,1,1)*pux+                  &
     &rrtr(irrtr,1,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,1,6)
            yv(1,j)=cotr(irrtr,2)+rrtr(irrtr,2,1)*pux+                  &
     &rrtr(irrtr,2,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,2,6)
            pux=xv(2,j)
            xv(2,j)=cotr(irrtr,3)+rrtr(irrtr,3,3)*pux+                  &
     &rrtr(irrtr,3,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,3,6)
            yv(2,j)=cotr(irrtr,4)+rrtr(irrtr,4,3)*pux+                  &
     &rrtr(irrtr,4,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,4,6)
          enddo
 
!----------------------------------------------------------------------
 
! Wire.
 
          goto 640
  745     continue
          xory=1
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 640
  746     continue
          xory=2
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 640
 
!----------------------------
 
! Wire.
 
  748     continue
!     magnetic rigidity
      chi = sqrt(e0*e0-pmap*pmap)*c1e6/clight
 
      ix = ixcav
      tx = xrms(ix)
      ty = zrms(ix)
      dx = xpl(ix)
      dy = zpl(ix)
      embl = ek(ix)
      l = wirel(ix)
      cur = ed(ix)
 
      leff = embl/cos(tx)/cos(ty)
      rx = dx *cos(tx)-embl*sin(tx)/2
      lin= dx *sin(tx)+embl*cos(tx)/2
      ry = dy *cos(ty)-lin *sin(ty)
      lin= lin*cos(ty)+dy  *sin(ty)
 
      do 750 j=1, napx
 
      xv(1,j) = xv(1,j) * c1m3
      xv(2,j) = xv(2,j) * c1m3
      yv(1,j) = yv(1,j) * c1m3
      yv(2,j) = yv(2,j) * c1m3
 
!      print *, 'Start: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
!     call tilt(tx,ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(tx)*yv(2,j)/sqrt((1+dpsv(j))**2-    &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-tx)
      xv(1,j) = xv(1,j)*(cos(tx)-sin(tx)*tan(atan(yv(1,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(ty)*yv(1,j)/sqrt((1+dpsv(j))**2-    &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-ty)
      xv(2,j) = xv(2,j)*(cos(ty)-sin(ty)*tan(atan(yv(2,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty)
 
!     call drift(lin)
 
      xv(1,j) = xv(1,j) + lin*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) + lin*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
 
!      call kick(l,cur,lin,rx,ry,chi)
 
      xi = xv(1,j)-rx
      yi = xv(2,j)-ry
      yv(1,j) = yv(1,j)-1.0d-7*cur/chi*xi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
!GRD FOR CONSISTENSY
!      yv(2,j) = yv(2,j)-1e-7*cur/chi*yi/(xi**2+yi**2)*                  &
      yv(2,j) = yv(2,j)-1.0d-7*cur/chi*yi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
 
!     call drift(leff-lin)
 
      xv(1,j) = xv(1,j) + (leff-lin)*yv(1,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
      xv(2,j) = xv(2,j) + (leff-lin)*yv(2,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
 
!     call invtilt(tx,ty)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(-ty)*yv(1,j)/sqrt((1+dpsv(j))**2-   &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+ty)
      xv(2,j) = xv(2,j)*(cos(-ty)-sin(-ty)*tan(atan(yv(2,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(-tx)*yv(2,j)/sqrt((1+dpsv(j))**2-   &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+tx)
      xv(1,j) = xv(1,j)*(cos(-tx)-sin(-tx)*tan(atan(yv(1,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx)
 
!     call shift(-embl*tan(tx),-embl*tan(ty)/cos(tx))
 
      xv(1,j) = xv(1,j) + embl*tan(tx)
      xv(2,j) = xv(2,j) + embl*tan(ty)/cos(tx)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
      xv(1,j) = xv(1,j) * c1e3
      xv(2,j) = xv(2,j) * c1e3
      yv(1,j) = yv(1,j) * c1e3
      yv(2,j) = yv(2,j) * c1e3
 
!      print *, 'End: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!-----------------------------------------------------------------------
 
  750     continue
          goto 640
 
!----------------------------
 
  640     continue
          llost=.false.
          do j=1,napx
             llost=llost.or.                                            &
     &abs(xv(1,j)).gt.aper(1).or.abs(xv(2,j)).gt.aper(2)
          enddo
          if (llost) then
             kpz=abs(kp(ix))
             if(kpz.eq.2) then
                call lostpar3(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             elseif(kpz.eq.3) then
                call lostpar4(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             else
                call lostpar2(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             endif
          endif
  650   continue
        call lostpart(nthinerr)
        if(nthinerr.ne.0) return
        if(ntwin.ne.2) call dist1
        if(mod(n,nwr(4)).eq.0) call write6(n)
  660 continue
      return
      end
      subroutine ripple(n)
!-----------------------------------------------------------------------
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
      implicit none
      integer i,n,nripple
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      save
!-----------------------------------------------------------------------
      nripple=nrturn+n
      do 20 i=1,iu
        if(abs(rsmi(i)).gt.pieni) then
          smiv(1,i)=rsmi(i)*cos(two*pi*(nripple-1)/rfres(i)+rzphs(i))
        strack(i)=smiv(1,i)
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        endif
   20 continue
      return
      end
      subroutine writebin(nthinerr)
!-----------------------------------------------------------------------
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
!  3 February 1999
!-----------------------------------------------------------------------
      implicit none
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      integer ia,ia2,ie,nthinerr
      save
!-----------------------------------------------------------------------
!GRD      do 10 ia=1,napx
      do 10 ia=1,napx-1
!GRD
        if(.not.pstop(nlostp(ia)).and..not.pstop(nlostp(ia)+1).and.     &
     &(mod(nlostp(ia),2).ne.0)) then
          ia2=(nlostp(ia)+1)/2
          ie=ia+1
          if(ntwin.ne.2) then
            write(91-ia2,iostat=ierro)                                  &
     &numx,nlostp(ia),dam(ia),                                          &
     &xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),sigmv(ia),dpsv(ia),e0
            endfile 91-ia2
            backspace 91-ia2
          else
            write(91-ia2,iostat=ierro)                                  &
     &numx,nlostp(ia),dam(ia),                                          &
     &xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),sigmv(ia),dpsv(ia),e0,        &
     &nlostp(ia)+1,dam(ia),                                             &
     &xv(1,ie),yv(1,ie),xv(2,ie),yv(2,ie),sigmv(ie),dpsv(ie),e0
            endfile 91-ia2
            backspace 91-ia2
          endif
          if(ierro.ne.0) then
            write(*,*)
            write(*,*) '*** ERROR ***,PROBLEMS WRITING TO FILE # : ',   &
     &91-ia2
            write(*,*) 'ERROR CODE : ',ierro
            write(*,*)
            endfile 12
            backspace 12
            nthinerr=3000
            return
          endif
        endif
   10 continue
      return
      end
      subroutine lostpart(nthinerr)
!-----------------------------------------------------------------------
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
!  3 February 1999
!-----------------------------------------------------------------------
      implicit none
!      logical isnan
      logical myisnan
      integer ib2,ib3,ilostch,j,jj,jj1,lnapx,nthinerr
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer nnumxv
      common/postr2/nnumxv(npart)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      save
!-----------------------------------------------------------------------
      ilostch=0
      do 10 j=1,napx
        if(abs(xv(1,j)).gt.aper(1).or.abs(xv(2,j)).gt.aper(2).or.       &
!     &isnan(xv(1,j),xv(1,j)).or.isnan(xv(2,j),xv(2,j))) then
     &myisnan(xv(1,j),xv(1,j)).or.myisnan(xv(2,j),xv(2,j))) then
          ilostch=1
          pstop(nlostp(j))=.true.
        endif
  10  continue
      do 20 j=1,napx
        if(pstop(nlostp(j))) then
          aperv(nlostp(j),1)=aper(1)
          aperv(nlostp(j),2)=aper(2)
          xvl(1,nlostp(j))=xv(1,j)
          xvl(2,nlostp(j))=xv(2,j)
          yvl(1,nlostp(j))=yv(1,j)
          yvl(2,nlostp(j))=yv(2,j)
          dpsvl(nlostp(j))=dpsv(j)
          ejvl(nlostp(j))=ejv(j)
          sigmvl(nlostp(j))=sigmv(j)
          numxv(nlostp(j))=numx
          nnumxv(nlostp(j))=numx
          if(mod(nlostp(j),2).eq.one) then
            write(*,10000) nlostp(j),nms(nlostp(j))*izu0,               &
     &dp0v(nlostp(j)),numxv(nlostp(j)),abs(xvl(1,nlostp(j))),           &
     &aperv(nlostp(j),1),abs(xvl(2,nlostp(j))),                         &
     &aperv(nlostp(j),2)
          else
            write(*,10000) nlostp(j),nms(nlostp(j)-1)*izu0,             &
     &dp0v(nlostp(j)-1),numxv(nlostp(j)),abs(xvl(1,nlostp(j))),         &
     &aperv(nlostp(j),1),abs(xvl(2,nlostp(j))),                         &
     &aperv(nlostp(j),2)
          endif
        endif
   20 continue
      lnapx=napx
      do 30 j=napx,1,-1
        if(pstop(nlostp(j))) then
          if(j.ne.lnapx) then
            do 35 jj=j,lnapx-1
              jj1=jj+1
              nlostp(jj)=nlostp(jj1)
              xv(1,jj)=xv(1,jj1)
              xv(2,jj)=xv(2,jj1)
              yv(1,jj)=yv(1,jj1)
              yv(2,jj)=yv(2,jj1)
              dpsv(jj)=dpsv(jj1)
              sigmv(jj)=sigmv(jj1)
              ejfv(jj)=ejfv(jj1)
              ejv(jj)=ejv(jj1)
              rvv(jj)=rvv(jj1)
              oidpsv(jj)=oidpsv(jj1)
              dpsv1(jj)=dpsv1(jj1)
              clo6v(1,jj)=clo6v(1,jj1)
              clo6v(2,jj)=clo6v(2,jj1)
              clo6v(3,jj)=clo6v(3,jj1)
              clop6v(1,jj)=clop6v(1,jj1)
              clop6v(2,jj)=clop6v(2,jj1)
              clop6v(3,jj)=clop6v(3,jj1)
!--beam-beam element
              di0xs(jj)=di0xs(jj1)
              dip0xs(jj)=dip0xs(jj1)
              di0zs(jj)=di0zs(jj1)
              dip0zs(jj)=dip0zs(jj1)
              do 210 ib2=1,6
                do 210 ib3=1,6
                  tasau(jj,ib2,ib3)=tasau(jj1,ib2,ib3)
  210         continue
   35       continue
          endif
          lnapx=lnapx-1
        endif
   30 continue
      if(lnapx.eq.0) then
        write(*,*)
        write(*,*)
        write(*,*) '***********************'
        write(*,*) '** ALL PARTICLE LOST **'
        write(*,*) '**   PROGRAM STOPS   **'
        write(*,*) '***********************'
        write(*,*)
        write(*,*)
        nthinerr=3001
        return
      endif
      if(ithick.eq.1.and.ilostch.eq.1)                                  &
     &call synuthck
      napx=lnapx
      return
10000 format(t10,'TRACKING ENDED ABNORMALLY'/t10, 'PARTICLE ',i3,       &
     &' RANDOM SEED ',i8,/ t10,' MOMENTUM DEVIATION ',g12.5,            &
     &' LOST IN REVOLUTION ',i8,/ t10,'HORIZ:  AMPLITUDE = ',f15.3,     &
     &'   APERTURE = ',f15.3/ t10,'VERT:   AMPLITUDE = ',f15.3,         &
     &'   APERTURE = ',f15.3/)
      end
      subroutine lostpar2(i,ix,nthinerr)
!-----------------------------------------------------------------------
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
!  3 February 1999
!-----------------------------------------------------------------------
      implicit none
!      logical isnan
      logical myisnan
      integer i,ib2,ib3,ilostch,ix,j,jj,jj1,lnapx,nthinerr
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer nnumxv
      common/postr2/nnumxv(npart)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      save
!-----------------------------------------------------------------------
      ilostch=0
      do 10 j=1,napx
        if(abs(xv(1,j)).gt.aper(1).or.abs(xv(2,j)).gt.aper(2).or.       &
!     &isnan(xv(1,j),xv(1,j)).or.isnan(xv(2,j),xv(2,j))) then
     &myisnan(xv(1,j),xv(1,j)).or.myisnan(xv(2,j),xv(2,j))) then
          ilostch=1
          pstop(nlostp(j))=.true.
        endif
  10  continue
      do 20 j=1,napx
        if(pstop(nlostp(j))) then
          aperv(nlostp(j),1)=aper(1)
          aperv(nlostp(j),2)=aper(2)
          iv(nlostp(j))=i
          ixv(nlostp(j))=ix
          xvl(1,nlostp(j))=xv(1,j)
          xvl(2,nlostp(j))=xv(2,j)
          yvl(1,nlostp(j))=yv(1,j)
          yvl(2,nlostp(j))=yv(2,j)
          dpsvl(nlostp(j))=dpsv(j)
          ejvl(nlostp(j))=ejv(j)
          sigmvl(nlostp(j))=sigmv(j)
          numxv(nlostp(j))=numx
          nnumxv(nlostp(j))=numx
          if(mod(nlostp(j),2).eq.one) then
            write(*,10000) nlostp(j),nms(nlostp(j))*izu0,               &
     &dp0v(nlostp(j)),numxv(nlostp(j)),iv(nlostp(j)),                   &
     &abs(xvl(1,nlostp(j))),aperv(nlostp(j),1),                         &
     &abs(xvl(2,nlostp(j))),aperv(nlostp(j),2),                         &
     &ixv(nlostp(j)),kz(ixv(nlostp(j))),bez(ixv(nlostp(j)))
          else
            write(*,10000) nlostp(j),nms(nlostp(j)-1)*izu0,             &
     &dp0v(nlostp(j)-1),numxv(nlostp(j)),iv(nlostp(j)),                 &
     &abs(xvl(1,nlostp(j))),aperv(nlostp(j),1),                         &
     &abs(xvl(2,nlostp(j))),aperv(nlostp(j),2),                         &
     &ixv(nlostp(j)),kz(ixv(nlostp(j))),bez(ixv(nlostp(j)))
          endif
        endif
   20 continue
      lnapx=napx
      do 30 j=napx,1,-1
        if(pstop(nlostp(j))) then
          if(j.ne.lnapx) then
            do 35 jj=j,lnapx-1
              jj1=jj+1
              nlostp(jj)=nlostp(jj1)
              xv(1,jj)=xv(1,jj1)
              xv(2,jj)=xv(2,jj1)
              yv(1,jj)=yv(1,jj1)
              yv(2,jj)=yv(2,jj1)
              dpsv(jj)=dpsv(jj1)
              sigmv(jj)=sigmv(jj1)
              ejfv(jj)=ejfv(jj1)
              ejv(jj)=ejv(jj1)
              rvv(jj)=rvv(jj1)
              oidpsv(jj)=oidpsv(jj1)
              dpsv1(jj)=dpsv1(jj1)
              clo6v(1,jj)=clo6v(1,jj1)
              clo6v(2,jj)=clo6v(2,jj1)
              clo6v(3,jj)=clo6v(3,jj1)
              clop6v(1,jj)=clop6v(1,jj1)
              clop6v(2,jj)=clop6v(2,jj1)
              clop6v(3,jj)=clop6v(3,jj1)
!--beam-beam element
              di0xs(jj)=di0xs(jj1)
              dip0xs(jj)=dip0xs(jj1)
              di0zs(jj)=di0zs(jj1)
              dip0zs(jj)=dip0zs(jj1)
              do 210 ib2=1,6
                do 210 ib3=1,6
                  tasau(jj,ib2,ib3)=tasau(jj1,ib2,ib3)
  210         continue
   35       continue
          endif
          lnapx=lnapx-1
        endif
   30 continue
      if(lnapx.eq.0) then
        write(*,*)
        write(*,*)
        write(*,*) '***********************'
        write(*,*) '** ALL PARTICLE LOST **'
        write(*,*) '**   PROGRAM STOPS   **'
        write(*,*) '***********************'
        write(*,*)
        write(*,*)
        nthinerr=3001
        return
      endif
      if(ithick.eq.1.and.ilostch.eq.1)                                  &
     &call synuthck
      napx=lnapx
      return
10000 format(t10,'TRACKING ENDED ABNORMALLY'/t10, 'PARTICLE ',i3,       &
     &' RANDOM SEED ',i8, ' MOMENTUM DEVIATION ',g12.5/ t10,            &
     &' LOST IN REVOLUTION ',i8,' AT ELEMENT ',i4/ t10,                 &
     &'HORIZ:  AMPLITUDE = ',f15.3,'RE-APERTURE = ',f15.3/ t10,         &
     &'VERT:   AMPLITUDE = ',f15.3,'RE-APERTURE = ',f15.3/ t10,         &
     &'ELEMENT - LIST NUMBER ',i4,' TYP NUMBER ',i4,' NAME ',a16/)
      end
      subroutine lostpar3(i,ix,nthinerr)
!-----------------------------------------------------------------------
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
!  3 February 1999
!-----------------------------------------------------------------------
      implicit none
!      logical isnan
      logical myisnan
      integer i,ib2,ib3,ilostch,ix,j,jj,jj1,lnapx,nthinerr
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer nnumxv
      common/postr2/nnumxv(npart)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      save
!-----------------------------------------------------------------------
      ilostch=0
      do 10 j=1,napx
        if(abs(xv(1,j)).gt.apx(ix).or.abs(xv(2,j)).gt.apz(ix).or.       &
!     &isnan(xv(1,j),xv(1,j)).or.isnan(xv(2,j),xv(2,j))) then
     &myisnan(xv(1,j),xv(1,j)).or.myisnan(xv(2,j),xv(2,j))) then
          ilostch=1
          pstop(nlostp(j))=.true.
        endif
  10  continue
      do 20 j=1,napx
        if(pstop(nlostp(j))) then
          aperv(nlostp(j),1)=apx(ix)
          aperv(nlostp(j),2)=apz(ix)
          iv(nlostp(j))=i
          ixv(nlostp(j))=ix
          xvl(1,nlostp(j))=xv(1,j)
          xvl(2,nlostp(j))=xv(2,j)
          yvl(1,nlostp(j))=yv(1,j)
          yvl(2,nlostp(j))=yv(2,j)
          dpsvl(nlostp(j))=dpsv(j)
          ejvl(nlostp(j))=ejv(j)
          sigmvl(nlostp(j))=sigmv(j)
          numxv(nlostp(j))=numx
          nnumxv(nlostp(j))=numx
          if(mod(nlostp(j),2).eq.one) then
            write(*,10000) nlostp(j),nms(nlostp(j))*izu0,               &
     &dp0v(nlostp(j)),numxv(nlostp(j)),iv(nlostp(j)),                   &
     &abs(xvl(1,nlostp(j))),aperv(nlostp(j),1),                         &
     &abs(xvl(2,nlostp(j))),aperv(nlostp(j),2),                         &
     &ixv(nlostp(j)),kz(ixv(nlostp(j))),bez(ixv(nlostp(j)))
          else
            write(*,10000) nlostp(j),nms(nlostp(j)-1)*izu0,             &
     &dp0v(nlostp(j)-1),numxv(nlostp(j)),iv(nlostp(j)),                 &
     &abs(xvl(1,nlostp(j))),aperv(nlostp(j),1),                         &
     &abs(xvl(2,nlostp(j))),aperv(nlostp(j),2),                         &
     &ixv(nlostp(j)),kz(ixv(nlostp(j))),bez(ixv(nlostp(j)))
          endif
        endif
   20 continue
      lnapx=napx
      do 30 j=napx,1,-1
        if(pstop(nlostp(j))) then
          if(j.ne.lnapx) then
            do 35 jj=j,lnapx-1
              jj1=jj+1
              nlostp(jj)=nlostp(jj1)
              xv(1,jj)=xv(1,jj1)
              xv(2,jj)=xv(2,jj1)
              yv(1,jj)=yv(1,jj1)
              yv(2,jj)=yv(2,jj1)
              dpsv(jj)=dpsv(jj1)
              sigmv(jj)=sigmv(jj1)
              ejfv(jj)=ejfv(jj1)
              ejv(jj)=ejv(jj1)
              rvv(jj)=rvv(jj1)
              oidpsv(jj)=oidpsv(jj1)
              dpsv1(jj)=dpsv1(jj1)
              clo6v(1,jj)=clo6v(1,jj1)
              clo6v(2,jj)=clo6v(2,jj1)
              clo6v(3,jj)=clo6v(3,jj1)
              clop6v(1,jj)=clop6v(1,jj1)
              clop6v(2,jj)=clop6v(2,jj1)
              clop6v(3,jj)=clop6v(3,jj1)
!--beam-beam element
              di0xs(jj)=di0xs(jj1)
              dip0xs(jj)=dip0xs(jj1)
              di0zs(jj)=di0zs(jj1)
              dip0zs(jj)=dip0zs(jj1)
              do 210 ib2=1,6
                do 210 ib3=1,6
                  tasau(jj,ib2,ib3)=tasau(jj1,ib2,ib3)
  210         continue
   35       continue
          endif
          lnapx=lnapx-1
        endif
   30 continue
      if(lnapx.eq.0) then
        write(*,*)
        write(*,*)
        write(*,*) '***********************'
        write(*,*) '** ALL PARTICLE LOST **'
        write(*,*) '**   PROGRAM STOPS   **'
        write(*,*) '***********************'
        write(*,*)
        write(*,*)
        nthinerr=3001
        return
      endif
      if(ithick.eq.1.and.ilostch.eq.1)                                  &
     &call synuthck
      napx=lnapx
      return
10000 format(t10,'TRACKING ENDED ABNORMALLY'/t10, 'PARTICLE ',i3,       &
     &' RANDOM SEED ',i8, ' MOMENTUM DEVIATION ',g12.5/ t10,            &
     &' LOST IN REVOLUTION ',i8,' AT ELEMENT ',i4/ t10,                 &
     &'HORIZ:  AMPLITUDE = ',f15.3,'RE-APERTURE = ',f15.3/ t10,         &
     &'VERT:   AMPLITUDE = ',f15.3,'RE-APERTURE = ',f15.3/ t10,         &
     &'ELEMENT - LIST NUMBER ',i4,' TYP NUMBER ',i4,' NAME ',a16/)
      end
      subroutine lostpar4(i,ix,nthinerr)
!-----------------------------------------------------------------------
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
!  3 February 1999
!-----------------------------------------------------------------------
      implicit none
!      logical isnan
      logical myisnan
      integer i,ib2,ib3,ilostch,ix,j,jj,jj1,lnapx,nthinerr
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer nnumxv
      common/postr2/nnumxv(npart)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      save
!-----------------------------------------------------------------------
      ilostch=0
      do 10 j=1,napx
        if(xv(1,j)*xv(1,j)*ape(1,ix)+xv(2,j)*xv(2,j)*ape(2,ix).gt.      &
     &ape(3,ix).or.                                                     &
!     &isnan(xv(1,j),xv(1,j)).or.isnan(xv(2,j),xv(2,j))) then
     &myisnan(xv(1,j),xv(1,j)).or.myisnan(xv(2,j),xv(2,j))) then
          ilostch=1
          pstop(nlostp(j))=.true.
        endif
  10  continue
      do 20 j=1,napx
        if(pstop(nlostp(j))) then
          aperv(nlostp(j),1)=apx(ix)
          aperv(nlostp(j),2)=apz(ix)
          iv(nlostp(j))=i
          ixv(nlostp(j))=ix
          xvl(1,nlostp(j))=xv(1,j)
          xvl(2,nlostp(j))=xv(2,j)
          yvl(1,nlostp(j))=yv(1,j)
          yvl(2,nlostp(j))=yv(2,j)
          dpsvl(nlostp(j))=dpsv(j)
          ejvl(nlostp(j))=ejv(j)
          sigmvl(nlostp(j))=sigmv(j)
          numxv(nlostp(j))=numx
          nnumxv(nlostp(j))=numx
          if(mod(nlostp(j),2).eq.one) then
            write(*,10000) nlostp(j),nms(nlostp(j))*izu0,               &
     &dp0v(nlostp(j)),numxv(nlostp(j)),iv(nlostp(j)),                   &
     &abs(xvl(1,nlostp(j))),aperv(nlostp(j),1),                         &
     &abs(xvl(2,nlostp(j))),aperv(nlostp(j),2),                         &
     &ixv(nlostp(j)),kz(ixv(nlostp(j))),bez(ixv(nlostp(j)))
          else
            write(*,10000) nlostp(j),nms(nlostp(j)-1)*izu0,             &
     &dp0v(nlostp(j)-1),numxv(nlostp(j)),iv(nlostp(j)),                 &
     &abs(xvl(1,nlostp(j))),aperv(nlostp(j),1),                         &
     &abs(xvl(2,nlostp(j))),aperv(nlostp(j),2),                         &
     &ixv(nlostp(j)),kz(ixv(nlostp(j))),bez(ixv(nlostp(j)))
          endif
        endif
   20 continue
      lnapx=napx
      do 30 j=napx,1,-1
        if(pstop(nlostp(j))) then
          if(j.ne.lnapx) then
            do 35 jj=j,lnapx-1
              jj1=jj+1
              nlostp(jj)=nlostp(jj1)
              xv(1,jj)=xv(1,jj1)
              xv(2,jj)=xv(2,jj1)
              yv(1,jj)=yv(1,jj1)
              yv(2,jj)=yv(2,jj1)
              dpsv(jj)=dpsv(jj1)
              sigmv(jj)=sigmv(jj1)
              ejfv(jj)=ejfv(jj1)
              ejv(jj)=ejv(jj1)
              rvv(jj)=rvv(jj1)
              oidpsv(jj)=oidpsv(jj1)
              dpsv1(jj)=dpsv1(jj1)
              clo6v(1,jj)=clo6v(1,jj1)
              clo6v(2,jj)=clo6v(2,jj1)
              clo6v(3,jj)=clo6v(3,jj1)
              clop6v(1,jj)=clop6v(1,jj1)
              clop6v(2,jj)=clop6v(2,jj1)
              clop6v(3,jj)=clop6v(3,jj1)
!--beam-beam element
              di0xs(jj)=di0xs(jj1)
              dip0xs(jj)=dip0xs(jj1)
              di0zs(jj)=di0zs(jj1)
              dip0zs(jj)=dip0zs(jj1)
              do 210 ib2=1,6
                do 210 ib3=1,6
                  tasau(jj,ib2,ib3)=tasau(jj1,ib2,ib3)
  210         continue
   35       continue
          endif
          lnapx=lnapx-1
        endif
   30 continue
      if(lnapx.eq.0) then
        write(*,*)
        write(*,*)
        write(*,*) '***********************'
        write(*,*) '** ALL PARTICLE LOST **'
        write(*,*) '**   PROGRAM STOPS   **'
        write(*,*) '***********************'
        write(*,*)
        write(*,*)
        nthinerr=3001
        return
      endif
      if(ithick.eq.1.and.ilostch.eq.1)                                  &
     &call synuthck
      napx=lnapx
      return
10000 format(t10,'TRACKING ENDED ABNORMALLY'/t10, 'PARTICLE ',i3,       &
     &' RANDOM SEED ',i8, ' MOMENTUM DEVIATION ',g12.5/ t10,            &
     &' LOST IN REVOLUTION ',i8,' AT ELEMENT ',i4/ t10,                 &
     &'HORIZ:  AMPLITUDE = ',f15.3,'EL-APERTURE = ',f15.3/ t10,         &
     &'VERT:   AMPLITUDE = ',f15.3,'EL-APERTURE = ',f15.3/ t10,         &
     &'ELEMENT - LIST NUMBER ',i4,' TYP NUMBER ',i4,' NAME ',a16/)
      end
      subroutine dist1
!-----------------------------------------------------------------------
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
!  3 February 1999
!-----------------------------------------------------------------------
      implicit none
      integer ia,ib2,ib3,ie
      double precision dam1
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      save
!-----------------------------------------------------------------------
      do 20 ia=1,napx,2
        if(.not.pstop(nlostp(ia)).and..not.pstop(nlostp(ia)+1).and.     &
     &(mod(nlostp(ia),2).ne.0)) then
          ie=ia+1
          dam(ia)=zero
          dam(ie)=zero
          xau(1,1)= xv(1,ia)
          xau(1,2)= yv(1,ia)
          xau(1,3)= xv(2,ia)
          xau(1,4)= yv(2,ia)
          xau(1,5)=sigmv(ia)
          xau(1,6)= dpsv(ia)
          xau(2,1)= xv(1,ie)
          xau(2,2)= yv(1,ie)
          xau(2,3)= xv(2,ie)
          xau(2,4)= yv(2,ie)
          xau(2,5)=sigmv(ie)
          xau(2,6)= dpsv(ie)
          cloau(1)= clo6v(1,ia)
          cloau(2)=clop6v(1,ia)
          cloau(3)= clo6v(2,ia)
          cloau(4)=clop6v(2,ia)
          cloau(5)= clo6v(3,ia)
          cloau(6)=clop6v(3,ia)
          di0au(1)= di0xs(ia)
          di0au(2)=dip0xs(ia)
          di0au(3)= di0zs(ia)
          di0au(4)=dip0zs(ia)
          do 10 ib2=1,6
            do 10 ib3=1,6
              tau(ib2,ib3)=tasau(ia,ib2,ib3)
   10     continue
          call distance(xau,cloau,di0au,tau,dam1)
          dam(ia)=dam1
          dam(ie)=dam1
        endif
   20 continue
      return
      end
      subroutine write6(n)
!-----------------------------------------------------------------------
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
!  3 February 1999
!-----------------------------------------------------------------------
      implicit none
      integer ia,ia2,id,ie,ig,n
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer nnumxv
      common/postr2/nnumxv(npart)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      save
!-----------------------------------------------------------------------
      id=0
      do 10 ia=1,napxo,2
        ig=ia+1
        ia2=ig/2
        endfile 91-ia2
        backspace 91-ia2
!-- PARTICLES STABLE
        if(.not.pstop(ia).and..not.pstop(ig)) then
          write(*,10000) ia,nms(ia)*izu0,dp0v(ia),n
          id=id+1
          ie=id+1
          write(*,10010)                                                &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &xv(1,ie),yv(1,ie),xv(2,ie),yv(2,ie),sigmv(ie),dpsv(ie),           &
     &e0,ejv(id),ejv(ie)
          write(12,10010,iostat=ierro)                                  &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &xv(1,ie),yv(1,ie),xv(2,ie),yv(2,ie),sigmv(ie),dpsv(ie),           &
     &e0,ejv(id),ejv(ie)
          id=id+1
!-- FIRST PARTICLES LOST
        else if(pstop(ia).and..not.pstop(ig)) then
          id=id+1
          write(12,10010,iostat=ierro)                                  &
     &xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),     &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &e0,ejvl(ia),ejv(id)
!-- SECOND PARTICLES LOST
        else if(.not.pstop(ia).and.pstop(ig)) then
          id=id+1
          write(12,10010,iostat=ierro)                                  &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &xvl(1,ig),yvl(1,ig),xvl(2,ig),yvl(2,ig),sigmvl(ig),dpsvl(ig),     &
     &e0,ejv(id),ejvl(ig)
!-- BOTH PARTICLES LOST
        else if(pstop(ia).and.pstop(ig)) then
        endif
   10 continue
      if(ierro.ne.0) write(*,*) 'Warning from write6: fort.12 has ',    &
     &'corrupted output probably due to lost particles'
      endfile 12
      backspace 12
      return
10000 format(1x/5x,'PARTICLE ',i3,' RANDOM SEED ',i8,                   &
     &' MOMENTUM DEVIATION ',g12.5 /5x,'REVOLUTION ',i8/)
10010 format(10x,f47.33)
      end
      subroutine trauthck(nthinerr)
!-----------------------------------------------------------------------
!
!  TRACK THICK LENS PART
!
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
      implicit none
      integer i,ix,j,jb,jj,jx,kpz,kzz,napx0,nbeaux,nmz,nthinerr
      double precision benkcc,cbxb,cbzb,cikveb,crkveb,crxb,crzb,r0,r000,&
     &r0a,r2b,rb,rho2b,rkb,tkb,xbb,xrb,zbb,zrb
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      double precision cc,xlim,ylim
      parameter(cc = 1.12837916709551d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),    &
     &r2b(npart),rb(npart),rkb(npart),                                  &
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),          &
     &crzb(npart),cbxb(npart),cbzb(npart)
      dimension nbeaux(nbb)
      save
!-----------------------------------------------------------------------
      do 5 i=1,npart
        nlostp(i)=i
   5  continue
      do 10 i=1,nblz
        ktrack(i)=0
        strack(i)=zero
        strackc(i)=zero
        stracks(i)=zero
   10 continue
!--beam-beam element
      if(nbeam.ge.1) then
        do 15 i=1,nbb
          nbeaux(i)=0
   15   continue
        do i=1,iu
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(kz(ix).eq.20.and.parbe(ix,2).eq.0) then
!--round beam
              if(sigman(1,imbb(i)).eq.sigman(2,imbb(i))) then
                if(nbeaux(imbb(i)).eq.2.or.nbeaux(imbb(i)).eq.3) then
                  call prror(89)
                else
                  nbeaux(imbb(i))=1
                  sigman2(1,imbb(i))=sigman(1,imbb(i))**2
                endif
              endif
!--elliptic beam x>z
              if(sigman(1,imbb(i)).gt.sigman(2,imbb(i))) then
                if(nbeaux(imbb(i)).eq.1.or.nbeaux(imbb(i)).eq.3) then
                  call prror(89)
                else
                  nbeaux(imbb(i))=2
                  sigman2(1,imbb(i))=sigman(1,imbb(i))**2
                  sigman2(2,imbb(i))=sigman(2,imbb(i))**2
                  sigmanq(1,imbb(i))=sigman(1,imbb(i))/sigman(2,imbb(i))
                  sigmanq(2,imbb(i))=sigman(2,imbb(i))/sigman(1,imbb(i))
                endif
              endif
!--elliptic beam z>x
              if(sigman(1,imbb(i)).lt.sigman(2,imbb(i))) then
                if(nbeaux(imbb(i)).eq.1.or.nbeaux(imbb(i)).eq.2) then
                  call prror(89)
                else
                  nbeaux(imbb(i))=3
                  sigman2(1,imbb(i))=sigman(1,imbb(i))**2
                  sigman2(2,imbb(i))=sigman(2,imbb(i))**2
                  sigmanq(1,imbb(i))=sigman(1,imbb(i))/sigman(2,imbb(i))
                  sigmanq(2,imbb(i))=sigman(2,imbb(i))/sigman(1,imbb(i))
                endif
              endif
            endif
          endif
        enddo
      endif
      do 290 i=1,iu
        if(mout2.eq.1.and.i.eq.1) call write4
        ix=ic(i)
        if(ix.gt.nblo) goto 30
        ktrack(i)=1
        do 20 jb=1,mel(ix)
          jx=mtyp(ix,jb)
          strack(i)=strack(i)+el(jx)
   20   continue
        if(abs(strack(i)).le.pieni) ktrack(i)=31
        goto 290
   30   ix=ix-nblo
        kpz=abs(kp(ix))
        if(kpz.eq.6) then
          ktrack(i)=2
          goto 290
        endif
   40   kzz=kz(ix)
        if(kzz.eq.0) then
          ktrack(i)=31
          goto 290
        endif
!--beam-beam element
        if(kzz.eq.20.and.nbeam.ge.1.and.parbe(ix,2).eq.0) then
          strack(i)=crad*ptnfac(ix)
          if(abs(strack(i)).le.pieni) then
            ktrack(i)=31
            goto 290
          endif
          if(nbeaux(imbb(i)).eq.1) then
            ktrack(i)=41
            if(ibeco.eq.1) then
              do 42 j=1,napx
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)                                       &
     &goto 42
            tkb(j)=rho2b(j)/(two*sigman2(1,imbb(i)))
                beamoff(4,imbb(i))=strack(i)*crkveb(j)/rho2b(j)*        &
     &(one-exp(-tkb(j)))
                beamoff(5,imbb(i))=strack(i)*cikveb(j)/rho2b(j)*        &
     &(one-exp(-tkb(j)))
   42         continue
            endif
          endif
          if(nbeaux(imbb(i)).eq.2) then
            ktrack(i)=42
            if(ibeco.eq.1) then
            if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              beamoff(4,imbb(i))=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))* &
     &sign(one,crkveb(j))
              beamoff(5,imbb(i))=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))* &
     &sign(one,cikveb(j))
            enddo
            else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do j=1,napx
              beamoff(4,imbb(i))=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))* &
     &sign(one,crkveb(j))
              beamoff(5,imbb(i))=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))* &
     &sign(one,cikveb(j))
            enddo
            endif
            endif
          endif
          if(nbeaux(imbb(i)).eq.3) then
            ktrack(i)=43
            if(ibeco.eq.1) then
            if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              beamoff(4,imbb(i))=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))* &
     &sign(one,crkveb(j))
              beamoff(5,imbb(i))=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))* &
     &sign(one,cikveb(j))
            enddo
            else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=ed(ix)
                cikveb(j)=ek(ix)
              else
                crkveb(j)=ed(ix)*bbcu(imbb(i),11)+                      &
     &ek(ix)*bbcu(imbb(i),12)
                cikveb(j)=-ed(ix)*bbcu(imbb(i),12)+                     &
     &ek(ix)*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do j=1,napx
              beamoff(4,imbb(i))=rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))* &
     &sign(one,crkveb(j))
              beamoff(5,imbb(i))=rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))* &
     &sign(one,cikveb(j))
            enddo
            endif
            endif
          endif
          goto 290
!--Hirata's 6D beam-beam kick
        else if(kzz.eq.20.and.parbe(ix,2).gt.0) then
          ktrack(i)=44
          parbe(ix,4)=-crad*ptnfac(ix)*half*c1m6
          if(ibeco.eq.1) then
            track6d(1,1)=ed(ix)*c1m3
            track6d(2,1)=zero
            track6d(3,1)=ek(ix)*c1m3
            track6d(4,1)=zero
            track6d(5,1)=zero
            track6d(6,1)=zero
            napx0=napx
            napx=1
            call beamint(napx,track6d,parbe,sigz,bbcu,imbb(i),ix,ibtyp, &
     &ibbc)
            beamoff(1,imbb(i))=track6d(1,1)*c1e3
            beamoff(2,imbb(i))=track6d(3,1)*c1e3
            beamoff(4,imbb(i))=track6d(2,1)*c1e3
            beamoff(5,imbb(i))=track6d(4,1)*c1e3
            beamoff(6,imbb(i))=track6d(6,1)
            napx=napx0
          endif
          goto 290
        endif
        if(kzz.eq.15) then
          ktrack(i)=45
          goto 290
        endif
        if(kzz.eq.16) then
          ktrack(i)=51
          goto 290
        else if(kzz.eq.-16) then
          ktrack(i)=52
          goto 290
        endif
        if(kzz.eq.22) then
          ktrack(i)=3
          goto 290
        endif
        if(mout2.eq.1.and.icextal(i).ne.0) then
          write(27,'(a16,2x,1p,2d14.6,d17.9)') bez(ix),extalign(i,1),   &
     &extalign(i,2),extalign(i,3)
        endif
        if(kzz.lt.0) goto 180
        goto(50,60,70,80,90,100,110,120,130,140,150),kzz
        ktrack(i)=31
        goto 290
   50   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=11
        strack(i)=smiv(1,i)*c1e3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   60   if(abs(smiv(1,i)).le.pieni.and.abs(ramp(ix)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=12
        strack(i)=smiv(1,i)
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   70   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=13
        strack(i)=smiv(1,i)*c1m3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   80   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=14
        strack(i)=smiv(1,i)*c1m6
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
   90   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=15
        strack(i)=smiv(1,i)*c1m9
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  100   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=16
        strack(i)=smiv(1,i)*c1m12
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  110   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=17
        strack(i)=smiv(1,i)*c1m15
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  120   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=18
        strack(i)=smiv(1,i)*c1m18
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  130   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=19
        strack(i)=smiv(1,i)*c1m21
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  140   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=20
        strack(i)=smiv(1,i)*c1m24
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  150   r0=ek(ix)
        nmz=nmu(ix)
        if(abs(r0).le.pieni.or.nmz.eq.0) then
          if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).le.pieni) then
            ktrack(i)=31
          else if(abs(dki(ix,1)).gt.pieni.and.abs(dki(ix,2)).le.pieni)  &
     &then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=33
              strack(i)=dki(ix,1)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=35
              strack(i)=dki(ix,1)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          else if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).gt.pieni)  &
     &then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=37
              strack(i)=dki(ix,2)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=39
              strack(i)=dki(ix,2)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          endif
        else
          if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).le.pieni) then
            ktrack(i)=32
          else if(abs(dki(ix,1)).gt.pieni.and.abs(dki(ix,2)).le.pieni)  &
     &then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=34
              strack(i)=dki(ix,1)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=36
              strack(i)=dki(ix,1)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          else if(abs(dki(ix,1)).le.pieni.and.abs(dki(ix,2)).gt.pieni)  &
     &then
            if(abs(dki(ix,3)).gt.pieni) then
              ktrack(i)=38
              strack(i)=dki(ix,2)/dki(ix,3)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            else
              ktrack(i)=40
              strack(i)=dki(ix,2)
              strackc(i)=strack(i)*tiltc(i)
              stracks(i)=strack(i)*tilts(i)
            endif
          endif
        endif
        if(abs(r0).le.pieni.or.nmz.eq.0) goto 290
        if(mout2.eq.1) then
          benkcc=ed(ix)*benkc(irm(ix))
          r0a=one
          r000=r0*r00(irm(ix))
          do 160 j=1,mmul
            fake(1,j)=bbiv(j,1,i)*r0a/benkcc
            fake(2,j)=aaiv(j,1,i)*r0a/benkcc
  160     r0a=r0a*r000
          write(9,'(a16)') bez(ix)
          write(9,'(1p,3d23.15)') (fake(1,j), j=1,3)
          write(9,'(1p,3d23.15)') (fake(1,j), j=4,6)
          write(9,'(1p,3d23.15)') (fake(1,j), j=7,9)
          write(9,'(1p,3d23.15)') (fake(1,j), j=10,12)
          write(9,'(1p,3d23.15)') (fake(1,j), j=13,15)
          write(9,'(1p,3d23.15)') (fake(1,j), j=16,18)
          write(9,'(1p,2d23.15)') (fake(1,j), j=19,20)
          write(9,'(1p,3d23.15)') (fake(2,j), j=1,3)
          write(9,'(1p,3d23.15)') (fake(2,j), j=4,6)
          write(9,'(1p,3d23.15)') (fake(2,j), j=7,9)
          write(9,'(1p,3d23.15)') (fake(2,j), j=10,12)
          write(9,'(1p,3d23.15)') (fake(2,j), j=13,15)
          write(9,'(1p,3d23.15)') (fake(2,j), j=16,18)
          write(9,'(1p,2d23.15)') (fake(2,j), j=19,20)
          do 170 j=1,20
            fake(1,j)=zero
  170     fake(2,j)=zero
        endif
        goto 290
  180   kzz=-kzz
        goto(190,200,210,220,230,240,250,260,270,280),kzz
        ktrack(i)=31
        goto 290
  190   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=21
        strack(i)=smiv(1,i)*c1e3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  200   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=22
        strack(i)=smiv(1,i)
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  210   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=23
        strack(i)=smiv(1,i)*c1m3
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  220   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=24
        strack(i)=smiv(1,i)*c1m6
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  230   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=25
        strack(i)=smiv(1,i)*c1m9
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  240   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=26
        strack(i)=smiv(1,i)*c1m12
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  250   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=27
        strack(i)=smiv(1,i)*c1m15
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  260   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=28
        strack(i)=smiv(1,i)*c1m18
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  270   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=29
        strack(i)=smiv(1,i)*c1m21
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
        goto 290
  280   if(abs(smiv(1,i)).le.pieni) then
          ktrack(i)=31
          goto 290
        endif
        ktrack(i)=30
        strack(i)=smiv(1,i)*c1m24
        strackc(i)=strack(i)*tiltc(i)
        stracks(i)=strack(i)*tilts(i)
  290 continue
      do 300 j=1,napx
        dpsv1(j)=dpsv(j)*c1e3/(one+dpsv(j))
  300 continue
      nwri=nwr(3)
      if(nwri.eq.0) nwri=numl+numlr+1
      if(idp.eq.0.or.ition.eq.0) then
        call thck4d(nthinerr)
      else
        hsy(3)=c1m3*hsy(3)*ition
        do 310 jj=1,nele
          if(kz(jj).eq.12) hsyc(jj)=c1m3*hsyc(jj)*itionc(jj)
  310   continue
        if(abs(phas).ge.pieni) then
          call thck6dua(nthinerr)
        else
          call thck6d(nthinerr)
        endif
      endif
      return
      end
      subroutine thck4d(nthinerr)
!-----------------------------------------------------------------------
!
!  TRACK THICK LENS 4D
!
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
      implicit none
      integer i,idz1,idz2,irrtr,ix,j,k,kpz,n,nmz,nthinerr
      double precision cbxb,cbzb,cccc,cikve,cikveb,crkve,crkveb,crkveuk,&
     &crxb,crzb,dpsv3,pux,puxve,puzve,r0,r2b,rb,rho2b,rkb,tkb,xbb,xlvj, &
     &xrb,yv1j,yv2j,zbb,zlvj,zrb
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer ireturn, xory, nac, nfree, nramp1,nplato, nramp2
      double precision e0fo,e0o,xv1j,xv2j
      double precision acdipamp, qd, acphase, acdipamp2,                &
     &acdipamp1
      double precision l,cur,dx,dy,tx,ty,embl,leff,rx,ry,lin,chi,xi,yi
      logical llost
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      double precision cc,xlim,ylim
      parameter(cc = 1.12837916709551d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),    &
     &r2b(npart),rb(npart),rkb(npart),                                  &
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),          &
     &crzb(npart),cbxb(npart),cbzb(npart)
      dimension dpsv3(npart)
      save
!-----------------------------------------------------------------------
      nthinerr=0
      idz1=idz(1)
      idz2=idz(2)
      do 490 n=1,numl
          numx=n-1
          if(irip.eq.1) call ripple(n)
          if(mod(numx,nwri).eq.0) call writebin(nthinerr)
          if(nthinerr.ne.0) return
          do 480 i=1,iu
            if(ktrack(i).eq.1) then
              ix=ic(i)
            else
              ix=ic(i)-nblo
            endif
          if(i.eq.1103) then
          endif
!----------count=43
            goto(20,480,740,480,480,480,480,480,480,480,40,60,80,100,   &
     &120,140,160,180,200,220,270,290,310,330,350,370,390,410,          &
     &430,450,470,240,500,520,540,560,580,600,620,640,680,700           &
     &,720,480,748,480,480,480,480,480,745,746),ktrack(i)
            goto 480
   20       do 30 j=1,napx
              puxve=xv(1,j)
              puzve=yv(1,j)
              xv(1,j)=bl1v(1,1,j,ix)*puxve+bl1v(2,1,j,ix)*puzve+ idz1   &
     &*bl1v(5,1,j,ix)*dpsv(j)*c1e3
              yv(1,j)=bl1v(3,1,j,ix)*puxve+bl1v(4,1,j,ix)*puzve+ idz1   &
     &*bl1v(6,1,j,ix)*dpsv(j)*c1e3
              puxve=xv(2,j)
              puzve=yv(2,j)
              xv(2,j)=bl1v(1,2,j,ix)*puxve+bl1v(2,2,j,ix)*puzve+ idz2   &
     &*bl1v(5,2,j,ix)*dpsv(j)*c1e3
              yv(2,j)=bl1v(3,2,j,ix)*puxve+bl1v(4,2,j,ix)*puzve+ idz2   &
     &*bl1v(6,2,j,ix)*dpsv(j)*c1e3
   30       continue
            goto 480
!--HORIZONTAL DIPOLE
   40       do 50 j=1,napx
            yv(1,j)=yv(1,j)+strackc(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+stracks(i)*oidpsv(j)
   50       continue
            goto 470
!--NORMAL QUADRUPOLE
   60       do 70 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
   70       continue
            goto 470
!--NORMAL SEXTUPOLE
   80       do 90 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
   90       continue
            goto 470
!--NORMAL OCTUPOLE
  100       do 110 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  110       continue
            goto 470
!--NORMAL DECAPOLE
  120       do 130 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  130       continue
            goto 470
!--NORMAL DODECAPOLE
  140       do 150 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  150       continue
            goto 470
!--NORMAL 14-POLE
  160       do 170 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  170       continue
            goto 470
!--NORMAL 16-POLE
  180       do 190 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  190       continue
            goto 470
!--NORMAL 18-POLE
  200       do 210 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  210       continue
            goto 470
!--NORMAL 20-POLE
  220       do 230 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  230       continue
            goto 470
  500     continue
          do 510 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  510     continue
          goto 470
  520     continue
          do 530 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  530     continue
          goto 240
  540     continue
          do 550 j=1,napx
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  550     continue
          goto 470
  560     continue
          do 570 j=1,napx
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
  570     continue
          goto 240
  580     continue
          do 590 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  590     continue
          goto 470
  600     continue
          do 610 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  610     continue
          goto 240
  620     continue
          do 630 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  630     continue
          goto 470
  640     continue
          do 650 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
  650     continue
  240       r0=ek(ix)
            nmz=nmu(ix)
          if(nmz.ge.2) then
            do 260 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
              yv1j=bbiv(1,1,i)+bbiv(2,1,i)*xlvj+aaiv(2,1,i)*zlvj
              yv2j=aaiv(1,1,i)-bbiv(2,1,i)*zlvj+aaiv(2,1,i)*xlvj
              crkve=xlvj
              cikve=zlvj
                do 250 k=3,nmz
                  crkveuk=crkve*xlvj-cikve*zlvj
                  cikve=crkve*zlvj+cikve*xlvj
                  crkve=crkveuk
                  yv1j=yv1j+bbiv(k,1,i)*crkve+aaiv(k,1,i)*cikve
                  yv2j=yv2j-bbiv(k,1,i)*cikve+aaiv(k,1,i)*crkve
  250           continue
              yv(1,j)=yv(1,j)+(tiltc(i)*yv1j-tilts(i)*yv2j)*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*yv2j+tilts(i)*yv1j)*oidpsv(j)
  260       continue
          else
            do 265 j=1,napx
              yv(1,j)=yv(1,j)+(tiltc(i)*bbiv(1,1,i)-                    &
     &tilts(i)*aaiv(1,1,i))*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*aaiv(1,1,i)+                    &
     &tilts(i)*bbiv(1,1,i))*oidpsv(j)
  265       continue
          endif
            goto 470
!--SKEW ELEMENTS
!--VERTICAL DIPOLE
  270       do 280 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+strackc(i)*oidpsv(j)
  280       continue
            goto 470
!--SKEW QUADRUPOLE
  290       do 300 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  300       continue
            goto 470
!--SKEW SEXTUPOLE
  310       do 320 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  320       continue
            goto 470
!--SKEW OCTUPOLE
  330       do 340 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  340       continue
            goto 470
!--SKEW DECAPOLE
  350       do 360 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  360       continue
            goto 470
!--SKEW DODECAPOLE
  370       do 380 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  380       continue
            goto 470
!--SKEW 14-POLE
  390       do 400 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  400       continue
            goto 470
!--SKEW 16-POLE
  410       do 420 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  420       continue
            goto 470
!--SKEW 18-POLE
  430       do 440 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  440       continue
            goto 470
!--SKEW 20-POLE
  450       do 460 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  460       continue
          goto 470
  680     continue
          do 690 j=1,napx
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)                                       &
     &goto 690
            tkb(j)=rho2b(j)/(two*sigman2(1,imbb(i)))
            if(ibbc.eq.0) then
              yv(1,j)=yv(1,j)+oidpsv(j)*(strack(i)*crkveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))
              yv(2,j)=yv(2,j)+oidpsv(j)*(strack(i)*cikveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))
            else
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),11)-          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
              yv(1,j)=yv(1,j)+oidpsv(j)*cccc
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),12)+          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
              yv(2,j)=yv(2,j)+oidpsv(j)*cccc
            endif
  690     continue
          goto 470
  700     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 470
  720     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 470
  740     continue
          irrtr=imtr(ix)
          do j=1,napx
            pux=xv(1,j)
            dpsv3(j)=dpsv(j)*c1e3
            xv(1,j)=cotr(irrtr,1)+rrtr(irrtr,1,1)*pux+                  &
     &rrtr(irrtr,1,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,1,6)
            yv(1,j)=cotr(irrtr,2)+rrtr(irrtr,2,1)*pux+                  &
     &rrtr(irrtr,2,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,2,6)
            pux=xv(2,j)
            xv(2,j)=cotr(irrtr,3)+rrtr(irrtr,3,3)*pux+                  &
     &rrtr(irrtr,3,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,3,6)
            yv(2,j)=cotr(irrtr,4)+rrtr(irrtr,4,3)*pux+                  &
     &rrtr(irrtr,4,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,4,6)
          enddo
 
!----------------------------------------------------------------------
 
! Wire.
 
          goto 470
  745     continue
          xory=1
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 470
  746     continue
          xory=2
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 470
 
!----------------------------
 
! Wire.
 
  748     continue
!     magnetic rigidity
      chi = sqrt(e0*e0-pmap*pmap)*c1e6/clight
 
      ix = ixcav
      tx = xrms(ix)
      ty = zrms(ix)
      dx = xpl(ix)
      dy = zpl(ix)
      embl = ek(ix)
      l = wirel(ix)
      cur = ed(ix)
 
      leff = embl/cos(tx)/cos(ty)
      rx = dx *cos(tx)-embl*sin(tx)/2
      lin= dx *sin(tx)+embl*cos(tx)/2
      ry = dy *cos(ty)-lin *sin(ty)
      lin= lin*cos(ty)+dy  *sin(ty)
 
      do 750 j=1, napx
 
      xv(1,j) = xv(1,j) * c1m3
      xv(2,j) = xv(2,j) * c1m3
      yv(1,j) = yv(1,j) * c1m3
      yv(2,j) = yv(2,j) * c1m3
 
!      print *, 'Start: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
!     call tilt(tx,ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(tx)*yv(2,j)/sqrt((1+dpsv(j))**2-    &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-tx)
      xv(1,j) = xv(1,j)*(cos(tx)-sin(tx)*tan(atan(yv(1,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(ty)*yv(1,j)/sqrt((1+dpsv(j))**2-    &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-ty)
      xv(2,j) = xv(2,j)*(cos(ty)-sin(ty)*tan(atan(yv(2,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty)
 
!     call drift(lin)
 
      xv(1,j) = xv(1,j) + lin*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) + lin*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
 
!      call kick(l,cur,lin,rx,ry,chi)
 
      xi = xv(1,j)-rx
      yi = xv(2,j)-ry
      yv(1,j) = yv(1,j)-1.0d-7*cur/chi*xi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
!GRD FOR CONSISTENSY
!      yv(2,j) = yv(2,j)-1e-7*cur/chi*yi/(xi**2+yi**2)*                  &
      yv(2,j) = yv(2,j)-1.0d-7*cur/chi*yi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
 
!     call drift(leff-lin)
 
      xv(1,j) = xv(1,j) + (leff-lin)*yv(1,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
      xv(2,j) = xv(2,j) + (leff-lin)*yv(2,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
 
!     call invtilt(tx,ty)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(-ty)*yv(1,j)/sqrt((1+dpsv(j))**2-   &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+ty)
      xv(2,j) = xv(2,j)*(cos(-ty)-sin(-ty)*tan(atan(yv(2,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(-tx)*yv(2,j)/sqrt((1+dpsv(j))**2-   &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+tx)
      xv(1,j) = xv(1,j)*(cos(-tx)-sin(-tx)*tan(atan(yv(1,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx)
 
!     call shift(-embl*tan(tx),-embl*tan(ty)/cos(tx))
 
      xv(1,j) = xv(1,j) + embl*tan(tx)
      xv(2,j) = xv(2,j) + embl*tan(ty)/cos(tx)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
      xv(1,j) = xv(1,j) * c1e3
      xv(2,j) = xv(2,j) * c1e3
      yv(1,j) = yv(1,j) * c1e3
      yv(2,j) = yv(2,j) * c1e3
 
!      print *, 'End: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!-----------------------------------------------------------------------
 
  750     continue
          goto 470
 
!----------------------------
 
  470       continue
          llost=.false.
          do j=1,napx
             llost=llost.or.                                            &
     &abs(xv(1,j)).gt.aper(1).or.abs(xv(2,j)).gt.aper(2)
          enddo
          if (llost) then
             kpz=abs(kp(ix))
             if(kpz.eq.2) then
                call lostpar3(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             elseif(kpz.eq.3) then
                call lostpar4(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             else
                call lostpar2(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             endif
          endif
  480     continue
          call lostpart(nthinerr)
          if(nthinerr.ne.0) return
          if(ntwin.ne.2) call dist1
          if(mod(n,nwr(4)).eq.0) call write6(n)
  490 continue
      return
      end
      subroutine thck6d(nthinerr)
!-----------------------------------------------------------------------
!
!  TRACK THICK LENS 6D
!
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
      implicit none
      integer i,idz1,idz2,irrtr,ix,j,jb,jmel,jx,k,kpz,n,nmz,nthinerr
      double precision cbxb,cbzb,cccc,cikve,cikveb,crkve,crkveb,crkveuk,&
     &crxb,crzb,dpsv3,pux,puxve1,puxve2,puzve1,puzve2,r0,r2b,rb,rho2b,  &
     &rkb,tkb,xbb,xlvj,xrb,yv1j,yv2j,zbb,zlvj,zrb
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer ireturn, xory, nac, nfree, nramp1,nplato, nramp2
      double precision e0fo,e0o,xv1j,xv2j
      double precision acdipamp, qd, acphase,acdipamp2,                 &
     &acdipamp1
      double precision l,cur,dx,dy,tx,ty,embl,leff,rx,ry,lin,chi,xi,yi
      logical llost
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      double precision cc,xlim,ylim
      parameter(cc = 1.12837916709551d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),    &
     &r2b(npart),rb(npart),rkb(npart),                                  &
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),          &
     &crzb(npart),cbxb(npart),cbzb(npart)
      dimension dpsv3(npart)
      save
!-----------------------------------------------------------------------
      nthinerr=0
      idz1=idz(1)
      idz2=idz(2)
      do 510 n=1,numl
          numx=n-1
          if(irip.eq.1) call ripple(n)
          if(mod(numx,nwri).eq.0) call writebin(nthinerr)
          if(nthinerr.ne.0) return
          do 500 i=1,iu
            if(ktrack(i).eq.1) then
              ix=ic(i)
            else
              ix=ic(i)-nblo
            endif
!----------count 44
            goto(20,40,740,500,500,500,500,500,500,500,60,80,100,120,   &
     &140,160,180,200,220,240,290,310,330,350,370,390,410,430,          &
     &450,470,490,260,520,540,560,580,600,620,640,660,680,700,720       &
     &,730,748,500,500,500,500,500,745,746),ktrack(i)
            goto 500
   20       jmel=mel(ix)
            do 30 jb=1,jmel
              jx=mtyp(ix,jb)
              do 30 j=1,napx
                puxve1=xv(1,j)
                puzve1=yv(1,j)
                puxve2=xv(2,j)
                puzve2=yv(2,j)
                sigmv(j)=sigmv(j)+as(1,1,j,jx)+puxve1*(as(2,1,j,jx)+ as &
     &(4,1,j,jx)*puzve1+as(5,1,j,jx)*puxve1)+ puzve1*(as                &
     &(3,1,j,jx)+as(6,1,j,jx)*puzve1)                                   &
     &+as(1,2,j,jx)+puxve2*(as(2,2,j,jx)+ as                            &
     &(4,2,j,jx)*puzve2+as(5,2,j,jx)*puxve2)+ puzve2*(as                &
     &(3,2,j,jx)+as(6,2,j,jx)*puzve2)
                xv(1,j)=al(1,1,j,jx)*puxve1+ al(2,1,j,jx)*puzve1+idz1*al&
     &(5,1,j,jx)
                xv(2,j)=al(1,2,j,jx)*puxve2+ al(2,2,j,jx)*puzve2+idz2*al&
     &(5,2,j,jx)
                yv(1,j)=al(3,1,j,jx)*puxve1+ al(4,1,j,jx)*puzve1+idz1*al&
     &(6,1,j,jx)
                yv(2,j)=al(3,2,j,jx)*puxve2+ al(4,2,j,jx)*puzve2+idz2*al&
     &(6,2,j,jx)
   30       continue
            goto 500
   40       do 50 j=1,napx
              ejf0v(j)=ejfv(j)
              if(abs(dppoff).gt.pieni) sigmv(j)=sigmv(j)-sigmoff(i)
              if(kz(ix).eq.12) then
                ejv(j)=ejv(j)+ed(ix)*sin(hsyc(ix)*sigmv(j)+             &
     &phasc(ix))
              else
                ejv(j)=ejv(j)+hsy(1)*sin(hsy(3)*sigmv(j))
              endif
              ejfv(j)=sqrt(ejv(j)*ejv(j)-pma*pma)
              rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
              dpsv(j)=(ejfv(j)-e0f)/e0f
              oidpsv(j)=one/(one+dpsv(j))
              dpsv1(j)=dpsv(j)*c1e3*oidpsv(j)
              yv(1,j)=ejf0v(j)/ejfv(j)*yv(1,j)
   50       yv(2,j)=ejf0v(j)/ejfv(j)*yv(2,j)
            if(n.eq.1) write(98,'(1p,6(2x,e25.18))')                    &
     &(xv(1,j),yv(1,j),xv(2,j),yv(2,j),sigmv(j),dpsv(j),                &
     &j=1,napx)
            call synuthck
            goto 490
!--HORIZONTAL DIPOLE
   60       do 70 j=1,napx
            yv(1,j)=yv(1,j)+strackc(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+stracks(i)*oidpsv(j)
   70       continue
            goto 490
!--NORMAL QUADRUPOLE
   80       do 90 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
   90       continue
            goto 490
!--NORMAL SEXTUPOLE
  100       do 110 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  110       continue
            goto 490
!--NORMAL OCTUPOLE
  120       do 130 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  130       continue
            goto 490
!--NORMAL DECAPOLE
  140       do 150 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  150       continue
            goto 490
!--NORMAL DODECAPOLE
  160       do 170 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  170       continue
            goto 490
!--NORMAL 14-POLE
  180       do 190 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  190       continue
            goto 490
!--NORMAL 16-POLE
  200       do 210 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  210       continue
            goto 490
!--NORMAL 18-POLE
  220       do 230 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  230       continue
            goto 490
!--NORMAL 20-POLE
  240       do 250 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  250       continue
            goto 490
  520       continue
            do 530 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  530       continue
            goto 490
  540       continue
            do 550 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  550       continue
            goto 260
  560       continue
            do 570 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  570       continue
            goto 490
  580       continue
            do 590 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  590       continue
            goto 260
  600       continue
            do 610 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  610       continue
            goto 490
  620       continue
            do 630 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  630       continue
            goto 260
  640       continue
            do 650 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  650       continue
            goto 490
  660       continue
            do 670 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  670       continue
  260       r0=ek(ix)
            nmz=nmu(ix)
          if(nmz.ge.2) then
            do 280 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
              yv1j=bbiv(1,1,i)+bbiv(2,1,i)*xlvj+aaiv(2,1,i)*zlvj
              yv2j=aaiv(1,1,i)-bbiv(2,1,i)*zlvj+aaiv(2,1,i)*xlvj
              crkve=xlvj
              cikve=zlvj
                do 270 k=3,nmz
                  crkveuk=crkve*xlvj-cikve*zlvj
                  cikve=crkve*zlvj+cikve*xlvj
                  crkve=crkveuk
                  yv1j=yv1j+bbiv(k,1,i)*crkve+aaiv(k,1,i)*cikve
                  yv2j=yv2j-bbiv(k,1,i)*cikve+aaiv(k,1,i)*crkve
  270           continue
              yv(1,j)=yv(1,j)+(tiltc(i)*yv1j-tilts(i)*yv2j)*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*yv2j+tilts(i)*yv1j)*oidpsv(j)
  280       continue
          else
            do 275 j=1,napx
              yv(1,j)=yv(1,j)+(tiltc(i)*bbiv(1,1,i)-                    &
     &tilts(i)*aaiv(1,1,i))*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*aaiv(1,1,i)+                    &
     &tilts(i)*bbiv(1,1,i))*oidpsv(j)
  275       continue
          endif
            goto 490
!--SKEW ELEMENTS
!--VERTICAL DIPOLE
  290       do 300 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+strackc(i)*oidpsv(j)
  300       continue
            goto 490
!--SKEW QUADRUPOLE
  310       do 320 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  320       continue
            goto 490
!--SKEW SEXTUPOLE
  330       do 340 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  340       continue
            goto 490
!--SKEW OCTUPOLE
  350       do 360 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  360       continue
            goto 490
!--SKEW DECAPOLE
  370       do 380 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  380       continue
            goto 490
!--SKEW DODECAPOLE
  390       do 400 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  400       continue
            goto 490
!--SKEW 14-POLE
  410       do 420 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  420       continue
            goto 490
!--SKEW 16-POLE
  430       do 440 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  440       continue
            goto 490
!--SKEW 18-POLE
  450       do 460 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  460       continue
            goto 490
!--SKEW 20-POLE
  470       do 480 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  480       continue
          goto 490
  680     continue
          do 690 j=1,napx
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)                                       &
     &goto 690
            tkb(j)=rho2b(j)/(two*sigman2(1,imbb(i)))
            if(ibbc.eq.0) then
              yv(1,j)=yv(1,j)+oidpsv(j)*(strack(i)*crkveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))
              yv(2,j)=yv(2,j)+oidpsv(j)*(strack(i)*cikveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))
            else
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),11)-          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
              yv(1,j)=yv(1,j)+oidpsv(j)*cccc
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),12)+          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
              yv(2,j)=yv(2,j)+oidpsv(j)*cccc
            endif
  690     continue
          goto 490
  700     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 490
  720     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 490
  730     continue
!--Hirata's 6D beam-beam kick
            do j=1,napx
              track6d(1,j)=(xv(1,j)+ed(ix)-clobeam(1,imbb(i)))*c1m3
              track6d(2,j)=(yv(1,j)/oidpsv(j)-clobeam(4,imbb(i)))*c1m3
              track6d(3,j)=(xv(2,j)+ek(ix)-clobeam(2,imbb(i)))*c1m3
              track6d(4,j)=(yv(2,j)/oidpsv(j)-clobeam(5,imbb(i)))*c1m3
              track6d(5,j)=(sigmv(j)-clobeam(3,imbb(i)))*c1m3
              track6d(6,j)=dpsv(j)-clobeam(6,imbb(i))
            enddo
            call beamint(napx,track6d,parbe,sigz,bbcu,imbb(i),ix,ibtyp, &
     &ibbc)
            do j=1,napx
              xv(1,j)=track6d(1,j)*c1e3+clobeam(1,imbb(i))-             &
     &beamoff(1,imbb(i))
              xv(2,j)=track6d(3,j)*c1e3+clobeam(2,imbb(i))-             &
     &beamoff(2,imbb(i))
              dpsv(j)=track6d(6,j)+clobeam(6,imbb(i))-beamoff(6,imbb(i))
              oidpsv(j)=one/(one+dpsv(j))
              yv(1,j)=(track6d(2,j)*c1e3+clobeam(4,imbb(i))-            &
     &beamoff(4,imbb(i)))*oidpsv(j)
              yv(2,j)=(track6d(4,j)*c1e3+clobeam(5,imbb(i))-            &
     &beamoff(5,imbb(i)))*oidpsv(j)
              ejfv(j)=dpsv(j)*e0f+e0f
              ejv(j)=sqrt(ejfv(j)*ejfv(j)+pma*pma)
              rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
              if(ithick.eq.1) call envarsv(dpsv,oidpsv,rvv,ekv)
            enddo
          goto 490
  740     continue
          irrtr=imtr(ix)
          do j=1,napx
            sigmv(j)=sigmv(j)+cotr(irrtr,5)+rrtr(irrtr,5,1)*xv(1,j)+    &
     &rrtr(irrtr,5,2)*yv(1,j)+rrtr(irrtr,5,3)*xv(2,j)+                  &
     &rrtr(irrtr,5,4)*yv(2,j)
            pux=xv(1,j)
            dpsv3(j)=dpsv(j)*c1e3
            xv(1,j)=cotr(irrtr,1)+rrtr(irrtr,1,1)*pux+                  &
     &rrtr(irrtr,1,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,1,6)
            yv(1,j)=cotr(irrtr,2)+rrtr(irrtr,2,1)*pux+                  &
     &rrtr(irrtr,2,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,2,6)
            pux=xv(2,j)
            xv(2,j)=cotr(irrtr,3)+rrtr(irrtr,3,3)*pux+                  &
     &rrtr(irrtr,3,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,3,6)
            yv(2,j)=cotr(irrtr,4)+rrtr(irrtr,4,3)*pux+                  &
     &rrtr(irrtr,4,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,4,6)
          enddo
 
!----------------------------------------------------------------------
 
! Wire.
 
          goto 490
  745     continue
          xory=1
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 490
  746     continue
          xory=2
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 490
 
!----------------------------
 
! Wire.
 
  748     continue
!     magnetic rigidity
      chi = sqrt(e0*e0-pmap*pmap)*c1e6/clight
 
      ix = ixcav
      tx = xrms(ix)
      ty = zrms(ix)
      dx = xpl(ix)
      dy = zpl(ix)
      embl = ek(ix)
      l = wirel(ix)
      cur = ed(ix)
 
      leff = embl/cos(tx)/cos(ty)
      rx = dx *cos(tx)-embl*sin(tx)/2
      lin= dx *sin(tx)+embl*cos(tx)/2
      ry = dy *cos(ty)-lin *sin(ty)
      lin= lin*cos(ty)+dy  *sin(ty)
 
      do 750 j=1, napx
 
      xv(1,j) = xv(1,j) * c1m3
      xv(2,j) = xv(2,j) * c1m3
      yv(1,j) = yv(1,j) * c1m3
      yv(2,j) = yv(2,j) * c1m3
 
!      print *, 'Start: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
!     call tilt(tx,ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(tx)*yv(2,j)/sqrt((1+dpsv(j))**2-    &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-tx)
      xv(1,j) = xv(1,j)*(cos(tx)-sin(tx)*tan(atan(yv(1,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(ty)*yv(1,j)/sqrt((1+dpsv(j))**2-    &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-ty)
      xv(2,j) = xv(2,j)*(cos(ty)-sin(ty)*tan(atan(yv(2,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty)
 
!     call drift(lin)
 
      xv(1,j) = xv(1,j) + lin*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) + lin*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
 
!      call kick(l,cur,lin,rx,ry,chi)
 
      xi = xv(1,j)-rx
      yi = xv(2,j)-ry
      yv(1,j) = yv(1,j)-1.0d-7*cur/chi*xi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
!GRD FOR CONSISTENSY
!      yv(2,j) = yv(2,j)-1e-7*cur/chi*yi/(xi**2+yi**2)*                  &
      yv(2,j) = yv(2,j)-1.0d-7*cur/chi*yi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
 
!     call drift(leff-lin)
 
      xv(1,j) = xv(1,j) + (leff-lin)*yv(1,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
      xv(2,j) = xv(2,j) + (leff-lin)*yv(2,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
 
!     call invtilt(tx,ty)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(-ty)*yv(1,j)/sqrt((1+dpsv(j))**2-   &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+ty)
      xv(2,j) = xv(2,j)*(cos(-ty)-sin(-ty)*tan(atan(yv(2,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(-tx)*yv(2,j)/sqrt((1+dpsv(j))**2-   &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+tx)
      xv(1,j) = xv(1,j)*(cos(-tx)-sin(-tx)*tan(atan(yv(1,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx)
 
!     call shift(-embl*tan(tx),-embl*tan(ty)/cos(tx))
 
      xv(1,j) = xv(1,j) + embl*tan(tx)
      xv(2,j) = xv(2,j) + embl*tan(ty)/cos(tx)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
      xv(1,j) = xv(1,j) * c1e3
      xv(2,j) = xv(2,j) * c1e3
      yv(1,j) = yv(1,j) * c1e3
      yv(2,j) = yv(2,j) * c1e3
 
!      print *, 'End: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!-----------------------------------------------------------------------
 
  750     continue
          goto 490
 
!----------------------------
 
  490       continue
          llost=.false.
          do j=1,napx
             llost=llost.or.                                            &
     &abs(xv(1,j)).gt.aper(1).or.abs(xv(2,j)).gt.aper(2)
          enddo
          if (llost) then
             kpz=abs(kp(ix))
             if(kpz.eq.2) then
                call lostpar3(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             elseif(kpz.eq.3) then
                call lostpar4(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             else
                call lostpar2(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             endif
          endif
  500     continue
          call lostpart(nthinerr)
          if(nthinerr.ne.0) return
          if(ntwin.ne.2) call dist1
          if(mod(n,nwr(4)).eq.0) call write6(n)
  510 continue
      return
      end
      subroutine thck6dua(nthinerr)
!-----------------------------------------------------------------------
!
!  TRACK THICK LENS  6D WITH ACCELERATION
!
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
      implicit none
      integer i,idz1,idz2,irrtr,ix,j,jb,jmel,jx,k,kpz,n,nmz,nthinerr
      double precision cbxb,cbzb,cccc,cikve,cikveb,crkve,crkveb,crkveuk,&
     &crxb,crzb,dpsv3,e0fo,e0o,pux,puxve1,puxve2,puzve1,puzve2,r0,r2b,  &
     &rb,rho2b,rkb,tkb,xbb,xlvj,xrb,yv1j,yv2j,zbb,zlvj,zrb
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer ireturn, xory, nac, nfree, nramp1,nplato, nramp2
      double precision xv1j,xv2j
      double precision acdipamp, qd, acphase,acdipamp2,                 &
     &acdipamp1
      double precision l,cur,dx,dy,tx,ty,embl,leff,rx,ry,lin,chi,xi,yi
      logical llost
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      double precision cc,xlim,ylim
      parameter(cc = 1.12837916709551d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension crkveb(npart),cikveb(npart),rho2b(npart),tkb(npart),    &
     &r2b(npart),rb(npart),rkb(npart),                                  &
     &xrb(npart),zrb(npart),xbb(npart),zbb(npart),crxb(npart),          &
     &crzb(npart),cbxb(npart),cbzb(npart)
      dimension dpsv3(npart)
      save
!-----------------------------------------------------------------------
      nthinerr=0
      idz1=idz(1)
      idz2=idz(2)
      do 510 n=1,numl
          numx=n-1
          if(irip.eq.1) call ripple(n)
          if(n.le.nde(1)) nwri=nwr(1)
          if(n.gt.nde(1).and.n.le.nde(2)) nwri=nwr(2)
          if(n.gt.nde(2)) nwri=nwr(3)
          if(nwri.eq.0) nwri=numl+numlr+1
          if(mod(numx,nwri).eq.0) call writebin(nthinerr)
          if(nthinerr.ne.0) return
          do 500 i=1,iu
            if(ktrack(i).eq.1) then
              ix=ic(i)
            else
              ix=ic(i)-nblo
            endif
!----------count 44
            goto(20,40,740,500,500,500,500,500,500,500,60,80,100,120,   &
     &140,160,180,200,220,240,290,310,330,350,370,390,410,430,          &
     &450,470,490,260,520,540,560,580,600,620,640,660,680,700,720       &
     &,730,748,500,500,500,500,500,745,746),ktrack(i)
            goto 500
   20       jmel=mel(ix)
            do 30 jb=1,jmel
              jx=mtyp(ix,jb)
              do 30 j=1,napx
                puxve1=xv(1,j)
                puzve1=yv(1,j)
                puxve2=xv(2,j)
                puzve2=yv(2,j)
                sigmv(j)=sigmv(j)+as(1,1,j,jx)+puxve1*(as(2,1,j,jx)+ as &
     &(4,1,j,jx)*puzve1+as(5,1,j,jx)*puxve1)+ puzve1*(as                &
     &(3,1,j,jx)+as(6,1,j,jx)*puzve1)                                   &
     &+as(1,2,j,jx)+puxve2*(as(2,2,j,jx)+ as                            &
     &(4,2,j,jx)*puzve2+as(5,2,j,jx)*puxve2)+ puzve2*(as                &
     &(3,2,j,jx)+as(6,2,j,jx)*puzve2)
                xv(1,j)=al(1,1,j,jx)*puxve1+ al(2,1,j,jx)*puzve1+idz1*al&
     &(5,1,j,jx)
                xv(2,j)=al(1,2,j,jx)*puxve2+ al(2,2,j,jx)*puzve2+idz2*al&
     &(5,2,j,jx)
                yv(1,j)=al(3,1,j,jx)*puxve1+ al(4,1,j,jx)*puzve1+idz1*al&
     &(6,1,j,jx)
                yv(2,j)=al(3,2,j,jx)*puxve2+ al(4,2,j,jx)*puzve2+idz2*al&
     &(6,2,j,jx)
   30       continue
            goto 500
   40       e0o=e0
            e0fo=e0f
            call adia(n,e0f)
            do 50 j=1,napx
              ejf0v(j)=ejfv(j)
              if(abs(dppoff).gt.pieni) sigmv(j)=sigmv(j)-sigmoff(i)
              if(sigmv(j).lt.zero) sigmv(j)=e0f*e0o/(e0fo*e0)*sigmv(j)
              if(kz(ix).eq.12) then
                ejv(j)=ejv(j)+ed(ix)*sin(hsyc(ix)*sigmv(j)+phas+        &
     &phasc(ix))
              else
                ejv(j)=ejv(j)+hsy(1)*sin(hsy(3)*sigmv(j)+phas)
              endif
              ejfv(j)=sqrt(ejv(j)*ejv(j)-pma*pma)
              rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
              dpsv(j)=(ejfv(j)-e0f)/e0f
              oidpsv(j)=one/(one+dpsv(j))
              dpsv1(j)=dpsv(j)*c1e3*oidpsv(j)
              if(sigmv(j).gt.zero) sigmv(j)=e0f*e0o/(e0fo*e0)*sigmv(j)
              yv(1,j)=ejf0v(j)/ejfv(j)*yv(1,j)
   50       yv(2,j)=ejf0v(j)/ejfv(j)*yv(2,j)
            if(n.eq.1) write(98,'(1p,6(2x,e25.18))')                    &
     &(xv(1,j),yv(1,j),xv(2,j),yv(2,j),sigmv(j),dpsv(j),                &
     &j=1,napx)
            call synuthck
            goto 490
!--HORIZONTAL DIPOLE
   60       do 70 j=1,napx
            yv(1,j)=yv(1,j)+strackc(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+stracks(i)*oidpsv(j)
   70       continue
            goto 490
!--NORMAL QUADRUPOLE
   80       do 90 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
   90       continue
            goto 490
!--NORMAL SEXTUPOLE
  100       do 110 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  110       continue
            goto 490
!--NORMAL OCTUPOLE
  120       do 130 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  130       continue
            goto 490
!--NORMAL DECAPOLE
  140       do 150 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  150       continue
            goto 490
!--NORMAL DODECAPOLE
  160       do 170 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  170       continue
            goto 490
!--NORMAL 14-POLE
  180       do 190 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  190       continue
            goto 490
!--NORMAL 16-POLE
  200       do 210 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  210       continue
            goto 490
!--NORMAL 18-POLE
  220       do 230 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  230       continue
            goto 490
!--NORMAL 20-POLE
  240       do 250 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(-strackc(i)*cikve+               &
     &stracks(i)*crkve)
  250       continue
            goto 490
  520       continue
            do 530 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  530       continue
            goto 490
  540       continue
            do 550 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tiltc(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-(strack(i)*xlvj*oidpsv(j)                   &
     &+dpsv1(j))*dki(ix,1)*tilts(i)                                     &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  550       continue
            goto 260
  560       continue
            do 570 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  570       continue
            goto 490
  580       continue
            do 590 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-strackc(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*(one-tiltc(i))
            yv(2,j)=yv(2,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,1)*oidpsv(j)*tilts(i)
            sigmv(j)=sigmv(j)+rvv(j)*dki(ix,1)*xlvj
  590       continue
            goto 260
  600       continue
            do 610 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  610       continue
            goto 490
  620       continue
            do 630 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)+(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tilts(i)                                     &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)-(strack(i)*zlvj*oidpsv(j)                   &
     &-dpsv1(j))*dki(ix,2)*tiltc(i)                                     &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  630       continue
            goto 260
  640       continue
            do 650 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  650       continue
            goto 490
  660       continue
            do 670 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            yv(1,j)=yv(1,j)-stracks(i)*dpsv1(j)                         &
     &+c1e3*dki(ix,2)*oidpsv(j)*tilts(i)
            yv(2,j)=yv(2,j)+strackc(i)*dpsv1(j)                         &
     &-c1e3*dki(ix,2)*oidpsv(j)*(one-tiltc(i))
            sigmv(j)=sigmv(j)-rvv(j)*dki(ix,2)*zlvj
  670       continue
  260       r0=ek(ix)
            nmz=nmu(ix)
          if(nmz.ge.2) then
            do 280 j=1,napx
            xlvj=(xv(1,j)-xsiv(1,i))*tiltc(i)+                          &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlvj=-(xv(1,j)-xsiv(1,i))*tilts(i)+                         &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
              yv1j=bbiv(1,1,i)+bbiv(2,1,i)*xlvj+aaiv(2,1,i)*zlvj
              yv2j=aaiv(1,1,i)-bbiv(2,1,i)*zlvj+aaiv(2,1,i)*xlvj
              crkve=xlvj
              cikve=zlvj
                do 270 k=3,nmz
                  crkveuk=crkve*xlvj-cikve*zlvj
                  cikve=crkve*zlvj+cikve*xlvj
                  crkve=crkveuk
                  yv1j=yv1j+bbiv(k,1,i)*crkve+aaiv(k,1,i)*cikve
                  yv2j=yv2j-bbiv(k,1,i)*cikve+aaiv(k,1,i)*crkve
  270           continue
              yv(1,j)=yv(1,j)+(tiltc(i)*yv1j-tilts(i)*yv2j)*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*yv2j+tilts(i)*yv1j)*oidpsv(j)
  280       continue
          else
            do 275 j=1,napx
              yv(1,j)=yv(1,j)+(tiltc(i)*bbiv(1,1,i)-                    &
     &tilts(i)*aaiv(1,1,i))*oidpsv(j)
              yv(2,j)=yv(2,j)+(tiltc(i)*aaiv(1,1,i)+                    &
     &tilts(i)*bbiv(1,1,i))*oidpsv(j)
  275       continue
          endif
            goto 490
!--SKEW ELEMENTS
!--VERTICAL DIPOLE
  290       do 300 j=1,napx
            yv(1,j)=yv(1,j)-stracks(i)*oidpsv(j)
            yv(2,j)=yv(2,j)+strackc(i)*oidpsv(j)
  300       continue
            goto 490
!--SKEW QUADRUPOLE
  310       do 320 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  320       continue
            goto 490
!--SKEW SEXTUPOLE
  330       do 340 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  340       continue
            goto 490
!--SKEW OCTUPOLE
  350       do 360 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  360       continue
            goto 490
!--SKEW DECAPOLE
  370       do 380 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  380       continue
            goto 490
!--SKEW DODECAPOLE
  390       do 400 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  400       continue
            goto 490
!--SKEW 14-POLE
  410       do 420 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  420       continue
            goto 490
!--SKEW 16-POLE
  430       do 440 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  440       continue
            goto 490
!--SKEW 18-POLE
  450       do 460 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  460       continue
            goto 490
!--SKEW 20-POLE
  470       do 480 j=1,napx
            xlv(j)=(xv(1,j)-xsiv(1,i))*tiltc(i)+                        &
     &(xv(2,j)-zsiv(1,i))*tilts(i)
            zlv(j)=-(xv(1,j)-xsiv(1,i))*tilts(i)+                       &
     &(xv(2,j)-zsiv(1,i))*tiltc(i)
            crkve=xlv(j)
            cikve=zlv(j)
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
           crkveuk=crkve*xlv(j)-cikve*zlv(j)
           cikve=crkve*zlv(j)+cikve*xlv(j)
           crkve=crkveuk
            yv(1,j)=yv(1,j)+oidpsv(j)*(strackc(i)*cikve-                &
     &stracks(i)*crkve)
            yv(2,j)=yv(2,j)+oidpsv(j)*(strackc(i)*crkve+                &
     &stracks(i)*cikve)
  480       continue
          goto 490
  680     continue
          do 690 j=1,napx
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
            rho2b(j)=crkveb(j)*crkveb(j)+cikveb(j)*cikveb(j)
            if(rho2b(j).le.pieni)                                       &
     &goto 690
            tkb(j)=rho2b(j)/(two*sigman2(1,imbb(i)))
            if(ibbc.eq.0) then
              yv(1,j)=yv(1,j)+oidpsv(j)*(strack(i)*crkveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))
              yv(2,j)=yv(2,j)+oidpsv(j)*(strack(i)*cikveb(j)/rho2b(j)*  &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))
            else
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),11)-          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
              yv(1,j)=yv(1,j)+oidpsv(j)*cccc
              cccc=(strack(i)*crkveb(j)/rho2b(j)*                       &
     &(one-exp(-tkb(j)))-beamoff(4,imbb(i)))*bbcu(imbb(i),12)+          &
     &(strack(i)*cikveb(j)/rho2b(j)*                                    &
     &(one-exp(-tkb(j)))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
              yv(2,j)=yv(2,j)+oidpsv(j)*cccc
            endif
  690     continue
          goto 490
  700     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(xrb(j),zrb(j),crxb(j),crzb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(xbb(j),zbb(j),cbxb(j),cbzb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(1,imbb(i))-sigman2(2,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,xrb(1),zrb(1),crxb(1),crzb(1))
            call wzsubv(napx,xbb(1),zbb(1),cbxb(1),cbzb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 490
  720     continue
          if(ibtyp.eq.0) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              call errf(zrb(j),xrb(j),crzb(j),crxb(j))
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
              call errf(zbb(j),xbb(j),cbzb(j),cbxb(j))
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          else if(ibtyp.eq.1) then
            do j=1,napx
              r2b(j)=two*(sigman2(2,imbb(i))-sigman2(1,imbb(i)))
              rb(j)=sqrt(r2b(j))
              rkb(j)=strack(i)*pisqrt/rb(j)
              if(ibbc.eq.0) then
                crkveb(j)=xv(1,j)-clobeam(1,imbb(i))+ed(ix)
                cikveb(j)=xv(2,j)-clobeam(2,imbb(i))+ek(ix)
              else
                crkveb(j)=                                              &
     &(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),11)+             &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),12)
                cikveb(j)=                                              &
     &-(xv(1,j)-clobeam(1,imbb(i))+ed(ix))*bbcu(imbb(i),12)+            &
     &(xv(2,j)-clobeam(2,imbb(i))+ek(ix))*bbcu(imbb(i),11)
              endif
              xrb(j)=abs(crkveb(j))/rb(j)
              zrb(j)=abs(cikveb(j))/rb(j)
              tkb(j)=(crkveb(j)*crkveb(j)/sigman2(1,imbb(i))+           &
     &cikveb(j)*cikveb(j)/sigman2(2,imbb(i)))*half
              xbb(j)=sigmanq(2,imbb(i))*xrb(j)
              zbb(j)=sigmanq(1,imbb(i))*zrb(j)
            enddo
            call wzsubv(napx,zrb(1),xrb(1),crzb(1),crxb(1))
            call wzsubv(napx,zbb(1),xbb(1),cbzb(1),cbxb(1))
            do j=1,napx
              if(ibbc.eq.0) then
                yv(1,j)=yv(1,j)+oidpsv(j)*(rkb(j)*(crzb(j)-exp(-tkb(j))*&
     &cbzb(j))*sign(one,crkveb(j))-beamoff(4,imbb(i)))
                yv(2,j)=yv(2,j)+oidpsv(j)*(rkb(j)*(crxb(j)-exp(-tkb(j))*&
     &cbxb(j))*sign(one,cikveb(j))-beamoff(5,imbb(i)))
              else
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),11)-(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),12)
                yv(1,j)=yv(1,j)+oidpsv(j)*cccc
                cccc=(rkb(j)*(crzb(j)-exp(-tkb(j))*cbzb(j))*            &
     &sign(one,crkveb(j))-beamoff(4,imbb(i)))*                          &
     &bbcu(imbb(i),12)+(rkb(j)*(crxb(j)-exp(-tkb(j))*cbxb(j))*          &
     &sign(one,cikveb(j))-beamoff(5,imbb(i)))*bbcu(imbb(i),11)
                yv(2,j)=yv(2,j)+oidpsv(j)*cccc
              endif
            enddo
          endif
          goto 490
  730     continue
!--Hirata's 6D beam-beam kick
            do j=1,napx
              track6d(1,j)=(xv(1,j)+ed(ix)-clobeam(1,imbb(i)))*c1m3
              track6d(2,j)=(yv(1,j)/oidpsv(j)-clobeam(4,imbb(i)))*c1m3
              track6d(3,j)=(xv(2,j)+ek(ix)-clobeam(2,imbb(i)))*c1m3
              track6d(4,j)=(yv(2,j)/oidpsv(j)-clobeam(5,imbb(i)))*c1m3
              track6d(5,j)=(sigmv(j)-clobeam(3,imbb(i)))*c1m3
              track6d(6,j)=dpsv(j)-clobeam(6,imbb(i))
            enddo
            call beamint(napx,track6d,parbe,sigz,bbcu,imbb(i),ix,ibtyp, &
     &ibbc)
            do j=1,napx
              xv(1,j)=track6d(1,j)*c1e3+clobeam(1,imbb(i))-             &
     &beamoff(1,imbb(i))
              xv(2,j)=track6d(3,j)*c1e3+clobeam(2,imbb(i))-             &
     &beamoff(2,imbb(i))
              dpsv(j)=track6d(6,j)+clobeam(6,imbb(i))-beamoff(6,imbb(i))
              oidpsv(j)=one/(one+dpsv(j))
              yv(1,j)=(track6d(2,j)*c1e3+clobeam(4,imbb(i))-            &
     &beamoff(4,imbb(i)))*oidpsv(j)
              yv(2,j)=(track6d(4,j)*c1e3+clobeam(5,imbb(i))-            &
     &beamoff(5,imbb(i)))*oidpsv(j)
              ejfv(j)=dpsv(j)*e0f+e0f
              ejv(j)=sqrt(ejfv(j)*ejfv(j)+pma*pma)
              rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
              if(ithick.eq.1) call envarsv(dpsv,oidpsv,rvv,ekv)
            enddo
          goto 490
  740     continue
          irrtr=imtr(ix)
          do j=1,napx
            sigmv(j)=sigmv(j)+cotr(irrtr,5)+rrtr(irrtr,5,1)*xv(1,j)+    &
     &rrtr(irrtr,5,2)*yv(1,j)+rrtr(irrtr,5,3)*xv(2,j)+                  &
     &rrtr(irrtr,5,4)*yv(2,j)
            pux=xv(1,j)
            dpsv3(j)=dpsv(j)*c1e3
            xv(1,j)=cotr(irrtr,1)+rrtr(irrtr,1,1)*pux+                  &
     &rrtr(irrtr,1,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,1,6)
            yv(1,j)=cotr(irrtr,2)+rrtr(irrtr,2,1)*pux+                  &
     &rrtr(irrtr,2,2)*yv(1,j)+idz(1)*dpsv3(j)*rrtr(irrtr,2,6)
            pux=xv(2,j)
            xv(2,j)=cotr(irrtr,3)+rrtr(irrtr,3,3)*pux+                  &
     &rrtr(irrtr,3,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,3,6)
            yv(2,j)=cotr(irrtr,4)+rrtr(irrtr,4,3)*pux+                  &
     &rrtr(irrtr,4,4)*yv(2,j)+idz(2)*dpsv3(j)*rrtr(irrtr,4,6)
          enddo
 
!----------------------------------------------------------------------
 
! Wire.
 
          goto 490
  745     continue
          xory=1
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 490
  746     continue
          xory=2
          nfree=nturn1(ix)
         if(n.gt.nfree) then
          nac=n-nfree
          pi=4d0*atan(1d0)
!---------ACdipAmp input in Tesla*meter converted to KeV/c
!---------ejfv(j) should be in MeV/c --> ACdipAmp/ejfv(j) is in mrad
          acdipamp=ed(ix)*clight*1.0d-3
!---------Qd input in tune units
          qd=ek(ix)
!---------ACphase input in radians
          acphase=acdipph(ix)
          nramp1=nturn2(ix)
          nplato=nturn3(ix)
          nramp2=nturn4(ix)
          do j=1,napx
      if (xory.eq.1) then
        acdipamp2=acdipamp*tilts(i)
        acdipamp1=acdipamp*tiltc(i)
      else
        acdipamp2=acdipamp*tiltc(i)
        acdipamp1=-acdipamp*tilts(i)
      endif
              if(nramp1.gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)*nac/dble(nramp1)/ejfv(j)
              endif
              if(nac.ge.nramp1.and.(nramp1+nplato).gt.nac) then
                yv(1,j)=yv(1,j)+acdipamp1*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
                yv(2,j)=yv(2,j)+acdipamp2*                              &
     &sin(2d0*pi*qd*nac+acphase)/ejfv(j)
              endif
              if(nac.ge.(nramp1+nplato).and.(nramp2+nramp1+nplato).gt.  &
     &nac)then
              yv(1,j)=yv(1,j)+acdipamp1*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              yv(2,j)=yv(2,j)+acdipamp2*sin(2d0*pi*qd*nac+acphase)*     &
     &(-(nac-nramp1-nramp2-nplato)*1d0/dble(nramp2))/ejfv(j)
              endif
      enddo
      endif
          goto 490
 
!----------------------------
 
! Wire.
 
  748     continue
!     magnetic rigidity
      chi = sqrt(e0*e0-pmap*pmap)*c1e6/clight
 
      ix = ixcav
      tx = xrms(ix)
      ty = zrms(ix)
      dx = xpl(ix)
      dy = zpl(ix)
      embl = ek(ix)
      l = wirel(ix)
      cur = ed(ix)
 
      leff = embl/cos(tx)/cos(ty)
      rx = dx *cos(tx)-embl*sin(tx)/2
      lin= dx *sin(tx)+embl*cos(tx)/2
      ry = dy *cos(ty)-lin *sin(ty)
      lin= lin*cos(ty)+dy  *sin(ty)
 
      do 750 j=1, napx
 
      xv(1,j) = xv(1,j) * c1m3
      xv(2,j) = xv(2,j) * c1m3
      yv(1,j) = yv(1,j) * c1m3
      yv(2,j) = yv(2,j) * c1m3
 
!      print *, 'Start: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
!     call tilt(tx,ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(tx)*yv(2,j)/sqrt((1+dpsv(j))**2-    &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-tx)
      xv(1,j) = xv(1,j)*(cos(tx)-sin(tx)*tan(atan(yv(1,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-tx)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(ty)*yv(1,j)/sqrt((1+dpsv(j))**2-    &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))-ty)
      xv(2,j) = xv(2,j)*(cos(ty)-sin(ty)*tan(atan(yv(2,j)/              &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))-ty)
 
!     call drift(lin)
 
      xv(1,j) = xv(1,j) + lin*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) + lin*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-   &
     &yv(2,j)**2)
 
!      call kick(l,cur,lin,rx,ry,chi)
 
      xi = xv(1,j)-rx
      yi = xv(2,j)-ry
      yv(1,j) = yv(1,j)-1.0d-7*cur/chi*xi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
!GRD FOR CONSISTENSY
!      yv(2,j) = yv(2,j)-1e-7*cur/chi*yi/(xi**2+yi**2)*                  &
      yv(2,j) = yv(2,j)-1.0d-7*cur/chi*yi/(xi**2+yi**2)*                &
     &(sqrt((lin+l)**2+xi**2+yi**2)-sqrt((lin-l)**2+                    &
     &xi**2+yi**2))
 
!     call drift(leff-lin)
 
      xv(1,j) = xv(1,j) + (leff-lin)*yv(1,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
      xv(2,j) = xv(2,j) + (leff-lin)*yv(2,j)/sqrt((1+dpsv(j))**2-       &
     &yv(1,j)**2-yv(2,j)**2)
 
!     call invtilt(tx,ty)
 
      xv(1,j) = xv(1,j)-xv(2,j)*sin(-ty)*yv(1,j)/sqrt((1+dpsv(j))**2-   &
     &yv(1,j)**2)/cos(atan(yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+ty)
      xv(2,j) = xv(2,j)*(cos(-ty)-sin(-ty)*tan(atan(yv(2,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty))
      yv(2,j) = sqrt((1+dpsv(j))**2-yv(1,j)**2)*sin(atan(yv(2,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+ty)
 
      xv(2,j) = xv(2,j)-xv(1,j)*sin(-tx)*yv(2,j)/sqrt((1+dpsv(j))**2-   &
     &yv(2,j)**2)/cos(atan(yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-      &
     &yv(2,j)**2))+tx)
      xv(1,j) = xv(1,j)*(cos(-tx)-sin(-tx)*tan(atan(yv(1,j)/            &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx))
      yv(1,j) = sqrt((1+dpsv(j))**2-yv(2,j)**2)*sin(atan(yv(1,j)/       &
     &sqrt((1+dpsv(j))**2-yv(1,j)**2-yv(2,j)**2))+tx)
 
!     call shift(-embl*tan(tx),-embl*tan(ty)/cos(tx))
 
      xv(1,j) = xv(1,j) + embl*tan(tx)
      xv(2,j) = xv(2,j) + embl*tan(ty)/cos(tx)
 
!     call drift(-embl/2)
 
      xv(1,j) = xv(1,j) - embl/2*yv(1,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
      xv(2,j) = xv(2,j) - embl/2*yv(2,j)/sqrt((1+dpsv(j))**2-yv(1,j)**2-&
     &yv(2,j)**2)
 
      xv(1,j) = xv(1,j) * c1e3
      xv(2,j) = xv(2,j) * c1e3
      yv(1,j) = yv(1,j) * c1e3
      yv(2,j) = yv(2,j) * c1e3
 
!      print *, 'End: ',j,xv(1,j),xv(2,j),yv(1,j),
!     &yv(2,j)
 
!-----------------------------------------------------------------------
 
  750     continue
          goto 490
 
!----------------------------
 
  490       continue
          llost=.false.
          do j=1,napx
             llost=llost.or.                                            &
     &abs(xv(1,j)).gt.aper(1).or.abs(xv(2,j)).gt.aper(2)
          enddo
          if (llost) then
             kpz=abs(kp(ix))
             if(kpz.eq.2) then
                call lostpar3(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             elseif(kpz.eq.3) then
                call lostpar4(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             else
                call lostpar2(i,ix,nthinerr)
                if(nthinerr.ne.0) return
             endif
          endif
  500     continue
          call lostpart(nthinerr)
          if(nthinerr.ne.0) return
          if(ntwin.ne.2) call dist1
          if(mod(n,nwr(4)).eq.0) call write6(n)
  510 continue
      return
      end
      subroutine synuthck
!-----------------------------------------------------------------------
!
!  TRACK THICK LENS PART
!
!
!  F. SCHMIDT
!-----------------------------------------------------------------------
!  3 February 1999
!-----------------------------------------------------------------------
      implicit none
      integer ih1,ih2,j,kz1,l
      double precision fokm
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      double precision c180e0,c1e1,c1e12,c1e13,c1e15,c1e16,c1e2,c1e3,   &
     &c1e4,c1e6,c1m1,c1m7,c1m10,c1m12,c1m13,c1m15,c1m18,c1m2,c1m21,     &
     &c1m24,c1m3,c1m36,c1m38,c1m6,c1m9,c2e3,c4e3,crade,clight,four,half,&
     &one,pieni,pmae,pmap,three,two,zero
      parameter(pieni = 1d-38)
      parameter(zero = 0.0d0,half = 0.5d0,one = 1.0d0)
      parameter(two = 2.0d0,three = 3.0d0,four = 4.0d0)
      parameter(c1e1 = 1.0d1,c1e2 = 1.0d2,c1m2 = 1.0d-2)
      parameter(c1e3 = 1.0d3,c2e3 = 2.0d3,c4e3 = 4.0d3,c1e4 = 1.0d4)
      parameter(c1e12 = 1.0d12,c1e13 = 1.0d13,c1e15 = 1.0d15,c1e16 =    &
     &1.0d16)
      parameter(c180e0 = 180.0d0,c1e6 = 1.0d6)
      parameter(c1m1 = 1.0d-1,c1m3 = 1.0d-3,c1m6 = 1.0d-6,c1m7 = 1.0d-7)
      parameter(c1m9 = 1.0d-9,c1m10 = 1.0d-10,c1m12 = 1.0d-12)
      parameter(c1m13 = 1.0d-13,c1m15 = 1.0d-15)
      parameter(c1m18 = 1.0d-18,c1m21 = 1.0d-21,c1m24 = 1.0d-24)
      parameter(c1m36 = 1.0d-36,c1m38 = 1.0d-38)
      parameter(pmap = 938.271998d0,pmae = .510998902d0)
      parameter(crade = 2.817940285d-15, clight = 2.99792458d8)
      integer iav,ibb6d,ibbc,ibeco,ibidu,ibtyp,ic,icext,icextal,iclo,   &
     &iclo6,iclo6r,icode,icoe,icomb,icomb0,iconv,icow,icr,idam,idfor,   &
     &idis,idp,ierro,iffw,ifh,iicav,il,ilin,imad,imbb,                  &
     &imc,imtr,iorg,iout,                                               &
     &ipos,ipr,iprint,ipt,iq,iqmod,iqmod6,iratioe,irco,ird,ire,ires,    &
     &irew,irip,irm,irmod2,ise,ise1,ise2,ise3,isea,iskew,iskip,istw,    &
     &isub,itco,itcro,itf,ithick,ition,itionc,itqv,its6d,iu,iver,ivox,  &
     &ivoz,iwg,ixcav,izu0,kanf,kp,kpa,kwtype,kz,lhc,m21,m22,m23,mblo,   &
     &mbloz,mcut,mel,mesa,mmac,mout2,mp,mper,mstr,msym,mtyp,mzu,napx,   &
     &napxo,nbeam,nch,ncororb,ncorrep,ncorru,ncy,ndafi,nde,nhcorr,      &
     &nhmoni,niu,nlin,nmu,npp,nprint,nqc,nre,nrel,nrr,nrturn,nskew,     &
     &nstart,nstop,nt,nta,ntco,nte,ntwin,nu,numl,numlr,nur,nvcorr,      &
     &nvmoni,nwr, nturn1, nturn2, nturn3, nturn4
      double precision a,ak0,aka,alfx,alfz,amp0,aper,apx,apz,ape,bbcu,  &
     &bclorb,beamoff,benkc,benki,betac,betam,betx,betz,bk0,bka,bl1,bl2, &
     &clo6,clobeam,clop6,cma1,cma2,cotr,crad,de0,dech,ded,dfft,         &
     &di0,dip0,dki,dkq,dma,dmap,dphix,dphiz,dppoff,dpscor,dqq,dres,dsi, &
     &dsm0,dtr,e0,ed,ej,ejf,ek,el,elbe,emitx,emity,emitz,extalign,      &
     &exterr,eui,euii,gammar,hsy,hsyc,pac,pam,parbe,parbe14,partnum,    &
     &phas,phas0,phasc,pi,pi2,pisqrt,pma,ptnfac,qs,qw0,qwsk,qx0,qxt,qz0,&
     &qzt,r00,rad,ramp,rat,ratio,ratioe,rfre,rrtr,rtc,rts,rvf,rzph,     &
     &sigcor,sige,sigma0,sigman,sigman2,sigmanq,sigmoff,sigz,sm,ta,tam1,&
     &tam2,tiltc,tilts,tlen,totl,track6d,xpl,xrms,zfz,zpl,zrms,wirel,   &
     &acdipph
      real hmal
      character*16 bez,bezb,bezr,erbez,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro,erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,niu(2),numlr,nde(2),nwr(4), &
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),          &
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,            &
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/corcom/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),                       &
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),bezr(3,nele),izu0,mmac,mcut
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),        &
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,&
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/bez(nele),elbe(nblo),bezb(nblo),ilin,nt,iprint,      &
     &ntco,eui,euii,nlin,bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),         &
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,         &
     &ncororb(nele)
      common/apert/apx(nele),apz(nele),ape(3,nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),           &
     &ratioe(nele),iratioe(nele),icoe
      common/seacom/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/pawc/hmal(nplo)
      common/tit/sixtit,commen,ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/sigman(2,nbb),sigman2(2,nbb),sigmanq(2,nbb),          &
     &clobeam(6,nbb),beamoff(6,nbb),parbe(nele,4),track6d(6,npart),     &
     &ptnfac(nele),sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,  &
     &nbeam,ibbc,ibeco,ibtyp,lhc
      common/trom/ cotr(ntr,6),rrtr(ntr,6,6),imtr(nele)
      common/bb6d/ bbcu(nbb,12),ibb6d,imbb(nblz)
      common/wireco/ wirel(nele)
      common/acdipco/ acdipph(nele), nturn1(nele), nturn2(nele),        &
     &nturn3(nele), nturn4(nele)
      integer idz,itra
      double precision al,as,chi0,chid,dp1,dps,exz,sigm
      common/syos/as(6,2,npart,nele),al(6,2,npart,nele),sigm(mpa),      &
     &dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      integer ichrom,is
      double precision alf0,amp,bet0,clo,clop,cro,x,y
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      double precision tasm
      common/tasm/tasm(6,6)
      integer iv,ixv,nlostp,nms,numxv
      double precision aaiv,aek,afok,alf0v,ampv,aperv,as3,as4,as6,bbiv, &
     &bet0v,bl1v,ci,clo0,clo6v,cloau,clop0,clop6v,clopv,clov,co,cr,dam, &
     &di0au,di0xs,di0zs,dip0xs,dip0zs,dp0v,dpd,dpsq,dpsv,dpsv6,dpsvl,   &
     &ejf0v,ejfv,ejv,ejvl,ekk,ekkv,ekv,eps,epsa,fake,fi,fok,fok1,fokqv, &
     &g,gl,hc,hi,hi1,hm,hp,hs,hv,oidpsv,qw,qwc,qwcs,rho,rhoc,rhoi,rvv,  &
     &si,sigmv,sigmv6,sigmvl,siq,sm1,sm12,sm2,sm23,sm3,smiv,tas,        &
     &tasau,tau,wf,wfa,wfhi,wx,x1,x2,xau,xlv,xsiv,xsv,xv,xvl,yv,yvl,zlv,&
     &zsiv,zsv
      logical pstop
      common/main1/                                                     &
     &ekv(npart,nele),fokqv(npart),aaiv(mmul,nmac,nblz),                &
     &bbiv(mmul,nmac,nblz),smiv(nmac,nblz),zsiv(nmac,nblz),             &
     &xsiv(nmac,nblz),xsv(npart),zsv(npart),qw(2),qwc(3),clo0(2),       &
     &clop0(2),eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),xv(2,npart),     &
     &yv(2,npart),dam(npart),ekkv(npart),sigmv(npart),dpsv(npart),      &
     &dp0v(npart),sigmv6(npart),dpsv6(npart),ejv(npart),ejfv(npart),    &
     &xlv(npart),zlv(npart),pstop(npart),rvv(npart),                    &
     &ejf0v(npart),numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),       &
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),sm1(npart),    &
     &sm2(npart),sm3(npart),sm12(npart),as3(npart),as4(npart),          &
     &as6(npart),sm23(npart),rhoc(npart),siq(npart),aek(npart),         &
     &afok(npart),hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),    &
     &wfa(npart),wfhi(npart),rhoi(npart),hi(npart),fi(npart),hi1(npart),&
     &xvl(2,npart),yvl(2,npart),ejvl(npart),dpsvl(npart),oidpsv(npart), &
     &sigmvl(npart),iv(npart),aperv(npart,2),ixv(npart),clov(2,npart),  &
     &clopv(2,npart),alf0v(npart,2),bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),hv(6,2,npart,nblo),  &
     &bl1v(6,2,npart,nblo),tas(npart,6,6),qwcs(npart,3),di0xs(npart),   &
     &di0zs(npart),dip0xs(npart),dip0zs(npart),xau(2,6),cloau(6),       &
     &di0au(4),tau(6,6),tasau(npart,6,6),wx(3),x1(6),x2(6),fake(2,20)
      integer numx
      double precision e0f
      common/main4/ e0f,numx
      integer ktrack,nwri
      double precision dpsv1,strack,strackc,stracks
      common/track/ ktrack(nblz),strack(nblz),strackc(nblz),            &
     &stracks(nblz),dpsv1(npart),nwri
      save
!---------------------------------------  SUBROUTINE 'ENVARS' IN-LINE
      do 10 j=1,napx
        dpd(j)=one+dpsv(j)
        dpsq(j)=sqrt(dpd(j))
!
   10 continue
      do 160 l=1,il
        if(abs(el(l)).le.pieni) goto 160
        kz1=kz(l)+1
        goto(20,40,80,60,40,60,100,100,140),kz1
        goto 160
!-----------------------------------------------------------------------
!  DRIFTLENGTH
!-----------------------------------------------------------------------
   20   do 30 j=1,napx
          as(6,1,j,l)=-rvv(j)*el(l)/c2e3
          as(6,2,j,l)=as(6,1,j,l)
          as(1,1,j,l)=el(l)*(one-rvv(j))*c1e3
   30   continue
        goto 160
!-----------------------------------------------------------------------
!  RECTANGULAR MAGNET
!  HORIZONTAL
!-----------------------------------------------------------------------
   40   fokm=el(l)*ed(l)
        if(abs(fokm).le.pieni) goto 20
        if(kz1.eq.2) then
          ih1=1
          ih2=2
        else
!  RECTANGULAR MAGNET VERTICAL
          ih1=2
          ih2=1
        endif
        do 50 j=1,napx
          fok(j)=fokm/dpsq(j)
          rho(j)=(one/ed(l))*dpsq(j)
          fok1(j)=(tan(fok(j)*half))/rho(j)
          si(j)=sin(fok(j))
          co(j)=cos(fok(j))
          al(2,ih1,j,l)=rho(j)*si(j)
          al(5,ih1,j,l)=-dpsv(j)*(rho(j)*(one-co(j))/dpsq(j))*c1e3
          al(6,ih1,j,l)=-dpsv(j)*(two*tan(fok(j)*half)/dpsq(j))*c1e3
          sm1(j)=cos(fok(j))
          sm2(j)=sin(fok(j))*rho(j)
          sm3(j)=-sin(fok(j))/rho(j)
          sm12(j)=el(l)-sm1(j)*sm2(j)
          sm23(j)=sm2(j)*sm3(j)
          as3(j)=-rvv(j)*(dpsv(j)*rho(j)/(two*dpsq(j))*sm23(j)- rho(j)  &
     &*dpsq(j)*(one-sm1(j)))
          as4(j)=-rvv(j)*sm23(j)/c2e3
          as6(j)=-rvv(j)*(el(l)+sm1(j)*sm2(j))/c4e3
          as(1,ih1,j,l)=(-rvv(j)*(dpsv(j)*dpsv(j)/(four*dpd(j))*sm12(j)+&
     &dpsv(j)*(el(l)-sm2(j)))+el(l)*(one-rvv(j)))*c1e3
          as(2,ih1,j,l)=-rvv(j)*(dpsv(j)/(two*rho(j)*dpsq(j))*sm12(j)-  &
     &sm2(j)*dpsq(j)/rho(j))+fok1(j)*as3(j)
          as(3,ih1,j,l)=as3(j)
          as(4,ih1,j,l)=as4(j)+two*as6(j)*fok1(j)
          as(5,ih1,j,l)=-rvv(j)*sm12(j)/(c4e3*rho(j)*rho(j))+ as6(j)    &
     &*fok1(j)*fok1(j)+fok1(j)*as4(j)
          as(6,ih1,j,l)=as6(j)
!--VERTIKAL
          g(j)=tan(fok(j)*half)/rho(j)
          gl(j)=el(l)*g(j)
          al(1,ih2,j,l)=one-gl(j)
          al(3,ih2,j,l)=-g(j)*(two-gl(j))
          al(4,ih2,j,l)=al(1,ih2,j,l)
          as6(j)=-rvv(j)*al(2,ih2,j,l)/c2e3
          as(4,ih2,j,l)=-two*as6(j)*fok1(j)
          as(5,ih2,j,l)=as6(j)*fok1(j)*fok1(j)
          as(6,ih2,j,l)=as6(j)
   50   continue
        goto 160
!-----------------------------------------------------------------------
!  SEKTORMAGNET
!  HORIZONTAL
!-----------------------------------------------------------------------
   60   fokm=el(l)*ed(l)
        if(abs(fokm).le.pieni) goto 20
        if(kz1.eq.4) then
          ih1=1
          ih2=2
        else
!  SECTOR MAGNET VERTICAL
          ih1=2
          ih2=1
        endif
        do 70 j=1,napx
          fok(j)=fokm/dpsq(j)
          rho(j)=(one/ed(l))*dpsq(j)
          si(j)=sin(fok(j))
          co(j)=cos(fok(j))
          rhoc(j)=rho(j)*(one-co(j))/dpsq(j)
          siq(j)=si(j)/dpsq(j)
          al(1,ih1,j,l)=co(j)
          al(2,ih1,j,l)=rho(j)*si(j)
          al(3,ih1,j,l)=-si(j)/rho(j)
          al(4,ih1,j,l)=co(j)
          al(5,ih1,j,l)=-dpsv(j)*rhoc(j)*c1e3
          al(6,ih1,j,l)=-dpsv(j)*siq(j)*c1e3
          sm12(j)=el(l)-al(1,ih1,j,l)*al(2,ih1,j,l)
          sm23(j)=al(2,ih1,j,l)*al(3,ih1,j,l)
          as(1,ih1,j,l)=(-rvv(j)*(dpsv(j)*dpsv(j)/(four*dpd(j))*sm12(j) &
     &+dpsv(j)*(el(l)-al(2,ih1,j,l)))+el(l)*(one-rvv(j)))*c1e3
          as(2,ih1,j,l)=-rvv(j)*(dpsv(j)/(two*rho(j)*dpsq(j))*sm12(j)-  &
     &dpd(j)*siq(j))
          as(3,ih1,j,l)=-rvv(j)*(dpsv(j)*rho(j)/(two*dpsq(j))*sm23(j)-  &
     &dpd(j)*rhoc(j))
          as(4,ih1,j,l)=-rvv(j)*sm23(j)/c2e3
          as(5,ih1,j,l)=-rvv(j)*sm12(j)/(c4e3*rho(j)*rho(j))
          as(6,ih1,j,l)=-rvv(j)*(el(l)+al(1,ih1,j,l)*al(2,ih1,j,l))/c4e3
!--VERTIKAL
          as(6,ih2,j,l)=-rvv(j)*al(2,ih2,j,l)/c2e3
   70   continue
        goto 160
!-----------------------------------------------------------------------
!  QUADRUPOLE
!  FOCUSSING
!-----------------------------------------------------------------------
   80   do 90 j=1,napx
          fok(j)=ekv(j,l)*oidpsv(j)
          aek(j)=abs(fok(j))
          hi(j)=sqrt(aek(j))
          fi(j)=el(l)*hi(j)
          if(fok(j).le.zero) then
            al(1,1,j,l)=cos(fi(j))
            hi1(j)=sin(fi(j))
            if(abs(hi(j)).le.pieni) then
              al(2,1,j,l)=el(l)
            else
              al(2,1,j,l)=hi1(j)/hi(j)
            endif
            al(3,1,j,l)=-hi1(j)*hi(j)
            al(4,1,j,l)=al(1,1,j,l)
            as(1,1,j,l)=el(l)*(one-rvv(j))*c1e3
            as(4,1,j,l)=-rvv(j)*al(2,1,j,l)*al(3,1,j,l)/c2e3
            as(5,1,j,l)=-rvv(j)*(el(l)-al(1,1,j,l)*al(2,1,j,l))* aek(j) &
     &/c4e3
            as(6,1,j,l)=-rvv(j)*(el(l)+al(1,1,j,l)*al(2,1,j,l))/c4e3
!--DEFOCUSSING
            hp(j)=exp(fi(j))
            hm(j)=one/hp(j)
            hc(j)=(hp(j)+hm(j))*half
            hs(j)=(hp(j)-hm(j))*half
            al(1,2,j,l)=hc(j)
            if(abs(hi(j)).le.pieni) then
              al(2,2,j,l)=el(l)
            else
              al(2,2,j,l)=hs(j)/hi(j)
            endif
            al(3,2,j,l)=hs(j)*hi(j)
            al(4,2,j,l)=hc(j)
            as(4,2,j,l)=-rvv(j)*al(2,2,j,l)*al(3,2,j,l)/c2e3
            as(5,2,j,l)=+rvv(j)*(el(l)-al(1,2,j,l)*al(2,2,j,l))* aek(j) &
     &/c4e3
            as(6,2,j,l)=-rvv(j)*(el(l)+al(1,2,j,l)*al(2,2,j,l))/c4e3
          else
            al(1,2,j,l)=cos(fi(j))
            hi1(j)=sin(fi(j))
            if(abs(hi(j)).le.pieni) then
              al(2,2,j,l)=el(l)
            else
              al(2,2,j,l)=hi1(j)/hi(j)
            endif
            al(3,2,j,l)=-hi1(j)*hi(j)
            al(4,2,j,l)=al(1,2,j,l)
            as(1,2,j,l)=el(l)*(one-rvv(j))*c1e3
            as(4,2,j,l)=-rvv(j)*al(2,2,j,l)*al(3,2,j,l)/c2e3
            as(5,2,j,l)=-rvv(j)*(el(l)-al(1,2,j,l)*al(2,2,j,l))* aek(j) &
     &/c4e3
            as(6,2,j,l)=-rvv(j)*(el(l)+al(1,2,j,l)*al(2,2,j,l))/c4e3
!--DEFOCUSSING
            hp(j)=exp(fi(j))
            hm(j)=one/hp(j)
            hc(j)=(hp(j)+hm(j))*half
            hs(j)=(hp(j)-hm(j))*half
            al(1,1,j,l)=hc(j)
            if(abs(hi(j)).le.pieni) then
              al(2,1,j,l)=el(l)
            else
              al(2,1,j,l)=hs(j)/hi(j)
            endif
            al(3,1,j,l)=hs(j)*hi(j)
            al(4,1,j,l)=hc(j)
            as(4,1,j,l)=-rvv(j)*al(2,1,j,l)*al(3,1,j,l)/c2e3
            as(5,1,j,l)=+rvv(j)*(el(l)-al(1,1,j,l)*al(2,1,j,l))* aek(j) &
     &/c4e3
            as(6,1,j,l)=-rvv(j)*(el(l)+al(1,1,j,l)*al(2,1,j,l))/c4e3
          endif
   90   continue
        goto 160
!-----------------------------------------------------------------------
!  COMBINED FUNCTION MAGNET HORIZONTAL
!  FOCUSSING
!-----------------------------------------------------------------------
  100   if(kz1.eq.7) then
          do 110 j=1,napx
            fokqv(j)=ekv(j,l)
  110     continue
          ih1=1
          ih2=2
        else
!  COMBINED FUNCTION MAGNET VERTICAL
          do 120 j=1,napx
            fokqv(j)=-ekv(j,l)
  120     continue
          ih1=2
          ih2=1
        endif
        do 130 j=1,napx
          wf(j)=ed(l)/dpsq(j)
          fok(j)=fokqv(j)/dpd(j)-wf(j)*wf(j)
          afok(j)=abs(fok(j))
          hi(j)=sqrt(afok(j))
          fi(j)=hi(j)*el(l)
          if(afok(j).le.pieni) then
            as(6,1,j,l)=-rvv(j)*el(l)/c2e3
            as(6,2,j,l)=as(6,1,j,l)
            as(1,1,j,l)=el(l)*(one-rvv(j))*c1e3
          endif
          if(fok(j).lt.-pieni) then
            si(j)=sin(fi(j))
            co(j)=cos(fi(j))
            wfa(j)=wf(j)/afok(j)*(one-co(j))/dpsq(j)
            wfhi(j)=wf(j)/hi(j)*si(j)/dpsq(j)
            al(1,ih1,j,l)=co(j)
            al(2,ih1,j,l)=si(j)/hi(j)
            al(3,ih1,j,l)=-si(j)*hi(j)
            al(4,ih1,j,l)=co(j)
            al(5,ih1,j,l)=-wfa(j)*dpsv(j)*c1e3
            al(6,ih1,j,l)=-wfhi(j)*dpsv(j)*c1e3
            sm12(j)=el(l)-al(1,ih1,j,l)*al(2,ih1,j,l)
            sm23(j)=al(2,ih1,j,l)*al(3,ih1,j,l)
            as(1,ih1,j,l)=(-rvv(j)*(dpsv(j)*dpsv(j)/(four*dpd(j))*sm12  &
     &(j)+ dpsv(j)*(el(l)-al(2,ih1,j,l)))/afok(j)*wf(j)*wf(j)+el        &
     &(l)* (one-rvv(j)))*c1e3
            as(2,ih1,j,l)=-rvv(j)*(dpsv(j)*wf(j)/(two*dpsq(j))*sm12(j)- &
     &dpd(j)*wfhi(j))
            as(3,ih1,j,l)=-rvv(j)*(dpsv(j)*half/afok(j)/dpd(j)* ed(l)   &
     &*sm23(j)-dpd(j)*wfa(j))
            as(4,ih1,j,l)=-rvv(j)*sm23(j)/c2e3
            as(5,ih1,j,l)=-rvv(j)*sm12(j)*afok(j)/c4e3
            as(6,ih1,j,l)=-rvv(j)*(el(l)+al(1,ih1,j,l)*al(2,ih1,j,l))   &
     &/c4e3
            aek(j)=abs(ekv(j,l)/dpd(j))
            hi(j)=sqrt(aek(j))
            fi(j)=hi(j)*el(l)
            hp(j)=exp(fi(j))
            hm(j)=one/hp(j)
            hc(j)=(hp(j)+hm(j))*half
            hs(j)=(hp(j)-hm(j))*half
            al(1,ih2,j,l)=hc(j)
            if(abs(hi(j)).gt.pieni) al(2,ih2,j,l)=hs(j)/hi(j)
            al(3,ih2,j,l)=hs(j)*hi(j)
            al(4,ih2,j,l)=hc(j)
            as(4,ih2,j,l)=-rvv(j)*al(2,ih2,j,l)*al(3,ih2,j,l)/c2e3
            as(5,ih2,j,l)=+rvv(j)*(el(l)-al(1,ih2,j,l)*al(2,ih2,j,l))*  &
     &aek(j)/c4e3
            as(6,ih2,j,l)=-rvv(j)*(el(l)+al(1,ih2,j,l)*al(2,ih2,j,l))   &
     &/c4e3
          endif
!--DEFOCUSSING
          if(fok(j).gt.pieni) then
            hp(j)=exp(fi(j))
            hm(j)=one/hp(j)
            hc(j)=(hp(j)+hm(j))*half
            hs(j)=(hp(j)-hm(j))*half
            al(1,ih1,j,l)=hc(j)
            al(2,ih1,j,l)=hs(j)/hi(j)
            al(3,ih1,j,l)=hs(j)*hi(j)
            al(4,ih1,j,l)=hc(j)
            wfa(j)=wf(j)/afok(j)*(one-hc(j))/dpsq(j)
            wfhi(j)=wf(j)/hi(j)*hs(j)/dpsq(j)
            al(5,ih1,j,l)= wfa(j)*dpsv(j)*c1e3
            al(6,ih1,j,l)=-wfhi(j)*dpsv(j)*c1e3
            sm12(j)=el(l)-al(1,ih1,j,l)*al(2,ih1,j,l)
            sm23(j)=al(2,ih1,j,l)*al(3,ih1,j,l)
            as(1,ih1,j,l)=(rvv(j)*(dpsv(j)*dpsv(j)/(four*dpd(j))*sm12(j)&
     &+dpsv(j)*(el(l)-al(2,ih1,j,l)))/afok(j)*wf(j)*wf(j)+el(l)*        &
     &(one-rvv(j)))*c1e3
            as(2,ih1,j,l)=-rvv(j)*(dpsv(j)*wf(j)/(two*dpsq(j))*sm12(j)- &
     &dpd(j)*wfhi(j))
            as(3,ih1,j,l)=rvv(j)*(dpsv(j)*half/afok(j)/dpd(j)* ed(l)    &
     &*sm23(j)-dpd(j)*wfa(j))
            as(4,ih1,j,l)=-rvv(j)*sm23(j)/c2e3
            as(5,ih1,j,l)=+rvv(j)*sm12(j)*afok(j)/c4e3
            as(6,ih1,j,l)=-rvv(j)*(el(l)+al(1,ih1,j,l)*al(2,ih1,j,l))   &
     &/c4e3
            aek(j)=abs(ekv(j,l)/dpd(j))
            hi(j)=sqrt(aek(j))
            fi(j)=hi(j)*el(l)
            si(j)=sin(fi(j))
            co(j)=cos(fi(j))
            al(1,ih2,j,l)=co(j)
            al(2,ih2,j,l)=si(j)/hi(j)
            al(3,ih2,j,l)=-si(j)*hi(j)
            al(4,ih2,j,l)=co(j)
            as(4,ih2,j,l)=-rvv(j)*al(2,ih2,j,l)*al(3,ih2,j,l)/c2e3
            as(5,ih2,j,l)=-rvv(j)*(el(l)-al(1,ih2,j,l)*al(2,ih2,j,l))*  &
     &aek(j)/c4e3
            as(6,ih2,j,l)=-rvv(j)*(el(l)+al(1,ih2,j,l)*al(2,ih2,j,l))   &
     &/c4e3
          endif
  130   continue
        goto 160
!-----------------------------------------------------------------------
!  EDGE FOCUSSING
!-----------------------------------------------------------------------
  140   do 150 j=1,napx
          rhoi(j)=ed(l)/dpsq(j)
          fok(j)=rhoi(j)*tan(el(l)*rhoi(j)*half)
          al(3,1,j,l)=fok(j)
          al(3,2,j,l)=-fok(j)
  150   continue
  160 continue
!---------------------------------------  END OF 'ENVARS' (2)
      return
      end
      subroutine collimate2(name_coll,
     &                      c_material, c_length, c_rotation,           &
     &c_aperture, c_offset, c_tilt,x_in, xp_in, y_in,yp_in,p_in, s_in,  &
     &np, enom, lhit,part_abs, impact, indiv, lint, onesided, name,     &
     &flagsec, j_slices)
!MAY2005
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
!----                                                                    -----
!-----  NEW ROUTINES PROVIDED FOR THE COLLIMATION STUDIES VIA SIXTRACK   -----
!-----                                                                   -----
!-----          G. ROBERT-DEMOLAIZE, November 1st, 2004                  -----
!-----                                                                   -----
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
!
!
!++  Based on routines by JBJ. Changed by RA 2001.
!
!
!GRD
!GRD MODIFIED VERSION FOR COLLIMATION SYSTEM: G. ROBERT-DEMOLAIZE
!GRD
!
!++  - Deleted all HBOOK stuff.
!++  - Deleted optics routine and all parser routines.
!++  - Replaced RANMAR call by RANLUX call
!++  - Included RANLUX code from CERNLIB into source
!++  - Changed dimensions from CGen(100,nmat) to CGen(200,nmat)
!++  - Replaced FUNPRE with FUNLXP
!++  - Replaced FUNRAN with FUNLUX
!++  - Included all CERNLIB code into source: RANLUX, FUNLXP, FUNLUX,
!++                                         FUNPCT, FUNLZ, RADAPT,
!++                                           RGS56P
!++    with additional entries:             RLUXIN, RLUXUT, RLUXAT,
!++                                           RLUXGO
!++
!++  - Changed program so that Nev is total number of particles
!++    (scattered and not-scattered)
!++  - Added debug comments
!++  - Put real dp/dx
!
      implicit none
!
      character*16 name_coll
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
! THIS BLOCK IS COMMON TO THIN6D, TRAUTHIN, COLLIMATE32 AND MAINCR
!
      integer ipencil
      double precision xp_pencil0,yp_pencil0,x_pencil(max_ncoll),       &
     &y_pencil(max_ncoll),pencil_dx(max_ncoll)
      common  /pencil/  xp_pencil0,yp_pencil0,pencil_dx,ipencil
      common  /pencil2/ x_pencil, y_pencil
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      integer ie,iturn,nabs_total
      common  /info/ ie,iturn,nabs_total
!
!
      logical onesided,hit
      integer nprim,filel,mat,nev,j,nabs,nhit,np,icoll
!MAY2005
!      integer lhit(npart),part_abs(npart)
      integer lhit(npart),part_abs(npart),name(npart)
!MAY2005
      double precision p0,xmin,xmax,xpmin,xpmax,zmin,zmax,zpmin,zpmax   &
     &,length,zlm,x,x00,xp,z,z00,zp,p,sp,dpop,s,enom,x_in(npart),       &
     &xp_in(npart),y_in(npart),yp_in(npart),p_in(npart),s_in(npart),    &
     &indiv(npart),lint(npart),x_out(max_npart),xp_out(max_npart),      &
     &y_out(max_npart),yp_out(max_npart),p_out(max_npart),              &
     &s_out(max_npart),keeps,fracab,mybetax,mybetaz,mymux,mymuz,sigx,   &
     &sigz,norma,xpmu,atdi,drift_length,mirror,tiltangle,impact(npart)
!
      double precision c_length    !length in m
      double precision c_rotation  !rotation angle vs vertical in radian
      double precision c_aperture  !aperture in m
      double precision c_offset    !offset in m
      double precision c_tilt(2)   !tilt in radian
      character*6      c_material  !material
!
!
!
      character*(nc) filen,tit
!
      real   rndm4,xlow,xhigh,xplow,xphigh,dx,dxp
!
!AUGUST2006 Added ran_gauss for generation of pencil/     ------- TW
!           sheet beam distribution  (smear in x and y)
!
      double precision ran_gauss
!
      common /cmom/xmin,xmax,xpmin,xpmax,zmin,zmax,zpmin,zpmax,length,  &
     &nev
      common /materia/mat
      common /phase/x,xp,z,zp,dpop
      common /nommom/p0
      common /cjaw1/zlm
      common /other/mybetax,mybetaz,mymux,mymuz,atdi
      common /icoll/  icoll
!
      data   dx,dxp/.5d-4,20.d-4/
!
!
!
!GRD
!GRD THIS BLOC IS COMMON TO MAINCR, DATEN, TRAUTHIN AND THIN6D
!GRD
!APRIL2005
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
     &,relative, diffusive
!SEPT2005 for slicing process
      integer nloop,rnd_seed,c_offsettilt_seed,ibeam,jobnumber,         &
     &do_thisdis,n_slices,pencil_distr
!JUNE2005
      double precision myenom,mynex,mdex,myney,mdey,                    &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
!SEPT2005 add these lines for the slicing procedure
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
!SEPT2005,OCT2006 added offset
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,ndr,                            &
!JUNE2005
     &driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,            &
     &sigsecut3,sigsecut2,enerror,bunchlength
!JUNE2005
!APRIL2005
!
      character*24 name_sel
      character*80 coll_db
      character*16 castordir
!JUNE2005
      character*80 filename_dis
!JUNE2005
!
      common /grd/ myenom,mynex,mdex,myney,mdey,                        &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo,nsig_cry,   &
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
     & relative,diffusive,
     &sigsecut3,sigsecut2,enerror,                                      &
     &bunchlength,coll_db,name_sel,                                     &
     &castordir,filename_dis,nloop,rnd_seed,c_offsettilt_seed,          &
     &ibeam,jobnumber,do_thisdis,n_slices,pencil_distr,                 &
     &do_coll,                                                          &
!
     &do_select,do_nominal,dowrite_dist,do_oneside,dowrite_impact,      &
     &dowrite_secondary,dowrite_amplitude,radial,systilt_antisymm,      &
     &dowritetracks,cern,do_nsig,do_mingap
!SEPT2005
!JUNE2005
!APRIL2005
!
!--September 2006 -- TW common to readcollimator and collimate2
!      logical           changed_tilt1(max_ncoll)
!      logical           changed_tilt2(max_ncoll)
!      common /tilt/ changed_tilt1, changed_tilt2
!--September 2006
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
      double precision x_flk,xp_flk,y_flk,yp_flk
!
      double precision s_impact
      integer flagsec(maxn)
!
!     SR, 18-08-2005: add temporary variable to write in FirstImpacts
!     the initial distribution of the impacting particles in the
!     collimator frame.
      double precision xinn,xpinn,yinn,ypinn
!
!     SR, 29-08-2005: add the slice number to calculate the impact
!     location within the collimator.
!     j_slices = 1 for the a non sliced collimator!
      integer j_slices
!
      save
!
      common /Process/ bool_proc,bool_create
      integer  bool_proc(maxn)
      logical  bool_create
!=======================================================================
! Be=1 Al=2 Cu=3 W=4 Pb=5
!
! LHC uses:    Al, 0.2 m
!              Cu, 1.0 m
!
!      write(*,*) 'enter collimate2 routine'
      if (c_material.eq.'BE') then
         mat = 1
      elseif (c_material.eq.'Be') then
         mat = 1
      elseif (c_material.eq.'AL') then
         mat = 2
      elseif (c_material.eq.'Al') then
         mat = 2
      elseif (c_material.eq.'CU') then
         mat = 3
      elseif (c_material.eq.'Cu') then
         mat = 3
      elseif (c_material.eq.'W') then
         mat = 4
      elseif (c_material.eq.'PB') then
         mat = 5
      elseif (c_material.eq.'Pb') then
         mat = 5
      elseif (c_material.eq.'C') then
         mat = 6
      elseif (c_material.eq.'C2') then
         mat = 7
!02/2008 TW added vacuum and black absorber (was missing) 
      elseif (c_material.eq.'VA') then
         mat = 11
      elseif (c_material.eq.'BL') then
         mat = 12
      else
         write(*,*) 'WARNING> Material not found.', c_material
!        STOP
      endif
!
      length  = c_length
      nev = np
      p0  = enom
!
!++  Initialize scattering processes
!
      call scatin(p0)
 
! EVENT LOOP,  initial distribution is here a flat distribution with
! xmin=x-, xmax=x+, etc. from the input file
!
      nhit    = 0
      fracab  = 0d0
      mirror  = 1d0
!
!==> SLICE here
!
 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      do j = 1, nev
!
! SR-GRD (04-08-2005):
!     Don't do scattering process for particles already absorbed
        if (part_abs(j) .ne. 0) goto 777
        impact(j) = -1d0
        lint(j)   = -1d0
        indiv(j)  = -1d0
        x   = x_in(j)
        xp  = xp_in(j)
        z   = y_in(j)
        zp  = yp_in(j)
        p   = p_in(j)
        sp   = 0d0
        dpop = (p - p0)/p0
        x_flk  = 0d0
        y_flk  = 0d0
        xp_flk = 0d0
        yp_flk = 0d0
!
!++  Transform particle coordinates to get into collimator coordinate
!++  system
!
!++  First do rotation into collimator frame
!
        x  = x_in(j)*cos(c_rotation) +sin(c_rotation)*y_in(j)
        z  = y_in(j)*cos(c_rotation) -sin(c_rotation)*x_in(j)
        xp = xp_in(j)*cos(c_rotation)+sin(c_rotation)*yp_in(j)
        zp = yp_in(j)*cos(c_rotation)-sin(c_rotation)*xp_in(j)
!
!++  For one-sided collimators consider only positive X. For negative
!++  X jump to the next particle
!
        if (name_coll(6:8).eq."SPS") then
                if (x.gt.0) goto 777
        else
                if (onesided .and. x.lt.0) goto 777
        endif
        
!
!++  Now mirror at the horizontal axis for negative X offset
!
        if (x.lt.0) then
                mirror = -1d0
                tiltangle = -1d0*c_tilt(2)
        endif
        if (x.ge.0) then
                mirror = 1d0
                tiltangle = c_tilt(1)
        endif
        x  = mirror * x
        xp = mirror * xp
        x  = x - c_aperture/2 - mirror*c_offset
!++  Include collimator tilt
        if (tiltangle.gt.0.) then
                xp = xp - tiltangle
        endif
        if (tiltangle.lt.0.) then
                x  = x + sin(tiltangle) * c_length
                xp = xp - tiltangle
        endif
c------------------------------------------------------------------------        
c                            PENCIL BEAM 
!++  For selected collimator, first turn reset particle distribution
!++  to simple pencil beam
!
! -- TW why did I set this to 0, seems to be needed for getting 
!       right amplitude => no "tilt" of jaw for the first turn !!!!
        nprim = 3
        if ( (icoll.eq.ipencil .and. iturn.eq.1) .or. (iturn.eq.1       &
     &  .and. ipencil.eq.999 .and. icoll.le.nprim .and.                 &
     &  (j.ge.(icoll-1)*nev/nprim) .and. (j.le.(icoll)*nev/nprim))) then
! -- TW why did I set this to 0, seems to be needed for getting 
!       right amplitude => no "tilt" of jaw for the first turn !!!!
                c_tilt(1) = 0d0
                c_tilt(2) = 0d0
!AUGUST2006: Standard pencil beam as implemented by GRD ------- TW
                if (pencil_rmsx.eq.0. .and. pencil_rmsy.eq.0.) then
                        x    = pencil_dx(icoll)
                        xp   = 0.
                        z    = 0.
                        zp   = 0.
                endif
!
!AUGUST2006: Rectangular (pencil-beam) sheet-beam with  ------ TW
!            pencil_offset is the rectangulars center
!            pencil_rmsx defines spread of impact parameter
!            pencil_rmsy defines spread parallel to jaw surface
! 
                if (pencil_distr.eq.0 .and.(pencil_rmsx.ne.0.           &
     &          .or.pencil_rmsy.ne.0.)) then
! how to assure that all generated particles are on the jaw ?!
                        x    = pencil_dx(icoll)                         &
     &                  + pencil_rmsx*(rndm4()-0.5)
                        xp   = 0.
                        z    = pencil_rmsy*(rndm4()-0.5)
                        zp   = 0.
                endif
!
!AUGUST2006: Gaussian (pencil-beam) sheet-beam with ------- TW
!            pencil_offset is the mean  gaussian distribution
!            pencil_rmsx defines spread of impact parameter
!            pencil_rmsy defines spread parallel to jaw surface
! 
                if (pencil_distr.eq.1 .and.(pencil_rmsx.ne.0.           &
     &          .or.pencil_rmsy.ne.0. )) then
                        x   =pencil_dx(icoll)+pencil_rmsx*ran_gauss(2d0)
! all generated particles are on the jaw now
                        x    = sqrt(x**2)
                        xp   = 0.
                        z    = pencil_rmsy*ran_gauss(2d0)
                        zp   = 0.
                endif
!AUGUST2006: Gaussian (pencil-beam) sheet-beam with ------- TW
!            pencil_offset is the mean  gaussian distribution
!            pencil_rmsx defines spread of impact parameter
!                        here pencil_rmsx is not gaussian!!!
!            pencil_rmsy defines spread parallel to jaw surface
! 
                if (pencil_distr.eq.2 .and.(pencil_rmsx.ne.0.           &
     &          .or.pencil_rmsy.ne.0. )) then
                        x    = pencil_dx(icoll)                         &
     &                  + pencil_rmsx*(rndm4()-0.5)
! all generated particles are on the jaw now
                        x    = sqrt(x**2)
                        xp   = 0.
                        z    = pencil_rmsy*ran_gauss(2d0)
                        zp   = 0.
                endif
!JULY2007: Selection of pos./neg. jaw  implemented by GRD ---- TW
! ensure that for onesided only particles on pos. jaw are created
                if (onesided) then
                        mirror = 1d0
                else
                        if(rndm4().lt.0.5) then 
                                mirror = -1d0
                        else 
                                mirror = 1d0
                        endif
                endif 
! -- TW SEP07 if c_tilt is set to zero before entering pencil beam 
!             section the assigning of the tilt will result in 
!             assigning zeros 
                if (mirror.lt.0) then
                        tiltangle = c_tilt(2)
                else 
                        tiltangle = c_tilt(1)
                endif
c       write(9997,'(f10.8,(2x,f10.8),(2x,f10.8),(2x,f10.8)(2x,f10.8))')     
c     &            x, xp, z, zp, tiltangle
        endif  !!!!!end of the pencil beam stuff!!!!!
c------------------------------------------------------------------------        
!     SR, 18-08-2005: after finishing the coordinate transformation,
!     or the coordinate manipulations in case of pencil beams,
!     write down the initial coordinates of the impacting particles
        xinn  = x
        xpinn = xp
        yinn  = z
        ypinn = zp
!
!++  particle passing above the jaw are discarded => take new event
!++  entering by the face, shorten the length (zlm) and keep track of
!++  entrance longitudinal coordinate (keeps) for histograms
!
!++  The definition is that the collimator jaw is at x>=0.
!
!++  1) Check whether particle hits the collimator
!
        hit     =  .false.
        s       =  0.
        keeps   =  0.
        zlm     =  -1d0 * length
!
        if (x.ge.0.) then
!
!++  Particle hits collimator and we assume interaction length ZLM equal
!++  to collimator length (what if it would leave collimator after
!++  small length due to angle???)
!
                zlm = length
                impact(j) = x
                indiv(j) = xp
        else if (xp.le.0.) then
!++  Particle does not hit collimator. Interaction length ZLM is zero.
                zlm = 0d0
        else
!++  Calculate s-coordinate of interaction point
                s = (-1d0*x) / xp
                if (s.le.0) then
                        write(*,*) 'S.LE.0 -> This should not happen'
                        stop
                endif
                if (s .lt. length) then
                        zlm = length - s
                        impact(j) = 0d0
                        indiv(j) = xp
                else
                        zlm = 0d0
                endif
        endif
!++  First do the drift part
        drift_length = length - zlm
        if (drift_length.gt.0.) then
                x  = x + xp* drift_length
                z  = z + zp * drift_length
                sp = sp + drift_length
        endif
!++  Now do the scattering part
        if (zlm.gt.0.) then
                s_impact = sp
                nhit = nhit + 1
                call jaw(s, nabs)
!JUNE2005 SR+GRD: CREATE A FILE TO CHECK THE VALUES OF IMPACT PARAMETERS
!     SR, 29-08-2005: Add to the longitudinal coordinates the position
!     of the slice beginning 
                if(dowrite_impact) then
                        if(flagsec(j).eq.0) then
                                write(39,*)               
     &                          name(j),iturn,icoll,nabs,               &
     &                          s_impact + (dble(j_slices)-1)* c_length,&
     &                          s+sp + (dble(j_slices)-1) * c_length,   &
     &                          xinn,xpinn,yinn,ypinn,                  &
     &                          x,xp,z,zp
                        endif
                endif
                lhit(j) = 100000000*ie + iturn
!++  If particle is absorbed then set x and y to 99.99 mm
!     SR: before assigning new (x,y) for nabs=1, write the
!     inelastic impact file .
                if (nabs.eq.1) then
                        if (tiltangle.gt.0.) then
                                x  = x  + tiltangle*(s+sp)
                                xp = xp + tiltangle
                        elseif (tiltangle.lt.0.) then
                                xp = xp + tiltangle
                                x  = x - sin(tiltangle)* (length-(s+sp))
                        endif
                        x = x + c_aperture/2d0 + mirror*c_offset
                        x    = mirror * x
                        xp   = mirror * xp
                        x_flk  = x  *cos(-1d0*c_rotation) +             &
     &                  z  *sin(-1d0*c_rotation)
                        y_flk  = z  *cos(-1d0*c_rotation) -             &
     &                  x  *sin(-1d0*c_rotation)
                        xp_flk = xp *cos(-1d0*c_rotation) +             &
     &                  zp *sin(-1d0*c_rotation)
                        yp_flk = zp *cos(-1d0*c_rotation) -             &
     &                  xp *sin(-1d0*c_rotation)
!     SR, 29-08-2005: Include the slice numer!
                        if(dowrite_impact) then
                        write(48,'(i4,(1x,f6.3),(1x,f8.6),4(1x,e19.10), &
     &                  i2,2(1x,i7))')                                  &
     &                  icoll,c_rotation,                               &
     &                  s + sp + (dble(j_slices)-1) * c_length,         &
     &                  x_flk*1d3, xp_flk*1d3, y_flk*1d3, yp_flk*1d3,   &
     &                  nabs,name(j),iturn
                        write(866,*)
     &                  name(j), iturn, icoll, bool_proc(j)
                        endif
!     Finally, the actual coordinate change to 99 mm
                        fracab = fracab + 1
                        x = 99.99d-3
                        z = 99.99d-3
                        part_abs(j) = 100000000*ie + iturn
                        lint(j) = zlm
                endif
        endif
!
!++  Do the rest drift, if particle left collimator early
!
        if (nabs.ne.1 .and. zlm.gt.0.) then
                drift_length = (length-(s+sp))
                if (drift_length.gt.1d-15) then
                        x  = x + xp * drift_length
                        z  = z + zp * drift_length
                        sp = sp + drift_length
                endif
                lint(j) = zlm - drift_length
        endif
!
!++  Transform back to particle coordinates with opening and offset
        if (x.lt.99.0d-3) then
!++  Include collimator tilt
                if (tiltangle.gt.0.) then
                        x  = x  + tiltangle*c_length
                        xp = xp + tiltangle
                elseif (tiltangle.lt.0.) then
                        x  = x + tiltangle*c_length
                        xp = xp + tiltangle
                        x  = x - sin(tiltangle) * c_length
                endif
!++  Transform back to particle coordinates with opening and offset
                z00 = z
                x00 = x + mirror*c_offset
                x = x + c_aperture/2d0 + mirror*c_offset
!++  Now mirror at the horizontal axis for negative X offset
                x    = mirror * x
                xp   = mirror * xp
!++  Last do rotation into collimator frame
!
                x_in(j)  = x  *cos(-1d0*c_rotation) +                   &
     &          z  *sin(-1d0*c_rotation)
                y_in(j)  = z  *cos(-1d0*c_rotation) -                   &
     &          x  *sin(-1d0*c_rotation)
                xp_in(j) = xp *cos(-1d0*c_rotation) +                   &
     &          zp *sin(-1d0*c_rotation)
                yp_in(j) = zp *cos(-1d0*c_rotation) -                   &
     &          xp *sin(-1d0*c_rotation)
!
                if ( (icoll.eq.ipencil                                  &
     &          .and. iturn.eq.1)   .or.                                &
     &          (iturn.eq.1 .and. ipencil.eq.999 .and.                  &
     &          icoll.le.nprim .and.                                    &
     &          (j.ge.(icoll-1)*nev/nprim) .and.                        &
     &          (j.le.(icoll)*nev/nprim)                                &
     &          )  ) then
!
                        x00  = mirror * x00
                        x_in(j)  = x00  *cos(-1d0*c_rotation) +         &
     &                  z00  *sin(-1d0*c_rotation)
                        y_in(j)  = z00  *cos(-1d0*c_rotation) -         &
     &                  x00  *sin(-1d0*c_rotation)
!
                        xp_in(j) = xp_in(j) + mirror*xp_pencil0
                        yp_in(j) = yp_in(j) + mirror*yp_pencil0
                        x_in(j) = x_in(j) + mirror*x_pencil(icoll)
                        y_in(j) = y_in(j) + mirror*y_pencil(icoll)
                endif
                p_in(j) = (1d0 + dpop) * p0
!     SR, 30-08-2005: add the initial position of the slice
                s_in(j) = sp + (dble(j_slices)-1) * c_length
        else
                x_in(j)  = x
                y_in(j)  = z
        endif
 777  end do
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!      WRITE(*,*) 'Number of particles:            ', Nev
!      WRITE(*,*) 'Number of particle hits:        ', Nhit
!      WRITE(*,*) 'Number of absorped particles:   ', fracab
!      WRITE(*,*) 'Number of escaped particles:    ', Nhit-fracab
!      WRITE(*,*) 'Fraction of absorped particles: ', 100.*fracab/Nhit
!
      end
!
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
!
      subroutine collimaterhic(c_material, c_length, c_rotation,        &
!JUNE2005
     &c_aperture, n_aperture,                                           &
!JUNE2005
     &c_offset, c_tilt,                                                 &
     &x_in, xp_in, y_in,                                                &
     &yp_in, p_in, s_in, np, enom, lhit,                                &
!     &part_abs, impact, indiv, lint, onesided)
     &part_abs, impact, indiv, lint, onesided,                          &
     &name)
!
!++  Based on routines by JBJ. Changed by RA 2001.
!
!++  - Deleted all HBOOK stuff.
!++  - Deleted optics routine and all parser routines.
!++  - Replaced RANMAR call by RANLUX call
!++  - Included RANLUX code from CERNLIB into source
!++  - Changed dimensions from CGen(100,nmat) to CGen(200,nmat)
!++  - Replaced FUNPRE with FUNLXP
!++  - Replaced FUNRAN with FUNLUX
!++  - Included all CERNLIB code into source: RANLUX, FUNLXP, FUNLUX,
!++                                         FUNPCT, FUNLZ, RADAPT,
!++                                           RGS56P
!++    with additional entries:             RLUXIN, RLUXUT, RLUXAT,
!++                                           RLUXGO
!++
!++  - Changed program so that Nev is total number of particles
!++    (scattered and not-scattered)
!++  - Added debug comments
!++  - Put real dp/dx
!
      implicit none
!
      double precision sx, sz
!
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
! THIS BLOCK IS COMMON TO THIN6D, TRAUTHIN, COLLIMATE32 AND MAINCR
!
      integer ipencil
      double precision xp_pencil0,yp_pencil0,x_pencil(max_ncoll),       &
     &y_pencil(max_ncoll),pencil_dx(max_ncoll)
      common  /pencil/  xp_pencil0,yp_pencil0,pencil_dx,ipencil
      common  /pencil2/ x_pencil, y_pencil
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      integer ie,iturn,nabs_total
      common  /info/ ie,iturn,nabs_total
!
!
      logical onesided,hit
      integer nprim,filel,mat,nev,j,nabs,nhit,np,icoll
!MAY2005
!      integer lhit(npart),part_abs(npart)
      integer lhit(npart),part_abs(npart),name(npart)
!MAY2005
      double precision p0,xmin,xmax,xpmin,xpmax,zmin,zmax,zpmin,zpmax   &
     &,length,zlm,x,x00,xp,z,z00,zp,p,sp,dpop,s,enom,x_in(npart),       &
     &xp_in(npart),y_in(npart),yp_in(npart),p_in(npart),s_in(npart),    &
     &indiv(npart),lint(npart),x_out(max_npart),xp_out(max_npart),      &
     &y_out(max_npart),yp_out(max_npart),p_out(max_npart),              &
     &s_out(max_npart),keeps,fracab,mybetax,mybetaz,mymux,mymuz,sigx,   &
     &sigz,norma,xpmu,atdi,drift_length,mirror,tiltangle,impact(npart)
!
      double precision c_length    !length in m
      double precision c_rotation  !rotation angle vs vertical in radian
      double precision c_aperture  !aperture in m
      double precision c_offset    !offset in m
      double precision c_tilt(2)   !tilt in radian
      character*6      c_material  !material
!
!
!
      character*(nc) filen,tit
!
      real   rndm4,xlow,xhigh,xplow,xphigh,dx,dxp
!
      common /cmom/xmin,xmax,xpmin,xpmax,zmin,zmax,zpmin,zpmax,length,  &
     &nev
      common /materia/mat
      common /phase/x,xp,z,zp,dpop
      common /nommom/p0
      common /cjaw1/zlm
      common /other/mybetax,mybetaz,mymux,mymuz,atdi
      common /icoll/  icoll
!
      data   dx,dxp/.5d-4,20.d-4/
!
!
!
!GRD
!GRD THIS BLOC IS COMMON TO MAINCR, DATEN, TRAUTHIN AND THIN6D
!GRD
!APRIL2005
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
     &,relative, diffusive
!     &systilt_antisymm,dowritetracks,cern
!APRIL2005
!
!      integer nloop,rnd_seed,ibeam,jobnumber,sigsecut2
!JUNE2005
!      integer nloop,rnd_seed,ibeam,jobnumber
!SEPT2005 for slicing process
!      integer nloop,rnd_seed,ibeam,jobnumber,do_thisdis
      integer nloop,rnd_seed,c_offsettilt_seed,ibeam,jobnumber,         &
     &do_thisdis,n_slices,pencil_distr
!JUNE2005
!
!UPGRADE JANUARY 2005
!APRIL2005
!      double precision myenom,mynex,mdex,myney,mdey,nsig_prim,nsig_sec, &
!     &nsig_ter,emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,         &
      double precision myenom,mynex,mdex,myney,mdey,                    &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
!SEPT2005 add these lines for the slicing procedure
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
!SEPT2005,OCT2006 added offset
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,ndr,                            &
     &driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,            &
     &sigsecut3,sigsecut2,enerror,bunchlength
!JUNE2005
!APRIL2005
!
      character*24 name_sel
      character*80 coll_db
      character*16 castordir
!JUNE2005
      character*80 filename_dis
!JUNE2005
!
!UPGRADE JANUARY 2005
!APRIL2005
!JUNE2005
      common /grd/ myenom,mynex,mdex,myney,mdey,                        &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
!
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
!
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,                                &
     &ndr,driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,        &
     & relative,diffusive,
     &sigsecut3,sigsecut2,enerror,                                      &
     &bunchlength,coll_db,name_sel,                                     &
     &castordir,filename_dis,nloop,rnd_seed,c_offsettilt_seed,          &
     &ibeam,jobnumber,do_thisdis,n_slices,pencil_distr,                 &
     &do_coll,                                                          &
!
     &do_select,do_nominal,dowrite_dist,do_oneside,dowrite_impact,      &
     &dowrite_secondary,dowrite_amplitude,radial,systilt_antisymm,      &
     &dowritetracks,cern,do_nsig,do_mingap
!SEPT2005
!JUNE2005
!APRIL2005
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
      double precision x_flk,xp_flk,y_flk,yp_flk
!JUNE2005
      double precision n_aperture  !aperture in m for the vertical plane
!JUNE2005
!DEBUG
      integer event
!DEBUG
      save
!=======================================================================
! Be=1 Al=2 Cu=3 W=4 Pb=5
!
! LHC uses:    Al, 0.2 m
!              Cu, 1.0 m
!
!      write(*,*) 'enter collimateRHIC routine'
      if (c_material.eq.'BE') then
         mat = 1
      elseif (c_material.eq.'AL') then
         mat = 2
      elseif (c_material.eq.'CU') then
         mat = 3
      elseif (c_material.eq.'W') then
         mat = 4
      elseif (c_material.eq.'PB') then
         mat = 5
      elseif (c_material.eq.'C') then
         mat = 6
      elseif (c_material.eq.'C2') then
         mat = 7
      else
         write(*,*) 'ERR>  Material not found. STOP (TW)', c_material
!        STOP
      endif
!
        length  = c_length
        nev = np
        p0  = enom
!
!++  Initialize scattering processes
!
      call scatin(p0)
 
! EVENT LOOP,  initial distribution is here a flat distribution with
! xmin=x-, xmax=x+, etc. from the input file
!
      nhit    = 0
      fracab  = 0.
      mirror  = 1.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      do j = 1, nev
!
        impact(j) = -1.
        lint(j)   = -1.
        indiv(j)  = -1.
!
        x   = x_in(j)
        xp  = xp_in(j)
        z   = y_in(j)
        zp  = yp_in(j)
        p   = p_in(j)
!        sp  = s_in(J)
        sp   = 0.
        dpop = (p - p0)/p0
!
!++  Transform particle coordinates to get into collimator coordinate
!++  system
!
!++  First check whether particle was lost before
!
!        if (x.lt.99.0*1e-3 .and. z.lt.99.0*1e-3) then
        if (x.lt.99.0*1d-3 .and. z.lt.99.0*1d-3) then
!
!++  First do rotation into collimator frame
!
!JUNE2005
!JUNE2005 CHANGE TO MAKE THE RHIC TREATMENT EASIER...
!JUNE2005
!+if crlibm
!          x  = x_in(j)*cos_rn(c_rotation) +sin_rn(c_rotation)*y_in(j)
!+ei
!+if .not.crlibm
!          x  = x_in(j)*cos(c_rotation) +sin(c_rotation)*y_in(j)
!+ei
!+if crlibm
!          z  = y_in(j)*cos_rn(c_rotation) -sin_rn(c_rotation)*x_in(j)
!+ei
!+if .not.crlibm
!          z  = y_in(j)*cos(c_rotation) -sin(c_rotation)*x_in(j)
!+ei
!+if crlibm
!          xp = xp_in(j)*cos_rn(c_rotation)+sin_rn(c_rotation)*yp_in(j)
!+ei
!+if .not.crlibm
!          xp = xp_in(j)*cos(c_rotation)+sin(c_rotation)*yp_in(j)
!+ei
!+if crlibm
!          zp = yp_in(j)*cos_rn(c_rotation)-sin_rn(c_rotation)*xp_in(j)
!+ei
!+if .not.crlibm
!          zp = yp_in(j)*cos(c_rotation)-sin(c_rotation)*xp_in(j)
!+ei
          x  = -1d0*x_in(j)
          z  = -1d0*y_in(j)
          xp = -1d0*xp_in(j)
          zp = -1d0*yp_in(j)
!JUNE2005
!
!++  For one-sided collimators consider only positive X. For negative
!++  X jump to the next particle
!
!GRD          IF (ONESIDED .AND. X.LT.0) GOTO 777
!JUNE2005          if (onesided .and. x.lt.0d0 .or. z.gt.0d0) goto 777
          if (onesided .and. (x.lt.0d0 .and. z.gt.0d0)) goto 777
!
!++  Now mirror at the horizontal axis for negative X offset
!
!GRD
!GRD THIS WE HAVE TO COMMENT OUT IN CASE OF RHIC BECAUSE THERE ARE
!GRD ONLY ONE-SIDED COLLIMATORS
!GRD
!          IF (X.LT.0) THEN
!            MIRROR = -1.
!            tiltangle = -1.*C_TILT(2)
!          ELSE
!            MIRROR = 1.
            tiltangle = c_tilt(1)
!          ENDIF
!          X  = MIRROR * X
!          XP = MIRROR * XP
!GRD
!
!++  Shift with opening and offset
!
          x  = x - c_aperture/2 - mirror*c_offset
!GRD
!GRD SPECIAL FEATURE TO TAKE INTO ACCOUNT THE PARTICULAR SHAPE OF RHIC PRIMARY C
!GRD
!JUNE2005  HERE WE ADD THE ABILITY TO HAVE 2 DIFFERENT OPENINGS FOR THE TWO PLAN
!JUNE2005  OF THE PRIMARY COLLIMATOR OF RHIC
!JUNE2005
!          z  = z + c_aperture/2 + mirror*c_offset
          z  = z + n_aperture/2 + mirror*c_offset
!JUNE2005
!          if(iturn.eq.1)                                                &
!     &write(*,*) 'check ',x,xp,z,zp,c_aperture,n_aperture
!JUNE2005
!
!++  Include collimator tilt
!
          if (tiltangle.gt.0.) then
            xp = xp - tiltangle
          elseif (tiltangle.lt.0.) then
            x  = x + sin(tiltangle) * c_length
            xp = xp - tiltangle
          endif
!
!++  For selected collimator, first turn reset particle distribution
!++  to simple pencil beam
!
            nprim = 3
            if ( (icoll.eq.ipencil                                      &
     &.and. iturn.eq.1) .or.                                            &
     &(iturn.eq.1 .and. ipencil.eq.999 .and.                            &
     &icoll.le.nprim .and.                                              &
     &(j.ge.(icoll-1)*nev/nprim) .and.                                  &
     &(j.le.(icoll)*nev/nprim)                                          &
     &)  ) then
              x    = pencil_dx(icoll)
              xp   = 0.
              z    = 0.
              zp   = 0.
              dpop = 0.
              if(rndm4().lt.0.5) mirror = -abs(mirror)
              if(rndm4().ge.0.5) mirror = abs(mirror)
            endif
!
!++  particle passing above the jaw are discarded => take new event
!++  entering by the face, shorten the length (zlm) and keep track of
!++  entrance longitudinal coordinate (keeps) for histograms
!
!++  The definition is that the collimator jaw is at x>=0.
!
!++  1) Check whether particle hits the collimator
!
          hit     =  .false.
          s       =  0.
          keeps   =  0.
          zlm     =  -1.0d0 * length
!
!GRD
!JUNE2005          if (x.ge.0d0 .and. z.le.0d0) then
          if (x.ge.0d0 .and. z.le.0d0) then
             goto 10
!
!++  Particle hits collimator and we assume interaction length ZLM equal
!++  to collimator length (what if it would leave collimator after
!++  small length due to angle???)
!
!JUNE2005
!            zlm = length
!            impact(j) = max(x,(-1d0*z))
!            if(impact(j).eq.x) then
!               indiv(j) = xp
!            else
!               indiv(j) = zp
!            endif
!          endif
!JUNE2005
!GRD
!JUNE2005          if(x.lt.0d0.and.z.gt.0d0.and.xp.le.0d0.and.zp.ge.0d0) then
          elseif(x.lt.0d0.and.z.gt.0d0.and.xp.le.0d0                    &
     &.and.zp.ge.0d0) then
             goto 20
!GRD
!JUNE2005          if(x.lt.0d0.and.z.gt.0d0.and.xp.le.0d0.and.zp.ge.0d0) then
!
!++  Particle does not hit collimator. Interaction length ZLM is zero.
!
!JUNE2005            zlm = 0.
!JUNE2005          endif
!GRD
!JUNE2005          if (x.lt.0d0.and.z.gt.0d0.and.xp.gt.0d0.and.zp.ge.0d0) then
!JUNE2005
!            zlm = 0.
!          endif
!JUNE2005
!
!JUNE2005
!JUNE2005 THAT WAS PIECE OF CAKE; NOW COMES THE TRICKY PART...
!JUNE2005
!JUNE2005 THE IDEA WOULD BE TO FIRST LIST ALL THE IMPACT
!JUNE2005 POSSIBILITIES, THEN SEND VIA GOTO TO THE CORRECT
!JUNE2005 TREATMENT
!JUNE2005
          elseif((x.lt.0d0).and.(z.le.0d0)) then
             goto 100
          elseif((x.ge.0d0).and.(z.gt.0d0)) then
             goto 200
          elseif((x.lt.0d0).and.(xp.gt.0d0)) then
             goto 300
          elseif((z.gt.0d0).and.(zp.lt.0d0)) then
             goto 400
          endif
!GRD
 10         continue
            event = 10
            zlm = length
            impact(j) = max(x,(-1d0*z))
            if(impact(j).eq.x) then
               indiv(j) = xp
            else
               indiv(j) = zp
            endif
            goto 999
!GRD
 20         continue
            event = 20
            zlm = 0.
            goto 999
!GRD
 100        continue
            event = 100
            zlm = length
            impact(j) = -1d0*z
            indiv(j) = zp
            goto 999
!GRD
 200        continue
            event = 200
            zlm = length
            impact(j) = x
            indiv(j) = xp
            goto 999
!GRD
!JUNE2005
!JUNE2005 HERE ONE HAS FIRST TO CHECK IF THERE'S NOT A HIT IN THE
!JUNE2005 OTHER PLANE AT THE SAME TIME
!JUNE2005
 300        continue
            event = 300
            if(z.gt.0d0.and.zp.lt.0d0) goto 500
!
!++  Calculate s-coordinate of interaction point
!
            s = (-1.0d0*x) / xp
            if (s.le.0d0) then
              write(*,*) 'S.LE.0 -> This should not happen (1)'
              stop
            endif
!
            if (s .lt. length) then
              zlm = length - s
              impact(j) = 0.
              indiv(j) = xp
            else
              zlm = 0.
            endif
            goto 999
!GRD
 400        continue
            event = 400
!JUNE2005          if (x.lt.0d0.and.z.gt.0d0.and.xp.le.0d0.and.zp.lt.0d0) then
!
!++  Calculate s-coordinate of interaction point
!
            s = (-1.0d0*z) / zp
            if (s.le.0) then
              write(*,*) 'S.LE.0 -> This should not happen (2)'
              stop
            endif
!
            if (s .lt. length) then
              zlm = length - s
              impact(j) = 0.
              indiv(j) = zp
            else
              zlm = 0.
            endif
!JUNE2005          endif
!GRD
            goto 999
!GRD
!GRD
!JUNE2005          if (x.lt.0d0.and.z.gt.0d0.and.xp.gt.0d0.and.zp.lt.0d0) then
 500        continue
            event = 500
!
!++  Calculate s-coordinate of interaction point
!
            sx = (-1.0d0*x) / xp
            sz = (-1.0d0*z) / zp
!
            if(sx.lt.sz) s=sx
            if(sx.ge.sz) s=sz
!
            if (s.le.0d0) then
              write(*,*) 'S.LE.0 -> This should not happen (3)'
              stop
            endif
!
            if (s .lt. length) then
              zlm = length - s
              impact(j) = 0.
              if(s.eq.sx) then
                indiv(j) = xp
              else
                indiv(j) = zp
              endif
            else
              zlm = 0.
            endif
!
!JUNE2005          endif
!GRD
!GRD
 999      continue
!JUNE2005
!          write(*,*) 'event ',event,x,xp,z,zp
!          if(impact(j).lt.0d0) then
!             if(impact(j).ne.-1d0)                                      &
!     &write(*,*) 'argh! ',impact(j),x,xp,z,zp,s,event
!          endif
!          if(impact(j).ge.0d0) then
!      write(*,*) 'impact! ',impact(j),x,xp,z,zp,s,event
!          endif
!JUNE2005
!
!++  First do the drift part
!
          drift_length = length - zlm
          if (drift_length.gt.0.) then
            x  = x + xp* drift_length
            z  = z + zp * drift_length
            sp = sp + drift_length
          endif
!
!++  Now do the scattering part
!
          if (zlm.gt.0.) then
            nhit = nhit + 1
!            WRITE(*,*) J,X,XP,Z,ZP,SP,DPOP
!DEBUG
!            write(*,*) 'abs?',s,zlm
!DEBUG
!JUNE2005
!JUNE2005 IN ORDER TO HAVE A PROPER TREATMENT IN THE CASE OF THE VERTICAL
!JUNE2005 PLANE, CHANGE AGAIN THE FRAME FOR THE SCATTERING SUBROUTINES...
!JUNE2005
            if(event.eq.100.or.event.eq.400) then
!GRD first go back into normal frame...
               x = x + c_aperture/2 + mirror*c_offset
               z = z - n_aperture/2 - mirror*c_offset
               x = -1d0*x
               xp = -1d0*xp
               z = -1d0*z
               zp = -1d0*zp
!GRD ...then do as for a vertical collimator
               x = z
               xp = zp
               z = -1d0*x
               zp = -1d0*x
               x  = x - n_aperture/2 - mirror*c_offset
               z  = z + c_aperture/2 + mirror*c_offset
            endif
!JUNE2005
            call jaw(s, nabs)
!DEBUG
!            write(*,*) 'abs?',nabs
!DEBUG
!JUNE2005
!JUNE2005 ...WITHOUT FORGETTING TO GO BACK TO THE "ORIGINAL" FRAME AFTER THE
!JUNE2005 ROUTINES, SO AS TO AVOID RIDICULOUS VALUES FOR KICKS IN EITHER PLANE
            if(event.eq.100.or.event.eq.400) then
!GRD first go back into normal frame...
               x = x + n_aperture/2 + mirror*c_offset
               z = z - c_aperture/2 - mirror*c_offset
               x = -1d0*z
               xp = -1d0*zp
               z = x
               zp = xp
!GRD ...then go back to face the horizontal jaw at 180 degrees
               x = -1d0*x
               xp = -1d0*xp
               z = -1d0*z
               zp = -1d0*zp
               x  = x - c_aperture/2 - mirror*c_offset
               z  = z + n_aperture/2 + mirror*c_offset
            endif
!JUNE2005
            lhit(j) = 100000000*ie + iturn
!
!++  If particle is absorbed then set x and y to 99.99 mm
!
            if (nabs.eq.1) then
!APRIL2005
!TO WRITE FLUKA INPUT CORRECTLY, WE HAVE TO GO BACK IN THE MACHINE FRAME
            if (tiltangle.gt.0.) then
              x  = x  + tiltangle*c_length
              xp = xp + tiltangle
            elseif (tiltangle.lt.0.) then
              x  = x + tiltangle*c_length
              xp = xp + tiltangle
!
              x  = x - sin(tiltangle) * c_length
            endif
!
!++  Transform back to particle coordinates with opening and offset
!
            x = x + c_aperture/2 + mirror*c_offset
!GRD
!JUNE2005  OF COURSE WE ADAPT ALSO THE PREVIOUS CHANGE WHEN SHIFTING BACK
!JUNE2005  TO  THE ACCELERATOR FRAME...
!            z = z - c_aperture/2 - mirror*c_offset
            z = z - n_aperture/2 - mirror*c_offset
!JUNE2005
!
!++   Last do rotation into collimator frame
!
                  x_flk  = -1d0*x
                  y_flk  = -1d0*z
                  xp_flk = -1d0*xp
                  yp_flk = -1d0*zp
!NOW WE CAN WRITE THE COORDINATES OF THE LOST PARTICLES
              if(dowrite_impact) then
      write(48,'(i4,(2x,f5.3),(2x,f8.6),4(1x,e16.7),2x,i2,2x,i5)')      &
     &icoll,c_rotation,s+sp,                                            &
     &x_flk*1d3, xp_flk*1d3, y_flk*1d3, yp_flk*1d3,                     &
     &nabs,name(j)
              endif
!APRIL2005
              fracab = fracab + 1
!              x = 99.99*1e-3
!              z = 99.99*1e-3
              x = 99.99*1.0d-3
              z = 99.99*1.0d-3
              part_abs(j) = 100000000*ie + iturn
              lint(j) = zlm
            endif
          endif
!
!++  Do the rest drift, if particle left collimator early
!
          if (nabs.ne.1 .and. zlm.gt.0.) then
            drift_length = (length-(s+sp))
!            if (drift_length.gt.1.e-15) then
            if (drift_length.gt.1.0d-15) then
!              WRITE(*,*) J, DRIFT_LENGTH
              x  = x + xp * drift_length
              z  = z + zp * drift_length
              sp = sp + drift_length
            endif
            lint(j) = zlm - drift_length
          endif
!
!++  Transform back to particle coordinates with opening and offset
!
!          if (x.lt.99.0*1e-3 .and. z.lt.99.0*1e-3) then
          if (x.lt.99.0*1d-3 .and. z.lt.99.0*1d-3) then
!
!++  Include collimator tilt
!
            if (tiltangle.gt.0.) then
              x  = x  + tiltangle*c_length
              xp = xp + tiltangle
            elseif (tiltangle.lt.0.) then
              x  = x + tiltangle*c_length
              xp = xp + tiltangle
!
              x  = x - sin(tiltangle) * c_length
            endif
!
!++  Transform back to particle coordinates with opening and offset
!
            z00 = z
            x00 = x + mirror*c_offset
            x = x + c_aperture/2 + mirror*c_offset
!GRD
!JUNE2005  OF COURSE WE ADAPT ALSO THE PREVIOUS CHANGE WHEN SHIFTING BACK
!JUNE2005  TO  THE ACCELERATOR FRAME...
!            z = z - c_aperture/2 - mirror*c_offset
            z = z - n_aperture/2 - mirror*c_offset
!JUNE2005
!
!++  Now mirror at the horizontal axis for negative X offset
!
            x    = mirror * x
            xp   = mirror * xp
!
!++  Last do rotation into collimator frame
!
!JUNE2005
!+if crlibm
!            x_in(j)  = x  *cos_rn(-1.*c_rotation) +                     &
!+ei
!+if .not.crlibm
!            x_in(j)  = x  *cos(-1.*c_rotation) +                        &
!+ei
!+if crlibm
!     &z  *sin_rn(-1.*c_rotation)
!+ei
!+if .not.crlibm
!     &z  *sin(-1.*c_rotation)
!+ei
!+if crlibm
!            y_in(j)  = z  *cos_rn(-1.*c_rotation) -                     &
!+ei
!+if .not.crlibm
!            y_in(j)  = z  *cos(-1.*c_rotation) -                        &
!+ei
!+if crlibm
!     &x  *sin_rn(-1.*c_rotation)
!+ei
!+if .not.crlibm
!     &x  *sin(-1.*c_rotation)
!+ei
!+if crlibm
!            xp_in(j) = xp *cos_rn(-1.*c_rotation) +                     &
!+ei
!+if .not.crlibm
!            xp_in(j) = xp *cos(-1.*c_rotation) +                        &
!+ei
!+if crlibm
!     &zp *sin_rn(-1.*c_rotation)
!+ei
!+if .not.crlibm
!     &zp *sin(-1.*c_rotation)
!+ei
!+if crlibm
!            yp_in(j) = zp *cos_rn(-1.*c_rotation) -                     &
!+ei
!+if .not.crlibm
!            yp_in(j) = zp *cos(-1.*c_rotation) -                        &
!+ei
!+if crlibm
!     &xp *sin_rn(-1.*c_rotation)
!+ei
!+if .not.crlibm
!     &xp *sin(-1.*c_rotation)
!+ei
            x_in(j) = -1d0*x
            y_in(j) = -1d0*z
            xp_in(j) = -1d0*xp
            yp_in(j) = -1d0*zp
!JUNE2005
!
            if ( (icoll.eq.ipencil                                      &
     &.and. iturn.eq.1)   .or.                                          &
     &(iturn.eq.1 .and. ipencil.eq.999 .and.                            &
     &icoll.le.nprim .and.                                              &
     &(j.ge.(icoll-1)*nev/nprim) .and.                                  &
     &(j.le.(icoll)*nev/nprim)                                          &
     &)  ) then
!
               x00  = mirror * x00
               x_in(j)  = x00  *cos(-1.*c_rotation) +                   &
     &z00  *sin(-1.*c_rotation)
               y_in(j)  = z00  *cos(-1.*c_rotation) -                   &
     &x00  *sin(-1.*c_rotation)
!
               xp_in(j) = xp_in(j) + mirror*xp_pencil0
               yp_in(j) = yp_in(j) + mirror*yp_pencil0
               x_in(j) = x_in(j) + mirror*x_pencil(icoll)
               y_in(j) = y_in(j) + mirror*y_pencil(icoll)
            endif
!
            p_in(j) = (1 + dpop) * p0
            s_in(j) = s_in(j) + sp
!
          else
            x_in(j)  = x
            y_in(j)  = z
          endif
!
!++  End of check for particles not being lost before
!
        endif
!
!        IF (X.GT.99.00) WRITE(*,*) 'After : ', X, X_IN(J)
!
!++  End of loop over all particles
!
 777  end do
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!      WRITE(*,*) 'Number of particles:            ', Nev
!      WRITE(*,*) 'Number of particle hits:        ', Nhit
!      WRITE(*,*) 'Number of absorped particles:   ', fracab
!      WRITE(*,*) 'Number of escaped particles:    ', Nhit-fracab
!      WRITE(*,*) 'Fraction of absorped particles: ', 100.*fracab/Nhit
!
      end
!
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
!
      subroutine makedis(mynp, myalphax, myalphay, mybetax, mybetay,    &
     &myemitx0, myemity0, myenom, mynex, mdex, myney, mdey,             &
     &myx, myxp, myy, myyp, myp, mys)
!
!  Generate distribution
!
      implicit none
!
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
!++ Vectors of coordinates
!
      logical cut_input
      integer i,j,mynp,nloop
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn),myalphax,mybetax,myemitx0,myemitx,mynex,mdex, &
     &mygammax,myalphay,mybetay,myemity0,myemity,myney,mdey,mygammay,   &
     &xsigmax,ysigmay,myenom,nr,ndr
!
!
      real      rndm4
!
!
      character*80   dummy
!
!
      common /cut/ cut_input
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      double precision pi
!
      save
!-----------------------------------------------------------------------
!++  Generate particle distribution
!
!
!++  Generate random distribution, assuming optical parameters at IP1
!
!
!++  Calculate the gammas
!
      pi=4d0*atan(1d0)
      mygammax = (1d0+myalphax**2)/mybetax
      mygammay = (1d0+myalphay**2)/mybetay
!++TW 11/07 reset j, helps if subroutine is called twice 
! was done during try to reset distribution, still needed
! will this subroutine ever called twice? 
      j = 0
!
!
!++  Number of points and generate distribution
!
      write(*,*)
      write(*,*) 'Generation of particle distribution Version 1'
      write(*,*)
      write(*,*) 'This routine generates particles in phase space'
      write(*,*) 'X/XP and Y/YP ellipses, as defined in the input'
      write(*,*) 'parameters. Distribution is flat in the band.'
      write(*,*) 'X and Y are fully uncorrelated.'
      write(*,*)
!
      write(outlun,*)
      write(outlun,*) 'Generation of particle distribution Version 1'
      write(outlun,*)
      write(outlun,*) 'This routine generates particles in phase space'
      write(outlun,*) 'X/XP and Y/YP ellipses, as defined in the input'
      write(outlun,*) 'parameters. Distribution is flat in the band.'
      write(outlun,*) 'X and Y are fully uncorrelated.'
      write(outlun,*)
      write(outlun,*) 'INFO>  Number of particles   = ', mynp
      write(outlun,*) 'INFO>  Av number of x sigmas = ', mynex
      write(outlun,*) 'INFO>  +- spread in x sigmas = ', mdex
      write(outlun,*) 'I0NFO>  Av number of y sigmas = ', myney
      write(outlun,*) 'INFO>  +- spread in y sigmas = ', mdey
      write(outlun,*) 'INFO>  Nominal beam energy   = ', myenom
      write(outlun,*) 'INFO>  Sigma_x0 = ', sqrt(mybetax*myemitx0)
      write(outlun,*) 'INFO>  Sigma_y0 = ', sqrt(mybetay*myemity0)
      write(outlun,*) 'INFO>  Beta x   = ', mybetax
      write(outlun,*) 'INFO>  Beta y   = ', mybetay
      write(outlun,*) 'INFO>  Alpha x  = ', myalphax
      write(outlun,*) 'INFO>  Alpha y  = ', myalphay
      write(outlun,*) 'INFO>  DISP x  = '
      write(outlun,*) 'INFO>  DISP y  = '
!
      do while (j.lt.mynp)
!
        j = j + 1
        myemitx = myemitx0*(mynex + (2d0*dble(rndm4()-0.5)*mdex) )**2
        xsigmax = sqrt(mybetax*myemitx)
        myx(j)   = xsigmax * sin(2d0*pi*rndm4())
        if (rndm4().gt.0.5) then
          myxp(j)  = sqrt(myemitx/mybetax-myx(j)**2/mybetax**2)-        &
     &myalphax*myx(j)/mybetax
        else
          myxp(j)  = -1*sqrt(myemitx/mybetax-myx(j)**2/mybetax**2)-     &
     &myalphax*myx(j)/mybetax
        endif
!
        myemity = myemity0*(myney + (2d0*dble(rndm4()-0.5)*mdey) )**2
        ysigmay = sqrt(mybetay*myemity)
        myy(j)   = ysigmay * sin(2d0*pi*rndm4())
        if (rndm4().gt.0.5) then
          myyp(j)  = sqrt(myemity/mybetay-myy(j)**2/mybetay**2)-        &
     &myalphay*myy(j)/mybetay
        else
          myyp(j)  = -1*sqrt(myemity/mybetay-myy(j)**2/mybetay**2)-     &
     &myalphay*myy(j)/mybetay
        endif
!
!APRIL2005 TEST FOR FATS FLAG
        myp(j)   = myenom
!        if(j.eq.1) then
!          myp(j)   = myenom*(1-0.01)
!!       do j=2,mynp
!        else
!          myp(j) = myp(1) + (j-1)*2d0*0.01*myenom/(mynp-1)
!        endif
!APRIL2005 END OF TEST SECTION
        mys(j)   = 0d0
!
!++  Dangerous stuff, just for the moment
!
        if (cut_input) then
          if ( (.not. (myy(j).lt.-.008d-3 .and. myyp(j).lt.0.1d-3 .and. &
     &myyp(j).gt.0d0) ) .and.                                           &
     &(.not. (myy(j).gt..008d-3 .and. myyp(j).gt.-0.1d-3 .and.          &
     &myyp(j).lt.0d0) ) ) then
            j = j - 1
          endif
        endif
!
      end do
!
      return
      end
!
!========================================================================
!
! SR, 08-05-2005: Add the finite beam size in the other dimension
      subroutine makedis_st(mynp, myalphax, myalphay, mybetax, mybetay, &
     &     myemitx0, myemity0, myenom, mynex, mdex, myney, mdey,        &
     &     myx, myxp, myy, myyp, myp, mys)
 
!     Uses the old routine 'MAKEDIS' for the halo plane and adds the
!     transverse beam size in the other plane (matched distrubutions
!     are generated starting from thetwiss functions).
!     If 'mynex' and 'myney' are BOTH set to zero, nominal bunches
!     centred in the aperture centre are generated. (SR, 08-05-2005)
!
      implicit none
!
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
!++ Vectors of coordinates
!
      logical cut_input
      integer i,j,mynp,nloop
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn),myalphax,mybetax,myemitx0,myemitx,mynex,mdex, &
     &mygammax,myalphay,mybetay,myemity0,myemity,myney,mdey,mygammay,   &
     &xsigmax,ysigmay,myenom,nr,ndr
!
!
      real      rndm4
!
!
      character*80   dummy
!
!
      common /cut/ cut_input
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      double precision pi
!
      double precision iix, iiy, phix, phiy
!
      save
!
!-----------------------------------------------------------------------
!++  Generate particle distribution
!
!
!++  Generate random distribution, assuming optical parameters at IP1
!
!++  Calculate the gammas
!
      write(*,*) '  New routine to add the finite beam size in the'
      write(*,*) '  other dimension (SR, 08-06-2005).'
 
      pi=4d0*atan(1d0)
!
      mygammax = (1d0+myalphax**2)/mybetax
      mygammay = (1d0+myalphay**2)/mybetay
!
      do j=1, mynp
         if ((mynex.gt.0d0).and.(myney.eq.0d0)) then
            myemitx = myemitx0*(mynex+(2d0*dble(rndm4()-0.5)*mdex))**2
            xsigmax = sqrt(mybetax*myemitx)
            myx(j)   = xsigmax * sin(2d0*pi*rndm4())
            if (rndm4().gt.0.5) then
              myxp(j) = sqrt(myemitx/mybetax-myx(j)**2/mybetax**2)-     &
     &              myalphax*myx(j)/mybetax
            else
              myxp(j) = -1d0*sqrt(myemitx/mybetax-myx(j)**2/mybetax**2)-&
     &              myalphax*myx(j)/mybetax
            endif
!
            phiy = 2*pi*rndm4()
!
            iiy = -1d0*myemity0 * log( rndm4() )
!
            myy(j) = sqrt(2*iiy*mybetay) * cos(phiy)
            myyp(j) = -1d0*sqrt(2*iiy/mybetay) * (sin(phiy) +           &
     &           myalphay * cos(phiy))
         elseif ( mynex.eq.0.and.myney.gt.0 ) then
            myemity = myemity0*(myney+(2d0*dble(rndm4()-0.5)*mdey))**2
            ysigmay = sqrt(mybetay*myemity)
            myy(j)   = ysigmay * sin(2d0*pi*rndm4())
            if (rndm4().gt.0.5) then
              myyp(j) = sqrt(myemity/mybetay-myy(j)**2/mybetay**2)-     &
     &              myalphay*myy(j)/mybetay
            else
              myyp(j) = -1d0*sqrt(myemity/mybetay-myy(j)**2/mybetay**2)-&
     &              myalphay*myy(j)/mybetay
            endif
!
            phix = 2*pi*rndm4()
            iix = - myemitx0 * log( rndm4() )
!
            myx(j) = sqrt(2*iix*mybetax) * cos(phix)
            myxp(j) = -1d0*sqrt(2*iix/mybetax) * (sin(phix) +           &
     &           myalphax * cos(phix))
         elseif ( mynex.eq.0.and.myney.eq.0 ) then
            phix = 2*pi*rndm4()
            iix = - myemitx0 * log( rndm4() )
!
            myx(j) = sqrt(2*iix*mybetax) * cos(phix)
            myxp(j) = -1d0*sqrt(2*iix/mybetax) * (sin(phix) +           &
     &           myalphax * cos(phix))
            phiy = 2*pi*rndm4()
            iiy = - myemity0 * log( rndm4() )
!
            myy(j) = sqrt(2*iiy*mybetay) * cos(phiy)
            myyp(j) = -1d0*sqrt(2*iiy/mybetay) * (sin(phiy) +           &
     &           myalphay * cos(phiy))
         else
            write(*,*) "Error - beam parameters not correctly set!"
         endif
!
         myp(j)   = myenom
         mys(j)   = 0d0
!
      end do
!
      return
      end
!
!========================================================================
!
! SR, 09-05-2005: Add the energy spread and the finite bunch length.
!                 Gaussian distributions assumed
      subroutine makedis_de(mynp, myalphax, myalphay, mybetax, mybetay, &
     &     myemitx0, myemity0, myenom, mynex, mdex, myney, mdey,        &
     &     myx, myxp, myy, myyp, myp, mys,                              &
     &     enerror,bunchlength)
 
!     Uses the old routine 'MAKEDIS' for the halo plane and adds the
!     transverse beam size in the other plane (matched distrubutions
!     are generated starting from thetwiss functions).
!     If 'mynex' and 'myney' are BOTH set to zero, nominal bunches
!     centred in the aperture centre are generated. (SR, 08-05-2005)
      implicit none
!
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
!++ Vectors of coordinates
!
      logical cut_input
      integer i,j,mynp,nloop
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn),myalphax,mybetax,myemitx0,myemitx,mynex,mdex, &
     &mygammax,myalphay,mybetay,myemity0,myemity,myney,mdey,mygammay,   &
     &xsigmax,ysigmay,myenom,nr,ndr
!
!
      real      rndm4
!
!
      character*80   dummy
!
!
      common /cut/ cut_input
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      double precision pi
!
      double precision ran_gauss
      double precision iix, iiy, phix, phiy
      double precision enerror, bunchlength
      double precision en_error, bunch_length
!
      double precision long_cut
      double precision a_st, b_st
!
      save
!-----------------------------------------------------------------------
!++  Generate particle distribution
!
!
!++  Generate random distribution, assuming optical parameters at IP1
!
!++  Calculate the gammas
      pi=4d0*atan(1d0)
!
      mygammax = (1d0+myalphax**2)/mybetax
      mygammay = (1d0+myalphay**2)/mybetay
 
!     Assign bunch length and dp/p depending on the energy
!     Check if the units in metres are correct!
!GRD      if ( myenom.eq.7e6 ) then
!GRD         en_error     = 1.129e-4
!GRD         bunch_length = 7.55e-2
!GRD      elseif ( myenom.eq.4.5e5 ) then
!GRD         en_error     = 3.06e-4
!GRD         bunch_length = 11.24e-2
!GRD      else
      en_error = enerror
      bunch_length = bunchlength
!GRD         write(*,*)"Warning-Energy different from LHC inj or top!"
!GRD         write(*,*)"     => 7TeV values of dp/p and bunch length used!"
!GRD      endif
!GRD
      write (*,*) "Generation of bunch with dp/p and length:"
      write (*,*) "  RMS bunch length  = ", bunch_length
      write (*,*) "  RMS energy spread = ", en_error
      do j=1, mynp
         if ((mynex.gt.0d0).and.(myney.eq.0d0)) then
            myemitx = myemitx0*(mynex+(2d0*dble(rndm4()-0.5)*mdex))**2
            xsigmax = sqrt(mybetax*myemitx)
            myx(j)   = xsigmax * sin(2d0*pi*rndm4())
            if (rndm4().gt.0.5) then
              myxp(j) = sqrt(myemitx/mybetax-myx(j)**2/mybetax**2)-     &
     &              myalphax*myx(j)/mybetax
            else
              myxp(j) = -1d0*sqrt(myemitx/mybetax-myx(j)**2/mybetax**2)-&
     &              myalphax*myx(j)/mybetax
            endif
!
            phiy = 2*pi*rndm4()
!
            iiy = -1d0*myemity0 * log( rndm4() )
!
            myy(j) = sqrt(2*iiy*mybetay) * cos(phiy)
            myyp(j) = -1d0*sqrt(2*iiy/mybetay) * (sin(phiy) +           &
     &           myalphay * cos(phiy))
         elseif ( mynex.eq.0.and.myney.gt.0 ) then
            myemity = myemity0*(myney+(2d0*dble(rndm4()-0.5)*mdey))**2
            ysigmay = sqrt(mybetay*myemity)
            myy(j)   = ysigmay * sin(2d0*pi*rndm4())
            if (rndm4().gt.0.5) then
              myyp(j) = sqrt(myemity/mybetay-myy(j)**2/mybetay**2)-     &
     &              myalphay*myy(j)/mybetay
            else
              myyp(j) = -1d0*sqrt(myemity/mybetay-myy(j)**2/mybetay**2)-&
     &              myalphay*myy(j)/mybetay
            endif
!
            phix = 2*pi*rndm4()
            iix = - myemitx0 * log( rndm4() )
!
            myx(j) = sqrt(2*iix*mybetax) * cos(phix)
            myxp(j) = -1d0*sqrt(2*iix/mybetax) * (sin(phix) +           &
     &           myalphax * cos(phix))
         elseif ( mynex.eq.0.and.myney.eq.0 ) then
            phix = 2*pi*rndm4()
            iix = - myemitx0 * log( rndm4() )
!
            myx(j) = sqrt(2*iix*mybetax) * cos(phix)
            myxp(j) = -1d0*sqrt(2*iix/mybetax) * (sin(phix) +           &
     &           myalphax * cos(phix))
            phiy = 2*pi*rndm4()
            iiy = - myemity0 * log( rndm4() )
!
            myy(j) = sqrt(2*iiy*mybetay) * cos(phiy)
            myyp(j) = -1d0*sqrt(2*iiy/mybetay) * (sin(phiy) +           &
     &           myalphay * cos(phiy))
         else
            write(*,*) "Error - beam parameters not correctly set!"
         endif
!
      end do
! SR, 11-08-2005 For longitudinal phase-space, add a cut at 2 sigma
!++   1st: generate mynpnumbers within the chose cut
      long_cut = 2
      j = 1
      do while (j.le.mynp)
         a_st = ran_gauss(5d0)
         b_st = ran_gauss(5d0)
         do while ((a_st*a_st+b_st*b_st).gt.long_cut*long_cut)
            a_st = ran_gauss(5d0)
            b_st = ran_gauss(5d0)
         enddo
         mys(j) = a_st
         myp(j) = b_st
         j = j + 1
      enddo
!++   2nd: give the correct values
      do j=1,mynp
         myp(j) = myenom * (1d0 + myp(j) * en_error)
         mys(j) = bunch_length * mys(j)
      enddo
!
      return
      end
!
!========================================================================
!
      subroutine readdis(filename_dis, mynp,
     &     myx, myxp, myy, myyp, myp, mys)
!
!     SR, 09-08-2005
!     Format for the input file:
!               x, y   -> [ m ]
!               xp, yp -> [ rad ] 
!               s      -> [ mm ] 
!               DE     -> [ MeV ]
!
      implicit none
 
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
!++ Vectors of coordinates
!
      logical cut_input
      integer i,j,mynp,nloop
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn),myalphax,mybetax,myemitx0,myemitx,mynex,mdex, &
     &mygammax,myalphay,mybetay,myemity0,myemity,myney,mdey,mygammay,   &
     &xsigmax,ysigmay,myenom,nr,ndr
!
!
      real      rndm4
!
!
      character*80   dummy
!
!
      common /cut/ cut_input
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
 
      character*80   filename_dis
 
      save
 
      write(*,*) "Reading input bunch from ", filename_dis
 
      open(unit=111, file=filename_dis)
 
      do j=1,mynp
         read(111,*,end=10) myx(j), myxp(j), myy(j), myyp(j),
     &        mys(j), myp(j)
      enddo
 
 10   mynp = j - 1
      write(*,*) "Number of particles in the bunch = ",mynp
 
      close(111)
 
      return
      end
!
!========================================================================
!
      subroutine makedis_radial(mynp, myalphax, myalphay, mybetax,      &
     &mybetay, myemitx0, myemity0, myenom, nr, ndr,myx, myxp, myy,      &
     &myyp, myp, mys)
!
      implicit none
!
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
!++ Vectors of coordinates
!
      logical cut_input
      integer i,j,mynp,nloop
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn),myalphax,mybetax,myemitx0,myemitx,mynex,mdex, &
     &mygammax,myalphay,mybetay,myemity0,myemity,myney,mdey,mygammay,   &
     &xsigmax,ysigmay,myenom,nr,ndr
!
!
      real      rndm4
!
!
      character*80   dummy
!
!
      common /cut/ cut_input
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      double precision pi
!
      save
!-----------------------------------------------------------------------
!++  Generate particle distribution
!
!
!++  Generate random distribution, assuming optical parameters at IP1
!
!++  Calculate the gammas
!
      pi=4d0*atan(1d0)
      mygammax = (1d0+myalphax**2)/mybetax
      mygammay = (1d0+myalphay**2)/mybetay
!
!++  Number of points and generate distribution
!
      mynex = nr/sqrt(2d0)
      mdex = ndr/sqrt(2d0)
      myney = nr/sqrt(2d0)
      mdey = ndr/sqrt(2d0)
!
      write(*,*)
      write(*,*) 'Generation of particle distribution Version 2'
      write(*,*)
      write(*,*) 'This routine generates particles in that are fully'
      write(*,*) 'correlated between X and Y.'
      write(*,*)
!
      write(outlun,*)
      write(outlun,*) 'Generation of particle distribution Version 2'
      write(outlun,*)
      write(outlun,*)                                                   &
     &'This routine generates particles in that are fully'
      write(outlun,*) 'correlated between X and Y.'
      write(outlun,*)
      write(outlun,*)
      write(outlun,*) 'INFO>  Number of particles   = ', mynp
      write(outlun,*) 'INFO>  Av number of x sigmas = ', mynex
      write(outlun,*) 'INFO>  +- spread in x sigmas = ', mdex
      write(outlun,*) 'INFO>  Av number of y sigmas = ', myney
      write(outlun,*) 'INFO>  +- spread in y sigmas = ', mdey
      write(outlun,*) 'INFO>  Nominal beam energy   = ', myenom
      write(outlun,*) 'INFO>  Sigma_x0 = ', sqrt(mybetax*myemitx0)
      write(outlun,*) 'INFO>  Sigma_y0 = ', sqrt(mybetay*myemity0)
      write(outlun,*)
!
      do while (j.lt.mynp)
!
        j = j + 1
        myemitx = myemitx0*(mynex + (2d0*dble(rndm4()-0.5)*mdex) )**2
        xsigmax = sqrt(mybetax*myemitx)
        myx(j)   = xsigmax * sin(2d0*pi*rndm4())
        if (rndm4().gt.0.5) then
          myxp(j)  = sqrt(myemitx/mybetax-myx(j)**2/mybetax**2)-        &
     &myalphax*myx(j)/mybetax
        else
          myxp(j)  = -1*sqrt(myemitx/mybetax-myx(j)**2/mybetax**2)-     &
     &myalphax*myx(j)/mybetax
        endif
!
        myemity = myemity0*(myney + (2d0*dble(rndm4()-0.5)*mdey) )**2
        ysigmay = sqrt(mybetay*myemity)
        myy(j)   = ysigmay * sin(2d0*pi*rndm4())
        if (rndm4().gt.0.5) then
          myyp(j)  = sqrt(myemity/mybetay-myy(j)**2/mybetay**2)-        &
     &myalphay*myy(j)/mybetay
        else
          myyp(j)  = -1*sqrt(myemity/mybetay-myy(j)**2/mybetay**2)-     &
     &myalphay*myy(j)/mybetay
        endif
!
!APRIL2005
        myp(j)   = myenom
!        if(j.eq.1) then
!          myp(j)   = myenom*(1-0.05)
!!       do j=2,mynp
!        else
!          myp(j) = myp(1) + (j-1)*2d0*0.05*myenom/(mynp-1)
!        endif
!APRIL2005
        mys(j)   = 0d0
!
!++  Dangerous stuff, just for the moment
!
!        IF ( (.NOT. (Y(j).LT.-.008e-3 .AND. YP(j).LT.0.1e-3 .AND.
!     1               YP(j).GT.0.0) ) .AND.
!     2       (.NOT. (Y(j).GT..008e-3 .AND. YP(j).GT.-0.1e-3 .AND.
!     3               YP(j).LT.0.0) ) ) THEN
!          J = J - 1
!        ENDIF
!
      end do
!
      return
      end
!
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
!
      function ichoix(ma)
      implicit none
      integer nrmat,nmat,mat,irmat,mcurr
!     parameter(nmat=12,nrmat=5)
      parameter(nmat=12,nrmat=7)
      double precision xintl,radl,x,xp,z,zp,dpop,p0,zlm,zlm1,xpsd,zpsd, &
     &psd,dpodx(nmat),anuc,rho,emr,tlcut,hcut,cs,csref,bnref,freep,     &
     &cprob,bn,bpp,xln15s,ecmsq,pptot,ppel,ppsd,pptref,pperef,pref,     &
     &pptco,ppeco,sdcoe,freeco,fnavo,zatom
      parameter(fnavo=6.02e23)
      real cgen
      character * 4 mname(nmat)
      common/mater/anuc(nmat),zatom(nmat),rho(nmat),emr(nmat),irmat
      common/coul/tlcut,hcut(nmat),cgen(200,nmat),mcurr
      common/scat/cs(0:5,nmat),csref(0:5,nmat),bnref(nmat),freep(nmat)
      common/scatu/cprob(0:5,nmat),bn(nmat),bpp,xln15s,ecmsq
      common/scatu2/xintl(nmat),radl(nmat),mname
      common/scatpp/pptot,ppel,ppsd
      common/sppref/pptref,pperef,pref,pptco,ppeco,sdcoe,freeco
      common/phase/x,xp,z,zp,dpop
      common/nommom/p0
      common/cjaw1/zlm
      common/cmcs1/zlm1
      common/materia/mat
      common/sindif/xpsd,zpsd,psd
      common/cdpodx/dpodx
      integer ma,i,ichoix
      double precision aran
      real rndm4
      aran=dble(rndm4())
      i=1
  10  if ( aran.gt.cprob(i,ma) ) then
          i=i+1
          goto 10
      endif
      ichoix=i
      return
      end
!---------------------------------------------------------------
!
      function gettran(inter,xmat,p)
!
!++  This function determines: GETTRAN - rms transverse momentum transfer
!
!++  Note: For single-diffractive scattering the vector p of momentum
!++        is modified (energy loss is applied)
!
      implicit none
      integer nrmat,nmat,mat,irmat,mcurr
!     parameter(nmat=12,nrmat=5)
      parameter(nmat=12,nrmat=7)
      double precision xintl,radl,x,xp,z,zp,dpop,p0,zlm,zlm1,xpsd,zpsd, &
     &psd,dpodx(nmat),anuc,rho,emr,tlcut,hcut,cs,csref,bnref,freep,     &
     &cprob,bn,bpp,xln15s,ecmsq,pptot,ppel,ppsd,pptref,pperef,pref,     &
     &pptco,ppeco,sdcoe,freeco,fnavo,zatom
      parameter(fnavo=6.02e23)
      real cgen
      character * 4 mname(nmat)
      common/mater/anuc(nmat),zatom(nmat),rho(nmat),emr(nmat),irmat
      common/coul/tlcut,hcut(nmat),cgen(200,nmat),mcurr
      common/scat/cs(0:5,nmat),csref(0:5,nmat),bnref(nmat),freep(nmat)
      common/scatu/cprob(0:5,nmat),bn(nmat),bpp,xln15s,ecmsq
      common/scatu2/xintl(nmat),radl(nmat),mname
      common/scatpp/pptot,ppel,ppsd
      common/sppref/pptref,pperef,pref,pptco,ppeco,sdcoe,freeco
      common/phase/x,xp,z,zp,dpop
      common/nommom/p0
      common/cjaw1/zlm
      common/cmcs1/zlm1
      common/materia/mat
      common/sindif/xpsd,zpsd,psd
      common/cdpodx/dpodx
      integer inter,length,xmat
      double precision p,gettran,t,xm2,bsd
      real rndm4,truth,xran(1)
!
! inter=2: Nuclear Elastic, 3: pp Elastic, 4: Single Diffractif, 5:Coulomb
!
      if        ( inter.eq.2 ) then
           gettran = -log(dble(rndm4()))/bn(xmat)
!
         elseif ( inter .eq. 3 ) then
           gettran = -log(dble(rndm4()))/bpp
!
         elseif ( inter .eq. 4 ) then
           xm2 = exp( dble(rndm4()) * xln15s )
           p = p  *(1.d0 - xm2/ecmsq)
           if ( xm2 .lt. 2.d0 ) then
                bsd = 2.d0 * bpp
              elseif (( xm2 .ge. 2.d0 ).and. ( xm2 .le. 5.d0 )) then
                bsd = (106.d0-17.d0*xm2) *  bpp / 26.d0
              elseif ( xm2 .gt. 5.d0 ) then
                bsd = 7.d0 * bpp / 12.d0
           endif
           gettran = -log(dble(rndm4()))/bsd
!
         elseif ( inter.eq.5 ) then
           length=1
           call funlux( cgen(1,mat) , xran, length)
           truth=xran(1)
           t=truth
           gettran = t
      endif
      return
      end
!---------------------------------------------------------------
!
      subroutine tetat(t,p,tx,tz)
      implicit none
      double precision t,p,tx,tz,va,vb,va2,vb2,r2,teta
      real rndm4
      teta = sqrt(t)/p
! Generate sine and cosine of an angle uniform in [0,2pi](see RPP)
   10 va  =2d0*rndm4()-1d0
      vb = dble(rndm4())
      va2 = va*va
      vb2 = vb*vb
      r2 = va2 + vb2
      if ( r2.gt.1.d0) go to 10
      tx = teta * (2.d0*va*vb) / r2
      tz = teta * (va2 - vb2) / r2
      return
      end
!---------------------------------------------------------------
!
      function ruth(t)
      implicit none
      integer nrmat,nmat,mat,irmat,mcurr
!     parameter(nmat=12,nrmat=5)
      parameter(nmat=12,nrmat=7)
      double precision xintl,radl,x,xp,z,zp,dpop,p0,zlm,zlm1,xpsd,zpsd, &
     &psd,dpodx(nmat),anuc,rho,emr,tlcut,hcut,cs,csref,bnref,freep,     &
     &cprob,bn,bpp,xln15s,ecmsq,pptot,ppel,ppsd,pptref,pperef,pref,     &
     &pptco,ppeco,sdcoe,freeco,fnavo,zatom
      parameter(fnavo=6.02e23)
      real cgen
      character * 4 mname(nmat)
      common/mater/anuc(nmat),zatom(nmat),rho(nmat),emr(nmat),irmat
      common/coul/tlcut,hcut(nmat),cgen(200,nmat),mcurr
      common/scat/cs(0:5,nmat),csref(0:5,nmat),bnref(nmat),freep(nmat)
      common/scatu/cprob(0:5,nmat),bn(nmat),bpp,xln15s,ecmsq
      common/scatu2/xintl(nmat),radl(nmat),mname
      common/scatpp/pptot,ppel,ppsd
      common/sppref/pptref,pperef,pref,pptco,ppeco,sdcoe,freeco
      common/phase/x,xp,z,zp,dpop
      common/nommom/p0
      common/cjaw1/zlm
      common/cmcs1/zlm1
      common/materia/mat
      common/sindif/xpsd,zpsd,psd
      common/cdpodx/dpodx
      real ruth,t
      double precision cnorm,cnform
      parameter(cnorm=2.607d-4,cnform=0.8561d3)
!c      write(6,'('' t,exp'',2e15.8)')t,t*cnform*EMr(mcurr)**2
      ruth=cnorm*exp(-t*cnform*emr(mcurr)**2)*(zatom(mcurr)/t)**2
      end
!---------------------------------------------------------------
!
      block data scdata
!GRD
!GRD CHANGED ON 2/2003 TO INCLUDE CODE FOR C, C2 from JBJ (rwa)
!GRD
      implicit none
      integer nrmat,nmat,mat,irmat,mcurr
!     parameter(nmat=12,nrmat=5)
      parameter(nmat=12,nrmat=7)
      double precision xintl,radl,x,xp,z,zp,dpop,p0,zlm,zlm1,xpsd,zpsd, &
     &psd,dpodx(nmat),anuc,rho,emr,tlcut,hcut,cs,csref,bnref,freep,     &
     &cprob,bn,bpp,xln15s,ecmsq,pptot,ppel,ppsd,pptref,pperef,pref,     &
     &pptco,ppeco,sdcoe,freeco,fnavo,zatom
      parameter(fnavo=6.02e23)
      real cgen
      character * 4 mname(nmat)
      integer i
      common/mater/anuc(nmat),zatom(nmat),rho(nmat),emr(nmat),irmat
      common/coul/tlcut,hcut(nmat),cgen(200,nmat),mcurr
      common/scat/cs(0:5,nmat),csref(0:5,nmat),bnref(nmat),freep(nmat)
      common/scatu/cprob(0:5,nmat),bn(nmat),bpp,xln15s,ecmsq
      common/scatu2/xintl(nmat),radl(nmat),mname
      common/scatpp/pptot,ppel,ppsd
      common/sppref/pptref,pperef,pref,pptco,ppeco,sdcoe,freeco
      common/phase/x,xp,z,zp,dpop
      common/nommom/p0
      common/cjaw1/zlm
      common/cmcs1/zlm1
      common/materia/mat
      common/sindif/xpsd,zpsd,psd
      common/cdpodx/dpodx
! The last materials are 'vacuum' and 'black', see in sub. SCATIN
! Number of real materials defined here:
!
!++ CHANGE THE NUMBER OF REAL MATERIALS FROM 5 to 7 (bug in JBJ'S ROUTINE?)
!
!      data irmat/5/
!
      data irmat/7/
!
! Reference data at pRef=450Gev
!      data (mname(i),i=1,nrmat)/ 'Be' , 'Al' , 'Cu' , 'W'  , 'Pb' /
      data (mname(i),i=1,nrmat)/ 'Be','Al','Cu','W','Pb','C','C2' /
!
!      data mname(nmat-1), mname(nmat)/'vacu','blac'/
      data mname(nmat-1), mname(nmat)/'vacu','blac'/
!GRD
!GRD IMPLEMENT CHANGES FROM JBJ, 2/2003 RWA
!GRD
!      data (Anuc(i),i=1,nrmat)/ 9.01, 26.98, 63.55, 183.85, 207.19/
      data (anuc(i),i=1,5)/ 9.01d0,26.98d0,63.55d0,183.85d0,207.19d0/
      data (anuc(i),i=6,nrmat)/12.01d0,12.01d0/
!
!GRD      data (Z(i),i=1,nrmat)/       4,    13,    29,     74,     82/
      data (zatom(i),i=1,5)/ 4d0, 13d0, 29d0, 74d0, 82d0/
      data (zatom(i),i=6,nrmat)/   6d0,      6d0/
!GRD      data (Rho(i),i=1,nrmat)/ 1.848,  2.70,  8.96,   19.3,  11.35/
      data (rho(i),i=1,5)/ 1.848d0, 2.70d0, 8.96d0, 19.3d0, 11.35d0/
      data (rho(i),i=6,nrmat)/ 2.26d0, 4.52d0/
!GRD      data (RadL(i),i=1,nrmat)/ 0.353, 0.089, 0.0143, 0.0035, 0.0056/
      data (radl(i),i=1,5)/ 0.353d0,0.089d0,0.0143d0,0.0035d0,0.0056d0/
      data (radl(i),i=6,nrmat)/ 0.188d0, 0.094d0/
      data radl(nmat-1),radl(nmat)/ 1.d12, 1.d12 /
!GRD      data (EMR(i),i=1,nrmat)/  0.22, 0.302, 0.366,    0.0,  0.542/
!MAY06-GRD value for Tungsten (W) not stated
!      data (emr(i),i=1,5)/  0.22d0, 0.302d0, 0.366d0, 0.0d0, 0.542d0/
      data (emr(i),i=1,5)/  0.22d0, 0.302d0, 0.366d0, 0.520d0, 0.542d0/
!MAY06-GRD end of changes
      data (emr(i),i=6,nrmat)/  0.25d0, 0.25d0/
!GRD      data tLcut,(Hcut(i),i=1,nrmat)/0.9982e-3,0.02,0.02,3*0.01/
      data tlcut / 0.0009982d0/
      data (hcut(i),i=1,5)/0.02d0, 0.02d0, 3*0.01d0/
      data (hcut(i),i=6,nrmat)/0.02d0, 0.02d0/
!      data (dpodx(i),i=1,nrmat)/ nrmat*0.d0 /
!GRD      data (dpodx(i),i=1,nrmat)/ .55, .81, 2.69, 5.79, 3.4 /
      data (dpodx(i),i=1,5)/ .55d0, .81d0, 2.69d0, 5.79d0, 3.4d0 /
      data (dpodx(i),i=6,nrmat)/ .75d0, 1.5d0 /
!
! All cross-sections are in barns,nuclear values from RPP at 20geV
! Coulomb is integerated above t=tLcut[Gev2] (+-1% out Gauss mcs)
!
! in Cs and CsRef,1st index: Cross-sections for processes
! 0:Total, 1:absorption, 2:nuclear elastic, 3:pp or pn elastic
! 4:Single Diffractive pp or pn, 5:Coulomb for t above mcs
!
!MAY06-GRD: found an error in the values for Rutherford cross-sections,
!as the ones reported here are stated in fm^2 and not in barns, hence 
!being 100 times too large...
!      data csref(0,1),csref(1,1),csref(5,1)/0.268d0, 0.199d0 , 0.0035d0/
!      data csref(0,2),csref(1,2),csref(5,2)/0.634d0, 0.421d0 , 0.034d0/
!      data csref(0,3),csref(1,3),csref(5,3)/1.232d0, 0.782d0 , 0.153d0/
!      data csref(0,4),csref(1,4),csref(5,4)/2.767d0, 1.65d0  , 0.768d0/
!      data csref(0,5),csref(1,5),csref(5,5)/2.960d0, 1.77d0  , 0.907d0/
!!GRD
!      data csref(0,6),csref(1,6),csref(5,6)/0.331d0, 0.231d0, 0.0076d0/
!      data csref(0,7),csref(1,7),csref(5,7)/0.331d0, 0.231d0, 0.0076d0/
!
      data csref(0,1),csref(1,1),csref(5,1)/0.268d0, 0.199d0, 0.0035d-2/
      data csref(0,2),csref(1,2),csref(5,2)/0.634d0, 0.421d0, 0.034d-2/
      data csref(0,3),csref(1,3),csref(5,3)/1.232d0, 0.782d0, 0.153d-2/
      data csref(0,4),csref(1,4),csref(5,4)/2.767d0, 1.65d0 , 0.768d-2/
      data csref(0,5),csref(1,5),csref(5,5)/2.960d0, 1.77d0 , 0.907d-2/
!GRD
      data csref(0,6),csref(1,6),csref(5,6)/0.331d0, 0.231d0, 0.0076d-2/
      data csref(0,7),csref(1,7),csref(5,7)/0.331d0, 0.231d0, 0.0076d-2/
!MAY06-GRD end of changes
!
! pp cross-sections and parameters for energy dependence
      data pptref,pperef,sdcoe,pref/0.04d0,0.007d0,0.00068d0,450.0d0/
      data pptco,ppeco,freeco/0.05788d0,0.04792d0,1.618d0/
! Nuclear elastic slope from Schiz et al.,PRD 21(3010)1980
!GRD      data (bNRef(i),i=1,nrmat)/74.7,120.3,217.8,0.0,455.3/
!MAY06-GRD value for Tungsten (W) not stated
!      data (bnref(i),i=1,5)/74.7d0,120.3d0,217.8d0,0.0d0,455.3d0/
      data (bnref(i),i=1,5)/74.7d0,120.3d0,217.8d0,440.3d0,455.3d0/
!MAY06-GRD end of changes
      data (bnref(i),i=6,nrmat)/70.d0, 70.d0/
!GRD LAST 2 ONES INTERPOLATED
!
! Cprob to choose an interaction in iChoix
      data (cprob(0,i),i=1,nmat)/nmat*0.0d0/
      data (cprob(5,i),i=1,nmat)/nmat*1.0d0/
!
      end

!---------------------------------------------------------------
!
      subroutine scatin(plab)
      implicit none
      integer nrmat,nmat,mat,irmat,mcurr
!     parameter(nmat=12,nrmat=5)
      parameter(nmat=12,nrmat=7)
      double precision xintl,radl,x,xp,z,zp,dpop,p0,zlm,zlm1,xpsd,zpsd, &
     &psd,dpodx(nmat),anuc,rho,emr,tlcut,hcut,cs,csref,bnref,freep,     &
     &cprob,bn,bpp,xln15s,ecmsq,pptot,ppel,ppsd,pptref,pperef,pref,     &
     &pptco,ppeco,sdcoe,freeco,fnavo,zatom
      parameter(fnavo=6.02e23)
      real cgen
      character * 4 mname(nmat)
      common/mater/anuc(nmat),zatom(nmat),rho(nmat),emr(nmat),irmat
      common/coul/tlcut,hcut(nmat),cgen(200,nmat),mcurr
      common/scat/cs(0:5,nmat),csref(0:5,nmat),bnref(nmat),freep(nmat)
      common/scatu/cprob(0:5,nmat),bn(nmat),bpp,xln15s,ecmsq
      common/scatu2/xintl(nmat),radl(nmat),mname
      common/scatpp/pptot,ppel,ppsd
      common/sppref/pptref,pperef,pref,pptco,ppeco,sdcoe,freeco
      common/phase/x,xp,z,zp,dpop
      common/nommom/p0
      common/cjaw1/zlm
      common/cmcs1/zlm1
      common/materia/mat
      common/sindif/xpsd,zpsd,psd
      common/cdpodx/dpodx
      integer ma,i
      double precision plab
      real ruth,tlow,thigh
      external ruth
!      open(unit=6,file='scatin.out')
!
      ecmsq = 2 * 0.93828d0 * plab
      xln15s=log(0.15*ecmsq)
! pp(pn) data
      pptot = pptref *(plab / pref)** pptco
      ppel = pperef *(plab / pref)** ppeco
      ppsd = sdcoe * log(0.15d0 * ecmsq)
      bpp = 8.5d0 + 1.086d0 * log(sqrt(ecmsq))
! unmeasured tungsten data,computed with lead data and power laws
      bnref(4) = bnref(5)*(anuc(4) / anuc(5))**(2d0/3d0)
      emr(4) = emr(5) * (anuc(4)/anuc(5))**(1d0/3d0)
   10 format(/' ppRef TOT El     ',4f12.6//)
!      write(6,10)ppTRef,ppEref
   11 format(/' pp    TOT El Sd b',4f12.6//)
!      write(6,11)ppTot,ppEl,ppSD,bpp
!
! Compute cross-sections (CS) and probabilities + Interaction length
! Last two material treated below statement number 100
!
      tlow=tlcut
      do 100 ma=1,irmat
        mcurr=ma
! prepare for Rutherford differential distribution
        thigh=hcut(ma)
        call funlxp ( ruth , cgen(1,ma) ,tlow, thigh )
!
! freep: number of nucleons involved in single scattering
        freep(ma) = freeco * anuc(ma)**(1d0/3d0)
! compute pp and pn el+single diff contributions to cross-section
! (both added : quasi-elastic or qel later)
        cs(3,ma) = freep(ma) * ppel
        cs(4,ma) = freep(ma) * ppsd
!
! correct TOT-CSec for energy dependence of qel
! TOT CS is here without a Coulomb contribution
        cs(0,ma) = csref(0,ma) + freep(ma) * (pptot - pptref)
        bn(ma) = bnref(ma) * cs(0,ma) / csref(0,ma)
! also correct inel-CS
        cs(1,ma) = csref(1,ma) * cs(0,ma) / csref(0,ma)
!
! Nuclear Elastic is TOT-inel-qel ( see definition in RPP)
        cs(2,ma) = cs(0,ma) - cs(1,ma) - cs(3,ma) - cs(4,ma)
        cs(5,ma) = csref(5,ma)
! Now add Coulomb
        cs(0,ma) = cs(0,ma) + cs(5,ma)
! Interaction length in meter
        xintl(ma) = 0.01d0*anuc(ma)/(fnavo * rho(ma)*cs(0,ma)*1d-24)
!
   20   format(/1x,a4,' Int.Len. ',f10.6,' CsTot',2f12.4/)
!        write(6,20)mname(ma),xIntL(ma),Cs(0,ma),CsRef(0,ma)
   21   format('  bN freep',2 f12.6,'   emR ',f7.4/)
!        write(6,21)bN(ma),freep(ma),emR(ma)
! Filling CProb with cumulated normalised Cross-sections
        do 50 i=1,4
          cprob(i,ma)=cprob(i-1,ma)+cs(i,ma)/cs(0,ma)
!          write(6,22)i,Cprob(i,ma),Cs(i,ma),CsRef(i,ma)
 50     continue
!        write(6,22)5,Cprob(5,ma),Cs(5,ma),CsRef(5,ma)
   22   format(i4,' prob CS CsRref',3(f12.5,2x))
  100 continue
!
! Last two materials for 'vaccum' (nmat-1) and 'full black' (nmat)
!
      cprob(1,nmat-1)=1d0
      cprob(1,nmat)=1d0
      xintl(nmat-1)=1d12
      xintl(nmat)=0.0d0
  120 format(/1x,a4,' Int.Len. ',e10.3/)
!      write(6,120)mname(nmat-1),xIntL(nmat-1)
!      write(6,120)mname(nmat),xIntL(nmat)
      return
      end
 
!-----------------------------------------------------------------------
!
      subroutine jaw(s,nabs)
!
!++  Input:   ZLM is interaction length
!++           MAT is choice of material
!
!++  Output:  nabs = 1   Particle is absorped
!++           nabs = 4   Single-diffractive scattering
!++           dpop       Adjusted for momentum loss (dE/dx)
!++           s          Exit longitudinal position
!
!++  Physics:  If monte carlo interaction length greater than input
!++            interaction length, then use input interaction length
!++            Is that justified???
!
!     nabs=1....absorption
!
      implicit none
!
      integer nrmat,nmat,mat,irmat,mcurr
!     parameter(nmat=12,nrmat=5)
      parameter(nmat=12,nrmat=7)
      double precision xintl,radl,x,xp,z,zp,dpop,p0,zlm,zlm1,xpsd,zpsd, &
     &psd,dpodx(nmat),anuc,rho,emr,tlcut,hcut,cs,csref,bnref,freep,     &
     &cprob,bn,bpp,xln15s,ecmsq,pptot,ppel,ppsd,pptref,pperef,pref,     &
     &pptco,ppeco,sdcoe,freeco,fnavo,zatom
      parameter(fnavo=6.02e23)
      real cgen
      character * 4 mname(nmat)
      common/mater/anuc(nmat),zatom(nmat),rho(nmat),emr(nmat),irmat
      common/coul/tlcut,hcut(nmat),cgen(200,nmat),mcurr
      common/scat/cs(0:5,nmat),csref(0:5,nmat),bnref(nmat),freep(nmat)
      common/scatu/cprob(0:5,nmat),bn(nmat),bpp,xln15s,ecmsq
      common/scatu2/xintl(nmat),radl(nmat),mname
      common/scatpp/pptot,ppel,ppsd
      common/sppref/pptref,pperef,pref,pptco,ppeco,sdcoe,freeco
      common/phase/x,xp,z,zp,dpop
      common/nommom/p0
      common/cjaw1/zlm
      common/cmcs1/zlm1
      common/materia/mat
      common/sindif/xpsd,zpsd,psd
      common/cdpodx/dpodx
      integer nabs,inter,ichoix
      double precision p,rlen,s,t,gettran,dxp,dzp,p1
      real rndm4
!...cne=1/(sqrt(b))
!...dpodx=dE/(dx*c)
!
!++  Note that the input parameter is dpop. Here the momentum p is
!++  constructed out of this input.
!
!      p=p0/(1.d0-dpop)
      p=p0*(1.d0+dpop)
      nabs=0
      if(mat.eq.nmat) then
!
!++  Collimator treated as black absorber
!
        nabs=1
        s=0d0
        return
      else if(mat.eq.nmat-1) then
!
!++  Collimator treated as drift
!
        s=zlm
        x=x+s*xp
        z=z+s*zp
        return
      end if
!
!++  Initialize the interaction length to input interaction length
!
      rlen=zlm
!
!++  Do a step for a point-like interaction. This is a loop with
!++  label 10!!!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!++  Get monte-carlo interaction length.
!
10    zlm1=-xintl(mat)*log(dble(rndm4()))
!
      if(zlm1.gt.rlen) then
!
!++  If the monte-carlo interaction length is shorter than the
!++  remaining collimator length, then put it to the remaining
!++  length, do multiple coulomb scattering and return.
!++  LAST STEP IN ITERATION LOOP
!
       zlm1=rlen
       call mcs(s)
       s=zlm-rlen+s
       p=p-dpodx(mat)*s
!       dpop=1.d0-p0/p
       dpop=(p-p0)/p0
       return
      end if
!
!++  Otherwise do multi-coulomb scattering.
!++  REGULAR STEP IN ITERATION LOOP
!
      call mcs(s)
!
!++  Check if particle is outside of collimator (X.LT.0) after
!++  MCS. If yes, calculate output longitudinal position (s),
!++  reduce momentum (output as dpop) and return.
!++  PARTICLE LEFT COLLIMATOR BEFORE ITS END.
!
      if(x.le.0d0) then
       s=zlm-rlen+s
       p=p-dpodx(mat)*s
       dpop=(p-p0)/p0
       return
      end if
!
!++  Check whether particle is absorbed. If yes, calculate output
!++  longitudinal position (s), reduce momentum (output as dpop)
!++  and return.
!++  PARTICLE WAS ABSORPED INSIDE COLLIMATOR DURING MCS.
!
      inter=ichoix(mat)
      if(inter.eq.1) then
       nabs=1
       s=zlm-rlen+zlm1
       p=p-dpodx(mat)*s
       dpop=(p-p0)/p0
       return
      end if
!
!++  Now treat the other types of interaction, as determined by ICHOIX:
!
!++      Nuclear-Elastic:          inter = 2
!++      pp Elastic:               inter = 3
!++      Single-Diffractive:       inter = 4    (changes momentum p)
!++      Coulomb:                  inter = 5
!
!++  As the single-diffractive interaction changes the momentum, save
!++  input momentum in p1.
!
      p1 = p
!
!++  Gettran returns some monte carlo number, that, as I believe, gives
!++  the rms transverse momentum transfer.
!
      t = gettran(inter,mat,p)
!
!++  Tetat calculates from the rms transverse momentum transfer in
!++  monte-carlo fashion the angle changes for x and z planes. The
!++  angle change is proportional to SQRT(t) and 1/p, as expected.
!
      call tetat(t,p,dxp,dzp)
!
!++  Apply angle changes
!
      xp=xp+dxp
      zp=zp+dzp
!
!++  Treat single-diffractive scattering.
!
      if(inter.eq.4) then
        nabs=4
!
!++ added update for s
!
        s=zlm-rlen+zlm1
        xpsd=dxp
        zpsd=dzp
        psd=p1
!
!++  Add this code to get the momentum transfer also in the calling
!++  routine...
!
        dpop=(p-p0)/p0
!
      end if
!
!++  Calculate the remaining interaction length and close the iteration
!++  loop.
!
      rlen=rlen-zlm1
      goto 10
!
      end
!------------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine jaw0(s,nabs)
!
!++  Input:   ZLM is interaction length
!++           MAT is choice of material
!
!++  Output:  nabs = 1   Particle is absorped
!++           nabs = 4   Single-diffractive scattering
!++           dpop       Adjusted for momentum loss (dE/dx)
!++           s          Exit longitudinal position
!
!++  Physics:  If monte carlo interaction length greater than input
!++            interaction length, then use input interaction length
!++            Is that justified???
!
!     nabs=1....absorption
!
      implicit none
!
      integer nrmat,nmat,mat,irmat,mcurr
!     parameter(nmat=12,nrmat=5)
      parameter(nmat=12,nrmat=7)
      double precision xintl,radl,x,xp,z,zp,dpop,p0,zlm,zlm1,xpsd,zpsd, &
     &psd,dpodx(nmat),anuc,rho,emr,tlcut,hcut,cs,csref,bnref,freep,     &
     &cprob,bn,bpp,xln15s,ecmsq,pptot,ppel,ppsd,pptref,pperef,pref,     &
     &pptco,ppeco,sdcoe,freeco,fnavo,zatom
      parameter(fnavo=6.02e23)
      real cgen
      character * 4 mname(nmat)
      common/mater/anuc(nmat),zatom(nmat),rho(nmat),emr(nmat),irmat
      common/coul/tlcut,hcut(nmat),cgen(200,nmat),mcurr
      common/scat/cs(0:5,nmat),csref(0:5,nmat),bnref(nmat),freep(nmat)
      common/scatu/cprob(0:5,nmat),bn(nmat),bpp,xln15s,ecmsq
      common/scatu2/xintl(nmat),radl(nmat),mname
      common/scatpp/pptot,ppel,ppsd
      common/sppref/pptref,pperef,pref,pptco,ppeco,sdcoe,freeco
      common/phase/x,xp,z,zp,dpop
      common/nommom/p0
      common/cjaw1/zlm
      common/cmcs1/zlm1
      common/materia/mat
      common/sindif/xpsd,zpsd,psd
      common/cdpodx/dpodx
      integer nabs,inter,ichoix
      double precision p,rlen,s,t,gettran,dxp,dzp,p1
      real rndm4
!...cne=1/(sqrt(b))
!...dpodx=dE/(dx*c)
      p=p0/(1.d0-dpop)
      nabs=0
      if(mat.eq.nmat) then
!
!++  Collimator treated as black absorber
!
        nabs=1
        s=0d0
        return
      else if(mat.eq.nmat-1) then
!
!++  Collimator treated as drift
!
        s=zlm
        x=x+s*xp
        z=z+s*zp
        return
      end if
!
!++  Initialize the interaction length to input interaction length
!
      rlen=zlm
!
!++  Do a step for a point-like interaction. This is a loop with
!++  label 10!!!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!++  Get monte-carlo interaction length.
!
10    zlm1=-xintl(mat)*log(dble(rndm4()))
!
      if(zlm1.gt.rlen) then
!
!++  If the monte-carlo interaction length is shorter than the
!++  remaining collimator length, then put it to the remaining
!++  length, do multiple coulomb scattering and return.
!++  LAST STEP IN ITERATION LOOP
!
       zlm1=rlen
       call mcs(s)
       s=zlm-rlen+s
       p=p-dpodx(mat)*s
       dpop=1.d0-p0/p
       return
      end if
!
!++  Otherwise do multi-coulomb scattering.
!++  REGULAR STEP IN ITERATION LOOP
!
      call mcs(s)
!
!++  Check if particle is outside of collimator (X.LT.0) after
!++  MCS. If yes, calculate output longitudinal position (s),
!++  reduce momentum (output as dpop) and return.
!++  PARTICLE LEFT COLLIMATOR BEFORE ITS END.
!
      if(x.le.0.d0) then
       s=zlm-rlen+s
       p=p-dpodx(mat)*s
       dpop=1.d0-p0/p
       return
      end if
!
!++  Check whether particle is absorbed. If yes, calculate output
!++  longitudinal position (s), reduce momentum (output as dpop)
!++  and return.
!++  PARTICLE WAS ABSORPED INSIDE COLLIMATOR DURING MCS.
!
      inter=ichoix(mat)
      if(inter.eq.1) then
       nabs=1
       s=zlm-rlen+zlm1
       p=p-dpodx(mat)*s
       dpop=1.d0-p0/p
       return
      end if
!
!++  Now treat the other types of interaction, as determined by ICHOIX:
!
!++      Nuclear-Elastic:          inter = 2
!++      pp Elastic:               inter = 3
!++      Single-Diffractive:       inter = 4    (changes momentum p)
!++      Coulomb:                  inter = 5
!
!++  As the single-diffractive interaction changes the momentum, save
!++  input momentum in p1.
!
      p1 = p
!
!++  Gettran returns some monte carlo number, that, as I believe, gives
!++  the rms transverse momentum transfer.
!
      t = gettran(inter,mat,p)
!
!++  Tetat calculates from the rms transverse momentum transfer in
!++  monte-carlo fashion the angle changes for x and z planes. The
!++  angle change is proportional to SQRT(t) and 1/p, as expected.
!
      call tetat(t,p,dxp,dzp)
!
!++  Apply angle changes
!
      xp=xp+dxp
      zp=zp+dzp
!
!++  Treat single-diffractive scattering.
!
      if(inter.eq.4) then
        nabs=4
        xpsd=dxp
        zpsd=dzp
        psd=p1
      end if
!
!++  Calculate the remaining interaction length and close the iteration
!++  loop.
!
      rlen=rlen-zlm1
      goto 10
!
      end
!------------------------------------------------------------------------
 
      subroutine mcs(s)
!
!++  Input:   zlm1   Monte-carlo interaction length
!
!++  Output:  s      Longitudinal position
!++           p0     Reference momentum
!++           dpop   Relative momentum offset
!
!     collimator: x>0 and y<zlm1
!
      implicit none
!      save h,dh,bn
      integer nrmat,nmat,mat,irmat,mcurr
!     parameter(nmat=12,nrmat=5)
      parameter(nmat=12,nrmat=7)
      double precision xintl,radl,x,xp,z,zp,dpop,p0,zlm,zlm1,xpsd,zpsd, &
     &psd,dpodx(nmat),anuc,rho,emr,tlcut,hcut,cs,csref,bnref,freep,     &
     &cprob,bn,bpp,xln15s,ecmsq,pptot,ppel,ppsd,pptref,pperef,pref,     &
     &pptco,ppeco,sdcoe,freeco,fnavo,zatom
      parameter(fnavo=6.02e23)
      real cgen
      character * 4 mname(nmat)
      common/mater/anuc(nmat),zatom(nmat),rho(nmat),emr(nmat),irmat
      common/coul/tlcut,hcut(nmat),cgen(200,nmat),mcurr
      common/scat/cs(0:5,nmat),csref(0:5,nmat),bnref(nmat),freep(nmat)
      common/scatu/cprob(0:5,nmat),bn(nmat),bpp,xln15s,ecmsq
      common/scatu2/xintl(nmat),radl(nmat),mname
      common/scatpp/pptot,ppel,ppsd
      common/sppref/pptref,pperef,pref,pptco,ppeco,sdcoe,freeco
      common/phase/x,xp,z,zp,dpop
      common/nommom/p0
      common/cjaw1/zlm
      common/cmcs1/zlm1
      common/materia/mat
      common/sindif/xpsd,zpsd,psd
      common/cdpodx/dpodx
      double precision h,dh,theta,rlen0,rlen,ae,be,bn0,s
!   bn=sqrt(3)/(number of sigmas for s-determination(=4))
      data h/.001d0/dh/.0001d0/bn0/.4330127019d0/
!
!++
!
      theta=13.6d-3*(1.d0-dpop)/p0
      x=x/theta/radl(mat)
      xp=xp/theta
      z=z/theta/radl(mat)
      zp=zp/theta
      rlen0=zlm1/radl(mat)
      rlen=rlen0
10    ae=bn0*x
      be=bn0*xp
      call soln3(ae,be,dh,rlen,s)
      if(s.lt.h) s=h
      call scamcs(x,xp,s)
      if(x.le.0.d0) then
       s=rlen0-rlen+s
       goto 20
      end if
      if(s+dh.ge.rlen) then
       s=rlen0
       goto 20
      end if
      rlen=rlen-s
      goto 10
20    call scamcs(z,zp,s)
      s=s*radl(mat)
      x=x*theta*radl(mat)
      xp=xp*theta
      z=z*theta*radl(mat)
      zp=zp*theta
      end
 
      subroutine scamcs(xx,xxp,s)
      implicit none
      double precision v1,v2,r2,a,z1,z2,ss,s,xx,xxp,x0,xp0
      real rndm4
      x0=xx
      xp0=xxp
5     v1=2d0*rndm4()-1d0
      v2=2d0*rndm4()-1d0
      r2=v1*v1+v2*v2
      if(r2.ge.1.d0) goto 5
      a=dsqrt(-2.d0*log(r2)/r2)
      z1=v1*a
      z2=v2*a
      ss=dsqrt(s)
      xx=x0+s*(xp0+.5d0*ss*(z2+z1*.577350269d0))
!     x=x0+s*(xp0+.5d0*ss*(z2+z1/dsqrt(3.d0)))
      xxp=xp0+ss*z2
      end
 
!-------------------------------------------------------------
 
      subroutine soln3(a,b,dh,smax,s)
      implicit none
      double precision b,a,s,smax,c,dh
      if(b.eq.0.d0) then
       s=a**0.6666666666666667d0
!      s=a**(2.d0/3.d0)
       if(s.gt.smax) s=smax
       return
      end if
      if(a.eq.0.d0) then
       if(b.gt.0.d0) then
         s=b**2
       else
         s=0.d0
       end if
       if(s.gt.smax) s=smax
       return
      end if
      if(b.gt.0.d0) then
       if(smax**3.le.(a+b*smax)**2) then
        s=smax
        return
       else
        s=smax*.5d0
        call iterat(a,b,dh,s)
       end if
      else
       c=-a/b
       if(smax.lt.c) then
        if(smax**3.le.(a+b*smax)**2) then
         s=smax
         return
        else
         s=smax*.5d0
         call iterat(a,b,dh,s)
        end if
       else
        s=c*.5d0
        call iterat(a,b,dh,s)
       end if
      end if
      end
 
 
      subroutine iterat(a,b,dh,s)
      implicit none
      double precision ds,s,a,b,dh
 
      ds=s
10    ds=ds*.5d0
      if(s**3.lt.(a+b*s)**2) then
        s=s+ds
      else
        s=s-ds
      end if
      if(ds.lt.dh) then
        return
      else
        goto 10
      end if
      end
!
!cccccccccccccccccccccccccccccccccc
!
      function rndm4()
      implicit none
      integer len, in
      real rndm4, a
      save
      parameter ( len =  30000 )
      dimension a(len)
      data in/1/
!
      if ( in.eq.1 ) then
         call ranlux(a,len)
         rndm4=a(1)
         in=2
!        write(6,'('' LEN: '',i5)')LEN
      else
         rndm4=a(in)
         in=in+1
         if(in.eq.len+1)in=1
      endif
      return
      end
!
!
!ccccccccccccccccccccccccccccccccccccccc
!-TW-01/2007
! function rndm5(irnd) , irnd = 1 will reset 
! inn counter => enables reproducible set of 
! random unmbers
!cccccccccccccccccccccccccccccccccc
!
      function rndm5(irnd)
      implicit none
      integer len, inn, irnd
      real rndm5, a
      save
      parameter ( len =  30000 )
      dimension a(len)
      data inn/1/
!
! reset inn to 1 enable reproducible random numbers
      if ( irnd .eq. 1) inn = 1
      if ( inn.eq.1 ) then
         call ranlux(a,len)
         rndm5=a(1)
         inn=2
      else
         rndm5=a(inn)
         inn=inn+1
         if(inn.eq.len+1)inn=1
      endif
      return
      end
!
!ccccccccccccccccccccccccccccccccccccccc 
!
!
      double precision function myran_gauss(cut)
!*********************************************************************
!
! myran_gauss - will generate a normal distribution from a uniform
!     distribution between [0,1].
!     See "Communications of the ACM", V. 15 (1972), p. 873.
!
!     cut - double precision - cut for distribution in units of sigma
!     the cut must be greater than 0.5
!
!     changed rndm4 to rndm5(irnd) and defined flag as true 
! 
!*********************************************************************
      implicit none
      
      logical flag
      real rndm5
      double precision x, u1, u2, twopi, r,cut
      save
      
      flag = .true.

      twopi=8d0*atan(1d0)
 1    if (flag) then
         r = dble(rndm5(0))
         r = max(r, 0.5d0**32)
         r = min(r, 1d0-0.5d0**32)
         u1 = sqrt(-2d0*log( r ))
         u2 = dble(rndm5(0))
         x = u1 * cos(twopi*u2)
      else
         x = u1 * sin(twopi*u2)
      endif
      
      flag = .not. flag
      
!     cut the distribution if cut > 0.5
      if (cut .gt. 0.5d0 .and. abs(x) .gt. cut) goto 1
      
      myran_gauss = x
      return
      end
!
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!
! $Id: ranlux.F,v 1.2 1997/09/22 13:45:47 mclareni Exp $
!
! $Log: ranlux.F,v $
! Revision 1.2  1997/09/22 13:45:47  mclareni
! Correct error in initializing RANLUX by using RLUXIN with the output of
! RLUXUT from a previous run.
!
! Revision 1.1.1.1  1996/04/01 15:02:55  mclareni
! Mathlib gen
!
!
      subroutine ranlux(rvec,lenv)
!         Subtract-and-borrow random number generator proposed by
!         Marsaglia and Zaman, implemented by F. James with the name
!         RCARRY in 1991, and later improved by Martin Luescher
!         in 1993 to produce "Luxury Pseudorandom Numbers".
!     Fortran 77 coded by F. James, 1993
!
!   LUXURY LEVELS.
!   ------ ------      The available luxury levels are:
!
!  level 0  (p=24): equivalent to the original RCARRY of Marsaglia
!           and Zaman, very long period, but fails many tests.
!  level 1  (p=48): considerable improvement in quality over level 0,
!           now passes the gap test, but still fails spectral test.
!  level 2  (p=97): passes all known tests, but theoretically still
!           defective.
!  level 3  (p=223): DEFAULT VALUE.  Any theoretically possible
!           correlations have very small chance of being observed.
!  level 4  (p=389): highest possible luxury, all 24 bits chaotic.
!
!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!!  Calling sequences for RANLUX:                                  ++
!!!      CALL RANLUX (RVEC, LEN)   returns a vector RVEC of LEN     ++
!!!                   32-bit random floating point numbers between  ++
!!!                   zero (not included) and one (also not incl.). ++
!!!      CALL RLUXGO(LUX,INT,K1,K2) initializes the generator from  ++
!!!               one 32-bit integer INT and sets Luxury Level LUX  ++
!!!               which is integer between zero and MAXLEV, or if   ++
!!!               LUX .GT. 24, it sets p=LUX directly.  K1 and K2   ++
!!!               should be set to zero unless restarting at a break++
!!!               point given by output of RLUXAT (see RLUXAT).     ++
!!!      CALL RLUXAT(LUX,INT,K1,K2) gets the values of four integers++
!!!               which can be used to restart the RANLUX generator ++
!!!               at the current point by calling RLUXGO.  K1 and K2++
!!!               specify how many numbers were generated since the ++
!!!               initialization with LUX and INT.  The restarting  ++
!!!               skips over  K1+K2*E9   numbers, so it can be long.++
!!!   A more efficient but less convenient way of restarting is by: ++
!!!      CALL RLUXIN(ISVEC)    restarts the generator from vector   ++
!!!                   ISVEC of 25 32-bit integers (see RLUXUT)      ++
!!!      CALL RLUXUT(ISVEC)    outputs the current values of the 25 ++
!!!                 32-bit integer seeds, to be used for restarting ++
!!!      ISVEC must be dimensioned 25 in the calling program        ++
!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      implicit none
      integer lenv,isdext,iseeds,maxlev,ndskip,itwo24,next,j24,i24,     &
     &inseed,mkount,kount,in24,nskip,lxdflt,jsdflt,jseed,lp,i,k,icons,  &
     &inner,izip,izip2,ivec,isk,igiga,isd,k2,k1,inout,lout,ins,lux,ilx, &
     &iouter
      real rvec,seeds,twop12,twom12,twom24,carry,uni
      dimension rvec(lenv)
      dimension seeds(24), iseeds(24), isdext(25)
      parameter (maxlev=4, lxdflt=3)
      dimension ndskip(0:maxlev)
      dimension next(24)
      parameter (twop12=4096., igiga=1000000000,jsdflt=314159265)
      parameter (itwo24=2**24, icons=2147483563)
      save notyet, i24, j24, carry, seeds, twom24, twom12, luxlev
      save nskip, ndskip, in24, next, kount, mkount, inseed
      integer luxlev
      logical notyet
      data notyet, luxlev, in24, kount, mkount /.true., lxdflt, 0,0,0/
      data i24,j24,carry/24,10,0./
!                               default
!  Luxury Level   0     1     2   *3*    4
      data ndskip/0,   24,   73,  199,  365 /
!Corresponds to p=24    48    97   223   389
!     time factor 1     2     3     6    10   on slow workstation
!                 1    1.5    2     3     5   on fast mainframe
!
!  NOTYET is .TRUE. if no initialization has been performed yet.
!              Default Initialization by Multiplicative Congruential
      if (notyet) then
         notyet = .false.
         jseed = jsdflt
         inseed = jseed
         write(*,'(A,I12)') ' RANLUX DEFAULT INITIALIZATION: ',jseed
         luxlev = lxdflt
         nskip = ndskip(luxlev)
         lp = nskip + 24
         in24 = 0
         kount = 0
         mkount = 0
!         WRITE(6,'(A,I2,A,I4)')  ' RANLUX DEFAULT LUXURY LEVEL =  ',
!     &        LUXLEV,'      p =',LP
            twom24 = 1.
         do 25 i= 1, 24
            twom24 = twom24 * 0.5
         k = jseed/53668
         jseed = 40014*(jseed-k*53668) -k*12211
         if (jseed .lt. 0)  jseed = jseed+icons
         iseeds(i) = mod(jseed,itwo24)
   25    continue
         twom12 = twom24 * 4096.
         do 50 i= 1,24
         seeds(i) = real(iseeds(i))*twom24
         next(i) = i-1
   50    continue
         next(1) = 24
         i24 = 24
         j24 = 10
         carry = 0.
         if (seeds(24) .eq. 0.) carry = twom24
      endif
!
!          The Generator proper: "Subtract-with-borrow",
!          as proposed by Marsaglia and Zaman,
!          Florida State University, March, 1989
!
      do 100 ivec= 1, lenv
      uni = seeds(j24) - seeds(i24) - carry
      if (uni .lt. 0.)  then
         uni = uni + 1.
         carry = twom24
      else
         carry = 0.
      endif
      seeds(i24) = uni
      i24 = next(i24)
      j24 = next(j24)
      rvec(ivec) = uni
!  small numbers (with less than 12 "significant" bits) are "padded".
      if (uni .lt. twom12)  then
         rvec(ivec) = rvec(ivec) + twom24*seeds(j24)
!        and zero is forbidden in case someone takes a logarithm
         if (rvec(ivec) .eq. 0.)  rvec(ivec) = twom24*twom24
      endif
!        Skipping to luxury.  As proposed by Martin Luscher.
      in24 = in24 + 1
      if (in24 .eq. 24)  then
         in24 = 0
         kount = kount + nskip
         do 90 isk= 1, nskip
         uni = seeds(j24) - seeds(i24) - carry
         if (uni .lt. 0.)  then
            uni = uni + 1.
            carry = twom24
         else
            carry = 0.
         endif
         seeds(i24) = uni
         i24 = next(i24)
         j24 = next(j24)
   90    continue
      endif
  100 continue
      kount = kount + lenv
      if (kount .ge. igiga)  then
         mkount = mkount + 1
         kount = kount - igiga
      endif
      return
!
!           Entry to input and float integer seeds from previous run
      entry rluxin(isdext)
         notyet = .false.
         twom24 = 1.
         do 195 i= 1, 24
         next(i) = i-1
  195    twom24 = twom24 * 0.5
         next(1) = 24
         twom12 = twom24 * 4096.
      write(*,*) ' FULL INITIALIZATION OF RANLUX WITH 25 INTEGERS:'
      write(*,'(5X,5I12)') isdext
      do 200 i= 1, 24
      seeds(i) = real(isdext(i))*twom24
  200 continue
      carry = 0.
      if (isdext(25) .lt. 0)  carry = twom24
      isd = iabs(isdext(25))
      i24 = mod(isd,100)
      isd = isd/100
      j24 = mod(isd,100)
      isd = isd/100
      in24 = mod(isd,100)
      isd = isd/100
      luxlev = isd
        if (luxlev .le. maxlev) then
          nskip = ndskip(luxlev)
          write(*,'(A,I2)') ' RANLUX LUXURY LEVEL SET BY RLUXIN TO: ',  &
     &luxlev
        else  if (luxlev .ge. 24) then
          nskip = luxlev - 24
          write(*,'(A,I5)') ' RANLUX P-VALUE SET BY RLUXIN TO:',luxlev
        else
          nskip = ndskip(maxlev)
          write(*,'(A,I5)') ' RANLUX ILLEGAL LUXURY RLUXIN: ',luxlev
          luxlev = maxlev
        endif
      inseed = -1
      return
!
!                    Entry to ouput seeds as integers
      entry rluxut(isdext)
      do 300 i= 1, 24
         isdext(i) = int(seeds(i)*twop12*twop12)
  300 continue
      isdext(25) = i24 + 100*j24 + 10000*in24 + 1000000*luxlev
      if (carry .gt. 0.)  isdext(25) = -isdext(25)
      return
!
!                    Entry to output the "convenient" restart point
      entry rluxat(lout,inout,k1,k2)
      lout = luxlev
      inout = inseed
      k1 = kount
      k2 = mkount
      return
!
!                    Entry to initialize from one or three integers
      entry rluxgo(lux,ins,k1,k2)
         if (lux .lt. 0) then
            luxlev = lxdflt
         else if (lux .le. maxlev) then
            luxlev = lux
         else if (lux .lt. 24 .or. lux .gt. 2000) then
            luxlev = maxlev
            write(*,'(A,I7)') ' RANLUX ILLEGAL LUXURY RLUXGO: ',lux
         else
            luxlev = lux
            do 310 ilx= 0, maxlev
              if (lux .eq. ndskip(ilx)+24)  luxlev = ilx
  310       continue
         endif
      if (luxlev .le. maxlev)  then
         nskip = ndskip(luxlev)
         write(*,'(A,I2,A,I4)') ' RANLUX LUXURY LEVEL SET BY RLUXGO :', &
     &luxlev,'     P=', nskip+24
      else
          nskip = luxlev - 24
          write(*,'(A,I5)') ' RANLUX P-VALUE SET BY RLUXGO TO:',luxlev
      endif
      in24 = 0
      if (ins .lt. 0)  write(*,*)                                       &
     &' Illegal initialization by RLUXGO, negative input seed'
      if (ins .gt. 0)  then
        jseed = ins
        write(*,'(A,3I12)') ' RANLUX INITIALIZED BY RLUXGO FROM SEEDS', &
     &jseed, k1,k2
      else
        jseed = jsdflt
        write(*,*)' RANLUX INITIALIZED BY RLUXGO FROM DEFAULT SEED'
      endif
      inseed = jseed
      notyet = .false.
      twom24 = 1.
         do 325 i= 1, 24
           twom24 = twom24 * 0.5
         k = jseed/53668
         jseed = 40014*(jseed-k*53668) -k*12211
         if (jseed .lt. 0)  jseed = jseed+icons
         iseeds(i) = mod(jseed,itwo24)
  325    continue
      twom12 = twom24 * 4096.
         do 350 i= 1,24
         seeds(i) = real(iseeds(i))*twom24
         next(i) = i-1
  350    continue
      next(1) = 24
      i24 = 24
      j24 = 10
      carry = 0.
      if (seeds(24) .eq. 0.) carry = twom24
!        If restarting at a break point, skip K1 + IGIGA*K2
!        Note that this is the number of numbers delivered to
!        the user PLUS the number skipped (if luxury .GT. 0).
      kount = k1
      mkount = k2
      if (k1+k2 .ne. 0)  then
        do 500 iouter= 1, k2+1
          inner = igiga
          if (iouter .eq. k2+1)  inner = k1
          do 450 isk= 1, inner
            uni = seeds(j24) - seeds(i24) - carry
            if (uni .lt. 0.)  then
               uni = uni + 1.
               carry = twom24
            else
               carry = 0.
            endif
            seeds(i24) = uni
            i24 = next(i24)
            j24 = next(j24)
  450     continue
  500   continue
!         Get the right value of IN24 by direct calculation
        in24 = mod(kount, nskip+24)
        if (mkount .gt. 0)  then
           izip = mod(igiga, nskip+24)
           izip2 = mkount*izip + in24
           in24 = mod(izip2, nskip+24)
        endif
!       Now IN24 had better be between zero and 23 inclusive
        if (in24 .gt. 23) then
           write(*,'(A/A,3I11,A,I5)')                                   &
     &'  Error in RESTARTING with RLUXGO:','  The values', ins,         &
     &k1, k2, ' cannot occur at luxury level', luxlev
           in24 = 0
        endif
      endif
      return
      end
 
!cccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine funlxp (func,xfcum,x2low,x2high)
!         F. JAMES,   Sept, 1994
!
!         Prepares the user function FUNC for FUNLUX
!         Inspired by and mostly copied from FUNPRE and FUNRAN
!         except that
!    1. FUNLUX uses RANLUX underneath,
!    2. FUNLXP expands the first and last bins to cater for
!              functions with long tails on left and/or right,
!    3. FUNLXP calls FUNPCT to do the actual finding of percentiles.
!    4. both FUNLXP and FUNPCT use RADAPT for Gaussian integration.
!
      implicit none
      external func
      integer ifunc,ierr
      real x2high,x2low,xfcum,rteps,xhigh,xlow,xrange,uncert,x2,tftot1, &
     &x3,tftot2,func
      real tftot
      common/funint/tftot
      dimension xfcum(200)
      parameter (rteps=0.0002)
      save ifunc
      data ifunc/0/
      ifunc = ifunc + 1
!         FIND RANGE WHERE FUNCTION IS NON-ZERO.
      call funlz(func,x2low,x2high,xlow,xhigh)
      xrange = xhigh-xlow
      if(xrange .le. 0.)  then
        write(*,'(A,2G15.5)') ' FUNLXP finds function range .LE.0',     &
     &xlow,xhigh
        go to 900
      endif
      call radapt(func,xlow,xhigh,1,rteps,0.,tftot ,uncert)
!      WRITE(6,1003) IFUNC,XLOW,XHIGH,TFTOT
 1003 format(' FUNLXP: integral of USER FUNCTION',                      &
     &i3,' from ',e12.5,' to ',e12.5,' is ',e14.6)
!
!      WRITE (6,'(A,A)') ' FUNLXP preparing ',
!     + 'first the whole range, then left tail, then right tail.'
      call funpct(func,ifunc,xlow,xhigh,xfcum,1,99,tftot,ierr)
      if (ierr .gt. 0)  go to 900
      x2 = xfcum(3)
      call radapt(func,xlow,x2,1,rteps,0.,tftot1 ,uncert)
      call funpct(func,ifunc,xlow,x2 ,xfcum,101,49,tftot1,ierr)
      if (ierr .gt. 0)  go to 900
      x3 = xfcum(98)
      call radapt(func,x3,xhigh,1,rteps,0.,tftot2 ,uncert)
      call funpct(func,ifunc,x3,xhigh,xfcum,151,49,tftot2,ierr)
      if (ierr .gt. 0)  go to 900
!      WRITE(6,1001) IFUNC,XLOW,XHIGH
 1001 format(' FUNLXP has prepared USER FUNCTION',i3,                   &
     &' between',g12.3,' and',g12.3,' for FUNLUX')
      return
  900 continue
      write(*,*) ' Fatal error in FUNLXP. FUNLUX will not work.'
      end
!
      subroutine funpct(func,ifunc,xlow,xhigh,xfcum,nlo,nbins,tftot,    &
     &ierr)
!        Array XFCUM is filled from NLO to NLO+NBINS, which makes
!        the number of values NBINS+1, or the number of bins NBINS
      implicit none
      external func
      integer ierr,nbins,nlo,ifunc,nz,ibin,maxz,iz,nitmax,ihome
      real tftot,xhigh,xlow,func,xfcum,rteps,tpctil,tz,tzmax,x,f,tcum,  &
     &x1,f1,dxmax,fmin,fminz,xincr,tincr,xbest,dtbest,tpart,x2,precis,  &
     &refx,uncert,tpart2,dtpar2,dtabs,aberr
      dimension xfcum(*)
      parameter (rteps=0.005, nz=10, maxz=20, nitmax=6,precis=1e-6)
!      DOUBLE PRECISION TPCTIL, TZ, TCUM, XINCR, DTABS,
!     &  TINCR, TZMAX, XBEST, DTBEST, DTPAR2
!
      ierr = 0
      if (tftot .le. 0.) go to 900
      tpctil = tftot/nbins
      tz = tpctil/nz
      tzmax = tz * 2.
      xfcum(nlo) = xlow
      xfcum(nlo+nbins) = xhigh
      x = xlow
      f = func(x)
      if (f .lt. 0.) go to 900
!         Loop over percentile bins
      do 600 ibin = nlo, nlo+nbins-2
      tcum = 0.
      x1 = x
      f1 = f
      dxmax = (xhigh -x) / nz
      fmin = tz/dxmax
      fminz = fmin
!         Loop over trapezoids within a supposed percentil
      do 500 iz= 1, maxz
      xincr = tz/max(f1,fmin,fminz)
  350 x = x1 + xincr
      f = func(x)
      if (f .lt. 0.) go to 900
      tincr = (x-x1) * 0.5 * (f+f1)
      if (tincr .lt. tzmax) go to 370
      xincr = xincr * 0.5
      go to 350
  370 continue
      tcum = tcum + tincr
      if (tcum .ge. tpctil*0.99) go to 520
      fminz = tz*f/ (tpctil-tcum)
      f1 = f
      x1 = x
  500 continue
      write(*,*) ' FUNLUX:  WARNING. FUNPCT fails trapezoid.'
!         END OF TRAPEZOID LOOP
!         Adjust interval using Gaussian integration with
!             Newton corrections since F is the derivative
  520 continue
      x1 = xfcum(ibin)
      xbest = x
      dtbest = tpctil
      tpart = tpctil
!         Allow for maximum NITMAX more iterations on RADAPT
      do 550 ihome= 1, nitmax
  535 xincr = (tpctil-tpart) / max(f,fmin)
      x = xbest + xincr
      x2 = x
        if (ihome .gt. 1 .and. x2 .eq. xbest) then
        write(*,'(A,G12.3)')                                            &
     &' FUNLUX: WARNING from FUNPCT: insufficient precision at X=',x
        go to 580
        endif
      refx = abs(x)+precis
      call radapt(func,x1,x2,1,rteps,0.,tpart2,uncert)
      dtpar2 = tpart2-tpctil
      dtabs = abs(dtpar2)
      if(abs(xincr)/refx .lt. precis) goto 545
      if(dtabs .lt. dtbest) goto 545
      xincr = xincr * 0.5
      goto 535
  545 dtbest = dtabs
      xbest = x
      tpart = tpart2
      f = func(x)
      if(f .lt. 0.) goto 900
      if(dtabs .lt. rteps*tpctil) goto 580
  550 continue
      write(*,'(A,I4)')                                                 &
     &' FUNLUX: WARNING from FUNPCT: cannot converge, bin',ibin
!
  580 continue
      xincr = (tpctil-tpart) / max(f,fmin)
      x = xbest + xincr
      xfcum(ibin+1) = x
      f = func(x)
      if(f .lt. 0.) goto 900
  600 continue
!         END OF LOOP OVER BINS
      x1 = xfcum(nlo+nbins-1)
      x2 = xhigh
      call radapt(func,x1,x2,1,rteps,0.,tpart ,uncert)
      aberr = abs(tpart-tpctil)/tftot
!      WRITE(6,1001) IFUNC,XLOW,XHIGH
      if(aberr .gt. rteps)  write(*,1002) aberr
      return
  900 write(*,1000) x,f
      ierr = 1
      return
 1000 format(/' FUNLUX fatal error in FUNPCT: function negative:'/      &
     &,' at X=',e15.6,', F=',e15.6/)
! 1001 FORMAT(' FUNPCT has prepared USER FUNCTION',I3,
!     + ' between',G12.3,' and',G12.3,' for FUNLUX.')
 1002 format(' WARNING: Relative error in cumulative distribution',     &
     &' may be as big as',f10.7)
      end
 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
      subroutine funlux(array,xran,len)
!         Generation of LEN random numbers in any given distribution,
!         by 4-point interpolation in the inverse cumulative distr.
!         which was previously generated by FUNLXP
      implicit none
      real tftot
      common/funint/tftot
      integer len,ibuf,j,j1
      real array,xran,gap,gapinv,tleft,bright,gaps,gapins,x,p,a,b
      dimension array(200)
      dimension xran(len)
!  Bin width for main sequence, and its inverse
      parameter (gap= 1./99.,  gapinv=99.)
!  Top of left tail, bottom of right tail (each tail replaces 2 bins)
      parameter (tleft= 2./99.,bright=97./99.)
!  Bin width for minor sequences (tails), and its inverse
      parameter (gaps=tleft/49.,  gapins=1./gaps)
!
!   The array ARRAY is assumed to have the following structure:
!        ARRAY(1-100) contains the 99 bins of the inverse cumulative
!                     distribution of the entire function.
!        ARRAY(101-150) contains the 49-bin blowup of main bins
!                       1 and 2 (left tail of distribution)
!        ARRAY(151-200) contains the 49-bin blowup of main bins
!                       98 and 99 (right tail of distribution)
!
      call ranlux(xran,len)
 
      do 500 ibuf= 1, len
      x = xran(ibuf)
      j = int(  x    *gapinv) + 1
      if (j .lt. 3)  then
         j1 = int( x *gapins)
             j = j1 + 101
             j = max(j,102)
             j = min(j,148)
         p = (   x -gaps*(j1-1)) * gapins
         a = (p+1.0) * array(j+2) - (p-2.0)*array(j-1)
         b = (p-1.0) * array(j) - p * array(j+1)
         xran(ibuf) = a*p*(p-1.0)*0.16666667 + b*(p+1.)*(p-2.)*0.5
      else if (j .gt. 97)  then
         j1 = int((x-bright)*gapins)
             j = j1 + 151
             j = max(j,152)
             j = min(j,198)
         p = (x -bright -gaps*(j1-1)) * gapins
         a = (p+1.0) * array(j+2) - (p-2.0)*array(j-1)
         b = (p-1.0) * array(j) - p * array(j+1)
         xran(ibuf) = a*p*(p-1.0)*0.16666667 + b*(p+1.)*(p-2.)*0.5
      else
!      J = MAX(J,2)
!      J = MIN(J,98)
         p = (   x -gap*(j-1)) * gapinv
         a = (p+1.) * array(j+2) - (p-2.)*array(j-1)
         b = (p-1.) * array(j) - p * array(j+1)
         xran(ibuf) = a*p*(p-1.)*0.16666667 + b*(p+1.)*(p-2.)*0.5
      endif
  500 continue
      tftot = x
      return
      end
      subroutine funlz(func,x2low,x2high,xlow,xhigh)
!         FIND RANGE WHERE FUNC IS NON-ZERO.
!         WRITTEN 1980, F. JAMES
!         MODIFIED, NOV. 1985, TO FIX BUG AND GENERALIZE
!         TO FIND SIMPLY-CONNECTED NON-ZERO REGION (XLOW,XHIGH)
!         ANYWHERE WITHIN THE GIVEN REGION (X2LOW,H2HIGH).
!            WHERE 'ANYWHERE' MEANS EITHER AT THE LOWER OR UPPER
!            EDGE OF THE GIVEN REGION, OR, IF IN THE MIDDLE,
!            COVERING AT LEAST 1% OF THE GIVEN REGION.
!         OTHERWISE IT IS NOT GUARANTEED TO FIND THE NON-ZERO REGION.
!         IF FUNCTION EVERYWHERE ZERO, FUNLZ SETS XLOW=XHIGH=0.
      implicit none
      external func
      integer logn,nslice,i,k
      real xhigh,xlow,x2high,x2low,func,xmid,xh,xl,xnew
      xlow = x2low
      xhigh = x2high
!         FIND OUT IF FUNCTION IS ZERO AT ONE END OR BOTH
      xmid = xlow
      if (func(xlow) .gt. 0.) go to 120
      xmid = xhigh
      if (func(xhigh) .gt. 0.)  go to 50
!         FUNCTION IS ZERO AT BOTH ENDS,
!         LOOK FOR PLACE WHERE IT IS NON-ZERO.
      do 30 logn= 1, 7
      nslice = 2**logn
      do 20 i= 1, nslice, 2
      xmid = xlow + i * (xhigh-xlow) / nslice
      if (func(xmid) .gt. 0.)  go to 50
   20 continue
   30 continue
!         FALLING THROUGH LOOP MEANS CANNOT FIND NON-ZERO VALUE
      write(*,554)
      write(*,555) xlow, xhigh
      xlow = 0.
      xhigh = 0.
      go to 220
!
   50 continue
!         DELETE 'LEADING' ZERO RANGE
      xh = xmid
      xl = xlow
      do 70 k= 1, 20
      xnew = 0.5*(xh+xl)
      if (func(xnew) .eq. 0.) go to 68
      xh = xnew
      go to 70
   68 xl = xnew
   70 continue
      xlow = xl
      write(*,555) x2low,xlow
  120 continue
      if (func(xhigh) .gt. 0.) go to 220
!         DELETE 'TRAILING' RANGE OF ZEROES
      xl = xmid
      xh = xhigh
      do 170 k= 1, 20
      xnew = 0.5*(xh+xl)
      if (func(xnew) .eq. 0.) go to 168
      xl = xnew
      go to 170
  168 xh = xnew
  170 continue
      xhigh = xh
      write(*,555) xhigh, x2high
!
  220 continue
      return
  554 format('0CANNOT FIND NON-ZERO FUNCTION VALUE')
  555 format(' FUNCTION IS ZERO FROM X=',e12.5,' TO ',e12.5)
      end
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
! $Id: radapt.F,v 1.1.1.1 1996/04/01 15:02:13 mclareni Exp $
!
! $Log: radapt.F,v $
! Revision 1.1.1.1  1996/04/01 15:02:13  mclareni
! Mathlib gen
!
!
      subroutine radapt(f,a,b,nseg,reltol,abstol,res,err)
 
!     RES = Estimated Integral of F from A to B,
!     ERR = Estimated absolute error on RES.
!     NSEG  specifies how the adaptation is to be done:
!        =0   means use previous binning,
!        =1   means fully automatic, adapt until tolerance attained.
!        =n>1 means first split interval into n equal segments,
!             then adapt as necessary to attain tolerance.
!     The specified tolerances are:
!            relative: RELTOL ;  absolute: ABSTOL.
!        It stop s when one OR the other is satisfied, or number of
!        segments exceeds NDIM.  Either TOLA or TOLR (but not both!)
!        can be set to zero, in which case only the other is used.
 
      implicit none
      external f
      integer nseg,ndim,nter,nsegd,i,iter,ibig
      real err,res,abstol,reltol,b,a,xlo,xhi,tval,ters,te,root,xhib,    &
     &bin,xlob,bige,hf,xnew,r1,f
      double precision tvals,terss
 
      parameter (ndim=100)
      parameter (r1 = 1., hf = r1/2.)
 
      dimension xlo(ndim),xhi(ndim),tval(ndim),ters(ndim)
      save xlo,xhi,tval,ters,nter
      data nter /0/
 
      if(nseg .le. 0)  then
       if(nter .eq. 0) then
        nsegd=1
        go to 2
       endif
       tvals=0d0
       terss=0d0
       do 1 i = 1,nter
       call rgs56p(f,xlo(i),xhi(i),tval(i),te)
       ters(i)=te**2
       tvals=tvals+tval(i)
       terss=terss+ters(i)
    1  continue
       root= sqrt(2.*terss)
       go to 9
      endif
      nsegd=min(nseg,ndim)
    2 xhib=a
      bin=(b-a)/nsegd
      do 3 i = 1,nsegd
      xlo(i)=xhib
      xlob=xlo(i)
      xhi(i)=xhib+bin
      if(i .eq. nsegd) xhi(i)=b
      xhib=xhi(i)
      call rgs56p(f,xlob,xhib,tval(i),te)
      ters(i)=te**2
    3 continue
      nter=nsegd
      do 4 iter = 1,ndim
      tvals=tval(1)
      terss=ters(1)
      do 5 i = 2,nter
      tvals=tvals+tval(i)
      terss=terss+ters(i)
    5 continue
      root= sqrt(2.*terss)
      if(root .le. abstol .or. root .le. reltol*abs(tvals)) go to 9
      if(nter .eq. ndim) go to 9
      bige=ters(1)
      ibig=1
      do 6 i = 2,nter
      if(ters(i) .gt. bige) then
       bige=ters(i)
       ibig=i
      endif
    6 continue
      nter=nter+1
      xhi(nter)=xhi(ibig)
      xnew=hf*(xlo(ibig)+xhi(ibig))
      xhi(ibig)=xnew
      xlo(nter)=xnew
      call rgs56p(f,xlo(ibig),xhi(ibig),tval(ibig),te)
      ters(ibig)=te**2
      call rgs56p(f,xlo(nter),xhi(nter),tval(nter),te)
      ters(nter)=te**2
    4 continue
    9 res=tvals
      err=root
      return
      end
 
!cccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
!
! $Id: rgs56p.F,v 1.1.1.1 1996/04/01 15:02:14 mclareni Exp $
!
! $Log: rgs56p.F,v $
! Revision 1.1.1.1  1996/04/01 15:02:14  mclareni
! Mathlib gen
!
!
      subroutine rgs56p(f,a,b,res,err)
      implicit none
      integer i
      real err,res,b,a,f,w6,x6,w5,x5,rang,r1,hf
      double precision e5,e6
 
      parameter (r1 = 1., hf = r1/2.)
      dimension x5(5),w5(5),x6(6),w6(6)
 
      data (x5(i),w5(i),i=1,5)                                          &
     &/4.6910077030668004e-02, 1.1846344252809454e-01,                  &
     &2.3076534494715846e-01, 2.3931433524968324e-01,                   &
     &5.0000000000000000e-01, 2.8444444444444444e-01,                   &
     &7.6923465505284154e-01, 2.3931433524968324e-01,                   &
     &9.5308992296933200e-01, 1.1846344252809454e-01/
 
      data (x6(i),w6(i),i=1,6)                                          &
     &/3.3765242898423989e-02, 8.5662246189585178e-02,                  &
     &1.6939530676686775e-01, 1.8038078652406930e-01,                   &
     &3.8069040695840155e-01, 2.3395696728634552e-01,                   &
     &6.1930959304159845e-01, 2.3395696728634552e-01,                   &
     &8.3060469323313225e-01, 1.8038078652406930e-01,                   &
     &9.6623475710157601e-01, 8.5662246189585178e-02/
 
      rang=b-a
      e5=0d0
      e6=0d0
      do 1 i = 1,5
      e5=e5+w5(i)*f(a+rang*x5(i))
      e6=e6+w6(i)*f(a+rang*x6(i))
    1 continue
      e6=e6+w6(6)*f(a+rang*x6(6))
      res=hf*(e6+e5)*rang
      err=abs((e6-e5)*rang)
      return
      end
!GRD
!
!*********************************************************************
!
! Define INTEGER function MCLOCK that can differ from system to system
!
!*********************************************************************
!
      integer function mclock_liar( )
!
      implicit none
      save
!
      integer    mclock
      integer    count_rate, count_max
      logical    clock_ok
!
!        MCLOCK_LIAR = MCLOCK()
!
      clock_ok = .true.
!
      if (clock_ok) then
!
         call system_clock( mclock, count_rate, count_max )
         if ( count_max .eq. 0 ) then
            clock_ok = .false.
            write(*,*)'INFO>  System Clock not present or not',         &
     &' Responding'
            write(*,*)'INFO>  R.N.G. Reseed operation disabled.'
         endif
!
      endif
!
      mclock_liar = mclock
!
      return
      end
      double precision function ran_gauss(cut)
!*********************************************************************
!
! RAN_GAUSS - will generate a normal distribution from a uniform
!   distribution between [0,1].
!   See "Communications of the ACM", V. 15 (1972), p. 873.
!
! cut - double precision - cut for distribution in units of sigma
!                the cut must be greater than 0.5
!
!*********************************************************************
      implicit none
 
      logical flag
      real rndm4
      double precision x, u1, u2, twopi, r,cut
      save
 
            twopi=8d0*atan(1d0)
    1       if (flag) then
              r = dble(rndm4( ))
              r = max(r, 0.5d0**32)
              r = min(r, 1d0-0.5d0**32)
              u1 = sqrt(-2d0*log( r ))
              u2 = dble(rndm4( ))
              x = u1 * cos(twopi*u2)
            else
              x = u1 * sin(twopi*u2)
            endif
 
          flag = .not. flag
 
!  cut the distribution if cut > 0.5
          if (cut .gt. 0.5d0 .and. abs(x) .gt. cut) goto 1
 
          ran_gauss = x
        return
      end
        subroutine readcollimator
!
      integer I,J,K
      integer mbea,mcor,mcop,mmul,mpa,mran,nbb,nblo,nblz,ncom,ncor1,    &
     &nelb,nele,nema,ninv,nlya,nmac,nmon1,npart,nper,nplo,npos,nran,    &
     &nrco,ntr,nzfz
      parameter(npart = 64,nmac = 1)
      parameter(nele=5000,nblo=400,nper=16,nelb=140,nblz=15000,         &
     &nzfz = 300000,mmul = 11)
      parameter(nran = 280000,ncom = 100,mran = 500,mpa = 6,nrco = 5,   &
     &nema = 15)
      parameter(mcor = 10,mcop = mcor+6, mbea = 15)
      parameter(npos = 20000,nlya = 10000,ninv = 1000,nplo = 20000)
      parameter(nmon1 = 600,ncor1 = 600)
      parameter(ntr = 20,nbb = 160)
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!GRD
!GRD THIS BLOC IS COMMON TO MAINCR, DATEN, TRAUTHIN AND THIN6D
!GRD
!APRIL2005
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
     &,relative, diffusive
!     &systilt_antisymm,dowritetracks,cern
!APRIL2005
!
!      integer nloop,rnd_seed,ibeam,jobnumber,sigsecut2
!JUNE2005
!      integer nloop,rnd_seed,ibeam,jobnumber
!SEPT2005 for slicing process
!      integer nloop,rnd_seed,ibeam,jobnumber,do_thisdis
      integer nloop,rnd_seed,c_offsettilt_seed,ibeam,jobnumber,         &
     &do_thisdis,n_slices,pencil_distr
!JUNE2005
!
!UPGRADE JANUARY 2005
!APRIL2005
!      double precision myenom,mynex,mdex,myney,mdey,nsig_prim,nsig_sec, &
!     &nsig_ter,emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,         &
      double precision myenom,mynex,mdex,myney,mdey,                    &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
!SEPT2005 add these lines for the slicing procedure
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
!SEPT2005,OCT2006 added offset
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,ndr,                            &
     &driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,            &
     &sigsecut3,sigsecut2,enerror,bunchlength
!JUNE2005
!APRIL2005
!
      character*24 name_sel
      character*80 coll_db
      character*16 castordir
!JUNE2005
      character*80 filename_dis
!JUNE2005
!
!UPGRADE JANUARY 2005
!APRIL2005
!JUNE2005
!SEPT2005
      common /grd/ myenom,mynex,mdex,myney,mdey,                        &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
!
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
!
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
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
     & relative,diffusive,
     &sigsecut3,sigsecut2,enerror,                                      &
     &bunchlength,coll_db,name_sel,                                     &
     &castordir,filename_dis,nloop,rnd_seed,c_offsettilt_seed,          &
     &ibeam,jobnumber,do_thisdis,n_slices,pencil_distr,                 &
     &do_coll,                                                          &
!
     &do_select,do_nominal,dowrite_dist,do_oneside,dowrite_impact,      &
     &dowrite_secondary,dowrite_amplitude,radial,systilt_antisymm,      &
     &dowritetracks,cern,do_nsig,do_mingap
!SEPT2005
!JUNE2005
!APRIL2005
!
!--September 2006 -- TW common to readcollimator and collimate2
!      logical           changed_tilt1(max_ncoll)
!      logical           changed_tilt2(max_ncoll)
!      common /tilt/ changed_tilt1, changed_tilt2
!--September 2006
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
! THIS BLOCK IS COMMON TO BOTH THIN6D AND TRAUTHIN SUBROUTINES
!
      integer ieff
      double precision myemitx0,myemity0,myalphay,mybetay,myalphax,     &
     &mybetax,rselect
      common /ralph/ myemitx0,myemity0,myalphax,myalphay,mybetax,       &
     &mybetay,rselect
!
      integer absorbed(npart),counted(npart,numeff)
      double precision neff(numeff),rsig(numeff)
      common  /eff/ neff,rsig,counted,absorbed
!
      integer  nimpact(50)
      double precision sumimpact(50),sqsumimpact(50)
      common  /rimpact/ sumimpact,sqsumimpact,nimpact
!
      integer  nampl(nblz)
      character*16  ename(nblz)
      double precision sum_ax(nblz),sqsum_ax(nblz),sum_ay(nblz),        &
     &sqsum_ay(nblz),sampl(nblz)
      common  /ampl_rev/ sum_ax,sqsum_ax,sum_ay,sqsum_ay,sampl,ename,   &
     &nampl
!
      double precision neffx(numeff),neffy(numeff)
      common /efficiency/ neffx,neffy
!
      integer part_hit(maxn),part_abs(maxn),n_tot_absorbed,n_absorbed   &
     &,part_select(maxn)
      double precision part_impact(maxn)
      common /stats/ part_impact,part_hit,part_abs
      common /n_tot_absorbed/ n_tot_absorbed,n_absorbed
      common /part_select/ part_select
!
      double precision x00(maxn),xp00(maxn),y00(maxn),yp00(maxn)
      common   /beam00/ x00,xp00,y00,yp00
!
      logical firstrun
      common /firstrun/ firstrun
!
      integer nsurvive_end,num_selhit,n_impact
      integer nsurvive(10000000)
      common /outcoll/ nsurvive,num_selhit,n_impact,nsurvive_end
!
      integer napx00
      common /napx00/ napx00
!
      integer  icoll
      common  /icoll/  icoll
!
!UPGRADE January 2005
!     INTEGER DB_NCOLL
      integer db_ncoll
!
      character*16 db_name1(max_ncoll),db_name2(max_ncoll)
      character*6 db_material(max_ncoll)
!APRIL2005
      double precision db_nsig(max_ncoll),db_length(max_ncoll),         &
     &db_offset(max_ncoll),db_rotation(max_ncoll),                      &
     &db_bx(max_ncoll),db_by(max_ncoll),db_tilt(max_ncoll,2),           &
     &db_elense_thickness(max_ncoll),db_elense_j_e(max_ncoll)           &
     &,db_cry_rcurv(max_ncoll),db_cry_rmax(max_ncoll),                  &
     &db_cry_zmax(max_ncoll),db_cry_alayer(max_ncoll),                  &
     &db_cry_orient(max_ncoll),db_cry_tilt(max_ncoll) 
     &,db_miscut(max_ncoll)
     &,db_elens_center_x(max_ncoll),db_elens_center_y(max_ncoll),
     &db_elens_curr(max_ncoll),db_elens_voltage(max_ncoll),
     &db_elens_r2_ov_r1(max_ncoll),
     &db_elens_tune(max_ncoll),
     &db_elens_mult_tune(max_ncoll),db_elens_delta_tune(max_ncoll),
     &db_elens_step_tune(max_ncoll)
     &,db_tm_center_x(max_ncoll), db_tm_center_y(max_ncoll)
     &,db_tm_kick(max_ncoll),db_tm_tune(max_ncoll)
     &,db_tm_mult_tune(max_ncoll),db_tm_delta_tune(max_ncoll)
     &,db_tm_step_tune(max_ncoll)
      
      integer db_elens_op_mode(max_ncoll),
     & db_elens_step_turns(max_ncoll),
     & db_elens_resonant_turns(max_ncoll) 
     & , db_tm_step_turns(max_ncoll)

      logical  db_elens_jitter(max_ncoll),db_elens_radial(max_ncoll)
     & , db_tm_switch(max_ncoll)

      common /colldatabase/ db_nsig,db_length,db_rotation,db_offset,    &
     &db_bx,db_by,db_tilt,db_name1,db_name2,db_material,db_ncoll,       &
     &db_elense_thickness,db_elense_j_e                                 &
     &,db_cry_rcurv,db_cry_rmax,db_cry_zmax,db_cry_alayer,db_cry_orient,&
     &db_cry_tilt,db_miscut
     &,db_elens_center_x,db_elens_center_y,        
     & db_elens_curr,db_elens_voltage,db_elens_r2_ov_r1,
     &db_elens_op_mode,db_elens_tune,
     &db_elens_mult_tune,db_elens_delta_tune,
     &db_elens_step_tune,db_elens_step_turns,db_elens_resonant_turns,
     &db_elens_jitter,db_elens_radial
     &,db_tm_center_x, db_tm_center_y
     &,db_tm_kick,db_tm_tune
     &,db_tm_mult_tune,db_tm_delta_tune
     &,db_tm_step_tune, db_tm_step_turns
     & , db_tm_switch

!      double precision db_length(max_ncoll),db_rotation(max_ncoll),     &
!     &db_offset(max_ncoll),                                             &
!     &db_bx(max_ncoll),db_by(max_ncoll),db_tilt(max_ncoll,2)
!      common /colldatabase/ db_length,db_rotation,db_offset,db_bx,db_by,&
!!    &DB_TILT,DB_NAME1,DB_NAME2,DB_MATERIAL,DB_NCOLL
!     &db_tilt,db_name1,db_name2,db_material,db_ncoll,db_nabs,db_ntot,   &
!     &db_startabs
!APRIL2005
!
      integer cn_impact(max_ncoll),cn_absorbed(max_ncoll)
      double precision caverage(max_ncoll),csigma(max_ncoll)
      common /collsummary/ caverage,csigma,cn_impact,cn_absorbed
!
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn)
      common /coord/ myx,myxp,myy,myyp,myp,mys
!
      integer counted_r(maxn,numeff),counted_x(maxn,numeff),            &
     &counted_y(maxn,numeff),                                           &
     &ieffmax_r(npart),ieffmax_x(npart),ieffmax_y(npart)
      common /counting/ counted_r,counted_x,counted_y,ieffmax_r,        &
     &ieffmax_x, ieffmax_y
!
!APRIL2005
!      integer secondary(maxn),tertiary(maxn),part_hit_before(maxn)
      integer secondary(maxn),tertiary(maxn),other(maxn),               &
     &part_hit_before(maxn)
!APRIL2005
      double precision part_indiv(maxn),part_linteract(maxn)
!
      integer   samplenumber
      character*4 smpl
      character*80 pfile
      common /samplenumber/ pfile,smpl,samplenumber
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
      save
!
!--------------------------------------------------------------------
!++  Read collimator database
!
!      write(*,*) 'reading collimator database'
      open(unit=53,file=coll_db)
!
!      write(*,*) 'inside collimator database'
      I = 0
      read(53,*)
      read(53,*,iostat=ios) db_ncoll
!     write(*,*) 'ios = ',ios
      if (ios.ne.0) then
        write(outlun,*) 'ERR>  Problem reading collimator DB ',ios
        stop
      endif
      if (db_ncoll.gt.max_ncoll) then
         write(*,*) 'ERR> db_ncoll > max_ncoll '
         stop
      endif
!
      do j=1,db_ncoll
      write(*,*) 'inside collimator database',j
      read(53,*)
!GRD
!GRD ALLOW TO RECOGNIZE BOTH CAPITAL AND NORMAL LETTERS
!GRD
        read(53,*,iostat=ios) db_name1(j)
c        write(*,*) 'iDB_name ',db_name1(j)
        if (ios.ne.0) then
          write(outlun,*) 'ERR>  Problem reading collimator DB ', j,ios
          stop
        endif
!
        read(53,*,iostat=ios) db_name2(j)
c        write(*,*) 'iDB_name ',db_name1(j)
!        write(*,*) 'ios = ',ios
        if (ios.ne.0) then
          write(outlun,*) 'ERR>  Problem reading collimator DB ', j,ios
          stop
        endif
!
        if (db_name1(j)(1:2).ne.'TM') then
                read(53,*,iostat=ios) db_nsig(j)
                if (ios.ne.0) then
                   write(outlun,*) 
     &                   'ERR>  Problem reading collimator DB ',j,ios
                  stop
                endif
        else
                db_nsig(j)=0
        endif
c-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-Electron lens-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-
c valentina        
        if (db_name1(j)(1:5).eq.'ELENS') then
                db_material(j)='e-'
        elseif (db_name1(j)(1:2).eq.'TM') then
                db_material(j)='Bl'
        else        
          read(53,*,iostat=ios) db_material(j)
!          write(*,*) 'ios = ',ios
          if (ios.ne.0) then
            write(outlun,*) 'ERR>  Problem reading collimator DB', j,ios
            stop
          endif
        endif



        read(53,*,iostat=ios) db_length(j)
!        write(*,*) 'ios = ',ios
        if (ios.ne.0) then
          write(outlun,*) 'ERR>  Problem reading collimator DB ', j,ios
          stop
        endif
        read(53,*,iostat=ios) db_rotation(j)
!        write(*,*) 'ios = ',ios
        if (ios.ne.0) then
          write(outlun,*) 'ERR>  Problem reading collimator DB ', j,ios
          stop
        endif
        

       
c-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-Electron lens-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-
c valentina        
        if (db_name1(j)(1:5).eq.'ELENS') then                           
         read(53,*,iostat=ios)db_elens_center_x(j), db_elens_center_y(j)
         db_offset(j)=0.
         if (ios.ne.0) then
            write(outlun,*)                                             &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
         endif
        elseif (db_name1(j)(1:2).eq.'TM') then                      
         read(53,*,iostat=ios)db_tm_center_x(j), db_tm_center_y(j)
         db_offset(j)=0.
         if (ios.ne.0) then
            write(outlun,*)                                             &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
         endif
        else
          read(53,*,iostat=ios) db_offset(j)
!          write(*,*) 'ios = ',ios:w
         if (ios.ne.0) then
           write(outlun,*) 'ERR>  Problem reading collimator DB ', j,ios
           stop
         endif
        endif
       
c-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-Valentina-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-
c
c        for crystal I need more parameters to be put in the database
c
        if (db_name1(j)(1:3).EQ.'CRY') then
          READ(53,*,IOSTAT=ios) db_cry_rcurv(j)
          if (ios.NE.0) then
            WRITE(outlun,*) 'ERR>  Problem reading collimator DB ', j,  1
     1      ios
            stop
          endif
c          write(*,*) 'db_cry_rcurv(j)', db_cry_rcurv(j)
          READ(53,*,IOSTAT=ios) db_cry_rmax(j)
          if (ios.NE.0) then
            WRITE(outlun,*) 'ERR>  Problem reading collimator DB ', j,  1
     1      ios
            STOP
          endif
c          write(*,*) 'db_cry_rmax(j)', db_cry_rmax(j) 
          READ(53,*,IOSTAT=ios) db_cry_zmax(j)
          if (ios.NE.0) then
            WRITE(outlun,*) 'ERR>  Problem reading collimator DB ', j,  1
     1      ios
            STOP
          endif
c          write(*,*) 'db_cry_zmax(j)', db_cry_zmax(j) 
          READ(53,*,IOSTAT=ios) db_cry_alayer(j)
          if (ios.NE.0) then
            WRITE(outlun,*) 'ERR>  Problem reading collimator DB ', j,  1
     1      ios
            STOP
          endif
c          write(*,*) 'db_cry_alayer(j)', db_cry_alayer(j)
          READ(53,*,IOSTAT=ios) db_cry_orient(j)
          if (ios.NE.0) then
            WRITE(outlun,*) 'ERR>  Problem reading collimator DB ', j,  1
     1      ios
            STOP
          endif
c          write(*,*) 'db_cry_orient(j)',db_cry_orient(j)
          READ(53,*,IOSTAT=ios) db_cry_tilt(j)
          if (ios.NE.0) then
            WRITE(outlun,*) 'ERR>  Problem reading collimator DB ', j , 1
     1      ios
            STOP
          endif
c          write(*,*) 'db_cry_tilt(j)', db_cry_tilt(j) 
          READ(53,*,IOSTAT=ios) db_miscut(j)
          if (ios.NE.0) then
            WRITE(outlun,*) 'ERR>  Problem reading collimator DB ', j , 1
     1      ios
            STOP
          endif
c          write(*,*) 'db_miscut(j)', db_miscut(j) 
        endif
c 
c-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-Electron lens-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-
        if (db_name1(j)(1:5).eq.'ELENS') then                           &
          read(53,*,iostat=ios) db_elens_curr(j), db_elens_voltage(j) 
          if (ios.ne.0) then
            write(outlun,*)                                             &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
          endif
          read(53,*,iostat=ios) db_elens_r2_ov_r1(j)
          if (ios.ne.0) then
            write(outlun,*)                                             &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
          endif
          read(53,*,iostat=ios) db_elens_op_mode(j), db_elens_tune(j),
     &                          db_elens_mult_tune(j),     
     &                          db_elens_delta_tune(j),
     &                          db_elens_step_tune(j),
     &                          db_elens_step_turns(j),
     &                          db_elens_resonant_turns(j) 
          if (ios.ne.0) then
            write(outlun,*)                                             &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
          endif

          read(53,*,iostat=ios) db_elens_jitter(j),db_elens_radial(j)
          if (ios.ne.0) then
            write(outlun,*)                                             &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
          endif

        endif
c-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-Tune modulation elements TM-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-
        if ( db_name1(j)(1:2) .eq.'TM' ) then                           &
          read(53,*,iostat=ios) db_tm_kick(j)
          if (ios.ne.0) then
            write(outlun,*)                                             &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
          endif
          read(53,*,iostat=ios) db_tm_tune(j),
     &                          db_tm_mult_tune(j),     
     &                          db_tm_delta_tune(j),
     &                          db_tm_step_tune(j),
     &                          db_tm_step_turns(j)
c        write(*,*) 'ios = ',ios
          if (ios.ne.0) then
            write(*,*)                                                  &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
          endif

          read(53,*,iostat=ios) db_tm_switch(j)
          if (ios.ne.0) then
            write(outlun,*)                                             &
     &       'ERR>  Problem reading collimator elense DB ',j,ios
            stop
          endif


        endif
c-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-Valentina-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-~-o-
        read(53,*,iostat=ios) db_bx(j)
        if (ios.ne.0) then
          write(outlun,*) 'ERR>  Problem reading collimator DB ', j,ios
          stop
        endif
        read(53,*,iostat=ios) db_by(j)
        if (ios.ne.0) then
          write(outlun,*) 'ERR>  Problem reading collimator DB ', j,ios
          stop
        endif

      enddo

      write(*,*)"exit DB succesfully"
!
      close(53)
!
      end

      subroutine makedis_tr(mynp, myalphax, myalphay, mybetax, mybetay,    &
     &myemitx0, myemity0, myenom, mynex, mdex, myney, mdey,             &
     &myx, myxp, myy, myyp, myp, mys)
!
!  Generate distribution
!
      implicit none
!
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
!
!++ Vectors of coordinates
!
      logical cut_input
      integer i,j,mynp,nloop
      double precision myx(maxn),myxp(maxn),myy(maxn),myyp(maxn),       &
     &myp(maxn),mys(maxn),myalphax,mybetax,myemitx0,myemitx,mynex,mdex, &
     &mygammax,myalphay,mybetay,myemity0,myemity,myney,mdey,mygammay,   &
     &xsigmax,ysigmay,myenom,nr,ndr
!
      double precision maxnbx, minnbx, mynbx,maxnby, minnby, mynby,
     &    dummy,pdf
      
      integer checkx, checky
!
      real      rndm4
!
!
!
!
      common /cut/ cut_input
!
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
      double precision pi
!
      save
!-----------------------------------------------------------------------
!++  Generate particle distribution
!
!
!++  Generate random distribution, assuming optical parameters at IP1
!
!
!++  Calculate the gammas
!
      pi=4d0*atan(1d0)
      mygammax = (1d0+myalphax**2)/mybetax
      mygammay = (1d0+myalphay**2)/mybetay
!++TW 11/07 reset j, helps if subroutine is called twice 
! was done during try to reset distribution, still needed
! will this subroutine ever called twice? 
      j = 0
!
!
!++  Number of points and generate distribution
!
      write(*,*)
      write(*,*) 'Generation of particle distribution Version 5'
      write(*,*)
      write(*,*) 'This routine generates particles in phase space'
      write(*,*) 'X/XP and Y/YP ellipses, as defined in the input'
      write(*,*) 'parameters. Distribution is triangular in the band.'
      write(*,*) 'X and Y are fully uncorrelated.'
      write(*,*)
!
      write(outlun,*) 'Generation of particle distribution Version 5'
      write(outlun,*)
      write(outlun,*) 'This routine generates particles in phase space'
      write(outlun,*) 'X/XP and Y/YP ellipses, as defined in the input'
      write(outlun,*) 'parameters. Distribution is flat in the band.'
      write(outlun,*) 'X and Y are fully uncorrelated.'
      write(outlun,*)
      write(outlun,*) 'INFO>  Number of particles   = ', mynp
      write(outlun,*) 'INFO>  Av number of x sigmas = ', mynex
      write(outlun,*) 'INFO>  +- spread in x sigmas = ', mdex
      write(outlun,*) 'I0NFO>  Av number of y sigmas = ', myney
      write(outlun,*) 'INFO>  +- spread in y sigmas = ', mdey
      write(outlun,*) 'INFO>  Nominal beam energy   = ', myenom
      write(outlun,*) 'INFO>  Sigma_x0 = ', sqrt(mybetax*myemitx0)
      write(outlun,*) 'INFO>  Sigma_y0 = ', sqrt(mybetay*myemity0)
      write(outlun,*) 'INFO>  Beta x   = ', mybetax
      write(outlun,*) 'INFO>  Beta y   = ', mybetay
      write(outlun,*) 'INFO>  Alpha x  = ', myalphax
      write(outlun,*) 'INFO>  Alpha y  = ', myalphay
      write(outlun,*) 'INFO>  DISP x  = '
      write(outlun,*) 'INFO>  DISP y  = '
!
      j=1;
      do while (j.le.mynp)
!
        checkx=0
        if (  mdex .ne.0 ) then
                maxnbx=mynex+mdex
                minnbx=mynex-mdex
                if (minnbx.lt.0.) then
                        WRITE(*,*) "error in setting X distribution"
                        stop
                endif
                mynbx=minnbx + dble(rndm4())*(maxnbx-minnbx)
                dummy=dble(rndm4())
                pdf=-1/(maxnbx-minnbx)*mynbx+maxnbx/(maxnbx-minnbx);
                if ( dummy .le. pdf ) checkx=1
        else
                mynbx=mynex
                checkx=1
        endif

        myemitx = myemitx0*(mynbx)**2
        xsigmax = sqrt(mybetax*myemitx)
        myx(j)   = xsigmax * sin(2d0*pi*rndm4())
        if (rndm4().gt.0.5) then
                myxp(j)  = sqrt(myemitx/mybetax-myx(j)**2
     &          /mybetax**2)- myalphax*myx(j)/mybetax
        else
                myxp(j)  = -1*sqrt(myemitx/mybetax-myx(j)**2
     &           /mybetax**2)-myalphax*myx(j)/mybetax
        endif
!
        checky=0
        if (  mdey .ne.0 ) then
                maxnby=myney+mdey
                minnby=myney-mdey
                if (minnby.lt.0.) then
                        WRITE(*,*) "error in setting Y distribution"
                        stop
                endif
                mynby=minnby + dble(rndm4())*(maxnby-minnby)
                dummy=dble(rndm4())
                pdf=-1/(maxnby-minnby)*mynby+maxnby/(maxnby-minnby);
                if ( dummy .le. pdf ) checky=1
        else
                mynby=myney
                checky=1
        endif

        myemity = myemity0*(mynby )**2
        ysigmay = sqrt(mybetay*myemity)
        myy(j)   = ysigmay * sin(2d0*pi*rndm4())
        if (rndm4().gt.0.5) then
             myyp(j)  = sqrt(myemity/mybetay-myy(j)**2
     &       /mybetay**2)- myalphay*myy(j)/mybetay
        else
             myyp(j)  = -1*sqrt(myemity/mybetay-myy(j)**2/
     &       mybetay**2)-myalphay*myy(j)/mybetay
        endif

        myp(j)   = myenom
        mys(j)   = 0d0
        if ( (checky+checkx) .eq. 2 ) j=j+1
!
      end do
!
      return
      end
!
!========================================================================
!
