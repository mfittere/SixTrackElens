      subroutine errf(xx,yy,wx,wy)
!----------------------------------------------------------------------*
! purpose:                                                             *
!   modification of wwerf, double precision complex error function,    *
!   written at cern by k. koelbig.                                     *
!   taken from mad8                                                    *
! input:                                                               *
!   xx, yy    (real)    argument to cerf.                              *
! output:                                                              *
!   wx, wy    (real)    function result.                               *
!----------------------------------------------------------------------*
!---- double precision version.
      implicit none
      integer n,nc,nu
      double precision cc,h,one,q,rx,ry,saux,sx,sy,tn,two,tx,ty,wx,wy,x,&
     &xh,xl,xlim,xx,y,yh,ylim,yy
      parameter(cc = 1.12837916709551d0)
      parameter(one = 1.d0)
      parameter(two = 2.d0)
      parameter(xlim = 5.33d0)
      parameter(ylim = 4.29d0)
      dimension rx(33),ry(33)
      save
!-----------------------------------------------------------------------
      x=abs(xx)
      y=abs(yy)
      if(y.lt.ylim.and.x.lt.xlim) then
        q=(one-y/ylim)*sqrt(one-(x/xlim)**2)
        h=one/(3.2d0*q)
        nc=7+int(23.0*q)
        xl=h**(1-nc)
        xh=y+0.5d0/h
        yh=x
        nu=10+int(21d0*q)
        rx(nu+1)=0d0
        ry(nu+1)=0d0
        do 10 n=nu,1,-1
          tx=xh+n*rx(n+1)
          ty=yh-n*ry(n+1)
          tn=tx*tx+ty*ty
          rx(n)=0.5d0*tx/tn
          ry(n)=0.5d0*ty/tn
   10   continue
        sx=0d0
        sy=0d0
        do 20 n=nc,1,-1
          saux=sx+xl
          sx=rx(n)*saux-ry(n)*sy
          sy=rx(n)*sy+ry(n)*saux
          xl=h*xl
   20   continue
        wx=cc*sx
        wy=cc*sy
      else
        xh=y
        yh=x
        rx(1)=0d0
        ry(1)=0d0
        do 30 n=9,1,-1
          tx=xh+n*rx(1)
          ty=yh-n*ry(1)
          tn=tx*tx+ty*ty
          rx(1)=0.5d0*tx/tn
          ry(1)=0.5d0*ty/tn
   30   continue
        wx=cc*rx(1)
        wy=cc*ry(1)
      endif
!      if(y.eq.0.) wx=exp(-x**2)
      if(yy.lt.0.) then
        wx=two*exp(y*y-x*x)*cos(two*x*y)-wx
        wy=-two*exp(y*y-x*x)*sin(two*x*y)-wy
        if(xx.gt.0.) wy=-wy
      else
        if(xx.lt.0.) wy=-wy
      endif
      end
      subroutine wzsubv(napx,vx,vy,vu,vv)
!  *********************************************************************
!
!  This subroutine sets u=real(w(z)) and v=imag(w(z)), where z=x+i*y and
!  where w(z) is the complex error function defined by formula 7.1.3 in
!  "Handbook of Mathematical functions [eds. M.Abramowitz & I.A.Stegun,
!  Washington, 1966].  The absolute error of the computed value is less
!  than 1E-8.
!
!  *** Note.  Subroutine WZSET must have been called before this sub-
!  routine can be used.
!
!  For (x,y) inside the rectangle with opposite corners (xcut,0) and
!  (0,ycut), where xcut and ycut have been set by WZSET, an interpo-
!  lation formula is used.  For (x,y) outside this rectangle, a two-
!  term rational approximation is used.
!
!  (G.A.Erskine, 29.09.1997)
!
!  Vectorised for up to 64 argument values by E.McIntosh, 30.10.1997.
!
!
!  Third-order divided-difference interpolation over the corners of a
!  square [e.g. formula (2.5.1) in "Introduction to Numerical Analysis"
!  (F.B.Hildebrand New York, 1957), but with complex nodes and
!  function values].
!
!  In the interpolation formula the corners of the grid square contain-
!  ing (x,y) are numbered (0,0)=3, (h,0)=4, (h,h)=1, (0,h)=2.
!  Identifiers d, dd and ddd denote divided-differences of orders 1, 2
!  and 3 respectively, and a preceding 't' indicates twice the value.
!
!
!  Two-term rational approximation to w(z) [Footnote to Table 7.9
!  in "Handbook of Mathematical Functions (eds. M.Abramowitz &
!  I.A.Stegun, Washington, 1966), but with additional digits in
!  the constants]:
!              u+i*v = i*z*( a1/(z**2-b1) + a2/(z**2-b2) ).
!  Maximum absolute error:
!        <1.E-6  for  x>=4.9  or  y>=4.4
!        <1.E-7  for  x>=6.1  or  y>=5.7
!        <1.E-8  for  x>=7.8  or  y>=7.5
!
!  *********************************************************************
      implicit none
      integer j,k,napx,vmu,vnu
      double precision a1,a2,b1,b2,ss,vd12i,vd12r,vd23i,vd23r,          &
     &vd34i,vd34r,vp,vq,vqsq,vr,vsimag,vsreal,vt,vtdd13i,vtdd13r,       &
     &vtdd24i,vtdd24r,vtdddi,vtdddr,vti,vtr,vu,vusum,vusum3,vv,         &
     &vvsum,vvsum3,vw1i,vw1r,vw2i,vw2r,vw3i,vw3r,vw4i,vw4r,vx,          &
     &vxh,vxhrel,vy,vyh,vyhrel
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
      integer idim,kstep,nx,ny
      double precision h,half,hrecip,one,wtimag,wtreal,xcut,ycut
      parameter ( xcut = 7.77d0, ycut = 7.46d0 )
      parameter ( h = 1.d0/63.d0 )
      parameter ( nx = 490, ny = 470 )
      parameter ( idim = (nx+2)*(ny+2) )
      parameter ( half = 0.5d0, one = 1.d0 )
      common /wzcom1/ hrecip, kstep
      common /wzcom2/ wtreal(idim), wtimag(idim)
      parameter ( a1 = 0.5124242248d0, a2 = 0.0517653588d0 )
      parameter ( b1 = 0.2752551286d0, b2 = 2.7247448714d0 )
!     temporary arrays to facilitate vectorisation
      dimension vx(npart),vy(npart),vu(npart),vv(npart)
      dimension vsreal(npart),vsimag(npart),vp(npart),vq(npart)
      dimension vqsq(npart),vt(npart),vr(npart)
      dimension vxh(npart),vyh(npart),vmu(npart),vnu(npart)
      dimension vw4r(npart),vw4i(npart)
      dimension vw3r(npart),vw3i(npart),vd34r(npart),vd34i(npart)
      dimension vw2r(npart),vw2i(npart),vd23r(npart),vd23i(npart)
      dimension vtr(npart),vti(npart),vtdd24r(npart),vtdd24i(npart)
      dimension vw1r(npart),vw1i(npart)
      dimension vd12r(npart),vd12i(npart),vtdd13r(npart),vtdd13i(npart)
      dimension vtdddr(npart),vtdddi(npart),vxhrel(npart),vyhrel(npart)
      dimension vusum(npart),vvsum(npart),vusum3(npart),vvsum3(npart)
      save
!-----------------------------------------------------------------------
      ss=0d0
      do j=1,napx
!     if ( vx.ge.xcut .or. vy.ge.ycut )
        ss=ss+sign(1.0d0,                                               &
     &sign(1.0d0,vx(j)-xcut)+sign(1.0d0,vy(j)-ycut))
      enddo
!
      if (nint(ss).eq.napx) then
!     everything outside the rectangle so approximate
 
        do j=1,napx
 
      vp(j)=vx(j)**2-vy(j)**2
      vq(j)=2.d0*vx(j)*vy(j)
      vqsq(j)=vq(j)**2
!  First term.
      vt(j)=vp(j)-b1
      vr(j)=a1/(vt(j)**2+vqsq(j))
      vsreal(j)=vr(j)*vt(j)
      vsimag(j)=-vr(j)*vq(j)
!  Second term
      vt(j)=vp(j)-b2
      vr(j)=a2/(vt(j)**2+vqsq(j))
      vsreal(j)=vsreal(j)+vr(j)*vt(j)
      vsimag(j)=vsimag(j)-vr(j)*vq(j)
!  Multiply by i*z.
      vu(j)=-(vy(j)*vsreal(j)+vx(j)*vsimag(j))
      vv(j)=vx(j)*vsreal(j)-vy(j)*vsimag(j)
        enddo
 
      elseif (nint(ss).ne.-napx) then
!     we have a mixture
 
        do j=1,napx
 
          if ( vx(j).ge.xcut .or. vy(j).ge.ycut ) then
 
      vp(j)=vx(j)**2-vy(j)**2
      vq(j)=2.d0*vx(j)*vy(j)
      vqsq(j)=vq(j)**2
!  First term.
      vt(j)=vp(j)-b1
      vr(j)=a1/(vt(j)**2+vqsq(j))
      vsreal(j)=vr(j)*vt(j)
      vsimag(j)=-vr(j)*vq(j)
!  Second term
      vt(j)=vp(j)-b2
      vr(j)=a2/(vt(j)**2+vqsq(j))
      vsreal(j)=vsreal(j)+vr(j)*vt(j)
      vsimag(j)=vsimag(j)-vr(j)*vq(j)
!  Multiply by i*z.
      vu(j)=-(vy(j)*vsreal(j)+vx(j)*vsimag(j))
      vv(j)=vx(j)*vsreal(j)-vy(j)*vsimag(j)
 
          else
 
      vxh(j) = hrecip*vx(j)
      vyh(j) = hrecip*vy(j)
      vmu(j) = int(vxh(j))
      vnu(j) = int(vyh(j))
!  Compute divided differences.
      k = 2 + vmu(j) + vnu(j)*kstep
      vw4r(j) = wtreal(k)
      vw4i(j) = wtimag(k)
      k = k - 1
      vw3r(j) = wtreal(k)
      vw3i(j) = wtimag(k)
      vd34r(j) = vw4r(j) - vw3r(j)
      vd34i(j) = vw4i(j) - vw3i(j)
      k = k + kstep
      vw2r(j) = wtreal(k)
      vw2i(j) = wtimag(k)
      vd23r(j) = vw2i(j) - vw3i(j)
      vd23i(j) = vw3r(j) - vw2r(j)
      vtr(j) = vd23r(j) - vd34r(j)
      vti(j) = vd23i(j) - vd34i(j)
      vtdd24r(j) = vti(j) - vtr(j)
      vtdd24i(j) = - ( vtr(j) + vti(j) )
      k = k + 1
      vw1r(j) = wtreal(k)
      vw1i(j) = wtimag(k)
      vd12r(j) = vw1r(j) - vw2r(j)
      vd12i(j) = vw1i(j) - vw2i(j)
      vtr(j) = vd12r(j) - vd23r(j)
      vti(j) = vd12i(j) - vd23i(j)
      vtdd13r(j) = vtr(j) + vti(j)
      vtdd13i(j) = vti(j) - vtr(j)
      vtdddr(j) = vtdd13i(j) - vtdd24i(j)
      vtdddi(j) = vtdd24r(j) - vtdd13r(j)
!  Evaluate polynomial.
      vxhrel(j) = vxh(j) - dble(vmu(j))
      vyhrel(j) = vyh(j) - dble(vnu(j))
      vusum3(j)=half*(vtdd13r(j)+                                       &
     &(vxhrel(j)*vtdddr(j)-vyhrel(j)*vtdddi(j)))
      vvsum3(j)=half*(vtdd13i(j)+                                       &
     &(vxhrel(j)*vtdddi(j)+vyhrel(j)*vtdddr(j)))
      vyhrel(j) = vyhrel(j) - one
      vusum(j)=vd12r(j)+(vxhrel(j)*vusum3(j)-vyhrel(j)*vvsum3(j))
      vvsum(j)=vd12i(j)+(vxhrel(j)*vvsum3(j)+vyhrel(j)*vusum3(j))
      vxhrel(j) = vxhrel(j) - one
      vu(j)=vw1r(j)+(vxhrel(j)*vusum(j)-vyhrel(j)*vvsum(j))
      vv(j)=vw1i(j)+(vxhrel(j)*vvsum(j)+vyhrel(j)*vusum(j))
 
          endif
 
        enddo
 
      else
!     everything inside the square, so interpolate
 
        do j=1,napx
 
      vxh(j) = hrecip*vx(j)
      vyh(j) = hrecip*vy(j)
      vmu(j) = int(vxh(j))
      vnu(j) = int(vyh(j))
!  Compute divided differences.
      k = 2 + vmu(j) + vnu(j)*kstep
      vw4r(j) = wtreal(k)
      vw4i(j) = wtimag(k)
      k = k - 1
      vw3r(j) = wtreal(k)
      vw3i(j) = wtimag(k)
      vd34r(j) = vw4r(j) - vw3r(j)
      vd34i(j) = vw4i(j) - vw3i(j)
      k = k + kstep
      vw2r(j) = wtreal(k)
      vw2i(j) = wtimag(k)
      vd23r(j) = vw2i(j) - vw3i(j)
      vd23i(j) = vw3r(j) - vw2r(j)
      vtr(j) = vd23r(j) - vd34r(j)
      vti(j) = vd23i(j) - vd34i(j)
      vtdd24r(j) = vti(j) - vtr(j)
      vtdd24i(j) = - ( vtr(j) + vti(j) )
      k = k + 1
      vw1r(j) = wtreal(k)
      vw1i(j) = wtimag(k)
      vd12r(j) = vw1r(j) - vw2r(j)
      vd12i(j) = vw1i(j) - vw2i(j)
      vtr(j) = vd12r(j) - vd23r(j)
      vti(j) = vd12i(j) - vd23i(j)
      vtdd13r(j) = vtr(j) + vti(j)
      vtdd13i(j) = vti(j) - vtr(j)
      vtdddr(j) = vtdd13i(j) - vtdd24i(j)
      vtdddi(j) = vtdd24r(j) - vtdd13r(j)
!  Evaluate polynomial.
      vxhrel(j) = vxh(j) - dble(vmu(j))
      vyhrel(j) = vyh(j) - dble(vnu(j))
      vusum3(j)=half*(vtdd13r(j)+                                       &
     &(vxhrel(j)*vtdddr(j)-vyhrel(j)*vtdddi(j)))
      vvsum3(j)=half*(vtdd13i(j)+                                       &
     &(vxhrel(j)*vtdddi(j)+vyhrel(j)*vtdddr(j)))
      vyhrel(j) = vyhrel(j) - one
      vusum(j)=vd12r(j)+(vxhrel(j)*vusum3(j)-vyhrel(j)*vvsum3(j))
      vvsum(j)=vd12i(j)+(vxhrel(j)*vvsum3(j)+vyhrel(j)*vusum3(j))
      vxhrel(j) = vxhrel(j) - one
      vu(j)=vw1r(j)+(vxhrel(j)*vusum(j)-vyhrel(j)*vvsum(j))
      vv(j)=vw1i(j)+(vxhrel(j)*vvsum(j)+vyhrel(j)*vusum(j))
        enddo
      endif
      return
      end
      subroutine wzsub(x,y,u,v)
!  *********************************************************************
!
!  This subroutine sets u=real(w(z)) and v=imag(w(z)), where z=x+i*y and
!  where w(z) is the complex error function defined by formula 7.1.3 in
!  "Handbook of Mathematical functions [eds. M.Abramowitz & I.A.Stegun,
!  Washington, 1966].  The absolute error of the computed value is less
!  than 1E-8.
!
!  *** Note.  Subroutine WZSET must have been called before this sub-
!  routine can be used.
!
!  For (x,y) inside the rectangle with opposite corners (xcut,0) and
!  (0,ycut), where xcut and ycut have been set by WZSET, an interpo-
!  lation formula is used.  For (x,y) outside this rectangle, a two-
!  term rational approximation is used.
!
!  (G.A.Erskine, 29.09.1997)
!
!
!  Third-order divided-difference interpolation over the corners of a
!  square [e.g. formula (2.5.1) in "Introduction to Numerical Analysis"
!  (F.B.Hildebrand New York, 1957), but with complex nodes and
!  function values].
!
!  In the interpolation formula the corners of the grid square contain-
!  ing (x,y) are numbered (0,0)=3, (h,0)=4, (h,h)=1, (0,h)=2.
!  Identifiers d, dd and ddd denote divided-differences of orders 1, 2
!  and 3 respectively, and a preceding 't' indicates twice the value.
!
!  *********************************************************************
      implicit none
      integer k,mu,nu
      double precision a1,a2,b1,b2,d12i,d12r,d23i,d23r,d34i,d34r,p,     &
     &q,qsq,r,simag,sreal,t,tdd13i,tdd13r,tdd24i,tdd24r,tdddi,tdddr,ti, &
     &tr,u,usum,usum3,v,vsum,vsum3,w1i,w1r,w2i,w2r,w3i,w3r,w4i,w4r,x,xh,&
     &xhrel,y,yh,yhrel
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
      integer idim,kstep,nx,ny
      double precision h,half,hrecip,one,wtimag,wtreal,xcut,ycut
      parameter ( xcut = 7.77d0, ycut = 7.46d0 )
      parameter ( h = 1.d0/63.d0 )
      parameter ( nx = 490, ny = 470 )
      parameter ( idim = (nx+2)*(ny+2) )
      parameter ( half = 0.5d0, one = 1.d0 )
      common /wzcom1/ hrecip, kstep
      common /wzcom2/ wtreal(idim), wtimag(idim)
      parameter ( a1 = 0.5124242248d0, a2 = 0.0517653588d0 )
      parameter ( b1 = 0.2752551286d0, b2 = 2.7247448714d0 )
      save
!-----------------------------------------------------------------------
      if ( x.ge.xcut .or. y.ge.ycut ) goto 1000
      xh = hrecip*x
      yh = hrecip*y
      mu = int(xh)
      nu = int(yh)
!  Compute divided differences.
      k = 2 + mu + nu*kstep
      w4r = wtreal(k)
      w4i = wtimag(k)
      k = k - 1
      w3r = wtreal(k)
      w3i = wtimag(k)
      d34r = w4r - w3r
      d34i = w4i - w3i
      k = k + kstep
      w2r = wtreal(k)
      w2i = wtimag(k)
      d23r = w2i - w3i
      d23i = w3r - w2r
      tr = d23r - d34r
      ti = d23i - d34i
      tdd24r = ti - tr
      tdd24i = - ( tr + ti )
      k = k + 1
      w1r = wtreal(k)
      w1i = wtimag(k)
      d12r = w1r - w2r
      d12i = w1i - w2i
      tr = d12r - d23r
      ti = d12i - d23i
      tdd13r = tr + ti
      tdd13i = ti - tr
      tdddr = tdd13i - tdd24i
      tdddi = tdd24r - tdd13r
!  Evaluate polynomial.
      xhrel = xh - dble(mu)
      yhrel = yh - dble(nu)
      usum3 = half*( tdd13r + ( xhrel*tdddr - yhrel*tdddi ) )
      vsum3 = half*( tdd13i + ( xhrel*tdddi + yhrel*tdddr ) )
      yhrel = yhrel - one
      usum = d12r + ( xhrel*usum3 - yhrel*vsum3 )
      vsum = d12i + ( xhrel*vsum3 + yhrel*usum3 )
      xhrel = xhrel - one
      u = w1r + ( xhrel*usum - yhrel*vsum )
      v = w1i + ( xhrel*vsum + yhrel*usum )
      return
!
!  Two-term rational approximation to w(z) [Footnote to Table 7.9
!  in "Handbook of Mathematical Functions (eds. M.Abramowitz &
!  I.A.Stegun, Washington, 1966), but with additional digits in
!  the constants]:
!              u+i*v = i*z*( a1/(z**2-b1) + a2/(z**2-b2) ).
!  Maximum absolute error:
!        <1.E-6  for  x>=4.9  or  y>=4.4
!        <1.E-7  for  x>=6.1  or  y>=5.7
!        <1.E-8  for  x>=7.8  or  y>=7.5
!
 1000 p=x**2-y**2
      q=2.d0*x*y
      qsq=q**2
!  First term.
      t=p-b1
      r=a1/(t**2+qsq)
      sreal=r*t
      simag=-r*q
!  Second term
      t=p-b2
      r=a2/(t**2+qsq)
      sreal=sreal+r*t
      simag=simag-r*q
!  Multiply by i*z.
      u=-(y*sreal+x*simag)
      v=x*sreal-y*simag
      return
!
      end
      subroutine adia(numx,e0f)
!-----------------------------------------------------------------------
!  ADIABATIC ENERGY-INCREASE
!-----------------------------------------------------------------------
      implicit none
      integer numx
      double precision e0f
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
      save
!-----------------------------------------------------------------------
      if(numx.eq.1) phas0=phas
      if(numx.le.nde(1)) phas=zero
      if(numx.le.nde(1)) return
      if(numx.gt.nde(2)) phas=zero
      if(numx.gt.nde(2)) return
      phas=phas0
      e0=e0+hsy(1)*sin(phas)
      e0f=sqrt(e0*e0-pma*pma)
      return
      end
      subroutine adib(e0f)
!-----------------------------------------------------------------------
!  ADIABATIC ENERGY-DECREASE
!-----------------------------------------------------------------------
      implicit none
      double precision e0f
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
      save
!-----------------------------------------------------------------------
      if(abs(phas0).le.pieni) return
      e0=e0+hsy(1)*sin(phas)
      e0f=sqrt(e0*e0-pma*pma)
      return
      end
      subroutine daten
!-----------------------------------------------------------------------
!  READS INPUT DATA FROM FILE FORT.3 AND/OR FORT.2
!-----------------------------------------------------------------------
      implicit none
      integer i,i1,i2,ia,icc,ichrom0,iclr,ico,icy,idi,iexnum,iexread,   &
     &ifiend16,ifiend8,ii,il1,ilin0,im,imo,imod,imtr0,irecuin,iw,iw0,ix,&
     &izu,j,j0,j1,j2,jj,k,k0,k10,k11,ka,ke,ki,kk,kpz,kzz,l,l1,l2,l3,l4, &
     &ll,m,mblozz,mout,mout1,mout3,mout4,nac,nbidu,ncy2,ndum,nfb,nft
      double precision ak0d,akad,alc,alignx,alignz,apxx,apzz,bk0d,bkad, &
     &cosy,dummy,emitnx,emitny,extaux,halc,halc2,halc3,harm,phag,pmat,  &
     &qbet,qigam,r0,r0a,ram,rdev,rfr,rmean,rph,rsqsum,rsum,rv,tilt,u0,  &
     &xang,xpl0,xplane,xrms0,zpl0,zrms0
      character*16 sing,stru,prin,trac,diff,sync,ende,bloc,comm
      character*16 fluc,chro,tune,iter,limi,orbi,deco
      character*16 beze,bez0,go,rect,elli,comb,sear,subr,reso,bezext
      character*16 free,geom,cavi,disp
      character*16 idat,next,mult,line,init,ic0,imn,icel,irel
      character*16 iss,iqq,iele,ilm,ilm0,idum,corr,norm
      character*16 kl,kr,orga,post,ripp,beam,trom
      character*16 coll
      character*60 ihead
      character*80 ch
      character*160 ch1
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
!
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
!      parameter (max_ncoll=75,max_npart=20000,nc=32,numeff=19,          &
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,          &
     &maxn=20000,outlun=54)
      logical cut_input
      common /cut/ cut_input
!
      double precision rselect,remitxn,remityn,remitx,remity
      common  /remit/ remitxn,remityn,remitx,remity
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
!GRD
!GRD THIS BLOC IS COMMON TO MAINCR, DATEN, TRAUTHIN AND THIN6D
!GRD
!APRIL2005
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
     &,relative ,diffusive
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
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo,nsig_cry,   &
!SEPT2005 add these lines for the slicing procedure
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
!SEPT2005
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
      logical write_c_out, write_SPS_out, write_elens_out
     &   , write_TM_QUAD_out
      common /outputs/ write_c_out, write_SPS_out, write_elens_out
     &   , write_TM_QUAD_out

      character*200 dummyline
!-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
!
!
      dimension icel(ncom,20),iss(2),iqq(5)
      dimension beze(nblo,nelb),ilm(nelb),ilm0(40),bez0(nele),ic0(10)
      dimension extaux(40),bezext(nblz)
      data sing,stru,prin,sync,ende,next,comm                           &
     &/'SING','STRU','PRIN','SYNC','ENDE','NEXT','COMM'/
      data fluc,mult,chro,iter,tune,line,trac,diff                      &
     &/'FLUC','MULT','CHRO','ITER','TUNE','LINE','TRAC','DIFF'/
      data limi,orbi,bloc,init,go,sear,subr,reso,disp,post,ripp,deco    &
     &/'LIMI','ORBI','BLOC','INIT','GO','SEAR','SUBR',                  &
     &'RESO','DISP','POST','RIPP','DECO'/
      data rect,elli,comb,free,geom,cavi,beam,trom                      &
     &/'RE','EL','COMB','FREE','GEOM','CAV','BEAM','TROM'/
      data idum,kl,kr,orga,norm,corr/' ','(',')','ORGA','NORM','CORR'/
      data coll/'COLL'/
      save
!-----------------------------------------------------------------------
      if(mmul.lt.10.or.mmul.gt.20) call prror(85)
      irecuin=0
      iss(1)=' '
      iss(2)=' '
      do 10 i=1,5
        iqq(i)=' '
   10 continue
      do 20 i=1,nele
        bez0(i)=' '
   20 continue
      do 30 i=1,nblo
        do 30 j=1,nelb
          beze(i,j)=' '
   30 continue
      do 40 i=1,40
        ilm0(i)=' '
        extaux(i)=zero
   40 continue
      do 50 i=1,10
        coel(i)=' '
   50 continue
      do 60 i=1,ncom
        do 60 j=1,20
          icel(i,j)=' '
   60 continue
      do 70 i=1,10
        ic0(i)=' '
   70 continue
      do 80 i=1,nelb
        ilm(i)=' '
   80 continue
      emitnx=zero
      emitny=zero
      ihead=' '
      sixtit=' '
      nbidu=0
      iclo6=0
      iclo6r=0
      iclr=0
      icy=0
      ncy=0
      ncy2=0
      ndum=0
      numl=1
      napx=0
      amp(1)=c1m3
      amp0=zero
      ird=0
      imc=0
      idial=0
      idz(1)=1
      idz(2)=1
      idfor=0
      irew=0
      nde(1)=0
      nde(2)=0
      nwr(1)=1
      nwr(2)=1
      nwr(3)=1
      nwr(4)=10000
      ntwin=1
      harm=one
      alc=c1m3
      phag=zero
      tlen=one
      pma=pmap
      ition=0
      dpscor=one
      sigcor=one
      iconv=0
      imad=0
      iskip=1
      cma1=one
      cma2=one
      qs=zero
      itra=0
      chi0=zero
      chid=zero
      rat=zero
      rv=one
      ipos=0
      iav=1
      iwg=1
      dphix=zero
      dphiz=zero
      qx0=zero
      qz0=zero
      ivox=1
      ivoz=1
      ires=1
      dres=one
      ifh=0
      dfft=one
      idis=0
      icow=0
      istw=0
      iffw=0
      nprint=1
      ndafi=1
      itco=50
      dma=c1m12
      dmap=c1m15
      itcro=10
      dech=c1m10
      de0=c1m9
      ded=c1m9
      dsi=c1m9
      dsm0=c1m10
      itqv=10
      dkq=c1m10
      dqq=c1m10
      ichrom=0
      iqmod=0
      im=0
      imtr0=0
      ilin=0
      nlin=0
      iout=0
      idp=0
      izu0=0
      mmac=1
      mcut=0
      mout=0
      mout1=0
      mout2=0
      mout3=0
      mout4=0
      kanf=1
      iclo=0
      isub=0
      irmod2=0
      iorg=0
      ise=0
      irip=0
      irco=0
      iskew=0
      preda=c1m38
   90 read(3,10010,end=1530,iostat=ierro) idat,ihead
      if(ierro.gt.0) call prror(58)
      if(idat(1:1).eq.'/') goto 90
      if(idat.ne.free.and.idat.ne.geom) call prror(1)
      imod=1
      if(idat.eq.geom) imod=2
      write(*,10130)
      write(*,10030)
      write(*,10180) ihead
      sixtit(1:60)=ihead
      if(imod.eq.1) write(*,10190)
      if(imod.eq.2) write(*,10200)
      write(*,10130)
      if(imod.eq.2) then
  100   read(2,10000,end=1520,iostat=ierro) idat
        if(ierro.gt.0) call prror(57)
        if(idat(1:1).eq.'/') goto 100
        if(idat.eq.sing) goto 120
        call prror(15)
      endif
  110 read(3,10000,end=1530,iostat=ierro) idat
      if(ierro.gt.0) call prror(58)
      if(idat(1:1).eq.'/') goto 110
      if(idat.eq.sing) goto 120
      if(idat.eq.bloc) goto 190
      if(idat.eq.stru) goto 320
      if(idat.eq.prin) goto 550
      if(idat.eq.disp) goto 170
      if(idat.eq.tune) goto 600
      if(idat.eq.sync) goto 710
      if(idat.eq.iter) goto 940
      if(idat.eq.fluc) goto 790
      if(idat.eq.mult) goto 740
      if(idat.eq.chro) goto 560
      if(idat.eq.trac) goto 510
      if(idat.eq.diff) goto 520
      if(idat.eq.line) goto 660
      if(idat.eq.limi) goto 950
      if(idat.eq.orbi) goto 980
      if(idat.eq.init) goto 500
      if(idat.eq.comb) goto 1030
      if(idat.eq.subr) goto 1110
      if(idat.eq.reso) goto 1120
      if(idat.eq.sear) goto 1200
      if(idat.eq.orga) goto 880
      if(idat.eq.post) goto 1280
      if(idat.eq.ripp) goto 1290
      if(idat.eq.deco) goto 1320
      if(idat.eq.comm) goto 1390
      if(idat.eq.norm) goto 1400
      if(idat.eq.corr) goto 1410
      if(idat.eq.beam) goto 1600
      if(idat.eq.trom) goto 1700
!GRD
      if(idat.eq.coll) goto 1285
!GRD
      if(idat.eq.next) goto 110
      if(idat.eq.ende) goto 771
      call prror(15)
!-----------------------------------------------------------------------
!  DATENBLOCK SINGLE ELEMENTS
!  ELLEMENTLISTE
!-----------------------------------------------------------------------
  120 i=1
  130 if(imod.eq.1) then
  140   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call prror(58)
        if(ch(1:1).eq.'/') goto 140
        if(ch(:4).eq.next) goto 110
      else if(imod.eq.2) then
  150   read(2,10020,end=1520,iostat=ierro) ch
        if(ierro.gt.0) call prror(57)
        if(ch(1:1).eq.'/') goto 150
        if(ch(:4).eq.next) then
  160     read(2,10000,end=1520,iostat=ierro) idat
          if(ierro.gt.0) call prror(57)
          if(idat(1:1).eq.'/') goto 160
          if(idat.ne.bloc) call prror(15)
          goto 190
        endif
      endif
      call intepr(1,1,ch,ch1)
      read(ch1,*) idat,kz(i),ed(i),ek(i),el(i)
!--CHANGING SIGN OF CURVATURE OF VERTICAL THICK DIPOLE
      if((kz(i).eq.4.or.kz(i).eq.5).and.abs(el(i)).gt.pieni)            &
     &ed(i)=-ed(i)
!--THIN LENS
      if(kz(i).eq.11.and.abs(el(i)+one).le.pieni) then
        dki(i,1) = ed(i)
        dki(i,3) = ek(i)
        ed(i) = one
        ek(i) = one
        el(i) = zero
      else if(kz(i).eq.11.and.abs(el(i)+two).le.pieni) then
        dki(i,2) = ed(i)
        dki(i,3) = ek(i)
        ed(i) = one
        ek(i) = one
        el(i) = zero
      endif
!--CAVITIES
      if(abs(kz(i)).eq.12) then
        if(abs(ed(i)).gt.pieni.and.abs(ek(i)).gt.pieni) then
          ncy2=ncy2+1
          itionc(i)=kz(i)/abs(kz(i))
          kp(i)=6
        endif
        phasc(i)=el(i)
        el(i)=zero
      endif
!--WIRE
      if(abs(kz(i)).eq.15) then
        if(abs(ed(i)*el(i)).le.pieni.or.el(i).le.pieni                  &
     &.or.ek(i).le.pieni) then
           kz(i)=0
           ed(i)=0
           ek(i)=0
           el(i)=0
        else
           wirel(i)=el(i)
           el(i)=0
        endif
      endif
!--ACDIPOLE
      if(abs(kz(i)).eq.16) then
        if(abs(ed(i)).le.pieni.or.ek(i).le.pieni) then
           kz(i)=0
           ed(i)=0
           ek(i)=0
           el(i)=0
        else
           acdipph(i)=el(i)
           el(i)=0
        endif
      endif
!--BEAM-BEAM
      if(kz(i).eq.20) then
        ptnfac(i)=el(i)
        el(i)=zero
      endif
      if(abs(el(i)).gt.pieni.and.kz(i).ne.0) ithick=1
      if(i.gt.nele-1) call prror(16)
      if(abs(kz(i)).ne.12) kp(i)=0
      bez(i)=idat
      bez0(i)=idat
      if(ncy2.eq.0) then
        i=i+1
        il=i
        bez(i)=cavi
        bez0(i)=cavi
        kp(i)=6
      else
        il=i
        i=i+1
      endif
      goto 130
!-----------------------------------------------------------------------
!  DATENBLOCK DISPLACEMENT OF ELEMENTS
!-----------------------------------------------------------------------
  170 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 170
      if(ch(:4).eq.next) goto 110
      call intepr(1,1,ch,ch1)
      xpl0=zero
      xrms0=zero
      zpl0=zero
      zrms0=zero
      read(ch1,*) idat,xpl0,xrms0,zpl0,zrms0
      do 180 j=1,il
        if(idat.ne.bez(j)) goto 180
        xpl(j)=xpl0
        xrms(j)=xrms0
        zpl(j)=zpl0
        zrms(j)=zrms0
!----Insertion for AC dipole
        if(abs(kz(j)).eq.16) then
          nturn1(j)=int(xpl0)
          nturn2(j)=int(xrms0)
          nturn3(j)=int(zpl0)
          nturn4(j)=int(zrms0)
          xpl(j)=0d0
          xrms(j)=0d0
          zpl(j)=0d0
          zrms(j)=0d0
          if(xrms0.eq.0.and.zpl0.eq.0.and.zrms0.eq.0) then
            write(*,*) "ac dipole disregarded (0 length)"
            kz(j)=0
            ed(j)=0
            ek(j)=0
          endif
        endif
  180 continue
      goto 170
!-----------------------------------------------------------------------
!  BLOCK DEFINITIONS
!-----------------------------------------------------------------------
  190 if(imod.eq.1) then
  200   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call prror(58)
        if(ch(1:1).eq.'/') goto 200
      endif
      if(imod.eq.2) then
  210   read(2,10020,end=1520,iostat=ierro) ch
        if(ierro.gt.0) call prror(57)
        if(ch(1:1).eq.'/') goto 210
      endif
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) mper,(msym(k),k=1,mper)
      if(mper.gt.nper) call prror(17)
      i=0
  220 do 230 m=1,40
  230 ilm0(m)=idum
      if(imod.eq.1) then
  240   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call prror(58)
        if(ch(1:1).eq.'/') goto 240
        if(ch(:4).eq.next) goto 110
      else if(imod.eq.2) then
  250   read(2,10020,end=1520,iostat=ierro) ch
        if(ierro.gt.0) call prror(57)
        if(ch(1:1).eq.'/') goto 250
        if(ch(:4).eq.next) then
  260     read(2,10000,end=1520,iostat=ierro) idat
          if(ierro.gt.0) call prror(57)
          if(idat(1:1).eq.'/') goto 260
          if(idat.ne.stru) call prror(15)
          goto 320
        endif
      endif
      call intepr(2,1,ch,ch1)
      read(ch1,*) idat,(ilm0(m),m=1,40)
      if(idat.eq.idum) goto 270
      i=i+1
      if(i.gt.nblo-1) call prror(18)
      bezb(i)=idat
      k0=0
      mblo=i
  270 ka=k0+1
      ke=k0+40
      do 300 l=ka,ke
        if(l.gt.nelb) call prror(26)
        ilm(l)=ilm0(l-k0)
        if(ilm(l).eq.idum) goto 310
        mel(i)=l
        beze(i,l)=ilm(l)
        do 280 j=1,il
          if(bez0(j).eq.ilm(l)) goto 290
  280   continue
        erbez=ilm(l)
        call prror(19)
  290   mtyp(i,l)=j
        if(kz(j).ne.8) elbe(i)=elbe(i)+el(j)
  300 continue
  310 k0=l-1
      goto 220
!-----------------------------------------------------------------------
!  STRUCTURE INPUT
!-----------------------------------------------------------------------
  320 i=0
  330 do 340 k=1,40
  340 ilm0(k)=idum
      if(imod.eq.1) then
  350   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call prror(58)
        if(ch(1:1).eq.'/') goto 350
      endif
      if(imod.eq.2) then
  360   read(2,10020,end=1520,iostat=ierro) ch
        if(ierro.gt.0) call prror(57)
        if(ch(1:1).eq.'/') goto 360
      endif
      if(ch(:4).eq.next) goto 110
      i2=1
      do 420 ii=1,80
        if(ch(ii:ii).eq.kl) then
          if(ii.gt.1) then
            do 370 jj=1,ii-1
  370       if(ch(jj:jj).ne.' ') goto 380
          endif
          iw=1
          goto 390
  380     read(ch(:ii-1),*) iw
  390     ia=i
          iw0=iw-1
          i2=ii+1
          goto 430
        endif
        if(ch(ii:ii).eq.kr) then
          if(iw0.le.0) goto 330
          idi=i-ia
          do 410 k=1,iw0
            do 400 j=1,idi
  400       ic(i+j)=ic(i+j-idi)
            i=i+idi
  410     continue
          mbloz=i
          goto 330
        endif
  420 continue
  430 call intepr(3,i2,ch,ch1)
      read(ch1,*) (ilm0(k),k=1,40)
      do 490 k=1,40
        if(ilm0(k).eq.idum) goto 490
        if(ilm0(k).eq.go) goto 480
        i=i+1
        do 440 j=1,mblo
          if(bezb(j).eq.ilm0(k)) goto 470
  440   continue
        do 450 l=1,il
          if(bez0(l).eq.ilm0(k)) goto 460
  450   continue
        erbez=ilm0(k)
        call prror(20)
  460   continue
        ic(i)=l+nblo
        if(bez0(l).eq.cavi) icy=icy+1
        goto 490
  470   ic(i)=j
        goto 490
  480   kanf=i+1
  490 continue
      mbloz=i
      if(mbloz.gt.nblz-2) call prror(21)
      goto 330
!-----------------------------------------------------------------------
!  INITIAL COORDINATES
!-----------------------------------------------------------------------
  500 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).ne.'/') then
        iclr=iclr+1
      else
        goto 500
      endif
      ch1(:83)=ch(:80)//' / '
      if(iclr.eq.1) then
        read(ch1,*) itra,chi0,chid,rat,iver
        if(itra.gt.2) call prror(40)
      endif
      if(iclr.eq.2) read(ch1,*) exz(1,1)
      if(iclr.eq.3) read(ch1,*) exz(1,2)
      if(iclr.eq.4) read(ch1,*) exz(1,3)
      if(iclr.eq.5) read(ch1,*) exz(1,4)
      if(iclr.eq.6) read(ch1,*) exz(1,5)
      if(iclr.eq.7) read(ch1,*) exz(1,6)
      if(iclr.eq.8) read(ch1,*) exz(2,1)
      if(iclr.eq.9) read(ch1,*) exz(2,2)
      if(iclr.eq.10) read(ch1,*) exz(2,3)
      if(iclr.eq.11) read(ch1,*) exz(2,4)
      if(iclr.eq.12) read(ch1,*) exz(2,5)
      if(iclr.eq.13) read(ch1,*) exz(2,6)
      if(iclr.eq.14) read(ch1,*) e0
      if(iclr.eq.15) read(ch1,*) ej(1)
      if(iclr.eq.16) read(ch1,*) ej(2)
      if(iclr.ne.16) goto 500
      dp1=exz(1,6)
      iclr=0
      if(iver.ne.0.and.iver.ne.1) iver=0
      nbidu=1
      goto 110
!-----------------------------------------------------------------------
!  TRACKING PARAMETERS
!-----------------------------------------------------------------------
  510 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).ne.'/') then
        iclr=iclr+1
      else
        goto 510
      endif
      ch1(:83)=ch(:80)//' / '
      if(iclr.eq.1) read(ch1,*)                                         &
     &numl,numlr,napx,amp(1),amp0,ird,imc,niu(1),niu(2)
      if(iclr.eq.2) read(ch1,*) idz(1),idz(2),idfor,irew,iclo6
      if(iclr.eq.3) read(ch1,*) nde(1),nde(2),                          &
     &nwr(1),nwr(2),nwr(3),nwr(4),ntwin,ibidu
      if(iclo6.eq.5.or.iclo6.eq.6) then
        iclo6=iclo6-4
        iclo6r=1
      endif
      if(iclo6.eq.2.and.idfor.eq.0) idfor=1
      if(iclo6.eq.1.or.iclo6.eq.2) nsix=0
      if(iclr.ne.3) goto 510
      iclr=0
      nbidu=1
      goto 110
!-----------------------------------------------------------------------
!  DIFFERENTIAL ALGEBRA
!-----------------------------------------------------------------------
  520 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 520
      ndum=ndum+1
      if(ch(:4).eq.next) then
        ndum=0
        goto 110
      endif
      if(ndum.eq.1) then
        ch1(:83)=ch(:80)//' / '
        idial=1
        numlr=0
        napx=1
        imc=1
        read(ch1,*) nord,nvar,preda,nsix,ncor
        if(nvar.le.4) ition=0
        if(nord.le.0.or.nvar.le.0) call prror(91)
      else
        call intepr(3,1,ch,ch1)
        read(ch1,*) (ilm0(i),i=1,ncor)
      endif
      if(iclo6.eq.1.or.iclo6.eq.2) nsix=0
      if(nvar.ne.6) then
      nsix=0
      iclo6=0
      endif
      if(nvar.eq.5) then
        idp=1
        ition=1
        hsy(1)=zero
      endif
      if(ndum.eq.1) then
      if(nsix.ne.1) nsix=0
      if(nord.gt.nema) call prror(52)
      nvar2=nvar
      goto 520
      else
      if(ncor.gt.mcor) call prror(65)
      if(ncor.gt.0) then
        do 540 j1=1,ncor
          do 530 j2=1,il
            if(ilm0(j1).eq.bez(j2)) then
              if(el(j2).ne.zero.or.kz(j2).gt.10) call prror(67)
              ipar(j1)=j2
              goto 540
            endif
  530     continue
          call prror(66)
  540   continue
      else
        ncor=0
        write(*,*)' '
        write(*,*)'NO EXTRA PARAMETERS FOR THE MAP SPECIFIED'
        write(*,*)' '
      endif
      ndum=0
      nvar=nvar2+ncor
      goto 110
      endif
!-----------------------------------------------------------------------
!  PRINTOUT INPUT PARAMETERS
!-----------------------------------------------------------------------
  550 iout=1
      goto 110
!-----------------------------------------------------------------------
!  CHROMATCITY ADJUSTMENT
!-----------------------------------------------------------------------
  560 ichrom=1
      ichrom0=0
      do 580 l=1,2
  570 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 570
      call intepr(1,1,ch,ch1)
      if(l.eq.1) read(ch1,*) iss(1),cro(1),ichrom0
      if(l.eq.2) read(ch1,*) iss(2),cro(2)
  580 continue
      do 590 j=1,il
      if(iss(1).eq.bez(j)) is(1)=j
  590 if(iss(2).eq.bez(j)) is(2)=j
      if(ichrom0.ge.1.and.ichrom0.le.3) ichrom=ichrom0
      goto 110
!-----------------------------------------------------------------------
!  TUNE ADJUSTMENT
!-----------------------------------------------------------------------
  600 iqmod=1
      do 630 l=1,3
  610 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 610
      if(ch(:4).eq.next) then
        if(abs(qw0(1)).gt.pieni.and.abs(qw0(2)).gt.pieni) then
          do 620 j=1,il
            if(iqq(1).eq.bez(j)) iq(1)=j
  620     if(iqq(2).eq.bez(j)) iq(2)=j
          goto 110
        else
          write(*,10370)
          iqmod=0
          iqmod6=0
          goto 110
        endif
      endif
      call intepr(1,1,ch,ch1)
      if(l.eq.1) then
        read(ch1,*) iqq(1),qw0(1),iqmod6
        if(iqmod6.eq.1) then
          iqmod6=0
        elseif(iqmod6.eq.2) then
          iqmod6=1
          iqmod=0
        elseif(iqmod6.eq.3) then
          iqmod6=1
        endif
      endif
      if(l.eq.2) read(ch1,*) iqq(2),qw0(2)
      if(l.eq.3) read(ch1,*) iqq(3),qw0(3)
  630 continue
  640 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 640
      call intepr(4,1,ch,ch1)
      read(ch1,*) iqq(4),iqq(5)
      if(abs(qw0(1)).gt.pieni.and.abs(qw0(2)).gt.pieni                  &
     &.and.abs(qw0(3)).gt.pieni) then
        do 650 j=1,il
          if(iqq(1).eq.bez(j)) iq(1)=j
          if(iqq(2).eq.bez(j)) iq(2)=j
          if(iqq(3).eq.bez(j)) iq(3)=j
          if(iqq(4).eq.bez(j)) kpa(j)=1
          if(iqq(5).eq.bez(j)) kpa(j)=2
 650    continue
        goto 110
      else
        write(*,10370)
        iqmod=0
        iqmod6=0
        goto 110
      endif
!-----------------------------------------------------------------------
!  LINEAR OPTICS CALCULATION
!-----------------------------------------------------------------------
 660  continue
      ilin0=1
      read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 660
      if(ch(:4).eq.next) goto 110
      call intepr(1,1,ch,ch1)
      read(ch1,*) idat,nt,ilin0,ntco,eui,euii
      iprint=0
      if(idat.ne.'BLOCK'.and.idat.ne.'ELEMENT') call prror(45)
      if(idat.eq.'BLOCK') iprint=1
      ilin=1
      if(ilin0.ge.1.and.ilin0.le.3) ilin=ilin0
  670 do 680 m=1,40
  680 ilm0(m)=idum
  690 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 690
      if(ch(:4).eq.next) goto 110
      call intepr(2,1,ch,ch1)
      read(ch1,*) (ilm0(m),m=1,40)
      do 700 m=1,40
      if(ilm0(m).eq.idum) goto 700
      nlin=nlin+1
      if(nlin.gt.nele) call prror(81)
      bezl(nlin)=ilm0(m)
  700 continue
      goto 670
!-----------------------------------------------------------------------
!  SYNCHROTRON OSCILLATIONS
!-----------------------------------------------------------------------
  710 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).ne.'/') then
      iclr=iclr+1
      else
      goto 710
      endif
      ch1(:83)=ch(:80)//' / '
      if(iclr.eq.1) read(ch1,*) harm,alc,u0,phag,tlen,pma,ition,dppoff
      if(iclr.eq.2) read(ch1,*) dpscor,sigcor
      if(iclr.ne.2) goto 710
      iclr=0
      if(abs(pma-pmap).le.c1m1) pmat=pmap
      if(abs(pma-pmae).le.c1m1) pmat=pmae
      if(pmat.ne.pmap.and.pmat.ne.pmae) then
        write(*,*)
        write(*,*) 'Warning: Particle is neither proton nor electron'
        write(*,*)
      endif
      if(pma.lt.pieni) call prror(27)
      crad=crade*pmae/pma
      if(abs(tlen).le.pieni) call prror(25)
      if(ncy2.eq.0) then
        ncy=icy*mper
        idp=1
        if(ncy.ne.0) goto 720
        idp=0
        write(*,10130)
        write(*,10340)
  720   phas=phag*rad
        if(ncy.ne.0) then
          hsy(1)=u0/dble(ncy)
        else
          hsy(1)=u0
        endif
        if(nvar.eq.5) then
          idp=1
          ition=1
          hsy(1)=zero
        endif
        halc=harm*alc
        halc2=harm/tlen
        hsy(3)=two*pi*halc2
        cosy=cos(phas)
        qigam=pma*pma/e0/e0
        qbet=one-qigam
        halc3=-(qigam-alc)*ition*harm*u0/e0*cosy/(two*pi*qbet)
        if(halc3.lt.zero) call prror(28)
        qs=sqrt(halc3)
      else
        idp=1
        ncy=0
        do 725 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(abs(kz(ix)).eq.12) ncy=ncy+1
          endif
  725   continue
        do 730 j=1,il
          if(abs(kz(j)).eq.12) then
            hsyc(j)=two*pi*ek(j)/tlen
            if(nvar.eq.5) then
              ition=1
              ed(j)=zero
            endif
          endif
  730   continue
      endif
      goto 110
!-----------------------------------------------------------------------
!  MULTIPOLE COEFFICIENTS  FOR KZ = 11
!-----------------------------------------------------------------------
  740 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 740
      call intepr(1,1,ch,ch1)
      read(ch1,*) imn,r0,benki
      i=1
      r0a=one
      im=im+1
      benkc(im)=benki
      r00(im)=r0
      do 750 j=1,il
      if(imn.eq.bez(j)) then
        irm(j)=im
        goto 760
      endif
  750 continue
  760 write(*,10130)
      write(*,10210) imn,r0,benki
  770 bk0d=zero
      bkad=zero
      ak0d=zero
      akad=zero
  780 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 780
      if(ch(:4).eq.next) goto 110
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) bk0d,bkad,ak0d,akad
      if(abs(bk0d).gt.pieni.or.abs(bkad).gt.pieni                       &
     &.or.abs(ak0d).gt.pieni.or.abs(akad).gt.pieni) nmu(j)=i
      write(*,10220) i,bk0d,bkad,ak0d,akad
      bk0(im,i)=benki*bk0d/r0a
      ak0(im,i)=benki*ak0d/r0a
      bka(im,i)=benki*bkad/r0a
      aka(im,i)=benki*akad/r0a
      i=i+1
      r0a=r0a*r0
      if(i.le.mmul+1) goto 770
      write(*,10380)
      goto 770
!-----------------------------------------------------------------------
!  FLUCTUATION RANDOM STARTING NUMBER
!-----------------------------------------------------------------------
  790 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 790
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) izu0, mmac, mout, mcut
      mcut=iabs(mcut)
      if(mmac.gt.nmac) call prror(55)
      call recuin(izu0,irecuin)
      call ranecu(zfz,nzfz,mcut)
      rsum=zero
      do 800 i=1,nzfz
  800 rsum=rsum+zfz(i)
      rmean=rsum/nzfz
      rsqsum=zero
      do 810 i=1,nzfz
  810 rsqsum=rsqsum+(zfz(i)-rmean)*(zfz(i)-rmean)
      rdev=sqrt(rsqsum/nzfz)
      write(*,10410) izu0,nzfz,rmean,rdev
      if(mcut.eq.0) write(*,10430)
      if(mcut.gt.0) write(*,10440) mcut
      write(*,10130)
      if(mout.ge.8) mout4=1
      if(mout.eq.7.or.mout.eq.15) then
        mout1=1
        mout2=1
        mout3=1
      else if(mout.eq.6.or.mout.eq.14) then
        mout2=1
        mout3=1
      else if(mout.eq.5.or.mout.eq.13) then
        mout1=1
        mout3=1
      else if(mout.eq.4.or.mout.eq.12) then
        mout3=1
      else if(mout.eq.3.or.mout.eq.11) then
        mout1=1
        mout2=1
      else if(mout.eq.2.or.mout.eq.10) then
        mout2=1
      else if(mout.eq.1.or.mout.eq.9) then
        mout1=1
      endif
      if(mout1.eq.1) then
        write(*,*)
        write(*,*) '          Multipole errors read in ' ,              &
     &'from external file'
        write(*,*)
        iexread=0
        ifiend16=0
        iexnum=0
        read(16,10020,end=861)
        rewind 16
        do 860 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(iexread.eq.0) then
              ilm0(1)=' '
! READ IN REGULAR MULTIPOLES FIRST AND THEN THE SKEW COMPONENTS
              if(ifiend16.eq.0) then
                read(16,10020,end=820,iostat=ierro) ch
              else
                goto 820
              endif
              call intepr(3,1,ch,ch1)
              read(ch1,*) ilm0(1)
              iexnum=iexnum+1
              bezext(iexnum)=ilm0(1)
              read(16,*,end=870,iostat=ierro) extaux(1),extaux(2),      &
     &extaux(3)
              read(16,*,end=870,iostat=ierro) extaux(4),extaux(5),      &
     &extaux(6)
              read(16,*,end=870,iostat=ierro) extaux(7),extaux(8),      &
     &extaux(9)
              read(16,*,end=870,iostat=ierro) extaux(10),extaux(11),    &
     &extaux(12)
              read(16,*,end=870,iostat=ierro) extaux(13),extaux(14),    &
     &extaux(15)
              read(16,*,end=870,iostat=ierro) extaux(16),extaux(17),    &
     &extaux(18)
              read(16,*,end=870,iostat=ierro) extaux(19),extaux(20)
              read(16,*,end=870,iostat=ierro) extaux(21),extaux(22),    &
     &extaux(23)
              read(16,*,end=870,iostat=ierro) extaux(24),extaux(25),    &
     &extaux(26)
              read(16,*,end=870,iostat=ierro) extaux(27),extaux(28),    &
     &extaux(29)
              read(16,*,end=870,iostat=ierro) extaux(30),extaux(31),    &
     &extaux(32)
              read(16,*,end=870,iostat=ierro) extaux(33),extaux(34),    &
     &extaux(35)
              read(16,*,end=870,iostat=ierro) extaux(36),extaux(37),    &
     &extaux(38)
              read(16,*,end=870,iostat=ierro) extaux(39),extaux(40)
              if(ierro.gt.0) call prror(80)
              iexread=1
              goto 840
  820         ifiend16=1
              if(iexnum.eq.0) call prror(80)
              do 830 j=1,iexnum
                if(bez(ix).eq.bezext(j)) call prror(80)
  830         continue
  840         continue
            endif
            if(ilm0(1).eq.bez(ix)) then
              icext(i)=ix
              do 850 k=1,40
                exterr(i,k)=extaux(k)
  850         continue
              iexread=0
              goto 860
            endif
          endif
  860   continue
  861   continue
        write(*,*) '        From file fort.16 :',iexnum,                &
     &' values read in.'
        write(*,*)
      endif
      if(mout3.eq.1) then
        write(*,*)
        write(*,*) '          Alignment errors read in ' ,              &
     &'from external file'
        write(*,*)
        iexread=0
        ifiend8=0
        iexnum=0
        read(8,10020,end=1581)
        rewind 8
        do 1580 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(iexread.eq.0) then
              ilm0(1)=' '
! READ IN HORIZONTAL AND VERTICAL MISALIGNMENT AND TILT
              if(ifiend8.eq.0) then
                read(8,10020,end=1550,iostat=ierro) ch
                if(ierro.gt.0) call prror(86)
              else
                goto 1550
              endif
              call intepr(1,1,ch,ch1)
              read(ch1,*) ilm0(1),alignx,alignz,tilt
              iexnum=iexnum+1
              bezext(iexnum)=ilm0(1)
              iexread=1
              goto 1570
 1550         ifiend8=1
              if(iexnum.eq.0) call prror(86)
              do 1560 j=1,iexnum
                if(bez(ix).eq.bezext(j)) call prror(86)
 1560         continue
 1570         continue
            endif
            if(ilm0(1).eq.bez(ix)) then
              icextal(i)=ix
              extalign(i,1)=alignx
              extalign(i,2)=alignz
              extalign(i,3)=tilt
              iexread=0
              goto 1580
            endif
          endif
 1580   continue
 1581   continue
        write(*,*) '        From file fort.8 :',iexnum,                 &
     &' values read in.'
        write(*,*)
      endif
      izu=0
      iexnum=0
      if(mout4.eq.1) then
        read(30,10020,end=1591)
        rewind 30
        do 1590 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            kpz=kp(ix)
            kzz=kz(ix)
            if(kpz.eq.6.or.kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 1590
            izu=izu+3
            read(30,10020,end=1591,iostat=ierro) ch
            if(ierro.gt.0) call prror(87)
            call intepr(1,1,ch,ch1)
            read(ch1,*) ilm0(1),zfz(izu-2)
            iexnum=iexnum+1
            if(kz(ix).eq.11) izu=izu+2*mmul
          endif
 1590   continue
        if(iexnum.gt.0) then
          write(*,*)
          write(*,*) '          Single (random) kick errors read in ' , &
     &'from external file'
          write(*,*)
          write(*,*) '        From file fort.30 :',iexnum,              &
     &' values read in.'
          write(*,*)
        endif
        iexread=0
        ifiend8=0
        iexnum=0
        rewind 30
        do 1593 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(iexread.eq.0) then
 1595         ilm0(1)=' '
! READ IN HORIZONTAL AND VERTICAL MISALIGNMENT AND TILT
              if(ifiend8.eq.0) then
                read(30,10020,end=1594,iostat=ierro) ch
                if(ierro.gt.0) call prror(87)
              else
                goto 1594
              endif
              call intepr(1,1,ch,ch1)
              read(ch1,*) ilm0(1),dummy,alignx,alignz,tilt
              if((abs(alignx)+abs(alignz)+abs(tilt)).le.pieni)          &
     &goto 1595
              iexnum=iexnum+1
              bezext(iexnum)=ilm0(1)
              iexread=1
              goto 1596
 1594         ifiend8=1
              do 1597 j=1,iexnum
                if(bez(ix).eq.bezext(j)) call prror(87)
 1597         continue
 1596         continue
            endif
            if(ilm0(1).eq.bez(ix)) then
              icextal(i)=ix
              extalign(i,1)=alignx
              extalign(i,2)=alignz
              extalign(i,3)=tilt
              iexread=0
              goto 1593
            endif
          endif
 1593   continue
 1591   continue
      endif
      goto 110
  870 call prror(80)
 
!-----------------------------------------------------------------------
!  ORGANISATION OF RANDOM NUMBERS
!-----------------------------------------------------------------------
  880 write(*,10130)
      write(*,10350)
      do 890 i=1,3
      do 890 j=1,nele
  890 bezr(i,j)=idum
  900 iorg=iorg+1
  910 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 910
      if(ch(:4).eq.next) goto 110
      call intepr(3,1,ch,ch1)
      read(ch1,*) idat,bezr(2,iorg),bezr(3,iorg)
      if(idat.ne.next) then
      if(idat.ne.mult.and.idat.ne.idum.and.bezr(2,iorg).eq.idum) write  &
     &(6,10360) idat
      if(idat.ne.mult.and.idat.ne.idum.and.bezr(2,iorg).ne.idum) write  &
     &(6,10390) idat,bezr(2,iorg)
      if(idat.ne.mult) bezr(1,iorg)=idat
      if(idat.eq.mult.and.bezr(2,iorg).ne.idum.and.bezr(3,iorg).ne.idum)&
     &then
        write(*,10400)bezr(2,iorg),bezr(3,iorg)
        im=im+1
        j0=0
        j1=0
        do 920 i=1,il
          if(bez(i).eq.bezr(2,iorg)) j1=i
  920   if(bez(i).eq.bezr(3,iorg)) j0=i
        if(j0.eq.0.or.j1.eq.0.or.kz(j0).ne.11.or.kz(j1).ne.11)          &
     &call prror(29)
        irm(j0)=im
        benkc(j0)=benkc(j1)
        r00(j0)=r00(j1)
        imo=irm(j1)
        nmu(j0)=nmu(j1)
        do 930 i1=1,nmu(j0)
          bk0(im,i1)=bk0(imo,i1)
          bka(im,i1)=bka(imo,i1)
          ak0(im,i1)=ak0(imo,i1)
  930   aka(im,i1)=aka(imo,i1)
      endif
      goto 900
      endif
      write(*,10130)
      goto 110
!-----------------------------------------------------------------------
!  ITERATION ERRORS FOR CLOSED ORBIT ,TUNE ADJUSTMENT AND CHROMATICITY
!-----------------------------------------------------------------------
  940 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).ne.'/') then
      iclr=iclr+1
      else
      goto 940
      endif
      if(ch(:4).eq.next) then
      iclr=0
      goto 110
      endif
      ch1(:83)=ch(:80)//' / '
      if(iclr.eq.1) read(ch1,*) itco,dma,dmap
      if(iclr.eq.2) read(ch1,*) itqv,dkq,dqq
      if(iclr.eq.3) read(ch1,*) itcro,dsm0,dech
      if(iclr.eq.4) read(ch1,*) de0,ded,dsi
      if(iclr.ne.4) goto 940
      iclr=0
      goto 110
!-----------------------------------------------------------------------
!  APERTURE LIMITATIONS
!-----------------------------------------------------------------------
  950 write(*,10320)
  960 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 960
      apxx=zero
      apzz=zero
      call intepr(8,1,ch,ch1)
      read(ch1,*) idat,irel,apxx,apzz
      do 970 j=1,il
      if(idat.ne.bez(j)) goto 970
      kp(j)=1
      if(irel.eq.rect) kp(j)=2
      apx(j)=apxx
      apz(j)=apzz
      if(irel.eq.rect) then
        kp(j)=3
        ape(1,j)=apzz*apzz
        ape(2,j)=apxx*apxx
        ape(3,j)=apxx*apxx*apzz*apzz
      endif
      write(*,10330) bez(j),irel,apxx,apzz
  970 continue
      if(idat.ne.next) goto 960
      goto 110
!-----------------------------------------------------------------------
!  ORBIT CORRECTION
!-----------------------------------------------------------------------
  980 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 980
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) sigma0,ncorru,ncorrep
      iclo=1
  990 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 990
      iele=idum
      call intepr(4,1,ch,ch1)
      read(ch1,*) idat,iele
      if(idat.eq.next) goto 110
      if(idat.ne.'HMON='.and.idat.ne.'HCOR='.and. idat.ne.'VMON='.and.  &
     &idat.ne.'VCOR=') call prror(44)
      if(idat.eq.'HMON='.or.idat.eq.'VMON=') goto 1010
      do 1000 j=1,il
      if(iele.ne.bez(j)) goto 1000
      if(idat.eq.'HCOR=') then
        if(kp(j).eq.-4.or.kp(j).eq.3.or.kp(j).eq.-3) call prror(83)
        if(kz(j).ne.1.and.kz(j).ne.11) call prror(82)
        kp(j)=4
      endif
      if(idat.eq.'VCOR=') then
        if(kp(j).eq.4.or.kp(j).eq.3.or.kp(j).eq.-3) call prror(83)
        if(kz(j).ne.-1.and.kz(j).ne.11) call prror(82)
        kp(j)=-4
      endif
 1000 continue
      goto 990
 1010 do 1020 j=1,il
      if(iele.ne.bez(j)) goto 1020
      if(idat.eq.'HMON=') then
        if(kp(j).eq.4.or.kp(j).eq.-4.or.kp(j).eq.-3) call prror(83)
        kp(j)=3
      endif
      if(idat.eq.'VMON=') then
        if(kp(j).eq.4.or.kp(j).eq.-4.or.kp(j).eq.3) call prror(83)
        kp(j)=-3
      endif
 1020 continue
      goto 990
!-----------------------------------------------------------------------
!  COMBINATION OF ELEMENTS
!-----------------------------------------------------------------------
 1030 ii=0
      do 1040 jj=1,ncom
      do 1040 ll=1,20
 1040 icel(jj,ll)=idum
      write(*,10130)
      write(*,10300)
 1050 ii=ii+1
      if(ii.gt.ncom) goto 1100
 1060 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1060
      if(ch(:4).eq.next) goto 110
      icoe=ii
      call intepr(5,1,ch,ch1)
      read(ch1,*) idat,(ratio(ii,l),icel(ii,l),l=1,20)
      do 1080 j=1,il
      if(idat.ne.bez(j)) goto 1070
      kp(j)=5
      icomb0(ii)=j
      ratioe(j)=one
 1070 do 1080 l=1,20
        if(bez(j).eq.icel(ii,l)) then
          icomb(ii,l)=j
          ratioe(j)=ratio(ii,l)
        endif
 1080 continue
      jj=icomb0(ii)
      if(jj.eq.0) goto 1050
      do 1090 m=1,20
        ico=icomb(ii,m)
        if(ico.eq.jj) call prror(92)
        if(ico.eq.0) goto 1090
        write(*,10310) bez(jj),bez(ico),ratio(ii,m)
        iratioe(ico)=jj
        if(el(jj).le.pieni) then
          if(el(ico).le.pieni) then
            ed(ico)=ed(jj)*ratio(ii,m)
          else
            ek(ico)=ed(jj)*ratio(ii,m)
          endif
        else
          if(el(ico).le.pieni) then
            ed(ico)=ek(jj)*ratio(ii,m)
          else
            ek(ico)=ek(jj)*ratio(ii,m)
          endif
        endif
 1090 continue
      goto 1050
 1100 write(*,10290) ncom
      goto 110
!-----------------------------------------------------------------------
!  SUBRESONANCE CALCULATION
!-----------------------------------------------------------------------
 1110 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1110
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) nta,nte,qxt,qzt,tam1,tam2,ipt,totl
      if(nta.lt.2) call prror(37)
      if(nte.lt.nta.or.nte.gt.9) call prror(37)
      isub=1
      goto 110
!-----------------------------------------------------------------------
!  RESONANCE-COMPENSATION
!-----------------------------------------------------------------------
 1120 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1120
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) nre
      if(nre.ne.0) read(ch1,*) nre,npp,nrr(1),nrr(2),nrr(3),            &
     &ipr(1),ipr(2),ipr(3)
      if(nre.ne.0.and.(npp.lt.2.or.npp.gt.nrco)) call prror(46)
      if(nre.lt.0.or.nre.gt.3) call prror(47)
      if(abs(nrr(1)).gt.npp.or.abs(nrr(2)).gt.npp                       &
     &.or.abs(nrr(3)).gt.npp) call prror(48)
 1130 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1130
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) nur
      if(nur.ne.0) read(ch1,*) nur,nu(1),nu(2),nu(3)
      if(nur.lt.0.or.nur.gt.3) call prror(49)
      if(nu(1).gt.9.or.nu(2).gt.9.or.nu(3).gt.9                         &
     &.or.nu(1).lt.0.or.nu(2).lt.0.or.nu(3).lt.0) call prror(50)
 1140 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1140
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) totl,qxt,qzt,tam1,tam2
 1150 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1150
      call intepr(3,1,ch,ch1)
      read(ch1,*) (ilm0(i),i=1,6)
 1160 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1160
      call intepr(6,1,ch,ch1)
      read(ch1,*) nch
      if(nch.ne.0) read(ch1,*) nch,ilm0(7),ilm0(8)
 1170 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1170
      call intepr(7,1,ch,ch1)
      read(ch1,*) nqc
      if(nqc.ne.0) read(ch1,*) nqc,ilm0(9),ilm0(10),qw0
      do 1190 k=1,10
      do 1180 j=1,il
        if(ilm0(k).ne.bez(j)) goto 1180
        ire(k)=j
        if(nre.eq.1.and.k.lt.3.and.abs(kz(j)).ne.npp) call prror(39)
        if(nre.eq.2.and.k.lt.5.and.abs(kz(j)).ne.npp) call prror(39)
        if(nre.eq.3.and.k.lt.7.and.abs(kz(j)).ne.npp) call prror(39)
        if(nch.eq.1.and.(k.eq.7.or.k.eq.8).and.kz(j).ne.3) call prror   &
     &(11)
        if(nqc.eq.1.and.(k.eq.9.or.k.eq.10).and.kz(j).ne.2) call prror  &
     &(8)
        goto 1190
 1180 continue
      if((nre.eq.1.and.k.lt.3).or.(nre.eq.2.and.k.lt.5).or.             &
     &(nre.eq.3.and.k.lt.7).or.(nch.eq.1.and.(k.eq.7.or.k.eq.8)).or.    &
     &(nqc.eq.1.and.(k.eq.9.or.k.eq.10))) call prror(3)
 1190 continue
      irmod2=1
      goto 110
!-----------------------------------------------------------------------
!  SEARCH FOR OPTIMUM PLACES TO COMPENSATE RESONANCES
!-----------------------------------------------------------------------
 1200 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1200
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) qxt,qzt,tam1,tam2,totl
 1210 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1210
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) mesa,mp,m21,m22,m23,ise1,ise2,ise3
      if(mp.lt.2.or.mp.gt.9) call prror(37)
      if(abs(m21).gt.mp.or.abs(m22).gt.mp                               &
     &.or.abs(m23).gt.mp) call prror(48)
      ise=1
      k0=0
 1220 do 1230 m=1,40
 1230 ilm0(m)=idum
 1240 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1240
      call intepr(3,1,ch,ch1)
      read(ch1,*) idat,(ilm0(m),m=2,40)
      if(idat.eq.next) goto 110
      ilm0(1)=idat
      ka=k0+1
      ke=k0+40
      do 1260 k=ka,ke
      if(k.gt.nele) call prror(2)
      if(k.gt.mesa) goto 110
      ki=k-k0
      if(ilm0(ki).eq.idum) goto 1270
      do 1250 j=1,il
        if(ilm0(ki).ne.bez(j)) goto 1250
        isea(k)=j
        if(abs(kz(j)).ne.mp) call prror(39)
        goto 1260
 1250 continue
      call prror(3)
 1260 continue
 1270 k0=k-1
      goto 1220
!-----------------------------------------------------------------------
!  POSTPROCESSING
!-----------------------------------------------------------------------
 1280 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).ne.'/') then
      iclr=iclr+1
      else
      goto 1280
      endif
      ch1(:83)=ch(:80)//' / '
      if(iclr.eq.1) toptit(1)=ch
      if(iclr.eq.2) read(ch1,*) iav,nstart,nstop,iwg,dphix,dphiz,       &
     &iskip,iconv,imad,cma1,cma2
      if(iclr.eq.3) read(ch1,*) qx0,qz0,ivox,ivoz,ires,dres,ifh,dfft
      if(iclr.eq.4) read(ch1,*) kwtype,itf,icr,idis,icow,istw,iffw,     &
     &nprint,ndafi
      kwtype=0
      icr=0
      if(iskip.le.0) iskip=1
      if(iclr.ne.4) goto 1280
      if(nprint.ne.1) nprint=0
      iclr=0
      if(nstart.lt.0) nstart=0
      if(nstop.lt.0) nstop=0
      if(nstop.lt.nstart) then
      nstart=0
      nstop=0
      endif
      if(iconv.ne.1) iconv=0
      if(abs(cma1).le.pieni) cma1=one
      cma1=cma1*c1e3
      if(abs(cma2).le.pieni) cma2=one
      ipos=1
      goto 110
!-----------------------------------------------------------------------
!  POWER SUPPLY RIPPLE
!-----------------------------------------------------------------------
 1290 irip=1
 1300 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1300
      if(ch(:4).eq.next) goto 110
      call intepr(1,1,ch,ch1)
      irco=irco+1
      if(irco.gt.nele) call prror(51)
      read(ch1,*) idat,ram,rfr,rph,nrturn
      do 1310 j=1,il
      if(idat.eq.bez(j)) then
        nrel(irco)=j
        ramp(j)=ram
        rfre(j)=rfr
        rzph(j)=rph
        goto 1300
      endif
 1310 continue
      goto 1300
!-----------------------------------------------------------------------
!  DECOUPLING ROUTINE
!-----------------------------------------------------------------------
 1320 iskew=1
 1330 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1330
      call intepr(3,1,ch,ch1)
      read(ch1,*) idat,(ilm0(m),m=2,4)
      if(idat.eq.next) then
      iskew=0
      goto 110
      endif
      ilm0(1)=idat
      do 1350 i=1,2
 1340 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1340
      if(ch(:4).eq.next) then
        iskew=2
        goto 1360
      endif
      call intepr(1,1,ch,ch1)
      read(ch1,*) ilm0(4+i),qwsk(i)
 1350 continue
 1360 continue
      do 1380 i=1,6
      do 1380 j=1,il
        if(iskew.eq.2.and.i.gt.4) goto 1380
        if(ilm0(i).eq.bez(j)) then
          if(i.le.4) then
            if(kz(j).ne.-2) call prror(62)
          else
            if(kz(j).ne.2) call prror(8)
          endif
          nskew(i)=j
          do 1370 i2=1,6
            if(nskew(i2).ne.0.and.(nskew(i2).eq.nskew(i)) .and.(i2.ne.i)&
     &) call prror(63)
 1370     continue
        endif
 1380 continue
      goto 110
!GRD-----------------------------------------------------------------------
!  COLLIMATION INPUT BLOCK
!GRD-----------------------------------------------------------------------
 1285 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).ne.'/') then
         iclr=iclr+1
      else
         goto 1285
      endif
      ch1(:83)=ch(:80)//' / '
!APRIL2005
      if(iclr.eq.1) read(ch1,*) do_coll
      if(iclr.eq.2) read(ch1,*) nloop,myenom
!JUNE2005
!      if(iclr.eq.3) read(ch1,*) mynex,mdex,myney,mdey
      if(iclr.eq.3) read(ch1,*) do_thisdis,mynex,mdex,myney,mdey,       &
     &filename_dis,enerror,bunchlength
!JUNE2005
!UPGRADE JANUARY 2005
!      if(iclr.eq.4) read(ch1,*) NSIG_PRIM,NSIG_SEC
!      if(iclr.eq.4) read(ch1,*) nsig_prim,nsig_sec,nsig_ter
      if(iclr.eq.4) read(ch1,*) do_nsig,                                &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,             &
     &nsig_tcli,                                                        &
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi
      if(iclr.eq.5) read(ch1,*)                                         &
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
     &nsig_tcxrp,nsig_tcryo, nsig_cry                  !valentina added nsig for crystal collimators
      if(iclr.eq.6) read(ch1,*) n_slices,smin_slices,smax_slices,       &
     &recenter1,recenter2
      if(iclr.eq.7) read(ch1,*)                                         &
     & fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1
      if(iclr.eq.8) read(ch1,*)                                         &
     & fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2
!
      if(iclr.eq.9) read(ch1,*) emitx0,emity0
      if(iclr.eq.10) then 
        read(ch1, "(A)") dummyline 
        read(dummyline, *, iostat=ierro) do_select,do_nominal,          &
     &rnd_seed,dowrite_dist,name_sel,do_oneside,                        &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude
     & ,write_elens_out, write_TM_QUAD_out 
        endif
      if(iclr.eq.11) read(ch1,*) xbeat,xbeatphase,ybeat,                &
     &ybeatphase
!AUGUST2006 ADDED offset variables for mechanical errors    ---- TW
!JANUAR2007 added rms error for gap and switch to min gap   ---- TW
      if(iclr.eq.12) read(ch1,*) c_rmstilt_prim,c_rmstilt_sec,          &
     &c_systilt_prim,c_systilt_sec,c_rmsoffset_prim,c_rmsoffset_sec,    &
     &c_sysoffset_prim,c_sysoffset_sec,c_offsettilt_seed,               &
     &c_rmserror_gap,do_mingap
      if(iclr.eq.13) read(ch1,*) radial,nr,ndr
      if(iclr.eq.14) read(ch1,*) driftsx, driftsy, cut_input,           &
     &systilt_antisymm, relative, diffusive
!AUGUST2006 ADDED rmsx, rmsy and distr. type for pencil beam ---- TW
      if(iclr.eq.15) read(ch1,*)                                        &
     $ipencil,pencil_offset,pencil_rmsx,pencil_rmsy,pencil_distr
!APRIL2005
      if(iclr.eq.16) read(ch1,*) coll_db,ibeam
!APRIL2005
      if(iclr.eq.17) read(ch1,*) dowritetracks, cern, castordir,        &
     &jobnumber, sigsecut2, sigsecut3
!
      if(iclr.ne.17) goto 1285
 1287 continue
      iclr=0
      goto 110
!-----------------------------------------------------------------------
!  COMMENT LINE
!-----------------------------------------------------------------------
 1390 read(3,10020,end=1530,iostat=ierro) commen
      if(ierro.gt.0) call prror(58)
      if(commen(1:1).eq.'/') goto 1390
      if(commen(:4).eq.next) then
      commen=' '
      endif
      goto 110
!-----------------------------------------------------------------------
!  NORMAL FORMS
!-----------------------------------------------------------------------
 1400 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1400
      if(ch(:4).eq.next) then
      goto 110
      else
      if(idial.eq.0.and.numl.ne.0) then
        write(*,10130)
        write(*,*)
        call prror(78)
      endif
      inorm=1
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) nordf,nvarf,nord1,idptr
      if(nord.ne.0.and.nordf.gt.nord+1) imod1=1
      if(nvar.ne.0.and.nvarf.gt.nvar) then
        nvarf=nvar
        imod2=1
      endif
      if(idptr.lt.0.or.idptr.gt.6) idptr=0
      endif
!-----------------------------------------------------------------------
!  TUNESHIFT CORRECTIONS
!-----------------------------------------------------------------------
 1410 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1410
      if(ch(:4).eq.next) goto 110
      icorr=1
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) nctype,ncor
      if(ncor.gt.mcor) call prror(65)
      if(ncor.gt.0) then
      read(3,10020,end=1530,iostat=ierro) ch
      ch1(:83)=ch(:80)//' / '
      call intepr(3,1,ch,ch1)
      read(ch1,*) (coel(i),i=1,ncor)
      do 1430 j1=1,ncor
        do 1420 j2=1,il
          if(coel(j1).eq.bez(j2)) then
            if(el(j2).ne.zero.or.kz(j2).gt.10) call prror(67)
            ipar(j1)=j2
            goto 1430
          endif
 1420   continue
        call prror(66)
 1430 continue
      else
      call prror(70)
      endif
      if(nctype.eq.0) then
      read(3,*) namp,nmom,dummy,dummy,dummy
      if(namp+nmom.eq.0) call prror(71)
      if(namp*nmom.ne.0) call prror(72)
      if(namp.lt.0.or.namp.gt.2) call prror(73)
      if(nmom.lt.0.or.nmom.eq.1.or.nmom.gt.3) call prror(74)
      if(namp.eq.1.or.nmom.eq.2) then
        nord=6
      else
        nord=7
      endif
      else
      read(3,*) nmom1,nmom2,weig1,weig2,dpmax
      if(nmom1.lt.2.or.nmom1.gt.3) call prror(75)
      if(nmom1.gt.nmom2) call prror(76)
      if(nmom2.lt.2.or.nmom2.gt.3) call prror(77)
      nord=2*(nmom2+1)
      endif
!-----------------------------------------------------------------------
      idial=1
      numlr=0
!     NUML=1
      napx=1
      imc=1
      preda=1.d-38
      nsix=1
      nvar=5
      nvar2=nvar
      nvar=nvar2+ncor
!-----------------------------------------------------------------------
      inorm=1
      nordf=nord+1
      nvarf=nvar
!-----------------------------------------------------------------------
      goto 1410
!-----------------------------------------------------------------------
!  Beam-Beam Element
!-----------------------------------------------------------------------
 1600 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1600
      if(ch(:4).eq.next) goto 110
      ch1(:83)=ch(:80)//' / '
      read(ch1,*) partnum,emitnx,emitny,sigz,sige,ibeco,ibtyp,lhc,ibbc
      if(emitnx.le.pieni.or.emitny.le.pieni) call prror(88)
      if(ibeco.ne.0.and.ibeco.ne.1) ibeco=1
      if(ibtyp.ne.0.and.ibtyp.ne.1) ibtyp=0
      if(lhc.ne.0.and.lhc.ne.1) lhc=1
      if(ibbc.ne.0.and.ibbc.ne.1) ibbc=0
      nbeam=1
      if(ibtyp.eq.1) call wzset
 1610 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1610
      if(ch(:4).eq.next) goto 110
      call intepr(1,1,ch,ch1)
      read(ch1,*) idat,i,xang,xplane
      if(i.lt.0) i=0
      do 1620 j=1,il
      if(idat.eq.bez(j).and.kz(j).eq.20) then
        ibb6d=1
        parbe(j,2)=i
        parbe(j,1)=xang
        parbe(j,3)=xplane
        goto 1610
      endif
 1620 continue
      goto 1610
!-----------------------------------------------------------------------
!  TROMBONE ELEMENT KZ=22
!-----------------------------------------------------------------------
 1700 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1700
      if(ch(:4).eq.next) goto 110
      call intepr(1,1,ch,ch1)
      read(ch1,*) imn
      imtr0=imtr0+1
      if(imtr0.gt.ntr) call prror(100)
      do 1710 j=1,il
        if(imn.eq.bez(j)) then
          imtr(j)=imtr0
          goto 1720
        endif
 1710 continue
      call prror(98)
 1720 j1=0
      if(imtr0.eq.1) write(*,10130)
      if(imtr0.eq.1) write(*,10700)
      write(*,10710) imtr0,imn
 1730 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call prror(58)
      if(ch(1:1).eq.'/') goto 1730
      if(ch(:4).eq.next) call prror(99)
      ch1(:83)=ch(:80)//' / '
      j1=j1+3
      read(ch1,*) cotr(imtr0,j1-2),cotr(imtr0,j1-1),cotr(imtr0,j1)
      if(j1.lt.6) goto 1730
      do j=1,6
        j1=0
 1740   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call prror(58)
        if(ch(1:1).eq.'/') goto 1740
        if(ch(:4).eq.next) call prror(99)
        ch1(:83)=ch(:80)//' / '
        j1=j1+3
        read(ch1,*) rrtr(imtr0,j,j1-2),rrtr(imtr0,j,j1-1),              &
     &rrtr(imtr0,j,j1)
        if(j1.lt.6) goto 1740
      enddo
      goto 1700
!-----------------------------------------------------------------------
  771 if(napx.ge.1) then
        if(e0.lt.pieni.or.e0.le.pma) call prror(27)
        if(nbeam.ge.1) parbe14=-crad*partnum/four/pi/emitnx*c1e6
        gammar=pma/e0
        crad=two*crad*partnum*gammar*c1e6
        emitx=emitnx*gammar
        emity=emitny*gammar
        remitxn=emitnx
        remityn=emitny
        remitx=emitx
        remity=emity
      endif
      if(idp.eq.0.or.ition.eq.0.or.nbeam.lt.1) then
        do j=1,il
          parbe(j,2)=0
        enddo
      else
        do j=1,il
          if(parbe(j,2).gt.mbea) then
            write(*,'(a48,i4,a29,i4)') '     WARNING: Number of ',      &
     &'slices set to maximum : ',mbea,' for 6D beam-beam element',      &
     &' #: ',j
            parbe(j,2)=mbea
          endif
        enddo
      endif
      do j=1,il
         if(kz(j).eq.15) then
            if(abs(xpl(j)).lt.pieni.and.abs(zpl(j)).lt.pieni) then
               kz(j)=0
               ed(j)=0
               ek(j)=0
               el(j)=0
            endif
         endif
      enddo
      if(iout.eq.0) return
      write(*,10050)
      write(*,10060)
      il1=il
      if(ncy2.eq.0) il1=il-1
      do 1435 k=1,il1
      if(abs(kz(k)).eq.12) then
        write(*,10070) k,bez(k),kz(k),ed(k),ek(k),phasc(k),xpl(k),      &
     &xrms(k),zpl(k),zrms(k)
        kz(k)=abs(kz(k))
        phasc(k)=phasc(k)*rad
      else
        write(*,10070) k,bez(k),kz(k),ed(k),ek(k),el(k),xpl(k),xrms(k), &
     &zpl(k),zrms(k)
      endif
 1435 continue
      write(*,10130)
      write(*,10080)
      write(*,10090) mper,(msym(k),k=1,mper)
      write(*,10250) mblo,mbloz
      write(*,10100)
      do 1450 l=1,mblo
      kk=mel(l)
      ll=kk/6
      if(ll.ne.0) then
        do 1440 l1=1,ll
          l2=(l1-1)*6+1
          l3=l2+5
          if(l2.eq.1) then
            write(*,10260) l,bezb(l),kk,(beze(l,k),k=1,6)
          else
            write(*,10270) (beze(l,k),k=l2,l3)
          endif
 1440   continue
        if(mod(kk,6).ne.0) then
          l4=ll*6+1
          write(*,10270) (beze(l,k),k=l4,kk)
        endif
      else
        write(*,10260) l,bezb(l),kk,(beze(l,k),k=1,kk)
      endif
 1450 continue
      write(*,10120)
      mblozz=mbloz/5+1
      do 1480 k=1,mblozz
      k10=(k-1)*5
      if((mbloz-k10).eq.0) goto 1480
      do 1470 l=1,5
        if((k10+l).gt.mbloz) ic0(l)=' '
        if((k10+l).gt.mbloz) goto 1470
        icc=ic(k10+l)
        if(icc.gt.nblo) goto 1460
        ic0(l)=bezb(icc)
        goto 1470
 1460   ic0(l)=bez0(icc-nblo)
 1470 continue
      k11=k10+1
      write(*,10280) k11,(ic0(l),l=1,5)
 1480 continue
      write(*,10130)
 1490 if(idp.eq.0) goto 1500
      if(nbeam.ge.1) then
        if(partnum.gt.zero) then
          write(*,10140) ncy,dp1,dppoff,tlen,pma,partnum,parbe14,ibeco, &
     &ibtyp,ibb6d,sigz,sige,emitnx,emitny,e0
        else
          write(*,10141) ncy,dp1,dppoff,tlen,pma,abs(partnum),parbe14,  &
     &ibeco,ibtyp,ibb6d,sigz,sige,emitnx,emitny,e0
        endif
      else
        write(*,10142) ncy,dp1,dppoff,tlen,pma,e0
      endif
      if(ncy2.eq.0) then
        write(*,10143) harm,u0,phag,qs,alc
      else
        write(*,*)
      endif
        if(ibb6d.eq.1) then
          write(*,10144)
          do j=1,il
            if(parbe(j,2).gt.0) write(*,10145) bez(j),                  &
     &int(parbe(j,2)),parbe(j,1),parbe(j,3)
          enddo
        endif
      write(*,10130)
 1500 continue
      write(*,10150)
      nfb=nde(1)
      nac=nde(2)
      nft=numl-nde(2)
      if(numl.le.nde(2)) nft=0
      if(numl.le.nde(2)) nac=numl
      if(numl.le.nde(1)) nac=0
      if(numl.le.nde(1)) nfb=numl
      write(*,10160) numl,numlr,nwr(4),nfb,nwr(1),nac,nwr(2),nft,nwr(3),&
     &kanf,amp(1),rat,itco,dma,dmap,itqv,dkq,dqq
      write(*,10170) itcro,dsm0,dech,de0,ded,dsi
      if(irip.eq.1) then
      write(*,10230)
      do 1510 i=1,irco
        j=nrel(i)
 1510 write(*,10240) bez(j),ramp(j),rfre(j),rzph(j),nrturn
      endif
      write(*,10130)
      write(*,10040)
      write(*,10130)
      goto 1540
 1520 call prror(41)
 1530 call prror(42)
 1540 continue
      if(2*mmac*imc*napx.gt.npart) call prror(54)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      return
10000 format(11(a4,1x))
10010 format(a4,8x,a60)
10020 format(a80)
10030 format(t10,22('O')/t10,2('O'),18x,2('O')/t10,                     &
     &'OO  SIXTRACK-INPUT  OO', /t10,2('O'),18x,2('O')/t10,22('O'))
10040 format(t10,21('O')/t10,2('O'),17x,2('O')/t10,                     &
     &'OO  PREPROCESSING  OO', /t10,2('O'),17x,2('O')/t10,21('O'))
10050 format(//131('-')//t43,'*** RING PARAMETERS ***'/)
10060 format(t30,'SINGLE ELEMENTS:'/'  NO   NAME  TYP      ',           &
     &' 1/RHO          STRENGTH          LENGTH           X-POS     ',  &
     &'     X-RMS            Y-PO          Y-RMS     ' /131('-'))
10080 format(/t30,'RINGSTRUCTURE:'//)
10090 format(t10,'NO. OF SUPERPERIODS AND SYMMETRY ' ,t50,i3,'   ',15i4,&
     &'   ')
10100 format(//131('-')//t30,'BLOCKSTRUCTURE:'/ t30,                    &
     &'(BLOCKTYP--NO. OF SINGLE ELEMENTS--SINGLE ELEMENT TYPES)'//)
10110 format(t10,i3,' ---',i3,' --- ',30i3)
10120 format(//131('-')//t30,'BLOCKSTRUCTURE OF SUPERPERIOD:'//)
10130 format(/131('-')/)
10140 format(t30,'SYNCHROTRON OSCILLATIONS AND BEAM-BEAM'//             &
     &t10,'NUMBER OF CAVITIES    ', t76,i4/                             &
     &t10,'MOMENTUM AMPLITUDE DP/P ',t66,f14.9/                         &
     &t10,'OFFSET MOMENTUM AMPLITUDE DP/P ',t66,f14.9/                  &
     &t10,'MACHINE LENGTH IN (M) ', t63,f17.9/                          &
     &t10,'PARTICLE MASS (MEV) ', t66,f14.9/                            &
     &t10,'PARTICLE NUMBER ',t66,1pe14.7/                               &
     &t10,'BEAMS HAVE SAME CHARGE'/                                     &
     &t10,'BEAM-BEAM PARAMETER ',t66,1pe14.7,0p/                        &
     &t10,'CLOSED ORBIT DUE TO BEAM-BEAM KICK (0=LEFT,1=SUBTRACTED) : ',&
     &t79,i1/                                                           &
     &t10,'FAST BEAM-BEAM KICK SWITCH (0=OFF,1=ON) : ',t79,i1/          &
     &t10,'Hirata 6D (1 => on/0 => off)  : ',t76,i4/                    &
     &t10,'BUNCH LENGTH               ',t66,f14.9/                      &
     &t10,'ENERGY SPREAD              ',t66,f14.9/                      &
     &t10,'NORMALIZED HORIZONTAL EMMITTANCE (mu-meter rad)',t71,f9.4/   &
     &t10,'NORMALIZED VERTICAL EMMITTANCE (mu-meter rad)',t71,f9.4/     &
     &t10,'ENERGY IN (MEV)',t66,f14.3)
10141 format(t30,'SYNCHROTRON OSCILLATIONS AND BEAM-BEAM'//             &
     &t10,'NUMBER OF CAVITIES    ', t76,i4/                             &
     &t10,'MOMENTUM AMPLITUDE DP/P ',t66,f14.9/                         &
     &t10,'OFFSET MOMENTUM AMPLITUDE DP/P ',t66,f14.9/                  &
     &t10,'MACHINE LENGTH IN (M) ', t63,f17.9/                          &
     &t10,'PARTICLE MASS (MEV) ', t66,f14.9/                            &
     &t10,'PARTICLE NUMBER ',t66,1pe14.7/                               &
     &t10,'BEAMS HAVE OPPOSITE CHARGE'/                                 &
     &t10,'BEAM-BEAM PARAMETER ',t66,1pe14.7,0p/                        &
     &t10,'CLOSED ORBIT DUE TO BEAM-BEAM KICK (0=LEFT,1=SUBTRACTED) : ',&
     &t79,i1/                                                           &
     &t10,'FAST BEAM-BEAM KICK SWITCH (0=OFF,1=ON) : ',t79,i1/          &
     &t10,'Hirata 6D (1 => on/0 => off)  : ',t76,i4/                    &
     &t10,'BUNCH LENGTH               ',t66,f14.9/                      &
     &t10,'ENERGY SPREAD              ',t66,f14.9/                      &
     &t10,'NORMALIZED HORIZONTAL EMMITTANCE (mu-meter rad)',t71,f9.4/   &
     &t10,'NORMALIZED VERTICAL EMMITTANCE (mu-meter rad)',t71,f9.4/     &
     &t10,'ENERGY IN (MEV)',t66,f14.3)
10142 format(t30,'SYNCHROTRON OSCILLATIONS'//                           &
     &t10,'NUMBER OF CAVITIES    ', t76,i4/                             &
     &t10,'MOMENTUM AMPLITUDE DP/P ',t66,f14.9/                         &
     &t10,'OFFSET MOMENTUM AMPLITUDE DP/P ',t66,f14.9/                  &
     &t10,'MACHINE LENGTH IN (M) ', t63,f17.9/                          &
     &t10,'PARTICLE MASS (MEV) ', t66,f14.9/                            &
     &t10,'ENERGY IN (MEV)',t66,f14.3)
10143 format(                                                           &
     &t10,'HARMONIC NUMBER',t74,f6.0/                                   &
     &t10,'CIRCUMF. VOLTAGE   (MV)',t66,f14.9/                          &
     &t10,'EQUILIBRIUM PHASE     (DEG)',t66,f14.9/                      &
     &t10,'FREQUENCY (IN UNITS OF REVOLUTION-FREQ.) QS-LINEAR',         &
     &t66 ,f14.9/                                                       &
     &t10,'MOMENTUM COMPACTION',t66,f14.9/)
10144 format(t30,'HIRATA''s 6D BEAM-BEAM ELEMENTS'/t30,30('-')//        &
     &t10,'ELEMENT           #_OF_SLICES    CROSSING_ANGLE',            &
     &'    CROSSING_PLANE    COUPLING_ANGLE'/t10,85('-')/)
10145 format(t10,a16,5x,i4,7x,d16.10,2x,d16.10)
10150 format(//t43,'*** TRACKING PARAMETERS ***'/)
10160 format(t10,'NUMBER OF REVOLUTIONS  ',t48,i8/ t10,                 &
     &'NUMBER OF REVERSE-REVOLUTIONS',t48,i8/ t10,                      &
     &'TURNS PER COOR.-PRINTOUT',t48,i8/ t10,'FLAT BOTTOM UP TO TURN ', &
     &t48,i8/ t10,'TURNS PER PRINT ON DATASET',t48,i8/ t10,             &
     &'ACCELERATION UP TO TURN',t48,i8/ t10,'TURNS PER PRINT ON DATASET'&
     &,t48,i8/ t10,'FLAT TOP NUMBER OF TURNS',t48,i8/ t10,              &
     &'TURNS PER PRINT ON DATASET',t48,i8/ t10,                         &
     &'TRACKING START AT ELEMENT NO.',t48,i8/ t10,                      &
     &'INITIAL AMPLITUDE-H IN (MM)',t49,f7.3/ t10,                      &
     &'COUPLING  EPS-Y/EPS-X',t49,f7.3/ t10,                            &
     &'NUMBER OF C.-O. ITERATIONS ',t48,i8/ t10,                        &
     &'PRECISION OF C.-O. DEVIATION',t47,d9.3/ t10,                     &
     &'PRECISION OF C.-O. SLOPE   ',t47,d9.3/ t10,                      &
     &'NUMBER OF Q-ADJ. ITERATIONS',t48,i8/ t10,                        &
     &'CHANGE IN K-STRENGTH BY',t47,d9.3/ t10,                          &
     &'PRECISION OF Q-ADJUSTEMENT',t47,d9.3)
10170 format(t10,'NUMBER OF CHROMAT.-ADJ. ITER.',t48,i8/ t10,           &
     &'CHANGE IN SEX.-STRENGTH BY',t47,d9.3/ t10,                       &
     &'PRECISION OF CHROMAT.-ADJ.',t47,d9.3/ t10,                       &
     &'DP-INTERVAL F. CROMAT.-ADJ.',t47,d9.3/ t10,                      &
     &'DP-INTERVAL FOR DISPERSION',t47,d9.3/ t10,                       &
     &'PRECISION FOR C.-O. RMS',t47,d9.3/)
10180 format(t5/t10,a60)
10190 format(t10,'PROGRAM MODE : FREE FORMAT INPUT')
10200 format(t10,'PROGRAM MODE : FREE FORMAT INPUT --READ FROM ',       &
     &'EXTRA GEOMETRY STRENGTH FILE--')
10220 format(t10,i4,2(' ',d15.8),5x,2(' ',d15.8))
10230 format(//131('-')//t10,'DATA BLOCK RIPPLE OF POWER SUPPLIES'//    &
     &t10,'ELEMENT',6x,'AMPLITUDE',9x,'FREQUENCY' ,9x,'STARTPHASE',9x,  &
     &'INI. TURNNUMBER'/t10,62('-')/)
10250 format(t10,'NUMBER OF DIFFERENT BLOCKS',t50,i3/ t10,              &
     &'BLOCKS PER PERIOD',t49,i5//)
10290 format(t10,'MORE THAN ',i5,' COMBINATIONS SPECIFIED'/)
10300 format(//131('-')//t10,'DATA BLOCK COMBINATION OF ELEMENTS',      &
     &'  THE FOLLOWING ELEMENTS ARE RELATED IN STRENGTHS--->'/ t10,     &
     &'ELEMENT RELATED TO ELEMENT BY THE RATIO'/)
10320 format(//131('-')//t10,'DATA BLOCK APERTURE LIMITATIONS'/ /t10,   &
     &'TYP',t20,'FORM',t30,'APERT-H',t40,'APERT-V')
10340 format(t10,'NO CAVITIES SPECIFIED'/)
10350 format(//131('-')//t10,'DATA BLOCK ORGANISATION OF RANDOM NUMBERS'&
     &/5x,'|          |      OWN RANDOM NUMBERS      |      SAME RAN' , &
     &'DOM NUMBERS      |   SAME MULTIPOLECOEFFICIENTS  |'/131('-'))
10370 format(t10,'DESIRED TUNE TO ADJUST IS ZERO'/ t10,                 &
     &'DATA BLOCK TUNE ADJUSTMENT  IGNORED')
10380 format(t10,'HIGHER MULTIPOLES THAN 20-POLES ARE NOT ALLOWED' ,    &
     &' AND THEREFORE IGNORED')
10410 format(//131('-')//t10,'DATA BLOCK FLUCTUATIONS OF MULTIPOLES'//  &
     &t10,'RANDOM STARTING NUMBER=  ',i20/ t10,                         &
     &'RANDOM NUMBERS GENERATED:',i20/ t10,'MEAN VALUE=',f15.7,         &
     &'  -   DEVIATION=',f15.7)
10420 format(t10,22('O')/t10,2('O'),18x,2('O')/t10,                     &
     &'OO   NORMAL FORMS   OO', /t10,2('O'),18x,2('O')/t10,22('O'))
10430 format(/5x,'No cut on random distribution'//)
10440 format(/5x,'Random distribution has been cut to: ',i4,' sigma.'//)
10070 format(1x,i3,1x,a16,1x,i3,1x,d16.10,1x,d16.10,1x,d16.10,1x,d13.7, &
     &1x,d12.6,1x,d13.7,1x,d12.6)
10210 format(t10,'DATA BLOCK MULTIPOLE COEFFICIENTS'/ t10,              &
     &'MULTIPOLE                    ',a16/t10,'RADIUS IN MM            '&
     &,f15.7/ t10,'BENDING STRENGTH IN MRAD',f15.7// t10,19x,'NORMAL',25&
     &x,'      SKEW '// t10,'      MEAN            RMS-VALUE     ',     &
     &'       MEAN            RMS-VALUE'/)
10240 format(t10,a16,3(2x,d16.10),2x,i10)
10260 format(t4,i3,1x,a16,1x,i2,1x,6(1x,a16))
10270 format(t28,6(1x,a16))
10280 format(t3,i6,1x,5(a16,1x))
10310 format(t10,a16,10x,a16,6x,f20.15)
10330 format(t8,a16,t18,a2,t30,f8.2,t40,f8.2)
10360 format(5x,'| ELEMENT  |           ',a16,'           |           ',&
     &'    |               |               |               |')
10390 format(5x,'| ELEMENTS |                              |    ',a16,  &
     &'   |    ',a16,'   |               |               |')
10400 format(5x,'| ELEMENTS |                              |          ' &
     &,'     |               |    ',a16,'   |    ',a16,'   |')
10700 format(t10,'DATA BLOCK TROMBONE ELEMENT'/                         &
     &t10,'TROMBONE #      NAME'/)
10710 format(t22,i4,5x,a16)
      end
      subroutine write4
!-----------------------------------------------------------------------
!     WRITE MODIFIED GEOMETRY FILE ON UNIT 4
!-----------------------------------------------------------------------
      implicit none
      integer ii,ikz
      double precision rdum1,rdum2,rel1
      character*80  ch
      character*160 ch1
      character*16 idat
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
      save
!-----------------------------------------------------------------------
      ii=0
      rewind 2
 1    read(2,'(A80)',end=90) ch
      if(ch(:1).eq.'/') then
        write(4,'(A80)') ch
        goto 1
      elseif(ch(:4).eq.'SING') then
        write(4,'(A80)') ch
      else
        return
      endif
 2    read(2,'(A80)',end=90) ch
      if(ch(:1).eq.'/') then
        write(4,'(A80)') ch
      else
        ii=ii+1
        if(ch(:4).ne.'NEXT') then
          call intepr(1,1,ch,ch1)
          read(ch1,*) idat,ikz,rdum1,rdum2,rel1
          if(ikz.eq.11) then
            write(4,10000) idat,ikz,rdum1,rdum2,rel1
          else
            if(abs(rel1).le.pieni) then
              if(ncororb(ii).eq.0) then
                write(4,10000) idat,ikz,sm(ii),rdum2,rel1
              else
                write(4,10000) idat,ikz,sm(ii),ek(ii),rel1
              endif
            else
              write(4,10000) idat,ikz,rdum1,ek(ii),rel1
            endif
          endif
        else
          write(4,'(A80)') ch
          goto 3
        endif
      endif
      goto 2
 3    read(2,'(A80)',end=90) ch
      write(4,'(A80)') ch
      goto 3
 90   continue
10000 format(a16,1x,i2,1x,d21.15,1x,d21.15,1x,d16.10)
      end
      subroutine intepr(i,j,ch,ch1)
!-----------------------------------------------------------------------
!     SUBROUTINE TO INTEPRET INPUT WITH CHARACTERS AND NUMBERS MIXED
!
!     I ... TYPE OF COMBINATION
!
!         1  LINE WITH 1 CHARACTERSTRING FOLLOWED BY NUMBERS
!         2  LINE WITH CHARACTERSTRINGS, IF THE FIRST 5 CHARACTERS
!            ARE BLANKS THIS IS INTERPRETED AS A BLANK CHARACTER
!         3  LINE WITH CHARACTERSTRINGS
!         4  LINE WITH 2 CHARACTERSTRINGS
!         5  LINE WITH 1 CHARACTERSTRING AND N*(NUMBER,CHA.STRING)
!         6  LINE WITH 1 NUMBER AND 2 CHARACTERSTRINGS
!         7  LINE WITH 1 NUMBER, 2 CHARACTERSTRINGS AND NUMBERS
!         8  LINE WITH 2 CHARACTERSTRINGS AND NUMBERS
!
!     J ... SKIP THE FIRST (J-1) CHARACTERS OF CHARACTERSTRING CH
!    CH ... INPUT CHARACTERSTRING
!   CH1 ... OUTPUT CHARACTERSTRING
!-----------------------------------------------------------------------
      implicit none
      integer i,i0,i1,i2,i3,i4,iev,ii,j
      character*80 ch
      character*160 ch1
      save
!-----------------------------------------------------------------------
      i0=0
      i1=j
      i2=1
      i4=0
      do 10 ii=j,80
        if(i0.eq.0.and.ch(ii:ii).eq.' ') then
          if(i.eq.2.and.ii.eq.5.and.ch(:5).eq.'     ') then
            ch1(:4)=''' '' '
            i2=5
          endif
          i1=ii+1
          goto 10
        endif
        i0=1
        if(ch(ii:ii).eq.' ') then
          i4=i4+1
          iev=1
          if(mod(i4,2).eq.0) iev=0
          if(i.eq.1) goto 20
          if(i.eq.2.or.i.eq.3.or.i.eq.4.or. (i.eq.5.and.iev.eq.1.).or.  &
     &(i.eq.6.and.i4.ge.2).or. (i.eq.7.and.(i4.eq.2.or.i4.eq.3)).or.    &
     &(i.eq.8.and.i4.lt.3)) then
            i3=i2+ii-i1+2
            ch1(i2:i3)=''''//ch(i1:ii-1)//''' '
            if(i.eq.4.and.i4.eq.2) goto 30
            i2=i3+1
          endif
          if((i.eq.5.and.iev.eq.0).or. (i4.eq.1.and.(i.eq.6.or.i.eq.7)))&
     &then
            i3=i2+ii-i1
            ch1(i2:i3)=ch(i1:ii)
            i2=i3+1
          endif
          if((i.eq.7.and.i4.gt.3).or.(i.eq.8.and.i4.eq.3)) goto 40
          i0=0
          i1=ii+1
        endif
   10 continue
      goto 30
   20 ch1(1:85)=''''//ch(i1:ii-1)//''''//ch(ii:80)//' / '
      return
   30 i3=i3+1
      ch1(i3:i3+2)=' / '
      return
   40 i3=i2+83-i1
      ch1(i2:i3)=ch(i1:80)//' / '
      return
      end
      subroutine wzset
!  *********************************************************************
!
!  This subroutine must be called before subroutine WZSUB can be used to
!  compute values of the complex error function w(z).
!
!  Parameters xcut and ycut specify the opposite corners (xcut,0) and
!  (0,ycut) of the rectangle inside which interpolation is to be used
!  by subroutine WZSUB.
!
!  Parameter h is the side of the squares of the interpolation grid.
!
!  Parameters nx and ny must be set to the nearest integers to xcut/h
!  and ycut/h respectively (or to larger values).
!
!  Calls MYWWERF new version of (CERN library) WWERF (C335)
!
!  (G.A.Erskine, 29.09.1995)
!
!  *********************************************************************
      implicit none
      integer i,j,k
      double precision wi,wr,x,y
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
      integer idim,kstep,nx,ny
      double precision h,half,hrecip,one,wtimag,wtreal,xcut,ycut
      parameter ( xcut = 7.77d0, ycut = 7.46d0 )
      parameter ( h = 1.d0/63.d0 )
      parameter ( nx = 490, ny = 470 )
      parameter ( idim = (nx+2)*(ny+2) )
      parameter ( half = 0.5d0, one = 1.d0 )
      common /wzcom1/ hrecip, kstep
      common /wzcom2/ wtreal(idim), wtimag(idim)
      save
!-----------------------------------------------------------------------
      hrecip = 1.d0/h
      kstep = nx+2
      k = 0
      do 2 j=0,ny+1
         do 1 i=0,nx+1
            k = k+1
            x=i*h
            y=j*h
            call mywwerf(x,y,wr,wi)
            wtreal(k)=wr
            wtimag(k)=wi
 1       continue
 2    continue
      end
      subroutine mywwerf(x,y,wr,wi)
      implicit none
      integer n
      double precision c,c1,c2,c3,c4,hf,p,rr,ri,sr0,sr,si,tr,ti,vi,vr,  &
     &wi,wr,x,xa,xl,y,ya,zhi,zhr,z1,z10
      parameter (z1=1,hf=z1/2d0,z10=10d0)
      parameter (c1=74d0/z10,c2=83d0/z10,c3=z10/32d0,c4=16d0/z10)
      parameter (c=1.12837916709551257d0,p=(2d0*c4)**33)
      dimension rr(37),ri(37)
      save
!-----------------------------------------------------------------------
      xa=abs(x)
      ya=abs(y)
      if(ya.lt.c1.and.xa.lt.c2) then
!        zh=dcmplx(ya+c4,xa)
        zhr=ya+c4
        zhi=xa
        rr(37)=0d0
        ri(37)=0d0
        do n=36,1,-1
!          t=zh+n*dconjg(r(n+1))
          tr=zhr+n*rr(n+1)
          ti=zhi-n*ri(n+1)
!          r(n)=hf*t/(dreal(t)**2+dimag(t)**2)
          rr(n)=hf*tr/(tr**2+ti**2)
          ri(n)=hf*ti/(tr**2+ti**2)
        enddo
        xl=p
        sr=0d0
        si=0d0
        do n=33,1,-1
          xl=c3*xl
!          s=r(n)*(s+xl)
          sr0=rr(n)*(sr+xl)-ri(n)*si
          si=rr(n)*si+ri(n)*(sr+xl)
          sr=sr0
        enddo
!        v=c*s
        vr=c*sr
        vi=c*si
      else
        zhr=ya
        zhi=xa
        rr(1)=0d0
        ri(1)=0d0
        do n=9,1,-1
!          t=zh+n*dconjg(r(1))
          tr=zhr+n*rr(1)
          ti=zhi-n*ri(1)
!          r(1)=hf*t/(dreal(t)**2+dimag(t)**2)
          rr(1)=hf*tr/(tr**2+ti**2)
          ri(1)=hf*ti/(tr**2+ti**2)
        enddo
!        v=c*r(1)
        vr=c*rr(1)
        vi=c*ri(1)
      endif
      if(ya.eq.0) then
!        v=dcmplx(exp(-xa**2),dimag(v))
        vr=exp(-xa**2)
      endif
      if(y.lt.0) then
!        v=2*exp(-dcmplx(xa,ya)**2)-v
        vr=2d0*exp(ya**2-xa**2)*cos(2d0*xa*ya)-vr
        vi=-2d0*exp(ya**2-xa**2)*sin(2d0*xa*ya)-vi
        if(x.gt.0) vi=-vi
      else
        if(x.lt.0) vi=-vi
      endif
      wr=vr
      wi=vi
      return
      end
      subroutine ranecu(rvec,len,mcut)
      implicit none
      integer i,is1,is2,iseed1,iseed2,iz,j,k,len,mcut
      double precision rvec0,rvec,pi,r
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
      dimension rvec(*),r(2)
      data iseed1,iseed2 / 12345, 67890 /
      save
!-----------------------------------------------------------------------
      pi = four*atan(one)
!     DO 100 I = 1,LEN
      i=1
   10 do 20 j = 1,2
        k = iseed1/53668
        iseed1 = 40014*(iseed1-k*53668) - k*12211
        if (iseed1.lt.0) iseed1 = iseed1+2147483563
        k = iseed2/52774
        iseed2 = 40692*(iseed2-k*52774) - k*3791
        if (iseed2.lt.0) iseed2 = iseed2+2147483399
        iz = iseed1-iseed2
        if (iz.lt.1) iz = iz+2147483562
        r(j) = real(iz)*4.656613e-10
   20 continue
      rvec0 = ((-two*log(r(1)))**half)*cos(two*pi*r(2))
      if(abs(rvec0).le.mcut.or.mcut.eq.0) then
        rvec(i) = rvec0
        i=i+1
      endif
      if(i.le.len) goto 10
!     RVEC(I) = ((-TWO*LOG(R(1)))**HALF)*COS(TWO*PI*R(2))
! 100 CONTINUE
      return
 
      entry recuin(is1,is2)
      iseed1 = is1
      iseed2 = is2
      return
 
      entry recuut(is1,is2)
      is1 = iseed1
      is2 = iseed2
      return
      end
      subroutine envars(j,dpp,rv)
!-----------------------------------------------------------------------
!  CALCULATION OF : MOMENTUM-DEPENDING ELEMENT-MATRICES AND
!                   CHANGE OF PATH LENGTHS FOR EACH PARTICLE.
!-----------------------------------------------------------------------
      implicit none
      integer i,ih,j,kz1,l,ll
      double precision aek,afok,as3,as4,as6,co,dpd,dpp,dpsq,fi,fok,fok1,&
     &fokq,g,gl,hc,hi,hi1,hm,hp,hs,rho,rhoc,rhoi,rv,si,siq,sm1,         &
     &sm12,sm2,sm23,sm3,sm5,sm6,wf,wfa,wfhi
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
      save
!-----------------------------------------------------------------------
      dpd=one+dpp
      dpsq=sqrt(dpd)
      do 190 i=1,il
        do ll=1,6
          do l=1,2
            al(ll,l,j,i)=zero
            as(ll,l,j,i)=zero
          enddo
        enddo
        if(abs(el(i)).le.pieni) goto 190
        kz1=kz(i)+1
        goto(10,30,90,50,70,80,120,170,180),kz1
        goto 190
!-----------------------------------------------------------------------
!  DRIFTLENGTH
!-----------------------------------------------------------------------
   10   do 20 l=1,2
          al(1,l,j,i)=one
          al(2,l,j,i)=el(i)
          al(3,l,j,i)=zero
          al(4,l,j,i)=one
   20   as(6,l,j,i)=-rv*al(2,l,j,i)/c2e3
        as(1,1,j,i)=el(i)*(one-rv)*c1e3
        goto 190
!-----------------------------------------------------------------------
!  RECTANGULAR MAGNET
!  HORIZONTAL
!-----------------------------------------------------------------------
   30   ih=1
   40   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        fok1=(tan(fok*half))/rho
        si=sin(fok)
        co=cos(fok)
        al(1,ih,j,i)=one
        al(2,ih,j,i)=rho*si
        al(3,ih,j,i)=zero
        al(4,ih,j,i)=one
        al(5,ih,j,i)=-dpp*(rho*(one-co)/dpsq)*c1e3
        al(6,ih,j,i)=-dpp*(two*tan(fok*half)/dpsq)*c1e3
        sm1=cos(fok)
        sm2=sin(fok)*rho
        sm3=-sin(fok)/rho
        sm5=-rho*dpsq*(one-sm1)
        sm6=-sm2*dpsq/rho
        sm12=el(i)-sm1*sm2
        sm23=sm2*sm3
        as3=-rv*(dpp*rho/(two*dpsq)*sm23+sm5)
        as4=-rv*sm23/c2e3
        as6=-rv*(el(i)+sm1*sm2)/c4e3
        as(1,ih,j,i)=(-rv*(dpp*dpp/(four*dpd)*sm12+dpp*(el(i)-sm2))+    &
     &el(i)*(one-rv))*c1e3
        as(2,ih,j,i)=-rv*(dpp/(two*rho*dpsq)*sm12+sm6)+fok1*as3
        as(3,ih,j,i)=as3
        as(4,ih,j,i)=as4+two*as6*fok1
        as(5,ih,j,i)=-rv*sm12/(c4e3*rho*rho)+as6*fok1*fok1+fok1*as4
        as(6,ih,j,i)=as6
!--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        g=tan(fok*half)/rho
        gl=el(i)*g
        al(1,ih,j,i)=one-gl
        al(2,ih,j,i)=el(i)
        al(3,ih,j,i)=-g*(two-gl)
        al(4,ih,j,i)=al(1,ih,j,i)
        as6=-rv*al(2,ih,j,i)/c2e3
        as(4,ih,j,i)=-two*as6*fok1
        as(5,ih,j,i)=as6*fok1*fok1
        as(6,ih,j,i)=as6
        goto 190
!-----------------------------------------------------------------------
!  SEKTORMAGNET
!  HORIZONTAL
!-----------------------------------------------------------------------
   50   ih=1
   60   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        si=sin(fok)
        co=cos(fok)
        rhoc=rho*(one-co)/dpsq
        siq=si/dpsq
        al(1,ih,j,i)=co
        al(2,ih,j,i)=rho*si
        al(3,ih,j,i)=-si/rho
        al(4,ih,j,i)=co
        al(5,ih,j,i)=-dpp*rhoc*c1e3
        al(6,ih,j,i)=-dpp*siq*c1e3
        sm12=el(i)-al(1,ih,j,i)*al(2,ih,j,i)
        sm23=al(2,ih,j,i)*al(3,ih,j,i)
        as(1,ih,j,i)=(-rv*(dpp*dpp/(four*dpd)*sm12 +dpp*(el(i)-         &
     &al(2,ih,j,i)))+el(i)*(one-rv))*c1e3
        as(2,ih,j,i)=-rv*(dpp/(two*rho*dpsq)*sm12-dpd*siq)
        as(3,ih,j,i)=-rv*(dpp*rho/(two*dpsq)*sm23-dpd*rhoc)
        as(4,ih,j,i)=-rv*sm23/c2e3
        as(5,ih,j,i)=-rv*sm12/(c4e3*rho*rho)
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
!--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        al(1,ih,j,i)=one
        al(2,ih,j,i)=el(i)
        al(3,ih,j,i)=zero
        al(4,ih,j,i)=one
        as(6,ih,j,i)=-rv*al(2,ih,j,i)/c2e3
        goto 190
!-----------------------------------------------------------------------
!  RECTANGULAR MAGNET VERTIKAL
!-----------------------------------------------------------------------
   70   ih=2
        goto 40
!-----------------------------------------------------------------------
!  SEKTORMAGNET VERTIKAL
!-----------------------------------------------------------------------
   80   ih=2
        goto 60
!-----------------------------------------------------------------------
!  QUADRUPOLE
!  FOCUSSING
!-----------------------------------------------------------------------
   90   fok=ek(i)/(one+dpp)
        aek=abs(fok)
        if(abs(fok).le.pieni) goto 10
        ih=0
        hi=sqrt(aek)
        fi=el(i)*hi
        if(fok.gt.zero) goto 110
  100   ih=ih+1
        al(1,ih,j,i)=cos(fi)
        hi1=sin(fi)
        al(2,ih,j,i)=hi1/hi
        al(3,ih,j,i)=-hi1*hi
        al(4,ih,j,i)=al(1,ih,j,i)
        as(1,ih,j,i)=el(i)*(one-rv)*c1e3
        as(4,ih,j,i)=-rv*al(2,ih,j,i)*al(3,ih,j,i)/c2e3
        as(5,ih,j,i)=-rv*(el(i)-al(1,ih,j,i)*al(2,ih,j,i))*aek/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        if(ih.eq.2) goto 190
!--DEFOCUSSING
  110   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        al(1,ih,j,i)=hc
        al(2,ih,j,i)=hs/hi
        al(3,ih,j,i)=hs*hi
        al(4,ih,j,i)=hc
        as(4,ih,j,i)=-rv*al(2,ih,j,i)*al(3,ih,j,i)/c2e3
        as(5,ih,j,i)=+rv*(el(i)-al(1,ih,j,i)*al(2,ih,j,i))*aek/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        if(ih.eq.1) goto 100
        goto 190
!-----------------------------------------------------------------------
!  COMBINED FUNCTION MAGNET HORIZONTAL
!  FOCUSSING
!-----------------------------------------------------------------------
  120   ih=0
        fokq=ek(i)
  130   wf=ed(i)/dpsq
        fok=fokq/(dpd)-wf*wf
        if(abs(fok).le.pieni) goto 10
        afok=abs(fok)
        hi=sqrt(afok)
        fi=hi*el(i)
        if(fok.gt.zero) goto 160
  140   ih=ih+1
        si=sin(fi)
        co=cos(fi)
        wfa=wf/afok*(one-co)/dpsq
        wfhi=wf/hi*si/dpsq
        al(1,ih,j,i)=co
        al(2,ih,j,i)=si/hi
        al(3,ih,j,i)=-si*hi
        al(4,ih,j,i)=co
        al(5,ih,j,i)=-wfa*dpp*c1e3
        al(6,ih,j,i)=-wfhi*dpp*c1e3
        sm12=el(i)-al(1,ih,j,i)*al(2,ih,j,i)
        sm23=al(2,ih,j,i)*al(3,ih,j,i)
        as(1,ih,j,i)=(-rv*(dpp*dpp/(four*dpd)*sm12+dpp*(el(i)-          &
     &al(2,ih,j,i)))/afok*wf*wf+el(i)*(one-rv))*c1e3
        as(2,ih,j,i)=-rv*(dpp*wf/(two*dpsq)*sm12-dpd*wfhi)
        as(3,ih,j,i)=-rv*(dpp*half/afok/dpd*ed(i)*sm23-dpd*wfa)
        as(4,ih,j,i)=-rv*sm23/c2e3
        as(5,ih,j,i)=-rv*sm12*afok/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        ih=ih+1
        if(ih.gt.2) ih=1
        aek=abs(ek(i)/dpd)
        hi=sqrt(aek)
        fi=hi*el(i)
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        al(1,ih,j,i)=hc
        al(2,ih,j,i)=el(i)
        if(abs(hi).le.pieni) goto 150
        al(2,ih,j,i)=hs/hi
  150   al(3,ih,j,i)=hs*hi
        al(4,ih,j,i)=hc
        as(4,ih,j,i)=-rv*al(2,ih,j,i)*al(3,ih,j,i)/c2e3
        as(5,ih,j,i)=+rv*(el(i)-al(1,ih,j,i)*al(2,ih,j,i))*aek/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        goto 190
!--DEFOCUSSING
  160   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        al(1,ih,j,i)=hc
        al(2,ih,j,i)=hs/hi
        al(3,ih,j,i)=hs*hi
        al(4,ih,j,i)=hc
        wfa=wf/afok*(one-hc)/dpsq
        wfhi=wf/hi*hs/dpsq
        al(5,ih,j,i)= wfa*dpp*c1e3
        al(6,ih,j,i)=-wfhi*dpp*c1e3
        sm12=el(i)-al(1,ih,j,i)*al(2,ih,j,i)
        sm23=al(2,ih,j,i)*al(3,ih,j,i)
        as(1,ih,j,i)=(rv*(dpp*dpp/(four*dpd)*sm12+dpp*(el(i)-           &
     &al(2,ih,j,i)))/afok*wf*wf+el(i)*(one-rv))*c1e3
        as(2,ih,j,i)=-rv*(dpp*wf/(two*dpsq)*sm12-dpd*wfhi)
        as(3,ih,j,i)=rv*(dpp*half/afok/dpd*ed(i)*sm23-dpd*wfa)
        as(4,ih,j,i)=-rv*sm23/c2e3
        as(5,ih,j,i)=+rv*sm12*afok/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        ih=ih+1
        if(ih.gt.2) ih=1
        aek=abs(ek(i)/dpd)
        hi=sqrt(aek)
        fi=hi*el(i)
        si=sin(fi)
        co=cos(fi)
        al(1,ih,j,i)=co
        al(2,ih,j,i)=si/hi
        al(3,ih,j,i)=-si*hi
        al(4,ih,j,i)=co
        as(4,ih,j,i)=-rv*al(2,ih,j,i)*al(3,ih,j,i)/c2e3
        as(5,ih,j,i)=-rv*(el(i)-al(1,ih,j,i)*al(2,ih,j,i))*aek/c4e3
        as(6,ih,j,i)=-rv*(el(i)+al(1,ih,j,i)*al(2,ih,j,i))/c4e3
        goto 190
!-----------------------------------------------------------------------
!  COMBINED FUNCTION MAGNET VERTICAL
!-----------------------------------------------------------------------
  170   ih=1
        fokq=-ek(i)
        goto 130
!-----------------------------------------------------------------------
!  EDGE FOCUSSING
!-----------------------------------------------------------------------
  180   rhoi=ed(i)/dpsq
        fok=rhoi*tan(el(i)*rhoi*half)
        al(1,1,j,i)=one
        al(2,1,j,i)=zero
        al(3,1,j,i)=fok
        al(4,1,j,i)=one
        al(1,2,j,i)=one
        al(2,2,j,i)=zero
        al(3,2,j,i)=-fok
        al(4,2,j,i)=one
        goto 190
!-----------------------------------------------------------------------
!   NONLINEAR INSERTION
!-----------------------------------------------------------------------
  190 continue
      return
      end
      program maincr
      implicit none
!-----------------------------------------------------------------------
!
!  SIXTRACK
!
!  SIXDIMENSIONAL PARTICLE-TRACKING
!
!-----------------------------------------------------------------------
!
!  F. SCHMIDT, M. VANTTINEN
!
!  COLLIMATION VERSION, NOVEMBER 2004
!
!  G. ROBERT-DEMOLAIZE
!
!  COLLIMATION UPGRADE, JUNE 2005
!
!  G. ROBERT-DEMOLAIZE, S. REDAELLI
!
!-----------------------------------------------------------------------
!  SIXTRACR CHECKPOINT/RESTART and CRLIBM (ENS Lyon)
!
!  E. MCINTOSH FEBRUARY 2005
!-----------------------------------------------------------------------
!  USED DISKS:
!
!  GEOMETRY AND STRENGTH OF THE ACCELERATOR : UNIT  2
!  TRACKING PARAMETER                       : UNIT  3
!  NORMAL PRINTOUT                          : UNIT  6
!  TRACKING DATA                            : UNIT  8
!  DATA FOR SUMMARY OF THE POSTPROCESSING   : UNIT 10
!  AUXILIARY FILE FOR THE INPUT             : UNIT 11
!  ASCII FILE WITH THE HORIZONTAL FFT DATA  : UNIT 14
!  ASCII FILE WITH THE VERTICAL FFT DATA    : UNIT 15
!  METAFILE FOR PLOTTING WITH GKS           : UNIT 20
!
!-----------------------------------------------------------------------
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
      double precision aml6,edcor
      common/sixdim/aml6(6,6),edcor(2)
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
      integer ichromc,ilinc,iqmodc
      double precision clon,chromc,corr,wxys
      common/correct/ corr(3,3),chromc(2),wxys(3),clon(6),iqmodc,       &
     &ichromc,ilinc
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
!     parameter (max_ncoll=75,max_npart=20000,nc=32,numeff=19,          &
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,         &
     &maxn=20000,outlun=54)
      double precision myemitx0,myemity0,myalphax,myalphay,mybetax,     &
     &mybetay,rselect
      common /ralph/ myemitx0,myemity0,myalphax,myalphay,mybetax,       &
     &mybetay,rselect
!
      logical cut_input
      common /cut/ cut_input
!
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
!GRD
!GRD THIS BLOC IS COMMON TO MAINCR, DATEN, TRAUTHIN AND THIN6D
!GRD
!APRIL2005
      logical do_coll,do_select,do_nominal,dowrite_dist,do_oneside,     &
     &dowrite_impact,dowrite_secondary,dowrite_amplitude,radial,        &
     &systilt_antisymm,dowritetracks,cern,do_nsig,do_mingap
     & ,relative, diffusive
!     &systilt_antisymm,dowritetracks,cern
!APRIL2005
!
!      integer nloop,rnd_seed,ibeam,jobnumber,sigsecut2
!JUNE2005
!      integer nloop,rnd_seed,ibeam,jobnumber
!SEPT2005 for slicing process
!      integer nloop,rnd_seed,ibeam,jobnumber,do_thisdis
      integer nloop,rnd_seed,c_offsettilt_seed,                         &
     &ibeam,jobnumber,do_thisdis,n_slices,pencil_distr
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
!SEPT2005
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
      common /grd/ myenom,mynex,mdex,myney,mdey,                        &
     &nsig_tcp3,nsig_tcsg3,nsig_tcsm3,nsig_tcla3,                       &
     &nsig_tcp7,nsig_tcsg7,nsig_tcsm7,nsig_tcla7,nsig_tclp,nsig_tcli,   &
     &nsig_tcth1,nsig_tcth2,nsig_tcth5,nsig_tcth8,                      &
     &nsig_tctv1,nsig_tctv2,nsig_tctv5,nsig_tctv8,                      &
     &nsig_tcdq,nsig_tcstcdq,nsig_tdi,nsig_tcxrp,nsig_tcryo, nsig_cry,  &
     &smin_slices,smax_slices,recenter1,recenter2,                      &
     &fit1_1,fit1_2,fit1_3,fit1_4,fit1_5,fit1_6,ssf1,                   &
     &fit2_1,fit2_2,fit2_3,fit2_4,fit2_5,fit2_6,ssf2,                   &
     &emitx0,emity0,xbeat,xbeatphase,ybeat,ybeatphase,                  &
     &c_rmstilt_prim,c_rmstilt_sec,c_systilt_prim,c_systilt_sec,        &
     &c_rmsoffset_prim,c_rmsoffset_sec,c_sysoffset_prim,                &
     &c_sysoffset_sec,c_rmserror_gap,nr,                                &
     &ndr,driftsx,driftsy,pencil_offset,pencil_rmsx,pencil_rmsy,        &
     & relative, diffusive,
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
      integer i,itiono,i1,i2,i3,ia,ia2,iar,iation,ib,ib0,ib1,ib2,ib3,id,&
     &idate,ie,ig,ii,ikk,im,imonth,iposc,irecuin,itime,ix,izu,j,j2,jj,  &
     &jm,k,kpz,kzz,l,lkk,ll,m,mkk,napxto,ncorruo,ncrr,nd,nd2,ndafi2,    &
     &nerror,nlino,nlinoo,nmz,nthinerr
      real time,time2
      double precision alf0s1,alf0s2,alf0s3,alf0x2,alf0x3,alf0z2,alf0z3,&
     &amp00,bet0s1,bet0s2,bet0s3,bet0x2,bet0x3,bet0z2,bet0z3,chi,coc,   &
     &dam1,dchi,ddp1,dp0,dp00,dp10,dpoff,dpsic,dps0,dsign,gam0s1,gam0s2,&
     &gam0s3,gam0x1,gam0x2,gam0x3,gam0z1,gam0z2,gam0z3,phag,r0,r0a,rat0,&
     &rdev,rmean,rsqsum,rsum,sic,tasia56,tasiar16,tasiar26,tasiar36,    &
     &tasiar46,tasiar56,tasiar61,tasiar62,tasiar63,tasiar64,tasiar65,   &
     &taus,x11,x13
      integer idummy(6)
      character*10 cmonth
      character*4 cpto
      character*80 day,runtim
      character*8 cdate,ctime,progrm
      dimension cmonth(12)
      data (cmonth(i),i=1,12)/' January ',' February ','  March   ',    &
     &'  April   ','   May    ','   June   ','   July   ',' August  ',  &
     &' September',' October  ',' November ',' December '/
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!--OPENING DATA FILES
      open(2,file='fort.2',form='formatted',status='unknown')
      open(3,file='fort.3',form='formatted',status='unknown')
      open(4,file='fort.4',form='formatted',status='unknown')
      open(7,file='fort.7',form='formatted',status='unknown')
      open(8,file='fort.8',form='formatted',status='unknown')
      open(9,file='fort.9',form='formatted',status='unknown')
      open(10,file='fort.10',form='formatted',status='unknown')
      open(11,file='fort.11',form='unformatted',status='unknown')
      open(12,file='fort.12',form='formatted',status='unknown')
      open(13,file='fort.13',form='formatted',status='unknown')
      open(14,file='fort.14',form='formatted',status='unknown')
      open(15,file='fort.15',form='formatted',status='unknown')
      open(16,file='fort.16',form='formatted',status='unknown')
      open(17,file='fort.17',form='formatted',status='unknown')
      open(18,file='fort.18',form='formatted',status='unknown')
      open(19,file='fort.19',form='formatted',status='unknown')
      open(20,file='fort.20',form='formatted',status='unknown')
      open(21,file='fort.21',form='formatted',status='unknown')
      open(22,file='fort.22',form='formatted',status='unknown')
      open(23,file='fort.23',form='formatted',status='unknown')
      open(24,file='fort.24',form='formatted',status='unknown')
      open(25,file='fort.25',form='formatted',status='unknown')
      open(26,file='fort.26',form='formatted',status='unknown')
      open(27,file='fort.27',form='formatted',status='unknown')
      open(28,file='fort.28',form='formatted',status='unknown')
      open(29,file='fort.29',form='formatted',status='unknown')
      open(30,file='fort.30',form='formatted',status='unknown')
      open(31,file='fort.31',form='formatted',status='unknown')
      open(32,file='fort.32',form='unformatted',status='unknown')
      open(33,file='fort.33',form='formatted',status='unknown')
      open(34,file='fort.34',form='formatted',status='unknown')
      open(59,file='fort.59',form='unformatted',status='unknown')
      open(60,file='fort.60',form='unformatted',status='unknown')
      open(61,file='fort.61',form='unformatted',status='unknown')
      open(62,file='fort.62',form='unformatted',status='unknown')
      open(63,file='fort.63',form='unformatted',status='unknown')
      open(64,file='fort.64',form='unformatted',status='unknown')
      open(65,file='fort.65',form='unformatted',status='unknown')
      open(66,file='fort.66',form='unformatted',status='unknown')
      open(67,file='fort.67',form='unformatted',status='unknown')
      open(68,file='fort.68',form='unformatted',status='unknown')
      open(69,file='fort.69',form='unformatted',status='unknown')
      open(70,file='fort.70',form='unformatted',status='unknown')
      open(71,file='fort.71',form='unformatted',status='unknown')
      open(72,file='fort.72',form='unformatted',status='unknown')
      open(73,file='fort.73',form='unformatted',status='unknown')
      open(74,file='fort.74',form='unformatted',status='unknown')
      open(75,file='fort.75',form='unformatted',status='unknown')
      open(76,file='fort.76',form='unformatted',status='unknown')
      open(77,file='fort.77',form='unformatted',status='unknown')
      open(78,file='fort.78',form='unformatted',status='unknown')
      open(79,file='fort.79',form='unformatted',status='unknown')
      open(80,file='fort.80',form='unformatted',status='unknown')
      open(81,file='fort.81',form='unformatted',status='unknown')
      open(82,file='fort.82',form='unformatted',status='unknown')
      open(83,file='fort.83',form='unformatted',status='unknown')
      open(84,file='fort.84',form='unformatted',status='unknown')
      open(85,file='fort.85',form='unformatted',status='unknown')
      open(86,file='fort.86',form='unformatted',status='unknown')
      open(87,file='fort.87',form='unformatted',status='unknown')
      open(88,file='fort.88',form='unformatted',status='unknown')
      open(89,file='fort.89',form='unformatted',status='unknown')
      open(90,file='fort.90',form='unformatted',status='unknown')
      open(98,file='fort.98',form='formatted',status='unknown')
      write(*,10010)
      tlim=1e7
      call timest(tlim)
      call datime(idate,itime)
      write(cdate,'(I6.6)') idate
      write(ctime,'(I4.4)') itime
      read(cdate(3:4),'(I2)') imonth
      if(cdate(6:6).eq.'1'.and.cdate(5:5).ne.'1') then
        day='SIXTRACK starts on: '//cdate(5:6)//'st of' //cmonth(imonth)&
     &//' 20'//cdate(1:2)//', '
      else if(cdate(6:6).eq.'2'.and.cdate(5:5).ne.'1') then
        day='SIXTRACK starts on: '//cdate(5:6)//'nd of' //cmonth(imonth)&
     &//' 20'//cdate(1:2)//', '
      else if(cdate(6:6).eq.'3'.and.cdate(5:5).ne.'1') then
        day='SIXTRACK starts on: '//cdate(5:6)//'rd of' //cmonth(imonth)&
     &//' 20'//cdate(1:2)//', '
      else
        day='SIXTRACK starts on: '//cdate(5:6)//'th of' //cmonth(imonth)&
     &//' 20'//cdate(1:2)//', '
      endif
      if(ctime(1:2).ne.'  ') then
        if(ctime(3:4).eq.'  ') then
          runtim=day(1:44)//' at '//ctime(1:2)//'.'
        else if(ctime(3:4).eq.'01') then
          runtim=day(1:44)//' one minute after '//ctime(1:2)//'.'
        else
          runtim=day(1:44)//ctime(3:4)//' minutes after '//ctime(1:2)// &
     &'.'
        endif
      else
        if(ctime(3:4).eq.'  ') then
          runtim=day(1:44)//' at midnight.'
        else if(ctime(3:4).eq.'01') then
          runtim=day(1:44)//' one minute after midnight.'
        else
          runtim=day(1:44)//ctime(3:4)//' minutes after midnight.'
        endif
      endif
      write(*,*) runtim
!     A normal start, time0 is used to accumulate total
      time0=0.
      do 10 i=1,nblz
        xsi(i)=zero
        zsi(i)=zero
        smi(i)=zero
        rsmi(i)=zero
        rfres(i)=zero
        rzphs(i)=zero
   10 continue
      do 20 i=1,mmul
        cr(i)=zero
        ci(i)=zero
   20 continue
      do 30 i=1,2
        eps(i)=zero
        epsa(i)=zero
        ekk(i)=zero
        qw(i)=zero
        qwc(i)=zero
   30 continue
      do 60 i=1,npart
        nnumxv(i)=0
        xv(1,i)=zero
        xv(2,i)=zero
        yv(1,i)=zero
        yv(2,i)=zero
        dam(i)=zero
        ekkv(i)=zero
        sigmv(i)=zero
        dpsv(i)=zero
        dp0v(i)=zero
        ejv(i)=zero
        ejfv(i)=zero
        xlv(i)=zero
        zlv(i)=zero
        rvv(i)=one
        ejf0v(i)=zero
        dpd(i)=zero
        dpsq(i)=zero
        fok(i)=zero
        rho(i)=zero
        fok1(i)=zero
        si(i)=zero
        co(i)=zero
        g(i)=zero
        gl(i)=zero
        sm1(i)=zero
        sm2(i)=zero
        sm3(i)=zero
        sm12(i)=zero
        as3(i)=zero
        as4(i)=zero
        as6(i)=zero
        sm23(i)=zero
        rhoc(i)=zero
        siq(i)=zero
        aek(i)=zero
        afok(i)=zero
        hp(i)=zero
        hm(i)=zero
        hc(i)=zero
        hs(i)=zero
        wf(i)=zero
        wfa(i)=zero
        wfhi(i)=zero
        rhoi(i)=zero
        hi(i)=zero
        fi(i)=zero
        hi1(i)=zero
        xvl(1,i)=zero
        xvl(2,i)=zero
        yvl(1,i)=zero
        yvl(2,i)=zero
        ejvl(i)=zero
        dpsvl(i)=zero
        oidpsv(i)=one
        sigmvl(i)=zero
        iv(i)=0
        aperv(i,1)=zero
        aperv(i,2)=zero
        ixv(i)=0
        clov(1,i)=zero
        clov(2,i)=zero
        clo6v(1,i)=zero
        clo6v(2,i)=zero
        clo6v(3,i)=zero
        clopv(1,i)=zero
        clopv(2,i)=zero
        clop6v(1,i)=zero
        clop6v(2,i)=zero
        clop6v(3,i)=zero
        alf0v(i,1)=zero
        alf0v(i,2)=zero
        bet0v(i,1)=zero
        bet0v(i,2)=zero
        ampv(i)=zero
        do 40 i1=1,nblo
          do 40 i2=1,2
            do 40 i3=1,6
              hv(i3,i2,i,i1)=zero
              bl1v(i3,i2,i,i1)=zero
   40   continue
        do 50 i1=1,6
          do 50 i2=1,6
            tas(i,i1,i2)=zero
   50   continue
        qwcs(i,1)=zero
        qwcs(i,2)=zero
        qwcs(i,3)=zero
        di0xs(i)=zero
        di0zs(i)=zero
        dip0xs(i)=zero
        dip0zs(i)=zero
   60 continue
      qwc(3)=zero
      call comnul
      commen=' '
      progrm='SIXTRACK'
      pi=four*atan(one)
      pi2=pi*half
      pisqrt=sqrt(pi)
      rad=pi/180
      call daten
      if(ithick.eq.1) write(*,10030)
      if(ithick.eq.0) write(*,10040)
      if(ibidu.eq.2) then
        write(*,10025)
        goto 550
      endif
!--SETTING UP THE PLOTTING
      if(ipos.eq.1.and.                                                 &
     &(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0)) then
        call hlimit(nplo)
        call hplint(kwtype)
        call igmeta(-20,-111)
        cpto='NPTO'
        if(icr.eq.1) cpto='PTO '
        call hplopt(cpto,1)
        call hplopt('DATE',1)
        call hplset('DATE',1.)
        call hplset('CSIZ',.15)
      endif
      if(ipos.eq.1.and.napx.eq.0) then
        do 70 i=1,ndafi
          call postpr(91-i)
   70   continue
        call sumpos
        goto 520
      endif
      do 90 i=1,20
        fake(1,i)=zero
   90 fake(2,i)=zero
      itra=2
      amp00=amp(1)
      if(napx.ne.1) damp=(amp00-amp0)/(napx-1)/2
      napx=2*napx
      iation=abs(ition)
      ib0=0
      dp00=dp1
      if(napx.le.0.or.imc.le.0) goto 490
      do 260 m=1,mmac
!--MULTIPOLE WITH THEIR RANDOM VALUES ADDED
        if(m.ge.2) then
          call recuin(m*izu0,irecuin)
          call ranecu(zfz,nzfz,mcut)
          rsum=zero
          do 100 i=1,nzfz
  100     rsum=rsum+zfz(i)
          rmean=rsum/nzfz
          rsqsum=zero
          do 110 i=1,nzfz
  110     rsqsum=rsqsum+(zfz(i)-rmean)*(zfz(i)-rmean)
          rdev=sqrt(rsqsum/nzfz)
          write(*,10320) m*izu0,nzfz,rmean,rdev
          write(*,10070)
        endif
        if(m.eq.1) call ord
        call clorb(ded)
        do 120 l=1,2
          clo0(l)=clo(l)
  120   clop0(l)=clop(l)
        call clorb(zero)
        do 130 l=1,2
          ll=2*l
          di0(l)=(clo0(l)-clo(l))/ded
  130   dip0(l)=(clop0(l)-clop(l))/ded
        call corrorb
        if(irmod2.eq.1) call rmod(dp1)
        if(iqmod.ne.0) call qmod0
        if(ichrom.eq.1.or.ichrom.eq.3) call chroma
        if(iskew.ne.0) call decoup
        if(ilin.eq.1.or.ilin.eq.3) call linopt(dp1)
!--beam-beam element
        nlino=nlin
        nlin=0
        if(nbeam.ge.1) then
          do 135 i=1,nele
            if(kz(i).eq.20) then
              nlin=nlin+1
              if(nlin.gt.nele) call prror(81)
              bezl(nlin)=bez(i)
            endif
  135     continue
        endif
        if(isub.eq.1) call subre(dp1)
        if(ise.eq.1) call search(dp1)
        izu=0
        do 150 i=1,iu
          ix=ic(i)
          if(ix.le.nblo) goto 150
          ix=ix-nblo
          kpz=kp(ix)
          kzz=kz(ix)
          if(kpz.eq.6.or.kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 150
          if(iorg.lt.0) mzu(i)=izu
          izu=mzu(i)+1
          smizf(i)=zfz(izu)*ek(ix)
          smiv(m,i)=sm(ix)+smizf(i)
          smi(i)=smiv(m,i)
          izu=izu+1
          xsiv(m,i)=xpl(ix)+zfz(izu)*xrms(ix)
          xsi(i)=xsiv(m,i)
          izu=izu+1
          zsiv(m,i)=zpl(ix)+zfz(izu)*zrms(ix)
          zsi(i)=zsiv(m,i)
          if(mout2.eq.1) then
            if(kzz.eq.11) zfz(izu-2)=zero
            if(abs(ek(ix)).le.pieni) zfz(izu-2)=zero
            if(abs(xrms(ix)).le.pieni) zfz(izu-1)=zero
            if(abs(zrms(ix)).le.pieni) zfz(izu)=zero
            write(31,'(a16,1p,d19.11,2d14.6,d17.9)') bez(ix),           &
     &zfz(izu-2),zfz(izu-1),zfz(izu),extalign(i,3)
          endif
          if(kzz.eq.11) then
            r0=ek(ix)
            if(abs(r0).le.pieni) goto 150
            nmz=nmu(ix)
            if(nmz.eq.0) then
              izu=izu+2*mmul
              goto 150
            endif
            im=irm(ix)
            r0a=one
            do 140 k=1,nmz
              izu=izu+1
              aaiv(k,m,i)=ed(ix)*(ak0(im,k)+zfz(izu)*aka(im,k))/r0a
              aai(i,k)=aaiv(k,m,i)
              izu=izu+1
              bbiv(k,m,i)=ed(ix)*(bk0(im,k)+zfz(izu)*bka(im,k))/r0a
              bbi(i,k)=bbiv(k,m,i)
  140       r0a=r0a*r0
            izu=izu+2*mmul-2*nmz
          endif
  150   continue
        dp1=zero
        if(ichrom.gt.1) then
          itiono=ition
          ition=0
          call chromda
          ition=itiono
          do ncrr=1,iu
            ix=ic(ncrr)
            if(ix.gt.nblo) ix=ix-nblo
            if(ix.eq.is(1).or.iratioe(ix).eq.is(1)) then
              smiv(m,ncrr)=smi(ncrr)
            else if(ix.eq.is(2).or.iratioe(ix).eq.is(2)) then
              smiv(m,ncrr)=smi(ncrr)
            endif
          enddo
        endif
        dp1=dp00
        dp0=dp00
        if(imc.gt.1) then
          ddp1=two*dp0/(imc-one)
        endif
        do 250 ib=1,imc
          if(imc.gt.1) then
            dp1=dp0-(ib-1)*ddp1
          endif
          dp10=dp1
!-----------------------------------------------------------------------
          if(idp.ne.1.or.iation.ne.1) iclo6=0
          if (iclo6.eq.1.or.iclo6.eq.2) then
            if(ib.eq.1) then
              if(iclo6r.eq.0) then
                clo6(1)=clo(1)
                clop6(1)=clop(1)
                clo6(2)=clo(2)
                clop6(2)=clop(2)
                clo6(3)=zero
                clop6(3)=zero
              else
                read(33,*) (clo6(l),clop6(l), l=1,3)
              endif
              call clorb(zero)
              call betalf(zero,qw)
              call phasad(zero,qwc)
              sigm(1)=clo6(3)
              dps(1)=clop6(3)
              call qmodda(3,qwc)
              if(ilin.ge.2) then
                nlinoo=nlin
                nlin=nlino
                ilinc=1
                call mydaini(2,2,6,3,6,1)
                nlin=nlinoo
              endif
              dp1=dp10+clop6(3)
            endif
            if(iqmod6.eq.1) then
              do ncrr=1,iu
                ix=ic(ncrr)
                if(ix.gt.nblo) ix=ix-nblo
                if(ix.eq.iq(1).or.iratioe(ix).eq.iq(1)) then
                  smiv(m,ncrr)=smi(ncrr)
                else if(ix.eq.iq(2).or.iratioe(ix).eq.iq(2)) then
                  smiv(m,ncrr)=smi(ncrr)
                endif
              enddo
            endif
            do 190 ib1=1,napx
              ib3=ib1+(m+ib-2)*napx
!--beam-beam element
              clo6v(1,ib3)=clo6(1)
              clo6v(2,ib3)=clo6(2)
              clo6v(3,ib3)=clo6(3)
              clop6v(1,ib3)=clop6(1)
              clop6v(2,ib3)=clop6(2)
              clop6v(3,ib3)=clop6(3)
              di0xs(ib3)=di0(1)
              di0zs(ib3)=di0(2)
              dip0xs(ib3)=dip0(1)
              dip0zs(ib3)=dip0(2)
              qwcs(ib3,1)=qwc(1)
              qwcs(ib3,2)=qwc(2)
              qwcs(ib3,3)=qwc(3)
              do 180 i2=1,6
                do 180 j2=1,6
                  tas(ib3,i2,j2)=tasm(i2,j2)
  180         continue
  190       continue
          else
            if(idp.eq.1.and.iation.eq.1) then
              ncorruo=ncorru
              ncorru=1
              call clorb(zero)
              call betalf(zero,qw)
              call phasad(zero,qwc)
!--beam-beam element
              if(nbeam.ge.1) then
              nd=3
              nd2=6
              dps0=dps(1)
              dps(1)=zero
              iqmodc=4
              call mydaini(1,2,nd2,nd,nd2,1)
              ilinc=2
              call mydaini(2,2,nd2,nd,nd2,1)
              dps(1)=dps0
              endif
              ncorru=ncorruo
              iqmodc=3
              call mydaini(2,2,6,3,6,1)
              do i=1,2
                qwc(i)=int(qwc(i))+wxys(i)
              enddo
              if(ilin.ge.2) then
                nlinoo=nlin
                nlin=nlino
                ilinc=1
                call mydaini(2,2,6,3,6,1)
                nlin=nlinoo
              endif
            else
              dps(1)=dp1
              ncorruo=ncorru
              ncorru=1
              call clorb(dp1)
              call betalf(dp1,qw)
              call phasad(dp1,qwc)
              dp1=zero
!--beam-beam element
              dp1=dps(1)
              ncorru=ncorruo
              if(nvar2.le.5) then
                itiono=ition
                ition=0
              endif
              call qmodda(2,qwc)
              if(nvar2.le.5) ition=itiono
              if(nvar2.le.4.and.ithick.eq.1) call envar(dp1)
              if(ilin.ge.2) then
                nlinoo=nlin
                nlin=nlino
                iqmodc=2
                call mydaini(1,2,5,2,5,1)
                ilinc=1
                call mydaini(2,2,5,2,5,1)
                nlin=nlinoo
              endif
              do ncrr=1,iu
                ix=ic(ncrr)
                if(ix.gt.nblo) ix=ix-nblo
                if(ix.eq.iq(1).or.iratioe(ix).eq.iq(1)) then
                  smiv(m,ncrr)=smi(ncrr)
                else if(ix.eq.iq(2).or.iratioe(ix).eq.iq(2)) then
                  smiv(m,ncrr)=smi(ncrr)
                endif
              enddo
            endif
            do 170 i=1,napx
              iar=(m+ib-2)*napx+i
              clo6v(1,iar)=clo(1)
              clop6v(1,iar)=clop(1)
              clo6v(2,iar)=clo(2)
              clop6v(2,iar)=clop(2)
              di0xs(iar)=di0(1)
              di0zs(iar)=di0(2)
              dip0xs(iar)=dip0(1)
              dip0zs(iar)=dip0(2)
              qwcs(iar,1)=qwc(1)
              qwcs(iar,2)=qwc(2)
              qwcs(iar,3)=zero
              do 160 i2=1,4
                do 160 j2=1,4
  160         tas(iar,i2,j2)=tasm(i2,j2)
  170       continue
          endif
          iar=(m+ib-2)*napx+1
          tasiar16=tas(iar,1,6)*c1m3
          tasiar26=tas(iar,2,6)*c1m3
          tasiar36=tas(iar,3,6)*c1m3
          tasiar46=tas(iar,4,6)*c1m3
          tasiar56=tas(iar,5,6)*c1m3
          tasiar61=tas(iar,6,1)*c1e3
          tasiar62=tas(iar,6,2)*c1e3
          tasiar63=tas(iar,6,3)*c1e3
          tasiar64=tas(iar,6,4)*c1e3
          tasiar65=tas(iar,6,5)*c1e3
          bet0(1)=tas(iar,1,1)*tas(iar,1,1)+tas(iar,1,2)*tas(iar,1,2)
          bet0x2 =tas(iar,1,3)*tas(iar,1,3)+tas(iar,1,4)*tas(iar,1,4)
          bet0x3 =tas(iar,1,5)*tas(iar,1,5)+tasiar16*tasiar16
          gam0x1 =tas(iar,2,1)*tas(iar,2,1)+tas(iar,2,2)*tas(iar,2,2)
          gam0x2 =tas(iar,2,3)*tas(iar,2,3)+tas(iar,2,4)*tas(iar,2,4)
          gam0x3 =tas(iar,2,5)*tas(iar,2,5)+tasiar26*tasiar26
          alf0(1)=-(tas(iar,1,1)*tas(iar,2,1)+tas(iar,1,2)*tas(iar,2,2))
          alf0x2 =-(tas(iar,1,3)*tas(iar,2,3)+tas(iar,1,4)*tas(iar,2,4))
          alf0x3 =-(tas(iar,1,5)*tas(iar,2,5)+tasiar16*tasiar26)
          bet0(2)=tas(iar,3,3)*tas(iar,3,3)+tas(iar,3,4)*tas(iar,3,4)
          bet0z2 =tas(iar,3,1)*tas(iar,3,1)+tas(iar,3,2)*tas(iar,3,2)
          bet0z3 =tas(iar,3,5)*tas(iar,3,5)+tasiar36*tasiar36
          gam0z1 =tas(iar,4,3)*tas(iar,4,3)+tas(iar,4,4)*tas(iar,4,4)
          gam0z2 =tas(iar,4,1)*tas(iar,4,1)+tas(iar,4,2)*tas(iar,4,2)
          gam0z3 =tas(iar,4,5)*tas(iar,4,5)+tasiar46*tasiar46
          alf0(2)=-(tas(iar,3,3)*tas(iar,4,3)+tas(iar,3,4)*tas(iar,4,4))
          alf0z2 =-(tas(iar,3,1)*tas(iar,4,1)+tas(iar,3,2)*tas(iar,4,2))
          alf0z3 =-(tas(iar,3,5)*tas(iar,4,5)+tasiar36*tasiar46)
          bet0s1 =tas(iar,5,5)*tas(iar,5,5)+tasiar56*tasiar56
          bet0s2 =tas(iar,5,1)*tas(iar,5,1)+tas(iar,5,2)*tas(iar,5,2)
          bet0s3 =tas(iar,5,3)*tas(iar,5,3)+tas(iar,5,4)*tas(iar,5,4)
          gam0s1 =tasiar65*tasiar65+tas(iar,6,6)*tas(iar,6,6)
          gam0s2 =tasiar61*tasiar61+tasiar62*tasiar62
          gam0s3 =tasiar63*tasiar63+tasiar64*tasiar64
          alf0s1 =-(tas(iar,5,5)*tasiar65+tasiar56*tas(iar,6,6))
          alf0s2 =-(tas(iar,5,1)*tasiar61+tas(iar,5,2)*tasiar62)
          alf0s3 =-(tas(iar,5,3)*tasiar63+tas(iar,5,4)*tasiar64)
          do 220 ib1=1,napx
            iar=ib1+(m+ib-2)*napx
            do 200 ib2=1,6
              do 200 ib3=1,6
  200       tau(ib2,ib3)=tas(iar,ib3,ib2)
            if(abs(tau(1,1)).le.pieni.and.abs(tau(2,2)).le.pieni) then
              tau(1,1)=one
              tau(2,2)=one
            endif
            if(abs(tau(3,3)).le.pieni.and.abs(tau(4,4)).le.pieni) then
              tau(3,3)=one
              tau(4,4)=one
            endif
            if(abs(tau(5,5)).le.pieni.and.abs(tau(6,6)).le.pieni) then
              tau(5,5)=one
              tau(6,6)=one
              call dinv(6,tau,6,idummy,nerror)
              its6d=0
              if(ntwin.ne.2) then
                taus=abs(tau(5,1))+abs(tau(5,2))+abs(tau(5,3))+abs      &
     &(tau(5,4)) +abs(tau(5,5))+abs(tau(5,6))+abs(tau(6,1))             &
     &+abs(tau(6,2)) +abs(tau(6,3))+abs(tau(6,4))+abs                   &
     &(tau(6,5))+abs(tau(6,6)) +abs(tau(1,5))+abs(tau(2,5))             &
     &+abs(tau(3,5))+abs(tau(4,5)) +abs(tau(1,6))+abs                   &
     &(tau(2,6))+abs(tau(3,6))+abs(tau(4,6))-two
                if(abs(taus).ge.pieni) its6d=1
              endif
              do 210 ib2=1,6
                do 210 ib3=1,6
                  tasau(iar,ib2,ib3)=tau(ib2,ib3)
  210         continue
            endif
  220     continue
          if(ierro.ne.0) then
            write(*,10230) dp1
            goto 520
          endif
          write(*,10070)
          phag=phas*180/pi
          if((idp.eq.0).or.(abs(phas).le.pieni.and.ition.eq.0))         &
     &write(*,10170)                                                    &
     &qwc(1),clo(1),clop(1),                                            &
     &bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,                      &
     &qwc(2),clo(2),clop(2),                                            &
     &bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2
          if(idp.eq.1.and.iation.eq.1.and.abs(phas).gt.pieni) then
            if(iclo6.eq.0) then
              write(*,10150) phag,                                      &
     &qwc(1),clo(1),clop(1),                                            &
     &bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,                      &
     &qwc(2),clo(2),clop(2),                                            &
     &bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2
            else
              write(*,10160) phag,                                      &
     &qwc(1),clo6(1),clop6(1),                                          &
     &bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,                      &
     &bet0x3,alf0x3,gam0x3,                                             &
     &qwc(2),clo6(2),clop6(2),                                          &
     &bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2,                      &
     &bet0z3,alf0z3,gam0z3,                                             &
     &qwc(3),clo6(3),clop6(3),                                          &
     &bet0s1,alf0s1,gam0s1,bet0s2,alf0s2,gam0s2,                        &
     &bet0s3,alf0s3,gam0s3
            endif
          endif
          if(idp.eq.1.and.ition.eq.0.and.abs(phas).gt.pieni)            &
     &write(*,10190) phag,                                              &
     &qwc(1),clo(1),clop(1),                                            &
     &bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,                      &
     &qwc(2),clo(2),clop(2),                                            &
     &bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2
          if(idp.eq.1.and.abs(phas).le.pieni.and.iation.eq.1) then
            if(iclo6.eq.0) then
              write(*,10210)                                            &
     &qwc(1),clo(1),clop(1),                                            &
     &bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,                      &
     &qwc(2),clo(2),clop(2),                                            &
     &bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2
            else
              write(*,10220)                                            &
     &qwc(1),clo6(1),clop6(1),                                          &
     &bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,                      &
     &bet0x3,alf0x3,gam0x3,                                             &
     &qwc(2),clo6(2),clop6(2),                                          &
     &bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2,                      &
     &bet0z3,alf0z3,gam0z3,                                             &
     &qwc(3),clo6(3),clop6(3),                                          &
     &bet0s1,alf0s1,gam0s1,bet0s2,alf0s2,gam0s2,                        &
     &bet0s3,alf0s3,gam0s3
            endif
          endif
          write(*,10080) dp1
          e0f=sqrt(e0*e0-pma*pma)
          if(iclo6.eq.0) then
            write(*,10110) clo(1),clop(1),clo(2),clop(2),idz(1),idz(2), &
     &iver, idfor,iclo6,ition
          else
            write(*,10120) clo6(1),clop6(1),clo6(2),clop6(2),clo6(3),   &
     &clop6(3), idz(1),idz(2),iver,idfor,iclo6,ition
          endif
          do 240 ib1=1,napx
            ib2=ib0+ib1
            clov(1,ib2)=clo(1)
            clov(2,ib2)=clo(2)
            clopv(1,ib2)=clop(1)
            clopv(2,ib2)=clop(2)
            bet0v(ib2,1)=bet0(1)
            bet0v(ib2,2)=bet0(2)
            alf0v(ib2,1)=alf0(1)
            alf0v(ib2,2)=alf0(2)
            ampv(ib2)=amp(1)-damp*(ib1-1)
            dp0v(ib2)=dp10
            dpsv(ib2)=dp10
            oidpsv(ib2)=one/(one+dp1)
            nms(ib2)=m
            do 230 i=1,nele
              ekv(ib2,i)=ek(i)
  230       continue
  240     continue
          ib0=ib0+napx
  250   continue
  260 continue
      if(irip.eq.1) then
        do 280 i=1,iu
          ix=ic(i)
          if(ix.le.nblo) goto 280
          ix=ix-nblo
          do 270 j=1,irco
            jj=nrel(j)
            if(ix.eq.jj) then
              rsmi(i)=ramp(jj)
              rfres(i)=rfre(jj)
              rzphs(i)=rzph(jj)
            endif
  270     continue
  280   continue
      endif
      napx=napx*imc*mmac
      napxo=napx
      if(ibidu.eq.1) then
      write(32)                                                         &
!
!  left out to do tracking
!  numl,niu,amp0,amp(2),damp,chi0,chid,rat,exz(2,6),time0,time1
!
!
     &ierro,erbez,pi,pi2,pisqrt,rad,il,mper,mblo,mbloz,msym,kanf,iu,ic, &
     &ed,el,ek,sm,kz,kp,xpl,xrms,zpl,zrms,mel,mtyp,mstr,a,bl1,bl2,rvf,  &
     &idfor,napx,napxo,numlr,nde,nwr,ird,imc,irew,ntwin,iclo6,iclo6r,   &
     &iver,ibidu,qs,e0,pma,ej,ejf,phas0,phas,hsy,crad,                  &
     &hsyc,phasc,dppoff,sigmoff,tlen,                                   &
     &iicav,itionc,ition,idp,ncy,ixcav,dpscor,                          &
     &sigcor,icode,idam,its6d,bk0,ak0,bka,aka,benki,benkc,r00,irm,nmu,  &
     &zfz,iorg,mzu,bezr,izu0,mmac,mcut,exterr,extalign,tiltc,tilts,     &
     &mout2,icext,icextal,aper,di0,dip0,ta,dma,dmap,dkq,dqq,de0,ded,dsi,&
     &dech,dsm0,itco,itcro,itqv,iout,qw0,iq,iqmod,kpa,iqmod6,bez,elbe,  &
     &bezb,ilin,nt,iprint,ntco,eui,euii,nlin,bezl,betam,pam,betac,pac,  &
     &bclorb,nhmoni,nhcorr,nvmoni,nvcorr,ncororb,apx,apz,sigma0,iclo,   &
     &ncorru,ncorrep,icomb0,icomb,ratio,ratioe,iratioe,                 &
     &icoe,ise,mesa,mp,m21,m22,m23,                                     &
     &ise1,ise2,ise3,isea,qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl,rtc,  &
     &rts,ire,ipr,irmod2,dtr,nre,nur,nch,nqc,npp,nrr,nu,dphix,dphiz,qx0,&
     &qz0,dres,dfft,cma1,cma2,nstart,nstop,iskip,iconv,imad,ipos,iav,   &
     &iwg,ivox,ivoz,ires,ifh,toptit,kwtype,itf,icr,idis,icow,istw,iffw, &
     &nprint,ndafi,irip,irco,ramp,rfre,rzph,nrel,nrturn,qwsk,betx,betz, &
     &alfx,alfz,iskew,nskew,hmal,sixtit,commen,ithick,clo6,clop6,dki,   &
     &sigman,sigman2,sigmanq,clobeam,beamoff,parbe,track6d,ptnfac,      &
     &sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,nbeam,ibbc,    &
     &ibeco,ibtyp,lhc,cotr,rrtr,imtr,bbcu,ibb6d,imbb,                   &
     &as,al,sigm,dps,idz,dp1,itra,                                      &
     &x,y,bet0,alf0,clo,clop,cro,is,ichrom,nnumxv,xsi,zsi,smi,aai,      &
     &bbi,rsmi,rfres,rzphs,ampt,tlim,tasm,preda,idial,nord,nvar,        &
     &nvar2,nsix,ncor,ipar,nordf,                                       &
     &nvarf,nord1,ndimf,idptr,inorm,imod1,imod2,                        &
     &icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,coel,        &
     &ekv,fokqv,aaiv,bbiv,smiv,zsiv,xsiv,xsv,zsv,qw,qwc,clo0,           &
     &clop0,eps,epsa,ekk,cr,ci,xv,yv,dam,ekkv,sigmv,dpsv,dp0v,sigmv6,   &
     &dpsv6,ejv,ejfv,xlv,zlv,pstop,rvv,                                 &
     &ejf0v,numxv,nms,nlostp,dpd,                                       &
     &dpsq,fok,rho,fok1,si,co,g,gl,sm1,sm2,sm3,sm12,as3,as4,as6,sm23,   &
     &rhoc,siq,aek,afok,hp,hm,hc,hs,wf,wfa,wfhi,rhoi,hi,fi,hi1,xvl,yvl, &
     &ejvl,dpsvl,oidpsv,sigmvl,iv,aperv,ixv,clov,clopv,alf0v,bet0v,ampv,&
     &clo6v,clop6v,hv,bl1v,tas,qwcs,di0xs,di0zs,dip0xs,dip0zs,xau,cloau,&
     &di0au,tau,tasau,wx,x1,x2,fake,e0f,numx,cotr,rrtr,imtr
      endif
  550 continue
      if(ibidu.eq.2) then
      read(32)                                                          &
!
!  left out to do tracking
!  numl,niu,amp0,amp(2),damp,chi0,chid,rat,exz(2,6),time0,time1
!
!
     &ierro,erbez,pi,pi2,pisqrt,rad,il,mper,mblo,mbloz,msym,kanf,iu,ic, &
     &ed,el,ek,sm,kz,kp,xpl,xrms,zpl,zrms,mel,mtyp,mstr,a,bl1,bl2,rvf,  &
     &idfor,napx,napxo,numlr,nde,nwr,ird,imc,irew,ntwin,iclo6,iclo6r,   &
     &iver,ibidu,qs,e0,pma,ej,ejf,phas0,phas,hsy,crad,                  &
     &hsyc,phasc,dppoff,sigmoff,tlen,                                   &
     &iicav,itionc,ition,idp,ncy,ixcav,dpscor,                          &
     &sigcor,icode,idam,its6d,bk0,ak0,bka,aka,benki,benkc,r00,irm,nmu,  &
     &zfz,iorg,mzu,bezr,izu0,mmac,mcut,exterr,extalign,tiltc,tilts,     &
     &mout2,icext,icextal,aper,di0,dip0,ta,dma,dmap,dkq,dqq,de0,ded,dsi,&
     &dech,dsm0,itco,itcro,itqv,iout,qw0,iq,iqmod,kpa,iqmod6,bez,elbe,  &
     &bezb,ilin,nt,iprint,ntco,eui,euii,nlin,bezl,betam,pam,betac,pac,  &
     &bclorb,nhmoni,nhcorr,nvmoni,nvcorr,ncororb,apx,apz,sigma0,iclo,   &
     &ncorru,ncorrep,icomb0,icomb,ratio,ratioe,iratioe,                 &
     &icoe,ise,mesa,mp,m21,m22,m23,                                     &
     &ise1,ise2,ise3,isea,qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl,rtc,  &
     &rts,ire,ipr,irmod2,dtr,nre,nur,nch,nqc,npp,nrr,nu,dphix,dphiz,qx0,&
     &qz0,dres,dfft,cma1,cma2,nstart,nstop,iskip,iconv,imad,ipos,iav,   &
     &iwg,ivox,ivoz,ires,ifh,toptit,kwtype,itf,icr,idis,icow,istw,iffw, &
     &nprint,ndafi,irip,irco,ramp,rfre,rzph,nrel,nrturn,qwsk,betx,betz, &
     &alfx,alfz,iskew,nskew,hmal,sixtit,commen,ithick,clo6,clop6,dki,   &
     &sigman,sigman2,sigmanq,clobeam,beamoff,parbe,track6d,ptnfac,      &
     &sigz,sige,partnum,parbe14,emitx,emity,emitz,gammar,nbeam,ibbc,    &
     &ibeco,ibtyp,lhc,cotr,rrtr,imtr,bbcu,ibb6d,imbb,                   &
     &as,al,sigm,dps,idz,dp1,itra,                                      &
     &x,y,bet0,alf0,clo,clop,cro,is,ichrom,nnumxv,xsi,zsi,smi,aai,      &
     &bbi,rsmi,rfres,rzphs,ampt,tlim,tasm,preda,idial,nord,nvar,        &
     &nvar2,nsix,ncor,ipar,nordf,                                       &
     &nvarf,nord1,ndimf,idptr,inorm,imod1,imod2,                        &
     &icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,coel,        &
     &ekv,fokqv,aaiv,bbiv,smiv,zsiv,xsiv,xsv,zsv,qw,qwc,clo0,           &
     &clop0,eps,epsa,ekk,cr,ci,xv,yv,dam,ekkv,sigmv,dpsv,dp0v,sigmv6,   &
     &dpsv6,ejv,ejfv,xlv,zlv,pstop,rvv,                                 &
     &ejf0v,numxv,nms,nlostp,dpd,                                       &
     &dpsq,fok,rho,fok1,si,co,g,gl,sm1,sm2,sm3,sm12,as3,as4,as6,sm23,   &
     &rhoc,siq,aek,afok,hp,hm,hc,hs,wf,wfa,wfhi,rhoi,hi,fi,hi1,xvl,yvl, &
     &ejvl,dpsvl,oidpsv,sigmvl,iv,aperv,ixv,clov,clopv,alf0v,bet0v,ampv,&
     &clo6v,clop6v,hv,bl1v,tas,qwcs,di0xs,di0zs,dip0xs,dip0zs,xau,cloau,&
     &di0au,tau,tasau,wx,x1,x2,fake,e0f,numx,cotr,rrtr,imtr
!--SETTING UP THE PLOTTING
        damp=(amp(1)-amp0)/(napx/2-1)/2
        if(ipos.eq.1.and.                                               &
     &(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0)) then
          call hlimit(nplo)
          call hplint(kwtype)
          call igmeta(-20,-111)
          cpto='NPTO'
          if(icr.eq.1) cpto='PTO '
          call hplopt(cpto,1)
          call hplopt('DATE',1)
          call hplset('DATE',1.)
          call hplset('CSIZ',.15)
        endif
      endif
      do 80 i=1,npart
        pstop(i)=.false.
        nnumxv(i)=numl
   80 numxv(i)=numl
      rat0=rat
      do 340 ia=1,napx,2
        if(idfor.ne.2) then
!---------------------------------------  SUBROUTINE 'ANFB' IN-LINE
          write(*,10050)
          tasia56=tas(ia,5,6)*c1m3
          bet0x2=tas(ia,1,3)*tas(ia,1,3)+tas(ia,1,4)*tas(ia,1,4)
          bet0z2=tas(ia,3,1)*tas(ia,3,1)+tas(ia,3,2)*tas(ia,3,2)
          bet0s1=tas(ia,5,5)*tas(ia,5,5)+tasia56*tasia56
          dsign=one
          rat=rat0
          if(tas(ia,3,3).lt.-pieni) rat=-rat
          if(rat.lt.-pieni) dsign=-one
          x11=ampv(ia)/(sqrt(bet0v(ia,1))+sqrt(abs(rat)*bet0x2))
          x13=x11*dsign*sqrt(abs(rat))
          amp(2)=dsign*(1-iver)* (abs(x11)*sqrt(bet0z2)+abs(x13)*sqrt   &
     &(bet0v(ia,2)))
          x1(5)=zero
          x1(6)=dpsv(ia)*sqrt(bet0s1)
          chi=chi0*rad
          dchi=chid*rad
          do 320 i2=1,2
            i3=ia+i2-1
            sic=sin(chi)
            coc=cos(chi)
            x1(1)=x11*coc
            x1(2)=x11*sic
            x1(3)=x13*coc
            x1(4)=x13*sic
            do 300 ii=1,6
              x2(ii)=zero
              do 290 jj=1,6
                x2(ii)=x2(ii)+tas(ia,ii,jj)*x1(jj)
  290         continue
  300       continue
            if(iclo6.eq.1.or.iclo6.eq.2) then
              x2(2)=x2(2)/(one+x2(6)+clop6v(3,ia))
              x2(4)=x2(4)/(one+x2(6)+clop6v(3,ia))
            endif
            if(abs(bet0s1).le.pieni) x2(6)=dpsv(ia)
            if(iver.eq.1) then
              x2(3)=zero
              x2(4)=zero
            endif
            do 310 l=1,2
              ll=(l-1)*2
              xv(l,i3)=x2(1+ll)+exz(i2,1+ll)
              yv(l,i3)=x2(2+ll)+exz(i2,2+ll)
  310       continue
            sigmv(i3)=x2(5)+exz(i2,5)
            dpsv(i3)=x2(6)
            dpsic=dpsv(i3)+clop6v(3,ia)
            if(idp.eq.1.and.abs(ition).eq.1.and.iclo6.eq.0) then
              xv(1,i3)=xv(1,i3)+di0xs(ia)*dpsic
              xv(2,i3)=xv(2,i3)+di0zs(ia)*dpsic
              yv(1,i3)=yv(1,i3)+dip0xs(ia)*dpsic
              yv(2,i3)=yv(2,i3)+dip0zs(ia)*dpsic
            endif
            chi=chi+dchi
  320     continue
          write(*,10260) ia,nms(ia)*izu0,dpsv(ia)
          write(*,10060) xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),           &
     &sigmv(ia),                                                        &
     &dpsv(ia),xv(1,ia+1),yv(1,ia+1),xv(2,ia+1),yv(2,ia+1), sigmv       &
     &(ia+1),dpsv(ia+1)
!---------------------------------------  END OF 'ANFB'
          if(iclo6.eq.2) then
            xv(1,ia)=xv(1,ia)+clo6v(1,ia)
            yv(1,ia)=yv(1,ia)+clop6v(1,ia)
            xv(2,ia)=xv(2,ia)+clo6v(2,ia)
            yv(2,ia)=yv(2,ia)+clop6v(2,ia)
            sigmv(ia)=sigmv(ia)+clo6v(3,ia)
            dpsv(ia)=dpsv(ia)+clop6v(3,ia)
            xv(1,ia+1)=xv(1,ia+1)+clo6v(1,ia)
            yv(1,ia+1)=yv(1,ia+1)+clop6v(1,ia)
            xv(2,ia+1)=xv(2,ia+1)+clo6v(2,ia)
            yv(2,ia+1)=yv(2,ia+1)+clop6v(2,ia)
            sigmv(ia+1)=sigmv(ia+1)+clo6v(3,ia)
            dpsv(ia+1)=dpsv(ia+1)+clop6v(3,ia)
            oidpsv(ia)=one/(one+dpsv(ia))
            oidpsv(ia+1)=one/(one+dpsv(ia+1))
          endif
          if(iclo6.ne.2) then
            xv(1,ia)=xv(1,ia)+clov(1,ia)*idz(1)*(1-idfor)
            yv(1,ia)=yv(1,ia)+clopv(1,ia)*idz(1)*(1-idfor)
            xv(2,ia)=xv(2,ia)+clov(2,ia)*idz(2)*(1-idfor)
            yv(2,ia)=yv(2,ia)+clopv(2,ia)*idz(2)*(1-idfor)
            xv(1,ia+1)=xv(1,ia+1)+clov(1,ia)*idz(1)*(1-idfor)
            yv(1,ia+1)=yv(1,ia+1)+clopv(1,ia)*idz(1)*(1-idfor)
            xv(2,ia+1)=xv(2,ia+1)+clov(2,ia)*idz(2)*(1-idfor)
            yv(2,ia+1)=yv(2,ia+1)+clopv(2,ia)*idz(2)*(1-idfor)
          endif
          ejfv(ia)=e0f*(one+dpsv(ia))
          ejfv(ia+1)=e0f*(one+dpsv(ia+1))
          ejv(ia)=sqrt(ejfv(ia)*ejfv(ia)+pma*pma)
          ejv(ia+1)=sqrt(ejfv(ia+1)*ejfv(ia+1)+pma*pma)
          epsa(1)=(ampv(ia)*ampv(ia)/bet0v(ia,1))
          epsa(2)=(amp(2)*amp(2)/bet0v(ia,2))
          write(*,10020) ampv(ia),amp(2),epsa
        else
          read(13,*,iostat=ierro) xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),  &
     &sigmv(ia),dpsv(ia),xv(1,ia+1),yv(1,ia+1),xv(2,ia+1),yv            &
     &(2,ia+1), sigmv(ia+1),dpsv(ia+1),e0,ejv(ia),ejv(ia+1)
          if(ierro.ne.0) call prror(56)
          e0f=sqrt(e0*e0-pma*pma)
          ejfv(ia)=sqrt(ejv(ia)*ejv(ia)-pma*pma)
          ejfv(ia+1)=sqrt(ejv(ia+1)*ejv(ia+1)-pma*pma)
          oidpsv(ia)=one/(one+dpsv(ia))
          oidpsv(ia+1)=one/(one+dpsv(ia+1))
        endif
        write(*,10090) xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),sigmv(ia),   &
     &dpsv(ia),xv(1,ia+1),yv(1,ia+1),xv(2,ia+1),yv(2,ia+1), sigmv       &
     &(ia+1),dpsv(ia+1),e0,ejv(ia),ejv(ia+1)
        idam=3
        icode=0
        if(abs(xv(1,ia)).le.pieni.and.abs(yv(1,ia)).le.pieni) then
          idam=idam-1
        else
          icode=icode+1
        endif
        if(abs(xv(2,ia)).le.pieni.and.abs(yv(2,ia)).le.pieni) then
          idam=idam-1
        else
          icode=icode+2
        endif
        if(idp.eq.0.or.abs(ition).eq.0) then
          idam=idam-1
        else
          icode=icode+4
        endif
        if(idam.le.0) idam=1
        if(icode.le.0) icode=1
        ia2=(ia+1)/2
        if(ntwin.ne.2) then
          if(mod(ia+1,2).eq.0) then
            xau(1,1)= xv(1,ia)
            xau(1,2)= yv(1,ia)
            xau(1,3)= xv(2,ia)
            xau(1,4)= yv(2,ia)
            xau(1,5)=sigmv(ia)
            xau(1,6)= dpsv(ia)
            xau(2,1)= xv(1,ia+1)
            xau(2,2)= yv(1,ia+1)
            xau(2,3)= xv(2,ia+1)
            xau(2,4)= yv(2,ia+1)
            xau(2,5)=sigmv(ia+1)
            xau(2,6)= dpsv(ia+1)
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
            do 330 ib2=1,6
              do 330 ib3=1,6
                tau(ib2,ib3)=tasau(ia,ib2,ib3)
  330       continue
            call distance(xau,cloau,di0au,tau,dam1)
            dam(ia)=dam1
            dam(ia+1)=dam1
          endif
          write(91-ia2,iostat=ierro) sixtit,commen,cdate, ctime,progrm, &
     &ia,ia,napx,icode,numl,qwcs(ia,1),qwcs(ia,2), qwcs(ia,3),clo6v     &
     &(1,ia),clop6v(1,ia),clo6v(2,ia),clop6v(2,ia), clo6v(3,ia),        &
     &clop6v(3,ia), di0xs(ia),dip0xs(ia),di0zs(ia),dip0zs(ia),zero,     &
     &one, tas(ia,1,1),tas(ia,1,2),tas(ia,1,3),tas(ia,1,4),tas          &
     &(ia,1,5), tas(ia,1,6), tas(ia,2,1),tas(ia,2,2),tas(ia,2,3),tas    &
     &(ia,2,4),tas(ia,2,5), tas(ia,2,6), tas(ia,3,1),tas(ia,3,2),tas    &
     &(ia,3,3),tas(ia,3,4),tas(ia,3,5), tas(ia,3,6), tas(ia,4,1),tas    &
     &(ia,4,2),tas(ia,4,3),tas(ia,4,4),tas(ia,4,5), tas(ia,4,6), tas    &
     &(ia,5,1),tas(ia,5,2),tas(ia,5,3),tas(ia,5,4),tas(ia,5,5), tas     &
     &(ia,5,6), tas(ia,6,1),tas(ia,6,2),tas(ia,6,3),tas(ia,6,4),tas     &
     &(ia,6,5), tas(ia,6,6),                                            &
     &dble(mmac),dble(nms(ia)),dble(izu0),                              &
     &dble(numlr),sigcor,dpscor,dble(nrturn),zero,zero,zero,            &
     &zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,                &
     &zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,                &
     &zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,                &
     &zero,zero,zero,zero,zero,zero,zero,zero,zero,zero
        else
          write(91-ia2,iostat=ierro) sixtit,commen,cdate, ctime,progrm, &
     &ia,ia+1,napx,icode,numl,qwcs(ia,1),qwcs(ia,2), qwcs(ia,3),        &
     &clo6v(1,ia),clop6v(1,ia),clo6v(2,ia),clop6v(2,ia), clo6v          &
     &(3,ia),clop6v(3,ia), di0xs(ia),dip0xs(ia),di0zs(ia),dip0zs        &
     &(ia),zero,one, tas(ia,1,1),tas(ia,1,2),tas(ia,1,3),tas            &
     &(ia,1,4),tas(ia,1,5), tas(ia,1,6), tas(ia,2,1),tas(ia,2,2),tas    &
     &(ia,2,3),tas(ia,2,4),tas(ia,2,5), tas(ia,2,6), tas(ia,3,1),tas    &
     &(ia,3,2),tas(ia,3,3),tas(ia,3,4),tas(ia,3,5), tas(ia,3,6), tas    &
     &(ia,4,1),tas(ia,4,2),tas(ia,4,3),tas(ia,4,4),tas(ia,4,5), tas     &
     &(ia,4,6), tas(ia,5,1),tas(ia,5,2),tas(ia,5,3),tas(ia,5,4),tas     &
     &(ia,5,5), tas(ia,5,6), tas(ia,6,1),tas(ia,6,2),tas(ia,6,3),tas    &
     &(ia,6,4),tas(ia,6,5), tas(ia,6,6),                                &
     &dble(mmac),dble(nms(ia)),dble(izu0),                              &
     &dble(numlr),sigcor,dpscor,dble(nrturn),zero,zero,zero,            &
     &zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,                &
     &zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,                &
     &zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,                &
     &zero,zero,zero,zero,zero,zero,zero,zero,zero,zero
        endif
        if(ierro.ne.0) then
          write(*,*)
          write(*,*) '*** ERROR ***,PROBLEMS WRITING TO FILE # : ',91   &
     &-ia2
          write(*,*) 'ERROR CODE : ',ierro
          write(*,*)
          goto 520
        endif
  340 continue
      if(e0.gt.pieni) then
        do 350 j=1,napx
  350   rvv(j)=(ejv(j)*e0f)/(e0*ejfv(j))
      else
        call prror(79)
      endif
      if(ithick.eq.1) call envarsv(dpsv,oidpsv,rvv,ekv)
!-------------------------------------  START OF 'BLOCK'
      do 440 k=1,mblo
        jm=mel(k)
        ikk=mtyp(k,1)
        do 370 lkk=1,2
          do 370 mkk=1,6
            do 360 ia=1,napx
              dpoff=dpsv(ia)*c1e3
              if(abs(dpoff).le.pieni) dpoff=one
              hv(mkk,lkk,ia,1)=al(mkk,lkk,ia,ikk)
  360       if(mkk.eq.5.or.mkk.eq.6) hv(mkk,lkk,ia,1)=hv(mkk,lkk,ia,1)  &
     &/dpoff
  370   continue
        if(jm.eq.1) goto 410
        do 400 j=2,jm
          ikk=mtyp(k,j)
          do 390 lkk=1,2
            do 380 ia=1,napx
              dpoff=dpsv(ia)*c1e3
              if(abs(dpoff).le.pieni) dpoff=one
              hv(1,lkk,ia,j)=hv(1,lkk,ia,j-1)*al(1,lkk,ia,ikk)+ hv(3,   &
     &lkk,ia,j-1)*al(2,lkk,ia,ikk)
              hv(2,lkk,ia,j)=hv(2,lkk,ia,j-1)*al(1,lkk,ia,ikk)+ hv(4,   &
     &lkk,ia,j-1)*al(2,lkk,ia,ikk)
              hv(3,lkk,ia,j)=hv(1,lkk,ia,j-1)*al(3,lkk,ia,ikk)+ hv(3,   &
     &lkk,ia,j-1)*al(4,lkk,ia,ikk)
              hv(4,lkk,ia,j)=hv(2,lkk,ia,j-1)*al(3,lkk,ia,ikk)+ hv(4,   &
     &lkk,ia,j-1)*al(4,lkk,ia,ikk)
              hv(5,lkk,ia,j)=hv(5,lkk,ia,j-1)*al(1,lkk,ia,ikk)+ hv(6,   &
     &lkk,ia,j-1)*al(2,lkk,ia,ikk)+al(5,lkk,ia,ikk)/dpoff
              hv(6,lkk,ia,j)=hv(5,lkk,ia,j-1)*al(3,lkk,ia,ikk)+ hv(6,   &
     &lkk,ia,j-1)*al(4,lkk,ia,ikk)+al(6,lkk,ia,ikk)/dpoff
  380       continue
  390     continue
  400   continue
  410   do 430 lkk=1,2
          do 430 mkk=1,6
            do 420 ia=1,napx
  420       bl1v(mkk,lkk,ia,k)=hv(mkk,lkk,ia,jm)
  430   continue
  440 continue
!---------------------------------------  END OF 'BLOCK'
      write(*,10200)
      time1=0.
      call timex(time1)
!---------------------------------------  LOOP OVER TURNS TO BE TRACKED
      if(ithick.eq.0) call trauthin(nthinerr)
      if(ithick.eq.1) call trauthck(nthinerr)
      time2=0.
      call timex(time2)
      if(nthinerr.eq.3000) goto 520
      if(nthinerr.eq.3001) goto 460
!---------------------------------------  END OF LOOP OVER TURNS
  460 continue
      napxto=0
      id=0
      numx=numx+1
      call writebin(nthinerr)
      if(nthinerr.eq.3000) goto 520
      do 470 ia=1,napxo,2
        ie=ia+1
        ia2=(ie)/2
        napxto=napxto+numxv(ia)+numxv(ie)
        if(pstop(ia).and.pstop(ie)) then
!-- BOTH PARTICLES LOST
          write(*,10000) ia,nms(ia)*izu0,dp0v(ia),numxv(ia),            &
     &abs(xvl(1,ia)),aperv(ia,1),abs(xvl(2,ia)),aperv(ia,2)
          write(*,10000) ie,nms(ia)*izu0,dp0v(ia),numxv(ie),            &
     &abs(xvl(1,ie)),aperv(ie,1),abs(xvl(2,ie)),aperv(ie,2)
          write(*,10280)                                                &
     &xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),     &
     &xvl(1,ie),yvl(1,ie),xvl(2,ie),yvl(2,ie),sigmvl(ie),dpsvl(ie),     &
     &e0,ejvl(ia),ejvl(ie)
          write(12,10280,iostat=ierro)                                  &
     &xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),     &
     &xvl(1,ie),yvl(1,ie),xvl(2,ie),yvl(2,ie),sigmvl(ie),dpsvl(ie),     &
     &e0,ejvl(ia),ejvl(ie)
          if(ierro.ne.0) write(*,*) 'Warning from maincr: fort.12 has ',&
     &'corrupted output probably due to lost particle: ',ia,            &
     &' or: ',ie
        endif
        if(.not.pstop(ia).and.pstop(ie)) then
!-- SECOND PARTICLE LOST
          id=id+1
          write(*,10240) ia,nms(ia)*izu0,dp0v(ia),numxv(ia)
          write(*,10000) ie,nms(ia)*izu0,dp0v(ia),numxv(ie),            &
     &abs(xvl(1,ie)),aperv(ie,1),abs(xvl(2,ie)),aperv(ie,2)
          write(*,10280)                                                &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &xvl(1,ie),yvl(1,ie),xvl(2,ie),yvl(2,ie),sigmvl(ie),dpsvl(ie),     &
     &e0,ejv(id),ejvl(ie)
          write(12,10280,iostat=ierro)                                  &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &xvl(1,ie),yvl(1,ie),xvl(2,ie),yvl(2,ie),sigmvl(ie),dpsvl(ie),     &
     &e0,ejv(id),ejvl(ie)
          if(ierro.ne.0) write(*,*) 'Warning from maincr: fort.12 has ',&
     &'corrupted output probably due to lost particle: ',ie
        endif
        if(pstop(ia).and..not.pstop(ie)) then
!-- FIRST PARTICLE LOST
          id=id+1
          write(*,10000) ia,nms(ia)*izu0,dp0v(ia),numxv(ia),            &
     &abs(xvl(1,ia)),aperv(ia,1),abs(xvl(2,ia)),aperv(ia,2)
          write(*,10240) ie,nms(ia)*izu0,dp0v(ia),numxv(ie)
          write(*,10280)                                                &
     &xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),     &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &e0,ejvl(ia),ejv(id)
          write(12,10280,iostat=ierro)                                  &
     &xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),     &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &e0,ejvl(ia),ejv(id)
          if(ierro.ne.0) write(*,*) 'Warning from maincr: fort.12 has ',&
     &'corrupted output probably due to lost particle: ',ia
        endif
        if(.not.pstop(ia).and..not.pstop(ie)) then
!-- BOTH PARTICLES STABLE
          id=id+1
          ig=id+1
          write(*,10270) ia,ie,nms(ia)*izu0,dp0v(ia),numxv(ia)
          write(*,10280)                                                &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &xv(1,ig),yv(1,ig),xv(2,ig),yv(2,ig),sigmv(ig),dpsv(ig),           &
     &e0,ejv(id),ejv(ig)
          write(12,10280,iostat=ierro)                                  &
     &xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),           &
     &xv(1,ig),yv(1,ig),xv(2,ig),yv(2,ig),sigmv(ig),dpsv(ig),           &
     &e0,ejv(id),ejv(ig)
          if(ierro.ne.0) write(*,*) 'Warning from maincr: fort.12 has ',&
     &'corrupted output although particles stable'
          id=ig
        endif
  470 continue
      iposc=0
      if(ipos.eq.1) then
        do 480 ia=1,napxo,2
          ia2=(ia+1)/2
          iposc=iposc+1
          call postpr(91-ia2)
  480   continue
        if(iposc.ge.1) call sumpos
      endif
      goto 520
  490 if(ipos.eq.1) then
        ndafi2=ndafi
        do 500 ia=1,ndafi2
          if(ia.gt.ndafi) goto 510
          call postpr(91-ia)
  500   continue
  510   if(ndafi.ge.1) call sumpos
      endif
  520 continue
!--HPLOTTING END
      if(ipos.eq.1.and.                                                 &
     &(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0)) then
        call igmeta(999,0)
        call hplend
      endif
      time=time2-time1
      time2=0.
      call timex(time2)
      write(*,10290) time1
      write(*,10300) napxto,time
      write(*,10310) time2
!-----------------------------------------------------------------------
!--CLOSE(DATA FILES
      close(2)
      close(3)
      close(4)
      close(7)
      close(8)
      close(9)
      close(10)
      close(11)
      close(12)
      close(13)
      close(14)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)
      close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      close(26)
      close(27)
      close(32)
      close(33)
      close(34)
      close(59)
      close(60)
      close(61)
      close(62)
      close(63)
      close(64)
      close(65)
      close(66)
      close(67)
      close(68)
      close(69)
      close(70)
      close(71)
      close(72)
      close(73)
      close(74)
      close(75)
      close(76)
      close(77)
      close(78)
      close(79)
      close(80)
      close(81)
      close(82)
      close(83)
      close(84)
      close(85)
      close(86)
      close(87)
      close(88)
      close(89)
      close(90)
      close(98)
      close(99)
      stop
10000 format(/t10,'TRACKING ENDED ABNORMALLY'/t10, 'PARTICLE ',i3,      &
     &' RANDOM SEED ',i8,/ t10,' MOMENTUM DEVIATION ',g12.5,            &
     &' LOST IN REVOLUTION ',i8,/ t10,'HORIZ:  AMPLITUDE = ',f15.3,     &
     &'   APERTURE = ',f15.3/ t10,'VERT:   AMPLITUDE = ',f15.3,         &
     &'   APERTURE = ',f15.3/)
10010 format(/t10,'SIXTRACK VECTOR VERSION 4.0.08 (with tilt)',         &
     &'  --  (last change: 20.10.2005)'//)
10020 format(/t10,'UNCOUPLED AMPLITUDES AND EMITTANCES:', /t10,         &
     &'AMPLITUDE-X = ',f15.3,10x,'AMPLITUDE-Y = ',f15.3, '  MM'/t10,    &
     &'EMITTANCE-X = ',f15.3,10x,'EMITTANCE-Y = ',f15.3, '  PI*MRAD*MM')
10025 format(/t10,'Run started from binary dump file # 32')
10030 format(/t10,'STRUCTURE INPUT FILE HAS -THICK- LINEAR ',           &
     &'ELEMENTS'//)
10040 format(/t10,'STRUCTURE INPUT FILE HAS ONLY -THIN- LINEAR ',       &
     &'ELEMENTS'//)
10050 format(//131('-')//t10,27('O')/t10,2('O'),23x,2('O')/t10,         &
     &'OO  INITIAL COORDINATES  OO'/ t10,2('O'),23x,2('O')/t10,27('O')  &
     &//131('-')//)
10060 format(/5x,'---- TWIN-TRAJECTORIES NO CL.ORBIT ADDED'/ 5x,'/X1  /'&
     &,f47.33/5x,'/XP1 /',f47.33/ 5x,'/Y1  /',f47.33/5x,'/YP1 /',f47.33/&
     &5x,'/SIG1/',f47.33/5x,'/DP1 /',f47.33/ 5x,'/X2  /',f47.33/5x,     &
     &'/XP2 /',f47.33/ 5x,'/Y2  /',f47.33/5x,'/YP2 /',f47.33/ 5x,       &
     &'/SIG2/',f47.33/5x,'/DP2 /',f47.33/)
10070 format(/131('-'))
10080 format(/t10,'REL. MOMENTUM DEVIATION=',f19.16/ t8,                &
     &'========================================')
10090 format(/5x,'---- INITIAL COORD. OF TWIN-TRAJECTORIES'/ 15(10x,f47.&
     &33/))
10110 format(/5x,'---- CLOSED ORBIT AND DECOUPLING (1=COU,0=DECOU)'/ 5x,&
     &'/CLX /',f47.33/5x,'/CLXP/',f47.33/ 5x,'/CLY /',f47.33/5x,'/CLYP/'&
     &,f47.33/ 5x,'/DCX / ',i4/5x,'/DCY / ',i4/ 5x,'/IVER /',i4/ 5x,    &
     &'/IDFOR/',i4/ 5x,'/ICLO6/',i4/ 5x,'/ITION/',i4/5x/)
10120 format(/5x,'---- CLOSED ORBIT AND DECOUPLING (1=COU,0=DECOU)'/ 5x,&
     &'/CLX /',f47.33/5x,'/CLXP/',f47.33/ 5x,'/CLY /',f47.33/5x,'/CLYP/'&
     &,f47.33/ 5x,'/CLS /',f47.33/5x,'/CLSP/',f47.33/ 5x,'/DCX / ',i4/5 &
     &x,'/DCY / ',i4/ 5x,'/IVER /',i4/ 5x,'/IDFOR/',i4/ 5x,'/ICLO6/',i4/&
     &5x,'/ITION/',i4/5x/)
10150 format(/t10,'TRACKING WITH SYNCHROTRON OSCILLATIONS'/ 15x,        &
     &'ACCELERATION WITH PHASE = ',f8.4/ t15,                           &
     &'       TUNE         CLO            CLOP           ',             &
     &'   BET0           ALF0           GAMMA      '//                  &
     &t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/                                        &
     &t10,'  Y  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9,f15.9/)
10160 format(/t10,'TRACKING WITH SYNCHROTRON OSCILLATIONS'/ 15x,        &
     &'ACCELERATION WITH PHASE = ',f8.4/ t15,                           &
     &'       TUNE         CLO            CLOP           ',             &
     &'   BET0           ALF0           GAMMA      '//                  &
     &t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/              &
     &t10,'  Y  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/              &
     &t10,'  S  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/)
10170 format(/t10,'TRACKING FOR CONSTANT MOMENTUM DEVIATION'// 15x,     &
     &'------ NO ACCELERATION ------'// t15,                            &
     &'       TUNE         CLO            CLOP           ',             &
     &'   BET0           ALF0           GAMMA      '//                  &
     &t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/                                        &
     &t10,'  Y  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/)
10180 format(t5//t5,'BACK-TRACKING'/ t5, '============='//)
10190 format(t10,'TRACKING FOR CONSTANT MOMENTUM DEVIATION'// 15x,      &
     &'ACCELERATION WITH PHASE = ',f8.4/ t15,                           &
     &'       TUNE         CLO            CLOP           ',             &
     &'   BET0           ALF0           GAMMA      '//                  &
     &t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/                                        &
     &t10,'  Y  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/)
10200 format(//131('-')//t10,16('O')/t10,2('O'),12x,2('O')/t10,         &
     &'OO  TRACKING  OO', /t10,2('O'),12x,2('O')/t10,16('O')//131('-')//&
     &)
10210 format(/t10,'TRACKING WITH SYNCHROTRON OSCILLATIONS'/ 15x,        &
     &'------ NO ACCELERATION ------'// t15,                            &
     &'       TUNE         CLO            CLOP           ',             &
     &'   BET0           ALF0           GAMMA      '//                  &
     &t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/                                        &
     &t10,'  Y  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/)
10220 format(/t10,'TRACKING WITH SYNCHROTRON OSCILLATIONS'/ 15x,        &
     &'------ NO ACCELERATION ------'// t15,                            &
     &'       TUNE         CLO            CLOP           ',             &
     &'   BET0           ALF0           GAMMA      '//                  &
     &t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/              &
     &t10,'  Y  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/              &
     &t10,'  S  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/          &
     &t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/)
10230 format(t10,'NO OPTICAL SOLUTION FOR',2x,f19.16,2x,                &
     &'RELATIVE MOMENTUM DEVIATION')
10240 format(1x/5x,'PARTICLE ',i3,' STABLE - RANDOM SEED ', i8,         &
     &' MOMENTUM DEVIATION ',g12.5 /5x,'REVOLUTION ',i8/)
10250 format(1x/5x,'PARTICLE ',i3,' RANDOM SEED ',i8,                   &
     &' MOMENTUM DEVIATION ',g12.5 /5x,'REVOLUTION ',i8/)
10260 format(1x/5x,'PARTICLE ',i3,' RANDOM SEED ',i8,                   &
     &' MOMENTUM DEVIATION ',g12.5/)
10270 format(1x/5x,'PARTICLE ',i3,' AND ',i3,' STABLE - RANDOM SEED ',  &
     &i8,' MOMENTUM DEVIATION ',g12.5 /5x,'REVOLUTION ',i8/)
10280 format(10x,f47.33)
10290 format(/10x,'The Preparating Calculations took',f12.3,' second(s)'&
     &,' of Computing Time')
10300 format(/10x,'For ',i12,' Turn(s)',g12.3,' second(s) of ',         &
     &'Computing Time was needed'//131('-'))
10310 format(//10x,'Total Time used: ',g12.3,' second(s)'//131('-'))
10320 format(//131('-')//t10,'DATA BLOCK FLUCTUATIONS OF MULTIPOLES'//  &
     &t10,'RANDOM STARTING NUMBER=  ',i20/ t10,                         &
     &'RANDOM NUMBERS GENERATED:',i20/ t10,'MEAN VALUE=',f15.7,         &
     &'  -   DEVIATION=',f15.7)
10330 format(/10x,'ERROR IN OPENING FILES')
      end
      subroutine envarsv(dpsv,oidpsv,rvv,ekv)
!-----------------------------------------------------------------------
!  CALCULATION OF : MOMENTUM-DEPENDING ELEMENT-MATRICES AND
!                   CHANGE OF PATH LENGTHS FOR EACH PARTICLE.
!  CAUTION:
!          A SPECIAL VERSION FOR VECTORIZATION - AUGUST   1994
!-----------------------------------------------------------------------
      implicit none
      integer ih1,ih2,j,kz1,l,l1,l2
      double precision aek,afok,as3,as4,as6,co,dpd,dpsq,dpsv,ekv,fi,    &
     &fok,fok1,fokm,fokqv,g,gl,hc,hi,hi1,hm,hp,hs,oidpsv,rho,rhoc,rhoi, &
     &rvv,si,siq,sm1,sm12,sm2,sm23,sm3,wf,wfa,wfhi
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
!-----------------------------------------------------------------------
      save
!-----------------------------------------------------------------------
      dimension ekv(npart,nele),fokqv(npart),dpsv(npart)
      dimension rvv(npart),oidpsv(npart)
      dimension dpd(npart),dpsq(npart),fok(npart),rho(npart)
      dimension fok1(npart),si(npart),co(npart),g(npart),gl(npart)
      dimension sm1(npart),sm2(npart),sm3(npart),sm12(npart)
      dimension as3(npart),as4(npart),as6(npart),sm23(npart)
      dimension rhoc(npart),siq(npart),aek(npart),afok(npart)
      dimension hp(npart),hm(npart),hc(npart),hs(npart),wf(npart)
      dimension wfa(npart),wfhi(npart),rhoi(npart)
      dimension hi(npart),fi(npart),hi1(npart)
      do 10 j=1,napx
        dpd(j)=one+dpsv(j)
        dpsq(j)=sqrt(dpd(j))
   10 continue
      do 160 l=1,il
        do l1=1,6
          do j=1,napx
            do l2=1,2
              al(l1,l2,j,l)=zero
              as(l1,l2,j,l)=zero
            enddo
          enddo
        enddo
        if(abs(el(l)).le.pieni) goto 160
        kz1=kz(l)+1
!       goto(20,40,80,60,40,60,100,100,140),kz1
        if (kz1.eq.1) goto 20
        if (kz1.eq.2) goto 40
        if (kz1.eq.3) goto 80
        if (kz1.eq.4) goto 60
        if (kz1.eq.5) goto 40
        if (kz1.eq.6) goto 60
        if (kz1.eq.7) goto 100
        if (kz1.eq.8) goto 100
        if (kz1.eq.9) goto 140
        goto 160
!-----------------------------------------------------------------------
!  DRIFTLENGTH
!-----------------------------------------------------------------------
   20   do 30 j=1,napx
          al(1,1,j,l)=one
          al(1,2,j,l)=one
          al(2,1,j,l)=el(l)
          al(2,2,j,l)=el(l)
          al(3,1,j,l)=zero
          al(3,2,j,l)=zero
          al(4,1,j,l)=one
          al(4,2,j,l)=one
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
          al(1,ih1,j,l)=one
          al(2,ih1,j,l)=rho(j)*si(j)
          al(3,ih1,j,l)=zero
          al(4,ih1,j,l)=one
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
          al(2,ih2,j,l)=el(l)
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
          al(1,ih2,j,l)=one
          al(2,ih2,j,l)=el(l)
          al(3,ih2,j,l)=zero
          al(4,ih2,j,l)=one
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
            al(1,1,j,l)=one
            al(1,2,j,l)=one
            al(2,1,j,l)=el(l)
            al(2,2,j,l)=el(l)
            al(3,1,j,l)=zero
            al(3,2,j,l)=zero
            al(4,1,j,l)=one
            al(4,2,j,l)=one
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
            al(2,ih2,j,l)=el(l)
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
          al(1,1,j,l)=one
          al(2,1,j,l)=zero
          al(3,1,j,l)=fok(j)
          al(4,1,j,l)=one
          al(1,2,j,l)=one
          al(2,2,j,l)=zero
          al(3,2,j,l)=-fok(j)
          al(4,2,j,l)=one
  150   continue
  160 continue
      return
      end
      subroutine comnul
!-----------------------------------------------------------------------
!  SUBROUTINE TO SET THE ALL COMMON VARIABLES TO ZERO
!-----------------------------------------------------------------------
      implicit none
      integer i,i1,i2,i3,i4,j
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
      integer alda_da,asda_da,aldaq_da,asdaq_da,smida_da,xx_da,         &
     &yy_da,dpda_da,dpda1_da,sigmda_da,ej1_da,ejf1_da,rv_da
      common/daele/alda_da(2,6),asda_da(2,6),aldaq_da(2,6),             &
     &asdaq_da(2,6),smida_da(mcor),xx_da(2),yy_da(2),dpda_da,dpda1_da,  &
     &sigmda_da,ej1_da,ejf1_da,rv_da
      integer ichromc,ilinc,iqmodc
      double precision clon,chromc,corr,wxys
      common/correct/ corr(3,3),chromc(2),wxys(3),clon(6),iqmodc,       &
     &ichromc,ilinc
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      save
!-----------------------------------------------------------------------
      ncorru=0
      ncorrep=0
      nrturn=0
      ithick=0
      ierro=0
      il=0
      iclo6=0
      iclo6r=0
      mper=0
      mblo=0
      mbloz=0
      kanf=0
      iu=0
      itra=0
      napx=0
      numl=0
      numlr=0
      ird=0
      imc=0
      niu(1)=0
      niu(2)=0
      idp=0
      irew=0
      iorg=0
      itco=0
      itcro=0
      itqv=0
      ichrom=0
      iqmod=0
      iqmod6=0
      ilin=0
      iqmodc=0
      ichromc=0
      ilinc=0
      ntco=0
      nt=0
      iprint=0
      iclo=0
      icoe=0
      ise=0
      mesa=0
      mp=0
      m21=0
      m22=0
      m23=0
      ise1=0
      ise2=0
      ise3=0
      isub=0
      nta=0
      nte=0
      ipt=0
      irmod2=0
      nre=0
      nur=0
      nch=0
      nqc=0
      npp=0
      ipos=0
      iconv=0
      imad=0
      nstart=0
      nstop=0
      iskip=1
      iav=0
      iwg=0
      ivox=0
      ivoz=0
      ires=0
      ifh=0
      idis=0
      icow=0
      istw=0
      iffw=0
      irip=0
      irco=0
      idial=0
      nord=0
      nvar=0
      nvar2=0
      ndimf=0
      nordf=0
      nvarf=0
      nord1=1
      nsix=0
      nvar2=0
      ncor=0
      idptr=0
      nbeam=0
      ibb6d=0
      ibeco=1
      ibtyp=0
      lhc=1
      ibbc=0
      iver=0
      ibidu=0
!-----------------------------------------------------------------------
      inorm=0
      imod1=0
      imod2=0
!-----------------------------------------------------------------------
      icorr=0
      nctype=0
      namp=0
      nmom=0
      nmom1=0
      nmom2=0
      weig1=zero
      weig2=zero
      dpmax=zero
!--DA-------------------------------------------------------------------
      dpda_da=0
      dpda1_da=0
      sigmda_da=0
      ej1_da=0
      ejf1_da=0
      rv_da=0
!-----------------------------------------------------------------------
      pi=zero
      pi2=zero
      pisqrt=zero
      rad=zero
      chi0=zero
      chid=zero
      dp1=zero
      idfor=0
      rat=zero
      qs=zero
      e0=zero
      crad=zero
      dppoff=zero
      tlen=zero
      pma=zero
      phas0=zero
      phas=zero
      ition=0
      dpscor=one
      sigcor=one
      benki=zero
      dma=zero
      dmap=zero
      dkq=zero
      dqq=zero
      de0=zero
      ded=zero
      dsi=zero
      dech=zero
      dsm0=zero
      amp0=zero
      qxt=zero
      qzt=zero
      eui=zero
      euii=zero
      tam1=zero
      tam2=zero
      totl=zero
      dphix=zero
      dphiz=zero
      qx0=zero
      qz0=zero
      dres=zero
      dfft=zero
      preda=zero
      partnum=zero
      emitx=zero
      emity=zero
      emitz=zero
      gammar=one
      sigz=zero
      sige=zero
      damp=zero
      ampt=zero
!-----------------------------------------------------------------------
      tlim=0.
      time0=0.
      time1=0.
!-----------------------------------------------------------------------
      do 10 i=1,2
        nde(i)=0
        is(i)=0
        idz(i)=0
        amp(i)=zero
        bet0(i)=zero
        alf0(i)=zero
        clo(i)=zero
        clop(i)=zero
        aper(i)=c1e3
        di0(i)=zero
        dip0(i)=zero
        cro(i)=zero
        sigma0(i)=zero
        qwsk(i)=zero
        betx(i)=zero
        betz(i)=zero
        alfx(i)=zero
        alfz(i)=zero
   10 continue
      do 20 i=1,3
        iq(i)=0
        hsy(i)=zero
        qw0(i)=zero
        clo6(i)=zero
        clop6(i)=zero
        clon(i)=zero
        wxys(i)=zero
        do i1=1,3
          corr(i,i1)=zero
        enddo
   20 continue
      corr(1,1)=zero
      corr(1,2)=zero
      chromc(1)=9.999999d23
      chromc(2)=9.999999d23
      do 30 i=1,4
        nwr(i)=0
   30 continue
      do 40 i=1,5
        ipr(i)=0
        nrr(i)=0
        nu(i)=0
        toptit(i)=' '
   40 continue
      do 50 i=1,6
        nskew(i)=0
   50 continue
      do 60 i=1,10
        dtr(i)=zero
        coel(i)=' '
   60 continue
      do 70 i=1,12
        ire(i)=0
   70 continue
      do 80 i=1,nper
        msym(i)=0
   80 continue
      do 90 i=1,6
        do 90 j=1,6
          ta(i,j)=zero
   90 continue
      do 100 i=1,2
        do 100 j=1,6
          exz(i,j)=zero
  100 continue
      do 110 i1=1,9
        do 110 i2=1,18
          do 110 i3=1,10
            do 110 i4=1,5
              rtc(i1,i2,i3,i4)=zero
              rts(i1,i2,i3,i4)=zero
  110 continue
!--NUMBER OF ELEMENTS---------------------------------------------------
      do 150 i=1,nele
        kz(i)=0
        kp(i)=0
        irm(i)=0
        imtr(i)=0
        nmu(i)=0
        kpa(i)=0
        isea(i)=0
        nrel(i)=0
        ncororb(i)=0
        iratioe(i)=0
        itionc(i)=0
        dki(i,1)=zero
        dki(i,2)=zero
        dki(i,3)=zero
        ed(i)=zero
        el(i)=zero
        ek(i)=zero
        sm(i)=zero
        xpl(i)=zero
        xrms(i)=zero
        zpl(i)=zero
        zrms(i)=zero
        benkc(i)=zero
        r00(i)=zero
        apx(i)=c1e3
        apz(i)=c1e3
        ape(1,i)=c1e6
        ape(2,i)=c1e6
        ape(3,i)=c1e12
        ramp(i)=zero
        rfre(i)=zero
        rzph(i)=zero
        ratioe(i)=one
        hsyc(i)=zero
        phasc(i)=zero
        ptnfac(i)=zero
        wirel(i)=zero
        acdipph(i)=zero
        bez(i)=' '
        bezl(i)=' '
        do 120 i3=1,2
          do 120 i4=1,6
            a(i,i3,i4)=zero
              do 120 i1=1,npart
                al(i4,i3,i1,i)=zero
                as(i4,i3,i1,i)=zero
  120   continue
        do 130 i1=1,mmul
          bk0(i,i1)=zero
          ak0(i,i1)=zero
          bka(i,i1)=zero
          aka(i,i1)=zero
  130   continue
        do 140 i1=1,3
          bezr(i1,i)=' '
  140   continue
        do i1=1,4
          parbe(i,i1)=zero
        enddo
  150 continue
!--NUMBER OF BLOCKS-----------------------------------------------------
      do 180 i=1,nblo
        mel(i)=0
        mstr(i)=0
        elbe(i)=zero
        bezb(i)=' '
        do 160 i1=1,2
          do 160 i2=1,6
            bl1(i,i1,i2)=zero
            bl2(i,i1,i2)=zero
  160   continue
        do 170 j=1,nelb
          mtyp(i,j)=0
  170   continue
  180 continue
!--# OF STRUCTURE ELEMENTS----------------------------------------------
      do 190 i=1,nblz
        ic(i)=0
        mzu(i)=0
        icext(i)=0
        icextal(i)=0
        extalign(i,1)=zero
        extalign(i,2)=zero
        extalign(i,3)=zero
        sigmoff(i)=zero
        tiltc(i)=one
        tilts(i)=zero
!--Beam-Beam------------------------------------------------------------
        imbb(i)=0
        do 190 j=1,40
          exterr(i,j)=zero
        xsi(i)=zero
        zsi(i)=zero
        smi(i)=zero
        smizf(i)=zero
        rsmi(i)=zero
        rfres(i)=zero
        rzphs(i)=zero
        do i1=1,mmul
          aai(i,i1)=zero
          bbi(i,i1)=zero
        enddo
  190 continue
!--RANDOM NUMBERS-------------------------------------------------------
      do 200 i=1,nzfz
        zfz(i)=zero
  200 continue
!--# OF TRAJECTORIES----------------------------------------------------
      do 220 i=1,mpa
        rvf(i)=one
        sigm(i)=zero
        dps(i)=zero
        ej(i)=zero
        ejf(i)=zero
        do 210 i1=1,2
          x(i,i1)=zero
          y(i,i1)=zero
  210   continue
  220 continue
!--COMBINATION OF ELEMENTS----------------------------------------------
      do 240 i1=1,20
        icomb0(i1)=0
        do 230 i=1,ncom
          icomb(i,i1)=0
          ratio(i,i1)=zero
  230   continue
  240 continue
!--PAW------------------------------------------------------------------
      do 250 i=1,nplo
        hmal(i)=0.0
  250 continue
!--TROMBONES------------------------------------------------------------
      do i=1,ntr
        do i1=1,6
          cotr(i,i1)=zero
          do i2=1,6
            rrtr(i,i1,i2)=zero
          enddo
        enddo
      enddo
!--Beam-Beam------------------------------------------------------------
      do i=1,nbb
        do j=1,2
          sigman(j,i)=zero
          sigman2(j,i)=zero
          sigmanq(j,i)=zero
        enddo
        do j=1,6
          clobeam(j,i)=zero
          beamoff(j,i)=zero
        enddo
        do j=1,12
          bbcu(i,j)=zero
        enddo
        bbcu(i,11)=one
      enddo
!--DA-------------------------------------------------------------------
      do i1=1,2
        xx_da(i1)=0
        yy_da(i1)=0
        do i2=1,6
          alda_da(i1,i2)=0
          asda_da(i1,i2)=0
          aldaq_da(i1,i2)=0
          asdaq_da(i1,i2)=0
        enddo
      enddo
      do i=1,mcor
        smida_da(i)=0
      enddo
!-----------------------------------------------------------------------
      return
      end
      subroutine distance(x,clo,di0,t,dam)
!-----------------------------------------------------------------------
!  CALCULATION OF DISTANCE IN PHASE SPACE FOR POST-PROCESSING
!-----------------------------------------------------------------------
      implicit none
      integer i,ii,iq,j,jq
      double precision clo,cx,dam,di0,phi,pi,sx,t,x,x1
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
      integer icode,idam,its6d
      double precision dpscor,sigcor
      common/corcom/dpscor,sigcor,icode,idam,its6d
      dimension x(2,6),x1(2,6),clo(6),di0(4),t(6,6),phi(3)
      save
!-----------------------------------------------------------------------
      pi=four*atan(one)
      if(icode.ge.4.and.its6d.eq.0) then
        do 10 i=1,2
          do 10 j=1,4
            x(i,j)=x(i,j)-di0(j)*x(i,6)
   10   continue
      endif
      do 60 i=1,2
        do 20 j=1,6
          x(i,j)=x(i,j)-clo(j)
   20   continue
        if(its6d.eq.1) then
          x(i,2)=x(i,2)/(one+x(i,6)+clo(6))
          x(i,4)=x(i,4)/(one+x(i,6)+clo(6))
        endif
        do 40 iq=1,6
          x1(i,iq)=zero
          do 30 jq=1,6
            x1(i,iq)=x1(i,iq)+t(jq,iq)*x(i,jq)
   30     continue
   40   continue
        do 50 j=1,6
          x(i,j)=x1(i,j)
   50   continue
   60 continue
      do 70 i=1,2
        x(i,5)=x(i,5)*sigcor
        x(i,6)=x(i,6)*dpscor
   70 continue
      do 80 i=1,3
        ii=2*i
        sx=x(2,ii-1)*x(1,ii)-x(1,ii-1)*x(2,ii)
        cx=x(1,ii-1)*x(2,ii-1)+x(1,ii)*x(2,ii)
        if(abs(sx).gt.c1m15.or.abs(cx).gt.c1m15) then
          phi(i)=atan2(sx,cx)
        else
          phi(i)=zero
        endif
   80 continue
      dam=sqrt((phi(1)*phi(1)+phi(2)*phi(2)+phi(3)*phi(3))/idam)/pi
!-----------------------------------------------------------------------
      return
      end
      subroutine betalf(dpp,qw)
!-----------------------------------------------------------------------
!  CALCULATION OF : OPT. PARAMETERS AT THE STARTING POSITION:
!                   BETA-, ALFA-FUNCTIONS, Q-VALUES
!-----------------------------------------------------------------------
      implicit none
      integer i,j
      double precision am,det,detb,detc,dpp,egwg1,egwg2,f0,f1,f2,fak1,  &
     &fak2,qw,rca1,rca2,rclam1,rclam2,rcw1(4),rcw2(4),rn1,rn2,spa,spd,  &
     &sqrn,yca1,yca2,yclam1,yclam2,ycw1(4),ycw2(4)
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
      dimension am(4,4)
      dimension qw(2)
      save
!-----------------------------------------------------------------------
      ierro=0
      call matrix(dpp,am)
!--CALCULATION OF EIGENVALUES
   10 spa=am(1,1)+am(2,2)
      spd=am(3,3)+am(4,4)
      det=(am(1,3)+am(4,2))*(am(2,4)+am(3,1))                           &
     &-(am(1,4)-am(3,2))*(am(2,3)-am(4,1))
      f0=spa-spd
      f1=spa+spd
      f2=f0*f0+four*det
      if(f2 .lt. zero) goto 160
      f2=sqrt(f2)
      if(f0.lt.0) goto 30
      if(f0.ge.0) goto 20
   20 egwg1=(f1+f2)*half
      egwg2=(f1-f2)*half
      goto 40
   30 egwg1=(f1-f2)*half
      egwg2=(f1+f2)*half
   40 continue
      f1=egwg1*egwg1-four
      f2=egwg2*egwg2-four
      rca1=f1
      yca1=zero
      rca2=f2
      yca2=zero
      if (rca1.ge.0) then
        rca1=sqrt(rca1)
      else
        yca1=sqrt(-rca1)
        rca1=zero
      endif
      if (rca2.ge.0) then
        rca2=sqrt(rca2)
      else
        yca2=sqrt(-rca2)
        rca2=zero
      endif
      rclam1=(egwg1+rca1)*half
      yclam1=yca1*half
      rclam2=(egwg2+rca2)*half
      yclam2=yca2*half
      if(egwg1*egwg1 .ge. four) goto 160
      if(egwg2*egwg2 .ge. four) goto 160
   50 continue
      detb=am(1,3)*am(2,4)-am(1,4)*am(2,3)
      detc=am(3,1)*am(4,2)-am(3,2)*am(4,1)
      fak1=spd-egwg1
      if(abs(fak1).gt.pieni) then
        rcw1(1)=-(am(1,3)*am(3,2)+am(1,4)*am(4,2))/fak1+am(1,2)
        ycw1(1)=zero
        rcw1(2)=(am(1,3)*am(3,1)+am(1,4)*am(4,1)+detb)/fak1-(am(1,1)    &
     &-rclam1)
        ycw1(2)=yclam1
        rcw1(3)=-((am(3,1)+am(2,4))*rcw1(1)+(am(3,2)-am(1,4))*rcw1(2))  &
     &/fak1
        ycw1(3)=-((am(3,1)+am(2,4))*ycw1(1)+(am(3,2)-am(1,4))*ycw1(2))  &
     &/fak1
        rcw1(4)=-((am(4,1)-am(2,3))*rcw1(1)+(am(4,2)+am(1,3))*rcw1(2))  &
     &/fak1
        ycw1(4)=-((am(4,1)-am(2,3))*ycw1(1)+(am(4,2)+am(1,3))*ycw1(2))  &
     &/fak1
      else
        rcw1(1)=am(1,2)
        ycw1(1)=zero
        rcw1(2)=-am(1,1)+rclam1
        ycw1(2)=yclam1
        rcw1(3)=zero
        ycw1(3)=zero
        rcw1(4)=zero
        ycw1(4)=zero
      endif
      fak2=spa-egwg2
      if(abs(fak2).gt.pieni) then
        rcw2(3)=-(am(3,1)*am(1,4)+am(3,2)*am(2,4))/fak2+am(3,4)
        ycw2(3)=zero
        rcw2(4)=(am(3,1)*am(1,3)+am(3,2)*am(2,3)+detc)/fak2-(am(3,3)    &
     &-rclam2)
        ycw2(4)=yclam2
        rcw2(1)=-((am(1,3)+am(4,2))*rcw2(3)+(am(1,4)-am(3,2))*rcw2(4))  &
     &/fak2
        ycw2(1)=-((am(1,3)+am(4,2))*ycw2(3)+(am(1,4)-am(3,2))*ycw2(4))  &
     &/fak2
        rcw2(2)=-((am(2,3)-am(4,1))*rcw2(3)+(am(2,4)+am(3,1))*rcw2(4))  &
     &/fak2
        ycw2(2)=-((am(2,3)-am(4,1))*ycw2(3)+(am(2,4)+am(3,1))*ycw2(4))  &
     &/fak2
      else
        rcw2(3)=am(3,4)
        ycw2(3)=zero
        rcw2(4)=-am(3,3)+rclam2
        ycw2(4)=yclam2
        rcw2(1)=zero
        ycw2(1)=zero
        rcw2(2)=zero
        ycw2(2)=zero
      endif
!--LEAVING COMPLEX NUMBERS
      do 60 i=1,4
        ta(i,1)=rcw1(i)
        ta(i,3)=rcw2(i)
        ta(i,2)=ycw1(i)
        ta(i,4)=ycw2(i)
   60 continue
!--NORMALISATION OF EIGENVALUES
      rn1=ta(1,1)*ta(2,2)-ta(2,1)*ta(1,2)                               &
     &+ta(3,1)*ta(4,2)-ta(4,1)*ta(3,2)
      if(rn1.lt.0) goto 70
      if(rn1.eq.0) goto 160
      if(rn1.gt.0) goto 90
   70 yclam1=-yclam1
      do 80 i=1,4
   80 ta(i,2)=-ta(i,2)
   90 sqrn=sqrt(abs(rn1))
      do 100 i=1,4
        ta(i,1)=ta(i,1)/sqrn
  100 ta(i,2)=ta(i,2)/sqrn
      rn2=ta(1,3)*ta(2,4)-ta(2,3)*ta(1,4)                               &
     &+ta(3,3)*ta(4,4)-ta(4,3)*ta(3,4)
      if(rn2.lt.0) goto 110
      if(rn2.eq.0) goto 160
      if(rn2.gt.0) goto 130
  110 yclam2=-yclam2
      do 120 i=1,4
  120 ta(i,4)=-ta(i,4)
  130 sqrn=sqrt(abs(rn2))
      do 140 i=1,4
        ta(i,3)=ta(i,3)/sqrn
  140 ta(i,4)=ta(i,4)/sqrn
      qw(1)= atan(yclam1/(one+rclam1))/pi
      qw(2)= atan(yclam2/(one+rclam2))/pi
!-----------------------------------------------------------------------
!  OPTICAL PARAMETERS AT THE STARTING POINT
!-----------------------------------------------------------------------
      betx(1)=ta(1,1)*ta(1,1)+ta(1,2)*ta(1,2)
      alfx(1)=-(ta(1,1)*ta(2,1)+ta(1,2)*ta(2,2))
      betx(2)=ta(1,3)*ta(1,3)+ta(1,4)*ta(1,4)
      alfx(2)=-(ta(1,3)*ta(2,3)+ta(1,4)*ta(2,4))
      betz(1)=ta(3,1)*ta(3,1)+ta(3,2)*ta(3,2)
      alfz(1)=-(ta(3,1)*ta(4,1)+ta(3,2)*ta(4,2))
      betz(2)=ta(3,3)*ta(3,3)+ta(3,4)*ta(3,4)
      alfz(2)=-(ta(3,3)*ta(4,3)+ta(3,4)*ta(4,4))
      bet0(1)=betx(1)
      alf0(1)=alfx(1)
      bet0(2)=betz(2)
      alf0(2)=alfz(2)
      if(ta(1,1).lt.-pieni) then
        do 150 i=1,4
          do 150 j=1,4
            ta(i,j)=-ta(i,j)
  150   continue
      endif
      return
!-----------------------------------------------------------------------
  160 ierro=1
      return
      end
      subroutine block
!-----------------------------------------------------------------------
!  COMBINATION OF LINEAR ELEMENTS TO ONE MATRIX
!-----------------------------------------------------------------------
      implicit none
      integer i,j,jm,k,l,m,n
      double precision g,h
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
      dimension h(nblo,2,6),g(nblo,2,6)
      save
!-----------------------------------------------------------------------
      do 60 k=1,mblo
        jm=mel(k)
        i=mtyp(k,1)
        n=mtyp(k,jm)
        do 10 l=1,2
          do 10 m=1,6
            h(1,l,m)=a(i,l,m)
   10   g(1,l,m)=a(n,l,m)
        if(jm.eq.1) goto 40
        do 30 j=2,jm
          i=mtyp(k,j)
          n=mtyp(k,jm-j+1)
          do 20 l=1,2
            h(j,l,1)=h(j-1,l,1)*a(i,l,1)+h(j-1,l,3)*a(i,l,2)
            h(j,l,2)=h(j-1,l,2)*a(i,l,1)+h(j-1,l,4)*a(i,l,2)
            h(j,l,3)=h(j-1,l,1)*a(i,l,3)+h(j-1,l,3)*a(i,l,4)
            h(j,l,4)=h(j-1,l,2)*a(i,l,3)+h(j-1,l,4)*a(i,l,4)
            g(j,l,1)=g(j-1,l,1)*a(n,l,1)+g(j-1,l,3)*a(n,l,2)
            g(j,l,2)=g(j-1,l,2)*a(n,l,1)+g(j-1,l,4)*a(n,l,2)
            g(j,l,3)=g(j-1,l,1)*a(n,l,3)+g(j-1,l,3)*a(n,l,4)
            g(j,l,4)=g(j-1,l,2)*a(n,l,3)+g(j-1,l,4)*a(n,l,4)
            h(j,l,5)=h(j-1,l,5)*a(i,l,1)+h(j-1,l,6)*a(i,l,2)+a(i,l,5)
            h(j,l,6)=h(j-1,l,5)*a(i,l,3)+h(j-1,l,6)*a(i,l,4)+a(i,l,6)
            g(j,l,5)=g(j-1,l,5)*a(n,l,1)+g(j-1,l,6)*a(n,l,2)+a(n,l,5)
            g(j,l,6)=g(j-1,l,5)*a(n,l,3)+g(j-1,l,6)*a(n,l,4)+a(n,l,6)
   20     continue
   30   continue
   40   do 50 l=1,2
          do 50 m=1,6
            bl1(k,l,m)=h(jm,l,m)
   50   bl2(k,l,m)=g(jm,l,m)
   60 continue
      return
      end
      subroutine blockdis(aeg,bl1eg,bl2eg)
!-----------------------------------------------------------------------
!  COMBINATION OF LINEAR ELEMENTS TO ONE MATRIX, USED FOR DISPERSION
!-----------------------------------------------------------------------
      implicit none
      integer i,j,jm,k,l,m,n
      double precision aeg,bl1eg,bl2eg,g,h
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
      dimension h(nblo,2,6),g(nblo,2,6)
      dimension aeg(nele,2,6),bl1eg(nblo,2,6),bl2eg(nblo,2,6)
      save
!-----------------------------------------------------------------------
      do 60 k=1,mblo
        jm=mel(k)
        i=mtyp(k,1)
        n=mtyp(k,jm)
        do 10 l=1,2
          do 10 m=1,6
            h(1,l,m)=aeg(i,l,m)
   10   g(1,l,m)=aeg(n,l,m)
        if(jm.eq.1) goto 40
        do 30 j=2,jm
          i=mtyp(k,j)
          n=mtyp(k,jm-j+1)
          do 20 l=1,2
            h(j,l,1)=h(j-1,l,1)*aeg(i,l,1)+h(j-1,l,3)*aeg(i,l,2)
            h(j,l,2)=h(j-1,l,2)*aeg(i,l,1)+h(j-1,l,4)*aeg(i,l,2)
            h(j,l,3)=h(j-1,l,1)*aeg(i,l,3)+h(j-1,l,3)*aeg(i,l,4)
            h(j,l,4)=h(j-1,l,2)*aeg(i,l,3)+h(j-1,l,4)*aeg(i,l,4)
            g(j,l,1)=g(j-1,l,1)*aeg(n,l,1)+g(j-1,l,3)*aeg(n,l,2)
            g(j,l,2)=g(j-1,l,2)*aeg(n,l,1)+g(j-1,l,4)*aeg(n,l,2)
            g(j,l,3)=g(j-1,l,1)*aeg(n,l,3)+g(j-1,l,3)*aeg(n,l,4)
            g(j,l,4)=g(j-1,l,2)*aeg(n,l,3)+g(j-1,l,4)*aeg(n,l,4)
            h(j,l,5)=h(j-1,l,5)*aeg(i,l,1)+h(j-1,l,6)*aeg(i,l,2)+aeg    &
     &(i,l,5)
            h(j,l,6)=h(j-1,l,5)*aeg(i,l,3)+h(j-1,l,6)*aeg(i,l,4)+aeg    &
     &(i,l,6)
            g(j,l,5)=g(j-1,l,5)*aeg(n,l,1)+g(j-1,l,6)*aeg(n,l,2)+aeg    &
     &(n,l,5)
            g(j,l,6)=g(j-1,l,5)*aeg(n,l,3)+g(j-1,l,6)*aeg(n,l,4)+aeg    &
     &(n,l,6)
   20     continue
   30   continue
   40   do 50 l=1,2
          do 50 m=1,6
            bl1eg(k,l,m)=h(jm,l,m)
   50   bl2eg(k,l,m)=g(jm,l,m)
   60 continue
      return
      end
      subroutine chroma
!-----------------------------------------------------------------------
!  CALCULATION OF CHROMATICITY FROM 5 ENERGIE-VALUES
!-----------------------------------------------------------------------
      implicit none
      integer i,ii,isl,j,jj,l,n
      double precision cor,coro,cro0,de2,det,dm,dpp,dsm,ox,oz,qwc,sens, &
     &sm0,su2,suxy,suzy,xi,zi
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
      integer ichromc,ilinc,iqmodc
      double precision clon,chromc,corr,wxys
      common/correct/ corr(3,3),chromc(2),wxys(3),clon(6),iqmodc,       &
     &ichromc,ilinc
      dimension dsm(2,4),sens(2,4),xi(2),zi(2),dm(2),sm0(2)
      dimension qwc(3),cro0(2)
      save
!-----------------------------------------------------------------------
      cor=0d0
      coro=1d38
      do 10 i=1,2
        do 10 j=1,4
          dsm(i,j)=zero
          sens(i,j)=zero
   10 continue
      do 20 i=1,2
        xi(i)=zero
        zi(i)=zero
        dm(i)=zero
        sm0(i)=zero
        qwc(i)=zero
        cro0(i)=zero
   20 continue
      qwc(3)=zero
      write(*,10010)
      dsm(1,2)=dsm0
      dsm(2,3)=dsm0
      de2=de0*half
      do 90 jj=1,itcro
        do 80 ii=1,4
          su2=zero
          suxy=zero
          suzy=zero
          do 30 l=1,2
            isl=is(l)
            if(kz(isl).ne.3) call prror(11)
            ed(isl)=ed(isl)+dsm(l,ii)
            if(kp(isl).eq.5) call combel(isl)
   30     continue
          do 40 n=1,5
            dpp=de2*(3-n)
            call clorb(dpp)
            if(ierro.gt.0) call prror(12)
            call phasad(dpp,qwc)
            if(ierro.gt.0) call prror(13)
            ox=qwc(1)
            oz=qwc(2)
            su2=su2+dpp*dpp
            suxy=suxy+ox*dpp
            suzy=suzy+oz*dpp
   40     continue
          do 50 l=1,2
            isl=is(l)
            ed(isl)=ed(isl)-dsm(l,ii)
            if(kp(isl).eq.5) call combel(isl)
   50     continue
          sens(1,ii)=suxy/su2
          sens(2,ii)=suzy/su2
          if(ii.ne.3) goto 80
!--COMPENSATION OF CHROMATICITY
          do 60 l=1,2
            cro0(l)=sens(l,1)-cro(l)
            xi(l)=(sens(1,l+1)-sens(1,1))/dsm0
   60     zi(l)=(sens(2,l+1)-sens(2,1))/dsm0
          cor=sqrt(cro0(1)*cro0(1)+cro0(2)*cro0(2))
          if(jj.eq.1.or.cor.lt.coro) then
            coro=cor
            det=xi(1)*zi(2)-zi(1)*xi(2)
            dm(1)=(-cro0(1)*zi(2)+cro0(2)*xi(2))/det
            dm(2)=(-cro0(2)*xi(1)+cro0(1)*zi(1))/det
            do 70 l=1,2
              sm0(l)=ed(is(l))
              isl=is(l)
              ed(isl)=ed(isl)+dm(l)
              if(kp(isl).eq.5) call combel(isl)
 70         continue
          else
            write(*,10035)
            return
          endif
   80   continue
        write(*,10020) sens(1,1),sens(1,4),sens(2,1),sens(2,4)
        chromc(1)=sens(1,4)*c1m3
        chromc(2)=sens(2,4)*c1m3
        write(*,10030) sm0(1),ed(is(1)),bez(is(1)), sm0(2),ed(is(2)),bez&
     &(is(2))
        write(*,10040) xi,zi
        write(*,10010)
        if(abs(sens(1,4)-cro(1)).lt.dech.and.abs(sens(2,4)-cro(2))      &
     &.lt.dech) return
   90 continue
      write(*,10000) itcro
!-----------------------------------------------------------------------
      return
10000 format(/131('-')//t10,'CHROMATICITY CORRECTION'/t10,              &
     &'MAXIMUM NUMBER OF ITERATIONS ACHIEVED--->',2x,i4/ t10,           &
     &'PROCEDURE MAY NOT HAVE CONVERGED')
10010 format(/131('-'))
10020 format(/131('-')//t10,'DATA BLOCK CHROMATICITY CORRECTION'/t10,   &
     &'CHROMATICITIES         BEFORE           AFTER CORRECTION'/t10,   &
     &'HORIZONTAL       ',d16.10,7x,d16.10/ t10,'VERTICAL         ',d16.&
     &10,7x,d16.10/)
10040 format(t10,'SEXTUPOLE SENSITIVITIES    XI/M1 XI/M2 YI/M1 YI/M2  ',&
     &4d15.8)
10030 format(t10,'SEXTUP.STRENGTHS ',g16.10,7x,g16.10,'   INDEX   ',a16/&
     &t10,'                 ',g16.10,7x,g16.10,'           ',a16)
10035 format(/t5,'---- NO Improvement in last Step ----'/)
      end
      subroutine chromda
!-----------------------------------------------------------------------
!  CHROMATICITY CORRECTION VIA DA
!-----------------------------------------------------------------------
      implicit none
      integer icht,iq1,iq2,ix,ncorr,ncorruo,nd,nd2
      double precision cor,coro,dps0,dq1,dq2,edcor1,edcor2,qw,qwc
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
      double precision aml6,edcor
      common/sixdim/aml6(6,6),edcor(2)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      integer ichromc,ilinc,iqmodc
      double precision clon,chromc,corr,wxys
      common/correct/ corr(3,3),chromc(2),wxys(3),clon(6),iqmodc,       &
     &ichromc,ilinc
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      dimension qw(2),qwc(3)
      save
!-----------------------------------------------------------------------
      write(*,10000)
      nd=2
      nd2=4
      dps(1)=dp1+dppoff
      ncorruo=ncorru
      ncorru=1
      call clorb(dp1)
      call betalf(dp1,qw)
      call phasad(dp1,qwc)
      if(nbeam.ge.1) then
              dps0=dps(1)
              dps(1)=zero
              iqmodc=4
              call mydaini(1,2,nd2,nd,nd2,1)
              ilinc=2
              call mydaini(2,2,nd2,nd,nd2,1)
              dps(1)=dps0
      endif
      ncorru=ncorruo
      iq1=is(1)
      iq2=is(2)
      edcor(1)=ed(iq1)
      edcor(2)=ed(iq2)
      edcor1=edcor(1)
      edcor2=edcor(2)
      coro=1d38
      cor=0
      ncorr=0
      do ncorr=1,itcro+1
        ichromc=2
        call mydaini(1,1,nd2,nd,nd2,1)
        ichromc=1
        call mydaini(2,4,7,2,5,1)
        dq1=corr(1,1)-cro(1)*c1m3
        dq2=corr(1,2)-cro(2)*c1m3
        if(ncorr.eq.1) cor=c1e3*sqrt(dq1*dq1+dq2*dq2)
        if(cor.gt.dech) then
          cor=c1e3*sqrt(dq1*dq1+dq2*dq2)
          if(ncorr.eq.1.or.cor.lt.coro) then
            coro=cor
            ed(iq1)=ed(iq1)-corr(2,1)*dq1-corr(2,2)*dq2
            ed(iq2)=ed(iq2)-corr(3,1)*dq1-corr(3,2)*dq2
            do icht=1,iu
              ix=ic(icht)
              if(ix.gt.nblo) then
                ix=ix-nblo
                if(ix.eq.iq1.or.iratioe(ix).eq.iq1) then
                  smi(icht)=ed(iq1)*ratioe(ix)+smizf(icht)
                else if(ix.eq.iq2.or.iratioe(ix).eq.iq2) then
                  smi(icht)=ed(iq2)*ratioe(ix)+smizf(icht)
                endif
              endif
            enddo
            edcor(1)=ed(iq1)
            edcor(2)=ed(iq2)
            if(ncorr.eq.1) then
              write(*,10010) cro(1),corr(1,1)*c1e3,cro(2),              &
     &corr(1,2)*c1e3,ncorr-1,cor
              write(*,10030) edcor1,ed(iq1),bez(iq1),edcor2,ed(iq2),    &
     &bez(iq2)
            else
              write(*,10020) cro(1),corr(1,1)*c1e3,cro(2),              &
     &corr(1,2)*c1e3,ncorr-1,cor
              write(*,10030) edcor1,ed(iq1),bez(iq1),edcor2,ed(iq2),    &
     &bez(iq2)
            endif
          else
            write(*,10040) ncorr-1
            goto 1
          endif
        else
          write(*,10050) ncorr-1
          goto 1
        endif
      enddo
 1    continue
      chromc(1)=corr(1,1)
      chromc(2)=corr(1,2)
      if(ncorr.eq.itcro+1) write(*,10060) itcro
      if(ncorr.eq.1) then
        write(*,10010) cro(1),corr(1,1)*c1e3,cro(2),                    &
     &corr(1,2)*c1e3,ncorr-1,cor
      else
        write(*,10020) cro(1),corr(1,1)*c1e3,cro(2),corr(1,2)*c1e3,     &
     &ncorr-1,cor
      endif
      write(*,10030) edcor1,ed(iq1),bez(iq1),edcor2,ed(iq2),bez(iq2)
!-----------------------------------------------------------------------
10000 format(/131('-')/t10,'ENTERING DA CHROMATICITY CORRECTION'/)
10010 format(/131('-')/t10,                                             &
     &'CHROMATICITY'   ,18x,'THEORET.        BEFORE CORRECTION'/ t10,   &
     &'HORIZONTAL'     ,15x,g20.14,1x,g20.14/ t10,                      &
     &'VERTICAL'       ,17x,g20.14,1x,g20.14// t10,                     &
     &'ITERATION:'     ,21x,i3/ t10,                                    &
     &'ACCURACY:'      ,17x,g16.10/)
10020 format(/131('-')/t10,                                             &
     &'CHROMATICITY'   ,18x,'THEORET.        AFTER CORRECTION'/ t10,    &
     &'HORIZONTAL'     ,15x,g20.14,1x,g20.14/ t10,                      &
     &'VERTICAL'       ,17x,g20.14,1x,g20.14// t10,                     &
     &'ITERATION:'     ,21x,i3/ t10,                                    &
     &'ACCURACY:'      ,17x,g16.10/)
10030 format(t10,'SEXTUPOLE STRENGTH',5x,g16.10,2x,g16.10,'   TYP     ',&
     &a16/t10,                  23x,g16.10,2x,g16.10,'           ',     &
     &a16)
10040 format(/t5,'---- NO IMPROVEMENT OF DA CHROMATICITY CORRECTION ',  &
     &'IN ITERATION: ',i4/)
10050 format(t5/t10,'DA CHROMATICITY CORRECTION SUCCESSFUL IN ',        &
     &'ITERATION: ',i4/)
10060 format(/t10,'DA CHROMATICITY CORRECTION'/ t10,                    &
     &'MAXIMUM NUMBER OF ITERATIONS ACHIEVED--->',2x,i4/ t10,           &
     &'PROCEDURE MAY NOT HAVE CONVERGED')
      end
      subroutine clorb(dpp)
!-----------------------------------------------------------------------
!  CALCULATION OF THE CLOSED ORBIT   'CLO(2),CLOP(2)'
!-----------------------------------------------------------------------
      implicit none
      integer ierr,ii,l,ll
      double precision am,cor,dclo,dclop,dcx,dcxp,dcz,dczp,det,dpp,dx,  &
     &dy,x0,x1,y0,y1
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
      dimension x1(2),y1(2),x0(2),y0(2)
      dimension dclo(2),dclop(2)
      dimension dx(2),dy(2),am(4,4)
      save
!-----------------------------------------------------------------------
      ierro=0
      do 10 l=1,2
        clo(l)=dpp*di0(l)
        clop(l)=dpp*dip0(l)
        dx(l)=1e6
        dy(l)=1e6
   10 continue
      call envar(dpp)
      call umlauf(dpp,1,ierr)
      ierro=ierr
      if(ierro.ne.0) return
      do 40 ii=1,itco
        dcx=abs(dx(1))
        dcxp=abs(dy(1))
        dcz=abs(dx(2))
        dczp=abs(dy(2))
        if(dcx.le.dma.and.dcz.le.dma.and.dcxp.le.dmap.and.dczp.le.dmap) &
     &goto 50
        do 20 l=1,2
          x(1,l)=clo(l)
          y(1,l)=clop(l)
          x0(l)=x(1,l)
   20   y0(l)=y(1,l)
        call matrix(dpp,am)
        if(ierro.ne.0) return
        do 30 l=1,2
          ll=2*l
          x1(l)=x(1,l)
          y1(l)=y(1,l)
          det=two-am(ll-1,ll-1)-am(ll,ll)
          dx(l)=x0(l)-x1(l)
          dy(l)=y0(l)-y1(l)
          dclo(l)=(dx(l)*(am(ll,ll)-one)-dy(l)*am(ll-1,ll))/det
          dclop(l)=(dy(l)*(am(ll-1,ll-1)-one)-dx(l)*am(ll,ll-1))/det
          clo(l)=clo(l)+dclo(l)
          clop(l)=clop(l)+dclop(l)
   30   continue
   40 continue
      if(ncorru.ne.1) write(*,10000) itco
   50 cor=c1e3*sqrt(dcx*dcx+dcz*dcz)
      if(iout.eq.1.and.ncorru.ne.1) then
        write(*,10010) dpp,clo(1),clop(1),clo(2),clop(2),ii,cor
      endif
!-----------------------------------------------------------------------
      return
10000 format(t5/t10,'CLOSED ORBIT CALCULATION'/ t10,                    &
     &'MAXIMUM NUMBER OF ITERATIONS ACHIEVED--->',2x,i4/ t10,           &
     &'PROCEDURE MAY NOT HAVE CONVERGED')
10010 format(t5,'---- ENTRY CLORB ----/DPP=',f8.5,' /CLOX/', 2f10.5,    &
     &' /CLOY/',2f10.5,' /ITERAT.=',i3,'/ ACCURACY=',d13.6)
      end
      subroutine clorb2(dpp)
!-----------------------------------------------------------------------
!  CALCULATION OF THE CLOSED ORBIT - NO WRITEOUT
!-----------------------------------------------------------------------
      implicit none
      integer ierr,ii,l,ll
      double precision am,dclo,dclop,dcx,dcxp,dcz,dczp,det,dpp,dx,dy,x0,&
     &x1,y0,y1
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
      dimension x1(2),y1(2),x0(2),y0(2)
      dimension dclo(2),dclop(2)
      dimension dx(2),dy(2),am(4,4)
      save
!-----------------------------------------------------------------------
      ierro=0
      do 10 l=1,2
        clo(l)=dpp*di0(l)
        clop(l)=dpp*dip0(l)
        dx(l)=1e6
        dy(l)=1e6
   10 continue
      call envar(dpp)
      call umlauf(dpp,1,ierr)
      ierro=ierr
      if(ierro.ne.0) call prror(36)
      do 40 ii=1,itco
        dcx=abs(dx(1))
        dcxp=abs(dy(1))
        dcz=abs(dx(2))
        dczp=abs(dy(2))
        if(dcx.le.dma.and.dcz.le.dma.and.dcxp.le.dmap.and.dczp.le.dmap) &
     &return
        do 20 l=1,2
          x(1,l)=clo(l)
          y(1,l)=clop(l)
          x0(l)=x(1,l)
   20   y0(l)=y(1,l)
        call matrix(dpp,am)
        if(ierro.ne.0) call prror(36)
        do 30 l=1,2
          ll=2*l
          x1(l)=x(1,l)
          y1(l)=y(1,l)
          det=two-am(ll-1,ll-1)-am(ll,ll)
          dx(l)=x0(l)-x1(l)
          dy(l)=y0(l)-y1(l)
          dclo(l)=(dx(l)*(am(ll,ll)-one)-dy(l)*am(ll-1,ll))/det
          dclop(l)=(dy(l)*(am(ll-1,ll-1)-one)-dx(l)*am(ll,ll-1))/det
          clo(l)=clo(l)+dclo(l)
          clop(l)=clop(l)+dclop(l)
   30   continue
   40 continue
!-----------------------------------------------------------------------
      return
      end
      subroutine combel(iql)
!-----------------------------------------------------------------------
!  COMBINATION OF ELEMENTS
!-----------------------------------------------------------------------
      implicit none
      integer ico,ico0,iql,j,m
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
      save
!-----------------------------------------------------------------------
      do 20 j=1,icoe
        ico0=icomb0(j)
        if(iql.ne.ico0) goto 20
        do 10 m=1,20
          ico=icomb(j,m)
          if(ico.eq.0) goto 10
          if(kz(ico0).ne.kz(ico)) call prror(14)
          if(abs(el(ico0)).gt.pieni) then
            if(abs(el(ico)).gt.pieni) then
              ek(ico)=ek(ico0)*ratio(j,m)
            else
              ed(ico)=ek(ico0)*ratio(j,m)
            endif
          endif
          if(abs(el(ico0)).le.pieni) then
            if(abs(el(ico)).le.pieni) then
              ed(ico)=ed(ico0)*ratio(j,m)
            else
              ek(ico)=ed(ico0)*ratio(j,m)
            endif
          endif
   10   continue
   20 continue
!-----------------------------------------------------------------------
      return
      end
      subroutine envar(dpp)
!-----------------------------------------------------------------------
!  CALCULATION OF ELEMENT MATRICES
!-----------------------------------------------------------------------
      implicit none
      integer i,ih,kz1,l,ll
      double precision afok,co,dpd,dpp,dpsq,fi,fok,fokq,g,gl,hc,hi,hi1, &
     &hm,hp,hs,rho,rhoi,si,wf
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
      save
!-----------------------------------------------------------------------
      dpd=one+dpp
      dpsq=sqrt(dpd)
      do 200 i=1,il
        do ll=1,6
          do l=1,2
            a(i,l,ll)=zero
          enddo
        enddo
        if(abs(el(i)).le.pieni) goto 190
        kz1=kz(i)+1
        goto(10,30,90,50,70,80,120,170,180),kz1
        goto 200
!-----------------------------------------------------------------------
!  DRIFTLENGTH
!-----------------------------------------------------------------------
   10   do 20 l=1,2
          a(i,l,1)=one
          a(i,l,2)=el(i)
          a(i,l,3)=zero
   20   a(i,l,4)=one
        goto 200
!-----------------------------------------------------------------------
!  RECTANGULAR MAGNET
!  HORIZONTAL
!-----------------------------------------------------------------------
   30   ih=1
   40   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        si=sin(fok)
        co=cos(fok)
        a(i,ih,1)=one
        a(i,ih,2)=rho*si
        a(i,ih,3)=zero
        a(i,ih,4)=one
        a(i,ih,5)=-rho*(one-co)/dpsq
        a(i,ih,6)=-two*tan(fok*half)/dpsq
!--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        g=tan(fok*half)/rho
        gl=el(i)*g
        a(i,ih,1)=one-gl
        a(i,ih,2)=el(i)
        a(i,ih,3)=-g*(two-gl)
        a(i,ih,4)=a(i,ih,1)
        goto 200
!-----------------------------------------------------------------------
!  SEKTORMAGNET
!  HORIZONTAL
!-----------------------------------------------------------------------
   50   ih=1
   60   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        si=sin(fok)
        co=cos(fok)
        a(i,ih,1)=co
        a(i,ih,2)=rho*si
        a(i,ih,3)=-si/rho
        a(i,ih,4)=co
        a(i,ih,5)=-rho*(one-co)/dpsq
        a(i,ih,6)=-si/dpsq
!--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        a(i,ih,1)=one
        a(i,ih,2)=el(i)
        a(i,ih,3)=zero
        a(i,ih,4)=one
        goto 200
!-----------------------------------------------------------------------
!  RECTANGULAR MAGNET VERTIKAL
!-----------------------------------------------------------------------
   70   ih=2
        goto 40
!-----------------------------------------------------------------------
!  SEKTORMAGNET VERTIKAL
!-----------------------------------------------------------------------
   80   ih=2
        goto 60
!-----------------------------------------------------------------------
!  QUADRUPOLE
!  FOCUSSING
!-----------------------------------------------------------------------
   90   fok=ek(i)/(one+dpp)
        if(abs(fok).le.pieni) goto 10
        ih=0
        hi=sqrt(abs(fok))
        fi=el(i)*hi
        if(fok.gt.zero) goto 110
  100   ih=ih+1
        a(i,ih,1)=cos(fi)
        hi1=sin(fi)
        a(i,ih,2)=hi1/hi
        a(i,ih,3)=-hi1*hi
        a(i,ih,4)=a(i,ih,1)
        if(ih.eq.2) goto 200
!--DEFOCUSSING
  110   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        a(i,ih,1)=hc
        a(i,ih,2)=hs/hi
        a(i,ih,3)=hs*hi
        a(i,ih,4)=hc
        if(ih.eq.1) goto 100
        goto 200
!-----------------------------------------------------------------------
!  COMBINED FUNCTION MAGNET HORIZONTAL
!  FOCUSSING
!-----------------------------------------------------------------------
  120   ih=0
        fokq=ek(i)
  130   wf=ed(i)/dpsq
        fok=fokq/dpd-wf*wf
        if(abs(fok).le.pieni) goto 10
        afok=abs(fok)
        hi=sqrt(afok)
        fi=hi*el(i)
        if(fok.gt.zero) goto 160
  140   ih=ih+1
        si=sin(fi)
        co=cos(fi)
        a(i,ih,1)=co
        a(i,ih,2)=si/hi
        a(i,ih,3)=-si*hi
        a(i,ih,4)=co
        a(i,ih,5)=-wf/afok*(one-co)/dpsq
        a(i,ih,6)=-wf/hi*si/dpsq
        ih=ih+1
        if(ih.gt.2) ih=1
        hi=sqrt(abs(ek(i)/dpd))
        fi=hi*el(i)
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        a(i,ih,1)=hc
        a(i,ih,2)=el(i)
        if(abs(hi).le.pieni) goto 150
        a(i,ih,2)=hs/hi
  150   a(i,ih,3)=hs*hi
        a(i,ih,4)=hc
        goto 200
!--DEFOCUSSING
  160   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        a(i,ih,1)=hc
        a(i,ih,2)=hs/hi
        a(i,ih,3)=hs*hi
        a(i,ih,4)=hc
        a(i,ih,5)= wf/afok*(one-hc)/dpsq
        a(i,ih,6)=-wf/hi*hs/dpsq
        ih=ih+1
        if(ih.gt.2) ih=1
        hi=sqrt(abs(ek(i)/dpd))
        fi=hi*el(i)
        si=sin(fi)
        co=cos(fi)
        a(i,ih,1)=co
        a(i,ih,2)=si/hi
        a(i,ih,3)=-si*hi
        a(i,ih,4)=co
        goto 200
!-----------------------------------------------------------------------
!  COMBINED FUNCTION MAGNET VERTICAL
!-----------------------------------------------------------------------
  170   ih=1
        fokq=-ek(i)
        goto 130
!-----------------------------------------------------------------------
!  EDGE FOCUSSING
!-----------------------------------------------------------------------
  180   rhoi=ed(i)/dpsq
        fok=rhoi*tan(el(i)*rhoi*half)
        a(i,1,1)=one
        a(i,1,2)=zero
        a(i,1,3)=fok
        a(i,1,4)=one
        a(i,2,1)=one
        a(i,2,2)=zero
        a(i,2,3)=-fok
        a(i,2,4)=one
        goto 200
!-----------------------------------------------------------------------
!   NONLINEAR INSERTION
!-----------------------------------------------------------------------
  190   sm(i)=ed(i)
  200 continue
      call block
      return
      end
      subroutine envardis(dpp,aeg,bl1eg,bl2eg)
!-----------------------------------------------------------------------
!  CALCULATION OF ELEMENT MATRICES
!-----------------------------------------------------------------------
      implicit none
      integer i,ih,kz1,l,ll
      double precision aeg,afok,bl1eg,bl2eg,co,dpd,dpp,dpsq,fi,fok,fokq,&
     &g,gl,hc,hi,hi1,hm,hp,hs,rho,rhoi,si,wf
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
      dimension aeg(nele,2,6),bl1eg(nblo,2,6),bl2eg(nblo,2,6)
      save
!-----------------------------------------------------------------------
      dpd=one+dpp
      dpsq=sqrt(dpd)
      do 190 i=1,il
        do ll=1,6
          do l=1,2
            aeg(i,l,ll)=zero
          enddo
        enddo
        if(abs(el(i)).le.pieni) goto 190
        kz1=kz(i)+1
        goto(10,30,90,50,70,80,120,170,180),kz1
        goto 190
!-----------------------------------------------------------------------
!  DRIFTLENGTH
!-----------------------------------------------------------------------
   10   do 20 l=1,2
          aeg(i,l,1)=one
          aeg(i,l,2)=el(i)
          aeg(i,l,3)=zero
   20   aeg(i,l,4)=one
        goto 190
!-----------------------------------------------------------------------
!  RECTANGULAR MAGNET
!  HORIZONTAL
!-----------------------------------------------------------------------
   30   ih=1
   40   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        si=sin(fok)
        co=cos(fok)
        aeg(i,ih,1)=one
        aeg(i,ih,2)=rho*si
        aeg(i,ih,3)=zero
        aeg(i,ih,4)=one
        aeg(i,ih,5)=-rho*(one-co)/dpsq
        aeg(i,ih,6)=-two*tan(fok*half)/dpsq
!--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        g=tan(fok*half)/rho
        gl=el(i)*g
        aeg(i,ih,1)=one-gl
        aeg(i,ih,2)=el(i)
        aeg(i,ih,3)=-g*(two-gl)
        aeg(i,ih,4)=aeg(i,ih,1)
        goto 190
!-----------------------------------------------------------------------
!  SEKTORMAGNET
!  HORIZONTAL
!-----------------------------------------------------------------------
   50   ih=1
   60   fok=el(i)*ed(i)/dpsq
        if(abs(fok).le.pieni) goto 10
        rho=(one/ed(i))*dpsq
        si=sin(fok)
        co=cos(fok)
        aeg(i,ih,1)=co
        aeg(i,ih,2)=rho*si
        aeg(i,ih,3)=-si/rho
        aeg(i,ih,4)=co
        aeg(i,ih,5)=-rho*(one-co)/dpsq
        aeg(i,ih,6)=-si/dpsq
!--VERTIKAL
        ih=ih+1
        if(ih.gt.2) ih=1
        aeg(i,ih,1)=one
        aeg(i,ih,2)=el(i)
        aeg(i,ih,3)=zero
        aeg(i,ih,4)=one
        goto 190
!-----------------------------------------------------------------------
!  RECTANGULAR MAGNET VERTIKAL
!-----------------------------------------------------------------------
   70   ih=2
        goto 40
!-----------------------------------------------------------------------
!  SEKTORMAGNET VERTIKAL
!-----------------------------------------------------------------------
   80   ih=2
        goto 60
!-----------------------------------------------------------------------
!  QUADRUPOLE
!  FOCUSSING
!-----------------------------------------------------------------------
   90   fok=ek(i)/(one+dpp)
        if(abs(fok).le.pieni) goto 10
        ih=0
        hi=sqrt(abs(fok))
        fi=el(i)*hi
        if(fok.gt.zero) goto 110
  100   ih=ih+1
        aeg(i,ih,1)=cos(fi)
        hi1=sin(fi)
        aeg(i,ih,2)=hi1/hi
        aeg(i,ih,3)=-hi1*hi
        aeg(i,ih,4)=aeg(i,ih,1)
        if(ih.eq.2) goto 190
!--DEFOCUSSING
  110   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        aeg(i,ih,1)=hc
        aeg(i,ih,2)=hs/hi
        aeg(i,ih,3)=hs*hi
        aeg(i,ih,4)=hc
        if(ih.eq.1) goto 100
        goto 190
!-----------------------------------------------------------------------
!  COMBINED FUNCTION MAGNET HORIZONTAL
!  FOCUSSING
!-----------------------------------------------------------------------
  120   ih=0
        fokq=ek(i)
  130   wf=ed(i)/dpsq
        fok=fokq/dpd-wf*wf
        if(abs(fok).le.pieni) goto 10
        afok=abs(fok)
        hi=sqrt(afok)
        fi=hi*el(i)
        if(fok.gt.zero) goto 160
  140   ih=ih+1
        si=sin(fi)
        co=cos(fi)
        aeg(i,ih,1)=co
        aeg(i,ih,2)=si/hi
        aeg(i,ih,3)=-si*hi
        aeg(i,ih,4)=co
        aeg(i,ih,5)=-wf/afok*(one-co)/dpsq
        aeg(i,ih,6)=-wf/hi*si/dpsq
        ih=ih+1
        if(ih.gt.2) ih=1
        hi=sqrt(abs(ek(i)/dpd))
        fi=hi*el(i)
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        aeg(i,ih,1)=hc
        aeg(i,ih,2)=el(i)
        if(abs(hi).le.pieni) goto 150
        aeg(i,ih,2)=hs/hi
  150   aeg(i,ih,3)=hs*hi
        aeg(i,ih,4)=hc
        goto 190
!--DEFOCUSSING
  160   ih=ih+1
        hp=exp(fi)
        hm=one/hp
        hc=(hp+hm)*half
        hs=(hp-hm)*half
        aeg(i,ih,1)=hc
        aeg(i,ih,2)=hs/hi
        aeg(i,ih,3)=hs*hi
        aeg(i,ih,4)=hc
        aeg(i,ih,5)= wf/afok*(one-hc)/dpsq
        aeg(i,ih,6)=-wf/hi*hs/dpsq
        ih=ih+1
        if(ih.gt.2) ih=1
        hi=sqrt(abs(ek(i)/dpd))
        fi=hi*el(i)
        si=sin(fi)
        co=cos(fi)
        aeg(i,ih,1)=co
        aeg(i,ih,2)=si/hi
        aeg(i,ih,3)=-si*hi
        aeg(i,ih,4)=co
        goto 190
!-----------------------------------------------------------------------
!  COMBINED FUNCTION MAGNET VERTICAL
!-----------------------------------------------------------------------
  170   ih=1
        fokq=-ek(i)
        goto 130
!-----------------------------------------------------------------------
!  EDGE FOCUSSING
!-----------------------------------------------------------------------
  180   rhoi=ed(i)/dpsq
        fok=rhoi*tan(el(i)*rhoi*half)
        aeg(i,1,1)=one
        aeg(i,1,2)=zero
        aeg(i,1,3)=fok
        aeg(i,1,4)=one
        aeg(i,2,1)=one
        aeg(i,2,2)=zero
        aeg(i,2,3)=-fok
        aeg(i,2,4)=one
        goto 190
!-----------------------------------------------------------------------
!   NONLINEAR INSERTION
!-----------------------------------------------------------------------
  190 continue
      call blockdis(aeg,bl1eg,bl2eg)
      return
      end
      subroutine prror(ier)
!-----------------------------------------------------------------------
!  ERROR OUTPUT
!-----------------------------------------------------------------------
      implicit none
      integer ier
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
      save
!-----------------------------------------------------------------------
      write(*,10000)
      goto(10  ,20  ,30  ,40  ,50  ,60  ,70  ,80  ,90  ,100 ,           &
     &110 ,120 ,130 ,140 ,150 ,160 ,170 ,180 ,190 ,200 ,                &
     &210 ,220 ,230 ,240 ,250 ,260 ,270 ,280 ,290 ,300 ,                &
     &310 ,320 ,330 ,340 ,350 ,360 ,370 ,380 ,390 ,400 ,                &
     &410 ,420 ,430 ,440 ,450 ,460 ,470 ,480 ,490 ,500 ,                &
     &510 ,520 ,530 ,540 ,550 ,560 ,570 ,580 ,590 ,600 ,                &
     &610 ,620 ,630 ,640 ,650 ,660 ,670 ,680 ,690 ,700 ,                &
     &710 ,720 ,730 ,740 ,750 ,760 ,770 ,780 ,790 ,800 ,                &
     &810 ,820 ,830 ,840 ,850 ,860 ,870 ,880 ,890 ,900 ,                &
     &910 ,920 ,930 ,940 ,950 ,960 ,970 ,980 ,990 ,1000,                &
     &1010,1020,1030),ier
      goto 1870
   10 write(*,10010)
      goto 1870
   20 write(*,10020) nele
      goto 1870
   30 write(*,10030)
      goto 1870
   40 write(*,10040)
      goto 1870
   50 write(*,10050)
      goto 1870
   60 write(*,10060)
      goto 1870
   70 write(*,10070)
      goto 1870
   80 write(*,10080)
      goto 1870
   90 write(*,10090)
      goto 1870
  100 write(*,10100)
      goto 1870
  110 write(*,10110)
      goto 1870
  120 write(*,10120)
      goto 1870
  130 write(*,10130)
      goto 1870
  140 write(*,10140)
      goto 1870
  150 write(*,10150)
      goto 1870
  160 write(*,10160) nele
      goto 1870
  170 write(*,10170) nper
      goto 1870
  180 write(*,10180) nblo
      goto 1870
  190 write(*,10190) erbez
      goto 1870
  200 write(*,10200) erbez
      goto 1870
  210 write(*,10210)
      goto 1870
  220 write(*,10220)
      goto 1870
  230 write(*,10230)
      goto 1870
  240 write(*,10240)
      goto 1870
  250 write(*,10250)
      goto 1870
  260 write(*,10260) nelb
      goto 1870
  270 write(*,10270)
      goto 1870
  280 write(*,10280)
      goto 1870
  290 write(*,10290)
      goto 1870
  300 write(*,10300) nran
      goto 1870
  310 write(*,10310)
      goto 1870
  320 write(*,10320)
      goto 1870
  330 write(*,10330)
      goto 1870
  340 write(*,10340) mran
      goto 1870
  350 write(*,10350)
      goto 1870
  360 write(*,10360)
      goto 1870
  370 write(*,10370)
      goto 1870
  380 write(*,10380)
      goto 1870
  390 write(*,10390)
      goto 1870
  400 write(*,10400)
      goto 1870
  410 write(*,10410)
      goto 1870
  420 write(*,10420)
      goto 1870
  430 write(*,10430) nzfz
      goto 1870
  440 write(*,10440)
      goto 1870
  450 write(*,10450)
      goto 1870
  460 write(*,10460) nrco
      goto 1870
  470 write(*,10470)
      goto 1870
  480 write(*,10480)
      goto 1870
  490 write(*,10490)
      goto 1870
  500 write(*,10500)
      goto 1870
  510 write(*,10510)
      goto 1870
  520 write(*,10520) nema
      goto 1870
  530 write(*,10530)
      goto 1870
  540 write(*,10540) npart
      goto 1870
  550 write(*,10550) nmac
      goto 1870
  560 write(*,10560) ierro
      goto 1870
  570 write(*,10570) ierro
      goto 1870
  580 write(*,10580) ierro
      goto 1870
  590 write(*,10590) ierro
      goto 1870
  600 write(*,10600) ierro
      goto 1870
  610 write(*,10610) ierro
      goto 1870
  620 write(*,10620)
      goto 1870
  630 write(*,10630)
      goto 1870
  640 write(*,10640)
      goto 1870
  650 write(*,10650) mcor
      goto 1870
  660 write(*,10660)
      goto 1870
  670 write(*,10670)
      goto 1870
  680 write(*,10680)
      goto 1870
  690 write(*,10690)
      goto 1870
  700 write(*,10700)
      goto 1870
  710 write(*,10710)
      goto 1870
  720 write(*,10720)
      goto 1870
  730 write(*,10730)
      goto 1870
  740 write(*,10740)
      goto 1870
  750 write(*,10750)
      goto 1870
  760 write(*,10760)
      goto 1870
  770 write(*,10770)
      goto 1870
  780 write(*,10780)
      goto 1870
  790 write(*,10790)
      goto 1870
  800 write(*,10800)
      goto 1870
  810 write(*,10810)
      goto 1870
  820 write(*,10820)
      goto 1870
  830 write(*,10830)
      goto 1870
  840 write(*,10840)
      goto 1870
  850 write(*,10850) mmul
      goto 1870
  860 write(*,10860)
      goto 1870
  870 write(*,10870)
      goto 1870
  880 write(*,10880)
      goto 1870
  890 write(*,10890)
      goto 1870
  900 write(*,10900)
      goto 1870
  910 write(*,10910)
      goto 1870
  920 write(*,10920)
      goto 1870
  930 write(*,10930)
      goto 1870
  940 write(*,10940)
      goto 1870
  950 write(*,10950)
      goto 1870
  960 write(*,10960)
      goto 1870
  970 write(*,10970)
      goto 1870
  980 write(*,10980)
      goto 1870
  990 write(*,10990)
      goto 1870
 1000 write(*,11000) ntr
      goto 1870
 1010 write(*,11010)
      goto 1870
 1020 write(*,11020) nbb
      goto 1870
 1030 write(*,11030)
 1870 continue
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!--CLOSE(DATA FILES
      close(2)
      close(3)
      close(4)
      close(7)
      close(8)
      close(9)
      close(10)
      close(11)
      close(12)
      close(13)
      close(14)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)
      close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      close(26)
      close(27)
      close(32)
      close(33)
      close(34)
      close(59)
      close(60)
      close(61)
      close(62)
      close(63)
      close(64)
      close(65)
      close(66)
      close(67)
      close(68)
      close(69)
      close(70)
      close(71)
      close(72)
      close(73)
      close(74)
      close(75)
      close(76)
      close(77)
      close(78)
      close(79)
      close(80)
      close(81)
      close(82)
      close(83)
      close(84)
      close(85)
      close(86)
      close(87)
      close(88)
      close(89)
      close(90)
      close(98)
      close(99)
      stop
10000 format(5x///t10,'++++++++++++++++++++++++'/ t10,                  &
     &'+++++ERROR DETECTED+++++'/ t10,'++++++++++++++++++++++++'/ t10,  &
     &'RUN TERMINATED ABNORMALLY !!!'//)
10010 format(t10,'WRONG MODE DEFINITION')
10020 format(t10,'NOT MORE THAN: ',i4,' POSITIONS FOR RESONANCE-COMPEN',&
     &'SATION ALLOWED')
10030 format(t10,'ELEMENT FOR RESONANCE-COMPENSATION IS NOT IN THE ELE',&
     &'MENTLIST')
10040 format(t10,'UNSTABLE CLOSED ORBIT DURING INITIAL DISPERSION' ,    &
     &' CALCULATION'/ t10,                                              &
     &'INSTABILITY OCCURED FOR SMALL RELATIVE ENERGY DEVIATION')
10050 format(t10,'UNSTABLE CLOSED ORBIT FOR ZERO ENERGY DEVIATION')
10060 format(t10,'UNSTABLE CLOSED ORBIT DURING DISPERSION CALCULATION' ,&
     &' AFTER ORBIT SCALING'/ t10,                                      &
     &'INSTABILITY OCCURED FOR SMALL RELATIVE ENERGY DEVIATION')
10070 format(t10,'UNSTABLE CLOSED ORBIT AFTER ORBIT SCALING')
10080 format(t10,'ELEMENTS SPECIFIED FOR TUNE VARIATION ARE NOT' ,      &
     &' QUADRUPOLES')
10090 format(t10,'UNSTABLE CLOSED ORBIT DURING TUNE VARIATION')
10100 format(t10,'NO OPTICAL SOLUTION DURING TUNE VARIATION')
10110 format(t10,'ELEMENTS SPECIFIED FOR CHROMATICITY CORRECTION ARE' , &
     &' NOT SEXTUPOLES')
10120 format(t10,'UNSTABLE CLOSED ORBIT DURING CHROMATICITY CORRECTION')
10130 format(t10,'NO OPTICAL SOLUTION DURING CHROMATICITY CORRECTION')
10140 format(t10,'ELEMENTS OF DIFFERENT TYPES ARE COMBINED IN DATA' ,   &
     &' BLOCK COMBINATION OF ELEMENTS')
10150 format(t10,'UNKNOWN BLOCK SPECIFICATION')
10160 format(t10,'NO. OF SINGLE ELEMENTS EXCEEDS THE MAXIMUM ALLOWED' , &
     &' VALUE: ',i4)
10170 format(t10,'NO. OF SUPERPERIODS LARGER THAN : ',i4)
10180 format(t10,'NO. OF DIFFERENT BLOCKS EXCEEDS THE MAXIMUM ALLOWED' ,&
     &' VALUE: ',i5)
10190 format(t10,'UNKNOWN SINGLE ELEMENT : ',a16,                       &
     &' IN THE BLOCK DEFINITION')
10200 format(t10,'UNKNOWN BLOCK NAME OR INSERTION NAME : ',a16,' IN THE'&
     &,' STRUCTURE INPUT')
10210 format(t10,'MAXIMUM NUMBER OF STRUCTURE ELEMENTS SURPASSED')
10220 format(t10,'NO SOLUTION FOR ORBIT SCALING - POSSIBLE REASONS:'/   &
     &t10,'--> DIPOLE STRENGTHS OF NON-CORRECTOR ELEMENTS TO HIGH'/ t10,&
     &'--> NONLINEARITIES TOO STRONG, TRY TO INCREASE INITIAL'/ t10,    &
     &'    CORRECTOR STRENGTHS'/ t10,                                   &
     &'--> USE ALL DIPOLE ELEMENTS FOR SCALING'/)
10230 format(t10,'NO OPTICAL SOLUTION')
10240 format(t10,'NO SOLUTION FOR DISPERSION')
10250 format(t10,'--> PLEASE INCLUDE LENGTH OF MACHINE IN THE' ,        &
     &' <SYNCHROTRON>-BLOCK')
10260 format(t10,'ONE BLOCK CAN NOT HAVE MORE THAN ',i4,' ELEMENTS')
10270 format(t10,'KINETIC ENERGY OF THE PARTICLE IS LESS OR EQUAL ZERO')
10280 format(t10,'EITHER YOUR RF-FREQENCY IS SHIFTED BY 180 DEGREES'/ , &
     &t10,'THEN CHANGE THE SIGN OF <ITION> IN THE ',                    &
     &'<SYNCHROTRON>-INPUTBLOCK',/t10,'OR YOUR ALFA-P IS WRONGLY ',     &
     &'INTRODUCED IN THE SAME INPUTBLOCK')
10290 format(t10,'MULTIPOLECOEFFITIONS CANNOT BE SET EQUAL')
10300 format(t10,'THE RANDOM NUMBER: ',i6,' FOR THE INITIAL',           &
     &' STRUCTURE IS TOO SMALL')
10310 format(t10,'ELEMENTS THAT NEED RANDOM NUMBERS HAVE A KZ > 0')
10320 format(t10,'THERE ARE NOT ENOUGH RANDOM NUMBERS FOR THE INSERTED',&
     &' ELEMENTS')
10330 format(t10,'TO USE THE SAME RANDOMNUMBERS FOR 2 ELEMENTS, THE',   &
     &' INSERTED ELEMENT MUST NOT NEED MORE OF SUCH NUMBERS THAN THE',  &
     &' REFERENCE ELEMENT')
10340 format(t10,'NOT MORE THAN',i4,' OF EACH TYP OF INSERTED ELEMENTS',&
     &' CAN BE USED')
10350 format(t10,'PROBLEMS DURING MATRIX-INVERSION IN QMOD')
10360 format(t10,'NO CONVERGENCE IN RMOD')
10370 format(t10,'CHOSEN ORDERS OF RESONANCES CAN NOT BE CALCULATED')
10380 format(t10,'PROBLEMS DURING MATRIX-INVERSION IN RMOD')
10390 format(t10,'WITH THE SPECIFIED ELEMENTS THE RESONANCE CANNOT BE', &
     &' COMPENSATED - RESONANCEORDER AND ELEMENTTYP # MUST BE THE SAME')
10400 format(t10,'NOT MORE THAN 2 PARTICLES CAN BE TRACKED')
10410 format(t10,'GEOMETRY AND STRENGTH FILE (UNIT 2) IS EMPTY OR ' ,   &
     &' DISTROYED')
10420 format(t10,'TRACKING PARAMETER FILE (UNIT 3) IS EMPTY OR ' ,      &
     &' NONEXISTING')
10430 format(t10,'NOT MORE THAN ',i4,' RANDOM NUMBERS CAN BE USED')
10440 format(t10,'FOR THE INPUTBLOCK - ORBIT CORRECTION - ONLY CORRE',  &
     &'CTORS WITH THE KEYWORDS ( HCOR= ; VCOR= )'/t10,                  &
     &'AND MONITORS WITH THE', ' KEYWORDS ( HMON= ; VMON= ) ARE ALLOWED'&
     &)
10450 format(t10,'FOR THE INPUTBLOCK - LINEAR OPTICS - ONLY',           &
     &' THE KEYWORD ( ELEMENT ) AND ( BLOCK ) ARE ALLOWED')
10460 format(t10,'ORDER OF COMPENSATION CAN NOT BE LARGER THAN : ',i4)
10470 format(t10,'ONLY UP TO 3 RESONANCES CAN BE COMPENSATED')
10480 format(t10,'RESONANCE TYPE IS OUT OF THE RANGE OF THE RESONANCE', &
     &' ORDER')
10490 format(t10,'ONLY UP TO 3 SUBRESONANCES CAN BE COMPENSATED')
10500 format(t10,'THE MULTIPOLE ORDER FOR THE SUBRESONANCE COMPENSATION'&
     &,' SHOULD NOT EXCEED THE VALUE 9')
10510 format(t10,'TOO MANY RIPPLE ELEMENTS')
10520 format(t10,'MAXIMUM ORDER OF THE ONE TURN MAP IS ',i4, /,         &
     &' NEMA HAS TO BE LARGER THAN NORD')
10530 format(t10,'# OF VARIABLES -NV- OF THE ONE TURN MAP IS NOT',      &
     &' IN THE ALLOWED RANGE [2 <= NV <= 5]')
10540 format(t10,'MAXIMUM NUMBER OF PARTICLES FOR VECTORIZATION', ' IS '&
     &,i4)
10550 format(t10,'MAXIMUM NUMBER OF DIFFERENT SEEDS FOR VECTORIZATION', &
     &' IS ',i4)
10560 format(t10,'PROBLEMS WITH FILE 13 WITH INITIAL COORDINATES ',     &
     &' - ERROR CODE : ',i10)
10570 format(t10,'PROBLEMS WITH FILE 2 WITH ACCELERATOR STRUCTURE ',    &
     &' - ERROR CODE : ',i10)
10580 format(t10,'PROBLEMS WITH FILE 3 WITH TRACKING PARAMETERS ',      &
     &' - ERROR CODE : ',i10)
10590 format(t10,'PROBLEMS WITH FILE 11 FOR CRAY INPUT ',               &
     &' - ERROR CODE : ',i10)
10600 format(t10,'PROBLEMS WITH FILE 99 FOR BINARY OUTPUT ',            &
     &' - ERROR CODE : ',i10)
10610 format(t10,'PROBLEMS WITH FILE 12 FOR END COORDINATES',           &
     &' - ERROR CODE : ',i10)
10620 format(t10,'ELEMENTS SPECIFIED FOR DECOUPLING ARE NOT' ,          &
     &' SKEW QUADRUPOLES')
10630 format(t10,'THERE ARE THE APPROPRIATE ELEMENTS FOR' ,             &
     &' THE DECOUPLING OR SIMULTANEOUS TUNE ADJUSTMENT')
10640 format(t10,'PROBLEMS DURING MATRIX-INVERSION IN DECOUP')
10650 format(t10,'MAXIMUM NUMBER OF EXTRA PARAMETERS IS : ',i4)
10660 format(t10,'EXTRA PARAMETERS FOR THE MAP DOES NOT EXIST')
10670 format(t10,'ONLY SINGLE KICK ELEMENTS ALLOWED FOR MAP CALCULATION'&
     &)
10680 format(t10,'THE ORDER OF THE NORMAL FORM IS TOO HIGH. CHECK THE' ,&
     &' DIFFERENTIAL ALGEBRA PARAMETERS')
10690 format(t10,'TOO MANY VARIABLES SPECIFIED. CHECK THE DIFFERENTIAL' &
     &,' ALGEBRA PARAMETERS')
10700 format(t10,'NO CORRECTORS SPECIFIED')
10710 format(t10,'BOTH AMPLITUDE AND MOMENTUM ORDER ARE ZERO!')
10720 format(t10,'BOTH AMPLITUDE AND MOMENTUM ORDER ARE DIFFERENT FROM',&
     &' ZERO!')
10730 format(t10,'AMPLITUDE ORDER OUTSIDE RANGE [0,2]')
10740 format(t10,'MOMENTUM ORDER OUTSIDE RANGE [0,3] (ONE EXCLUDED!)')
10750 format(t10,'MINIMUM ORDER OUTSIDE RANGE [2,3]')
10760 format(t10,'MINIMUM ORDER GREATER THAN MAXIMUM!')
10770 format(t10,'MAXIMUM ORDER OUTSIDE RANGE [2,3]')
10780 format(t10,'NORMAL FORMS ANALYSIS IMPOSSIBLE',/ ,t10,             &
     &'THE TRANSFER MAP DOES NOT EXIST!')
10790 format(t10,'ZERO OR NEGATIVE ENERGY DOES NOT MAKE MUCH SENSE')
10800 format(t10,'PROBLEM READING EXTERNAL MULTIPOLE ERRORS')
10810 format(t10,'TOO MANY ELEMENTS FOR LINEAR OPTICS WRITE-OUT')
10820 format(t10,'FOR CLOSED ORBIT CORRECTORS ONLY DIPOLES OF LEGTH',   &
     &' ZERO OR MULTIPOLE LENSES ALLOWED')
10830 format(t10,'AN ELEMENT FOR CLOSED ORBIT CORRECTION CAN BE ONLY',  &
     &' EITHER A HORIZONTAL MONITOR',/,t10, 'OR A VERTICAL MONITOR OR', &
     &' A HORIZONTAL CORRECTOR OR A VERTICAL CORRECTOR')
10840 format(t10,'NUMBER OF ORBIT CORRECTORS IS ZERO')
10850 format(t10,'THE ORDER OF MULTIPOLES MMUL: ',i4,' HAS TO BE LARGER'&
     &,' THAN 10 BUT SMALLER THAN 20')
10860 format(t10,'PROBLEM READING EXTERNAL MISALIGNMENTS')
10870 format(t10,'PROBLEM READING FROM FILE 30 (SINGLE KICKS AND ',     &
     &'MISALIGNMENTS')
10880 format(t10,'BEAM_BEAM: EITHER NORMALIZED EMITTANCES OR THE ',     &
     &'RESULTING SIGMA VALUES EQUAL TO ZERO')
10890 format(t10,'BEAM_BEAM: AT EACH INTERACTION POINT THE BEAM ',      &
     &'MUST BE EITHER ROUND OR ELLIPTICAL FOR ALL PARTICLES')
10900 format(t10,'QUADRUPOLES ARE NOT SUITED TO ADJUST THE TUNES')
10910 format(t10,'ORDER AND NUMBER OF VARIABLES HAVE TO BE LARGER ',    &
     &'THAN ZERO TO CALCULATE A DIFFERENTIAL ALGEBRA MAP')
10920 format(t10,'YOU CANNOT COMBINE AN ELEMENT WITH ITSELF')
10930 format(t10,'INVERTED LINEAR BLOCKS NOT ALLOWED')
10940 format(t10,'  NUMBER OF NORMAL FORM VARIABLES HAVE TO BE: ',      &
     &'2, 4, 5, 6 + PARAMETERS')
10950 format(t10,'  DA CORRECTIONS IMPLEMENTED FOR 4-D AND 6-D ONLY ')
10960 format(t10,'SEXTUPOLES ARE NOT SUITED TO ADJUST THE CHROMATICITY')
10970 format(t10,'UNSTABLE CLOSED ORBIT IN DA CALCULATION')
10980 format(t10,'TROMBONE ELEMENT NOT IN LIST OF SINGLE ELEMENTS')
10990 format(t10,'INCOMPLETE PARAMETERS FOR TROMBONE ELEMENT')
11000 format(t10,'MAXIMUM NUMBER OF TROMBONES EXCEEDED : NTR = ',i4)
11010 format(t10,'AMPLITUDES EXCEED THE MAXIMUM VALUES IN UMLAUF')
11020 format(t10,'MAXIMUM ELEMENT NUMBER FOR BEAM_BEAM WITH COUPLING ', &
     &'EXCEEDED:  NBB = ',i4)
11030 format(t10,'6D BEAM-BEAM WITH TILT NOT POSSIBLE')
      end
      subroutine linopt(dpp)
!-----------------------------------------------------------------------
!  LINEAR PARAMETERS AT THE POSITION OF EVERY ELEMENT OR BLOCK
!-----------------------------------------------------------------------
      implicit none
      integer i,iflag,iiii,im,ium,ix,izu,j,jj,jk,jm,k,kpz,kzz,l,l1,ll,  &
     &nmz,nr,dj
      double precision aa,aeg,alfa,bb,benkr,beta,bexi,bezii,bl1eg,bl2eg,&
     &ci,cikve,clo0,clop0,cr,crkve,crkveuk,di00,dip00,dphi,dpp,dpp1,    &
     &dppi,dpr,dyy1,dyy2,ekk,etl,phi,phibf,pie,puf,qu,qv,qw,qwc,r0,r0a, &
     &t,xl,xs,zl,zs
      double precision dyy11,qu1,tiltck,tiltsk
      character*16 idum
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
!
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
!      parameter (max_ncoll=75,max_npart=20000,nc=32,numeff=19,          &
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,          &
     &maxn=20000,outlun=54)
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
      dimension t(6,4)
      dimension beta(2),alfa(2),phibf(2),phi(2)
      dimension clo0(2),clop0(2),di00(2),dip00(2),qw(2),qwc(3)
      dimension aa(mmul),bb(mmul),dpr(6)
      dimension cr(mmul),ci(mmul)
      dimension aeg(nele,2,6),bl1eg(nblo,2,6),bl2eg(nblo,2,6)
      data dpr/6*0d0/
      save
!-----------------------------------------------------------------------
      nhmoni=0
      nvmoni=0
      nhcorr=0
      nvcorr=0
      ium=6
      pie=two*pi
      if(ncorru.eq.0) then
        write(*,10010)
        write(*,10000)
      endif
      do 10 i=1,ium
        dpr(i)=zero
   10 continue
      do 20 i=1,ium
        do 20 j=1,4
          t(i,j)=zero
   20 continue
      do 30 i=1,2
        beta(i)=zero
        alfa(i)=zero
        phibf(i)=zero
        phi(i)=zero
        clo0(i)=zero
        clop0(i)=zero
        di00(i)=zero
        dip00(i)=zero
        qw(i)=zero
        qwc(i)=zero
   30 continue
      qwc(3)=zero
      do 40 i=1,mmul
        aa(i)=zero
        bb(i)=zero
        cr(i)=zero
        ci(i)=zero
   40 continue
      etl=zero
      nr=0
      dpr(1)=dpp*c1e3
      dpr(6)=one
      dpp1=dpp+ded
      call clorb(dpp1)
      do 50 l=1,2
        clo0(l)=clo(l)
   50 clop0(l)=clop(l)
      call clorb(dpp)
      do 60 l=1,2
        ll=2*l
        di0(l)=(clo0(l)-clo(l))/ded
        dip0(l)=(clop0(l)-clop(l))/ded
        t(6,ll-1)=di0(l)
   60 t(6,ll)=dip0(l)
      if(ncorru.eq.0) then
        write(*,10010)
        write(*,10050) (di0(l),dip0(l),l=1,2)
      endif
      call betalf(dpp,qw)
      call phasad(dpp,qwc)
      if(ierro.ne.0) call prror(22+ierro)
      if(ncorru.eq.0) write(*,10040) dpp,qwc(1),qwc(2)
      call envar(dpp)
      if(ithick.eq.1) call envardis(dpp1,aeg,bl1eg,bl2eg)
!--STARTVALUES OF THE TRAJECTORIES
      do 70 l=1,2
        ll=2*l
        t(1,ll-1)=clo(l)
   70 t(1,ll)=clop(l)
      do 80 i=1,4
        do 80 j=1,4
          t(i+1,j)=ta(j,i)
   80 t(i+1,j)=ta(j,i)
      if(ncorru.eq.0) then
        write(*,10010)
        if(iprint.eq.1) write(*,10030)
        write(*,10020)
        write(*,10010)
      endif
      iflag=0
      idum='START'
      call writelin(nr,idum,etl,phi,t,1,k)
      if(ntco.ne.0) then
        if(mod(nr,ntco).eq.0) call cpltwis(idum,t,etl,phi)
      endif
!--SINGLE TURN BLOCKLOOP
      if(nt.le.0.or.nt.gt.iu) nt=iu
      izu=0
      do 500 k=1,nt
        ix=ic(k)
        if(ix.gt.nblo) goto 220
        if(ithick.eq.1.and.iprint.eq.1) goto 160
        jj=0
        dj=1
        if(ix.gt.0) goto 90
        ix=-ix
        jj=mel(ix)+1
        dj=-1
   90   jm=mel(ix)
!--BLOCKELEMENTLOOP
        do 150 j=1,jm
          jj=jj+dj
          jk=mtyp(ix,jj)
          if(ithick.eq.1.and.kz(jk).ne.0) goto 120
!GRD UPGRADE JANUARY 2005
!         if(ithick.eq.0.and.kz(jk).ne.0)
         if(ithick.eq.0.and.kz(jk).ne.0) then
          call writelin(nr,bez(jk),etl,phi,t,ix,k)
          goto 500
         endif
!GRD END OF UPGRADE
!GRD
!--PURE DRIFTLENGTH
          etl=etl+el(jk)
          do 100 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=pi2
            endif
            do 100 i=1,ium
  100     t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
          do 110 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=pi2-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
  110     phi(l)=phi(l)+dphi/pie
          nr=nr+1
          call writelin(nr,bez(jk),etl,phi,t,ix,k)
          if(ntco.ne.0) then
            if(mod(nr,ntco).eq.0) call cpltwis(bez(jk),t,etl, phi)
          endif
          goto 150
!--MAGNETELEMENT
  120     continue
          if(kz(jk).ne.8) etl=etl+el(jk)
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            puf=t(6,ll-1)
            t(6,ll-1)=(aeg(jk,l,1)*(t(1,ll-1)+puf*ded)+ aeg(jk,l,2)*(t  &
     &(1,ll)+t(6,ll)*ded)+aeg(jk,l,5)*dpp1*c1e3- a(jk,l,1)*t            &
     &(1,ll-1)-a(jk,l,2)*t(1,ll)- a(jk,l,5)*dpr(1))/ded
            t(6,ll)=(aeg(jk,l,3)*(t(1,ll-1)+puf*ded)+ aeg(jk,l,4)*(t    &
     &(1,ll)+t(6,ll)*ded)+aeg(jk,l,6)*dpp1*c1e3- a(jk,l,3)*t            &
     &(1,ll-1)-a(jk,l,4)*t(1,ll)- a(jk,l,6)*dpr(1))/ded
            do i=1,ium-1
              puf=t(i,ll-1)
              t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a(jk,l,5)
              t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
            enddo
          enddo
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(kz(jk).ne.8.and.-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi/pie
          enddo
          nr=nr+1
          call writelin(nr,bez(jk),etl,phi,t,ix,k)
          if(ntco.ne.0) then
            if(mod(nr,ntco).eq.0) call cpltwis(bez(jk),t,etl, phi)
          endif
  150   continue
!GRD UPGRADE JANUARY 2005
        call writelin(nr,bez(jk),etl,phi,t,ix,k)
        goto 500
!--BETACALCULATION FOR SERIES OF BLOCKS
  160   continue
        if(ix.le.0) goto 190
!--REGULAR RUN THROUGH BLOCKS
        etl=etl+elbe(ix)
        do 170 l=1,2
          ll=2*l
          if(abs(t(ll,ll-1)).gt.pieni) then
            phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
          else
            phibf(l)=zero
          endif
          puf=t(6,ll-1)
          t(6,ll-1)=(bl1eg(ix,l,1)*(t(1,ll-1)+puf*ded)+ bl1eg(ix,l,2)*(t&
     &(1,ll)+t(6,ll)*ded)+ bl1eg(ix,l,5)*dpp1*c1e3- bl1(ix,l,1)*t       &
     &(1,ll-1)-bl1(ix,l,2)*t(1,ll)- bl1(ix,l,5)*dpr(1))/ded
          t(6,ll)=(bl1eg(ix,l,3)*(t(1,ll-1)+puf*ded)+ bl1eg(ix,l,4)*(t  &
     &(1,ll)+t(6,ll)*ded)+ bl1eg(ix,l,6)*dpp1*c1e3- bl1(ix,l,3)*t       &
     &(1,ll-1)-bl1(ix,l,4)*t(1,ll)- bl1(ix,l,6)*dpr(1))/ded
          do 170 i=1,ium-1
            puf=t(i,ll-1)
            t(i,ll-1)=bl1(ix,l,1)*puf+bl1(ix,l,2)*t(i,ll)+dpr(i)*bl1    &
     &(ix,l,5)
  170   t(i,ll)=bl1(ix,l,3)*puf+bl1(ix,l,4)*t(i,ll)+dpr(i)*bl1(ix,l,6)
        do 180 l=1,2
          ll=2*l
          if(abs(t(ll,ll-1)).gt.pieni) then
            dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
          else
            dphi=-phibf(l)
          endif
          if(-dphi.gt.pieni) dphi=dphi+pi
  180   phi(l)=phi(l)+dphi/pie
        nr=nr+1
        call writelin(nr,bezb(ix),etl,phi,t,ix,k)
        if(ntco.ne.0) then
          if(mod(nr,ntco).eq.0) call cpltwis(bezb(ix),t,etl,phi)
        endif
        goto 500
!--REVERSE RUN THROUGH BLOCKS
  190   ix=-ix
        etl=etl+elbe(ix)
        do 200 l=1,2
          ll=2*l
          if(abs(t(ll,ll-1)).gt.pieni) then
            phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
          else
            phibf(l)=zero
          endif
          puf=t(6,ll-1)
          t(6,ll-1)=(bl2eg(ix,l,1)*(t(1,ll-1)+puf*ded)+ bl2eg(ix,l,2)*(t&
     &(1,ll)+t(6,ll)*ded)+ bl2eg(ix,l,5)*dpp1*c1e3- bl2(ix,l,1)*t       &
     &(1,ll-1)-bl2(ix,l,2)*t(1,ll)- bl2(ix,l,5)*dpr(1))/ded
          t(6,ll)=(bl2eg(ix,l,3)*(t(1,ll-1)+puf*ded)+ bl2eg(ix,l,4)*(t  &
     &(1,ll)+t(6,ll)*ded)+ bl2eg(ix,l,6)*dpp1*c1e3- bl2(ix,l,3)*t       &
     &(1,ll-1)-bl2(ix,l,4)*t(1,ll)- bl2(ix,l,6)*dpr(1))/ded
          do 200 i=1,ium-1
            puf=t(i,ll-1)
            t(i,ll-1)=bl2(ix,l,1)*puf+bl2(ix,l,2)*t(i,ll)+dpr(i)*bl2    &
     &(ix,l,5)
  200   t(i,ll)=bl2(ix,l,3)*puf+bl2(ix,l,4)*t(i,ll)+dpr(i)*bl2(ix,l,6)
        do 210 l=1,2
          ll=2*l
          if(abs(t(ll,ll-1)).gt.pieni) then
            dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
          else
            dphi=-phibf(l)
          endif
          if(-dphi.gt.pieni) dphi=dphi+pi
  210   phi(l)=phi(l)+dphi/pie
        nr=nr+1
        call writelin(nr,bezb(ix),etl,phi,t,ix,k)
        if(ntco.ne.0) then
          if(mod(nr,ntco).eq.0) call cpltwis(bezb(ix),t,etl,phi)
        endif
        goto 500
!--NL-INSERTION
  220   ix=ix-nblo
        qu=zero
        qv=zero
        dyy1=zero
        dyy2=zero
        kpz=kp(ix)
!GRD UPGRADE JANUARY 2005
!       if(kpz.eq.6) goto 500
        if(kpz.eq.6) then
        call writelin(nr,bez(jk),etl,phi,t,ix,k)
        goto 500
        endif
!GRD END OF UPGRADE
        kzz=kz(ix)
        if(kzz.eq.20.and.nbeam.ge.1) then
          nbeam=k
          nr=nr+1
          call writelin(nr,bez(ix),etl,phi,t,ix,k)
        endif
        if(kzz.eq.22) then
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*rrtr(imtr(ix),ll-1,ll-1)+                   &
     &t(i,ll)*rrtr(imtr(ix),ll-1,ll)+                                   &
     &dpr(i)*rrtr(imtr(ix),ll-1,6)
              t(i,ll)=puf*rrtr(imtr(ix),ll,ll-1)+                       &
     &t(i,ll)*rrtr(imtr(ix),ll,ll)+                                     &
     &dpr(i)*rrtr(imtr(ix),ll,6)
            enddo
            t(1,ll-1)=t(1,ll-1)+cotr(imtr(ix),ll-1)
            t(1,ll)=t(1,ll)+cotr(imtr(ix),ll)
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi/pie
          enddo
          call writelin(nr,bez(ix),etl,phi,t,ix,k)
          if(ntco.ne.0) then
            if(mod(nr,ntco).eq.0) call cpltwis(bez(ix),t,etl,phi)
          endif
        endif
        if(kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) then
          call writelin(nr,bez(ix),etl,phi,t,ix,k)
          goto 500
        endif
        dyy1=zero
        dyy2=zero
        if(iorg.lt.0) mzu(k)=izu
        izu=mzu(k)+1
        ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
        izu=izu+1
        xs=xpl(ix)+zfz(izu)*xrms(ix)
        izu=izu+1
        zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(t(1,1)-xs)*tiltc(k)+(t(1,3)-zs)*tilts(k)
        zl=-(t(1,1)-xs)*tilts(k)+(t(1,3)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
        if(kzz.lt.0) goto 370
        goto(230,240,250,260,270,280,290,300,310,320,330),kzz
!GRD UPGRADE JANUARY 2005
        call writelin(nr,bez(ix),etl,phi,t,ix,k)
!GRD END OF UPGRADE
        goto 500
!--HORIZONTAL DIPOLE
  230   ekk=ekk*c1e3
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
        goto 480
!--NORMAL QUADRUPOLE
  240   continue
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        goto 480
!--NORMAL SEXTUPOLE
  250   ekk=ekk*c1m3
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*crkve+tiltsk*cikve)
        qv=ekk*two*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
!--NORMAL OCTUPOLE
  260   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=three*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
!--NORMAL DECAPOLE
  270   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=four*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
!--NORMAL DODECAPOLE
  280   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=5*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
!--NORMAL 14-POLE
  290   ekk=ekk*c1m15
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=6*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
!--NORMAL 16-POLE
  300   ekk=ekk*c1m18
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=7*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
!--NORMAL 18-POLE
  310   ekk=ekk*c1m21
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=8*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
!--NORMAL 20-POLE
  320   ekk=ekk*c1m24
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=9*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
  330   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)                     &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            t(6,2)=t(6,2)-(qu*xl+dppi)/(one+dpp)*tiltc(k)               &
     &-dppi/(one+dpp)*(one-tiltc(k))
            t(6,4)=t(6,4)-(qu*xl+dppi)/(one+dpp)*tilts(k)               &
     &-dppi/(one+dpp)*tilts(k)
            do 340 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  340       continue
          else
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tiltc(k)                             &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
            t(6,2)=t(6,2)-dppi/(one+dpp)*tiltc(k)                       &
     &-dppi/(one+dpp)*(one-tiltc(k))
            t(6,4)=t(6,4)-dppi/(one+dpp)*tilts(k)                       &
     &-dppi/(one+dpp)*tilts(k)
          endif
        endif
        if(abs(dki(ix,2)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)+(qu*zl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+(-qu*zl+dppi*dpp)*tiltc(k)                    &
     &-dppi*(one-tiltc(k))
            t(6,2)=t(6,2)+(-qu*zl-dppi)/(one+dpp)*tilts(k)              &
     &-dppi/(one+dpp)*tilts(k)
            t(6,4)=t(6,4)+(qu*zl+dppi)/(one+dpp)*tiltc(k)               &
     &+dppi/(one+dpp)*(one-tiltc(k))
            do 350 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  350       continue
          else
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+dppi*dpp*tiltc(k)                             &
     &-dppi*(one-tiltc(k))
            t(6,2)=t(6,2)-dppi/(one+dpp)*tilts(k)                       &
     &-dppi/(one+dpp)*tilts(k)
            t(6,4)=t(6,4)+dppi/(one+dpp)*tiltc(k)                       &
     &+dppi/(one+dpp)*(one-tiltc(k))
          endif
        endif
        if(abs(r0).le.pieni) goto 500
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
!GRD
!UPGRADE JANUARY 2005
! --> THAT'S THE IMPORTANT ONE !!!!
         call writelin(nr,bez(jk),etl,phi,t,ix,k)
!GRD
         goto 500
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        do 360 l=1,nmz
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
  360   continue
        if(nmz.ge.2) then
          qu=bb(2)
          qv=-aa(2)
          dyy1=bb(1)+bb(2)*crkve+aa(2)*cikve
          dyy2=aa(1)-bb(2)*cikve+aa(2)*crkve
          do 365 l=3,nmz
            l1=l-1
            qu=qu+l1*(bb(l)*crkve+aa(l)*cikve)
            qv=qv+l1*(bb(l)*cikve-aa(l)*crkve)
            crkveuk=crkve*xl-cikve*zl
            cikve=crkve*zl+cikve*xl
            crkve=crkveuk
            dyy1=dyy1+bb(l)*crkve+aa(l)*cikve
            dyy2=dyy2-bb(l)*cikve+aa(l)*crkve
  365     continue
        else
          qu=zero
          qv=zero
          dyy1=bb(1)
          dyy2=aa(1)
        endif
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
        izu=izu+2*mmul-2*nmz
        goto 480
!--SKEW ELEMENTS
  370   kzz=-kzz
        goto(380,390,400,410,420,430,440,450,460,470),kzz
        goto 500
!--VERTICAL DIPOLE
  380   ekk=ekk*c1e3
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
        goto 480
!--SKEW QUADRUPOLE
  390   continue
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        goto 480
!--SKEW SEXTUPOLE
  400   ekk=ekk*c1m3
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*cikve-tiltsk*crkve)
        qv=-ekk*two*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
!--SKEW OCTUPOLE
  410   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-three*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
!--SKEW DECAPOLE
  420   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-four*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
!--SKEW DODECAPOLE
  430   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-5*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
!--SKEW 14-POLE
  440   ekk=ekk*c1m15
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-6*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
!--SKEW 16-POLE
  450   ekk=ekk*c1m18
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-7*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
!--SKEW 18-POLE
  460   ekk=ekk*c1m21
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-8*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
!--SKEW 20-POLE
  470   ekk=ekk*c1m24
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-9*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
  480   t(6,2)=t(6,2)-dyy1/(one+dpp)
        t(6,4)=t(6,4)-dyy2/(one+dpp)
        t(1,2)=t(1,2)+dyy1
        t(1,4)=t(1,4)+dyy2
        do 490 i=2,ium
          t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
  490   t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
        nr=nr+1
        bexi=t(2,1)*t(2,1)+t(3,1)*t(3,1)
        bezii=t(4,3)*t(4,3)+t(5,3)*t(5,3)
        if(ncorru.eq.0) then
          if(kz(ix).eq.11) then
            if(abs(aa(2)).gt.pieni.and.nmz.gt.1) write(34,10070) etl,   &
     &bez(ix),-2,aa(2),bexi,bezii,phi
            do iiii=3,nmz
              if(abs(bb(iiii)).gt.pieni) write(34,10070) etl,bez(ix),   &
     &iiii,bb(iiii),bexi,bezii,phi
              if(abs(aa(iiii)).gt.pieni) write(34,10070) etl,bez(ix),   &
     &-iiii,aa(iiii),bexi,bezii,phi
            enddo
          elseif(abs(ekk).gt.pieni.and.abs(kz(ix)).ge.3) then
            write(34,10070) etl,bez(ix),kz(ix),ekk,bexi,bezii,phi
          elseif(abs(ekk).gt.pieni.and.kz(ix).eq.-2) then
            write(34,10070) etl,bez(ix),kz(ix),ekk,bexi,bezii,phi
          endif
        endif
        call writelin(nr,bez(ix),etl,phi,t,ix,k)
        if(ntco.ne.0) then
          if(mod(nr,ntco).eq.0) call cpltwis(bez(ix),t,etl,phi)
        endif
  500 continue
      call clorb(ded)
      do 510 l=1,2
        clo0(l)=clo(l)
        clop0(l)=clop(l)
  510 continue
      call clorb(zero)
      do 520 l=1,2
        ll=2*l
        di0(l)=(clo0(l)-clo(l))/ded
        dip0(l)=(clop0(l)-clop(l))/ded
  520 continue
      iiii=100
      idum='END'
      bexi=t(2,1)*t(2,1)+t(3,1)*t(3,1)
      bezii=t(4,3)*t(4,3)+t(5,3)*t(5,3)
      if(ncorru.eq.0) write(34,10070) etl,idum,iiii,zero,bexi,bezii,phi
      if(ncorru.eq.0)                                                   &
     &write(*,10060)
!-----------------------------------------------------------------------
      return
10000 format(t5 ,'---- ENTRY LINOPT ----')
10010 format(132('-'))
10020 format('  NR     TYP      L-TOTAL    P     PHI          ',        &
     &'BETA         ALFA         GAMMA        DIS        DISP         ',&
     &'CLO        CLOP'/ 1x,                                            &
     &'                    (M)           (2*PI)        ',               &
     &'(M)          (RAD)         (M)         (M)        (RAD)        ',&
     &'(MM)       (MRAD)')
10030 format('  LINEAR OPTICS CALCULATION WITH PRINTOUT ',              &
     &'AFTER EACH BLOCK'/                                               &
     &'   A T T E N T I O N : BETATRON PHASE CALCULATION MIGHT BE WRONG'&
     &,' BY A MULTIPLE OF 0.5 FOR EACH LARGE BLOCK'/)
10040 format(/10x,'RELATIVE ENERGY DEVIATION  ',t40,f10.7/ 10x,         &
     &'TUNES -HORIZONTAL',t40,f10.7/ 10x,'      -VERTICAL  ',t40,f10.7/)
10050 format(t8,'  PLANE          DISP(MM)                 DISP(MRAD)'/ &
     &t6,'      X  ',2(f20.12,6x)/t10,'  Y  ',2(f20.12,6x)/)
10060 format(//131('-')//)
10070 format(1x,1pg21.14,1x,a,1x,i4,5(1x,1pg21.14))
      end
      subroutine writelin(nr,typ,tl,p1,t,ixwl,ielem)
!-----------------------------------------------------------------------
!  WRITE OUT LINEAR OPTICS PARAMETERS
!-----------------------------------------------------------------------
      implicit none
      integer i,istart,iwrite,ixwl,l,ll,nr
      double precision al1,al2,b1,b2,c,cp,d,dp,g1,g2,p1,t,tl
      character*16 typ
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
      dimension p1(2),t(6,4),b1(2),b2(2),al1(2),al2(2),g1(2),g2(2)
      dimension d(2),dp(2),c(2),cp(2)
      integer max_ncoll,max_npart,maxn,numeff,outlun,nc
!UPGRADE January 2005
!     PARAMETER (MAX_NCOLL=68,MAX_NPART=20000,nc=32,NUMEFF=19,
!      parameter (max_ncoll=75,max_npart=20000,nc=32,numeff=19,          &
      parameter (max_ncoll=100,max_npart=20000,nc=32,numeff=19,          &
     &maxn=20000,outlun=54)
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
      integer ielem
      save
!-----------------------------------------------------------------------
      istart=0
      if(typ.eq.'START') istart=1
      iwrite=0
      if(nlin.eq.0) then
        iwrite=1
      else
        do 10 i=1,nlin
          if(typ.eq.bezl(i)) iwrite=1
   10   continue
      endif
      if(iwrite.eq.1) then
        do 20 l=1,2
          ll=2*l
          b1(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
          b2(l)=t(6-ll,ll-1)*t(6-ll,ll-1)+t(7-ll,ll-1)*t(7-ll,ll-1)
          al1(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
          al2(l)=-(t(6-ll,ll-1)*t(6-ll,ll)+t(7-ll,ll-1)*t(7-ll,ll))
          g1(l)=t(ll,ll)*t(ll,ll)+t(ll+1,ll)*t(ll+1,ll)
          g2(l)=t(6-ll,ll)*t(6-ll,ll)+t(7-ll,ll)*t(7-ll,ll)
          d(l)=t(6,ll-1)*c1m3
          dp(l)=t(6,ll)*c1m3
          c(l)=t(1,ll-1)
          cp(l)=t(1,ll)
   20   continue
        tbetax(max(ielem,1))  = b1(1)
        tbetay(max(ielem,1))  = b1(2)
        talphax(max(ielem,1)) = al1(1)
        talphay(max(ielem,1)) = al1(2)
        torbx(max(ielem,1))   = c(1)
        torbxp(max(ielem,1))  = cp(1)
        torby(max(ielem,1))   = c(2)
        torbyp(max(ielem,1))  = cp(2)
        tdispx(max(ielem,1))  = d(1)
        tdispy(max(ielem,1))  = d(2)
      if(ncorru.eq.0) then
          write(*,10000) nr,typ(:8),tl,p1(1),b1(1),al1(1),g1(1),d(1),   &
     &dp(1),c(1),cp(1)
          write(*,10010) b2(1),al2(1),g2(1)
          write(*,10030) typ(9:16)
          write(*,10020) p1(2),b1(2),al1(2),g1(2),d(2),dp(2),           &
     &c(2),cp(2)
          write(*,10010) b2(2),al2(2),g2(2)
          write(*,10040)
        else
          if(kp(ixwl).eq.3) then
            nhmoni=nhmoni+1
            betam(nhmoni,1)=b1(1)
            pam(nhmoni,1)=p1(1)*2*pi
            bclorb(nhmoni,1)=c(1)
          else if(kp(ixwl).eq.4) then
            nhcorr=nhcorr+1
            betac(nhcorr,1)=b1(1)
            pac(nhcorr,1)=p1(1)*2*pi
          else if(kp(ixwl).eq.-3) then
            nvmoni=nvmoni+1
            betam(nvmoni,2)=b1(2)
            pam(nvmoni,2)=p1(2)*2*pi
            bclorb(nvmoni,2)=c(2)
          else if(kp(ixwl).eq.-4) then
            nvcorr=nvcorr+1
            betac(nvcorr,2)=b1(2)
            pac(nvcorr,2)=p1(2)*2*pi
          endif
        endif
      endif
!-----------------------------------------------------------------------
      return
10010 format('|',6x,'|',8x,'|',12x,'|',1x,'|',12x,'|',f12.6,'|', f13.7, &
     &'|',f11.6,'|',11x,'|',11x,'|',11x,'|',11x,'|')
10020 format('|',6x,'|',8x,'|',12x,'|','Y','|',f12.7,'|',f12.6,'|', f13.&
     &7,'|',f11.6,'|',f11.7,'|',f11.7,'|',f11.7,'|',f11.7,'|')
10040 format(132('-'))
10000 format('|',i6,'|',a8,'|',f12.5,'|','X','|',f12.7,'|',f12.6,'|',   &
     &f13.7,'|',f11.6,'|',f11.7,'|',f11.7,'|',f11.7,'|',f11.7,'|')
10030 format('|',6x,'|',a8,'|',12x,'|',102('-'))
      end
      subroutine cpltwis(typ,t,etl,phi)
!-----------------------------------------------------------------------
!  CALCULATES COUPLED TWISS PARAMETERS AROUND THE RING AND ALSO THE
!  ANGLE OF THE MAJOR AXIS OF A ELLIPSE IN THE X-Y PROJECTION WITH
!  THE X-AXIS. THE 4-D ELLIPSOID IS GIVEN BY THE BOUNDARY OF A
!  DISTRIBUTION OF PARTICLES WITH MAXIMUM EMITANCE OF MODE I AND II,
!  EUI AND EUII RESPECTIVELY.
!  BINARY PRINT ON FILE 11 OF 22 VALUES :
!  POSITION [M],
!  BET(1-4), ALF(1-4), GAM(1-4), COOR-PHI(1-4), COOR-PRIME-PHI(1-4),
!  COUUANGL
!-----------------------------------------------------------------------
      implicit none
      integer i,iwrite
      double precision alxi,alxii,alzi,alzii,bexi,bexii,bezi,bezii,     &
     &couuang,etl,gaxi,gaxii,gazi,gazii,phi,phxi,phxii,phxpi,phxpii,    &
     &phzi,phzii,phzpi,phzpii,t
      character*16 typ
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
      dimension t(6,4),phi(2)
      save
!-----------------------------------------------------------------------
      iwrite=0
      if(nlin.eq.0) then
        iwrite=1
      else
        do 10 i=1,nlin
          if(typ.eq.bezl(i)) iwrite=1
   10   continue
      endif
      if(iwrite.eq.1) then
        bexi=t(2,1)*t(2,1)+t(3,1)*t(3,1)
        bexii=t(4,1)*t(4,1)+t(5,1)*t(5,1)
        bezi=t(2,3)*t(2,3)+t(3,3)*t(3,3)
        bezii=t(4,3)*t(4,3)+t(5,3)*t(5,3)
        alxi=-(t(2,1)*t(2,2)+t(3,1)*t(3,2))
        alxii=-(t(4,1)*t(4,2)+t(5,1)*t(5,2))
        alzi=-(t(2,3)*t(2,4)+t(3,3)*t(3,4))
        alzii=-(t(4,3)*t(4,4)+t(5,3)*t(5,4))
        gaxi=t(2,2)*t(2,2)+t(3,2)*t(3,2)
        gaxii=t(4,2)*t(4,2)+t(5,2)*t(5,2)
        gazi=t(2,4)*t(2,4)+t(3,4)*t(3,4)
        gazii=t(4,4)*t(4,4)+t(5,4)*t(5,4)
        if(abs(t(2,1)).gt.pieni) phxi=atan2(t(3,1),t(2,1))
        if(abs(t(4,1)).gt.pieni) phxii=atan2(t(5,1),t(4,1))
        if(abs(t(2,3)).gt.pieni) phzi=atan2(t(3,3),t(2,3))
        if(abs(t(4,3)).gt.pieni) phzii=atan2(t(5,3),t(4,3))
        if(abs(t(2,2)).gt.pieni) phxpi=atan2(t(3,2),t(2,2))
        if(abs(t(4,2)).gt.pieni) phxpii=atan2(t(5,2),t(4,2))
        if(abs(t(2,4)).gt.pieni) phzpi=atan2(t(3,4),t(2,4))
        if(abs(t(4,4)).gt.pieni) phzpii=atan2(t(5,4),t(4,4))
        if(abs(t(2,1)).le.pieni) phxi=pi*half
        if(abs(t(4,1)).le.pieni) then
          if(bexii.gt.pieni) phxii=pi*half
          if(bexii.le.pieni) phxii=zero
        endif
        if(abs(t(2,3)).le.pieni) then
          if(bezi.gt.pieni) phzi=pi*half
          if(bezi.le.pieni) phzi=zero
        endif
        if(abs(t(4,3)).le.pieni) phzii=pi*half
        if(abs(t(2,2)).le.pieni) phxpi=pi*half
        if(abs(t(4,2)).le.pieni) then
          if(gaxii.gt.pieni) phxpii=pi*half
          if(gaxii.le.pieni) phxpii=zero
        endif
        if(abs(t(2,4)).le.pieni) then
          if(gazi.gt.pieni) phzpi=pi*half
          if(gazi.le.pieni) phzpi=zero
        endif
        if(abs(t(4,4)).le.pieni) phzpii=pi*half
        if(abs(eui*(bexi-bezi)+euii*(bexii-bezii)).gt.pieni) then
          couuang=half*atan(two*(eui*sqrt(bexi*bezi)*cos(phxi-phzi)+    &
     &euii*sqrt(bexii*bezii)*cos(phxii-phzii))/ (eui*(bexi-bezi)        &
     &+euii*(bexii-bezii)))
        else
          couuang=zero
        endif
        write(11) typ,etl,phi,bexi,bexii,bezi,bezii, alxi,alxii,alzi,   &
     &alzii, gaxi,gaxii,gazi,gazii,phxi,phxii,phzi,phzii, phxpi,        &
     &phxpii,phzpi,phzpii,couuang,t(6,1),t(6,2),t(6,3),t(6,4),t(1,1),   &
     &t(1,2),t(1,3),t(1,4)
 
      endif
      return
      end
      subroutine loesd (rmat, vec,dimakt,dimtot,kod)
!-----------------------------------------------------------------------
!  SOLUTION OF A SYSTEM OF LINEAR EQUATIONS
!  VEC1 = VEC2 * RMAT , WITH VEC2 AS RESULT
!-----------------------------------------------------------------------
      implicit none
      integer ik,indi,j,jk,jy,k,kk,kod,l,n,n1,dimtot,dimakt
      double precision emax,eps,r,rmat,vec
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
      dimension rmat(dimtot,dimakt),vec(dimakt)
      data eps /1d-20/
      save
!-----------------------------------------------------------------------
      kod=1
      do 50 j=1,dimakt
        emax=zero
        do 10 ik=j,dimakt
          if(abs(emax).gt.abs(rmat(j,ik)) .or.emax.ne.emax) goto 10
          emax=rmat(j,ik)
          indi=ik
   10   continue
        if(abs(emax).lt.eps) then
          write(*,*) '  ****   ERROR IN LOESD   **** '
          return
        endif
   20   do 30 l=j,dimakt
          r=rmat(l,j)
          rmat(l,j)=rmat(l,indi)
          rmat(l,indi)=r
          rmat(l,j)=rmat(l,j)/emax
   30   continue
        r=vec(indi)
        vec(indi)=vec(j)
        vec(j)=r/emax
        if(j.eq.dimakt) goto 60
        jy=j+1
        do 50 jk=jy,dimakt
          r=rmat(j,jk)
          do 40 kk=jy,dimakt
            rmat(kk,jk)= rmat(kk,jk)-r*rmat(kk,j)
   40     continue
          vec(jk)=vec(jk)-vec(j)*r
   50 continue
   60 n=dimakt
      n1=dimakt-1
      do 70 j=1,n1
        do 70 k=1,j
          vec(n-j)=vec(n-j)-rmat(n-k+1,n-j)*vec(n-k+1)
   70 continue
      kod = 0
      return
      end
      subroutine matrix(dpp,am)
      implicit none
      integer i,ierr,l
      double precision am,dpp
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
      dimension am(4,4)
      save
!-----------------------------------------------------------------------
      do 10 i=2,5
        do 10 l=1,2
          x(i,l)=zero
   10 y(i,l)=zero
      x(2,1)=one
      y(3,1)=one
      x(4,2)=one
      y(5,2)=one
      do 20 l=1,2
        x(1,l)=clo(l)
   20 y(1,l)=clop(l)
      call umlauf(dpp,5,ierr)
      ierro=ierr
      do 30 i=1,4
        am(1,i)=x(i+1,1)
        am(2,i)=y(i+1,1)
        am(3,i)=x(i+1,2)
   30 am(4,i)=y(i+1,2)
!-----------------------------------------------------------------------
      return
      end
      subroutine corrorb
!-----------------------------------------------------------------------
!  CORRECTION OF CLOSED ORBIT FIRST (MOST EFFECTIV CORRECTOR STRATEGY
!  USING MICADO), THEN
!  SCALING OF DIPOLE-ERRORS FOR RMS-VALUES OF THE CLOSED ORBIT
!-----------------------------------------------------------------------
      implicit none
      integer i,icflag,ihflag,ii,ij,im,iprinto,ivflag,j,k,kpz,kzz,l,    &
     &nlino,ntcoo,nto,nx
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
      real ar(nmon1,ncor1)
      real b(nmon1),orbr(nmon1),xinc(ncor1)
      real rmsx,ptpx,rmsz,ptpz,rzero,rzero1
      double precision clo0,clop0,hfac,qwc1,vfac
      character*16 bezlo(nele)
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
      dimension clo0(2),clop0(2)
      dimension qwc1(3),nx(ncor1)
      save
!-----------------------------------------------------------------------
      rzero=0.0
      rzero1=0.0
      do 10 l=1,2
        clo0(l)=zero
        clop0(l)=zero
        di0(l)=zero
   10 dip0(l)=zero
      call clorb(ded)
      if(ierro.gt.0) call prror(4)
      do 20 l=1,2
        clo0(l)=clo(l)
   20 clop0(l)=clop(l)
      call clorb(zero)
      if(ierro.gt.0) call prror(5)
      do 30 l=1,2
        di0(l)=(clo0(l)-clo(l))/ded
   30 dip0(l)=(clop0(l)-clop(l))/ded
      do 40 l=1,ncor1
        xinc(l)=0.0
   40 nx(l)=0
      if(iclo.eq.0) return
!-- ORBIT CORRECTION
      ihflag=0
      ivflag=0
      icflag=0
 
      write(*,*)
      write(*,10000)
      if(ncorru.eq.0) then
        call prror(84)
      else
        if(ncorrep.le.0) then
          write(*,10010) ncorru,sigma0(1),sigma0(2)
        else
          write(*,10020) ncorru,ncorrep
        endif
      endif
      write(*,*)
 
!-- SAVE OLD 'LINOPT' SETTINGS
      iprinto=iprint
      nto=nt
      ntcoo=ntco
      do 50 i=1,nlin
        bezlo(i)=bezl(i)
   50 continue
      nlino=nlin
!-- PUT MONITORS AND CORRECTORS INTO LINOPT SETTINGS
!-- GET TWISS PARAMETERS AND DISTORTED ORBIT BACK
      iprint=0
      ntco=0
      nlin=0
      do 60 i=1,il
        if(kp(i).eq.3.or.kp(i).eq.4.or. kp(i).eq.-3.or.kp(i).eq.-4) bezl&
     &(i)=bez(i)
        nlin=nlin+1
   60 continue
      call linopt(zero)
      call phasad(zero,qwc1)
 
!-- CHECK SOME CONDITIONS
      write(*,10100) nhmoni,nhcorr,nvmoni,nvcorr
      if(nhmoni.gt.nmon1) then
        write(*,10070) nhmoni,nmon1
        return
      endif
      if(nvmoni.gt.nmon1) then
        write(*,10070) nvmoni,nmon1
        return
      endif
      if(nhcorr.gt.ncor1) then
        write(*,10080) nhcorr,ncor1
        return
      endif
      if(nvcorr.gt.ncor1) then
        write(*,10080) nvcorr,ncor1
        return
      endif
      if(nhmoni.lt.nhcorr.or.nvmoni.lt.nvcorr) write(*,10090)
 
      write(*,*)
      call orbinit
!-- CORRECT BOTH PLANES
      if(ncorrep.eq.0) then
        icflag=1
        ncorrep=itco
      endif
      do 110 ii=1,ncorrep
!-- HORIZONTAL PLANE FIRST
        do 70 i=1,nhmoni
          b(i)=bclorb(i,1)
          do 70 j=1,nhcorr
            ar(i,j)=sqrt(betam(i,1)*betac(j,1))*cos(abs(pam(i,1)- pac
     &(j,1))-qwc1(1)*pi)*c1e3/(2*sin(qwc1(1)*pi))
   70   continue
        call calrms(b,nhmoni,rmsx,ptpx)
!-- MICADO WITH HOUSEHOLDER TRANSFORMATION
        call htls(ar,b,nhmoni,nhcorr,xinc,nx,orbr,ncorru,rzero,rzero1)
 
!-- VERTICAL PLANE HERE
        do 80 i=1,nvmoni
          b(i)=bclorb(i,2)
          do 80 j=1,nvcorr
            ar(i,j)=sqrt(betam(i,2)*betac(j,2))*cos(abs(pam(i,2)- pac
     &(j,2))-qwc1(2)*pi)*c1e3/(2*sin(qwc1(2)*pi))
   80   continue
        call calrms(b,nvmoni,rmsz,ptpz)
        write(*,10030) ii-1,rmsx,rmsz
        write(*,10040) ii-1,ptpx,ptpz
        if(icflag.eq.1.and.sigma0(1).gt.rmsx.and.ihflag.eq.0) then
          write(*,10110)
          ihflag=1
        endif
        if(icflag.eq.1.and.sigma0(2).gt.rmsz.and.ivflag.eq.0) then
          write(*,10120)
          ivflag=1
        endif
 
        if(ihflag.eq.0) then
          write(*,*)
          do 90 ij=1,ncorru/10
            write(*,10050) (nx(10*(ij-1)+k), k=1,10)
   90     continue
          if(mod(ncorru,10).gt.0) then
            write(*,10050) (nx(10*(ij-1)+k), k=1,mod(ncorru,10))
          endif
          call putorb(xinc,nx,1)
        endif
!-- MICADO WITH HOUSEHOLDER TRANSFORMATION
        call htls(ar,b,nvmoni,nvcorr,xinc,nx,orbr,ncorru,rzero,rzero1)
 
        if(ivflag.eq.0) then
          write(*,*)
          do 100 ij=1,ncorru/10
            write(*,10060) (nx(10*(ij-1)+k), k=1,10)
  100     continue
          if(mod(ncorru,10).gt.0) then
            write(*,10060) (nx(10*(ij-1)+k), k=1,mod(ncorru,10))
          endif
          call putorb(xinc,nx,2)
        endif
 
        if(ihflag.eq.1.and.ivflag.eq.1) goto 140
        call linopt(zero)
        call phasad(zero,qwc1)
  110 continue
 
!-- GET LAST VALUES AFTER CORRECTION
      do 120 i=1,nhmoni
        b(i)=bclorb(i,1)
  120 continue
      call calrms(b,nhmoni,rmsx,ptpx)
      do 130 i=1,nvmoni
        b(i)=bclorb(i,2)
  130 continue
      call calrms(b,nvmoni,rmsz,ptpz)
      write(*,10030) ncorrep,rmsx,rmsz
      write(*,10040) ncorrep,ptpx,ptpz
      write(*,*)
 
  140 continue
      if((ii-1).eq.itco) write(*,10130) itco
 
!-- SCALE TO DESIRED RMS VALUE IF IT IS GREATER THAN ZERO
      if(sigma0(1).gt.pieni.or.sigma0(2).gt.pieni) then
        do 180 ii=1,itco
          write(*,10140)
          hfac=sigma0(1)/rmsx
          vfac=sigma0(2)/rmsz
          do 150 i=1,il
            kzz=kz(i)
            kpz=kp(i)
            if(kzz.eq.1.and.el(i).lt.pieni) then
              ed(i)=ed(i)*hfac
              ek(i)=ek(i)*hfac
            endif
            if(kzz.eq.-1.and.el(i).lt.pieni) then
              ed(i)=ed(i)*vfac
              ek(i)=ek(i)*vfac
            endif
            if(kzz.eq.11) then
              im=irm(i)
              ak0(im,1)=ak0(im,1)*vfac
              aka(im,1)=aka(im,1)*vfac
              bk0(im,1)=bk0(im,1)*hfac
              bka(im,1)=bka(im,1)*hfac
            endif
  150     continue
          call linopt(zero)
          do 160 i=1,nhmoni
            b(i)=bclorb(i,1)
  160     continue
          call calrms(b,nhmoni,rmsx,ptpx)
          do 170 i=1,nvmoni
            b(i)=bclorb(i,2)
  170     continue
          call calrms(b,nvmoni,rmsz,ptpz)
          write(*,10150) ii,rmsx,rmsz
          write(*,10160) ii,ptpx,ptpz
          write(*,*)
          if(abs(rmsx-sigma0(1)).lt.dsi.and. abs(rmsz-sigma0(2)).lt.dsi)&
     &goto 190
  180   continue
      endif
      if((ii-1).eq.itco) write(*,10130) itco
  190 continue
 
!-- WRITE OUT ADJUSTED CLOSED ORBIT
      do 200 i=1,nhmoni
        write(28,*) i,bclorb(i,1)
  200 continue
      do 210 i=1,nhmoni
        write(29,*) i,bclorb(i,2)
  210 continue
 
!-- CHANGE BACK TO OLD 'LINOPT' SETTINGS
      iprint=iprinto
      nt=nto
      ntco=ntcoo
      nlin=nlino
      do 220 i=1,nlin
        bezl(i)=bezlo(i)
  220 continue
      ncorru=0
!-----------------------------------------------------------------------
      return
10000 format(t5,'---- ORBIT CORRECTION WITH MOST EFFCTIVE CORRECTOR ',  &
     &'STRATEGY ----')
10010 format(t5,'     ORBIT CORRECTION WITH ',i4,' CORRECTORS UNTIL',/, &
     &t5,'       HOR. RMS SMALLER THAN ',f6.3,' MM',/, t5,              &
     &'       VER. RMS SMALLER THAN ',f6.3,' MM')
10020 format(t5,'     ORBIT CORRECTION WITH ',i4,' CORRECTORS AND ',i4, &
     &' ITERATIONS.')
10030 format(t5,'---- CORRECTION ITERATION NO. ',i4,' HOR.-RMS: ',f6.3, &
     &' VER.-RMS: ',f6.3)
10040 format(t5,'---- CORRECTION ITERATION NO. ',i4,' HOR.-PTP: ',f6.3, &
     &' VER.-PTP: ',f6.3)
10050 format(t5,'     HORIZONTAL CORRECTORS USED:', i4,i4,i4,i4,i4,i4,  &
     &i4,i4,i4,i4)
10060 format(t5,'     VERTICAL   CORRECTORS USED:', i4,i4,i4,i4,i4,i4,  &
     &i4,i4,i4,i4)
10070 format(/,t5,'ERROR: NUMBER OF MONITORS TOO BIG.',/                &
     &'    THERE ARE ',i4,' MONITORS SET, BUT ONLY ',i4, ' ALLOWED.',/  &
     &'    NO CORRECTION DONE.',/)
10080 format(/,t5,'ERROR: NUMBER OF CORRECTORS TOO BIG.',/              &
     &'    THERE ARE ',i4,' MONITORS SET, BUT ONLY ',i4, ' ALLOWED.',/  &
     &'    NO CORRECTION DONE.',/)
10090 format(/,t5,'WARNING: NUMBER OF MONITORS IS SMALLER THAN NUMBER', &
     &' OF CORRECTORS.',/ '    NUMERICAL PROBLEMS MIGHT BE ENCOUNTERED.'&
     &)
10100 format(/,t5,'NUMBER OF HOR. MONITORS: ',i4,                       &
     &'  NUMBER OF HOR. CORRECTORS: ',i4,/, t5,                         &
     &'NUMBER OF VER. MONITORS: ',i4, '  NUMBER OF VER. CORRECTORS: ',  &
     &i4)
10110 format(t10,'HORIZONTAL RMS GOAL REACHED')
10120 format(t10,'VERTICAL RMS GOAL REACHED')
10130 format(t10,'MAXIMUM NUMBER OF ITERATIONS ACHIVED: ',i4,/ ,t10,    &
     &'INCREASE ITCO TO INCREASE THE NUMBER OF ' ,                      &
     &'CLOSED ORBIT ITERATIONS',/)
10140 format(t5,'---- ORBIT SCALING USING ALL POSSIBLE ELEMENTS ')
10150 format(t5,'---- SCALING ITERATION NO. ',i4,' HOR.-RMS: ',f6.3,    &
     &' VER.-RMS: ',f6.3)
10160 format(t5,'---- SCALING ITERATION NO. ',i4,' HOR.-PTP: ',f6.3,    &
     &' VER.-PTP: ',f6.3)
      end
      subroutine putorb(xinc,nx,npflag)
!-----------------------------------------------------------------------
!  PUT ORBIT CHANGES FROM MICADO TO THE GIVEN ORBIT CORRECTORS
!-----------------------------------------------------------------------
      implicit none
      integer i,im,ix,izu,j,k,kcorr,kcorru,kpz,kzz,nmz,npflag,nx
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
      real xinc(ncor1)
      double precision ckicknew,ckickold,r0,r0a
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
      dimension nx(ncor1)
      save
!-----------------------------------------------------------------------
      kcorru=0
      kcorr=0
      izu=0
 
      do 60 i=1,iu
        ix=ic(i)
        if(ix.le.nblo) goto 60
        ix=ix-nblo
        kpz=kp(ix)
        kzz=kz(ix)
        if(kpz.eq.6.or.kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 60
        if(iorg.lt.0) mzu(i)=izu
        izu=mzu(i)+1
        if(kpz.eq.4.and.kzz.eq.1.and.npflag.eq.1.or.                    &
     &kpz.eq.-4.and.kzz.eq.-1.and.npflag.eq.2) then
          kcorr=kcorr+1
          do 10 j=1,ncorru
            if(nx(j).eq.kcorr) then
              kcorru=kcorru+1
              ckickold=sm(ix)+zfz(izu)*ek(ix)
              zfz(izu)=zfz(izu)+xinc(j)/ek(ix)
              ckicknew=sm(ix)+zfz(izu)*ek(ix)
              write(*,10000) kcorru,kcorr,bez(ix), ckickold*c1e3,       &
     &ckicknew*c1e3
            endif
   10     continue
        endif
        izu=izu+2
 
        if(kzz.eq.11) then
          r0=ek(ix)
          if(abs(r0).le.pieni) goto 60
          nmz=nmu(ix)
          if(nmz.eq.0) then
            izu=izu+2*mmul
            goto 60
          endif
          im=irm(ix)
          r0a=one
          do 50 k=1,nmz
            izu=izu+1
            if(kpz.eq.-4.and.npflag.eq.2.and.k.eq.1) then
              kcorr=kcorr+1
              do 30, j=1,ncorru
                if(nx(j).eq.kcorr) then
                  kcorru=kcorru+1
                  ckickold=ed(ix)*(ak0(im,k)+zfz(izu)* aka(im,k))/r0a
                  zfz(izu)=zfz(izu)+c1e3* (xinc(j)/(r0a*ed(ix))-ak0     &
     &(im,k))/aka(im,k)
                  ckicknew=ed(ix)*(ak0(im,k)+zfz(izu)* aka(im,k))/r0a
                  write(*,10000) kcorru,kcorr,bez(ix), ckickold,ckicknew
                endif
   30         continue
            endif
            izu=izu+1
            if(kpz.eq.4.and.npflag.eq.1.and.k.eq.1) then
              kcorr=kcorr+1
              do 40, j=1,ncorru
                if(nx(j).eq.kcorr) then
                  kcorru=kcorru+1
                  ckickold=ed(ix)*(bk0(im,k)+zfz(izu)* bka(im,k))/r0a
                  zfz(izu)=zfz(izu)+c1e3* (xinc(j)/(r0a*ed(ix))-bk0     &
     &(im,k))/bka(im,k)
                  ckicknew=ed(ix)*(bk0(im,k)+zfz(izu)* bka(im,k))/r0a
                  write(*,10000) kcorru,kcorr,bez(ix), ckickold,ckicknew
                endif
   40         continue
            endif
   50     continue
          izu=izu+2*mmul-2*nmz
        endif
   60 continue
!-----------------------------------------------------------------------
      return
10000 format(t5,i4,i4,' ',a16,'  OLD: ',d13.7,' MRAD   NEW: ' ,d13.7,   &
     &' MRAD')
      end
      subroutine orbinit
!-----------------------------------------------------------------------
!  INITIALIZES THE RANDOM NUMBER OF NOT SET CORRCTORS
!-----------------------------------------------------------------------
      implicit none
      integer i,im,ix,izu,kpz,kzz,nmz
      double precision r0
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
      save
!-----------------------------------------------------------------------
      izu=0
      do 10 i=1,iu
        ix=ic(i)
        if(ix.le.nblo) goto 10
        ix=ix-nblo
        kpz=kp(ix)
        kzz=kz(ix)
        if(kpz.eq.6.or.kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 10
        if(iorg.lt.0) mzu(i)=izu
        izu=mzu(i)+1
        if((kpz.eq.4.and.kzz.eq.1).or.(kpz.eq.-4.and.kzz.eq.-1)) then
          zfz(izu)=zero
          ek(ix)=one
          ncororb(ix)=1
        endif
        izu=izu+2
 
        if(kzz.eq.11) then
          r0=ek(ix)
          if(abs(r0).le.pieni) goto 10
          nmz=nmu(ix)
          if(nmz.eq.0) then
            izu=izu+2*mmul
            goto 10
          endif
          im=irm(ix)
 
          izu=izu+1
          if(kpz.eq.-4) then
            zfz(izu)=zero
            aka(im,1)=one
          endif
          izu=izu+1
          if(kpz.eq.4) then
            zfz(izu)=zero
            bka(im,1)=one
          endif
          izu=izu+2*mmul-2
        endif
   10 continue
      return
      end
      subroutine htls(a,b,m,n,x,ipiv,r,iter,rms,ptp)
!*********************************************************************
!     Subroutine HTLS to make Householder transform                  *
!                                                                    *
!     Authors:     many                Date:  17.09.1989             *
!                                                                    *
!     DIMENSION OF ARRAY RHO SHOULD BE 3*NCOR1                       *
!     M    - NUMBER OF AVAILABLE MONITORS                            *
!     N    - NUMBERR OF AVAILABLE INDEPENDENT CORRECTORS             *
!     ITER - NUMBER OF CORRECTORS TO BE USED                         *
!     RMS  - RMS VALUE TO CORRECT FOR                                *
!     PTP  - PEAK TO PEAK VALUE TO CORRECT FOR                       *
!*********************************************************************
      implicit none
      integer i,iii,ij1,ip,ipiv,iter,j,j1,k,k2,k3,ki,kk,kpiv,m,n,ncor1, &
     &nmon1
      real a,b,piv,pivt,ptop,r,rho,rmss,x,xiter,xptp,xrms
      real rms,ptp
      real g,h,sig,beta
      parameter (nmon1 = 600)
      parameter (ncor1 = 600)
      dimension a(nmon1,ncor1),b(nmon1),x(ncor1),ipiv(ncor1),r(nmon1)
      dimension rho(3*ncor1),xiter(ncor1),xrms(ncor1),xptp(ncor1)
      dimension rmss(ncor1),ptop(ncor1)
      save
!-----------------------------------------------------------------------
 
! --- calcul du premier pivot
 
!============================
      beta=0.0
 
      do 10 ij1=1,500
   10 rho(ij1)=0.0
 
      k2=n + 1
      piv=0.0d0
 
      do 40 k=1,n
        ipiv(k)=k
        h=0.0d0
        g=0.0d0
        do 20 i=1,m
          h=h+a(i,k)*a(i,k)
          g=g+a(i,k)*b(i)
   20   continue
        rho(k)=h
        rho(k2) = g
        pivt = g*g/h
        if(pivt-piv.le.0) goto 40
        if(pivt-piv.gt.0) goto 30
   30   piv = pivt
        kpiv=k
   40 k2 = k2 + 1
 
! --- boucle pour chaque iteration
 
      do 150 k=1,iter
        if(kpiv.eq.k)goto 60
 
! --- on echange les K et KPIV si KPIV plus grand que K
        h=rho(k)
        rho(k)=rho(kpiv)
        rho(kpiv)=h
        k2=n+k
        k3=n+kpiv
        g = rho(k2)
        rho(k2) = rho(k3)
        rho(k3) = g
        do 50 i=1,m
          h=a(i,k)
          a(i,k)=a(i,kpiv)
          a(i,kpiv)=h
   50   continue
 
! --- calcul de beta,sigma et uk dans htul
   60   continue
        call htul(a,m,n,k,sig,beta)
 
! --- on garde SIGMA dans RHO(N+K)
        j=n+k
        rho(j)=-sig
        ip=ipiv(kpiv)
        ipiv(kpiv)=ipiv(k)
        ipiv(k)=ip
        if(k.eq.n) goto 70
 
! --- transformation de A dans HTAL
        call htal(a,m,n,k,beta)
 
! --- transformation de B dans HTBL
   70   continue
        call htbl(a,b,m,n,k,beta)
 
! --- recherche du pivot (K+1)
!=============================
 
        rho(k)=sqrt(piv)
        if(k.eq.n) goto 90
        piv=0.0d0
        kpiv = k + 1
        j1 = kpiv
        k2=n + j1
        do 80 j=j1,n
          h=rho(j)-(a(k,j))*(a(k,j))
 
          if(h.lt.0.0000001) then
            write(*,*)
            write(*,*) 'CORRECTION PROCESS ABORTED.'
            write(*,*) 'DIVISION BY ZERO EXPECTED.'
            write(*,*) 'PROBABLY TWO CORRECTORS TOO CLOSE.'
            write(*,10000) ' SUSPECTED CORRECTOR: ',j
!-----------------------------------------------------------------------
!--CLOSE(DATA FILES
      close(2)
      close(3)
      close(4)
      close(7)
      close(8)
      close(9)
      close(10)
      close(11)
      close(12)
      close(13)
      close(14)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)
      close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      close(26)
      close(27)
      close(32)
      close(33)
      close(34)
      close(59)
      close(60)
      close(61)
      close(62)
      close(63)
      close(64)
      close(65)
      close(66)
      close(67)
      close(68)
      close(69)
      close(70)
      close(71)
      close(72)
      close(73)
      close(74)
      close(75)
      close(76)
      close(77)
      close(78)
      close(79)
      close(80)
      close(81)
      close(82)
      close(83)
      close(84)
      close(85)
      close(86)
      close(87)
      close(88)
      close(89)
      close(90)
      close(98)
      close(99)
            stop 777
          endif
 
          rho(j)=h
          g=rho(k2)-(a(k,j))*(b(k))
          rho(k2) = g
          pivt = g*g/h
          if(pivt.lt.piv)goto 80
          kpiv=j
          piv=pivt
   80   k2 = k2 + 1
 
! --- calcul des X
   90   x(k)=b(k)/rho(n+k)
        if(k.eq.1)goto 120
        do 110 i=2,k
          kk=k-i+1
          x(kk)=b(kk)
          ki=kk+1
          do 100 j=ki,k
  100     x(kk)=x(kk)-a(kk,j)*x(j)
          x(kk)=x(kk)/rho(n+kk)
  110   continue
  120   continue
 
! --- save residual orbit and inverse sign of corrections (convention!)
        do 130 iii= 1,m
  130   r(iii) = b(iii)
        do 140 iii= 1,k
  140   x(iii) =-x(iii)
 
! --- calcul du vecteur residuel dans HTRL
!=========================================
 
!     transform orbit R back to "normal space"
        call htrl(a,r,m,n,k,rho)
        call calrms(r,m,rmss(k),ptop(k))
        xiter(k+1) = k
        xrms(k+1) = rmss(k)
        xptp(k+1) = ptop(k)
 
        if(ptop(k).le.ptp)goto 160
        if(rmss(k).le.rms)goto 160
  150 continue
      return
 
! --- correction is already good enough:
!=======================================
 
  160 ptp=ptop(k)
      rms=rmss(k)
10000 format(a,i4)
      end
      subroutine htal(a,m,n,k,beta)
!*********************************************************************
!     Subroutine HTAL to make Householder transform                  *
!                                                                    *
!     Authors:     many                Date:  17.09.1989             *
!                                                                    *
!     Householder transform of matrix A
!*********************************************************************
      implicit none
      integer j,k,k1,m,n,nc,ncor1,nmon1
      real a,beta,h
      parameter (nmon1 = 600)
      parameter (ncor1 = 600)
      dimension a(nmon1,ncor1)
      save
!-----------------------------------------------------------------------
 
      nc=n-k
 
      do 20 j=1,nc
        h=0.0d0
 
        do 10 k1=k,m
   10   h=h+a(k1,k)*a(k1,k+j)
 
        h=beta*h
        do 20 k1=k,m
   20 a(k1,k+j)=a(k1,k+j)-a(k1,k)*h
 
      end
      subroutine htbl(a,b,m,n,k,beta)
!*********************************************************************
!     Subroutine HTBL to make Householder transform                  *
!                                                                    *
!     Authors:     many                Date:  17.09.1989             *
!                                                                    *
!     Householder transform of vector B
!*********************************************************************
      implicit none
      integer k,k1,m,n,ncor1,nmon1
      real a,b,beta,h
      parameter (nmon1 = 600)
      parameter (ncor1 = 600)
      dimension a(nmon1,ncor1),b(nmon1)
      save
!-----------------------------------------------------------------------
 
      h=0.0d0
 
      do 10 k1=k,m
   10 h=h+a(k1,k)*b(k1)
 
      h=beta*h
 
      do 20 k1=k,m
   20 b(k1)=b(k1)-a(k1,k)*h
 
      end
      subroutine htrl(a,b,m,n,k,rho)
!*********************************************************************
!     Subroutine HTRL to make Householder transform                  *
!                                                                    *
!     Authors:     many                Date:  17.09.1989             *
!                                                                    *
!     calculate residual orbit vector
!*********************************************************************
      implicit none
      integer i,k,kk,kl,kn,lv,m,n,ncor1,nmon1
      real a,b,beta,rho
      parameter (nmon1 = 600)
      parameter (ncor1 = 600)
      dimension a(nmon1,ncor1),b(nmon1),rho(3*ncor1)
      save
!-----------------------------------------------------------------------
 
      do 10 i= 1,k,1
        b(i)= 0.0d0
   10 continue
 
      do 20 kk=1,k
        lv=m-k+kk
        kn=n+k-kk+1
        kl=k-kk+1
 
        beta=-1d0/(rho(kn)*a(kl,kl))
        call htbl(a,b,m,n,kl,beta)
   20 continue
 
      end
      subroutine htul(a,m,n,k,sig,beta)
!*********************************************************************
!     Subroutine HTUL to make Householder transform                  *
!                                                                    *
!     Authors:     many                Date:  17.09.1989             *
!                                                                    *
!     calculate vector U
!*********************************************************************
      implicit none
      integer i,k,m,n,ncor1,nmon1
      real a,beta,h,sig
      parameter (nmon1 = 600)
      parameter (ncor1 = 600)
      dimension a(nmon1,ncor1)
      save
!-----------------------------------------------------------------------
      sig=0.0d0
 
      do 10 i=k,m
        sig=sig+a(i,k)* a(i,k)
   10 continue
 
      sig=sqrt(sig)
!     on choisit le signe correct pour SIG:
      h=a(k,k)
      if(h.lt.0.0d0)sig=-sig
      beta=h + sig
      a(k,k)=beta
      beta=1d0/(sig*beta)
      end
      subroutine calrms(r,m,rms,ptp)
!*********************************************************************
!     Subroutine CALRMS to calculate rms                             *
!                                                                    *
!     Authors:     many                Date:  17.09.1989             *
!                                                                    *
!     calculates rms and p.to.p value of R(1) .... R(M)
!*********************************************************************
      implicit none
      integer i,imax,imin,m,maxmin
      real ave,ptp,r,rms,xave,xrms
      dimension r(m)
      save
!-----------------------------------------------------------------------
      xave = 0.0
      xrms = 0.0
 
      do 10 i=1,m
        xave = xave + r(i)
        xrms = xrms + (r(i)*r(i))
   10 continue
 
      ave = xave / real(m)
      rms = xrms / real(m)
 
      imax=maxmin(r(1),m,1)
      imin=maxmin(r(1),m,0)
      ptp=r(imax)-r(imin)
      rms=sqrt(rms)
      return
      end
      function maxmin (a,n,m)
!-----------------------------------------------------------------------
!     if M=0, MAXMIN=lowest index of minimum element in A
!     if M=1, MAXMIN=lowest index of maximun element in A
!     if N<1, MAXMIN=1
!-----------------------------------------------------------------------
      implicit none
      integer i,m,maxmin,n
      real a,curent
      dimension a(n)
      save
!-----------------------------------------------------------------------
      maxmin=1
      if (n.lt.1) return
      curent=a(1)
      do 10 i=2,n
        if ((m.eq.0).and.(a(i).ge.curent)) goto 10
        if ((m.eq.1).and.(a(i).le.curent)) goto 10
        curent=a(i)
        maxmin=i
   10 continue
      return
      end
      subroutine ord
!-----------------------------------------------------------------------
!  ORGANISATION OF BLOCKS, NONLINEAR ELEMENTS AND RANDOM NUMBERS
!-----------------------------------------------------------------------
      implicit none
      integer i,icext1,icextal1,ihi,ii,ilf,ilfr,inz,iran,ix,izu,j,jra,  &
     &jra3,kanf1,kpz,kzz,kzz1,kzz2,nra1
      double precision extalig1,exterr1
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
      dimension ilf(nblz),ilfr(nblz),jra(nele,5),iran(nele),inz(nele)
      dimension exterr1(nblz,40),extalig1(nblz,3),icext1(nblz),         &
     &icextal1(nblz)
      save
!-----------------------------------------------------------------------
      do 10 i=1,nblz
        ilf(i)=0
        ilfr(i)=0
   10 continue
      do 20 i=1,nele
        iran(i)=0
        inz(i)=0
        do 20 j=1,5
          jra(i,j)=0
   20 continue
      if(mper.eq.1) goto 40
      do 30 i=2,mper
        do 30 j=1,mbloz
          ii=(i-1)*mbloz+j
          ihi=j
          if(msym(i).lt.0) ihi=mbloz-j+1
          ic(ii)=msym(i)*ic(ihi)
   30 if(ic(ii).lt.-nblo) ic(ii)=-ic(ii)
!--ORGANISATION OF RANDOM NUMBERS
   40 iu=mper*mbloz
      if(niu(1).lt.0) niu(1)=iabs(niu(1))
      if(niu(2).lt.0) niu(2)=iabs(niu(2))
      if(niu(1).eq.0) niu(1)=1
      if(niu(2).eq.0) niu(2)=iu
      if(niu(1).gt.iu) niu(1)=1
      if(niu(2).gt.iu) niu(2)=iu
      izu=0
      nra1=nran
      iorg=iorg-1
      if(iorg.ge.0) then
        if(iorg.eq.0) then
          do 50 i=1,iu
            ix=ic(i)
            if(ix.le.nblo) goto 50
            ix=ix-nblo
            kpz=kp(ix)
            kzz=kz(ix)
            if(kpz.eq.6.or.kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 50
            mzu(i)=izu
            izu=izu+3
            if(kzz.eq.11.and.abs(ek(ix)).gt.pieni) izu=izu+2*mmul
            if(izu.gt.nran) call prror(30)
   50     continue
        else
          do 70 i=1,iorg
            do 60 j=1,il
              if(bez(j).eq.bezr(1,i)) then
                jra(i,1)=j
                if(kz(j).eq.0.or.kz(j).eq.20.or.kz(j).eq.22)            &
     &call prror(31)
                jra(i,2)=kz(j)
              endif
              if(bez(j).eq.bezr(2,i)) then
                jra(i,3)=j
                if(kz(j).eq.0.or.kz(j).eq.20.or.kz(j).eq.22)            &
     &call prror(31)
                jra(i,4)=kz(j)
              endif
   60       continue
            kzz1=jra(i,2)
            kzz2=jra(i,4)
            if(kzz1.ne.0.and.kzz2.eq.0) then
              jra(i,5)=nra1
              nra1=nra1+mran*3
              if(kzz1.eq.11.and.abs(ek(jra(i,1))).gt.pieni) nra1=nra1   &
     &+mran*2*mmul
              if(nra1.gt.nzfz) call prror(32)
            endif
            if(kzz1.eq.11.and.(kzz2.ne.11.and.kzz2.ne.0)) call prror(33)
   70     continue
          do 110 i=1,iu
            ix=ic(i)
            if(ix.le.nblo) goto 110
            ix=ix-nblo
            kpz=kp(ix)
            kzz=kz(ix)
            if(kpz.eq.6.or.kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 110
            do 80 j=1,iorg
              if(bez(ix).eq.bezr(1,j)) goto 90
   80       continue
            goto 100
   90       jra3=jra(j,3)
            if(jra3.ne.0) then
              mzu(i)=iran(jra3)
              iran(ix)=mzu(i)
            else
              inz(j)=inz(j)+1
              if(inz(j).gt.mran) call prror(34)
              mzu(i)=jra(j,5)
              iran(ix)=mzu(i)
              jra(j,5)=jra(j,5)+3
              if(jra(j,2).eq.11) jra(j,5)=jra(j,5)+2*mmul
            endif
            goto 110
  100       mzu(i)=izu
            iran(ix)=izu
            izu=izu+3
            if(kzz.eq.11.and.abs(ek(ix)).gt.pieni) izu=izu+2*mmul
            if(izu.gt.nran) call prror(30)
  110     continue
        endif
      else
        do 115 i=1,iu
          ix=ic(i)
          if(ix.le.nblo) goto 115
          ix=ix-nblo
          kpz=kp(ix)
          kzz=kz(ix)
          if(kpz.eq.6.or.kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 115
          izu=izu+3
          if(kzz.eq.11.and.abs(ek(ix)).gt.pieni) izu=izu+2*mmul
          if(izu.gt.nran) call prror(30)
  115   continue
      endif
      if(kanf.ne.1) then
!--UMSPEICHERUNG AUF DEN STARTPUNKT
        kanf1=kanf-1
        do 130 i=1,kanf1
          if(iorg.ge.0) ilfr(i)=mzu(i)
          ilf(i)=ic(i)
          icext1(i)=icext(i)
          icextal1(i)=icextal(i)
          extalig1(i,1)=extalign(i,1)
          extalig1(i,2)=extalign(i,2)
          extalig1(i,3)=extalign(i,3)
          do 120 ii=1,40
            exterr1(i,ii)=exterr(i,ii)
  120     continue
  130   continue
        do 150 i=kanf,iu
          if(iorg.ge.0) mzu(i-kanf1)=mzu(i)
          ic(i-kanf1)=ic(i)
          icext(i-kanf1)=icext(i)
          icextal(i-kanf1)=icextal(i)
          extalign(i-kanf1,1)=extalign(i,1)
          extalign(i-kanf1,2)=extalign(i,2)
          extalign(i-kanf1,3)=extalign(i,3)
          do 140 ii=1,40
            exterr(i-kanf1,ii)=exterr(i,ii)
  140     continue
  150   continue
        do 170 i=1,kanf1
          if(iorg.ge.0) mzu(iu-kanf1+i)=ilfr(i)
          ic(iu-kanf1+i)=ilf(i)
          icext(iu-kanf1+i)=icext1(i)
          icextal(iu-kanf1+i)=icextal1(i)
          extalign(iu-kanf1+i,1)=extalig1(i,1)
          extalign(iu-kanf1+i,2)=extalig1(i,2)
          extalign(iu-kanf1+i,3)=extalig1(i,3)
          do 160 ii=1,40
            exterr(iu-kanf1+i,ii)=exterr1(i,ii)
  160     continue
  170   continue
      endif
      izu=0
      do 190 i=1,iu
        ix=ic(i)
        if(ix.le.nblo) goto 190
        ix=ix-nblo
        kpz=kp(ix)
        kzz=kz(ix)
        if(kpz.eq.6.or.kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 190
        if(icextal(i).ne.0) then
          izu=izu+2
          xrms(ix)=one
          zrms(ix)=one
          zfz(izu)=extalign(i,1)
          izu=izu+1
          zfz(izu)=extalign(i,2)
          tiltc(i)=cos(extalign(i,3)*c1m3)
          tilts(i)=sin(extalign(i,3)*c1m3)
        else
          izu=izu+3
        endif
        if(kzz.eq.11.and.abs(ek(ix)).gt.pieni.and.icext(i).ne.0) then
          do 180 j=1,mmul
            izu=izu+1
            zfz(izu)=exterr(i,20+j)
            izu=izu+1
            zfz(izu)=exterr(i,j)
  180     continue
        else if(kzz.eq.11.and.abs(ek(ix)).gt.pieni.and.                 &
     &icext(i).eq.0) then
          izu=izu+2*mmul
        endif
  190 continue
      return
      end
      subroutine phasad(dpp,qwc)
!-----------------------------------------------------------------------
!  ADDITIONAL ADJUSTMENT OF THE X-PHASEADVANCE BETWEEN 2 POSITIONS
!-----------------------------------------------------------------------
      implicit none
      integer i,ikpv,im,ium,ix,izu,j,jj,jk,jm,k,kpv,kpz,kzz,l,l1,ll,nmz,&
     &dj
      double precision aa,alfa,bb,benkr,beta,ci,cikve,cr,crkve,crkveuk, &
     &dphi,dpp,dppi,dpr,dyy1,dyy2,ekk,phi,phibf,pie,puf,qu,qv,qw,qwc,   &
     &qxsa,qxse,r0,r0a,t,xl,xs,zl,zs
      double precision dyy11,qu1,tiltck,tiltsk
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
      dimension t(5,4)
      dimension beta(2),alfa(2),phi(2),phibf(2)
      dimension qw(2),qwc(3)
      dimension aa(mmul),bb(mmul),dpr(5)
      dimension cr(mmul),ci(mmul)
      save
!-----------------------------------------------------------------------
      ium=5
!GRD
      qxsa = zero
      qxse = zero
!GRD
      do 10 i=1,ium
        dpr(i)=zero
   10 continue
      do 20 i=1,ium
        do 20 j=1,4
          t(i,j)=zero
   20 continue
      do 30 i=1,2
        beta(i)=zero
        alfa(i)=zero
        phi(i)=zero
        phibf(i)=zero
        qw(i)=zero
        qwc(i)=zero
   30 continue
      qwc(3)=zero
      do 40 i=1,mmul
        aa(i)=zero
        bb(i)=zero
        cr(i)=zero
        ci(i)=zero
   40 continue
      pie=two*pi
      ikpv=0
      dpr(1)=dpp*c1e3
      call clorb(dpp)
      call betalf(dpp,qw)
      if(ierro.ne.0) call prror(22+ierro)
      call envar(dpp)
!--STARTVALUES OF THE TRAJECTORIES
      do 50 l=1,2
        ll=2*l
        alfa(l)=alf0(l)
        beta(l)=bet0(l)
        t(1,ll-1)=clo(l)
   50 t(1,ll)=clop(l)
      do 60 i=1,4
        do 60 j=1,4
          t(i+1,j)=ta(j,i)
   60 t(i+1,j)=ta(j,i)
!--SINGLE TURN BLOCKLOOP
      izu=0
      do 450 k=1,iu
        ix=ic(k)
        if(ix.gt.nblo) goto 140
        jj=0
        dj=1
        if(ix.gt.0) goto 70
        ix=-ix
        jj=mel(ix)+1
        dj=-1
   70   jm=mel(ix)
!--BLOCKELEMENTLOOP
        do 130 j=1,jm
          jj=jj+dj
          jk=mtyp(ix,jj)
          if(ithick.eq.1.and.kz(jk).ne.0) goto 100
          if(ithick.eq.0.and.kz(jk).ne.0) goto 450
!--PURE DRIFTLENGTH
          do 80 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=pi2
            endif
            do 80 i=1,ium
   80     t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
          do 90 l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=pi2-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
   90     phi(l)=phi(l)+dphi/pie
          goto 130
!--MAGNETELEMENT
  100     continue
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a(jk,l,5)
              t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
            enddo
          enddo
          do l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(kz(jk).ne.8.and.-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi/pie
          enddo
  130   continue
        goto 450
!--NL-INSERTION
  140   ix=ix-nblo
        qu=zero
        qv=zero
        dyy1=zero
        dyy2=zero
        kpz=kp(ix)
        if(kpz.eq.6) goto 450
        kzz=kz(ix)
        kpv=kpa(ix)
        if(kpv.ne.1) goto 150
        qxsa=phi(1)
  150   if(kpv.ne.2.or.ikpv.eq.1) goto 160
        qxse=phi(1)
        ikpv=1
  160   continue
        if(kzz.eq.22) then
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*rrtr(imtr(ix),ll-1,ll-1)+                   &
     &t(i,ll)*rrtr(imtr(ix),ll-1,ll)+                                   &
     &dpr(i)*rrtr(imtr(ix),ll-1,6)
              t(i,ll)=puf*rrtr(imtr(ix),ll,ll-1)+                       &
     &t(i,ll)*rrtr(imtr(ix),ll,ll)+                                     &
     &dpr(i)*rrtr(imtr(ix),ll,6)
            enddo
            t(1,ll-1)=t(1,ll-1)+cotr(imtr(ix),ll-1)
            t(1,ll)=t(1,ll)+cotr(imtr(ix),ll)
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi/pie
          enddo
        endif
        if(kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 450
        dyy1=zero
        dyy2=zero
        if(iorg.lt.0) mzu(k)=izu
        izu=mzu(k)+1
        ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
        izu=izu+1
        xs=xpl(ix)+zfz(izu)*xrms(ix)
        izu=izu+1
        zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(t(1,1)-xs)*tiltc(k)+(t(1,3)-zs)*tilts(k)
        zl=-(t(1,1)-xs)*tilts(k)+(t(1,3)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
        if(kzz.lt.0) goto 310
        goto(170,180,190,200,210,220,230,240,250,260,270),kzz
        goto 450
!--HORIZONTAL DIPOLE
  170   ekk=ekk*c1e3
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
        goto 420
!--NORMAL QUADRUPOLE
  180   continue
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        goto 420
!--NORMAL SEXTUPOLE
  190   ekk=ekk*c1m3
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*crkve+tiltsk*cikve)
        qv=ekk*two*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
!--NORMAL OCTUPOLE
  200   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=three*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
!--NORMAL DECAPOLE
  210   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=four*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
!--NORMAL DODECAPOLE
  220   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=5*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
!--NORMAL 14-POLE
  230   ekk=ekk*c1m15
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=6*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
!--NORMAL 16-POLE
  240   ekk=ekk*c1m18
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=7*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
!--NORMAL 18-POLE
  250   ekk=ekk*c1m21
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=8*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
!--NORMAL 20-POLE
  260   ekk=ekk*c1m24
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=9*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
  270   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)                     &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            do 280 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  280       continue
          else
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tiltc(k)                             &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
          endif
        endif
        if(abs(dki(ix,2)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)+(qu*zl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+(-qu*zl+dppi*dpp)*tiltc(k)                    &
     &-dppi*(one-tiltc(k))
            do 290 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  290       continue
          else
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+dppi*dpp*tiltc(k)                             &
     &-dppi*(one-tiltc(k))
          endif
        endif
        if(abs(r0).le.pieni) goto 450
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
          goto 450
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        do 300 l=1,nmz
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
  300   continue
        if(nmz.ge.2) then
          qu=bb(2)
          qv=-aa(2)
          dyy1=bb(1)+bb(2)*crkve+aa(2)*cikve
          dyy2=aa(1)-bb(2)*cikve+aa(2)*crkve
          do 305 l=3,nmz
            l1=l-1
            qu=qu+l1*(bb(l)*crkve+aa(l)*cikve)
            qv=qv+l1*(bb(l)*cikve-aa(l)*crkve)
            crkveuk=crkve*xl-cikve*zl
            cikve=crkve*zl+cikve*xl
            crkve=crkveuk
            dyy1=dyy1+bb(l)*crkve+aa(l)*cikve
            dyy2=dyy2-bb(l)*cikve+aa(l)*crkve
  305     continue
        else
          qu=zero
          qv=zero
          dyy1=bb(1)
          dyy2=aa(1)
        endif
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
        izu=izu+2*mmul-2*nmz
        goto 420
!--SKEW ELEMENTS
  310   kzz=-kzz
        goto(320,330,340,350,360,370,380,390,400,410),kzz
        goto 450
!--VERTICAL DIPOLE
  320   ekk=ekk*c1e3
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
        goto 420
!--SKEW QUADRUPOLE
  330   continue
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        goto 420
!--SKEW SEXTUPOLE
  340   ekk=ekk*c1m3
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*cikve-tiltsk*crkve)
        qv=-ekk*two*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 420
!--SKEW OCTUPOLE
  350   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-three*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 420
!--SKEW DECAPOLE
  360   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-four*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 420
!--SKEW DODECAPOLE
  370   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-5*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 420
!--SKEW 14-POLE
  380   ekk=ekk*c1m15
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-6*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 420
!--SKEW 16-POLE
  390   ekk=ekk*c1m18
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-7*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 420
!--SKEW 18-POLE
  400   ekk=ekk*c1m21
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-8*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 420
!--SKEW 20-POLE
  410   ekk=ekk*c1m24
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-9*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
  420   t(1,2)=t(1,2)+dyy1
        t(1,4)=t(1,4)+dyy2
        do 430 i=2,ium
          t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
          t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
  430   continue
        do 440 l=1,2
          ll=2*l
  440   alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
  450 continue
      qwc(1)=phi(1)
      qwc(2)=phi(2)
      if(qxse.ge.qxsa) then
        qwc(3)=qxse-qxsa
      else
        qwc(3)=phi(1)+qxse-qxsa
      endif
!-----------------------------------------------------------------------
      return
      end
      subroutine qmod0
!-----------------------------------------------------------------------
!  ADJUSTMENT OF THE Q-VALUES PLUS AN ADDITIONAL ADJUSTMENT OF A
!  X-PHASEADVANCE BETWEEN 2 POSITIONS IN THE MACHINE
!-----------------------------------------------------------------------
      implicit none
      integer i,ierr,ii,iq1,iq2,iq3,iql,j,l,n,nite
      double precision a11,a12,a13,a21,a22,a23,a31,a32,a33,aa,aa1,bb,   &
     &dpp,dq1,dq2,dq3,qwc,qx,qz,sens,sm0,sqx,sqxh,sqz
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
      dimension sens(3,5),aa(3,3),bb(3),qx(3),qz(3),sm0(3),qwc(3)
      dimension aa1(2,2)
      save
!-----------------------------------------------------------------------
      do 10 i=1,3
        bb(i)=zero
        qx(i)=zero
        qz(i)=zero
        sm0(i)=zero
        qwc(i)=zero
        do 10 j=1,3
          aa(i,j)=zero
   10 continue
      do 20 i=1,3
        do 20 j=1,5
          sens(i,j)=zero
   20 continue
      do 30 i=1,2
        do 30 j=1,2
          aa1(i,j)=zero
   30 continue
      write(*,10010)
      sqx=zero
      sqz=zero
      sqxh=zero
      dpp=zero
      iq1=iq(1)
      iq2=iq(2)
      if(kz(iq1).ne.2.or.kz(iq2).ne.2) call prror(8)
      if (abs(el(iq1)).le.pieni.or.abs(el(iq2)).le.pieni) then
        sm0(1)=ed(iq1)
        sm0(2)=ed(iq2)
      else
        sm0(1)=ek(iq1)
        sm0(2)=ek(iq2)
      endif
      if(kp(iq1).eq.5) call combel(iq1)
      if(kp(iq2).eq.5) call combel(iq2)
      sens(1,1)=qw0(1)
      sens(2,1)=qw0(2)
      if(abs(qw0(3)).gt.pieni) then
        iq3=iq(3)
        if(kz(iq3).ne.2) call prror(8)
        if (abs(el(iq3)).le.pieni) then
          sm0(3)=ed(iq3)
        else
          sm0(3)=ek(iq3)
        endif
        if(kp(iq3).eq.5) call combel(iq3)
        nite=3
      else
        nite=2
      endif
      call clorb(dpp)
      if(ierro.gt.0) call prror(9)
      call phasad(dpp,qwc)
      sens(1,5)=qwc(1)
      sens(2,5)=qwc(2)
      if(nite.eq.3) then
        sens(3,1)=qw0(3)
        sens(3,5)=qwc(3)
        write(*,10100)
        write(*,10120) qwc,qw0
      else
        write(*,10110)
        write(*,10130) qwc(1),qwc(2),qw0(1),qw0(2)
      endif
      do 60 ii=1,itqv
        do 40 n=1,nite
          iql=iq(n)
          if (abs(el(iql)).le.pieni) then
            ed(iql)=ed(iql)+dkq
          else
            ek(iql)=ek(iql)+dkq
          endif
          if(kp(iql).eq.5) call combel(iql)
          call clorb(dpp)
          if(ierro.gt.0) call prror(9)
          call phasad(dpp,qwc)
          sens(1,n+1)=qwc(1)
          sens(2,n+1)=qwc(2)
          if(nite.eq.3) then
            sens(3,n+1)=qwc(3)
            write(*,10140) ii,n,qwc
          else
            write(*,10150) ii,n,qwc(1),qwc(2)
          endif
          if (abs(el(iql)).le.pieni) then
            ed(iql)=ed(iql)-dkq
          else
            ek(iql)=ek(iql)-dkq
          endif
          if(kp(iql).eq.5) call combel(iql)
   40   continue
!--Q-VALUE ADJUSTMENT
        aa1(1,1)=(sens(1,2)-sens(1,5))/dkq
        aa1(1,2)=(sens(2,2)-sens(2,5))/dkq
        aa1(2,1)=(sens(1,3)-sens(1,5))/dkq
        aa1(2,2)=(sens(2,3)-sens(2,5))/dkq
        a11=aa1(1,1)
        a12=aa1(1,2)
        a21=aa1(2,1)
        a22=aa1(2,2)
        bb(1)=sens(1,5)-sens(1,1)
        bb(2)=sens(2,5)-sens(2,1)
        sqx=sqx+abs(bb(1))
        sqz=sqz+abs(bb(2))
        if(nite.eq.3) then
          aa(1,1)=a11
          aa(1,2)=a12
          aa(1,3)=(sens(3,2)-sens(3,5))/dkq
          aa(2,1)=a21
          aa(2,2)=a22
          aa(2,3)=(sens(3,3)-sens(3,5))/dkq
          aa(3,1)=(sens(1,4)-sens(1,5))/dkq
          aa(3,2)=(sens(2,4)-sens(2,5))/dkq
          aa(3,3)=(sens(3,4)-sens(3,5))/dkq
          a13=aa(1,3)
          a23=aa(2,3)
          a31=aa(3,1)
          a32=aa(3,2)
          a33=aa(3,3)
          bb(3)=sens(3,5)-sens(3,1)
          sqxh=sqxh+abs(bb(3))
          call loesd(aa,bb,nite,nite,ierr)
        else
          call loesd(aa1,bb,nite,nite,ierr)
        endif
        if(ierr.eq.1) call prror(35)
        do 50 l=1,nite
          iql=iq(l)
          if (abs(el(iql)).le.pieni) then
            ed(iql)=ed(iql)-bb(l)
          else
            ek(iql)=ek(iql)-bb(l)
          endif
          if(kp(iql).eq.5) call combel(iql)
   50   continue
        call clorb(dpp)
        if(ierro.gt.0) call prror(9)
        call phasad(dpp,qwc)
        sens(1,5)=qwc(1)
        sens(2,5)=qwc(2)
        if(nite.eq.3) then
          sens(3,5)=qwc(3)
          write(*,10020) qw0(1),qwc(1),qw0(2),qwc(2),qw0(3),qwc(3)
          if (abs(el(iq1)).le.pieni) then
            write(*,10040) sm0(1),ed(iq1),bez(iq1),sm0(2),ed(iq2),bez   &
     &(iq2),sm0(3),ed(iq3),bez(iq3)
          else
            write(*,10040) sm0(1),ek(iq1),bez(iq1),sm0(2),ek(iq2),bez   &
     &(iq2),sm0(3),ek(iq3),bez(iq3)
          endif
          write(*,10080) sqx,sqz,sqxh
          write(*,10060) a11,a12,a13,a21,a22,a23,a31,a32,a33
        else
          write(*,10030) qw0(1),qwc(1),qw0(2),qwc(2)
          if (abs(el(iq1)).le.pieni) then
            write(*,10050) sm0(1),ed(iq1),bez(iq1),sm0(2),ed(iq2),bez   &
     &(iq2)
          else
            write(*,10050) sm0(1),ek(iq1),bez(iq1),sm0(2),ek(iq2),bez   &
     &(iq2)
          endif
          write(*,10090) sqx,sqz
          write(*,10070) a11,a12,a21,a22
        endif
        if (abs(el(iq(1))).le.pieni) then
          sm0(1)=ed(iq(1))
          sm0(2)=ed(iq(2))
        else
          sm0(1)=ek(iq(1))
          sm0(2)=ek(iq(2))
        endif
        dq1=abs(qwc(1)-qw0(1))
        dq2=abs(qwc(2)-qw0(2))
        if(nite.eq.3) then
          if (abs(el(iq(3))).le.pieni) then
            sm0(3)=ed(iq(3))
          else
            sm0(3)=ek(iq(3))
          endif
          dq3=abs(qwc(3)-qw0(3))
          if(dq1.lt.dqq.and.dq2.lt.dqq.and.dq3.lt.dqq) return
        else
          if(dq1.lt.dqq.and.dq2.lt.dqq) return
        endif
   60 continue
      write(*,10000) itqv
!-----------------------------------------------------------------------
      return
10000 format(t5/t10,'TUNE ADJUSTMENT'/ t10,                             &
     &'MAXIMUM NUMBER OF ITERATIONS ACHIEVED--->',2x,i4/ t10,           &
     &'PROCEDURE MAY NOT HAVE CONVERGED')
10010 format(/131('-'))
10020 format(//131('-')//t10,'DATA BLOCK TUNE-VARIATION' / /t10,        &
     &'TUNE'           ,26x,'THEORET.     AFTER CORRECTION'/ t10,       &
     &'HORIZONTAL'     ,17x,g16.10,2x,g16.10/ t10,                      &
     &'VERTICAL'       ,19x,g16.10,2x,g16.10/ t10,                      &
     &'PART-HORIZONTAL',12x,g16.10,2x,g16.10/)
10030 format(//131('-')//t10,'DATA BLOCK TUNE-VARIATION' / /t10,        &
     &'TUNE'           ,26x,'THEORET.      AFTER CORRECTION'/ t10,      &
     &'HORIZONTAL'     ,17x,g16.10,2x,g16.10/ t10,                      &
     &'VERTICAL'       ,19x,g16.10,2x,g16.10/)
10060 format(t10,'QUADRUPOLE SENSITIVITIES',6x,'D-QX',14x,'D-QY',14x,   &
     &'D-QXH'/29x,'QF   ',d15.8,3x,d15.8,3x,d15.8/29x,                  &
     &'QD   ',d15.8,3x,d15.8,3x,d15.8/29x,                              &
     &'QF2  ',d15.8,3x,d15.8,3x,d15.8//131('-')//)
10070 format(t10,'QUADRUPOLE SENSITIVITIES',6x,'D-QX',14x,'D-QY', /29x, &
     &'QF   ',d15.8,3x,d15.8/29x,'QD   ',d15.8,3x,d15.8 //131('-')//)
10080 format(t10,'TOTAL TUNE SHIFT',10x,'QX =',f10.7,'    QY =',f10.7,  &
     &'   QXH =',f10.7)
10090 format(t10,'TOTAL TUNE SHIFT',10x,'QX =',f10.7,'    QY =',f10.7)
10100 format(t5,'---- QMOD FOR SPLIT-Q-VALUES ENTRY ---- ',             &
     &'(ZERO MOMENTUM-DEVIATION)')
10110 format(t5,'---- QMOD ENTRY ---- (ZERO MOMENTUM-DEVIATION)')
10120 format(t10,'START-QX-QY-QXH',3f12.7,' END-QX-QY-QXH',3f12.7)
10130 format(t10,'START-QX-QY',2f12.7,' END-QX-QY',2f12.7)
10140 format(t10,'ITER=',i3,'/QUAD=',i3,'/QX-QY-QXH',3f12.7)
10150 format(t10,'ITER=',i3,'/QUAD=',i3,'/QX-QY',2f12.7)
10040 format(t10,'QUADRU.STRENGTHS',7x,g16.10,2x,g16.10,'   TYP     ',  &
     &a16/t10,                  23x,g16.10,2x,g16.10,'           ',     &
     &a16)
10050 format(t10,'QUADRU.STRENGTHS',7x,g16.10,2x,g16.10,'   TYP     ',  &
     &a16/t10,                  23x,g16.10,2x,g16.10,'           ',     &
     &a16)
      end
      subroutine qmodda(mm,qwc)
!-----------------------------------------------------------------------
!  ADJUSTMENT OF THE Q-VALUES VIA DA
!-----------------------------------------------------------------------
      implicit none
      integer i,intwq,ix,mm,ncorr,ncorruo,ncrr,nd,nd2,ndh
      double precision cor,coro,dq1,dq2,dps0,edcor1,edcor2,qwc
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
      double precision aml6,edcor
      common/sixdim/aml6(6,6),edcor(2)
      double precision aai,ampt,bbi,damp,rfres,rsmi,rzphs,smi,smizf,xsi,&
     &zsi
      real tlim,time0,time1
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),smizf(nblz),              &
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/ttime/tlim,time0,time1
      integer ichromc,ilinc,iqmodc
      double precision clon,chromc,corr,wxys
      common/correct/ corr(3,3),chromc(2),wxys(3),clon(6),iqmodc,       &
     &ichromc,ilinc
      integer icorr,idial,idptr,imod1,imod2,inorm,ipar,namp,ncor,nctype,&
     &ndimf,nmom,nmom1,nmom2,nord,nord1,nordf,nsix,nvar,nvar2,nvarf
      double precision dpmax,preda,weig1,weig2
      character*16 coel
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor)
      common/norf/nordf,nvarf,nord1,ndimf,idptr,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,&
     &coel(10)
      dimension intwq(3),qwc(3)
      save
!-----------------------------------------------------------------------
      ncorruo=ncorru
      ncorru=1
      nd2=2*mm
      ndh=nd2+2
      intwq(1)=int(qwc(1))
      intwq(2)=int(qwc(2))
      intwq(3)=0
      dq1=zero
      dq2=zero
      if(iqmod6.eq.1) then
        if(el(iq(1)).le.pieni) then
          edcor(1)=ed(iq(1))
        else
          edcor(1)=ek(iq(1))
        endif
        if(el(iq(2)).le.pieni) then
          edcor(2)=ed(iq(2))
        else
          edcor(2)=ek(iq(2))
        endif
        edcor1=edcor(1)
        edcor2=edcor(2)
        cor=0d0
        coro=1d38
      endif
      do ncorr=1,itqv+1
        if(nbeam.ge.1) then
          nd=mm
              dps0=dps(1)
              dps(1)=zero
              iqmodc=4
              call mydaini(1,2,nd2,nd,nd2,1)
              ilinc=2
              call mydaini(2,2,nd2,nd,nd2,1)
              dps(1)=dps0
        endif
        if(iqmod6.eq.1) write(*,10080) nd2
        if(iqmod6.ne.1) write(*,10090) nd2
        if(mm.eq.2) then
          write(*,10010) clo(1),clop(1)
          write(*,10010) clo(2),clop(2)
        elseif(mm.eq.3) then
          write(*,10010) clo6(1),clop6(1)
          write(*,10010) clo6(2),clop6(2)
          write(*,10010) clo6(3),clop6(3)
        endif
        iqmodc=2
        call mydaini(1,1,nd2,mm,nd2,1)
        if(iqmod6.eq.1) then
          write(*,10000) nd2
          iqmodc=1
          call mydaini(2,3,ndh,mm,nd2,1)
          do i=1,mm
            qwc(i)=intwq(i)+corr(1,i)
          enddo
          dq1=qwc(1)-qw0(1)
          dq2=qwc(2)-qw0(2)
          if(ncorr.eq.1) cor=sqrt(dq1*dq1+dq2*dq2)
          if(abs(dq1).gt.dqq.or.abs(dq2).gt.dqq) then
            cor=sqrt(dq1*dq1+dq2*dq2)
            if(ncorr.eq.1.or.cor.lt.coro) then
              coro=cor
              if(el(iq(1)).le.pieni) then
                ed(iq(1))=ed(iq(1))-corr(2,1)*dq1-corr(2,2)*dq2
              else
                ek(iq(1))=ek(iq(1))-corr(2,1)*dq1-corr(2,2)*dq2
              endif
              if(el(iq(2)).le.pieni) then
                ed(iq(2))=ed(iq(2))-corr(3,1)*dq1-corr(3,2)*dq2
              else
                ek(iq(2))=ek(iq(2))-corr(3,1)*dq1-corr(3,2)*dq2
              endif
              do ncrr=1,iu
                ix=ic(ncrr)
                if(ix.gt.nblo) then
                  ix=ix-nblo
                  if(ix.eq.iq(1).or.iratioe(ix).eq.iq(1)) then
                    smi(ncrr)=ed(iq(1))*ratioe(ix)+smizf(ncrr)
                  else if(ix.eq.iq(2).or.iratioe(ix).eq.iq(2)) then
                    smi(ncrr)=ed(iq(2))*ratioe(ix)+smizf(ncrr)
                  endif
                endif
              enddo
              if(el(iq(1)).le.pieni) then
                edcor(1)=ed(iq(1))
              else
                edcor(1)=ek(iq(1))
              endif
              if(el(iq(2)).le.pieni) then
                edcor(2)=ed(iq(2))
              else
                edcor(2)=ek(iq(2))
              endif
              if(ncorr.eq.1) then
                write(*,10020) nd2,qw0(1),qwc(1),qw0(2),qwc(2),ncorr-1, &
     &cor
              else
                write(*,10030) nd2,qw0(1),qwc(1),qw0(2),qwc(2),ncorr-1, &
     &cor
              endif
              if(el(iq(1)).le.pieni.and.el(iq(2)).le.pieni) then
                write(*,10040) edcor1,ed(iq(1)),bez(iq(1)),edcor2,      &
     &ed(iq(2)),bez(iq(2))
              elseif(el(iq(1)).le.pieni.and.el(iq(2)).gt.pieni) then
                write(*,10040) edcor1,ed(iq(1)),bez(iq(1)),edcor2,      &
     &ek(iq(2)),bez(iq(2))
              elseif(el(iq(1)).gt.pieni.and.el(iq(2)).le.pieni) then
                write(*,10040) edcor1,ek(iq(1)),bez(iq(1)),edcor2,      &
     &ed(iq(2)),bez(iq(2))
              else
                write(*,10040) edcor1,ek(iq(1)),bez(iq(1)),edcor2,      &
     &ek(iq(2)),bez(iq(2))
              endif
            else
              write(*,10050) nd2,ncorr-1
              goto 1
            endif
          else
            write(*,10060) nd2,ncorr-1
            goto 1
          endif
        else
          iqmodc=3
          call mydaini(2,2,nd2,mm,nd2,1)
          do i=1,mm
            qwc(i)=intwq(i)+wxys(i)
          enddo
          goto 1
        endif
      enddo
 1    continue
      if(iqmod6.eq.1) then
        do ncrr=1,iu
          ix=ic(ncrr)
          if(ix.le.nblo) then
            if(iratioe(ix).eq.iq(1)) ek(ix)=ek(iq(1))*ratioe(ix)
            if(iratioe(ix).eq.iq(2)) ek(ix)=ek(iq(2))*ratioe(ix)
          endif
        enddo
        iqmodc=3
        call mydaini(2,2,nd2,mm,nd2,1)
        do i=1,mm
          qwc(i)=intwq(i)+wxys(i)
        enddo
        if(ncorr.eq.itqv+1) write(*,10070) nd2,itqv
        if(ncorr.eq.1) then
          write(*,10020) nd2,qw0(1),qwc(1),qw0(2),qwc(2),ncorr-1,cor
        else
          write(*,10030) nd2,qw0(1),qwc(1),qw0(2),qwc(2),ncorr-1,cor
        endif
        if(el(iq(1)).le.pieni.and.el(iq(2)).le.pieni) then
          write(*,10040) edcor1,ed(iq(1)),bez(iq(1)),edcor2,ed(iq(2)),  &
     &bez(iq(2))
        elseif(el(iq(1)).le.pieni.and.el(iq(2)).gt.pieni) then
          write(*,10040) edcor1,ed(iq(1)),bez(iq(1)),edcor2,ek(iq(2)),  &
     &bez(iq(2))
        elseif(el(iq(1)).gt.pieni.and.el(iq(2)).le.pieni) then
          write(*,10040) edcor1,ek(iq(1)),bez(iq(1)),edcor2,ed(iq(2)),  &
     &bez(iq(2))
        else
          write(*,10040) edcor1,ek(iq(1)),bez(iq(1)),edcor2,ek(iq(2)),  &
     &bez(iq(2))
        endif
      endif
      ncorru=ncorruo
!-----------------------------------------------------------------------
10000 format(/131('-')/t10,'ENTERING ',i1,'D DA TUNE-VARIATION')
10010 format(1x,f47.33/1x,f47.33)
10020 format(/131('-')/t10,i1,'D DA TUNE-VARIATION'/t10,                &
     &'TUNE'           ,26x,'THEORET.       BEFORE CORRECTION'/ t10,    &
     &'HORIZONTAL'     ,15x,g20.14,g20.14/ t10,                         &
     &'VERTICAL'       ,17x,g20.14,g20.14// t10,                        &
     &'ITERATION:'     ,21x,i3/ t10,                                    &
     &'ACCURACY:'      ,17x,g16.10/)
10030 format(/131('-')/t10,i1,'D DA TUNE-VARIATION'/t10,                &
     &'TUNE'           ,26x,'THEORET.       AFTER CORRECTION'/ t10,     &
     &'HORIZONTAL'     ,15x,g20.14,g20.14/ t10,                         &
     &'VERTICAL'       ,17x,g20.14,g20.14// t10,                        &
     &'ITERATION:'     ,21x,i3/ t10,                                    &
     &'ACCURACY:'      ,17x,g16.10/)
10040 format(t10,'QUADRUPOLE STRENGTH',6x,g16.10,4x,g16.10,'   TYP     '&
     &,a16/t10,                  25x,g16.10,4x,g16.10,'           ',    &
     &a16)
10050 format(/t5,'---- NO IMPROVEMENT OF ',i1,'D DA TUNE-VARIATION ',   &
     &'IN ITERATION: ',i4/)
10060 format(/t10,i1,'D DA TUNE-VARIATION SUCCESSFUL IN ITERATION: ',   &
     &i4/)
10070 format(/t10,i1,'D DA TUNE-VARIATION'/ t10,                        &
     &'MAXIMUM NUMBER OF ITERATIONS ACHIEVED--->',2x,i4/ t10,           &
     &'PROCEDURE MAY NOT HAVE CONVERGED')
10080 format(/t10,'Initial ',i1,'-D DA CLOSED ORBIT IN QMODDA')
10090 format(/t10,'Initial ',i1,'-D DA CLOSED ORBIT IN QMODDA (NO ',    &
     &'TUNE ADJUSTEMENT)')
      end
      subroutine umlauf(dpp,ium,ierr)
!-----------------------------------------------------------------------
!     ONE TURN-TRANSFORMATION (INCLUDING QUADRUPOLE CONTRIBUTIONS)
!-----------------------------------------------------------------------
      implicit none
      integer i,ierr,im,ium,ix,izu,j,k,kpz,kx,kzz,l,ll,l1,nmz
      double precision aa,bb,benkr,ci,cikve,cr,crkve,crkveuk,dpp,dpr,   &
     &dyy1,dyy2,ekk,puf,qu,qv,r0,r0a,xl,xs,zl,zs
      double precision dyy11,qu1,tiltck,tiltsk
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
      dimension aa(mmul),bb(mmul),dpr(5)
      dimension cr(mmul),ci(mmul)
      save
!-----------------------------------------------------------------------
      do 10 i=1,mmul
        aa(i)=zero
        bb(i)=zero
        cr(i)=zero
        ci(i)=zero
   10 continue
      do 20 i=1,5
        dpr(i)=zero
   20 continue
      ierr=0
      dpr(1)=dpp*c1e3
      izu=0
      do 350 k=1,iu
        ix=ic(k)
        if(ix.gt.nblo) goto 60
        if(ix.le.0) goto 40
        do 30 j=1,ium
          do 30 kx=1,2
            if(ithick.eq.1) then
              puf=x(j,kx)
              x(j,kx)=bl1(ix,kx,1)*puf+bl1(ix,kx,2)*y(j,kx)+dpr(j)*bl1  &
     &(ix,kx,5)
              y(j,kx)=bl1(ix,kx,3)*puf+bl1(ix,kx,4)*y(j,kx)+dpr(j)*bl1  &
     &(ix,kx,6)
            else
              x(j,kx)=x(j,kx)+bl1(ix,kx,2)*y(j,kx)
            endif
   30   continue
        goto 350
   40   ix=-ix
        do 50 j=1,ium
          do 50 kx=1,2
            if(ithick.eq.1) then
              puf=x(j,kx)
              x(j,kx)=bl2(ix,kx,1)*puf+bl2(ix,kx,2)*y(j,kx)+dpr(j)*bl2  &
     &(ix,kx,5)
              y(j,kx)=bl2(ix,kx,3)*puf+bl2(ix,kx,4)*y(j,kx)+dpr(j)*bl2  &
     &(ix,kx,6)
            else
              x(j,kx)=x(j,kx)+bl2(ix,kx,2)*y(j,kx)
            endif
   50   continue
        goto 350
   60   ix=ix-nblo
        qu=zero
        qv=zero
        dyy1=zero
        dyy2=zero
        kpz=kp(ix)
        if(kpz.eq.6) goto 350
        kzz=kz(ix)
        if(abs(x(1,1)).lt.c1e4.and.abs(x(1,2)).lt.c1e4) goto 70
        ierr=1
        call prror(101)
        return
 70     continue
        if(kzz.eq.22) then
          do j=1,ium
            do kx=1,2
              ll=kx*2
              puf=x(j,kx)
              x(j,kx)=cotr(imtr(ix),ll-1)+rrtr(imtr(ix),ll-1,ll-1)*puf+ &
     &rrtr(imtr(ix),ll-1,ll)*y(j,kx)+dpr(j)*                            &
     &rrtr(imtr(ix),ll-1,6)
              y(j,kx)=cotr(imtr(ix),ll)+rrtr(imtr(ix),ll,ll-1)*puf+     &
     &rrtr(imtr(ix),ll,ll)*y(j,kx)+dpr(j)*                              &
     &rrtr(imtr(ix),ll,6)
            enddo
          enddo
        endif
        if(kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 350
        if(iorg.lt.0) mzu(k)=izu
        izu=mzu(k)+1
        ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
        izu=izu+1
        xs=xpl(ix)+zfz(izu)*xrms(ix)
        izu=izu+1
        zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(x(1,1)-xs)*tiltc(k)+(x(1,2)-zs)*tilts(k)
        zl=-(x(1,1)-xs)*tilts(k)+(x(1,2)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
        if(kzz.lt.0) goto 220
        goto(80,90,100,110,120,130,140,150,160,170,180),kzz
        goto 350
!--HORIZONTAL DIPOLE
   80   ekk=ekk*c1e3
        y(1,1)=y(1,1)+ekk*tiltc(k)
        y(1,2)=y(1,2)+ekk*tilts(k)
        goto 350
!--NORMAL QUADRUPOLE
   90   continue
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        goto 330
!--NORMAL SEXTUPOLE
  100   ekk=ekk*c1m3
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*crkve+tiltsk*cikve)
        qv=ekk*two*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--NORMAL OCTUPOLE
  110   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=three*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--NORMAL DECAPOLE
  120   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=four*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--NORMAL DODECAPOLE
  130   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=5*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--NORMAL 14-POLE
  140   ekk=ekk*c1m15
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=6*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--NORMAL 16-POLE
  150   ekk=ekk*c1m18
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=7*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--NORMAL 18-POLE
  160   ekk=ekk*c1m21
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=8*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--NORMAL 20-POLE
  170   ekk=ekk*c1m24
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=9*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
  180   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            y(1,1)=y(1,1)+(qu*xl-dpp*c1e3*dki(ix,1)                     &
     &/(one+dpp))*tiltc(k)                                              &
     &+c1e3*dki(ix,1)/(one+dpp)*(one-tiltc(k))
            y(1,2)=y(1,2)+(qu*xl-dpp*c1e3*dki(ix,1)                     &
     &/(one+dpp))*tilts(k)                                              &
     &+c1e3*dki(ix,1)/(one+dpp)*tilts(k)
            do 190 j=2,ium
              y(j,1)=y(j,1)+qu*x(j,1)*tiltc(k)
              y(j,2)=y(j,2)+qu*x(j,2)*tilts(k)
  190       continue
          else
            y(1,1)=y(1,1)-dki(ix,1)*dpp/(one+dpp)*c1e3*tiltc(k)         &
     &+c1e3*dki(ix,1)/(one+dpp)*(one-tiltc(k))
            y(1,2)=y(1,2)-dki(ix,1)*dpp/(one+dpp)*c1e3*tilts(k)         &
     &+c1e3*dki(ix,1)/(one+dpp)*tilts(k)
          endif
        endif
        if(abs(dki(ix,2)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            y(1,1)=y(1,1)+(qu*zl-dpp*c1e3*dki(ix,2)                     &
     &/(one+dpp))*tilts(k)                                              &
     &+c1e3*dki(ix,2)/(one+dpp)*tilts(k)
            y(1,2)=y(1,2)+(-qu*zl+dpp*c1e3*dki(ix,2)                    &
     &/(one+dpp))*tiltc(k)                                              &
     &-c1e3*dki(ix,2)/(one+dpp)*(one-tiltc(k))
            do 200 j=2,ium
              y(j,1)=y(j,1)+qu*x(j,1)*tilts(k)
              y(j,2)=y(j,2)-qu*x(j,2)*tiltc(k)
  200       continue
          else
            y(1,1)=y(1,1)-dki(ix,2)*dpp/(one+dpp)*c1e3*tilts(k)         &
     &+dki(ix,2)/(one+dpp)*c1e3*tilts(k)
            y(1,2)=y(1,2)+dki(ix,2)*dpp/(one+dpp)*c1e3*tiltc(k)         &
     &-dki(ix,2)/(one+dpp)*c1e3*(one-tiltc(k))
          endif
        endif
        if(abs(r0).le.pieni) goto 350
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
          goto 350
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        do 210 l=1,nmz
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
  210   continue
        if(nmz.ge.2) then
          qu=bb(2)
          qv=-aa(2)
          dyy1=bb(1)+bb(2)*crkve+aa(2)*cikve
          dyy2=aa(1)-bb(2)*cikve+aa(2)*crkve
          do 215 l=3,nmz
            l1=l-1
            qu=qu+l1*(bb(l)*crkve+aa(l)*cikve)
            qv=qv+l1*(bb(l)*cikve-aa(l)*crkve)
            crkveuk=crkve*xl-cikve*zl
            cikve=crkve*zl+cikve*xl
            crkve=crkveuk
            dyy1=dyy1+bb(l)*crkve+aa(l)*cikve
            dyy2=dyy2-bb(l)*cikve+aa(l)*crkve
  215     continue
        else
          qu=zero
          qv=zero
          dyy1=bb(1)
          dyy2=aa(1)
        endif
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
        izu=izu+2*mmul-2*nmz
        y(1,1)=y(1,1)+dyy1
        y(1,2)=y(1,2)+dyy2
        if(ium.eq.1) goto 350
        goto 330
!--SKEW ELEMENTS
  220   kzz=-kzz
        goto(230,240,250,260,270,280,290,300,310,320),kzz
        goto 350
!--VERTICAL DIPOLE
  230   ekk=ekk*c1e3
        y(1,1)=y(1,1)-ekk*tilts(k)
        y(1,2)=y(1,2)+ekk*tiltc(k)
        goto 350
!--SKEW QUADRUPOLE
  240   continue
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        goto 330
!--SKEW SEXTUPOLE
  250   ekk=ekk*c1m3
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*cikve-tiltsk*crkve)
        qv=-ekk*two*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--SKEW OCTUPOLE
  260   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-three*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--SKEW DECAPOLE
  270   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-four*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--SKEW DODECAPOLE
  280   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-5*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--SKEW 14-POLE
  290   ekk=ekk*c1m15
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-6*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--SKEW 16-POLE
  300   ekk=ekk*c1m18
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-7*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--SKEW 18-POLE
  310   ekk=ekk*c1m21
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-8*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
!--SKEW 20-POLE
  320   ekk=ekk*c1m24
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-9*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
  330   do 340 j=2,ium
          y(j,1)=y(j,1)+x(j,1)*qu-qv*x(j,2)
          y(j,2)=y(j,2)-x(j,2)*qu-qv*x(j,1)
  340   continue
  350 continue
!-----------------------------------------------------------------------
      return
      end
      subroutine resex(dpp)
!-----------------------------------------------------------------------
!  CALCULATION OF DRIVINGTERMS OF RESONANCES INCLUDING SUBRESONANCE
!  USED FOR RMOD
!-----------------------------------------------------------------------
      implicit none
      integer i,i1,i2,ii,ik,im,ip,ium,ix,izu,j,jj,jk,jl,jm,k,k1,kpz,    &
     &kzz,l,l1,l2,ll,lmin,m2,m4,m6,min,mm,mpe,mx,n,n2,n2e,nf1,nf3,nf4,  &
     &nkk,nmz,nn1,nn2,nnf,np,np2,ns,nv,nv1,nv11,nv2,nv21,nz2,dj
      double precision aa,ab1,ab2,alfa,b,b1,b2,bb,benkr,beta,btc,bts,   &
     &chy,ci,cikve,cr,crkve,cxzi,cxzr,cxzyi,cxzyr,cxzyrr,del,dphi,dpp,  &
     &dppi,dpr,dt,dyy1,dyy2,e,ea,eb,ekk,ep,etl,gerad,phi,phibf,phy,pie, &
     &puf,qu,qv,qw,r0,r0a,radi,re,re1,res,rn2,sb1,sb2,sea,seb,shy,t,    &
     &vdt1,vdt2,vdt3,xl,xs,zl,zs
      double precision dyy11,qu1,tiltck,tiltck1,tiltck2,tiltck3,tiltck4,&
     &tiltck5,tiltckuk,tiltsk,tiltsk1,tiltsk2,tiltsk3,tiltsk4,tiltsk5
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
      dimension t(5,4)
      dimension beta(2),alfa(2),phi(2),phibf(2)
      dimension qw(2)
      dimension aa(mmul),bb(mmul),dpr(5)
      dimension nnf(10),ep(2)
      dimension ab1(10),ab2(10),re(10,18),ip(10,18)
      dimension b(10,10),nz2(9),e(10,10)
      dimension chy(9,18),shy(9,18),min(5)
      dimension cr(mmul),ci(mmul)
      save
!-----------------------------------------------------------------------
      ium=5
      do 10 i=1,ium
        dpr(i)=zero
   10 continue
      do 20 i=1,ium
        do 20 j=1,4
          t(i,j)=zero
   20 continue
      do 30 i=1,2
        beta(i)=zero
        alfa(i)=zero
        phi(i)=zero
        phibf(i)=zero
        qw(i)=zero
        ep(i)=zero
   30 continue
      do 40 i=1,10
        nnf(i)=0
        do 40 j=1,18
          ip(i,j)=0
          re(i,j)=zero
   40 continue
      do 50 i=1,mmul
        aa(i)=zero
        bb(i)=zero
        cr(i)=zero
        ci(i)=zero
   50 continue
      do 100 i=1,9
        nz2(i)=0
        do 90 j=1,18
          chy(i,j)=zero
          shy(i,j)=zero
          do 80 k=1,10
            do 60 ii=1,10
              e(k,ii)=zero
              b(k,ii)=zero
   60       continue
            do 70 l=1,5
              rtc(i,j,k,l)=zero
              rts(i,j,k,l)=zero
              min(l)=0
   70       continue
   80     continue
   90   continue
  100 continue
      btc=zero
      bts=zero
      phy=zero
      dt=zero
      del=zero
      ns=0
      ik=0
      pie=two*pi
      etl=zero
      radi=totl/pie
      dpr(1)=dpp*c1e3
      call clorb(dpp)
      call betalf(dpp,qw)
      if(ierro.ne.0) call prror(22+ierro)
      call envar(dpp)
!--STARTVALUES OF THE TRAJECTORIES
      do 110 l=1,2
        ll=2*l
        alfa(l)=alf0(l)
        beta(l)=bet0(l)
        t(1,ll-1)=clo(l)
  110 t(1,ll)=clop(l)
      do 120 i=1,4
        do 120 j=1,4
          t(i+1,j)=ta(j,i)
  120 t(i+1,j)=ta(j,i)
!--EP=EMITTANCE IN PI*MM*MRAD
      ep(1)=tam1*tam1/beta(1)
      ep(2)=tam2*tam2/beta(2)
!--SINGLE TURN BLOCKLOOP
      izu=0
      do 770 k=1,iu
        do 130 k1=1,10
          ab1(k1)=zero
  130   ab2(k1)=zero
        ix=ic(k)
        if(ix.gt.nblo) goto 210
        jj=0
        dj=1
        if(ix.gt.0) goto 140
        ix=-ix
        jj=mel(ix)+1
        dj=-1
  140   jm=mel(ix)
!--BLOCKELEMENTLOOP
        do 200 j=1,jm
          jj=jj+dj
          jk=mtyp(ix,jj)
          if(ithick.eq.1.and.kz(jk).ne.0) goto 170
          if(ithick.eq.0.and.kz(jk).ne.0) goto 770
!--PURE DRIFTLENGTH
          etl=etl+el(jk)
          do 150 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=pi2
            endif
            do 150 i=1,ium
  150     t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
          do 160 l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=pi2-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
  160     phi(l)=phi(l)+dphi
          goto 200
!--MAGNETELEMENT
  170     continue
          if(kz(jk).ne.8) etl=etl+el(jk)
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a(jk,l,5)
              t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
            enddo
          enddo
          do l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(kz(jk).ne.8.and.-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi
          enddo
  200   continue
        goto 770
!--NL-INSERTION
  210   ix=ix-nblo
        qu=zero
        qv=zero
        kpz=kp(ix)
        if(kpz.eq.6) goto 770
        kzz=kz(ix)
        if(kzz.eq.22) then
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*rrtr(imtr(ix),ll-1,ll-1)+                   &
     &t(i,ll)*rrtr(imtr(ix),ll-1,ll)+                                   &
     &dpr(i)*rrtr(imtr(ix),ll-1,6)
              t(i,ll)=puf*rrtr(imtr(ix),ll,ll-1)+                       &
     &t(i,ll)*rrtr(imtr(ix),ll,ll)+                                     &
     &dpr(i)*rrtr(imtr(ix),ll,6)
            enddo
            t(1,ll-1)=t(1,ll-1)+cotr(imtr(ix),ll-1)
            t(1,ll)=t(1,ll)+cotr(imtr(ix),ll)
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi
          enddo
        endif
        if(kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 770
        dyy1=zero
        dyy2=zero
        if(iorg.lt.0) mzu(k)=izu
        izu=mzu(k)+1
        ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
        izu=izu+1
        xs=xpl(ix)+zfz(izu)*xrms(ix)
        izu=izu+1
        zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(t(1,1)-xs)*tiltc(k)+(t(1,3)-zs)*tilts(k)
        zl=-(t(1,1)-xs)*tilts(k)+(t(1,3)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
        if(kzz.lt.0) goto 370
        goto(220,230,240,250,260,270,280,290,300,310,320),kzz
        goto 770
!--HORIZONTAL DIPOLE
  220   ekk=ekk*c1e3
        mpe=20
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
        goto 480
!--NORMAL QUADRUPOLE
  230   continue
        dyy1=ekk*(tiltc(k)*xl+tilts(k)*zl)
        dyy2=ekk*(-tiltc(k)*zl+tilts(k)*xl)
        mpe=20
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        ab1(2)=qu
        ab2(2)=-qv
        goto 480
!--NORMAL SEXTUPOLE
  240   ekk=ekk*c1m3
        mpe=3
        mx=1
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*xl+tiltsk*zl)
        qv=ekk*two*(tiltck*zl-tiltsk*xl)
        ab1(2)=qu
        ab2(2)=-qv
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=ekk*tiltck
        ab2(3)=ekk*tiltsk
        goto 480
!--NORMAL OCTUPOLE
  250   ekk=ekk*c1m6
        mpe=4
        mx=2
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=three*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=three*ekk*(tiltck*xl+tiltsk*zl)
        ab2(3)=three*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=ekk*tiltck
        ab2(4)=ekk*tiltsk
        goto 480
!--NORMAL DECAPOLE
  260   ekk=ekk*c1m9
        mpe=5
        mx=3
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        ab1(3)=6*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=6*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=four*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=four*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=four*ekk*(tiltck*xl+tiltsk*zl)
        ab2(4)=four*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=ekk*tiltck
        ab2(5)=ekk*tiltsk
        goto 480
!--NORMAL DODECAPOLE
  270   ekk=ekk*c1m12
        mpe=6
        mx=4
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        ab1(4)=10*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=10*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=10*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=10*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=5*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=5*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=5*ekk*(tiltck*xl+tiltsk*zl)
        ab2(5)=5*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=ekk*tiltck
        ab2(6)=ekk*tiltsk
        goto 480
!--NORMAL 14-POLE
  280   ekk=ekk*c1m15
        mpe=7
        mx=5
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        ab1(5)=15*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=15*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=20*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=20*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=15*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(5)=15*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=6*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=6*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=6*ekk*(tiltck*xl+tiltsk*zl)
        ab2(6)=6*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=ekk*tiltck
        ab2(7)=ekk*tiltsk
        goto 480
!--NORMAL 16-POLE
  290   ekk=ekk*c1m18
        mpe=8
        mx=6
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        ab1(6)=21*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        ab2(6)=21*ekk*(-tiltck4*cxzyi+tiltsk4*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=35*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=35*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=35*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=35*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=21*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=21*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=7*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=7*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=7*ekk*(tiltck*xl+tiltsk*zl)
        ab2(7)=7*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=ekk*tiltck
        ab2(8)=ekk*tiltsk
        goto 480
!--NORMAL 18-POLE
  300   ekk=ekk*c1m21
        mpe=9
        mx=7
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk5=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck5=tiltckuk
        ab1(7)=28*ekk*(tiltck5*cxzyr+tiltsk5*cxzyi)
        ab2(7)=28*ekk*(-tiltck5*cxzyi+tiltsk5*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(6)=56*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        ab2(6)=56*ekk*(-tiltck4*cxzyi+tiltsk4*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=70*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=70*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=56*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=56*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=28*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=28*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=8*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=8*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck5*tiltc(k)-tiltsk5*tilts(k)
        tiltsk=tiltck5*tilts(k)+tiltsk5*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=8*ekk*(tiltck*xl+tiltsk*zl)
        ab2(8)=8*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(9)=ekk*tiltck
        ab2(9)=ekk*tiltsk
        goto 480
!--NORMAL 20-POLE
  310   ekk=ekk*c1m24
        mpe=20
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=9*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        goto 480
  320   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)                     &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            do 330 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  330       continue
          else
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tiltc(k)                             &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
          endif
        endif
        if(abs(dki(ix,2)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)+(qu*zl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+(-qu*zl+dppi*dpp)*tiltc(k)                    &
     &-dppi*(one-tiltc(k))
            do 340 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  340       continue
          else
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+dppi*dpp*tiltc(k)                             &
     &-dppi*(one-tiltc(k))
          endif
        endif
        mpe=9
        mx=0
        if(abs(r0).le.pieni) goto 770
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
          goto 770
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        cr(1)=one
        cr(2)=xl
        ci(2)=zl
        cxzyr=xl
        cxzyi=zl
        cxzr=cxzyr
        cxzi=cxzyi
        dyy1=zero
        dyy2=zero
        qu=zero
        qv=zero
        lmin=3
        if(nmz.eq.1) lmin=2
        do 350 l=lmin,mmul
          cr(l)=zero
  350   ci(l)=zero
        do 360 l=1,nmz
          l1=l-1
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
          if(l.gt.2) then
            cxzyrr=cxzyr*cxzr-cxzyi*cxzi
            cxzyi=cxzyr*cxzi+cxzyi*cxzr
            cxzyr=cxzyrr
            cr(l)=cxzyr
            ci(l)=cxzyi
          endif
          dyy1=dyy1+bb(l)*cr(l)+aa(l)*ci(l)
          dyy2=dyy2-bb(l)*ci(l)+aa(l)*cr(l)
          if(l.gt.1.and.ium.ne.1) then
            qu=qu+l1*(bb(l)*cr(l1)+aa(l)*ci(l1))
            qv=qv+l1*(bb(l)*ci(l1)-aa(l)*cr(l1))
          endif
  360   continue
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
        izu=izu+2*mmul-2*nmz
        goto 480
!--SKEW ELEMENTS
  370   kzz=-kzz
        goto(380,390,400,410,420,430,440,450,460,470),kzz
        goto 770
!--VERTICAL DIPOLE
  380   ekk=ekk*c1e3
        mpe=20
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
        goto 480
!--SKEW QUADRUPOLE
  390   continue
        dyy1=ekk*(tiltc(k)*zl-tilts(k)*xl)
        dyy2=ekk*(tiltc(k)*xl+tilts(k)*zl)
        mpe=2
        mx=-1
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        ab1(2)=qu
        ab2(2)=-qv
        goto 480
!--SKEW SEXTUPOLE
  400   ekk=ekk*c1m3
        mpe=3
        mx=1
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*zl-tiltsk*xl)
        qv=-ekk*two*(tiltck*xl+tiltsk*zl)
        ab1(2)=qu
        ab2(2)=-qv
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=ekk*tiltsk
        ab2(3)=ekk*tiltck
        goto 480
!--SKEW OCTUPOLE
  410   ekk=ekk*c1m6
        mpe=4
        mx=2
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-three*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=three*ekk*(tiltck*zl-tiltsk*xl)
        ab2(3)=three*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=ekk*tiltsk
        ab2(4)=ekk*tiltck
        goto 480
!--SKEW DECAPOLE
  420   ekk=ekk*c1m9
        mpe=5
        mx=3
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        ab1(3)=6*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=6*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=four*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-four*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=four*ekk*(tiltck*zl-tiltsk*xl)
        ab2(4)=four*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=ekk*tiltsk
        ab2(5)=ekk*tiltck
        goto 480
!--SKEW DODECAPOLE
  430   ekk=ekk*c1m12
        mpe=6
        mx=4
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        ab1(4)=10*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=10*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=10*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=10*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=5*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-5*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=5*ekk*(tiltck*zl-tiltsk*xl)
        ab2(5)=5*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=ekk*tiltsk
        ab2(6)=ekk*tiltck
        goto 480
!--SKEW 14-POLE
  440   ekk=ekk*c1m15
        mpe=7
        mx=5
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        ab1(5)=15*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=15*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=20*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=20*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=15*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=15*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=6*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-6*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=6*ekk*(tiltck*zl-tiltsk*xl)
        ab2(6)=6*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=ekk*tiltsk
        ab2(7)=ekk*tiltck
        goto 480
!--SKEW 16-POLE
  450   ekk=ekk*c1m18
        mpe=8
        mx=6
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        ab1(6)=21*ekk*(tiltck4*cxzyi-tiltsk4*cxzyr)
        ab2(6)=21*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=35*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=35*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=35*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=35*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=21*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=21*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=7*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-7*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=7*ekk*(tiltck*zl-tiltsk*xl)
        ab2(7)=7*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=ekk*tiltsk
        ab2(8)=ekk*tiltck
        goto 480
!--SKEW 18-POLE
  460   ekk=ekk*c1m21
        mpe=9
        mx=7
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk5=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck5=tiltckuk
        ab1(7)=28*ekk*(tiltck5*cxzyi-tiltsk5*cxzyr)
        ab2(7)=28*ekk*(tiltck5*cxzyr+tiltsk5*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(6)=56*ekk*(tiltck4*cxzyi-tiltsk4*cxzyr)
        ab2(6)=56*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=70*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=70*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=56*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=56*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=28*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=28*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=8*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-8*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck5*tiltc(k)-tiltsk5*tilts(k)
        tiltsk=tiltck5*tilts(k)+tiltsk5*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=8*ekk*(tiltck*zl-tiltsk*xl)
        ab2(8)=8*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(9)=ekk*tiltsk
        ab2(9)=ekk*tiltck
        goto 480
!--SKEW 20-POLE
  470   ekk=ekk*c1m24
        mpe=20
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-9*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
  480   t(1,2)=t(1,2)+dyy1
        t(1,4)=t(1,4)+dyy2
        do 490 i=2,ium
          t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
          t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
  490   continue
        do 500 l=1,2
          ll=2*l
          alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
  500   continue
        if(mpe.gt.9.or.(mpe.eq.9.and.nmz.le.1)) goto 770
        if(mpe.lt.nta) goto 770
        if(mpe.gt.nte) mpe=nte
        if(nta.gt.2) goto 520
        if(mx.eq.-1.or.mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx  &
     &.eq.5.or.mx.eq.6.or.mx.eq.7) goto 520
!-----------------------------------------------------------------------
!  SKEW-QUADRUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        do 510 l=2,nmz
          l1=l-1
  510   ab2(2)=ab2(2)+l1*(aa(l)*cr(l1)-bb(l)*ci(l1))
  520   b1=beta(1)
        b2=beta(2)
        sb1=sqrt(b1)
        sb2=sqrt(b2)
        b(3,1)=b1
        b(1,3)=b2
        b(2,2)=sb1*sb2
        if(nta.gt.3) goto 540
        if(mpe.eq.2.or.(mpe.eq.9.and.nmz.le.2)) goto 670
        if(mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq&
     &.6.or.mx.eq.7) goto 540
!-----------------------------------------------------------------------
!  REGULAR-SEXTUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 530 l=3,nmz
          l1=l-2
          ab1(3)=ab1(3)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(3)=ab2(3)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  530   l2=l2*l/l1
  540   b(4,1)=b1*sb1
        b(1,4)=b2*sb2
        b(3,2)=b1*sb2
        b(2,3)=b2*sb1
        if(nta.gt.4) goto 560
        if(mpe.eq.3.or.(mpe.eq.9.and.nmz.le.3)) goto 670
        if(mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq&
     &.7) goto 560
!-----------------------------------------------------------------------
!  REGULAR-OCTUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 550 l=4,nmz
          l1=l-3
          ab1(4)=ab1(4)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(4)=ab2(4)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  550   l2=l2*l/l1
  560   b(5,1)=b1*b1
        b(1,5)=b2*b2
        b(4,2)=b(3,2)*sb1
        b(2,4)=b(2,3)*sb2
        b(3,3)=b1*b2
        if(nta.gt.5) goto 580
        if(mpe.eq.4.or.(mpe.eq.9.and.nmz.le.4)) goto 670
        if(mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7)        &
     &goto 580
!-----------------------------------------------------------------------
!  REGULAR-DEKAPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 570 l=5,nmz
          l1=l-4
          ab1(5)=ab1(5)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(5)=ab2(5)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  570   l2=l2*l/l1
  580   b(6,1)=b(5,1)*sb1
        b(1,6)=b(1,5)*sb2
        b(5,2)=b(4,2)*sb1
        b(2,5)=b(2,4)*sb2
        b(4,3)=b(4,2)*sb2
        b(3,4)=b(2,4)*sb1
        if(nta.gt.6) goto 600
        if(mpe.eq.5.or.(mpe.eq.9.and.nmz.le.5)) goto 670
        if(mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 600
!-----------------------------------------------------------------------
!  REGULAR-12-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 590 l=6,nmz
          l1=l-5
          ab1(6)=ab1(6)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(6)=ab2(6)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  590   l2=l2*l/l1
  600   b(7,1)=b(6,1)*sb1
        b(1,7)=b(1,6)*sb2
        b(6,2)=b(5,2)*sb1
        b(2,6)=b(2,5)*sb2
        b(5,3)=b(5,2)*sb2
        b(3,5)=b(2,5)*sb1
        b(4,4)=b(3,4)*sb1
        if(nta.gt.7) goto 620
        if(mpe.eq.6.or.(mpe.eq.9.and.nmz.le.6)) goto 670
        if(mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 620
!-----------------------------------------------------------------------
!  REGULAR-14-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 610 l=7,nmz
          l1=l-6
          ab1(7)=ab1(7)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(7)=ab2(7)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  610   l2=l2*l/l1
  620   b(8,1)=b(7,1)*sb1
        b(1,8)=b(1,7)*sb2
        b(7,2)=b(7,1)*sb2
        b(2,7)=b(1,7)*sb1
        b(6,3)=b(5,3)*sb1
        b(3,6)=b(3,5)*sb2
        b(5,4)=b(4,4)*sb1
        b(4,5)=b(4,4)*sb2
        if(nta.gt.8) goto 640
        if(mpe.eq.7.or.(mpe.eq.9.and.nmz.le.7)) goto 670
        if(mx.eq.6.or.mx.eq.7) goto 640
!-----------------------------------------------------------------------
!  REGULAR-16-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 630 l=8,nmz
          l1=l-7
          ab1(8)=ab1(8)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(8)=ab2(8)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  630   l2=l2*l/l1
  640   b(9,1)=b(8,1)*sb1
        b(1,9)=b(1,8)*sb2
        b(8,2)=b(8,1)*sb2
        b(2,8)=b(1,8)*sb1
        b(7,3)=b(7,2)*sb2
        b(3,7)=b(2,7)*sb1
        b(6,4)=b(6,3)*sb2
        b(4,6)=b(3,6)*sb1
        b(5,5)=b(4,5)*sb1
        if(mpe.eq.8.or.(mpe.eq.9.and.nmz.le.8)) goto 670
        if(mx.eq.7) goto 660
!-----------------------------------------------------------------------
!  REGULAR-18-POLE
!-----------------------------------------------------------------------
        l2=1
        do 650 l=9,nmz
          l1=l-8
          ab1(9)=ab1(9)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(9)=ab2(9)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  650   l2=l2*l/l1
  660   b(10,1)=b(9,1)*sb1
        b(1,10)=b(1,9)*sb2
        b(9,2)=b(9,1)*sb2
        b(2,9)=b(1,9)*sb1
        b(8,3)=b(8,2)*sb2
        b(3,8)=b(2,8)*sb1
        b(4,7)=b(3,7)*sb1
        b(7,4)=b(7,3)*sb2
        b(5,6)=b(4,6)*sb1
        b(6,5)=b(6,4)*sb2
!-----------------------------------------------------------------------
  670   do 700 np=1,mpe
          n2e=2*np
          do 690 nv=1,n2e
            n2=nv-np
            nn2=abs(n2)
            nn1=np-nn2
            re1=nn1*qxt+n2*qzt
            ipt=0
            do 680 ii=1,nre
  680       if(n2.eq.nrr(ii)) ipt=ipr(ii)
            ip(np,nv)=int(re1+half)+ipt
            if(-re1.gt.pieni) ip(np,nv)=-int(abs(re1)+half)-ipt
!--RE=DISTANCE FROM THE RESONANCE
            re(np,nv)=re1-ip(np,nv)
            res=re(np,nv)/radi
            chy(np,nv)=cos(nn1*phi(1)+n2*phi(2)-res*etl)
            shy(np,nv)=sin(nn1*phi(1)+n2*phi(2)-res*etl)
  690     continue
  700   continue
        do 760 np=nta,mpe
          np2=np
          nkk=0
  710     nkk=nkk+1
          n2e=2*np2
          do 750 i=1,nkk
            do 740 nv=1,n2e
              nn2=abs(nv-np2)
              nv1=np2-nn2+(i-1)*2+1
              nv2=np-nv1+2
              rn2=nn2*half
!--EVENESS OF N2
              mm=0
              gerad=rn2-aint(rn2)
              if(abs(gerad).le.pieni) mm=1
!--MM=0 =>N2 UNEVEN, MM=1 => N2 EVEN
              if (mm.eq.0) goto 720
              btc=ab1(np)*b(nv1,nv2)*chy(np2,nv)
              bts=ab1(np)*b(nv1,nv2)*shy(np2,nv)
              goto 730
  720         btc=ab2(np)*b(nv1,nv2)*chy(np2,nv)
              bts=ab2(np)*b(nv1,nv2)*shy(np2,nv)
  730         rtc(np2,nv,np,i)=rtc(np2,nv,np,i)+btc
              rts(np2,nv,np,i)=rts(np2,nv,np,i)+bts
  740       continue
  750     continue
          np2=np2-2
          if(np2.ge.1) goto 710
  760   continue
  770 continue
      nnf(1)=1
      nnf(2)=1
      nnf(3)=2
      nz2(2)=2
      sea=sqrt(ep(1))
      seb=sqrt(ep(2))
      ea=ep(1)
      eb=ep(2)
      e(3,1)=one/eb
      e(1,3)=one/ea
      e(2,2)=one/seb/sea
      nnf(4)=6
      nz2(3)=4
      e(4,1)=sea/eb
      e(1,4)=seb/ea
      e(3,2)=one/seb
      e(2,3)=one/sea
      nnf(5)=24
      nz2(4)=8
      e(5,1)=ea/eb
      e(1,5)=eb/ea
      e(4,2)=sea/seb
      e(2,4)=seb/sea
      e(3,3)=one
      nnf(6)=120
      nz2(5)=16
      e(6,1)=e(5,1)*sea
      e(1,6)=e(1,5)*seb
      e(5,2)=ea/seb
      e(2,5)=eb/sea
      e(4,3)=sea
      e(3,4)=seb
      nnf(7)=720
      nz2(6)=32
      e(7,1)=e(6,1)*sea
      e(1,7)=e(1,6)*seb
      e(6,2)=e(5,2)*sea
      e(2,6)=e(2,5)*seb
      e(5,3)=ea
      e(3,5)=eb
      e(4,4)=sea*seb
      nnf(8)=5040
      nz2(7)=64
      e(8,1)=e(7,1)*sea
      e(1,8)=e(1,7)*seb
      e(7,2)=e(6,2)*sea
      e(2,7)=e(2,6)*seb
      e(6,3)=ea*sea
      e(3,6)=eb*seb
      e(5,4)=ea*seb
      e(4,5)=sea*eb
      nnf(9)=40320
      nz2(8)=128
      e(9,1)=e(8,1)*sea
      e(1,9)=e(1,8)*seb
      e(8,2)=e(7,2)*sea
      e(2,8)=e(2,7)*seb
      e(7,3)=ea*ea
      e(3,7)=eb*eb
      e(6,4)=e(5,4)*sea
      e(4,6)=e(4,5)*seb
      e(5,5)=ea*eb
      nnf(10)=362880
      nz2(9)=256
      e(10,1)=e(9,1)*sea
      e(1,10)=e(1,9)*seb
      e(9,2)=e(8,2)*sea
      e(2,9)=e(2,8)*seb
      e(8,3)=e(7,3)*sea
      e(3,8)=e(3,7)*seb
      e(7,4)=e(6,4)*sea
      e(4,7)=e(4,6)*seb
      e(6,5)=e(5,5)*sea
      e(5,6)=e(5,5)*seb
      do 810 np=nta,nte
        vdt1=nnf(np)/(nz2(np)*pi)
        np2=np
        nkk=0
  780   nkk=nkk+1
        n2e=2*np2
        do 800 i=1,nkk
          do 790 nv=1,n2e
            n2=nv-np2
            nn2=abs(n2)
            nn1=np2-nn2
            nv1=nn1+(i-1)*2+1
            nv2=np-nv1+2
            nv11=nv1-1
            nv21=nv2-1
            nf1=nn1+i
            nf3=nkk-i+1
            nf4=nf3+nn2
            vdt2=vdt1*e(nv1,nv2)/(nnf(nf1)*nnf(i)*nnf(nf3)*nnf(nf4))
            vdt3=nn2*ea+nn1*eb
            if(n2.ge.0) vdt3=n2*nv21*ea+nn1*nv11*eb
            rtc(np2,nv,np,i)=rtc(np2,nv,np,i)*vdt2*vdt3
            rts(np2,nv,np,i)=rts(np2,nv,np,i)*vdt2*vdt3
  790     continue
  800   continue
        np2=np2-2
        if(np2.ge.1) goto 780
  810 continue
      if(nur.eq.0) goto 840
      do 830 j=1,nur
        jk=j*2
        do 820 i=1,nur
          jl=nu(i)-npp-jk
          if(jl.eq.0) min(j)=1
  820   if(jl.eq.0) goto 830
  830 continue
  840 m2=npp+2
      m4=npp+4
      m6=npp+6
      do 850 i=1,nre
        i2=2*i
        i1=i2-1
        n=nrr(i)+npp
        dtr(i1)=rtc(npp,n,npp,1)+min(1)*(-rtc(npp,n,m2,1)+rtc           &
     &(npp,n,m2,2))+min(2)*(rtc(npp,n,m4,1)-rtc(npp,n,m4,2)+rtc         &
     &(npp,n,m4,3))+ min(3)*(-rtc(npp,n,m6,1)+rtc(npp,n,m6,2)-rtc       &
     &(npp,n,m6,3)+ rtc(npp,n,m6,4))
        dtr(i2)=rts(npp,n,npp,1)+min(1)*(-rts(npp,n,m2,1)+rts           &
     &(npp,n,m2,2))+min(2)*(rts(npp,n,m4,1)-rts(npp,n,m4,2)+rts         &
     &(npp,n,m4,3))+ min(3)*(-rts(npp,n,m6,1)+rts(npp,n,m6,2)-rts       &
     &(npp,n,m6,3)+ rts(npp,n,m6,4))
  850 continue
      return
      end
      subroutine rmod(dppr)
!-----------------------------------------------------------------------
!  CALCULATION OF THE STRENGTH OF CORRECTION-ELEMENTS
!-----------------------------------------------------------------------
      implicit none
      integer i,i1,i2,ierr,irr,j,j1,j2,j3,j4,jj1,jj2,jjr,k,n,no,ntao,   &
     &nteo
      double precision aa,bb,d1,de2,dpp,dppr,dsm,ox,oz,qwc,se11,se12,   &
     &se2,sen,sen15,sen16,sen17,sen18,sn,ss
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
      dimension aa(10,10),bb(10),dsm(10),sn(10),sen(10),ss(10)
      dimension qwc(3),d1(10),irr(12)
      save
!-----------------------------------------------------------------------
      ntao=nta
      nteo=nte
      nta=npp
      nte=npp
      dpp=dppr
      do 10 i=1,10
        bb(i)=zero
        dsm(i)=zero
        sn(i)=zero
        sen(i)=zero
        ss(i)=zero
        d1(i)=zero
        do 10 j=1,10
          aa(i,j)=zero
   10 continue
      do 20 i=1,12
        irr(i)=0
   20 continue
      do 30 i=1,3
        qwc(i)=zero
   30 continue
      k=1
      jj1=0
      jj2=0
      jjr=2*nre
      de2=de0*half
      if(nre.eq.0) goto 50
      write(*,10000)
      write(*,10010) npp,totl,qxt,qzt,tam1
      call resex(dpp)
      do 40 i=1,nre
        i2=2*i
        i1=i2-1
        irr(i1)=ire(i1)
        irr(i2)=ire(i2)
        sn(i1)=ed(irr(i1))
        sn(i2)=ed(irr(i2))
        dsm(i1)=dsm0
        dsm(i2)=dsm0
        write(*,10020) i,nrr(i),ipr(i)
        sen(i1)=dtr(i1)
        bb(i1)=sen(i1)
        sen(i2)=dtr(i2)
        bb(i2)=sen(i2)
        ss(i1)=sen(i1)
        ss(i2)=sen(i2)
   40 continue
      j2=jjr
   50 if(nur.eq.0) goto 70
      write(*,10030) nur
      do 60 i=1,nur
        write(*,10040) nu(i),i
   60 continue
   70 if(nch.eq.0) goto 90
      write(*,10050)
      j1=j2+1
      j2=j2+2
      irr(j1)=ire(7)
      irr(j2)=ire(8)
      sn(j1)=ed(irr(j1))
      sn(j2)=ed(irr(j2))
      dsm(j1)=dsm0
      dsm(j2)=dsm0
      se2=zero
      se11=zero
      se12=zero
      do 80 n=1,5
        dpp=de2*(3-n)
        call clorb2(dpp)
        call phasad(dpp,qwc)
        ox=qwc(1)
        oz=qwc(2)
        se2=se2+dpp*dpp
        se11=se11+ox*dpp
        se12=se12+oz*dpp
   80 continue
      sen(j1)=se11/se2
      sen(j2)=se12/se2
      bb(j1)=sen(j1)
      bb(j2)=sen(j2)
      ss(j1)=sen(j1)
      ss(j2)=sen(j2)
   90 if(nqc.eq.0) goto 100
      write(*,10060)
      j1=j2+1
      j2=j2+2
      jj1=j1
      jj2=j2
      irr(j1)=ire(9)
      irr(j2)=ire(10)
      if (abs(el(irr(j1))).le.pieni.or.abs(el(irr(j2))).le.pieni) then
        sn(j1)=ed(irr(j1))
        sn(j2)=ed(irr(j2))
      else
        sn(j1)=ek(irr(j1))
        sn(j2)=ek(irr(j2))
      endif
      dsm(j1)=dkq
      dsm(j2)=dkq
      dpp=zero
      call clorb2(dpp)
      call phasad(dpp,qwc)
      sen(j1)=qwc(1)
      sen(j2)=qwc(2)
      bb(j1)=sen(j1)-qw0(1)
      bb(j2)=sen(j2)-qw0(2)
      ss(j1)=sen(j1)
      ss(j2)=sen(j2)
  100 do 330 no=1,itcro
        do 160 i=1,j2
          if(i.ne.jj1.and.i.ne.jj2) ed(irr(i))=ed(irr(i))+dsm(i)
          if(i.eq.jj1.or.i.eq.jj2) then
            if (abs(el(irr(i))).le.pieni) then
              ed(irr(i))=ed(irr(i))+dsm(i)
            else
              ek(irr(i))=ek(irr(i))+dsm(i)
            endif
          endif
          if(kp(irr(i)).eq.5) call combel(irr(i))
          if(nre.eq.0) goto 120
          call resex(dpp)
          do 110 j=1,jjr
            aa(i,j)=(dtr(j)-ss(j))/dsm(i)
  110     continue
  120     if(nch.eq.0) goto 140
          j3=jjr+1
          j4=jjr+2
          se2=zero
          se11=zero
          se12=zero
          do 130 n=1,5
            dpp=de2*(3-n)
            call clorb2(dpp)
            call phasad(dpp,qwc)
            ox=qwc(1)
            oz=qwc(2)
            se2=se2+dpp*dpp
            se11=se11+ox*dpp
            se12=se12+oz*dpp
  130     continue
          sen15=se11/se2
          sen16=se12/se2
          aa(i,j3)=(sen15-ss(j3))/dsm(i)
          aa(i,j4)=(sen16-ss(j4))/dsm(i)
  140     if(nqc.eq.0) goto 150
          dpp=zero
          call clorb2(dpp)
          call phasad(dpp,qwc)
          sen17=qwc(1)
          sen18=qwc(2)
          aa(i,j1)=(sen17-ss(j1))/dsm(i)
          aa(i,j2)=(sen18-ss(j2))/dsm(i)
  150     continue
          if(i.eq.jj1.or.i.eq.jj2) then
            if (abs(el(irr(i))).le.pieni) then
              ed(irr(i))=ed(irr(i))-dsm(i)
            else
              ek(irr(i))=ek(irr(i))-dsm(i)
            endif
          endif
          if(i.ne.jj1.and.i.ne.jj2)ed(irr(i))=ed(irr(i))-dsm(i)
          if(kp(irr(i)).eq.5) call combel(irr(i))
  160   continue
        call loesd(aa,bb,j2,10,ierr)
        if(ierr.eq.1) call prror(38)
        do 170 i=1,j2
          if(i.eq.jj1.or.i.eq.jj2) then
            if (abs(el(irr(i))).le.pieni) then
              ed(irr(i))=ed(irr(i))-bb(i)
            else
              ek(irr(i))=ek(irr(i))-bb(i)
            endif
          endif
          if(i.ne.jj1.and.i.ne.jj2)ed(irr(i))=ed(irr(i))-bb(i)
          if(kp(irr(i)).eq.5) call combel(irr(i))
  170   continue
        if(nre.eq.0) goto 190
        call resex(dpp)
        do 180 i=1,jjr
          ss(i)=dtr(i)
  180   d1(i)=abs(ss(i))
  190   if(nch.eq.0) goto 210
        se2=zero
        se11=zero
        se12=zero
        do 200 n=1,5
          dpp=de2*(3-n)
          call clorb2(dpp)
          call phasad(dpp,qwc)
          ox=qwc(1)
          oz=qwc(2)
          se2=se2+dpp*dpp
          se11=se11+ox*dpp
          se12=se12+oz*dpp
  200   continue
        ss(j3)=se11/se2
        ss(j4)=se12/se2
        d1(j3)=abs(ss(j3))
        d1(j4)=abs(ss(j4))
  210   if(nqc.eq.0) goto 220
        dpp=zero
        call clorb2(dpp)
        call phasad(dpp,qwc)
        ss(j1)=qwc(1)
        ss(j2)=qwc(2)
        d1(j1)=abs(qwc(1)-qw0(1))
        d1(j2)=abs(qwc(2)-qw0(2))
  220   write(*,10070)
        if(nre.eq.0) goto 270
        write(*,10080) no,nrr(1),sen(1),ss(1),sen(2),ss(2)
        if(nre.eq.1) goto 240
        do 230 i=2,nre
          i2=2*i
          i1=i2-1
  230   write(*,10090) nrr(i),sen(i1),ss(i1),sen(i2),ss(i2)
  240   write(*,10100)
        write(*,10110) bez(irr(1)),sn(1),ed(irr(1)),bez(irr(2)),sn(2),  &
     &ed(irr(2))
        if(nre.eq.1) goto 260
        do 250 i=2,nre
          i2=2*i
          i1=i2-1
  250   write(*,10110) bez(irr(i1)),sn(i1),ed(irr(i1)),bez(irr(i2)),sn  &
     &(i2), ed(irr(i2))
  260   write(*,10070)
  270   if(nch.eq.0) goto 280
        write(*,10120) sen(j3),ss(j3),sen(j4),ss(j4)
        write(*,10110) bez(irr(j3)),sn(j3),ed(irr(j3)),bez(irr(j4)),sn  &
     &(j4), ed(irr(j4))
        write(*,10070)
  280   if(nqc.eq.0) goto 290
        write(*,10130) qw0(1),qwc(1),qw0(2),qwc(2)
        if (abs(el(irr(j1))).le.pieni) then
          write(*,10140) sn(j1),ed(irr(j1)),irr(j1),sn(j2),ed(irr(j2)), &
     &irr(j2)
        else
          write(*,10140) sn(j1),ek(irr(j1)),irr(j1),sn(j2),ek(irr(j2)), &
     &irr(j2)
        endif
  290   do 300 i=1,j2
  300   if(d1(i).gt.dsi) goto 310
        nta=ntao
        nte=nteo
        return
  310   do 320 i=1,j2
  320   bb(i)=ss(i)
        if(nqc.eq.1) bb(j1)=bb(j1)-qw0(1)
        if(nqc.eq.1) bb(j2)=bb(j2)-qw0(2)
  330 continue
      nta=ntao
      nte=nteo
!-----------------------------------------------------------------------
      return
10000 format(t5,'---- ENTRY RMOD ----')
10010 format(/10x,'N=',i1,' IS THE ORDER OF RESONACE, THAT WILL BE',    &
     &' COMPENSATED'// 10x,'L=',f15.6,'; QX=',f10.5,'; QY=',f10.5,      &
     &'; AMAX=',f10.5)
10020 format(/10x,i1,' RESONANCE; NY=',i2,';CHANGE OF P=',i2)
10030 format(/10x,'NUMBER OF SUBRESONANCES THAT ARE CONSIDERED IS ',i2)
10040 format(/10x,'NU=',i2,' IS THE ',i1,' SUBRESONANCE-MULTIPOLE-ORDER'&
     &,i2)
10050 format(/10x,'CHROMATICITY IS COMPENSATED')
10060 format(/10x,'Q-VALUES ARE ADJUSTED')
10070 format(131('-'))
10080 format(/10x,'RESONANCE-CORRECTION     ITERATION #',i2// 15x,      &
     &'DRIVING-TERM',13x,'BEFORE         AFTER     COMPENSATION'// 10x, &
     &'NY=',i2,'  COS-COMPONENT  ',2g15.5/ 17x,'SIN-COMPONENT  ',2g15.5/&
     &)
10090 format(10x,'NY=',i2,'  COS-COMPONENT  ',2g15.5/ 17x,              &
     &'SIN-COMPONENT  ',2g15.5/)
10100 format(10x,'  ELEMENT NAME'/)
10130 format(10x,'Q-VARIATION' / 10x,                                   &
     &'Q-VALUE            THEORET.        AFTER     COMPENSATION'/ 10x, &
     &'HORIZONTAL     ',2g15.7/ 10x,'VERTICAL       ',2g15.7/)
10140 format(10x,'QUADRU.STRENGTH',2g15.8,'   INDEX ',i3/ 10x,          &
     &'               ',2g15.8,'         ',i3)
10120 format(10x,'CHROMATICITY-CORRECTION'/ 15x,'CHROMATICITY',13x,     &
     &'BEFORE         AFTER     COMPENSATION'// 19x,'HORIZONTAL   ',2g15&
     &.5/ 19x,'VERTICAL     ',2g15.5/ 10x,'   SEXTUPOLE'/)
10110 format(14x,a16,2x,g16.10,1x,g16.10/14x,a16,2x,g16.10,1x,g16.10)
      end
      subroutine search(dpp)
!-----------------------------------------------------------------------
!  FINDING THE BEST POSITIONS FOR CORRECTION-ELEMENTS
!-----------------------------------------------------------------------
      implicit none
      integer i,id,n21,n22,n23,ntao,nteo
      double precision b,c,c1,c2,c3,d,dpp,e,f,g,s1,s2,s3
      character*16 ref
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
      save
!-----------------------------------------------------------------------
      ntao=nta
      nteo=nte
      nta=mp
      nte=mp
      ref='REFERENCE       '
      id=0
      write(*,10010)
      write(*,10000)
      write(*,10010)
      write(*,10020) mp
      write(*,10010)
      write(*,10030) m21,ise1,m22,ise2,m23,ise3
      write(*,10010)
      write(*,10040)
      write(*,10010)
      n21=m21+mp
      n22=m22+mp
      n23=m23+mp
      ipt=ise1
      call subsea(dpp)
      c1=rtc(mp,n21,mp,1)
      s1=rts(mp,n21,mp,1)
      ipt=ise2
      call subsea(dpp)
      c2=rtc(mp,n22,mp,1)
      s2=rts(mp,n22,mp,1)
      ipt=ise3
      call subsea(dpp)
      c3=rtc(mp,n23,mp,1)
      s3=rts(mp,n23,mp,1)
      write(*,10050) ref,id,c1,s1,c2,s2,c3,s3
      do 10 i=1,mesa
        ed(isea(i))=ed(isea(i))+dsm0
        if(kp(isea(i)).eq.5) call combel(isea(i))
        ipt=ise1
        call subsea(dpp)
        b=rtc(mp,n21,mp,1)-c1
        c=rts(mp,n21,mp,1)-s1
        ipt=ise2
        call subsea(dpp)
        d=rtc(mp,n22,mp,1)-c2
        e=rts(mp,n22,mp,1)-s2
        ipt=ise3
        call subsea(dpp)
        f=rtc(mp,n23,mp,1)-c3
        g=rts(mp,n23,mp,1)-s3
        write(*,10050) bez(isea(i)),i,b,c,d,e,f,g
        ed(isea(i))=ed(isea(i))-dsm0
        if(kp(isea(i)).eq.5) call combel(isea(i))
   10 continue
      nta=ntao
      nte=nteo
!-----------------------------------------------------------------------
      return
10000 format(t5,'---- ENTRY SEARCH ----')
10010 format(1x ,131('-'))
10020 format(10x,///'RESONANCES OF ORDER',i4,'  ARE CONSIDERED'//)
10030 format(24x ,'|',6x,'NY =',i4,';D-P= ',i4,7x, '|',6x,'NY =',i4,    &
     &';D-P= ',i4,7x,'|',6x,'NY =',i4,';D-P= ',i4,7x, '|')
10040 format(1x,'ELEMENT          | POS |',6x,'COS',13x,'SIN',6x,'|',   &
     &6x,'COS',13x,'SIN',6x,'|', 6x,'COS',13x,'SIN',6x,'|')
10050 format(1x,a16,1x,'|',i3,'  |',g15.5,'|',g15.5,'|',g15.5,'|',      &
     &g15.5,'|',g15.5,'|',g15.5,'|')
      end
      subroutine subre(dpp)
!-----------------------------------------------------------------------
!  CALCULATION OF RESONANCE- AND SUBRESONANCE-DRIVINGTERMS
!-----------------------------------------------------------------------
      implicit none
      integer i,ii,ik,im,ip,ipc,ipcc,ipl,ium,iv,ix,izu,j,jj,jk,jm,k,    &
     &k1,kpz,kzz,l,l1,l2,ll,lmin,min1,min2,mis,mm,mpe,mx,n2,n22,n2e,nf1,&
     &nf3,nf4,nkk,nmz,nn1,nn2,nnf,np,np2,nph,nr,ns,ntx,nv,nv1,nv11,nv2, &
     &nv21,nz2,dj
      double precision aa,ab1,ab2,alfa,b,b1,b2,bb,benkr,beta,btc,bts,cc,&
     &chy,ci,cikve,clo0,clop0,cr,crkve,cxzi,cxzr,cxzyi,cxzyr,cxzyrr,del,&
     &dfac,dphi,dpp,dpp1,dppi,dpr,dt,dtu,dtup,dyy1,dyy2,e,ea,eb,ekk,    &
     &ekko,ep,etl,gerad,gtu1,gtu2,phi,phibf,phy,pie,puf,qu,qv,qw,qwc,r0,&
     &r0a,radi,rc,re,re1,res,rn2,rs,sb1,sb2,sdel,sdel2,sea,seb,shy,ss,t,&
     &vdt1,vdt2,vdt3,vdt4,xl,xs,zl,zs
      double precision dyy11,qu1,tiltck,tiltck1,tiltck2,tiltck3,tiltck4,&
     &tiltck5,tiltck6,tiltck8,tiltck10,tiltckuk,tiltsk,tiltsk1,tiltsk2, &
     &tiltsk3,tiltsk4,tiltsk5,tiltsk6,tiltsk8,tiltsk10
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
      dimension t(6,4)
      dimension beta(2),alfa(2),phi(2),phibf(2)
      dimension clo0(2),clop0(2)
      dimension aa(mmul),bb(mmul)
      dimension qw(2),qwc(3),dpr(6)
      dimension nnf(10),ep(2)
      dimension ab1(10),ab2(10),re(10,18),ip(10,18)
      dimension b(10,10),nz2(9),e(10,10)
      dimension chy(9,18),shy(9,18)
      dimension dfac(10),dtu(2,5),dtup(2,5,0:4,0:4)
      dimension cr(mmul),ci(mmul)
      save
!-----------------------------------------------------------------------
      ium=5
      ipl=1
      gtu1=zero
      gtu2=zero
      dfac(1)=one
      dfac(2)=one
      dfac(3)=two
      dfac(4)=6
      dfac(5)=24
      dfac(6)=120
      dfac(7)=720
      dfac(8)=5040
      dfac(9)=40320
      dfac(10)=362880
      if(ipt.eq.1) ipl=3
      do 940 ipcc=1,ipl
        ipc=ipcc-ipl+1
        if(ipt.eq.0) ipc=0
        btc=zero
        bts=zero
        phy=zero
        dt=zero
        del=zero
        ns=0
        ik=0
        do 10 i=1,ium
          dpr(i)=zero
   10   continue
        do 20 i=1,ium
          do 20 j=1,4
            t(i,j)=zero
   20   continue
        do 30 i=1,2
          beta(i)=zero
          alfa(i)=zero
          phi(i)=zero
          phibf(i)=zero
          qw(i)=zero
          qwc(i)=zero
          clo0(i)=zero
          clop0(i)=zero
          ep(i)=zero
   30   continue
        qwc(3)=zero
        do 40 i=1,10
          nnf(i)=0
          do 40 j=1,18
            re(i,j)=zero
            ip(i,j)=0
   40   continue
        do 50 i=1,mmul
          aa(i)=zero
          bb(i)=zero
          cr(i)=zero
          ci(i)=zero
   50   continue
        do 60 i=1,2
          do 60 j=1,5
            dtu(i,j)=zero
   60   continue
        do 70 i=1,5
          do 70 j=0,4
            do 70 k=0,4
              dtup(1,i,j,k)=zero
              dtup(2,i,j,k)=zero
   70   continue
        do 120 i=1,9
          nz2(i)=0
          do 110 j=1,18
            chy(i,j)=zero
            shy(i,j)=zero
            do 100 k=1,10
              do 80 ii=1,10
                e(k,ii)=zero
                b(k,ii)=zero
   80         continue
              do 90 l=1,5
                rtc(i,j,k,l)=zero
                rts(i,j,k,l)=zero
   90         continue
  100       continue
  110     continue
  120   continue
        write(*,10030)
        write(*,10020)
        pie=two*pi
        etl=zero
        radi=totl/pie
        nr=0
        dpr(1)=dpp*c1e3
        dpr(6)=c1e3
        dpp1=dpp+ded
        call clorb(dpp1)
        do 130 l=1,2
          clo0(l)=clo(l)
  130   clop0(l)=clop(l)
        call clorb(dpp)
        do 140 l=1,2
          di0(l)=(clo0(l)-clo(l))/ded
  140   dip0(l)=(clop0(l)-clop(l))/ded
        write(*,10030)
        write(*,10120) (di0(l),dip0(l),l=1,2)
        call betalf(dpp,qw)
        call phasad(dpp,qwc)
        if(ierro.ne.0) call prror(22+ierro)
        write(*,10070) dpp,qwc(1),qwc(2)
        call envar(dpp)
!--STARTVALUES OF THE TRAJECTORIES
        do 150 l=1,2
          ll=2*l
          alfa(l)=alf0(l)
          beta(l)=bet0(l)
          t(1,ll-1)=clo(l)
          t(1,ll)=clop(l)
          clo0(l)=clo(l)
  150   clop0(l)=clop(l)
        do 160 i=1,4
          do 160 j=1,4
            t(i+1,j)=ta(j,i)
  160   t(i+1,j)=ta(j,i)
        write(*,10030)
        write(*,10040)
        write(*,10030)
        write(*,10010) nr,'START   ',zero,zero,(beta(l),alfa(l),phi(l), &
     &di0(l),dip0(l),clo0(l),clop0(l),l=1,2)
!--EP=EMITTANCE IN PI*MM*MRAD
        ep(1)=tam1*tam1/beta(1)
        ep(2)=tam2*tam2/beta(2)
        write(*,10050) tam1,ep(1),tam2,ep(2)
        write(*,10030)
!--SINGLE TURN BLOCKLOOP
        izu=0
        do 790 k=1,iu
          do 170 k1=1,10
            ab1(k1)=zero
  170     ab2(k1)=zero
          ix=ic(k)
          if(ix.gt.nblo) goto 250
          jj=0
          dj=1
          if(ix.gt.0) goto 180
          ix=-ix
          jj=mel(ix)+1
          dj=-1
  180     jm=mel(ix)
!--SINGLE TURN BLOCKLOOP
          do 240 j=1,jm
            jj=jj+dj
            jk=mtyp(ix,jj)
            if(ithick.eq.1.and.kz(jk).ne.0) goto 210
            if(ithick.eq.0.and.kz(jk).ne.0) goto 790
!--PURE DRIFTLENGTH
            etl=etl+el(jk)
            do 190 l=1,2
              ll=2*l
              if(abs(t(ll,ll-1)).gt.pieni) then
                phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
              else
                phibf(l)=pi2
              endif
              do 190 i=1,ium
  190       t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
            do 200 l=1,2
              ll=2*l
              beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
              alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
              clo0(l)=t(1,ll-1)
              clop0(l)=t(1,ll)
              if(abs(t(ll,ll-1)).gt.pieni) then
                dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
              else
                dphi=pi2-phibf(l)
              endif
              if(-dphi.gt.pieni) dphi=dphi+pi
  200       phi(l)=phi(l)+dphi/pie
            nr=nr+1
            goto 240
!--MAGNETELEMENT
  210       continue
            if(kz(jk).ne.8) etl=etl+el(jk)
            do l=1,2
              ll=2*l
              if(abs(t(ll,ll-1)).gt.pieni) then
                phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
              else
                phibf(l)=zero
              endif
              do i=1,ium
                puf=t(i,ll-1)
                t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a      &
     &(jk,l,5)
                t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
              enddo
            enddo
            do l=1,2
              ll=2*l
              beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
              alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
              clo0(l)=t(1,ll-1)
              clop0(l)=t(1,ll)
              if(abs(t(ll,ll-1)).gt.pieni) then
                dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
              else
                dphi=-phibf(l)
              endif
              if(kz(jk).ne.8.and.-dphi.gt.pieni) dphi=dphi+pi
              phi(l)=phi(l)+dphi/pie
            enddo
            nr=nr+1
  240     continue
          goto 790
!--NL-INSERTION
  250     ix=ix-nblo
          qu=zero
          qv=zero
          kpz=kp(ix)
          if(kpz.eq.6) goto 790
          kzz=kz(ix)
        if(kzz.eq.22) then
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*rrtr(imtr(ix),ll-1,ll-1)+                   &
     &t(i,ll)*rrtr(imtr(ix),ll-1,ll)+                                   &
     &dpr(i)*rrtr(imtr(ix),ll-1,6)
              t(i,ll)=puf*rrtr(imtr(ix),ll,ll-1)+                       &
     &t(i,ll)*rrtr(imtr(ix),ll,ll)+                                     &
     &dpr(i)*rrtr(imtr(ix),ll,6)
            enddo
            t(1,ll-1)=t(1,ll-1)+cotr(imtr(ix),ll-1)
            t(1,ll)=t(1,ll)+cotr(imtr(ix),ll)
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi
          enddo
        endif
          clo0(1)=t(1,1)
          clop0(1)=t(1,2)
          clo0(2)=t(2,3)
          clop0(2)=t(2,4)
          if(kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 790
          dyy1=zero
          dyy2=zero
          if(iorg.lt.0) mzu(k)=izu
          izu=mzu(k)+1
          ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
          izu=izu+1
          xs=xpl(ix)+zfz(izu)*xrms(ix)
          izu=izu+1
          zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(t(1,1)-xs)*tiltc(k)+(t(1,3)-zs)*tilts(k)
        zl=-(t(1,1)-xs)*tilts(k)+(t(1,3)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
          if(kzz.lt.0) goto 400
          goto(260,270,280,290,300,310,320,330,340,350,360),kzz
          goto 790
!--HORIZONTAL DIPOLE
  260     ekk=ekk*c1e3
        mpe=20
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
          goto 510
!--NORMAL QUADRUPOLE
  270     continue
        dyy1=ekk*(tiltc(k)*xl+tilts(k)*zl)
        dyy2=ekk*(-tiltc(k)*zl+tilts(k)*xl)
        mpe=20
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        ab1(2)=qu
        ab2(2)=-qv
          goto 510
!--NORMAL SEXTUPOLE
  280     ekk=ekk*c1m3
        mpe=3
        mx=1
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*xl+tiltsk*zl)
        qv=ekk*two*(tiltck*zl-tiltsk*xl)
        ab1(2)=qu
        ab2(2)=-qv
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=ekk*tiltck
        ab2(3)=ekk*tiltsk
          goto 510
!--NORMAL OCTUPOLE
  290     ekk=ekk*c1m6
        mpe=4
        mx=2
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=three*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=three*ekk*(tiltck*xl+tiltsk*zl)
        ab2(3)=three*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=ekk*tiltck
        ab2(4)=ekk*tiltsk
          call detune(2,ekk,ep,beta,dtu,dtup,dfac)
          goto 510
!--NORMAL DECAPOLE
  300     ekk=ekk*c1m9
        mpe=5
        mx=3
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        ab1(3)=6*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=6*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=four*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=four*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=four*ekk*(tiltck*xl+tiltsk*zl)
        ab2(4)=four*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=ekk*tiltck
        ab2(5)=ekk*tiltsk
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          goto 510
!--NORMAL DODECAPOLE
  310     ekk=ekk*c1m12
        mpe=6
        mx=4
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        ab1(4)=10*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=10*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=10*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=10*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=5*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=5*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=5*ekk*(tiltck*xl+tiltsk*zl)
        ab2(5)=5*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=ekk*tiltck
        ab2(6)=ekk*tiltsk
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ekk,ep,beta,dtu,dtup,dfac)
          goto 510
!--NORMAL 14-POLE
  320     ekk=ekk*c1m15
        mpe=7
        mx=5
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        ab1(5)=15*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=15*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=20*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=20*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=15*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(5)=15*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=6*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=6*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=6*ekk*(tiltck*xl+tiltsk*zl)
        ab2(6)=6*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=ekk*tiltck
        ab2(7)=ekk*tiltsk
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          goto 510
!--NORMAL 16-POLE
  330     ekk=ekk*c1m18
        mpe=8
        mx=6
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        ab1(6)=21*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        ab2(6)=21*ekk*(-tiltck4*cxzyi+tiltsk4*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=35*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=35*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=35*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=35*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=21*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=21*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=7*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=7*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=7*ekk*(tiltck*xl+tiltsk*zl)
        ab2(7)=7*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=ekk*tiltck
        ab2(8)=ekk*tiltsk
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          call detune(4,ekk,ep,beta,dtu,dtup,dfac)
          goto 510
!--NORMAL 18-POLE
  340     ekk=ekk*c1m21
        mpe=9
        mx=7
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk5=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck5=tiltckuk
        ab1(7)=28*ekk*(tiltck5*cxzyr+tiltsk5*cxzyi)
        ab2(7)=28*ekk*(-tiltck5*cxzyi+tiltsk5*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(6)=56*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        ab2(6)=56*ekk*(-tiltck4*cxzyi+tiltsk4*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=70*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=70*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=56*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=56*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=28*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=28*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=8*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=8*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck5*tiltc(k)-tiltsk5*tilts(k)
        tiltsk=tiltck5*tilts(k)+tiltsk5*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=8*ekk*(tiltck*xl+tiltsk*zl)
        ab2(8)=8*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(9)=ekk*tiltck
        ab2(9)=ekk*tiltsk
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          call detune(4,ab1(8),ep,beta,dtu,dtup,dfac)
          goto 510
!--NORMAL 20-POLE
  350     ekk=ekk*c1m24
        mpe=20
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=9*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
          tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
          tiltsk=two*tiltc(k)*tilts(k)
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk4=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck4=tiltckuk
          tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
          tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk6=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck6=tiltckuk
          tiltckuk=tiltck6*tiltc(k)-tiltsk6*tilts(k)
          tiltsk=tiltck6*tilts(k)+tiltsk6*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk8=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck8=tiltckuk
          tiltckuk=tiltck8*tiltc(k)-tiltsk8*tilts(k)
          tiltsk=tiltck8*tilts(k)+tiltsk8*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk10=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck10=tiltckuk
          ekko=ekk
          ekk=ekko*tiltck10
          call detune(5,ekk,ep,beta,dtu,dtup,dfac)
          cxzyr=cxzr*cxzr-cxzi*cxzi
          cxzyi=cxzr*cxzi+cxzi*cxzr
          ekk=36*ekko*(tiltck8*cxzyr+tiltsk8*cxzyi)
          call detune(4,ekk,ep,beta,dtu,dtup,dfac)
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          ekk=126*ekko*(tiltck6*cxzyr+tiltsk6*cxzyi)
          call detune(3,ekk,ep,beta,dtu,dtup,dfac)
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          ekk=84*ekko*(tiltck4*cxzyr+tiltsk4*cxzyi)
          call detune(2,ekk,ep,beta,dtu,dtup,dfac)
          goto 510
  360     r0=ek(ix)
          if(abs(dki(ix,1)).gt.pieni) then
            if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)                     &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            do 363 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  363       continue
            else
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tiltc(k)                             &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
            endif
          endif
          if(abs(dki(ix,2)).gt.pieni) then
            if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)+(qu*zl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+(-qu*zl+dppi*dpp)*tiltc(k)                    &
     &-dppi*(one-tiltc(k))
            do 366 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  366       continue
            else
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+dppi*dpp*tiltc(k)                             &
     &-dppi*(one-tiltc(k))
            endif
          endif
          mpe=9
          mx=0
          if(abs(r0).le.pieni) goto 790
          nmz=nmu(ix)
          if(nmz.eq.0) then
            izu=izu+2*mmul
            goto 790
          endif
          im=irm(ix)
          r0a=one
          benkr=ed(ix)/(one+dpp)
          cr(1)=one
          cr(2)=xl
          ci(2)=zl
          cxzyr=xl
          cxzyi=zl
          cxzr=cxzyr
          cxzi=cxzyi
          dyy1=zero
          dyy2=zero
          qu=zero
          qv=zero
          lmin=3
          if(nmz.eq.1) lmin=2
          do 370 l=lmin,mmul
            aa(l)=zero
            bb(l)=zero
            cr(l)=zero
  370     ci(l)=zero
          do 380 l=1,nmz
          l1=l-1
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
          if(l.gt.2) then
            cxzyrr=cxzyr*cxzr-cxzyi*cxzi
            cxzyi=cxzyr*cxzi+cxzyi*cxzr
            cxzyr=cxzyrr
            cr(l)=cxzyr
            ci(l)=cxzyi
          endif
          dyy1=dyy1+bb(l)*cr(l)+aa(l)*ci(l)
          dyy2=dyy2-bb(l)*ci(l)+aa(l)*cr(l)
          if(l.gt.1.and.ium.ne.1) then
            qu=qu+l1*(bb(l)*cr(l1)+aa(l)*ci(l1))
            qv=qv+l1*(bb(l)*ci(l1)-aa(l)*cr(l1))
          endif
  380     continue
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
          izu=izu+2*mmul-2*nmz
          do 390 iv=2,5
            tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
            tiltsk=two*tiltc(k)*tilts(k)
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck=tiltckuk
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk4=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck4=tiltckuk
            tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
            tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
            tiltck=tiltckuk
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk6=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck6=tiltckuk
            tiltckuk=tiltck6*tiltc(k)-tiltsk6*tilts(k)
            tiltsk=tiltck6*tilts(k)+tiltsk6*tiltc(k)
            tiltck=tiltckuk
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk8=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck8=tiltckuk
            tiltckuk=tiltck8*tiltc(k)-tiltsk8*tilts(k)
            tiltsk=tiltck8*tilts(k)+tiltsk8*tiltc(k)
            tiltck=tiltckuk
            tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
            tiltsk10=tiltck*tilts(k)+tiltsk*tiltc(k)
            tiltck10=tiltckuk
            if(iv.eq.2) then
              ekk= tiltck4* bb(4)                     +                 &
     &tiltsk4*(            -aa(4)       )+                              &
     &4  *tiltck4*(bb(5) *cr(2)+aa(5) *ci(2))+                          &
     &4  *tiltsk4*(bb(5) *ci(2)-aa(5) *cr(2))+                          &
     &10 *tiltck4*(bb(6) *cr(3)+aa(6) *ci(3))+                          &
     &10 *tiltsk4*(bb(6) *ci(3)-aa(6) *cr(3))+                          &
     &20 *tiltck4*(bb(7) *cr(4)+aa(7) *ci(4))+                          &
     &20 *tiltsk4*(bb(7) *ci(4)-aa(7) *cr(4))+                          &
     &35 *tiltck4*(bb(8) *cr(5)+aa(8) *ci(5))+                          &
     &35 *tiltsk4*(bb(8) *ci(5)-aa(8) *cr(5))+                          &
     &56 *tiltck4*(bb(9) *cr(6)+aa(9) *ci(6))+                          &
     &56 *tiltsk4*(bb(9) *ci(6)-aa(9) *cr(6))+                          &
     &84 *tiltck4*(bb(10)*cr(7)+aa(10)*ci(7))+                          &
     &84 *tiltsk4*(bb(10)*ci(7)-aa(10)*cr(7))
            endif
            if(iv.eq.3) then
              ekk= tiltck6* bb(6)                     +                 &
     &tiltsk6*(            -aa(6)       )+                              &
     &6  *tiltck6*(bb(7) *cr(2)+aa(7) *ci(2))+                          &
     &6  *tiltsk6*(bb(7) *ci(2)-aa(7) *cr(2))+                          &
     &21 *tiltck6*(bb(8) *cr(3)+aa(8) *ci(3))+                          &
     &21 *tiltsk6*(bb(8) *ci(3)-aa(8) *cr(3))+                          &
     &56 *tiltck6*(bb(9) *cr(4)+aa(9) *ci(4))+                          &
     &56 *tiltsk6*(bb(9) *ci(4)-aa(9) *cr(4))+                          &
     &126*tiltck6*(bb(10)*cr(5)+aa(10)*ci(5))+                          &
     &126*tiltsk6*(bb(10)*ci(5)-aa(10)*cr(5))
            endif
            if(iv.eq.4) then
              ekk= tiltck8* bb(8)                     +                 &
     &tiltsk8*(            -aa(8)       )+                              &
     &8  *tiltck8*(bb(9) *cr(2)+aa(9) *ci(2))+                          &
     &8  *tiltsk8*(bb(9) *ci(2)-aa(9) *cr(2))+                          &
     &36 *tiltck8*(bb(10)*cr(3)+aa(10)*ci(3))+                          &
     &36 *tiltsk8*(bb(10)*ci(3)-aa(10)*cr(3))
            endif
            if(iv.eq.5) then
              ekk= tiltck10*bb(10)-tiltsk10*aa(10)
            endif
            call detune(iv,ekk,ep,beta,dtu,dtup,dfac)
  390     continue
          goto 510
!--SKEW ELEMENTS
  400     kzz=-kzz
          goto(410,420,430,440,450,460,470,480,490,500),kzz
          goto 790
!--VERTICAL DIPOLE
  410     ekk=ekk*c1e3
        mpe=20
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
          goto 510
!--SKEW QUADRUPOLE
  420     continue
        dyy1=ekk*(tiltc(k)*zl-tilts(k)*xl)
        dyy2=ekk*(tiltc(k)*xl+tilts(k)*zl)
        mpe=2
        mx=-1
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        ab1(2)=qu
        ab2(2)=-qv
          goto 510
!--SKEW SEXTUPOLE
  430     ekk=ekk*c1m3
        mpe=3
        mx=1
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*zl-tiltsk*xl)
        qv=-ekk*two*(tiltck*xl+tiltsk*zl)
        ab1(2)=qu
        ab2(2)=-qv
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=ekk*tiltsk
        ab2(3)=ekk*tiltck
          goto 510
!--SKEW OCTUPOLE
  440     ekk=ekk*c1m6
        mpe=4
        mx=2
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-three*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=three*ekk*(tiltck*zl-tiltsk*xl)
        ab2(3)=three*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=ekk*tiltsk
        ab2(4)=ekk*tiltck
          goto 510
!--SKEW DECAPOLE
  450     ekk=ekk*c1m9
        mpe=5
        mx=3
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        ab1(3)=6*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=6*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=four*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-four*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=four*ekk*(tiltck*zl-tiltsk*xl)
        ab2(4)=four*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=ekk*tiltsk
        ab2(5)=ekk*tiltck
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          goto 510
!--SKEW DODECAPOLE
  460     ekk=ekk*c1m12
        mpe=6
        mx=4
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        ab1(4)=10*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=10*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=10*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=10*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=5*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-5*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=5*ekk*(tiltck*zl-tiltsk*xl)
        ab2(5)=5*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=ekk*tiltsk
        ab2(6)=ekk*tiltck
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          goto 510
!--SKEW 14-POLE
  470     ekk=ekk*c1m15
        mpe=7
        mx=5
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        ab1(5)=15*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=15*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=20*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=20*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=15*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=15*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=6*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-6*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=6*ekk*(tiltck*zl-tiltsk*xl)
        ab2(6)=6*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=ekk*tiltsk
        ab2(7)=ekk*tiltck
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          goto 510
!--SKEW 16-POLE
  480     ekk=ekk*c1m18
        mpe=8
        mx=6
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        ab1(6)=21*ekk*(tiltck4*cxzyi-tiltsk4*cxzyr)
        ab2(6)=21*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=35*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=35*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=35*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=35*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=21*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=21*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=7*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-7*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=7*ekk*(tiltck*zl-tiltsk*xl)
        ab2(7)=7*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=ekk*tiltsk
        ab2(8)=ekk*tiltck
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          goto 510
!--SKEW 18-POLE
  490     ekk=ekk*c1m21
        mpe=9
        mx=7
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk5=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck5=tiltckuk
        ab1(7)=28*ekk*(tiltck5*cxzyi-tiltsk5*cxzyr)
        ab2(7)=28*ekk*(tiltck5*cxzyr+tiltsk5*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(6)=56*ekk*(tiltck4*cxzyi-tiltsk4*cxzyr)
        ab2(6)=56*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=70*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=70*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=56*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=56*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=28*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=28*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=8*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-8*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck5*tiltc(k)-tiltsk5*tilts(k)
        tiltsk=tiltck5*tilts(k)+tiltsk5*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=8*ekk*(tiltck*zl-tiltsk*xl)
        ab2(8)=8*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(9)=ekk*tiltsk
        ab2(9)=ekk*tiltck
          call detune(2,ab1(4),ep,beta,dtu,dtup,dfac)
          call detune(3,ab1(6),ep,beta,dtu,dtup,dfac)
          call detune(4,ab1(8),ep,beta,dtu,dtup,dfac)
          goto 510
!--SKEW 20-POLE
  500     ekk=ekk*c1m24
        mpe=20
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-9*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
          tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
          tiltsk=two*tiltc(k)*tilts(k)
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk4=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck4=tiltckuk
          tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
          tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk6=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck6=tiltckuk
          tiltckuk=tiltck6*tiltc(k)-tiltsk6*tilts(k)
          tiltsk=tiltck6*tilts(k)+tiltsk6*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk8=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck8=tiltckuk
          tiltckuk=tiltck8*tiltc(k)-tiltsk8*tilts(k)
          tiltsk=tiltck8*tilts(k)+tiltsk8*tiltc(k)
          tiltck=tiltckuk
          tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
          tiltsk10=tiltck*tilts(k)+tiltsk*tiltc(k)
          tiltck10=tiltckuk
          ekko=ekk
          ekk=-ekko*tiltsk10
          call detune(5,ekk,ep,beta,dtu,dtup,dfac)
          cxzyr=cxzr*cxzr-cxzi*cxzi
          cxzyi=cxzr*cxzi+cxzi*cxzr
          ekk=36*ekko*(tiltck8*cxzyi-tiltsk8*cxzyr)
          call detune(4,ekk,ep,beta,dtu,dtup,dfac)
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          ekk=126*ekko*(tiltck6*cxzyi-tiltsk6*cxzyr)
          call detune(3,ekk,ep,beta,dtu,dtup,dfac)
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          cxzyrr=cxzyr*cxzr-cxzyi*cxzi
          cxzyi=cxzyr*cxzi+cxzyi*cxzr
          cxzyr=cxzyrr
          ekk=84*ekko*(tiltck4*cxzyi-tiltsk4*cxzyr)
          call detune(2,ekk,ep,beta,dtu,dtup,dfac)
  510     t(1,2)=t(1,2)+dyy1
          t(1,4)=t(1,4)+dyy2
          do 520 i=2,ium
            t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
            t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
  520     continue
          do 530 l=1,2
            ll=2*l
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
  530     clop0(l)=t(1,ll)
          if(mpe.gt.9.or.(mpe.eq.9.and.nmz.le.1)) goto 790
          if(mpe.lt.nta) goto 790
          if(mpe.gt.nte) mpe=nte
          if(nta.gt.2) goto 550
          if(mx.eq.-1.or.mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.  &
     &mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 550
!-----------------------------------------------------------------------
!  SKEW-QUADRUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
          do 540 l=2,nmz
            l1=l-1
  540     ab2(2)=ab2(2)+l1*(aa(l)*cr(l1)-bb(l)*ci(l1))
  550     b1=beta(1)
          b2=beta(2)
          sb1=sqrt(b1)
          sb2=sqrt(b2)
          b(3,1)=b1
          b(1,3)=b2
          b(2,2)=sb1*sb2
          if(nta.gt.3) goto 570
          if(mpe.eq.2.or.(mpe.eq.9.and.nmz.le.2)) goto 700
          if(mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx &
     &.eq.6.or.mx.eq.7) goto 570
!-----------------------------------------------------------------------
!  REGULAR-SEXTUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
          l2=1
          do 560 l=3,nmz
            l1=l-2
            ab1(3)=ab1(3)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(3)=ab2(3)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  560     l2=l2*l/l1
  570     b(4,1)=b1*sb1
          b(1,4)=b2*sb2
          b(3,2)=b1*sb2
          b(2,3)=b2*sb1
          if(nta.gt.4) goto 590
          if(mpe.eq.3.or.(mpe.eq.9.and.nmz.le.3)) goto 700
          if(mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx &
     &.eq.7) goto 590
!-----------------------------------------------------------------------
!  REGULAR-OCTUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
          l2=1
          do 580 l=4,nmz
            l1=l-3
            ab1(4)=ab1(4)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(4)=ab2(4)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  580     l2=l2*l/l1
  590     b(5,1)=b1*b1
          b(1,5)=b2*b2
          b(4,2)=b(3,2)*sb1
          b(2,4)=b(2,3)*sb2
          b(3,3)=b1*b2
          if(nta.gt.5) goto 610
          if(mpe.eq.4.or.(mpe.eq.9.and.nmz.le.4)) goto 700
          if(mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7)      &
     &goto 610
!-----------------------------------------------------------------------
!  REGULAR-DEKAPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
          l2=1
          do 600 l=5,nmz
            l1=l-4
            ab1(5)=ab1(5)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(5)=ab2(5)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  600     l2=l2*l/l1
  610     b(6,1)=b(5,1)*sb1
          b(1,6)=b(1,5)*sb2
          b(5,2)=b(4,2)*sb1
          b(2,5)=b(2,4)*sb2
          b(4,3)=b(4,2)*sb2
          b(3,4)=b(2,4)*sb1
          if(nta.gt.6) goto 630
          if(mpe.eq.5.or.(mpe.eq.9.and.nmz.le.5)) goto 700
          if(mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 630
!-----------------------------------------------------------------------
!  REGULAR-12-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
          l2=1
          do 620 l=6,nmz
            l1=l-5
            ab1(6)=ab1(6)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(6)=ab2(6)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  620     l2=l2*l/l1
  630     b(7,1)=b(6,1)*sb1
          b(1,7)=b(1,6)*sb2
          b(6,2)=b(5,2)*sb1
          b(2,6)=b(2,5)*sb2
          b(5,3)=b(5,2)*sb2
          b(3,5)=b(2,5)*sb1
          b(4,4)=b(3,4)*sb1
          if(nta.gt.7) goto 650
          if(mpe.eq.6.or.(mpe.eq.9.and.nmz.le.6)) goto 700
          if(mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 650
!-----------------------------------------------------------------------
!  REGULAR-14-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
          l2=1
          do 640 l=7,nmz
            l1=l-6
            ab1(7)=ab1(7)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(7)=ab2(7)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  640     l2=l2*l/l1
  650     b(8,1)=b(7,1)*sb1
          b(1,8)=b(1,7)*sb2
          b(7,2)=b(7,1)*sb2
          b(2,7)=b(1,7)*sb1
          b(6,3)=b(5,3)*sb1
          b(3,6)=b(3,5)*sb2
          b(5,4)=b(4,4)*sb1
          b(4,5)=b(4,4)*sb2
          if(nta.gt.8) goto 670
          if(mpe.eq.7.or.(mpe.eq.9.and.nmz.le.7)) goto 700
          if(mx.eq.6.or.mx.eq.7) goto 670
!-----------------------------------------------------------------------
!  REGULAR-16-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
          l2=1
          do 660 l=8,nmz
            l1=l-7
            ab1(8)=ab1(8)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(8)=ab2(8)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  660     l2=l2*l/l1
  670     b(9,1)=b(8,1)*sb1
          b(1,9)=b(1,8)*sb2
          b(8,2)=b(8,1)*sb2
          b(2,8)=b(1,8)*sb1
          b(7,3)=b(7,2)*sb2
          b(3,7)=b(2,7)*sb1
          b(6,4)=b(6,3)*sb2
          b(4,6)=b(3,6)*sb1
          b(5,5)=b(4,5)*sb1
          if(mpe.eq.8.or.(mpe.eq.9.and.nmz.le.8)) goto 700
          if(mx.eq.7) goto 690
!-----------------------------------------------------------------------
!  REGULAR-18-POLE
!-----------------------------------------------------------------------
          l2=1
          do 680 l=9,nmz
            l1=l-8
            ab1(9)=ab1(9)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
            ab2(9)=ab2(9)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  680     l2=l2*l/l1
  690     b(10,1)=b(9,1)*sb1
          b(1,10)=b(1,9)*sb2
          b(9,2)=b(9,1)*sb2
          b(2,9)=b(1,9)*sb1
          b(8,3)=b(8,2)*sb2
          b(3,8)=b(2,8)*sb1
          b(4,7)=b(3,7)*sb1
          b(7,4)=b(7,3)*sb2
          b(5,6)=b(4,6)*sb1
          b(6,5)=b(6,4)*sb2
!-----------------------------------------------------------------------
  700     do 720 np=1,mpe
            n2e=2*np
            do 710 nv=1,n2e
              n2=nv-np
              nn2=abs(n2)
              nn1=np-nn2
              re1=nn1*qxt+n2*qzt
              ip(np,nv)=int(re1+half)+ipc
              if(-re1.gt.pieni) ip(np,nv)=-int(abs(re1)+half)-ipc
!--RE=DISTANCE FROM THE RESONANCE
              re(np,nv)=re1-ip(np,nv)
              res=re(np,nv)/radi
              chy(np,nv)=cos(nn1*pie*phi(1)+n2*pie*phi(2)-res*etl)
              shy(np,nv)=sin(nn1*pie*phi(1)+n2*pie*phi(2)-res*etl)
  710       continue
  720     continue
          do 780 np=nta,mpe
            np2=np
            nkk=0
  730       nkk=nkk+1
            n2e=2*np2
            do 770 i=1,nkk
              do 760 nv=1,n2e
                nn2=abs(nv-np2)
                nv1=np2-nn2+(i-1)*2+1
                nv2=np-nv1+2
                rn2=nn2*half
!--EVENESS OF N2
                mm=0
                gerad=rn2-aint(rn2)
                if(abs(gerad).le.pieni) mm=1
!--MM=0 =>N2 UNEVEN, MM=1 => N2 EVEN
                if (mm.eq.0) goto 740
                btc=ab1(np)*b(nv1,nv2)*chy(np2,nv)
                bts=ab1(np)*b(nv1,nv2)*shy(np2,nv)
                goto 750
  740           btc=ab2(np)*b(nv1,nv2)*chy(np2,nv)
                bts=ab2(np)*b(nv1,nv2)*shy(np2,nv)
  750           rtc(np2,nv,np,i)=rtc(np2,nv,np,i)+btc
                rts(np2,nv,np,i)=rts(np2,nv,np,i)+bts
  760         continue
  770       continue
            np2=np2-2
            if(np2.ge.1) goto 730
  780     continue
          nr=nr+1
  790   continue
        nnf(1)=1
        nnf(2)=1
        nnf(3)=2
        nz2(2)=2
        sea=sqrt(ep(1))
        seb=sqrt(ep(2))
        ea=ep(1)
        eb=ep(2)
        e(3,1)=one/eb
        e(1,3)=one/ea
        e(2,2)=one/seb/sea
        nnf(4)=6
        nz2(3)=4
        e(4,1)=sea/eb
        e(1,4)=seb/ea
        e(3,2)=one/seb
        e(2,3)=one/sea
        nnf(5)=24
        nz2(4)=8
        e(5,1)=ea/eb
        e(1,5)=eb/ea
        e(4,2)=sea/seb
        e(2,4)=seb/sea
        e(3,3)=one
        nnf(6)=120
        nz2(5)=16
        e(6,1)=e(5,1)*sea
        e(1,6)=e(1,5)*seb
        e(5,2)=ea/seb
        e(2,5)=eb/sea
        e(4,3)=sea
        e(3,4)=seb
        nnf(7)=720
        nz2(6)=32
        e(7,1)=e(6,1)*sea
        e(1,7)=e(1,6)*seb
        e(6,2)=e(5,2)*sea
        e(2,6)=e(2,5)*seb
        e(5,3)=ea
        e(3,5)=eb
        e(4,4)=sea*seb
        nnf(8)=5040
        nz2(7)=64
        e(8,1)=e(7,1)*sea
        e(1,8)=e(1,7)*seb
        e(7,2)=e(6,2)*sea
        e(2,7)=e(2,6)*seb
        e(6,3)=ea*sea
        e(3,6)=eb*seb
        e(5,4)=ea*seb
        e(4,5)=sea*eb
        nnf(9)=40320
        nz2(8)=128
        e(9,1)=e(8,1)*sea
        e(1,9)=e(1,8)*seb
        e(8,2)=e(7,2)*sea
        e(2,8)=e(2,7)*seb
        e(7,3)=ea*ea
        e(3,7)=eb*eb
        e(6,4)=e(5,4)*sea
        e(4,6)=e(4,5)*seb
        e(5,5)=ea*eb
        nnf(10)=362880
        nz2(9)=256
        e(10,1)=e(9,1)*sea
        e(1,10)=e(1,9)*seb
        e(9,2)=e(8,2)*sea
        e(2,9)=e(2,8)*seb
        e(8,3)=e(7,3)*sea
        e(3,8)=e(3,7)*seb
        e(7,4)=e(6,4)*sea
        e(4,7)=e(4,6)*seb
        e(6,5)=e(5,5)*sea
        e(5,6)=e(5,5)*seb
        write(*,10000)
        write(*,10030)
        write(*,10010) nr,'END     ',etl,zero,(beta(l),alfa(l),phi(l),  &
     &di0(l),dip0(l),clo0(l),clop0(l),l=1,2)
        write(*,10030)
        write(*,10110) etl,qwc(1),qwc(2)
        write(*,10030)
        do 800 iv=2,5
          gtu1=gtu1+dtu(1,iv)
          gtu2=gtu2+dtu(2,iv)
  800   continue
        write(*,10150) dtu(1,2),dtu(1,3),dtu(1,4),dtu(1,5),gtu1, dtu    &
     &(2,2),dtu(2,3),dtu(2,4),dtu(2,5),gtu2
        do 810 i=1,2
          do 810 j=1,5
            do 810 l=0,4
              do 810 k=0,4
                if(i.eq.2.and.j.eq.1.and.k.eq.1.and.l.eq.1) write       &
     &(6,10160)
                if(abs(dtup(i,j,k,l)).gt.pieni) write(6,                &
     &'(10X,G16.10,3X,I2,2X,I2)') dtup(i,j,k,l),k,l
  810   continue
        write(*,10060)
        write(*,10030)
        do 880 np=nta,nte
          write(*,10080) np
          write(*,10030)
          vdt1=nnf(np)/(nz2(np)*pi)
          np2=np
          nkk=0
          write(*,10090) np
          goto 830
  820     write(*,10100) np,np2
  830     nkk=nkk+1
          n2e=2*np2
          do 850 i=1,nkk
            do 840 nv=1,n2e
              n2=nv-np2
              nn2=abs(n2)
              nn1=np2-nn2
              nv1=nn1+(i-1)*2+1
              nv2=np-nv1+2
              nv11=nv1-1
              nv21=nv2-1
              nf1=nn1+i
              nf3=nkk-i+1
              nf4=nf3+nn2
              vdt2=vdt1*e(nv1,nv2)/(nnf(nf1)*nnf(i)*nnf(nf3)*nnf(nf4))
              vdt3=nn2*ea+nn1*eb
              vdt4=vdt3
              if(n2.ge.0) vdt3=n2*nv21*ea+nn1*nv11*eb
              rtc(np2,nv,np,i)=rtc(np2,nv,np,i)*vdt2*vdt3
              rts(np2,nv,np,i)=rts(np2,nv,np,i)*vdt2*vdt3
  840       continue
  850     continue
          do 870 nv=1,n2e
            mis=1
            rc=zero
            rs=zero
            do 860 i=1,nkk
              rc=rc+mis*rtc(np2,nv,np,i)
              rs=rs+mis*rts(np2,nv,np,i)
              mis=-mis
  860       continue
            sdel2=sqrt(rc*rc+rs*rs)
            n22=nv-np2
            write(*,10140) n22,ip(np2,nv),ipc,rc,rs,re(np2,nv),sdel2
  870     continue
          np2=np2-2
          if(np2.ge.1) goto 820
  880   continue
        ntx=nte-2
        write(*,10130)
        do 930 np=1,nte
          write(*,10090) np
          n2e=2*np
          do 920 nv=1,n2e
            n2=nv-np
            nkk=2
            nph=np+2
            min1=-1
  890       min2=min1
            do 900 i=1,nkk
              rtc(np,nv,np,1)=rtc(np,nv,np,1)+min2*rtc(np,nv,nph,i)
              rts(np,nv,np,1)=rts(np,nv,np,1)+min2*rts(np,nv,nph,i)
              min2=-min2
  900       continue
            nph=nph+2
            if(nph.gt.nte) goto 910
            nkk=nkk+1
            min1=-min1
            goto 890
  910       cc=rtc(np,nv,np,1)
            ss=rts(np,nv,np,1)
            sdel=sqrt(cc*cc+ss*ss)
            write(*,10140) n2,ip(np,nv),ipc,cc,ss,re(np,nv),sdel
  920     continue
  930   continue
  940 continue
      call clorb(ded)
      do 950 l=1,2
        clo0(l)=clo(l)
        clop0(l)=clop(l)
  950 continue
      call clorb(zero)
      do 960 l=1,2
        ll=2*l
        di0(l)=(clo0(l)-clo(l))/ded
        dip0(l)=(clop0(l)-clop(l))/ded
  960 continue
!-----------------------------------------------------------------------
      return
10000 format(1x,i4,27x,f7.2,1x,f6.2,1x,f6.2,1x,f6.2,1x,f6.3,1x,f6.2,1x, &
     &f6.3,1x,f7.2,1x,f6.2,1x,f6.2,1x,f6.2,1x,f6.3,1x,f6.2,1x,f6.3)
10020 format(t5,'---- ENTRY SUBRES ----')
10030 format(131('-'))
10040 format('  NR  TYP      L-TOTAL  LENGTH   BETAH  ALFAH  ',         &
     &' PHIH   DISH  DISPH   CLOH  CLOPH',                              &
     &'   BETAV  ALFAV   PHIV   DISV  DISPV   CLOV  CLOPV'/ 1x,         &
     &'                 (M)      (M)     (M)           ',               &
     &'(QE)   (M)   (RAD)   (MM) (MRAD)',                               &
     &'    (M)           (QE)   (M)   (RAD)   (MM) (MRAD)')
10050 format(//7x,'INIT. X-AMPLITUDE=',g15.8,'X-EMITTANCE=',g15.8,/40x, &
     &/7x,'INIT. Y-AMPLITUDE=',g15.8,'Y-EMITTANCE=',g15.8,              &
     &'UNITS ARE (PI X MM X MRAD)'//)
10060 format(//10x,'E=NX*QX+NY*QY-P',//10x,'CLOSESET P-VALUE CHANGED ', &
     &'BY D-P',//10x,'DELTA-E STANDS FOR THE RESONANCE-WIDTH' //10x)
10070 format(/10x,'RELATIVE ENERGY DEVIATION  ',t40,f10.7/ 10x,         &
     &'TUNES -HORIZONTAL',t40,f10.7/ 10x,'      -VERTICAL  ',t40,f10.7)
10080 format(/10x,'RESONANCE EXCITING MULTIPOLE-ORDER = ',i2)
10090 format(//20x,'RESONANCE-ORDER =',i2/20x,100('-')/ 20x,'| NY |',   &
     &'   P  | D-P |',2x,'DRIVING-COS ',3x,'|', 2x,'DRIVING-SIN ',3x,'|'&
     &, 8x,'E',8x,'|',5x,'DELTA-E',5x,'|')
10100 format(//20x,'RESONANCE-ORDER =',i2,5x,'SUBRESONANCE-ORDER = ',i2,&
     &/20x,100('-')/ 20x,'| NY |','   P  | D-P |',2x,'DRIVING-COS ',3x, &
     &'|', 2x,'DRIVING-SIN ',3x,'|', 8x,'E',8x,'|',5x,'DELTA-E',5x,'|')
10110 format(/10x,'PRECISE LENGTH OF THE MACHINE : ',f43.33/ /10x,      &
     &'   PRECISE HORIZONTAL Q-VALUE : ',f43.33/ /10x,                  &
     &'     PRECISE VERTICAL Q-VALUE : ',f43.33/)
10120 format(t8,'  PLANE     DISP(MM)     DISP(MRAD)   '/ t6,'      X  '&
     &,2(f12.3,3x)/t10,'  Y  ',2(f12.3,3x)/)
10130 format(//10x,'E=NX*QX+NY*QY-P',//10x,'CLOSESET P-VALUE CHANGED ', &
     &'BY D-P',//10x,'DELTA-E STANDS FOR THE RESONANCE-WIDTH' //10x,    &
     &'!!!! ALL SUBRESONANCES ARE INCLUDED !!!! ')
10140 format(20x,'| ',i2,' | ',i4,' | ',i3,' |', g16.8,' |',g16.8,' |', &
     &g16.8,' |',g16.8,' |')
10150 format(/10x,'NONLINEAR DETUNING  '// 10x,'CHANGE IN QX'/ 10x,     &
     &' 4. ORDER ',f15.12/ 10x,' 6. ORDER ',f15.12/ 10x,' 8. ORDER ',f15&
     &.12/ 10x,'10. ORDER ',f15.12/ 10x,'   TOTAL  ',f15.12/ 10x,       &
     &'CHANGE IN QY'/ 10x,' 4. ORDER ',f15.12/ 10x,' 6. ORDER ',f15.12/ &
     &10x,' 8. ORDER ',f15.12/ 10x,'10. ORDER ',f15.12/ 10x,'   TOTAL  '&
     &,f15.12// 10x,'DETUNING ORDER BY ORDER'// 10x,                    &
     &'Qx - COEFFICIENT   Ex  EY'/ 10x,'-------------------------')
10160 format(/ 10x,'Qy - COEFFICIENT   Ex  Ey'/ 10x,                    &
     &'-------------------------')
10010 format(1x,i4,1x,a8,1x,f8.2,1x,f7.3,1x, f7.2,1x,f6.2,1x,f6.2,1x,f6.&
     &2,1x,f6.3,1x,f6.2,1x,f6.3,1x, f7.2,1x,f6.2,1x,f6.2,1x,f6.2,1x,f6. &
     &3,1x,f6.2,1x,f6.3)
      end
      subroutine detune(iv,ekk,ep,beta,dtu,dtup,dfac)
!-----------------------------------------------------------------------
!  USED FOR SUBRE - CALCULATES DETUNING
!-----------------------------------------------------------------------
      implicit none
      integer iv,iv2,iv3,iv4,iv5,iv6
      double precision beta,dfac,dtu,dtu1,dtu2,dtup,ekk,ep,pi,vor,vtu1, &
     &vtu2
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
      dimension dfac(10),dtu(2,5),ep(2),beta(2),dtup(2,5,0:4,0:4)
      save
!-----------------------------------------------------------------------
      if(iv.lt.2) then
        write(*,*)
        write(*,*) '       ***** ERROR IN DETUNE *****'
        write(*,*)
        write(*,*) '       IV LESS THAN 2, NO DETUNING POSSIBLE'
        write(*,*)
        return
      endif
      pi=four*atan(one)
      iv2=2*iv
      iv3=iv+1
      vtu1=-ekk*(half**iv2)*dfac(iv2)/pi
      dtu1=zero
      dtu2=zero
      do 10 iv4=1,iv3
        iv5=iv4-1
        iv6=iv-iv5
        vor=one
        if(mod(iv6,2).ne.0) vor=-one
        vtu2=vor/(dfac(iv5+1)**2)/(dfac(iv6+1)**2)*(beta(1)**iv5)* (beta&
     &(2)**iv6)
        if(iv5.ne.0) then
          dtu1=dtu1+vtu2*iv5*(ep(1)**(iv5-1))*(ep(2)**iv6)
          dtup(1,iv,iv5-1,iv6)=dtup(1,iv,iv5-1,iv6)+vtu2*iv5*vtu1
        endif
        if(iv6.ne.0) then
          dtu2=dtu2+vtu2*iv6*(ep(1)**iv5)*(ep(2)**(iv6-1))
          dtup(2,iv,iv5,iv6-1)=dtup(2,iv,iv5,iv6-1)+vtu2*iv6*vtu1
        endif
   10 continue
      dtu(1,iv)=dtu(1,iv)+vtu1*dtu1
      dtu(2,iv)=dtu(2,iv)+vtu1*dtu2
      return
      end
      subroutine subsea(dpp)
!-----------------------------------------------------------------------
!  CALCULATION OF DRIVINGTERMS OF RESONANCES INCLUDING SUBRESONANCE
!  USED FOR SEARCH
!-----------------------------------------------------------------------
      implicit none
      integer i,ii,ik,im,ip,ium,ix,izu,j,jj,jk,jm,k,k1,kpz,kzz,l,l1,    &
     &l2,ll,lmin,mm,mpe,mx,n2,n2e,nf1,nf3,nf4,nkk,nmz,nn1,nn2,nnf,np,   &
     &np2,ns,nv,nv1,nv11,nv2,nv21,nz2,dj
      double precision aa,ab1,ab2,alfa,b,b1,b2,bb,benkr,beta,btc,bts,   &
     &chy,ci,cikve,cr,crkve,cxzi,cxzr,cxzyi,cxzyr,cxzyrr,del,dphi,dpp,  &
     &dppi,dpr,dt,dyy1,dyy2,e,ea,eb,ekk,ep,etl,gerad,phi,phibf,phy,pie, &
     &puf,qu,qv,qw,r0,r0a,radi,re,re1,res,rn2,sb1,sb2,sea,seb,shy,t,    &
     &vdt1,vdt2,vdt3,xl,xs,zl,zs
      double precision dyy11,qu1,tiltck,tiltck1,tiltck2,tiltck3,tiltck4,&
     &tiltck5,tiltckuk,tiltsk,tiltsk1,tiltsk2,tiltsk3,tiltsk4,tiltsk5
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
      dimension t(5,4)
      dimension beta(2),alfa(2),phi(2),phibf(2)
      dimension aa(mmul),bb(mmul)
      dimension qw(2),dpr(5)
      dimension nnf(10),ep(2)
      dimension ab1(10),ab2(10),re(10,18),ip(10,18)
      dimension b(10,10),nz2(9),e(10,10)
      dimension chy(9,18),shy(9,18)
      dimension cr(mmul),ci(mmul)
      save
!-----------------------------------------------------------------------
      ium=5
      do 10 i=1,ium
        dpr(i)=zero
   10 continue
      do 20 i=1,ium
        do 20 j=1,4
          t(i,j)=zero
   20 continue
      do 30 i=1,2
        beta(i)=zero
        alfa(i)=zero
        phi(i)=zero
        phibf(i)=zero
        qw(i)=zero
        ep(i)=zero
   30 continue
      do 40 i=1,10
        nnf(i)=0
        do 40 j=1,18
          ip(i,j)=0
          re(i,j)=zero
   40 continue
      do 50 i=1,mmul
        aa(i)=zero
        bb(i)=zero
        cr(i)=zero
        ci(i)=zero
   50 continue
      do 100 i=1,9
        nz2(i)=0
        do 90 j=1,18
          chy(i,j)=zero
          shy(i,j)=zero
          do 80 k=1,10
            do 60 ii=1,10
              e(k,ii)=zero
              b(k,ii)=zero
   60       continue
            do 70 l=1,5
              rtc(i,j,k,l)=zero
              rts(i,j,k,l)=zero
   70       continue
   80     continue
   90   continue
  100 continue
      btc=zero
      bts=zero
      phy=zero
      dt=zero
      del=zero
      ns=0
      ik=0
      pie=two*pi
      etl=zero
      radi=totl/pie
      dpr(1)=dpp*c1e3
      call clorb2(dpp)
      call betalf(dpp,qw)
      if(ierro.ne.0) call prror(22+ierro)
      call envar(dpp)
!--STARTVALUES OF THE TRAJECTORIES
      do 110 l=1,2
        ll=2*l
        alfa(l)=alf0(l)
        beta(l)=bet0(l)
        t(1,ll-1)=clo(l)
  110 t(1,ll)=clop(l)
      do 120 i=1,4
        do 120 j=1,4
          t(i+1,j)=ta(j,i)
  120 t(i+1,j)=ta(j,i)
!--EP=EMITTANCE IN PI*MM*MRAD
      ep(1)=tam1*tam1/beta(1)
      ep(2)=tam2*tam2/beta(2)
!--SINGLE TURN BLOCKLOOP
      izu=0
      do 740 k=1,iu
        do 130 k1=1,10
          ab1(k1)=zero
  130   ab2(k1)=zero
        ix=ic(k)
        if(ix.gt.nblo) goto 210
        jj=0
        dj=1
        if(ix.gt.0) goto 140
        ix=-ix
        jj=mel(ix)+1
        dj=-1
  140   jm=mel(ix)
!--BLOCKELEMENTLOOP
        do 200 j=1,jm
          jj=jj+dj
          jk=mtyp(ix,jj)
          if(ithick.eq.1.and.kz(jk).ne.0) goto 170
          if(ithick.eq.0.and.kz(jk).ne.0) goto 740
!--PURE DRIFTLENGTH
          etl=etl+el(jk)
          do 150 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=pi2
            endif
            do 150 i=1,ium
  150     t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
          do 160 l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=pi2-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
  160     phi(l)=phi(l)+dphi
          goto 200
!--MAGNETELEMENT
  170     continue
          if(kz(jk).ne.8) etl=etl+el(jk)
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a(jk,l,5)
              t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
            enddo
          enddo
          do l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(kz(jk).ne.8.and.-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi
          enddo
  200   continue
        goto 740
!--NL-INSERTION
  210   ix=ix-nblo
        qu=zero
        qv=zero
        kpz=kp(ix)
        if(kpz.eq.6) goto 740
        kzz=kz(ix)
        if(kzz.eq.22) then
          do l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*rrtr(imtr(ix),ll-1,ll-1)+                   &
     &t(i,ll)*rrtr(imtr(ix),ll-1,ll)+                                   &
     &dpr(i)*rrtr(imtr(ix),ll-1,6)
              t(i,ll)=puf*rrtr(imtr(ix),ll,ll-1)+                       &
     &t(i,ll)*rrtr(imtr(ix),ll,ll)+                                     &
     &dpr(i)*rrtr(imtr(ix),ll,6)
            enddo
            t(1,ll-1)=t(1,ll-1)+cotr(imtr(ix),ll-1)
            t(1,ll)=t(1,ll)+cotr(imtr(ix),ll)
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
            phi(l)=phi(l)+dphi
          enddo
        endif
        if(kzz.eq.0.or.kzz.eq.20.or.kzz.eq.22) goto 740
        dyy1=zero
        dyy2=zero
        if(iorg.lt.0) mzu(k)=izu
        izu=mzu(k)+1
        ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
        izu=izu+1
        xs=xpl(ix)+zfz(izu)*xrms(ix)
        izu=izu+1
        zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(t(1,1)-xs)*tiltc(k)+(t(1,3)-zs)*tilts(k)
        zl=-(t(1,1)-xs)*tilts(k)+(t(1,3)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
        if(kzz.lt.0) goto 350
        goto(220,230,240,250,260,270,280,290,300,310,320),kzz
        goto 740
!--HORIZONTAL DIPOLE
  220   ekk=ekk*c1e3
        mpe=20
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
        goto 460
!--NORMAL QUADRUPOLE
  230   continue
        dyy1=ekk*(tiltc(k)*xl+tilts(k)*zl)
        dyy2=ekk*(-tiltc(k)*zl+tilts(k)*xl)
        mpe=20
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        ab1(2)=qu
        ab2(2)=-qv
        goto 460
!--NORMAL SEXTUPOLE
  240   ekk=ekk*c1m3
        mpe=3
        mx=1
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*xl+tiltsk*zl)
        qv=ekk*two*(tiltck*zl-tiltsk*xl)
        ab1(2)=qu
        ab2(2)=-qv
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=ekk*tiltck
        ab2(3)=ekk*tiltsk
        goto 460
!--NORMAL OCTUPOLE
  250   ekk=ekk*c1m6
        mpe=4
        mx=2
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=three*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=three*ekk*(tiltck*xl+tiltsk*zl)
        ab2(3)=three*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=ekk*tiltck
        ab2(4)=ekk*tiltsk
        goto 460
!--NORMAL DECAPOLE
  260   ekk=ekk*c1m9
        mpe=5
        mx=3
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        ab1(3)=6*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=6*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=four*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=four*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=four*ekk*(tiltck*xl+tiltsk*zl)
        ab2(4)=four*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=ekk*tiltck
        ab2(5)=ekk*tiltsk
        goto 460
!--NORMAL DODECAPOLE
  270   ekk=ekk*c1m12
        mpe=6
        mx=4
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        ab1(4)=10*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=10*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=10*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=10*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=5*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=5*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=5*ekk*(tiltck*xl+tiltsk*zl)
        ab2(5)=5*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=ekk*tiltck
        ab2(6)=ekk*tiltsk
        goto 460
!--NORMAL 14-POLE
  280   ekk=ekk*c1m15
        mpe=7
        mx=5
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        ab1(5)=15*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=15*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=20*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=20*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=15*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(5)=15*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=6*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=6*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=6*ekk*(tiltck*xl+tiltsk*zl)
        ab2(6)=6*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=ekk*tiltck
        ab2(7)=ekk*tiltsk
        goto 460
!--NORMAL 16-POLE
  290   ekk=ekk*c1m18
        mpe=8
        mx=6
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        ab1(6)=21*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        ab2(6)=21*ekk*(-tiltck4*cxzyi+tiltsk4*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=35*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=35*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=35*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=35*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=21*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=21*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=7*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=7*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=7*ekk*(tiltck*xl+tiltsk*zl)
        ab2(7)=7*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=ekk*tiltck
        ab2(8)=ekk*tiltsk
        goto 460
!--NORMAL 18-POLE
  300   ekk=ekk*c1m21
        mpe=9
        mx=7
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk5=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck5=tiltckuk
        ab1(7)=28*ekk*(tiltck5*cxzyr+tiltsk5*cxzyi)
        ab2(7)=28*ekk*(-tiltck5*cxzyi+tiltsk5*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(6)=56*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        ab2(6)=56*ekk*(-tiltck4*cxzyi+tiltsk4*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=70*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        ab2(5)=70*ekk*(-tiltck3*cxzyi+tiltsk3*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=56*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        ab2(4)=56*ekk*(-tiltck2*cxzyi+tiltsk2*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=28*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        ab2(3)=28*ekk*(-tiltck1*cxzyi+tiltsk1*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=8*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=8*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        tiltckuk=tiltck5*tiltc(k)-tiltsk5*tilts(k)
        tiltsk=tiltck5*tilts(k)+tiltsk5*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=8*ekk*(tiltck*xl+tiltsk*zl)
        ab2(8)=8*ekk*(-tiltck*zl+tiltsk*xl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(9)=ekk*tiltck
        ab2(9)=ekk*tiltsk
        goto 460
!--NORMAL 20-POLE
  310   ekk=ekk*c1m24
        mpe=20
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        qv=9*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        dyy2=ekk*(-tiltc(k)*cxzyi+tilts(k)*cxzyr)
        goto 460
  320   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)                     &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            do 323 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  323       continue
          else
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tiltc(k)                             &
     &+dppi*(one-tiltc(k))
            t(1,4)=t(1,4)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
          endif
        endif
        if(abs(dki(ix,2)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)+(qu*zl-dppi*dpp)*tilts(k)                     &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+(-qu*zl+dppi*dpp)*tiltc(k)                    &
     &-dppi*(one-tiltc(k))
            do 326 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  326       continue
          else
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tilts(k)                             &
     &+dppi*tilts(k)
            t(1,4)=t(1,4)+dppi*dpp*tiltc(k)                             &
     &-dppi*(one-tiltc(k))
          endif
        endif
        mpe=9
        mx=0
        if(abs(r0).le.pieni) goto 740
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
          goto 740
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        cr(1)=one
        cr(2)=xl
        ci(2)=zl
        cxzyr=xl
        cxzyi=zl
        cxzr=cxzyr
        cxzi=cxzyi
        dyy1=zero
        dyy2=zero
        qu=zero
        qv=zero
        lmin=3
        if(nmz.eq.1) lmin=2
        do 330 l=lmin,mmul
          cr(l)=zero
  330   ci(l)=zero
        do 340 l=1,nmz
          l1=l-1
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
          if(l.gt.2) then
            cxzyrr=cxzyr*cxzr-cxzyi*cxzi
            cxzyi=cxzyr*cxzi+cxzyi*cxzr
            cxzyr=cxzyrr
            cr(l)=cxzyr
            ci(l)=cxzyi
          endif
          dyy1=dyy1+bb(l)*cr(l)+aa(l)*ci(l)
          dyy2=dyy2-bb(l)*ci(l)+aa(l)*cr(l)
          if(l.gt.1.and.ium.ne.1) then
            qu=qu+l1*(bb(l)*cr(l1)+aa(l)*ci(l1))
            qv=qv+l1*(bb(l)*ci(l1)-aa(l)*cr(l1))
          endif
  340   continue
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
        izu=izu+2*mmul-2*nmz
        goto 460
!--SKEW ELEMENTS
  350   kzz=-kzz
        goto(360,370,380,390,400,410,420,430,440,450),kzz
        goto 740
!--VERTICAL DIPOLE
  360   ekk=ekk*c1e3
        mpe=20
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
        goto 460
!--SKEW QUADRUPOLE
  370   continue
        dyy1=ekk*(tiltc(k)*zl-tilts(k)*xl)
        dyy2=ekk*(tiltc(k)*xl+tilts(k)*zl)
        mpe=2
        mx=-1
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        ab1(2)=qu
        ab2(2)=-qv
        goto 460
!--SKEW SEXTUPOLE
  380   ekk=ekk*c1m3
        mpe=3
        mx=1
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*zl-tiltsk*xl)
        qv=-ekk*two*(tiltck*xl+tiltsk*zl)
        ab1(2)=qu
        ab2(2)=-qv
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=ekk*tiltsk
        ab2(3)=ekk*tiltck
        goto 460
!--SKEW OCTUPOLE
  390   ekk=ekk*c1m6
        mpe=4
        mx=2
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-three*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(3)=three*ekk*(tiltck*zl-tiltsk*xl)
        ab2(3)=three*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=ekk*tiltsk
        ab2(4)=ekk*tiltck
        goto 460
!--SKEW DECAPOLE
  400   ekk=ekk*c1m9
        mpe=5
        mx=3
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        ab1(3)=6*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=6*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=four*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-four*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck=tiltckuk
        ab1(4)=four*ekk*(tiltck*zl-tiltsk*xl)
        ab2(4)=four*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=ekk*tiltsk
        ab2(5)=ekk*tiltck
        goto 460
!--SKEW DODECAPOLE
  410   ekk=ekk*c1m12
        mpe=6
        mx=4
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        ab1(4)=10*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=10*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=10*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=10*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=5*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-5*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck=tiltckuk
        ab1(5)=5*ekk*(tiltck*zl-tiltsk*xl)
        ab2(5)=5*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=ekk*tiltsk
        ab2(6)=ekk*tiltck
        goto 460
!--SKEW 14-POLE
  420   ekk=ekk*c1m15
        mpe=7
        mx=5
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        ab1(5)=15*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=15*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=20*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=20*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=15*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=15*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=6*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-6*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck=tiltckuk
        ab1(6)=6*ekk*(tiltck*zl-tiltsk*xl)
        ab2(6)=6*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=ekk*tiltsk
        ab2(7)=ekk*tiltck
        goto 460
!--SKEW 16-POLE
  430   ekk=ekk*c1m18
        mpe=8
        mx=6
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        ab1(6)=21*ekk*(tiltck4*cxzyi-tiltsk4*cxzyr)
        ab2(6)=21*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=35*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=35*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=35*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=35*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=21*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=21*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=7*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-7*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck=tiltckuk
        ab1(7)=7*ekk*(tiltck*zl-tiltsk*xl)
        ab2(7)=7*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=ekk*tiltsk
        ab2(8)=ekk*tiltck
        goto 460
!--SKEW 18-POLE
  440   ekk=ekk*c1m21
        mpe=9
        mx=7
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk1=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck1=tiltckuk
        tiltckuk=tiltck1*tiltc(k)-tiltsk1*tilts(k)
        tiltsk2=tiltck1*tilts(k)+tiltsk1*tiltc(k)
        tiltck2=tiltckuk
        tiltckuk=tiltck2*tiltc(k)-tiltsk2*tilts(k)
        tiltsk3=tiltck2*tilts(k)+tiltsk2*tiltc(k)
        tiltck3=tiltckuk
        tiltckuk=tiltck3*tiltc(k)-tiltsk3*tilts(k)
        tiltsk4=tiltck3*tilts(k)+tiltsk3*tiltc(k)
        tiltck4=tiltckuk
        tiltckuk=tiltck4*tiltc(k)-tiltsk4*tilts(k)
        tiltsk5=tiltck4*tilts(k)+tiltsk4*tiltc(k)
        tiltck5=tiltckuk
        ab1(7)=28*ekk*(tiltck5*cxzyi-tiltsk5*cxzyr)
        ab2(7)=28*ekk*(tiltck5*cxzyr+tiltsk5*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(6)=56*ekk*(tiltck4*cxzyi-tiltsk4*cxzyr)
        ab2(6)=56*ekk*(tiltck4*cxzyr+tiltsk4*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(5)=70*ekk*(tiltck3*cxzyi-tiltsk3*cxzyr)
        ab2(5)=70*ekk*(tiltck3*cxzyr+tiltsk3*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(4)=56*ekk*(tiltck2*cxzyi-tiltsk2*cxzyr)
        ab2(4)=56*ekk*(tiltck2*cxzyr+tiltsk2*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        ab1(3)=28*ekk*(tiltck1*cxzyi-tiltsk1*cxzyr)
        ab2(3)=28*ekk*(tiltck1*cxzyr+tiltsk1*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        qu=8*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-8*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        ab1(2)=qu
        ab2(2)=-qv
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
        tiltckuk=tiltck5*tiltc(k)-tiltsk5*tilts(k)
        tiltsk=tiltck5*tilts(k)+tiltsk5*tiltc(k)
        tiltck=tiltckuk
        ab1(8)=8*ekk*(tiltck*zl-tiltsk*xl)
        ab2(8)=8*ekk*(tiltck*xl+tiltsk*zl)
        tiltckuk=tiltck*tiltc(k)-tiltsk*tilts(k)
        tiltsk=tiltck*tilts(k)+tiltsk*tiltc(k)
        tiltck=tiltckuk
        ab1(9)=ekk*tiltsk
        ab2(9)=ekk*tiltck
        goto 460
!--SKEW 20-POLE
  450   ekk=ekk*c1m24
        mpe=20
        cxzr=xl
        cxzi=zl
        cxzyr=cxzr*cxzr-cxzi*cxzi
        cxzyi=cxzr*cxzi+cxzi*cxzr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cxzyi-tiltsk*cxzyr)
        qv=-9*ekk*(tiltck*cxzyr+tiltsk*cxzyi)
        cxzyrr=cxzyr*cxzr-cxzyi*cxzi
        cxzyi=cxzyr*cxzi+cxzyi*cxzr
        cxzyr=cxzyrr
        dyy1=ekk*(tiltc(k)*cxzyi-tilts(k)*cxzyr)
        dyy2=ekk*(tiltc(k)*cxzyr+tilts(k)*cxzyi)
  460   t(1,2)=t(1,2)+dyy1
        t(1,4)=t(1,4)+dyy2
        do 470 i=2,ium
          t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
          t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
  470   continue
        do 480 l=1,2
          ll=2*l
          alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
  480   continue
        if(mpe.gt.9.or.(mpe.eq.9.and.nmz.le.1)) goto 740
        if(mpe.lt.nta) goto 740
        if(mpe.gt.nte) mpe=nte
        if(nta.gt.2) goto 500
        if(mx.eq.-1.or.mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx  &
     &.eq.5.or.mx.eq.6.or.mx.eq.7) goto 500
!-----------------------------------------------------------------------
!  SKEW-QUADRUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        do 490 l=2,nmz
          l1=l-1
  490   ab2(2)=ab2(2)+l1*(aa(l)*cr(l1)-bb(l)*ci(l1))
  500   b1=beta(1)
        b2=beta(2)
        sb1=sqrt(b1)
        sb2=sqrt(b2)
        b(3,1)=b1
        b(1,3)=b2
        b(2,2)=sb1*sb2
        if(nta.gt.3) goto 520
        if(mpe.eq.2.or.(mpe.eq.9.and.nmz.le.2)) goto 650
        if(mx.eq.1.or.mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq&
     &.6.or.mx.eq.7) goto 520
!-----------------------------------------------------------------------
!  REGULAR-SEXTUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 510 l=3,nmz
          l1=l-2
          ab1(3)=ab1(3)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(3)=ab2(3)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  510   l2=l2*l/l1
  520   b(4,1)=b1*sb1
        b(1,4)=b2*sb2
        b(3,2)=b1*sb2
        b(2,3)=b2*sb1
        if(nta.gt.4) goto 540
        if(mpe.eq.3.or.(mpe.eq.9.and.nmz.le.3)) goto 650
        if(mx.eq.2.or.mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq&
     &.7) goto 540
!-----------------------------------------------------------------------
!  REGULAR-OCTUPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 530 l=4,nmz
          l1=l-3
          ab1(4)=ab1(4)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(4)=ab2(4)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  530   l2=l2*l/l1
  540   b(5,1)=b1*b1
        b(1,5)=b2*b2
        b(4,2)=b(3,2)*sb1
        b(2,4)=b(2,3)*sb2
        b(3,3)=b1*b2
        if(nta.gt.5) goto 560
        if(mpe.eq.4.or.(mpe.eq.9.and.nmz.le.4)) goto 650
        if(mx.eq.3.or.mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7)        &
     &goto 560
!-----------------------------------------------------------------------
!  REGULAR-DEKAPOLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 550 l=5,nmz
          l1=l-4
          ab1(5)=ab1(5)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(5)=ab2(5)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  550   l2=l2*l/l1
  560   b(6,1)=b(5,1)*sb1
        b(1,6)=b(1,5)*sb2
        b(5,2)=b(4,2)*sb1
        b(2,5)=b(2,4)*sb2
        b(4,3)=b(4,2)*sb2
        b(3,4)=b(2,4)*sb1
        if(nta.gt.6) goto 580
        if(mpe.eq.5.or.(mpe.eq.9.and.nmz.le.5)) goto 650
        if(mx.eq.4 .or.mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 580
!-----------------------------------------------------------------------
!  REGULAR-12-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 570 l=6,nmz
          l1=l-5
          ab1(6)=ab1(6)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(6)=ab2(6)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  570   l2=l2*l/l1
  580   b(7,1)=b(6,1)*sb1
        b(1,7)=b(1,6)*sb2
        b(6,2)=b(5,2)*sb1
        b(2,6)=b(2,5)*sb2
        b(5,3)=b(5,2)*sb2
        b(3,5)=b(2,5)*sb1
        b(4,4)=b(3,4)*sb1
        if(nta.gt.7) goto 600
        if(mpe.eq.6.or.(mpe.eq.9.and.nmz.le.6)) goto 650
        if(mx.eq.5.or.mx.eq.6.or.mx.eq.7) goto 600
!-----------------------------------------------------------------------
!  REGULAR-14-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 590 l=7,nmz
          l1=l-6
          ab1(7)=ab1(7)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(7)=ab2(7)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  590   l2=l2*l/l1
  600   b(8,1)=b(7,1)*sb1
        b(1,8)=b(1,7)*sb2
        b(7,2)=b(7,1)*sb2
        b(2,7)=b(1,7)*sb1
        b(6,3)=b(5,3)*sb1
        b(3,6)=b(3,5)*sb2
        b(5,4)=b(4,4)*sb1
        b(4,5)=b(4,4)*sb2
        if(nta.gt.8) goto 620
        if(mpe.eq.7.or.(mpe.eq.9.and.nmz.le.7)) goto 650
        if(mx.eq.6.or.mx.eq.7) goto 620
!-----------------------------------------------------------------------
!  REGULAR-16-POLE;MULTIPOLES UP TO 9-TH ORDER
!-----------------------------------------------------------------------
        l2=1
        do 610 l=8,nmz
          l1=l-7
          ab1(8)=ab1(8)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(8)=ab2(8)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  610   l2=l2*l/l1
  620   b(9,1)=b(8,1)*sb1
        b(1,9)=b(1,8)*sb2
        b(8,2)=b(8,1)*sb2
        b(2,8)=b(1,8)*sb1
        b(7,3)=b(7,2)*sb2
        b(3,7)=b(2,7)*sb1
        b(6,4)=b(6,3)*sb2
        b(4,6)=b(3,6)*sb1
        b(5,5)=b(4,5)*sb1
        if(mpe.eq.8.or.(mpe.eq.9.and.nmz.le.8)) goto 650
        if(mx.eq.7) goto 640
!-----------------------------------------------------------------------
!  REGULAR-18-POLE
!-----------------------------------------------------------------------
        l2=1
        do 630 l=9,nmz
          l1=l-8
          ab1(9)=ab1(9)+l2*(aa(l)*ci(l1)+bb(l)*cr(l1))
          ab2(9)=ab2(9)+l2*(aa(l)*cr(l1)-bb(l)*ci(l1))
  630   l2=l2*l/l1
  640   b(10,1)=b(9,1)*sb1
        b(1,10)=b(1,9)*sb2
        b(9,2)=b(9,1)*sb2
        b(2,9)=b(1,9)*sb1
        b(8,3)=b(8,2)*sb2
        b(3,8)=b(2,8)*sb1
        b(4,7)=b(3,7)*sb1
        b(7,4)=b(7,3)*sb2
        b(5,6)=b(4,6)*sb1
        b(6,5)=b(6,4)*sb2
!-----------------------------------------------------------------------
  650   do 670 np=1,mpe
          n2e=2*np
          do 660 nv=1,n2e
            n2=nv-np
            nn2=abs(n2)
            nn1=np-nn2
            re1=nn1*qxt+n2*qzt
            ip(np,nv)=int(re1+half)+ipt
            if(-re1.gt.pieni) ip(np,nv)=-int(abs(re1)+half)-ipt
!--RE=DISTANCE FROM THE RESONANCE
            re(np,nv)=re1-ip(np,nv)
            res=re(np,nv)/radi
            chy(np,nv)=cos(nn1*phi(1)+n2*phi(2)-res*etl)
            shy(np,nv)=sin(nn1*phi(1)+n2*phi(2)-res*etl)
  660     continue
  670   continue
        do 730 np=nta,mpe
          np2=np
          nkk=0
  680     nkk=nkk+1
          n2e=2*np2
          do 720 i=1,nkk
            do 710 nv=1,n2e
              nn2=abs(nv-np2)
              nv1=np2-nn2+(i-1)*2+1
              nv2=np-nv1+2
              rn2=nn2*half
!--EVENESS OF N2
              mm=0
              gerad=rn2-aint(rn2)
              if(abs(gerad).le.pieni) mm=1
!--MM=0 =>N2 UNEVEN, MM=1 => N2 EVEN
              if (mm.eq.0) goto 690
              btc=ab1(np)*b(nv1,nv2)*chy(np2,nv)
              bts=ab1(np)*b(nv1,nv2)*shy(np2,nv)
              goto 700
  690         btc=ab2(np)*b(nv1,nv2)*chy(np2,nv)
              bts=ab2(np)*b(nv1,nv2)*shy(np2,nv)
  700         rtc(np2,nv,np,i)=rtc(np2,nv,np,i)+btc
              rts(np2,nv,np,i)=rts(np2,nv,np,i)+bts
  710       continue
  720     continue
          np2=np2-2
          if(np2.ge.1) goto 680
  730   continue
  740 continue
      nnf(1)=1
      nnf(2)=1
      nnf(3)=2
      nz2(2)=2
      sea=sqrt(ep(1))
      seb=sqrt(ep(2))
      ea=ep(1)
      eb=ep(2)
      e(3,1)=one/eb
      e(1,3)=one/ea
      e(2,2)=one/seb/sea
      nnf(4)=6
      nz2(3)=4
      e(4,1)=sea/eb
      e(1,4)=seb/ea
      e(3,2)=one/seb
      e(2,3)=one/sea
      nnf(5)=24
      nz2(4)=8
      e(5,1)=ea/eb
      e(1,5)=eb/ea
      e(4,2)=sea/seb
      e(2,4)=seb/sea
      e(3,3)=one
      nnf(6)=120
      nz2(5)=16
      e(6,1)=e(5,1)*sea
      e(1,6)=e(1,5)*seb
      e(5,2)=ea/seb
      e(2,5)=eb/sea
      e(4,3)=sea
      e(3,4)=seb
      nnf(7)=720
      nz2(6)=32
      e(7,1)=e(6,1)*sea
      e(1,7)=e(1,6)*seb
      e(6,2)=e(5,2)*sea
      e(2,6)=e(2,5)*seb
      e(5,3)=ea
      e(3,5)=eb
      e(4,4)=sea*seb
      nnf(8)=5040
      nz2(7)=64
      e(8,1)=e(7,1)*sea
      e(1,8)=e(1,7)*seb
      e(7,2)=e(6,2)*sea
      e(2,7)=e(2,6)*seb
      e(6,3)=ea*sea
      e(3,6)=eb*seb
      e(5,4)=ea*seb
      e(4,5)=sea*eb
      nnf(9)=40320
      nz2(8)=128
      e(9,1)=e(8,1)*sea
      e(1,9)=e(1,8)*seb
      e(8,2)=e(7,2)*sea
      e(2,8)=e(2,7)*seb
      e(7,3)=ea*ea
      e(3,7)=eb*eb
      e(6,4)=e(5,4)*sea
      e(4,6)=e(4,5)*seb
      e(5,5)=ea*eb
      nnf(10)=362880
      nz2(9)=256
      e(10,1)=e(9,1)*sea
      e(1,10)=e(1,9)*seb
      e(9,2)=e(8,2)*sea
      e(2,9)=e(2,8)*seb
      e(8,3)=e(7,3)*sea
      e(3,8)=e(3,7)*seb
      e(7,4)=e(6,4)*sea
      e(4,7)=e(4,6)*seb
      e(6,5)=e(5,5)*sea
      e(5,6)=e(5,5)*seb
      do 780 np=nta,nte
        vdt1=nnf(np)/(nz2(np)*pi)
        np2=np
        nkk=0
  750   nkk=nkk+1
        n2e=2*np2
        do 770 i=1,nkk
          do 760 nv=1,n2e
            n2=nv-np2
            nn2=abs(n2)
            nn1=np2-nn2
            nv1=nn1+(i-1)*2+1
            nv2=np-nv1+2
            nv11=nv1-1
            nv21=nv2-1
            nf1=nn1+i
            nf3=nkk-i+1
            nf4=nf3+nn2
            vdt2=vdt1*e(nv1,nv2)/(nnf(nf1)*nnf(i)*nnf(nf3)*nnf(nf4))
            vdt3=nn2*ea+nn1*eb
            if(n2.ge.0) vdt3=n2*nv21*ea+nn1*nv11*eb
            rtc(np2,nv,np,i)=rtc(np2,nv,np,i)*vdt2*vdt3
            rts(np2,nv,np,i)=rts(np2,nv,np,i)*vdt2*vdt3
  760     continue
  770   continue
        np2=np2-2
        if(np2.ge.1) goto 750
  780 continue
      return
      end
      subroutine decoup
!-----------------------------------------------------------------------
!  DECOUPLING USING MATRIX ELEMENTS
!
!-----------------------------------------------------------------------
      implicit none
      integer i,ierr,j,no
      double precision aa,bb,d1,dpp,dsm,qw,qwc,sen,sn,ss
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
      dimension aa(6,6),bb(6),dsm(6),sn(6),sen(6),ss(6)
      dimension qwc(3),qw(2),d1(6)
      save
!-----------------------------------------------------------------------
      do 10 i=1,6
        bb(i)=zero
        dsm(i)=zero
        sn(i)=zero
        sen(i)=zero
        ss(i)=zero
        d1(i)=zero
        do 10 j=1,6
          aa(i,j)=zero
   10 continue
      do 20 i=1,3
        qwc(i)=zero
   20 continue
      dpp=zero
      write(*,10000)
      call betalf(dpp,qw)
      call phasad(dpp,qwc)
      sen(1)=ta(3,1)
      sen(2)=ta(3,2)
      sen(3)=ta(4,1)
      sen(4)=ta(4,2)
      if(iskew.eq.1) then
        sen(5)=qwc(1)
        sen(6)=qwc(2)
      endif
      do 30 i=1,6
        if(iskew.eq.2.and.i.gt.4) goto 30
        if(i.le.4) then
          sn(i)=ed(nskew(i))
          dsm(i)=dsm0
          bb(i)=sen(i)
        else
          if (abs(el(nskew(i))).le.pieni) then
            sn(i)=ed(nskew(i))
          else
            sn(i)=ek(nskew(i))
          endif
          dsm(i)=dkq
          bb(i)=sen(i)-qwsk(i-4)
        endif
        ss(i)=sen(i)
   30 continue
      do 100 no=1,itcro
        do 40 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 40
          if(i.le.4) then
            ed(nskew(i))=ed(nskew(i))+dsm(i)
          else
            if (abs(el(nskew(i))).le.pieni) then
              ed(nskew(i))=ed(nskew(i))+dsm(i)
            else
              ek(nskew(i))=ek(nskew(i))+dsm(i)
            endif
          endif
          if(kp(nskew(i)).eq.5) call combel(nskew(i))
          call betalf(dpp,qw)
          call phasad(dpp,qwc)
          aa(i,1)=(ta(3,1)-ss(1))/dsm(i)
          aa(i,2)=(ta(3,2)-ss(2))/dsm(i)
          aa(i,3)=(ta(4,1)-ss(3))/dsm(i)
          aa(i,4)=(ta(4,2)-ss(4))/dsm(i)
          if(iskew.eq.1) then
            aa(i,5)=(qwc(1)-ss(5))/dsm(i)
            aa(i,6)=(qwc(2)-ss(6))/dsm(i)
          endif
          if(i.le.4) then
            ed(nskew(i))=ed(nskew(i))-dsm(i)
          else
            if (abs(el(nskew(i))).le.pieni) then
              ed(nskew(i))=ed(nskew(i))-dsm(i)
            else
              ek(nskew(i))=ek(nskew(i))-dsm(i)
            endif
          endif
          if(kp(nskew(i)).eq.5) call combel(nskew(i))
   40   continue
        if(iskew.eq.1) then
          call loesd(aa,bb,6,6,ierr)
        else if(iskew.eq.2) then
          call loesd(aa,bb,4,4,ierr)
        endif
        if(ierr.eq.1) call prror(64)
        do 50 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 50
          if(i.le.4) then
            ed(nskew(i))=ed(nskew(i))-bb(i)
          else
            if (abs(el(nskew(i))).le.pieni) then
              ed(nskew(i))=ed(nskew(i))-bb(i)
            else
              ek(nskew(i))=ek(nskew(i))-bb(i)
            endif
          endif
          if(kp(nskew(i)).eq.5) call combel(nskew(i))
   50   continue
        call betalf(dpp,qw)
        call phasad(dpp,qwc)
        ss(1)=ta(3,1)
        ss(2)=ta(3,2)
        ss(3)=ta(4,1)
        ss(4)=ta(4,2)
        if(iskew.eq.1) then
          ss(5)=qwc(1)
          ss(6)=qwc(2)
        endif
        write(*,10010)
        write(*,10020) no,sen(1),ss(1),sen(2),ss(2),sen(3),ss(3), sen   &
     &(4),ss(4)
        write(*,10030) bez(nskew(1)),sn(1),ed(nskew(1)),bez(nskew(2)),sn&
     &(2),ed(nskew(2)),bez(nskew(3)),sn(3),ed(nskew(3)), bez            &
     &(nskew(4)),sn(4),ed(nskew(4))
        if(iskew.eq.1) then
          write(*,10010)
          write(*,10040) qwsk(1),qwc(1),qwsk(2),qwc(2)
          if (abs(el(nskew(5))).le.pieni) then
            write(*,10060) sn(5),ed(nskew(5)),nskew(5),sn(6),ed         &
     &(nskew(6)), nskew(6)
          else
            write(*,10060) sn(5),ek(nskew(5)),nskew(5),sn(6),ek         &
     &(nskew(6)), nskew(6)
          endif
        else if(iskew.eq.2) then
          write(*,10010)
          write(*,10050) qwc(1),qwc(2)
        endif
        do 60 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 60
          if(i.le.4) then
            d1(i)=abs(ss(i))
          else
            d1(i)=abs(ss(i)-qwsk(i-4))
          endif
   60   continue
        do 70 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 70
          if(d1(i).gt.dsi) goto 80
   70   continue
        return
   80   do 90 i=1,6
          if(iskew.eq.2.and.i.gt.4) goto 90
          if(i.le.4) then
            bb(i)=ss(i)
          else
            bb(i)=ss(i)-qwsk(i-4)
          endif
   90   continue
  100 continue
!-----------------------------------------------------------------------
      return
10000 format(t5,'---- ENTRY DECOUP ----')
10010 format(131('-'))
10020 format(/10x,'DECOUPLING ROUTINE  ITERATION #',i2// 30x,           &
     &'BEFORE         AFTER     DECOUPLING'// 17x,'   M(3,1)      ',2g15&
     &.5/ 17x,'   M(3,2)      ',2g15.5/ 17x,'   M(4,1)      ',2g15.5/ 17&
     &x,'   M(4,2)      ',2g15.5// 5x,'SKEW QUDRUPOLE STRENGTHS')
10040 format(10x,'Q-VARIATION' / 10x,                                   &
     &'Q-VALUE            THEORET.        AFTER     COMPENSATION'/ 10x, &
     &'HORIZONTAL     ',2g15.7/ 10x,'VERTICAL       ',2g15.7/)
10050 format(10x,'CURRENT TUNE' / 10x,'Q-VALUE'/ 10x,'HORIZONTAL     ', &
     &g15.7/ 10x,'VERTICAL       ',g15.7/)
10060 format(10x,'QUADRU.STRENGTH',2g15.8,'   INDEX ',i3/ 10x,          &
     &'               ',2g15.8,'         ',i3)
10030 format(14x,a16,2x,g16.10,1x,g16.10/14x,a16,2x,g16.10,1x,          &
     &g16.10/14x,a16,2x,g16.10,1x,g16.10/14x,a16,2x,g16.10,1x,g16.10)
      end
      subroutine postpr(nfile)
!-----------------------------------------------------------------------
!  POST PROCESSING
!
!  NFILE   :  FILE UNIT
!
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
      integer iav,iconv,icow,icr,idis,iffw,ifh,imad,ipos,ires,iskip,    &
     &istw,itf,ivox,ivoz,iwg,kwtype,ndafi,nprint,nstart,nstop
      double precision cma1,cma2,dfft,dphix,dphiz,dres,qx0,qz0
      real hmal
      character*80 toptit,sixtit,commen
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,             &
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh,toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/pawc/hmal(nplo)
      integer nnumxv
      common/postr2/nnumxv(npart)
      integer icode,idam,its6d
      double precision dpscor,sigcor
      common/corcom/dpscor,sigcor,icode,idam,its6d
      integer ichromc,ilinc,iqmodc
      double precision clon,chromc,corr,wxys
      common/correct/ corr(3,3),chromc(2),wxys(3),clon(6),iqmodc,       &
     &ichromc,ilinc
      common/phasecom/ phase(3,npos)
      common/invari/ dani(ninv+1)
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
!-----                                                                   -----
!-----    NEW BLOCKS PROVIDED FOR THE COLLIMATION STUDIES VIA SIXTRACK   -----
!-----                                                                   -----
!-----        G. ROBERT-DEMOLAIZE, October 27th, 2004                    -----
!-----                                                                   -----
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
      integer i,i1,i11,i2,i3,ia,ia0,iaa,iab,iap6,iapx,iapz,ich,idnt,    &
     &ierro,idummy,if1,if2,ife,ife2,ifipa,ifp,ii,ilapa,ilyap,im1,im1s,  &
     &invx,invz,iq,iskc,itopa,iturn,ivo6,iwar6,iwarx,iwarz,j,jm1,jm1s,  &
     &jq,k,k1,nerror,nfft,nfile,nivh,nlost,ntwin,nuex,nuez,nuix,nuiz,   &
     &numl
      real const,dle,fxs,fzs,slope,tim1,tim2,tle,tlim,varlea,wgh
      double precision alf0,alf04,alf0s2,alf0s3,alf0x2,alf0x3,alf0z2,   &
     &alf0z3,ampx0,ampz0,angi,angii,angiii,ared,ares,armin,armin0,b,b0, &
     &bet0,bet04,bet0s2,bet0s3,bet0x2,bet0x3,bet0z2,bet0z3,biav,bold,c, &
     &c0,c1,c6,clo,cloau,clop,cx,cz,d,d0,d1,dani,dared,dares,di0,di0au, &
     &di11,dife,dip0,dizu0,dle1,dle1c,dmmac,dnms,dnumlr,dp1,dph6,dphx,  &
     &dphz,dpx,dpxp,dpz,dpzp,dummy,e,e0,e1,emag,emat,emax,emaz,emi,emig,&
     &emii,emiii,emit,emix,emiz,emt,emta,emts,emx,emx0,emxa,emxs,emz,   &
     &emz0,emza,emzs,evt,evt1,evtm,evtma,evtmi,evx,evx1,evx2,evxm,evxma,&
     &evxmi,evz,evz1,evz2,evzm,evzma,evzmi,f,f0,f1,ffx,ffz,finv,g,g0,g1,&
     &gam0s1,gam0s2,gam0s3,gam0x1,gam0x2,gam0x3,gam0z1,gam0z2,gam0z3,h, &
     &h0,h1,p,p1,pcha,phase,pieni2,pinx,pinz,pixr,pizr,pmax,pmin,prec,  &
     &qs0,qwc,ratemx,ratemz,rbeta,s6,sdp6,sdpx,sdpz,sevt,sevx,sevz,     &
     &slopem,sumda,sx,sz,t,ta,ta16,ta26,ta36,ta46,ta56,ta61,ta62,ta63,  &
     &ta64,ta65,tasum,tidnt,tle1,tlo,tph6,tphx,tphz,tpi,txyz,txyz2,x,   &
     &xing,xinv,xp,xp0,xxaux,xxmax,xxmin,xxi,xxr,xyzv,xyzv2,zing,zinv,  &
     &zp,zp0,zzaux,zzmax,zzmin,zzi,zzr
      character*80 title(20),chxtit(20),chytit(20)
      character*8 cdate,ctime,progrm
      character*11 hvs
      character*8192 ch
      dimension tle(nlya),dle(nlya)
      dimension wgh(nlya),biav(nlya),slope(nlya),varlea(nlya)
      dimension xinv(ninv),invx(ninv),zinv(ninv),invz(ninv)
      dimension xxr(npos),xxi(npos),zzr(npos),zzi(npos),fxs(npos),      &
     &fzs(npos)
      dimension bet0(3),alf0(3),t(6,6)
      dimension bet04(2),alf04(2)
      dimension pmin(30),pmax(30)
      dimension idummy(6)
      dimension sumda(60)
      dimension x(2,6),cloau(6),di0au(4)
      dimension qwc(3),clo(3),clop(3),di0(2),dip0(2)
      dimension ta(6,6),txyz(6),txyz2(6),xyzv(6),xyzv2(6),rbeta(6)
      save
!----------------------------------------------------------------------
!--TIME START
      pieni2=1d-8
      tlim=1e7
      call timest(tlim)
      tim1=0.
      call timex(tim1)
      do 10 i=1,npos
        do 10 j=1,3
          phase(j,i)=zero
   10 continue
      do 20 i=1,2
        bet04(i)=zero
        alf04(i)=zero
        di0(i)=zero
        dip0(i)=zero
        di0au(i)=zero
        di0au(i+2)=zero
   20 continue
      do 30 i=1,3
        bet0(i)=zero
        alf0(i)=zero
        qwc(i)=zero
        clo(i)=zero
        clop(i)=zero
   30 continue
      do 40 i=1,ninv
        invx(i)=0
        invz(i)=0
        xinv(i)=zero
        zinv(i)=zero
        dani(i)=zero
   40 continue
      dani(ninv+1)=zero
      do 50 i=1,npos
        xxr(i)=zero
        xxi(i)=zero
        zzr(i)=zero
        zzi(i)=zero
        fxs(i)=zero
        fzs(i)=zero
   50 continue
      do 60 i=1,6
        txyz(i)=zero
        txyz2(i)=zero
        xyzv(i)=zero
        xyzv2(i)=zero
        rbeta(i)=zero
        cloau(i)=zero
        x(1,i)=zero
        x(2,i)=zero
   60 continue
      do 70 i=1,6
        do 70 j=1,6
          t(i,j)=zero
          ta(i,j)=zero
   70 continue
      do 80 i=1,30
        pmax(i)=zero
        pmin(i)=zero
   80 continue
      do 90 i=1,20
        title(i)=' '
        chxtit(i)=' '
        chytit(i)=' '
   90 continue
      do 100 i=1,nlya
        tle(i)=zero
        dle(i)=zero
        slope(i)=0.0
        varlea(i)=0.0
        wgh(i)=zero
        biav(i)=zero
  100 continue
      do 110 i=1,60
        sumda(i)=zero
  110 continue
      b0=zero
      nlost=0
      ntwin=1
      nfft=1
      do 120 j=1,npos
        if(nfft.gt.npos/2) goto 130
        nfft=nfft*2
  120 continue
  130 continue
!----------------------------------------------------------------------
!--READING HEADER
!----------------------------------------------------------------------
      rewind nfile
      ia=0
      read(nfile,end=510,iostat=ierro) sixtit,commen,cdate,ctime,       &
     &progrm,ifipa,ilapa,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo(1), &
     &clop(1),clo(2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0(2),dip0 &
     &(2),dummy,dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta(1,5),ta(1,6), &
     &ta(2,1),ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6), ta(3,1),ta(3,2), &
     &ta(3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta(4,2),ta(4,3),ta(4,4), &
     &ta(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),ta(5,4),ta(5,5),ta(5,6), &
     &ta(6,1),ta(6,2),ta(6,3),ta(6,4),ta(6,5),ta(6,6), dmmac,dnms,dizu0,&
     &dnumlr,sigcor,dpscor
      if(ierro.gt.0) then
        write(*,10320) nfile
        goto 550
      endif
      sumda(1)=numl
      idam=1
      if(icode.eq.1.or.icode.eq.2.or.icode.eq.4) idam=1
      if(icode.eq.3.or.icode.eq.5.or.icode.eq.6) idam=2
      if(icode.eq.7) idam=3
      if(ilapa.ne.ifipa) ntwin=2
      if(imad.eq.1.and.progrm.eq.'MAD') then
        imad=0
        rewind nfile
        call join
        read(nfile,end=520,iostat=ierro) sixtit,commen,cdate,ctime,     &
     &progrm,ifipa,ilapa,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo     &
     &(1),clop(1),clo(2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0     &
     &(2),dip0(2),dummy,dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta       &
     &(1,5),ta(1,6), ta(2,1),ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6),   &
     &ta(3,1),ta(3,2),ta(3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta       &
     &(4,2),ta(4,3),ta(4,4),ta(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),   &
     &ta(5,4),ta(5,5),ta(5,6), ta(6,1),ta(6,2),ta(6,3),ta(6,4),ta       &
     &(6,5),ta(6,6), dmmac,dnms,dizu0,dnumlr,sigcor,dpscor
        ta(1,6)=ta(1,6)*c1e3
        ta(2,6)=ta(2,6)*c1e3
        ta(3,6)=ta(3,6)*c1e3
        ta(4,6)=ta(4,6)*c1e3
        ta(5,6)=ta(5,6)*c1e3
        ta(6,1)=ta(6,1)*c1m3
        ta(6,2)=ta(6,2)*c1m3
        ta(6,3)=ta(6,3)*c1m3
        ta(6,4)=ta(6,4)*c1m3
        ta(6,5)=ta(6,5)*c1m3
        if(ierro.gt.0) then
          write(*,10320) nfile
          goto 550
        endif
      endif
!--PREVENT FAULTY POST-PROCESSING
      read(nfile,end=530,iostat=ierro) iaa
      if(ierro.gt.0) then
        write(*,10320) nfile
        goto 550
      endif
      read(nfile,end=535,iostat=ierro) iab
      if(ierro.gt.0) then
        write(*,10320) nfile
        goto 550
      endif
 600  if((numl+1)/iskip/(iab-iaa)/iav.gt.nlya) nstop=iav*nlya
      rewind nfile
      read(nfile)
      sumda(5)=ta(1,1)*ta(1,1)+ta(1,2)*ta(1,2)
      sumda(6)=ta(3,3)*ta(3,3)+ta(3,4)*ta(3,4)
      if(iconv.eq.1) then
        cma1=one
        cma2=one
        clo(1)=zero
        clo(2)=zero
        clo(3)=zero
        clop(1)=zero
        clop(2)=zero
        clop(3)=zero
        di0(1)=zero
        di0(2)=zero
        dip0(1)=zero
        dip0(2)=zero
        do 140 i=1,6
          do 140 j=1,6
            if(i.ne.j) then
              ta(i,j)=zero
            else
              ta(i,j)=one
            endif
  140   continue
      endif
      cloau(1)= clo(1)
      cloau(2)=clop(1)
      cloau(3)= clo(2)
      cloau(4)=clop(2)
      cloau(5)= clo(3)
      cloau(6)=clop(3)
      di0au(1)= di0(1)
      di0au(2)=dip0(1)
      di0au(3)= di0(2)
      di0au(4)=dip0(2)
      sigcor=cma2
      dpscor=cma1
      if(ifipa.eq.ilapa.and.ndafi.gt.itopa) ndafi=itopa
      if(ilapa-ifipa.eq.1.and.ndafi.gt.itopa/2) ndafi=itopa/2
!-----------------------------------------------------------------------
!  OPTICAL PARAMETERS AT THE STARTING POINT
!-----------------------------------------------------------------------
      ta16=ta(1,6)*c1m3
      ta26=ta(2,6)*c1m3
      ta36=ta(3,6)*c1m3
      ta46=ta(4,6)*c1m3
      ta56=ta(5,6)*c1m3
      ta61=ta(6,1)*c1e3
      ta62=ta(6,2)*c1e3
      ta63=ta(6,3)*c1e3
      ta64=ta(6,4)*c1e3
      ta65=ta(6,5)*c1e3
      bet0(1)=ta(1,1)*ta(1,1)+ta(1,2)*ta(1,2)
      bet0x2 =ta(1,3)*ta(1,3)+ta(1,4)*ta(1,4)
      bet0x3 =ta(1,5)*ta(1,5)+ta16*ta16
      gam0x1 =ta(2,1)*ta(2,1)+ta(2,2)*ta(2,2)
      gam0x2 =ta(2,3)*ta(2,3)+ta(2,4)*ta(2,4)
      gam0x3 =ta(2,5)*ta(2,5)+ta26*ta26
      alf0(1)=-(ta(1,1)*ta(2,1)+ta(1,2)*ta(2,2))
      alf0x2 =-(ta(1,3)*ta(2,3)+ta(1,4)*ta(2,4))
      alf0x3 =-(ta(1,5)*ta(2,5)+ta16*ta26)
      bet0(2)=ta(3,3)*ta(3,3)+ta(3,4)*ta(3,4)
      bet0z2 =ta(3,1)*ta(3,1)+ta(3,2)*ta(3,2)
      bet0z3 =ta(3,5)*ta(3,5)+ta36*ta36
      gam0z1 =ta(4,3)*ta(4,3)+ta(4,4)*ta(4,4)
      gam0z2 =ta(4,1)*ta(4,1)+ta(4,2)*ta(4,2)
      gam0z3 =ta(4,5)*ta(4,5)+ta46*ta46
      alf0(2)=-(ta(3,3)*ta(4,3)+ta(3,4)*ta(4,4))
      alf0z2 =-(ta(3,1)*ta(4,1)+ta(3,2)*ta(4,2))
      alf0z3 =-(ta(3,5)*ta(4,5)+ta36*ta46)
      bet0(3)=ta(5,5)*ta(5,5)+ta56*ta56
      bet0s2 =ta(5,1)*ta(5,1)+ta(5,2)*ta(5,2)
      bet0s3 =ta(5,3)*ta(5,3)+ta(5,4)*ta(5,4)
      gam0s1 =ta65*ta65+ta(6,6)*ta(6,6)
      gam0s2 =ta61*ta61+ta62*ta62
      gam0s3 =ta63*ta63+ta64*ta64
      alf0(3)=-(ta(5,5)*ta65+ta56*ta(6,6))
      alf0s2 =-(ta(5,1)*ta61+ta(5,2)*ta62)
      alf0s3 =-(ta(5,3)*ta63+ta(5,4)*ta64)
      bet04(1)=bet0(1)
      bet04(2)=bet0(2)
      alf04(1)=alf0(1)
      alf04(2)=alf0(2)
      if(bet0(1).le.pieni.or.bet0(2).le.pieni) then
        write(*,*) 'WARNING: BETA VALUES ARE ZERO'
        bet0(1)=zero
        bet0(2)=zero
      endif
      do 135 i=1,3
        ii=2*i
        rbeta(ii-1)=sqrt(bet0(i))
        rbeta(ii)=rbeta(ii-1)
        if(abs(rbeta(ii-1)).lt.pieni) rbeta(ii-1)=one
        if(abs(rbeta(ii)).lt.pieni) rbeta(ii)=one
  135 continue
!----------------------------------------------------------------------
!--SETTING UP OF THE PARAMETERS
!----------------------------------------------------------------------
!--HPLOT TITLES
      if(icode.eq.1) hvs(1:11)='Hor        '
      if(icode.eq.2) hvs(1:11)='    Ver    '
      if(icode.eq.3) hvs(1:11)='Hor Ver    '
      if(icode.eq.4) hvs(1:11)='        Syn'
      if(icode.eq.5) hvs(1:11)='Hor     Syn'
      if(icode.eq.6) hvs(1:11)='    Ver Syn'
      if(icode.eq.7) hvs(1:11)='Hor Ver Syn'
      toptit(2)(1:13)='Particle no. '
      write(toptit(2)(14:16),'(I3)') ifipa
      toptit(2)(17:30)=', Phase Space '
      write(toptit(2)(31:41),'(A11)') hvs
      toptit(2)(42:50)=', Dp/p = '
      toptit(2)(61:80)=' '
      toptit(3)(1:5)='Ax = '
      toptit(3)(16:22)=', Ay = '
      toptit(3)(33:80)=' '
      toptit(4)(1:5)='Qx = '
      write(toptit(4)(6:15),10010) qwc(1)
      toptit(4)(16:22)=', Qy = '
      write(toptit(4)(23:32),10010) qwc(2)
      toptit(4)(33:39)=', Qs = '
      write(toptit(4)(39:48),10010) qwc(3)
      toptit(4)(49:80)=' '
      title(1)='Normalized Distance of Phase Space D as a Function '    &
     &//'of Turn Number N'
      title(2)='Normalized Horizontal Phase Space Projection'
      title(3)='Normalized Vertical Phase Space Projection'
      title(4)='Physical Phase Space Projection'
      title(5)='Synchrotron Phase Space Projection'
      title(6)='Synchrotron versus Horizontal Phase Space '             &
     &//'Projection'
      title(7)='Synchrotron versus Vertical Phase Space '               &
     &//'Projection'
      title(8)='Energy E as a Function of Turn Number N'
      title(9)='Stroboscoped Normalized Horizontal Phase Space '        &
     &//'Projection'
      title(10)='Stroboscoped Normalized Vertical Phase Space '         &
     &//'Projection'
      title(11)='FFT Analysis of the X Coordinate'
      title(12)='FFT Analysis of the Y Coordinate'
      chxtit(1)='N'
      chytit(1)='D (PI rad)'
      chxtit(2)='X (mm)'
      chytit(2)='Normalized Slope of X (mm)'
      chxtit(3)='Y (mm)'
      chytit(3)='Normalized Slope of Y (MM)'
      chxtit(4)='X (mm)'
      chytit(4)='Y (mm)'
      chxtit(5)='Sigma (mm)'
      chytit(5)='Relative Momentum Deviation'
      chxtit(6)='X (mm)'
      chytit(6)='Relative Momentum Deviation'
      chxtit(7)='Y (mm)'
      chytit(7)='Relative Momentum Deviation'
      chxtit(8)='N'
      chytit(8)='E (MeV)'
      chxtit(9)='X (mm)'
      chytit(9)='Normalized Slope of X (mm)'
      chxtit(10)='Y (mm)'
      chytit(10)='Normalized Slope of Y (mm)'
      chxtit(11)='Horizontal Tune Qx'
      chytit(11)='Normalized FFT Signal'
      chxtit(12)='Vertical Tune Qy'
      chytit(12)='Normalized FFT Signal'
      if(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0) then
        call hplsiz(15.,15.,' ')
        call hplset('VSIZ',.24)
        call hplset('ASIZ',.19)
        call hplset('XLAB',1.5)
        call hplset('YLAB',1.0)
        call hplset('GSIZ',.19)
      endif
      if(iav.lt.1) iav=1
      if(nprint.eq.1) then
        write(*,10040) sixtit,commen
        write(*,10050) progrm,ifipa,itopa,hvs,numl,                     &
     &bet0(1),bet0x2,bet0x3,                                            &
     &bet0(2),bet0z2,bet0z3,bet0(3),bet0s2,bet0s3,                      &
     &alf0(1),alf0x2,alf0x3
        write(*,10060) alf0(2),alf0z2,alf0z3,alf0(3),alf0s2,alf0s3,     &
     &gam0x1,gam0x2,gam0x3,                                             &
     &gam0z1,gam0z2,gam0z3,gam0s1,gam0s2,gam0s3,                        &
     &clo(1),clo(2),clo(3),clop(1),clop(2),clop(3),                     &
     &di0(1),di0(2),dip0(1),dip0(2),qwc(1),qwc(2),qwc(3)
        write(*,10070) iav,nstart,nstop,dphix,dphiz,iwg, qx0,qz0
        write(*,10080) ivox,ivoz,ires,dres,ifh,dfft
        write(*,10090) idis,icow,istw,iffw
        write(*,10100) iskip,iconv,imad,cma1,cma2,nprint,ndafi
      endif
!--INITIALISATION
      tpi=8*atan(one)
      prec=c1m1
      i1=0
      i11=1
      tlo=zero
      i2=0
      ifp=0
      iwarx=0
      iwarz=0
      iwar6=0
      iapx=0
      iapz=0
      iap6=0
      ivo6=1
      qs0=zero
      armin0=1e9
      armin=armin0
      nivh=ninv/2
      finv=tpi/ninv
      dani(1)=zero
      dani(ninv+1)=tpi
      do 150 i=1,ninv-1
  150 dani(i+1)=i*finv
      dle1=zero
      bold=zero
      dle1c=zero
      const=0.0
      dphx=zero
      dphz=zero
      dph6=zero
      tphx=zero
      tphz=zero
      tph6=zero
      sdpx=zero
      sdpz=zero
      sdp6=zero
      evx=zero
      evz=zero
      evx2=zero
      evz2=zero
      evt=zero
      sevx=zero
      sevz=zero
      sevt=zero
      emax=zero
      emix=zero
      emaz=zero
      emiz=zero
      emag=zero
      emig=zero
      emxa=zero
      emza=zero
      emta=zero
      emxs=zero
      emzs=zero
      emts=zero
      nuex=0
      nuez=0
      nuix=0
      nuiz=0
      xing=zero
      zing=zero
      pinx=zero
      pinz=zero
      pixr=zero
      pizr=zero
!--INVERTING THE MATRIX OF THE GENERATING VECTORS
      do 160 i=1,6
        do 160 j=1,6
  160 t(i,j)=ta(j,i)
      if(abs(t(1,1)).le.pieni.and.abs(t(2,2)).le.pieni) then
        t(1,1)=one
        t(2,2)=one
      endif
      if(abs(t(3,3)).le.pieni.and.abs(t(4,4)).le.pieni) then
        t(3,3)=one
        t(4,4)=one
      endif
      if(abs(t(5,5)).le.pieni.and.abs(t(6,6)).le.pieni) then
        t(5,5)=one
        t(6,6)=one
      endif
      tasum=zero
      its6d=0
      do 170 i=1,6
        tasum=tasum+abs(t(i,5))+abs(t(i,6))
  170 continue
      do 180 i=1,4
        tasum=tasum+abs(t(5,i))+abs(t(6,i))
  180 continue
      tasum=tasum-two
      if(abs(tasum).ge.pieni) its6d=1
      call dinv(6,t,6,idummy,nerror)
      if(nerror.eq.-1) then
        write(*,10290) nfile
        goto 550
      endif
!----------------------------------------------------------------------
!--FIND MINIMUM VALUE OF THE DISTANCE IN PHASESPACE
!----------------------------------------------------------------------
  190 ifipa=0
      if(ntwin.eq.1) read(nfile,end=200,iostat=ierro) ia,ifipa,b,c,d,e, &
     &f,g,h,p
      if(ntwin.eq.2) read(nfile,end=200,iostat=ierro) ia,ifipa,b,c,d,e, &
     &f,g,h,p, ilapa,b,c1,d1,e1,f1,g1,h1,p1
      if(ierro.gt.0) then
        write(*,10320) nfile
        goto 550
      endif
      if(ifipa.lt.1) goto 190
      if((ia-nstart).lt.zero) goto 190
      if(progrm.eq.'MAD') then
        c=c*c1e3
        d=d*c1e3
        e=e*c1e3
        f=f*c1e3
        h=h*c1e3
        p=p*c1e3
        if(ntwin.eq.2) then
          c1=c1*c1e3
          d1=d1*c1e3
          e1=e1*c1e3
          f1=f1*c1e3
          h1=h1*c1e3
          p1=p1*c1e3
        endif
      endif
      if(ntwin.eq.2) then
        x(1,1)=c
        x(1,2)=d
        x(1,3)=e
        x(1,4)=f
        x(1,5)=g
        x(1,6)=h
        x(2,1)=c1
        x(2,2)=d1
        x(2,3)=e1
        x(2,4)=f1
        x(2,5)=g1
        x(2,6)=h1
        call distance(x,cloau,di0au,t,b)
      endif
      if(nstop.gt.nstart.and.(ia-nstop).gt.zero) goto 200
      if(b.lt.b0.or.abs(b0).le.pieni) b0=b
      goto 190
  200 if(ia.le.0) goto 530
      rewind nfile
!----------------------------------------------------------------------
!--GET FIRST DATA POINT AS A REFERENCE
!----------------------------------------------------------------------
      read(nfile,iostat=ierro)
      if(ierro.gt.0) then
        write(*,10320) nfile
        goto 550
      endif
  210 ifipa=0
      if(ntwin.eq.1) read(nfile,end=530,iostat=ierro) ia,ifipa,b,c,d,e, &
     &f,g,h,p
      if(ntwin.eq.2) read(nfile,end=530,iostat=ierro) ia,ifipa,b,c,d,e, &
     &f,g,h,p, ilapa,b,c1,d1,e1,f1,g1,h1,p1
      if(ierro.gt.0) then
        write(*,10320) nfile
        goto 550
      endif
      if(ifipa.lt.1) goto 210
      if((ia-nstart).lt.0) goto 210
      if(progrm.eq.'MAD') then
        c=c*c1e3
        d=d*c1e3
        e=e*c1e3
        f=f*c1e3
        g=g*c1e3
        p=p*c1e3
        if(ntwin.eq.2) then
          c1=c1*c1e3
          d1=d1*c1e3
          e1=e1*c1e3
          f1=f1*c1e3
          g1=g1*c1e3
          p1=p1*c1e3
        endif
      endif
      dp1=h
      write(toptit(2)(51:60),10000) dp1-clop(3)
      if(nprint.eq.1.and.ia.eq.0) then
        write(*,*) 'INITIAL COORDINATES'
        write(*,*) '       X = ',c
        write(*,*) '      XP = ',d
        write(*,*) '       Z = ',e
        write(*,*) '      ZP = ',f
        write(*,*) '   SIGMA = ',g
        write(*,*) '    DP/P = ',h
        write(*,*) '  ENERGY = ',p
      endif
      if(nstop.gt.nstart.and.(ia-nstop).gt.0) goto 540
      ia=ia-nstart
!--LYAPUNOV
      if(ntwin.eq.2) then
        x(1,1)=c
        x(1,2)=d
        x(1,3)=e
        x(1,4)=f
        x(1,5)=g
        x(1,6)=h
        x(2,1)=c1
        x(2,2)=d1
        x(2,3)=e1
        x(2,4)=f1
        x(2,5)=g1
        x(2,6)=h1
        call distance(x,cloau,di0au,t,b)
      endif
!--KEEP THE FIRST TURN NUMBER : IA0
      ia0=ia
      xxr(1)=c
      xxi(1)=zero
      zzr(1)=e
      zzi(1)=zero
      c=c-clo(1)
      d=d-clop(1)
      e=e-clo(2)
      f=f-clop(2)
      g=g-clo(3)
      h=h-clop(3)
      c1=c1-clo(1)
      d1=d1-clop(1)
      e1=e1-clo(2)
      f1=f1-clop(2)
      g1=g1-clo(3)
      h1=h1-clop(3)
      if(icode.ge.4) then
        c=c-di0(1)*h
        d=d-dip0(1)*h
        e=e-di0(2)*h
        f=f-dip0(2)*h
        c1=c1-di0(1)*h
        d1=d1-dip0(1)*h
        e1=e1-di0(2)*h
        f1=f1-dip0(2)*h
      endif
!--EMITTANCES
      xp0=bet0(1)*d+alf0(1)*c
      zp0=bet0(2)*f+alf0(2)*e
      emx=(c*c+xp0*xp0)/bet0(1)
      emz=(e*e+zp0*zp0)/bet0(2)
      if(icode.ge.4.and.its6d.ne.0) then
        c=c+di0(1)*h
        d=d+dip0(1)*h
        e=e+di0(2)*h
        f=f+dip0(2)*h
        c1=c1+di0(1)*h
        d1=d1+dip0(1)*h
        e1=e1+di0(2)*h
        f1=f1+dip0(2)*h
      endif
      emt=emx+emz
      emax=emx
      emix=emx
      emxa=emx
      emaz=emz
      emiz=emz
      emza=emz
      emat=emt
      emit=emt
      emta=emt
      emx0=emx
      emz0=emz
!--COURANT SYNDER
      xyzv(1)=c
      xyzv(2)=d
      xyzv(3)=e
      xyzv(4)=f
      xyzv(5)=g
      xyzv(6)=h
!--CONVERT TO CANONICAL VARIABLES
      if(its6d.eq.1) then
        xyzv(2)=xyzv(2)*(one+xyzv(6)+clop(3))
        xyzv(4)=xyzv(4)*(one+xyzv(6)+clop(3))
      endif
      do 220 iq=1,6
        txyz(iq)=zero
        do 220 jq=1,6
          txyz(iq)=txyz(iq)+t(jq,iq)*xyzv(jq)
  220 continue
!--INITIAL COORDINATES
      if(nprint.eq.1.and.ia.eq.0) then
        write(*,*) 'DISTANCE = ',b
      endif
!--EMITTANCES WITH LINEAR COUPLING
      evx=txyz(1)*txyz(1)+txyz(2)*txyz(2)
      evz=txyz(3)*txyz(3)+txyz(4)*txyz(4)
      xyzv2(1)=c1
      xyzv2(2)=d1
      xyzv2(3)=e1
      xyzv2(4)=f1
      xyzv2(5)=g1
      xyzv2(6)=h1
!--CONVERT TO CANONICAL VARIABLES
      if(its6d.eq.1) then
        xyzv2(2)=xyzv2(2)*(one+xyzv2(6)+clop(3))
        xyzv2(4)=xyzv2(4)*(one+xyzv2(6)+clop(3))
      endif
      do 225 iq=1,6
        txyz2(iq)=zero
        do 225 jq=1,6
          txyz2(iq)=txyz2(iq)+t(jq,iq)*xyzv2(jq)
  225 continue
      evx2=txyz2(1)*txyz2(1)+txyz2(2)*txyz2(2)
      evz2=txyz2(3)*txyz2(3)+txyz2(4)*txyz2(4)
      write(toptit(3)(6:15),10010) sqrt(evx*bet0(1))+sqrt(evz*bet0x2)
      write(toptit(3)(23:32),10010) sqrt(evz*bet0(2))+sqrt(evx*bet0z2)
      if(its6d.eq.1) then
        emiii=txyz(5)*txyz(5)*cma2*cma2+txyz(6)*txyz(6)*cma1*cma1
      else
        emiii=zero
      endif
!--COURANT SYNDER CONT.
      do 230 iq=1,6
        txyz(iq)=txyz(iq)*rbeta(iq)
  230 continue
      c0=txyz(1)
      d0=txyz(2)
      e0=txyz(3)
      f0=txyz(4)
      g0=txyz(5)*cma2
      h0=txyz(6)*cma1
!--MIN MAX VALUES
      pmin(2)=b
      pmax(2)=b
      pmin(3)=c0
      pmax(3)=c0
      pmin(4)=d0
      pmax(4)=d0
      pmin(5)=e0
      pmax(5)=e0
      pmin(6)=f0
      pmax(6)=f0
      pmin(9)=g0
      pmax(9)=g0
      pmin(10)=h0
      pmax(10)=h0
      pmin(16)=p
      pmax(16)=p
!--EMITTANCES WITH LINEAR COUPLING CONT.
      emi=evx
      emii=evz
      angi=zero
      angii=zero
      angiii=zero
      if(abs(txyz(1)).gt.pieni.or.abs(txyz(2)).gt.pieni)                &
     &angi=atan2(txyz(2),txyz(1))
      if(abs(txyz(3)).gt.pieni.or.abs(txyz(4)).gt.pieni)                &
     &angii=atan2(txyz(4),txyz(3))
      if(abs(txyz(5)).gt.pieni.or.abs(txyz(6)).gt.pieni)                &
     &angiii=atan2(txyz(6)*cma1,txyz(5)*cma2)
      evt=evx+evz
      evxma=evx
      evzma=evz
      evtma=evt
      evxmi=evx
      evzmi=evz
      evtmi=evt
!--COORDINATE-ANGLE CONVERSION
      call caconv(dpx,d0,c0)
      call caconv(dpz,f0,e0)
      dpxp=tpi+dpx
      dpzp=tpi+dpz
!--INVARIANTS
      call cinvar(dpx,dphix,dpz,dpzp,nuex,emz,zinv,invz)
      call cinvar(dpz,dphiz,dpx,dpxp,nuez,emx,xinv,invx)
!----------------------------------------------------------------------
!--GET DATA POINTS
!----------------------------------------------------------------------
      iskc=0
  240 ifipa=0
      if(ntwin.eq.1) read(nfile,end=270,iostat=ierro) ia,ifipa,b,c,d,e, &
     &f,g,h,p
      if(ntwin.eq.2) read(nfile,end=270,iostat=ierro) ia,ifipa,b,c,d,e, &
     &f,g,h,p, ilapa,b,c1,d1,e1,f1,g1,h1,p1
      if(ierro.gt.0) then
        write(*,10320) nfile
        goto 550
      endif
      if(ifipa.lt.1) goto 240
      if(progrm.eq.'MAD') then
        c=c*c1e3
        d=d*c1e3
        e=e*c1e3
        f=f*c1e3
        g=g*c1e3
        p=p*c1e3
        if(ntwin.eq.2) then
          c1=c1*c1e3
          d1=d1*c1e3
          e1=e1*c1e3
          f1=f1*c1e3
          g1=g1*c1e3
          p1=p1*c1e3
        endif
      endif
!--LYAPUNOV
      if(ntwin.eq.2) then
        x(1,1)=c
        x(1,2)=d
        x(1,3)=e
        x(1,4)=f
        x(1,5)=g
        x(1,6)=h
        x(2,1)=c1
        x(2,2)=d1
        x(2,3)=e1
        x(2,4)=f1
        x(2,5)=g1
        x(2,6)=h1
        call distance(x,cloau,di0au,t,b)
      endif
      iskc=iskc+1
      if(mod(iskc,iskip).ne.0) goto 240
      if(nstop.gt.nstart.and.(ia-nstop).gt.0) goto 270
      i1=i1+1
      i11=i1+1
      if(i2.ge.nlya.and.i11.gt.nfft.and.iapx.gt.npos.and.iapz.gt.npos)  &
     &goto 270
      if(i11.le.nfft) then
        xxr(i11)=c
        xxi(i11)=zero
      endif
      if(i11.le.nfft) then
        zzr(i11)=e
        zzi(i11)=zero
      endif
      c=c-clo(1)
      d=d-clop(1)
      e=e-clo(2)
      f=f-clop(2)
      g=g-clo(3)
      h=h-clop(3)
      if(icode.ge.4) then
        c=c-di0(1)*h
        d=d-dip0(1)*h
        e=e-di0(2)*h
        f=f-dip0(2)*h
      endif
!--EMITTANCES
      xp=bet0(1)*d+alf0(1)*c
      zp=bet0(2)*f+alf0(2)*e
      emx=(c*c+xp*xp)/bet0(1)
      emz=(e*e+zp*zp)/bet0(2)
      if(icode.ge.4.and.its6d.ne.0) then
        c=c+di0(1)*h
        d=d+dip0(1)*h
        e=e+di0(2)*h
        f=f+dip0(2)*h
      endif
      emt=emx+emz
      emxa=emxa+emx
      emza=emza+emz
      emta=emta+emt
      emax=max(emx,emax)
      emix=min(emx,emix)
      emaz=max(emz,emaz)
      emiz=min(emz,emiz)
      emat=max(emt,emat)
      emit=min(emt,emit)
!--COURANT SYNDER
      xyzv(1)=c
      xyzv(2)=d
      xyzv(3)=e
      xyzv(4)=f
      xyzv(5)=g
      xyzv(6)=h
!--CONVERT TO CANONICAL VARIABLES
      if(its6d.eq.1) then
        xyzv(2)=xyzv(2)*(one+xyzv(6)+clop(3))
        xyzv(4)=xyzv(4)*(one+xyzv(6)+clop(3))
      endif
      do 250 iq=1,6
        txyz(iq)=zero
        do 250 jq=1,6
          txyz(iq)=txyz(iq)+t(jq,iq)*xyzv(jq)
  250 continue
!--EMITTANCES WITH LINEAR COUPLING
      evx1=txyz(1)*txyz(1)+txyz(2)*txyz(2)
      evz1=txyz(3)*txyz(3)+txyz(4)*txyz(4)
!--COURANT SYNDER CONT.
      do 260 iq=1,6
        txyz(iq)=txyz(iq)*rbeta(iq)
  260 continue
      c=txyz(1)
      d=txyz(2)
      e=txyz(3)
      f=txyz(4)
      g=txyz(5)*cma2
      h=txyz(6)*cma1
!--MIN MAX VALUES
      pmin(2)= min(pmin(2) ,b)
      pmax(2)= max(pmax(2) ,b)
      pmin(3)= min(pmin(3) ,c)
      pmax(3)= max(pmax(3) ,c)
      pmin(4)= min(pmin(4) ,d)
      pmax(4)= max(pmax(4) ,d)
      pmin(5)= min(pmin(5) ,e)
      pmax(5)= max(pmax(5) ,e)
      pmin(6)= min(pmin(6) ,f)
      pmax(6)= max(pmax(6) ,f)
      pmin(9)= min(pmin(9) ,g)
      pmax(9)= max(pmax(9) ,g)
      pmin(10)=min(pmin(10),h)
      pmax(10)=max(pmax(10),h)
      pmin(16)=min(pmin(16),p)
      pmax(16)=max(pmax(16),p)
!--ADDING (LOG OF) THE DISTANCES OF PHASE SPACE
      ia=ia-nstart
!--GET DIFFERENCE IN THE NUMBER OF TURNS PER DATA ENTRY : IDNT
      if(i1.eq.1) idnt=ia-ia0
      bold=bold+b
      b=b-b0
      dle1c=zero
      if(b.gt.zero) dle1c=log(b)
      if(b.lt.zero) dle1c=-log(-b)
      dle1=dle1+dle1c
!--EMITTANCES WITH LINEAR COUPLING CONT.
      evt1=evx1+evz1
      evxma=max(evx1,evxma)
      evzma=max(evz1,evzma)
      evtma=max(evt1,evtma)
      evxmi=min(evx1,evxmi)
      evzmi=min(evz1,evzmi)
      evtmi=min(evt1,evtmi)
      evx=evx+evx1
      evz=evz+evz1
      evt=evt+evt1
!--ADDING OF THE PHASE ADVANCES
      sx=c*d0-c0*d
      cx=c0*c+d*d0
      if(iapx.le.npos)                                                  &
     &call cphase(1,dphx,sx,cx,qx0,ivox,iwarx,iapx)
      sz=e*f0-e0*f
      cz=e0*e+f*f0
      if(iapz.le.npos)                                                  &
     &call cphase(2,dphz,sz,cz,qz0,ivoz,iwarz,iapz)
      s6=g*h0-g0*h
      c6=h0*h+g*g0
      if(iap6.le.npos)                                                  &
     &call cphase(3,dph6,s6,c6,qs0,ivo6,iwar6,iap6)
!--AVERAGING AFTER IAV TURNS
      if(mod(i1,iav).eq.0) then
        if(i2.ge.nlya) goto 240
        i2=i2+1
        dle(i2)=dle1/iav
        if(ia.gt.0) then
          tle1=log(real(ia))
          if(i2.gt.1) then
            biav(i2-1)=bold/iav
            if(i2.eq.2) biav(1)=biav(1)*half
            bold=zero
            tle(i2)=(tle1+tlo)*half
            if(abs(tle1-tlo).gt.pieni) then
              wgh(i2)=one/(tle1-tlo)
            else
              write(*,10310) nfile
              wgh(i2)=zero
            endif
          else
            tle(i2)=tle1*half
            wgh(i2)=one/(tle1)
          endif
        else
          tle(i2)=zero
          wgh(i2)=zero
        endif
        tlo=tle1
        dle1=zero
      endif
!--COORDINATE-ANGLE CONVERSION
      call caconv(dpx,d,c)
      call caconv(dpz,f,e)
      dpxp=tpi+dpx
      dpzp=tpi+dpz
!--INVARIANTS
      call cinvar(dpx,dphix,dpz,dpzp,nuex,emz,zinv,invz)
      call cinvar(dpz,dphiz,dpx,dpxp,nuez,emx,xinv,invx)
!--RESET OF COORDINATES
      c0=c
      d0=d
      e0=e
      f0=f
      g0=g
      h0=h
      goto 240
  270 if(i2.lt.1) i2=1
!----------------------------------------------------------------------
!--ANALYSING DATA
!----------------------------------------------------------------------
!--FIT OF DISTANCE IN PHASESPACE + MEAN PHASEADVANCE
      do 280 i=2,i2
        if(iwg.eq.1) call lfitw(tle,dle,wgh,i,1,slope(i-1),const,varlea &
     &(i-1))
        if(iwg.eq.0) call lfit(tle,dle,i,1,slope(i-1),const,varlea(i-1))
  280 continue
      if(iapx.eq.0) then
        write(*,*) 'WARNING: IAPX IS ZERO'
        iapx=1
      endif
      if(iapz.eq.0) then
        write(*,*) 'WARNING: IAPZ IS ZERO'
        iapz=1
      endif
      tphx=dphx/iapx
      tphz=dphz/iapz
      if(iap6.gt.0) tph6=dph6/iap6
!--STANDARD DEVIATION OF PHASEADVANCES
      do 290 i=1,iapx
  290 sdpx=sdpx+(phase(1,i)-tphx)*(phase(1,i)-tphx)
      do 300 i=1,iapz
  300 sdpz=sdpz+(phase(2,i)-tphz)*(phase(2,i)-tphz)
      do 310 i=1,iap6
  310 sdp6=sdp6+(phase(3,i)-tph6)*(phase(3,i)-tph6)
      sdpx=sqrt(sdpx)/iapx
      sdpz=sqrt(sdpz)/iapz
      if(iap6.gt.0) sdp6=sqrt(sdp6)/iap6
!--AVERAGED EMITTANCES
      di11=i11
      if(i11.eq.0) then
        write(*,*) '** ERROR ** - I11 IS ZERO'
        goto 550
      endif
      emxa=emxa/di11
      emza=emza/di11
      emta=emta/di11
      evxm=evx/di11
      evzm=evz/di11
      evtm=evt/di11
!--SMEAR CALCULATION AND 4D-SMEAR
      rewind nfile
      read(nfile,iostat=ierro)
      if(ierro.gt.0) then
        write(*,10320) nfile
        goto 550
      endif
      iskc=-1
      do 340 i=1,i11*iskip+nstart
        ifipa=0
        read(nfile,end=350,iostat=ierro) ia,ifipa,b,c,d,e,f,g,h,p
        if(ierro.gt.0) then
          write(*,10320) nfile
          goto 550
        endif
        if(ifipa.lt.1) goto 340
        if(progrm.eq.'MAD') then
          c=c*c1e3
          d=d*c1e3
          e=e*c1e3
          f=f*c1e3
          g=g*c1e3
          p=p*c1e3
        endif
        iskc=iskc+1
        if(mod(iskc,iskip).ne.0) goto 340
        if((ia-nstart).lt.0) goto 340
        c=c-clo(1)
        d=d-clop(1)
        e=e-clo(2)
        f=f-clop(2)
        g=g-clo(3)
        h=h-clop(3)
        if(icode.ge.4) then
          c=c-di0(1)*h
          d=d-dip0(1)*h
          e=e-di0(2)*h
          f=f-dip0(2)*h
        endif
!--MEAN EMITTANCES
        xp=bet0(1)*d+alf0(1)*c
        zp=bet0(2)*f+alf0(2)*e
        emx=(c*c+xp*xp)/bet0(1)
        emz=(e*e+zp*zp)/bet0(2)
        if(icode.ge.4.and.its6d.ne.0) then
          c=c+di0(1)*h
          d=d+dip0(1)*h
          e=e+di0(2)*h
          f=f+dip0(2)*h
        endif
        emt=emx+emz
        emxs=emxs+(emx-emxa)*(emx-emxa)
        emzs=emzs+(emz-emza)*(emz-emza)
        emts=emts+(emt-emta)*(emt-emta)
!--COURANT SYNDER
        xyzv(1)=c
        xyzv(2)=d
        xyzv(3)=e
        xyzv(4)=f
        xyzv(5)=g
        xyzv(6)=h
!--CONVERT TO CANONICAL VARIABLES
        if(its6d.eq.1) then
          xyzv(2)=xyzv(2)*(one+xyzv(6)+clop(3))
          xyzv(4)=xyzv(4)*(one+xyzv(6)+clop(3))
        endif
        do 320 iq=1,6
          txyz(iq)=zero
          do 320 jq=1,6
            txyz(iq)=txyz(iq)+t(jq,iq)*xyzv(jq)
  320   continue
!--MEAN EMITTANCES WITH LINEAR COUPLING
        evx=txyz(1)*txyz(1)+txyz(2)*txyz(2)
        evz=txyz(3)*txyz(3)+txyz(4)*txyz(4)
!--COURANT SYNDER CONT.
        do 330 iq=1,6
          txyz(iq)=txyz(iq)*rbeta(iq)
  330   continue
        c=txyz(1)
        d=txyz(2)
        e=txyz(3)
        f=txyz(4)
        g=txyz(5)
        h=txyz(6)
!--MEAN EMITTANCES WITH LINEAR COUPLING CONT.
        evt=evx+evz
        sevx=sevx+(evx-evxm)*(evx-evxm)
        sevz=sevz+(evz-evzm)*(evz-evzm)
        sevt=sevt+(evt-evtm)*(evt-evtm)
  340 continue
  350 continue
!--SMEAR IN %
      call sinpro(emxa,di11,emxs,emax,emix)
      call sinpro(emza,di11,emzs,emaz,emiz)
      call sinpro(emta,di11,emts,emat,emit)
      call sinpro(evxm,di11,sevx,evxma,evxmi)
      call sinpro(evzm,di11,sevz,evzma,evzmi)
      call sinpro(evtm,di11,sevt,evtma,evtmi)
!----------------------------------------------------------------------
!--PRINTING
!----------------------------------------------------------------------
      if(nstop.lt.ia.and.(ia.lt.numl.or.ia.lt.nint(dnumlr))) nlost=1
      if(nnumxv(ifipa).eq.0.and.nnumxv(ilapa).eq.0) then
        sumda(22)=ia
        sumda(23)=ia
      else
        sumda(22)=nnumxv(ifipa)
        sumda(23)=nnumxv(ilapa)
      endif
      sumda(2)=nlost
      sumda(9)=dp1-clop(3)
!--GET DIFFERENCE IN THE NUMBER OF TURNS PER DATA ENTRY : TIDNT
!--NOW CONSIDERING ONLY TURNS LARGER THAN NSTART
      tidnt=(ia-nstart+idnt)/i11
      if(i2.ge.2) then
        if(nprint.eq.1) write(*,10110)
        ilyap=0
        slopem=zero
        do 360 i=1,i2-1
          iturn=nint((i+1)*iav*tidnt)
          if(nprint.eq.1) write(*,10120) iturn,biav(i),slope(i),        &
     &varlea(i)
          if(biav(i).gt.0.1d0) ilyap=1
          slopem=max(slopem,dble(slope(i)))
  360   continue
        if(nprint.eq.1) write(*,10130)
        sumda(10)=biav(i2-1)
        if(ilyap.eq.0) then
         sumda(11)=slope(i2-1)
        else
         sumda(11)=slopem
        endif
      endif
!--CALCULATION OF AVERAGED PHASEADVANCES
      tph6=abs(tph6)
      if(nprint.eq.1) write(*,10140)tphx,sdpx,tphz,sdpz,tph6,sdp6,qwc   &
     &(1),tphx-qwc(1) ,qwc(2),tphz-qwc(2),qwc(3),tph6-qwc(3),dres,ires
      sumda(3)=qwc(1)
      sumda(4)=qwc(2)
      if(abs(tphx).gt.pieni) then
        sumda(12)=tphx-qwc(1)
      else
        sumda(12)=zero
      endif
      sumda(13)=sdpx
      if(abs(tphz).gt.pieni) then
        sumda(14)=tphz-qwc(2)
      else
        sumda(14)=zero
      endif
      sumda(15)=sdpz
      sumda(25)=tph6
!--DISTANCE OF Q-VALUES (AVERAGED PHASEADVANCE) TO RESONANCES
      do 370 i=1,21
        do 370 j=1,21
          im1=i-1
          jm1=j-1
          if(im1.eq.0.and.jm1.eq.0) goto 370
          if(im1+jm1.gt.ires) goto 370
          ares=im1*tphx+jm1*tphz
          dares=anint(ares)
          ares=ares-dares
          if(abs(ares).lt.armin) then
            armin=abs(ares)
            im1s=im1
            jm1s=jm1
          endif
          ared=im1*tphx-jm1*tphz
          dared=anint(ared)
          ared=ared-dared
          if(abs(ared).lt.armin) then
            armin=abs(ared)
            im1s=im1
            jm1s=-jm1
          endif
          if(abs(ares).lt.dres.and.nprint.eq.1) write(*,10170) im1,jm1, &
     &dares,ares
          if(abs(ared).lt.dres.and.jm1.ne.0.and.im1.ne.0.and.nprint.eq. &
     &1) write(*,10170) im1,-jm1,dared,ared
  370 continue
      if(armin.lt.armin0) then
        sumda(16)=im1s
        sumda(17)=jm1s
        sumda(18)=sumda(16)+abs(sumda(17))
      endif
      if(iwarx.eq.1.and.nprint.eq.1) write(*,10150)
      if(iwarz.eq.1.and.nprint.eq.1) write(*,10160)
!--Q-VALUES BY AN FFT-ROUTINE
  380 ifp=ifp+1
      ife=2**ifp
      if(ife.le.i11.and.ife.le.nfft) then
        goto 380
      else
        ifp=ifp-1
        ife=ife/2
      endif
      if(ife.eq.0) then
        write(*,*) '** ERROR ** - IFE IS ZERO'
        goto 550
      endif
      dife=ife
      if(ifp.gt.1) then
        if(nprint.eq.1) write(*,10180) ife,dfft*100
        call fft(xxr,xxi,ifp,ife)
        call fft(zzr,zzi,ifp,ife)
        xxmax=zero
        zzmax=zero
        xxmin=one
        zzmin=one
        if(ifh.eq.0) then
          if1=1
          if2=ife
          ife2=ife
          pmin(21)=qx0
          pmax(21)=qx0+one
          pmin(23)=qz0
          pmax(23)=qz0+one
        else if(ifh.eq.1) then
          if1=1
          if2=ife/2
          ife2=ife/2
          pmin(21)=qx0
          pmax(21)=qx0+half
          pmin(23)=qz0
          pmax(23)=qz0+half
        else
          if1=ife/2+1
          if2=ife
          ife2=ife/2
          pmin(21)=qx0+half
          pmax(21)=qx0+one
          pmin(23)=qz0+half
          pmax(23)=qz0+one
        endif
        do 390 i=if1,if2
          xxmax=max(xxmax,sqrt(xxr(i)**2+xxi(i)**2))
          zzmax=max(zzmax,sqrt(zzr(i)**2+zzi(i)**2))
          xxmin=min(xxmin,sqrt(xxr(i)**2+xxi(i)**2))
          zzmin=min(zzmin,sqrt(zzr(i)**2+zzi(i)**2))
  390   continue
        if(abs(xxmax).gt.pieni) xxmin=xxmin/xxmax
        if(abs(zzmax).gt.pieni) zzmin=zzmin/zzmax
        if(xxmax.le.pieni) then
          write(*,*) 'WARNING: XXMAX IS SET TO : ',pieni
          xxmax=pieni
        endif
        if(zzmax.le.pieni) then
          write(*,*) 'WARNING: ZZMAX IS SET TO : ',pieni
          zzmax=pieni
        endif
        do 400 i=if1,if2
          xxaux=sqrt(xxr(i)**2+xxi(i)**2)
          zzaux=sqrt(zzr(i)**2+zzi(i)**2)
          if(abs(xxaux-xxmax).le.pieni) ffx=(i-1)/dife+qx0
          if(abs(zzaux-zzmax).le.pieni) ffz=(i-1)/dife+qz0
          xxaux=xxaux/xxmax
          zzaux=zzaux/zzmax
          if(xxaux.gt.dfft.and.nprint.eq.1) write(*,10190) (i-1)/dife   &
     &+qx0,xxaux*100
          if(zzaux.gt.dfft.and.nprint.eq.1) write(*,10200) (i-1)/dife   &
     &+qz0,zzaux*100
  400   continue
        if(nprint.eq.1) write(*,10210) ffx,ffz,qwc(1),ffx-qwc(1),qwc(2),&
     &ffz-qwc(2),dres,ires
!--DISTANCE OF Q-VALUES (FFT) TO RESONANCES
        do 410 i=1,21
          do 410 j=1,21
            im1=i-1
            jm1=j-1
            if(im1.eq.0.and.jm1.eq.0) goto 410
            if(im1+jm1.gt.ires) goto 410
            ares=im1*ffx+jm1*ffz
            dares=anint(ares)
            ares=ares-dares
            ared=im1*ffx-jm1*ffz
            dared=anint(ared)
            ared=ared-dared
            if(abs(ares).lt.dres.and.nprint.eq.1) write(*,10170) im1,   &
     &jm1,dares,ares
            if(abs(ared).lt.dres.and.jm1.ne.0.and.im1.ne.0.and.nprint.eq&
     &.1) write(*,10170) im1,-jm1,dared,ared
  410   continue
      endif
!--PRINT 4-D INVARIANTS WITH LINEAR COUPLING
      if(nprint.eq.1) write(*,10270) emi,emii,emiii,angi,angii,angiii,  &
     &evxm,sevx,evxma,evxmi,evzm,sevz,evzma,evzmi,evtm,sevt,evtma,evtmi
!--PRINT EMITTANCES AND SMEAR
      ampx0=sqrt(bet0(1)*emx0)
      ampz0=sqrt(bet0(2)*emz0)
      if(nprint.eq.1) write(*,10220) emx0,ampx0,emz0,ampz0,emxa,emxs,   &
     &emax,emix,emza, emzs,emaz,emiz,emta,emts,emat,emit
      sumda(46)=emi
      sumda(47)=emii
      sumda(48)=bet0x2
      sumda(49)=bet0z2
      sumda(7)=sqrt(bet0(1)*emi)+sqrt(bet0x2*emii)
      sumda(8)=sqrt(bet0(2)*emii)+sqrt(bet0z2*emi)
      sumda(26)=sqrt(bet0(1)*evx2)+sqrt(bet0x2*evz2)
      sumda(27)=sqrt(bet0(2)*evz2)+sqrt(bet0z2*evx2)
      sumda(19)=sevx
      sumda(20)=sevz
      sumda(21)=sevt
      sumda(59)=dmmac
      sumda(60)=dnms
      sumda(24)=dizu0
      emax=emax/100*emxa+emxa
      emix=emix/100*emxa+emxa
      emaz=emaz/100*emza+emza
      emiz=emiz/100*emza+emza
      sumda(28)=sqrt(bet0(1)*abs(emix))
      sumda(29)=sqrt(bet0(1)*emxa)
      sumda(30)=sqrt(bet0(1)*emax)
      sumda(31)=sqrt(bet0(2)*abs(emiz))
      sumda(32)=sqrt(bet0(2)*emza)
      sumda(33)=sqrt(bet0(2)*emaz)
      evxma=evxma/100*evxm+evxm
      evxmi=evxmi/100*evxm+evxm
      evzma=evzma/100*evzm+evzm
      evzmi=evzmi/100*evzm+evzm
      sumda(34)=sqrt(bet0(1)*abs(evxmi))
      sumda(35)=sqrt(bet0(1)*evxm)
      sumda(36)=sqrt(bet0(1)*evxma)
      sumda(37)=sqrt(bet0(2)*abs(evzmi))
      sumda(38)=sqrt(bet0(2)*evzm)
      sumda(39)=sqrt(bet0(2)*evzma)
      evtma=evtma/100*evtm+evtm
      evtmi=evtmi/100*evtm+evtm
      if(abs(evxm+evzm).gt.pieni) then
        ratemx=evxm/(evxm+evzm)
        ratemz=evzm/(evxm+evzm)
      else
        ratemx=zero
        ratemz=zero
      endif
      sumda(40)=sqrt(bet0(1)*abs(evtmi)*ratemx)
      sumda(41)=sqrt(bet0(1)*evtm*ratemx)
      sumda(42)=sqrt(bet0(1)*evtma*ratemx)
      sumda(43)=sqrt(bet0(2)*abs(evtmi)*ratemz)
      sumda(44)=sqrt(bet0(2)*evtm*ratemz)
      sumda(45)=sqrt(bet0(2)*evtma*ratemz)
!--PUT IN THE CHROMATICITY
      sumda(50)=chromc(1)*c1e3
      sumda(51)=chromc(2)*c1e3
!--WRITE DATA FOR THE SUMMARY OF THE POSTPROCESSING ON FILE # 10
      write(ch,*,iostat=ierro) (sumda(i),i=1,60)
      do ich=8192,1,-1
        if(ch(ich:ich).ne.' ') goto 700
      enddo
 700  write(10,'(a)') ch(:ich)
      if(ierro.ne.0) then
        write(*,*)
        write(*,*) '*** ERROR ***,PROBLEMS WRITING TO FILE # : ',10
        write(*,*) 'ERROR CODE : ',ierro
        write(*,*)
      endif
!--CALCULATION THE INVARIANCES OF THE 4D TRANSVERSAL MOTION
      do 420 i=1,ninv
        if(invx(i).gt.0) then
          nuix=nuix+1
          xing=xing+xinv(i)/invx(i)
        endif
        if(invz(i).gt.0) then
          nuiz=nuiz+1
          zing=zing+zinv(i)/invz(i)
        endif
  420 continue
      pinx=nuix
      pinz=nuiz
      if(nuix.ne.0) then
        pixr=dble(nuez)/dble(nuix)
        xing=xing/nuix
      endif
      if(nuiz.ne.0) then
        pizr=dble(nuex)/dble(nuiz)
        zing=zing/nuiz
      endif
      pinx=pinx/ninv*100
      pinz=pinz/ninv*100
      if(nprint.eq.1) write(*,10230)
      if(nuez.lt.ninv.and.nprint.eq.1) write(*,10240) nuez,ninv
      if(nuex.lt.ninv.and.nprint.eq.1) write(*,10250) nuex,ninv
      if(nprint.eq.1) write(*,10260) nuez,nuix,nuex,nuiz, ninv,pinx,    &
     &pixr,pinz,pizr,xing,zing
!----------------------------------------------------------------------
!--PLOTTING
!----------------------------------------------------------------------
      pmin(1)=zero
      pmax(1)=ia
      pmin(7)=pmin(3)
      pmax(7)=pmax(3)
      pmin(8)=pmin(5)
      pmax(8)=pmax(5)
      pmin(11)=pmin(3)
      pmax(11)=pmax(3)
      pmin(12)=pmin(10)
      pmax(12)=pmax(10)
      pmin(13)=pmin(5)
      pmax(13)=pmax(5)
      pmin(14)=pmin(10)
      pmax(14)=pmax(10)
      pmax(15)=ia
      pmin(17)=pmin(3)
      pmax(17)=pmax(3)
      pmin(18)=pmin(4)
      pmax(18)=pmax(4)
      pmin(19)=pmin(5)
      pmax(19)=pmax(5)
      pmin(20)=pmin(6)
      pmax(20)=pmax(6)
      pmin(22)=zero
      pmin(24)=zero
      pmax(22)=one
      pmax(24)=one
      do 500 i=1,12
        i2=2*i
        i1=i2-1
        if(pmin(i1).gt.pmax(i1)) pmin(i1)=pmax(i1)
        if(pmin(i2).gt.pmax(i2)) pmin(i2)=pmax(i2)
        if((abs(pmin(i1)-pmax(i1)).le.pieni2) .or.(abs(pmin(i2)-pmax(i2)&
     &).le.pieni2)) then
          goto 500
        endif
        do 430 i3=i1,i2
          pcha=(pmax(i3)-pmin(i3))*prec
          pmin(i3)=pmin(i3)-pcha
          pmax(i3)=pmax(i3)+pcha
  430   continue
        if(iffw.eq.2) then
          pmin(22)=xxmin/(1d0+abs(prec))
          pmin(24)=zzmin/(1d0+abs(prec))
        endif
        if((i.eq.1.and.idis.eq.1).or. (i.gt.1.and.i.le.8.and.icow.eq.1) &
     &.or. ((i.eq.9.or.i.eq.10).and.istw.eq.1).and. (pmin(i1).ne.pmax   &
     &(i1).and.pmin(i2).ne.pmax(i2))) then
!--HBOOK FRAME
          call htitle(title(i))
          call hbook2(i,' ',2,real(pmin(i1)),real(pmax(i1)), 2,real     &
     &(pmin(i2)),real(pmax(i2)),0.)
          call hplot(i,' ',' ',0)
          call hplax(chxtit(i),chytit(i))
          call hplsof(4.,14.75,toptit(1),.15,0.,99.,-1)
          call hplsof(4.,14.50,toptit(2),.15,0.,99.,-1)
          call hplsof(4.,14.25,toptit(3),.15,0.,99.,-1)
          call hplsof(4.,14.00,toptit(4),.15,0.,99.,-1)
          call iselnt(10)
          rewind nfile
          read(nfile,iostat=ierro)
          if(ierro.gt.0) then
            write(*,10320) nfile
            goto 550
          endif
          iskc=-1
          do 460 j=1,i11*iskip+nstart
            ifipa=0
            if(ntwin.eq.1) read(nfile,end=470,iostat=ierro) ia,ifipa,b, &
     &c,d,e,f,g,h,p
            if(ntwin.eq.2) read(nfile,end=470,iostat=ierro) ia,ifipa,b, &
     &c,d,e,f,g,h,p, ilapa,b,c1,d1,e1,f1,g1,h1,p1
            if(ierro.gt.0) then
              write(*,10320) nfile
              goto 550
            endif
            if(ifipa.lt.1) goto 460
            iskc=iskc+1
            if(mod(iskc,iskip).ne.0) goto 460
            if((ia-nstart).lt.0) goto 460
            if(progrm.eq.'MAD') then
              c=c*c1e3
              d=d*c1e3
              e=e*c1e3
              f=f*c1e3
              g=g*c1e3
              p=p*c1e3
              if(ntwin.eq.2) then
                c1=c1*c1e3
                d1=d1*c1e3
                e1=e1*c1e3
                f1=f1*c1e3
                g1=g1*c1e3
                p1=p1*c1e3
              endif
            endif
!--LYAPUNOV
            if(ntwin.eq.2) then
              x(1,1)=c
              x(1,2)=d
              x(1,3)=e
              x(1,4)=f
              x(1,5)=g
              x(1,6)=h
              x(2,1)=c1
              x(2,2)=d1
              x(2,3)=e1
              x(2,4)=f1
              x(2,5)=g1
              x(2,6)=h1
              call distance(x,cloau,di0au,t,b)
            endif
            if(icode.ge.4.and.its6d.eq.0) then
              c=c-di0(1)*h
              d=d-dip0(1)*h
              e=e-di0(2)*h
              f=f-dip0(2)*h
            endif
            c=c-clo(1)
            d=d-clop(1)
            e=e-clo(2)
            f=f-clop(2)
            g=g-clo(3)
            h=h-clop(3)
            xyzv(1)=c
            xyzv(2)=d
            xyzv(3)=e
            xyzv(4)=f
            xyzv(5)=g
            xyzv(6)=h
!--CONVERT TO CANONICAL VARIABLES
            if(its6d.eq.1) then
              xyzv(2)=xyzv(2)*(one+xyzv(6)+clop(3))
              xyzv(4)=xyzv(4)*(one+xyzv(6)+clop(3))
            endif
            do 440 iq=1,6
              txyz(iq)=zero
              do 440 jq=1,6
                txyz(iq)=txyz(iq)+t(jq,iq)*xyzv(jq)
  440       continue
            do 450 iq=1,6
              txyz(iq)=txyz(iq)*rbeta(iq)
  450       continue
            c=txyz(1)
            d=txyz(2)
            e=txyz(3)
            f=txyz(4)
            g=txyz(5)*cma2
            h=txyz(6)*cma1
            if(idis.eq.1.and.i.eq.1) call ipm(1,real(ia),real(b))
            if(icow.eq.1.and.i.le.8) then
              if(i.eq.2) call ipm(1,real(c),real(d))
              if(i.eq.3) call ipm(1,real(e),real(f))
              if(i.eq.4) call ipm(1,real(c),real(e))
              if(i.eq.5) call ipm(1,real(g),real(h))
              if(i.eq.6) call ipm(1,real(c),real(h))
              if(i.eq.7) call ipm(1,real(e),real(h))
              if(i.eq.8) call ipm(1,real(ia),real(p))
            endif
            if(istw.eq.1.and.(i.eq.9.or.i.eq.10)) then
              if(i.eq.9) then
                call caconv(dpz,f,e)
                if(abs(dpz).lt.dphiz) call ipm(1,real(c),real(d))
              endif
              if(i.eq.10) then
                call caconv(dpx,d,c)
                if(abs(dpx).lt.dphix) call ipm(1,real(e),real(f))
              endif
            endif
  460     continue
  470     continue
        else if((iffw.eq.1.or.iffw.eq.2).and.(i.eq.11.or.i.eq.12)) then
!--HBOOK FRAME
          call htitle(title(i))
          call hbook2(i,' ',2,real(pmin(i1)),real(pmax(i1)), 2,real     &
     &(pmin(i2)),real(pmax(i2)),0.)
          if(iffw.eq.2) call hplopt('LOGY',1)
          call hplot(i,' ',' ',0)
          call hplax(chxtit(i),chytit(i))
          call hplsof(4.,14.75,toptit(1),.15,0.,99.,-1)
          call hplsof(4.,14.50,toptit(2),.15,0.,99.,-1)
          call hplsof(4.,14.25,toptit(3),.15,0.,99.,-1)
          call hplsof(4.,14.00,toptit(4),.15,0.,99.,-1)
          call iselnt(10)
          if(i.eq.11) then
            do 480 k=if1,if2
              k1=k-if1+1
              xxaux=sqrt(xxr(k)**2+xxi(k)**2)
              xxaux=xxaux/xxmax
              fxs(k1)=real((k-1)/dife+qx0)
              if(iffw.eq.2) then
                if(abs(xxaux).lt.pieni) then
                  write(*,*) '* * * ERROR * * *'
                  write(*,*)                                            &
     &'Apparently horizontal FFT data are corrupted'
                  xxaux=one
                endif
                fzs(k1)=log10(real(xxaux))
              else
                fzs(k1)=real(xxaux)
              endif
              if(nprint.eq.1) then
                write(14,10030,iostat=ierro) fxs(k1),fzs(k1)
                if(ierro.ne.0) then
                  write(*,*)
                  write(*,*)                                            &
     &'*** ERROR ***,PROBLEMS WRITING TO FILE # : ',14
                  write(*,*) 'ERROR CODE : ',ierro
                  write(*,*)
                endif
              endif
  480       continue
            call ipl(ife2,fxs,fzs)
          else if(i.eq.12) then
            do 490 k=if1,if2
              k1=k-if1+1
              zzaux=sqrt(zzr(k)**2+zzi(k)**2)
              zzaux=zzaux/zzmax
              fxs(k1)=real((k-1)/dife+qz0)
              if(iffw.eq.2) then
                if(abs(zzaux).lt.pieni) then
                  write(*,*) '* * * ERROR * * *'
                  write(*,*)                                            &
     &'Apparently vertical FFT data are corrupted'
                  zzaux=one
                endif
                fzs(k1)=log10(real(zzaux))
              else
                fzs(k1)=real(zzaux)
              endif
              if(nprint.eq.1) then
                write(15,10030,iostat=ierro) fxs(k1),fzs(k1)
                if(ierro.ne.0) then
                  write(*,*)
                  write(*,*)                                            &
     &'*** ERROR ***,PROBLEMS WRITING TO FILE # : ',14
                  write(*,*) 'ERROR CODE : ',ierro
                  write(*,*)
                endif
              endif
  490       continue
            call ipl(ife2,fxs,fzs)
          endif
        endif
        if(iffw.eq.2) call hplopt('LINY',1)
  500 continue
      if(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0)              &
     &call hdelet(0)
      goto 560
  510 continue
      write(*,10300) nfile,'HEADER CORRUPTED'
      goto 550
  520 continue
      write(*,10300) nfile,'HEADER OF MADFILE CORRUPTED'
      goto 550
  530 continue
      write(*,10300) nfile,'NO DATA'
      goto 550
  535 continue
      write(*,10300) nfile,'ONLY START VALUES'
      goto 550
  540 continue
      write(*,10300) nfile,'WRONG RANGE OF DATA FOR PROCESSING'
      goto 550
  550 write(ch,*,iostat=ierro) (sumda(i),i=1,60)
      do ich=8192,1,-1
        if(ch(ich:ich).ne.' ') goto 707
      enddo
 707  write(10,'(a)') ch(:ich)
!--REWIND USED FILES
  560 rewind nfile
      rewind 14
      rewind 15
!--TIME COUNT
      tim2=0.
      call timex(tim2)
      if(nprint.eq.1) write(*,10280) tim2-tim1
!----------------------------------------------------------------------
      return
10000 format(d10.4)
10010 format(f10.6)
10020 format(a80)
10030 format(2f10.6)
10040 format( //131('-')//10x,'OOOOOOOOOOOOOOOOOOOOOO' /10x,            &
     &'OO                  OO' /10x,'OO  POSTPROCESSING  OO' /10x,      &
     &'OO                  OO' /10x,'OOOOOOOOOOOOOOOOOOOOOO'// /10x,    &
     &'TITLE AND COMMENT :'//a80//a80// )
10050 format(10x,'THE FOLLOWING PARAMETERS ARE USED:'//                 &
     &10x,'PROGRAM NAME',t102,a8/                                       &
     &10x,'PARTICLE NUMBER',t102,i3/                                    &
     &10x,'TOTAL NUMBER OF PARTICLES',t102,i3/                          &
     &10x,'PHASE SPACE',t102,a11/                                       &
     &10x,'MAXIMUM NUMBER OF TURNS',t102,i8/                            &
     &10x,'HORIZONTAL BETA',t102,f16.10/                                &
     &10x,'HORIZONTAL BETA-II',t102,f16.10/                             &
     &10x,'HORIZONTAL BETA-III',t102,f16.10/                            &
     &10x,'VERTICAL BETA',t102,f16.10/                                  &
     &10x,'VERTICAL BETA-II',t102,f16.10/                               &
     &10x,'VERTICAL BETA-III',t102,f16.10/                              &
     &10x,'LONGITUDINAL BETA',t98,f20.10/                               &
     &10x,'LONGITUDINAL BETA-II',t102,f16.10/                           &
     &10x,'LONGITUDINAL BETA-III',t102,f16.10/                          &
     &10x,'HORIZONTAL ALFA',t102,f16.10/                                &
     &10x,'HORIZONTAL ALFA-II',t102,f16.10/                             &
     &10x,'HORIZONTAL ALFA-III',t102,f16.10)
10060 format(                                                           &
     &10x,'VERTICAL ALFA',t102,f16.10/                                  &
     &10x,'VERTICAL ALFA-II',t102,f16.10/                               &
     &10x,'VERTICAL ALFA-III',t102,f16.10/                              &
     &10x,'LONGITUDINAL ALFA',t102,f16.10/                              &
     &10x,'LONGITUDINAL ALFA-II',t102,f16.10/                           &
     &10x,'LONGITUDINAL ALFA-III',t102,f16.10/                          &
     &10x,'HORIZONTAL GAMMA',t102,f16.10/                               &
     &10x,'HORIZONTAL GAMMA-II',t102,f16.10/                            &
     &10x,'HORIZONTAL GAMMA-III',t102,f16.10/                           &
     &10x,'VERTICAL GAMMA',t102,f16.10/                                 &
     &10x,'VERTICAL GAMMA-II',t102,f16.10/                              &
     &10x,'VERTICAL GAMMA-III',t102,f16.10/                             &
     &10x,'LONGITUDINAL GAMMA',t102,f16.10/                             &
     &10x,'LONGITUDINAL GAMMA-II',t102,f16.10/                          &
     &10x,'LONGITUDINAL GAMMA-III',t102,f16.10/                         &
     &10x,'HORIZONTAL CLOSED ORBIT',t102,d16.10/                        &
     &10x,'VERTICAL CLOSED ORBIT',t102,d16.10/                          &
     &10x,'LONGITUDINAL CLOSED ORBIT',t102,d16.10/                      &
     &10x,'SLOPE OF HORIZONTAL CLOSED ORBIT',t102,d16.10/               &
     &10x,'SLOPE OF VERTICAL CLOSED ORBIT',t102,d16.10/                 &
     &10x,'SLOPE OF LONGITUDINAL CLOSED ORBIT',t102,d16.10/             &
     &10x,'HORIZONTAL DISPERSION',t102,f16.10/                          &
     &10x,'VERTICAL DISPERSION',t102,f16.10/                            &
     &10x,'SLOPE OF HORIZONTAL DISPERSION',t102,f16.10/                 &
     &10x,'SLOPE OF VERTICAL DISPERSION',t102,f16.10/                   &
     &10x,'LINEAR HORIZONTAL TUNE',t102,f16.10/                         &
     &10x,'LINEAR VERTICAL TUNE',t102,f16.10/                           &
     &10x,'LINEAR LONGITUDINAL TUNE',t102,f16.10)
10070 format( 10x,'DATA IS AVERAGED IN SAMPLES OF IAV TURNS',t96,       &
     &'IAV =    ',i7 /10x,'START TURN NUMBER FOR THE ANALYSIS ',t93,    &
     &'NSTART =  ',i9 /10x,'THE ANALYSIS STOPS AFTER TURN NUMBER ',t94, &
     &'NSTOP =  ',i9, /10x,                                             &
     &'HORIZONTAL ANGLE-INTERVAL FOR STROBOSCOPING THE VERTICAL',       &
     &' PHASESPACE PROJECTION',t94,'DPHIX = ',d16.10 /10x,              &
     &'VERTICAL ANGLE-INTERVAL FOR STROBOSCOPING THE HORIZONTAL',       &
     &' PHASESPACE PROJECTION',t94,'DPHIY = ',d16.10 /10x,              &
     &'SWITCH FOR THE WEIGHTING OF THE LINEAR FIT FOR THE ' ,           &
     &'DISTANCE IN PHASESPACE ',t96,'IWG = ',i4 /10x,                   &
     &'INTEGER PART FOR THE HORIZONTAL TUNE ',t96,'QX0 = ' ,f16.10 /10x,&
     &'INTEGER PART FOR THE VERTICAL TUNE ',t96,'QY0 = ' ,f16.10 )
10080 format( 10x,'SWITCH FOR THE QX-VALUE CLOSE TO AN HALF-INTEGER' ,  &
     &' ( INT => 1 ; HALF-INT => 0 )',t95,'IVOX = ',i4 /10x,            &
     &'SWITCH FOR THE QY-VALUE CLOSE TO AN HALF-INTEGER' ,              &
     &' ( INT => 1 ; HALF-INT => 0 )',t95,'IVOY = ',i4 /10x,            &
     &'Q-VALUES ARE CHECKED FOR RESONANCES UP TO ORDER',t95, 'IRES = ', &
     &i4 /10x,'A RESONANCE IS CONSIDERED TO BE STRONG WHEN THE Q-VALUES'&
     &, ' ARE CLOSER TO IT THAN',t95,'DRES = ',d16.10 /10x,             &
     &'SWITCH FOR FFT-RANGE ( IFH=0 => 0-1 ; IFH=1 => 0-.5 ' ,          &
     &'; IFH=2 => .5-1 )',t96,'IFH = ',i4 /10x,                         &
     &'Q-PEAKS OF THE FFT ARE CONSIDERED IF THEY ARE LARGER THAN' ,t95, &
     &'DFFT = ',d16.10 )
10090 format( 10x,                                                      &
     &'SWITCH FOR PRINTING THE DISTANCE IN PHASE SPACE' ,t95,'IDIS = ', &
     &i4 /10x,'SWITCH FOR PRINTING THE COORDINATES',t95,'ICOW = ',i4 /10&
     &x,'SWITCH FOR PRINTING THE STROBOSCOPED PHASESPACES',t95,         &
     &'ISTW = ',i4 /10x,'SWITCH FOR PRINTING THE FFT-SIGNALS',t95,      &
     &'IFFW = ',i4 ,i4 )
10100 format( 10x,'EVERY ISKIP VALUE IS USED FOR THE ANALYSIS' ,t94,    &
     &'ISKIP = ',i4 /10x,                                               &
     &'SWITCH OF COURANT SYNDER TRANSFORMATION (ICONV = 1 => OFF)' ,t93,&
     &' ICONV = ',i4 /10x,                                              &
     &'SWITCH FOR READING MAD DATA ( IMAD = 1 => MAD-DATA ' ,           &
     &'WITH LYAPUNOV ANALYSIS )',t95,'IMAD = ',i4 /10x,                 &
     &'SCALING OF MOMENTUM', ' WITH LYAPUNOV ANALYSIS',t95,'CMA1 = ',f16&
     &.10 /10x,'SCALING OF PATH-LENGTH', ' WITH LYAPUNOV ANALYSIS',t95, &
     &'CMA2 = ',f16.10 /10x,                                            &
     &'SWITCH FOR PRINTING OF THE POSTPROCESSING OUTPUT' ,              &
     &' NPRINT = ( 0 => OFF ; 1 => ON) ',t93,'NPRINT = ',i4 /10x,       &
     &'NUMBER OF BINARY FILES TO BE PROCESSED', ' ( 90 - [90-NDAFI+1] )'&
     &,t94,'NDAFI = ',i4 //)
10110 format(/10x,'ANALYSING THE INCREASE OF THE DISTANCE IN PHASE-' ,  &
     &'SPACE'/10x,53('-')/ //12x,'TURNS',10x,'DISTANCE',13x,            &
     &'SLOPE          RESIDUAL' /10x,63('-'))
10120 format(10x,i7,6x,d16.10,2(2x,f15.10))
10130 format(10x,63('-')//)
10140 format(//10x,'AVERAGED PHASE-ADVANCE' /10x,22('-')/ /10x,         &
     &'X-PHASE :  ',f14.10,'   +/_ ',f14.10 /10x,'Y-PHASE :  ',f14.10,  &
     &'   +/_ ',f14.10/ /10x,'S-PHASE :  ',f14.10,'   +/_ ',f14.10/ /10 &
     &x,'START-QX : ',f14.10,'   CHANGE IN X : ',d16.10 /10x,           &
     &'START-QY : ',f14.10,'   CHANGE IN Y : ',d16.10 /10x,'START-QS : '&
     &,f14.10,'   CHANGE IN S : ',d16.10// /10x,                        &
     &'THE AVERAGED PHASE-ADVANCES ARE CLOSER THEN ',d10.4,' TO ' ,     &
     &'THE FOLLOWING RESONANCES UP TO ',i3,' ORDER'/10x,98('-')/ /10x,  &
     &'NX * QX   +   NY * QY   -      P      =      DELTA'/10x, 52('-'))
10150 format(/10x,'WARNING ! X-PHASE MIGHT NOT BE PRECISE'/)
10160 format(/10x,'WARNING ! Y-PHASE MIGHT NOT BE PRECISE'//)
10170 format(12x,i2,11x,i3,7x,f8.1,9x,d10.4)
10180 format(//10x,'Q-VALUES FROM FFT-ROUTINE' /10x,25('-')/ /10x,      &
     &'THE ANALYSIS WAS DONE WITH ',i7,' ENTRIES.'/ /10x,               &
     &'THE FOLLOWING Q-PEAKS ARE LARGER THEN ',f8.4,' PERCENT.'/ /10x,  &
     &'PLANE          Q-VALUE            SIZE [%]'/10x,43('-'))
10190 format(12x,'X',7x,f14.10,5x,f14.10)
10200 format(12x,'Y',7x,f14.10,5x,f14.10)
10210 format(//10x,'MAXIMUM PEAK'/ /10x,'HORIZONTAL PLANE :  ',f14.10   &
     &/10x,'VERTICAL PLANE   :  ',f14.10/ /10x,'START-QX : ',f14.10,    &
     &'   CHANGE IN X : ',d16.10 /10x,'START-QY : ',f14.10,             &
     &'   CHANGE IN Y : ',d16.10// /10x,                                &
     &'THE MAXIMUM Q-PEAKS ARE CLOSER THEN ',d10.4,' TO ' ,             &
     &'THE FOLLOWING RESONANCES UP TO ',i3,' ORDER'/10x,96('-')/ /10x,  &
     &'NX * QX   +   NY * QY   -      P      =      DELTA'/10x, 52('-'))
10220 format(////10x,'CALCULATION OF THE AVERAGED EMITTANCES' /10x,38(  &
     &'-')// 24x,'START-EMITTANCE           START-AMPLITUDE'// 10x,     &
     &'HORIZONTAL   ',f16.10,9x,f16.10/ 10x,'VERTICAL     ',f16.10,9x,  &
     &f16.10// 14x,'PLANE',10x,'EMITTANCE',16x,'SMEAR',12x,'MAXIMUM',11 &
     &x, 'MINIMUM'/28x,'[PI*MM*MRAD]',15x,'[%]',15x,'[%]',15x, '[%]'/10 &
     &x,86('-')/ 10x,'HORIZONTAL',6x,f16.10,3(6x,f12.6)/ 10x,'VERTICAL',&
     &8x,f16.10,3(6x,f12.6)/ 10x,'SUM',13x,f16.10,3(6x,f12.6)/10x,86('-'&
     &)//)
10230 format(//10x,'INVARIANTS OF THE 4-DIMENSIONAL PHASE-SPACE' /10x,43&
     &('-')//)
10240 format(/10x,'WARNING ! CALCULATION OF THE HORIZONTAL INVARIANT' , &
     &' MIGHT NOT BE PRECISE'/10x,'ONLY ',i5,' ENTRIES COMPARED TO ' ,  &
     &i5,' ANGLE-INTERVALS !'/10x,'INCREASE THE VERTICAL ANGLE-INT' ,   &
     &'ERVAL <DPHIY>'/)
10250 format(/10x,'WARNING ! CALCULATION OF THE VERTICAL INVARIANT' ,   &
     &' MIGHT NOT BE PRECISE'/10x,'ONLY ',i5,' ENTRIES COMPARED TO ' ,  &
     &i5,' ANGLE-INTERVALS !'/10x,'INCREASE THE HORIZONTAL ANGLE-INT' , &
     &'ERVAL <DPHIX>'/)
10260 format(/10x,'THERE ARE ',i5,' ENTRIES FOR THE CALCULATION OF' ,   &
     &' THE HORIZONTAL INVARIANT GROUPED IN ',i5,' ANGLE-INTERVALS' /10 &
     &x,'--------- ',i5,' ENTRIES ----------------------' ,             &
     &'---- VERTICAL   -------------------- ',i5,' ANGLE-INTERVALS' //10&
     &x,'IF THE MOTION IS CLOSE TO FIXPOINTS THE NUMBER OF THOSE' ,     &
     &' ANGLE-INTERVALS WILL BE ONLY A SMALL FRACTION'/10x,             &
     &'OF THE TOTAL NUMBER OF ',i5,' INTERVALS.'/ /25x,                 &
     &'PERCENTAGE OF OCCUPIED INTERVALS     NUMBER OF ENTRIES ' ,       &
     &'PER OCCUPIED INTERVAL' /10x,'HORIZONTAL',16x,f10.6,30x,f12.6 /10 &
     &x,'VERTICAL  ',16x,f10.6,30x,f12.6/ //10x,                        &
     &'THE CALCULATED INVARIANTS ARE IN UNITS OF [PI*MM*MRAD]'/ /10x,   &
     &'HORIZONTAL',10x,f16.10 /10x,'VERTICAL  ',10x,f16.10//)
10270 format(////10x,'LINEARLY DECOUPLED INVARIANTS' /10x,35('-')/ 10x, &
     &'INITIAL EMITTANCE MODE I   :',f16.10/ 10x,                       &
     &'INITIAL EMITTANCE MODE II  :',f16.10/ 10x,                       &
     &'INITIAL EMITTANCE MODE III :',f16.10/ 10x,                       &
     &'INITIAL ANGLE     MODE I   :',f16.10/ 10x,                       &
     &'INITIAL ANGLE     MODE II  :',f16.10/ 10x,                       &
     &'INITIAL ANGLE     MODE III :',f16.10/ /10x,35('-')// 14x,'PLANE',&
     &10x,'EMITTANCE',14x,'4D-SMEAR',11x,'MAXIMUM',11x, 'MINIMUM'/28x,  &
     &'[PI*MM*MRAD]',15x,'[%]',15x,'[%]',15x, '[%]'/10x,86('-')/ 10x,   &
     &'HORIZONTAL',6x,f16.10,3(6x,f12.6)/ 10x,'VERTICAL',8x,f16.10,3    &
     &(6x,f12.6)/ 10x,'SUM',13x,f16.10,3(6x,f12.6)/10x,86('-')//)
10280 format(/10x,'Postprocessing took ',f12.3,' second(s)',            &
     &' of Execution Time'//131('-')//)
10290 format(//10x,'** ERROR ** ----- TRANSFORMATION MATRIX SINGULAR ' ,&
     &'(FILE : ',i2,') -----'//)
10300 format(//10x,'** ERROR ** ----- FILE :',i2,' WITH TRACKING ' ,    &
     &'DATA EMPTY OR CORRUPTED-----'/10x,'PROBLEM : ',a80//)
10310 format(//10x,'** ERROR ** ----- WEIGHTING OF DISTANCE IN PHASE' , &
     &' SPACE (FILE : ',i2,') NOT POSSIBLE-----'//)
10320 format(//10x,'** ERROR ** ----- INPUT DATA CORRUPTED' ,' (FILE : '&
     &,i2,') -----'//)
      end
      subroutine fft(ar,ai,m,n)
!---------------------------------------------------------------------
!      A(N) IS A COMPLEX ARRAY. THE INPUT IS A(N)=(X(N),0.0), WHERE
!      X(N) IS THE SEQUENCE TO FOURIER ANALYSE.
!      N=2**M.
!      THIS ROUTINE ONLY WORKS FOR N EQUAL TO A POWER OF TWO.
!      AFTER RETURN A(N)=(..,..) CONTAINS THE COEFFICIENTS OF THE FFT.
!      TO COMPUTE POWER SPECTRA DO   ...=ABS(A(N))
!
!      WRITTEN BY : RUI DILAO
!
!---------------------------------------------------------------------
      implicit none
      integer i,ip,j,k,l,le,le1,m,n,nm1,nv2
      double precision ar,ai,pi,tr,ti,ui,ur,uur,wr,wi
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
      dimension ar(n),ai(n)
      save
!-----------------------------------------------------------------------
      n=2**m
      nv2=n/2
      nm1=n-1
      j=1
      do 30 i=1,nm1
        if(i.gt.j)goto 10
        tr=ar(j)
        ti=ai(j)
        ar(j)=ar(i)
        ai(j)=ai(i)
        ar(i)=tr
        ai(i)=ti
   10   k=nv2
   20   if(k.ge.j)goto 30
        j=j-k
        k=k/2
        goto 20
   30 j=j+k
      pi=four*atan(one)
      do 50 l=1,m
        le=2**l
        le1=le/2
        ur=one
        ui=zero
        wr=cos(pi/le1)
        wi=-sin(pi/le1)
        do 50 j=1,le1
          do 40 i=j,n,le
            ip=i+le1
            tr=ar(ip)*ur-ai(ip)*ui
            ti=ar(ip)*ui+ai(ip)*ur
            ar(ip)=ar(i)-tr
            ai(ip)=ai(i)-ti
            ar(i)=ar(i)+tr
            ai(i)=ai(i)+ti
   40     continue
        uur=ur*wr-ui*wi
        ui=ur*wi+ui*wr
        ur=uur
   50 continue
      do 60 i=1,n
        ar(i)=ar(i)/n*2
        ai(i)=ai(i)/n*2
   60 continue
      return
      end
      subroutine caconv(a,b,c)
      implicit none
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
      double precision a,b,c
      save
!---------------------------------------------------------------------
      if(abs(b).gt.pieni.or.abs(c).gt.pieni) then
        a=atan2(b,c)
      else
        a=zero
      endif
      return
      end
      subroutine cphase(k,a,b,c,d,i,j,ie)
      implicit none
      integer i,ie,j,k
      double precision a,b,c,d,f,phase,tpi,dani
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
      common/phasecom/ phase(3,npos)
      common/invari/ dani(ninv+1)
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
!-----                                                                   -----
!-----    NEW BLOCKS PROVIDED FOR THE COLLIMATION STUDIES VIA SIXTRACK   -----
!-----                                                                   -----
!-----        G. ROBERT-DEMOLAIZE, October 27th, 2004                    -----
!-----                                                                   -----
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
      save
!---------------------------------------------------------------------
      tpi=8*atan(one)
      if(abs(b).gt.pieni.or.abs(c).gt.pieni) then
        f=atan2(b,c)
        ie=ie+1
        phase(k,ie)=f/tpi+d
        if(i.ne.1.and.-f.gt.pieni) phase(k,ie)=phase(k,ie)+one
        a=a+phase(k,ie)
      else
        j=1
      endif
      return
      end
      subroutine cinvar(a,b,c,d,j,e,xinv,invx)
      implicit none
      integer i,invx,j
      double precision a,b,c,d,phase,dani,e,xinv
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
      common/phasecom/ phase(3,npos)
      common/invari/ dani(ninv+1)
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
!-----                                                                   -----
!-----    NEW BLOCKS PROVIDED FOR THE COLLIMATION STUDIES VIA SIXTRACK   -----
!-----                                                                   -----
!-----        G. ROBERT-DEMOLAIZE, October 27th, 2004                    -----
!-----                                                                   -----
!-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----GRD-----
      dimension xinv(ninv),invx(ninv)
      save
!---------------------------------------------------------------------
      if(abs(a).le.b) then
        do 10 i=1,ninv
          if((c.ge.zero.and.c.ge.dani(i).and.c.lt.dani(i+1)).or. (c.lt. &
     &zero.and.d.ge.dani(i).and.d.lt.dani(i+1))) then
            j=j+1
            if(abs(xinv(i)).le.pieni) then
              xinv(i)=e
              invx(i)=1
            else
              xinv(i)=xinv(i)+e
              invx(i)=invx(i)+1
            endif
          endif
   10   continue
      endif
      return
      end
      subroutine sinpro(a,b,c,d,e)
      implicit none
      double precision a,b,c,d,e
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
      save
!---------------------------------------------------------------------
      if(abs(a).gt.pieni) then
        if(c.gt.pieni.and.b.gt.pieni) then
          c=sqrt(c/b)/a*100
        else
          c=zero
        endif
        d=(d-a)/a*100
        e=(e-a)/a*100
      else
        c=zero
        d=zero
        e=zero
      endif
      return
      end
      subroutine join
      implicit none
      integer i,ia,idummy,ierro,ifipa,ihalf,ilapa,ipa,ipa1,itopa,numl
      double precision alf0,bet0,clo,clop,dam,di0,dip0,dps,dummy,e0,pi, &
     &qwc,sigm,ta,x,y
      character*80 sixtit,commen
      character*8 cdate,ctime,progrm
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
      integer icode,idam,its6d
      double precision dpscor,sigcor
      common/corcom/dpscor,sigcor,icode,idam,its6d
      dimension bet0(2),alf0(2),ta(6,6)
      dimension qwc(3),clo(3),clop(3)
      dimension x(mpa,2),y(mpa,2),sigm(mpa),dps(mpa)
      dimension di0(2),dip0(2)
      save
!-----------------------------------------------------------------------
      pi=four*atan(one)
      sigcor=one
      dpscor=one
      read(90,end=60,iostat=ierro) sixtit,commen,cdate,ctime, progrm,   &
     &ifipa,ilapa,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo(1),clop(1),&
     &clo(2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0(2),dip0(2),     &
     &dummy,dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta(1,5),ta(1,6), ta  &
     &(2,1),ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6), ta(3,1),ta(3,2),ta &
     &(3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta(4,2),ta(4,3),ta(4,4),ta &
     &(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),ta(5,4),ta(5,5),ta(5,6), ta&
     &(6,1),ta(6,2),ta(6,3),ta(6,4),ta(6,5),ta(6,6)
      if(ierro.gt.0) then
        write(*,10010) 90,ierro
        goto 70
      endif
!-----------------------------------------------------------------------
!  OPTICAL PARAMETERS AT THE STARTING POINT
!-----------------------------------------------------------------------
      bet0(1)=ta(1,1)*ta(1,1)+ta(1,2)*ta(1,2)
      alf0(1)=-(ta(1,1)*ta(2,1)+ta(1,2)*ta(2,2))
      bet0(2)=ta(3,3)*ta(3,3)+ta(3,4)*ta(3,4)
      alf0(2)=-(ta(3,3)*ta(4,3)+ta(3,4)*ta(4,4))
      rewind 90
      ihalf=itopa/2
      if(icode.eq.1.or.icode.eq.2.or.icode.eq.4) idam=1
      if(icode.eq.3.or.icode.eq.5.or.icode.eq.6) idam=2
      if(icode.eq.7) idam=3
      do 50 i=1,ihalf
        read(91-i,end=50,iostat=ierro)
        if(ierro.gt.0) then
          write(*,10010) 91-i,ierro
          goto 50
        endif
        read(91-i-ihalf,end=50,iostat=ierro)
        if(ierro.gt.0) then
          write(*,10010) 91-i-ihalf,ierro
          goto 50
        endif
   10   read(91-i,end=20,iostat=ierro) ia,ipa,dummy, x(1,1),y(1,1),x    &
     &(1,2),y(1,2),sigm(1),dps(1),e0
        if(ierro.gt.0) then
          write(*,10010) 91-i,ierro
          goto 20
        endif
        x(1,1)=x(1,1)*c1e3
        y(1,1)=y(1,1)*c1e3
        x(1,2)=x(1,2)*c1e3
        y(1,2)=y(1,2)*c1e3
        sigm(1)=sigm(1)*c1e3
        e0=e0*c1e3
        read(91-i-ihalf,end=20,iostat=ierro) idummy,idummy,dummy, x     &
     &(2,1),y(2,1),x(2,2),y(2,2),sigm(2),dps(2)
        if(ierro.gt.0) then
          write(*,10010) 91-i-ihalf,ierro
          goto 20
        endif
        x(2,1)=x(2,1)*c1e3
        y(2,1)=y(2,1)*c1e3
        x(2,2)=x(2,2)*c1e3
        y(2,2)=y(2,2)*c1e3
        sigm(2)=sigm(2)*c1e3
        write(90,iostat=ierro) ia,ipa,dummy, x(1,1),y(1,1),x(1,2),y     &
     &(1,2),sigm(1),dps(1),e0, ipa+1,dam,x(2,1),y(2,1),x(2,2),y(2,2),   &
     &sigm(2),dps(2),e0
        if(ierro.ne.0) then
          write(*,10010) 90,ierro
          goto 20
        endif
        goto 10
   20   rewind 91-i
        rewind 91-i-ihalf
        write(91-i-ihalf,iostat=ierro)
        if(ierro.ne.0) then
          write(*,10010) 91-i-ihalf,ierro
        endif
        rewind 90
        read(91-i,iostat=ierro) sixtit,commen,cdate,ctime, progrm,ifipa,&
     &ilapa,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo(1),clop(1),clo   &
     &(2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0(2),dip0(2),dummy,  &
     &dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta(1,5),ta(1,6), ta(2,1),  &
     &ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6), ta(3,1),ta(3,2),ta       &
     &(3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta(4,2),ta(4,3),ta(4,4),   &
     &ta(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),ta(5,4),ta(5,5),ta       &
     &(5,6), ta(6,1),ta(6,2),ta(6,3),ta(6,4),ta(6,5),ta(6,6)
        if(ierro.gt.0) then
          write(*,10010) 91-i,ierro
          goto 40
        endif
        rewind 91-i
        progrm='MADTOSIX'
        write(91-i,iostat=ierro) sixtit,commen,cdate,ctime, progrm,2*i  &
     &-1,2*i,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo(1),clop(1),clo  &
     &(2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0(2),dip0(2),dummy,  &
     &dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta(1,5),ta(1,6), ta(2,1),  &
     &ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6), ta(3,1),ta(3,2),ta       &
     &(3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta(4,2),ta(4,3),ta(4,4),   &
     &ta(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),ta(5,4),ta(5,5),ta       &
     &(5,6), ta(6,1),ta(6,2),ta(6,3),ta(6,4),ta(6,5),ta(6,6), zero,     &
     &zero,zero,zero,sigcor,dpscor, zero,zero,zero,zero, zero,zero,     &
     &zero,zero,zero,zero,zero,zero,zero,zero, zero,zero,zero,zero,     &
     &zero,zero,zero,zero,zero,zero, zero,zero,zero,zero,zero,zero,     &
     &zero,zero,zero,zero, zero,zero,zero,zero,zero,zero,zero,zero,     &
     &zero,zero
        if(ierro.ne.0) then
          write(*,10010) 91-i,ierro
          goto 40
        endif
   30   read(90,end=40,iostat=ierro) ia,ipa,dam, x(1,1),y(1,1),x(1,2),y &
     &(1,2),sigm(1),dps(1),e0, ipa1,dam,x(2,1),y(2,1),x(2,2),y(2,2),    &
     &sigm(2),dps(2),e0
        if(ierro.gt.0) then
          write(*,10010) 90,ierro
          goto 40
        endif
        write(91-i,iostat=ierro) ia,ipa,dam, x(1,1),y(1,1),x(1,2),y     &
     &(1,2),sigm(1),dps(1),e0, ipa1,dam,x(2,1),y(2,1),x(2,2),y(2,2),    &
     &sigm(2),dps(2),e0
        if(ierro.ne.0) then
          write(*,10010) 91-i,ierro
          goto 40
        endif
        goto 30
   40   rewind 90
        rewind 91-i
   50 continue
      goto 70
   60 continue
      write(*,10000) 90
   70 continue
!-----------------------------------------------------------------------
      return
10000 format(//10x,'** ERROR IN JOIN** ----- INPUT DATA EMPTY' ,        &
     &' (FILE : ',i2,') -----'//)
10010 format(//10x,'** ERROR IN JOIN** ----- PROBLEMS WITH DATA ' ,     &
     &'FILE : ',i2,' ----- ERROR CODE : ',i10//)
      end
      subroutine sumpos
!-----------------------------------------------------------------------
!  SUBROUTINE TO SUMMARIZE THE RESULTS OF THE POSTPROCESSING
!-----------------------------------------------------------------------
      implicit none
      integer i,ierro,j
      double precision d,dlost
      character*4 ch
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
      dimension d(60)
      save
!-----------------------------------------------------------------------
      rewind 10
      do 10 i=1,1000
        ch=' '
        read(10,*,end=20,iostat=ierro) (d(j),j=1,60)
        if(ierro.gt.0) then
          write(*,*) '**ERROR**'
          write(*,*) 'CORRUPTED INPUT FILE FOR SUMMARY OF THE',         &
     &' POSTPROCESSING ERROR # : ',ierro
          return
        endif
        if(i.eq.1) write(*,10000)
        if(abs(d(2)).gt.pieni) ch='LOST'
        if(d(22).ge.d(23)) then
          dlost=d(23)
        else
          dlost=d(22)
        endif
        write(*,10010) nint(dlost),d(3),d(5),d(7),d(9),d(10),d(11),     &
     &d(12),nint(d(16)),nint(d(18)),d(19),d(21),ch,d(4),d(6),d(8),      &
     &d(13),nint(d(17)),d(20),d(25),d(14),d(15)
   10 continue
   20 rewind 10
      write(*,10020)
      do 30 i=1,1000
        read(10,*,end=40,iostat=ierro) (d(j),j=1,60)
        if(ierro.gt.0) then
          write(*,*) '**ERROR**'
          write(*,*) 'CORRUPTED INPUT FILE FOR SUMMARY OF THE',         &
     &' POSTPROCESSING ERROR # : ',ierro
          return
        endif
        write(*,10030) i,nint(d(60)),nint(d(59)),nint(d(60))*nint(d(24))
   30 continue
   40 continue
      write(*,10040)
!-----------------------------------------------------------------------
      return
10000 format(/131('-')/t10,'SUMMARY OF THE POSTPROCESSING' //t1,125(    &
     &'-'), /t1,'|',8x,'|',11x,'|',11x,'|',12x,'|',10x,                 &
     &'|NORMALIZED| SLOPE  |',13x,'|',10x,'|',21x,'|', /t1,             &
     &'|  TURN  |   LINEAR  |   BETA-   | AMPLITUDES | MOMENTUM |',     &
     &'PHASESPACE| OF THE |  NONLINEAR  |  NEAREST |',7x,'SMEAR OF',6x, &
     &'|', /t1,                                                         &
     &'| NUMBER |   TUNES   | FUNCTIONS |            | DEVIATION|',     &
     &' DISTANCE |DISTANCE|  DETUNING   | RESONANCE|    THE EMITTANCES' &
     &,3x,'|',/t1,125('-'), /t1,                                        &
     &'|        |           |     [M]   |     [MM]   |          |',     &
     &'          |        |             |     |ORD.|',                  &
     &'    [%]  |      [%]  |'/t1,125('-'))
10010 format(t1,'|',i8,'|X ',f9.5,'|X ',f9.4,'|X ',f10.6,'|',d10.4, '|',&
     &d10.4,'|',f8.4,'|X ',d11.5,'|X ',i3,'| ',i2,' |X ', f7.3,'|X+Y ', &
     &f7.3,'|' /t1,'|  ',a4,'  |Y ',f9.5,'|Y ',f9.4,'|Y ',f10.6,'|',10x,&
     &'|',10x,'|',8x,'|+/- ',d9.3,'|Y ',i3,'|    |Y ', f7.3,'|    ',7x, &
     &'|' /t1,'|',8x,'|QS ',f8.6,'|  ',9x,'|  ',10x,'|',10x, '|',10x,'|'&
     &,8x,'|Y ',d11.5,'|  ',3x,'|    |  ', 7x,'|    ',7x,'|' /t1,'|',8x,&
     &'|  ',9x,'|  ',9x,'|  ',10x,'|',10x, '|',10x,'|',8x,'|+/- ',d9.3, &
     &'|  ',3x,'|    |  ', 7x,'|    ',7x,'|'/t1,125('-'))
10020 format(/131('-')/t10,'RANDOM SETS USED' //                        &
     &'  CASE  |  # OF RANDOM SET  |  MAX. POSSIBLE SETS   |    ',      &
     &' SEED'/65('-'))
10030 format(3x,i2,13x,i2,19x,i2,13x,i8)
10040 format(65('-')//131('-'))
      end
      subroutine beamint(np,track,param,sigzs,bcu,ibb,ne,ibtyp,ibbc)
!-----------------------------------------------------------------------
!
!   Hirata's 6d beam-beam from BBC
!   SIXTRACK version courtesy Peter Leunissen
!   January 1999
!
!-----------------------------------------------------------------------
      implicit none
      integer ibb,ibbc,ibtyp,ne,np,nsli
      double precision alpha,bcu,calpha,cphi,f,param,phi,salpha,sigzs,  &
     &sphi,tphi,track,star
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
      dimension track(6,npart)
      dimension param(nele,4),bcu(nbb,12)
      dimension star(3,mbea)
      save
!-----------------------------------------------------------------------
      phi=param(ne,1)
      nsli=param(ne,2)
      alpha=param(ne,3)
      f=param(ne,4)/nsli
      sphi=sin(phi)
      cphi=cos(phi)
      tphi=tan(phi)
      salpha=sin(alpha)
      calpha=cos(alpha)
!     define slices
      call stsld(star,cphi,sphi,sigzs,nsli,calpha,salpha)
      call boost(np,sphi,cphi,tphi,salpha,calpha,track)
      call sbc(np,star,cphi,nsli,f,ibtyp,ibb,bcu,track,ibbc)
      call boosti(np,sphi,cphi,tphi,salpha,calpha,track)
      return
      end
      subroutine boost(np,sphi,cphi,tphi,salpha,calpha,track)
!-----------------------------------------------------------------------
!
!   Hirata's 6d beam-beam from BBC
!   SIXTRACK version courtesy Peter Leunissen
!   January 1999
!
! BOOST Boost Operation ********************************************
!    P,Q,E are all normalized by P0
!-----------------------------------------------------------------------
      implicit none
      integer i,np
      double precision calpha,cphi,h,h1x,h1y,h1z,hd1,salpha,sphi,tphi,  &
     &track,x1,y1
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
      dimension track(6,npart)
      save
!-----------------------------------------------------------------------
      do 1000 i=1,np
        h=track(6,i)+one-sqrt((one+track(6,i))**2-                      &
     &track(2,i)**2-track(4,i)**2)
        track(6,i)=track(6,i)-calpha*tphi*track(2,i)                    &
     &-track(4,i)*salpha*tphi+h*tphi*tphi
        track(2,i)=(track(2,i)-tphi*h*calpha)/cphi
        track(4,i)=(track(4,i)-tphi*h*salpha)/cphi
        hd1=sqrt((one+track(6,i))**2-track(2,i)**2-track(4,i)**2)
        h1x=track(2,i)/hd1
        h1y=track(4,i)/hd1
        h1z=one-(one+track(6,i))/hd1
        x1=calpha*tphi*track(5,i)+(one+calpha*sphi*h1x)*track(1,i)      &
     &+track(3,i)*salpha*sphi*h1x
        y1=salpha*tphi*track(5,i)+(one+salpha*sphi*h1y)*track(3,i)      &
     &+track(1,i)*calpha*sphi*h1y
        track(5,i)=track(5,i)/cphi+h1z*(sphi*calpha*track(1,i)          &
     &+sphi*salpha*track(3,i))
        track(1,i)=x1
        track(3,i)=y1
 1000 continue
      return
      end
      subroutine sbc(np,star,cphi,nsli,f,ibtyp,ibb,bcu,track,ibbc)
!-----------------------------------------------------------------------
!
!   Hirata's 6d beam-beam from BBC
!   SIXTRACK version courtesy Peter Leunissen
!   January 1999
!
!**SBC ***Synchro-Beam for headon collision**********************
!  call BBF  (table) disabled
!****************************************************************
!-----------------------------------------------------------------------
      implicit none
      integer i,ibb,ibbc,ibbc1,ibtyp,jsli,np,nsli
      double precision bbf0,bbfx,bbfy,bbgx,bbgy,bcu,costh,costhp,cphi,  &
     &dum,f,s,sepx,sepx0,sepy,sepy0,sfac,sinth,sinthp,sp,star,sx,       &
     &sy,track
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
      dimension track(6,npart),bcu(nbb,12)
      dimension star(3,mbea),dum(13)
      save
!-----------------------------------------------------------------------
      do 2000 jsli=1,nsli
        do 1000 i=1,np
          s=(track(5,i)-star(3,jsli))*half
          sp=s/cphi
          dum(1)=bcu(ibb,1)+two*bcu(ibb,4)*sp+bcu(ibb,6)*sp*sp
          dum(2)=bcu(ibb,2)+two*bcu(ibb,9)*sp+bcu(ibb,10)*sp*sp
          dum(3)=bcu(ibb,3)+(bcu(ibb,5)+bcu(ibb,7))*sp+                 &
     &bcu(ibb,8)*sp*sp
          dum(4)=dum(1)-dum(2)
          dum(5)=dum(4)*dum(4)+four*dum(3)*dum(3)
          if(ibbc.eq.1.and.(abs(dum(4)).gt.pieni.and.                   &
     &abs(dum(5)).gt.pieni)) then
            ibbc1=1
            dum(5)=sqrt(dum(5))
         else
            ibbc1=0
          endif
          sepx0=track(1,i)+track(2,i)*s-star(1,jsli)
          sepy0=track(3,i)+track(4,i)*s-star(2,jsli)
          if(ibbc1.eq.1) then
            sfac=one
            if(dum(4).lt.zero) sfac=-one
            dum(6)=sfac*dum(4)/dum(5)
            dum(7)=dum(1)+dum(2)
            costh=half*(one+dum(6))
            if(abs(costh).gt.pieni) then
              costh=sqrt(costh)
            else
              costh=zero
            endif
            sinth=half*(one-dum(6))
            if(abs(sinth).gt.pieni) then
              sinth=-sfac*sqrt(sinth)
            else
              sinth=zero
            endif
            if(dum(3).lt.zero) sinth=-sinth
            sy=sfac*dum(5)
            sx=(dum(7)+sy)*half
            sy=(dum(7)-sy)*half
            sepx=sepx0*costh+sepy0*sinth
            sepy=-sepx0*sinth+sepy0*costh
          else
            sx=dum(1)
            sy=dum(2)
            sepx=sepx0
            sepy=sepy0
          endif
          if(sx.gt.sy) then
            call bbf(sepx,sepy,sx,sy,bbfx,bbfy,bbgx,bbgy,ibtyp)
          else
            call bbf(sepy,sepx,sy,sx,bbfy,bbfx,bbgy,bbgx,ibtyp)
          endif
          bbfx=f*bbfx
          bbfy=f*bbfy
          bbgx=f*bbgx
          bbgy=f*bbgy
          if(ibbc1.eq.1) then
            dum(8)=two*(bcu(ibb,4)-bcu(ibb,9)+                          &
     &(bcu(ibb,6)-bcu(ibb,10))*sp)
            dum(9)=bcu(ibb,5)+bcu(ibb,7)+two*bcu(ibb,8)*sp
            dum(10)=(dum(4)*dum(8)+four*dum(3)*dum(9))/                 &
     &dum(5)/dum(5)/dum(5)
            dum(11)=sfac*(dum(8)/dum(5)-dum(4)*dum(10))
            dum(12)=bcu(ibb,4)+bcu(ibb,9)+(bcu(ibb,6)+bcu(ibb,10))*sp
            dum(13)=sfac*(dum(4)*dum(8)*half+two*dum(3)*dum(9))/dum(5)
            if(abs(costh).gt.pieni) then
              costhp=dum(11)/four/costh
            else
              costhp=zero
            endif
            if(abs(sinth).gt.pieni) then
              sinthp=-dum(11)/four/sinth
            else
              sinthp=zero
            endif
            track(6,i)=track(6,i)-                                      &
     &(bbfx*(costhp*sepx0+sinthp*sepy0)+                                &
     &bbfy*(-sinthp*sepx0+costhp*sepy0)+                                &
     &bbgx*(dum(12)+dum(13))+bbgy*(dum(12)-dum(13)))/                   &
     &cphi*half
            bbf0=bbfx
            bbfx=bbf0*costh-bbfy*sinth
            bbfy=bbf0*sinth+bbfy*costh
          else
            track(6,i)=track(6,i)-                                      &
     &(bbgx*(bcu(ibb,4)+bcu(ibb,6)*sp)+                                 &
     &bbgy*(bcu(ibb,9)+bcu(ibb,10)*sp))/cphi
          endif
          track(6,i)=track(6,i)-(bbfx*(track(2,i)-bbfx*half)+           &
     &bbfy*(track(4,i)-bbfy*half))*half
          track(1,i)=track(1,i)+s*bbfx
          track(2,i)=track(2,i)-bbfx
          track(3,i)=track(3,i)+s*bbfy
          track(4,i)=track(4,i)-bbfy
 1000   continue
 2000 continue
      return
      end
      subroutine boosti(np,sphi,cphi,tphi,salpha,calpha,track)
!-----------------------------------------------------------------------
!
!   Hirata's 6d beam-beam from BBC
!   SIXTRACK version courtesy Peter Leunissen
!   January 1999
!
! BOOSTI **************inverse boost *****************
!-----------------------------------------------------------------------
      implicit none
      integer i,np
      double precision calpha,cphi,det,h1,h1d,h1x,h1y,h1z,salpha,sphi,  &
     &tphi,track,x1,y1,z1
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
      dimension track(6,npart)
      save
!-----------------------------------------------------------------------
      do 1000 i=1,np
        h1d=sqrt((one+track(6,i))**2-track(2,i)**2-track(4,i)**2)
        h1x=track(2,i)/h1d
        h1y=track(4,i)/h1d
        h1z=one-(one+track(6,i))/h1d
        h1=(track(6,i)+one-sqrt((one+track(6,i))**2-                    &
     &track(2,i)**2-track(4,i)**2))*cphi*cphi
        det=one/cphi+tphi*(h1x*calpha+h1y*salpha-h1z*sphi)
        x1= track(1,i)*(one/cphi+salpha*(h1y-h1z*salpha*sphi)*tphi)     &
     &+track(3,i)*salpha*tphi*(-h1x+h1z*calpha*sphi)                    &
     &-track(5,i)*(calpha+h1y*calpha*salpha*sphi                        &
     &-h1x*salpha*salpha*sphi)*tphi
        y1= track(1,i)*calpha*tphi*(-h1y+h1z*salpha*sphi)               &
     &+track(3,i)*(one/cphi+calpha*(h1x-h1z*calpha*sphi)*tphi)          &
     &-track(5,i)*(salpha-h1y*calpha*calpha*sphi                        &
     &+h1x*calpha*salpha*sphi)*tphi
        z1=-track(1,i)*h1z*calpha*sphi-track(3,i)*h1z*salpha*sphi       &
     &+track(5,i)*(one+h1x*calpha*sphi+h1y*salpha*sphi)
        track(1,i)=x1/det
        track(3,i)=y1/det
        track(5,i)=z1/det
        track(6,i)=track(6,i)+calpha*sphi*track(2,i)                    &
     &+salpha*sphi*track(4,i)
        track(2,i)=(track(2,i)+calpha*sphi*h1)*cphi
        track(4,i)=(track(4,i)+salpha*sphi*h1)*cphi
 1000 continue
      return
      end
      subroutine bbf(sepx,sepy,sigxx,sigyy,bbfx,bbfy,bbgx,bbgy,ibtyp)
!-----------------------------------------------------------------------
!
!   Hirata's 6d beam-beam from BBC
!   SIXTRACK version courtesy Peter Leunissen
!   January 1999
!
!-----------------------------------------------------------------------
!**BBF   without using table ******************************************
! gives transverse (f_x and f_y) and longitudinal(g_x and g_y)
! beam-beam kicks except for the kinematical term (nr_e/\gamma)
! SIGXX is \Sigma
!**********************************************************************
      implicit none
      integer ibtyp
      double precision arg1x,arg1y,arg2x,arg2y,bbfx,bbfy,bbgx,bbgy,     &
     &comfac,comfac2,const,expfac,fac,fac2,sepx,sepy,sigxx,sigxy,sigyy, &
     &sqrpi2,wx1,wx2,wy1,wy2,x,xxyy
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
      data sqrpi2/3.544907701811032d0/
      save
!-----------------------------------------------------------------------
      if(sigxx.eq.sigyy) then
        x=sepx**2+sepy**2
        xxyy=sigxx+sigyy
        const=0.0d0
        if(abs(xxyy).gt.pieni) const=x/xxyy
        expfac=exp(-const)
        bbfx=0.0d0
        bbfy=0.0d0
        bbgx=0.0d0
        bbgy=0.0d0
        if(abs(x).gt.pieni) then
          bbfx=2.0d0*sepx*(1d0-expfac)/x
          bbfy=2.0d0*sepy*(1d0-expfac)/x
          comfac=-sepx*bbfx+sepy*bbfy
          comfac2=(abs(sigxx)+abs(sigyy))**2
          bbgx=(comfac+4d0*sepx**2*const/x*expfac)/(2d0*x)
          bbgy=(-comfac+4d0*sepy**2*const/x*expfac)/(2d0*x)
        endif
      else
        x=sepx**2/sigxx+sepy**2/sigyy
        fac2=2.d0*abs(sigxx-sigyy)
        fac=sqrt(fac2)
        const=sqrpi2/fac
        sigxy=sqrt(sigxx/sigyy)
        arg1x=abs(sepx/fac)
        arg1y=abs(sepy/fac)
        if(ibtyp.eq.0) call errf(arg1x,arg1y,wy1,wx1)
        if(ibtyp.eq.1) call wzsub(arg1x,arg1y,wy1,wx1)
        if(x.lt.100.d0) then
          expfac=exp(-x*0.5d0)
          arg2x=arg1x/sigxy
          arg2y=arg1y*sigxy
          if(ibtyp.eq.0) call errf(arg2x,arg2y,wy2,wx2)
          if(ibtyp.eq.1) call wzsub(arg2x,arg2y,wy2,wx2)
          bbfx=const*(wx1-expfac*wx2)
          bbfy=const*(wy1-expfac*wy2)
          if(sepx.lt.0) bbfx=-bbfx
          if(sepy.lt.0) bbfy=-bbfy
          comfac=sepx*bbfx+sepy*bbfy
          bbgx=-(comfac+2d0*(expfac/sigxy -1))/fac2
          bbgy= (comfac+2d0*(expfac*sigxy -1))/fac2
        else
          bbfx=const*wx1
          bbfy=const*wy1
          if(sepx.lt.0) bbfx=-bbfx
          if(sepy.lt.0) bbfy=-bbfy
          comfac=sepx*bbfx+sepy*bbfy
          bbgx=-(comfac-2d0)/fac2
          bbgy= -bbgx
        endif
      endif
      return
      end
      subroutine stsld(star,cphi,sphi,sigzs,nsli,calpha,salpha)
!-----------------------------------------------------------------------
!
!   Hirata's 6d beam-beam from BBC
!   SIXTRACK version courtesy Peter Leunissen
!   January 1999
!
!*******STSLD*********************************************************
!   makes longitudinal position of the strong slice for all slices
!*********************************************************************
!-----------------------------------------------------------------------
      implicit none
      integer i,nsli
      double precision bord,bord1,border,calpha,cphi,gauinv,pi,         &
     &salpha,sigz,sigzs,sphi,star,yy
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
      dimension star(3,mbea)
!-----------------------------------------------------------------------
      data border /8d0/
      save
!-----------------------------------------------------------------------
      pi=4d0*atan(1d0)
      sigz=sigzs/cphi
! DEFINE `STARRED' COORDINATES
!  BORD is longitudinal border star(3,mbea) is the barycenter of region
!  divided two borders.
      bord=+border
      do 101 i=nsli,1,-1
        yy=1d0/dble(nsli)*(i-1)
        if(i.ne.1) bord1=dble(gauinv(yy))
        if(i.eq.1) bord1=-border
        star(3,i)=(exp(-bord**2*half)-exp(-bord1**2*half))/             &
     &sqrt(2d0*pi)*dble(nsli)*sigz
        bord=bord1
        star(1,i)=star(3,i)*sphi*calpha
        star(2,i)=star(3,i)*sphi*salpha
 101  continue
      return
      end
      function gauinv(p0)
!GAUINV***********************************************
!  INVERSE OF (INTEGRATED) NORMAL DISTRIBUTION FUNCTION
!              1         X= Y
!     P(Y)=-----------* INTEGRAL EXP(-X**2/2) DX
!          SQRT(2*PI)    X= -INF
!     IF P(Y)=P0, THEN GAUINV(P0)=Y.
!        0 < P0 < 1 ,   -INF < Y < +INF
!  IF THIS ROUTINE IS USED TO CONVERT UNIFORM RANDOM NUMBERS TO
!  GAUSSIAN, MAXIMUM RELATIVE ERROR IN THE DISTRIBUTION FUNCTION
!  DP/DX=EXP(-X**2/2)/SQRT(2*PI) IS LESS THAN 0.640E-3 EVERYWHERE
!  IN THE RANGE  2**(-31) < P0 < 1-2**31.  (MINIMAX APPROXIMATION)
      implicit none
      double precision a0,a1,a2,a3,b0,b1,b2,b3,b4,c0,c1,c2,c3,c4,d0,d1, &
     &d2,d3,d4,e0,e1,e2,e3,e4,f0,f1,f2,gauinv,p,p0,p1,p2,pp1,q,qq2,qq3, &
     &qq4,qq5,t
!-----------------------------------------------------------------------
      data pp1/0.334624883253d0/, qq2/0.090230446775d0/,                &
     &qq3/0.049905685242d0/, qq4/0.027852994157d0/,                     &
     &qq5/0.015645650215d0/
      data a3/ 4.5585614d+01/, a2/ 2.1635544d+00/, a1/ 2.7724523d+00/,  &
     &a0/ 2.5050240d+00/,                                               &
     &b4/ 4.0314354d+02/, b3/-2.7713713d+02/, b2/ 7.9731883d+01/,       &
     &b1/-1.4946512d+01/, b0/ 2.2157257d+00/,                           &
     &c4/ 4.1394487d+03/, c3/-1.5585873d+03/, c2/ 2.4648581d+02/,       &
     &c1/-2.4719139d+01/, c0/ 2.4335936d+00/,                           &
     &d4/ 4.0895693d+04/, d3/-8.5400893d+03/, d2/ 7.4942805d+02/,       &
     &d1/-4.1028898d+01/, d0/ 2.6346872d+00/,                           &
     &e4/ 3.9399134d+05/, e3/-4.6004775d+04/, e2/ 2.2566998d+03/,       &
     &e1/-6.8317697d+01/, e0/ 2.8224654d+00/
      data f0/-8.1807613d-02/, f1/-2.8358733d+00/, f2/ 1.4902469d+00/
      save
!-----------------------------------------------------------------------
      p=p0-0.5d0
      p1=abs(p)
      if(p1.ge.pp1) goto 120
      p2=p**2
      gauinv=(((a3*p2+a2)*p2+a1)*p2+a0)*p
      return
 120  q=0.5d0-p1
      if(q.le.qq2) goto 140
      gauinv=(((b4*q+b3)*q+b2)*q+b1)*q+b0
      goto 200
 140  if(q.le.qq3) goto 150
      gauinv=(((c4*q+c3)*q+c2)*q+c1)*q+c0
      goto 200
 150  if(q.le.qq4) goto 160
      gauinv=(((d4*q+d3)*q+d2)*q+d1)*q+d0
      goto 200
 160  if(q.le.qq5) goto 170
      gauinv=(((e4*q+e3)*q+e2)*q+e1)*q+e0
      goto 200
 170  if(q.le.0d0) goto 900
      t=sqrt(-2d0*log(q))
      gauinv=t+f0+f1/(f2+t)
 200  if(p.lt.0d0) gauinv=-gauinv
      return
 900  write(*,910) p0
 910  format(' (FUNC.GAUINV) INVALID INPUT ARGUMENT ',1pd20.13)
!-----------------------------------------------------------------------
!--CLOSE(DATA FILES
      close(2)
      close(3)
      close(4)
      close(7)
      close(8)
      close(9)
      close(10)
      close(11)
      close(12)
      close(13)
      close(14)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)
      close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      close(25)
      close(26)
      close(27)
      close(32)
      close(33)
      close(34)
      close(59)
      close(60)
      close(61)
      close(62)
      close(63)
      close(64)
      close(65)
      close(66)
      close(67)
      close(68)
      close(69)
      close(70)
      close(71)
      close(72)
      close(73)
      close(74)
      close(75)
      close(76)
      close(77)
      close(78)
      close(79)
      close(80)
      close(81)
      close(82)
      close(83)
      close(84)
      close(85)
      close(86)
      close(87)
      close(88)
      close(89)
      close(90)
      close(98)
      close(99)
      stop
      end
      subroutine kerset(ercode,lgfile,limitm,limitr)
      implicit none
      integer i,kounte,l,lgfile,limitm,limitr,log,logf
      parameter(kounte = 27)
      character*6         ercode,   code(kounte)
      logical             mflag,    rflag
      integer             kntm(kounte),       kntr(kounte)
!-----------------------------------------------------------------------
      data      logf      /  0  /
      data      code(1), kntm(1), kntr(1)  / 'C204.1', 255, 255 /
      data      code(2), kntm(2), kntr(2)  / 'C204.2', 255, 255 /
      data      code(3), kntm(3), kntr(3)  / 'C204.3', 255, 255 /
      data      code(4), kntm(4), kntr(4)  / 'C205.1', 255, 255 /
      data      code(5), kntm(5), kntr(5)  / 'C205.2', 255, 255 /
      data      code(6), kntm(6), kntr(6)  / 'C305.1', 255, 255 /
      data      code(7), kntm(7), kntr(7)  / 'C308.1', 255, 255 /
      data      code(8), kntm(8), kntr(8)  / 'C312.1', 255, 255 /
      data      code(9), kntm(9), kntr(9)  / 'C313.1', 255, 255 /
      data      code(10),kntm(10),kntr(10) / 'C336.1', 255, 255 /
      data      code(11),kntm(11),kntr(11) / 'C337.1', 255, 255 /
      data      code(12),kntm(12),kntr(12) / 'C341.1', 255, 255 /
      data      code(13),kntm(13),kntr(13) / 'D103.1', 255, 255 /
      data      code(14),kntm(14),kntr(14) / 'D106.1', 255, 255 /
      data      code(15),kntm(15),kntr(15) / 'D209.1', 255, 255 /
      data      code(16),kntm(16),kntr(16) / 'D509.1', 255, 255 /
      data      code(17),kntm(17),kntr(17) / 'E100.1', 255, 255 /
      data      code(18),kntm(18),kntr(18) / 'E104.1', 255, 255 /
      data      code(19),kntm(19),kntr(19) / 'E105.1', 255, 255 /
      data      code(20),kntm(20),kntr(20) / 'E208.1', 255, 255 /
      data      code(21),kntm(21),kntr(21) / 'E208.2', 255, 255 /
      data      code(22),kntm(22),kntr(22) / 'F010.1', 255,   0 /
      data      code(23),kntm(23),kntr(23) / 'F011.1', 255,   0 /
      data      code(24),kntm(24),kntr(24) / 'F012.1', 255,   0 /
      data      code(25),kntm(25),kntr(25) / 'F406.1', 255,   0 /
      data      code(26),kntm(26),kntr(26) / 'G100.1', 255, 255 /
      data      code(27),kntm(27),kntr(27) / 'G100.2', 255, 255 /
      save
!-----------------------------------------------------------------------
      logf  =  lgfile
         l  =  0
      if(ercode .ne. ' ')  then
         do 10  l = 1, 6
            if(ercode(1:l) .eq. ercode)  goto 12
  10        continue
  12     continue
      endif
      do 14     i  =  1, kounte
         if(l .eq. 0)  goto 13
         if(code(i)(1:l) .ne. ercode(1:l))  goto 14
  13     if(limitm.ge.0) kntm(i)  =  limitm
         if(limitr.ge.0) kntr(i)  =  limitr
  14     continue
      return
      entry kermtr(ercode,log,mflag,rflag)
      log  =  logf
      do 20     i  =  1, kounte
         if(ercode .eq. code(i))  goto 21
  20     continue
      write(*,1000)  ercode
      call abend('KERNLIB Library Error                             ')
      return
  21  rflag  =  kntr(i) .ge. 1
      if(rflag  .and.  (kntr(i) .lt. 255))  kntr(i)  =  kntr(i) - 1
      mflag  =  kntm(i) .ge. 1
      if(mflag  .and.  (kntm(i) .lt. 255))  kntm(i)  =  kntm(i) - 1
      if(.not. rflag)  then
         if(logf .lt. 1)  then
            write(*,1001)  code(i)
         else
            write(logf,1001)  code(i)
         endif
      endif
      if(mflag .and. rflag)  then
         if(logf .lt. 1)  then
            write(*,1002)  code(i)
         else
            write(logf,1002)  code(i)
         endif
      endif
      return
1000  format(' KERNLIB LIBRARY ERROR. ' /                               &
     &' ERROR CODE ',a6,' NOT RECOGNIZED BY KERMTR',                    &
     &' ERROR MONITOR. RUN ABORTED.')
1001  format(/' ***** RUN TERMINATED BY CERN LIBRARY ERROR ',           &
     &'CONDITION ',a6)
1002  format(/' ***** CERN LIBRARY ERROR CONDITION ',a6)
      end
      subroutine rinv(n,a,idim,ir,ifail)
!-----------------------------------------------------------------------
!
!     ******************************************************************
!
!     REPLACES A BY ITS INVERSE.
!
!     (PARAMETERS AS FOR REQINV.)
!
!     CALLS ... RFACT, RFINV, F010PR, ABEND.
!
!     ******************************************************************
!-----------------------------------------------------------------------
      implicit none
      integer idim,ifail,ir,jfail,k,kprnt,n
      real t1,t2,t3,a,det,temp,s,c11,c12,c13,c21,c22,c23,c31,c32,c33
      character*6 name
      dimension ir(n),a(idim,n)
      data name/'RINV'/,kprnt/0/
      save
!-----------------------------------------------------------------------
!
!  TEST FOR PARAMETER ERRORS.
!
      if((n.lt.1).or.(n.gt.idim)) goto 7
!
!  TEST FOR N.LE.3.
!
      if(n.gt.3) goto 6
      ifail=0
      if(n.lt.3) goto 4
!
!  N=3 CASE.
!
!     COMPUTE COFACTORS.
      c11=a(2,2)*a(3,3)-a(2,3)*a(3,2)
      c12=a(2,3)*a(3,1)-a(2,1)*a(3,3)
      c13=a(2,1)*a(3,2)-a(2,2)*a(3,1)
      c21=a(3,2)*a(1,3)-a(3,3)*a(1,2)
      c22=a(3,3)*a(1,1)-a(3,1)*a(1,3)
      c23=a(3,1)*a(1,2)-a(3,2)*a(1,1)
      c31=a(1,2)*a(2,3)-a(1,3)*a(2,2)
      c32=a(1,3)*a(2,1)-a(1,1)*a(2,3)
      c33=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      t1=abs(a(1,1))
      t2=abs(a(2,1))
      t3=abs(a(3,1))
!
!     (SET TEMP=PIVOT AND DET=PIVOT*DET.)
      if(t1.ge.t2) goto 1
         if(t3.ge.t2) goto 2
!        (PIVOT IS A21)
            temp=a(2,1)
            det=c13*c32-c12*c33
            goto 3
    1 if(t3.ge.t1) goto 2
!     (PIVOT IS A11)
         temp=a(1,1)
         det=c22*c33-c23*c32
         goto 3
!     (PIVOT IS A31)
    2    temp=a(3,1)
         det=c23*c12-c22*c13
!
!     SET ELEMENTS OF INVERSE IN A.
    3 if(det.eq.0.) goto 8
      s=temp/det
      a(1,1)=s*c11
      a(1,2)=s*c21
      a(1,3)=s*c31
      a(2,1)=s*c12
      a(2,2)=s*c22
      a(2,3)=s*c32
      a(3,1)=s*c13
      a(3,2)=s*c23
      a(3,3)=s*c33
      return
!
    4 if(n.lt.2) goto 5
!
!  N=2 CASE BY CRAMERS RULE.
!
      det=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      if(det.eq.0.) goto 8
      s=1d0/det
      c11   =s*a(2,2)
      a(1,2)=-s*a(1,2)
      a(2,1)=-s*a(2,1)
      a(2,2)=s*a(1,1)
      a(1,1)=c11
      return
!
!  N=1 CASE.
!
    5 if(a(1,1).eq.0.) goto 8
      a(1,1)=1d0/a(1,1)
      return
!
!  N.GT.3 CASES.  FACTORIZE MATRIX AND INVERT.
!
    6 call rfact(n,a,idim,ir,ifail,det,jfail)
      if(ifail.ne.0) return
      call rfinv(n,a,idim,ir)
      return
!
!  ERROR EXITS.
!
    7 ifail=+1
      call f010pr(name,n,idim,k,kprnt)
      return
!
    8 ifail=-1
      return
!
      end
      subroutine dinv(n,a,idim,ir,ifail)
!-----------------------------------------------------------------------
!
!     ******************************************************************
!
!     REPLACES A BY ITS INVERSE.
!
!     (PARAMETERS AS FOR DEQINV.)
!
!     CALLS ... DFACT, DFINV, F010PR, ABEND.
!
!     ******************************************************************
!-----------------------------------------------------------------------
      implicit none
      integer idim,ifail,jfail,k,kprnt,n
      integer ir
      real t1,t2,t3
      double precision a,det,temp,s,c11,c12,c13,c21,c22,c23,c31,c32,c33
      character*6 name
      dimension ir(n),a(idim,n)
      data name/'DINV'/,kprnt/0/
      save
!-----------------------------------------------------------------------
!
!  TEST FOR PARAMETER ERRORS.
!
      if((n.lt.1).or.(n.gt.idim)) goto 7
!
!  TEST FOR N.LE.3.
!
      if(n.gt.3) goto 6
      ifail=0
      if(n.lt.3) goto 4
!
!  N=3 CASE.
!
!     COMPUTE COFACTORS.
      c11=a(2,2)*a(3,3)-a(2,3)*a(3,2)
      c12=a(2,3)*a(3,1)-a(2,1)*a(3,3)
      c13=a(2,1)*a(3,2)-a(2,2)*a(3,1)
      c21=a(3,2)*a(1,3)-a(3,3)*a(1,2)
      c22=a(3,3)*a(1,1)-a(3,1)*a(1,3)
      c23=a(3,1)*a(1,2)-a(3,2)*a(1,1)
      c31=a(1,2)*a(2,3)-a(1,3)*a(2,2)
      c32=a(1,3)*a(2,1)-a(1,1)*a(2,3)
      c33=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      t1=abs(sngl(a(1,1)))
      t2=abs(sngl(a(2,1)))
      t3=abs(sngl(a(3,1)))
!
!     (SET TEMP=PIVOT AND DET=PIVOT*DET.)
      if(t1.ge.t2) goto 1
         if(t3.ge.t2) goto 2
!        (PIVOT IS A21)
            temp=a(2,1)
            det=c13*c32-c12*c33
            goto 3
    1 if(t3.ge.t1) goto 2
!     (PIVOT IS A11)
         temp=a(1,1)
         det=c22*c33-c23*c32
         goto 3
!     (PIVOT IS A31)
    2    temp=a(3,1)
         det=c23*c12-c22*c13
!
!     SET ELEMENTS OF INVERSE IN A.
    3 if(det.eq.0d0) goto 8
      s=temp/det
      a(1,1)=s*c11
      a(1,2)=s*c21
      a(1,3)=s*c31
      a(2,1)=s*c12
      a(2,2)=s*c22
      a(2,3)=s*c32
      a(3,1)=s*c13
      a(3,2)=s*c23
      a(3,3)=s*c33
      return
!
    4 if(n.lt.2) goto 5
!
!  N=2 CASE BY CRAMERS RULE.
!
      det=a(1,1)*a(2,2)-a(1,2)*a(2,1)
      if(det.eq.0d0) goto 8
      s=1d0/det
      c11   =s*a(2,2)
      a(1,2)=-s*a(1,2)
      a(2,1)=-s*a(2,1)
      a(2,2)=s*a(1,1)
      a(1,1)=c11
      return
!
!  N=1 CASE.
!
    5 if(a(1,1).eq.0d0) goto 8
      a(1,1)=1d0/a(1,1)
      return
!
!  N.GT.3 CASES.  FACTORIZE MATRIX AND INVERT.
!
    6 call dfact(n,a,idim,ir,ifail,det,jfail)
      if(ifail.ne.0) return
      call dfinv(n,a,idim,ir)
      return
!
!  ERROR EXITS.
!
    7 ifail=+1
      call f010pr(name,n,idim,k,kprnt)
      return
!
    8 ifail=-1
      return
!
      end
      subroutine f010pr(name,n,idim,k,kprnt)
!     ******************************************************************
!
!     PRINT ROUTINE FOR PARAMETER ERRORS IN MATRIX SUBROUTINES $EQINV,
!     $EQN, $INV (WHERE $ IS A LETTER SPECIFYING THE ARITHMETIC TYPE).
!
!     NAME         (CHARACTER*6) NAME OF THE CALLING ROUTINE.
!
!     N,IDIM,K     PARAMETERS OF THE CALLING ROUTINE (WITH K=0 IF K IS
!                  NOT TO BE PRINTED).
!
!     KPRNT        PRINT FLAG FOR K (K IS NOT PRINTED IF KPRNT=0).
!
!     ******************************************************************
      implicit none
      integer idim,k,kprnt,lgfile,n
      character*6 name
      logical mflag,rflag
      save
!-----------------------------------------------------------------------
      call kermtr('F010.1',lgfile,mflag,rflag)
      if(mflag) then
         if(lgfile.eq.0)  then
            if(kprnt.eq.0) write(*,2000) name,n,idim
            if(kprnt.ne.0) write(*,2001) name,n,idim,k
         else
            if(kprnt.eq.0) write(lgfile,2000) name,n,idim
            if(kprnt.ne.0) write(lgfile,2001) name,n,idim,k
         endif
      endif
      if(.not. rflag)                                                   &
     &call abend('KERNLIB F010PR: '//name//                             &
     &'                            ')
      return
!
 2000 format( 7x, 'subroutine ', a6,' ... parameter',                   &
     &' error (n.lt.1 or n.gt.idim).',                                  &
     &6x,'n =', i4, 6x,'idim =', i4,'.')
 2001 format( 7x, 'subroutine ', a6,' ... parameter',                   &
     &' error (n.lt.1 or n.gt.idim or k.lt.1).',                        &
     &6x,'n =', i4, 6x,'idim =', i4, 6x,'k =', i4,'.')
      end
      subroutine rfact(n,a,idim,ir,ifail,det,jfail)
      implicit none
      integer i,idim,ifail,imposs,ipairf,ir,j,jfail,jm1,jover,jp1,      &
     &jrange,junder,k,l,n,normal,nxch
      real a,det,g1,g2,one,p,pivotf,q,sizef,t,tf,x,y,zero
      double precision s11,s12,dotf
      character*6 hname
      dimension ir(*),a(idim,*)
!      data      g1, g2              /  1.e-37,  1.e37  /
      data      g1, g2              /  1.0d-37,  1.0d37  /
      data      hname               /  ' RFACT'  /
      data      zero, one           /  0., 1.  /
      data      normal, imposs      /  0, -1  /
      data      jrange, jover, junder  /  0, +1, -1  /
      save
!-----------------------------------------------------------------------
      dotf(x,y,s11)  =  dble(x)*dble(y) + s11
      ipairf(j,k)  =  j*2**12 + k
      pivotf(x)    =  abs(x)
      sizef(x)     =  abs(x)
      if(idim .ge. n  .and.  n .gt. 0)  goto 110
         call tmprnt(hname,n,idim,0)
         return
 110  ifail  =  normal
      jfail  =  jrange
      nxch   =  0
      det    =  one
      do 144    j  =  1, n
 120     k  =  j
         p  =  pivotf(a(j,j))
         if(j .eq. n)  goto 122
         jp1  =  j+1
         do 121    i  =  jp1, n
            q  =  pivotf(a(i,j))
            if(q .le. p)  goto 121
               k  =  i
               p  =  q
 121        continue
         if(k .ne. j)  goto 123
 122     if(p .gt. 0.)  goto 130
            det    =  zero
            ifail  =  imposs
            jfail  =  jrange
            return
 123     do 124    l  =  1, n
            tf      =  a(j,l)
            a(j,l)  =  a(k,l)
            a(k,l)  =  tf
 124        continue
         nxch      =  nxch + 1
         ir(nxch)  =  ipairf(j,k)
 130     det     =  det * a(j,j)
         a(j,j)  =  one / a(j,j)
         t  =  sizef(det)
         if(t .lt. g1)  then
            det    =  zero
            if(jfail .eq. jrange)  jfail  =  junder
         elseif(t .gt. g2)  then
            det    =  one
            if(jfail .eq. jrange)  jfail  =  jover
         endif
         if(j .eq. n)  goto 144
         jm1  =  j-1
         jp1  =  j+1
         do 143   k  =  jp1, n
            s11  =  -a(j,k)
            s12  =  -a(k,j+1)
            if(j .eq. 1)  goto 142
            do 141  i  =  1, jm1
               s11  =  dotf(a(i,k),a(j,i),s11)
               s12  =  dotf(a(i,j+1),a(k,i),s12)
 141           continue
 142        a(j,k)    =  -s11 * a(j,j)
            a(k,j+1)  =  -dotf(a(j,j+1),a(k,j),s12)
 143        continue
 144     continue
 150  if(mod(nxch,2) .ne. 0)  det  =  -det
      if(jfail .ne. jrange)   det  =  zero
      ir(n)  =  nxch
      return
      end
      subroutine dfact(n,a,idim,ir,ifail,det,jfail)
      implicit none
      integer i,idim,ifail,imposs,ipairf,ir,j,jfail,jm1,jover,jp1,      &
     &jrange,junder,k,l,n,normal,nxch
      real g1,g2,p,pivotf,q,sizef,t
      double precision a,det,dotf,zero,one,s11,s12,x,y,tf
      character*6         hname
      dimension ir(*),a(idim,*)
!      data      g1, g2              /  1.e-37,  1.e37  /
      data      g1, g2              /  1.0d-37,  1.0d37  /
      data      hname               /  ' DFACT'  /
      data      zero, one           /  0.d0, 1.d0  /
      data      normal, imposs      /  0, -1  /
      data      jrange, jover, junder  /  0, +1, -1  /
      save
!-----------------------------------------------------------------------
      ipairf(j,k)  =  j*2**12 + k
      pivotf(x)    =  abs(sngl(x))
      sizef(x)     =  abs(sngl(x))
      dotf(x,y,s11)  =  x * y + s11
      if(idim .ge. n  .and.  n .gt. 0)  goto 110
      call tmprnt(hname,n,idim,0)
      return
 110  ifail  =  normal
      jfail  =  jrange
      nxch   =  0
      det    =  one
      do 144    j  =  1, n
 120     k  =  j
         p  =  pivotf(a(j,j))
         if(j .eq. n)  goto 122
         jp1  =  j+1
         do 121    i  =  jp1, n
            q  =  pivotf(a(i,j))
            if(q .le. p)  goto 121
               k  =  i
               p  =  q
 121        continue
         if(k .ne. j)  goto 123
 122     if(p .gt. 0.)  goto 130
            det    =  zero
            ifail  =  imposs
            jfail  =  jrange
            return
 123     do 124    l  =  1, n
            tf      =  a(j,l)
            a(j,l)  =  a(k,l)
            a(k,l)  =  tf
 124        continue
         nxch      =  nxch + 1
         ir(nxch)  =  ipairf(j,k)
 130     det     =  det * a(j,j)
         a(j,j)  =  one / a(j,j)
         t  =  sizef(det)
         if(t .lt. g1)  then
            det    =  zero
            if(jfail .eq. jrange)  jfail  =  junder
         elseif(t .gt. g2)  then
            det    =  one
            if(jfail .eq. jrange)  jfail  =  jover
         endif
         if(j .eq. n)  goto 144
         jm1  =  j-1
         jp1  =  j+1
         do 143   k  =  jp1, n
            s11  =  -a(j,k)
            s12  =  -a(k,j+1)
            if(j .eq. 1)  goto 142
            do 141  i  =  1, jm1
               s11  =  dotf(a(i,k),a(j,i),s11)
               s12  =  dotf(a(i,j+1),a(k,i),s12)
 141           continue
 142        a(j,k)    =  -s11 * a(j,j)
            a(k,j+1)  =  -dotf(a(j,j+1),a(k,j),s12)
 143        continue
 144     continue
 150  if(mod(nxch,2) .ne. 0)  det  =  -det
      if(jfail .ne. jrange)   det  =  zero
      ir(n)  =  nxch
      return
      end
      subroutine rfeqn(n,a,idim,ir,k,b)
      implicit none
      integer i,idim,ij,im1,ir,j,k,l,m,n,nm1,nmi,nmjp1,nxch
      real a,b,te,x,y
      double precision dotf,s21,s22
      character*6 hname
      dimension ir(*),a(idim,*),b(idim,*)
      data      hname               /  ' RFEQN'  /
      save
!-----------------------------------------------------------------------
      dotf(x,y,s21)  =  dble(x)*dble(y) + s21
      if(idim .ge. n  .and.  n .gt. 0  .and.  k .gt. 0)  goto 210
      call tmprnt(hname,n,idim,k)
      return
 210  nxch  =  ir(n)
      if(nxch .eq. 0)  goto 220
      do 212    m  =  1, nxch
         ij  =  ir(m)
         i   =  ij / 4096
         j   =  mod(ij,4096)
         do 211   l  =  1, k
            te      =  b(i,l)
            b(i,l)  =  b(j,l)
            b(j,l)  =  te
 211        continue
 212     continue
 220  do 221    l  =  1, k
         b(1,l)  =  a(1,1)*b(1,l)
 221     continue
      if(n .eq. 1)  goto 299
      do 243    l  =  1, k
         do 232   i  =  2, n
            im1  =  i-1
            s21  =  - b(i,l)
            do 231   j  =  1, im1
               s21  =  dotf(a(i,j),b(j,l),s21)
 231           continue
            b(i,l)  =  - a(i,i)*s21
 232        continue
         nm1  =  n-1
         do 242   i  =  1, nm1
            nmi  =  n-i
            s22  =  - b(nmi,l)
            do 241   j  =  1, i
               nmjp1  =  n - j+1
               s22    =  dotf(a(nmi,nmjp1),b(nmjp1,l),s22)
 241           continue
            b(nmi,l)  =  - s22
 242        continue
 243     continue
 299  continue
      return
      end
      subroutine dfeqn(n,a,idim,ir,k,b)
      implicit none
      integer i,idim,ij,im1,ir,j,k,l,m,n,nm1,nmi,nmjp1,nxch
      double precision a,b,x,y,te
      double precision dotf,s21,s22
      character*6 hname
      dimension ir(*),a(idim,*),b(idim,*)
      data      hname               /  ' DFEQN'  /
      save
!-----------------------------------------------------------------------
      dotf(x,y,s21)  =  x*y + s21
      if(idim .ge. n  .and.  n .gt. 0  .and.  k .gt. 0)  goto 210
      call tmprnt(hname,n,idim,k)
      return
 210  nxch  =  ir(n)
      if(nxch .eq. 0)  goto 220
      do 212    m  =  1, nxch
         ij  =  ir(m)
         i   =  ij / 4096
         j   =  mod(ij,4096)
         do 211   l  =  1, k
            te      =  b(i,l)
            b(i,l)  =  b(j,l)
            b(j,l)  =  te
 211        continue
 212     continue
 220  do 221    l  =  1, k
         b(1,l)  =  a(1,1)*b(1,l)
 221     continue
      if(n .eq. 1)  goto 299
      do 243    l  =  1, k
         do 232   i  =  2, n
            im1  =  i-1
            s21  =  - b(i,l)
            do 231   j  =  1, im1
               s21  =  dotf(a(i,j),b(j,l),s21)
 231           continue
            b(i,l)  =  - a(i,i)*s21
 232        continue
         nm1  =  n-1
         do 242   i  =  1, nm1
            nmi  =  n-i
            s22  =  - b(nmi,l)
            do 241   j  =  1, i
               nmjp1  =  n - j+1
               s22    =  dotf(a(nmi,nmjp1),b(nmjp1,l),s22)
 241           continue
            b(nmi,l)  =  - s22
 242        continue
 243     continue
 299  continue
      return
      end
      subroutine rfinv(n,a,idim,ir)
      implicit none
      integer i,idim,ij,im2,ir,j,k,m,n,nm1,nmi,nxch
      real a,ti,x,y
      double precision dotf,s31,s32,s33,s34,zero
      character*6 hname
      dimension ir(*),a(idim,*)
      data      zero      /  0.d0  /
      data      hname               /  ' RFINV'  /
      save
!-----------------------------------------------------------------------
      dotf(x,y,s31)  =  dble(x)*dble(y) + s31
      if(idim .ge. n  .and.  n .gt. 0)  goto 310
         call tmprnt(hname,n,idim,0)
         return
 310  if(n .eq. 1)  return
      a(2,1)  =  -a(2,2) * dotf(a(1,1),a(2,1),zero)
      a(1,2)  =  -a(1,2)
      if(n .eq. 2)  goto 330
      do 314    i  =  3, n
         im2  =  i-2
         do 312 j  =  1, im2
            s31  =  zero
            s32  =  a(j,i)
            do 311  k  =  j, im2
               s31  =  dotf(a(k,j),a(i,k),s31)
               s32  =  dotf(a(j,k+1),a(k+1,i),s32)
 311           continue
            a(i,j)  =  -a(i,i) * dotf(a(i-1,j),a(i,i-1),s31)
            a(j,i)  =  -s32
 312        continue
         a(i,i-1)  =  -a(i,i) * dotf(a(i-1,i-1),a(i,i-1),zero)
         a(i-1,i)  =  -a(i-1,i)
 314     continue
 330  nm1  =  n-1
      do 335   i  =  1, nm1
         nmi  =  n-i
         do 332   j  =  1, i
            s33  =  a(i,j)
            do 331   k  =  1, nmi
               s33  =  dotf(a(i+k,j),a(i,i+k),s33)
 331           continue
            a(i,j)  =  s33
 332        continue
         do 334   j  =  1, nmi
            s34  =  zero
            do 333   k  =  j, nmi
               s34  =  dotf(a(i+k,i+j),a(i,i+k),s34)
 333           continue
            a(i,i+j)  =  s34
 334        continue
 335     continue
      nxch  =  ir(n)
      if(nxch .eq. 0)  return
        do 342 m  =  1, nxch
         k   =  nxch - m+1
         ij  =  ir(k)
         i   =  ij / 4096
         j   =  mod(ij,4096)
         do 341  k  =  1, n
            ti      =  a(k,i)
            a(k,i)  =  a(k,j)
            a(k,j)  =  ti
 341        continue
 342     continue
      return
      end
      subroutine dfinv(n,a,idim,ir)
      implicit none
      integer i,idim,ij,im2,ir,j,k,m,n,nm1,nmi,nxch
      double precision a,dotf,s31,s32,s33,s34,ti,x,y,zero
      character*6 hname
      dimension ir(*),a(idim,*)
      data      hname               /  ' DFINV'  /
      data      zero      /  0.d0  /
      save
!-----------------------------------------------------------------------
      dotf(x,y,s31)  =  x*y + s31
      if(idim .ge. n  .and.  n .gt. 0)  goto 310
         call tmprnt(hname,n,idim,0)
         return
 310  if(n .eq. 1)  return
      a(2,1)  =  -a(2,2) * dotf(a(1,1),a(2,1),zero)
      a(1,2)  =  -a(1,2)
      if(n .eq. 2)  goto 330
      do 314    i  =  3, n
         im2  =  i-2
         do 312 j  =  1, im2
            s31  =  zero
            s32  =  a(j,i)
            do 311  k  =  j, im2
               s31  =  dotf(a(k,j),a(i,k),s31)
               s32  =  dotf(a(j,k+1),a(k+1,i),s32)
 311           continue
            a(i,j)  =  -a(i,i) * dotf(a(i-1,j),a(i,i-1),s31)
            a(j,i)  =  -s32
 312        continue
         a(i,i-1)  =  -a(i,i) * dotf(a(i-1,i-1),a(i,i-1),zero)
         a(i-1,i)  =  -a(i-1,i)
 314     continue
 330  nm1  =  n-1
      do 335   i  =  1, nm1
         nmi  =  n-i
         do 332   j  =  1, i
            s33  =  a(i,j)
            do 331   k  =  1, nmi
               s33  =  dotf(a(i+k,j),a(i,i+k),s33)
 331           continue
            a(i,j)  =  s33
 332        continue
         do 334   j  =  1, nmi
            s34  =  zero
            do 333   k  =  j, nmi
               s34  =  dotf(a(i+k,i+j),a(i,i+k),s34)
 333           continue
            a(i,i+j)  =  s34
 334        continue
 335     continue
      nxch  =  ir(n)
      if(nxch .eq. 0)  return
        do 342 m  =  1, nxch
         k   =  nxch - m+1
         ij  =  ir(k)
         i   =  ij / 4096
         j   =  mod(ij,4096)
         do 341  k  =  1, n
            ti      =  a(k,i)
            a(k,i)  =  a(k,j)
            a(k,j)  =  ti
 341        continue
 342     continue
      return
      end
      subroutine tmprnt(name,n,idim,k)
      implicit none
      integer idim,k,lgfile,n
      character*6 name
      logical mflag,rflag
      save
!-----------------------------------------------------------------------
      if(name(2:2) .eq. 'S') then
         call kermtr('F012.1',lgfile,mflag,rflag)
      else
         call kermtr('F011.1',lgfile,mflag,rflag)
      endif
      if(mflag) then
         if(lgfile .eq. 0) then
            if(name(3:6) .eq. 'FEQN') then
               write(*,1002) name, n, idim, k
            else
               write(*,1001) name, n, idim
            endif
         else
            if(name(3:6) .eq. 'FEQN') then
               write(lgfile,1002) name, n, idim, k
            else
               write(lgfile,1001) name, n, idim
            endif
         endif
      endif
      if(.not. rflag)                                                   &
     &call abend('KERNLIB TMPRNT: '//name//                             &
     &'                            ')
      return
      return
1001  format(7x,' parameter error in subroutine ', a6,                  &
     &' ... (n.lt.1 or idim.lt.n).',                                    &
     &5x,'n =', i4, 5x,'idim =', i4,'.')
1002  format(7x,' parameter error in subroutine ', a6,                  &
     &' ... (n.lt.1 or idim.lt.n or k.lt.1).',                          &
     &5x,'n =', i4, 5x,'idim =', i4, 5x,'k =', i4,'.')
      end
      subroutine lfit(x,y,l,key,a,b,e)
!-----------------------------------------------------------------------
!
!     TO FIT A STRAIGHT LINE    Y=A*X+B    TO L POINTS WITH ERROR E
!     SEE MENZEL , FORMULAS OF PHYSICS P.116
!     POINTS WITH Y=0 ARE IGNOERD IF KEY=0
!     L IS NO. OF POINTS
!
!-----------------------------------------------------------------------
      implicit none
      integer j,key,l
      real a,b,count,e,scartx,scarty,sumx,sumxx,sumxy,sumy,sumyy,x,xmed,&
     &y,ymed
      dimension x(1),y(1)
      save
!-----------------------------------------------------------------------
!
!     CALCULATE SUMS
!
!-----------------------------------------------------------------------
      if(l-2.lt.0) goto 25
      if(l-2.ge.0) goto 1
    1 count=0.0
      sumx=0.0
      sumy=0.0
      sumxy=0.0
      sumxx=0.0
      sumyy=0.0
      do 10 j=1,l
      if(y(j).eq.0..and.key.eq.0) goto 10
      sumx=sumx+x(j)
      sumy=sumy+y(j)
      count=count+1.0
   10 continue
      if(count.le.1.) goto 25
      ymed=sumy/count
      xmed=sumx/count
      do 20 j=1,l
      if(y(j).eq.0..and.key.eq.0) goto 20
      scartx=x(j)-xmed
      scarty=y(j)-ymed
      sumxy=sumxy+scartx   *scarty
      sumxx=sumxx+scartx   *scartx
      sumyy=sumyy+scarty   *scarty
   20 continue
!
!     FIT PARAMETERS
      if(sumxx.eq.0.) goto 25
      a=sumxy/sumxx
      b=ymed-a*xmed
      if(count.lt.3.) goto 101
      e=(sumyy-sumxy*a          )/(count-2.0)
      goto 100
!
!     ISUFFICIENT POINTS
   25 a=0.0
      b=0.0
  101 e=0.0
  100 return
      end
      subroutine lfitw(x,y,w,l,key,a,b,e)
!-----------------------------------------------------------------------
!
!     TO PERFORM A WEIGHTED STRAIGHT LINE FIT
!
!     FOR FORMULAE USED SEE MENZEL, FORMULAS OF PHYSICS P.116
!
!     FIT IS OF Y=AX+B , WITH S**2 ESTIMATOR E. WEIGHTS ARE IN W.
!     IF KEY=0, POINTS WITH Y=0 ARE IGNORED
!     L IS NO. OF POINTS
!
!-----------------------------------------------------------------------
      implicit none
      integer icnt,j,key,l
      real a,b,e,w,w2,w2x,w2x2,w2xy,w2y,w2y2,ww,wwf,wwfi,x,y
      dimension x(1),y(1),w(1)
      save
!-----------------------------------------------------------------------
!
!     CALCULATE SUMS
!
!-----------------------------------------------------------------------
      if(l.le.1) goto 1
      w2=0.
      w2x=0.
      w2y=0.
      w2xy=0.
      w2x2=0.
      w2y2=0.
      icnt=0
      do 2 j=1,l
      if(y(j).eq.0..and.key.eq.0) goto 2
      ww=w(j)*w(j)
      w2=ww+w2
      wwf=ww*x(j)
      w2x=wwf+w2x
      w2x2=wwf*x(j)+w2x2
      w2xy=wwf*y(j)+w2xy
      wwfi=ww*y(j)
      w2y=wwfi+w2y
      w2y2=wwfi*y(j)+w2y2
      icnt=icnt+1
    2 continue
!
!     FIT PARAMETERS
      a=(w2xy-w2x*w2y/w2)/(w2x2-w2x**2/w2)
      b=(w2y-a*w2x)/w2
      if(icnt.le.2) goto 3
      e=(w2y2-w2y**2/w2-(w2xy-w2x*w2y/w2)**2/(w2x2-w2x**2/w2))/(icnt-2)
      goto 4
!
!     ISUFFICIENT POINTS
    1 a=0.
      b=0.
    3 e=0.
    4 return
      end
!      logical function isnan(arg1,arg2)
      logical function myisnan(arg1,arg2)
      implicit none
      double precision arg1,arg2
!      isnan=.false.
!      if(arg1.ne.arg2) isnan=.true.
      myisnan=.false.
      if(arg1.ne.arg2) myisnan=.true.
      end
      subroutine datime(nd,nt)
      implicit none
!
!-    call datime (nd,nt)   returns integer date   nd = yymmdd
!-                                  integer time   nt =   hhmm
!     integer nd,nt,mm(3),nn(3)
!     call idate (mm(1),mm(2),mm(3))
!     call itime (nn)
      character*8 date
      character*10 time
      character*5 zone
      integer values(8),mm(3),nd,nt
      save
      call date_and_time(date,time,zone,values)
      mm(3)=mod(values(1),100)
!     mm(3) = mod (mm(3),100)
      mm(2)=values(3)
      mm(1)=values(2)
      nd = (mm(3)*100+mm(1))*100 + mm(2)
!     nt =            nn(1) *100 + nn(2)
      nt=values(5)*100+values(6)
      return
      end
      subroutine timest(r1)
      implicit none
      real r1,timestart,timenow
      common /mytimes/timestart
      logical start
      data start /.false./
      save
      if (.not.start) then
        start=.true.
        call cpu_time(timestart)
      endif
      return
      end
      subroutine timex(r1)
      implicit none
      real r1,timestart,timenow
      common /mytimes/timestart
      save
      call timest(0.0)
      call cpu_time(timenow)
      r1=timenow-timestart
      return
      end
      subroutine abend(cstring)
      implicit none
      integer lout
      common /crflags/lout
      integer i,lstring,istat
      character*50 cstring
      character*255 arecord
      save
      write(*,*)                                                        &
     &'SIXTRACK STOP/ABEND '//cstring
      stop
      end
