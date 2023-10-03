      program ax1

c  coded from 425895 AX-1 A Computing Program For Coupled Neutronics-Hydrodynamics Calculations
c  retreived from osti.gov

c  minor changes to lessen the old style fortran
c      replace beta( with bta( [some fortran libs have a beta function]
c      replace print and write( with a write to a file
c      removed unused statement numbers
c      where needed removed do numbers and replace with do / enddo
c      used generic names for functions (e.g. max vs amax,...)
c      made input free format
c
c      hacked the sense light and sense switch logic
c           on old IBM's the console had 16 toggle switches that allowed the
c           user to interact with their code
c
c           example:
c               if (sense switch 1)'switch down','switch up'
c           e.g.
c               if (sense switch 1)10,20

      include 'real8.h'

      logical sl1,sl2,sl3,sl4,ss1,ss2,ss3,ss4,ss5,ss6,
     *  dbgpr

      character*132 fname,problemname

      include 'cax1.h'

      fname = 'test.inp'

      if (iargc().gt.0) then
         call getarg(1,fname)
      endif

      call zeroary

      totke  = 0.0d0
      totien = 0.0d0
      time   = 0.0d0
      qp     = 0.0d0
      power  = 0.0d0
      alpha  = 0.0d0
      delta  = 0.0d0
      w      = 0.0d0

      ko = 8

      dbgpr = .false.

      third = 1.0d0 / 3.0d0

c  sense light hack

      sl1 = .false.
      sl2 = .false.
      sl3 = .false.
      sl4 = .false.

c  sense switch hack (pressed = .true.)

c  cause a pause which allowed IBM 704 user to change input

      ss1 = .false.
      ss1 = .true.

c  dumps output

      ss2 = .true.
      ss2 = .false.

c  dumps out r and enn

      ss3 = .true.
      ss3 = .false.

c  dumps out f

      ss4 = .true.
      ss4 = .false.

c  reduces ns4

      ss5 = .true.
      ss5 = .false.

c  print out input variables

      ss6 = .false.
      ss6 = .true.

      open (unit=ko,status='unknown',file='ax1.out')
      write(ko,9900)fname

      open (unit=1,status='old',file=fname)
      
      read(1,9900)problemname

      read(1,*)ircnbr

      if (ircnbr.le.0)goto 7020

      if (ss1) then
         goto 7015
      else
         goto 9266
      endif

 7015 write(ko,*)'pause 111 on ibm 704 allows input to be modified'
      goto 9266

 7020 read(1,*)icntrl
      read(1,*)alpha
      read(1,*)power
      read(1,*)imax

      if (imax.lt.0)imax  =  1
      if (imax.gt.40)imax = 40

      kcalc = 0
      r(1)  = 0.0d0

c  N.B. changed read here to read the material index along with
c       the data to make inputs easier to deal with

      do i=2,imax
         read(1,*)r(i),ro(i),f(i),u(i),theta(i),k(i)
      enddo

      read(1,*)igmax
      read(1,*)nmax,mmax

      if (mmax.gt.0) then
         do m=1,mmax
            read(1,*)(p(m,is),is=1,8)
            read(1,*)(mn(m,is),is=1,9)
         enddo
      endif

      read(1,*)(v(ig),ig=1,igmax),(anu(ig),ig=1,igmax)

      do n=1,nmax
         do ig=1,igmax
            read(1,*)anusig(ig,n),sig(ig,n),(sigma(ig,ih,n),ih=1,igmax)
         enddo
      enddo

      kp(1) = k(2)
      jmax  = 1

      do 7120 i=2,imax
      m = k(i)
      do 7090 j=1,jmax

      if (m - kp(j))7090,7100,7090

 7090 continue

      jmax     = jmax + 1
      kp(jmax) = m
      goto 7110

 7100 if ((i-2))7120,7110,7120

 7110 read(1,*)rolab(m),alph(m),bta(m),tau(m),acv(m),bcv(m)

 7120 continue

      read(1,*)epsr,epsa,eps1,eta1,eta2,eta3
      read(1,*)cvp,csc
      read(1,*)delt,dtmax
      read(1,*)np,npoff,npoffp,kcntrl
      read(1,*)vj,ok1,ok2,ptest,epsk,powngl

      aitct = 0.d0
      a(4)  = alpha
      eps   = epsa

      if (icntrl.ne.0) then
         eps  = epsr
         a(4) = r(imax)
      endif

      a(3) = a(4) + 10.0d0 * eps

      am(1)    = 1.0d0
      am(2)    = 0.666666667d0
      am(3)    = 0.166666667d0
      am(4)    = 0.333333333d0
      am(5)    = 0.833333333d0

      ambar(1) = 0.0d0
      ambar(2) = 0.833333333d0
      ambar(3) = 0.333333333d0
      ambar(4) = 0.166666667d0
      ambar(5) = 0.666666667d0

      b(1)     = 0.0d0
      b(2)     = 1.666666667d0
      b(3)     = 3.666666667d0
      b(4)     = 3.666666667d0
      b(5)     = 1.666666667d0

      write(ko,9900)problemname

      if (ss6) then
         goto 7155
      else
         goto 9000
      endif

 7155 write(ko,*)'icntrl ',icntrl
      write(ko,*)'alpha  ',alpha
      write(ko,*)'power  ',power
      write(ko,*)'imax   ',imax

c  changed output here to write material index and number

      do i=2,imax
         write(ko,9912)i,k(i),r(i),ro(i),f(i),u(i),theta(i)
      enddo

      write(ko,9921)igmax
      write(ko,9921)nmax,mmax

      if (mmax.gt.0) then
         do m=1,mmax
            write(ko,9931)(p(m,is),is=1,8)
            write(ko,9941)(mn(m,is),is=1,9)
         enddo
      endif

      write(ko,9911)(v(ig),ig=1,igmax),(anu(ig),ig=1,igmax)

      do n=1,nmax
         do ig=1,igmax
            write(ko,9911)anusig(ig,n),
     *                   sig(ig,n),
     *                   (sigma(ig,ih,n),ih=1,igmax)
         enddo
      enddo

      do j=1,jmax
         m = kp(j)
         write(ko,9911)rolab(m),alph(m),bta(m),tau(m),acv(m),bcv(m)
      enddo

      write(ko,9911)epsr,epsa,eps1,eta1,eta2,eta3
      write(ko,9911)cvp,csc
      write(ko,9911)delt,dtmax
      write(ko,9915)np,npoff,npoffp,kcntrl
      write(ko,9911)vj,ok1,ok2,ptest,epsk,powngl

      call flush(8)
      
 9000 time   = 0.0d0
      nh     = 0
      alphap = 0.0d0
      akeff  = 1.0
      nitmax = 300
      flag1  = 0.0d0
      ndmax  = 64
      nlmax  = 64
      ndump  = ndmax
      deltp  = delt

      if (ss1) then
         goto 9002
      else
         goto 9003
      endif

 9002 write(ko,*)'pause 11111 again allow input modification'

 9003 ns4    = 1
      ns4r   = 0

c  mixture code

      if (mmax.ne.0) then
         do m=1,mmax
            do ig=1,igmax
               do ih=ig,igmax
                  sum(ih) = 0.0d0
               enddo

               sum1 = 0.0d0
               sum2 = 0.0d0

               do 212 is=1,8
                  ma = mn(m,is)
                  do ih=ig,igmax
                     sum(ih) = sum(ih) + p(m,is) * sigma(ig,ih,ma)
                  enddo

                  sum1 = sum1 + p(m,is) * anusig(ig,ma)
                  sum2 = sum2 + p(m,is) * sig(ig,ma)

                  if (mn(m,is+2).eq.0)goto 213
 212           continue

 213           ma = mn(m,is+1)

               do ih=ig,igmax
                  sigma(ig,ih,ma) = sum(ih)
               enddo

               anusig(ig,ma) = sum1
               sig(ig,ma)    = sum2
            enddo
         enddo
      endif

c  preliminary calculation

 8009 do i=2,imax
         m       = k(i)
         rosn(i) = ro(i)
         rho(i)  = ro(i) / rolab(m)
      enddo

 9    do i=2,imax
         rbar(i)  = (r(i) + r(i-1)) / 2.0d0
         delta(i) = rbar(i) - r(i-1)
         s(i)     = delta(i) / rbar(i)
         t(i)     = (r(i)**3 - r(i-1)**3) / 3.0d0
      enddo

c  start big g loop

 8000 ig = igmax

c  added this.... code is running forever and crushing delt -> 0

      if (abs(delt).le.1.0e-6) then
         call flush(ko)
         close (unit=ko)
         stop 'delt to small'
      endif

 2    do 11 i=2,imax
      n = k(i)

      if (kcalc.le.0) then
         h(i) = delta(i) * (sig(ig,n) * rho(i) + alpha / v(ig))
      else
         h(i) = delta(i) * sig(ig,n) * rho(i)
      endif

      if (h(i).lt.0.0) then
         if (dbgpr)write(ko,*)'pause 14 reset h'
         h(i) = 0.0
      endif

      sum1 = 0.0

      if (aitct)3,3,40

 40   do ih=ig,igmax
         sum1 = sum1 + en(ih,i) * sigma(ig,ih,n)
      enddo

      if (kcalc)3,3,10

 10   so(i) = 4.0d0 * delta(i) * (anu(ig) * f(i) / akeff + 
     *        rho(i) * sum1)
      goto 11

 3    so(i) = 4.0d0 * delta(i) * (anu(ig) * f(i) + rho(i) * sum1)

 11   continue

      do j=1,3
         enn(imax,j) = 0.0d0
      enddo

      do 110 j=1,5
      amt    = am(j)
      ambart = ambar(j)
      bt     = b(j)

      if (j - 3)102,102,103

 102  i = imax
      assign 104 to iloop

 104  l  = i
      ii = i
      i  = i - 1

      if (i)900,110,106

 900  write(ko,*)'stop 13571'
      stop '13571'

 103  jk       = 6 - j

      enn(1,j) = enn(1,jk)
      i        = 1
      assign 105 to iloop

 105  l   = i
      i   = i + 1
      ii  = i

      if (i - imax)106,106,110

 106  bs       = bt * s(ii)

      enn(i,j) = (amt - bs - h(ii)) * enn(l,j) + so(ii) / 2.0d0

      if (j - 1)901,109,108

 901  write(ko,*)'stop 12345'
      stop '12345'

 108  enn(i,j) = enn(i,j) + (ambart + bs - h(ii)) * enn(l,j-1) -
     *           (ambart - bs + h(ii)) * enn(i,j-1) + so(ii) / 2.0d0

 109  enn(i,j) = enn(i,j) / (amt + bs + h(ii))

      if (enn(i,j))1000,1001,1001

 1000 enn(i,j) = 0.0

 1001 goto iloop,(104,105)

 110  continue

c  calculate total neutron flux

      do i=2,imax
         sum1 = 0.0d0
         do j=1,5
            sum2 = enn(i,j) + enn(i-1,j)

            goto (80,81,81,81,80),j

  80        sum2 = sum2 / 2.0d0
  81        sum1  = sum1 + sum2
         enddo

         sum1     = sum1 / 8.0d0
         en(ig,i) = sum1
      enddo

      ig = ig - 1

      if (ig)300,300,2

 300  febarp = 0.0d0
      ffbarp = 0.0d0

      do i=2,imax
         sum1    = 0.0d0
         ennn(i) = 0.0d0
         do ig=1,igmax
           sum1    = sum1 + en(ig,i)
           ennn(i) = ennn(i) + en(ig,i) / v(ig)
         enddo
         wn(i) = sum1 * t(i)
      enddo

      do i=2,imax
         ffbarp = ffbarp + wn(i) * f(i)
         febarp = febarp + wn(i) * e(i)
      enddo
      
      do i=2,imax
         sum1 = 0.0d0
         sum2 = 0.0d0
         n    = k(i)

         do ig=1,igmax
            sum1 = sum1 + anusig(ig,n) * en(ig,i)
            sum2 = sum2 + sigma(ig,ig,n) * en(ig,i)
         enddo

         f(i) = sum1 * rho(i)
         e(i) = sum2 * rho(i)
      enddo

      febar  = 0.0d0
      ffbar  = 0.0d0
      fenbar = 0.0d0

      do i=2,imax
         febar  = febar + wn(i) * e(i)
         ffbar  = ffbar + wn(i) * f(i)
         fenbar = fenbar + wn(i) * ennn(i)
      enddo

      if (aitct - 3.0)321,321,305

 305  if (kcalc)319,319,311

 311  do i=1,3
         ak(i) = ak(i + 1)
      enddo

      akeff = akeff * ffbar * febar / (ffbarp * febarp)
      ak(4) = akeff
      ffake = 0.0d0

      do i=1,3
         if ((abs(ak(i+1) - ak(i)) - epsk).ge.0.0d0) then
            ffake = 1.0d0
         endif
      enddo

      if (ffake)8000,8999,8000

 8999 write(ko,9900)problemname

      write(ko,9942)akeff

      kcntrl = 0
      kcalc  = 0
      goto 6802

 319  do i=1,3
         a(i) = a(i + 1)
      enddo

      if (icntrl)8020,8015,8020

 8015 alpha = alpha + (ffbar + febar - ffbarp - febarp) / fenbar

      eps   = epsa
      a(4)  = alpha
      goto 321

 8020 z   = (ffbar + febar) / (ffbarp + febarp)

      eps = epsr

      do i=2,imax
         r(i) = r(i) / z
      enddo

      a(4) = r(imax)

 321  aitct = aitct + 1.0d0

      ffake = 0.0d0
      do i=1,3
         if ((abs(a(i+1)-a(i))-eps).ge.0.0d0)ffake = 1.0d0
      enddo

      if (ffake)339,915,339
 
 339  if (icntrl)9,8000,9

c  print routines

 915  if (ss4) then
         goto 331
      else
         goto 503
      endif

 331  write(ko,9900)problemname
      write(ko,9980)

      do i=2,imax
         write(ko,9981)f(i),(en(ig,i),ig=1,igmax)
      enddo

 503  if (ss3) then
         goto 502
      else
         goto 9010
      endif

 502  write(ko,9900)problemname
      write(ko,9985)
      write(ko,9911)(r(i),(enn(i,j),j=1,5),i=1,imax)

 9010 fbar  = 0.0d0

      do i=2,imax
         fbar = fbar + t(i) * f(i)
      enddo

      if (nh)6800,6800,9014

 6800 alpha0 = abs(alpha)

      if (kcntrl)6802,6803,6801

 6801 kcalc = 1

      akeff = 1.0d0
      ak(1) = 1.0d0
      ak(2) = 4.0d0
      ak(3) = 1.0d0
      ak(4) = 2.0d0
      goto 8000

 6803 write(ko,9900)problemname

 6802 rl(1)   = 0.0d0

c  original listing had a call to cubertf function
c  replaced with ()**(1./3.)

      do i=2,imax
         arg   = rl(i-1)**3 + ro(i) * (r(i)**3 - r(i-1)**3)
         rl(i) = arg**third
      enddo

      rl(imax + 1) = 2.0d0 * rl(imax) - rl(imax - 1)

 6820 if ((alpha0 * delt) - (4.0d0 * eta2))6830,6822,6822

 6822 delt = 0.50d0 * delt
      if (dbgpr)write(ko,*)'halve delt initially'
      goto 6820

 6830 deltp  = delt
      totke  = 0.0d0
      totien = 0.0d0
   
      do i=2,imax
         m        = k(i)

         arg      = alph(m) * ro(i) + bta(m) * theta(i) + tau(m)
         hp(i)    = max(0.0d0,arg)

         he0(i)   = tau(m) / ro(i) - alph(m) * log(ro(i))

         he(i)    = acv(m) * theta(i) + 0.50d0 * bcv(m) * theta(i)**2

         rke      = 0.25d0 * (u(i)**2 + u(i-1)**2)

         hmass(i) = rl(i)**3 - rl(i-1)**3

         totke    = totke + hmass(i) * rke

         totien   = totien + hmass(i) * he(i)
      enddo

      hp(imax + 1) = -hp(imax)
      qprime       = -1.0d0
      q            = totke + totien
      totke        = 4.18879d0 * totke
      totien       = 4.18879d0 * totien

      write(ko,9943)totke,totien

      if (icntrl)6990,9050,6990

 6990 write(ko,9944)r(imax)

      write(ko,9986)

      icntrl = 0
      goto 9050

 9014 alpha0 = max(abs(alpha),alpha0)

      if (alpha)9018,9018,9017

 9017 flag1 = 1.0d0

 9018 z     = abs(alphap - alpha) / (alpha0 + 3.0d0 * epsa)
      if (z - eta3)9015,9015,9020

 9015 if (ss5) then
         goto 9030
      else
         goto 9016
      endif

 9016 ns4 = ns4 + 1
      goto 9050

 9020 if ((3.0d0 * eta3) - z)9022,9022,9027

 9022 if (1 - ns4)9023,9024,9024

 9023 ns4 = ns4 - 1
      goto 9040

 9024 if (alpha)9050,9025,9025

 9025 if (sl3) then
         sl3 = .false.
         if (dbgpr)write(ko,*)'sl3 off'
      else
         sl3 = .true.
         if (dbgpr)write(ko,*)'sl3 turned on'
      endif
      goto 9050

 9027 if (ss5) then
         goto 9030
      else
         goto 9050
      endif

 9030 if (1-ns4)9023,9050,9050

 9040 if ((6.0d0 * eta3) - z)9045,9045,9050

 9045 ns4 = 1

 9050 if (ss6) then
         goto 9051
      else
         goto 9052
      endif

 9051 if (dbgpr)write(ko,*)'pause 66666'

 9052 nh = nh + 1
      if (ns4r)9054,9054,9056

 9054 qp = 4.18879 * q

      write(ko,9911)time,qp,power,alpha,delt,w
      write(33,9911)time,qp,power,alpha,delt,w

 9056 w     = 0.0
      time  = time + delt
      z     = alpha * deltp
      power = exp(z) * power

      qbar  = power * delt / (12.56637d0 * fbar)

      if (q - qprime)9061,9061,9066

 9061 if (alpha)9062,9066,9066

 9062 if (flag1)9066,9066,9065

 9065 ns4    = 30000

 9066 qprime = q

c  enter i loop

      do 9200 i=2,imax
      u(i) = u(i) - deltp * r(i)**2 * (hp(i + 1) - hp(i)) /
     *       (0.5d0 * rl(i)**2 * (rl(i+1) - rl(i-1)))

      r(i) = r(i) + u(i) * delt

      rhot = hmass(i) / (r(i)**3 - r(i-1)**3)

      delv = 1.0d0 / rhot - 1.0d0 / ro(i)

      if ((rhot * abs(delv)) - 0.1d0)9070,9070,9068

 9068 ircnbr = ircnbr
      if (dbgpr)write(ko,*)'pause 50 @ 9068'

      if (dbgpr) then
         write(ko,*)'pause @ 9069'
         write(ko,*)'trouble called dump ',ircnbr
         write(ko,*)'halve delt, rho delv large'
      endif
      goto 9290

 9070 delq = f(i) * qbar / rosn(i)
      q    = q + delq * hmass(i)
      delr = r(i) - r(i-1)

      if (delr)9080,9080,9082

 9080 if (dbgpr)write(ko,*)'pause 60 @ 9080'
      goto 9068

 9082 vp = 0.0

      if (delv)9120,9124,9124

 9120 vp = cvp * rhot * (rhot * delv * delr / delt)**2

      if (nh - 1)9123,9123,9124

 9123 hp(i)  = hp(i) + vp

 9124 hpt    = hp(i)
      thet   = theta(i)
      m      = k(i)
      nit    = 0

 9130 dele   = delq - 0.50d0 * (hpt + hp(i)) * delv

      z      = dele + delv * (tau(m) + 
     *         alph(m) * 0.5d0 * (rhot + ro(i)))

      arg = (theta(i) + 2.0d0 * z / (2.0d0 * acv(m) + 
     *       bcv(m) * (thet + theta(i))))
      thet   = max(0.0d0,arg)

      trm    = alph(m) * rhot + bta(m) * thet + tau(m)
      pstar  = max(0.0d0,trm) + vp

      arg = abs(pstar - hpt) / (abs(pstar) + eps1) 

      if (arg - eta1)9180,9180,9151

 9151 nit = nit + 1
      if (nit - nitmax)9160,9170,9170

 9160 hpt = pstar
      goto 9130

 9170 if (dbgpr)write(ko,*)'pause 11'

 9180 hp(i)    = pstar
      he(i)    = he(i) + dele
      theta(i) = thet

      wr    = csc * abs(he(i)) * delt**2 / delr**2 + 
     *        4.0d0 * cvp * rhot * abs(delv)
      w     = max(wr,w)
      ro(i) = rhot

 9200 continue

c  end of i loop

      hp(imax + 1) = -hp(imax)

      if (ns4 - 1)9210,9210,9202

 9202 if (alpha)9210,9210,9203

 9203 hpbar = 0.0
      do i=2,imax
         hpbar = max(hpbar,hp(i))
      enddo

      if (hpbar - ptest)9210,9210,9205

 9205 pbar = 0.0
      do i=2,imax
         pbar = pbar + hp(i) * t(i)
      enddo

      pbar  = pbar * 12.56637d0
      s4r   = ns4
      const = vj * delt**2 * s4r**2 * pbar

      if (const - ok1)9210,9213,9213

 9213 npoff = npoffp

      if (const - ok2)9209,9208,9208

 9208 ns4 = 1
      goto 9210

 9209 if (mod(ns4,2))9211,9212,9211

 9211 ns4 = (ns4 - 1) / 2
      goto 9210

 9212 ns4 = ns4 / 2

 9210 deltp = delt

      if (sl1) then
         sl1 = .false.
         if (dbgpr)write(ko,*)'sl1 off'
         goto 5999
      else
         sl1 = .true.
         if (dbgpr)write(ko,*)'sl1 on'
         goto 9220
      endif

 9220 if (sl2) then
         sl2 = .false.
         if (dbgpr)write(ko,*)'sl2 off'
         goto 9230
      else
         sl2 = .true.
         if (dbgpr)write(ko,*)'sl2 on'
         goto 9230
      endif

 9230 if(ss1) then
         goto 5999
      else
         goto 9240
      endif

 9240 if (mod(nh,np))5999,5999,9241

 9241 if (mod(nh,npoff))9242,9242,9255

 9242 ldont = 7
      goto 6000

 5999 ldont = 0

 6000 totke  = 0.0d0
      totien = 0.0d0
      errlcl = 0.0d0

      do i=2,imax
         m       = k(i)

         rie     = he0(i) + alph(m) * log(ro(i)) - tau(m) / ro(i) + 
     *             acv(m) * theta(i) + 0.50d0 * bcv(m) * theta(i)**2

         errlcl  = max(errlcl,abs(rie - he(i)))

         rke     = 0.25d0 * (u(i)**2 + u(i-1)**2)

         totke   = totke + hmass(i) * rke

         totien  = totien + hmass(i) * rie
      enddo

      check   = (q - totke - totien) / q
      qp      = 4.18879d0 * q
      totke   = 4.18879d0 * totke

      if (ldont)6055,6050,6055

 6050 write(ko,9911)time,qp,power,alpha,delt,w
      write(33,9911)time,qp,power,alpha,delt,w
      write(ko,9982)
      write(ko,9911)qp,totke,check,errlcl
      write(ko,9983)

ccc      write(ko,9912)(ro(i),r(i),u(i),hp(i),he0(i),theta(i),i=2,imax)
      do i=2,imax
         write(ko,9913)i,ro(i),r(i),u(i),hp(i),he0(i),theta(i)
      enddo
      write(ko,9900)problemname
      write(ko,9986)

c  since we only dump to file i added this to keep
c  from double writing data

      goto 9255
     
 6055 write(ko,9911)time,qp,power,alpha,delt,w
      write(33,9911)time,qp,power,alpha,delt,w
      write(ko,9982)
      write(ko,9911)qp,totke,check,errlcl
      write(ko,9983)

      do i=2,imax
         write(ko,9913)i,ro(i),r(i),u(i),hp(i),he0(i),theta(i)
      enddo

      write(ko,9900)problemname
      write(ko,9986)

 9255 if (sl3) then
         sl3 = .false.
         if (dbgpr)write(ko,*)'sl3 off'
         goto 9283
      else
         sl3 = .true.
         if (dbgpr)write(ko,*)'sl3 on'
         goto 9256
      endif

 9256 if (ndump)9257,9257,9258

 9257 ircnbr = ircnbr + 1
      ndump  = ndmax
      goto 9263

 9258 ndump = ndump - 1

      if (ss2) then
         goto 9264
      else
         goto 9267
      endif
 
 9264 ircnbr = ircnbr + 1

      if (dbgpr)write(ko,*)'pause 22222 @ 9265'

 9263 if (dbgpr)write(ko,*)'pause @ 9263'

 9266 write(ko,9984)ircnbr

 9267 if (w - 0.3d0)9268,9285,9285

 9268 if ((alpha * delt) - (4.0d0 * eta2))9269,9284,9284

 9269 if (w - 0.03d0)9270,9310,9310

 9270 nl = nl - 1

      if (nl)9274,9274,9320

 9274 if ((alpha * delt) - eta2)9278,9320,9320

 9278 if ((2.0d0 * delt) - dtmax)9280,9280,9320

 9280 if (mod(nh,2))9320,9281,9320

 9281 if (sl2) then
         sl2 = .false.
         if (dbgpr)write(ko,*)'sl2 off'
      else
         sl2 = .true.
         if (dbgpr)write(ko,*)'sl2 on'
      endif

      deltp = 1.5d0 * delt
      delt  = 2.0d0 * delt
      nh    = nh / 2
      ns4   = (ns4 + 1) / 2

      if (dbgpr)write(ko,*)'double delt'

      goto 9310

 9283 if (dbgpr)write(ko,*)'half delt sense light 3 on'
      goto 9290

 9284 if (dbgpr)write(ko,*)'half delt alphadelt large'
      goto 9290

 9285 if (dbgpr)write(ko,*)'half delt w large'

 9290 if (sl1) then
         sl1 = .false.
         if (dbgpr)write(ko,*)'sl1 off'
      else
         sl1 = .true.
         if (dbgpr)write(ko,*)'sl1 on'
      endif

      deltp = 0.75d0 * delt
      delt  = 0.50d0 * delt
      nh    = 2 * nh

 9310 nl    = nlmax

 9320 ns4r  = ns4r + 1

      if (ns4r - ns4)9050,9330,9330

 9330 ns4r = 0

      if (sl4) then
         sl4 = .false.
         if (dbgpr)write(ko,*)'sl4 off'
         goto 9331
      else
         sl4 = .true.
         if (dbgpr)write(ko,*)'sl4 on'
         goto 9332
      endif

 9331 if (sl4) then
         sl4 = .false.
         if (dbgpr)write(ko,*)'sl4 off'
      else
         sl4 = .true.
         if (dbgpr)write(ko,*)'sl4 on'
      endif
      goto 9337

 9332 if (alpha)9333,9337,9337

 9333 if (nh - 50)9337,9334,9334

 9334 if (alpha - (alphap + epsa))9335,9337,9337

 9335 if (power - powngl)9336,9337,9337

 9336 eta3 = 10.0d0 * eta3
      ns4  = ns4 + 4
      if (dbgpr)write(ko,*)'power small ns4 up'
      stop'power small ns4 up'

      if (sl4) then
         if (dbgpr)write(ko,*)'sl4 off'
         sl4 = .false.
      else
         sl4 = .true.
         if (dbgpr)write(ko,*)'sl4 on'
      endif

 9337 alphap = alpha
      goto 8009
     
      stop
 
 9900 format(a)
 9911 format(6(1pe15.4,1x))
 9912 format(2(i3,1x),6(1pe10.3,1x))
 9913 format(i3,1x,6(1pe10.3,1x))
 9915 format(' ',9i6)
 
 9921 format(' ',36i3)
 9931 format(' ',9f11.7)
 9941 format(' ',9i11)
 9942 format('k effective            = ',1pe15.7)
 9943 format('total kinetic energy   = ',1pe15.6,
     *       ' total internal energy = ',1pe15.6)
 9944 format('initial maximum radius = ',1pe15.6)
 9980 format('of en(ig)')
 9981 format(9(1pe13.5,1x))
 9982 format(/,'   total energy      kinetic energy    check error'
     *       '       local')
 9983 format(/,'  k   density     radius    velocity   pressure',
     *       '   intnrgy     temp')
 9984 format('dump ',i2)
 9985 format('or enn')
 9986 format(/,'       time             qp             power',
     *       '           alpha           delt            w')

      end
      subroutine zeroary

      include 'real8.h'
      include 'cax1.h'

      save

      do i=1,4
         a(i)  = 0.0d0
         ak(i) = 0.0d0
      enddo

      do i=1,5
         am(i)    = 0.0d0
         ambar(i) = 0.0d0
         b(i)     = 0.0d0
      enddo

      do i=1,7
         v(i)     = 0.0d0
         anu(i)   = 0.0d0
         sum(7)   = 0.0d0

         do j=1,8
            anusig(i,j) = 0.0d0
            sig(i,j)    = 0.0d0
            do l=1,7
               sigma(i,l,j) = 0.0d0
            enddo
         enddo
      enddo

      do i=1,8
         do j=1,8
            mn(i,j)  = 0
            p(i,j)   = 0.0d0
         enddo
      enddo

      do i=1,41
         hp(i)    = 0.0d0
         rl(i)    = 0.0d0
      enddo

      do i=1,40
         ro(i)     = 0.0d0
         r(i)      = 0.0d0
         u(i)      = 0.0d0
         he(i)     = 0.0d0
         theta(i)  = 0.0d0
         rho(i)    = 0.0d0
         f(i)      = 0.0d0
         e(i)      = 0.0d0
         rbar(i)   = 0.0d0
         delta(i)  = 0.0d0
         s(i)      = 0.0d0
         t(i)      = 0.0d0
         h(i)      = 0.0d0
         so(i)     = 0.0d0
         wn(i)     = 0.0d0
         ennn(i)   = 0.0d0
         hmass(i)  = 0.0d0
         rosn(i)   = 0.0d0

         do j=1,7
            en(j,i) = 0.0d0
         enddo

         do j=1,5
            enn(i,j) = 0.0d0
         enddo
      enddo

      return
      end
