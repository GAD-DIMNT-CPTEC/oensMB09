       subroutine evalue(hr,far,im1,clfr,fv)
c
c      This program will calculate the potential economic value of forecasts
c      copied from ~wd20zt/value/value.f of sgi machine
c      modified by Yuejian Zhu    (02/09/2001)
c
c      Changed: old- clfr = 0.1 for 10 climatological equally-likely-bin
c               new- clfr = sample climatological relative frequency
c               using subroutine pass this value from reliability diagram
c      modified by Yuejian Zhu    (09/03/2004)
c
c      Explaination of calculation from BAMS publication, Zhu. and etc..
c       ----------------------------------------------------
c              |       Yes           |        No
c       ----------------------------------------------------
c              |  h(its)             |   m(isses)
c         YES  |  M(itigated) L(oss) |   L(oss)
c              |  ML = ( C + Lu )    |   L = Lp + Lu
c       ----------------------------------------------------
c         NO   |  f(alse alarms)     |   c(orrect rejections)
c              |  C(ost)             |   N(o cost)
c       ----------------------------------------------------
c
c      o         - climatological frequency
c      ML=C+Lu   - Mitigated Loss
c      C         - Cost
c      L=Lp+Lu   - Loss
c      h         - hites
c      m         - misses
c      f         - false alarms
c
c      V = (xmecl - xmefc)/(xmecl - xmepf)
c      Mean Expense (general forecast) = h*ML + f*C + m*L         --> eq. (1)
c      Mean Expense (climate forecast) = min[o*L, o*ML + (1-o)*C] --> eq. (2)
c      Mean Expense (perfect forecast) = o*ML                     --> eq. (3)
c      Value = (ME(cl) - ME(fc))/(ME(cl) - ME(perf))              --> eq. (4)
c
c      by applying h+m=o, and h+m=h+f (bias free), define r=C/Lp, get:
c      (note: the equation will be independent of Lu)
c
c      V = (Min[o,r] - (h+f)r - m )/( Min[o,y] - or )             --> eq. (7)
c

       parameter (icl=18)
       dimension hr(*),   far(*),     v(im1-1,2), xmefc(im1)
       dimension xml(icl),xc(icl),    xl(icl),    fv(icl)

ccc... data for assume xml, xc and xl for totally 18 levels
       data xml/1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     *          1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00/
       data xc /1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     *          1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00/
       data xl /1.05,1.10,1.25,1.50,2.00,3.00,5.00,8.00,10.0,
     *          18.0,27.0,40.0,60.0,90.0,140.,210.,350.,500./
c
c      The following calculation will base on equation (7)
c
c      xmecl - mean expense for climate forecast: Min[o,r]
c      xmepf - mean expense for perfect forecast: o.r
c      xmefc - mean expense for general forecast: (h+f)r-m
c
c      xme1  - loss from climatological frequency  [ o ]
c      xme2  - loss from cost and mitigated loss   [ r ]
c      if xme2.lt.xme1 means need protect ( always )
c      if xme2.ge.xme1 means need giveup  ( never  )
c
c      input: (accumulative)
c      hr (i) = h/(h+m)
c      far(i) = f/(f+c)
c      when i=1,  not include in this calculation
c      when i=2,  start from 10% and above probabilities
c           i=3,  ......
c      when i=11, includes only 100% probabilities ( last for 10 members )
c
c      xml    - mitigrated loss ( the same as C, exclude Lu )
c      xc     - cost ( the same as C  )
c      xl     - loss ( the same as Lp, exclude Lu )
c      clfr   - climate frequency of forecast
c
ccc... loop over cost/loss ratios (r=C/Lp)

        do j = 1, 18

         do i = 1, im1-1
          v(i,1) = i
         enddo 

         xme1 = clfr*xl(j)/xl(j)
         xme2 = clfr*xml(j)/xl(j)+(1-clfr)*xc(j)/xl(j)
         xme  = xme1
         if(xme2.lt.xme1) xme=xme2
c        if(xme2.lt.xme1) print *,'always protect'
         if(xme2.lt.xme1) np=1
c        if(xme2.ge.xme1) print *,'never  protect'
         if(xme2.ge.xme1) np=0

         xmecl  = xme                                 ! min[o,r]
         xmepf  = clfr*xml(j)/xl(j)                   ! o.r     
c
c     loop over ensemble probabilities
c        if hr=1, far=0, perfect forecast, xmefc = xmepf, v = 1.0
c        if hr=0, far=1, all cost, xmefc = xmecl, v = 0.0
c        loop over all probabilities, will find maximum value 
c        do i = 1, im1
         do i = 2, im1
c note: h+m=o, h+m+f+c=1, clfr=o, hr=h/(h+m), far=f/(f+c), xml/xl=r, xc/xl=r
          xmefc(i) = clfr*hr(i)*xml(j)/xl(j)          ! h.r    
     *             + clfr*(1-hr(i))*xl(j)/xl(j)       ! m 
     *             + (1-clfr)*far(i)*xc(j)/xl(j)      ! f.r
          v(i-1,2)   = (xmecl-xmefc(i))/(xmecl-xmepf) 
c         print *, 'im=',i,' hr=',hr(i),' fa=',far(i),' value=',v(i,2)
         enddo     

         call sortm(v,im1-1,2,2)

         fv(j) = v(im1-1,2)
         write(6,66)  j,1./xl(j),np,v(im1-1,2),v(im1-1,1)
        enddo               ! do j = 1, 18

        write(6,67)
 66    format(1x,i4,f7.4,i4,2f8.3)
 67    format(1x)

       return
       end
