!---------------------------------------------------------
!  To compute 3D reflectivity using Thompson microphysics
!    -Guoqing Ge, 2019/4/23
!  
!  code adapted from WRFV3.9 in the RAP/HRRR repository
!  commit ID: If362e0f5da7. Based on the following codes:
!     -phys/module_mp_thompson.F
!     -phys/module_mp_radar.f90
!
!Usage: 
!  use module_mp_thompson
!  call refl_thompson(nx,ny,nz,qv,qc,qr,nr,qs,qg,t,p,refl)
!
!qv,qc,qr,qs,qg in unit of (kg/kg)
!t in unit of K
!p in unit of Pa
!
!---------------------------------------------------------
!
MODULE module_mp_radar
      PUBLIC :: rayleigh_soak_wetgraupel
      PUBLIC :: radar_init
      PRIVATE :: m_complex_water_ray
      PRIVATE :: m_complex_ice_maetzler
      PRIVATE :: m_complex_maxwellgarnett
      PRIVATE :: get_m_mix_nested
      PRIVATE :: get_m_mix
      PRIVATE :: WGAMMA
      PRIVATE :: GAMMLN
      INTEGER, PARAMETER, PUBLIC:: nrbins = 50
      DOUBLE PRECISION, DIMENSION(nrbins+1), PUBLIC:: xxDx
      DOUBLE PRECISION, DIMENSION(nrbins), PUBLIC:: xxDs,xdts,xxDg,xdtg
      DOUBLE PRECISION, PARAMETER, PUBLIC:: lamda_radar = 0.10
      DOUBLE PRECISION, PUBLIC:: K_w, PI5, lamda4
      COMPLEX*16, PUBLIC:: m_w_0, m_i_0
      DOUBLE PRECISION, DIMENSION(nrbins+1), PUBLIC:: simpson
      DOUBLE PRECISION, DIMENSION(3), PARAMETER, PUBLIC:: basis = &
                           (/1.d0/3.d0, 4.d0/3.d0, 1.d0/3.d0/)
      REAL, DIMENSION(4), PUBLIC:: xcre, xcse, xcge, xcrg, xcsg, xcgg
      REAL, PUBLIC:: xam_r, xbm_r, xmu_r, xobmr
      REAL, PUBLIC:: xam_s, xbm_s, xmu_s, xoams, xobms, xocms
      REAL, PUBLIC:: xam_g, xbm_g, xmu_g, xoamg, xobmg, xocmg
      REAL, PUBLIC:: xorg2, xosg2, xogg2
      INTEGER, PARAMETER, PUBLIC:: slen = 20
      CHARACTER(len=slen), PUBLIC:: &
              mixingrulestring_s, matrixstring_s, inclusionstring_s, &
              hoststring_s, hostmatrixstring_s, hostinclusionstring_s, &
              mixingrulestring_g, matrixstring_g, inclusionstring_g, &
              hoststring_g, hostmatrixstring_g, hostinclusionstring_g
      DOUBLE PRECISION, PARAMETER:: melt_outside_s = 0.9d0
      DOUBLE PRECISION, PARAMETER:: melt_outside_g = 0.9d0
      CHARACTER*256:: radar_debug
CONTAINS
      subroutine radar_init
      IMPLICIT NONE
      INTEGER:: n
      PI5 = 3.14159*3.14159*3.14159*3.14159*3.14159
      lamda4 = lamda_radar*lamda_radar*lamda_radar*lamda_radar
      m_w_0 = m_complex_water_ray (lamda_radar, 0.0d0)
      m_i_0 = m_complex_ice_maetzler (lamda_radar, 0.0d0)
      K_w = (ABS( (m_w_0*m_w_0 - 1.0) /(m_w_0*m_w_0 + 2.0) ))**2
      do n = 1, nrbins+1
         simpson(n) = 0.0d0
      enddo
      do n = 1, nrbins-1, 2
         simpson(n) = simpson(n) + basis(1)
         simpson(n+1) = simpson(n+1) + basis(2)
         simpson(n+2) = simpson(n+2) + basis(3)
      enddo
      do n = 1, slen
         mixingrulestring_s(n:n) = char(0)
         matrixstring_s(n:n) = char(0)
         inclusionstring_s(n:n) = char(0)
         hoststring_s(n:n) = char(0)
         hostmatrixstring_s(n:n) = char(0)
         hostinclusionstring_s(n:n) = char(0)
         mixingrulestring_g(n:n) = char(0)
         matrixstring_g(n:n) = char(0)
         inclusionstring_g(n:n) = char(0)
         hoststring_g(n:n) = char(0)
         hostmatrixstring_g(n:n) = char(0)
         hostinclusionstring_g(n:n) = char(0)
      enddo
      mixingrulestring_s = 'maxwellgarnett'
      hoststring_s = 'air'
      matrixstring_s = 'water'
      inclusionstring_s = 'spheroidal'
      hostmatrixstring_s = 'icewater'
      hostinclusionstring_s = 'spheroidal'
      mixingrulestring_g = 'maxwellgarnett'
      hoststring_g = 'air'
      matrixstring_g = 'water'
      inclusionstring_g = 'spheroidal'
      hostmatrixstring_g = 'icewater'
      hostinclusionstring_g = 'spheroidal'
      xxDx(1) = 100.D-6
      xxDx(nrbins+1) = 0.02d0
      do n = 2, nrbins
         xxDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(nrbins) &
                  *DLOG(xxDx(nrbins+1)/xxDx(1)) +DLOG(xxDx(1)))
      enddo
      do n = 1, nrbins
         xxDs(n) = DSQRT(xxDx(n)*xxDx(n+1))
         xdts(n) = xxDx(n+1) - xxDx(n)
      enddo
      xxDx(1) = 100.D-6
      xxDx(nrbins+1) = 0.05d0
      do n = 2, nrbins
         xxDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(nrbins) &
                  *DLOG(xxDx(nrbins+1)/xxDx(1)) +DLOG(xxDx(1)))
      enddo
      do n = 1, nrbins
         xxDg(n) = DSQRT(xxDx(n)*xxDx(n+1))
         xdtg(n) = xxDx(n+1) - xxDx(n)
      enddo
      xcre(1) = 1. + xbm_r
      xcre(2) = 1. + xmu_r
      xcre(3) = 1. + xbm_r + xmu_r
      xcre(4) = 1. + 2.*xbm_r + xmu_r
      do n = 1, 4
         xcrg(n) = WGAMMA(xcre(n))
      enddo
      xorg2 = 1./xcrg(2)
      xcse(1) = 1. + xbm_s
      xcse(2) = 1. + xmu_s
      xcse(3) = 1. + xbm_s + xmu_s
      xcse(4) = 1. + 2.*xbm_s + xmu_s
      do n = 1, 4
         xcsg(n) = WGAMMA(xcse(n))
      enddo
      xosg2 = 1./xcsg(2)
      xcge(1) = 1. + xbm_g
      xcge(2) = 1. + xmu_g
      xcge(3) = 1. + xbm_g + xmu_g
      xcge(4) = 1. + 2.*xbm_g + xmu_g
      do n = 1, 4
         xcgg(n) = WGAMMA(xcge(n))
      enddo
      xogg2 = 1./xcgg(2)
      xobmr = 1./xbm_r
      xoams = 1./xam_s
      xobms = 1./xbm_s
      xocms = xoams**xobms
      xoamg = 1./xam_g
      xobmg = 1./xbm_g
      xocmg = xoamg**xobmg
      end subroutine radar_init
      COMPLEX*16 FUNCTION m_complex_water_ray(lambda,T)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN):: T,lambda
      DOUBLE PRECISION:: epsinf,epss,epsr,epsi
      DOUBLE PRECISION:: alpha,lambdas,sigma,nenner
      COMPLEX*16, PARAMETER:: i = (0d0,1d0)
      DOUBLE PRECISION, PARAMETER:: PIx=3.1415926535897932384626434d0
      epsinf = 5.27137d0 + 0.02164740d0 * T - 0.00131198d0 * T*T
      epss = 78.54d+0 * (1.0 - 4.579d-3 * (T - 25.0) &
              + 1.190d-5 * (T - 25.0)*(T - 25.0) &
              - 2.800d-8 * (T - 25.0)*(T - 25.0)*(T - 25.0))
      alpha = -16.8129d0/(T+273.16) + 0.0609265d0
      lambdas = 0.00033836d0 * exp(2513.98d0/(T+273.16)) * 1e-2
      nenner = 1.d0+2.d0*(lambdas/lambda)**(1d0-alpha)*sin(alpha*PIx*0.5) &
             + (lambdas/lambda)**(2d0-2d0*alpha)
      epsr = epsinf + ((epss-epsinf) * ((lambdas/lambda)**(1d0-alpha) &
           * sin(alpha*PIx*0.5)+1d0)) / nenner
      epsi = ((epss-epsinf) * ((lambdas/lambda)**(1d0-alpha) &
           * cos(alpha*PIx*0.5)+0d0)) / nenner &
           + lambda*1.25664/1.88496
      m_complex_water_ray = SQRT(CMPLX(epsr,-epsi))
      END FUNCTION m_complex_water_ray
      COMPLEX*16 FUNCTION m_complex_ice_maetzler(lambda,T)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(IN):: T,lambda
      DOUBLE PRECISION:: f,c,TK,B1,B2,b,deltabeta,betam,beta,theta,alfa
      c = 2.99d8
      TK = T + 273.16
      f = c / lambda * 1d-9
      B1 = 0.0207
      B2 = 1.16d-11
      b = 335.0d0
      deltabeta = EXP(-10.02 + 0.0364*(TK-273.16))
      betam = (B1/TK) * ( EXP(b/TK) / ((EXP(b/TK)-1)**2) ) + B2*f*f
      beta = betam + deltabeta
      theta = 300. / TK - 1.
      alfa = (0.00504d0 + 0.0062d0*theta) * EXP(-22.1d0*theta)
      m_complex_ice_maetzler = 3.1884 + 9.1e-4*(TK-273.16)
      m_complex_ice_maetzler = m_complex_ice_maetzler &
                             + CMPLX(0.0d0, (alfa/f + beta*f))
      m_complex_ice_maetzler = SQRT(CONJG(m_complex_ice_maetzler))
      END FUNCTION m_complex_ice_maetzler
      subroutine rayleigh_soak_wetgraupel (x_g, a_geo, b_geo, fmelt, &
                     meltratio_outside, m_w, m_i, lambda, C_back, &
                     mixingrule,matrix,inclusion, &
                     host,hostmatrix,hostinclusion)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(in):: x_g, a_geo, b_geo, fmelt, lambda, &
                                     meltratio_outside
      DOUBLE PRECISION, INTENT(out):: C_back
      COMPLEX*16, INTENT(in):: m_w, m_i
      CHARACTER(len=*), INTENT(in):: mixingrule, matrix, inclusion, &
                                     host, hostmatrix, hostinclusion
      COMPLEX*16:: m_core, m_air
      DOUBLE PRECISION:: D_large, D_g, rhog, x_w, xw_a, fm, fmgrenz, &
                         volg, vg, volair, volice, volwater, &
                         meltratio_outside_grenz, mra
      INTEGER:: error
      DOUBLE PRECISION, PARAMETER:: PIx=3.1415926535897932384626434d0
      m_air = (1.0d0,0.0d0)
      fm = DMAX1(DMIN1(fmelt, 1.0d0), 0.0d0)
      mra = DMAX1(DMIN1(meltratio_outside, 1.0d0), 0.0d0)
      mra = mra + (1.0d0-mra)*fm
      x_w = x_g * fm
      D_g = a_geo * x_g**b_geo
      if (D_g .ge. 1d-12) then
       vg = PIx/6. * D_g**3
       rhog = DMAX1(DMIN1(x_g / vg, 900.0d0), 10.0d0)
       vg = x_g / rhog
       meltratio_outside_grenz = 1.0d0 - rhog / 1000.
       if (mra .le. meltratio_outside_grenz) then
        volg = vg * (1.0d0 - mra * fm)
       else
        fmgrenz=(900.0-rhog)/(mra*900.0-rhog+900.0*rhog/1000.)
        if (fm .le. fmgrenz) then
         volg = (1.0 - mra * fm) * vg
        else
         volg = (x_g - x_w) / 900.0 + x_w / 1000.
        endif
       endif
       D_large = (6.0 / PIx * volg) ** (1./3.)
       volice = (x_g - x_w) / (volg * 900.0)
       volwater = x_w / (1000. * volg)
       volair = 1.0 - volice - volwater
       m_core = get_m_mix_nested (m_air, m_i, m_w, volair, volice, &
                         volwater, mixingrule, host, matrix, inclusion, &
                         hostmatrix, hostinclusion, error)
       if (error .ne. 0) then
        C_back = 0.0d0
        return
       endif
       C_back = (ABS((m_core**2-1.0d0)/(m_core**2+2.0d0)))**2 &
                * PI5 * D_large**6 / lamda4
      else
       C_back = 0.0d0
      endif
      end subroutine rayleigh_soak_wetgraupel
      complex*16 function get_m_mix_nested (m_a, m_i, m_w, volair, &
                     volice, volwater, mixingrule, host, matrix, &
                     inclusion, hostmatrix, hostinclusion, cumulerror)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(in):: volice, volair, volwater
      COMPLEX*16, INTENT(in):: m_a, m_i, m_w
      CHARACTER(len=*), INTENT(in):: mixingrule, host, matrix, &
                     inclusion, hostmatrix, hostinclusion
      INTEGER, INTENT(out):: cumulerror
      DOUBLE PRECISION:: vol1, vol2
      COMPLEX*16:: mtmp
      INTEGER:: error
      cumulerror = 0
      get_m_mix_nested = CMPLX(1.0d0,0.0d0)
      if (host .eq. 'air') then
       if (matrix .eq. 'air') then
        write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        cumulerror = cumulerror + 1
       else
        vol1 = volice / MAX(volice+volwater,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, 0.0d0, vol1, vol2, &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error
        if (hostmatrix .eq. 'air') then
         get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a, &
                         volair, (1.0d0-volair), 0.0d0, mixingrule, &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'icewater') then
         get_m_mix_nested = get_m_mix (m_a, mtmp, 2.0*m_a, &
                         volair, (1.0d0-volair), 0.0d0, mixingrule, &
                         'ice', hostinclusion, error)
         cumulerror = cumulerror + error
        else
         write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', &
                           hostmatrix
         cumulerror = cumulerror + 1
        endif
       endif
      elseif (host .eq. 'ice') then
       if (matrix .eq. 'ice') then
        write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        cumulerror = cumulerror + 1
       else
        vol1 = volair / MAX(volair+volwater,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, vol1, 0.0d0, vol2, &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error
        if (hostmatrix .eq. 'ice') then
         get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a, &
                         (1.0d0-volice), volice, 0.0d0, mixingrule, &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'airwater') then
         get_m_mix_nested = get_m_mix (mtmp, m_i, 2.0*m_a, &
                         (1.0d0-volice), volice, 0.0d0, mixingrule, &
                         'air', hostinclusion, error)
         cumulerror = cumulerror + error
        else
         write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', &
                           hostmatrix
         cumulerror = cumulerror + 1
        endif
       endif
      elseif (host .eq. 'water') then
       if (matrix .eq. 'water') then
        write(radar_debug,*) 'GET_M_MIX_NESTED: bad matrix: ', matrix
        cumulerror = cumulerror + 1
       else
        vol1 = volair / MAX(volice+volair,1d-10)
        vol2 = 1.0d0 - vol1
        mtmp = get_m_mix (m_a, m_i, m_w, vol1, vol2, 0.0d0, &
                         mixingrule, matrix, inclusion, error)
        cumulerror = cumulerror + error
        if (hostmatrix .eq. 'water') then
         get_m_mix_nested = get_m_mix (2*m_a, mtmp, m_w, &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule, &
                         hostmatrix, hostinclusion, error)
         cumulerror = cumulerror + error
        elseif (hostmatrix .eq. 'airice') then
         get_m_mix_nested = get_m_mix (2*m_a, mtmp, m_w, &
                         0.0d0, (1.0d0-volwater), volwater, mixingrule, &
                         'ice', hostinclusion, error)
         cumulerror = cumulerror + error
        else
         write(radar_debug,*) 'GET_M_MIX_NESTED: bad hostmatrix: ', &
                           hostmatrix
         cumulerror = cumulerror + 1
        endif
       endif
      elseif (host .eq. 'none') then
       get_m_mix_nested = get_m_mix (m_a, m_i, m_w, &
                       volair, volice, volwater, mixingrule, &
                       matrix, inclusion, error)
       cumulerror = cumulerror + error
      else
       write(radar_debug,*) 'GET_M_MIX_NESTED: unknown matrix: ', host
       cumulerror = cumulerror + 1
      endif
      IF (cumulerror .ne. 0) THEN
       write(radar_debug,*) 'GET_M_MIX_NESTED: error encountered'
       get_m_mix_nested = CMPLX(1.0d0,0.0d0)
      endif
      end function get_m_mix_nested
      COMPLEX*16 FUNCTION get_m_mix (m_a, m_i, m_w, volair, volice, &
                     volwater, mixingrule, matrix, inclusion, error)
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(in):: volice, volair, volwater
      COMPLEX*16, INTENT(in):: m_a, m_i, m_w
      CHARACTER(len=*), INTENT(in):: mixingrule, matrix, inclusion
      INTEGER, INTENT(out):: error
      error = 0
      get_m_mix = CMPLX(1.0d0,0.0d0)
      if (mixingrule .eq. 'maxwellgarnett') then
       if (matrix .eq. 'ice') then
        get_m_mix = m_complex_maxwellgarnett(volice, volair, volwater, &
                           m_i, m_a, m_w, inclusion, error)
       elseif (matrix .eq. 'water') then
        get_m_mix = m_complex_maxwellgarnett(volwater, volair, volice, &
                           m_w, m_a, m_i, inclusion, error)
       elseif (matrix .eq. 'air') then
        get_m_mix = m_complex_maxwellgarnett(volair, volwater, volice, &
                           m_a, m_w, m_i, inclusion, error)
       else
        write(radar_debug,*) 'GET_M_MIX: unknown matrix: ', matrix
        error = 1
       endif
      else
       write(radar_debug,*) 'GET_M_MIX: unknown mixingrule: ', mixingrule
       error = 2
      endif
      if (error .ne. 0) then
       write(radar_debug,*) 'GET_M_MIX: error encountered'
      endif
      END FUNCTION get_m_mix
      COMPLEX*16 FUNCTION m_complex_maxwellgarnett(vol1, vol2, vol3, &
                     m1, m2, m3, inclusion, error)
      IMPLICIT NONE
      COMPLEX*16 :: m1, m2, m3
      DOUBLE PRECISION :: vol1, vol2, vol3
      CHARACTER(len=*) :: inclusion
      COMPLEX*16 :: beta2, beta3, m1t, m2t, m3t
      INTEGER, INTENT(out) :: error
      error = 0
      if (DABS(vol1+vol2+vol3-1.0d0) .gt. 1d-6) then
       write(radar_debug,*) 'M_COMPLEX_MAXWELLGARNETT: sum of the ', &
              'partial volume fractions is not 1...ERROR'
       m_complex_maxwellgarnett=CMPLX(-999.99d0,-999.99d0)
       error = 1
       return
      endif
      m1t = m1**2
      m2t = m2**2
      m3t = m3**2
      if (inclusion .eq. 'spherical') then
       beta2 = 3.0d0*m1t/(m2t+2.0d0*m1t)
       beta3 = 3.0d0*m1t/(m3t+2.0d0*m1t)
      elseif (inclusion .eq. 'spheroidal') then
       beta2 = 2.0d0*m1t/(m2t-m1t) * (m2t/(m2t-m1t)*LOG(m2t/m1t)-1.0d0)
       beta3 = 2.0d0*m1t/(m3t-m1t) * (m3t/(m3t-m1t)*LOG(m3t/m1t)-1.0d0)
      else
       write(radar_debug,*) 'M_COMPLEX_MAXWELLGARNETT: ', &
                         'unknown inclusion: ', inclusion
       m_complex_maxwellgarnett=DCMPLX(-999.99d0,-999.99d0)
       error = 1
       return
      endif
      m_complex_maxwellgarnett = &
       SQRT(((1.0d0-vol2-vol3)*m1t + vol2*beta2*m2t + vol3*beta3*m3t) / &
       (1.0d0-vol2-vol3+vol2*beta2+vol3*beta3))
      END FUNCTION m_complex_maxwellgarnett
      REAL FUNCTION GAMMLN(XX)
      IMPLICIT NONE
      REAL, INTENT(IN):: XX
      DOUBLE PRECISION, PARAMETER:: STP = 2.5066282746310005D0
      DOUBLE PRECISION, DIMENSION(6), PARAMETER:: &
               COF = (/76.18009172947146D0, -86.50532032941677D0, &
                       24.01409824083091D0, -1.231739572450155D0, &
                      .1208650973866179D-2, -.5395239384953D-5/)
      DOUBLE PRECISION:: SER,TMP,X,Y
      INTEGER:: J
      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.000000000190015D0
      DO 11 J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
11 CONTINUE
      GAMMLN=TMP+LOG(STP*SER/X)
      END FUNCTION GAMMLN
      REAL FUNCTION WGAMMA(y)
      IMPLICIT NONE
      REAL, INTENT(IN):: y
      WGAMMA = EXP(GAMMLN(y))
      END FUNCTION WGAMMA
END MODULE module_mp_radar

MODULE module_mp_thompson

      USE module_mp_radar
      IMPLICIT NONE
      public refl_thompson

      LOGICAL, PARAMETER, PRIVATE:: iiwarm = .false.
      INTEGER, PARAMETER, PRIVATE:: IFDRY = 0
      REAL, PARAMETER, PRIVATE:: T_0 = 273.15
      REAL, PARAMETER, PRIVATE:: PI = 3.1415926536

!..Densities of rain, snow, graupel, and cloud ice.
      REAL, PARAMETER, PRIVATE:: rho_w = 1000.0
      REAL, PARAMETER, PRIVATE:: rho_s = 100.0
      REAL, PARAMETER, PRIVATE:: rho_g = 500.0
      REAL, PARAMETER, PRIVATE:: rho_i = 890.0

!..Prescribed number of cloud droplets.  Set according to known data or
!.. roughly 100 per cc (100.E6 m^-3) for Maritime cases and
!.. 300 per cc (300.E6 m^-3) for Continental.  Gamma shape parameter,
!.. mu_c, calculated based on Nt_c is important in autoconversion
!.. scheme.  In 2-moment cloud water, Nt_c represents a maximum of
!.. droplet concentration and nu_c is also variable depending on local
!.. droplet number concentration.
      REAL, PARAMETER, PRIVATE:: Nt_c = 100.E6
      REAL, PARAMETER, PRIVATE:: Nt_c_max = 1999.E6

!..Declaration of constants for assumed CCN/IN aerosols when none in
!.. the input data.  Look inside the init routine for modifications
!.. due to surface land-sea points or vegetation characteristics.
      REAL, PARAMETER, PRIVATE:: naIN0 = 1.5E6
      REAL, PARAMETER, PRIVATE:: naIN1 = 0.5E6
      REAL, PARAMETER, PRIVATE:: naCCN0 = 300.0E6
      REAL, PARAMETER, PRIVATE:: naCCN1 = 50.0E6

!..Generalized gamma distributions for rain, graupel and cloud ice.
!.. N(D) = N_0 * D**mu * exp(-lamda*D);  mu=0 is exponential.
      REAL, PARAMETER, PRIVATE:: mu_r = 0.0
      REAL, PARAMETER, PRIVATE:: mu_g = 0.0
      REAL, PARAMETER, PRIVATE:: mu_i = 0.0
      REAL, PRIVATE:: mu_c

!..Sum of two gamma distrib for snow (Field et al. 2005).
!.. N(D) = M2**4/M3**3 * [Kap0*exp(-M2*Lam0*D/M3)
!..    + Kap1*(M2/M3)**mu_s * D**mu_s * exp(-M2*Lam1*D/M3)]
!.. M2 and M3 are the (bm_s)th and (bm_s+1)th moments respectively
!.. calculated as function of ice water content and temperature.
      REAL, PARAMETER, PRIVATE:: mu_s = 0.6357
      REAL, PARAMETER, PRIVATE:: Kap0 = 490.6
      REAL, PARAMETER, PRIVATE:: Kap1 = 17.46
      REAL, PARAMETER, PRIVATE:: Lam0 = 20.78
      REAL, PARAMETER, PRIVATE:: Lam1 = 3.29

!..Y-intercept parameter for graupel is not constant and depends on
!.. mixing ratio.  Also, when mu_g is non-zero, these become equiv
!.. y-intercept for an exponential distrib and proper values are
!.. computed based on same mixing ratio and total number concentration.
      REAL, PARAMETER, PRIVATE:: gonv_min = 1.E4
      REAL, PARAMETER, PRIVATE:: gonv_max = 3.E6

!..Mass power law relations:  mass = am*D**bm
!.. Snow from Field et al. (2005), others assume spherical form.
      REAL, PARAMETER, PRIVATE:: am_r = PI*rho_w/6.0
      REAL, PARAMETER, PRIVATE:: bm_r = 3.0
      REAL, PARAMETER, PRIVATE:: am_s = 0.069
      REAL, PARAMETER, PRIVATE:: bm_s = 2.0
      REAL, PARAMETER, PRIVATE:: am_g = PI*rho_g/6.0
      REAL, PARAMETER, PRIVATE:: bm_g = 3.0
      REAL, PARAMETER, PRIVATE:: am_i = PI*rho_i/6.0
      REAL, PARAMETER, PRIVATE:: bm_i = 3.0

!..Fallspeed power laws relations:  v = (av*D**bv)*exp(-fv*D)
!.. Rain from Ferrier (1994), ice, snow, and graupel from
!.. Thompson et al (2008). Coefficient fv is zero for graupel/ice.
      REAL, PARAMETER, PRIVATE:: av_r = 4854.0
      REAL, PARAMETER, PRIVATE:: bv_r = 1.0
      REAL, PARAMETER, PRIVATE:: fv_r = 195.0
      REAL, PARAMETER, PRIVATE:: av_s = 40.0
      REAL, PARAMETER, PRIVATE:: bv_s = 0.55
      REAL, PARAMETER, PRIVATE:: fv_s = 100.0
      REAL, PARAMETER, PRIVATE:: av_g = 442.0
      REAL, PARAMETER, PRIVATE:: bv_g = 0.89
      REAL, PARAMETER, PRIVATE:: av_i = 1847.5
      REAL, PARAMETER, PRIVATE:: bv_i = 1.0
      REAL, PARAMETER, PRIVATE:: av_c = 0.316946E8
      REAL, PARAMETER, PRIVATE:: bv_c = 2.0

!..Capacitance of sphere and plates/aggregates: D**3, D**2
      REAL, PARAMETER, PRIVATE:: C_cube = 0.5
      REAL, PARAMETER, PRIVATE:: C_sqrd = 0.15

!..Collection efficiencies.  Rain/snow/graupel collection of cloud
!.. droplets use variables (Ef_rw, Ef_sw, Ef_gw respectively) and
!.. get computed elsewhere because they are dependent on stokes
!.. number.
      REAL, PARAMETER, PRIVATE:: Ef_si = 0.05
      REAL, PARAMETER, PRIVATE:: Ef_rs = 0.95
      REAL, PARAMETER, PRIVATE:: Ef_rg = 0.75
      REAL, PARAMETER, PRIVATE:: Ef_ri = 0.95

!..Minimum microphys values
!.. R1 value, 1.E-12, cannot be set lower because of numerical
!.. problems with Paul Field's moments and should not be set larger
!.. because of truncation problems in snow/ice growth.
      REAL, PARAMETER, PRIVATE:: R1 = 1.E-12
      REAL, PARAMETER, PRIVATE:: R2 = 1.E-6
      REAL, PARAMETER, PRIVATE:: eps = 1.E-15

!..Constants in Cooper curve relation for cloud ice number.
      REAL, PARAMETER, PRIVATE:: TNO = 5.0
      REAL, PARAMETER, PRIVATE:: ATO = 0.304

!..Rho_not used in fallspeed relations (rho_not/rho)**.5 adjustment.
      REAL, PARAMETER, PRIVATE:: rho_not = 101325.0/(287.05*298.0)

!..Schmidt number
      REAL, PARAMETER, PRIVATE:: Sc = 0.632
      REAL, PRIVATE:: Sc3

!..Homogeneous freezing temperature
      REAL, PARAMETER, PRIVATE:: HGFR = 235.16

!..Water vapor and air gas constants at constant pressure
      REAL, PARAMETER, PRIVATE:: Rv = 461.5
      REAL, PARAMETER, PRIVATE:: oRv = 1./Rv
      REAL, PARAMETER, PRIVATE:: R = 287.04
      REAL, PARAMETER, PRIVATE:: Cp = 1004.0
      REAL, PARAMETER, PRIVATE:: R_uni = 8.314                           ! J (mol K)-1

      DOUBLE PRECISION, PARAMETER, PRIVATE:: k_b = 1.38065E-23           ! Boltzmann constant [J/K]
      DOUBLE PRECISION, PARAMETER, PRIVATE:: M_w = 18.01528E-3           ! molecular mass of water [kg/mol]
      DOUBLE PRECISION, PARAMETER, PRIVATE:: M_a = 28.96E-3              ! molecular mass of air [kg/mol]
      DOUBLE PRECISION, PARAMETER, PRIVATE:: N_avo = 6.022E23            ! Avogadro number [1/mol]
      DOUBLE PRECISION, PARAMETER, PRIVATE:: ma_w = M_w / N_avo          ! mass of water molecule [kg]
      REAL, PARAMETER, PRIVATE:: ar_volume = 4./3.*PI*(2.5e-6)**3        ! assume radius of 0.025 micrometer, 2.5e-6 cm

!..Enthalpy of sublimation, vaporization, and fusion at 0C.
      REAL, PARAMETER, PRIVATE:: lsub = 2.834E6
      REAL, PARAMETER, PRIVATE:: lvap0 = 2.5E6
      REAL, PARAMETER, PRIVATE:: lfus = lsub - lvap0
      REAL, PARAMETER, PRIVATE:: olfus = 1./lfus

!..Ice initiates with this mass (kg), corresponding diameter calc.
!..Min diameters and mass of cloud, rain, snow, and graupel (m, kg).
      REAL, PARAMETER, PRIVATE:: xm0i = 1.E-12
      REAL, PARAMETER, PRIVATE:: D0c = 1.E-6
      REAL, PARAMETER, PRIVATE:: D0r = 50.E-6
      REAL, PARAMETER, PRIVATE:: D0s = 200.E-6
      REAL, PARAMETER, PRIVATE:: D0g = 250.E-6
      REAL, PRIVATE:: D0i, xm0s, xm0g

!..Lookup table dimensions
      INTEGER, PARAMETER, PRIVATE:: nbins = 100
      INTEGER, PARAMETER, PRIVATE:: nbc = nbins
      INTEGER, PARAMETER, PRIVATE:: nbi = nbins
      INTEGER, PARAMETER, PRIVATE:: nbr = nbins
      INTEGER, PARAMETER, PRIVATE:: nbs = nbins
      INTEGER, PARAMETER, PRIVATE:: nbg = nbins
      INTEGER, PARAMETER, PRIVATE:: ntb_c = 37
      INTEGER, PARAMETER, PRIVATE:: ntb_i = 64
      INTEGER, PARAMETER, PRIVATE:: ntb_r = 37
      INTEGER, PARAMETER, PRIVATE:: ntb_s = 28
      INTEGER, PARAMETER, PRIVATE:: ntb_g = 28
      INTEGER, PARAMETER, PRIVATE:: ntb_g1 = 28
      INTEGER, PARAMETER, PRIVATE:: ntb_r1 = 37
      INTEGER, PARAMETER, PRIVATE:: ntb_i1 = 55
      INTEGER, PARAMETER, PRIVATE:: ntb_t = 9
      INTEGER, PRIVATE:: nic1, nic2, nii2, nii3, nir2, nir3, nis2, nig2, nig3
      INTEGER, PARAMETER, PRIVATE:: ntb_arc = 7
      INTEGER, PARAMETER, PRIVATE:: ntb_arw = 9
      INTEGER, PARAMETER, PRIVATE:: ntb_art = 7
      INTEGER, PARAMETER, PRIVATE:: ntb_arr = 5
      INTEGER, PARAMETER, PRIVATE:: ntb_ark = 4
      INTEGER, PARAMETER, PRIVATE:: ntb_IN = 55
      INTEGER, PRIVATE:: niIN2

      DOUBLE PRECISION, DIMENSION(nbins+1):: xDx
      DOUBLE PRECISION, DIMENSION(nbc):: Dc, dtc
      DOUBLE PRECISION, DIMENSION(nbi):: Di, dti
      DOUBLE PRECISION, DIMENSION(nbr):: Dr, dtr
      DOUBLE PRECISION, DIMENSION(nbs):: Ds, dts
      DOUBLE PRECISION, DIMENSION(nbg):: Dg, dtg
      DOUBLE PRECISION, DIMENSION(nbc):: t_Nc

!..For snow moments conversions (from Field et al. 2005)
      REAL, DIMENSION(10), PARAMETER, PRIVATE:: &
      sa = (/ 5.065339, -0.062659, -3.032362, 0.029469, -0.000285, &
              0.31255,   0.000204,  0.003199, 0.0,      -0.015952/)
      REAL, DIMENSION(10), PARAMETER, PRIVATE:: &
      sb = (/ 0.476221, -0.015896,  0.165977, 0.007468, -0.000141, &
              0.060366,  0.000079,  0.000594, 0.0,      -0.003577/)

!..Temperatures (5 C interval 0 to -40) used in lookup tables.
      REAL, DIMENSION(ntb_t), PARAMETER, PRIVATE:: &
      Tc = (/-0.01, -5., -10., -15., -20., -25., -30., -35., -40./)

!..Lookup tables for various accretion/collection terms.
!.. ntb_x refers to the number of elements for rain, snow, graupel,
!.. and temperature array indices.  Variables beginning with t-p/c/m/n
!.. represent lookup tables.  Save compile-time memory by making
!.. allocatable (2009Jun12, J. Michalakes).
      INTEGER, PARAMETER, PRIVATE:: R8SIZE = 8
      INTEGER, PARAMETER, PRIVATE:: R4SIZE = 4
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:,:)::             &
                tcg_racg, tmr_racg, tcr_gacr, tmg_gacr,                 &
                tnr_racg, tnr_gacr
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:,:)::             &
                tcs_racs1, tmr_racs1, tcs_racs2, tmr_racs2,             &
                tcr_sacr1, tms_sacr1, tcr_sacr2, tms_sacr2,             &
                tnr_racs1, tnr_racs2, tnr_sacr1, tnr_sacr2
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:,:)::             &
                tpi_qcfz, tni_qcfz
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:,:)::             &
                tpi_qrfz, tpg_qrfz, tni_qrfz, tnr_qrfz
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:)::                 &
                tps_iaus, tni_iaus, tpi_ide
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:):: t_Efrw
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:):: t_Efsw
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:):: tnr_rev
      REAL (KIND=R8SIZE), ALLOCATABLE, DIMENSION(:,:,:)::               &
                tpc_wev, tnc_wev
      REAL (KIND=R4SIZE), ALLOCATABLE, DIMENSION(:,:,:,:,:):: tnccn_act

!..Variables holding a bunch of exponents and gamma values (cloud water,
!.. cloud ice, rain, snow, then graupel).
      REAL, DIMENSION(5,15), PRIVATE:: cce, ccg
      REAL, DIMENSION(15), PRIVATE::  ocg1, ocg2
      REAL, DIMENSION(7), PRIVATE:: cie, cig
      REAL, PRIVATE:: oig1, oig2, obmi
      REAL, DIMENSION(13), PRIVATE:: cre, crg
      REAL, PRIVATE:: ore1, org1, org2, org3, obmr
      REAL, DIMENSION(18), PRIVATE:: cse, csg
      REAL, PRIVATE:: oams, obms, ocms
      REAL, DIMENSION(12), PRIVATE:: cge, cgg
      REAL, PRIVATE:: oge1, ogg1, ogg2, ogg3, oamg, obmg, ocmg

!..Declaration of precomputed constants in various rate eqns.
      REAL:: t1_qr_qc, t1_qr_qi, t2_qr_qi, t1_qg_qc, t1_qs_qc, t1_qs_qi
      REAL:: t1_qr_ev, t2_qr_ev
      REAL:: t1_qs_sd, t2_qs_sd, t1_qg_sd, t2_qg_sd
      REAL:: t1_qs_me, t2_qs_me, t1_qg_me, t2_qg_me

!+---+
!+---+-----------------------------------------------------------------+
!..END DECLARATIONS
!+---+-----------------------------------------------------------------+
!+---+
!ctrlL

      CONTAINS

      SUBROUTINE thompson_init()

      IMPLICIT NONE
      INTEGER :: n

!..From Martin et al. (1994), assign gamma shape parameter mu for cloud
!.. drops according to general dispersion characteristics (disp=~0.25
!.. for Maritime and 0.45 for Continental).
!.. disp=SQRT((mu+2)/(mu+1) - 1) so mu varies from 15 for Maritime
!.. to 2 for really dirty air.  This not used in 2-moment cloud water
!.. scheme and nu_c used instead and varies from 2 to 15 (integer-only).
      mu_c = MIN(15., (1000.E6/Nt_c + 2.))

!..Schmidt number to one-third used numerous times.
      Sc3 = Sc**(1./3.)

!..Compute min ice diam from mass, min snow/graupel mass from diam.
      D0i = (xm0i/am_i)**(1./bm_i)
      xm0s = am_s * D0s**bm_s
      xm0g = am_g * D0g**bm_g

!..These constants various exponents and gamma() assoc with cloud,
!.. rain, snow, and graupel.
      do n = 1, 15
         cce(1,n) = n + 1.
         cce(2,n) = bm_r + n + 1.
         cce(3,n) = bm_r + n + 4.
         cce(4,n) = n + bv_c + 1.
         cce(5,n) = bm_r + n + bv_c + 1.
         ccg(1,n) = WGAMMA(cce(1,n))
         ccg(2,n) = WGAMMA(cce(2,n))
         ccg(3,n) = WGAMMA(cce(3,n))
         ccg(4,n) = WGAMMA(cce(4,n))
         ccg(5,n) = WGAMMA(cce(5,n))
         ocg1(n) = 1./ccg(1,n)
         ocg2(n) = 1./ccg(2,n)
      enddo

      cie(1) = mu_i + 1.
      cie(2) = bm_i + mu_i + 1.
      cie(3) = bm_i + mu_i + bv_i + 1.
      cie(4) = mu_i + bv_i + 1.
      cie(5) = mu_i + 2.
      cie(6) = bm_i*0.5 + mu_i + bv_i + 1.
      cie(7) = bm_i*0.5 + mu_i + 1.
      cig(1) = WGAMMA(cie(1))
      cig(2) = WGAMMA(cie(2))
      cig(3) = WGAMMA(cie(3))
      cig(4) = WGAMMA(cie(4))
      cig(5) = WGAMMA(cie(5))
      cig(6) = WGAMMA(cie(6))
      cig(7) = WGAMMA(cie(7))
      oig1 = 1./cig(1)
      oig2 = 1./cig(2)
      obmi = 1./bm_i

      cre(1) = bm_r + 1.
      cre(2) = mu_r + 1.
      cre(3) = bm_r + mu_r + 1.
      cre(4) = bm_r*2. + mu_r + 1.
      cre(5) = mu_r + bv_r + 1.
      cre(6) = bm_r + mu_r + bv_r + 1.
      cre(7) = bm_r*0.5 + mu_r + bv_r + 1.
      cre(8) = bm_r + mu_r + bv_r + 3.
      cre(9) = mu_r + bv_r + 3.
      cre(10) = mu_r + 2.
      cre(11) = 0.5*(bv_r + 5. + 2.*mu_r)
      cre(12) = bm_r*0.5 + mu_r + 1.
      cre(13) = bm_r*2. + mu_r + bv_r + 1.
      do n = 1, 13
         crg(n) = WGAMMA(cre(n))
      enddo
      obmr = 1./bm_r
      ore1 = 1./cre(1)
      org1 = 1./crg(1)
      org2 = 1./crg(2)
      org3 = 1./crg(3)

      cse(1) = bm_s + 1.
      cse(2) = bm_s + 2.
      cse(3) = bm_s*2.
      cse(4) = bm_s + bv_s + 1.
      cse(5) = bm_s*2. + bv_s + 1.
      cse(6) = bm_s*2. + 1.
      cse(7) = bm_s + mu_s + 1.
      cse(8) = bm_s + mu_s + 2.
      cse(9) = bm_s + mu_s + 3.
      cse(10) = bm_s + mu_s + bv_s + 1.
      cse(11) = bm_s*2. + mu_s + bv_s + 1.
      cse(12) = bm_s*2. + mu_s + 1.
      cse(13) = bv_s + 2.
      cse(14) = bm_s + bv_s
      cse(15) = mu_s + 1.
      cse(16) = 1.0 + (1.0 + bv_s)/2.
      cse(17) = cse(16) + mu_s + 1.
      cse(18) = bv_s + mu_s + 3.
      do n = 1, 18
         csg(n) = WGAMMA(cse(n))
      enddo
      oams = 1./am_s
      obms = 1./bm_s
      ocms = oams**obms

      cge(1) = bm_g + 1.
      cge(2) = mu_g + 1.
      cge(3) = bm_g + mu_g + 1.
      cge(4) = bm_g*2. + mu_g + 1.
      cge(5) = bm_g*2. + mu_g + bv_g + 1.
      cge(6) = bm_g + mu_g + bv_g + 1.
      cge(7) = bm_g + mu_g + bv_g + 2.
      cge(8) = bm_g + mu_g + bv_g + 3.
      cge(9) = mu_g + bv_g + 3.
      cge(10) = mu_g + 2.
      cge(11) = 0.5*(bv_g + 5. + 2.*mu_g)
      cge(12) = 0.5*(bv_g + 5.) + mu_g
      do n = 1, 12
         cgg(n) = WGAMMA(cge(n))
      enddo
      oamg = 1./am_g
      obmg = 1./bm_g
      ocmg = oamg**obmg
      oge1 = 1./cge(1)
      ogg1 = 1./cgg(1)
      ogg2 = 1./cgg(2)
      ogg3 = 1./cgg(3)

      END SUBROUTINE thompson_init

!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
!+---+-----------------------------------------------------------------+
      REAL FUNCTION GAMMLN(XX)
!     --- RETURNS THE VALUE LN(GAMMA(XX)) FOR XX > 0.
      IMPLICIT NONE
      REAL, INTENT(IN):: XX
      DOUBLE PRECISION, PARAMETER:: STP = 2.5066282746310005D0
      DOUBLE PRECISION, DIMENSION(6), PARAMETER:: &
               COF = (/76.18009172947146D0, -86.50532032941677D0, &
                       24.01409824083091D0, -1.231739572450155D0, &
                      .1208650973866179D-2, -.5395239384953D-5/)
      DOUBLE PRECISION:: SER,TMP,X,Y
      INTEGER:: J

      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.000000000190015D0
      DO 11 J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER/X)
      END FUNCTION GAMMLN
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
!+---+-----------------------------------------------------------------+
      REAL FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL, INTENT(IN):: y

      WGAMMA = EXP(GAMMLN(y))

      END FUNCTION WGAMMA
!+---+-----------------------------------------------------------------+
!..Compute radar reflectivity assuming 10 cm wavelength radar and using
!.. Rayleigh approximation.  Only complication is melted snow/graupel
!.. which we treat as water-coated ice spheres and use Uli Blahak's
!.. library of routines.  The meltwater fraction is simply the amount
!.. of frozen species remaining from what initially existed at the
!.. melting level interface.
!+---+-----------------------------------------------------------------+
! dcd  added vt_dBZ, first_time_step
      subroutine calc_refl10cm (qv1d, qc1d, qr1d, nr1d, qs1d, qg1d,     &
                          t1d, p1d, dBZ, vt_dBZ, rand1, kts, kte,       &
                          ii, jj, first_time_step)

      IMPLICIT NONE

!..Sub arguments
      INTEGER, INTENT(IN):: kts, kte, ii, jj
      REAL, INTENT(IN):: rand1
      REAL, DIMENSION(kts:kte), INTENT(IN)::                            &
                          qv1d, qc1d, qr1d, nr1d, qs1d, qg1d, t1d, p1d
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: dBZ
      REAL, DIMENSION(kts:kte), INTENT(INOUT):: vt_dBZ
      LOGICAL, INTENT(IN) :: first_time_step       ! dcd

!..Local variables
      LOGICAL :: allow_wet_graupel    ! dcd
      LOGICAL :: allow_wet_snow       ! dcd
      REAL, DIMENSION(kts:kte):: temp, pres, qv, rho, rhof
      REAL, DIMENSION(kts:kte):: rc, rr, nr, rs, rg

      DOUBLE PRECISION, DIMENSION(kts:kte):: ilamr, ilamg, N0_r, N0_g
      REAL, DIMENSION(kts:kte):: mvd_r
      REAL, DIMENSION(kts:kte):: smob, smo2, smoc, smoz
      REAL:: oM3, M0, Mrat, slam1, slam2, xDs
      REAL:: ils1, ils2, t1_vts, t2_vts, t3_vts, t4_vts
      REAL:: vtr_dbz_wt, vts_dbz_wt, vtg_dbz_wt

      REAL, DIMENSION(kts:kte):: ze_rain, ze_snow, ze_graupel

      DOUBLE PRECISION:: N0_exp, N0_min, lam_exp, lamr, lamg
      REAL:: a_, b_, loga_, tc0
      DOUBLE PRECISION:: fmelt_s, fmelt_g

      INTEGER:: i, k, k_0, kbot, n
      LOGICAL:: melti
      LOGICAL, DIMENSION(kts:kte):: L_qr, L_qs, L_qg

      DOUBLE PRECISION:: cback, x, eta, f_d
      REAL:: xslw1, ygra1, zans1

!+---+
! dcd
      if (first_time_step) then
!       no bright banding, to be consistent with hydrometeor retrieval in GSI
        allow_wet_snow = .false.
      else
        allow_wet_snow = .true.
      endif
      allow_wet_graupel = .false.
!      print*, 'allow_wet_snow = ', allow_wet_snow
!      print*, 'allow_wet_graupel = ', allow_wet_graupel
! dcd

      do k = kts, kte
         dBZ(k) = -35.0
      enddo

!+---+-----------------------------------------------------------------+
!..Put column of data into local arrays.
!+---+-----------------------------------------------------------------+
      do k = kts, kte
         temp(k) = t1d(k)
         qv(k) = MAX(1.E-10, qv1d(k))
         pres(k) = p1d(k)
         rho(k) = 0.622*pres(k)/(R*temp(k)*(qv(k)+0.622))
         rhof(k) = SQRT(RHO_NOT/rho(k))
         rc(k) = MAX(R1, qc1d(k)*rho(k))
         if (qr1d(k) .gt. R1) then
            rr(k) = qr1d(k)*rho(k)
            nr(k) = MAX(R2, nr1d(k)*rho(k))
            lamr = (am_r*crg(3)*org2*nr(k)/rr(k))**obmr
            ilamr(k) = 1./lamr
            N0_r(k) = nr(k)*org2*lamr**cre(2)
            mvd_r(k) = (3.0 + mu_r + 0.672) * ilamr(k)
            L_qr(k) = .true.
         else
            rr(k) = R1
            nr(k) = R1
            mvd_r(k) = 50.E-6
            L_qr(k) = .false.
         endif
         if (qs1d(k) .gt. R2) then
            rs(k) = qs1d(k)*rho(k)
            L_qs(k) = .true.
         else
            rs(k) = R1
            L_qs(k) = .false.
         endif
         if (qg1d(k) .gt. R2) then
            rg(k) = qg1d(k)*rho(k)
            L_qg(k) = .true.
         else
            rg(k) = R1
            L_qg(k) = .false.
         endif
      enddo

!+---+-----------------------------------------------------------------+
!..Calculate y-intercept, slope, and useful moments for snow.
!+---+-----------------------------------------------------------------+
      do k = kts, kte
         tc0 = MIN(-0.1, temp(k)-273.15)
         smob(k) = rs(k)*oams

!..All other moments based on reference, 2nd moment.  If bm_s.ne.2,
!.. then we must compute actual 2nd moment and use as reference.
         if (bm_s.gt.(2.0-1.e-3) .and. bm_s.lt.(2.0+1.e-3)) then
            smo2(k) = smob(k)
         else
            loga_ = sa(1) + sa(2)*tc0 + sa(3)*bm_s &
     &         + sa(4)*tc0*bm_s + sa(5)*tc0*tc0 &
     &         + sa(6)*bm_s*bm_s + sa(7)*tc0*tc0*bm_s &
     &         + sa(8)*tc0*bm_s*bm_s + sa(9)*tc0*tc0*tc0 &
     &         + sa(10)*bm_s*bm_s*bm_s
            a_ = 10.0**loga_
            b_ = sb(1) + sb(2)*tc0 + sb(3)*bm_s &
     &         + sb(4)*tc0*bm_s + sb(5)*tc0*tc0 &
     &         + sb(6)*bm_s*bm_s + sb(7)*tc0*tc0*bm_s &
     &         + sb(8)*tc0*bm_s*bm_s + sb(9)*tc0*tc0*tc0 &
     &         + sb(10)*bm_s*bm_s*bm_s
            smo2(k) = (smob(k)/a_)**(1./b_)
         endif

!..Calculate bm_s+1 (th) moment.  Useful for diameter calcs.
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(1) &
     &         + sa(4)*tc0*cse(1) + sa(5)*tc0*tc0 &
     &         + sa(6)*cse(1)*cse(1) + sa(7)*tc0*tc0*cse(1) &
     &         + sa(8)*tc0*cse(1)*cse(1) + sa(9)*tc0*tc0*tc0 &
     &         + sa(10)*cse(1)*cse(1)*cse(1)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(1) + sb(4)*tc0*cse(1) &
     &        + sb(5)*tc0*tc0 + sb(6)*cse(1)*cse(1) &
     &        + sb(7)*tc0*tc0*cse(1) + sb(8)*tc0*cse(1)*cse(1) &
     &        + sb(9)*tc0*tc0*tc0 + sb(10)*cse(1)*cse(1)*cse(1)
         smoc(k) = a_ * smo2(k)**b_

!..Calculate bm_s*2 (th) moment.  Useful for reflectivity.
         loga_ = sa(1) + sa(2)*tc0 + sa(3)*cse(3) &
     &         + sa(4)*tc0*cse(3) + sa(5)*tc0*tc0 &
     &         + sa(6)*cse(3)*cse(3) + sa(7)*tc0*tc0*cse(3) &
     &         + sa(8)*tc0*cse(3)*cse(3) + sa(9)*tc0*tc0*tc0 &
     &         + sa(10)*cse(3)*cse(3)*cse(3)
         a_ = 10.0**loga_
         b_ = sb(1)+ sb(2)*tc0 + sb(3)*cse(3) + sb(4)*tc0*cse(3) &
     &        + sb(5)*tc0*tc0 + sb(6)*cse(3)*cse(3) &
     &        + sb(7)*tc0*tc0*cse(3) + sb(8)*tc0*cse(3)*cse(3) &
     &        + sb(9)*tc0*tc0*tc0 + sb(10)*cse(3)*cse(3)*cse(3)
         smoz(k) = a_ * smo2(k)**b_
      enddo

!+---+-----------------------------------------------------------------+
!..Calculate y-intercept, slope values for graupel.
!+---+-----------------------------------------------------------------+

      do k = kte, kts, -1
         ygra1 = alog10(max(1.E-9, rg(k)))
         zans1 = (2.5 + 2./7. * (ygra1+7.)) + rand1
         zans1 = MAX(2., MIN(zans1, 7.))
         N0_exp = 10.**(zans1)
         N0_exp = MAX(DBLE(gonv_min), MIN(N0_exp, DBLE(gonv_max)))
         lam_exp = (N0_exp*am_g*cgg(1)/rg(k))**oge1
         lamg = lam_exp * (cgg(3)*ogg2*ogg1)**obmg
         ilamg(k) = 1./lamg
         N0_g(k) = N0_exp/(cgg(2)*lam_exp) * lamg**cge(2)
      enddo

!+---+-----------------------------------------------------------------+
!..Locate K-level of start of melting (k_0 is level above).
!+---+-----------------------------------------------------------------+
      melti = .false.
      k_0 = kts
      do k = kte-1, kts, -1
         if ( (temp(k).gt.273.15) .and. L_qr(k)                         &
     &                            .and. (L_qs(k+1).or.L_qg(k+1)) ) then
            k_0 = MAX(k+1, k_0)
            melti=.true.
            goto 195
         endif
      enddo
 195  continue

!+---+-----------------------------------------------------------------+
!..Assume Rayleigh approximation at 10 cm wavelength. Rain (all temps)
!.. and non-water-coated snow and graupel when below freezing are
!.. simple. Integrations of m(D)*m(D)*N(D)*dD.
!+---+-----------------------------------------------------------------+

      do k = kts, kte
         ze_rain(k) = 1.e-22
         ze_snow(k) = 1.e-22
         ze_graupel(k) = 1.e-22
         if (L_qr(k)) ze_rain(k) = N0_r(k)*crg(4)*ilamr(k)**cre(4)
         if (L_qs(k)) ze_snow(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)     &
     &                           * (am_s/900.0)*(am_s/900.0)*smoz(k)
         if (L_qg(k)) ze_graupel(k) = (0.176/0.93) * (6.0/PI)*(6.0/PI)  &
     &                              * (am_g/900.0)*(am_g/900.0)         &
     &                              * N0_g(k)*cgg(4)*ilamg(k)**cge(4)
      enddo

!+---+-----------------------------------------------------------------+
!..Special case of melting ice (snow/graupel) particles.  Assume the
!.. ice is surrounded by the liquid water.  Fraction of meltwater is
!.. extremely simple based on amount found above the melting level.
!.. Uses code from Uli Blahak (rayleigh_soak_wetgraupel and supporting
!.. routines).
!+---+-----------------------------------------------------------------+

      if (.not. iiwarm .and. melti .and. k_0.ge.2) then
       do k = k_0-1, kts, -1

!..Reflectivity contributed by melting snow
! dcd  added allow_wet_snow
          if (allow_wet_snow .and. L_qs(k) .and. L_qs(k_0) ) then
           fmelt_s = MAX(0.05d0, MIN(1.0d0-rs(k)/rs(k_0), 0.99d0))
           eta = 0.d0
           oM3 = 1./smoc(k)
           M0 = (smob(k)*oM3)
           Mrat = smob(k)*M0*M0*M0
           slam1 = M0 * Lam0
           slam2 = M0 * Lam1
           do n = 1, nrbins
              x = am_s * xxDs(n)**bm_s
              call rayleigh_soak_wetgraupel (x, DBLE(ocms), DBLE(obms), &
     &              fmelt_s, melt_outside_s, m_w_0, m_i_0, lamda_radar, &
     &              CBACK, mixingrulestring_s, matrixstring_s,          &
     &              inclusionstring_s, hoststring_s,                    &
     &              hostmatrixstring_s, hostinclusionstring_s)
              f_d = Mrat*(Kap0*DEXP(-slam1*xxDs(n))                     &
     &              + Kap1*(M0*xxDs(n))**mu_s * DEXP(-slam2*xxDs(n)))
              eta = eta + f_d * CBACK * simpson(n) * xdts(n)
           enddo
           ze_snow(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif

!..Reflectivity contributed by melting graupel
! dcd  added allow_wet_graupel
          if (allow_wet_graupel .and. L_qg(k) .and. L_qg(k_0) ) then
           fmelt_g = MAX(0.05d0, MIN(1.0d0-rg(k)/rg(k_0), 0.99d0))
           eta = 0.d0
           lamg = 1./ilamg(k)
           do n = 1, nrbins
              x = am_g * xxDg(n)**bm_g
              call rayleigh_soak_wetgraupel (x, DBLE(ocmg), DBLE(obmg), &
     &              fmelt_g, melt_outside_g, m_w_0, m_i_0, lamda_radar, &
     &              CBACK, mixingrulestring_g, matrixstring_g,          &
     &              inclusionstring_g, hoststring_g,                    &
     &              hostmatrixstring_g, hostinclusionstring_g)
              f_d = N0_g(k)*xxDg(n)**mu_g * DEXP(-lamg*xxDg(n))
              eta = eta + f_d * CBACK * simpson(n) * xdtg(n)
           enddo
           ze_graupel(k) = SNGL(lamda4 / (pi5 * K_w) * eta)
          endif

       enddo
      endif

      do k = kte, kts, -1
         dBZ(k) = 10.*log10((ze_rain(k)+ze_snow(k)+ze_graupel(k))*1.d18)
      enddo


!..Reflectivity-weighted terminal velocity (snow, rain, graupel, mix).
      do k = kte, kts, -1
         vt_dBZ(k) = 1.E-3
         if (rs(k).gt.R2) then
          Mrat = smob(k) / smoc(k)
          ils1 = 1./(Mrat*Lam0 + fv_s)
          ils2 = 1./(Mrat*Lam1 + fv_s)
          t1_vts = Kap0*csg(5)*ils1**cse(5)
          t2_vts = Kap1*Mrat**mu_s*csg(11)*ils2**cse(11)
          ils1 = 1./(Mrat*Lam0)
          ils2 = 1./(Mrat*Lam1)
          t3_vts = Kap0*csg(6)*ils1**cse(6)
          t4_vts = Kap1*Mrat**mu_s*csg(12)*ils2**cse(12)
          vts_dbz_wt = rhof(k)*av_s * (t1_vts+t2_vts)/(t3_vts+t4_vts)
          if (temp(k).ge.273.15 .and. temp(k).lt.275.15) then
             vts_dbz_wt = vts_dbz_wt*1.5
          elseif (temp(k).ge.275.15) then
             vts_dbz_wt = vts_dbz_wt*2.0
          endif
         else
          vts_dbz_wt = 1.E-3
         endif

         if (rr(k).gt.R1) then
          lamr = 1./ilamr(k)
          vtr_dbz_wt = rhof(k)*av_r*crg(13)*(lamr+fv_r)**(-cre(13))      &
     &               / (crg(4)*lamr**(-cre(4)))
         else
          vtr_dbz_wt = 1.E-3
         endif

         if (rg(k).gt.R2) then
          lamg = 1./ilamg(k)
          vtg_dbz_wt = rhof(k)*av_g*cgg(5)*lamg**(-cge(5))               &
     &               / (cgg(4)*lamg**(-cge(4)))
         else
          vtg_dbz_wt = 1.E-3
         endif

         vt_dBZ(k) = (vts_dbz_wt*ze_snow(k) + vtr_dbz_wt*ze_rain(k)      &
     &                + vtg_dbz_wt*ze_graupel(k))                        &
     &                / (ze_rain(k)+ze_snow(k)+ze_graupel(k))
      enddo

      end subroutine calc_refl10cm
!
!--------------------------------------------------------------
! wrapper to compute 3D Reflectivity by calling calc_refl10cm
!--------------------------------------------------------------
!
subroutine refl_thompson(nx,ny,nz,qv,qc,qr,nr,qs,qg,t,p,refl)
!
!qv,qc,qr,qs,qg in unit of (kg/kg)
!t in unit of K
!p in unit of Pa
!
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: nx, ny, nz
  REAL, DIMENSION(nx,ny,nz), INTENT(IN):: qv, qc, qr, nr, qs, qg,t,p
  REAL, DIMENSION(nx,ny,nz), INTENT(OUT):: refl
!local variables
  LOGICAL :: first_time_step
  REAL    :: rand1 !random number for SPP microphysics, set to 0 here
  REAL, DIMENSION(nz):: qv1d, qc1d, qr1d, nr1d, qs1d, qg1d,t1d,p1d
  REAL, DIMENSION(nz) ::dBZ, vt_dBZ  !vt_dBZ not required at this time
  INTEGER :: i,j,k, kts, kte
  first_time_step=.TRUE.
  rand1=0
  kts=1
  kte=nz
! 
  call thompson_init()
!
  do j = 1,ny
  do i = 1,nx
      qv1d(:) = qv(i,j,:)
      qc1d(:) = qc(i,j,:)
      qr1d(:) = qr(i,j,:)
      nr1d(:) = nr(i,j,:)
      qs1d(:) = qs(i,j,:)
      qg1d(:) = qg(i,j,:)
      t1d(:) = t(i,j,:)
      p1d(:) = p(i,j,:)
      dBZ=-999.0
      call calc_refl10cm (qv1d, qc1d, qr1d, nr1d, qs1d, qg1d,       &
                  t1d, p1d, dBZ, vt_dBZ, rand1, kts, kte,           &
                  i, j, first_time_step)
      do k = kts,kte
         refl(i,j,k) = MAX(-35., dBZ(k))
      enddo
  enddo
  enddo

end subroutine
!
!+---+-----------------------------------------------------------------+
END MODULE module_mp_thompson
!+---+-----------------------------------------------------------------+
