function ruc_saturation(Tempin,pressure)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  ruc_saturation calculate saturation
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2011-11-28
!
! ABSTRACT: 
!  This subroutine calculate saturation
!
! PROGRAM HISTORY LOG:
!    2011-11-28  Hu  Initial 
!
!
!   input argument list:
!     pressure    - background pressure  (hPa)
!     Temp        - temperature (K)
!
!   output argument list:
!     ruc_saturation
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________

  use constants, only: rd_over_cp, h1000,one,zero
  use kinds, only: r_single,i_kind, r_kind
!
    implicit none
    real(r_single) :: ruc_saturation

    REAL(r_single),intent(in) :: Tempin     ! temperature in K
    real(r_single),intent(in) :: pressure   ! pressure  (hpa)

    REAL(r_kind) :: Temp       ! temperature in K
    real(r_kind) ::  es0_p
    parameter (es0_p=6.1121_r_kind)     ! saturation vapor pressure (mb)
    real(r_kind) SVP1,SVP2,SVP3
    data SVP1,SVP2,SVP3/es0_p,17.67_r_kind,29.65_r_kind/

    real(r_kind) :: temp_qvis1, temp_qvis2
    data temp_qvis1, temp_qvis2 /268.15_r_kind, 263.15_r_kind/

    REAL(r_kind) :: evs, qvs1, eis, qvi1, watwgt
!
    Temp=Tempin
!
! evs, eis in mb
!   For this part, must use the water/ice saturation as f(temperature)
        evs = svp1*exp(SVP2*(Temp-273.15_r_kind)/(Temp-SVP3))
        qvs1 = 0.62198_r_kind*evs/(pressure-evs)  ! qvs1 is mixing ratio kg/kg
                                                     !  so no need next line
!      qvs1 = qvs1/(1.0-qvs1)
!   Get ice saturation and weighted ice/water saturation ready to go
!    for ensuring cloud saturation below.
        eis = svp1 *exp(22.514_r_kind - 6.15e3_r_kind/Temp)
        qvi1 = 0.62198_r_kind*eis/(pressure-eis)  ! qvi1 is mixing ratio kg/kg,
                                                     ! so no need next line
!      qvi1 = qvi1/(1.0-qvi1)
!      watwgt = max(0.,min(1.,(Temp-233.15)/(263.15-233.15)))
!      watwgt = max(zero,min(one,(Temp-251.15_r_kind)/&
!                        (263.15_r_kind-251.15_r_kind)))
! ph - 2/7/2012 - use ice mixing ratio only for temp < 263.15
        watwgt = max(zero,min(one,(Temp-temp_qvis2)/&
                              (temp_qvis1-temp_qvis2)))
        ruc_saturation= (watwgt*qvs1 + (one-watwgt)*qvi1)  !  kg/kg
!
end function ruc_saturation
