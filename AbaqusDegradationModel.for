SUBROUTINE VUSDFLD(CSTEPNO, TINC, NFIELD, NSTATEV, NPROPS,
     +               NDMAX, NDP, COORDS, ALLDV, DELTAE,
     +               SDVINI, NFIELDI, NSTATEVI, NPROPSI,
     +               X, LFLAGS, LOAD, S, TEMP, DTEMP,
     +               PREDEF, DPRED, DROT, CMNAME, ACOORDS,
     +               DFGRD0, F0, PREDEF0, DPRED0, STRESS0,
     +               STATEV0, DSTATEV0, PROP0, SDEG, RPL, DDSDD,
     +               DRPLDE, RPLSTS, DDDSDD, DSTRAN, TIME,
     +               DTIME, TEMP0, DTEMP0, CTIME, KSTEP, KINC, JSTEP,
     +               JINC, NPROPS0, COORDS0, RPROPS, IS, FFLAGS,
     +               F0FL, C0FL, RPLFL, DFLFL, SFLFL, NFLFL,
     +               PNEWDT, DELTIM, MFLSTP, NSEMAX, NCUT, ICRIT,
     +               ICUT, IAPPLY, DFLG, DRPL0, DSTRESS0, STATEV00,
     +               DSTATEV00, NMATP, NCRPL, I, J, K, L)

  IMPLICIT REAL*8 (A-H,O-Z)
  
  INTEGER*4 CSTEPNO, NFIELD, NSTATEV, NPROPS, NDMAX, NDP, LFLAGS,
          NPROPSI, NSTATEVI, IS, FFLAGS, F0FL, C0FL, RPLFL,
          DFLFL, SFLFL, NFLFL, NPROPS0, NMATP, NCRPL
  INTEGER*4 KSTEP, KINC, JSTEP, JINC, NCUT, ICRIT, ICUT, IAPPLY,
          DFLG
  
  REAL*8 TINC, COORDS(*), ALLDV(*), DELTAE, SDVINI(*), NFIELDI(*),
         NSTATEVI(*), NPROPSI(*), X(*), LOAD(*), S(*), TEMP, DTEMP,
         DPRED, DROT, ACOORDS(*), DFGRD0(*), F0(*), PREDEF0(*),
         DPRED0(*), STRESS0(*), STATEV0(*), DSTATEV0(*), PROP0(*),
         SDEG, RPL, DDSDD(*), DRPLDE(*), RPLSTS(*), DDDSDD(*),
         DSTRAN(*), TIME, DTIME, TEMP0, DTEMP0, CTIME, COORDS0(*),
         RPROPS(*), DSTRESS0(*), STATEV00(*), DSTATEV00(*), PNEWDT,
         DELTIM, MFLSTP, NSEMAX, DRPL0, I, J, K, L
         
  REAL*8 eps, a, b, c, m, n, TDAY, CDD, CFF

  DO K = 1, NBLOCK
    ! Absolute value of current strain
    eps = ABS(STRAIN(K,3))

    ! Store the maximum strain as a solution-dependent state
    stateNew(K,1) = eps

    ! Update degradation degree based on current strain and time (in days)
    a = 0.385
    b = 0.152
    c = 0.616
    m = 0.342
    n = 0.236
    TDAY = 30
    CDD = (a*(b+c*eps**n))*(TDAY*m)

    ! Calculate current fracture strain using degradation degree
    CFF = 0.02 - (0.02 * CDD)
    stateNew(K,2) = CFF

    ! Element deletion step
    IF (CDD <= CFF) THEN
      stateNew(K,3) = 0
    END IF
  END DO

  RETURN
END SUBROUTINE VUSDFLD
