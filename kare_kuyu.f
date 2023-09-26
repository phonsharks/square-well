      PROGRAM KARE_KUYU
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON PI,R,N
      PI=DACOS(-1.D0)
      SABIT=3.81D0
      A=2.D0
      V0=50.D0
      R=DSQRT(V0/SABIT)*A
      TOL=1.D-12
      DO 1 N=1,50
            UILK=0.001D0*R
            USON=0.999D0*R
            CALL YARILA(UILK,USON,U,TOL)
            ENERJI=SABIT*(U/A)**2
            PRINT*, 'N=',N,' ENERJI = ', ENERJI
1     CONTINUE
      END

      SUBROUTINE YARILA(A,B,XM,TOL)
      IMPLICIT REAL*8(A-H,O-Z)
      IF(F(A)*F(B).GT.0.0) THEN
           STOP 'BU ARALIKTA KOK YOK.'
      ENDIF
      DX=B-A
      DO WHILE(DABS(DX).GT.TOL)
            XM=(A+B)/2
            IF((F(A)*F(XM)).LT.0.D0) THEN
                  B=XM
                  DX=B-A
            ELSE
                  A=XM
                  DX=B-A
            ENDIF
      ENDDO
      RETURN
      END

      FUNCTION F(X)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON PI,R,N
      F=X+DASIN(X/R)-N*PI/2
      RETURN
      END
