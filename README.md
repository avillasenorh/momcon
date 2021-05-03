# momcon

Conversion routines between fault plane representations


```
CNFG   : CoNjugate Fault Geometry                  S(3),N(3),B(3)    -> S(3),N(3),B(3) (overwrite)      B changes in sign
CROSS  : CROSS product of two vectors              A(3),B(3)         -> C(3)
FGF    : Fault Geometry Forward                    STRIKE,DIP,RAKE   -> S(3),N(3),B(3)                  B computed (B = N x S)
FGI    : Fault Geometry Inverse                    S(3),N(3),B(3)    -> STRIKE,DIP,RAKE                 B not used
FGMRTF : Fault Geometry Moment RT Forward          S(3),N(3),B(3),M0 -> M(3,3)                          B not used
FGMRTI : Fault Geometry Moment RT Inverse          M(3,3)            -> S(3),N(3),B(3),M0               B computed (B = P x T)
FGPAF  : Fault Geometry Principal Axes Forward     S(3),N(3)         -> P(3),T(3)
FGPAI  : Fault Geometry Principal Axes Inverse     P(3),T(3)         -> S(3),N(3)
FGPL   : Fault Geometry Planes (PAI)               S(3),N(3),B(3)    -> ZETA(3),ETA(3) ???              B used! N is already a nodal plane
JACOBI : JACOBI's method
MTOFF  : Moment Tensor OF Forward                  M(3,3)            -> G(6)
MTOFI  : Moment Tensor OF Inverse                  G(6)              -> M(3,3)
PAF    : Principal Axes Forward                    ZETA(3),ETA(3)    -> S(3),N(3),B(3)                  ZETA(3), ETA(3) recomputed (B)
PAI    : Principal Axes Inverse                    S(3),N(3),B(3)    -> ZETA(3),ETA(3) (1=P,2=T,3=B)    B used!

vectors are in spherical coordinates:  radius up, theta south, and phi east
horizontal = (theta,phi) plane

1 =              sin(eta)
2 =  cos(zeta) * cos(eta)
3 = -sin(zeta) * cos(eta)


According to Kagan: B = V x U (V = N: pole, normal to fault plane; U = S: slip vector)
                    B = N x S (same as in Buland subroutines)
                    B = T x P (in Buland subroutines B = P x T!)
                    T = (V + U)/sqrt(2) = (N + S)/sqrt(2)
                    P = (V - U)/sqrt(2) = (N - S)/sqrt(2)
```
