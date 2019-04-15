
DEF INPUT        PARAM ip-rowid1    AS   ROWID          NO-UNDO.
DEF INPUT        PARAM ip-rowid2    AS   ROWID          NO-UNDO.
DEF INPUT        PARAM ip-dept      LIKE est-op.dept    NO-UNDO.
DEF INPUT        PARAM ip-type      AS   CHAR           NO-UNDO.
DEF INPUT-OUTPUT PARAM io-crusiz    LIKE mach.mr-crusiz NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF VAR ld-sqft AS DEC NO-UNDO.
DEF VAR li AS INT NO-UNDO.


FIND mach WHERE ROWID(mach) EQ ip-rowid1 NO-LOCK NO-ERROR.

IF AVAIL mach THEN
FIND eb WHERE ROWID(eb) EQ ip-rowid2 NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ef
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-LOCK NO-ERROR.

IF AVAIL ef THEN DO:
  cocode = ef.company.

  {sys/inc/msfcalc.i}

  IF LOOKUP(ip-dept,"RS,RC") GT 0 OR mach.p-type EQ "R" THEN
    ld-sqft = IF ef.roll THEN (ef.gsh-wid * ef.gsh-len)
                         ELSE (ef.nsh-wid * ef.nsh-len).
  ELSE
  IF LOOKUP(ip-dept,"PR,GU,LM") GT 0 OR ef.n-out-l LE 1 THEN
    ld-sqft = ef.nsh-wid * ef.nsh-len.
  ELSE
    ld-sqft = ef.trim-w * ef.trim-l.

  IF mach.p-type EQ "B" THEN ld-sqft = eb.t-wid * eb.t-len.

  ld-sqft = IF v-corr THEN (ld-sqft * .007)
                      ELSE (ld-sqft / 144).

    IF ip-type = "M R" THEN DO:
       DO li = 1 TO EXTENT(mach.mr-crusiz-qty):                    
          IF mach.mr-crusiz-qty[li] GE ld-sqft THEN LEAVE.        
        IF li GE EXTENT(mach.mr-crusiz-qty) THEN li = li + 1.   
      END.                           
      IF li LE EXTENT(mach.mr-crusiz-qty) THEN DO: 
          ASSIGN io-crusiz = mach.mr-crusiz-cst[li].
      END.                  
    END. /*IF ip-type = "M R" THEN DO:*/
    ELSE IF ip-type = "RUN" THEN DO:
       DO li = 1 TO EXTENT(mach.run-crusiz-qty):                    
          IF mach.run-crusiz-qty[li] GE ld-sqft THEN LEAVE.        
        IF li GE EXTENT(mach.run-crusiz-qty) THEN li = li + 1.   
      END.
      IF li LE EXTENT(mach.run-crusiz-qty) THEN DO: 
          ASSIGN io-crusiz = mach.run-crusiz-cst[li].
      END.                                              
    END. /*IF ip-type = "RUN" THEN DO:*/
END.
