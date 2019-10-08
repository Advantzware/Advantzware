
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-type AS CHAR NO-UNDO.
DEF INPUT-OUTPUT PARAM io-comm AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAM io-marg AS DEC NO-UNDO.

DEF VAR v-comm-found AS LOG NO-UNDO.

FIND est NO-LOCK WHERE ROWID(est) EQ ip-rowid NO-ERROR.

IF AVAIL est THEN
FIND FIRST eb NO-LOCK
    WHERE eb.company EQ est.company
      AND eb.est-no  EQ est.est-no
      AND eb.form-no NE 0
      AND eb.cust-no NE ""
      AND eb.sman    NE ""
    NO-ERROR.

IF AVAIL eb AND CAN-FIND(FIRST sman
                         WHERE sman.company   EQ eb.company
                           AND sman.sman      EQ eb.sman
                           AND sman.commbasis EQ "M") THEN
FIND FIRST cust NO-LOCK
    WHERE cust.company EQ eb.company
      AND cust.cust-no EQ eb.cust-no
    NO-ERROR.

IF AVAIL cust THEN
DO:
   IF ip-type NE "C" OR
      (ip-type EQ "C" AND io-marg GE 0) THEN
      DO:
         FOR EACH smanmtrx NO-LOCK
             WHERE smanmtrx.company  EQ eb.company
               AND smanmtrx.sman     EQ eb.sman
               AND smanmtrx.custype  EQ cust.type
               AND smanmtrx.procat   EQ ""
               AND ((smanmtrx.comm   LE io-comm AND ip-type EQ "M") OR
                    (smanmtrx.netpct LE io-marg AND ip-type EQ "C"))
               BY (IF ip-type EQ "M" THEN smanmtrx.comm ELSE 0)   DESC
               BY (IF ip-type EQ "C" THEN smanmtrx.netpct ELSE 0) DESC
               BY ROWID(smanmtrx):
        
               ASSIGN
                  io-comm = smanmtrx.comm
                  v-comm-found = YES.

               IF ip-type EQ "M" THEN
                  io-marg = smanmtrx.netpct.
        
               LEAVE.
         END.
      END.
   
   IF ((ip-type EQ "C" AND (io-marg LT 0 OR v-comm-found = NO)) OR
       (ip-type EQ "M" AND v-comm-found = NO)) THEN
   DO:
      /*get lowest margin */
      FOR EACH smanmtrx FIELDS(comm netpct) NO-LOCK
          WHERE smanmtrx.company  EQ eb.company
            AND smanmtrx.sman     EQ eb.sman
            AND smanmtrx.custype  EQ cust.type
            AND smanmtrx.procat   EQ ""
            BY smanmtrx.netpct:
         
            io-comm = smanmtrx.comm.

            IF ip-type = "M" THEN
               io-marg = smanmtrx.netpct.

            LEAVE.
      END.
   END.
END.
