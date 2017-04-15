DEF INPUT        PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT-OUTPUT PARAM io-save-qty AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAM io-b-qty AS DEC NO-UNDO.

{sys/inc/var.i SHARED}

{cec/print4.i "NEW SHARED" "NEW SHARED"}
{cec/print42.i "NEW SHARED"}

DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef  FOR ef.
DEF NEW SHARED BUFFER xeb  FOR eb.

DEF BUFFER xop FOR est-op.
DEF BUFFER b-ef FOR ef.
DEF BUFFER groupcst FOR reftable.

DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR li-seq AS INT NO-UNDO.
DEF VAR v-out AS INT NO-UNDO.
DEF VAR v-on-f AS INT NO-UNDO.
DEF VAR tmp-waste AS de EXTENT 99 NO-UNDO.
DEF VAR max-w AS de NO-UNDO.
DEF VAR hold-gsh-qty LIKE ef.gsh-qty.
DEF VAR v-num-up AS INT NO-UNDO. 
def var v-num-out as int NO-UNDO.

DEF TEMP-TABLE tt-est-op NO-UNDO LIKE est-op.

DISABLE TRIGGERS FOR LOAD OF est-op.

{cec/msfcalc.i}

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST ef NO-LOCK
    WHERE ef.company EQ eb.company
      AND ef.est-no  EQ eb.est-no
      AND ef.form-no EQ eb.form-no
    NO-ERROR.

IF AVAIL ef THEN
FIND FIRST est NO-LOCK
    WHERE est.company EQ ef.company
      AND est.est-no  EQ ef.est-no
    NO-ERROR.

IF AVAIL est THEN DO:

  FIND FIRST groupcst NO-LOCK
      WHERE groupcst.reftable EQ "est/getqty.w"
        AND groupcst.company  EQ est.company
        AND groupcst.loc      EQ est.loc
        AND groupcst.code     EQ est.est-no
      USE-INDEX CODE NO-ERROR.

  IF NOT AVAIL groupcst AND est.est-type GE 7 THEN DO TRANSACTION:

     FIND LAST groupcst NO-LOCK WHERE groupcst.reftable EQ "est/getqty.w"
         USE-INDEX code2 NO-ERROR.
     
     li-seq = (IF AVAIL groupcst THEN INT(groupcst.code2) ELSE 0) + 1.
    
     CREATE groupcst.
     ASSIGN
      groupcst.reftable = "est/getqty.w"
      groupcst.company  = est.company
      groupcst.loc      = est.loc
      groupcst.code     = est.est-no.
      groupcst.code2    = STRING(li-seq,"9999999999").
  END.

  FIND CURRENT groupcst NO-LOCK NO-ERROR.
END.

IF AVAIL groupcst THEN DO:
   li-seq = INT(groupcst.code2).
  
   RELEASE groupcst.
  
   FOR EACH groupcst
       WHERE groupcst.reftable EQ "est/getqty.w"
         AND groupcst.code2    EQ STRING(li-seq,"9999999999")
         AND (groupcst.code    NE eb.est-no OR est.est-type GE 7)
       USE-INDEX code2 NO-LOCK
       BREAK BY groupcst.code:

     IF FIRST-OF(groupcst.code) THEN
     FOR EACH xeb
         WHERE xeb.company          EQ eb.company
           AND xeb.est-no           EQ groupcst.code
           AND xeb.num-up           EQ eb.num-up
           AND xeb.k-wid-array2[01] EQ eb.k-wid-array2[01]
           AND xeb.k-wid-array2[02] EQ eb.k-wid-array2[02]
           AND xeb.k-wid-array2[03] EQ eb.k-wid-array2[03]
           AND xeb.k-wid-array2[04] EQ eb.k-wid-array2[04]
           AND xeb.k-wid-array2[05] EQ eb.k-wid-array2[05]
           AND xeb.k-wid-array2[06] EQ eb.k-wid-array2[06]
           AND xeb.k-wid-array2[07] EQ eb.k-wid-array2[07]
           AND xeb.k-wid-array2[08] EQ eb.k-wid-array2[08]
           AND xeb.k-wid-array2[09] EQ eb.k-wid-array2[09]
           AND xeb.k-wid-array2[10] EQ eb.k-wid-array2[10]
           AND xeb.k-wid-array2[11] EQ eb.k-wid-array2[11]
           AND xeb.k-wid-array2[12] EQ eb.k-wid-array2[12]
           AND xeb.k-wid-array2[13] EQ eb.k-wid-array2[13]
           AND xeb.k-wid-array2[14] EQ eb.k-wid-array2[14]
           AND xeb.k-wid-array2[15] EQ eb.k-wid-array2[15]
           AND xeb.k-wid-array2[16] EQ eb.k-wid-array2[16]
           AND xeb.k-wid-array2[17] EQ eb.k-wid-array2[17]
           AND xeb.k-wid-array2[18] EQ eb.k-wid-array2[18]
           AND xeb.k-wid-array2[19] EQ eb.k-wid-array2[19]
           AND xeb.k-wid-array2[20] EQ eb.k-wid-array2[20]
           AND xeb.k-wid-array2[21] EQ eb.k-wid-array2[21]
           AND xeb.k-wid-array2[22] EQ eb.k-wid-array2[22]
           AND xeb.k-wid-array2[23] EQ eb.k-wid-array2[23]
           AND xeb.k-wid-array2[24] EQ eb.k-wid-array2[24]
           AND xeb.k-wid-array2[25] EQ eb.k-wid-array2[25]
           AND xeb.k-wid-array2[26] EQ eb.k-wid-array2[26]
           AND xeb.k-wid-array2[27] EQ eb.k-wid-array2[27]
           AND xeb.k-wid-array2[28] EQ eb.k-wid-array2[28]
           AND xeb.k-wid-array2[29] EQ eb.k-wid-array2[29]
           AND xeb.k-wid-array2[30] EQ eb.k-wid-array2[30]
           AND xeb.k-len-array2[01] EQ eb.k-len-array2[01]
           AND xeb.k-len-array2[02] EQ eb.k-len-array2[02]
           AND xeb.k-len-array2[03] EQ eb.k-len-array2[03]
           AND xeb.k-len-array2[04] EQ eb.k-len-array2[04]
           AND xeb.k-len-array2[05] EQ eb.k-len-array2[05]
           AND xeb.k-len-array2[06] EQ eb.k-len-array2[06]
           AND xeb.k-len-array2[07] EQ eb.k-len-array2[07]
           AND xeb.k-len-array2[08] EQ eb.k-len-array2[08]
           AND xeb.k-len-array2[09] EQ eb.k-len-array2[09]
           AND xeb.k-len-array2[10] EQ eb.k-len-array2[10]
           AND xeb.k-len-array2[11] EQ eb.k-len-array2[11]
           AND xeb.k-len-array2[12] EQ eb.k-len-array2[12]
           AND xeb.k-len-array2[13] EQ eb.k-len-array2[13]
           AND xeb.k-len-array2[14] EQ eb.k-len-array2[14]
           AND xeb.k-len-array2[15] EQ eb.k-len-array2[15]
           AND xeb.k-len-array2[16] EQ eb.k-len-array2[16]
           AND xeb.k-len-array2[17] EQ eb.k-len-array2[17]
           AND xeb.k-len-array2[18] EQ eb.k-len-array2[18]
           AND xeb.k-len-array2[19] EQ eb.k-len-array2[19]
           AND xeb.k-len-array2[20] EQ eb.k-len-array2[20]
           AND xeb.k-len-array2[21] EQ eb.k-len-array2[21]
           AND xeb.k-len-array2[22] EQ eb.k-len-array2[22]
           AND xeb.k-len-array2[23] EQ eb.k-len-array2[23]
           AND xeb.k-len-array2[24] EQ eb.k-len-array2[24]
           AND xeb.k-len-array2[25] EQ eb.k-len-array2[25]
           AND xeb.k-len-array2[26] EQ eb.k-len-array2[26]
           AND xeb.k-len-array2[27] EQ eb.k-len-array2[27]
           AND xeb.k-len-array2[28] EQ eb.k-len-array2[28]
           AND xeb.k-len-array2[29] EQ eb.k-len-array2[29]
           AND xeb.k-len-array2[30] EQ eb.k-len-array2[30]
           AND ROWID(xeb)           NE ROWID(eb)
         NO-LOCK,
  
         FIRST xef
         WHERE xef.company EQ xeb.company
           AND xef.est-no  EQ xeb.est-no
           AND xef.form-no EQ xeb.form-no
           AND xef.board   EQ ef.board
           AND xef.gsh-wid EQ ef.gsh-wid
           AND xef.gsh-len EQ ef.gsh-len
           AND xef.n-out   EQ ef.n-out
           AND xef.n-out-l EQ ef.n-out-l
           AND xef.n-out-d EQ ef.n-out-d
           AND ROWID(xef)  NE ROWID(ef)
           USE-INDEX est-qty
         NO-LOCK,
  
         FIRST xest
         WHERE xest.company EQ xef.company
           AND xest.est-no  EQ xef.est-no
         NO-LOCK
         BREAK BY xeb.form-no:

       IF est.est-type EQ 8 AND NOT FIRST-OF(xeb.form-no) THEN
          NEXT.
  
       RUN sys/inc/numup.p (xef.company, xef.est-no, xef.form-no, OUTPUT v-num-up).
  
       RUN est/ef-#out.p (ROWID(xef), OUTPUT v-out).
  
       IF xest.est-type LT 7 THEN
          FOR EACH est-op NO-LOCK
              WHERE est-op.company EQ xest.company 
                AND est-op.est-no  EQ xest.est-no 
                AND est-op.line    LT 500
              USE-INDEX est-qty
              BREAK BY est-op.qty:
         
            IF FIRST-OF(est-op.qty) THEN DO:
              IF FIRST(est-op.qty) OR
                 CAN-FIND(FIRST est-qty
                          WHERE est-qty.company EQ est-op.company
                            AND est-qty.est-no  EQ est-op.est-no
                            AND est-qty.eqty    EQ est-op.qty)
              THEN v-op-qty = est-op.qty.
              IF est-op.qty GE xest.est-qty[1] THEN LEAVE.
            END.
          END.
  
       ASSIGN
        qty      = IF xest.est-type GE 7 THEN xeb.yld-qty
                   ELSE
                     xest.est-qty[1] * (IF xeb.yld-qty LT 0 THEN (-1 / xeb.yld-qty)
                                                            ELSE xeb.yld-qty)
        save-qty = 0
        b-qty    = 0
  
        hold-gsh-qty = xef.gsh-qty.
  
       DO TRANSACTION:
          EMPTY TEMP-TABLE tt-est-op.
          FOR EACH est-op
              WHERE est-op.company EQ xest.company
                AND est-op.est-no  EQ xest.est-no
                AND (est-op.qty    EQ v-op-qty OR xest.est-type GE 7)
                AND est-op.line    GT 500
              USE-INDEX est-qty EXCLUSIVE:
            CREATE tt-est-op.
            BUFFER-COPY est-op TO tt-est-op.
            DELETE est-op.
          END.
          FOR EACH est-op
              WHERE est-op.company EQ xest.company
                AND est-op.est-no  EQ xest.est-no
                AND (est-op.qty    EQ v-op-qty OR xest.est-type GE 7)
                AND est-op.line    LE 500
              USE-INDEX est-qty EXCLUSIVE:
            CREATE xop.
            BUFFER-COPY est-op EXCEPT rec_key TO xop
            ASSIGN
             xop.line = est-op.line + 500.
          END.
       END.
  
       IF xest.est-type EQ 5 THEN DO:
          RUN cec/prokalk.p.
         
          ASSIGN         
           save-qty = save-qty + ((spo + r-spo[1]) * (v-num-up * v-out))
           b-qty    = IF v-corr THEN
                        (((xef.gsh-wid * xef.gsh-len) * (save-qty / v-num-up))
                         / v-out) * .000007
                      ELSE
                        (((xef.gsh-wid * xef.gsh-len) * (save-qty / v-num-up))
                         / v-out) / 144000.
       END.
  
       ELSE DO:
          t-shtfrm[xef.form-no] = qty / (v-num-up * v-out).
          {sys/inc/roundup.i t-shtfrm[xef.form-no]}
         
          IF xest.est-type EQ 6 THEN DO:
             RUN cec/box/prokalk2.p (500, ?).
            
             /*ASSIGN
              b-waste   = 0
              tmp-waste = 0.
            
             FOR EACH est-op
                 WHERE est-op.company EQ xest.company
                   AND est-op.est-no  EQ xest.est-no
                   AND (est-op.qty    EQ v-op-qty OR xest.est-type GE 7)
                   AND est-op.s-num   EQ xef.form-no
                   AND est-op.line    GT 500 
                 BY est-op.d-seq DESC BY est-op.line DESC:
            
               RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on-f).
            
               IF est-op.op-sb THEN
                 b-waste = b-waste + (est-op.op-waste / v-on-f).
               ELSE
                 tmp-waste[xeb.blank-no] = tmp-waste[xeb.blank-no] +
                                           (est-op.op-waste / (v-num-up * v-out)).
            
               IF b-waste EQ ? THEN b-waste = 0.
            
               {sys/inc/roundup.i b-waste}
               {sys/inc/roundup.i tmp-waste[xeb.blank-no]}
             END.
            
             max-w = 0.
             DO li = 1 TO 99:
               IF tmp-waste[li] GT max-w THEN max-w = tmp-waste[li].
             END.
             b-waste = b-waste + max-w.*/
            
             b-waste = spo.
          END.
         
          ELSE DO:
             RUN cec/com/prokalk.p.
            
             b-waste = 0.
             for each est-op where est-op.company = xest.company
                        aND est-op.est-no = xest.est-no
                        AND est-op.line > 500          and
                            est-op.s-num = xef.form-no and
                            est-op.b-num = 0 NO-LOCK:
               run sys/inc/numout.p (recid(est-op), output v-num-out).
               b-waste = b-waste + (est-op.op-waste / v-num-out).
             end.
             for each est-op where est-op.company = xest.company
                        aND est-op.est-no = xest.est-no
                        AND est-op.line > 500          and
                            est-op.s-num = xef.form-no and
                            est-op.b-num ne 0 no-lock:
               run sys/inc/numout.p (recid(est-op), output v-num-out).
               b-waste = b-waste + (est-op.op-waste / (v-num-up * v-num-out)).
             end.
            
             {sys/inc/roundup.i b-waste}
             r-spo[xef.form-no] = xef.gsh-qty - t-shtfrm[xef.form-no] - b-waste.
          END.
         
          ASSIGN     
           save-qty = t-shtfrm[xef.form-no] + b-waste + r-spo[xef.form-no]
           b-qty    = save-qty *
                      IF v-corr THEN (xef.gsh-len * xef.gsh-wid * .000007)
                                ELSE (xef.gsh-len * xef.gsh-wid / 144000)                  
           save-qty = save-qty * v-num-up * v-out.
       END.
  
       DO TRANSACTION:
         REPEAT:
            FIND b-ef WHERE ROWID(b-ef) EQ ROWID(xef) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAIL b-ef THEN DO:
               b-ef.gsh-qty = hold-gsh-qty.
               FIND CURRENT b-ef NO-LOCK.
               RELEASE b-ef.
               LEAVE.
            END.
         END.
  
         FOR EACH est-op
             WHERE est-op.company EQ xest.company
               AND est-op.est-no  EQ xest.est-no
               AND (est-op.qty    EQ v-op-qty OR xest.est-type GE 7)
               AND est-op.line    GT 500
             USE-INDEX est-qty EXCLUSIVE:
           DELETE est-op.
         END.
         FOR EACH tt-est-op:
            CREATE est-op.
            BUFFER-COPY tt-est-op TO est-op.
            DELETE tt-est-op.
         END.
       END.
  
       ASSIGN
        io-save-qty = io-save-qty + save-qty
        io-b-qty    = io-b-qty + b-qty.
     END.
   END.
END.
/* end ---------------------------------- copr. 2003  advanced software, inc. */
