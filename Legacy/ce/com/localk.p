/* ------------------------------------------------- ce/com/localk.p 10/94 gb */
/* recalculate values of sequenced machines.                                  */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-line  AS INT   NO-UNDO.
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef  FOR ef.
DEF SHARED BUFFER xeb  FOR eb.

DEFINE BUFFER b-eb3 FOR eb.
DEF BUFFER xop FOR est-op.

{ce/print4.i SHARED SHARED}

DEF NEW SHARED VAR maxco AS INT NO-UNDO.
DEF SHARED VAR save_id AS RECID NO-UNDO.
DEF SHARED VAR chosen AS LOG FORMAT "y/n" NO-UNDO.
DEF VAR mess AS ch FORMAT "x(80)" EXTENT 2 NO-UNDO.
DEF VAR v-rc-line AS INT INIT 999 NO-UNDO.
DEF NEW SHARED VAR v-n-out AS INT INIT 1 NO-UNDO.
DEF VAR cumul AS DEC NO-UNDO.
DEF VAR v-hold AS INT NO-UNDO.
DEF VAR v-num-up AS INT NO-UNDO.
DEF VAR v-num-up1 AS INT NO-UNDO.
DEF VAR v-spo AS DEC EXTENT 2 NO-UNDO.
DEF VAR v-dept LIKE est-op.dept NO-UNDO.
DEF VAR v-est-qty LIKE est.est-qty EXTENT 1  NO-UNDO.
DEF VAR v-eb-qty LIKE eb.yld-qty NO-UNDO.
DEF VAR hold-id AS ROWID NO-UNDO.
def var v-cumul like cumul NO-UNDO.
def var v-dec as DEC NO-UNDO.
def var v-sav as dec extent 2 NO-UNDO.
def var v-blk as DEC NO-UNDO.
def var vn-out as DEC NO-UNDO.
def var v-outw like xef.n-out NO-UNDO.
def var v-outl like xef.n-out-l NO-UNDO.
def var v-outf as DEC NO-UNDO.
def var v-on-f as DEC NO-UNDO.
def var v-on-l as DEC NO-UNDO.
def var sh-tmp like sh-len NO-UNDO.
def var v-widp as LOG NO-UNDO.
def var v-yld as DEC NO-UNDO.
def var v-pass-colors as INT NO-UNDO.
DEF VAR ll-no-more-blank-fed AS LOG NO-UNDO.

DEF SHARED VAR CALL_id AS RECID NO-UNDO. 

DEF VAR fil_id AS RECID NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.

DEF TEMP-TABLE w-qty NO-UNDO
  FIELD b-num  LIKE est-op.b-num
  FIELD num-up LIKE eb.num-up
  FIELD num-bl AS DEC
  FIELD sav-bl AS DEC
  FIELD spo-bl AS DEC.

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF BUFFER b-ef1 FOR ef.
DEF VAR v-num-inks AS INT NO-UNDO.
DEF VAR v-index AS INT NO-UNDO.
DEF VAR v-program-6 AS CHAR.

DEF TEMP-TABLE tt-est-op LIKE est-op FIELD row-id AS ROWID.

{ce/mach-ink.i}


FIND FIRST w-ink NO-ERROR.
IF NOT AVAIL w-ink THEN RUN ce/mach-ink.p.

FIND FIRST xeb OF xef NO-LOCK NO-ERROR.

save-qty = qty.

FIND b-ef WHERE ROWID(b-ef) EQ ROWID(xef) NO-LOCK.


IF xest.recalc EQ YES OR xest.recalc-mr EQ YES THEN DO:
  RUN mach-loop (1).

  FOR EACH est-op
      WHERE est-op.company EQ xest.company
        AND est-op.est-no  EQ xest.est-no
        AND est-op.eqty    EQ 0
        AND est-op.s-num   EQ b-ef.form-no
        AND est-op.line    GE ip-line
        AND est-op.line    LT ip-line + 500
        AND (ROWID(est-op) EQ ip-rowid OR ip-rowid EQ ?)
      USE-INDEX est-qty NO-LOCK:

    CREATE tt-est-op.
    BUFFER-COPY est-op TO tt-est-op
    ASSIGN
     tt-est-op.row-id = ROWID(est-op).
  END.
END.

RUN mach-loop (2).

FIND xef WHERE ROWID(xef) EQ ROWID(b-ef) NO-LOCK.


IF xest.recalc EQ YES OR xest.recalc-mr EQ YES THEN DO:
  FOR EACH tt-est-op:
    FIND FIRST est-op WHERE ROWID(est-op) EQ tt-est-op.row-id NO-ERROR.
    IF AVAIL est-op THEN DO:

        IF xest.recalc EQ YES THEN      
        ASSIGN
         est-op.op-waste   = tt-est-op.op-waste
         est-op.op-speed   = tt-est-op.op-speed
         est-op.op-spoil   = tt-est-op.op-spoil
         est-op.op-crew[1] = tt-est-op.op-crew[1]
         est-op.op-crew[2] = tt-est-op.op-crew[2]
         est-op.op-rate[1] = tt-est-op.op-rate[1]
         est-op.op-rate[2] = tt-est-op.op-rate[2].
      


        IF xest.recalc-mr EQ YES THEN DO:
        IF LOOKUP(est-op.dept,"PR,CT") GT 0 THEN ERROR-STATUS:ERROR = YES.
        ELSE RUN est/gluer-mr.p (BUFFER est-op) NO-ERROR.

        IF ERROR-STATUS:ERROR                      AND
           (LOOKUP(est-op.dept,"PR,CT") LE 0 OR
            est-op.plates + est-op.fountains EQ 0) THEN
          est-op.op-mr = tt-est-op.op-mr.
      END.


        IF xest.recalc EQ YES THEN RUN est/diewaste.p (BUFFER est-op).
    END.

    RELEASE est-op.
    FOR EACH est-op
        WHERE est-op.company EQ tt-est-op.company
          AND est-op.est-no  EQ tt-est-op.est-no
          AND est-op.eqty    EQ 0
          AND est-op.line    GE ip-line
          AND est-op.line    LT ip-line + 500
          AND est-op.m-code  EQ tt-est-op.m-code
          AND est-op.op-pass EQ tt-est-op.op-pass
          AND CAN-FIND(FIRST b-ef
                       WHERE b-ef.company EQ est-op.company
                         AND b-ef.est-no  EQ est-op.est-no
                         AND b-ef.form-no EQ est-op.s-num
                         AND b-ef.board   EQ xef.board
                         AND b-ef.gsh-wid EQ xef.gsh-wid
                         AND b-ef.gsh-len EQ xef.gsh-len
                         AND b-ef.nsh-wid EQ xef.nsh-wid
                         AND b-ef.nsh-len EQ xef.nsh-len
                         AND b-ef.trim-w  EQ xef.trim-w
                         AND b-ef.trim-l  EQ xef.trim-l
                         AND b-ef.n-out   EQ xef.n-out
                         AND b-ef.n-out-l EQ xef.n-out-l)
           AND NOT CAN-FIND(FIRST b-eb
                            WHERE b-eb.company EQ est-op.company
                              AND b-eb.est-no  EQ est-op.est-no
                              AND b-eb.form-no EQ est-op.s-num
                              AND (b-eb.style  NE xeb.style OR
                                   b-eb.len    NE xeb.len   OR
                                   b-eb.wid    NE xeb.wid   OR
                                   b-eb.dep    NE xeb.dep))
        USE-INDEX est-qty
        BREAK BY est-op.s-num
              BY est-op.b-num:

      IF NOT FIRST(est-op.s-num) THEN DO:

          IF xest.recalc-mr EQ YES THEN
          IF LOOKUP(est-op.dept,"PR,CT") GT 0 THEN ERROR-STATUS:ERROR = YES.
          ELSE RUN est/gluer-mr.p (BUFFER est-op) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN DO:

            IF xest.recalc-mr EQ YES THEN
            IF LOOKUP(est-op.dept,"PR,CT") LE 0      OR
               est-op.plates + est-op.fountains EQ 0 THEN est-op.op-mr = 0.


            IF xest.recalc EQ YES THEN
            IF LOOKUP(est-op.dept,"PR,CT") LE 0      OR
              est-op.op-mr EQ 0                      THEN est-op.op-waste = 0.
        END.


          IF xest.recalc EQ YES THEN RUN est/diewaste.p.
      END.
    END.
  END.

  RELEASE b-ef.

  FOR EACH b-ef
      WHERE b-ef.company EQ xef.company
        AND b-ef.est-no  EQ xef.est-no
      USE-INDEX est-qty NO-LOCK
      BY b-ef.form-no DESC:

    LEAVE.
  END.


   FIND CURRENT xest EXCLUSIVE-LOCK NO-ERROR.
           ASSIGN
             xest.recalc    = NO
             xest.recalc-mr = NO.
   FIND CURRENT xest NO-LOCK. 


  IF b-ef.form-no EQ xef.form-no THEN
  FOR EACH b-ef
      WHERE b-ef.company EQ xef.company
        AND b-ef.est-no  EQ xef.est-no
      USE-INDEX est-qty NO-LOCK
      BY b-ef.form-no:

    RUN mach-loop (3).
  END.
END.

qty = save-qty.

RETURN.

/* end ---------------------------------- copr. 1994  advanced software, inc. */

PROCEDURE mach-loop:

DEF INPUT PARAM ip-int AS INT NO-UNDO.

ll-no-more-blank-fed = NO.

RUN sys/inc/numup.p (b-ef.company, b-ef.est-no, b-ef.form-no, OUTPUT v-num-up).

vn-out = (IF b-ef.n-out   EQ 0 THEN 1 ELSE b-ef.n-out) *
         (IF b-ef.n-out-l EQ 0 THEN 1 ELSE b-ef.n-out-l).

FOR EACH w-qty:
  DELETE w-qty.
END.

v-est-qty[1] = 0.
FOR EACH eb
    WHERE eb.company EQ xest.company
      AND eb.est-no  EQ xest.est-no
      AND eb.form-no EQ b-ef.form-no
    NO-LOCK:
    
  v-eb-qty = IF xest.est-type EQ 2 THEN (eb.bl-qty * eb.cust-%) ELSE
             IF xest.est-type EQ 3 THEN eb.bl-qty ELSE eb.yld-qty.

  IF ip-int EQ 1 THEN
  FOR EACH b-ef1
      WHERE b-ef1.company EQ b-ef.company
        AND b-ef1.est-no  EQ b-ef.est-no
        AND b-ef1.board   EQ b-ef.board
        AND b-ef1.gsh-wid EQ b-ef.gsh-wid
        AND b-ef1.gsh-len EQ b-ef.gsh-len
        AND b-ef1.nsh-wid EQ b-ef.nsh-wid
        AND b-ef1.nsh-len EQ b-ef.nsh-len
        AND b-ef1.trim-w  EQ b-ef.trim-w
        AND b-ef1.trim-l  EQ b-ef.trim-l
        AND b-ef1.n-out   EQ b-ef.n-out
        AND b-ef1.n-out-l EQ b-ef.n-out-l
        AND ROWID(b-ef1)  NE ROWID(b-ef)
        AND NOT CAN-FIND(FIRST b-eb OF b-ef1
                         WHERE b-eb.style NE xeb.style
                            OR b-eb.len   NE xeb.len
                            OR b-eb.wid   NE xeb.wid
                            OR b-eb.dep   NE xeb.dep)
      NO-LOCK:

    RUN sys/inc/numup.p (b-ef1.company, b-ef1.est-no, b-ef1.form-no,
                         OUTPUT v-num-up1).

    IF v-num-up EQ v-num-up1 THEN
    FOR EACH b-eb OF b-ef1
        WHERE b-eb.style  EQ xeb.style
          AND b-eb.len    EQ xeb.len
          AND b-eb.wid    EQ xeb.wid
          AND b-eb.dep    EQ xeb.dep
          AND ROWID(b-eb) NE ROWID(eb)
        NO-LOCK:
      v-eb-qty = v-eb-qty +
                 IF xest.est-type EQ 2 THEN (b-eb.bl-qty * b-eb.cust-%) ELSE
                 IF xest.est-type EQ 3 THEN b-eb.bl-qty ELSE b-eb.yld-qty.
    END.
  END.

  {sys/inc/roundup.i v-eb-qty}

  CREATE w-qty.
  ASSIGN
   w-qty.b-num  = eb.blank-no
   w-qty.num-up = eb.num-up
   w-qty.num-bl = v-eb-qty.

  IF v-eb-qty / eb.num-up GT v-est-qty[1] THEN
    v-est-qty[1] = v-eb-qty / eb.num-up.

  /*{sys/inc/roundup.i w-qty.num-sh}.*/
END.

ASSIGN
 r-spo[b-ef.form-no] = 0
 qty                 = v-est-qty[1].

FOR EACH est-op
    WHERE est-op.company EQ xest.company
      AND est-op.est-no  EQ xest.est-no
      AND est-op.eqty    EQ 0
      AND est-op.s-num   EQ b-ef.form-no
      AND est-op.line    GE ip-line
      AND est-op.line    LT ip-line + 500
    USE-INDEX est-qty
    BREAK BY est-op.d-seq DESC BY est-op.b-num DESC BY est-op.line DESC:

  FIND FIRST xeb
      WHERE xeb.company   EQ est-op.company
        AND xeb.est-no    EQ est-op.est-no
        AND xeb.form-no   EQ est-op.s-num
        AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
      NO-LOCK.

  IF est-op.b-num EQ 0 AND NOT ll-no-more-blank-fed THEN DO:
    FOR EACH w-qty BREAK BY w-qty.num-bl / w-qty.num-up DESC:
      IF FIRST(w-qty.num-bl / w-qty.num-up) THEN
        ASSIGN
         w-qty.b-num  = 0
         w-qty.num-bl = w-qty.num-bl / w-qty.num-up * v-num-up.

      ELSE DELETE w-qty.
    END.

    ll-no-more-blank-fed = YES.
  END.

  FIND FIRST w-qty WHERE w-qty.b-num EQ est-op.b-num.
  ASSIGN
   v-blk    = w-qty.num-bl
   v-sav[2] = w-qty.sav-bl
   spo      = w-qty.spo-bl.

  IF FIRST(est-op.d-seq) OR ip-int EQ 3 THEN
    cumul = v-blk / (v-num-up * (IF b-ef.n-out   EQ 0 THEN 1 ELSE b-ef.n-out) *
                                (IF b-ef.n-out-l EQ 0 THEN 1 ELSE b-ef.n-out-l)).
      
  
    hold-id = ROWID(xef).
    FIND xef WHERE ROWID(xef) EQ ROWID(b-ef) NO-LOCK.

    /*task 03311005
      Add Tandem button OU1, speed, waste, mr need to populate*/

    IF ip-int EQ 2 AND
       index(program-name(5),"est/oeselest") GT 0 AND
       INDEX(PROGRAM-NAME(6),"order-from-est oe/v-ord") GT 0 AND
       est-op.LINE LT 500 THEN
       DO:

            FIND CURRENT xest EXCLUSIVE-LOCK NO-ERROR.
             ASSIGN
               xest.recalc    = YES
               xest.recalc-mr = YES.
          FIND CURRENT xest NO-LOCK. 

       END.

    {ce/box/prokalk.i}
    FIND xef WHERE ROWID(xef) EQ hold-id NO-LOCK.
  
      
  ASSIGN
   w-qty.num-bl = v-blk
   w-qty.sav-bl = v-sav[2]
   w-qty.spo-bl = v-sav[2].

  {sys/inc/roundup.i w-qty.num-bl}

  IF est-op.b-num NE 0 THEN spo = 0.
  
END.

END PROCEDURE.
