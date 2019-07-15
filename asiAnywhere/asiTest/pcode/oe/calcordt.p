
DEF INPUT PARAMETER v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR v-tax-rate AS DEC NO-UNDO.
DEF VAR v-frt-tax-rate AS DEC NO-UNDO.
DEF VAR v-fr-tax LIKE oe-ctrl.f-tax INIT NO NO-UNDO.
DEF VAR v-call-from-jc AS LOG NO-UNDO.
DEF VAR v-lock-first AS LOG INIT TRUE NO-UNDO.

FIND oe-ord WHERE ROWID(oe-ord) EQ v-rowid EXCLUSIVE NO-ERROR.

/*so cust is not locked when it does not need to be since
  jc-calc only updates cost*/
IF INDEX(PROGRAM-NAME(1),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(2),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(3),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(4),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(5),"jc-calc") > 0 THEN
   v-call-from-jc = YES.

IF AVAIL oe-ord THEN DO:

  IF oe-ord.cust-no NE "" AND v-call-from-jc EQ NO THEN
  DO:
     REPEAT:
  
        FIND FIRST cust
             where (cust.company = cocode)
             AND cust.cust-no EQ oe-ord.cust-no
             EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

        IF AVAIL cust THEN
        DO:
           cust.ord-bal = cust.ord-bal - oe-ord.t-revenue - oe-ord.tax.
           FIND CURRENT cust NO-LOCK.
           LEAVE.
        END.

        IF v-lock-first THEN
        DO:
           MESSAGE "Customer record in use, waiting for release..."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           session:set-wait-state("General").
           v-lock-first = NO.
        END.
     END.

     session:set-wait-state("").
  END.
  
  v-lock-first = YES.

  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ oe-ord.company NO-LOCK NO-ERROR.
  IF AVAIL oe-ctrl THEN v-fr-tax = oe-ctrl.f-tax.

  RUN ar/cctaxrt.p (oe-ord.company, oe-ord.tax-gr,
                    OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

  ASSIGN
   oe-ord.t-cost    = 0
   oe-ord.t-revenue = 0
   oe-ord.tax       = 0
   oe-ord.t-weight  = 0
   oe-ord.t-freight = 0.

  FOR EACH b-oe-ordl OF oe-ord NO-LOCK:
    FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(b-oe-ordl)
        EXCLUSIVE NO-ERROR NO-WAIT.
    
    IF NOT AVAIL oe-ordl THEN NEXT.

    oe-ordl.t-cost = oe-ordl.cost * (oe-ordl.qty / 1000).
  END.

  FOR EACH oe-ordl OF oe-ord NO-LOCK:
    ASSIGN
     oe-ord.t-cost    = oe-ord.t-cost    + oe-ordl.t-cost
     oe-ord.t-revenue = oe-ord.t-revenue + oe-ordl.t-price
     oe-ord.t-weight  = oe-ord.t-weight  + oe-ordl.t-weight
     oe-ord.t-freight = oe-ord.t-freight + oe-ordl.t-freight.

    IF oe-ordl.tax AND v-tax-rate GT 0 THEN
      oe-ord.tax = oe-ord.tax + ROUND((oe-ordl.t-price * v-tax-rate) / 100,2).
  END.

  FOR EACH oe-ordm OF oe-ord NO-LOCK
      WHERE oe-ordm.bill NE "N":

    ASSIGN
     oe-ord.t-revenue = oe-ord.t-revenue + oe-ordm.amt
     oe-ord.t-cost    = oe-ord.t-cost + oe-ordm.cost.

    IF oe-ordm.tax AND v-tax-rate > 0 THEN
      oe-ord.tax = oe-ord.tax + ROUND((oe-ordm.amt * v-tax-rate) / 100,2).
  END.

  ASSIGN
   oe-ord.t-cost = oe-ord.t-cost + oe-ord.t-comm.
   oe-ord.t-cost = oe-ord.t-cost + oe-ord.t-freight.

  IF oe-ord.f-bill THEN DO:
    oe-ord.t-revenue = oe-ord.t-revenue + oe-ord.t-freight.

    IF v-fr-tax THEN
      oe-ord.tax = oe-ord.tax +
          ROUND((oe-ord.t-freight * v-frt-tax-rate) / 100,2).
  END.

  IF oe-ord.cust-no NE "" AND
     v-call-from-jc EQ NO AND
     AVAIL cust THEN
     DO:
        REPEAT:
       
           FIND FIRST cust
                where (cust.company = cocode)
                AND cust.cust-no EQ oe-ord.cust-no
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
       
           IF AVAIL cust THEN
           DO:
              cust.ord-bal = cust.ord-bal + oe-ord.t-revenue + oe-ord.tax.
              FIND CURRENT cust NO-LOCK.
              LEAVE.
           END.
       
           IF v-lock-first THEN
           DO:
              MESSAGE "Customer record in use, waiting for release..."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       
              session:set-wait-state("General").
              v-lock-first = NO.
           END.
        END.

        session:set-wait-state("").
     END.
END.
