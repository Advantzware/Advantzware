
DEF INPUT PARAMETER v-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR v-tax-rate AS DEC NO-UNDO.
DEF VAR v-frt-tax-rate AS DEC NO-UNDO.
DEF VAR v-fr-tax LIKE oe-ctrl.f-tax INIT NO NO-UNDO.
DEF VAR v-call-from-jc AS LOG NO-UNDO.
DEF VAR v-lock-first AS LOG INIT TRUE NO-UNDO.
DEF VAR v-preptax-rate AS DEC NO-UNDO.
DEF VAR v-frt-preptax-rate AS DEC NO-UNDO.
DEF VAR v-tax-amt AS DEC NO-UNDO INIT 0.
DEF VAR lvdT-cost LIKE oe-ord.t-cost NO-UNDO.
DEF VAR lvdT-Revenue LIKE oe-ord.t-revenue NO-UNDO.
DEF VAR lvdTax LIKE oe-ord.tax NO-UNDO.
DEF VAR lvdT-Weight LIKE oe-ord.t-weight NO-UNDO.
DEF VAR lvdT-Freight LIKE oe-ord.t-freight NO-UNDO.

DEF BUFFER bf-oe-ord FOR oe-ord.

FIND oe-ord WHERE ROWID(oe-ord) EQ v-rowid NO-LOCK NO-ERROR.

/*so cust is not locked when it does not need to be since
  jc-calc only updates cost*/
IF INDEX(PROGRAM-NAME(1),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(2),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(3),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(4),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(5),"jc-calc") > 0 THEN
   v-call-from-jc = YES.

IF AVAIL oe-ord THEN DO:

  v-lock-first = YES.

  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ oe-ord.company NO-LOCK NO-ERROR.
  IF AVAIL oe-ctrl THEN v-fr-tax = oe-ctrl.f-tax.

  RUN ar/cctaxrt.p (oe-ord.company, oe-ord.tax-gr,
                    OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

  ASSIGN
    lvdT-Cost    = 0
    lvdT-revenue = 0
    lvdTax       = 0
    lvdT-Weight  = 0
    lvdT-Freight = 0.

  FOR EACH b-oe-ordl OF oe-ord NO-LOCK:

    IF b-oe-ordl.t-cost NE b-oe-ordl.cost * (b-oe-ordl.qty / 1000) THEN DO:    
      FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(b-oe-ordl)
          EXCLUSIVE NO-ERROR NO-WAIT.
      
      IF NOT AVAIL oe-ordl THEN NEXT.
  
      oe-ordl.t-cost = oe-ordl.cost * (oe-ordl.qty / 1000).
    END.
  END.

  FOR EACH oe-ordl OF oe-ord NO-LOCK:
    ASSIGN
     lvdT-Cost    = lvdT-Cost    + oe-ordl.t-cost
     lvdT-Revenue = lvdT-Revenue + oe-ordl.t-price
     lvdT-Weight  = lvdT-Weight  + oe-ordl.t-weight
     lvdT-Freight = lvdT-Freight + oe-ordl.t-freight.

    IF oe-ordl.tax AND v-tax-rate GT 0 THEN DO:

        RUN ar/calctax2.p (oe-ord.tax-gr,
                           NO,
                           oe-ordl.t-price,
                           oe-ord.company,
                           oe-ordl.i-no,
                           OUTPUT v-tax-amt).

        ASSIGN lvdTax = lvdTax + v-tax-amt.
/*             ROUND((oe-ordl.t-price * v-tax-rate) / 100,2). */
    END.
      
  END.

  FOR EACH oe-ordm OF oe-ord NO-LOCK
      WHERE oe-ordm.bill NE "N":

    ASSIGN
     lvdT-Revenue = lvdT-Revenue + oe-ordm.amt
     lvdT-Cost    = lvdT-Cost + oe-ordm.cost.

    RUN ar/cctaxrt.p (oe-ord.company, oe-ordm.spare-char-1,
                    OUTPUT v-preptax-rate, OUTPUT v-frt-preptax-rate).

    IF oe-ordm.tax AND (v-tax-rate > 0 OR v-preptax-rate > 0) THEN DO:

        RUN ar/calctax2.p (oe-ord.tax-gr,
                           NO,
                           oe-ordm.amt,
                           oe-ord.company,
                           oe-ordm.ord-i-no,
                           OUTPUT v-tax-amt).

        ASSIGN lvdTax = lvdTax + v-tax-amt.
    END.
  END.

  ASSIGN
   lvdT-Cost = lvdT-Cost + oe-ord.t-comm.
   lvdT-Cost = lvdT-Cost + lvdT-Freight.

  IF oe-ord.f-bill THEN DO:
    lvdT-Revenue = lvdT-Revenue + lvdT-Freight.

    IF v-fr-tax THEN
      lvdTax = lvdTax +
          ROUND((lvdT-Freight * v-frt-tax-rate) / 100,2).
  END.

  IF  oe-ord.t-cost     NE lvdT-cost      OR
      oe-ord.t-revenue  NE lvdT-Revenue   OR
      oe-ord.tax        NE lvdTax         OR
      oe-ord.t-weight   NE lvdT-Weight    OR
      oe-ord.t-freight  NE lvdT-Freight  THEN DO:
    
    FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord)
      EXCLUSIVE-LOCK.
    ASSIGN
       bf-oe-ord.t-cost    = lvdT-cost  
       bf-oe-ord.t-revenue = lvdT-Revenue  
       bf-oe-ord.tax       = lvdTax  
       bf-oe-ord.t-weight  = lvdT-Weight  
       bf-oe-ord.t-freight = lvdT-Freight  .
    RELEASE bf-oe-ord.
  END.



  IF oe-ord.cust-no NE "" AND
     v-call-from-jc EQ NO AND
     AVAIL cust AND 
     (oe-ord.t-revenue NE lvdT-Revenue OR oe-ord.tax NE lvdTax) THEN
     DO:
        REPEAT:
       
           FIND FIRST cust
                {sys/ref/custW.i}
                AND cust.cust-no EQ oe-ord.cust-no
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
       
           IF AVAIL cust THEN
           DO:
              cust.ord-bal = cust.ord-bal - oe-ord.t-revenue - oe-ord.tax.
              cust.ord-bal = cust.ord-bal + lvdT-Revenue + lvdTax.
              FIND CURRENT cust NO-LOCK.
              LEAVE.
           END.
       
           IF v-lock-first THEN
           DO:
              /*MESSAGE "Customer record in use, waiting for release..."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.*/
       
              session:set-wait-state("General").
              v-lock-first = NO.
           END.
        END. /* Repeat */

        session:set-wait-state("").
     END. /* avail cust */
END. /* if avail oe-ord */
