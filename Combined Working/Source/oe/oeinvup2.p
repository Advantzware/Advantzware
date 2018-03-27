DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-calc-misc AS LOG NO-UNDO.

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF BUFFER b-iline FOR inv-line.

DEF VAR v-tax AS DEC NO-UNDO.
DEF VAR v-total-tax AS DEC NO-UNDO.
DEF VAR v-basis LIKE sman.commbasis NO-UNDO.
DEF VAR v-line AS INT NO-UNDO.
DEF VAR v-charge-code AS cha NO-UNDO.
DEF VAR v-found-cust-charge AS LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF inv-misc.

FIND inv-head WHERE ROWID(inv-head) EQ ip-rowid EXCLUSIVE-LOCK NO-ERROR.

IF AVAIL inv-head THEN DO:
  FIND FIRST cust NO-LOCK
      WHERE cust.company EQ inv-head.company
        AND cust.cust-no EQ inv-head.cust-no
      NO-ERROR.

  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ inv-head.company NO-LOCK NO-ERROR.

  ASSIGN
   inv-head.t-inv-tax  = 0
   inv-head.t-inv-cost = 0
   inv-head.t-comm     = 0
   inv-head.t-inv-rev  = 0
   v-line              = 0.


  /* Find the tax record. */
  FIND FIRST stax WHERE 
        stax.company = inv-head.company AND
        stax.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.

  IF ip-calc-misc AND
     NOT inv-head.multi-invoice THEN
  FOR EACH surcharge NO-LOCK WHERE surcharge.company EQ inv-head.company,
      EACH inv-misc
      WHERE inv-misc.company EQ inv-head.company
        AND inv-misc.r-no    EQ inv-head.r-no
        AND inv-misc.charge  EQ surcharge.charge:
    ASSIGN inv-misc.amt = 0.
  END.

  FOR EACH inv-line WHERE inv-line.r-no EQ inv-head.r-no NO-LOCK:
    IF inv-line.tax THEN DO:
        /* Run the tax rate calculation program. */
        RUN ar/calctax2.p (inv-head.tax-gr,NO,inv-line.t-price,inv-head.company,inv-line.i-no,OUTPUT v-tax).
        ASSIGN inv-head.t-inv-tax = inv-head.t-inv-tax + v-tax. 
    END. /* Tax block */
      
    ASSIGN
     inv-head.t-inv-cost = inv-head.t-inv-cost +
                           ROUND((inv-line.cost *
                                 (inv-line.inv-qty / 1000)),2)
     inv-head.t-comm     = inv-head.t-comm +
                           inv-line.comm-amt[1] +
                           inv-line.comm-amt[2] +
                           inv-line.comm-amt[3]
     inv-head.t-inv-rev  = inv-head.t-inv-rev + inv-line.t-price
     v-line              = v-line + 1.

    /* surcharge mods*/
    IF ip-calc-misc AND NOT inv-head.multi-invoice THEN DO:  
      FIND FIRST itemfg WHERE itemfg.company = inv-head.company
                          AND itemfg.i-no = inv-line.i-no NO-LOCK NO-ERROR.

      IF AVAIL itemfg THEN
      FIND FIRST reftable NO-LOCK
          WHERE reftable.reftable EQ "chargecode"
            AND reftable.company  EQ inv-head.company
            AND reftable.loc      EQ itemfg.procat
          NO-ERROR.
      v-charge-code = IF AVAIL reftable THEN reftable.CODE ELSE "".
   
         v-found-cust-charge = NO.

         FOR EACH surcharge NO-LOCK
             WHERE surcharge.company  EQ inv-head.company
               AND surcharge.calc-on  EQ "I"
               AND surcharge.cust-no EQ inv-head.cust-no:

           RUN inv-surcharge.
           
           v-found-cust-charge = YES.
         END.

         IF v-found-cust-charge = NO THEN
            FOR EACH surcharge NO-LOCK
                WHERE surcharge.company  EQ inv-head.company
                  AND surcharge.calc-on  EQ "I"
                  AND surcharge.cust-no EQ "":
           
              RUN inv-surcharge.
              LEAVE.
            END.
    END.
    /* end of surchage mods*/
  END.

  RELEASE surcharge.

  IF ip-calc-misc AND NOT inv-head.multi-invoice THEN
  FOR EACH surcharge NO-LOCK
      WHERE surcharge.company  EQ inv-head.company
        AND (surcharge.calc-on EQ "C" OR
             surcharge.calc-on EQ "B")
        AND (surcharge.cust-no EQ inv-head.cust-no OR
             surcharge.cust-no EQ "")
      BREAK BY surcharge.charge
            BY surcharge.cust-no DESC:
            
    IF FIRST-OF(surcharge.charge) THEN RUN inv-surcharge.
  END.

  FOR EACH inv-misc WHERE
      inv-misc.company EQ inv-head.company AND
      inv-misc.r-no EQ inv-head.r-no AND
      inv-misc.bill EQ "Y" NO-LOCK:
    
    IF inv-misc.tax THEN DO:
        ASSIGN v-total-tax = 0.
        /* Tax on some other tax group also. */
        IF inv-misc.spare-char-1 <> "" THEN DO:
               RUN ar/calctax2.p (inv-misc.spare-char-1,NO,inv-misc.amt,inv-head.company,inv-misc.inv-i-no,OUTPUT v-tax).
               ASSIGN v-total-tax = v-total-tax + v-tax.
        END.
        ELSE DO:       
            /* Tax on the invoice tax group. */
           RUN ar/calctax2.p (inv-head.tax-gr,NO,inv-misc.amt,inv-head.company,inv-misc.inv-i-no,OUTPUT v-tax).
           ASSIGN v-total-tax = v-total-tax + v-tax.
        END.
        
        /* Assign the total tax amount to invoice header. */
       ASSIGN inv-head.t-inv-tax = inv-head.t-inv-tax + v-total-tax.
    END. /* IF inv-misc.tax  */
  
    ASSIGN
     inv-head.t-inv-rev  = inv-head.t-inv-rev + inv-misc.amt
     inv-head.t-inv-cost = inv-head.t-inv-cost + inv-misc.cost.

    IF oe-ctrl.prep-comm THEN DO:
      FIND FIRST prep NO-LOCK
          WHERE prep.company EQ inv-misc.company
            AND prep.code    EQ inv-misc.charge
          NO-ERROR.
      DO k = 1 TO 3:
        IF inv-misc.s-man[k] NE "" THEN DO:
	      RUN custom/combasis.p (inv-head.company, inv-misc.s-man[k],
                                 (IF AVAIL cust THEN cust.type ELSE ""),
                                 (IF AVAIL prep THEN prep.fgcat ELSE ""), 0,
                                 (IF AVAIL cust THEN cust.cust-no ELSE ""),
                                 OUTPUT v-basis).

          inv-head.t-comm = inv-head.t-comm +
			                 ROUND((inv-misc.amt - IF v-basis EQ "G" THEN inv-misc.cost ELSE 0) *
			                       (inv-misc.s-pct[k] / 100) * (inv-misc.s-comm[k] / 100),2).
        END.
      END.
    END.
  END.

  IF inv-head.f-bill THEN DO:
    RUN ar/calctax.p (inv-head.tax-gr, YES,
                      inv-head.t-inv-freight, OUTPUT v-tax).
    
    ASSIGN
     inv-head.t-inv-tax = inv-head.t-inv-tax + v-tax
     inv-head.t-inv-rev = inv-head.t-inv-rev + inv-head.t-inv-freight.
  END.

  inv-head.t-inv-rev = inv-head.t-inv-rev + inv-head.t-inv-tax.
END.

RETURN.

PROCEDURE inv-surcharge:

  DEF VAR ld-charge AS DEC NO-UNDO.
  DEF VAR ld-factor AS DEC NO-UNDO.

  ld-charge = surcharge.amt / 1000.

  CASE surcharge.calc-on:
    WHEN "I" THEN do:
            ld-factor = ld-factor + ( /*IF ld-charge GE 1 THEN 1 ELSE*/ inv-line.t-price).
    END.
    OTHERWISE
    FOR EACH inv-line WHERE inv-line.r-no EQ inv-head.r-no NO-LOCK,
        FIRST oe-bolh WHERE oe-bolh.b-no EQ inv-line.b-no NO-LOCK
        BREAK BY inv-line.b-no:

      IF FIRST-OF(inv-line.b-no) THEN
        IF surcharge.calc-on EQ "C"                                 OR
           (NOT CAN-FIND(FIRST b-iline
                         WHERE b-iline.b-no EQ inv-line.b-no
                           AND b-iline.r-no LT inv-line.r-no) AND
            NOT CAN-FIND(FIRST ar-invl
                         WHERE ar-invl.company EQ oe-bolh.company
                           AND ar-invl.bol-no  EQ oe-bolh.bol-no))  THEN
          ld-factor = ld-factor + 1.
    END.
  END CASE.

  IF surcharge.calc-on EQ "I" THEN
  ld-charge = ld-charge * ld-factor / 100  .
  ELSE ld-charge = ld-charge * ld-factor . 
  IF ld-charge EQ ? THEN ld-charge = 0.    

  IF ld-charge NE 0 THEN DO:

     FIND FIRST inv-misc
          WHERE inv-misc.company  EQ inv-head.company
            AND inv-misc.r-no     EQ inv-head.r-no                    
            AND inv-misc.charge   EQ surcharge.charge
            AND inv-misc.dscr     EQ surcharge.dscr
          NO-ERROR.

     IF NOT AVAIL inv-misc THEN DO:
       CREATE inv-misc.
       {custom/rec_key.i inv-misc}
       ASSIGN
        inv-misc.company  = inv-head.company
        inv-misc.r-no     = inv-head.r-no
        inv-misc.posted   = NO 
        inv-misc.deleted  = NO
        inv-misc.charge   = surcharge.charge
        inv-misc.dscr     = surcharge.dscr
        inv-misc.bill     = "Y".
        
        FIND FIRST prep NO-LOCK
            WHERE prep.company EQ inv-head.company
            AND prep.code    EQ surcharge.charge
            NO-ERROR.
        IF AVAIL prep THEN
            ASSIGN
            inv-misc.actnum = prep.actnum .
     END.
     
     inv-misc.amt = inv-misc.amt + ld-charge.
  END.

END PROCEDURE.
