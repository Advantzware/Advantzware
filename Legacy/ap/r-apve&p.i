
FOR EACH xap-inv NO-LOCK
    WHERE xap-inv.company  EQ cocode
      AND xap-inv.posted   EQ NO
      AND xap-inv.recur    EQ NO
      AND xap-inv.stat     EQ "R"
      AND xap-inv.inv-date GE v-s-date
      AND xap-inv.inv-date LE v-e-date 
      AND xap-inv.vend-no  GE v-s-vend
      AND xap-inv.vend-no  LE v-e-vend
      AND xap-inv.user-id  GE begin_user
      AND xap-inv.user-id  LE end_user
      AND CAN-FIND(FIRST ap-invl WHERE ap-invl.i-no EQ xap-inv.i-no
                   USE-INDEX i-no)
      AND NOT CAN-FIND(FIRST ap-invl WHERE ap-invl.i-no   EQ xap-inv.i-no
                                       AND ap-invl.actnum EQ ""
                       USE-INDEX i-no)
    USE-INDEX posted
    TRANSACTION:
    
  FIND FIRST ap-inv WHERE ROWID(ap-inv) EQ ROWID(xap-inv)
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

  IF AVAIL ap-inv THEN DO:
     v-upd = YES.
    
     FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no NO-LOCK,
     
         FIRST po-ordl NO-LOCK
         WHERE po-ordl.company   EQ cocode
           AND po-ordl.po-no     EQ (IF ap-invl.po-no EQ 0 THEN ap-inv.po-no
                                                           ELSE ap-invl.po-no)
           AND po-ordl.line      EQ {ap/invlline.i -1}
           AND po-ordl.item-type EQ NO
         USE-INDEX po-no:
         
       v-po-no = TRIM(STRING(po-ordl.po-no,">>>>>>>>>>")).
           
       FIND FIRST fg-rcpth NO-LOCK
           WHERE fg-rcpth.company   EQ cocode
             AND fg-rcpth.i-no      EQ po-ordl.i-no
             AND fg-rcpth.po-no     EQ v-po-no
             AND fg-rcpth.rita-code EQ "R"
           USE-INDEX item-po NO-ERROR.
       IF NOT AVAIL fg-rcpth THEN DO:
         v-upd = NO.
         LEAVE.
       END.  
       
       FIND FIRST fg-rcpts NO-LOCK
           WHERE fg-rcpts.company   EQ cocode
             AND fg-rcpts.i-no      EQ po-ordl.i-no
             AND fg-rcpts.po-no     EQ v-po-no
             AND fg-rcpts.rita-code EQ "R"
           USE-INDEX i-no NO-ERROR.
       IF AVAIL fg-rcpts THEN DO:
         v-upd = NO. 
         LEAVE.
       END.
     END.  
    
     IF v-upd THEN DO:
       CREATE tt-report.
       tt-report.rec-id = RECID(ap-inv).
     END.
  END.
END.
    
FIND CURRENT ap-inv NO-LOCK NO-ERROR.

FOR EACH tt-report,
    FIRST ap-inv WHERE RECID(ap-inv) EQ tt-report.rec-id NO-LOCK
    BREAK BY ap-inv.vend-no
          BY ap-inv.inv-no          
    WITH FRAME a1 STREAM-IO:

  v-postable = YES.  
  IF FIRST-OF(ap-inv.vend-no) THEN DO:
    FIND FIRST vend NO-LOCK
        WHERE vend.company eq cocode
          AND vend.vend-no eq ap-inv.vend-no
        USE-INDEX vend NO-ERROR.
        
    PUT vend.vend-no
        SPACE(1)
        vend.name.
  END.
  
  ELSE
  IF FIRST-OF(ap-inv.inv-no) THEN PUT SKIP(1).
    
  RELEASE currency.
  IF lv-comp-curr NE "" AND lv-comp-curr NE ap-inv.curr-code[1] THEN
  FIND FIRST currency NO-LOCK
      WHERE currency.company     EQ ap-inv.company
        AND currency.c-code      EQ ap-inv.curr-code[1]
        AND currency.ar-ast-acct NE ""
        AND currency.ex-rate     GT 0
      NO-ERROR.

  IF AVAIL currency THEN
    ASSIGN
     tt-report.actnum  = currency.ar-ast-acct
     tt-report.ex-rate = currency.ex-rate.

  tt-report.curr-amt = (ap-inv.net + ap-inv.freight) * tt-report.ex-rate.
  
  PUT ap-inv.inv-no         TO 55
      ap-inv.inv-date       AT 60 FORMAT "99/99/99"
      ap-inv.due-date       AT 71 FORMAT "99/99/99"                       
      tt-report.curr-amt    TO 94 FORMAT "->,>>>,>>9.99".

  ASSIGN
   v2 = v2 + tt-report.curr-amt
   v1 = v1 + ap-inv.disc-taken * tt-report.ex-rate.
   
  FOR EACH ap-invl WHERE ap-invl.i-no EQ ap-inv.i-no NO-LOCK USE-INDEX i-no
      WITH FRAME a2 NO-BOX NO-LABELS WIDTH 132:

    CREATE tt-ap-invl.
    ASSIGN
     tt-ap-invl.row-id  = ROWID(ap-invl)
     tt-ap-invl.actnum  = ap-invl.actnum
     tt-ap-invl.unit-pr = ap-invl.unit-pr
     tt-ap-invl.amt     = ap-invl.amt.

    IF AVAIL currency THEN
      ASSIGN
       tt-ap-invl.unit-pr = tt-ap-invl.unit-pr * currency.ex-rate
       tt-ap-invl.amt     = tt-ap-invl.amt     * currency.ex-rate.
      
    PUT ap-invl.actnum AT 96 FORMAT "x(19)"
        SPACE(1)
        tt-ap-invl.amt SKIP.
        
    IF v-prt-dscr THEN DO:
      FIND FIRST account NO-LOCK
          WHERE account.company EQ cocode
            AND account.actnum  EQ ap-invl.actnum
          NO-ERROR.
      IF AVAIL account THEN PUT account.dscr AT 90 FORMAT "x(40)" SKIP.
    END.   
  END. /* each ap-invl */

  ACCUM ap-inv.freight * tt-report.ex-rate (TOTAL BY ap-inv.vend-no).

  IF ap-inv.tax-amt NE 0 THEN DO:
    FIND FIRST stax
        WHERE stax.company   EQ ap-inv.company
          AND stax.tax-group EQ ap-inv.tax-gr
        NO-LOCK NO-ERROR.

    IF AVAIL stax THEN DO:
      DEF VAR tot-tax AS DECIMAL NO-UNDO.
      DEF VAR ws_taxacct AS CHAR NO-UNDO.
      DEF VAR last_one AS INTEGER NO-UNDO.
      DEF VAR v-jd-taxamt AS DECIMAL NO-UNDO.
      DEF VAR v-tax-rate AS DECIMAL NO-UNDO DECIMALS 8.
      ASSIGN
         v-tax-rate = 0
         tot-tax = ap-inv.tax-amt.

      DO i = 1 TO extent(stax.tax-rate1):
        v-tax-rate = v-tax-rate + stax.tax-rate1[i].
        IF stax.tax-rate1[i] NE 0 THEN last_one = i.
      END.
      DO i = 1 TO extent(stax.tax-rate1):
        IF stax.tax-rate1[i] NE 0 THEN DO:
          FIND account NO-LOCK
              WHERE account.company = cocode
                AND account.actnum = stax.tax-acc1[i]
              NO-ERROR.
          ASSIGN
           ws_taxacct  = IF AVAIL account THEN stax.tax-acc1[i] ELSE xap-stax
           v-jd-taxamt = ROUND((stax.tax-rate1[i] / v-tax-rate) * ap-inv.tax-amt,2)
           tot-tax     = tot-tax - v-jd-taxamt.
          /* add in any residual amount */
          IF i EQ last_one THEN v-jd-taxamt = v-jd-taxamt + tot-tax.

          CREATE tt-ap-tax.
          ASSIGN
           tt-ap-tax.row-id   = ROWID(ap-inv)
           tt-ap-tax.actnum   = ws_taxacct
           tt-ap-tax.amt      = v-jd-taxamt
           tt-ap-tax.curr-amt = v-jd-taxamt *
                                (IF AVAIL currency THEN currency.ex-rate ELSE 1).

          PUT tt-ap-tax.actnum AT 96 FORMAT "x(19)"
              SPACE(1)
              tt-ap-tax.curr-amt SKIP.
        END.
      END.
    END.

    ELSE DO:
      CREATE tt-ap-tax.
      ASSIGN
       tt-ap-tax.row-id   = ROWID(ap-inv)
       tt-ap-tax.actnum   = xap-stax
       tt-ap-tax.amt      = ap-inv.tax-amt
       tt-ap-tax.curr-amt = ap-inv.tax-amt *
                            (IF AVAIL currency THEN currency.ex-rate ELSE 1).

      PUT tt-ap-tax.actnum AT 96 FORMAT "x(19)"
          SPACE(1)
          tt-ap-tax.curr-amt SKIP.
    END.
  END.  /* non-zero tax amount */
  
  IF LAST-OF(ap-inv.vend-no) THEN DO:
    IF (ACCUM TOTAL BY ap-inv.vend-no ap-inv.freight * tt-report.ex-rate) NE 0 THEN DO:
      PUT v-frt-acct AT 96 FORMAT "x(19)"
          SPACE(1)            
          (ACCUM TOTAL BY ap-inv.vend-no ap-inv.freight * tt-report.ex-rate) 
                     TO 127 FORMAT "->>,>>>,>>9.99" SKIP.

      IF v-prt-dscr THEN DO:
        FIND FIRST account NO-LOCK
            WHERE account.company EQ cocode
              AND account.actnum  EQ v-frt-acct
            NO-ERROR.
        IF AVAIL account THEN PUT account.dscr AT 90 FORMAT "x(40)" SKIP.
      END.   
    END.

    DISPLAY "* VENDOR TOTAL " TO 113
            v2                TO 127 "*"
            SKIP(1)
        WITH FRAME vtot NO-BOX NO-LABELS WIDTH 132 STREAM-IO.
        
    ASSIGN    
     g1 = g1 + v1
     g2 = g2 + v2
     v1 = 0
     v2 = 0.
  END.
END. /* each invoice */

display  "** GRAND TOTAL "  to 113  g2 to 127 "**"
    with no-labels no-underline width 132 frame GT STREAM-IO.

hide frame f-top.

ASSIGN
   str-tit3 = "Period " + string(tran-period,"99") + " - " + "Summary by Account"
   x = (132 - length(str-tit3)) / 2
   str-tit3 = fill(" ",x) + str-tit3.

page.

form header
     "ACCOUNT                             PO#   DATE   VENDOR#  INVOICE#    "
     "LINE DESCRIPTION              QTY    UNIT PRICE       AMOUNT" skip
     fill("_",132) format "x(132)"
  
    with no-labels no-box no-underline frame f-top2 page-top width 132 STREAM-IO .

display "" with frame f-top2.

v-loop = 1.

for each tt-report,

    first ap-inv where recid(ap-inv) eq tt-report.rec-id NO-LOCK,
    
    first vend
    where vend.company eq cocode
      and vend.vend-no eq ap-inv.vend-no
    USE-INDEX vend NO-LOCK
    
    break by ap-inv.vend-no:

  for each ap-invl
      where ap-invl.i-no    eq ap-inv.i-no
        and ap-invl.posted  eq no
      USE-INDEX i-no NO-LOCK,

      FIRST tt-ap-invl WHERE tt-ap-invl.row-id EQ ROWID(ap-invl):

    find first po-ordl NO-LOCK
        where po-ordl.company eq cocode
          and po-ordl.po-no   eq (if ap-invl.po-no eq 0 then ap-inv.po-no
                                                        else ap-invl.po-no)
          and po-ordl.line    eq {ap/invlline.i -1}
        USE-INDEX po-no no-error.

    IF AVAIL po-ordl AND po-ordl.item-type AND
       ap-invl.amt NE 0 AND rmpostgl       THEN DO:
      FIND FIRST item
          WHERE item.company EQ cocode
            AND item.i-no    EQ po-ordl.i-no
          NO-LOCK NO-ERROR.

      RELEASE costtype.
      IF AVAIL item THEN
      FIND FIRST costtype
          WHERE costtype.company   EQ cocode
            AND costtype.cost-type EQ item.cost-type
          NO-LOCK NO-ERROR.

      IF AVAIL costtype AND costtype.ap-accrued NE "" THEN /* Debit RM AP Accrued */
        tt-ap-invl.actnum = costtype.ap-accrued.
    END.
  end.
    
  if ap-inv.freight ne 0 then do:
    if v-loop eq 1 then do:
      v-loop = 2.
      find first account
          where account.company eq cocode
            and account.actnum  eq v-frt-acct
          NO-LOCK no-error.
      put v-frt-acct + " - " + account.dscr format "x(40)".
    end.
    
    put ap-inv.inv-date at 41 FORMAT "99/99/99"       space(1)
        ap-inv.vend-no              space(1)
        ap-inv.inv-no               space(6)
        "Freight"    format "x(18)" space(7)
        1.0          format "9.9"   space(1)
        ap-inv.freight              to 118
        ap-inv.freight              to 131
        skip.
  end.
  
  accumulate ap-inv.freight (total).
  
  if last(ap-inv.vend-no) and (accum total ap-inv.freight) ne 0 then
    put "** TOTAL " to 113
        (accum total ap-inv.freight) format "->>,>>>,>>9.99" to 127
        " *" skip(1).
end.

FOR EACH tt-ap-tax,

    FIRST ap-inv WHERE ROWID(ap-inv) EQ tt-ap-tax.row-id NO-LOCK

    BREAK BY tt-ap-tax.actnum
          BY ap-inv.inv-no:
      
  IF FIRST-OF(tt-ap-tax.actnum) THEN DO:
    FIND FIRST account
        WHERE account.company EQ cocode
          AND account.actnum  EQ tt-ap-tax.actnum
        NO-LOCK NO-ERROR.
        
    PUT tt-ap-tax.actnum + " - " + account.dscr FORMAT "x(40)" SKIP.
  END.
    
  PUT ap-inv.inv-date at 41 FORMAT "99/99/99"       SPACE(1)
      ap-inv.vend-no              SPACE(1)
      ap-inv.inv-no               SPACE(6)
      "Tax"        FORMAT "x(18)" SPACE(7)
      1.0          FORMAT "9.9"   SPACE(1)
      tt-ap-tax.amt               TO 118
      tt-ap-tax.amt               TO 131
      SKIP.
  
  ACCUMULATE tt-ap-tax.amt (TOTAL).
  
  IF LAST(tt-ap-tax.actnum) AND (ACCUM TOTAL tt-ap-tax.amt) NE 0 THEN
    PUT "** TOTAL " TO 113
        (ACCUM TOTAL tt-ap-tax.amt) FORMAT "->>,>>>,>>9.99" TO 127
        " *" SKIP(1).
END.

for each tt-report,

    first ap-inv where recid(ap-inv) eq tt-report.rec-id NO-LOCK,
    
    each ap-invl
    where ap-invl.i-no    eq ap-inv.i-no
      and ap-invl.posted  eq no
    USE-INDEX i-no NO-LOCK,

    first tt-ap-invl where tt-ap-invl.row-id eq rowid(ap-invl)

    break by tt-ap-invl.actnum
          by ap-inv.inv-no
          by ap-invl.line:

  find first vend
      where vend.company eq cocode
        and vend.vend-no eq ap-inv.vend-no
      USE-INDEX vend NO-LOCK no-error.
      
  if first-of(tt-ap-invl.actnum) then do:
    find first account
        where account.company eq cocode
          and account.actnum  eq tt-ap-invl.actnum
        NO-LOCK no-error.
        
    put tt-ap-invl.actnum + " - " + account.dscr format "x(40)" skip.
  end.
  
  put ap-invl.po-no         at 34
      space(1)
      ap-inv.inv-date       FORMAT "99/99/99"
      space(1)
      ap-inv.vend-no
      space(1)
      ap-inv.inv-no
      space(1)
      {ap/invlline.i -1}    format ">>>>"
      space(1)
      ap-invl.dscr          format "x(18)"
      space(1)
      ap-invl.qty           format "->>,>>9.9<<"
      space(1)
      ap-invl.unit-pr
      space(1)
      ap-invl.amt
      space(1)
      skip.
      
  accumulate ap-invl.amt (total by tt-ap-invl.actnum).
  accumulate ap-invl.amt (total).
  
  if last-of(tt-ap-invl.actnum) then
    put "** TOTAL " to 113
        accum total BY tt-ap-invl.actnum ap-invl.amt format "->>,>>>,>>9.99" to 127
        " *" skip(1).
end.

for each tt-report WHERE tt-report.actnum NE "",

    first ap-inv where recid(ap-inv) eq tt-report.rec-id NO-LOCK

    break by tt-report.actnum
          by ap-inv.inv-no:

  find first vend
      where vend.company eq cocode
        and vend.vend-no eq ap-inv.vend-no
      USE-INDEX vend NO-LOCK no-error.
      
  if first-of(tt-report.actnum) then do:
    find first account
        where account.company eq cocode
          and account.actnum  eq tt-report.actnum
        NO-LOCK no-error.
        
    put tt-report.actnum + " - " + account.dscr format "x(40)" skip.
  end.
  
  put ap-inv.inv-date       AT 41 FORMAT "99/99/99"
      space(1)
      ap-inv.vend-no
      space(1)
      ap-inv.inv-no
      space(51)
      tt-report.curr-amt - (ap-inv.net + ap-inv.freight)
      space(1)
      skip.
      
  accumulate tt-report.curr-amt - (ap-inv.net + ap-inv.freight) (total by tt-report.actnum).
  accumulate tt-report.curr-amt - (ap-inv.net + ap-inv.freight) (total).
  
  if last-of(tt-report.actnum) then
    put "** TOTAL " to 113
        accum total BY tt-report.actnum tt-report.curr-amt - (ap-inv.net + ap-inv.freight) format "->>,>>>,>>9.99" to 127
        " *" skip(1).
end.

put "***** TOTAL for ALL ACCOUNTS " to 113
    (accum total ap-invl.amt) +
      (accum total ap-inv.freight) +
        (accum total tt-ap-tax.amt) +
          (accum total tt-report.curr-amt - (ap-inv.net + ap-inv.freight))
                                    format "->>,>>>,>>9.99" to 127 skip(2).
    
lv-frt-total = (accum total ap-inv.freight).

HIDE FRAME f-top.
