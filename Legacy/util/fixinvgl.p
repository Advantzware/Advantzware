
{sys/inc/VAR.i NEW SHARED}

{oe/invwork.i NEW}

def new shared var v-ar-acct like ar-ctrl.receivables.
def new shared var v-ar-freight like ar-ctrl.freight.
def new shared var v-ar-stax like ar-ctrl.stax.
def new shared var v-ar-sales like ar-ctrl.sales.
def new shared var v-ar-disc like ar-ctrl.discount.
def new shared var v-return as log init no.

DEF VAR v-u-cost LIKE ar-invl.cost NO-UNDO.
DEF VAR v-t-cost LIKE ar-invl.t-cost NO-UNDO.
def var v-inv-disc as dec format "->>,>>9.99".
def var v-line-tot like inv-line.t-price.
def var v-misc-tot like ar-invl.amt.
def var v-cas-cnt like itemfg.case-count.
def var v-cost as dec extent 4.
def var v-invl-pric as dec.
def var v-tax-rate as dec extent 4.
DEFINE VARIABLE cCostUOM AS CHARACTER.
DEFINE VARIABLE cCostSource AS CHARACTER.


FOR EACH ar-ledger
    WHERE company EQ '001'
      AND ref-num BEGINS 'inv'
      AND tr-num  EQ 49621
    NO-LOCK,
    
    FIRST period
    WHERE period.company EQ ar-ledger.company
      AND period.pstat   EQ YES
      AND period.pst     LE ar-ledger.tr-date
      AND period.pend    GE ar-ledger.tr-date
    NO-LOCK,

    FIRST ar-inv
    WHERE ar-inv.company EQ ar-ledger.company
      AND ar-inv.posted  EQ YES
      AND ar-inv.cust-no EQ ar-ledger.cust-no
      AND ar-inv.inv-no  EQ INT(SUBSTR(ar-ledger.ref-num,6,LENGTH(ar-ledger.ref-num)))
    USE-INDEX posted NO-LOCK
    
    BREAK BY tr-num:

        ASSIGN
         cocode     = ar-ledger.company
         v-inv-disc = 0
         v-line-tot = 0
         v-misc-tot = 0.

        RUN oe/getacct.p.

        FIND FIRST ar-ctrl WHERE ar-ctrl.company EQ cocode NO-LOCK.

/************ line ITEMS ************************************************/
        FOR EACH ar-invl
            WHERE ar-invl.x-no EQ ar-inv.x-no
              AND ar-invl.misc EQ NO
            NO-LOCK,

            FIRST itemfg
            {sys/look/itemfgrlW.i}
              AND itemfg.i-no eq ar-invl.i-no
            NO-LOCK,

            FIRST fgcat
            WHERE fgcat.company EQ cocode
              and fgcat.procat  EQ itemfg.procat
            NO-LOCK:

          FIND FIRST uom
              WHERE uom.uom  EQ ar-invl.pr-uom
                AND uom.mult NE 0
              NO-LOCK NO-ERROR.

          FIND FIRST oe-ordl
               WHERE oe-ordl.company EQ cocode
                 AND oe-ordl.ord-no  EQ ar-invl.ord-no
                 AND oe-ordl.i-no    EQ ar-invl.i-no
               USE-INDEX ord-no NO-LOCK NO-ERROR.
          
          ASSIGN
           v-cas-cnt = if ar-invl.cas-cnt ne 0 then
                         ar-invl.cas-cnt
                       else
                       if avail oe-ordl and oe-ordl.cas-cnt ne 0 then
                         oe-ordl.cas-cnt
                       else
                       if avail itemfg and itemfg.case-count ne 0 then
                         itemfg.case-count
                       else 1.

          run oe/GetCostInvl.p (ROWID(ar-invl),
                             output v-cost[1], output v-cost[2],
                             output v-cost[3], output v-cost[4],
                             output v-u-cost, OUTPUT cCostUOM, 
                             output v-t-cost, OUTPUT cCostSource).

          run oe/invposty.p (ar-inv.inv-no, ar-invl.i-no, ar-invl.inv-qty,
                             "M", v-cost[1], v-cost[2], v-cost[3], v-cost[4]).

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-line"
           tt-report.key-02  = if avail fgcat and fgcat.glacc ne ""
                               then fgcat.glacc else v-ar-sales
           tt-report.key-03  = string(ar-inv.inv-no,"999999")
           tt-report.key-04  = ar-invl.i-no
           v-invl-pric       = ar-invl.amt.

          IF ar-invl.disc NE 0 THEN
            ASSIGN
             v-invl-pric = ROUND((if ar-invl.pr-uom begins "L" then
                                    if ar-invl.inv-qty lt 0 then -1 else 1
                                  else
                                  if ar-invl.pr-uom eq "CS" then
                                    ar-invl.inv-qty / v-cas-cnt
                                  else
                                  if avail uom then
                                    ar-invl.inv-qty / uom.mult
                                  else
                                    ar-invl.inv-qty / 1000) *
                                 ar-invl.unit-pr,2)
             v-inv-disc  = v-inv-disc + (v-invl-pric - ar-invl.amt).

          tt-report.key-05 = STRING(v-invl-pric).
          
          v-line-tot = v-line-tot + ar-invl.amt.
        END. /* each ar-invl */

  /******************* MISCELLANEOUS ITEMS ***********************************/
  /* Be aware that job nos are not stroed in ar-invl records for misc charges*/

        FOR EACH ar-invl
            WHERE ar-invl.x-no     EQ ar-inv.x-no
              AND ar-invl.misc     EQ YES
              AND ar-invl.billable EQ YES
            NO-LOCK:

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-misc"
           tt-report.key-02  = if ar-invl.actnum ne ""
                               then ar-invl.actnum else v-ar-sales
           tt-report.key-03  = string(ar-inv.inv-no,"999999")
           tt-report.key-04  = ar-invl.prep-charge
           tt-report.key-05  = string(ar-invl.amt).

          v-misc-tot = v-misc-tot + ar-invl.amt.
        END. /* each ar-invl */

        v-post-disc = v-post-disc + v-inv-disc.

  /******************* MISCELLANEOUS ITEMS ***********************************/
        create tt-report.
        assign
         tt-report.term-id = ""
         tt-report.key-01  = "work-disc"
         tt-report.key-02  = string(ar-inv.inv-no,"999999")
         tt-report.key-05  = string(v-inv-disc).

        if ar-inv.tax-amt ne 0 then do:
          if ar-inv.tax-code ne "" then do:
            find first stax
                {sys/ref/stax1W.i}
                  and {sys/ref/taxgroup.i stax} eq ar-inv.tax-code
                no-lock no-error.
            if not avail stax then
            find first stax
                where stax.company = ar-inv.company AND
                stax.tax-group eq ar-inv.tax-code
                no-lock no-error.
            
            if avail stax then do:
              do i = 1 to 3:
                v-tax-rate[i] = stax.tax-rate[i].
                
                if stax.company eq "yes" and i gt 1 then
                do k = 1 to i - 1:
                  v-tax-rate[i] = v-tax-rate[i] +
                                  (v-tax-rate[i] * (stax.tax-rate[k] / 100)).
                end.
              end.
              
              v-tax-rate[4] = v-tax-rate[1] + v-tax-rate[2] + v-tax-rate[3].
              
              do i = 1 to 3:
                v-tax-rate[i] = round(v-tax-rate[i] / v-tax-rate[4] *
                                      ar-inv.tax-amt,2).
              end.
              
              v-tax-rate[4] = v-tax-rate[1] + v-tax-rate[2] + v-tax-rate[3].
              
              if ar-inv.tax-amt ne v-tax-rate[4] then
                v-tax-rate[1] = v-tax-rate[1] +
                                (ar-inv.tax-amt - v-tax-rate[4]).
              
              do i = 1 to 3:
                find first account
                    where account.company eq cocode
                      and account.actnum  eq stax.tax-acc[i]
                    no-lock no-error.
                    
                if avail account then do:
                  create tt-report.
                  assign
                   tt-report.term-id = ""
                   tt-report.key-01  = "work-tax"
                   tt-report.key-02  = account.actnum
                   tt-report.key-03  = string(ar-inv.inv-no,"999999")
                   tt-report.key-04  = ar-inv.tax-code
                   tt-report.key-05  = string(v-tax-rate[i]).
                end. /* avail account */

                
              end. /* 1 to 3 */

            end. /* avail stax */
          end.

          else do:
            find first account
                where account.company eq cocode
                  and account.actnum  eq v-ar-stax
                no-lock no-error.
            create tt-report.
            assign
             tt-report.term-id = ""
             tt-report.key-01  = "work-tax"
             tt-report.key-02  = account.actnum
             tt-report.key-03  = string(ar-inv.inv-no,"999999")
             tt-report.key-05  = string(ar-inv.tax-amt).
          end.
        end.
       
        v-post-total = v-post-total + ar-inv.gross.

        /** if Freight Is Billable then Post to GL **/
        if ar-inv.f-bill then do:
          v-post-freight = v-post-freight - ar-inv.freight.

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-freight"
           tt-report.key-02  = string(ar-inv.inv-no,"999999")
           tt-report.key-05  = string(- ar-inv.freight).
        end.

        if ar-inv.terms eq "CASH" then do:
          assign
           v-post-cash  = v-post-cash  + ar-inv.gross
           v-post-total = v-post-total - ar-inv.gross.

          create tt-report.
          assign
           tt-report.term-id = ""
           tt-report.key-01  = "work-cash"
           tt-report.key-02  = string(ar-inv.inv-no,"999999")
           tt-report.key-05  = string(ar-inv.gross).
        end.

  IF LAST-OF(tr-num) THEN DO:
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-line"
        no-lock
        break by tt-report.key-02:

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).

      if last-of(tt-report.key-02) then do:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = tt-report.key-02
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = "ORDER ENTRY INVOICE LINES"
         gltrans.tr-date = ar-ledger.tr-date
         gltrans.tr-amt  = - (accumulate total by tt-report.key-02 dec(tt-report.key-05))
         gltrans.period  = period.pnum
         gltrans.trnum   = ar-ledger.tr-num.
      end. /* last actnum */
    end. /* each work-line */
                                              /** POST MISC. TO G/L TRANS **/
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-misc"
        no-lock
        break by tt-report.key-02:

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).

      if last-of(tt-report.key-02) then do:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = "ORDER ENTRY INVOICE MISC."
         gltrans.tr-date = ar-ledger.tr-date
         gltrans.actnum  = tt-report.key-02
         gltrans.tr-amt  = - (accumulate total by tt-report.key-02 dec(tt-report.key-05))
         gltrans.period  = period.pnum
         gltrans.trnum   = ar-ledger.tr-num.
      end. /* last actnum */
    end. /* each work-misc */
                                           /** POST SALES TAX TO G/L TRANS **/
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "work-tax"
        no-lock
        break by tt-report.key-02:

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).

      if last-of(tt-report.key-02) then do:
        create gltrans.
        assign
         gltrans.company = cocode
         gltrans.actnum  = tt-report.key-02
         gltrans.jrnl    = "OEINV"
         gltrans.tr-dscr = "ORDER ENTRY INVOICE TAX"
         gltrans.tr-date = ar-ledger.tr-date
         gltrans.tr-amt  = - (accumulate total by tt-report.key-02 dec(tt-report.key-05))
         gltrans.period  = period.pnum
         gltrans.trnum   = ar-ledger.tr-num.
      end. /* last actnum */
    end. /* each work-tax */

    for each work-job break by work-job.actnum:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-job.actnum
       gltrans.jrnl    = "OEINV"
       gltrans.tr-date = ar-ledger.tr-date
       gltrans.period  = period.pnum
       gltrans.trnum   = ar-ledger.tr-num.

      if work-job.fg then
        assign
         gltrans.tr-amt  = - work-job.amt
         gltrans.tr-dscr = "ORDER ENTRY INVOICE FG".
      else
        assign
         gltrans.tr-amt  = work-job.amt
         gltrans.tr-dscr = "ORDER ENTRY INVOICE COGS".
    end. /* each work-job */

                                          /** POST FREIGHT TO G/L TRANS **/
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-ar-freight
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE FREIGHT"
     gltrans.tr-date = ar-ledger.tr-date
     gltrans.tr-amt  = v-post-freight
     gltrans.period  = period.pnum
     gltrans.trnum   = ar-ledger.tr-num.
     
                                           /** POST DISCOUNT TO G/L TRANS **/
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-ar-disc
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE DISCOUNT"
     gltrans.tr-date = ar-ledger.tr-date
     gltrans.tr-amt  = v-post-disc
     gltrans.period  = period.pnum
     gltrans.trnum   = ar-ledger.tr-num.
                                           /** POST CASH TO G/L TRANS **/
    if v-post-cash ne 0 then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = ar-ctrl.cash-act
       gltrans.jrnl    = "CASHR"
       gltrans.tr-dscr = "CASH RECEIPT - INVOICE"
       gltrans.tr-date = ar-ledger.tr-date
       gltrans.tr-amt  = v-post-cash
       gltrans.period  = period.pnum
       gltrans.trnum   = ar-ledger.tr-num.
    end.
                                                  /** OFFSET ENTRY TO G/L **/
    create gltrans.
    assign
     gltrans.company = cocode
     gltrans.actnum  = v-ar-acct
     gltrans.jrnl    = "OEINV"
     gltrans.tr-dscr = "ORDER ENTRY INVOICE"
     gltrans.tr-date = ar-ledger.tr-date
     gltrans.tr-amt  = v-post-total
     gltrans.period  = period.pnum
     gltrans.trnum   = ar-ledger.tr-num.
  END.
END.

 
