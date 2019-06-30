

/*------------------------------------------------------------------------
    File        : QuoteListRep.p
    Purpose     : Quote List
    Main File   : cerep\r-quotes.w
    Syntax      :

    Description : Return a Dataset of Request For Quote

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttQuoteListReport NO-UNDO
        FIELD vFile AS CHAR
        FIELD dfjzzvbvsj AS CHAR
        .

    DEFINE DATASET dsQuoteListReport FOR ttQuoteListReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCustomer     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegSalrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSalrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSortBy          AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER prmBoCost          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuoteListReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF  prmUser         = ?        THEN ASSIGN     prmUser         = "".
    IF  prmAction       = ?        THEN ASSIGN     prmAction       = "".
    IF  prmBegCustomer  = ?        THEN ASSIGN     prmBegCustomer  = "".
    IF  prmEndCustomer  = ?        THEN ASSIGN     prmEndCustomer  = "".
    IF  prmBeginDate    = ?        THEN ASSIGN     prmBeginDate    = "".
    IF  prmEndDate      = ?        THEN ASSIGN     prmEndDate      = "".
    IF  prmBegSalrep    = ?        THEN ASSIGN     prmBegSalrep    = "".
    IF  prmEndSalrep    = ?        THEN ASSIGN     prmEndSalrep    = "".
    IF  prmSortBy       = ?        THEN ASSIGN     prmSortBy       = "".
    IF  prmBoCost       = ?        THEN ASSIGN     prmBoCost       = "".
    IF  prmOut          = ?        THEN ASSIGN     prmOut          = "".

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.

    DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
    DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX" NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
    DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer" NO-UNDO.
    DEFINE VARIABLE tb_cost AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEF STREAM excel.



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    assign
 cocode = prmComp.


 FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
 
 ASSIGN    
 v-today = TODAY . 

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "QuoteListRep" THEN DO:
    ASSIGN 
        begin_cust-no  = prmBegCustomer
        begin_date     = datetime(prmBeginDate) 
        begin_slsmn    = prmBegSalrep 
        end_cust-no    = prmEndCustomer 
        end_date       = datetime(prmEndDate)
        end_slsmn      = prmEndSalrep 
        rd_sort        = prmSortBy .

    ASSIGN
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE
        tb_cost       = IF prmBoCost = "yes" THEN TRUE ELSE FALSE  
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "qtlist" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "qtlist" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "qtlist" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttQuoteListReport.
        ASSIGN ttQuoteListReport.vFile = excel-file.
    END.
    ELSE DO:
        CREATE ttQuoteListReport.
        ASSIGN ttQuoteListReport.vFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
PROCEDURE run-report :
{sys/form/r-top3w.f}

DEF VAR fcust like quote.cust-no.
DEF VAR tcust like fcust init "zzzzzzzz".
DEF VAR fsman like quote.sman.
DEF VAR tsman like fsman init "zzz".
DEF VAR fdate as date format "99/99/9999" init 01/01/01.
DEF VAR tdate like fdate init today.
DEF VAR v-srt as log format "Customer/SalesRep" init yes.

DEF VAR v-cst as log format "yes/no" init no.

DEF VAR v-cst-hdr as char format "x(60)".
DEF VAR v-lines as int.
DEF VAR i as int.
DEF VAR j as int.

DEF VAR v-misc  as char format "x" no-undo.
DEF VAR blk-dim as char.
DEF VAR v-cost  as dec format ">>>,>>9.99".
DEF VAR v-gp$   as dec format "->>,>>9.99".
DEF VAR v-gp%   as dec format "->>9.99".
DEF VAR v-dscr  like quoteitm.part-dscr1.
DEF VAR v-ext   as dec format ">>>,>>>,>>9.99" extent 2.
DEF VAR v-tot   as dec format ">>>,>>>,>>9.99" extent 4.
DEF VAR v-price as dec.
DEF VAR lv-q-qty AS INT EXTENT 2 NO-UNDO.

/* gdm - 10130805 */
DEF VAR v_quo-date AS CHAR         NO-UNDO.
DEF VAR v_sname    LIKE sman.sname NO-UNDO.

form space(1)
     quotehd.q-no
     quotehd.est-no             format "x(8)"
     quotehd.billto[1]          format "x(28)"
     v-dscr                     format "x(28)"
     quotehd.quo-date           format "99/99/99"
     space(1)
     sman.sname
     v-ext

header "Quote#     Est# Customer                     Part Description               Date   SalesRep                  Ext Price       Ext Cost"

   with frame quote no-labels no-attr-space stream-io width 150 down.


/*SESSION:SET-WAIT-STATE ("general").*/

assign
 str-tit2 = "Quote List"
 {sys/inc/ctrtext.i str-tit2 112}
  
 fcust = begin_cust-no
 tcust = end_cust-no
 fsman = begin_slsmn
 tsman = end_slsmn
 fdate = begin_date
 tdate = end_date
 v-srt = rd_sort eq "Customer"
 v-cst = tb_cost.

EMPTY TEMP-TABLE tt-report.
  
/*{sys/inc/print1.i}*/

 if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

    {sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.

display str-tit with frame r-top. */

/* gdm - 10130805 */
IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED
       "Quote#,Est#,Customer,Part Description,Date,SalesRep,Ext Price,Ext Cost,Qty,Price/M,Cost/M,GP$,GP%,"
        .    

    IF v-cst 
      THEN 
        PUT STREAM excel UNFORMATTED
           "Mat,DL,VO,FO,Misc,Charge,ChargeAmt" 
         SKIP.
      ELSE
        PUT STREAM excel UNFORMATTED
            "Misc,Charge,ChargeAmt"
            SKIP.

END.

if v-cst then
  v-cst-hdr = "Costs --->        Mat           DL           VO           FO".


FOR EACH quotehd
    WHERE quotehd.company  EQ cocode
      AND quotehd.loc      EQ locode
      AND quotehd.cust-no  GE fcust
      AND quotehd.cust-no  LE tcust
      AND quotehd.sman     GE fsman
      AND quotehd.sman     LE tsman
      AND quotehd.quo-date GE fdate
      AND quotehd.quo-date LE tdate
    NO-LOCK,
    
    FIRST quoteitm OF quotehd NO-LOCK,
    
    FIRST quoteqty OF quoteitm NO-LOCK:

  CREATE tt-report.
  ASSIGN
   tt-report.term-id = ""
   tt-report.key-01  = IF v-srt THEN quotehd.cust-no ELSE quotehd.sman
   tt-report.key-02  = STRING(quotehd.q-no,"9999999999")
   tt-report.rec-id  = RECID(quotehd).
END.

for each tt-report where tt-report.term-id eq "",
    first quotehd where recid(quotehd) eq tt-report.rec-id no-lock,
    first est
    where est.company eq quotehd.company
      and est.loc     eq quotehd.loc
      and est.est-no  eq quotehd.est-no
    no-lock

    break by tt-report.key-01
          by tt-report.key-02:
    
  IF FIRST-OF(tt-report.key-02) THEN lv-q-qty[1] = lv-q-qty[1] + 1.
    
  find first sman
      where sman.company eq quotehd.company
        and sman.sman    eq quotehd.sman
      no-lock no-error.

  /* gdm - 10130805 */
  ASSIGN v_sname = IF AVAIL sman THEN sman.sname ELSE "".

  for each quoteitm of quotehd  no-lock,
  
      each quoteqty of quoteitm no-lock
      
      break by quoteitm.part-no
            by quoteqty.qty
      
      with frame quote:

    IF quoteqty.uom EQ "M" THEN
      v-price = quoteqty.price.
    ELSE
      RUN sys/ref/convcuom.p (quoteqty.uom, "M", 0, 0, 0, 0,
                              quoteqty.price, OUTPUT v-price).

    v-cost = quoteqty.mat-cost + quoteqty.lab-cost +
             quoteqty.fo-cost  + quoteqty.vo-cost.

    if first-of(quoteitm.part-no) then do:
      assign
       v-ext[1] = v-price * (quoteqty.qty / 1000)
       v-ext[2] = v-cost  * (quoteqty.qty / 1000)

       v-tot[1] = v-tot[1] + v-ext[1]
       v-tot[2] = v-tot[2] + v-ext[2]

       v-dscr   = quoteitm.part-dscr1.

      if v-dscr eq "" then v-dscr = quoteitm.part-no.
      
      if not first(quoteitm.part-no) then put skip(1).

      display quotehd.q-no
              quotehd.est-no
              quotehd.billto[1]
              v-dscr
              quotehd.quo-date
              sman.sname when avail sman
              v-ext.
            
      put skip(1)
          "    Qty     Price/M      Cost/M         GP$      GP%"
          space(5)
          v-cst-hdr
          skip.
            
    end.

    assign
     v-gp$ = v-price - v-cost
     v-gp% = v-gp$ / v-price * 100.

    if v-gp% eq ? then v-gp% = 0.

    put quoteqty.qty
        space(2)
        v-price  format ">>>,>>9.99"
        space(2)
        v-cost
        space(2)
        v-gp$
        space(2)
        v-gp%
        space(15).

    if v-cst then
      put quoteqty.mat-cost
          space(2)
          quoteqty.lab-cost
          space(2)
          quoteqty.vo-cost
          space(2)
          quoteqty.fo-cost.

    put skip.

    /* gdm - 10130805 */
    IF tb_excel THEN DO:

        IF FIRST-OF(quoteitm.part-no) 
          THEN 
            PUT STREAM excel UNFORMATTED
              '"' quotehd.q-no      '",'
              '"' quotehd.est-no    '",'
              '"' quotehd.billto[1] '",'
              '"' v-dscr            '",'
              '"' v_quo-date        '",'
              '"' v_sname           '",'
              '"' v-ext[1]          '",'
              '"' v-ext[2]          '",'
              '"' quoteqty.qty      '",'
              '"' STRING(ROUND(v-price,10),'->>,>>9.99') '",'
              '"' STRING(ROUND(v-cost,10),'->>,>>9.99')  '",'
              '"' STRING(ROUND(v-gp$,10),'->>,>>9.99')   '",'
              '"' STRING(ROUND(v-gp%,10),'->>,>>9.99')   '",'.

          ELSE
            PUT STREAM excel UNFORMATTED
              ',,,,,,,,'
              '"' quoteqty.qty      '",'
              '"' STRING(ROUND(v-price,10),'->>,>>9.99') '",'
              '"' STRING(ROUND(v-cost,10),'->>,>>9.99')  '",'
              '"' STRING(ROUND(v-gp$,10),'->>,>>9.99')   '",'
              '"' STRING(ROUND(v-gp%,10),'->>,>>9.99')   '",'.

        IF v-cst 
          THEN 
            PUT STREAM excel UNFORMATTED
              '"' quoteqty.mat-cost '",'
              '"' quoteqty.lab-cost '",'
              '"' quoteqty.vo-cost  '",'
              '"' quoteqty.fo-cost  '",' .        

    END. /* if excel */

    /* gdm - 10130805 */
    FIND FIRST quotechg NO-LOCK 
        WHERE quotechg.company EQ quoteqty.company
          AND quotechg.loc     EQ quoteqty.loc
          AND quotechg.q-no    EQ quoteqty.q-no
          AND quotechg.line    EQ quoteqty.line
          AND quotechg.qty     EQ quoteqty.qty NO-ERROR.
    IF NOT AVAIL quotechg THEN 
        IF tb_excel THEN
            PUT STREAM excel UNFORMATTED SKIP.


    for each quotechg no-lock
        where quotechg.company eq quoteqty.company
          and quotechg.loc eq quoteqty.loc
          and quotechg.q-no eq quoteqty.q-no
          and quotechg.line eq quoteqty.line
          and quotechg.qty eq quoteqty.qty
        break by quotechg.charge:
    
      if first(quotechg.charge) then put skip(1).
    
      if (quotechg.labf ne 0 or  quotechg.labm ne 0) and
         (quotechg.matf eq 0 and quotechg.matm eq 0) then
        v-misc = "L".
      else
      if (quotechg.labf eq 0 and quotechg.labm eq 0) and
         (quotechg.matf ne 0 or  quotechg.matm ne 0) then
        v-misc = "M".
      else
      if quotechg.labf ne 0 or quotechg.labm ne 0 or
         quotechg.matf ne 0 or quotechg.matm ne 0 then
        v-misc = "T".
      else
        v-misc = "".
   
      put space(40)
          v-misc
          quotechg.charge
          space(43)
          quotechg.amt
          skip.
      
      /* gdm - 10130805 */
      IF tb_excel THEN DO:

          IF FIRST(quotechg.charge) THEN DO:
              PUT STREAM excel UNFORMATTED
                  '"' v-misc          '",'
                  '"' quotechg.charge '",'
                  '"' quotechg.amt    '"'
                  SKIP.
          END.
          ELSE DO:

              IF v-cst 
                THEN 
                  PUT STREAM excel UNFORMATTED
                    ',,,,,,,,,,,,,,,,,'                   
                    '"' v-misc          '",'
                    '"' quotechg.charge '",'
                    '"' quotechg.amt    '"'
                   SKIP.
                ELSE 
                  PUT STREAM excel UNFORMATTED
                    ',,,,,,,,,,,,,'
                    '"' v-misc          '",'
                    '"' quotechg.charge '",'
                    '"' quotechg.amt    '"'
                   SKIP.
          END.
      END.

      if last(quotechg.charge) then put skip(1). 

    end.

  end.

  put fill("-",133) format "x(133)"
      skip.

  if last-of(tt-report.key-01) then do:
    put fill("-",133) format "x(133)"
        skip.

    if v-srt then put "Customer Totals".
    else put "SalesRep Totals".

    put space(66)
        "# of Quotes:"
        SPACE(1)
        TRIM(STRING(lv-q-qty[1],">,>>>,>>9")) FORMAT "x(9)"
        SPACE(1)
        v-tot[1]
        space(1)
        v-tot[2]
        skip
        fill("-",133) format "x(133)"
        skip
        fill("-",133) format "x(133)"
        skip.

    assign
     v-tot[3]    = v-tot[3] + v-tot[1]
     v-tot[4]    = v-tot[4] + v-tot[2]
     lv-q-qty[2] = lv-q-qty[2] + lv-q-qty[1]

     v-tot[1]    = 0
     v-tot[2]    = 0
     lv-q-qty[1] = 0.
  end.

  IF LAST(tt-report.key-01) THEN
    PUT SKIP(2)
        "Grand Totals"
        SPACE(69)
        "# of Quotes:"
        SPACE(1)
        TRIM(STRING(lv-q-qty[2],">,>>>,>>9")) FORMAT "x(9)"
        SPACE(1)
        v-tot[3]
        SPACE(1)
        v-tot[4].

  delete tt-report.
end.

end procedure.
