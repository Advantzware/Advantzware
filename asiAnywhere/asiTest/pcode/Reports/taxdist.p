

/*------------------------------------------------------------------------
    File        : taxdist.p
    Purpose     : Tax Distribution Schedule
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


MESSAGE "enter " .
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttTaxDistributionSchedule NO-UNDO
        FIELD taxdis AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsTaxDistributionSchedule FOR ttTaxDistributionSchedule.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtaxdis        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegyr         AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTaxDistributionSchedule.

     IF prmUser    = ? THEN ASSIGN    prmUser     = "".   
     IF prmtaxdis  = ? THEN ASSIGN    prmtaxdis   = "". 
     IF prmbegyr   = ? THEN ASSIGN    prmbegyr    = 0. 
     IF prmperiod  = ? THEN ASSIGN    prmperiod   = 0. 
     IF prmbegdt   = ? THEN ASSIGN    prmbegdt    = "".
     IF prmenddt   = ? THEN ASSIGN    prmenddt    = "". 
     IF prmOut     = ? THEN ASSIGN    prmOut      = "". 
     
DEFINE VARIABLE begin_year   AS INTEGER FORMAT ">>>>":U INITIAL 9999 NO-UNDO.
DEFINE VARIABLE begin_period AS INTEGER FORMAT ">9":U INITIAL 1 NO-UNDO.
DEFINE VARIABLE begin_date   AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 NO-UNDO.
DEFINE VARIABLE end_date     AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.


DEF VAR g_company AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
def var list-name as cha no-undo.
def var list-name2 as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-r-no LIKE rm-rctd.r-no NO-UNDO.
DEF VAR t-setup AS LOG INITIAL NO NO-UNDO.
DEF NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE tb_excel      AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)"  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO.
DEFINE VARIABLE tb_runExcel   AS LOGICAL INITIAL no NO-UNDO.
DEF VAR lv-txt-file AS cha NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .


assign
 cocode = prmComp 
 g_company = prmComp
 vuser     = prmUser .




FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

DEF TEMP-TABLE tt-report LIKE report.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF STREAM excel.

assign
   begin_year   = year(today)
   begin_period = month(today)
   begin_date   = today
   end_date     = today.
   
  find first period
      where period.company eq cocode
        and period.yr      eq begin_year
        and period.pst     le today
        and period.pend    ge today
        and period.pstat
      no-lock no-error.

  if avail period then
    assign
     begin_period = period.pnum
     begin_year   = period.yr
     begin_date   = period.pst.



  IF prmtaxdis = "taxdis" THEN DO:
     
        ASSIGN
        v-today       = TODAY
        begin_year    = prmbegyr 
        begin_period  = prmperiod
        begin_date    = date(prmbegdt) 
        end_date      = date(prmenddt) . 
    
       assign
        init-dir    = v-webrootpath
        lv-txt-file =  'TaxDist' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) +  ".txt" .
       
        
        v-excel-file =  'TaxDist' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .


        FIND FIRST period
            WHERE period.company EQ cocode
            AND period.yr      EQ INT(prmbegyr)
            AND period.pnum    EQ INT(prmperiod)
            NO-LOCK NO-ERROR.

        IF AVAIL period THEN
            ASSIGN
            prmbegdt = STRING(period.pst)
            prmenddt  = STRING(IF period.pend LT TODAY THEN period.pend
                                                                        ELSE TODAY).


        
        run run-report. 

        CREATE ttTaxDistributionSchedule.

        IF prmOut = "Yes" THEN
        ASSIGN ttTaxDistributionSchedule.taxdis = v-excel-file.
        ELSE
            ASSIGN ttTaxDistributionSchedule.taxdis = lv-txt-file .


 


  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* ---------------------------------------------- ap/rep/taxsched.p 07/99 JLF */
/* Tax Distribution Schedule by Customer                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def buffer b-stax       for stax.

def var v-period        like INT init 1.
def var v-date          as   date extent 2 format "99/99/9999"
                             init [01/01/0001, today].                       
def var v-year          as   int.

def var v-tax-gl        as   char.
def var v-tax-dscr      like stax.tax-dscr1.
def var v-sal-amt       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-taxable       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-tax-amt       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-rate          as   dec.
def var v-frtr          as   dec.
def var v-rate-t        as   dec.
def var v-frtr-t        as   dec.
def var v-inv-tax       as   dec.
def var v-frt-tax       as   dec.
def var v-actnum        like ar-cashl.actnum.
DEF VAR ld              AS   DEC.
DEF VAR excelheader AS CHAR NO-UNDO.
DEF VAR v-found         AS   logi NO-UNDO.

format header
 /*      skip(1)
       "Tax Authority:" 
       v-tax-dscr[1]    */
       skip(1)
       "Code"
       "Tax Jurisdiction Name"      at 7
       "Gross Sales $"              to 46
       "Taxable $"                  to 61
       "Exempt $"                   to 76
       "Tax $"                      to 91
       fill("-",91)                 format "x(91)"

    with FRAME r-top.
    
{sa/sa-sls01.i}


assign
 str-tit2 = "Tax Distribution Schedule"
 {sys/inc/ctrtext.i str-tit2 112}
 
 v-period  = begin_period
 v-date[1] = begin_date
 v-date[2] = end_date
    
  str-tit3 = "(" + string(v-date[1]) + "-" + string(v-date[2]) + ")"
 {sys/inc/ctrtext.i str-tit3 132}. 

 /*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Tax Authority,Code,Tax Jurisdiction Name,Gross Sales $,"
              + "Taxable $,Exempt $,Tax $".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


    for each cust where cust.company eq cocode no-lock:
      for each ar-inv
          where ar-inv.company        eq cocode
            and ar-inv.inv-date       ge v-date[1]
            and ar-inv.inv-date       le v-date[2]
            and ar-inv.cust-no        eq cust.cust-no
            and ar-inv.tax-code       ne ""
            and ar-inv.posted         eq yes
          use-index inv-date no-lock:

        create tt-report.
        assign
         tt-report.term-id = v-term
         tt-report.key-01  = ar-inv.tax-code
         tt-report.rec-id  = recid(ar-inv).
      end.

      for each ar-cash
          where ar-cash.company    eq cocode
            and ar-cash.cust-no    eq cust.cust-no
            and ar-cash.check-date ge v-date[1]
            and ar-cash.check-date le v-date[2]
            and ar-cash.posted     eq yes
          use-index ar-cash no-lock:

        v-actnum = "".

        for each ar-cashl
            where ar-cashl.c-no   eq ar-cash.c-no
              and ar-cashl.posted eq yes
              and ar-cashl.memo   eq yes
            use-index c-no no-lock,

            first ar-inv
            where ar-inv.company  eq cocode
              and ar-inv.inv-no   eq ar-cashl.inv-no
              and ar-inv.tax-code ne ""
            no-lock:

          find first stax
              where (stax.company eq cocode)
                and stax.tax-group  eq stax.tax-code1[1]
                and stax.tax-acc1[1] eq ar-cashl.actnum
              no-lock no-error.

          if avail stax then do:
            v-actnum = stax.tax-acc1[1].
            leave.
          end.
        end.

        for each ar-cashl
            where ar-cashl.c-no   eq ar-cash.c-no
              and ar-cashl.posted eq yes
              and ar-cashl.memo   eq yes
            use-index c-no no-lock,

            first ar-inv
            where ar-inv.company  eq cocode
              and ar-inv.inv-no   eq ar-cashl.inv-no
              and ar-inv.tax-code ne ""
            no-lock,

            first stax
            where (stax.company eq cocode)
              and stax.tax-group eq ar-inv.tax-code
              and stax.tax-group eq stax.tax-code1[1]
            no-lock:

          create tt-report.
          assign
           tt-report.term-id = v-term
           tt-report.key-01  = ar-inv.tax-code
           tt-report.key-02  = if v-actnum ne "" then v-actnum
                                                 else stax.tax-acc1[1]
           tt-report.rec-id  = recid(ar-cashl).
        end.
      end.
    end.

    VIEW FRAME r-top.

    for each stax
        where (stax.company eq cocode)
          AND stax.tax-group eq stax.tax-code1[1]
        no-lock
        by stax.tax-acc1[1]:

      v-tax-dscr[1] = stax.tax-dscr1[1].
      IF tb_excel THEN PUT STREAM excel UNFORMATTED v-tax-dscr[1].

      /* page. */
       PUT UNFORMATTED "Tax Authority:" v-tax-dscr[1] SKIP.

      assign
       v-sal-amt[1] = 0
       v-taxable[1] = 0
       v-tax-amt[1] = 0.

      for each tt-report
          where tt-report.term-id eq v-term     
          break by tt-report.key-01:

        if first-of(tt-report.key-01) then do:
          assign
           v-found  = NO
           v-rate   = 0
           v-frtr   = 0
           v-rate-t = 0
           v-frtr-t = 0.
          FIND FIRST b-stax
              WHERE b-stax.company   EQ cocode
                AND b-stax.tax-group EQ tt-report.key-01
              NO-LOCK.
          do i = 1 to EXTENT(stax.tax-code1):
            if b-stax.tax-code1[i] eq stax.tax-group then do:
              assign
               v-found       = YES
               v-rate        = v-rate + b-stax.tax-rate1[i]
               v-tax-dscr[2] = b-stax.tax-dscr1[i].
              if b-stax.tax-frt1[i] then v-frtr = v-frtr + b-stax.tax-rate1[i].
            end.
            v-rate-t = v-rate-t + b-stax.tax-rate1[i].
            if b-stax.tax-frt1[i] then v-frtr-t = v-frtr-t + b-stax.tax-rate1[i].
          end.
        end.

        if v-found then do:
          find first ar-inv where recid(ar-inv) eq tt-report.rec-id
              no-lock no-error.

          if avail ar-inv then do:
            if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
              ld = ar-inv.net.
            else
              ld = ar-inv.gross.

            assign
             v-sal-amt[1] = v-sal-amt[1] + (ld - ar-inv.tax-amt)
             v-taxable[1] = v-taxable[1] + (ld - ar-inv.tax-amt -
                                            (IF v-frtr EQ 0 AND v-rate > 0 THEN ar-inv.freight ELSE 0)).

            if ar-inv.f-bill AND v-frtr NE 0 then
              if ld - ar-inv.tax-amt ne 0 then
                assign
                 v-inv-tax = ar-inv.tax-amt *
                             ((ld - ar-inv.tax-amt - ar-inv.freight) /
                              (ld - ar-inv.tax-amt))
                 v-frt-tax = ar-inv.tax-amt *
                             (ar-inv.freight / (ld - ar-inv.tax-amt)).
              else.

            else
              assign
               v-inv-tax    = ar-inv.tax-amt
               v-frt-tax    = 0.

            IF v-inv-tax EQ ? THEN v-inv-tax = 0.
            IF v-frt-tax EQ ? THEN v-frt-tax = 0.

            if v-rate-t ne 0 then
              v-tax-amt[1] = v-tax-amt[1] + (v-inv-tax * (v-rate / v-rate-t)).

            if v-frtr-t ne 0 then
              v-tax-amt[1] = v-tax-amt[1] + (v-frt-tax * (v-frtr / v-frtr-t)).

            for each ar-invl
                where ar-invl.company       eq ar-inv.company
                  and ar-invl.cust-no       eq ar-inv.cust-no
                  and ar-invl.inv-no        eq ar-inv.inv-no
                  and ar-invl.posted
                no-lock:
              if not ar-invl.tax then
                v-taxable[1] = v-taxable[1] - ar-invl.amt.
              /*if ar-invl.disc ne 0 then
                assign
                 v-taxable[1] = v-taxable[1] -
                                ((ar-invl.amt / (1 - (ar-invl.disc / 100))) -
                                 ar-invl.amt)
                 v-sal-amt[1] = v-sal-amt[1] -
                                ((ar-invl.amt / (1 - (ar-invl.disc / 100))) -
                                 ar-invl.amt).*/
            end.

          end.

          else
          if tt-report.key-02 eq stax.tax-acc1[1] then do:
            find ar-cashl where recid(ar-cashl) eq tt-report.rec-id
                no-lock no-error.

            if avail ar-cashl then 
              if ar-cashl.actnum eq stax.tax-acc1[1] then
 /*               v-tax-amt[1] = v-tax-amt[1] +
                               (ar-cashl.amt-paid - ar-cashl.amt-disc) */.
              else
                assign
/*                 v-taxable[1] = v-taxable[1] +
                                (ar-cashl.amt-paid - ar-cashl.amt-disc) 
                 v-sal-amt[1] = v-sal-amt[1] +
                                (ar-cashl.amt-paid - ar-cashl.amt-disc) */. 
          end.                                                                              
        end.

        if last-of(tt-report.key-01) then do:
          IF TRUE /* v-sal-amt[1] ne 0 or
             v-taxable[1] ne 0 or
             v-tax-amt[1] ne 0 */ then
          DO:
            display tt-report.key-01   format "x(3)"
                    v-tax-dscr[2]      at 7
                    v-sal-amt[1]
                    v-taxable[1]
                    v-sal-amt[1] - v-taxable[1]   format "->>,>>>,>>9.99"
                    v-tax-amt[1]
                    SKIP(1)
                with frame detail no-box no-labels stream-io width 132.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                   '"' v-tax-dscr[1]                                        '",'
                   '"' tt-report.key-01                                     '",'
                   '"' v-tax-dscr[2]                                        '",'
                   '"' STRING(v-sal-amt[1],"->>,>>>,>>9.99")                '",'
                   '"' STRING(v-taxable[1],"->>,>>>,>>9.99")                '",'
                   '"' STRING(v-sal-amt[1] - v-taxable[1],"->>,>>>,>>9.99") '",'
                   '"' STRING(v-tax-amt[1],"->>,>>>,>>9.99")                '",'
                   SKIP.
          END.

          assign
           v-sal-amt[2] = v-sal-amt[2] + v-sal-amt[1]
           v-taxable[2] = v-taxable[2] + v-taxable[1]
           v-tax-amt[2] = v-tax-amt[2] + v-tax-amt[1].
          
          ASSIGN
           v-sal-amt[1] = 0
           v-taxable[1] = 0
           v-tax-amt[1] = 0.

        end.
      end.


    end.  /* for each stax */
      clear frame totals no-pause.

      display skip(1)
              "TOTALS:"                 at 7
              v-sal-amt[2]              to 46
              v-taxable[2]
              v-sal-amt[2] - v-taxable[2]       format "->>,>>>,>>9.99"
              v-tax-amt[2]
              SKIP(2)
          with frame totals no-box no-labels stream-io width 132.
      DOWN WITH FRAME totals.
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED SKIP
             ',TOTALS:,,'
             '"' STRING(v-sal-amt[2],"->>,>>>,>>9.99") '",'
             '"' STRING(v-taxable[2],"->>,>>>,>>9.99") '",'
             '"' STRING(v-sal-amt[2] - v-taxable[2],"->>,>>>,>>9.99") '",'
             '"' STRING(v-tax-amt[2],"->>,>>>,>>9.99") '",'
             SKIP(1).
    for each tt-report where tt-report.term-id eq v-term:
      delete tt-report.
    end.

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     
  END.


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
