

/*------------------------------------------------------------------------
    File        : mtdsub.p
    Purpose     : MTD Destination Subtotals
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/



{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttMtdDestinationSubtotals NO-UNDO
        FIELD mtdsub AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsMtdDestinationSubtotals FOR ttMtdDestinationSubtotals.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmmtdsub        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegyr         AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMtdDestinationSubtotals.

     IF prmUser    = ? THEN ASSIGN    prmUser     = "".   
     IF prmmtdsub  = ? THEN ASSIGN    prmmtdsub   = "". 
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



  IF prmmtdsub = "mtdsub" THEN DO:
     
        ASSIGN
        v-today       = TODAY
        begin_year    = prmbegyr 
        begin_period  = prmperiod
        begin_date    = date(prmbegdt) 
        end_date      = date(prmenddt) . 
    
       assign
        init-dir    = v-webrootpath
        lv-txt-file =  'MTDsub' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) +  ".txt" .
       
        
        v-excel-file =  'MTDsub' +
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

        CREATE ttMtdDestinationSubtotals.

        IF prmOut = "Yes" THEN
        ASSIGN ttMtdDestinationSubtotals.mtdsub = v-excel-file.
        ELSE
            ASSIGN ttMtdDestinationSubtotals.mtdsub = lv-txt-file .


 


  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* ---------------------------------------------- ap/rep/monthdst.p 10/96 JLF */
/* MTD Destination Code Subtotals                                             */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def buffer b-stax       for stax.

def var v-period        like uperiod init 1.
def var v-date          as   date extent 2 format "99/99/9999"
                             init [01/01/0001, today].                       
def var v-year          as   int.

def var v-tax-gl        as   char.
def var v-msf           as   int format ">>,>>9".
def var v-tons          as   int format ">>,>>9".
def var v-index         as   int format ">>>>9".
def var v-sal-amt       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-mis-amt       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-tax-amt       as   dec format "->>,>>>,>>9.99" extent 2.
def var v-actnum        like ar-cashl.actnum.
DEF VAR ld              AS   DEC.
DEF VAR excelheader AS CHAR NO-UNDO.

format header
       skip(1)
       "Dest Code"
       "Tax Jurisdiction Name"      at 11
       "Trade $"                    to 50
       "MSF"                        to 57
       "Tons"                       to 64
       "Index"                      to 70
       "Misc $"                     to 85
       "Sales Tax"                  to 100
       "Total $"                    to 115
       fill("-",132)                format "x(132)"

    with no-labels no-box no-underline stream-io width 132 frame f-top page-top.
    
{sa/sa-sls01.i}


assign
 str-tit2 = "MTD Destination Subtotals"
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


/*  ASSIGN tb_excel
         tb_runExcel
         fi_file. */


IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Dest Code,Tax Jurisdiction Name,Trade $,MSF,Tons,"
              + "Index,Misc $,Sales Tax,Total $".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.


    display "" with frame r-top.
    display "" with frame f-top.

    assign
     v-sal-amt = 0
     v-mis-amt = 0
     v-tax-amt = 0.

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
            no-lock:

          create tt-report.
          assign
           tt-report.term-id = v-term
           tt-report.key-01  = ar-inv.tax-code
           tt-report.key-02  = if v-actnum ne "" then v-actnum else stax.tax-acc1[1]
           tt-report.rec-id  = recid(ar-cashl).
        end.
      end.
    end.
    
    for each tt-report
        where tt-report.term-id eq v-term
        break by tt-report.key-01:

      find first stax
          where (stax.company eq cocode)
            and stax.tax-group eq tt-report.key-01
          no-lock no-error.

      find first ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock no-error.

      if avail ar-inv then do:
        if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
          ld = ar-inv.net.
        else
          ld = ar-inv.gross.

        assign
         v-sal-amt[1] = v-sal-amt[1] + (ld - ar-inv.tax-amt)
         v-tax-amt[1] = v-tax-amt[1] + ar-inv.tax-amt
         v-tons       = v-tons       + ar-inv.t-weight.

        for each ar-invl
            where ar-invl.company       eq ar-inv.company
              and ar-invl.cust-no       eq ar-inv.cust-no
              and ar-invl.inv-no        eq ar-inv.inv-no
              and ar-invl.posted
            no-lock:
          if ar-invl.misc then
            assign
             v-sal-amt[1] = v-sal-amt[1] - ar-invl.amt
             v-mis-amt[1] = v-mis-amt[1] + ar-invl.amt.

          else do:
            /*if ar-invl.disc ne 0 then
              v-sal-amt[1] = v-sal-amt[1] -
                             ((ar-invl.amt / (1 - (ar-invl.disc / 100))) -
                              ar-invl.amt).*/

            if ar-inv.t-weight eq 0 then do:
              find first itemfg
                  where itemfg.company eq cocode
                    and itemfg.i-no    eq ar-invl.i-no
                  no-lock no-error.
              if avail itemfg then
                v-tons = v-tons + (itemfg.weight-100 * (ar-invl.inv-qty / 100)).
            end.
          end.
        end.
      end.

      else
      if tt-report.key-02 eq stax.tax-acc1[1] then do:
        find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.

        if ar-cashl.actnum eq stax.tax-acc1[1] then
          v-tax-amt[1] = v-tax-amt[1] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
        else
          v-sal-amt[1] = v-sal-amt[1] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
      end.

      if last-of(tt-report.key-01) then do:

        v-tons = round(v-tons / 2000,0).

        if v-sal-amt[1] ne 0 then
        DO:
          display tt-report.key-01
                  stax.tax-dscr1[1]    at 11
                  v-sal-amt[1]
                  v-msf
                  v-tons
                  v-index
                  v-mis-amt[1]
                  v-tax-amt[1]
                  v-sal-amt[1] + v-mis-amt[1] + v-tax-amt[1]
                                                format "->>,>>>,>>9.99"
                  fill("-",132)                 format "x(132)"

            with frame detail no-box no-labels stream-io width 132.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
               '"'  tt-report.key-01                                     '",'
               '"'  stax.tax-dscr1[1]                                     '",'
               '"'  STRING(v-sal-amt[1],"->>,>>>,>>9.99")                '",'
               '"'  STRING(v-msf,">>,>>9")                               '",'
               '"'  STRING(v-tons,">>,>>9")                              '",'
               '"'  STRING(v-index,">>>>9")                              '",'
               '"'  STRING(v-mis-amt[1],"->>,>>>,>>9.99")                '",'
               '"'  STRING(v-tax-amt[1],"->>,>>>,>>9.99")                '",'
               '"'  STRING(v-sal-amt[1] + v-mis-amt[1] + v-tax-amt[1],"->>,>>>,>>9.99") '",'
               SKIP.
        END.

        assign
         v-sal-amt[2] = v-sal-amt[2] + v-sal-amt[1]
         v-mis-amt[2] = v-mis-amt[2] + v-mis-amt[1]
         v-tax-amt[2] = v-tax-amt[2] + v-tax-amt[1]

         v-tons       = 0
         v-sal-amt[1] = 0
         v-mis-amt[1] = 0
         v-tax-amt[1] = 0.
      end.
    end.

    clear frame totals no-pause.

    display skip(1)
            "TOTALS:"                 at 11
            v-sal-amt[2]              to 50
            v-mis-amt[2]              to 85
            v-tax-amt[2]              to 100
            v-sal-amt[2] + v-mis-amt[2] + v-tax-amt[2]
                                      to 115    format "->>,>>>,>>9.99"

        with frame totals no-box no-labels stream-io width 132.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
         '"'  ""                                                   '",'
         '"'  "TOTALS:"                                            '",'
         '"'  STRING(v-sal-amt[2],"->>,>>>,>>9.99")                '",'
         '"'  ""                                                   '",'
         '"'  ""                                                   '",'
         '"'  ""                                                   '",' 
         '"'  STRING(v-mis-amt[2],"->>,>>>,>>9.99")                '",'
         '"'  STRING(v-tax-amt[2],"->>,>>>,>>9.99")                '",'
         '"'  STRING(v-sal-amt[2] + v-mis-amt[2] + v-tax-amt[2],"->>,>>>,>>9.99") '",'
         SKIP.

    IF tb_excel THEN DO:
       OUTPUT STREAM excel CLOSE.
       
    END.

  

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
