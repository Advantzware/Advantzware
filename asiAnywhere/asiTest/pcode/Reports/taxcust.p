

/*------------------------------------------------------------------------
    File        : taxcust.p
    Purpose     : Tax Schedule By Customer
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/



{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttTaxScheduleByCustomer NO-UNDO
        FIELD taxcust AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsTaxScheduleByCustomer FOR ttTaxScheduleByCustomer.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtaxcust        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegyr         AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTaxScheduleByCustomer.

     IF prmUser    = ? THEN ASSIGN    prmUser     = "".   
     IF prmtaxcust  = ? THEN ASSIGN    prmtaxcust   = "". 
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



  IF prmtaxcust = "taxcust" THEN DO:
     
        ASSIGN
        v-today       = TODAY
        begin_year    = prmbegyr 
        begin_period  = prmperiod
        begin_date    = date(prmbegdt) 
        end_date      = date(prmenddt) . 
    
       assign
        init-dir    = v-webrootpath
        lv-txt-file =  'TaxCust' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) +  ".txt" .
       
        
        v-excel-file = 'TaxCust' +
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

        CREATE ttTaxScheduleByCustomer.

        IF prmOut = "Yes" THEN
        ASSIGN ttTaxScheduleByCustomer.taxcust = v-excel-file.
        ELSE
            ASSIGN ttTaxScheduleByCustomer.taxcust = lv-txt-file .


 


  END.
/*****************************************************************************************/
PROCEDURE run-report :
/* ---------------------------------------------- ap/rep/taxsched.p 07/99 JLF */
/* Tax Distribution Schedule by Customer                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def var v-period        like uperiod init 1 NO-UNDO.
def var v-date          as   date extent 2 format "99/99/9999"
                             init [01/01/0001, today] NO-UNDO.                       
def var v-year          as   INT NO-UNDO.

def var v-tax-gl        as   CHAR NO-UNDO.
def var v-tax-dscr      like stax.tax-dscr NO-UNDO.
def var v-sal-gro       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-tax-amt       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-freight       as   dec format "->>,>>>,>>9.99" extent 3 NO-UNDO.
def var v-actnum        like ar-cashl.actnum NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

/* gdm - */
def var v-grtot         as   dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-grfrght       as   dec format "->>,>>>,>>9.99" NO-UNDO.

/* aj */
def var v-rate          as   DEC NO-UNDO.
def var v-frtr          as   DEC NO-UNDO.
def var v-rate-t        as   DEC NO-UNDO.
def var v-frtr-t        as   DEC NO-UNDO.
def var v-inv-tax       as   DEC NO-UNDO.
def var v-frt-tax       as   DEC NO-UNDO.
DEF VAR v-found         AS   logi NO-UNDO.
DEF VAR ld              AS   DEC NO-UNDO.

def buffer b-stax       for stax.

format header
       skip(1)
       "Tax Authority:"
       v-tax-dscr[1]
       skip(1)
       "Cust Name"
       "Date"                       at 32
       "Invoice"                    to 47
       "Gross Sales $"              to 62
       "Tax $"                      to 77
       "Freight $"                  to 92
       "Net Sales $"                to 107
       fill("-",107)                format "x(107)"

    with frame r-top.
    
{sa/sa-sls01.i}



EMPTY TEMP-TABLE tt-report.

assign
 str-tit2 = "Tax Schedule by Customer"
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
   excelheader = "Tax Authority,Cust Name,Date,Invoice,Gross Sales $,"
               + "Tax $,Freight $,Net Sales $".
   PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.



    FOR each cust where cust.company eq cocode no-lock:
      STATUS DEFAULT "Processing Cust # " + STRING(cust.cust-no). 

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
           tt-report.key-02  = cust.name
           tt-report.key-03  = string(ar-inv.inv-no,"9999999999")
           tt-report.rec-id  = recid(ar-inv).
          RELEASE tt-report.
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
                    and stax.tax-group  eq stax.tax-code[1]
                    and stax.tax-acc[1] eq ar-cashl.actnum
                  no-lock no-error.
              
              if avail stax then do:
                 v-actnum = stax.tax-acc[1].
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
                and stax.tax-group eq stax.tax-code[1]
              no-lock:
         
              create tt-report.
              assign
               tt-report.term-id = v-term
               tt-report.key-01  = ar-inv.tax-code
               tt-report.key-02  = cust.name
               tt-report.key-03  = string(ar-cashl.inv-no,"9999999999")
               tt-report.key-04  = if v-actnum ne "" then v-actnum
                                                     else stax.tax-acc[1]
               tt-report.rec-id  = recid(ar-cashl).
              RELEASE tt-report.
          end.
      end.
    end.    

    VIEW FRAME r-top.
    
    for each stax
         where (stax.company eq cocode)
          and stax.tax-group eq stax.tax-code[1]
        no-lock
        by stax.tax-acc[1]:

      v-tax-dscr[1] = stax.tax-dscr[1].

      page.            

      for each tt-report
        where tt-report.term-id eq v-term
/*             and tt-report.key-01  eq stax.tax-group gdm - */
        BREAK BY tt-report.key-02
              by tt-report.key-01              
              by tt-report.key-03:

        release ar-inv.
        release ar-cashl.
        release ar-cash.        

        /* gdm - */
        STATUS DEFAULT "Processing Cust # " + STRING(tt-report.key-02) + 
                       " Tax Authority " + STRING(tt-report.key-01).
        
        /* if first-of(tt-report.key-02) then do: gdm - */
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
                 v-rate        = v-rate + b-stax.tax-rate1[i].
                if b-stax.tax-frt1[i] then
                   v-frtr = v-frtr + b-stax.tax-rate1[i].
              end.
              v-rate-t = v-rate-t + b-stax.tax-rate1[i].
              if b-stax.tax-frt1[i] then
                 v-frtr-t = v-frtr-t + b-stax.tax-rate1[i].
           end. 
        end. /* first-of */        

        /* gdm - */
        if v-found then do:

           find first ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock no-error.
          
           if avail ar-inv then do:
            
              if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
                 ld = ar-inv.net.
              else
                 ld = ar-inv.gross.
             
              assign
                v-sal-gro[1] = v-sal-gro[1] + (ld - ar-inv.tax-amt).

              if ar-inv.f-bill AND v-frtr NE 0 then 
                 if ld - ar-inv.tax-amt ne 0 then
                    assign
                       v-inv-tax = ar-inv.tax-amt *
                                   ((ld - ar-inv.tax-amt - ar-inv.freight) /
                                   (ld - ar-inv.tax-amt))
                       v-frt-tax = ar-inv.tax-amt *
                                   (ar-inv.freight / (ld - ar-inv.tax-amt)).
                 ELSE
                    ASSIGN 
                       v-inv-tax = 0
                       v-frt-tax = 0.
             
              else
                 assign
                    v-inv-tax    = ar-inv.tax-amt
                    v-frt-tax    = 0.
             
              IF v-inv-tax EQ ? THEN v-inv-tax = 0.
              IF v-frt-tax EQ ? THEN v-frt-tax = 0.
             
              if v-rate-t ne 0 then
                 v-tax-amt[1] = v-tax-amt[1] +
                               (v-inv-tax * (v-rate / v-rate-t)).
             
              if v-frtr-t ne 0 then
                 v-tax-amt[1] = v-tax-amt[1] +
                               (v-frt-tax * (v-frtr / v-frtr-t)).
             
              v-freight[1] = IF ar-inv.f-bill THEN ar-inv.freight ELSE 0.  /* OLD actual code */

           end.
          
           else
           if tt-report.key-04 eq stax.tax-acc[1] then do:
              find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.
              find first ar-cash
                  where ar-cash.c-no eq ar-cashl.c-no
                  no-lock no-error.
             
              if ar-cashl.actnum eq stax.tax-acc[1] then
               /* aj old v-tax-amt[1] = ar-cashl.amt-paid - ar-cashl.amt-disc.  */
                 v-tax-amt[1] = v-tax-amt[1] + 
                                ((ar-cashl.amt-paid - ar-cashl.amt-disc) *
                                 (v-rate / v-rate-t) ).
              else
                 v-sal-gro[1] = v-sal-gro[1] + ar-cashl.amt-paid - ar-cashl.amt-disc.
           end.      
         
        END. /* gdm - */
        
        IF /*last-of(tt-report.key-01) AND*/ /* gdm - */
           (v-sal-gro[1] ne 0 or
            v-tax-amt[1] ne 0 or
            v-freight[1] ne 0) then
        DO:
           display tt-report.key-02      
                 /* when first-of(tt-report.key-02)  gdm - */
                                         format "x(30)" 
                   ar-inv.inv-date       when avail ar-inv
                                         FORMAT "99/99/99"
                   ar-cash.check-date    when avail ar-cash  @ ar-inv.inv-date
                 space(2)
                   ar-inv.inv-no         when avail ar-inv
                   ar-cashl.inv-no       when avail ar-cashl @ ar-inv.inv-no
                   v-sal-gro[1]
                   v-tax-amt[1]
                   v-freight[1]
                   v-sal-gro[1] - /* - v-tax-amt[1] */ v-freight[1]
                                         format "->>,>>>,>>9.99"
          
               with frame detail no-box no-labels stream-io width 132.
           DOWN with frame detail no-box no-labels stream-io width 132.
          
           IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
               '"' (IF FIRST-OF(tt-report.key-02) THEN v-tax-dscr[1]
                    ELSE "")                                             '",'
               '"' (IF FIRST-OF(tt-report.key-02) THEN tt-report.key-02
                    ELSE "")                                             '",'
               '"' (IF AVAIL ar-cash AND ar-cash.check-date NE ? THEN
                       STRING(ar-cash.check-date,"99/99/9999")
                    ELSE IF AVAIL ar-inv AND ar-inv.inv-date NE ? THEN
                       STRING(ar-inv.inv-date,"99/99/9999")
                    ELSE "")                                             '",'
               '"' (IF avail ar-cashl THEN STRING(ar-cashl.inv-no)
                    ELSE IF AVAIL ar-inv THEN STRING(ar-inv.inv-no)
                    ELSE "")                                             '",'
               '"' STRING(v-sal-gro[1],"->>,>>>,>>9.99")                 '",'
               '"' STRING(v-tax-amt[1],"->>,>>>,>>9.99")                 '",'
               '"' STRING(v-freight[1],"->>,>>>,>>9.99")                 '",'
               '"' STRING(v-sal-gro[1]  - v-freight[1],"->>,>>>,>>9.99")  '",'
               SKIP.         
          
            ASSIGN 
               v-sal-gro[2] = v-sal-gro[2] + v-sal-gro[1]  
               v-tax-amt[2] = v-tax-amt[2] + v-tax-amt[1]  
               v-freight[2] = v-freight[2] + v-freight[1].
        END.
            
        ASSIGN
         v-sal-gro[1] = 0
         v-tax-amt[1] = 0
         v-freight[1] = 0.
      end.

      clear frame totals1 no-pause.

      display skip(1)
              "TOTALS:"                 to 47
              v-sal-gro[2]
              v-tax-amt[2]
              v-freight[2]
              v-sal-gro[2]  /* - v-tax-amt[2] */ - v-freight[2]
                                       format "->>,>>>,>>9.99"

          with frame totals1 no-box no-labels stream-io width 132.
      
      IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
          '"' ""                                                    '",'
          '"' ""                                                    '",'
          '"' ""                                                    '",'
          '"' "TOTALS:"                                             '",'
          '"' STRING(v-sal-gro[2],"->>,>>>,>>9.99")                 '",'
          '"' STRING(v-tax-amt[2],"->>,>>>,>>9.99")                 '",'
          '"' STRING(v-freight[2],"->>,>>>,>>9.99")                 '",'
          '"' STRING(v-sal-gro[2] - v-freight[2],"->>,>>>,>>9.99")  '",'
          SKIP(1).

      ASSIGN               
        v-sal-gro[3] = v-sal-gro[3] + v-sal-gro[2] 
        v-tax-amt[3] = v-tax-amt[3] + v-tax-amt[2]
        v-freight[3] = v-freight[3] + v-freight[2]
        v-sal-gro[2] = 0
        v-tax-amt[2] = 0
        v-freight[2] = 0.
    end.  /* for each stax */

    display skip(5)
            "GRAND TOTALS:"             to 47
            v-sal-gro[3]
            v-tax-amt[3]
            v-freight[3]
            v-sal-gro[3] /* - v-tax-amt[3] */ - v-freight[3]
                                        format "->>,>>>,>>9.99"

          with frame totals2 no-box no-labels stream-io width 132.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
         '"' ""                                                    '",'
         '"' ""                                                    '",'
         '"' ""                                                    '",'
         '"' "GRAND TOTALS:"                                       '",'
         '"' STRING(v-sal-gro[3],"->>,>>>,>>9.99")                 '",'
         '"' STRING(v-tax-amt[3],"->>,>>>,>>9.99")                 '",'
         '"' STRING(v-freight[3],"->>,>>>,>>9.99")                 '",'
         '"' STRING(v-sal-gro[3] - v-freight[3] ,"->>,>>>,>>9.99")  '",'
         SKIP(1).

  IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     
  END.

  

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
