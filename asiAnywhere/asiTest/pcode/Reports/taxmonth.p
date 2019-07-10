

/*------------------------------------------------------------------------
    File        : taxmonth.p
    Purpose     : Monthly Tax
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttMonthlyTax NO-UNDO
        FIELD taxmon AS CHAR
        FIELD extra  AS CHAR .
        
       

DEFINE DATASET dsMonthlyTax FOR ttMonthlyTax.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmtaxmon        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegyr         AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmperiod        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER  prmbegdt        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmenddt        AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMonthlyTax.

     IF prmUser    = ? THEN ASSIGN    prmUser     = "".   
     IF prmtaxmon  = ? THEN ASSIGN    prmtaxmon   = "". 
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

def TEMP-TABLE w-tax no-undo
     field tax-gr like ar-inv.tax-code
     field rec-id as   recid.

DEF TEMP-TABLE tt-stax NO-UNDO LIKE stax.
DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

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



  IF prmtaxmon = "taxmon" THEN DO:
     
        ASSIGN
        v-today       = TODAY
        begin_year    = prmbegyr 
        begin_period  = prmperiod
        begin_date    = date(prmbegdt) 
        end_date      = date(prmenddt) . 
    
         

    assign
        init-dir    = v-webrootpath
        lv-txt-file =  'MonthlyTax' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) +  ".txt" .
       
        
        v-excel-file =  'MonthlyTax' +
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

        CREATE ttMonthlyTax.

        IF prmOut = "Yes" THEN
        ASSIGN ttMonthlyTax.taxmon = v-excel-file.
        ELSE
            ASSIGN ttMonthlyTax.taxmon = lv-txt-file .

 


  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* ---------------------------------------------- ap/rep/monthtax.p 10/96 JLF */
/* Monthly Tax tt-report                                                         */
/* -------------------------------------------------------------------------- */
/*{sys/form/r-top.f}*/

      /* ------------------------------------------------- sys/form/r-top.f 9/92 cd */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 56}.
 
form header
     skip(1)
     day_str
     str-tit format "x(56)"
     "Page" at 68 page-number format ">>>"
     skip
     tim_str
     str-tit2 format "x(56)"
     skip(1)
      
    with frame r-top no-labels no-box width 250 stream-io no-underline page-top.

/* end ---------------------------------- copr. 1992  advanced software, inc. */


   
   
   def var v-period        like INT init 1.
   def var v-date          as   date extent 2 format "99/99/9999"
                                init [01/01/0001, today].                       
   def var v-year          as   int no-undo.
   def var v-tax-gl        as   char.
   def var v-tax-gr        like stax.tax-gr extent 10.
   def var v-head1         as   char format "x(30)"  extent 5.
   def var v-head2         as   char format "x(30)"  extent 5.
   def var v-head3         as   char format "x(30)"  extent 5.
   def var v-sal-amt       as   dec format "->>,>>>,>>9.99" extent 2.
   def var v-taxable       as   dec format "->>,>>>,>>9.99" extent 10.
   def var v-tax-amt       as   dec format "->>,>>>,>>9.99" extent 10.
   def var v-rate          like stax.tax-rate1.
   def var v-frtr          like stax.tax-rate1.
   def var v-frtr-s        AS   DEC.
   def var v-rate-t        as   dec.
   DEF VAR v-rate-tt       AS   DEC EXTENT 5.
   DEF VAR v-frtr-tt       AS   DEC EXTENT 5.
   def var v-frtr-t        as   dec.
   def var v-inv-tax       as   dec.
   def var v-frt-tax       as   dec.
   def var v-actnum        like ar-cashl.actnum.
   DEF VAR ld              AS   DEC.
   DEF VAR excelheader1    AS CHAR NO-UNDO.
   DEF VAR excelheader2    AS CHAR NO-UNDO.
   DEF VAR v-tot-tax       AS DEC NO-UNDO.
   DEF VAR v-tot-taxable   AS DEC NO-UNDO.

  

   IF tb_excel THEN
     OUTPUT STREAM excel TO VALUE(fi_file).
   
   format header
      skip(1)
      v-head1[1]        at 43
      space(0)
      v-head1[2]
      space(0)
      v-head1[3] 
      space(0)
      v-head1[4]
      space(0)
      v-head1[5]      
      skip
      "Code Name"
      "Gross Sales $"   at 30
      space(0)
      v-head2[1]        at 43
      space(0)
      v-head2[2]
      space(0)
      v-head2[3] 
      space(0)
      v-head2[4] 
      space(0)
      v-head2[5]       
      skip
      fill("-",42)      format "x(42)"
      space(0)
      v-head3[1]        at 43
      space(0)
      v-head3[2]
      space(0)
      v-head3[3]               
      space(0)
      v-head3[4]    
      space(0)
      v-head3[5]       
      SKIP
      WITH FRAME r-top .
   
   /*{sa/sa-sls01.i}*/

   def buffer xreport for report.

DEF var v-term   like report.term-id format "x(20)".
def var v-term-2 like v-term.

assign
 v-term   = string(year(today),"9999")      +
            string(month(today),"99")       +
            string(day(today),"99")         +
            string(time,"99999")            +
            string(program-name(1),"x(40)") +
            string(prmUser,"x(40)")

 v-term-2 = string(year(today - 2),"9999")  +
            string(month(today - 2),"99")   +
            string(day(today - 2),"99").

for each report where report.term-id lt v-term-2 no-lock TRANSACTION:
  find first xreport where recid(xreport) eq recid(report)
      exclusive-lock no-wait no-error.
  if avail xreport then delete xreport.
end.

   
   
   assign
      str-tit2 = "Monthly Tax Report" 
      {sys/inc/ctrtext.i str-tit2 55} 
    
      v-period  = begin_period
      v-date[1] = begin_date
      v-date[2] = end_date. 
 
   /*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.
       
   
   {sys/inc/outprint.i value(lines-per-page)}
   
  

   for each stax
            where (stax.company eq cocode)
            and stax.tax-group  eq stax.tax-code1[1]
            and stax.tax-acc1[1] GT v-tax-gl no-lock
             by stax.tax-acc1[1]:
      CREATE tt-stax.
      BUFFER-COPY stax TO tt-stax NO-ERROR.
   END.

   for each cust where cust.company eq cocode no-lock:
      for each ar-inv where ar-inv.company        eq cocode
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
      END. /* for each ar-inv */
   
      for each ar-cash where ar-cash.company    eq cocode
                         and ar-cash.cust-no    eq cust.cust-no
                         and ar-cash.check-date ge v-date[1]
                         and ar-cash.check-date le v-date[2]
                         and ar-cash.posted     eq yes
                         use-index ar-cash no-lock:
   
         v-actnum = "".
   
         for each ar-cashl where ar-cashl.c-no   eq ar-cash.c-no
                             and ar-cashl.posted eq yes
                             and ar-cashl.memo   eq yes
                             use-index c-no no-lock,
            first ar-inv where ar-inv.company  eq cocode
                           and ar-inv.inv-no   eq ar-cashl.inv-no
                           and ar-inv.tax-code ne "" no-lock:
   
            find first stax
                       where (stax.company eq cocode)
                       and stax.tax-group  eq stax.tax-code1[1]
                       and stax.tax-acc1[1] eq ar-cashl.actnum no-lock no-error.
                           
            if avail stax then do:
               v-actnum = stax.tax-acc1[1].
               leave.
            end.
         end. /* for each ar-cashl */
   
         for each ar-cashl where ar-cashl.c-no   eq ar-cash.c-no
                             and ar-cashl.posted eq yes
                             and ar-cashl.memo   eq yes
                             use-index c-no no-lock,
            first ar-inv where ar-inv.company  eq cocode
                           and ar-inv.inv-no   eq ar-cashl.inv-no
                           and ar-inv.tax-code ne "" no-lock,
            first stax
                  where (stax.company eq cocode)
                  and stax.tax-group eq ar-inv.tax-code no-lock:
   
            create tt-report.
            assign
               tt-report.term-id = v-term
               tt-report.key-01  = ar-inv.tax-code
               tt-report.key-02  = if v-actnum ne "" then v-actnum else stax.tax-acc1[1]
               tt-report.rec-id  = recid(ar-cashl).
         end. /* for each ar-cashl */
      end. /* for each ar-cash */
   end. /* for each cust */
   
   VIEW FRAME r-top.
   
   
   do while true:
      assign
         v-tax-gr = ""
         v-head1  = ""
         v-head2  = ""
         v-head3  = ""
         i        = 0.
      for each tt-stax WHERE tt-stax.company = cocode
                by tt-stax.tax-acc1[1]:

         assign
            i           = i + 1
            x           = (27 - length(trim(substr(tt-stax.tax-dscr[1],1,23)))) / 2
            v-head1[i]  = "   " + fill("*",x - 1) + " " +
                                  trim(substr(tt-stax.tax-dscr[1],1,23)) +
                                  " " + fill("*",x - 1)
            v-head2[i]  = "      Taxable $          Tax $"
            v-head3[i]  = fill("-",30)
            v-tax-gr[i] = tt-stax.tax-group
            v-tax-gl    = tt-stax.tax-acc1[1].

         DELETE tt-stax.
         if i gt 4 then leave.
      
      end. /* for each stax */

      if v-tax-gr[1] eq "" then leave.

      IF tb_excel THEN DO:
         ASSIGN
            excelheader1 = ",,," + v-head1[1] + ",," + v-head1[2] + ",," + v-head1[3] + ",," + v-head1[4] + ",," + v-head1[5]
            excelheader2 = "Code,Name,Gross Sales,Taxable $,Tax $,Taxable $,Tax $," + "Taxable $,Tax $," + "Taxable $,Tax $," + "Taxable $,Tax $".
         PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader1,',','","') '"' SKIP.
         PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader2,',','","') '"' SKIP.
      END.
   
      page.
   
      assign
         v-sal-amt = 0
         v-taxable = 0
         v-tax-amt = 0.
   
      for each tt-report where tt-report.term-id eq v-term
                      break by tt-report.key-01:
         
         if first-of(tt-report.key-01) then do:
            assign
               v-rate   = 0
               v-frtr   = 0.
   
         find first stax
                    where (stax.company eq cocode)
                    and stax.tax-group eq tt-report.key-01 no-lock no-error.
                 
         DO i = 1 TO 5:
            IF v-tax-gr[i] NE "" THEN DO:
               ASSIGN
                  v-rate-t = 0
                  v-frtr-t = 0
                  v-frtr-s = 0.
               DO j = 1 TO 5:
                  IF v-tax-gr[i] EQ stax.tax-code1[j] THEN DO:
                     v-rate[i] = v-rate[i] + stax.tax-rate1[j].
                     IF stax.tax-frt1[j] THEN DO:
                        ASSIGN 
                          v-frtr[i] = v-frtr[i] + stax.tax-rate1[j]
                          v-frtr-s = v-frtr-s + stax.tax-rate1[j].
                     END.

                  END.
                  ASSIGN
                     v-rate-t = v-rate-t + stax.tax-rate1[j]
                     v-frtr-t = v-frtr-t + stax.tax-rate1[j].
                 END.  
                 v-rate-tt[i] = v-rate-t.
                 IF stax.tax-frt1[i] THEN                   
                    v-frtr-tt[i] = v-frtr-t.
               END.
            END.
         END.

         find first ar-inv where recid(ar-inv) eq tt-report.rec-id no-lock no-error.
   
         if avail ar-inv then do:
            if ar-inv.net eq ar-inv.gross + ar-inv.freight + ar-inv.tax-amt then
               ld = ar-inv.net.
            else
               ld = ar-inv.gross.
            v-sal-amt[1] = v-sal-amt[1] + (ld - ar-inv.tax-amt).
            DO i = 1 TO 5:
               IF v-rate[i] EQ 0 THEN NEXT.
               
               v-taxable[i] = v-taxable[i] + (ld - ar-inv.tax-amt).
               IF ar-inv.f-bill AND v-frtr-s > 0 THEN
                  IF ld - ar-inv.tax-amt NE 0 THEN DO:
                 
                     assign
                        v-inv-tax = ar-inv.tax-amt * ((ld - ar-inv.tax-amt - ar-inv.freight) / (ld - ar-inv.tax-amt))
                        v-frt-tax = ar-inv.tax-amt * (ar-inv.freight / (ld - ar-inv.tax-amt)).
                  END.
                  else.
               
               else
                  assign
                     v-inv-tax    = ar-inv.tax-amt
                     v-frt-tax    = 0.

               if v-rate-tt[i] ne 0 then
                  v-tax-amt[i] = v-tax-amt[i] + (v-inv-tax * (v-rate[i] / v-rate-tt[i])).
               
               if v-frtr-tt[i] ne 0 THEN DO:
                   v-tax-amt[i] = v-tax-amt[i] + (v-frt-tax * (v-frtr[i] / v-frtr-tt[i])).
              END.

            END. /* DO i = 1 TO 5: */

   
            for each ar-invl FIELDS(amt tax t-freight) where ar-invl.company       eq ar-inv.company
                                               and ar-invl.cust-no       eq ar-inv.cust-no
                                               and ar-invl.inv-no        eq ar-inv.inv-no
                                               and ar-invl.posted no-lock:
 
               do i = 1 to 5:
                  if v-rate[i] eq 0 then next.
                  if not ar-invl.tax THEN DO:

                     v-taxable[i] = v-taxable[i] - ar-invl.amt.

                  END.
                  ELSE
                      IF v-frtr[i] = 0 AND ar-inv.freight > 0 THEN
                         v-taxable[i] = v-taxable[i] - ar-inv.freight.
               end.
            end. /* for each ar-invl */
         end. /* avail ar-inv */
         else
            if tt-report.key-02 eq stax.tax-acc1[1] then do:
/*        Per Julie, take out anything related to ar-cashl in this column
               find ar-cashl where recid(ar-cashl) eq tt-report.rec-id no-lock no-error.
             
               if ar-cashl.actnum eq stax.tax-acc1[1] then
                  do i = 1 to 5:
                     if v-rate[i] eq 0 then next.
             
                     v-inv-tax = (ar-cashl.amt-paid - ar-cashl.amt-disc).
             
                     if v-rate-tt[i] ne 0 then
                        v-tax-amt[i] = v-tax-amt[i] + (v-inv-tax * (v-rate[i] / v-rate-tt[i])).
                  end.
               else do:
                  v-sal-amt[1] = v-sal-amt[1] + (ar-cashl.amt-paid - ar-cashl.amt-disc).
                  do i = 1 to 5:
                     if v-rate[i] eq 0 then next.

                     if v-rate-tt[i] ne 0 then
                        v-taxable[i] = v-taxable[i] + ((ar-cashl.amt-paid - ar-cashl.amt-disc) * (v-rate[i] / v-rate-tt[i])).

                  end.
               end. /* else do: */
               */
            end. /* if tt-report.key-02 eq stax.tax-acc1[1] */
   
         if last-of(tt-report.key-01) then do:
            display 
               stax.tax-group    format "x(4)"
               stax.tax-dscr[1]  at 6    format "x(22)"
               v-sal-amt[1]
               v-taxable[1]
               v-tax-amt[1]
               v-taxable[2]      when v-tax-gr[2] ne ""
               v-tax-amt[2]      when v-tax-gr[2] ne ""
               v-taxable[3]      when v-tax-gr[3] ne ""
               v-tax-amt[3]      when v-tax-gr[3] ne ""
               v-taxable[4]      when v-tax-gr[4] ne ""
               v-tax-amt[4]      when v-tax-gr[4] ne ""
               v-taxable[5]      when v-tax-gr[5] ne ""
               v-tax-amt[5]      when v-tax-gr[5] ne ""
                  with frame detail no-box no-labels stream-io width 250.
             
            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                  '"' stax.tax-group                           '",'
                  '"' stax.tax-dscr[1]                         '",'
                  '"' STRING(v-sal-amt[1],"->>,>>>,>>9.99")    '",'
                  '"' STRING(v-taxable[1],"->>,>>>,>>9.99")    '",'
                  '"' STRING(v-tax-amt[1],"->>,>>>,>>9.99")    '",'
                  '"' IF v-tax-gr[2] NE "" THEN
                        STRING(v-taxable[2],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[2] NE "" THEN
                        STRING(v-tax-amt[2],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[3] NE "" THEN
                        STRING(v-taxable[3],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[3] NE "" THEN
                        STRING(v-tax-amt[3],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[4] NE "" THEN
                        STRING(v-taxable[4],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[4] NE "" THEN
                        STRING(v-tax-amt[4],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[5] NE "" THEN
                        STRING(v-taxable[5],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  '"' IF v-tax-gr[5] NE "" THEN
                        STRING(v-tax-amt[5],"->>,>>>,>>9.99")
                      ELSE ""                                  '",'
                  SKIP.
             
            assign
               v-sal-amt[2] = v-sal-amt[2] + v-sal-amt[1]
               v-sal-amt[1] = 0.
             
            do i = 1 to 5:
               ASSIGN
                  v-taxable[i + 5] = v-taxable[i + 5] + v-taxable[i]
                  v-tax-amt[i + 5] = v-tax-amt[i + 5] + v-tax-amt[i]
                  v-taxable[i] = 0
                  v-tax-amt[i] = 0.
             
            END.
         end. /* last-of(tt-report.key-01) then do: */
      end. /* for each tt-report */
   
      clear frame totals no-pause.
   
      display skip(1)
         "TOTALS:"                 at 6
         v-sal-amt[2]              to 42
         v-taxable[6]
         v-tax-amt[6]
         v-taxable[7]              when v-tax-gr[2] ne ""
         v-tax-amt[7]              when v-tax-gr[2] ne ""
         v-taxable[8]              when v-tax-gr[3] ne ""
         v-tax-amt[8]              when v-tax-gr[3] ne ""
         v-taxable[9]              when v-tax-gr[4] ne ""
         v-tax-amt[9]              when v-tax-gr[4] ne ""
         v-taxable[10]              when v-tax-gr[5] ne ""
         v-tax-amt[10]              when v-tax-gr[5] ne ""

         with frame totals no-box no-labels stream-io width 250.
   
      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED
            SKIP(1)
            '"' "TOTALS:"                                '",'
            '"' ""                                       '",'
            '"' STRING(v-sal-amt[2],"->>,>>>,>>9.99")    '",'
            '"' STRING(v-taxable[6],"->>,>>>,>>9.99")    '",'
            '"' STRING(v-tax-amt[6],"->>,>>>,>>9.99")    '",'
            '"' IF v-tax-gr[2] NE "" THEN
                  STRING(v-taxable[7],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[2] NE "" THEN
                  STRING(v-tax-amt[7],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[3] NE "" THEN
                  STRING(v-taxable[8],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[3] NE "" THEN
                  STRING(v-tax-amt[8],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[4] NE "" THEN
                  STRING(v-taxable[9],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[4] NE "" THEN
                  STRING(v-tax-amt[9],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[5] NE "" THEN
                  STRING(v-taxable[10],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            '"' IF v-tax-gr[5] NE "" THEN
                  STRING(v-tax-amt[10],"->>,>>>,>>9.99")
                ELSE ""                                  '",'
            SKIP(1).
   
   end.   /* do while true */
   
   IF tb_excel THEN DO:
      OUTPUT STREAM excel CLOSE.
     /* IF tb_runExcel THEN
         OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).*/
   END.
   
   
   
   /* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.


