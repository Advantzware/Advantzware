
/* ---------------------------------------------- cecrep/jobprem.p  */
/*  factory ticket                                                            */
/* -------------------------------------------------------------------------- */
/*  YSK 06/08/01  change local var v-out1-id, v-out2-id to shared var for despr~nt1.p  */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR lv-ord-qty LIKE oe-ordl.qty NO-UNDO.
DEFINE VARIABLE lFirstPage AS LOGICAL NO-UNDO.
{jcrep/r-ticket.i "shared"}

{cecrep/jobtickprm.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.
def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
 
def var laser           as   log    init no     format "Y/N"            no-undo.
def var v-vend-no       like oe-ordl.vend-no                            no-undo.
def var v-po-no         like oe-ordl.po-no-po                           no-undo.
def var v-qty-or-sup    as   char               format "x(38)"          no-undo.
def var v-i-line        as   char   extent 4    format "x(38)"          no-undo.
def var v-flag          as   log    init no                             no-undo.
def var v-local-copies  as   int                                        no-undo.
def var v-local-loop    as   int    init 1                              no-undo.
def var v-print-score   as   log    init yes                            no-undo.
def var v-pqty          as   dec                                        no-undo.
DEF VAR lv-part-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-rt-num AS INT NO-UNDO.
def stream ctl.
DEF VAR lv-m-dscr LIKE w-m.dscr NO-UNDO.
DEF VAR lv-add-entry AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
{cecrep/jc-prem.i}
DEF BUFFER b-ef FOR ef.
DEF workfile tt-wm LIKE w-m.
DEF VAR v-xg-flag AS LOG NO-UNDO.
DEF VAR v-tmp-stype AS cha NO-UNDO.
DEF VAR v-len-score2 AS cha EXTENT 13 NO-UNDO.
DEF VAR v-tmp-score AS cha NO-UNDO.
DEF VAR v-item-name AS cha NO-UNDO.
DEF VAR v-plate-loc LIKE prep.loc-bin NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF VAR tb_app-unprinted AS LOG NO-UNDO.

FUNCTION barCode RETURNS CHARACTER (ipBarCode AS CHARACTER):
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE dash AS INTEGER NO-UNDO.
  DEFINE VARIABLE rtnBarCode AS CHARACTER NO-UNDO.

  DO i = 1 TO LENGTH(ipBarCode):
    IF SUBSTR(ipBarCode,i,1) EQ '-' THEN dash = dash + 1.
  END.
  RETURN IF dash EQ 3 THEN SUBSTR(ipBarCode,1,R-INDEX(ipBarCode,'-') - 1)
         ELSE ipBarCode.
END FUNCTION.

{XMLOutput/XMLOutput.i &XMLOutput=XMLJobTicket &Company=cocode} /* rstark 05181205 */
RUN XMLOutput (lXMLOutput,'','','Header'). /* rstark 05181205 */
RUN XMLOutput (lXMLOutput,'',STRING(PAGE-NUM),'Page'). /* rstark 05181205 */

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.

assign
 v-line[1] = chr(95) + fill(chr(95),40) + chr(95) + "  " +
             chr(95) + fill(chr(95),40) + chr(95) + "  " +
             chr(95) + fill(chr(95),40) + chr(95)  
 v-line[2] = v-line[1]
 v-line[3] = chr(95) + fill(chr(95),128) + chr(95)
 v-line[4] = v-line[3]
 v-line[5] = chr(95) + fill(chr(95),84) + chr(95) + "  " +
                chr(95) + fill(chr(95),40) + chr(95)
 v-line[6] = v-line[5]
 v-line[7] = chr(95) + fill(chr(95),25) + chr(95) + "  " +
             chr(95) + fill(chr(95),99) + chr(95)
 v-line[8] = v-line[7]
 v-qty-or-sup = if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat,ASI") gt 0
                then ("Supplier: "     + fill("_",28))
                else ("Qty Received: " + fill("_",24)).


    assign /*v-local-copies = if lookup(printer.pr-port, "{&PR-PORT}") eq 0 then 
                              prt-copies
                            else 1  */
           v-local-copies = 1
           prt-copies = 1.
lFirstPage = YES.
do v-local-loop = 1 to v-local-copies:
  {cecrep/jobprem.i}
      break by job.job-no BY job.job-no2:

  /*  for each job-hdr
        where job-hdr.company               eq cocode
          and job-hdr.ftick-prnt            eq reprint

          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no

          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no,

        first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
        no-lock,
        
        first est
        where est.company = job.company
          AND est.est-no                    eq job.est-no
          and est.est-type                  gt 4
        no-lock,

        first cust
        where cust.company                  eq cocode
          and cust.cust-no                  eq job-hdr.cust-no
        no-lock,

        first itemfg
        where itemfg.company                eq cocode
          and itemfg.i-no                   eq job-hdr.i-no
        no-lock

              break by job.job-no
              by job.job-no2:
*/
      v-break = first-of(job.job-no2).

      release xest.
      release xef.
      release xeb.
      release xoe-ord.
      release xoe-ordl.
      release xoe-rel.

      run cecrep/jobtickprm.p (recid(job-hdr), v-format,
                              v-local-loop, v-local-copies).

      for each w-ef WHERE w-ef.frm EQ job-hdr.frm OR est.est-type NE 8:
        release xef.
        release xeb.
        release xstyle.
        release xxprep.
        
        run cecrep/jobtick2.p (recid(w-ef), recid(job-hdr)).

        v-pqty = 1.
        if avail xeb then do:
          if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          if xest.est-type eq 6 then v-fg = trim(v-fg) + "  CP#: " +
                                            xeb.part-no.
          
          {cec/rollfac.i}
          v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                   if xeb.quantityPerSet lt 0 then (-1 / xeb.quantityPerSet)
                                       else xeb.quantityPerSet.

/*         FIND first eb WHERE eb.company     EQ job-hdr.company                      */
/*                         AND eb.est-no      eq job-hdr.est-no                       */
/*                         and eb.form-no     eq job-hdr.frm                          */
/*                         AND eb.stock-no = job-hdr.i-no NO-LOCK NO-ERROR.           */
/*         IF NOT AVAIL eb THEN FIND first eb WHERE eb.company     EQ job-hdr.company */
/*                         AND eb.est-no      eq job-hdr.est-no                       */
/*                         and eb.form-no     eq job-hdr.frm                          */
/*                         AND eb.blank-no > 0 NO-LOCK NO-ERROR.                      */
        ASSIGN
          v-bar-no = /*IF AVAIL eb THEN eb.spc-no ELSE */ trim(job-hdr.job-no) + "-" + trim(STRING(job-hdr.job-no2,"99")).
          /* v-bar-no = barCode(v-bar-no).*/


        end.
        
        assign
         v-loc     = ""
         v-loc-bin = "".
         
        if v-format eq "Brick" or v-format eq "Corrugat" OR  v-format eq "ASI"
        then do: 
          v-iso = "ISO# CS-05-1-F".

          release fg-rdtlh.
          
          find first fg-bin
            where fg-bin.company   eq cocode
              and fg-bin.i-no      eq job-hdr.i-no
              and fg-bin.job-no    eq job-hdr.job-no
              and fg-bin.job-no2   eq job-hdr.job-no2
              and fg-bin.loc       eq job-hdr.loc
              and fg-bin.qty       ne 0
          no-lock no-error.
          if avail fg-bin then do:
            assign
              v-loc     = "Whs: " + fg-bin.loc
              v-loc-bin = "Bin: " + fg-bin.loc-bin.
          end.
          else
          if avail itemfg then do:                             
            assign
              v-loc     = "Whs: " + itemfg.def-loc
              v-loc-bin = "Bin: " + itemfg.def-loc-bin.
          end.

        end. /*brick format*/
        IF NOT lFirstPage THEN 
          PAGE.
        lFirstPage = NO.
        view frame head.  /* factory header display  */
        
        /* rstark 05181205 */
        RUN XMLOutput (lXMLOutput,'JobTicketHeader','','Row').
        RUN XMLOutput (lXMLOutput,'Page',STRING(PAGE-NUM),'Col').
        RUN XMLOutput (lXMLOutput,'User_ID',USERID('NoSweat'),'Col').
        RUN XMLOutput (lXMLOutput,'Job',v-job-prt,'Col').
        RUN XMLOutput (lXMLOutput,'Estimate',v-est-no,'Col').
        RUN XMLOutput (lXMLOutput,'Order',v-ord-no,'Col').
        RUN XMLOutput (lXMLOutput,'FGItem',v-fg,'Col').
        RUN XMLOutput (lXMLOutput,'Order_Date',v-ord-date,'Col').
        RUN XMLOutput (lXMLOutput,'Due_Date',v-due-date,'Col').
        /* rstark 05181205 */
        
        if v-format eq "RFC" or v-format eq "Boxtech" then
          assign
           v-i-line[1] = itemfg.i-name
           v-i-line[2] = itemfg.part-dscr1
           v-i-line[3] = itemfg.part-dscr2
           v-i-line[4] = itemfg.part-dscr3.
        ELSE DO:
          assign
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = "Style: " + if avail xstyle then xstyle.dscr else ""
           v-i-line[3] = "Size: "  + if avail xeb    then
                     trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
           v-i-line[4] = "Joint: " + if avail xeb then v-joint-dscr else "".
        END.
        
       /*===== for xprint */

        PUT "C".
        /* 3 Boxes for customer */
        PUT "<#2><R+6><C+26><RECT#2><|3>"
            "<#3><R-6><C+26><RECT#3><|3>"
            "<#4><R+6><C+26><RECT#4><|3>" SKIP.
        /* 3 boxes for board */
        PUT "B".
        /* 3 Boxes for customer */
        PUT "<#5><R+6><C+26><RECT#5><|3>"
            "<#6><R-6><C+26><RECT#6><|3>"
            "<#7><R+6><C+26><RECT#7><|3>" SKIP.

        PUT "<=#2>" SKIP
            "U" SKIP
            "S" SKIP
            "T" SKIP.
        PUT "<=#5>" SKIP
            "O" SKIP 
            "A" SKIP
            "R" SKIP
            "D" SKIP.
            
  /*      PUT "<=#8><R+1><C-1>O" 
            "<=#8><R+2><C-1>U" 
            "<=#8><R+3><C-1>T" 
            "<=#8><R+4><C-1>E" .
  */
        lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.
        lv-ord-qty = (if avail xoe-ordl then xoe-ordl.qty
                     else job-hdr.qty) * 
                     (if est.form-qty le 1 then 1 else v-pqty).

        DISP "<=#2> CUSTOMER INFORMATION" SKIP
              v-cus[1] AT 3 SKIP
              v-cus[2] AT 3 SKIP
              v-cus[3] AT 3 SKIP
              v-cus[4] AT 3 SKIP
            "<=#3><R-6> ORDER INFORMATION"
            "<=#3><R-5> PO #:" 
            xoe-ord.po-no WHEN AVAIL xoe-ord
            "Set Qty:"
            trim(string(if avail xoe-ordl then xoe-ordl.qty
                                          else job-hdr.qty,">>>,>>9"))
                        when avail xeb and xeb.est-type eq 6    format "x(9)"
            "<=#3><R-4> Job Qty:"
             trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
            "Order Qty:"
            trim(string( (if avail xoe-ordl then xoe-ordl.qty
                                             else job-hdr.qty) *
                                            if est.form-qty le 1 then 1
                                            else v-pqty,">>>,>>9"))
                                            format "x(7)"
            "<=#3><R-3> Cust Part #:" lv-part-no 
            "<=#3><R-2> Overrun:" 
             trim(string((IF AVAIL xoe-ordl THEN xoe-ordl.over-pct ELSE
                          IF AVAIL xoe-ord  THEN xoe-ord.over-pct  ELSE 0),">>9.99%")) format "x(7)"                                                         
            "Underrun:"
            trim(string((IF AVAIL xoe-ordl THEN xoe-ordl.under-pct ELSE
                         IF AVAIL xoe-ord  THEN xoe-ord.under-pct  ELSE 0),">>9.99%")) format "x(7)"
            "<=#3><R-1>        " lv-ord-qty * ( 1 + round((IF AVAIL xoe-ordl THEN xoe-ordl.over-pct ELSE
                                                           IF AVAIL xoe-ord  THEN xoe-ord.over-pct  ELSE 0) / 100,2)) FORM ">>>,>>9"
                       "         "     lv-ord-qty * ( 1 - round((IF AVAIL xoe-ordl THEN xoe-ordl.under-pct ELSE
                                                                 IF AVAIL xoe-ord  THEN xoe-ord.under-pct  ELSE 0) / 100,2)) FORM ">>>,>>9"
            WITH NO-BOX NO-LABELS NO-ATTR-SPACE WIDTH 145 STREAM-IO.

    /* rstark 05181205 */
    RUN XMLOutput (lXMLOutput,'Customer_Name',v-cus[1],'Col').
    RUN XMLOutput (lXMLOutput,'Address_1',v-cus[2],'Col').
    RUN XMLOutput (lXMLOutput,'Address_2',v-cus[3],'Col').
    RUN XMLOutput (lXMLOutput,'Address_3',v-cus[4],'Col').
    IF AVAIL xoe-ord THEN
    RUN XMLOutput (lXMLOutput,'PO_Number',xoe-ord.po-no,'Col').
    IF AVAIL xeb AND xeb.est-type EQ 6 THEN
    RUN XMLOutput (lXMLOutput,'Set_Qty',TRIM(STRING(IF AVAIL xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty)),'Col').
    RUN XMLOutput (lXMLOutput,'Job_Qty',TRIM(STRING(job-hdr.qty * v-pqty)),'Col').
    RUN XMLOutput (lXMLOutput,'Order_Qty',TRIM(STRING((IF AVAIL xoe-ordl THEN xoe-ordl.qty ELSE job-hdr.qty) *
                                                       IF est.form-qty LE 1 THEN 1 ELSE v-pqty,">>>,>>9")),'Col').
    RUN XMLOutput (lXMLOutput,'Customer_Part',lv-part-no,'Col').
    RUN XMLOutput (lXMLOutput,'OverRun',TRIM(STRING((IF AVAIL xoe-ordl THEN xoe-ordl.over-pct ELSE
                                                     IF AVAIL xoe-ord  THEN xoe-ord.over-pct  ELSE 0),">>9.99%")),'Col').
    RUN XMLOutput (lXMLOutput,'UnderRun',TRIM(STRING((IF AVAIL xoe-ordl THEN xoe-ordl.under-pct ELSE
                                                      IF AVAIL xoe-ord  THEN xoe-ord.under-pct  ELSE 0),">>9.99%")),'Col').
    IF NOT (v-format EQ "RFC" OR v-format EQ "Boxtech") THEN DO:
      IF AVAIL xstyle THEN
      RUN XMLOutput (lXMLOutput,'Style',xstyle.dscr,'Col').
      IF AVAIL xeb THEN
      RUN XMLOutput (lXMLOutput,'Size',TRIM(STRING({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                                       TRIM(STRING({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                                       TRIM(STRING({sys/inc/k16v.i xeb.dep},">,>>9.99")),'Col').
      IF AVAIL xeb THEN
      RUN XMLOutput (lXMLOutput,'Joint',v-joint-dscr,'Col').
    END.
    DO i = 1 TO 4:
      RUN XMLOutput (lXMLOutput,'Line_' + STRING(i),v-i-line[i],'Col').
    END.
    RUN XMLOutput (lXMLOutput,'Adders',v-adders,'Col').
    RUN XMLOutput (lXMLOutput,'Sheets_Required',TRIM(STRING(v-sht-qty)),'Col').
    RUN XMLOutput (lXMLOutput,'SqFt',TRIM(STRING(v-form-sqft)),'Col').
    IF AVAILABLE xeb THEN
    RUN XMLOutput (lXMLOutput,'Plate',xeb.plate-no,'Col').
    /* rstark 05181205 */

     PUT UNFORMATTED
            "<=#4> " v-i-line[1] FORM "x(40)"
            "<=#4><R+.6>" " Name: " (IF AVAIL xeb THEN xeb.part-dscr1 else itemfg.i-name) FORMAT "x(36)"
            "<=#4><R+1.2> " v-i-line[2] FORM "x(40)"
            "<=#4><R+1.8>" " CAD#: " (IF AVAIL xeb THEN xeb.cad-no ELSE itemfg.cad-no) FORMAT "x(30)"
            "<=#4><R+2.4> " v-i-line[3] FORM "x(40)"
            "<=#4><R+3.2> " v-i-line[4] FORM "x(40)"
            "<=#4><R+4> Adders:" v-adders FORM "x(33)" .
                    
        v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
                                      else (v-form-len * v-form-wid / 144),3).
        
        display "<=#5> Shts Req'd:"
                trim(string(v-sht-qty))   format "x(9)"
                " Sq Ft:"
                trim(string(v-form-sqft)) format "x(7)"
                "<=#6><R-6> PRINTING PLATE #:"
                xeb.plate-no when avail xeb
                "<=#7> DIE CUTTING, SLIT, & SAW"                
            with no-box no-labels frame m2 width 145 NO-ATTR-SPACE STREAM-IO.

        i = 0.
        for each w-i:
          i = i + 1.
        end.
        if i lt 4 then do i = i + 1 to 4:
          create w-i.
        end.

        find first w-i.
        v-plate-loc = "".
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.plate-no
                            no-lock no-error.
        IF AVAIL xxprep THEN
            v-plate-loc = "Bin: " + xxprep.loc-bin + "   Whse: " + xxprep.loc.

        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.die-no
                            no-lock no-error.
        v-die-loc = IF AVAIL xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

        display /* Line 2, 2nd row of boxes */
                "<=#5><R+1>"
                "W: " + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
                "   " +
                "L: " + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))
                                                                format "x(25)"
                "MSF:"  +
                trim(string(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
                                                                format "x(11)"
                "<=#6><R-5>"
                /* xeb.plate-no when avail xeb */
                v-plate-loc FORMAT "x(25)"
                /* w-i.i-dscr  - Put Die# here */
                /* w-i.i-qty when w-i.i-qty ne 0 */
                /* "LBS" when w-i.i-dscr ne "" */
                
                "<=#7><R+1> Die #:"
                xeb.die-no when avail xeb
                " Loc:"   /*v-die-loc */ 
                xxprep.loc-bin when avail xeb and avail xxprep      
            with no-box no-labels frame i10 width 150 no-attr-space STREAM-IO.

         /* rstark 05181205 */
         RUN XMLOutput (lXMLOutput,'Width',TRIM(STRING({sys/inc/k16v.i v-form-wid})),'Col').
         RUN XMLOutput (lXMLOutput,'Length',TRIM(STRING({sys/inc/k16v.i v-form-len})),'Col').
         RUN XMLOutput (lXMLOutput,'MSF',TRIM(STRING(v-sht-qty * v-form-sqft / 1000)),'Col').
         RUN XMLOutput (lXMLOutput,'Ink_1',w-i.i-dscr,'Col').
         RUN XMLOutput (lXMLOutput,'LBS_1',w-i.i-qty,'Col').
         IF AVAIL xeb THEN
         RUN XMLOutput (lXMLOutput,'Die',xeb.die-no,'Col').
         IF AVAIL xeb AND AVAIL xxprep THEN
         RUN XMLOutput (lXMLOutput,'Location',xxprep.loc-bin,'Col').
         /* rstark 05181205 */

        /* find next w-i.         */
        find first w-i.

        /* Line 3, 2nd row of boxes */
        display "<=#5><R+2> Board:"
                v-form-code
                "<=#6><R-4> Ink 1:"
                w-i.i-dscr
                w-i.i-qty when w-i.i-qty ne 0
                "LBS" when w-i.i-dscr ne ""
                "<=#7><R+2> Blank Size:"
                "W:" TRIM(string({sys/inc/k16v.i xeb.t-wid},">,>>9.99")) WHEN AVAIL xeb 
                "L:" TRIM(string({sys/inc/k16v.i xeb.t-len},">,>>9.99")) when avail xeb 
            with no-box no-labels frame i2 width 155 no-attr-space STREAM-IO.

         /* rstark 05181205 */
         RUN XMLOutput (lXMLOutput,'Board',v-form-code,'Col').
         RUN XMLOutput (lXMLOutput,'Ink_2',w-i.i-dscr,'Col').
         RUN XMLOutput (lXMLOutput,'LBS_2',w-i.i-qty,'Col').
         RUN XMLOutput (lXMLOutput,'Blank_Width',TRIM(STRING({sys/inc/k16v.i v-form-wid})),'Col').
         RUN XMLOutput (lXMLOutput,'Blank_Length',TRIM(STRING({sys/inc/k16v.i v-form-len})),'Col').
         /* rstark 05181205 */

        find next w-i.
        display "<=#5><R+3><C+5> " v-form-dscr
                "<=#6><R-3> Ink 2:"
                w-i.i-dscr
                w-i.i-qty when w-i.i-qty ne 0
                "LBS" when w-i.i-dscr ne ""
                "<=#7><R+3> Up:"
                "W:" v-upl FORM ">9" 
                "  L:"  v-upw FORM ">9"
                "Slit:  W:"  v-outw FORM ">9"
                "L:"  v-outl FORM ">9"                                          
            with no-box no-labels frame i3 width 155 no-attr-space STREAM-IO.

        /* rstark 05181205 */
        RUN XMLOutput (lXMLOutput,'Form',v-form-dscr,'Col').
        RUN XMLOutput (lXMLOutput,'Ink_3',w-i.i-dscr,'Col').
        RUN XMLOutput (lXMLOutput,'LBS_3',w-i.i-qty,'Col').
        RUN XMLOutput (lXMLOutput,'Up_Width',v-upl,'Col').
        RUN XMLOutput (lXMLOutput,'Up_Length',v-upw,'Col').
        RUN XMLOutput (lXMLOutput,'Slit_Width',v-outw,'Col').
        RUN XMLOutput (lXMLOutput,'Slit_Length',v-outl,'Col').
        /* rstark 05181205 */
        
        find next w-i.
        /* display score type task# 04190502 */ 
        DEF VAR v-score-type AS cha NO-UNDO.

        j = 0.
        v-xg-flag = (xef.xgrain eq "S" AND xef.est-type GE 5) OR
                     xef.xgrain eq "B".
        v-len-score2 = "".
        IF v-len-score <> "" THEN DO:
           v-tmp-score = " " /*SUBSTRING(v-len-score,1,1).*/.
           DO i = 2 TO LENGTH(v-len-score):
              IF (SUBSTRING(v-len-score,i,1) = "" AND
                 SUBSTRING(v-len-score,i - 1,1) <> "") or
                 i = LENGTH(v-len-score)
              THEN DO:
                 j = j + 1.
                 v-tmp-stype = IF v-xg-flag THEN xeb.k-len-scr-type2[j]
                               ELSE xeb.k-wid-scr-type2[j].
                 /*SUBSTRING(v-len-score,i,1) = v-tmp-stype.*/
                 v-len-score2[j] = v-tmp-score + v-tmp-stype.
                 v-tmp-score = "".
              END.
              ELSE v-tmp-score = v-tmp-score + " " /*SUBSTRING(v-len-score,i,1)*/.
           END.          

           v-score-type = "Type :".  
           DO i = 1 TO 13:
              IF v-len-score2[i] <> "" THEN v-score-type = v-score-type + v-len-score2[i] .
           END.
        END.
        /* 6th Line 2nd Row of boxes */ 
        display "<=#5><R+4> Score:"
                 v-len-score     WHEN xstyle.TYPE <> "F"  format "x(32)"
                "<=#6><R-2> Ink 3:"
                w-i.i-dscr
                w-i.i-qty when w-i.i-qty ne 0
                "LBS" when w-i.i-dscr ne ""
                "<=#7><R+4> Impressions:"
                trim(string(v-dc-qty))    format "x(7)"
                " To: " +
                trim(string({sys/inc/k16v.i xef.nsh-wid},">>9.99")) +
                "x" +
                trim(string({sys/inc/k16v.i xef.nsh-len},">>9.99"))
                          when avail xef and 
                               (xef.nsh-wid ne xef.gsh-wid or
                                xef.nsh-len ne xef.gsh-len)
                                          format "x(17)"
            with no-box no-labels frame i4 width 155 no-attr-space STREAM-IO.

        /* rstark 05181205 */
        IF xstyle.type NE 'F' THEN
        RUN XMLOutput (lXMLOutput,'Score',v-len-score,'Col').
        RUN XMLOutput (lXMLOutput,'Ink_4',w-i.i-dscr,'Col').
        RUN XMLOutput (lXMLOutput,'LBS_4',w-i.i-qty,'Col').
        RUN XMLOutput (lXMLOutput,'Impression',TRIM(STRING(v-dc-qty)),'Col').
        IF AVAIL xef AND (xef.nsh-wid NE xef.gsh-wid OR xef.nsh-len ne xef.gsh-len) THEN DO:
          RUN XMLOutput (lXMLOutput,'Impression_Width',TRIM(STRING({sys/inc/k16v.i xef.nsh-wid})),'Col').
          RUN XMLOutput (lXMLOutput,'Impression_Length',TRIM(STRING({sys/inc/k16v.i xef.nsh-len})),'Col').
        END.
        /* rstark 05181205 */

        IF LOOKUP(v-format,"TriState,RFC,Boxtech,Brick,Corrugat,ASI,Xprint,Pacific") GT 0 THEN DO:
          RUN sys/ref/getpo#.p (IF AVAIL xoe-ordl AND est.est-type NE 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
                                w-ef.frm, OUTPUT v-po-no).

          RELEASE po-ord.
          
          IF v-po-no NE 0 THEN
          FIND FIRST po-ord
              WHERE po-ord.company EQ cocode
                AND po-ord.po-no   EQ v-po-no
              NO-LOCK NO-ERROR.
          
          ASSIGN
           v-vend-no    = IF AVAIL po-ord THEN po-ord.vend-no ELSE ""
           v-qty-or-sup = "Supplier: ".
          
          IF v-vend-no NE "" THEN DO:
            v-qty-or-sup = v-qty-or-sup + TRIM(v-vend-no).
            
            IF v-po-no NE 0 THEN
              v-qty-or-sup = v-qty-or-sup + "  PO#: " +
                             TRIM(STRING(v-po-no,">>>>>>>>>>")).
          END.
        END.
          
        ELSE v-qty-or-sup = "Qty Received: " + fill("_",24).
          
        display "<=#5><R+5>" v-score-type FORM "x(32)" /*v-qty-or-sup*/
                     "<=#6><R-1> Color Desc:"
                     "<=#6><R-1><C+12> " xeb.i-coldscr when avail xeb
                     "<=#7><R+5> D/C Style:"                             
                         with no-box no-labels frame m3 width 132 no-attr-space STREAM-IO.

        i = 0.
        for each w-m:
          i = i + 1.
        end.
        if i lt 3 then do i = i + 1 to 3:
          create w-m.
          w-m.dseq = 999999999.
        end.
            /* box for route */
        
        lv-rt-num = i + 3.
        PUT SKIP " " SKIP " ".        
        PUT UNFORMATTED "<#8><R+" lv-rt-num "><C+78><RECT#8><|3>" SKIP.                

        /* rstark 05181205 */
        IF xstyle.type NE 'F' THEN
        RUN XMLOutput (lXMLOutput,'Score_Type',v-score-type,'Col').
        IF AVAIL xeb THEN
        RUN XMLOutput (lXMLOutput,'Color_Description',xeb.i-coldscr,'Col').
        RUN XMLOutput (lXMLOutput,'DC_Style',lv-rt-num,'Col').
        RUN XMLOutput (lXMLOutput,'/JobTicketHeader','','Row').
        /* rstark 05181205 */
        
        /* box for pack */
        PUT " ".        
        PUT "<#9><R+6><C+78><RECT#9><|3>" SKIP.       
        /* box for notes */
        PUT " ".        
        PUT "<#10><R+7><C+78><RECT#10><|3>" SKIP.

        if v-format eq "Brick" OR v-format = "ASI" then 
             put "<=#8> Machine Routing        SU:    Start    Stop    Total   RUN:  Hours   Start   Stop   Total     QTY: In     Out      Waste" SKIP.
        ELSE PUT "<=#8> Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP.
        
        PUT "<=#8><R+1><C+1><from><C+76><Line>" SKIP.
        i = 0.

        for each w-m by w-m.dseq:
          i = i + 1.
          FIND first mach where mach.company eq cocode
                            and mach.m-dscr  eq w-m.dscr NO-LOCK NO-ERROR.
          /*lv-m-dscr = IF AVAIL mach THEN mach.m-code ELSE w-m.dscr.
          v-letter = substr("UTE",i,1).*/   
          display w-m.dscr AT 3
                  w-m.s-hr when w-m.s-hr ne 0
                  fill("_",7)  format "x(7)"    to 38   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 46   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 54   when w-m.dscr ne ""
                  space(2)
                  w-m.r-sp when w-m.r-sp ne 0
                  fill("_",7)  format "x(7)"    to 69   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 77   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 85   when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 99   when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 108  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 117  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 129  when w-m.dscr ne ""
                  /*chr(124) format "x"           at 131   */                  
              with no-box no-labels frame o2 width 132 no-attr-space down STREAM-IO.
          v-lines = v-lines + 1.
          /* rstark 05181205 */
          IF w-m.dscr NE '' THEN DO:
            RUN XMLOutput (lXMLOutput,'JobTicketRouting','','Row').
            RUN XMLOutput (lXMLOutput,'Machine_Routing',w-m.dscr,'Col').
            RUN XMLOutput (lXMLOutput,'SU',w-m.s-hr,'Col').
            RUN XMLOutput (lXMLOutput,'Run',w-m.r-sp,'Col').
            RUN XMLOutput (lXMLOutput,'/JobTicketRouting','','Row').
          END.
          /* rstark 05181205 */
        END.

        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                           AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
        IF AVAIL b-ef AND b-ef.form-no = w-ef.frm THEN 
           FOR EACH w-m:
               CREATE tt-wm.
               BUFFER-COPY w-m TO tt-wm.
        END.

        PUT "<=#8>"
            SKIP
            "R" SKIP
            "O" SKIP
            "U" SKIP
            "T" SKIP
            "E" SKIP.

        PUT SKIP(1).
        run cecrep/jobtick3.p (recid(job-hdr),v-format,cust.terms).
        ASSIGN v-tmp-lines = 0
               j = 0
               K = 0
               lv-got-return = 0
               v-dept-inst = "".
        
        {custom/notespr2.i job v-inst2 6 "notes.rec_key = job.rec_key and
                           (notes.note_form_no = w-ef.frm /*OR notes.note_form_no = 0*/)" }
        DO i = 1 TO 6:
             v-dept-inst[i] = v-inst2[i].
        END.
        IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */

        PUT "<=#10>" SKIP
                "N" SKIP
                "O" SKIP
                "T" SKIP
                "E" SKIP
                "S" SKIP.
        display "<=#10><R-1>"
                "SPECIAL INSTRUCTIONS" AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                "SHIPPING INFO"
                "      Ship To #:"
                xoe-ord.sold-id when avail xoe-ord
                xeb.ship-id when avail xeb @ xoe-ord.sold-id
                xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id
              /*chr(124) format "x"       at 131 */
                
              /*"O"                       at 1*/
              /*entry(1,v-inst) */ v-dept-inst[1] FORM "x(82)" AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                v-shp[1]
              /*"T"                       at 1*/
              /*ENTRY(2,v-inst)*/ v-dept-inst[2] format "x(82)" AT 3 
                /*chr(124) format "x"       at 2 */
                /*substr(v-inst,144,082)*/             
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                v-shp[2]
              /*"E"                       at 1    */
              /*ENTRY(3,v-inst)*/ v-dept-inst[3] format "x(82)" AT 3   
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                v-shp[3]
              /* "S"                       at 1*/
              /*ENTRY(4,v-inst)*/ v-dept-inst[4] format "x(82)" AT 3 
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                v-shp[4]
              /*ENTRY(5,v-inst)*/ v-dept-inst[5] format "x(82)" AT 3
                chr(124) format "x"       at 87
                chr(124) format "x"       at 90
                "Item PO #:"
                xoe-ordl.po-no when avail xoe-ordl
              /*ENTRY(6,v-inst) */ v-dept-inst[6] format "x(128)" AT 3
                skip(1)
            with no-box no-labels frame m8 width 132 no-attr-space STREAM-IO.
        
        /* rstark 05181205 */
        RUN XMLOutput (lXMLOutput,'JobTicketNotes','','Row').
        RUN XMLOutput (lXMLOutput,'Note_1',v-dept-inst[1],'Col').
        RUN XMLOutput (lXMLOutput,'Note_2',v-dept-inst[2],'Col').
        RUN XMLOutput (lXMLOutput,'Note_3',v-dept-inst[3],'Col').
        RUN XMLOutput (lXMLOutput,'Note_4',v-dept-inst[4],'Col').
        RUN XMLOutput (lXMLOutput,'Note_5',v-dept-inst[5],'Col').
        RUN XMLOutput (lXMLOutput,'Note_6',v-dept-inst[6],'Col').
        RUN XMLOutput (lXMLOutput,'/JobTicketNotes','','Row').

        RUN XMLOutput (lXMLOutput,'JobTicketShipTo','','Row').
        IF AVAIL xoe-rel THEN
        RUN XMLOutput (lXMLOutput,'Ship_To',xoe-rel.ship-id,'Col').
        ELSE IF AVAIL xeb THEN
        RUN XMLOutput (lXMLOutput,'Ship_To',xeb.ship-id,'Col').
        ELSE IF AVAIL xoe-ord THEN
        RUN XMLOutput (lXMLOutput,'Ship_To',xoe-ord.sold-id,'Col').
        RUN XMLOutput (lXMLOutput,'Ship_1',v-shp[1],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_2',v-shp[2],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_3',v-shp[3],'Col').
        RUN XMLOutput (lXMLOutput,'Ship_4',v-shp[4],'Col').
        IF AVAIL xoe-ordl THEN
        RUN XMLOutput (lXMLOutput,'Item_PO',xoe-ordl.po-no,'Col').
        RUN XMLOutput (lXMLOutput,'/JobTicketShipTo','','Row').
        /* rstark 05181205 */

        if print-box and avail xest then do:            
            run cec/desprnt3.p (recid(xef),
                               input-output v-lines,
                               recid(xest)).
        end.
        ELSE PAGE.
        
      end.  /* for each w-ef */
      
      IF s-prt-set-header AND last-of(job.job-no2) AND est.est-type = 6 THEN DO: /* print set header */
        i = 0.
        FOR EACH bf-eb WHERE bf-eb.company = est.company
                          AND bf-eb.est-no = est.est-no
                          AND bf-eb.form-no > 0 NO-LOCK:
             i = i + 1.
        END.   
        IF i > 1 THEN DO:
           DEF VAR v-set-qty AS INT NO-UNDO.
           DEF VAR v-ord-qty AS INT NO-UNDO.
           DEF VAR v-over-run AS cha NO-UNDO.
           DEF VAR v-under-run AS cha NO-UNDO.
           DEF VAR v-fg-set AS cha FORM "x(15)" NO-UNDO.
           v-fg-set = job-hdr.i-no.
           v-set-qty = if avail xeb and xeb.est-type eq 6 THEN
                         if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty
                       ELSE 0.
           v-ord-qty = (if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty) /**
                       if est.form-qty le 1 then 1 else v-pqty*/.
           v-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                        IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE "".
           v-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                         IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE "".
           PAGE.                         
           PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(1)
               "User Id:" AT 3 v-user-id   "<c24>"
               "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
               "<C60>Our Date: " v-ord-date SKIP
               "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date  SKIP
               "<=1><R+7><C2><From><R+5><C78><RECT><||3>" SKIP
               "<=1><R+7><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C53>ITEM DESCRIPTION"  SKIP
               v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
               v-i-line[2] AT 90
               SKIP
               v-cus[2] AT 3 " Job Qty:" trim(string(job-hdr.qty /** v-pqty*/ ,">>>,>>9"))    format "x(7)"
               " Order Qty:" string(v-ord-qty) format "x(7)"
               v-i-line[3] AT 90 SKIP
               v-cus[3] AT 3  " Cust Part #:" lv-part-no 
               v-i-line[4] AT 90 SKIP
               v-cus[4]  AT 3 " Overrun:"  format "x(7)"  
               " Underrun:" format "x(7)"  
               "Adders:" v-adders FORM "x(33)" AT 90 SKIP
               "<=1><R+12><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
               "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
           /* each components */
           
           /* rstark 05181205 */
           RUN XMLOutput (lXMLOutput,'JobTicketSetHeader','','Row').
           RUN XMLOutput (lXMLOutput,'Page',STRING(PAGE-NUM),'Col').
           RUN XMLOutput (lXMLOutput,'User_ID',v-user-id,'Col').
           RUN XMLOutput (lXMLOutput,'Job',v-job-prt,'Col').
           RUN XMLOutput (lXMLOutput,'Estimate',v-est-no,'Col').
           RUN XMLOutput (lXMLOutput,'Our_Order',v-ord-no,'Col').
           RUN XMLOutput (lXMLOutput,'FGItem',v-fg-set,'Col').
           RUN XMLOutput (lXMLOutput,'Our_Date',v-ord-date,'Col').
           RUN XMLOutput (lXMLOutput,'Due_Date',v-due-date,'Col').
           RUN XMLOutput (lXMLOutput,'Customer_1',v-cus[1],'Col').
           RUN XMLOutput (lXMLOutput,'Customer_2',v-cus[2],'Col').
           RUN XMLOutput (lXMLOutput,'Customer_3',v-cus[3],'Col').
           RUN XMLOutput (lXMLOutput,'Customer_4',v-cus[4],'Col').
           RUN XMLOutput (lXMLOutput,'PO',v-po-no,'Col').
           RUN XMLOutput (lXMLOutput,'Set_Qty',v-set-qty,'Col').
           RUN XMLOutput (lXMLOutput,'Job_Qty',TRIM(STRING(job-hdr.qty)),'Col').
           RUN XMLOutput (lXMLOutput,'Order_Qty',STRING(v-ord-qty),'Col').
           RUN XMLOutput (lXMLOutput,'Customer_Part',lv-part-no,'Col').
           RUN XMLOutput (lXMLOutput,'Adders',v-adders,'Col').
           RUN XMLOutput (lXMLOutput,'SetItem',v-fg-set,'Col').
           RUN XMLOutput (lXMLOutput,'/JobTicketSetHeader','','Row').
           /* rstark 05181205 */
           
           DEF VAR v-tmp-line AS INT NO-UNDO.
           DEF VAR v-shipto AS cha NO-UNDO.

           v-tmp-line = 0.
           FOR EACH xeb WHERE xeb.company = est.company
                           AND xeb.est-no = est.est-no
                           AND xeb.form-no > 0 NO-LOCK:
               PUT xeb.stock-no AT 3 space(14) xeb.part-dscr1 space(5) xeb.quantityPerSet FORMAT ">>>>9.9<<<" SKIP.
               /* rstark 05181205 */
               RUN XMLOutput (lXMLOutput,'JobTicketSetComponent','','Row').
               RUN XMLOutput (lXMLOutput,'FG_Number',xeb.stock-no,'Col').
               RUN XMLOutput (lXMLOutput,'FG_Description',xeb.part-dscr1,'Col').
               RUN XMLOutput (lXMLOutput,'Ratio_Per_Set',xeb.quantityPerSet,'Col').
               RUN XMLOutput (lXMLOutput,'/JobTicketSetComponent','','Row').
               /* rstark 05181205 */
               v-tmp-line = v-tmp-line + 1.
           END.
           v-tmp-line = v-tmp-line + 1.
           /* print raw materials from misc/fram of Est */ 
           FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                              AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
           DO i = 1 TO 8:
              IF b-ef.spec-no[i] <> "" THEN DO:
                 RUN custom/extradec.p (.0001, b-ef.spec-qty[i],
                                        OUTPUT lv-spec-qty[i]).
                 PUT b-ef.spec-dscr[i] AT 32 space(16) lv-spec-qty[i] SKIP.
                 /* rstark 05181205 */
                 RUN XMLOutput (lXMLOutput,'JobTicketRawMaterial','','Row').
                 RUN XMLOutput (lXMLOutput,'Raw_Description_' + STRING(i),b-ef.spec-dscr[i],'Col').
                 RUN XMLOutput (lXMLOutput,'Ratio_Per_Set_' + STRING(i),lv-spec-qty[i],'Col').
                 RUN XMLOutput (lXMLOutput,'/JobTicketRawMaterial','','Row').
                 /* rstark 05181205 */
                 v-tmp-line = v-tmp-line + 1.
              END.
           END.
           PUT "<=1><R+13><C2><FROM><R+" + string(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)"  SKIP.
           v-tmp-line = v-tmp-line + 13 .

           i = 0.
           for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0:
                i = i + 1.
           END.
           i = i + 2.
           PUT /*"<C2>Machine Routing:  <C15> SU:    Start    Stop     Total    Run:   Start   Stop    total   qty   in   out  waste  date" SKIP*/
               "  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
               "<=1><R+" + string(v-tmp-line + 1) + "><C2><FROM><R+" + string(i) + "><C78><RECT><||3>" FORM "x(150)" SKIP
               "<=1><R+" + string(v-tmp-line + 1) + ">" FORM "x(20)".
               .
           
           i = 0.
           for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0  by tt-wm.dseq:
             i = i + 1.
             display tt-wm.dscr AT 3
                  tt-wm.s-hr when tt-wm.s-hr ne 0
                  fill("_",7)  format "x(7)"    to 38   when tt-wm.dscr ne ""
                  fill("_",7)  format "x(7)"    to 46   when tt-wm.dscr ne ""
                  fill("_",7)  format "x(7)"    to 54   when tt-wm.dscr ne ""
                  space(2)
                  tt-wm.r-sp when tt-wm.r-sp ne 0
                  fill("_",7)  format "x(7)"    to 69   when tt-wm.dscr ne ""
                  fill("_",7)  format "x(7)"    to 77   when tt-wm.dscr ne ""
                  fill("_",7)  format "x(7)"    to 85   when tt-wm.dscr ne ""
                  fill("_",8)  format "x(8)"    to 99   when tt-wm.dscr ne ""
                  fill("_",8)  format "x(8)"    to 108  when tt-wm.dscr ne ""
                  fill("_",8)  format "x(8)"    to 117  when tt-wm.dscr ne ""
                  fill("_",8)  format "x(8)"    to 129  when tt-wm.dscr ne ""
                  /*chr(124) format "x"           at 131   */                  
                  with no-box no-labels frame o21 width 132 no-attr-space down STREAM-IO.
             IF tt-wm.dscr NE '' THEN DO:
               /* rstark 05181205 */
               RUN XMLOutput (lXMLOutput,'JobTicketSetRouting','','Row').
               RUN XMLOutput (lXMLOutput,'Set_Routing',tt-wm.dscr,'Col').
               RUN XMLOutput (lXMLOutput,'Set_SU',tt-wm.s-hr,'Col').
               RUN XMLOutput (lXMLOutput,'Set_Run',tt-wm.r-sp,'Col').
               RUN XMLOutput (lXMLOutput,'JobTicketSetRouting','','Row').
               /* rstark 05181205 */
             END.
           end.
           RUN XMLOutput (lXMLOutput,'/JobTicketSetRouting','','Row'). /* rstark 05181205 */
           FOR EACH tt-wm:
               DELETE tt-wm.
           END.
           v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */.
           
           v-shipto = IF AVAIL xoe-rel THEN xoe-rel.ship-id 
                      ELSE IF avail xeb THEN xeb.ship-id
                      ELSE IF avail xoe-ord THEN xoe-ord.sold-id 
                      ELSE "".
           FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
                                 AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-prem THEN CREATE tt-prem.

           ASSIGN v-tmp-lines = 0
                  j = 0
                  K = 0
                  lv-got-return = 0
                  v-dept-inst = "".
        
           {custom/notespr2.i job v-inst2 6 "notes.rec_key = job.rec_key and
                            notes.note_form_no = 0" }
           DO i = 1 TO 6:
              v-dept-inst[i] = v-inst2[i].
           END.
           IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */
           PUT "<=1><R+" + string(v-tmp-line) + ">" form "X(20)".
           v-tmp-line = v-tmp-line + 1.
           PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete" AT 3 SKIP
                             "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________" skip
                             "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial" skip
                             "Pattern: " AT 3 tt-prem.tt-pattern "<C20>_____________________ <C40>____________________  <C60>________________" skip
                             "Pallet: " AT 3 tt-prem.tt-pallet "<C20>_____________________ <C40>____________________ " skip
                             "<=1><R+" + string(v-tmp-line) + "><C2><FROM><R+6><C78><RECT><||3>" FORM "x(150)" SKIP
                             "<=1><R+" + string(v-tmp-line + 7) + "><C2><FROM><R+7><C78><RECT><||3>" FORM "x(150)" SKIP

                             "<=1><R+" + string(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
                             v-dept-inst[1] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[1] SKIP
                             v-dept-inst[2] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[2] SKIP
                             v-dept-inst[3] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[3] SKIP
                             v-dept-inst[4] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[4] SKIP
                             v-dept-inst[5] AT 3 FORM "x(82)" chr(124) format "xx" "Item PO #:" v-po-no SKIP
                             v-dept-inst[6] 
                             .
           PAGE.

           /* rstark 05181205 */
           RUN XMLOutput (lXMLOutput,'JobTicketSetPack','','Row').
           IF AVAIL xeb THEN DO:
             RUN XMLOutput (lXMLOutput,'No_Per_Bundle',tt-prem.tt-#-bundle,'Col').
             RUN XMLOutput (lXMLOutput,'No_Per_Unit',tt-prem.tt-#-unit,'Col').
             RUN XMLOutput (lXMLOutput,'Pattern',tt-prem.tt-pattern,'Col').
             RUN XMLOutput (lXMLOutput,'Pallet',tt-prem.tt-pallet,'Col').
           END.
           RUN XMLOutput (lXMLOutput,'/JobTicketSetPack','','Row').

           RUN XMLOutput (lXMLOutput,'JobTicketSetNotes','','Row').
           DO i = 1 TO 6:
             RUN XMLOutput (lXMLOutput,'SetNote_' + STRING(i),v-dept-inst[i],'Col').
           END.
           RUN XMLOutput (lXMLOutput,'/JobTicketSetNotes','','Row').

           RUN XMLOutput (lXMLOutput,'JobTicketSetShipTo','','Row').
           RUN XMLOutput (lXMLOutput,'SetShip_To',v-shipto,'Col').
           DO i = 1 TO 4:
             RUN XMLOutput (lXMLOutput,'SetShip_' + STRING(i),v-shp[1],'Col').
           END.
           RUN XMLOutput (lXMLOutput,'Item_PO',v-po-no,'Col').
           RUN XMLOutput (lXMLOutput,'/JobTicketSetShipTo','','Row').
           /* rstark 05181205 */

        END. /* set header printing */
      END. /* est.est-type = 6 */
     
   end.  /* each job */
end.  /* end v-local-loop  */

hide all no-pause.

{XMLOutput/XMLOutput.i &XMLClose} /* rstark 05181205 */

/* end ---------------------------------- copr. 1997  advanced software, inc. */

