/* ----------------------------------------------  */
/*  cecrep/jobsoule.p  Corrugated factory ticket  for Xprint landscape */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.

DEF SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(30)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(30)" NO-UNDO.
DEF var v-dept-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF VAR v-deptnote AS cha NO-UNDO.
DEF VAR v-dept-length AS DEC NO-UNDO.
DEF VAR lv-under-run AS cha NO-UNDO.
DEF VAR lv-over-run AS cha NO-UNDO.
DEF VAR lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg-name AS cha NO-UNDO.
DEF VAR tb_app-unprinted AS LOG NO-UNDO.

DEF VAR v-rel-qty AS INT NO-UNDO.
DEF VAR v-rel-date AS DATE NO-UNDO.
DEF VAR v-sman AS cha FORM "x(25)" NO-UNDO.
DEF VAR v-blk-per-frm AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-due-code AS cha FORM "x(4)" NO-UNDO.
DEF VAR v-cad-no AS cha FORM "X(12)" NO-UNDO.
DEF VAR v-tmp-line AS INT NO-UNDO.
DEF VAR v-max-qty AS DECI NO-UNDO.
DEF SHARED VAR s-sample-required AS LOG NO-UNDO.
DEF VAR v-sampreq LIKE reftable.val[2] NO-UNDO.

{jcrep/r-ticket.i "shared"}
{cecrep/jc-soule.i}
{cecrep/jobtickL.i "new shared"}   /* includes: {cecrep/jobxpr1.i "new shared"} */
{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}
{custom/formtext.i NEW}


def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
 
def var v-vend-no       like oe-ordl.vend-no                            no-undo.
def var v-po-no         like oe-ordl.po-no-po                           no-undo.
def var v-qty-or-sup    as   char               format "x(38)"          no-undo.
def var v-i-line        as   char   extent 5    format "x(38)"          no-undo.
def var v-flag          as   log    init no                             no-undo.
def var v-local-copies  as   int                                        no-undo.
def var v-local-loop    as   int    init 1                              no-undo.
def var v-print-score   as   log    init yes                            no-undo.
def var v-pqty          as   dec                                        no-undo.
DEF VAR lv-part-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-loop-cnt AS INT NO-UNDO.
DEF VAR v-note-cnt AS INT NO-UNDO.
DEF VAR v-note-length AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-plate-loc AS CHAR FORM "X(8)" NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.

{custom/notesdef.i}
{cecrep/jc-prem.i}

DEF BUFFER b-ef FOR ef.
DEF workfile tt-wm LIKE w-m.
DEF VAR v-xg-flag AS LOG NO-UNDO.
DEF VAR v-tmp-stype AS cha NO-UNDO.
DEF VAR v-len-score2 AS cha EXTENT 13 NO-UNDO.
DEF VAR v-tmp-score AS cha NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 15 NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF BUFFER b-eb FOR eb.
DEF VAR v-job-cust AS LOG NO-UNDO.
DEF VAR xJobQty LIKE job-hdr.qty NO-UNDO.
DEFINE BUFFER bf-itemfg FOR itemfg.

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.

   FIND FIRST sys-ctrl
       WHERE sys-ctrl.company EQ cocode
       AND sys-ctrl.name    EQ "JOBQTYCUST"
       NO-LOCK NO-ERROR.

   IF NOT AVAIL sys-ctrl THEN
       DO TRANSACTION:
       create sys-ctrl.
       assign
           sys-ctrl.company  = cocode
           sys-ctrl.NAME     = "JOBQTYCUST"
           sys-ctrl.module   = "JC"
           sys-ctrl.descrip = "Create Job Quantity with overrun % from customer if no order?"
           sys-ctrl.log-fld = NO .
       end.

  v-job-cust = sys-ctrl.log-fld.

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
 v-qty-or-sup = if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") gt 0
                then ("Supplier:"     + fill("_",28))
                else ("Qty Received: " + fill("_",24))
 v-local-copies = 1.

do v-local-loop = 1 to v-local-copies:
{cecrep/jobprem.i}
      break by job.job-no BY job.job-no2 BY job-hdr.frm:
/* same as cecrep/jobprem.i 
    for each job-hdr
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

      run cecrep/jobtick8.p (recid(job-hdr), v-format,
                              v-local-loop, v-local-copies).

      for each w-ef WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
          EACH b-eb NO-LOCK WHERE b-eb.company = job-hdr.company
                              AND b-eb.est-no = job-hdr.est-no 
                              AND b-eb.form-no = w-ef.frm
                              AND (b-eb.blank-no = job-hdr.blank-no OR est.est-type NE 8)
          BREAK BY w-ef.frm BY b-eb.blank-no:
        release xef.
        release xeb.
        release xstyle.
        release xxprep.

          
        run cecrep/jobtick2csc.p (recid(w-ef), recid(job-hdr), ROWID(b-eb)).
        
        /*Process results from d-Soule.w prompt*/
        FIND FIRST ttSoule 
                        WHERE ttSoule.job-no = job-hdr.job-no
                        AND ttSoule.job-no2  = job-hdr.job-no2
                        AND ttSoule.frm      = w-ef.frm
                        AND ttSoule.i-no     = xeb.stock-no
                        NO-LOCK NO-ERROR.
        IF AVAIL ttSoule THEN DO:
                IF NOT ttSoule.RunForm THEN NEXT.
                xJobQty = ttSoule.qty.
            END.
        ASSIGN
        v-pqty = 1
        v-cp = "".
        if avail xeb then do:
          if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          ASSIGN
             v-cp = xeb.part-no
             lv-fg-name = itemfg.i-name.

          {cec/rollfac.i}
          v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                   if xeb.quantityPerSet lt 0 then (-1 / xeb.quantityPerSet)
                                       else xeb.quantityPerSet.
        end.
        
        assign
         v-loc     = ""
         v-loc-bin = "".
         
        if v-format eq "Brick" or v-format eq "Corrugat" then do: 
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

        ASSIGN lv-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                             IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE ""
               lv-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                              IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE ""
               lv-part-name = xeb.part-dscr1    .
           IF v-job-cust AND NOT AVAIL xoe-ord AND AVAIL cust THEN
               ASSIGN
                    lv-over-run  = trim(string(cust.over-pct,">>9.99%"))
                    lv-under-run = trim(string(cust.under-pct,">>9.99%")) .

       PUT   /* "</PROGRESS>"*/
       "<P10><R1><C1>F-850-001-A    <P12><B>JOB TICKET" AT 9
       "<C40>Production Specification </B><P10>Part item name:" lv-part-name SKIP
       "<#1><C1><FROM><C105><R+45><RECT><|3>" 
       "<=1><C32><FROM><R+19><C32><LINE><|3>"
       "<=1><C66><FROM><R+19><C66><LINE><|3>"
       "<=1><C90><FROM><R+4><C90><LINE><|3>"
       "<=1><R+4><C1><FROM><C105><LINE><|3>"
       "<=#1><R+10><C1><FROM><C105><LINE><|3>"
       "<=1><R+19><C1><FROM><C105><LINE><|3>"       
       .

       view frame head.  /* factory header display  */  

       if v-format eq "RFC" or v-format eq "Boxtech" then
          assign
           v-i-line[1] = itemfg.i-name
           v-i-line[2] = itemfg.part-dscr1
           v-i-line[3] = itemfg.part-dscr2
           v-i-line[4] = itemfg.part-dscr3.
        else
          assign
           v-i-line[1] = "ITEM DESCRIPTION"
           v-i-line[2] = "Style: " + if avail xstyle then xstyle.dscr else ""
           v-i-line[3] = "Size: "  + if avail xeb    then
                     trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                     trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99")) else ""
           v-i-line[4] = "Joint: " + if avail xeb then v-joint-dscr else "".
        lv-part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no 
                     ELSE itemfg.part-no.
        
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

        if v-vend-no ne "" then do:
           v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
           IF v-po-no ne 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                                               trim(string(v-po-no,">>>>>>>>>>")).
        end.
        
        i = 0.
        for each w-i:
          i = i + 1.
        end.
        if i lt 4 then do i = i + 1 to 4:
          create w-i.
        end.

        find first w-i.
        v-ink-1 =  w-i.i-dscr +
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find next w-i.
        v-ink-2 =  w-i.i-dscr +
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find next w-i.
        v-ink-3 =  w-i.i-dscr +
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find NEXT w-i.
        v-ink-4 =  w-i.i-dscr +
                   (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".

        v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
                                       else (v-form-len * v-form-wid / 144),3).
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.die-no
                            no-lock no-error.
        v-die-loc = IF AVAIL xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.plate-no
                            no-lock no-error.

        v-plate-loc = IF AVAIL xxprep THEN xxprep.loc-bin ELSE "".

        DISP  v-cus[1] AT 2 "PO #:" AT 40 xoe-ord.po-no WHEN AVAIL xoe-ord 
              "Set Qty:" trim(string(if avail xoe-ordl then xoe-ordl.qty
                                          else xJobQty,">>>,>>9"))
                        when avail xeb and xeb.est-type eq 9    format "x(9)"
              "Style:" AT 80 xstyle.dscr WHEN AVAIL xstyle              
              SKIP
              v-cus[2] AT 2  
              "Job Qty:" AT 40 trim(string(xJobQty * v-pqty,">>>,>>9"))    format "x(7)"
              "Order Qty:" trim(string((if avail xoe-ordl then xoe-ordl.qty
                                             else xJobQty) *
                                            if est.form-qty le 1 then 1
                                            else v-pqty,">>>,>>9"))
                                            format "x(7)"
              "   "
              "Size:" (trim(string({sys/inc/k16v.i xeb.len},">,>>9.99")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.wid},">,>>9.99")) + " x " +
                      trim(string({sys/inc/k16v.i xeb.dep},">,>>9.99"))) FORM "x(30)" WHEN AVAIL xeb            
              SKIP
              v-cus[3] AT 2 "Cust Part#:" AT 40 lv-part-no
              "Joint:" AT 80 v-joint-dscr             
              SKIP
              v-cus[4] AT 2
             
              "Item Name: " AT 40 lv-fg-name FORM "x(26)"
              "Adders:" AT 80 v-adders FORM "x(23)"
              "<P7>"SKIP(1)
              "Board:" AT 2 "Printing:" AT 55 "Die Cutting, Slit, & Saw" AT 115 "<P10>"SKIP
              "Shts Req'd:" AT 2 trim(string(v-sht-qty))   format "x(9)"
                " Sq Ft:" trim(string(v-form-sqft)) format "x(7)"
              "PLATE #:" AT 39 xeb.plate-no FORM "X(15)" when avail xeb
              v-plate-loc WHEN AVAIL xeb
              "Die #" AT 80 xeb.die-no when avail xeb " Loc:" v-die-loc SKIP
              "W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))  format "x(22)" AT 2
              "MSF:"  + trim(string(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
                                                                format "x(11)"
              "Ink 1:" AT 39 v-ink-1
              "Gross Size:" AT 80  
              "W:" + trim(string({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.gsh-len},">>>9.99"))
                                                                format "x(20)"
              "Slit: W:" + string(v-outw) + " L:" + string(v-outl) FORM "x(15)"   
              SKIP
              "Board:" AT 2 v-form-code FORM "x(30)" "Ink 2:" AT 39 v-ink-2 "Net   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) format "x(22)"
              SKIP
              v-form-dscr AT 4 FORM "x(30)" "Ink 3:" AT 39 v-ink-3 "Die   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(22)"
              "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"
              SKIP
               v-qty-or-sup AT 2 FORM "x(36)" 
              "Ink 4:" AT 39 v-ink-4 "Blank Size:" AT 80 
              "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"
              SKIP
              "Score:" AT 2 substring(v-len-score,1,30) WHEN xstyle.TYPE <> "F" format "x(30)" 
              "Color Desc:" AT 39 xeb.i-coldscr when avail xeb
              "Impressions: " AT 80 trim(string(v-dc-qty))    format "x(7)"
              SKIP
              SUBSTRING(v-len-score,31,40) AT 6 FORM "x(40)"
              "D/C Style:" AT 80 SKIP(1)
              WITH FRAME job1 NO-LABEL NO-BOX WIDTH 150 STREAM-IO.
        
        i = 0.
        for each w-m:
          i = i + 1.
        end.
        if i lt 3 then do i = i + 1 to 3:
          create w-m.
          w-m.dseq = 999999999.
        end.
            /* box for route */
        
        put skip
            "<P7>   Machine Routing                    SU:      Start       Stop        Total          RUN:     Hours      Start       Stop       Total       QTY:  In        Out         Waste <P10>" SKIP.
        i = 0.

        for each w-m by w-m.dseq:

          ASSIGN
             i = i + 1
             v-letter = substr("UTE",i,1).
         
          IF s-prt-mstandard THEN
          DO:
             display w-m.dscr AT 3
                     w-m.s-hr                              when w-m.s-hr ne 0
                     fill("_",7)  format "x(7)"    to 38   when w-m.dscr ne ""
                     fill("_",7)  format "x(7)"    to 46   when w-m.dscr ne ""
                     fill("_",7)  format "x(7)"    to 54   when w-m.dscr ne ""
                     space(4)
                     w-m.r-sp                              when w-m.r-sp ne 0
                     w-m.r-hr                              when w-m.r-hr ne 0
                     fill("_",7)  format "x(7)"    to 80   when w-m.dscr ne ""
                     fill("_",7)  format "x(7)"    to 88   when w-m.dscr ne ""
                     fill("_",7)  format "x(7)"    to 96    when w-m.dscr ne ""
                     fill("_",8)  format "x(8)"    to 105  when w-m.dscr ne ""
                     fill("_",8)  format "x(8)"    to 114  when w-m.dscr ne ""
                     fill("_",8)  format "x(8)"    to 123  when w-m.dscr ne ""
                 with no-box no-labels frame oo1 width 150 no-attr-space down STREAM-IO.
          END.
          ELSE
              display w-m.dscr AT 3
                  fill("_",7)  format "x(7)"    to 38   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 46   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 54   when w-m.dscr ne ""
                  space(4)
                  fill("_",7)  format "x(7)"    to 80   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 88   when w-m.dscr ne ""
                  fill("_",7)  format "x(7)"    to 96    when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 105  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 114  when w-m.dscr ne ""
                  fill("_",8)  format "x(8)"    to 123  when w-m.dscr ne ""
              with no-box no-labels frame oo12 width 150 no-attr-space down STREAM-IO.
                  
          v-lines = v-lines + 1.
        end.
      
        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                           AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
        IF AVAIL b-ef AND b-ef.form-no = w-ef.frm THEN 
           FOR EACH w-m:
               CREATE tt-wm.
               BUFFER-COPY w-m TO tt-wm.
        END.

        PUT "<#6><C1><FROM><C105><LINE><|3>" .
            
        RUN cecrep/jobsoul2.p (RECID(job-hdr),v-format,cust.terms).

        PUT "<=6><C28><FROM><R+8><C28><LINE><|3>"
            "<=6><C44><FROM><R+8><C44><LINE><|3>"
            "<=6><C64><FROM><R+8><C64><LINE><|3>"
            "<=6><C79><FROM><R+8><C79><LINE><|3>"            
            "<=6><C93><FROM><R+8><C93><LINE><|3>".
        
        RUN stackImage.

        PUT "<#7><C1><FROM><C105><LINE><|3>"
            "<=7><C28><FROM><R+14><C28><LINE><|3>"
            "<=7><C75><FROM><R+14><C75><LINE><|3>"
            "<P7>" SKIP.

        DISPLAY "<=7><C30>Floor Comments" AT 2   "Shipping Info: <P10>" AT 92 SKIP
                "Ship To #:" AT 90
                xoe-ord.sold-id when avail xoe-ord
                xeb.ship-id when avail xeb @ xoe-ord.sold-id
                xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id SKIP
                "____________________________________________________" AT 35 v-shp[1] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[2] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[3] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[4] AT 90 SKIP
                "____________________________________________________" AT 35 "Item PO #:" AT 90 xoe-ordl.po-no when avail xoe-ordl
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                /*"____________________________________________________________________________" SKIP */
               with no-box no-labels frame m8 width 170 no-attr-space STREAM-IO.

        PAGE.
        /* dept notes */
        ASSIGN
           v-note-length = 124
           v-tmp-lines = 0
           j = 0
           K = 0
           lv-got-return = 0
           v-dept-note = "" 
           v-prev-note-rec = ?.

        FOR EACH notes WHERE notes.rec_key = job.rec_key and
                    (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
                 NO-LOCK:
            IF v-prev-note-rec <> ? AND
               v-prev-note-rec <> RECID(notes) THEN v-prev-extent = v-prev-extent + k.

            DO i = 1 TO LENGTH(notes.note_text):
               IF i - j >= v-note-length THEN ASSIGN j = i
                                            lv-got-return = lv-got-return + 1.

               v-tmp-lines = ( i - j ) / v-note-length.
               {SYS/INC/ROUNDUP.I v-tmp-lines}

               k = v-tmp-lines + lv-got-return + 
                   IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.
               IF k < 7 THEN v-dept-note[k] = v-dept-note[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
                             ELSE "" .              

               IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
               THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
               END.         
            END.
            
            ASSIGN v-prev-note-rec = RECID(notes)
                   j = 0
                   lv-got-return = 0.


        END.
        
        ASSIGN
        v-inst = ""
        v-spec-note = ""
        v-tmp-lines = 0
        j = 0
        K = 0
        lv-got-return = 0.

        IF v-fg NE "" THEN
        FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company EQ cocode
            AND bf-itemfg.i-no EQ v-fg NO-ERROR.
        FOR EACH notes WHERE notes.rec_key = bf-itemfg.rec_key
            AND lookup(notes.note_code,spec-list) NE 0 NO-LOCK.
         
            DO i = 1 TO LENGTH(notes.note_text) :        
               IF i - j >= v-note-length THEN ASSIGN j = i
                                              lv-got-return = lv-got-return + 1.
                   
               v-tmp-lines = ( i - j ) / v-note-length.
               {SYS/INC/ROUNDUP.I v-tmp-lines}

               k = v-tmp-lines + lv-got-return.
               IF k < 9 THEN v-spec-note[k] = v-spec-note[k] + IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1) 
                                  ELSE "" .              
           
               IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
               THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
               END.         
            END.
         END.

        PUT "<#11><C1><FROM><C105><R+47><RECT><|3>"  
            "<=11> Department Notes" SKIP
            v-dept-note[1]  AT 2 SKIP
            v-dept-note[2] AT 2 SKIP
            v-dept-note[3] AT 2 SKIP
            v-dept-note[4] AT 2 SKIP
            v-dept-note[5] AT 2 SKIP
            v-dept-note[6] AT 2 SKIP
            "<C1><FROM><C105><LINE><|3>" 
            "<C1> Spec Notes" SKIP
            v-spec-note[1] FORM "x(122)" AT 2 SKIP
            v-spec-note[2] FORM "x(122)" AT 2 SKIP
            v-spec-note[3] FORM "x(122)" AT 2 SKIP
            v-spec-note[4] FORM "x(122)" AT 2 SKIP
            v-spec-note[5] FORM "x(122)" AT 2 SKIP
            v-spec-note[6] FORM "x(122)" AT 2 SKIP
            "<C1><R48>F-850-001-A".
        if print-box and avail xest then do:
            
           run cec/desprntLa.p (recid(xef),
                                input-output v-lines,
                                recid(xest),
                                IF AVAIL xeb THEN ROWID(xeb) ELSE ?).
           PAGE.
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
            ASSIGN
            v-fg-set = job-hdr.i-no
            v-set-qty = if avail xeb and xeb.est-type eq 6 THEN
                          if avail xoe-ordl then xoe-ordl.qty else xJobQty
                        ELSE 0
            v-ord-qty = (if avail xoe-ordl then xoe-ordl.qty else xJobQty) *
                        if est.form-qty le 1 then 1 else v-pqty
            v-over-run = IF avail xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%"))
                         ELSE IF avail xoe-ord THEN trim(string(xoe-ord.over-pct,">>9.99%")) ELSE ""
            v-under-run = IF avail xoe-ordl THEN trim(STRING(xoe-ordl.under-pct,">>9.99%"))
                          ELSE IF avail xoe-ord THEN trim(STRING(xoe-ord.under-pct,">>9.99%")) ELSE "" 
            v-max-qty = /*v-ord-qty */ v-set-qty  * (1 + IF AVAIL xoe-ordl THEN xoe-ordl.over-pct / 100 
                                          ELSE IF AVAIL xoe-ord  THEN xoe-ord.over-pct / 100 
                                          ELSE 0 ).


            PUT "<R1><C1><#15><C45><P16><B> SET HEADER<P10>" SKIP
                "<P14><B>" v-cus[1] AT 3 "<P10></B>"
                "<P13><B>" lv-fg-name FORMAT "x(30)" AT 80 "<P10></B>"
                "<P11><B>Job #: " AT 3 v-job-prt "<C35>Our Order #: " v-ord-no
                "<P10></B><C75>Our Date: " v-ord-date SKIP
                "Est #: " AT 3 v-est-no "<C35>FG #: " v-fg-set "<C75>Due Date: " v-due-date SKIP
                "<=1><R+4><C1><From><R+5><C105><RECT><||3>" SKIP
                "<=1><R+4><C2>CUSTOMER INFORMATION <C35> ORDER INFORMATION <C72>ITEM DESCRIPTION" SKIP
                v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
                " Max Qty: " STRING(v-max-qty,">>>,>>9")
                v-i-line[2] AT 86
                SKIP
                v-cus[2] AT 3
                /*" Job Qty:" trim(string(xJobQty * v-pqty,">>>,>>9"))    format "x(7)"
                " Order Qty:" string(v-ord-qty) format "x(7)"*/
                v-i-line[3] AT 86 SKIP
                v-cus[3] AT 3  "<B> Cust Part #:" lv-part-no "</B>"
                v-i-line[4] AT 93 SKIP
                v-cus[4]  AT 3 " Overrun:"  format "x(7)"
                " Underrun:" format "x(7)"
                "Adders:" v-adders FORM "x(36)" v-i-line[5] AT 86 SKIP
                "<=1><R+9><C30><P10><B>Set Components<P10></B> <C50>Set item: " v-fg-set SKIP
                "<P10><C2>FINISHED GOOD #  DESCRIPTION                     SAMPLES REQ'D  RATIO PER SET     DIE #      CAD#        STYLE" SKIP.
            /* each components */                                                    

            DEF VAR v-shipto AS cha NO-UNDO.

            v-tmp-line = 0.
            FOR EACH xeb NO-LOCK 
                WHERE xeb.company = est.company
                  AND xeb.est-no = est.est-no
                  AND xeb.form-no > 0 
                BY xeb.stock-no:

                /* gdm - 11030802 */
                IF s-sample-required = TRUE THEN DO:

                  ASSIGN v-sampreq = 0.

                  FIND FIRST b-ef NO-LOCK 
                      WHERE b-ef.company EQ xeb.company
                        AND b-ef.est-no  EQ xeb.est-no 
                        AND b-ef.eqty    EQ xeb.eqty   
                        AND b-ef.form-no EQ xeb.form-no NO-ERROR.
                  IF AVAIL b-ef THEN
                      FIND FIRST reftable NO-LOCK
                         WHERE reftable.reftable = "cecrep/d-hughes.w"
                           AND reftable.company  = xeb.company
                           AND reftable.loc      = TRIM(job-hdr.job-no)
                           AND reftable.code     = STRING(job-hdr.job-no2,"9999999999")
                           AND reftable.code2    = string(b-ef.form-no,"999")  /*form-no*/
                           AND reftable.val[1]   = xeb.blank-no NO-ERROR. /*blank-no*/
                      IF AVAILABLE reftable 
                        THEN
                         ASSIGN v-sampreq = reftable.val[2].
                        ELSE 
                         ASSIGN v-sampreq = 0.
                END.

                PUT xeb.stock-no   AT 3  
                    xeb.part-dscr1 AT 20 
                    v-sampreq      AT 52.

                IF xeb.quantityPerSet LT 0 THEN
                  PUT -1 / xeb.quantityPerSet FORMAT ">>>>>>9.9<<<<<<" AT 67.
                ELSE
                  PUT xeb.quantityPerSet FORMAT ">>>>>>9.9<<<<<<" AT 67.

                FIND FIRST xstyle NO-LOCK
                   WHERE xstyle.company  EQ xeb.company
                     AND xstyle.style    EQ xeb.style
                     AND xstyle.industry EQ "2" NO-ERROR.

                PUT 
                   xeb.die-no FORMAT "x(10)" AT 85
                   xeb.cad-no FORMAT "x(10)" AT 96
                   if avail xstyle then xstyle.dscr else "" FORMAT "x(30)" AT 108. 

                PUT SKIP.
                v-tmp-line = v-tmp-line + 1.
                IF v-tmp-line + 65 > PAGE-SIZE THEN DO:
                  PUT "<=1><R+10><C1><FROM><R+" + string(v-tmp-line + 1) + "><C105><RECT><||3>" FORM "x(150)" SKIP.
                  PAGE. 
                  v-tmp-line = 0.

                  PUT "<R1><C45><P16><B> SET HEADER<P10>" SKIP
                "<P14><B>" v-cus[1] AT 2 "<P10></B>" 
                "<P13><B>" lv-fg-name FORMAT "x(30)" AT 80 "<P10></B>"
                "<P11><B>Job #: " AT 3 v-job-prt "<C35>Our Order #: " v-ord-no 
                "<P10></B><C75>Our Date: " v-ord-date SKIP
                "Est #: " AT 3 v-est-no "<C35>FG #: " v-fg-set "<C75>Due Date: " v-due-date SKIP
                "<=1><R+4><C1><From><R+5><C105><RECT><||3>" SKIP
                "<=1><R+4><C2>CUSTOMER INFORMATION <C35> ORDER INFORMATION <C72>ITEM DESCRIPTION" SKIP
                v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
                " Max Qty: " STRING(v-max-qty,">>>,>>>")
                v-i-line[2] AT 86
                SKIP
                v-cus[2] AT 3
                /*" Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9")) format "x(7)"
                " Order Qty:" string(v-ord-qty) format "x(7)" */
                v-i-line[3] AT 86 SKIP
                v-cus[3] AT 3  "<b> Cust Part #:" lv-part-no "</B">
                v-i-line[4] AT 93 SKIP
                v-cus[4]  AT 3 " Overrun:"  format "x(7)" 
                " Underrun:" format "x(7)"  
                "Adders:" v-adders FORM "x(36)" v-i-line[5] AT 86 SKIP
                "<=1><R+9><C30><P10><B>Set Components<P10></B> <C50>Set item: " v-fg-set SKIP
                "<P10><C2>FINISHED GOOD #  DESCRIPTION                     SAMPLES REQ'D  RATIO PER SET     DIE #      CAD#        STYLE" SKIP.             
                END.
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
                  v-tmp-line = v-tmp-line + 1.
               END.
            END.
            PUT "<=1><R+10><C1><FROM><R+" + string(v-tmp-line) + "><C105><RECT><||3>" FORM "x(150)" SKIP.
            v-tmp-line = v-tmp-line + 10.

            i = 0.
            for each tt-wm WHERE lookup(tt-wm.m-code,tspostfg-char) > 0:
                 i = i + 1.
            END.
            i = i + 2.
            PUT /*"<C2>Machine Routing:  <C15> SU:    Start    Stop     Total    Run:   Start   Stop    total   qty   in   out  waste  date" SKIP*/
                "<p9>  Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP
                "<=1><R+" + string(v-tmp-line + 1) + "><C1><FROM><R+" + string(i) + "><C105><RECT><||3>" FORM "x(150)" SKIP
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
            end.
            PUT "<p10>".
            FOR EACH tt-wm:
                DELETE tt-wm.
            END.
            ASSIGN
            v-tmp-line = v-tmp-line + 3 + i /* 4 and add machine routing lines */

            v-shipto = IF AVAIL xoe-rel THEN xoe-rel.ship-id 
                       ELSE IF avail xeb THEN xeb.ship-id
                       ELSE IF avail xoe-ord THEN xoe-ord.sold-id 
                       ELSE "".
            FIND FIRST tt-prem WHERE tt-prem.tt-job-no  EQ job-hdr.job-no
                                  AND tt-prem.tt-job-no2  EQ job-hdr.job-no2 NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-prem THEN do:
               FIND FIRST bf-eb WHERE bf-eb.company = est.company
                                  AND bf-eb.est-no = est.est-no
                                  AND bf-eb.form-no = 0
                                  AND bf-eb.blank-no = 0 NO-LOCK NO-ERROR.
               CREATE tt-prem.
               IF AVAIL bf-eb THEN 
                  ASSIGN tt-prem.tt-#-bundle = string(bf-eb.cas-cnt)
                         tt-prem.tt-#-unit = string(bf-eb.tr-cnt)
                         tt-prem.tt-pattern = bf-eb.tr-no
                         tt-prem.tt-pallet = bf-eb.cas-no.
            END.
        /*    
            ASSIGN v-tmp-lines = 0
                   j = 0
                   K = 0
                   lv-got-return = 0
                   v-dept-inst = "".
                                                                             /*notes.note_form_no = 0"*/
            {custom/notespr2.i job v-inst2 6 "notes.rec_key = job.rec_key and note.note_code = "SH"}
            DO i = 1 TO 6:
               v-dept-inst[i] = v-inst2[i].
            END. 
            ===*/

            FOR EACH tt-formtext:
               DELETE tt-formtext.
            END.    
            lv-text = "".
            FOR EACH notes NO-LOCK WHERE notes.rec_key = job.rec_key
                                     AND notes.note_code = "SH":
                lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
            END.
            FOR EACH notes NO-LOCK WHERE notes.rec_key = itemfg.rec_key
                                     AND lookup(notes.note_code,spec-list) NE 0:
                lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
            END.
            DO li = 1 TO 15:
               CREATE tt-formtext.
               ASSIGN
                  tt-line-no = li
                  tt-length  = 72.
            END.
            RUN custom/formtext.p (lv-text).
            i = 0.
            v-dept-inst = "".
            FOR EACH tt-formtext:
               i = i + 1.
               IF  i <= 15 THEN v-dept-inst[i] = tt-formtext.tt-text.      
            END.
            IF v-ship <> "" THEN v-dept-inst[15] = v-ship.  /* shipto notes */
            PUT "<=1><R+" + string(v-tmp-line) + ">" form "X(20)".
            v-tmp-line = v-tmp-line + 1.
            PUT "Unitizing Bale <C24>Date <C44>Units <C62>Complete <C79>OP <C93>QA" AT 3 SKIP
                "# Per Bndl: " AT 3 tt-prem.tt-#-bundle "<C20>_____________________ <C40>____________________  <C60>________________  <C75>________________  <C90>________________" skip
                "# Per Unit: " AT 3 tt-prem.tt-#-unit "<C20>_____________________ <C40>____________________  <C62>Partial  <C75>________________  <C90>________________" skip
                "Pattern: " AT 3 tt-prem.tt-pattern FORM "x(10)" "<C20>_____________________ <C40>____________________  <C60>________________  <C75>________________  <C90>________________" skip
                "Pallet: " AT 3 tt-prem.tt-pallet FORM "x(10)"  "<C20>_____________________ <C40>____________________  <C75>________________  <C90>________________" skip
                "<=1><R+" + string(v-tmp-line) + "><C1><FROM><R+6><C105><RECT><||3>" FORM "x(150)" SKIP
                "<=1><R+" + string(v-tmp-line + 7) + "><C1><FROM><R+15><C105><RECT><||3>" FORM "x(150)" SKIP

                "<=1><R+" + string(v-tmp-line + 7) + "><C2>Special instructions  <C51>SHIPPING INFO       Ship to: " + v-shipto FORM "x(250)" SKIP
                v-dept-inst[1] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[1] SKIP
                v-dept-inst[2] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[2] SKIP
                v-dept-inst[3] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[3] SKIP
                v-dept-inst[4] AT 3 FORM "x(82)" chr(124) format "xx" v-shp[4] SKIP
                v-dept-inst[5] AT 3 FORM "x(82)" chr(124) format "xx" "Item PO #:" v-po-no SKIP
                v-dept-inst[6] AT 3 SKIP
                v-dept-inst[7] AT 3 SKIP
                v-dept-inst[8] AT 3 SKIP
                v-dept-inst[9] AT 3 SKIP
                v-dept-inst[10] AT 3 SKIP
                v-dept-inst[11] AT 3 SKIP
                v-dept-inst[12] AT 3 SKIP
                v-dept-inst[13] AT 3 SKIP
                v-dept-inst[14] AT 3 SKIP
                v-dept-inst[15] AT 3 
                .
            PAGE.
         END. /* i > 1*/
      END. /* set header printing  est.est-type = 6 */


    end.  /* each job */
    end.  /* end v-local-loop  */
 
        hide all no-pause.


PROCEDURE stackImage:
  DEFINE BUFFER pattern FOR reftable.
  DEFINE BUFFER stackPattern FOR stackPattern.
  IF v-stackcode EQ '' THEN RETURN.
  FIND FIRST stackPattern NO-LOCK
       WHERE stackPattern.stackCode EQ SUBSTR(v-stackcode,9,1) NO-ERROR.
  IF AVAILABLE stackPattern AND SEARCH(stackPattern.stackImage) NE ? THEN
  PUT UNFORMATTED
    "<#71><C27><R+1><FROM><C2><R+12>"
    "<IMAGE#71=" stackPattern.stackImage ">"
    "<R-13>".
END PROCEDURE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
