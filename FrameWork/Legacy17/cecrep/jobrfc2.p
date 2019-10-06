/* ----------------------------------------------  */
/*  cecrep/jobrfc2.p  Corrugated factory ticket  for RFC2 */
/* -------------------------------------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF INPUT PARAMETER tb_app-unprinted AS LOG NO-UNDO.

DEF SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF VAR prt-copies AS INT  NO-UNDO.
DEF VAR v-start-compress AS cha NO-UNDO.
DEF VAR v-end-compress AS cha NO-UNDO.
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
DEF VAR v-ink-1 AS cha FORM "X(20)" NO-UNDO.
DEF VAR v-ink-2 AS cha FORM "X(20)" NO-UNDO.
DEF VAR v-ink-3 AS cha FORM "X(20)" NO-UNDO.
DEF VAR v-ink-4 AS cha FORM "X(20)" NO-UNDO.
DEF VAR v-ink-5 AS cha FORM "X(20)" NO-UNDO.
DEF VAR v-ink-6 AS cha FORM "X(20)" NO-UNDO.
DEF VAR v-ink-7 AS cha FORM "X(20)" NO-UNDO.
DEF VAR v-ink-8 AS cha FORM "X(20)" NO-UNDO.
DEF VAR v-lb-1 AS cha FORM "X(10)" NO-UNDO.
DEF VAR v-lb-2 AS cha FORM "X(10)" NO-UNDO.
DEF VAR v-lb-3 AS cha FORM "X(10)" NO-UNDO.
DEF VAR v-lb-4 AS cha FORM "X(10)" NO-UNDO.
DEF VAR v-lb-5 AS cha FORM "X(10)" NO-UNDO.
DEF VAR v-lb-6 AS cha FORM "X(10)" NO-UNDO.
DEF VAR v-lb-7 AS cha FORM "X(10)" NO-UNDO.
DEF VAR v-lb-8 AS cha FORM "X(10)" NO-UNDO.
DEF var v-dept-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF var v-spec-note AS cha FORM "x(124)" EXTENT 10 NO-UNDO.
DEF VAR v-deptnote AS cha NO-UNDO.
DEF VAR v-dept-length AS DEC NO-UNDO.
DEF VAR lv-under-run AS cha NO-UNDO.
DEF VAR lv-over-run AS cha NO-UNDO.
DEF VAR lv-part-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-fg-name AS cha NO-UNDO.
DEF VAR lv-status AS cha NO-UNDO.
DEF VAR lv-sts-code AS cha INIT "O,R,C,T,N,X,Q" NO-UNDO.
DEF VAR lv-sts-desc AS cha INIT "O-Original,R-Repeat,C-Change,T-Transfer,N-New Customers,X-Complete Re-run,Q-Quality/Re-work" NO-UNDO.

DEF VAR v-sman AS cha FORM "x(25)" NO-UNDO.
DEF VAR v-p-wax AS CHAR NO-UNDO.

{jcrep/r-ticket.i "shared"}

{cecrep/jobrfc2.i "new shared"}

{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

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
DEF VAR lv-add-entry AS INT NO-UNDO.
DEF VAR v-loop-cnt AS INT NO-UNDO.
DEF VAR v-note-cnt AS INT NO-UNDO.
DEF VAR v-note-length AS INT NO-UNDO.
DEF VAR v-die-loc AS cha FORM "x(15)" NO-UNDO.
{custom/notesdef.i}
{cecrep/jc-prem.i}
DEF BUFFER b-ef FOR ef.
DEF WORKFILE tt-wm LIKE w-m.
DEF VAR v-xg-flag AS LOG NO-UNDO.
DEF VAR v-tmp-stype AS cha NO-UNDO.
DEF VAR v-len-score2 AS cha EXTENT 13 NO-UNDO.
DEF VAR v-tmp-score AS cha NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-itemfg FOR itemfg .
DEF VAR lv-spec-qty LIKE ef.spec-qty FORMAT ">>>,>>9.9<<<<" NO-UNDO.
DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEF VAR v-inst2 AS cha EXTENT 6 NO-UNDO.    
DEF VAR lv-text AS cha NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.
DEF VAR cha-val AS cha NO-UNDO.

DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.
DEF VAR ls-fgitem-img AS cha FORM "x(50)" NO-UNDO.
DEF VAR v-po-due-date AS DATE FORM "99/99/9999" NO-UNDO.

{custom/formtext.i NEW}

FIND sys-ctrl WHERE company = "001" AND NAME = "notes" NO-LOCK NO-ERROR.
 IF AVAILABLE sys-ctrl THEN 
   ASSIGN cha-val = TRIM(sys-ctrl.char-fld).
 
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
                else ("Qty Received: " + fill("_",24)).


    assign /*v-local-copies = if lookup(printer.pr-port, "{&PR-PORT}") eq 0 then 
                              prt-copies
                            else 1  */
           v-local-copies = 1
           prt-copies = 1.

do v-local-loop = 1 to v-local-copies:
{cecrep/jobprem.i}
      break by job.job-no BY job.job-no2:
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

      run cecrep/jobtick1.p (recid(job-hdr), v-format,
                              v-local-loop, v-local-copies).

      for each w-ef WHERE (w-ef.frm = job-hdr.frm OR est.est-type <> 8),
       /* gdm - 09300905 */
          EACH bf-eb NO-LOCK WHERE bf-eb.company   EQ job-hdr.company
                               AND bf-eb.est-no    EQ job-hdr.est-no 
                               AND bf-eb.form-no   EQ w-ef.frm
                               AND (bf-eb.blank-no EQ job-hdr.blank-no OR 
                                                      est.est-type NE 8)
          BREAK BY w-ef.frm 
          BY bf-eb.blank-no:
      /* gdm - 09300905 end */

        release xef.
        release xeb.
        release xstyle.
        release xxprep.
        
        run cecrep/jobtick2.p (recid(w-ef), recid(job-hdr)).

        v-pqty = 1.
        v-cp = "".
        v-cat = "" .

        IF AVAIL itemfg THEN  ASSIGN v-cat = itemfg.procat.

        if avail xeb then do:
          if xeb.stock-no ne "" then v-fg = xeb.stock-no.
          v-cp = xeb.part-no.

          FIND FIRST bf-itemfg WHERE bf-itemfg.company = cocode 
                                    AND bf-itemfg.i-no = xeb.stock-no NO-LOCK NO-ERROR .
          IF AVAIL bf-itemfg THEN ASSIGN v-cat = bf-itemfg.procat.
          /*if xest.est-type eq 6 then v-fg = trim(v-fg) + "  CP#: " +
                                            xeb.part-no . */            
         /* FIND FIRST sman WHERE sman.company = xeb.company AND
                                sman.sman = xeb.sman NO-LOCK NO-ERROR.
          v-sman = IF AVAIL sman THEN sman.sname ELSE xeb.sman.
         */
          v-sman = xeb.sman.
          ASSIGN lv-fg-name = itemfg.i-name .
                 

          {cec/rollfac.i}
          v-pqty = if v-rollfac OR xeb.est-type EQ 8 then 1 else
                   if xeb.yld-qty lt 0 then (-1 / xeb.yld-qty)
                                       else xeb.yld-qty.
          v-form-hdr = "FORM: " + string(xeb.form-no) + " OF " + string(est.form-qty) .
                    /*+ "  BLANK " + STRING(b-eb.blank-no) + " OF " + STRING(xef.blank-qty)*/.
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

       PUT /*"<C77><B>" cust.NAME "</B>"*/ "<P10>"skip(1)
           "<P12><B>JOB TICKET" AT 9
       "Production Specification <P10>" AT 50 /* " Part item name:" lv-part-name*/
          /* "<C77>" v-fg "</B>"*/
            SKIP
       "<#1><C1><FROM><C105><R+46><RECT><||5>" 
       "<=1><C32><FROM><R+20><C32><LINE><||5>"
       "<=1><C66><FROM><R+20><C66><LINE><||5>"
       "<=1><C90><FROM><R+5><C90><LINE><||5>"
       "<=1><R+5><C1><FROM><C105><LINE><||5>"
       "<=1><R+10><C1><FROM><C105><LINE><||5>"
       "<=1><R+20><C1><FROM><C105><LINE><||5>"       
       "<=1><P14><R-3><C77><B>" cust.NAME "</B>"
       "<=1><R-2><C77><B>" v-fg "</B>"
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
                           
        /*if lookup(v-format,"TriState,RFC,Boxtech,Brick,Corrugat") gt 0 then do:*/
        RUN sys/ref/getpo#.p (IF AVAIL xoe-ordl AND est.est-type NE 6 THEN ROWID(xoe-ordl) ELSE ROWID(job),
                              w-ef.frm, OUTPUT v-po-no).

        RELEASE po-ord.
          
        IF v-po-no NE 0 THEN
        FIND FIRST po-ord
            WHERE po-ord.company EQ cocode
              AND po-ord.po-no   EQ v-po-no
            NO-LOCK NO-ERROR.
        FIND FIRST po-ordl WHERE po-ordl.company = cocode 
                             AND po-ordl.po-no = v-po-no
                             AND po-ordl.i-no = v-form-code NO-LOCK NO-ERROR.
        v-po-due-date =  IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.
        
        ASSIGN
         v-vend-no    = IF AVAIL po-ord THEN po-ord.vend-no ELSE ""
         v-qty-or-sup = "Supplier: ".

        if v-vend-no ne "" then do:
           v-qty-or-sup = v-qty-or-sup + trim(v-vend-no).
           IF v-po-no ne 0 THEN v-qty-or-sup = v-qty-or-sup + " PO#:" +
                                               trim(string(v-po-no,">>>>>>>>>>")).
        end.
       /* v-qty-or-sup = v-qty-or-sup + fill("_",38 - length(v-qty-or-sup)).*/
        
        i = 0.
        for each w-i:
          i = i + 1.
        end.
        if i LE 8 then do i = i + 1 to 8:
          create w-i.
        end.

        find first w-i.
        v-ink-1 =  w-i.i-dscr .
        v-lb-1  = (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find next w-i.
        v-ink-2 =  w-i.i-dscr .
        v-lb-2  =  (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find next w-i.
        v-ink-3 =  w-i.i-dscr .
        v-lb-3  =  (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find NEXT w-i.
        v-ink-4 =  w-i.i-dscr .
        v-lb-4  =  (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find NEXT w-i.
        v-ink-5 =  w-i.i-dscr .
        v-lb-5  =  (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find NEXT w-i.
        v-ink-6 =  w-i.i-dscr .
        v-lb-6  =  (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find NEXT w-i.
        v-ink-7 =  w-i.i-dscr .
        v-lb-7  =  (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".
        find NEXT w-i.
        v-ink-8 =  w-i.i-dscr .
        v-lb-8  = (IF w-i.i-qty <> 0 THEN string(w-i.i-qty,">>>,>>9") ELSE "" ) +
                   IF w-i.i-dscr <> "" THEN "LBS" ELSE "".

        v-form-sqft = round(if v-corr then (v-form-len * v-form-wid * .007)
                                       else (v-form-len * v-form-wid / 144),3).
        find first xxprep where xxprep.company eq cocode
                            and xxprep.code eq xeb.die-no
                            no-lock no-error.
        v-die-loc = IF AVAIL xxprep THEN xxprep.loc + " " + xxprep.loc-bin ELSE "".

        DISP  v-cus[1] AT 2 "PO #:" AT 40 xoe-ord.po-no WHEN AVAIL xoe-ord 
              "Set Qty:" trim(string(if avail xoe-ordl then xoe-ordl.qty
                                          else job-hdr.qty,">>>,>>9"))
                        when avail xeb and xeb.est-type eq 9    format "x(9)"
              "Style:" AT 80 xstyle.dscr WHEN AVAIL xstyle              
              SKIP
              v-cus[2] AT 2  
              "Job Qty:" AT 40 trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
              "Order Qty:" trim(string((if avail xoe-ordl then xoe-ordl.qty
                                             else job-hdr.qty) *
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
             /* 
              "Overrun:" AT 40 trim(string(IF AVAIL xoe-ordl THEN xoe-ordl.over-pct ELSE
                                           IF AVAIL xoe-ord  THEN xoe-ord.over-pct  ELSE 0,">>9.99%")) format "x(7)"
              "Underrun:" trim(string(IF AVAIL xoe-ordl THEN xoe-ordl.under-pct ELSE
                                      IF AVAIL xoe-ord  THEN xoe-ord.under-pct  ELSE 0,">>9.99%")) format "x(7)"
              */
              "Item Name: " AT 40 lv-fg-name FORM "x(26)"
              /*"Adders:" AT 80 v-adders FORM "x(23)"*/
              SKIP
              "<P8><C13><B><U>Board:</U>" /*AT 12*/ "<C45><U>Printing:</U>" /*AT 65*/ "<C80><U>Die Cutting, Slit, & Saw</B></U>" /*AT 125*/ "<P10>"SKIP
              "Shts Req'd:" AT 2 trim(string(v-sht-qty))   format "x(9)"
                " Sq Ft:" trim(string(v-form-sqft)) format "x(7)"
              "PRINTING PLATE #:" AT 40 xeb.plate-no when avail xeb  
              "Die #" AT 80 xeb.die-no when avail xeb /*" Loc:" v-die-loc*/ SKIP
              "W:" + trim(string({sys/inc/k16v.i v-form-wid},">,>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i v-form-len},">,>>9.99"))  format "x(22)" AT 2
              "MSF:"  + trim(string(v-sht-qty * v-form-sqft / 1000,">>>9.9<"))
                                                                format "x(11)"
              "Ink 1:" AT 40 v-ink-1 v-lb-1
              "Gross Size:" AT 80  
              "W:" + trim(string({sys/inc/k16v.i xef.gsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.gsh-len},">>>9.99"))
                                                                format "x(20)"
              "Slit: W:" + string(v-outw) + " L:" + string(v-outl) FORM "x(15)"   
              SKIP
              "Board:<B>" AT 2 v-form-code FORM "x(20)" "</B><C33.4>Ink 2:" /*AT 40*/ v-ink-2 v-lb-2 "<C66.8>Net   Size:" /*AT 80*/
              "W:" + trim(string({sys/inc/k16v.i xef.nsh-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.nsh-len},">>>9.99")) format "x(22)"
              SKIP
              v-form-dscr AT 4 FORM "x(30)"
              "Ink 3:" AT 40 v-ink-3 v-lb-3 "Die   Size:" AT 80
              "W:" + trim(string({sys/inc/k16v.i xef.trim-w},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xef.trim-l},">>>9.99")) format "x(22)"
              "Up:" "W:" + string(v-upl) + " L:" + string(v-upw) FORM "x(9)"
              SKIP
           /*   "Score:" AT 2 v-len-score               format "x(30)" */
              "Due In:" AT 2 v-po-due-date 
              "Ink 4:" AT 40 v-ink-4 v-lb-4 "Blank Size:" AT 80 
              "W:" + trim(string({sys/inc/k16v.i xeb.t-wid},">>>9.99")) +
              "  " +
              "L:" + trim(string({sys/inc/k16v.i xeb.t-len},">>>9.99")) format "x(22)"
              SKIP
              /*v-qty-or-sup AT 3 FORM "x(36)" */
               v-qty-or-sup AT 2 FORM "x(36)" 
              /*"Color Desc:" AT 40 xeb.i-coldscr when avail xeb*/
              "Ink 5:" AT 40 v-ink-5 v-lb-5
              "Impressions: " AT 80 trim(string(v-dc-qty))    format "x(7)" SKIP
              /*SUBSTRING(v-len-score,31,40) AT 6 FORM "x(40)"*/
              "Score:" AT 2 substring(v-len-score,1,30) WHEN xstyle.TYPE <> "F" format "x(30)" 
              "Ink 6:"  v-ink-6 v-lb-6
              "<C66.8>D/C Style:" /*AT 80*/ SKIP
              "Adders:<B>" AT 2 v-adders FORM "x(22)" "</B><C33.4>Ink 7:"  v-ink-7 v-lb-7 SKIP
              "Ink 8:" AT 40 v-ink-8 v-lb-8 SKIP
              WITH FRAME job1 NO-LABEL NO-BOX WIDTH 200 STREAM-IO.
                         
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
/*        PUT SKIP " " SKIP "        ".        
        PUT UNFORMATTED "<#8><R+" lv-rt-num "><C+105><RECT#8><|3>" SKIP.                
        /* box for pack */
        PUT "        ".       
        PUT "<#9><R+6><C+105><RECT#9><|3>" SKIP.       
        /* box for notes */
        PUT "        ".        
        PUT "<#10><R+7><C+105><RECT#10><|3>" SKIP.

        if v-format eq "Brick" then 
             put "<=#8>   Machine Routing        SU:    Start    Stop    Total   RUN:  Hours   Start   Stop   Total     QTY: In     Out      Waste" SKIP.
        ELSE PUT "<=#8>   Machine Routing        SU:    Start    Stop    Total   RUN:  Start   Stop    Total   QTY:    In     Out     Waste     Date" SKIP.
        
        PUT "<=#8><R+1><C+4><from><C+100><Line>"
                    SKIP.
*/
        put skip
            "<P7>   Machine Routing                  % of Wax        SU:    Start       Stop      Total    RUN:        Hours          Start    Stop    Total  QTY: In     Out    Date   Checkout<P10>" SKIP.
        i = 0.
        for each w-m by w-m.dseq:

          ASSIGN 
            i = i + 1
            v-letter = substr("UTE",i,1).

          FIND FIRST mach NO-LOCK WHERE mach.company = cocode
                                    AND mach.m-code = w-m.m-code 
               NO-ERROR.
          IF AVAIL mach AND mach.dept[1] = "WN" THEN
              ASSIGN v-p-wax = (IF AVAIL xef THEN xef.leaf[1] ELSE "").
          ELSE 
              ASSIGN v-p-wax = "".

         /* if v-format eq "Brick" then*/
          display /*v-letter                      at 1
                  chr(124) format "x"           at 2 */
                  w-m.dscr AT 3 FORM "x(20)" 
                  v-p-wax FORM "x(10)" WHEN w-m.dscr NE ""
                  /*fill("_",4)  format "x(4)"    to 25   when w-m.dscr ne ""
                  fill("_",1)  format "x"    to 27   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 33   when w-m.dscr ne ""*/
                  w-m.s-hr                              when w-m.s-hr ne 0
                  fill("_",5)  format "x(5)"    to 46   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 54   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 62   when w-m.dscr ne ""
                  space(1)
                  w-m.r-sp                              when w-m.r-sp ne 0
                  w-m.r-hr                              when w-m.r-hr ne 0
                  fill("_",5)  format "x(5)"    to 87   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 93   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 99   when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 105  when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 111  when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 117  when w-m.dscr ne ""
                  fill("_",5)  format "x(5)"    to 123  when w-m.dscr ne ""
                 /* chr(124) format "x"           at 131 */                  
              with no-box no-labels frame oo1 width 150 no-attr-space down STREAM-IO.
                  
          v-lines = v-lines + 1.
        end.
      /*
        display v-line[4]                 at 2

            with no-box no-labels frame m5 width 132 no-attr-space STREAM-IO.
      */
        FIND LAST b-ef USE-INDEX est-qty WHERE b-ef.company = est.company
                                           AND b-ef.est-no = est.est-no NO-LOCK NO-ERROR.
        
        IF AVAIL b-ef AND b-ef.form-no = w-ef.frm THEN 
           FOR EACH w-m:
               CREATE tt-wm.
               BUFFER-COPY w-m TO tt-wm.
        END.
        PUT SKIP(1).
        PUT "<#6><C1><FROM><C105><LINE><|3>" .
            
        RUN cecrep/jobrfc22.p (RECID(job-hdr),v-format,cust.terms).
        PUT "<=6><C28><FROM><R+10><C28><LINE><|3>"
            /*"<=6><C44><FROM><R+8><C44><LINE><|3>"*/
            "<=6><C65><FROM><R+10><C65><LINE><|3>"
            /*"<=6><C79><FROM><R+8><C79><LINE><|3>"            
            "<=6><C93><FROM><R+8><C93><LINE><|3>"*/ .
        
        RUN stackImage.

        PUT "<#7><C1><FROM><C105><LINE><|3>"
            "<=7><C28><FROM><R+11><C28><LINE><|3>"
            "<=7><C74><FROM><R+11><C74><LINE><|3>"
            "<P8>" SKIP.

        DISPLAY "<=7><C45><U><B>Floor Comments</U>" AT 2   "<U>Shipping Info: </U></B> <P10>" AT 90 SKIP
                "Ship To #:" AT 90 
                xoe-ord.sold-id when avail xoe-ord
                xeb.ship-id when avail xeb @ xoe-ord.sold-id
                xoe-rel.ship-id when avail xoe-rel @ xoe-ord.sold-id  SKIP
                "____________________________________________________" AT 35 v-shp[1] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[2] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[3] AT 90 SKIP
                "____________________________________________________" AT 35 v-shp[4] AT 90 SKIP
                "____________________________________________________" AT 35 "Item PO #:" AT 90 xoe-ordl.po-no when avail xoe-ordl
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                /*"____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP
                "____________________________________________________" AT 35 SKIP*/
                /*"____________________________________________________________________________" SKIP */
               with no-box no-labels frame m8 width 170 no-attr-space STREAM-IO.

        /*PAGE.*/
        /* dept notes */
        v-note-length = 124.
        
        DEF VAR li AS INT NO-UNDO.

        FOR EACH tt-formtext:
            DELETE tt-formtext.
        END.
        lv-text = "".
        FOR EACH notes WHERE notes.rec_key = job.rec_key and
                       (notes.note_form_no = w-ef.frm OR notes.note_form_no = 0)
                       AND NOT CAN-DO(cha-val,notes.note_code) NO-LOCK:
            lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
        END.
        DO li = 1 TO 6:
           CREATE tt-formtext.
           ASSIGN tt-line-no = li
                  tt-length  = 124.
        END.
        RUN custom/formtext.p (lv-text).
        i = 0.
        v-dept-note = "".
        FOR EACH tt-formtext:
            i = i + 1.
            IF i <= 6 THEN v-dept-note[i] = tt-formtext.tt-text.      
        END.


        v-inst = "".
        

        FOR EACH tt-formtext:
            DELETE tt-formtext.
        END.
        lv-text = "".
        FOR EACH notes WHERE notes.rec_key = itemfg.rec_key NO-LOCK.
            lv-text = lv-text + notes.note_title + ":" + trim(notes.note_text) + CHR(10).
        END.
        DO li = 1 TO 6:
           CREATE tt-formtext.
           ASSIGN tt-line-no = li
                  tt-length  = 124.
        END.
        RUN custom/formtext.p (lv-text).
        i = 0.
        v-spec-note = "".
        FOR EACH tt-formtext:
            i = i + 1.
            IF  i <= 6 THEN v-spec-note[i] = tt-formtext.tt-text.      
        END.

        PAGE.
        PUT "<#11><C1><FROM><C106><R+47><RECT><||3><C80><P10>"
            "<=11><C30><FROM><R+3><C30><LINE><||3>"
            "<=11><C60><FROM><R+3><C60><LINE><||3>"
            "<=11><R+3><C1><FROM><C106><LINE><||3>"
            "<=11>Job # <C30>"" Estimate # <C60> Cust Part #"  SKIP
            "<P12><C12>" v-job-prt 
            "<C40>" v-est-no
            "<C70>" lv-part-no.

        if print-box and avail xest then do:
            /*PAGE. */
           /* gdm - 09300905 */
           v-out1-id = RECID(bf-eb).
           run cec/desprnL4.p (recid(xef),
                               input-output v-lines,
                               recid(xest)).
        end.
        PUT "<=11><R+33><C1><FROM><C105><LINE><|3>" SKIP
            "<=11><R+33><C1> Department Notes" SKIP
            v-dept-note[1] AT 2 SKIP
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
            .
          PAGE.

          IF s-prt-fgimage THEN DO:        
            /*ls-fgitem-img "r:\asi_gui9\source\images\c1318a.jpg".*/
            FIND FIRST b-itemfg WHERE b-itemfg.company = job-hdr.company
                                  AND b-itemfg.i-no = v-fg NO-LOCK NO-ERROR.
            ls-fgitem-img = IF AVAIL b-itemfg THEN b-itemfg.box-image ELSE itemfg.box-image .
        
            PUT UNFORMATTED "<#12><C1><FROM><C106><R+47><RECT><||3><C80>" 
                "<=12><C30><FROM><R+3><C30><LINE><||3>"
            "<=11><C60><FROM><R+3><C60><LINE><||3>"
            "<=11><R+3><C1><FROM><C106><LINE><||3>"
            "<=11>Job # <C30> Estimate # <C60> Cust Part #"  SKIP
            "<P12><C12>" v-job-prt 
            "<C40>" v-est-no
            "<C70>" lv-part-no SKIP
               /*"<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
              "<=12><R+3><C1><FROM><C106><LINE><||3>"*/
              "<=12><R+5><C5><#21><R+45><C+90><IMAGE#21=" ls-fgitem-img ">" SKIP. 
            PAGE.
        END. 
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
             v-ord-qty = (if avail xoe-ordl then xoe-ordl.qty else job-hdr.qty) *
                         if est.form-qty le 1 then 1 else v-pqty.
             v-over-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.over-pct,">>9.99%")) ELSE
                          IF AVAIL xoe-ord  THEN trim(string(xoe-ord.over-pct,">>9.99%"))  ELSE "".
             v-under-run = IF AVAIL xoe-ordl THEN trim(string(xoe-ordl.under-pct,">>9.99%")) ELSE
                           IF AVAIL xoe-ord  THEN trim(string(xoe-ord.under-pct,">>9.99%"))  ELSE "".            
             PUT "<R3><C1><#15><C30><P16><B> SET HEADER<P7></B>" SKIP(2)
                 "Job #: " AT 3 v-job-prt "<C25>Our Order #: " v-ord-no 
                 "<C67>Our Date: " v-ord-date SKIP
                 "Est #: " AT 3 v-est-no "<C25>FG #: " v-fg-set "<C60>Due Date: " v-due-date SKIP
                 "<=1><R+6><C2><From><R+5><C78><RECT><||3>" SKIP
                 "<=1><R+6><C2>CUSTOMER INFORMATION <C25> ORDER INFORMATION <C58>ITEM DESCRIPTION" SKIP
                 v-cus[1] AT 3 " PO#: " v-po-no " Set Qty: "  v-set-qty
                 v-i-line[2] AT 90
                 SKIP
                 v-cus[2] AT 3 " Job Qty:" trim(string(job-hdr.qty * v-pqty,">>>,>>9"))    format "x(7)"
                 " Order Qty:" string(v-ord-qty) format "x(7)"
                 v-i-line[3] AT 90 SKIP
                 v-cus[3] AT 3  " Cust Part #:" lv-part-no 
                 v-i-line[4] AT 90 SKIP
                 v-cus[4]  AT 3 " Overrun:"  format "x(7)"  
                 " Underrun:" format "x(7)"  
                 "Adders:" v-adders FORM "x(33)" AT 90 SKIP
                 "<=1><R+11><C30><P8><B>Set Components<P7></B> <C50>Set item: " v-fg-set SKIP
                 "<C2>FINISHED GOOD #                 DESCRIPTION                       RATIO PER SET" SKIP.
             /* each components */
             DEF VAR v-tmp-line AS INT NO-UNDO.
             DEF VAR v-shipto AS cha NO-UNDO.
        
             v-tmp-line = 0.
             FOR EACH xeb WHERE xeb.company = est.company
                             AND xeb.est-no = est.est-no
                             AND xeb.form-no > 0 NO-LOCK:
                 PUT xeb.stock-no AT 3 space(14) xeb.part-dscr1 space(5) xeb.yld-qty SKIP.
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
                   v-tmp-line = v-tmp-line + 1.
                END.
             END.
             PUT "<=1><R+12><C2><FROM><R+" + string(v-tmp-line) + "><C78><RECT><||3>" FORM "x(150)" SKIP.
             v-tmp-line = v-tmp-line + 12 .
        
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
        
             end.
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
          END. /* set header printing */
        END. /* est.est-type = 6 */
        /* end of set header printing */

    end.  /* each job */
    end.  /* end v-local-loop  */
 
        hide all no-pause.


PROCEDURE stackImage:
  DEFINE BUFFER pattern FOR reftable.

  IF v-stackcode EQ '' THEN RETURN.
  FIND FIRST pattern NO-LOCK
       WHERE pattern.reftable EQ 'STACKPAT'
         AND pattern.company EQ ''
         AND pattern.loc EQ ''
         AND pattern.code EQ SUBSTR(v-stackcode,9,1) NO-ERROR.
  IF AVAILABLE pattern AND SEARCH(pattern.dscr) NE ? THEN
  PUT UNFORMATTED
    "<#stackImage><C27><R+1><FROM><C2><R+10>"
    "<IMAGE#stackImage=" pattern.dscr ">"
    "<R-11>".
END PROCEDURE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
