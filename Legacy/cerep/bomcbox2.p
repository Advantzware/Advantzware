/* cerep/bomcbox.p  copied from cerep/jobcbox.p               05/05 YSK     */
/* Bill of Materials for folding Century Box                                   */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}
DEFINE INPUT PARAMETER ipl-part-no AS LOG INIT NO NO-UNDO.

def new shared var save_id as recid.
def new shared var v-today as date init today.
def new shared var v-job as char format "x(6)" extent 2 init [" ","zzzzzz"].
def new shared var v-job2 as int format "99" extent 2 init [00,99].
def new shared var v-stypart like style.dscr.
def new shared var v-dsc like oe-ordl.part-dscr1 extent 2.
def new shared var v-size as char format "x(26)" extent 2.
def new shared var v-bld-job like oe-ord.job-no.
def new shared var v-bld-job2 like oe-ord.job-no2.
def new shared var v-fill as char format "x(90)".
def new shared var v-frst as log.
def new shared var v-ok as log.
def new shared var v-est-qty as int format "->>,>>>,>>9".
def new shared var v-job-qty as int format "->>,>>>,>>9".
def new shared var v-fac as DEC .
def new shared var v-job-no like oe-ordl.job-no.
def new shared var v-job-no2 like oe-ordl.job-no2.
def new shared var v-due-date like oe-ord.due-date.
def new shared var v-reprint as log.
def new shared var v-up like eb.num-up.
def new shared var v-tandem as log.
def new shared var v-form-no like eb.form-no.
def new shared var v-fup as char.
def new shared var v-layout as char format "x(30)".
DEF SHARED VAR s-committed-board-only AS LOG NO-UNDO.
def var v-line as int init 1 no-undo.
def var v-gsh-qty as int no-undo.
def var cnt as int init 1 no-undo.
def var v-frm-blk as char format "x(6)" no-undo.
def var v-dec as dec no-undo.
def var v-ovund as char format "x(34)" no-undo.
def var v-mrhr as char format "x(5)".
def var v-cas-dscr like item.est-dscr.
def var v-first as log no-undo.
def var v-spec-list as char format "x(20)"init "QA" no-undo.
DEF VAR lv-form-note AS cha NO-UNDO.
DEF VAR v-itm-printed AS INT NO-UNDO.
DEF VAR v-alloc AS cha NO-UNDO.
DEF VAR v-prep AS cha EXTENT 8 NO-UNDO.
DEF VAR v-misc AS cha EXTENT 6 NO-UNDO.
DEF VAR v-spec-no AS cha EXTENT 8 NO-UNDO.
DEF VAR v-skip AS LOG NO-UNDO.
DEF VAR v-fill2 AS cha INIT "-" FORM "x(90)" NO-UNDO.
DEF VAR v-spoil LIKE job-mch.wst-prct NO-UNDO.
DEF VAR v-unit AS INT NO-UNDO.
DEF VAR v-spec-cnt AS INT NO-UNDO.
DEF VAR v-job-qty-tot AS INT NO-UNDO.
DEF VAR v-stock-no LIKE eb.stock NO-UNDO.
DEF VAR ld-film-w AS DEC NO-UNDO.
DEF VAR v-basis-w LIKE job-mat.basis-w NO-UNDO.

def TEMP-TABLE w-lo NO-UNDO
  field layout like v-layout.

def new shared buffer xjob-hdr for job-hdr.

def buffer b-eb for eb.
DEF BUFFER b-rt FOR reftable.

def new shared workfile wrk-op
  field m-dscr like est-op.m-dscr
  field m-code like est-op.m-code
  field d-seq like est-op.d-seq
  field dept like est-op.dept
  field b-num like est-op.b-num
  field s-num like est-op.s-num
  field pass like est-op.op-pass
  field mr like est-op.op-mr extent 100
  field speed like est-op.op-speed extent 100
  field run-hr like job-mch.run-hr extent 100
  field num-sh like est-op.num-sh extent 100
  FIELD spoil LIKE job-mch.wst-prct EXTENT 100
  FIELD mr-waste LIKE job-mch.mr-waste EXTENT 100.

def new shared workfile wrk-die
  field die-no like eb.die-no
  FIELD cad-no LIKE eb.cad-no
  field form-no like eb.form-no
  field die-size as char format "x(17)".

def new shared workfile wrk-sheet
  field gsh-qty like ef.gsh-qty
  field cal like ef.cal
  FIELD i-no LIKE ef.board
  field brd-dscr like ef.brd-dscr
  field form-no like ef.form-no
  field sh-wid like ef.nsh-len
  field sh-len like ef.nsh-wid.

def new shared workfile wrk-film
  field form-no like ef.form-no
  field snum as int format "99"
  field bnum as int format "99"
  field leaf as char format "x(10)"
  field leaf-l as dec format ">9.9999"
  field leaf-w as dec format ">9.9999".

def new shared workfile wrk-ink
  field i-code as char format "x(10)"
  field form-no like eb.form-no
  field blank-no like eb.blank-no
  field i-dscr as char format "x(20)"
  field i-qty as dec format ">,>>9.9<"
  field i-pass as dec
  FIELD i-unit AS DEC.

def new shared workfile wrk-prep
  field code like est-prep.code
  field dscr like est-prep.dscr
  field s-num as int format "99"
  field b-num as int format "99"
  field ml like est-prep.ml.

def new shared workfile wrk-spec
  field form-no like ef.form-no
  field spec-no as char format "x(10)"
  field dscr as char format "x(20)"
  field qty as dec format ">>>9.9<<<"
  field uom as char format "x(3)".

def new shared workfile wrk-inst
  field d-seq like dept.fc
  field dscr like est-inst.dscr
  field line like est-inst.line-no
  field rec-id as recid.

def new shared workfile wrk-misc
  field form-no like ef.form-no
  field snum as int format "99"
  field bnum as int format "99"
  field cost as char format "x(20)".
  
DEF TEMP-TABLE tt-sum 
     FIELD form-no LIKE eb.form-no
     FIELD blank-no LIKE eb.blank-no
     FIELD dscr AS cha
     FIELD job-qty LIKE job-hdr.qty
     FIELD i-no LIKE eb.stock-no
     FIELD style LIKE eb.style
     FIELD SIZE AS cha
     FIELD cas-cnt LIKE eb.cas-cnt
     FIELD cas-no LIKE eb.cas-no
     FIELD cas-cost LIKE eb.cas-cost
     FIELD cas-size AS cha
     FIELD tr-no LIKE eb.tr-no
     FIELD tr-cnt LIKE eb.tr-cnt
     FIELD tr-cost LIKE eb.tr-cost
     FIELD tr-size AS cha
     FIELD po-no LIKE oe-ordl.po-no
     FIELD cad-no LIKE eb.cad-no
     FIELD part-no LIKE eb.part-no
     FIELD eb-recid AS RECID.

DEF VAR v-sign-line AS cha NO-UNDO.
DEF VAR v-sign-label AS cha NO-UNDO.
DEF VAR ld-qty-ton AS DEC NO-UNDO.
DEF VAR ld-qty-lf AS INT NO-UNDO.
DEF VAR ld-cost-amt AS INT NO-UNDO.
DEF VAR ld-qty-cost AS int NO-UNDO.
DEF VAR ld-qty-avail AS INT NO-UNDO.
DEF VAR ld-tot-ton AS DEC NO-UNDO.
DEF VAR ld-qty-onh AS INT NO-UNDO.
DEF VAR ld-tot-lf AS INT NO-UNDO.
DEF VAR ld-tot-avail AS INT NO-UNDO.
DEF VAR ld-tot-onh AS INT NO-UNDO.
DEF VAR v-start-date AS DATE NO-UNDO.
DEF VAR v-req-date AS DATE NO-UNDO.
DEF VAR v-shipto AS cha FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-case-size AS cha NO-UNDO.
DEF VAR v-vend LIKE po-ord.vend-no NO-UNDO.
DEF VAR v-item AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink1 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink2 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink10 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink20 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no NO-UNDO.
DEF VAR lv-mat-dept-list AS cha INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach AS cha NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF VAR v-fgitm AS cha FORM "x(15)" EXTENT 10 NO-UNDO.
DEF VAR v-fgdsc LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEF VAR v-fgqty LIKE job-hdr.qty EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEF VAR v-pono LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEF VAR v-part-no LIKE eb.part-no EXTENT 10 NO-UNDO.
DEF VAR v-cas-pal LIKE eb.cas-cnt EXTENT 10 NO-UNDO.
DEF VAR v-cas-cnt LIKE eb.cas-cnt EXTENT 10 NO-UNDO.

DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF TEMP-TABLE tt-fgitm NO-UNDO FIELD i-no AS cha FORM "x(15)"
                        FIELD seq AS INT
                        FIELD qty AS INT 
                        FIELD i-dscr AS cha
                        FIELD po-no AS cha
                        FIELD part-no AS cha 
                        FIELD cas-cnt LIKE eb.cas-cnt
                        FIELD cas-pal LIKE eb.cas-pal.
DEF VAR v-board-po LIKE oe-ordl.po-no-po NO-UNDO.
DEF VAR v-plate-printed AS LOG NO-UNDO.
DEF BUFFER xoe-ordl FOR oe-ordl.
DEF VAR v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name2 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name3 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-last-j AS INT NO-UNDO.
DEF VAR v-po-no2 LIKE v-po-no NO-UNDO.
DEF VAR v-po-no3 LIKE v-po-no NO-UNDO.
DEF VAR v-num-of-inks AS INT NO-UNDO.
DEF VAR v-bar-no AS cha NO-UNDO.

v-fill = /*fill("=",128)*/ "<||3><C3><FROM><C81><LINE><||3>".

def new shared frame head.
/*
DEF SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF SHARED VAR s-run-speed AS LOG NO-UNDO.
*/
DEF VAR v-po-duedate LIKE po-ordl.due-date NO-UNDO.
DEF VAR v-upc-no AS cha NO-UNDO.
DEF VAR v-upc-lbl AS cha FORM "x(10)" NO-UNDO.
DEF VAR v-shipto1 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shipto2 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.

DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-pg-num2 AS INT INIT 1 NO-UNDO.
DEF VAR lv-tot-pg AS INT INIT 1 NO-UNDO.
DEF BUFFER x-hdr FOR job-hdr.
DEF VAR lv-prt-sts AS cha NO-UNDO.
DEF VAR lv-prt-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR lv-prt-time AS cha NO-UNDO.
DEF VAR lv-cad-image AS cha NO-UNDO.
DEF VAR lv-cad-image-list AS cha NO-UNDO.
DEF VAR v-film-size AS cha NO-UNDO.
DEF VAR ld-tot-qty like ef.gsh-qty NO-UNDO.
DEF VAR lv-last-form-no AS INT NO-UNDO.
DEF SHARED VAR s-print-revised AS LOG NO-UNDO.
DEF VAR lv-status AS cha NO-UNDO.
DEF VAR v-job-len LIKE job-mat.len NO-UNDO.
DEF VAR v-job-wid LIKE job-mat.wid NO-UNDO.
def shared var v-export     as log init no format "Y/N".
def shared var v-exp-name   as char format "x(40)" initial "c:~\tmp~\r-bilmat.csv".
def shared stream s-temp.
DEFINE  VARIABLE excelheader AS CHARACTER  NO-UNDO.

lv-status = IF s-print-revised THEN "<FGCOLOR=RED>REVISED<FGCOLOR=BLACK>" ELSE "<FGCOLOR=BLACK>ORIGINAL".

ASSIGN v-sign-line  = "     ____________    __________    _____________    _______________    ___________    ___________"
       v-sign-label = "     Date Ordered     Supplier         PO No.         Amt Ordered        Due Date     Initials".

format "Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
       "Salesman:" at 68 oe-ord.sname[1] "Order#:" at 113 oe-ord.ord-no
    with no-box frame line-head no-labels stream-io width 132.
    
{sys/inc/notes.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
FIND FIRST job-hdr NO-LOCK NO-ERROR.

FORM header "<#1><P14> <C30><B>Bill of Materials<P9>" skip(1)
          "<C3>JOB NUMBER:</B> " trim(v-job-no) + "-" + string(v-job-no2,"99") format "x(9)" 
          "<C40><B>Customer Name:</B> <C53>" v-cust-name SKIP
          "<C3><B>Due Date:</B>   " v-due-date  "<C40><B>Ship To:</B><C53>" v-shipto[1] SKIP
          "<C3><B>Estimate:</B>   " trim(job-hdr.est-no) FORM "x(8)" "<C53>" v-shipto[2] SKIP
          "<C3><B>Printed Date:</B>  " string(TODAY,"99/99/9999") FORM "x(10)" " " STRING(TIME,"HH:MM:SS")   "<C53>" v-shipto[4] SKIP
          "<C3><B>Status:</B>   " lv-status FORM "x(35)"
          "<C44>CAD Size Checked on Order <C40><FROM><C+3><R+2><RECT>" SKIP
         WITH FRAME head PAGE-TOP WIDTH 100 STREAM-IO NO-LABEL NO-BOX.      

ASSIGN
 v-job[1]    = fjob-no
 v-job[2]    = tjob-no
 v-job2[1]   = fjob-no2
 v-job2[2]   = tjob-no2
 v-reprint   = reprint
 v-spec-list = spec-list.


PUT "<FCourier New>".

for each job-hdr
        where job-hdr.company               eq cocode
          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no
          /*and job-hdr.ftick-prnt            eq v-reprint
          and can-find(first job where job.company eq cocode
                                   and job.job     eq job-hdr.job
                                   and job.job-no  eq job-hdr.job-no
                                   and job.job-no2 eq job-hdr.job-no2
                                   and job.stat    ne "H")  */
        use-index job-no no-lock,

        first est
        where est.company  eq job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          /*and est.est-type le 4  */  /* Ticket 15842 */
        no-lock

        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY job-hdr.frm:       

      ASSIGN lv-prt-sts = IF NOT job-hdr.ftick-prnt THEN "ORIGINAL" ELSE "REVISED"
             lv-prt-date = TODAY
             lv-prt-time = STRING(TIME,"hh:mm am").

      v-est-qty = if avail est then est.est-qty[1] else 0.
      find first oe-ord where oe-ord.company eq job-hdr.company
                          and oe-ord.ord-no  eq job-hdr.ord-no no-lock no-error.

      IF FIRST-OF(job-hdr.job-no2) THEN do:
         lv-tot-pg = 0.
         FOR EACH x-hdr WHERE x-hdr.company = job-hdr.company
                          AND x-hdr.job-no = job-hdr.job-no
                          AND x-hdr.job-no2 = job-hdr.job-no2 NO-LOCK
                          break by x-hdr.job by x-hdr.job-no by x-hdr.job-no2 BY x-hdr.frm :
              IF FIRST-OF(x-hdr.frm) THEN lv-tot-pg = lv-tot-pg + 1.
         END.
         lv-pg-num2 = 1.
         lv-cad-image-list = "".
         v-first = YES.
         ASSIGN v-job-no  = job-hdr.job-no
                v-job-no2 = job-hdr.job-no2.

         if avail oe-ord then
            if not oe-ctrl.p-fact and oe-ord.stat eq "H" then next.

         v-due-date = if avail oe-ord then oe-ord.due-date else ?.
         v-start-date = job-hdr.start-date.

         v-shipto = "".
         find first oe-ordl
              where oe-ordl.company eq job-hdr.company
                and oe-ordl.ord-no  eq job-hdr.ord-no
                and oe-ordl.job-no  eq job-hdr.job-no
                and oe-ordl.job-no2 eq job-hdr.job-no2
                and oe-ordl.i-no    eq job-hdr.i-no
              no-lock no-error.
         IF AVAIL oe-ordl THEN
            find first oe-rel where oe-rel.company eq cocode
                 and oe-rel.ord-no  eq oe-ordl.ord-no
                 and oe-rel.i-no    eq oe-ordl.i-no
                 and oe-rel.line    eq oe-ordl.line
                 no-lock no-error.
         if avail oe-rel then do:
            find first shipto where shipto.company eq cocode
              and shipto.cust-no eq oe-rel.cust-no
              and shipto.ship-id eq oe-rel.ship-id no-lock no-error.  
            if avail shipto then
               ASSIGN v-shipto[1] = shipto.ship-name
                   v-shipto[2] = shipto.ship-addr[1]
                   v-shipto[3] = shipto.ship-addr[2]
                   v-shipto[4] = trim(oe-rel.ship-city) + ", " +
                                 oe-rel.ship-state + "  " + oe-rel.ship-zip.          
         end.

         v-req-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?.
         FIND FIRST cust WHERE cust.company = job-hdr.company AND
                            cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.
         v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                    ELSE IF AVAIL cust THEN cust.name
                    ELSE job-hdr.cust-no.
         FIND first eb WHERE eb.company     EQ job-hdr.company
                      AND eb.est-no      eq job-hdr.est-no
                      and eb.form-no     eq job-hdr.frm
                      AND eb.stock-no = job-hdr.i-no NO-LOCK NO-ERROR.
         IF NOT AVAIL eb THEN FIND first eb WHERE eb.company     EQ job-hdr.company
                      AND eb.est-no      eq job-hdr.est-no
                      and eb.form-no     eq job-hdr.frm
                      AND eb.blank-no > 0 NO-LOCK NO-ERROR.
         v-bar-no = IF AVAIL eb THEN eb.spc-no ELSE trim(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99").
         
       
         if first(job-hdr.job-no) then 
            PUT "<#1><P14> <C30><B>Bill of Materials<P9>" skip(1)
              "<C3>JOB NUMBER:</B> " trim(v-job-no) + "-" + string(v-job-no2,"99") format "x(9)" 
              "<C40><B>Customer Name:</B> <C53>" v-cust-name SKIP
              "<C3><B>Due Date:</B>   " v-due-date  "<C40><B>Ship To:</B><C53>" v-shipto[1] SKIP
              "<C3><B>Estimate:</B>   " trim(job-hdr.est-no) FORM "x(8)" "<C53>" v-shipto[2] SKIP
              "<C3><B>Printed Date:</B>  " string(TODAY,"99/99/9999") FORM "x(10)" " " STRING(TIME,"HH:MM:SS") "<C53>" v-shipto[4] SKIP
              "<C3><B>Status:</B>   " lv-status FORM "x(35)"
              "<C44>CAD Size Checked on Order <C40><FROM><C+3><R+2><RECT>" SKIP
              .
         
         VIEW FRAME head.    

         IF FIRST-OF(job-hdr.job-no) THEN DO:
           IF v-export THEN
            PUT STREAM s-temp UNFORMATTED
            ',,'
            '"' "Bill of Materials"  '",' SKIP
            '"' "JOB NUMBER:  " string(v-job-no) + "-" + string(v-job-no2,"99")   '",,,,'
            '"' "Customer Name:  " v-cust-name  '",' SKIP
            '"' "Due Date:  " string(v-due-date) '",,,,'
            '"' "Ship To:  " v-shipto[1]  '",' SKIP
            '"' "Estimate:  " string(job-hdr.est-no) '",,,,'
            '"'  "               " v-shipto[2]  '",' SKIP
            '"' "Printed Date:  " string(TODAY,"99/99/9999") "," STRING(TIME,"HH:MM:SS") '",,,,'
            '"'  "               " v-shipto[4]  '",' SKIP
            '"' "Status:  " IF s-print-revised THEN "REVISED" ELSE "ORIGINAL" '",,,,'
            '"'  "CAD Size Checked on Order" '",' SKIP(2)

            
             SKIP.
         END. 
         
      END. /*if first-of(job-hdr.frm) then ASSIGN v-first = yes.*/

      /** PRINT JOB HEADER **/
      if v-first then do:
        
        /* barcode print */
        
        v-line = if avail est                            and
                 est.est-type gt 2 and est.est-type lt 5 then 500 else 50.

        /** SUM UP NUMBER OF SHEETS **/
        find first job
            where job.company eq cocode
              and job.job     eq job-hdr.job
              and job.job-no  eq v-job-no
              and job.job-no2 eq v-job-no2
            no-lock no-error.
            
        if avail job then
        for each job-mch
            where job-mch.company eq cocode
              and job-mch.job     eq job.job
              and job-mch.job-no  eq job.job-no
              and job-mch.job-no2 eq job.job-no2
              AND job-mch.frm = job-hdr.frm
            no-lock,

            first mach
            {sys/ref/machW.i}
              and mach.m-code eq job-mch.m-code
            no-lock

            by mach.d-seq
            by job-mch.frm
            by job-mch.blank-no
            by job-mch.pass
            by job-mch.run-qty desc:

          find first wrk-op
              where wrk-op.m-code eq job-mch.m-code
                and wrk-op.s-num  eq job-mch.frm
               and wrk-op.b-num  eq job-mch.blank-no
                and wrk-op.pass   eq job-mch.pass 
              no-error.
          if not avail wrk-op then do:
            create wrk-op.
            assign
             wrk-op.m-code = job-mch.m-code
             wrk-op.m-dscr = mach.m-dscr
             wrk-op.d-seq  = mach.d-seq
             wrk-op.dept   = job-mch.dept
             wrk-op.s-num  = job-mch.frm
             wrk-op.b-num  = job-mch.blank-no
             wrk-op.pass   = job-mch.pass.
          end.
          assign
           wrk-op.mr[job-mch.frm]     = job-mch.mr-hr
           wrk-op.speed[job-mch.frm]  = job-mch.speed
           wrk-op.num-sh[job-mch.frm] = job-mch.run-qty
           wrk-op.spoil[job-mch.frm] = job-mch.wst-prct   
           wrk-op.mr-waste[job-mch.frm] = job-mch.mr-waste   
           wrk-op.run-hr[job-mch.frm] = job-mch.run-hr   .
        end.

        /** BUILD PREP WORK FILE **/
        for each job-prep
            where job-prep.company eq cocode
              and job-prep.job     eq job-hdr.job
              and job-prep.job-no  eq job-hdr.job-no
              and job-prep.job-no2 eq job-hdr.job-no2
            no-lock:
          find first prep
              where prep.company eq cocode
                and prep.code    eq job-prep.code
              no-lock no-error.
          create wrk-prep.
          assign
           wrk-prep.code = job-prep.code
           wrk-prep.dscr = if avail prep then prep.dscr else ""
           wrk-prep.s-num = job-prep.frm
           wrk-prep.b-num = job-prep.blank-no
           wrk-prep.ml = job-prep.ml.
        end. /* each job-prep */

        if avail est then
        for each est-prep
            where est-prep.company eq est.company
              AND est-prep.est-no  eq est.est-no
              and index("SON",est-prep.simon) gt 0
            no-lock:
          find first prep
              where prep.company eq cocode
                and prep.code    eq est-prep.code
              no-lock no-error.
          create wrk-prep.
          assign
           wrk-prep.code  = est-prep.code
           wrk-prep.dscr  = if avail prep then prep.dscr else ""
           wrk-prep.s-num = est-prep.s-num
           wrk-prep.b-num = est-prep.b-num
           wrk-prep.ml    = est-prep.ml.
        end.

        if avail oe-ord then
        for each oe-ordm 
            where oe-ordm.company eq cocode
              and oe-ordm.ord-no  eq oe-ord.ord-no
            no-lock:
          find first wrk-prep where wrk-prep.code eq oe-ordm.charge no-error.
          if not avail wrk-prep then do:
            find first prep
                where prep.company eq cocode
                  and prep.code    eq oe-ordm.charge
                no-lock no-error.
            create wrk-prep.
            assign
             wrk-prep.code  = oe-ordm.charge
             wrk-prep.dscr  = if avail prep then prep.dscr else ""
             wrk-prep.s-num = 9
             wrk-prep.b-num = 99
             wrk-prep.ml    = if avail prep then prep.ml else ?.
          end.
        end.
      

      for each ef
          WHERE ef.company EQ job-hdr.company
            AND ef.est-no  EQ job-hdr.est-no
            AND ef.form-no = job-hdr.frm
          break by ef.est-no by ef.form-no:

        v-job-qty = 0.
        for each xjob-hdr
            where xjob-hdr.company eq cocode
              and xjob-hdr.job     eq job-hdr.job
              and xjob-hdr.job-no  eq job-hdr.job-no
              and xjob-hdr.job-no2 eq job-hdr.job-no2
              and xjob-hdr.i-no    eq job-hdr.i-no
            no-lock:
          v-job-qty = v-job-qty + xjob-hdr.qty.
        end.
          
        v-est-qty = 0.
        if est.est-type eq 4 then
        for each eb
            WHERE eb.company  EQ ef.company
              AND eb.est-no   eq ef.est-no
              and eb.stock-no eq job-hdr.i-no
            no-lock:
          v-est-qty = v-est-qty + eb.yld-qty.
        end.

        else v-fac = 1.
        v-itm-printed = 0.
     
        if ef.form-no eq job-hdr.frm then ebloop:
        for each eb
            WHERE eb.company     EQ ef.company
              AND eb.est-no      eq ef.est-no
              and eb.form-no     eq ef.form-no
              /*and ((eb.stock-no  eq job-hdr.i-no and
                    eb.stock-no  ne "") or
                   (eb.blank-no  eq job-hdr.blank-no or
                    (eb.blank-no eq 1 and
                     (est.est-type eq 2 or est.est-type eq 6)) and
                    eb.stock-no  eq ""))  */  NO-LOCK
            break by eb.form-no BY eb.blank-no.

      /*    if est.est-type eq 4 and eb.stock-no ne job-hdr.i-no then next ebloop. */

          create w-lo.
          for each b-eb
              WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  eq eb.est-no
                and b-eb.part-no eq eb.part-no
              no-lock break by b-eb.est-no:
            v-fup = "F" + trim(string(b-eb.form-no,">>9")) + "-" +
                    trim(string(b-eb.blank-no,"99")) + "/" +
                    trim(string(b-eb.num-up,">>9")) + "up".
            if length(trim(v-fup)) + length(trim(w-lo.layout)) gt 30 then do:
              substr(w-lo.layout,length(trim(w-lo.layout)),1) = "".
              create w-lo.
            end.
            w-lo.layout = trim(w-lo.layout + " " + trim(v-fup) + ",").
            if last(b-eb.est-no) then
              substr(w-lo.layout,length(trim(w-lo.layout)),1) = "".
          end.
          
          find first wrk-die where wrk-die.die-no eq eb.die-no no-error.
          if not avail wrk-die and eb.die-no gt "" then do:
            create wrk-die.
            assign wrk-die.die-no = eb.die-no
                   wrk-die.cad-no = eb.cad-no
              wrk-die.form-no = eb.form-no
              wrk-die.die-size = string(ef.trim-w) + "x" +
              string(ef.trim-l).
          end.

          /** BUILD INK WORK FILE **/
          for each job-mat
              where job-mat.company eq cocode
                and job-mat.job     eq job-hdr.job
                and job-mat.frm     eq eb.form-no
              no-lock,
              first item
              {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no
              no-lock:

              FIND FIRST reftable
                  WHERE reftable EQ "ce/v-est3.w Unit#"
                    AND reftable.company EQ b-eb.company
                    AND reftable.loc     EQ eb.est-no
                    AND reftable.code    EQ STRING(eb.form-no,"9999999999")
                    AND reftable.code2   EQ STRING(eb.blank-no,"9999999999")
                  NO-LOCK NO-ERROR.

              FIND FIRST b-rt
                  WHERE b-rt.reftable EQ "ce/v-est3.w Unit#1"
                    AND b-rt.company  EQ b-eb.company
                    AND b-rt.loc      EQ eb.est-no
                    AND b-rt.code     EQ STRING(eb.form-no,"9999999999")
                    AND b-rt.code2    EQ STRING(eb.blank-no,"9999999999")
                  NO-LOCK NO-ERROR.

            do i = 1 to 20:
              v-unit = IF i LE 12 AND AVAIL reftable THEN reftable.val[i]
                       ELSE IF i > 12 AND AVAIL b-rt THEN b-rt.val[i - 12]
                       ELSE 0.

              if eb.i-code2[i] eq job-mat.i-no then do:
                find first wrk-ink
                    where wrk-ink.i-code   eq eb.i-code2[i]
                      and wrk-ink.form-no  eq eb.form-no
                      and wrk-ink.blank-no eq eb.blank-no
                      AND wrk-ink.i-pass   EQ eb.i-ps2[i]
                      AND wrk-ink.i-unit   EQ v-unit
                    no-error.

                if not avail wrk-ink then do:
                  create wrk-ink.
                  assign
                   wrk-ink.i-code   = eb.i-code2[i]
                   wrk-ink.form-no  = eb.form-no
                   wrk-ink.blank-no = eb.blank-no
                   wrk-ink.i-dscr   = eb.i-dscr2[i]
                   wrk-ink.i-pass   = eb.i-ps2[i]
                   wrk-ink.i-unit   = v-unit.
                end.
              end.
            end. /* loop i */

            find first wrk-ink
                where wrk-ink.i-code    eq job-mat.i-no
                  and wrk-ink.form-no   eq job-mat.frm
                  and (wrk-ink.blank-no eq job-mat.blank-no or
                       est.est-type     eq 4)
                no-error.

            if not avail wrk-ink                              and
               (job-mat.blank-no  eq eb.blank-no or
                (job-mat.blank-no eq 0 and eb.blank-no eq 1)) then do:
              create wrk-ink.
              assign
               wrk-ink.i-code   = job-mat.i-no
               wrk-ink.form-no  = eb.form-no
               wrk-ink.blank-no = eb.blank-no
               wrk-ink.i-dscr   = item.est-dscr
               wrk-ink.i-unit = 0 
               wrk-ink.i-pass   = 1.
            end.

            if avail wrk-ink then wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.
          end. /* JOB-MAT */

          if eb.est-type eq 4 then v-fac = eb.yld-qty / v-est-qty.
          
        /*  if last-of(eb.form-no) then do: */
            find first style
                where style.company eq eb.company
                  and style.style   eq eb.style
                no-lock no-error.
            if avail style then v-stypart = style.dscr.
            assign
             v-dsc[1] = eb.part-dscr1
             v-dsc[2] = eb.part-dscr2
             v-size[1] = string(eb.len) + "x" + string(eb.wid) + "x" +
                         string(eb.dep)
             v-size[2] = eb.i-coldscr.

             IF eb.blank-no > 0 AND eb.blank-no < 11 THEN 
                 ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
                                                             
            /*if v-first then*/
            /*v-upc-lbl = IF s-prt-sellprc THEN "Sell Price" ELSE "   UPC#". */
            v-upc-lbl = "    QC#".
            
              
            v-job-qty = 0.
            v-stock-no = IF est.est-type = 2 THEN job-hdr.i-no ELSE eb.stock.
            for each xjob-hdr where xjob-hdr.company eq cocode
               and xjob-hdr.job     eq job-hdr.job
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.job-no2 eq job-hdr.job-no2
               and xjob-hdr.i-no    eq v-stock-no
               AND xjob-hdr.frm = eb.form-no no-lock:  
               v-job-qty = v-job-qty + xjob-hdr.qty.
            end.
            /** PRINT ITEM **/
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq v-stock-no /* job-hdr.i-no */
                no-lock no-error.

            if avail oe-ordl then do:
              v-est-qty = oe-ordl.qty.
              find first oe-ord of oe-ordl no-lock.
              v-ovund = string("Overrun/Underrun %:  " +
                               trim(string(oe-ordl.over-pct,">>9.99")) + "/" +
                               trim(string(oe-ordl.under-pct,">>9.99"))).
            end.
            else v-est-qty = v-job-qty.
            
            release w-lo.
            find first w-lo no-error.
            v-case-size = string(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
                         string(eb.cas-dep).
            v-up = eb.num-up.
            v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".
            v-upc-no = eb.spc-no.
            FIND FIRST tt-sum WHERE tt-sum.form-no = eb.form-no
                                AND tt-sum.blank-no = eb.blank-no NO-ERROR.
            IF NOT AVAIL tt-sum THEN DO:
               CREATE tt-sum.
               ASSIGN tt-sum.form-no = eb.form-no
                   tt-sum.blank-no = eb.blank-no
                   tt-sum.dscr = v-dsc[1]
                   tt-sum.job-qty = v-job-qty
                   tt-sum.po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE ""
                   tt-sum.i-no = eb.stock-no
                   tt-sum.style = eb.style
                   tt-sum.SIZE = v-size[1]
                   tt-sum.cas-cnt = eb.cas-cnt
                   tt-sum.cas-no = eb.cas-no
                   tt-sum.cas-cost = eb.cas-cost
                   tt-sum.cas-size = v-case-size
                   tt-sum.tr-no = eb.tr-no
                   tt-sum.tr-cnt = eb.tr-cnt
                   tt-sum.tr-cost = eb.tr-cost
                   tt-sum.tr-size = string(eb.tr-len) + "x" + string(eb.tr-wid) /*( + "x" +
                                    string(eb.tr-dep)*/
                   tt-sum.cad-no = eb.cad-no
                   tt-sum.part-no = eb.part-no
                   tt-sum.eb-recid = RECID(eb)
                       .        
            END.
            v-itm-printed = v-itm-printed + 1.    

            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".
          
          IF LAST-OF(eb.form-no) THEN DO:        
             /* Number of sheets Single board - oe/rep/ticket1.p, multi ticket2.p */
             IF eb.form-no <> lv-last-form-no THEN
                run oe/rep/ticket2.p (recid(ef), recid(job-hdr)).
             lv-last-form-no = eb.form-no.
          END. /* last-of(eb.form-no) */
          
        end. /* each eb */
      end. /* each ef */
      end. /* first job-no */
      
      /** PRINT MULT COPIES OF TICKETS **/
      save_id = recid(job-hdr).
      if last-of(job-hdr.job-no2) then do:
         PUT /*"<R-1>" "Case" AT 97 SKIP */
             "<C3><B><U>Summary of Items on Order:</B></U>"  SKIP      
             "<B><C3>F/B  Description                 Form Qty ".
             IF ipl-part-no THEN 
                 PUT  "Part #         ".
             ELSE PUT "PO#            ".
             PUT "FG Item#     Style   Size       CAD#</B>" SKIP.

           IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     '"'  "Summary of Items on Order:"  '",' SKIP
                     '"' "F/B"    '",'
                     '"' "Description"    '",'
                     '"' "Form Qty"    '",'
                     '"' "Part #"    '",'
                     '"' "PO#"    '",'
                     '"' "FG Item#"    '",'
                     '"' "Style"    '",'
                     '"' "Size"    '",'
                     '"' "CAD#"    '",'  SKIP.
           END.

         v-job-qty-tot = 0.
         FOR EACH tt-sum :
             PUT "<C3>" trim(string(tt-sum.form-no,">9")) + "-" +
                    trim(string(tt-sum.blank-no,">9")) FORM "x(5)"
                 tt-sum.dscr FORM "x(25)"
                 tt-sum.job-qty format "->>,>>>,>>9" SPACE(1).
             IF ipl-part-no THEN
                 PUT tt-sum.part-no FORMAT "X(15)".
             ELSE PUT tt-sum.po-no FORMAT "X(15)".
             PUT
                 tt-sum.i-no
                    tt-sum.style 
                    tt-sum.size FORM "x(10)" SPACE(1)
                    tt-sum.cad-no FORM "X(15)"
                  skip.
              v-job-qty-tot = v-job-qty-tot + tt-sum.job-qty.

              IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     '"' string(tt-sum.form-no,">9") + "-" + string(tt-sum.blank-no,">9")  '",'
                     '"' tt-sum.dscr    '",'
                     '"' STRING(tt-sum.job-qty)   '",'
                     '"' tt-sum.part-no   '",'
                     '"' tt-sum.po-no    '",'
                     '"' tt-sum.i-no    '",'
                     '"' tt-sum.style     '",'
                     '"' tt-sum.size    '",'
                     '"' tt-sum.cad-no    '",'  SKIP.
              END.

         END.
         PUT "Total Job Qty:" AT 16 v-job-qty-tot AT 34 SKIP
              v-fill AT 3 SKIP .
         IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     '"' "Total Job Qty:" '",'
                     '"' v-job-qty-tot    '",' SKIP(2).
              END.

        PUT  "<C30><B><U>Material Requirment</B></U>" SKIP.
          /* Paper*/
         PUT "<C3><P9><B><U>Paper:</U>" SKIP
             "<C3>Board Code Sheet Size        RM Item#    Tot Shts   Tot Tons   Total LF    On Hand     Avail   Order Amt</B>"
             SKIP.

         IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     ',,'
                     '"'  "Material Requirment"  '",' SKIP
                     '"' "Paper:"    '",' SKIP
                     '"' "Board Code"    '",'
                     '"' "Sheet Size"    '",'
                     '"' "RM Item#"    '",'
                     '"' "Tot Shts"    '",'
                     '"' "Tot Tons"    '",'
                     '"' "Total LF"    '",'
                     '"' "On Hand"    '",'
                     '"' "Avail"    '",'  
                     '"' "Order Amt"    '",'  SKIP.
           END.

         FOR EACH wrk-sheet BREAK BY wrk-sheet.i-no /*BY wrk-sheet.form-no*/
                                  BY wrk-sheet.sh-wid
                                  BY wrk-sheet.sh-len :
              IF FIRST-OF(wrk-sheet.sh-len) THEN ASSIGN ld-tot-ton = 0
                                                      ld-tot-lf = 0 
                                                      ld-tot-qty = 0.
              
              FIND FIRST ITEM WHERE ITEM.company = job-hdr.company
                                AND ITEM.i-no = wrk-sheet.i-no NO-LOCK NO-ERROR.
              FIND first job-mat where job-mat.company eq job-hdr.company
                                   and job-mat.job     eq job-hdr.job
                                   AND job-mat.job-no = job-hdr.job-no
                                   AND job-mat.job-no2 = job-hdr.job-no2
                                   and job-mat.frm     eq wrk-sheet.Form-no
                                   AND job-mat.i-no = wrk-sheet.i-no NO-LOCK NO-ERROR.
              IF job-mat.qty-uom = "TON" THEN ld-qty-ton = job-mat.qty.
              ELSE RUN sys/ref/convquom.p (job-mat.qty-uom, "TON",
                                      job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                      job-mat.qty, OUTPUT ld-qty-ton).
              IF job-mat.qty-uom = "LF" THEN ld-qty-lf = job-mat.qty.
              ELSE RUN sys/ref/convquom.p (job-mat.qty-uom, "LF",
                                      job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                      job-mat.qty, OUTPUT ld-qty-lf).
              
              ASSIGN
              ld-tot-ton = ld-tot-ton + ld-qty-ton
              ld-tot-lf = ld-tot-lf + ld-qty-lf
              ld-tot-qty = ld-tot-qty + wrk-sheet.gsh-qty.
              
              IF LAST-OF(wrk-sheet.sh-len) THEN DO:
                 ASSIGN
                  ld-tot-avail = item.q-avail
                  ld-tot-onh   = item.q-onh.

                 IF ITEM.cons-uom NE "LF" THEN DO:
                   RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                           job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                           ld-tot-avail, OUTPUT ld-tot-avail).
                   RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                           job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                           ld-tot-onh, OUTPUT ld-tot-onh).
                 END.
                 ld-cost-amt = ld-tot-lf - ld-tot-avail.
                 IF ld-cost-amt < 0 THEN ld-cost-amt = 0.
                 display wrk-sheet.brd-dscr AT 4 FORM "x(10)"
                      string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len) format "x(16)"
                      wrk-sheet.i-no 
                     ld-tot-qty FORM "->,>>>,>>9"
                 /*   string(ef.nsh-wid) + "x" + string(ef.nsh-len) FORM "x(13)" 
                    string(ef.trim-w) + "x" + string(ef.trim-l) FORM "x(16)"
                    v-board-po  
                    v-vend    
                    eb.die-no 
                    eb.cad-no */
                    ld-tot-ton 
                    ld-tot-lf
                    ld-tot-onh  FORM "->,>>>,>>9"
                    ld-tot-avail /*ITEM.q-onh  item.q-avail*/
                    ld-cost-amt
                    with DOWN stream-io width 170 no-labels no-box frame sheet.
                 DOWN WITH FRAME sheet.  

                IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     '"' wrk-sheet.brd-dscr '",'
                     '"' string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len)    '",'
                     '"' wrk-sheet.i-no  '",'
                     '"' ld-tot-qty   '",'
                     '"' ld-tot-ton   '",'
                     '"' ld-tot-lf   '",'
                     '"' ld-tot-onh   '",'
                     '"' ld-tot-avail   '",'
                     '"' ld-cost-amt   '",'  SKIP.
                END.

              END.
         END.
         PUT skip(1)
              v-sign-line FORM "x(100)" SKIP
              v-sign-label FORM "x(100)" SKIP
              v-fill AT 1 SKIP.
                                   
         IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     SKIP(1)
                     '"' "Date Ordered" '",,'
                     '"' "Supplier"   '",,'
                     '"' "PO No."   '",,'
                     '"' "Amt Ordered"   '",,'
                     '"' "Due Date"   '",'
                       SKIP(2).
         END.

          /*Corrugated*/
         PUT "<C3><B><U>Corrugated:</U>" SKIP
              "<C3>Corrugated Code        Size             Case Count Total Cases     On Hand   Available   Order Amt  </B>"
                 SKIP.

         IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     '"' "Corrugated:"    '",' SKIP
                     '"' "Corrugated Code"    '",'
                     '"' "Size"    '",'
                     '"' "Case Count"    '",'
                     '"' "Total Cases"    '",'
                     '"' "On Hand"    '",'
                    '"' "Available"    '",'
                    '"' "Order Amt"    '",'   SKIP.
           END.

          FOR EACH tt-sum BREAK BY tt-sum.cas-no:
              IF FIRST-OF(tt-sum.cas-no) THEN ASSIGN ld-tot-ton = 0
                                                     ld-tot-lf = 0.
              FIND FIRST ITEM WHERE ITEM.company = job-hdr.company
                                AND ITEM.i-no = tt-sum.cas-no NO-LOCK NO-ERROR.
              FIND first job-mat where job-mat.company eq job-hdr.company
                                   and job-mat.job     eq job-hdr.job
                                   AND job-mat.job-no = job-hdr.job-no
                                   AND job-mat.job-no2 = job-hdr.job-no2
                                   and job-mat.frm     eq tt-sum.form-no
                                   AND job-mat.i-no = tt-sum.cas-no NO-LOCK NO-ERROR.
              IF job-mat.qty-uom = "LF" THEN ld-qty-lf = job-mat.qty.
              ELSE RUN sys/ref/convquom.p (job-mat.qty-uom, "LF",
                                      job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                      job-mat.qty, OUTPUT ld-qty-lf).
              
              ld-tot-lf = ld-tot-lf + ld-qty-lf.

              IF LAST-OF(tt-sum.cas-no) THEN DO:
                 ASSIGN
                  ld-tot-avail = item.q-avail
                  ld-tot-onh   = item.q-onh.

                 IF ITEM.cons-uom NE "LF" THEN DO:
                   RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                           job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                           ld-tot-avail, OUTPUT ld-tot-avail).
                   RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                           job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                           ld-tot-onh, OUTPUT ld-tot-onh).
                 END.
                 ld-cost-amt = ld-tot-lf - ld-tot-avail.
                 IF ld-cost-amt < 0 THEN ld-cost-amt = 0.
                 DISPLAY /*trim(string(tt-sum.form-no,">>9")) + "-" + trim(string(tt-sum.blank-no,">>9")) FORM "x(7)" AT 4
                         */
                      tt-sum.cas-no AT 4 SPACE(9)
                      tt-sum.cas-size FORMAT "x(20)" SPACE(4)
                      tt-sum.cas-cnt SPACE(3)
                      ld-tot-lf    SPACE(2)
                      ld-tot-onh SPACE(2)
                      ld-tot-avail SPACE(2)
                      ld-cost-amt  
                      with stream-io width 100 no-labels no-box frame cases.
                 DOWN WITH FRAME cases.
                 IF v-export THEN DO:
                     PUT STREAM s-temp UNFORMATTED
                         '"' tt-sum.cas-no    '",'
                         '"' tt-sum.cas-size    '",'
                         '"' tt-sum.cas-cnt    '",'
                         '"' ld-tot-lf    '",'
                         '"' ld-tot-onh    '",'
                         '"' ld-tot-avail    '",'
                         '"' ld-cost-amt    '",'   SKIP.
                 END.    END.
          END.
          PUT skip(1)
              v-sign-line FORM "x(100)" SKIP
              v-sign-label FORM "x(100)" SKIP
              v-fill AT 1 SKIP.

          IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     SKIP(1)
                     '"' "Date Ordered" '",,'
                     '"' "Supplier"   '",,'
                     '"' "PO No."   '",,'
                     '"' "Amt Ordered"   '",,'
                     '"' "Due Date"   '",'
                       SKIP(2).
         END.

          /* film*/
          PUT "<C3><B><U>Window Film:</U>" SKIP
              "<C3>Film Code        Size                    Total LF      On Hand     Available    Order Amt</B>"
                 SKIP.
           IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     '"' "Window Film:"    '",' SKIP
                     '"' "Film Code"    '",'
                     '"' "Size"    '",'
                     '"' "Total LF"    '",'
                     '"' "On Hand"    '",'
                     '"' "Available"    '",'
                     '"' "Order Amt"    '",'  SKIP.
           END.
          FOR EACH wrk-film BREAK BY wrk-film.leaf:
              IF FIRST-OF(wrk-film.leaf) THEN ASSIGN ld-tot-ton = 0
                                                     ld-tot-lf = 0.
              
              FIND FIRST ITEM WHERE ITEM.company = job-hdr.company
                                AND ITEM.i-no = wrk-film.leaf NO-LOCK NO-ERROR.
              FIND first job-mat where job-mat.company eq job-hdr.company
                                   and job-mat.job     eq job-hdr.job
                                   AND job-mat.job-no = job-hdr.job-no
                                   AND job-mat.job-no2 = job-hdr.job-no2
                                   and job-mat.frm     eq wrk-film.snum
                                   AND job-mat.i-no = wrk-film.leaf NO-LOCK NO-ERROR.
              IF wrk-film.leaf-l = 0 AND wrk-film.leaf-w = 0 THEN
                 ASSIGN v-film-size = string(job-mat.len) + "x" + string(wrk-film.leaf-w)
                        v-job-len = job-mat.len + 1
                        v-job-wid = job-mat.wid + 1.
              ELSE ASSIGN v-film-size = string(wrk-film.leaf-l) + "x" + string(wrk-film.leaf-w)
                          v-job-len = wrk-film.leaf-l + 1
                          v-job-wid = wrk-film.leaf-w + 1.

              v-basis-w = IF job-mat.basis-w <> 0 THEN job-mat.basis-w ELSE (144000 / ITEM.sqin-lb).
              IF job-mat.qty-uom = "LF" THEN ld-qty-lf = job-mat.qty.
              ELSE RUN sys/ref/convquom.p (job-mat.qty-uom, "LF",
                                      v-basis-w, v-job-len, v-job-wid, 0,
                                      job-mat.qty, OUTPUT ld-qty-lf).
        
              ld-tot-ton = ld-tot-ton + job-mat.qty.
              ld-tot-lf = ld-tot-lf + ld-qty-lf.

              IF LAST-OF(wrk-film.leaf) THEN DO:
                 ASSIGN
                  ld-tot-avail = item.q-avail
                  ld-tot-onh   = item.q-onh.

                 IF ITEM.cons-uom NE "LF" THEN DO:
                   RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                           v-basis-w, v-job-len, v-job-wid, 0,
                                           ld-tot-avail, OUTPUT ld-tot-avail).
                   RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                           v-basis-w, v-job-len, v-job-wid, 0,
                                           ld-tot-onh, OUTPUT ld-tot-onh).
                 END.
                 /* override total lf from total blank qty -
                    may not work if more than one window film exists */
                 ld-tot-lf = v-job-qty-tot * v-job-len / 12.

                 ld-cost-amt = ld-tot-lf - ld-tot-avail.
                 IF ld-cost-amt < 0 THEN ld-cost-amt = 0.
                 DISPLAY /*trim(string(wrk-film.snum,">>9")) + "-" + trim(string(wrk-film.bnum,">>9")) FORM "x(7)" AT 4
                 */
                      wrk-film.leaf    AT 4 SPACE(5)
                      v-film-size FORM "x(20)" SPACE(3)
                      ld-tot-lf  SPACE(3)
                      ld-tot-onh SPACE(3)
                      ld-tot-avail SPACE(3)
                      ld-cost-amt  SPACE(3)
                      /*ld-tot-ton*/
                      with stream-io width 100 no-labels no-box frame films.
                 DOWN WITH FRAME films.
                  IF v-export THEN DO:
                     PUT STREAM s-temp UNFORMATTED
                         '"' wrk-film.leaf    '",'
                         '"' v-film-size    '",'
                         '"' ld-tot-lf    '",'
                         '"' ld-tot-onh    '",'
                         '"' ld-tot-avail    '",'
                         '"' ld-cost-amt    '",'   SKIP.
                 END.
              END.
          END.
          PUT skip(1)
              v-sign-line FORM "x(100)" SKIP
              v-sign-label FORM "x(100)" SKIP
              v-fill AT 1 SKIP.
          IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     SKIP(1)
                     '"' "Date Ordered" '",,'
                     '"' "Supplier"   '",,'
                     '"' "PO No."   '",,'
                     '"' "Amt Ordered"   '",,'
                     '"' "Due Date"   '",'
                       SKIP(2).
         END.
          /* pallets */
          PUT "<C3><B><U>Pallets:</U>" SKIP
              "<C3>Pallet Code       Size                Total Number     On Hand    Available     Order Amt</B>"
                 SKIP.
          IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     '"' "Pallets:"    '",' SKIP
                     '"' "Pallets Code"    '",'
                     '"' "Size"    '",'
                     '"' "Total Number"    '",'
                     '"' "On Hand"    '",'
                     '"' "Available"    '",'
                     '"' "Order Amt"    '",'  SKIP.
           END.
          FOR EACH tt-sum BREAK BY tt-sum.tr-no:
              IF FIRST-OF(tt-sum.tr-no) THEN ASSIGN ld-tot-ton = 0
                                                    ld-tot-lf = 0.
              FIND FIRST ITEM WHERE ITEM.company = job-hdr.company
                                AND ITEM.i-no = tt-sum.tr-no NO-LOCK NO-ERROR.
              FIND first job-mat where job-mat.company eq job-hdr.company
                                   and job-mat.job     eq job-hdr.job
                                   AND job-mat.job-no = job-hdr.job-no
                                   AND job-mat.job-no2 = job-hdr.job-no2
                                   and job-mat.frm     eq job-hdr.frm
                                   AND job-mat.i-no = tt-sum.tr-no NO-LOCK NO-ERROR.
              
              IF job-mat.qty-uom = "LF" THEN ld-qty-lf = job-mat.qty.
              ELSE RUN sys/ref/convquom.p (job-mat.qty-uom, "LF",
                                      job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                      job-mat.qty, OUTPUT ld-qty-lf).
              
              ld-tot-lf = ld-tot-lf + ld-qty-lf.

              IF LAST-OF(tt-sum.tr-no) THEN DO:
                 ASSIGN
                  ld-tot-avail = item.q-avail
                  ld-tot-onh   = item.q-onh.

                 IF ITEM.cons-uom NE "LF" THEN DO:
                   RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                           job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                           ld-tot-avail, OUTPUT ld-tot-avail).
                   RUN sys/ref/convquom.p (item.cons-uom, "LF",
                                           job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                                           ld-tot-onh, OUTPUT ld-tot-onh).
                 END.
                 ld-cost-amt = ld-tot-lf - ld-tot-avail.
                 IF ld-cost-amt < 0 THEN ld-cost-amt = 0.
                 DISPLAY /*trim(string(tt-sum.form-no,">>9")) + "-" + trim(string(tt-sum.blank-no,">>9")) FORM "x(7)" AT 4*/
                      tt-sum.tr-no AT 4 SPACE(5)
                      tt-sum.tr-size FORMAT "x(20)" SPACE(3)
                      ld-tot-lf   SPACE(3)
                      ld-tot-onh SPACE(3) 
                      ld-tot-avail SPACE(3)
                      ld-cost-amt  SPACE(3)
                      /*tt-sum.tr-cnt        */
                      with stream-io width 100 no-labels no-box frame pallets.
                 DOWN WITH FRAME pallets.
                 IF v-export THEN DO:
                     PUT STREAM s-temp UNFORMATTED
                         '"' tt-sum.tr-no    '",'
                         '"' tt-sum.tr-size    '",'
                         '"' ld-tot-lf    '",'
                         '"' ld-tot-onh    '",'
                         '"' ld-tot-avail    '",'
                         '"' ld-cost-amt    '",'   SKIP.
                 END.
              END.
          END.
          PUT skip(1)
              v-sign-line FORM "x(100)" SKIP
              v-sign-label FORM "x(100)" SKIP
              .
          IF v-export THEN DO:
                 PUT STREAM s-temp UNFORMATTED
                     SKIP(1)
                     '"' "Date Ordered" '",,'
                     '"' "Supplier"   '",,'
                     '"' "PO No."   '",,'
                     '"' "Amt Ordered"   '",,'
                     '"' "Due Date"   '",'
                       SKIP(4).
         END.

        for each wrk-op:
          delete wrk-op.
        end.
        for each wrk-prep:
          delete wrk-prep.
        end.
        lv-pg-num = PAGE-NUM .
      

      for each wrk-spec:
        delete wrk-spec.
      end.
      for each wrk-film:
        delete wrk-film.
      end.
      for each wrk-die:
        delete wrk-die.
      end.
      for each wrk-sheet:
        delete wrk-sheet.
      end.
      for each wrk-misc:
        delete wrk-misc.
      end.
      for each wrk-inst:
        delete wrk-inst.
      end.
      FOR EACH tt-sum:
          DELETE tt-sum.
      END.
      
      v-first = no.
      PAGE.
      END. /* last-of(job-no2) */

    end. /* each job-hdr */
    
/* end ---------------------------------- copr. 1994  advanced software, inc. */
