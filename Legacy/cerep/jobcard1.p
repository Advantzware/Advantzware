/* cerep/jobacc.p                                                             */
/*--------------------------------------------------------------------------- */

def input parameter v-format like sys-ctrl.char-fld.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}

def new shared var save_id as recid.
def new shared var v-today as date init today.
def new shared var v-job as char format "x(6)" extent 2 init [" ","zzzzzz"].
def new shared var v-job2 as int format "99" extent 2 init [00,99].
def new shared var v-stypart like style.dscr.
def new shared var v-dsc like oe-ordl.part-dscr1 extent 2.
def new shared var v-size as char format "x(26)" extent 2.
def new shared var v-bld-job like oe-ord.job-no.
def new shared var v-bld-job2 like oe-ord.job-no2.
def new shared var v-fill as char format "x(128)".
def new shared var v-frst as log.
def new shared var v-ok as log.
def new shared var v-est-qty as int format "->>,>>>,>>9".
def new shared var v-job-qty as int format "->>,>>>,>>9".
def new shared var v-fac as DEC .
def new shared var v-job-no like oe-ordl.job-no.
def new shared var v-job-no2 like oe-ordl.job-no2.
def new shared var v-due-date like oe-ord.due-date.
def new shared var v-prod-date like oe-ord.due-date.
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
DEF VAR v-fill2 AS cha INIT "-" FORM "x(125)" NO-UNDO.
DEF VAR v-spoil LIKE job-mch.wst-prct NO-UNDO.
DEF VAR v-unit AS INT NO-UNDO.
DEF VAR v-spec-cnt AS INT NO-UNDO.
DEF VAR v-prod-code AS cha NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-label-item-no AS INT NO-UNDO.

def TEMP-TABLE w-lo NO-UNDO
  field layout like v-layout.

def new shared buffer xjob-hdr for job-hdr.

def buffer b-eb for eb.
def buffer b-ef for ef.
def buffer v-eb for eb.
def buffer v-ef for ef.
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
  
form header
     skip(1)
     "07/22/02 Job Ticket QF-130"   to 132
    with no-box no-attr-space frame bott page-bottom stream-io width 132.
     
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 20 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 20 NO-UNDO.
DEF VAR v-spec-inst AS cha FORM "x(80)" EXTENT 10 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.

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
DEF VAR v-ink30 AS cha EXTENT 20 NO-UNDO.
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
DEF VAR v-cust-name-extent AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship1-extent AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship2-extent AS CHAR EXTENT 10 NO-UNDO.
DEF VAR v-ship4-extent AS CHAR EXTENT 10 NO-UNDO.
DEF BUFFER b-est FOR est.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-shipto FOR shipto.
DEF BUFFER b-cust FOR cust.
DEF VAR vs-len AS cha NO-UNDO.
DEF VAR vs-wid AS cha NO-UNDO.
DEF VAR vs-dep AS cha NO-UNDO.


DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF TEMP-TABLE tt-fgitm FIELD i-no AS cha FORM "x(15)"
                        FIELD seq AS INT
                        FIELD qty AS INT 
                        FIELD i-dscr AS cha
                        FIELD po-no AS cha
                        FIELD part-no AS cha 
                        FIELD cas-cnt LIKE eb.cas-cnt
                        FIELD cas-pal LIKE eb.cas-pal
                        FIELD cust-name AS cha
                        FIELD shipto1 AS CHAR
                        FIELD shipto2 AS CHAR
                        FIELD shipto4 AS CHAR.
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
DEF VAR v-stock-no LIKE eb.stock NO-UNDO.
DEF VAR v-net-size AS cha NO-UNDO.
DEF VAR v-sht-size AS cha NO-UNDO.
DEF VAR v-die-size AS cha NO-UNDO.
DEF VAR v-cat AS cha NO-UNDO.
DEF VAR v-i-qty AS INT NO-UNDO.
DEF VAR v-cas-no AS cha NO-UNDO.
DEF VAR v-job-cnt AS INT NO-UNDO.
DEF VAR v-item-name LIKE itemfg.i-name NO-UNDO.
DEF BUFFER bf-ink FOR wrk-ink.
DEF BUFFER bf-jobhdr FOR job-hdr.
DEF BUFFER bf-item FOR ITEM.
DEF BUFFER bf-jobmat FOR job-mat.
v-fill = /*fill("=",128)*/ "<||3><C1><FROM><C108><LINE><||3>".

def new shared frame head.

DEF SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF VAR v-po-duedate LIKE po-ordl.due-date NO-UNDO.
DEF VAR v-upc-no AS cha NO-UNDO.
DEF VAR v-upc-lbl AS cha FORM "x(10)" NO-UNDO.
DEF VAR v-shipto1 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shipto2 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-pg-num2 AS INT INIT 1 NO-UNDO.
DEF VAR lv-tot-pg AS INT INIT 1 NO-UNDO.
DEF BUFFER x-hdr FOR job-hdr.
DEF VAR lv-prt-sts AS cha NO-UNDO.
DEF VAR lv-prt-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR lv-prt-time AS cha NO-UNDO.
DEF VAR lv-cad-image AS cha NO-UNDO.
DEF VAR lv-cad-image-list AS cha NO-UNDO.
DEFINE VARIABLE idx AS INTEGER NO-UNDO.
DEF VAR v-output AS INT FORM ">,>>>,>>9" NO-UNDO.
DEF TEMP-TABLE tt-reftable NO-UNDO LIKE reftable
    FIELD est-type LIKE est.est-type.

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

format HEADER 
       "<OLANDSCAPE><P10>" skip
       "JOB NUMBER:<B>" v-job-no space(0) "-" space(0) v-job-no2 format "99" "</B>"
       "<B><P12>Carded Graphics, LLC Factory Ticket</B><P10>" at 52  "ORDER DATE:" at 120 v-start-date skip
       v-fill
    with no-box frame head no-labels stream-io width 155.

format "Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
       "Salesman:" at 68 oe-ord.sname[1] "Order#:" at 113 oe-ord.ord-no
    with no-box frame line-head no-labels stream-io width 132.
    
{sys/inc/notes.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

ASSIGN
 v-job[1]    = fjob-no
 v-job[2]    = tjob-no
 v-job2[1]   = fjob-no2
 v-job2[2]   = tjob-no2
 v-reprint   = reprint
 v-spec-list = spec-list.


    {cerep/jobacc.i}

        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY job-hdr.frm:       
                                                         /**/
      ASSIGN lv-prt-sts = IF NOT job-hdr.ftick-prnt THEN "ORIGINAL" ELSE "REVISED"
             lv-prt-date = TODAY
             lv-prt-time = STRING(TIME,"hh:mm am").

      find first job
        where job.company eq cocode
          and job.job     eq job-hdr.job
          and job.job-no  eq job-hdr.job-no
          and job.job-no2 eq job-hdr.job-no2
        no-lock no-error.

      IF production AND
         job.cs-trans-date NE ? THEN DO:
         li = 0.
         DO WHILE li LT 1000:
           li = li + 1.
           FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
           IF AVAIL job THEN
             ASSIGN
              job.pr-printed    = YES
              job.pr-user-id-p  = USERID("nosweat")
              job.pr-print-date = TODAY
              job.pr-print-time = TIME
              li                = 1000.
         END.
      END.
      
      ELSE DO:
        li = 0.
        IF NOT job-hdr.ftick-prnt THEN DO WHILE li LT 1000:
          li = li + 1.
          FIND xjob-hdr EXCLUSIVE-LOCK
              WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
              NO-ERROR NO-WAIT.
          IF AVAIL xjob-hdr THEN
            ASSIGN
             xjob-hdr.ftick-prnt = YES
             li                  = 1000.
        END.
      
        li = 0.
        DO WHILE li LT 1000:
          li = li + 1.
          FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF AVAIL job THEN DO:
            li = 1000.
      
            IF NOT job.cs-printed THEN
              ASSIGN
               job.cs-printed    = YES
               job.cs-user-id-p  = USERID("nosweat")
               job.cs-print-date = TODAY
               job.cs-print-time = TIME.
      
            IF approve THEN
              ASSIGN
               job.cs-to-pr      = YES
               job.cs-user-id-t  = USERID("nosweat")
               job.cs-trans-date = TODAY
               job.cs-trans-time = TIME.
          END.
        END.
      END.
      
      FIND CURRENT job NO-LOCK NO-ERROR.
      
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
         ASSIGN
            lv-pg-num2 = 1
            lv-cad-image-list = "".
      END.
      if first-of(job-hdr.frm) then ASSIGN v-first = yes.

      /** PRINT JOB HEADER **/
      if v-first then do:
        assign
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2.

        if avail oe-ord then
          if not oe-ctrl.p-fact and oe-ord.stat eq "H" then next.

        ASSIGN
           v-due-date = if avail oe-ord then oe-ord.due-date else ?
           v-start-date = IF AVAIL oe-ord THEN oe-ord.ord-date ELSE job-hdr.start-date
           v-prod-date  = if avail oe-ord then oe-ord.prod-date else ? .

        if not first(job-hdr.job-no) then page.
        view frame head.
       
        v-shipto = "".

        FOR EACH wrk-ink:
            DELETE wrk-ink.
        END.

        find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq job-hdr.i-no
                no-lock no-error.
        IF AVAIL oe-ordl THEN
        find first oe-rel
            where oe-rel.company eq cocode
              and oe-rel.ord-no  eq oe-ordl.ord-no
              and oe-rel.i-no    eq oe-ordl.i-no
              and oe-rel.line    eq oe-ordl.line
            no-lock no-error.
        if avail oe-rel then do:
           find first shipto
              where shipto.company eq cocode
                and shipto.cust-no eq oe-rel.cust-no
                and shipto.ship-id eq oe-rel.ship-id
              no-lock no-error.  
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
        ASSIGN
          v-bar-no = IF AVAIL eb THEN eb.spc-no ELSE trim(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99")
          v-bar-no = barCode(v-bar-no)
          v-label-item-no = 1.

          PUT "  <B>CUSTOMER NAME:<P18><R-0.3>" v-cust-name  "<P10><R+0.3>"
            "<B>DUE DATE:         ESTIMATE:    LAYOUT:           CREATED BY: " AT 65 SKIP
            "  SHIPTO:</B>" v-shipto[1]  v-due-date AT 69  job-hdr.est-no FORMAT "x(8)" AT 85  eb.die-no FORM "x(15)" AT 100   job.USER-ID AT 119
            SKIP
            v-shipto[2] AT 10             "PROD DATE:" AT 65  SKIP      
            v-shipto[4] AT 10              v-prod-date AT 69    
            "   Page#:" AT 107 string(lv-pg-num2,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" AT 116 SKIP
            /*v-shipto[4] AT 10 " <B>SHIP VIA:</B>" v-shipvia FORMAT "x(25)"*/   /*"<B>OVERRUN:</B> " AT 87  v-over% "   <B>UNDERRUN:</B> "  v-under% SKIP*/
             v-fill  .   

        /*PUT "<R-1><#1><C91>Date/Time Generated:" SKIP
            "<B>CUSTOMER NAME:</B>" v-cust-name "<B> DUE DATE:     ESTIMATE:" lv-prt-date AT 119 space(1) lv-prt-time SKIP
            "SHIPTO:</B>" v-shipto[1] /*v-req-date AT 49*/ v-due-date AT 49 trim(job-hdr.est-no) FORM "x(8)" AT 66
            "<C91.7>Status" SKIP
            v-shipto[2] AT 7 "<C91.7>" lv-prt-sts SKIP
            v-shipto[4] AT 7 SKIP
            v-fill SKIP.   */  
        /* barcode print */
       /* PUT UNFORMATTED "<UNITS=INCHES><AT=.54,7><FROM><AT=+.6,+2><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
        /*    trim(job-hdr.job-no) "-" STRING(job-hdr.job-no2,"99") ">"
            "<AT=,8.8>" trim(job-hdr.job-no) "-" STRING(job-hdr.job-no2,"99") */
              v-bar-no ">" "   Page#:" string(lv-pg-num2,">>9") + " of " + string(lv-tot-pg) FORM "x(20)"
            "<AT=,7.8>" v-bar-no "<=#1><R+5>".*/

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
                AND lookup(item.mat-type,"I,V") > 0 
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
                       ELSE
                       IF AVAIL b-rt                 THEN b-rt.val[i - 12]
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
            v-upc-lbl = " QC#".
       
            IF FIRST-OF(eb.form-no) THEN
              PUT "<C1>  <P10><B>F/B   FG ITEM/PO #    #UP    FORM/JOB QTY  NAME/DESCRIPTION          CARTON SIZE/STYLE        COUNT  CASE/SIZE   " /*v-upc-lbl */ "</B>" SKIP.
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

            ASSIGN
            v-cas-no = ""
            v-job-cnt = 0.
            FOR EACH bf-jobmat NO-LOCK WHERE bf-jobmat.company = eb.company
                                 AND bf-jobmat.job-no = job-hdr.job-no
                                 AND bf-jobmat.job-no2 = job-hdr.job-no2
                                 AND bf-jobmat.frm = eb.form-no
                                 AND (bf-jobmat.blank-no = eb.blank-no OR bf-jobmat.blank-no = 0)
                        AND CAN-FIND(FIRST ITEM WHERE item.company = bf-jobmat.company
                                                  AND ITEM.i-no = bf-jobmat.i-no 
                                                  AND ITEM.mat-type = "C")
                       BY bf-jobmat.j-no DESCENDING BY bf-jobmat.blank-no DESCENDING:
                ASSIGN
                v-cas-no = bf-jobmat.i-no
                v-job-cnt = bf-jobmat.qty.
                LEAVE.
            END.           
            
            release w-lo.
            find first w-lo no-error.
            v-case-size = string(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
                         string(eb.cas-dep).
            v-up = eb.num-up.
            v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".
            v-upc-no = eb.spc-no.

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

            FIND FIRST itemfg WHERE itemfg.company = job-hdr.company
                            AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
             v-item-name = IF AVAIL itemfg THEN ITEMfg.i-name ELSE v-dsc[1].

            v-prod-code = ENTRY(INDEX("ONR",
                                      IF AVAIL oe-ordl THEN oe-ordl.type-code
                                                       ELSE itemfg.type-code),
                                "NEW,NEW,REPEAT") NO-ERROR.
            IF ERROR-STATUS:ERROR THEN v-prod-code = "".

            display " " trim(string(eb.form-no,">>9")) + "-" +
                    trim(string(eb.blank-no,">>9")) FORM "x(5)" 
                    eb.stock-no @ job-hdr.i-no                     
                    v-up FORM ">9"
                    v-job-qty * v-fac format "->>,>>>,>>9" SPACE(7)
                    v-item-name FORM "x(25)"
                    v-size[1] FORM "x(22)"
                    /*eb.style FORM "x(10)"*/
                    eb.cas-cnt  
                    oe-ordl.cas-cnt WHEN AVAIL oe-ordl AND oe-ordl.cas-cnt <> 0 @ eb.cas-cnt format "->>>>>9" SPACE(2)
                    eb.cas-no
                    /*v-cas-no WHEN v-cas-no <> "" @ eb.cas-no
                    v-case-due-date*/
                    skip
                    v-po-no FORM "x(15)" AT 9
                    v-est-qty FORM  "->>,>>>,>>9"         AT 28 SPACE(7)
                    v-dsc[1] FORM "x(25)"  
                    /*"PREV:"  v-prev-job FORM "x(21)"*/ v-stypart FORM "x(20)"
                    v-job-cnt FORM ">>>>9"  AT 97
                    " " + trim(string(eb.cas-len) + "x" + string(eb.cas-wid) + "x" + string(eb.cas-dep))
                with stream-io width 150 no-labels no-box frame line-det1.
           
            v-itm-printed = v-itm-printed + 1.    

            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".
          /* end. /* last-of(eb.form-no) */      */
          IF LAST-OF(eb.form-no) THEN DO:
             IF v-itm-printed < 4 THEN PUT SKIP(4 - v-itm-printed).

             /* Number of sheets Single board - oe/rep/ticket1.p, multi ticket2.p */
             run oe/rep/ticket2.p (recid(ef), recid(job-hdr)).
             find first wrk-sheet where recid(wrk-sheet) eq save_id NO-ERROR.

             ASSIGN
              v-vend       = ""
              v-board-po   = 0
              v-po-duedate = ?.

             FOR EACH po-ordl NO-LOCK
                 WHERE po-ordl.company EQ job.company
                   AND po-ordl.job-no  EQ job.job-no
                   AND po-ordl.job-no2 EQ job.job-no2
                   AND (po-ordl.s-num  EQ eb.form-no OR
                        po-ordl.s-num  EQ ?)
                   AND po-ordl.item-type
                 USE-INDEX job-no,
                 FIRST item NO-LOCK
                 WHERE item.company EQ po-ordl.company
                   AND item.i-no    EQ po-ordl.i-no
                   AND CAN-DO("1,2,3,4,B,P,R",item.mat-type),
                 FIRST po-ord NO-LOCK
                 WHERE po-ord.company EQ po-ordl.company
                   AND po-ord.po-no   EQ po-ordl.po-no
                 BY po-ordl.po-no:
               LEAVE.
             END.

             IF AVAIL po-ordl THEN
               ASSIGN 
                v-vend       = po-ord.vend-no
                v-board-po   = po-ord.po-no
                v-po-duedate = po-ordl.due-date.
          
             PUT "<P10>" v-fill SKIP                       /*REQ'D*/                 
                 "<B>F#  BOARD CODE                SHEETS    HOUSE SHEET SIZE        MIN SHEET SIZE        CAD #         PO#  DUE DATE    VENDOR </B>"
                 SKIP.
            /** PRINT SHEET **/
                 v-up = v-up + eb.num-up.
            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".
             X = 2.
             for each wrk-sheet  break by wrk-sheet.form-no:
                 ASSIGN vs-len = ""
                        vs-wid = ""
                        vs-dep = "".

                 IF AVAIL po-ord THEN
                   FIND FIRST po-ordl WHERE
                        po-ordl.company EQ po-ord.company AND
                        po-ordl.po-no   EQ po-ord.po-no AND
                        po-ordl.i-no = wrk-sheet.i-no
                        NO-LOCK NO-ERROR.
               v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

                 IF wrk-sheet.sh-len <> 0 THEN RUN sys/inc/dec-frac.p (wrk-sheet.sh-len,32,OUTPUT vs-len).
                 IF wrk-sheet.sh-wid <> 0 THEN RUN sys/inc/dec-frac.p (wrk-sheet.sh-wid,32,OUTPUT vs-wid).
                 v-sht-size = (IF vs-wid <> "" THEN trim(vs-wid) + "x" ELSE "") +
                             trim(vs-len).
                 ASSIGN vs-len = ""
                        vs-wid = ""
                        vs-dep = "".
                 IF ef.trim-l <> 0 THEN RUN sys/inc/dec-frac.p (ef.trim-l,32,OUTPUT vs-len).
                 IF ef.trim-w <> 0 THEN RUN sys/inc/dec-frac.p (ef.trim-w,32,OUTPUT vs-wid).
                 v-die-size = trim(vs-wid) + "x" + trim(vs-len) /* eb.die-no */.
                 FIND FIRST bf-item WHERE bf-item.company = job-hdr.company
                                      AND bf-item.i-no = wrk-sheet.i-no NO-LOCK NO-ERROR.
                 v-cat = IF AVAIL bf-ITEM THEN bf-ITEM.procat ELSE "".
                 /*IF AVAIL po-ord THEN
                    FIND FIRST po-ordl OF po-ord WHERE po-ordl.i-no = wrk-sheet.i-no NO-LOCK NO-ERROR.
                    v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.*/
                 
                 IF AVAIL oe-ord THEN FIND FIRST vend WHERE vend.company = oe-ordl.company
                                                   AND vend.vend-no = oe-ordl.vend-no NO-LOCK NO-ERROR.
                 v-vend = /*IF AVAIL po-ord THEN po-ord.vend-no ELSE "".*/
                         IF AVAIL vend THEN vend.NAME ELSE "".
                 IF v-vend = "" AND AVAIL bf-item THEN DO:
                    FIND FIRST vend WHERE vend.company = oe-ordl.company
                                       AND vend.vend-no = bf-item.vend-no NO-LOCK NO-ERROR.
                    v-vend = IF AVAIL vend THEN vend.NAME
                             ELSE IF AVAIL oe-ordl THEN oe-ordl.vend-no ELSE "".
                 END.
                /* v-po-duedate = IF AVAIL vend THEN vend.due-date ELSE "" .*/
                 v-board-po = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0.

                IF ef.xgrain EQ "S" THEN 
                    ASSIGN v-net-size = string(ef.nsh-len) + "x" + string(ef.nsh-wid).
                 ELSE ASSIGN v-net-size = string(ef.nsh-wid) + "x" + string(ef.nsh-len).
                 
               PUT UNFORMATTED  wrk-sheet.form-no  FORM ">9" " "
                    " " wrk-sheet.brd-dscr  FORM "x(22)" " "
                       /*v-cat FORM "x(5)"*/
                    wrk-sheet.gsh-qty  FORM ">,>>>,>>9" "    "
                    v-sht-size  format "x(16)" "   "
                   /* v-hg*/  "     "
                    v-net-size  FORM "x(16)" "   "
                    " "
                    eb.cad-no  FORM "x(10)" " " 
                    v-board-po FORM ">>>>>>99" "  "
                    v-po-duedate  FORM "99/99/9999" "  " 
                    v-vend   FORM "x(10)" " "         SKIP
                    /*v-die-size FORM "x(16)" AT 78*/
                    SKIP.
                X = 1.
             end. /* each wrk-sheet */
       
             put v-fill at 1 skip.
            /*         
             /** PRINT INK **/
             PUT "<B>UNIT INK             INK NAME                      UNIT  INK             INK NAME                          PLATE #</B>"
                 SKIP.
            */     
             x = 2.
             i = 1.
             v-ink1 = "".
             v-ink2 = "".
             for each wrk-ink WHERE wrk-ink.form-no = eb.form-no
                BREAK /* by wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.i-unit
                      BY wrk-ink.blank-no */
                      BY wrk-ink.i-code
                      BY wrk-ink.i-pass
                      BY wrk-ink.i-unit
                      :
                IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.
                IF FIRST-OF(wrk-ink.i-unit) THEN v-item[i] = "".

                v-item[i] = IF LOOKUP(string(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ",".
                /*IF LAST-OF(wrk-ink.i-pass) THEN DO:
                   IF wrk-ink.i-pass = 1 THEN
                      v-ink1[i] = wrk-ink.i-code + " " + wrk-ink.i-dscr + " " + v-item[i].
                   ELSE IF wrk-ink.i-pass = 2 THEN
                        v-ink2[i] = wrk-ink.i-code + " " + wrk-ink.i-dscr + " " + v-item[i].
                   i = i + 1.
                END. */
                IF first-OF(wrk-ink.i-code) /*OR v-ink-use-per-blank > 1 */ THEN DO: 
                   v-i-qty = 0.
                   
                   FOR EACH bf-ink WHERE bf-ink.form-no = wrk-ink.form-no 
                                     /*AND bf-ink.i-pass = wrk-ink.i-pass*/
                                      AND bf-ink.i-code = wrk-ink.i-code  :
                        
                       v-i-qty = v-i-qty + bf-ink.i-qty.
                   END.
                END.

                IF LAST-OF(wrk-ink.i-unit) THEN DO:
                    IF SUBSTRING(v-item[i],LENGTH(v-item[i]),1) = "," THEN v-item[i] = substring(v-item[i],1,LENGTH(v-item[i]) - 1).                    
                    v-alloc = v-item[i].
                    if num-entries(v-item[i]) gt 1 then do:
                       v-alloc = "".
                       do j = 1 to num-entries(v-item[i]):
                          if j eq 1 or j eq num-entries(v-item[i]) THEN v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                          else do:
                             if int(entry(j,v-item[i])) - int(entry(j - 1,v-item[i])) le 1 then
                                substr(v-alloc,length(trim(v-alloc)),1) = "-".
                             else do:
                                if substr(v-alloc,length(trim(v-alloc)),1) eq "-" then
                                   v-alloc = v-alloc + entry(j - 1,v-item[i]) + ",".

                                v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                             END.
                          end.
                       end.                    
                       if v-alloc ne "" then substr(v-alloc,length(trim(v-alloc)),1) = "".
                       
                    end.
                   
                    IF wrk-ink.i-pass = 1 THEN DO:
                         /*
                       ASSIGN v-ink1[i] = /*STRING(wrk-ink.i-unit,">>>") + "  " +*/  string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") /* + " " + trim(v-alloc) */ /*v-item[i]*/
                                          /*+ (IF i = 1 THEN "  " + eb.plate-no ELSE "") */ */

                        IF wrk-ink.i-unit = i THEN  
                            v-ink1[i] =   string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") + "  " +
                                          STRING(v-i-qty,">>>,>>9")   .
                        ELSE IF wrk-ink.i-unit > 0 AND wrk-ink.i-unit <= 8 THEN v-ink1[int(wrk-ink.i-unit)] = string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") + "  " + STRING(v-i-qty,">>>,>>9") .
                        i = i + 1         . 
                    END.
                    ELSE IF wrk-ink.i-pass = 2 THEN DO:
                    /*   ASSIGN v-ink2[i] = /*STRING(wrk-ink.i-unit,">>>") + "  " +*/ string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") /*+ " " + trim(v-alloc)*/ /*v-item[i]*/ )
                                         */
                        IF wrk-ink.i-unit = i THEN  
                            v-ink2[i] =   string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") + "  " +
                                          STRING(v-i-qty,">>>,>>9") .
                        ELSE IF wrk-ink.i-unit <= 8 THEN v-ink2[int(wrk-ink.i-unit)] = string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") + "  " + STRING(v-i-qty,">>>,>>9").
                              i = i + 1.           
                    END.
                END.
                   

              
                /*delete wrk-ink.*/
             end. /* each wrk-ink */

             /* need to print 8 ink all the time */
             v-num-of-inks = 0.
             DO j = 1 TO 8:
                IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
                IF TRIM(v-ink2[j]) = "-" THEN v-ink2[j] = "".               
                IF v-ink1[j] <> "" THEN v-num-of-inks = v-num-of-inks + 1.
                IF v-ink2[j] <> "" THEN v-num-of-inks = v-num-of-inks + 1.
             END.
             IF v-num-of-inks <= 8 THEN DO j = v-num-of-inks + 1 TO 8:
                IF v-ink1[j] = "" THEN v-ink1[j] = "UNIT".  
                ELSE IF v-ink2[j] = "" THEN v-ink2[j] = "UNIT".  
             END.
             /*======================*/
             ASSIGN v-ink10 = ""
                    v-ink20 = ""
                    v-ink30 = "" .

             /* display top to bottom by column 1 5
                                               2 6 ...*/
             DO j = 5 TO 8 :
                 IF v-ink1[j] <> "" THEN ASSIGN v-ink10[j - 4] = v-ink1[j]
                                                v-ink1[j] = "".
                 IF v-ink2[j] <> "" THEN ASSIGN v-ink20[j - 4] = v-ink2[j]
                                                v-ink2[j] = ""  .
             END.
             v-skip = NO.
             v-plate-printed = NO.
             PUT "LBS" AT 51 "LBS" AT 106 SKIP.
             PUT "<#15><FROM><R+4><C105><RECT><||3>"
                 "<=15><C46><FROM><R+4><C46><LINE><||3>" 
                 "<=15><R+1><C1><FROM><C105><LINE><||3> "
                 "<=15><R+2><C1><FROM><C105><LINE><||3> "
                 "<=15><R+3><C1><FROM><C105><LINE><||3> "
                 " <=15>".
             v-num-of-inks = 0. /* 1 thru 8 */
             DO j = 1 TO 4:
               IF v-ink1[j] <> "" OR v-ink10[j] <> "" THEN DO:
                  v-num-of-inks = v-num-of-inks + 1.
                  IF v-ink1[j] = "UNIT" THEN v-ink1[j] = "".
                  IF v-ink10[j] = "UNIT" THEN v-ink10[j] = "".
                  PUT  "UNIT" j /*v-num-of-inks*/ FORM ">9: "
                       v-ink1[j] FORM "x(45)" 
                       "  UNIT"  j /*v-num-of-inks*/ + 4 FORM ">9: "
                       v-ink10[j] FORM "x(45)" .
                  /*IF j = 1 THEN PUT    "PLATE#:" eb.plate-no /*AT 106*/  SKIP.
                  ELSE*/ PUT  SKIP.
                /*  PUT v-fill AT 1 "<R-1>" SKIP.*/
               END.
             END.
             DO j = 1 TO 4:
               IF (v-ink2[j] <> "" OR v-ink20[j] <> "" ) AND v-num-of-inks < 4 THEN DO:
                  v-num-of-inks = v-num-of-inks + 1.
                  IF v-ink2[j] = "UNIT" THEN v-ink2[j] = "".
                  IF v-ink20[j] = "UNIT" THEN v-ink20[j] = "".
                  PUT  "UNIT" j /*v-num-of-inks*/ FORM ">9: "
                       v-ink2[j] FORM "x(52)" 
                       "  UNIT" j /*v-num-of-inks*/ + 4 FORM ">9: "
                       v-ink20[j] FORM "x(52)" SKIP.
                  /*PUT v-fill AT 1 "<R-1>" SKIP */.
               END.
             END.
             PUT "" SKIP v-fill at 1 skip.
          END. /* last-of(eb.form-no) */
          
        end. /* each eb */
      end. /* each ef */
      end. /* first job-no */

      if last-of(job-hdr.frm) then do:
         PUT "<R-1>".
         IF s-run-speed THEN
            PUT "<B>MACHINE                 MR WASTE   MR HRS  RUN SPEED   SPOIL          INPUT           GOOD SHTS/PCS         OPER INIT/DATE</B>"
                SKIP.
         else
            PUT "<B>MACHINE                 MR WASTE   MR HRS   RUN HOUR    SPOIL          INPUT           GOOD SHTS/PCS         OPER INIT/DATE</B>"
                SKIP.
         FOR EACH wrk-op WHERE wrk-op.s-num = job-hdr.frm BREAK by wrk-op.d-seq by wrk-op.b-num:
             v-mat-for-mach = "".
             IF lookup(wrk-op.dept,lv-mat-dept-list) > 0 THEN DO:
                 
                FOR EACH xjob-mat WHERE xjob-mat.company eq cocode
                                       and xjob-mat.job     eq job-hdr.job
                                       and xjob-mat.job-no  eq job-hdr.job-no
                                       and xjob-mat.job-no2 eq job-hdr.job-no2
                                       AND xjob-mat.frm = job-hdr.frm
                                       /*AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0)*/  NO-LOCK,
                     first ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND
                                      ITEM.mat-type = SUBSTRING(wrk-op.dept,1,1) NO-LOCK :
                     v-mat-for-mach = ITEM.i-name + fill(" ", 33 - LENGTH(ITEM.i-name))  /*"       " */ +
                                      string(xjob-mat.wid) + "x" + STRING(xjob-mat.len) +
                                      "      " + string(xjob-mat.qty).                   
                     LEAVE.                 
                END.                            
             END.     

             IF LAST(wrk-op.d-seq) THEN DO: /* pallet code*/
                FOR EACH xjob-mat WHERE xjob-mat.company eq cocode
                                       and xjob-mat.job     eq job-hdr.job
                                       and xjob-mat.job-no  eq job-hdr.job-no
                                       and xjob-mat.job-no2 eq job-hdr.job-no2
                                       AND xjob-mat.frm = job-hdr.frm
                                       AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0) NO-LOCK,
                     first ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND 
                                      ITEM.mat-type = "D" NO-LOCK :
                     v-mat-for-mach = v-mat-for-mach + "      " + ITEM.i-name.
                 END.
             END.          
             v-spoil = ROUND( ((wrk-op.num-sh[job-hdr.frm] - wrk-op.mr-waste[job-hdr.frm])
                       * wrk-op.spoil[job-hdr.frm] / 100),0).
             v-output = wrk-op.num-sh[job-hdr.frm] - wrk-op.mr-waste[job-hdr.frm] - v-spoil.
             IF s-prt-mstandard THEN DO:                
                IF s-run-speed THEN
                   PUT wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                       wrk-op.mr[job-hdr.frm]         SPACE(5)
                       wrk-op.speed[job-hdr.frm]      SPACE(5)
                       /*wrk-op.spoil[job-hdr.frm]*/ v-spoil FORM ">>,>>9"     SPACE(5)
                       wrk-op.num-sh[job-hdr.frm] SPACE(3)
                       /*v-output */
                      /* v-mat-for-mach FORM "x(60)"*/
                      "<R-1><C67><FROM><R+1><LINE><||3>" /*RDR*/
                      "<R-1><C87><FROM><R+1><LINE><||3>" /*RDR*/
                      "<R-1><C108><FROM><R+1><LINE><||3>" /*RDR*/
                      "<C67><FROM><C108><LINE><||3>" /*RDR*/
                       SKIP. 
               ELSE
                   PUT wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                       wrk-op.mr[job-hdr.frm]         SPACE(5)
                       wrk-op.run-hr[job-hdr.frm]     SPACE(5)
                       /*wrk-op.spoil[job-hdr.frm]*/ v-spoil   FORM ">>,>>9"   SPACE(5)
                       wrk-op.num-sh[job-hdr.frm] SPACE(3)
                       /*v-output */
                       /*v-mat-for-mach FORM "x(60)"    */
                      "<R-1><C67><FROM><R+1><LINE><||3>" /*RDR*/
                      "<R-1><C87><FROM><R+1><LINE><||3>" /*RDR*/
                      "<R-1><C108><FROM><R+1><LINE><||3>" /*RDR*/                      
                       "<C67><FROM><C108><LINE><||3>" /*RDR*/
                       SKIP.
             END.
             ELSE PUT wrk-op.m-dscr   SPACE(5)
                     /* wrk-op.mr-waste[job-hdr.frm]   */ SPACE(10)
                      /*wrk-op.mr[job-hdr.frm] >>9.99 */   SPACE(11)
                      /*wrk-op.speed[job-hdr.frm] >>>>9*/      SPACE(10)
                      /*wrk-op.spoil[job-hdr.frm]   >>9.99*/   SPACE(12)
                     /* v-mat-for-mach FORM "x(60)" */
                       "<R-1><C67><FROM><R+1><LINE><||3>" /*RDR*/
                      "<R-1><C87><FROM><R+1><LINE><||3>" /*RDR*/
                      "<R-1><C108><FROM><R+1><LINE><||3>" /*RDR*/                    
                     "<C67><FROM><C108><LINE><||3>" /*RDR*/
                      SKIP.
                     
        end. /* each wrk-op*/

        PUT v-fill AT 1 
            /*"<R-1><C60<FROM><R+1><LINE><||3>" /*RDR*/*/
            "<R-1><C67><FROM><R+1><LINE><||3>" /*RDR*/
            "<R-1><C87><FROM><R+1><LINE><||3>" /*RDR*/
            "<R-1><C108><FROM><R+1><LINE><||3>" /*RDR*/
            "<C60><FROM><C108><LINE><||3>" /*RDR*/
            SKIP. 
        /** PRINT JOB INSTRUCTIONS **/

        /* dept notes*/
        lv-line-chars = 123.
        FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
        {custom/notespr5.i job v-inst2 EXTENT(v-dept-inst) "notes.rec_key = job.rec_key and notes.note_code <> '' AND LOOKUP(notes.note_code,v-exc-depts) EQ 0 and notes.note_form_no = job-hdr.frm"}
        DO i = 1 TO EXTENT(v-dept-inst):
           v-dept-inst[i] = v-inst2[i].
        END.
        
        /* spec note */        
        v-inst2 = "".
        v-spec-cnt = 0.
        v-spec-inst = "".

        SPEC-NOTE:
        FOR EACH x-hdr WHERE x-hdr.company = job-hdr.company
                         AND x-hdr.job = job-hdr.job
                         AND x-hdr.job-no = job-hdr.job-no
                         AND x-hdr.job-no2 = job-hdr.job-no2
                         AND x-hdr.frm = job-hdr.frm NO-LOCK
                         BREAK BY x-hdr.i-no:
           IF FIRST-OF(x-hdr.i-no) THEN DO:
              v-inst2 = "". 
              FIND FIRST itemfg WHERE itemfg.company = job-hdr.company
                            AND itemfg.i-no = x-hdr.i-no NO-LOCK NO-ERROR.
              {custom/notespr2.i itemfg v-inst2 4
                                 "notes.rec_key = itemfg.rec_key and lookup(notes.note_code,spec-list) > 0 " }
              DO i = 1 TO 4:
                 IF v-inst2[i] <> "" THEN do:
                     v-spec-cnt = v-spec-cnt  + 1.
                     v-spec-inst[v-spec-cnt] = v-inst2[i].
                 END.
                 IF j > 5 THEN LEAVE spec-note.
              END.
           END.
        END.

        PUT "<R-1><B>DEPARTMENT   INSTRUCTION NOTES</B>" SKIP.
        DO idx = 1 TO EXTENT(v-dept-inst):
          IF v-dept-inst[idx] NE '' THEN
          PUT v-dept-inst[idx] FORM "x(123)" SKIP.
        END.

        PUT v-fill SKIP
            "<R-1><B>DIE CUTTING</B>" SKIP.
        DO idx = 1 TO EXTENT(v-spec-inst):
          PUT v-spec-inst[idx] FORM "x(123)" SKIP.
        END.

        PUT v-fill SKIP
            "<R-1><B>SPEC NOTES</B>" SKIP.
        DO idx = 1 TO EXTENT(v-spec-inst):
          PUT v-spec-inst[idx] FORM "x(123)" SKIP.
        END.

        i = 1.
        v-fgitm = "".
        FOR EACH tt-fgitm.
            DELETE tt-fgitm.
        END.
        for each xjob-hdr where xjob-hdr.company eq cocode
                            and xjob-hdr.job     eq job-hdr.job
                            and xjob-hdr.job-no  eq job-hdr.job-no
                            and xjob-hdr.job-no2 eq job-hdr.job-no2
                            and xjob-hdr.frm     eq job-hdr.frm NO-LOCK BY xjob-hdr.blank-no:

           find first xoe-ordl
                where xoe-ordl.company eq xjob-hdr.company
                  and xoe-ordl.ord-no  eq xjob-hdr.ord-no
                  and xoe-ordl.job-no  eq xjob-hdr.job-no
                  and xoe-ordl.job-no2 eq xjob-hdr.job-no2
                  and xoe-ordl.i-no    eq xjob-hdr.i-no
                  no-lock no-error.
           IF AVAIL xoe-ordl THEN v-fgqty[i] = xoe-ordl.cas-cnt.
                 /*FIND FIRST itemfg WHERE itemfg.company = xjob-hdr.company AND
                                   itemfg.i-no = xjob-hdr.i-no NO-LOCK NO-ERROR.*/
           FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                             AND b-eb.est-no  eq xjob-hdr.est-no
                             AND b-eb.form-no = xjob-hdr.frm
                             AND b-eb.blank-no = xjob-hdr.blank-no
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.
           CREATE tt-fgitm.
           ASSIGN tt-fgitm.i-no = xjob-hdr.i-no
                  tt-fgitm.qty = xjob-hdr.qty
                  tt-fgitm.i-dscr = IF AVAIL b-eb THEN b-eb.part-dscr1 ELSE xjob-hdr.i-no
                  tt-fgitm.po-no = IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE ""
                  tt-fgitm.part-no = IF AVAIL b-eb THEN b-eb.part-no ELSE xjob-hdr.i-no
                  tt-fgitm.cas-cnt = /*IF AVAIL b-eb THEN b-eb.cas-cnt ELSE 0*/
                                     IF AVAIL xoe-ordl AND xoe-ordl.cas-cnt <> 0 THEN xoe-ordl.cas-cnt
                                     ELSE IF AVAIL b-eb THEN b-eb.cas-cnt
                                     ELSE 0
                  tt-fgitm.cas-pal = IF AVAIL b-eb THEN b-eb.cas-pal ELSE 0
                  tt-fgitm.seq = i
                  i = i + 1.

           FIND FIRST b-est WHERE
                 b-est.company  eq xjob-hdr.company AND
                 b-est.est-no   EQ xjob-hdr.est-no
                 NO-LOCK NO-ERROR.
           
            IF AVAIL b-est AND b-est.est-type EQ 4 THEN /*combo*/
            DO:
               FIND FIRST b-cust WHERE
                    b-cust.company EQ xjob-hdr.company AND
                    b-cust.cust-no EQ xjob-hdr.cust-no
                    NO-LOCK NO-ERROR.
           
               IF AVAIL b-cust THEN
               DO:
                  tt-fgitm.cust-name = b-cust.NAME.
           
                  FIND FIRST b-oe-ordl WHERE
                       b-oe-ordl.company eq xjob-hdr.company AND
                       b-oe-ordl.ord-no  eq xjob-hdr.ord-no AND
                       b-oe-ordl.job-no  eq xjob-hdr.job-no AND
                       b-oe-ordl.job-no2 eq xjob-hdr.job-no2 AND
                       b-oe-ordl.i-no    eq xjob-hdr.i-no
                       no-lock no-error.
           
                  IF AVAIL b-oe-ordl THEN 
                     find first b-oe-rel WHERE
                          b-oe-rel.company eq cocode AND
                          b-oe-rel.ord-no  eq b-oe-ordl.ord-no AND
                          b-oe-rel.i-no    eq b-oe-ordl.i-no AND
                          b-oe-rel.line    eq b-oe-ordl.LINE
                          no-lock no-error.
           
                   if avail b-oe-rel then do:
                      find first b-shipto WHERE
                           b-shipto.company eq cocode AND
                           b-shipto.cust-no eq b-oe-rel.cust-no AND
                           b-shipto.ship-id eq b-oe-rel.ship-id
                           no-lock no-error.  
                  
                     if avail b-shipto then
                         ASSIGN
                           tt-fgitm.shipto1 = b-shipto.ship-name
                           tt-fgitm.shipto2 = b-shipto.ship-addr[1]
                           tt-fgitm.shipto4 = trim(b-oe-rel.ship-city) + ", " +
                                              b-oe-rel.ship-state + "  " + b-oe-rel.ship-zip.
                  
                         RELEASE b-cust.
                         RELEASE b-oe-ordl.
                         RELEASE b-oe-rel.
                         RELEASE b-shipto.
                   END.
               END.
            END.
           /* IF i > 10 THEN LEAVE.*/
        END.
        
        IF s-prt-shipto THEN DO i = 1 TO 4:
            ASSIGN v-shipto1[i] = v-shipto[i]
                   v-shipto2[i] = v-shipto[i].
        END.
        ASSIGN v-cust-name2 = v-cust-name 
               v-cust-name3 = v-cust-name
               .
        /* label prints per item */
        i = 0.
        j = 0.
        FOR EACH tt-fgitm BY tt-fgitm.seq.

          IF PAGE-SIZE - LINE-COUNTER < 11 THEN PAGE.    /* page-size : 55 ,  label start at 41*/
          
          ASSIGN i = i + 1
                 v-fgitm[i] = tt-fgitm.i-no
                 v-fgdsc[i] = tt-fgitm.i-dscr
                 v-fgqty[i] = tt-fgitm.qty
                 v-pono[i] = tt-fgitm.po-no
                 v-part-no[i] = tt-fgitm.part-no
                 v-cas-cnt[i] = tt-fgitm.cas-cnt
                 v-cas-pal[i] = tt-fgitm.cas-pal
                 v-cust-name-extent[i] = tt-fgitm.cust-name
                 v-ship1-extent[i] = tt-fgitm.shipto1
                 v-ship2-extent[i] = tt-fgitm.shipto2
                 v-ship4-extent[i] = tt-fgitm.shipto4
                 j = j + 1.
          IF i >= 3 THEN DO:

             IF v-cust-name-extent[2] NE "" THEN
                  ASSIGN
                  v-cust-name2 = v-cust-name-extent[2]
                  v-cust-name3 = v-cust-name-extent[3]
                  v-shipto1[1] = v-ship1-extent[2]
                  v-shipto2[1] = v-ship1-extent[3]
                  v-shipto1[2] = v-ship2-extent[2]
                  v-shipto2[2] = v-ship2-extent[3]
                  v-shipto1[4] = v-ship4-extent[2]
                  v-shipto2[4] = v-ship4-extent[3].

             display v-fill skip
                "<B><U>LABEL ITEM" + trim(string(v-label-item-no)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(string(v-label-item-no + 1)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 54
               "<U>LABEL ITEM" + TRIM(STRING(v-label-item-no + 2)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 104
               SKIP
               "Shipto   :" v-shipto[1]
               "Shipto   :" WHEN v-fgitm[2] <> "" AT 44 v-shipto1[1] WHEN v-fgitm[2] <> ""
               "Shipto   :" WHEN v-fgitm[3] <> "" AT 87 v-shipto2[1] WHEN v-fgitm[3] <> ""
               SKIP
               "Cust Name:" v-cust-name 
               "Cust Name:"  WHEN v-fgitm[2] <> ""  AT 44 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Cust Name:" WHEN v-fgitm[3] <> "" AT 87  v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               SKIP
               "PO#      :" v-pono[1]
               "PO#      :"  WHEN v-fgitm[2] <> "" AT 44 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "PO#      :" WHEN v-fgitm[3] <> "" AT 87  v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Cust P/N :" v-part-no[1]      
               "Cust P/N :" WHEN v-fgitm[2] <> "" AT 44 v-part-no[2]  WHEN v-fgitm[2] <> "" 
               "Cust P/N :" WHEN v-fgitm[3] <> "" AT 87  v-part-no[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Descriptn:" v-fgdsc[1]
               "Descriptn:"  WHEN v-fgitm[2] <> "" AT 44 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Descriptn:" WHEN v-fgitm[3] <> "" AT 87  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Qty/Case :"  v-cas-cnt[1]
               "Qty/Case :"  WHEN v-fgitm[2] <> "" AT 44 v-cas-cnt[2]  WHEN v-fgitm[2] <> "" 
               "Qty/Case :"  WHEN v-fgitm[3] <> "" AT 87  v-cas-cnt[3] WHEN v-fgitm[3] <> ""                               
               SKIP
               "Qty/Pal  :"  v-cas-pal[1]
               "Qty/Pal  :"  WHEN v-fgitm[2] <> "" AT 44 v-cas-pal[2]  WHEN v-fgitm[2] <> "" 
               "Qty/Pal  :"  WHEN v-fgitm[3] <> "" AT 87  v-cas-pal[3] WHEN v-fgitm[3] <> ""                               
               
               SKIP
               WITH FRAME itmlbl NO-BOX NO-LABELS STREAM-IO WIDTH 180.
            DOWN WITH FRAME itmlbl.
            
            IF v-fgitm[2] <> "" THEN
               v-label-item-no = v-label-item-no + 1.

            IF v-fgitm[3] <> "" THEN
               v-label-item-no = v-label-item-no + 1.

            ASSIGN v-label-item-no = v-label-item-no + 1
                   i = 0
                   v-fgitm[1] = ""
                   v-fgdsc[1] = ""
                   v-fgqty[1] = 0
                   v-fgitm[2] = ""
                   v-fgdsc[2] = ""
                   v-fgqty[2] = 0
                   v-fgitm[3] = ""
                   v-fgdsc[3] = ""
                   v-fgqty[3] = 0
                   v-pono[1] = ""
                   v-pono[2] = ""
                   v-pono[3] = ""
                   v-part-no[1] = ""
                   v-part-no[2] = ""
                   v-part-no[3] = ""
                   v-cas-cnt[1] = 0
                   v-cas-cnt[2] = 0
                   v-cas-cnt[3] = 0
                   v-cas-pal[1] = 0
                   v-cas-pal[2] = 0
                   v-cas-pal[3] = 0
                   v-cust-name-extent[1] = ""
                   v-cust-name-extent[2] = ""
                   v-cust-name-extent[3] = ""
                   v-ship1-extent[1] = ""
                   v-ship1-extent[2] = ""
                   v-ship1-extent[3] = ""
                   v-ship2-extent[1] = ""
                   v-ship2-extent[2] = ""
                   v-ship2-extent[3] = ""
                   v-ship4-extent[1] = ""
                   v-ship4-extent[2] = ""
                   v-ship4-extent[3] = ""
                   v-last-j = j.
          END.
        END.
        IF i > 0 THEN DO:
            IF PAGE-SIZE - LINE-COUNTER < 11 THEN PAGE.

            IF v-cust-name-extent[2] NE "" THEN
               ASSIGN
               v-cust-name2 = v-cust-name-extent[2]
               v-cust-name3 = v-cust-name-extent[3]
               v-shipto1[1] = v-ship1-extent[2]
               v-shipto2[1] = v-ship1-extent[3]
               v-shipto1[2] = v-ship2-extent[2]
               v-shipto2[2] = v-ship2-extent[3]
               v-shipto1[4] = v-ship4-extent[2]
               v-shipto2[4] = v-ship4-extent[3].

            display v-fill  skip
                "<B><U>LABEL ITEM" + trim(string(v-label-item-no)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(string(v-label-item-no + 1)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 54
               "<U>LABEL ITEM" + TRIM(STRING(v-label-item-no + 2)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 104
               SKIP
               "Cust Name:" v-cust-name 
               "Cust Name:"  WHEN v-fgitm[2] <> ""  AT 44 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Cust Name:" WHEN v-fgitm[3] <> "" AT 87  v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               "Shipto   :" v-shipto[1]
               "Shipto   :" WHEN v-fgitm[2] <> "" AT 44 v-shipto1[1] WHEN v-fgitm[2] <> ""
               "Shipto   :" WHEN v-fgitm[3] <> "" AT 87 v-shipto2[1] WHEN v-fgitm[3] <> ""
               SKIP
               "PO#      :" v-pono[1]
               "PO#      :"  WHEN v-fgitm[2] <> "" AT 44 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "PO#      :" WHEN v-fgitm[3] <> "" AT 87  v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Cust P/N :" v-part-no[1]      
               "Cust P/N :" WHEN v-fgitm[2] <> "" AT 44 v-part-no[2]  WHEN v-fgitm[2] <> "" 
               "Cust P/N :" WHEN v-fgitm[3] <> "" AT 87  v-part-no[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Descriptn:" v-fgdsc[1]
               "Descriptn:"  WHEN v-fgitm[2] <> "" AT 44 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Descriptn:" WHEN v-fgitm[3] <> "" AT 87  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Qty/Case :"  v-cas-cnt[1]
               "Qty/Case :"  WHEN v-fgitm[2] <> "" AT 44 v-cas-cnt[2]  WHEN v-fgitm[2] <> "" 
               "Qty/Case :"  WHEN v-fgitm[3] <> "" AT 87  v-cas-cnt[3] WHEN v-fgitm[3] <> ""                               
               SKIP
               "Qty/Pal  :"  v-cas-pal[1]
               "Qty/Pal  :"  WHEN v-fgitm[2] <> "" AT 44 v-cas-pal[2]  WHEN v-fgitm[2] <> "" 
               "Qty/Pal  :"  WHEN v-fgitm[3] <> "" AT 87  v-cas-pal[3] WHEN v-fgitm[3] <> ""                               
               
               SKIP
               WITH FRAME itmlbl2 NO-BOX NO-LABELS STREAM-IO WIDTH 180.

            ASSIGN
               i = 0
               v-label-item-no = v-label-item-no + 1.

            IF v-fgitm[2] <> "" THEN
               v-label-item-no = v-label-item-no + 1.

            IF v-fgitm[3] <> "" THEN
               v-label-item-no = v-label-item-no + 1.
          END. /* i <= 3 */

          lv-pg-num2 = lv-pg-num2 + 1.
          /* print die# image */
          FIND FIRST b-ef WHERE b-ef.company = job-hdr.company
                        AND b-ef.est-no = job-hdr.est-no
                        AND b-ef.form-no = job-hdr.frm NO-LOCK NO-ERROR.
          IF AVAIL b-ef AND b-ef.cad-image <> "" THEN DO:
             FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                            AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
             IF LOOKUP(b-ef.cad-image,lv-cad-image-list) <= 0
             THEN DO:             
               lv-cad-image = IF index(b-ef.cad-image,":") > 0 THEN b-ef.cad-image
                              ELSE IF AVAIL sys-ctrl THEN trim(sys-ctrl.char-fld) + b-ef.cad-image + ".JPG"
                              ELSE b-ef.cad-image + ".JPG".
               lv-cad-image-list = lv-cad-image-list + b-ef.cad-image + ",".
               PAGE.
               PUT "<R2><C2><#21>".
               PUT unformatted "<=21><#22><R+47><C+105><IMAGE#22=" lv-cad-image ">" .
             END.
          END.
          ELSE DO:
            FIND FIRST eb WHERE eb.company = job-hdr.company
                          AND eb.est-no = job-hdr.est-no
                          AND eb.form-no = job-hdr.frm NO-LOCK NO-ERROR.
            FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                                AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
            IF AVAIL eb AND eb.die-no <> "" AND LOOKUP(eb.die-no,lv-cad-image-list) <= 0
            THEN DO:             
             lv-cad-image =  (IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") +
                          eb.die-no + ".JPG".
             lv-cad-image-list = lv-cad-image-list + eb.die-no + ",".
             PAGE.
             PUT "<R2><C2><#21>".
             PUT unformatted "<=21><#22><R+47><C+105><IMAGE#22=" lv-cad-image ">" .
            END.
          END.
      end. /* last-of job-hdr.frm */

      /** PRINT MULT COPIES OF TICKETS **/
      save_id = recid(job-hdr).
      if last-of(job-hdr.job-no2) then do:
        for each wrk-op:
          delete wrk-op.
        end.
        for each wrk-prep:
          delete wrk-prep.
        end.
        lv-pg-num = PAGE-NUM .
      end.

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
      
      
      
      v-first = no.
  

    end. /* each job-hdr */
    
    if v-format eq "Fibre" then page.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
