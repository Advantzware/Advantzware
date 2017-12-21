/* cerep/jobunipk.p copied from cerep/jobcbox.p              05/17/05 YSK     */
/*  factory ticket  for Unipak                                                */
/* -------------------------------------------------------------------------- */

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
def var v-spec-list as char format "x(20)" init "QA" no-undo.
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
DEF VAR li AS INT NO-UNDO.
DEF BUFFER b-est FOR est.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-shipto FOR shipto.
DEF BUFFER b-cust FOR cust.
DEF VAR v-cust-name-extent AS CHAR EXTENT 15 NO-UNDO.
DEF VAR v-ship1-extent AS CHAR EXTENT 15 NO-UNDO.
DEF VAR v-ship2-extent AS CHAR EXTENT 15 NO-UNDO.
DEF VAR v-ship4-extent AS CHAR EXTENT 15 NO-UNDO.
DEF TEMP-TABLE tt-reftable NO-UNDO LIKE reftable 
                         FIELD est-type LIKE est.est-type.
DEF VAR reftable-frm-int AS INT NO-UNDO.

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
  FIELD i-unit AS DEC
  FIELD i-% AS INT  .

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
DEF VAR v-spec-inst AS cha FORM "x(80)" EXTENT 20 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.

DEF VAR v-start-date AS DATE NO-UNDO.
DEF VAR v-shipto AS cha FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-case-size AS cha NO-UNDO.
DEF VAR v-vend LIKE po-ord.vend-no NO-UNDO.
DEF VAR v-item AS cha EXTENT 20 NO-UNDO.
DEF VAR v-i-qty AS DEC EXTENT 20 NO-UNDO.
DEF VAR v-ink1 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink2 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink10 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink20 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-po-no AS CHAR  NO-UNDO.
DEF VAR lv-mat-dept-list AS cha INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach AS cha NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF VAR v-fgitm AS cha FORM "x(15)" EXTENT 15 NO-UNDO.
DEF VAR v-fgdsc LIKE eb.part-dscr1 EXTENT 15 NO-UNDO.
DEF VAR v-fgqty LIKE job-hdr.qty EXTENT 15 FORM ">>,>>>,>>>" NO-UNDO.
DEF VAR v-pono LIKE oe-ordl.po-no EXTENT 15 NO-UNDO.
DEF VAR v-part-no LIKE eb.part-no EXTENT 15 NO-UNDO.
DEF VAR v-cas-pal LIKE eb.cas-cnt EXTENT 15 NO-UNDO.
DEF VAR v-cas-cnt LIKE eb.cas-cnt EXTENT 15 NO-UNDO.
DEFINE VARIABLE dPromDate AS DATE NO-UNDO.
DEFINE VARIABLE cCsr AS CHARACTER NO-UNDO.

DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF TEMP-TABLE tt-fgitm NO-UNDO FIELD i-no AS cha FORM "x(15)"
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
DEF VAR v-po-no2 LIKE oe-ordl.po-no NO-UNDO.
DEF VAR v-po-no3 LIKE oe-ordl.po-no NO-UNDO.
DEF VAR v-num-of-inks AS INT NO-UNDO.
DEF VAR v-bar-no AS cha NO-UNDO.

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
DEF VAR v-output AS INT FORM ">,>>>,>>9" NO-UNDO.
DEF VAR dCasCnt AS DECIMAL NO-UNDO . 

format HEADER 
       "<OLANDSCAPE><FArial><R+1><P12>" skip
       "CSR:<B>" cCsr "</B>"
       "<C80>JOB START DATE:"  v-start-date skip
       "JOB NUMBER:<B>" v-job-no space(0) "-" space(0) v-job-no2 format "99" "</B>"
       "<B><P14>F A C T O R Y   T I C K E T</B><P12>" at 52  "<C80>  PROMISE DATE:" dPromDate skip
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

/* build tt-reftable est-type 4 has no all reftable*/
for each job-hdr NO-LOCK
        where job-hdr.company               eq cocode
          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no
          and (production OR
               job-hdr.ftick-prnt           eq v-reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and can-find(first job where job.company eq cocode
                                   and job.job     eq job-hdr.job
                                   and job.job-no  eq job-hdr.job-no
                                   and job.job-no2 eq job-hdr.job-no2
                                   and job.stat    ne "H"
                                   AND (job.pr-printed EQ reprint OR
                                        NOT production))
          use-index job-no,

        first est NO-LOCK where est.company  eq job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          and est.est-type le 4
    break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2 :  

   FOR EACH reftable NO-LOCK WHERE reftable.reftable EQ "jc/jc-calc.p"
           AND reftable.company  EQ job-hdr.company
           AND reftable.loc      EQ ""
           AND reftable.code     EQ STRING(job-hdr.job,"999999999"):
       CREATE tt-reftable.
       BUFFER-COPY reftable TO tt-reftable.
       tt-reftable.est-type = est.est-type.
   END.
   FIND FIRST tt-reftable WHERE tt-reftable.reftable EQ "jc/jc-calc.p"
        AND tt-reftable.company  EQ job-hdr.company
        AND tt-reftable.loc      EQ ""
        AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")
        AND tt-reftable.val[12] = job-hdr.frm   
        NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tt-reftable THEN DO:
      CREATE tt-reftable.
      ASSIGN tt-reftable.reftable = "jc/jc-calc.p"
             tt-reftable.company = job-hdr.company
             tt-reftable.loc = ""
             tt-reftable.CODE = STRING(job-hdr.job,"999999999")
             tt-reftable.val[12] = job-hdr.frm
             tt-reftable.val[13] = job-hdr.blank-no
             tt-reftable.est-type = est.est-type.

   END.
END.
/* end of tt-reftable building*/

for each job-hdr NO-LOCK
        where job-hdr.company               eq cocode
          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no
          and (production OR
               job-hdr.ftick-prnt           eq v-reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and can-find(first job where job.company eq cocode
                                   and job.job     eq job-hdr.job
                                   and job.job-no  eq job-hdr.job-no
                                   and job.job-no2 eq job-hdr.job-no2
                                   and job.stat    ne "H"
                                   AND (job.pr-printed EQ reprint OR
                                        NOT production))
        use-index job-no,

        first est
        where est.company  eq job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          and est.est-type le 4  
        no-lock,
        EACH tt-reftable
        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY tt-reftable.val[12]:       

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
         lv-pg-num2 = 1.
         lv-cad-image-list = "".
      END.
      
      if first-of(tt-reftable.val[12]) then v-first = yes.

      reftable-frm-int = INT(tt-reftable.val[12]).

      /** PRINT JOB HEADER **/
      if v-first then do:
        assign
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2.


        if avail oe-ord then
          if not oe-ctrl.p-fact and oe-ord.stat eq "H" then next.

        /*v-due-date = if avail oe-ord then oe-ord.due-date else ?.*/
        v-start-date = job-hdr.start-date.

       /* if not first(job-hdr.job-no) then page.
        view frame head.*/
       
        v-shipto = "".
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

        ASSIGN
           v-due-date = IF AVAIL oe-ordl THEN oe-ordl.req-date
                        ELSE job-hdr.due-date
           dPromDate = IF AVAIL oe-ordl THEN oe-ordl.prom-date
                        ELSE IF AVAIL oe-ord THEN oe-ord.due-date ELSE ?   .

        FIND FIRST cust WHERE cust.company = job-hdr.company AND
                              cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.
        v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                      ELSE IF AVAIL cust THEN cust.name
                      ELSE job-hdr.cust-no.
        cCsr = cust.spare-char-2.
        FIND first eb WHERE eb.company     EQ job-hdr.company
                        AND eb.est-no      eq job-hdr.est-no
                        and eb.form-no     eq reftable-frm-int
                        AND eb.stock-no = job-hdr.i-no NO-LOCK NO-ERROR.
        IF NOT AVAIL eb THEN FIND first eb WHERE eb.company     EQ job-hdr.company
                        AND eb.est-no      eq job-hdr.est-no
                        and eb.form-no     eq reftable-frm-int
                        AND eb.blank-no > 0 NO-LOCK NO-ERROR.
        v-bar-no = IF AVAIL eb AND eb.spc-no NE "" THEN eb.spc-no ELSE trim(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99").


       if not first(job-hdr.job-no) THEN 
           page.
       view frame head.

       
        PUT "<R-1><#1><C91.5>Date/Time Generated:" SKIP
            "<B>CUSTOMER NAME:</B>" v-cust-name "<B> DUE DATE:     ESTIMATE:" "<C91.5>" lv-prt-date space(1) lv-prt-time SKIP
            "SHIPTO:</B>" v-shipto[1] "<C44>" v-due-date "<C57>" trim(job-hdr.est-no) FORM "x(8)" 
            "<C91.9>Status" SKIP
            v-shipto[2] AT 7 "<C91.9>" lv-prt-sts SKIP
            v-shipto[4] AT 7 SKIP
            v-fill SKIP.     
        /* barcode print */
        PUT UNFORMATTED "<r-5.6><#1><UNITS=INCHES><C70.5><FROM><c90.8><r+3.5><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
        /*    trim(job-hdr.job-no) "-" STRING(job-hdr.job-no2,"99") ">"
            "<AT=,8.8>" trim(job-hdr.job-no) "-" STRING(job-hdr.job-no2,"99") */
              v-bar-no ">" "   Page#:" string(lv-pg-num2,">>9") + " of " + string(lv-tot-pg) FORM "x(20)"
            "<C71>" v-bar-no  "<=#1><R+5>".

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
              AND job-mch.frm = reftable-frm-int
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
            AND ef.form-no = tt-reftable.val[12]
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

        if ef.form-no eq tt-reftable.val[12] then ebloop:
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
                  WHERE reftable.reftable EQ "ce/v-est3.w Unit#"
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
                   wrk-ink.i-unit   = v-unit
                   wrk-ink.i-% = eb.i-%2[i]   .
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

            /*if avail wrk-ink then wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.*/
            if avail wrk-ink AND
               ((est.est-type eq 4 and eb.form-no = job-mat.frm AND eb.blank-no = job-mat.blank-no) OR
                 est.est-type <> 4 ) 
                 then wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.  
/*IF AVAIL wrk-ink THEN
    MESSAGE wrk-ink.form-no wrk-ink.blank-no skip
    wrk-ink.i-code wrk-ink.i-unit wrk-ink.i-pass wrk-ink.i-qty VIEW-AS ALERT-BOX.*/
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
            IF FIRST-OF(eb.form-no) THEN
              PUT  /*"<R-1>" "Case" AT 97 SKIP */
                "<P10><B>F/B <C5>FG ITEM# <C21>O/U%<C29>JOB QTY<C38>PO#<C49>STYLE<C55>CARTON SIZE<C69>COUNT<C77>CASE CODE<C93.5>#UP" "<C98>" v-upc-lbl "</B>" SKIP.
            /*else
              put fill("-",132) format "x(132)". */
            v-job-qty = 0.
            for each xjob-hdr where xjob-hdr.company eq cocode
               and xjob-hdr.job     eq job-hdr.job
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.job-no2 eq job-hdr.job-no2
               and xjob-hdr.i-no    eq eb.stock-no /*job-hdr.i-no*/  no-lock:
               v-job-qty = v-job-qty + xjob-hdr.qty.
            end.
            /** PRINT ITEM **/
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq eb.stock-no /* job-hdr.i-no */
                no-lock no-error.

            if avail oe-ordl then do:
              v-est-qty = oe-ordl.qty.
              find first oe-ord of oe-ordl no-lock.
              v-ovund = string(trim(string(oe-ordl.over-pct,">9")) + "/" +
                               trim(string(oe-ordl.under-pct,">9"))).
                               
            end.
            else v-est-qty = v-job-qty.
            
            release w-lo.
            find first w-lo no-error.
            v-case-size = string(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
                         string(eb.cas-dep).
            v-up = eb.num-up.
            v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".
            v-upc-no = eb.spc-no.
            dCasCnt = IF AVAIL oe-ordl THEN oe-ordl.cas-cnt ELSE eb.cas-cnt .

            PUT  trim(string(eb.form-no,">>9")) + "-" +
                    trim(string(eb.blank-no,">>9")) FORM "x(5)" 
                    "<C5>" eb.stock-no FORM "x(15)" /* was 19, b4 that 21*/
                    "<C21>"  v-ovund FORMAT "x(7)"  
                    "<C29>"  v-job-qty FORMAT "->>>>>>>>9" 
                    "<C38>"  v-po-no  FORMat "x(10)" 
                    "<C49>"  eb.style FORMAT "x(6)" /*v-stypart */
                    "<C55>"  v-size[1] FORM "x(19)"
                    "<C69>"  dCasCnt FORM ">>>>>9" 
                    "<C77>" eb.cas-no /*v-case-size*/  FORM "x(19)" /* was 15 */ 
                    "<C92.5>" v-up   
                    
                    "<C98>" v-upc-no FORM "x(15)"
                    skip.
             PUT "<C5>" v-dsc[1] FORMAT "x(30)" SKIP.
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
             IF AVAIL oe-ordl THEN 
               FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                            AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
             v-vend = /*IF AVAIL po-ord THEN po-ord.vend-no ELSE "".*/
                      IF AVAIL oe-ordl THEN oe-ordl.vend-no ELSE "".
             v-board-po = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0.
             IF AVAIL po-ord THEN
                FIND FIRST po-ordl WHERE
                     po-ordl.company EQ po-ord.company AND
                     po-ordl.po-no   EQ po-ord.po-no AND
                     po-ordl.i-no = ef.board
                     NO-LOCK NO-ERROR. 

             v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

             PUT "<P11>" v-fill SKIP                       /*REQ'D*/                 
                 "<B>BOARD CODE<C17>DUE DATE<C25>SHEETS<C34>SHEET SIZE<C47>CALIPER<C54.5>DIE SIZE<C67>BOARD PO#<C77>VENDOR#<C85>DIE#<C92.5>PRE-PRESS<C102>CAD#</B>"
                 SKIP.
            /** PRINT SHEET **/
             x = 2.
             for each wrk-sheet break by wrk-sheet.form-no:
               IF AVAIL po-ord THEN
                  FIND FIRST po-ordl WHERE
                       po-ordl.company EQ po-ord.company AND
                       po-ordl.po-no   EQ po-ord.po-no AND
                       po-ordl.i-no = wrk-sheet.i-no
                       NO-LOCK NO-ERROR. 

               v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.
               PUT  wrk-sheet.brd-dscr FORMAT "x(20)"
                    "<C17>" v-po-duedate "<C26>" wrk-sheet.gsh-qty 
                    "<C34>" string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len)
                    format "x(16)"
                    "<C47>" wrk-sheet.cal
                    "<C53.5>" string(ef.trim-w) + "x" + string(ef.trim-l) FORM "x(16)"
                    "<C67>" v-board-po 
                    "<C77>" v-vend  
                    "<C85>" eb.die-no  FORM "x(8)"
                    "<C92.5>" eb.plate-no FORM "x(8)" SPACE(2)
                    "<C102>" eb.cad-no FORM "x(6)" SKIP
                   .
               x = 1.
             end. /* each wrk-sheet */
             if x ne 2 then put v-fill at 1 skip(1).
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
                      BY wrk-ink.i-pass
                      BY wrk-ink.i-unit
                       :
                IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.
                IF FIRST-OF(wrk-ink.i-unit) THEN ASSIGN v-item[i] = ""
                                                        v-i-qty[i] = 0.

                v-item[i] = IF LOOKUP(string(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ",".
                /*IF LAST-OF(wrk-ink.i-pass) THEN DO:
                   IF wrk-ink.i-pass = 1 THEN
                      v-ink1[i] = wrk-ink.i-code + " " + wrk-ink.i-dscr + " " + v-item[i].
                   ELSE IF wrk-ink.i-pass = 2 THEN
                        v-ink2[i] = wrk-ink.i-code + " " + wrk-ink.i-dscr + " " + v-item[i].
                   i = i + 1.
                END. */
                v-i-qty[i] = v-i-qty[i] + wrk-ink.i-qty.                
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
                            v-ink1[i] =   string(wrk-ink.i-code,"X(10)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") + " " + /*string(wrk-ink.i-%,">>9%")*/ STRING(v-i-qty[i],"->>>,>>9.99").
                        ELSE IF wrk-ink.i-unit > 0 AND wrk-ink.i-unit <= 8 THEN v-ink1[int(wrk-ink.i-unit)] = string(wrk-ink.i-code,"X(10)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)")  + " " + /*string(wrk-ink.i-%,">>9%")*/ STRING(v-i-qty[i],"->>>,>>9.99").
                        i = i + 1         . 
                    END.
                    ELSE IF wrk-ink.i-pass = 2 THEN DO:
                    /*   ASSIGN v-ink2[i] = /*STRING(wrk-ink.i-unit,">>>") + "  " +*/ string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") /*+ " " + trim(v-alloc)*/ /*v-item[i]*/ )
                                         */
                        IF wrk-ink.i-unit = i THEN  
                            v-ink2[i] =   string(wrk-ink.i-code,"X(10)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") + " " + /*string(wrk-ink.i-%,">>9%")*/ STRING(v-i-qty[i],"->>>,>>9.99").
                        ELSE IF wrk-ink.i-unit <= 8 THEN v-ink2[int(wrk-ink.i-unit)] = string(wrk-ink.i-code,"X(10)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") + " " + /*string(wrk-ink.i-%,">>9%")*/ STRING(v-i-qty[i],"->>>,>>9.99").
                              i = i + 1.           
                    END.
                END.
                   
                /*delete wrk-ink.*/                
             end. /* each wrk-ink */

             /* need to print 8 ink all the time */
             v-num-of-inks = 0.
             DO j = 1 TO 7:
                IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
                IF TRIM(v-ink2[j]) = "-" THEN v-ink2[j] = "".               
                IF v-ink1[j] <> "" THEN v-num-of-inks = v-num-of-inks + 1.
                IF v-ink2[j] <> "" THEN v-num-of-inks = v-num-of-inks + 1.
             END.
             IF v-num-of-inks <= 14 THEN DO j = v-num-of-inks + 1 TO 14:
                IF v-ink1[j] = "" THEN v-ink1[j] = "UNIT".  
                IF v-ink2[j] = "" THEN v-ink2[j] = "UNIT".  
             END.
             /*======================*/
             ASSIGN v-ink10 = ""
                    v-ink20 = "".
             /* display top to bottom by column 1 5
/*                                                2 6 ...*/
             DO j = 5 TO 8 :
                 IF v-ink1[j] <> "" THEN ASSIGN v-ink10[j - 4] = v-ink1[j]
                                                v-ink1[j] = "".
                 IF v-ink2[j] <> "" THEN ASSIGN v-ink20[j - 4] = v-ink2[j]
                                                v-ink2[j] = ""  .
             END.
*/             
             v-skip = NO.
             v-plate-printed = NO.
             /*====== display from left to right 1 2
                                                 3 4...
             DO j = 1 TO 8:
                IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
                IF v-ink1[j] <> "" THEN do:
                   IF v-skip THEN do:
                       PUT  v-ink1[j] FORM "x(52)" .
                       IF j = 2 THEN do:
                           PUT eb.plate-no AT 106.
                           v-plate-printed = YES.
                       END.
                       PUT SKIP.
                   END.
                   ELSE PUT v-ink1[j] FORM "x(52)".                                                             
                   v-skip = NOT v-skip.             
                END.
             END.
             IF NOT v-plate-printed THEN PUT eb.plate-no AT 106 SKIP.

            /* PUT v-fill2 AT 3 SKIP. */
             v-skip = NO.
             DO j = 1 TO 8:
                 IF TRIM(v-ink2[j]) = "-" THEN v-ink2[j] = "".                 
                IF v-ink2[j] <> "" THEN do:
                   IF v-skip THEN PUT v-ink2[j] FORM "x(52)" SKIP.
                   ELSE PUT v-ink2[j] FORM "x(52)".
                   v-skip = NOT v-skip.
                END.                
             END.
             put v-fill at 1 skip.
             ===*/
             PUT "<#15><FROM><R+7><C105><RECT><||3>"
                 "<FGColor=White><BGColor=Black><=15><R-1><#16><FROM><R+1><C105><FILLRECT#16><=16>PASS 1 <C45>PASS 2<FGColor=Black><BGColor=White>" 
                 "<=15><C44><FROM><R+7><C44><LINE><||3>" 
                 "<=15><R+1><C1><FROM><C105><LINE><||3> "
                 "<=15><R+2><C1><FROM><C105><LINE><||3> "
                 "<=15><R+3><C1><FROM><C105><LINE><||3> "
                 "<=15><R+4><C1><FROM><C105><LINE><||3> "
                 "<=15><R+5><C1><FROM><C105><LINE><||3> "
                 "<=15><R+6><C1><FROM><C105><LINE><||3> "
                 "<=15>".
             v-num-of-inks = 0. /* 1 thru 8 */
             DO j = 1 TO 7:
                 /*MESSAGE "ink" j v-ink1[j] length(v-ink1[j]) SKIP
                              "pass2:"  v-ink2[j] length(v-ink2[j]) VIEW-AS ALERT-BOX.*/
               /*IF v-ink1[j] <> "" OR v-ink2[j] <> "" THEN DO:*/
                  v-num-of-inks = v-num-of-inks + 1.
                  IF v-ink1[j] = "UNIT" THEN v-ink1[j] = "".
                  IF v-ink2[j] = "UNIT" THEN v-ink2[j] = "".
                  PUT  "UNIT" v-num-of-inks FORM ">9: "
                       v-ink1[j] FORM "x(43)" 
                      "<C45>" "  UNIT"  v-num-of-inks FORM ">9: "
                       v-ink2[j] FORM "x(43)" .
                  IF j = 1 THEN PUT    "<C90>PRE-PRESS:"   SKIP.
                  ELSE IF j = 2 THEN PUT "<C90>" eb.plate-no SKIP.
                  ELSE PUT  SKIP.
                /*  PUT v-fill AT 1 "<R-1>" SKIP.*/
               /*END.*/
             END.
             /*
             DO j = 1 TO 4:
               IF v-ink1[j] <> "" OR v-ink10[j] <> "" THEN DO:
                  v-num-of-inks = v-num-of-inks + 1.
                  IF v-ink1[j] = "UNIT" THEN v-ink1[j] = "".
                  IF v-ink10[j] = "UNIT" THEN v-ink10[j] = "".
                  PUT  "UNIT" v-num-of-inks FORM ">9: "
                       v-ink1[j] FORM "x(45)" 
                       "  UNIT"  v-num-of-inks + 4 FORM ">9: "
                       v-ink10[j] FORM "x(45)" .
                  IF j = 1 THEN PUT    "PLATE#:" eb.plate-no /*AT 106*/  SKIP.
                  ELSE PUT  SKIP.
                /*  PUT v-fill AT 1 "<R-1>" SKIP.*/
               END.
             END.
             DO j = 1 TO 4:
               IF (v-ink2[j] <> "" OR v-ink20[j] <> "" ) AND v-num-of-inks < 4 THEN DO:
                  v-num-of-inks = v-num-of-inks + 1.
                  IF v-ink2[j] = "UNIT" THEN v-ink2[j] = "".
                  IF v-ink20[j] = "UNIT" THEN v-ink20[j] = "".
                  PUT  "UNIT" v-num-of-inks FORM ">9: "
                       v-ink2[j] FORM "x(52)" 
                       "  UNIT" v-num-of-inks + 4 FORM ">9: "
                       v-ink20[j] FORM "x(52)" SKIP.
                  /*PUT v-fill AT 1 "<R-1>" SKIP */.
               END.
             END.
             */

             PUT "<FGColor=Black><BGcolor=White>" SKIP v-fill at 1 skip(1).
          END. /* last-of(eb.form-no) */
          
        end. /* each eb */
      end. /* each ef */
      end. /* first job-no */

      if last-of(tt-reftable.val[12]) then do:
         PUT "<R-1.4>".
         IF s-run-speed THEN
            PUT "<B>MACHINE                            MR WASTE     MR HRS    RUN SPEED     SPOIL%           INPUT        OUTPUT</B>"
                SKIP.
         else
            PUT "<B>MACHINE                            MR WASTE     MR HRS      RUN HOUR      SPOIL%            INPUT        OUTPUT</B>"
                SKIP.
         FOR EACH wrk-op WHERE wrk-op.s-num = tt-reftable.val[12] BREAK by wrk-op.d-seq by wrk-op.b-num:
             v-mat-for-mach = "".
             IF lookup(wrk-op.dept,lv-mat-dept-list) > 0 THEN DO:
                 
                FOR EACH xjob-mat WHERE xjob-mat.company eq cocode
                                       and xjob-mat.job     eq job-hdr.job
                                       and xjob-mat.job-no  eq job-hdr.job-no
                                       and xjob-mat.job-no2 eq job-hdr.job-no2
                                       AND xjob-mat.frm = reftable-frm-int
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
                                       AND xjob-mat.frm = reftable-frm-int
                                       AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0) NO-LOCK,
                     first ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND 
                                      ITEM.mat-type = "D" NO-LOCK :
                     v-mat-for-mach = v-mat-for-mach + "      " + ITEM.i-name.
                 END.
             END.          
             v-spoil = ROUND( ((wrk-op.num-sh[reftable-frm-int] - wrk-op.mr-waste[reftable-frm-int])
                       * wrk-op.spoil[reftable-frm-int] / 100),0).
             v-output = wrk-op.num-sh[reftable-frm-int] - wrk-op.mr-waste[reftable-frm-int] - v-spoil.
             IF s-prt-mstandard THEN DO:
                IF s-run-speed THEN
                   PUT wrk-op.m-dscr   SPACE(5)
                       "<C25>" wrk-op.mr-waste[reftable-frm-int]   SPACE(5)
                       "<C33>" wrk-op.mr[reftable-frm-int]         SPACE(5)
                       "<C44>" wrk-op.speed[reftable-frm-int]      SPACE(5)
                       "<C52>" /*wrk-op.spoil[job-hdr.frm]*/ v-spoil FORM ">>,>>9"     SPACE(5)
                       "<C60>" wrk-op.num-sh[reftable-frm-int] SPACE(3)
                       /* v-mat-for-mach FORM "x(60)"*/
                       "<C70>" v-output 
                       . 
               ELSE
                   PUT wrk-op.m-dscr   SPACE(5)
                      "<C25>" wrk-op.mr-waste[reftable-frm-int]   SPACE(5)
                      "<C33>" wrk-op.mr[reftable-frm-int]         SPACE(5)
                      "<C44>"  wrk-op.run-hr[reftable-frm-int]     SPACE(5)
                      "<C52>" /*wrk-op.spoil[job-hdr.frm]*/ v-spoil   FORM ">>,>>9"   SPACE(5)
                      "<C60>" wrk-op.num-sh[reftable-frm-int] SPACE(3)
                      "<C70>" v-output 
                       /*v-mat-for-mach FORM "x(60)"    */
                       .
             END.
             ELSE PUT wrk-op.m-dscr   SPACE(5)
                     /* wrk-op.mr-waste[job-hdr.frm]   */ SPACE(10)
                      /*wrk-op.mr[job-hdr.frm] >>9.99 */   SPACE(11)
                      /*wrk-op.speed[job-hdr.frm] >>>>9*/      SPACE(10)
                      /*wrk-op.spoil[job-hdr.frm]   >>9.99*/   SPACE(12)
                     /* v-mat-for-mach FORM "x(60)" */
                      .
             IF first(wrk-op.d-seq) AND  s-prt-mstandard THEN PUT "<C76>   Copy approval _____________________" SKIP.
             ELSE IF first(wrk-op.d-seq)  THEN PUT "<C60>   Copy approval _______________________" SKIP.
             ELSE PUT SKIP.
        end. /* each wrk-op*/

        PUT v-fill AT 1 SKIP.
        /** PRINT JOB INSTRUCTIONS **/

        /* dept notes*/
        lv-line-chars = 128.
        FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
        {custom/notespr5.i job v-inst2 20 "notes.rec_key = job.rec_key and notes.note_code <> '' AND LOOKUP(notes.note_code,v-exc-depts) EQ 0"}
        DO i = 1 TO 20:
           v-dept-inst[i] = v-inst2[i].
        END.
        
        /* spec note */
        ASSIGN
        v-inst2 = ""
        v-spec-cnt = 0
        v-spec-inst = "".

        SPEC-NOTE:
        FOR EACH x-hdr WHERE x-hdr.company = job-hdr.company
                         AND x-hdr.job = job-hdr.job
                         AND x-hdr.job-no = job-hdr.job-no
                         AND x-hdr.job-no2 = job-hdr.job-no2
                         AND x-hdr.frm = reftable-frm-int NO-LOCK
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

    /*  IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */ */
        PUT "<R-1><B>DEPARTMENT   INSTRUCTION NOTES</B>" SKIP
            v-dept-inst[1] FORM "x(128)" SKIP
            v-dept-inst[2] FORM "x(128)" SKIP
            v-dept-inst[3] FORM "x(128)" SKIP
            v-dept-inst[4] FORM "x(128)" SKIP
            /*v-dept-inst[5] FORM "x(128)" SKIP
            v-dept-inst[6] FORM "x(128)" SKIP
            v-dept-inst[7] FORM "x(128)" SKIP
            v-dept-inst[8] FORM "x(128)" SKIP
            */
            v-fill SKIP
            "<R-1><B>SPEC NOTES</B>" SKIP
            v-spec-inst[1] FORM "x(128)" SKIP
            v-spec-inst[2] FORM "x(128)" SKIP
            v-spec-inst[3] FORM "x(128)" SKIP
            v-spec-inst[4] FORM "x(128)" SKIP

            /*
            
            "            " v-dept-inst[9] SKIP
            "            " v-dept-inst[10] SKIP
            "            " v-dept-inst[11] SKIP
            "            " v-dept-inst[12] SKIP
            "            " v-dept-inst[13] SKIP
            "            " v-dept-inst[14] SKIP
            "            " v-dept-inst[15] SKIP
            "            " v-dept-inst[16] SKIP
            "            " v-dept-inst[17] SKIP
            "            " v-dept-inst[18] SKIP
            "            " v-dept-inst[19] SKIP
            "            " v-dept-inst[20] SKIP
            */
            .


        i = 1.
        v-fgitm = "".
        FOR EACH tt-fgitm.
            DELETE tt-fgitm.
        END.
        for each xjob-hdr where xjob-hdr.company eq cocode
                            and xjob-hdr.job     eq job-hdr.job
                            and xjob-hdr.job-no  eq job-hdr.job-no
                            and xjob-hdr.job-no2 eq job-hdr.job-no2
                            and xjob-hdr.frm     eq reftable-frm-int NO-LOCK BY xjob-hdr.blank-no:
          /*
           ASSIGN v-fgitm[i] = xjob-hdr.i-no
                  v-fgqty[i] = xjob-hdr.qty.
           */
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
                             AND b-eb.form-no = reftable-frm-int
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
                "<B><U>LABEL ITEM" + trim(string(j - 2)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(string(j - 1)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 54
               "<U>LABEL ITEM" + TRIM(STRING(j)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 104
               SKIP
            /*   "Job#:" v-job-no + "-" + string(v-job-no2)
               "Job#:"  WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + string(v-job-no2)   WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + string(v-job-no2) WHEN v-fgitm[3] <> "" 
               SKIP */
               "Cust Name:" v-cust-name 
               "Cust Name:"  WHEN v-fgitm[2] <> ""  AT 44 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Cust Name:" WHEN v-fgitm[3] <> "" AT 87  v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               "Shipto   :" /*WHEN s-prt-shipto*/ v-shipto[1] /*WHEN s-prt-shipto */
               "Shipto   :" WHEN /*s-prt-shipto AND*/ v-fgitm[2] <> "" AT 44 v-shipto1[1] WHEN /*s-prt-shipto AND */ v-fgitm[2] <> ""
               "Shipto   :" WHEN /*s-prt-shipto AND*/ v-fgitm[3] <> "" AT 87 v-shipto2[1] WHEN /*s-prt-shipto AND */ v-fgitm[3] <> ""
               SKIP
               "PO#      :" v-pono[1]
               "PO#      :"  WHEN v-fgitm[2] <> "" AT 44 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "PO#      :" WHEN v-fgitm[3] <> "" AT 87  v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Cust P/N :" v-part-no[1]      
               "Cust P/N :" WHEN v-fgitm[2] <> "" AT 44 v-part-no[2]  WHEN v-fgitm[2] <> "" 
               "Cust P/N :" WHEN v-fgitm[3] <> "" AT 87  v-part-no[3] WHEN v-fgitm[3] <> "" 
               /*SKIP
               "<B>FG Item #:</B>" v-fgitm[1]
               "<B>FG Item #:</B>"  WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] WHEN v-fgitm[2] <> "" 
               "<B>FG Item #:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] WHEN v-fgitm[3] <> "" 
               */
               SKIP
               "Descriptn:" v-fgdsc[1]
               "Descriptn:"  WHEN v-fgitm[2] <> "" AT 44 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Descriptn:" WHEN v-fgitm[3] <> "" AT 87  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Qty/Case :" /*v-fgqty[1]*/ v-cas-cnt[1]
               "Qty/Case :"  WHEN v-fgitm[2] <> "" AT 44 v-cas-cnt[2]  WHEN v-fgitm[2] <> "" 
               "Qty/Case :"  WHEN v-fgitm[3] <> "" AT 87  v-cas-cnt[3] WHEN v-fgitm[3] <> ""                               
               SKIP
               "Qty/Pal  :" /*v-fgqty[1]*/ v-cas-pal[1]
               "Qty/Pal  :"  WHEN v-fgitm[2] <> "" AT 44 v-cas-pal[2]  WHEN v-fgitm[2] <> "" 
               "Qty/Pal  :"  WHEN v-fgitm[3] <> "" AT 87  v-cas-pal[3] WHEN v-fgitm[3] <> ""                               
               /*SKIP
               v-shipto[2] AT 8  WHEN s-prt-shipto
               v-shipto1[2] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[2] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               SKIP
               v-shipto[4] AT 8  WHEN s-prt-shipto
               v-shipto1[4] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[4] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               */
               SKIP
               WITH FRAME itmlbl NO-BOX NO-LABELS STREAM-IO WIDTH 180.
            DOWN WITH FRAME itmlbl.
            
            ASSIGN i = 0
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
                "<B><U>LABEL ITEM" + trim(string(v-last-j + 1)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(string(v-last-j + 2 )) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 54
               "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 3)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 104
               SKIP
            /*   "Job#:" v-job-no + "-" + string(v-job-no2)
               "Job#:"  WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + string(v-job-no2)   WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + string(v-job-no2) WHEN v-fgitm[3] <> "" 
               SKIP */
               "Cust Name:" v-cust-name 
               "Cust Name:"  WHEN v-fgitm[2] <> ""  AT 44 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Cust Name:" WHEN v-fgitm[3] <> "" AT 87  v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               "Shipto   :" /*WHEN s-prt-shipto*/ v-shipto[1] /*WHEN s-prt-shipto*/
               "Shipto   :" WHEN /*s-prt-shipto AND */ v-fgitm[2] <> "" AT 44 v-shipto1[1] WHEN /*s-prt-shipto AND */ v-fgitm[2] <> ""
               "Shipto   :" WHEN /*s-prt-shipto AND */ v-fgitm[3] <> "" AT 87 v-shipto2[1] WHEN /*s-prt-shipto AND */ v-fgitm[3] <> ""
               SKIP
               "PO#      :" v-pono[1]
               "PO#      :"  WHEN v-fgitm[2] <> "" AT 44 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "PO#      :" WHEN v-fgitm[3] <> "" AT 87  v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Cust P/N :" v-part-no[1]      
               "Cust P/N :" WHEN v-fgitm[2] <> "" AT 44 v-part-no[2]  WHEN v-fgitm[2] <> "" 
               "Cust P/N :" WHEN v-fgitm[3] <> "" AT 87  v-part-no[3] WHEN v-fgitm[3] <> "" 
               /*SKIP
               "<B>FG Item #:</B>" v-fgitm[1]
               "<B>FG Item #:</B>"  WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] WHEN v-fgitm[2] <> "" 
               "<B>FG Item #:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] WHEN v-fgitm[3] <> "" 
               */
               SKIP
               "Descriptn:" v-fgdsc[1]
               "Descriptn:"  WHEN v-fgitm[2] <> "" AT 44 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Descriptn:" WHEN v-fgitm[3] <> "" AT 87  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Qty/Case :" /*v-fgqty[1]*/ v-cas-cnt[1]
               "Qty/Case :"  WHEN v-fgitm[2] <> "" AT 44 v-cas-cnt[2]  WHEN v-fgitm[2] <> "" 
               "Qty/Case :"  WHEN v-fgitm[3] <> "" AT 87  v-cas-cnt[3] WHEN v-fgitm[3] <> ""                               
               SKIP
               "Qty/Pal  :" /*v-fgqty[1]*/ v-cas-pal[1]
               "Qty/Pal  :"  WHEN v-fgitm[2] <> "" AT 44 v-cas-pal[2]  WHEN v-fgitm[2] <> "" 
               "Qty/Pal  :"  WHEN v-fgitm[3] <> "" AT 87  v-cas-pal[3] WHEN v-fgitm[3] <> ""                               
               /*SKIP
               v-shipto[2] AT 8  WHEN s-prt-shipto
               v-shipto1[2] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[2] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               SKIP
               v-shipto[4] AT 8  WHEN s-prt-shipto
               v-shipto1[4] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[4] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               */
               SKIP
               WITH FRAME itmlbl2 NO-BOX NO-LABELS STREAM-IO WIDTH 180.
            i = 0.
          END. /* i <= 3 */

          lv-pg-num2 = lv-pg-num2 + 1.
          /* print die# image */
          FIND FIRST eb WHERE eb.company = job-hdr.company
                          AND eb.est-no = job-hdr.est-no
                          AND eb.form-no = reftable-frm-int NO-LOCK NO-ERROR.
          FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                                AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
          IF AVAIL eb AND eb.die-no <> "" AND LOOKUP(eb.die-no,lv-cad-image-list) <= 0
          THEN DO:             
             lv-cad-image =  (IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") +
                          eb.die-no + ".JPG".
             lv-cad-image-list = lv-cad-image-list + eb.die-no + ",".
             PAGE.
             /*PUT "<R3><C2><#21><FROM><R+44><C105><RECT><||3>"
                 "<=21><C50>DIE IMAGE" .
             PUT unformatted "<=21><R+2><C3><#22><R+39><C+100><IMAGE#22=" lv-cad-image ">" .
             */
             PUT "<R2><C2><#21>".
             PUT unformatted "<=21><#22><R+47><C+105><IMAGE#22=" lv-cad-image ">" .
          END.

      end. /* last-of reftable-frm-int */

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
