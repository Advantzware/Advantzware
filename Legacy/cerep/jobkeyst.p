/* cerep/jobkeyst.p   factory ticket  for folding , Keystone and MWFibre  */
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
def var v-dsc like oe-ordl.part-dscr1 extent 3.
def new shared var v-size as char format "x(26)" extent 2.
def new shared var v-bld-job like oe-ord.job-no.
def new shared var v-bld-job2 like oe-ord.job-no2.
def new shared var v-fill as char format "x(90)".
def var v-fill78 as char format "x(78)".
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
DEF VAR li AS INT NO-UNDO.
DEF VAR tb_app-unprinted AS LOG NO-UNDO.

def TEMP-TABLE w-lo NO-UNDO
  field layout like v-layout.

def new shared buffer xjob-hdr for job-hdr.

def buffer b-eb for eb.

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
  FIELD mr-waste LIKE job-mch.mr-waste EXTENT 100    .

def new shared workfile wrk-die
  field die-no like eb.die-no
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
  FIELD i-seq AS INT  .

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
DEF VAR v-item AS cha EXTENT 50 NO-UNDO.
DEF VAR v-ink1 AS cha EXTENT 50 NO-UNDO.
DEF VAR v-ink2 AS cha EXTENT 50 NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no NO-UNDO.
DEF VAR lv-mat-dept-list AS cha INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach AS cha NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF VAR v-fgitm AS cha FORM "x(15)" EXTENT 10 NO-UNDO.
DEF VAR v-fgdsc LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEF VAR v-fgqty LIKE job-hdr.qty EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEF VAR v-pono LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF VAR v-last-order AS CHAR NO-UNDO.
DEF VAR v-est-no LIKE est.est-no NO-UNDO.
DEF VAR v-del-date AS DATE NO-UNDO.
DEF VAR v-over-pct LIKE oe-ord.over-pct NO-UNDO.
DEF VAR v-sht-qty AS INT NO-UNDO.
DEF VAR v-prt-up LIKE v-up NO-UNDO.
DEF VAR v-prt-sht LIKE v-sht-qty NO-UNDO.
DEF TEMP-TABLE tt-fgitm NO-UNDO
    FIELD i-no AS cha FORM "x(15)"
    FIELD seq AS INT
    FIELD qty AS INT 
    FIELD i-dscr AS cha
    FIELD po-no AS cha.
DEF VAR v-board-po LIKE oe-ordl.po-no-po NO-UNDO.
DEF VAR v-plate-printed AS LOG NO-UNDO.
DEF BUFFER xoe-ordl FOR oe-ordl.
DEF VAR v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name2 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name3 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cus AS cha EXTENT 4 FORM "x(30)" NO-UNDO.
DEF VAR v-last-j AS INT NO-UNDO.
DEF VAR v-po-no2 LIKE v-po-no NO-UNDO.
DEF VAR v-po-no3 LIKE v-po-no NO-UNDO.
DEF VAR v-spc-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-ord-qty AS INT NO-UNDO.
DEF VAR v-job-qty2 AS INT NO-UNDO.
DEF VAR v-yld-qty AS INT NO-UNDO.
DEF BUFFER bf-eb FOR eb.
DEF VAR v-pg-num AS INT NO-UNDO.
DEF VAR v-tot-up AS INT NO-UNDO.
ASSIGN
v-fill = "<||6><C1><FROM><C82><LINE><||6>"
v-fill78 = "<||6><C1><FROM><C78><LINE><||6>"
v-fill2 = FILL("-",80).

def new shared frame head.

DEF SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF VAR v-po-duedate LIKE po-ordl.due-date NO-UNDO.
DEF VAR v-upc-lbl AS cha FORM "x(10)" NO-UNDO.
DEF VAR v-shipto1 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shipto2 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF VAR v-ink-seq AS INT NO-UNDO.
DEF VAR v-ink-list AS cha NO-UNDO.
DEF VAR v-ink-use-per-blank AS INT NO-UNDO.  
DEF VAR vs-len AS cha NO-UNDO.
DEF VAR vs-wid AS cha NO-UNDO.
DEF VAR vs-dep AS cha NO-UNDO.
DEF VAR v-i-qty AS INT NO-UNDO.
DEF BUFFER bf-ink FOR wrk-ink.
DEF VAR v-ord-no LIKE job-hdr.ord-no NO-UNDO.
DEF VAR ld-metric AS DEC NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.

/* get values to print */
{cerep/jc-keyst.i}
{cerep/jc-keys2.i}

DEF TEMP-TABLE tt-ink NO-UNDO
    FIELD i-code LIKE wrk-ink.i-code
    FIELD i-seq LIKE wrk-ink.i-seq.

DEF TEMP-TABLE tt-reftable NO-UNDO LIKE reftable
    FIELD est-type LIKE est.est-type.

PUT "<#1><FGCOLOR=RED><B><LINECOLOR=RED><R3><C65><From><R10><C65><Line><||6>"
    "<R18><C41><From><R31><C41><Line><||6>"
    "<R25><C41><FROM><R25><C78><LINE><||6>"
    "<R28><C41><FROM><R28><C82><LINE><||6> "
    " <R3><C78><FROM><R65><C78><LINE><||6> "
     "<R3><C79>S.O."
    "<R12><C79>CUST"
    "<R28><C79>VIA:"
    "<R29><C79>K"
    "<R30><C79>P/B"
    "<R31><C79>N/C"
    "<R32><C79>COLL"
    "<R34><C79>3RD"
    "<R36><C78><FROM><R36><C82><LINE><||6>"
    "<R36><C79>EX."
    "<R37><C79>CHGS"
    "<R39><C79>YES"
    "<R40><C79><FROM><R41><C81><RECT><||6>"
    "<R43><C79>NO"
    "<R44><C79><FROM><R45><C81><RECT><||6>"
    "<R50><C78><FROM><R50><C82><LINE><||6>"
    "<R50><C79>INIT"
    "<R55><C78><FROM><R55><C82><LINE><||6>"
    "<R55><C79>DATE<FGCOLOR=BLACK>" 
    "<=1>" .

format header
       "<FGCOLOR=RED><B>  S.O<FGCOLOR=BLACK>" v-job-no + "-" + STRING(v-job-no2,"99") FORM "X(9)"         
       "<FGCOLOR=RED>  ORDER<FGCOLOR=BLACK>" v-ord-no
       "<FGCOLOR=RED>  EST#<FGCOLOR=BLACK>" trim(v-est-no) FORM "x(8)"  "<FGCOLOR=RED>  DATE<FGCOLOR=BLACK>" TODAY
       "<FGCOLOR=RED>  PO<FGCOLOR=BLACK>" v-po-no "<FGCOLOR=RED>DEL<FGCOLOR=BLACK>" v-del-date SKIP
       v-fill
    with no-box frame head no-labels stream-io width 290.

format "  Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
       "Salesman:" at 90 oe-ord.sname[1] "Order#:" at 138 oe-ord.ord-no
    with no-box frame line-head no-labels stream-io width 162.
    
{sys/inc/notes.i}
DEF VAR lv-jobcard-int AS INT NO-UNDO.
FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode
                      AND sys-ctrl.NAME = "JOBCARDF"
                      NO-LOCK NO-ERROR.
lv-jobcard-int = IF AVAIL sys-ctrl THEN sys-ctrl.int-fld ELSE 0.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

ASSIGN
 v-job[1]    = fjob-no
 v-job[2]    = tjob-no
 v-job2[1]   = fjob-no2
 v-job2[2]   = tjob-no2
 v-reprint   = reprint
 v-spec-list = spec-list.

{cerep/jobkeyst.i no-LOCK}
        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY job-hdr.frm
              BY job-hdr.blank-no:

IF est.est-type = 2 THEN
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
             tt-reftable.code2 = job-hdr.i-no
             tt-reftable.val[12] = job-hdr.frm
             tt-reftable.val[13] = job-hdr.blank-no
             tt-reftable.est-type = est.est-type.

   END.
END.
/* end of tt-reftable build*/

{cerep/jobkeyst.i NO-LOCK}
    ,
    EACH tt-reftable NO-LOCK
        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY tt-reftable.val[12]
              BY tt-reftable.val[13]:

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

      assign
       v-est-qty = if avail est then est.est-qty[1] else 0
       ld-metric = IF AVAIL est AND est.metric THEN 25.4 ELSE 1. 

      find first oe-ord where oe-ord.company eq job-hdr.company
                          and oe-ord.ord-no  eq job-hdr.ord-no no-lock no-error.
      
      if first-of(tt-reftable.val[12]) then v-first = yes.
      /** PRINT JOB HEADER **/
      if v-first then do:
        assign
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2
         v-est-no = job-hdr.est-no   
         v-po-no = IF AVAIL oe-ord THEN oe-ord.po-no ELSE ""
         v-ord-no = job-hdr.ord-no
         v-last-order = "     0".

        if avail oe-ord then
        DO:
           v-last-order = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
                          ELSE STRING(oe-ord.pord-no).

           IF TRIM(v-last-order) EQ "0" THEN
              v-last-order = "     0".

          if not oe-ctrl.p-fact and (oe-ord.stat eq "H" OR oe-ord.priceHold) then next.
        END.

        ASSIGN
           v-due-date = if avail oe-ord then oe-ord.due-date else ?
           v-start-date = job-hdr.start-date.

        /* get the earlist release date for the order */
        IF AVAIL oe-ord THEN
        FOR EACH oe-rel
            WHERE oe-rel.company EQ oe-ord.company
              AND oe-rel.ord-no  EQ oe-ord.ord-no
            NO-LOCK BY oe-rel.rel-date:            
            FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                    AND oe-rell.r-no  EQ oe-rel.link-no
                    AND oe-rell.ord-no   EQ oe-rel.ord-no
                    AND oe-rell.i-no     EQ oe-rel.i-no
                    AND oe-rell.line     EQ oe-rel.line
                    AND oe-rell.rel-no   EQ oe-rel.rel-no
                    AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND oe-rell.po-no    EQ oe-rel.po-no NO-LOCK NO-ERROR.
            IF NOT AVAIL oe-rell THEN
               FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                    AND oe-rell.ord-no   EQ oe-rel.ord-no
                    AND oe-rell.i-no     EQ oe-rel.i-no
                    AND oe-rell.line     EQ oe-rel.line
                    AND oe-rell.rel-no   EQ oe-rel.rel-no
                    AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                    AND oe-rell.po-no    EQ oe-rel.po-no NO-LOCK NO-ERROR.
            IF AVAIL oe-rell THEN 
               FIND oe-relh WHERE oe-relh.company = oe-rel.company
                              AND oe-relh.r-no = oe-rell.r-no NO-LOCK NO-ERROR.
           v-del-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
           LEAVE.
        END.

        if not first(job-hdr.job-no) then do:
           page.
           put "<B><FGCOLOR=RED><R3><C78><FROM><R65><C78><LINE><||6> "
               "<R3><C65><From><R10><C65><Line><||6>"
               "<R18><C41><From><R31><C41><Line><||6>"
               "<R25><C41><FROM><R25><C78><LINE><||6>"
               "<R28><C41><FROM><R28><C82><LINE><||6> "
               " <R3><C78><FROM><R65><C78><LINE><||6> "
                "<R3><C79>S.O."
                "<R12><C79>CUST"
                "<R28><C79>VIA:"
                "<R29><C79>K"
                "<R30><C79>P/B"
                "<R31><C79>N/C"
                "<R32><C79>COLL"
                "<R34><C79>3RD"
                "<R36><C78><FROM><R36><C82><LINE><||6>"
                "<R36><C79>EX."
                "<R37><C79>CHGS"
                "<R39><C79>YES"
                "<R40><C79><FROM><R41><C81><RECT><||6>"
                "<R43><C79>NO"
                "<R44><C79><FROM><R45><C81><RECT><||6>"
                "<R50><C78><FROM><R50><C82><LINE><||6>"
                "<R50><C79>INIT"
                "<R55><C78><FROM><R55><C82><LINE><||6>"
                "<R55><C79>DATE<FGCOLOR=BLACK>"
                "<=1>" .
           
        END.
        view frame head.
        IF first-of(job-hdr.job-no2) THEN 
           v-pg-num = IF PAGE-NUM >= 2 THEN PAGE-NUM - 1 ELSE 0.

        IF FIRST-OF(tt-reftable.val[12]) THEN DO:
        v-shipto = "".
        find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq tt-reftable.code2 /*job-hdr.i-no*/
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
    END.

        v-req-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?.
        FIND FIRST cust WHERE cust.company = job-hdr.company AND
                              cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.
        v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                      ELSE IF AVAIL cust THEN cust.name
                      ELSE job-hdr.cust-no.
        FIND FIRST eb WHERE eb.company = est.company
                        AND eb.est-no = est.est-no
                        AND eb.form-no <> 0
                        AND eb.blank-no <> 0 NO-LOCK NO-ERROR.
        ASSIGN
           v-spc-no = IF AVAIL eb THEN eb.spc-no ELSE ""
           v-cus[2] = cust.addr[1]
           v-cus[3] = cust.addr[2]
           v-cus[4] = trim(cust.city) + ", " + cust.state + "  " + cust.zip
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
              AND job-mch.frm = int(tt-reftable.val[12])
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
        for each xjob-hdr FIELDS(qty)
            where xjob-hdr.company eq cocode
              and xjob-hdr.job     eq job-hdr.job
              and xjob-hdr.job-no  eq job-hdr.job-no
              and xjob-hdr.job-no2 eq job-hdr.job-no2
              and xjob-hdr.i-no    eq job-hdr.i-no
            no-lock:
          v-job-qty = v-job-qty + xjob-hdr.qty.
        end.
          
        ASSIGN
        v-est-qty = 0
        v-yld-qty = 0.
        if est.est-type eq 4 then
        for each eb FIELDS(yld-qty)
            WHERE eb.company  EQ ef.company
              AND eb.est-no   eq ef.est-no
              and eb.stock-no eq job-hdr.i-no
            no-lock:
          v-est-qty = v-est-qty + eb.yld-qty.          
        end.
        else v-fac = 1.

        if est.est-type eq 4 then
        for each eb FIELDS(yld-qty)
            WHERE eb.company  EQ ef.company
              AND eb.est-no   eq ef.est-no
              and eb.form-no  eq ef.form-no no-lock:
            v-yld-qty = v-yld-qty + eb.yld-qty.
        END.

        v-itm-printed = 0.

        if ef.form-no eq tt-reftable.val[12] then ebloop:
        for each eb
            WHERE eb.company     EQ ef.company
              AND eb.est-no      eq ef.est-no
              and eb.form-no     eq ef.form-no
              NO-LOCK
            break by eb.form-no BY eb.blank-no.

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
              wrk-die.form-no = eb.form-no
              wrk-die.die-size = string(ef.trim-w) + "x" +
              string(ef.trim-l).
          end.

          /** BUILD INK WORK FILE **/
          for each job-mat
              where job-mat.company eq cocode
                and job-mat.job     eq job-hdr.job
                and job-mat.frm     eq eb.form-no
                AND job-mat.blank-no = eb.blank-no
              no-lock,
              first item
              {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no
              no-lock:

            do i = 1 to 12:
              if eb.i-code2[i] eq job-mat.i-no then do:
           
                  create wrk-ink.
                  assign
                   wrk-ink.i-code   = eb.i-code2[i]
                   wrk-ink.form-no  = eb.form-no
                   wrk-ink.blank-no = eb.blank-no
                   wrk-ink.i-dscr   = eb.i-dscr2[i]
                   wrk-ink.i-pass   = eb.i-ps2[i].
              /*  end. */
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
               wrk-ink.i-pass   = 1.
            end.

            if avail wrk-ink then wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.
          end. /* JOB-MAT */

          if eb.est-type eq 4 then v-fac = eb.yld-qty / v-est-qty.
  
            find first style
                where style.company eq eb.company
                  and style.style   eq eb.style
                no-lock no-error.
            if avail style then v-stypart = style.dscr.
            assign
             v-dsc[1] = eb.part-dscr1
             v-dsc[2] = eb.part-dscr2
             v-size[1] = string(eb.len) + "x" + string(eb.wid) + "x" + string(eb.dep)
             v-size[2] = eb.i-coldscr.

             IF eb.blank-no > 0 AND eb.blank-no < 11 THEN 
                 ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.

            v-job-qty = 0.
            for each xjob-hdr FIELDS(qty) WHERE xjob-hdr.company eq cocode
               and xjob-hdr.job     eq job-hdr.job
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.job-no2 eq job-hdr.job-no2
               and xjob-hdr.i-no    eq eb.stock-no no-lock:
               v-job-qty = v-job-qty + xjob-hdr.qty.
            end.
            v-ord-qty = 0.
            IF job-hdr.ord-no EQ 0 THEN v-ord-qty = v-job-qty.
            ELSE
            FOR EACH oe-ordl FIELDS(qty)
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  AND oe-ordl.form-no = int(tt-reftable.val[12])
                no-lock .
                v-ord-qty = v-ord-qty + oe-ordl.qty .
            END.

            v-job-qty2 = 0.
            for each xjob-hdr FIELDS(qty) where
                xjob-hdr.company eq cocode
                and xjob-hdr.job     eq job-hdr.job
                and xjob-hdr.job-no  eq job-hdr.job-no
                and xjob-hdr.job-no2 eq job-hdr.job-no2
                and xjob-hdr.frm    eq int(tt-reftable.val[12]) no-lock:
                v-job-qty2 = v-job-qty2 + xjob-hdr.qty.
            end.
            /** PRINT ITEM **/
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq eb.stock-no
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

            ASSIGN vs-len = ""
                   vs-wid = ""
                   vs-dep = "".
            
            IF eb.cas-len <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-len,64,OUTPUT vs-len).
            IF eb.cas-len <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-wid,64,OUTPUT vs-wid).
            IF eb.cas-len <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-dep,64,OUTPUT vs-dep).
            
            ASSIGN
               v-case-size = (IF vs-len <> "" THEN trim(vs-len) + "x" ELSE "") +
                             (IF vs-wid <> "" THEN TRIM(vs-wid) + "x" ELSE "") +
                              trim(vs-dep)
               v-tot-up = 0.
            
            FOR EACH bf-eb fields(num-up) WHERE
                bf-eb.company = eb.company AND
                bf-eb.est-no = eb.est-no AND
                bf-eb.form-no = eb.form-no NO-LOCK:
                v-tot-up = v-tot-up + bf-eb.num-up.                
            END.

            ASSIGN
            v-up = eb.num-up
            v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE ""
            v-spc-no = eb.spc-no
            v-job-qty = IF AVAIL oe-ordl THEN oe-ordl.qty ELSE job-hdr.qty.            
            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".
          
          IF LAST-OF(eb.form-no) THEN DO:
            FIND FIRST tt-keyst
                WHERE tt-keyst.tt-job-no  EQ job-hdr.job-no
                  AND tt-keyst.tt-job-no2 EQ job-hdr.job-no2
                  AND tt-keyst.tt-frm     EQ int(tt-reftable.val[12])
                NO-ERROR.
             IF NOT AVAIL tt-keyst THEN CREATE tt-keyst.

             /* Number of sheets */
             run oe/rep/ticket1.p (recid(ef), recid(job-hdr)).
             find first wrk-sheet where recid(wrk-sheet) eq save_id.
             IF AVAIL oe-ordl THEN 
               FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                            AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
             ASSIGN
             v-vend = IF AVAIL oe-ordl THEN oe-ordl.vend-no ELSE ""
             v-board-po = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0.
             IF AVAIL oe-ordl THEN FIND oe-ord OF oe-ordl NO-LOCK NO-ERROR.
             ASSIGN
             v-over-pct = IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                          IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 1
             v-prt-up = v-tot-up * ef.n-out-l.
             IF AVAIL po-ord THEN
                FIND FIRST po-ordl WHERE
                     po-ordl.company EQ po-ord.company AND
                     po-ordl.po-no   EQ po-ord.po-no AND
                     po-ordl.i-no = wrk-sheet.i-no
                     NO-LOCK NO-ERROR. 

             v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

             PUT "<FGCOLOR=RED>  CUSTOMER                        SHIP TO"            "PAGE<FGCOLOR=BLACK>"  AT 92 PAGE-NUMBER - v-pg-num FORM ">>9" SKIP
             v-cust-name            v-shipto[1] AT 35 SKIP
             v-cus[2]  v-shipto[2] AT 35                         SKIP
             v-cus[3]  v-shipto[3] AT 35                        "<FGCOLOR=RED>LAST SO.<FGCOLOR=BLACK>" AT 79 v-last-order FORM "X(6)" SKIP
             v-cus[4]  v-shipto[4] AT 35 SKIP(1)
             v-fill78 SKIP
             "<FGCOLOR=RED>TOTAL QUANTITY<FGCOLOR=BLACK>"  v-job-qty2      "<C22><FGCOLOR=RED>PRINT #UP<FGCOLOR=BLACK>"   v-prt-up FORM ">>9"  " <FGCOLOR=RED>Die Cut #UP<FGCOLOR=BLACK>" v-tot-up  FORM ">>9"  "<C51><FGCOLOR=RED>TOTAL COLORS<FGCOLOR=BLACK> "  /*AT 115*/ eb.i-coldscr SKIP
             v-fill SKIP    .

             /** PRINT SHEET **/
             x = 2.
             for each wrk-sheet break by wrk-sheet.form-no:
                 v-size[1] = "".
                 v-size[2] = "".
                 RUN sys/inc/dec-frac.p (wrk-sheet.sh-len,64,OUTPUT vs-len).
                 RUN sys/inc/dec-frac.p (wrk-sheet.sh-wid,64,OUTPUT vs-wid).
                 IF substring(vs-len,1,1) = "-" THEN vs-len = SUBSTRING(vs-len,2).
                 IF substring(vs-wid,1,1) = "-" THEN vs-len = SUBSTRING(vs-wid,2).
                 IF substring(vs-dep,1,1) = "-" THEN vs-len = SUBSTRING(vs-dep,2).
                 assign v-size[1] = TRIM(vs-wid) + " x " + trim(vs-len).

                 RUN sys/inc/dec-frac.p (ef.nsh-len,64,OUTPUT vs-len).
                 RUN sys/inc/dec-frac.p (ef.nsh-wid,64,OUTPUT vs-wid).
                 IF substring(vs-len,1,1) = "-" THEN vs-len = SUBSTRING(vs-len,2).
                 IF substring(vs-wid,1,1) = "-" THEN vs-len = SUBSTRING(vs-wid,2).
                 /*task# 09260501*/
                 assign v-size[2] = IF lv-jobcard-int = 1 THEN trim(vs-len) + " x " + TRIM(vs-wid)
                                    ELSE trim(vs-wid) + " x " + TRIM(vs-len).

                 IF est.est-type <> 4 THEN v-sht-qty = (v-ord-qty / v-tot-up) + (v-ord-qty / v-tot-up * v-over-pct / 100).
                 ELSE v-sht-qty = (v-yld-qty / v-tot-up) + (v-yld-qty / v-tot-up * v-over-pct / 100).

                 v-prt-sht = v-sht-qty / ef.n-out-l.

               display "<FGCOLOR=RED>BOARD<FGCOLOR=BLACK>" wrk-sheet.brd-dscr 
                       "<FGCOLOR=RED>CAL<FGCOLOR=BLACK>" AT 60 wrk-sheet.cal FORM ">>>>>.999<<"  
                       "  <FGCOLOR=RED>MILL<FGCOLOR=BLACK>" AT 115 tt-keyst.tt-mill FORM "x(25)" SKIP
                       "<FGCOLOR=RED>SHEET SIZE<FGCOLOR=BLACK>" v-size[1]  FORM "X(20)"
                       "<FGCOLOR=RED>CUT TO<FGCOLOR=BLACK>" AT 60
                        v-size[2] FORM "x(20)"
                       "<FGCOLOR=RED><C51>PRINT SHEETS<C58><FGCOLOR=BLACK>" v-prt-sht SKIP
                       "<FGCOLOR=RED><C51>CUT SHEETS<C58><FGCOLOR=BLACK>" v-sht-qty
                    with stream-io width 250 no-labels no-box frame sheet.
               x = 1.
             end. /* each wrk-sheet */
             /*if x ne 2 then put v-fill at 1 skip.*/
             PUT SKIP(1)
                 "<FGCOLOR=RED>SHEETS REC'D _______________________________  SHEETS REQ'D <FGCOLOR=BLACK>" tt-keyst.tt-sheets-req FORM ">>>,>>9"  
                 SKIP
                 v-fill78 SKIP.
                
             ASSIGN
             x = 2
             i = 1
             v-ink1 = ""
             v-ink2 = "".
             
             for each wrk-ink WHERE wrk-ink.form-no = eb.form-no
                break by wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no
                       :
                IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.
                IF FIRST-OF(wrk-ink.i-code) THEN ASSIGN v-item[i] = ""
                                                        v-i-qty = 0.
                IF FIRST-OF(wrk-ink.blank-no) THEN v-ink-use-per-blank = 0.

                ASSIGN
                v-item[i] = IF LOOKUP(string(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ","
                v-ink-use-per-blank = v-ink-use-per-blank + 1.
               
                IF LAST-OF(wrk-ink.i-code) OR v-ink-use-per-blank > 1 THEN DO: 
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
                   v-i-qty = 0.
                   FOR EACH bf-ink WHERE bf-ink.form-no = wrk-ink.form-no
                                     AND bf-ink.i-pass = wrk-ink.i-pass
                                     AND bf-ink.i-code = wrk-ink.i-code:
                       v-i-qty = v-i-qty + bf-ink.i-qty.
                   END.
                   IF wrk-ink.i-pass = 1 THEN
                       ASSIGN v-ink1[i] = /*STRING(wrk-ink.i-seq,">9") + "  " + "1  " + */
                                          string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(21)") + "  " + trim(v-alloc) 
                                          /*v-item[i]*/
                                          /*+ (IF i = 1 THEN "  " + eb.plate-no ELSE "") */
                              i = i + 1         . 
                    ELSE IF wrk-ink.i-pass = 2 THEN
                       ASSIGN v-ink2[i] = /*STRING(wrk-ink.i-seq,">9") + "  " + "2  " + */
                                   string(wrk-ink.i-code,"X(15)") + " " + 
                                   string(wrk-ink.i-dscr,"x(21)") + "  " + trim(v-alloc)
                                   /*STRING(v-i-qty,">>>,>>9") /*v-item[i]*/              */
                              i = i + 1.           
                END. 
              
                delete wrk-ink.
             end. /* each wrk-ink */
             ASSIGN
             v-skip = NO
             v-plate-printed = NO.
             PUT "<#5>" .
             DO j = 1 TO 8:
                IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
                IF v-ink1[j] <> "" THEN
                   PUT  v-ink1[j] FORM "x(40)" SKIP .
             END.
             
             v-skip = NO.
             DO j = 1 TO 8:
                 IF TRIM(v-ink2[j]) = "-" THEN v-ink2[j] = "".                 
                IF v-ink2[j] <> "" THEN
                   PUT v-ink2[j] FORM "x(40)" SKIP.
             END.             
             
             PUT "<FGCOLOR=RED><=5><C41> DIGITAL FILE<FGCOLOR=BLACK> " tt-keyst.tt-negs FORM "x(25)" SKIP
                  "<FGCOLOR=RED><C41> PLATES<FGCOLOR=BLACK> " tt-keyst.tt-plate2 FORM "x(25)" /*eb.plate-no*/ SKIP
                  "<FGCOLOR=RED><C41> DIES<FGCOLOR=BLACK> " tt-keyst.tt-die2 FORM "x(25)" /*eb.die-no*/ SKIP
                  "<FGCOLOR=RED><C41> COPY<FGCOLOR=BLACK> " tt-keyst.tt-COPY FORM "x(25)" SKIP
                  "<FGCOLOR=RED><C41> SIZE & STYLE<FGCOLOR=BLACK> " tt-keyst.tt-size FORM "x(25)" /* v-size[1] eb.style*/ SKIP(1)
                 "<FGCOLOR=RED><C41> PLATES<FGCOLOR=BLACK> " SKIP
                 "<C45>" tt-keyst.tt-plate[1] FORM "x(25)" SKIP
                 "<C45>" tt-keyst.tt-plate[2] FORM "x(25)" SKIP
                 "<FGCOLOR=RED><C41> DIES<FGCOLOR=BLACK>" SKIP
                 "<C45>" tt-keyst.tt-die[1] FORM "x(25)" SKIP  
                 "<C45>" tt-keyst.tt-die[2] FORM "x(25)"  SKIP
                 v-fill78  SKIP.
                 
             
             PUT "<FGCOLOR=RED>-------------------- ITEM DESCRIPTION --------------------<FGCOLOR=BLACK>" AT 20 SKIP
                 V-FILL78  SKIP.
             FOR EACH bf-eb WHERE bf-eb.company = eb.company
                              AND bf-eb.est-no = eb.est-no
                              AND bf-eb.form-no = eb.form-no 
                            NO-LOCK BY bf-eb.blank-no:

                find first style where style.company eq eb.company
                      and style.style   eq bf-eb.style no-lock no-error.
                if avail style then v-stypart = style.dscr.

                find first oe-ordl where oe-ordl.company eq job-hdr.company
                      and oe-ordl.ord-no  eq job-hdr.ord-no
                      and oe-ordl.job-no  eq job-hdr.job-no
                      and oe-ordl.job-no2 eq job-hdr.job-no2
                      and oe-ordl.i-no    eq bf-eb.stock-no /*job-hdr.i-no*/
                      no-lock no-error.
                v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE v-po-no.
                IF AVAIL oe-ordl THEN
                FOR EACH oe-rel
                    WHERE oe-rel.company EQ oe-ordl.company
                      AND oe-rel.ord-no  EQ oe-ordl.ord-no
                      AND oe-rel.i-no    EQ oe-ordl.i-no
                      AND oe-rel.line    EQ oe-ordl.line
                    NO-LOCK BY oe-rel.rel-date:
                    FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                           AND oe-rell.r-no  EQ oe-rel.link-no
                           AND oe-rell.ord-no   EQ oe-rel.ord-no
                           AND oe-rell.i-no     EQ oe-rel.i-no
                           AND oe-rell.line     EQ oe-rel.line
                           AND oe-rell.rel-no   EQ oe-rel.rel-no
                           AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                           AND oe-rell.po-no    EQ oe-rel.po-no NO-LOCK NO-ERROR.
                   IF NOT AVAIL oe-rell THEN
                      FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
                           AND oe-rell.ord-no   EQ oe-rel.ord-no
                           AND oe-rell.i-no     EQ oe-rel.i-no
                           AND oe-rell.line     EQ oe-rel.line
                           AND oe-rell.rel-no   EQ oe-rel.rel-no
                           AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                           AND oe-rell.po-no    EQ oe-rel.po-no NO-LOCK NO-ERROR.
                   IF AVAIL oe-rell THEN 
                      FIND oe-relh WHERE oe-relh.company = oe-rel.company
                                     AND oe-relh.r-no = oe-rell.r-no NO-LOCK NO-ERROR.
                    v-del-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
                    LEAVE.
                END.

                v-shipto = "".
                IF AVAIL oe-ordl THEN
                    find first oe-rel where oe-rel.company eq cocode
                         and oe-rel.ord-no  eq oe-ordl.ord-no
                         and oe-rel.i-no    eq oe-ordl.i-no
                         and oe-rel.line    eq oe-ordl.line
                         no-lock no-error.
                if avail oe-rel then do:
                    find first shipto where shipto.company eq cocode
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
                FIND first xjob-hdr where xjob-hdr.company eq cocode
                                      and xjob-hdr.job     eq job-hdr.job
                                      and xjob-hdr.job-no  eq job-hdr.job-no
                                      and xjob-hdr.job-no2 eq job-hdr.job-no2
                                      AND xjob-hdr.frm EQ bf-eb.form-no
                                      and xjob-hdr.i-no    eq bf-eb.stock-no NO-LOCK NO-ERROR.

                ASSIGN
                 v-dsc[1]  = IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE bf-eb.part-dscr1
                 v-dsc[2]  = IF AVAIL oe-ordl THEN oe-ordl.part-dscr1 ELSE bf-eb.part-dscr2
                 v-dsc[3]  = IF AVAIL oe-ordl THEN oe-ordl.part-dscr2 ELSE ""
                 v-size[2] = bf-eb.i-coldscr
                 v-up      = bf-eb.num-up
                 v-job-qty = IF AVAIL xjob-hdr THEN xjob-hdr.qty ELSE bf-eb.bl-qty
                 ld-len    = bf-eb.len
                 ld-wid    = bf-eb.wid
                 ld-dep    = bf-eb.dep.

                IF ld-metric NE 1 THEN DO:
                  ASSIGN
                   ld-len = ld-len * ld-metric
                   ld-wid = ld-wid * ld-metric
                   ld-dep = ld-dep * ld-metric.

                  {sys/inc/roundup.i ld-len}
                  {sys/inc/roundup.i ld-wid}
                  {sys/inc/roundup.i ld-dep}

                  ASSIGN
                   vs-len = STRING(ld-len,"->>,>>>mm")
                   vs-wid = STRING(ld-wid,"->>,>>>mm")
                   vs-dep = STRING(ld-dep,"->>,>>>mm").
                END.

                ELSE DO:
                  RUN sys/inc/dec-frac.p (ld-len, 64, OUTPUT vs-len).
                  RUN sys/inc/dec-frac.p (ld-wid, 64, OUTPUT vs-wid).
                  RUN sys/inc/dec-frac.p (ld-dep, 64, OUTPUT vs-dep).

                  IF SUBSTR(vs-len,1,1) = "-" THEN vs-len = SUBSTR(vs-len,2).
                  IF SUBSTR(vs-wid,1,1) = "-" THEN vs-wid = SUBSTR(vs-wid,2).
                  IF SUBSTR(vs-dep,1,1) = "-" THEN vs-dep = SUBSTR(vs-dep,2).
                END.

                v-size[1] = TRIM(vs-len) + " x " + TRIM(vs-wid) + " x " + TRIM(vs-dep).

                IF LINE-COUNTER > 56 THEN DO:
                   PAGE.
                   put  "<FGCOLOR=RED><R3><C78><FROM><R65><C78><LINE><||6> "
                        "<R3><C79>S.O."
    "<R12><C79>CUST"
    "<R28><C79>VIA:"
    "<R29><C79>K"
    "<R30><C79>P/B"
    "<R31><C79>N/C"
    "<R32><C79>COLL"
    "<R34><C79>3RD"
    "<R36><C78><FROM><R36><C82><LINE><||6>"
    "<R36><C79>EX."
    "<R37><C79>CHGS"
    "<R39><C79>YES"
    "<R40><C79><FROM><R41><C81><RECT><||6>"
    "<R43><C79>NO"
    "<R44><C79><FROM><R45><C81><RECT><||6>"
    "<R50><C78><FROM><R50><C82><LINE><||6>"
    "<R50><C79>INIT"
    "<R55><C78><FROM><R55><C82><LINE><||6>"
    "<R55><C79>DATE<FGCOLOR=BLACK>"
    "<=1>" .
                    VIEW FRAME head.
                 END.

                 FIND FIRST tt-key2
                     WHERE tt-key2.tt-job-no  EQ job-hdr.job-no
                       AND tt-key2.tt-job-no2 EQ job-hdr.job-no2
                       AND tt-key2.tt-frm     EQ int(tt-reftable.val[12])
                       AND tt-key2.tt-i-no    EQ bf-eb.stock-no
                       AND tt-key2.tt-blank   EQ bf-eb.blank-no
                     NO-LOCK NO-ERROR.                 
                 IF NOT AVAIL tt-key2 THEN CREATE tt-key2.                                       
                 V-PRT-UP = v-up * ef.n-out-l.
                 PUT "<#6>"
                     "QTY: " v-job-qty FORM ">>>>,>>9" "   PRINT " v-prt-up FORM ">>>9" ", CUT " v-up FORM ">>>9" " UP" "<C38>" "SIZE: " /*AT 47 */ "<C43>" v-size[1] FORM "x(35)"  SKIP
                     "DESC:  " v-dsc[1] FORM "x(30)"  "<C38>" "STYLE:  " v-stypart SKIP
                     v-dsc[2] AT 7    FORM "x(30)"  "<C45>" tt-key2.tt-style[1] FORM "x(25)" SKIP
                     v-dsc[3] AT 7    FORM "x(30)"  "<C45>" tt-key2.tt-style[2] FORM "x(25)" SKIP
                     "PLATE#:  " bf-eb.plate-no "<C38>" "DIE#:  " "<C43>" bf-eb.die-no FORMAT "x(20)" SKIP
                     "CUSTOMER PART#:  " bf-eb.part-no  SKIP
                     "PO:  " v-po-no       "<C38>" "PRTD:  " bf-eb.i-coldscr SKIP
                     "DEL:  " v-del-date      SKIP
                     "SHIP TO:  " v-shipto[1]   
                      SKIP
                     v-shipto[2] AT 8 
                     "<C38>" "PACK:  "  bf-eb.cas-cnt "        PER CASE:  " bf-eb.cas-no  SKIP
                      v-shipto[3] AT 8  "<C38>PACK: " bf-eb.cas-pal FORM ">>9" "<C47>PER PALLET" SKIP       /*Task# 10181304 */
                      v-shipto[4] AT 8  "<C38>AQL:  "  tt-key2.tt-aql FORM "x(25)"
                     SKIP
                     "FG ITEM:  " bf-eb.stock-no SKIP
                     v-fill2 SKIP.
                 
                 IF AVAIL tt-key2 THEN DELETE tt-key2.
              END.    /* for each bf-eb*/
                 
              IF AVAIL tt-keyst THEN DELETE tt-keyst.
          END. /* last-of(eb.form-no) */
          
        end. /* each eb */
      end. /* each ef */
      end. /* first job-no */

      if last-of(tt-reftable.val[12]) then do:
        /** PRINT JOB INSTRUCTIONS **/
        /* dept notes*/
        IF LINE-COUNTER > 62 THEN DO:
            PAGE.
            put  "<FGCOLOR=RED><R3><C78><FROM><R65><C78><LINE><||6> "
                 "<R3><C79>S.O."
                 "<R12><C79>CUST"
                 "<R28><C79>VIA:"
                 "<R29><C79>K"
                 "<R30><C79>P/B"
                 "<R31><C79>N/C"
                 "<R32><C79>COLL"
                 "<R34><C79>3RD"
                 "<R36><C78><FROM><R36><C82><LINE><||6>"
                 "<R36><C79>EX."
                 "<R37><C79>CHGS"
                 "<R39><C79>YES"
                 "<R40><C79><FROM><R41><C81><RECT><||6>"
                 "<R43><C79>NO"
                 "<R44><C79><FROM><R45><C81><RECT><||6>"
                 "<R50><C78><FROM><R50><C82><LINE><||6>"
                 "<R50><C79>INIT"
                 "<R55><C78><FROM><R55><C82><LINE><||6>"
                 "<R55><C79>DATE<FGCOLOR=BLACK>"
                 "<=1>" .
            VIEW FRAME head.
        END.
        
        lv-line-chars = 80.
        FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
        {custom/notespr5.i job v-inst2 20 "notes.rec_key = job.rec_key and notes.note_code <> '' AND (notes.note_form_no EQ tt-reftable.val[12] OR notes.note_form_no EQ 0) AND LOOKUP(notes.note_code,v-exc-depts) EQ 0"}
        DO i = 1 TO 20:
           v-dept-inst[i] = v-inst2[i].
        END.
        
        DO i = 1 TO 20:
            IF LINE-COUNTER > 62 AND v-dept-inst[i] <> "" THEN DO:
               PAGE.
               put  "<FGCOLOR=RED><R3><C78><FROM><R65><C78><LINE><||6> "
                 "<R3><C79>S.O."
                 "<R12><C79>CUST"
                 "<R28><C79>VIA:"
                 "<R29><C79>K"
                 "<R30><C79>P/B"
                 "<R31><C79>N/C"
                 "<R32><C79>COLL"
                 "<R34><C79>3RD"
                 "<R36><C78><FROM><R36><C82><LINE><||6>"
                 "<R36><C79>EX."
                 "<R37><C79>CHGS"
                 "<R39><C79>YES"
                 "<R40><C79><FROM><R41><C81><RECT><||6>"
                 "<R43><C79>NO"
                 "<R44><C79><FROM><R45><C81><RECT><||6>"
                 "<R50><C78><FROM><R50><C82><LINE><||6>"
                 "<R50><C79>INIT"
                 "<R55><C78><FROM><R55><C82><LINE><||6>"
                 "<R55><C79>DATE<FGCOLOR=BLACK>"
                 "<=1>" .
               VIEW FRAME head.
           END.
           PUT "  " v-dept-inst[i] FORM "x(80)"  SKIP.
        END.
      end. /* last-of tt-reftable.val[12] */

      /** PRINT MULT COPIES OF TICKETS **/
      save_id = recid(job-hdr).
      if last-of(job-hdr.job-no2) then do:
        for each wrk-op:
          delete wrk-op.
        end.
        for each wrk-prep:
          delete wrk-prep.
        end.
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

/* end ---------------------------------- copr. 1994  advanced software, inc. */
