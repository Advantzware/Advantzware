/* cerep/jobcarded.p   Xprint FC Factory  Ticket for Carded */
/* -------------------------------------------------------------------------- */

def input parameter v-format like sys-ctrl.char-fld.
DEF STREAM st-st.

DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   v-dir = users.user_program[2] + "\".
ELSE
   v-dir = "c:\tmp\".

OUTPUT STREAM st-st TO value(v-dir + "job100.txt").

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

def var v-line as int init 1 no-undo.
def var v-gsh-qty as int no-undo.
def var cnt as int init 1 no-undo.
def var v-frm-blk as char format "x(6)" no-undo.
def var v-dec as dec no-undo.
def var v-ovund as char format "x(34)" no-undo.
DEF VAR v-over% LIKE oe-ord.over-pct NO-UNDO.
DEF VAR v-under% LIKE oe-ord.under-pct NO-UNDO.
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
DEF VAR v-fill2 AS cha INIT "-" FORM "x(130)" NO-UNDO.
def var v-fill3 as char format "x(128)" NO-UNDO.
DEF VAR li AS INT NO-UNDO.

def TEMP-TABLE w-lo NO-UNDO
  field layout like v-layout.

def new shared buffer xjob-hdr for job-hdr.
DEF var v-dept-note AS cha FORM "x(48)" EXTENT 50 NO-UNDO.
def buffer b-eb for eb.
def buffer b-ef for ef.
DEF BUFFER bf-item FOR ITEM.
DEF BUFFER bx-job-hdr FOR job-hdr.

DEF VAR v-ord-no AS INT NO-UNDO.

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
  FIELD spoil LIKE job-mch.wst-prct EXTENT 20
  FIELD mr-waste LIKE job-mch.mr-waste EXTENT 20    .

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

DEF SHARED VAR s-prt-set-header AS LOG NO-UNDO.  
DEF SHARED VAR s-prt-label AS LOG NO-UNDO.
    
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 70 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(80)" EXTENT 70 NO-UNDO.
DEF VAR v-spec-inst AS cha FORM "x(80)" EXTENT 10 NO-UNDO.
DEF VAR v-note-length AS INT INIT 80 NO-UNDO.

DEF VAR v-start-date AS DATE NO-UNDO.
DEF VAR v-req-date AS DATE NO-UNDO.
DEF VAR v-shipto AS cha FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-case-size AS cha NO-UNDO.
DEF VAR v-sht-size AS cha NO-UNDO.
DEF VAR v-die-size AS cha NO-UNDO.
DEF VAR v-vend LIKE po-ord.vend-no NO-UNDO.
DEF VAR v-item AS cha EXTENT 50 NO-UNDO.
DEF VAR v-ink1 AS cha EXTENT 50 NO-UNDO.
DEF VAR v-ink2 AS cha EXTENT 50 NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no NO-UNDO.
DEF VAR lv-mat-dept-list AS cha INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach AS cha NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF VAR v-fgitm AS cha FORM "x(15)" EXTENT 30 NO-UNDO.
DEF VAR v-fgdsc LIKE eb.part-dscr1 EXTENT 30 NO-UNDO.
DEF VAR v-fgqty LIKE job-hdr.qty EXTENT 30 FORM ">>,>>>,>>>" NO-UNDO.
DEF VAR v-pono LIKE oe-ordl.po-no EXTENT 30 NO-UNDO.
DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF TEMP-TABLE tt-fgitm NO-UNDO FIELD i-no AS cha FORM "x(15)"
                        FIELD seq AS INT
                        FIELD qty AS INT 
                        FIELD i-dscr AS cha
                        FIELD po-no AS cha
                        FIELD ord-no AS INT
                        FIELD cust-name AS cha
                        FIELD shipto AS cha EXTENT 4.
DEF VAR v-board-po LIKE oe-ordl.po-no-po NO-UNDO.
DEF VAR v-plate-printed AS LOG NO-UNDO.
DEF BUFFER xoe-ordl FOR oe-ordl.
DEF BUFFER bf-fg-bin FOR fg-bin.
DEF VAR v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name2 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name3 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-last-j AS INT NO-UNDO.
DEF VAR v-po-no2 LIKE v-po-no NO-UNDO.
DEF VAR v-po-no3 LIKE v-po-no NO-UNDO.
DEF VAR v-spc-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-ord-qty AS INT NO-UNDO.
DEF VAR v-stock-no LIKE eb.stock-no NO-UNDO.
DEF VAR v-stock-no2 LIKE eb.stock-no NO-UNDO.
DEF VAR v-have-note AS LOG NO-UNDO.
DEF VAR v-shipvia      LIKE carrier.dscr NO-UNDO.
DEF TEMP-TABLE tt-size NO-UNDO FIELD frm LIKE job-hdr.frm
                       FIELD blank-no LIKE eb.blank-no
                       FIELD cad# LIKE eb.cad-no FORM "x(10)"
                       FIELD cad-size AS cha FORM "x(25)"
                       FIELD COUNT LIKE eb.cas-cnt
                       FIELD vend-part AS cha FORM "x(30)"
                       FIELD seq AS INT
                       INDEX tt-size frm seq.
DEF VAR v-tt-seq AS INT NO-UNDO.
DEF VAR v-item-name LIKE itemfg.i-name NO-UNDO.
DEF VAR v-cat AS cha NO-UNDO.
{custom/formtext.i NEW}
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR lv-note-cnt AS INT NO-UNDO.
DEF VAR v-cas-no AS cha NO-UNDO.
DEF BUFFER bf-jobmat FOR job-mat.
ASSIGN
v-fill = "<||3><C1><FROM><C108><LINE><||3>"
v-fill2 = FILL("-",160)
v-fill3 = "<C2><FROM><C108><LINE><||3><R-1>".

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
DEF BUFFER bf-jobhdr FOR job-hdr.

DEF VAR v-cust-no AS cha NO-UNDO.
DEF VAR v-cname AS cha EXTENT 3 NO-UNDO.
DEF VAR v-shiptolabel AS cha EXTENT 12 NO-UNDO.
DEF VAR v-job-cnt AS INT NO-UNDO.
DEF VAR v-prev-job AS cha NO-UNDO.

DEF TEMP-TABLE tt-ink NO-UNDO
    FIELD i-code LIKE wrk-ink.i-code
    FIELD i-seq LIKE wrk-ink.i-seq.

DEF TEMP-TABLE tt-reftable NO-UNDO LIKE reftable
    FIELD est-type LIKE est.est-type.

DEF VAR lv-cad-image AS cha NO-UNDO.
DEF VAR lv-cad-image-list AS cha NO-UNDO.
DEF VAR v-case-due-date AS DATE NO-UNDO.
DEF VAR v-hg AS cha NO-UNDO.
DEF VAR v-net-size AS cha NO-UNDO.

format header
       "<TOP=10mm><B>  JOB NUMBER:<P18><R-0.3><P14>" v-job-no space(0) "-" space(0) v-job-no2 format "99" "</B><P10><R+0.3>"
       "<C30><B><P12>Carded Graphics, LLC Factory Ticket <P11>"  "<C77>JOB START DATE:"  v-start-date /*"  PRINTED DATE:" TODAY*/  "</B><P10>" skip
       v-fill
    with no-box frame head no-labels stream-io width 300 PAGE-TOP.

format "  Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
       "Salesman:" at 90 oe-ord.sname[1] "Order#:" at 138 oe-ord.ord-no
    with no-box frame line-head no-labels stream-io width 162.
    
{sys/inc/notes.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

ASSIGN
 v-job[1]    = fjob-no
 v-job[2]    = tjob-no
 v-job2[1]   = fjob-no2
 v-job2[2]   = tjob-no2
 v-reprint   = reprint
 v-spec-list = spec-list.

/* build tt-reftable */
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
        no-lock

        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY job-hdr.frm:

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
/* end of building tt-reftable */

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
        no-lock

        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY job-hdr.frm:

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
        IF AVAIL xjob-hdr THEN DO:
            ASSIGN
             xjob-hdr.ftick-prnt = YES
             li                  = 1000.
            FIND xjob-hdr NO-LOCK
                WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
                NO-ERROR NO-WAIT.

        END.
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

    if first-of(job-hdr.job-no2) then v-first = yes.
    /** PRINT JOB HEADER **/
    if v-first then do:   
       assign
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2.

       if avail oe-ord AND not oe-ctrl.p-fact and (oe-ord.stat eq "H" OR oe-ord.priceHold) then next.
       ASSIGN
       v-due-date = if avail oe-ord then oe-ord.due-date else ?
       v-start-date = IF AVAIL oe-ord THEN oe-ord.ord-date ELSE job-hdr.start-date.
       
       if not first(job-hdr.job-no) then page.
       ELSE PUT SKIP
            "<TOP=10mm><B>  JOB NUMBER:<P18><R-0.3>" v-job-no space(0) "-" space(0) v-job-no2 format "99" "</B><P10><R+0.3>"
            "<C30><B><P12>Carded Graphics, LLC Factory Ticket <P11>"  "<C77>JOB START DATE:" v-start-date "</B><P10>" skip
            v-fill SKIP.
       view frame head.
       VIEW FRAME bott.

       /* print all customeres if it's more than one*/
FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
             BREAK BY bf-jobhdr.ord-no:
    IF FIRST-OF(bf-jobhdr.ord-no) THEN DO:
    
       v-shipto = "".
       find first oe-ordl
           where oe-ordl.company eq job-hdr.company
             and oe-ordl.ord-no  eq bf-jobhdr.ord-no
             and oe-ordl.job-no  eq job-hdr.job-no
             and oe-ordl.job-no2 eq job-hdr.job-no2
             and oe-ordl.i-no    eq bf-jobhdr.i-no
           no-lock no-error.

       IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
       find first oe-ordl
           where oe-ordl.company eq job-hdr.company
             and oe-ordl.ord-no  eq bf-jobhdr.ord-no
             and oe-ordl.i-no    eq bf-jobhdr.i-no
           no-error.

       IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
       find first oe-ordl
           where oe-ordl.company eq job-hdr.company
             and oe-ordl.ord-no  eq bf-jobhdr.ord-no
           no-error.

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
       
       if avail oe-ordl then find first oe-ord of oe-ordl NO-LOCK .
       ASSIGN
       v-ovund = IF AVAIL oe-ordl THEN 
                               trim(string(oe-ordl.over-pct,">>9.99")) + "/" +
                               trim(string(oe-ordl.under-pct,">>9.99"))
                 ELSE ""              
       v-over% = IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE 0
       v-under% = IF AVAIL oe-ordl THEN oe-ordl.under-pct ELSE 0
       v-req-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?.
       FIND FIRST cust WHERE cust.company = job-hdr.company AND
                              cust.cust-no = bf-jobhdr.cust-no NO-LOCK NO-ERROR.
       v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                     ELSE IF AVAIL cust THEN cust.name
                     ELSE bf-jobhdr.cust-no.
       FIND FIRST eb WHERE eb.company = est.company
                       AND eb.est-no = est.est-no
                       AND eb.form-no <> 0
                       AND eb.blank-no <> 0 NO-LOCK NO-ERROR.
       v-spc-no = IF AVAIL eb THEN eb.spc-no ELSE "".
       FIND FIRST oe-rel where oe-rel.company eq cocode
                              and oe-rel.ord-no  eq oe-ordl.ord-no
                              no-lock NO-ERROR.
           
       FIND FIRST carrier WHERE carrier.company = oe-rel.company
           AND carrier.carrier  eq oe-rel.carrier NO-LOCK NO-ERROR.
       IF AVAIL carrier 
           THEN ASSIGN v-shipvia = carrier.dscr.
           ELSE ASSIGN v-shipvia = "".
        

       PUT "  <B>CUSTOMER NAME:<P18><R-0.3>" v-cust-name  "<P10><R+0.3>"
            "<B>DUE DATE:         ESTIMATE:       LAYOUT:" AT 85 SKIP
            "  SHIPTO:</B>" v-shipto[1]  v-due-date AT 85  job-hdr.est-no FORMAT "x(8)" AT 100  eb.die-no FORM "x(10)" AT 120
            SKIP
            v-shipto[2] AT 10 SKIP
            v-shipto[3] AT 10 SKIP
            v-shipto[4] AT 10 " <B>SHIP VIA:</B>" v-shipvia FORMAT "x(25)"   "<B>OVERRUN:</B> " AT 87  v-over% "   <B>UNDERRUN:</B> "  v-under% SKIP
            v-fill SKIP.     

    END. /* first-of(bf-jobhdr.ord-no)*/
END. /* for each bf-jobhdr*/

        v-line = if avail est                            and
                 est.est-type gt 2 and est.est-type lt 5 then 500 else 50.


        /* new */

     FOR EACH wrk-ink:
         DELETE wrk-ink.
     END.

     FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                                      BREAK BY bf-jobhdr.frm:
        IF FIRST(bf-jobhdr.frm) THEN
           PUT "  <P10><B>F/B   FG ITEM/PO #    #UP    FORM/JOB QTY  NAME/DESCRIPTION          CARTON SIZE/STYLE        COUNT  CASE     CASE SIZE " /*v-upc-lbl*/ "</B>" SKIP.
               
        IF LAST-OF(bf-jobhdr.frm) THEN DO:        

    FOR  EACH tt-reftable NO-LOCK WHERE tt-reftable.reftable EQ "jc/jc-calc.p"
           AND tt-reftable.company  EQ job-hdr.company
           AND tt-reftable.loc      EQ ""
           AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")
           AND (tt-reftable.est-type <> 4 or
                tt-reftable.val[12] = bf-jobhdr.frm)
         BREAK BY tt-reftable.val[12].
    IF FIRST-OF(tt-reftable.val[12]) THEN DO:
        
        /* build wrk-ink per form  BUILD INK WORK FILE **/
        for each job-mat
              where job-mat.company eq cocode
                and job-mat.job     eq bf-jobhdr.job
                and job-mat.frm     eq int(tt-reftable.val[12])
                no-lock,
              first item
              {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no 
                AND lookup(item.mat-type,"I,V") > 0 no-lock:

            find first wrk-ink where wrk-ink.i-code    eq job-mat.i-no
                no-error.                
            if not avail wrk-ink then do:
              create wrk-ink.
              assign
               wrk-ink.i-code   = job-mat.i-no
               wrk-ink.form-no  = job-mat.frm
               wrk-ink.blank-no = job-mat.blank-no
               wrk-ink.i-dscr   = item.i-name /*est-dscr*/
               wrk-ink.i-pass   = 1.
            end.
            wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.

          end. /* JOB-MAT */
          /* get color info for the board */
          v-ink-seq = 0.            
          for each wrk-ink WHERE wrk-ink.form-no = tt-reftable.val[12] /*bf-jobhdr.frm*/
                break by wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no :

                IF FIRST-OF(wrk-ink.blank-no) THEN v-ink-use-per-blank = 0.
                v-ink-use-per-blank = v-ink-use-per-blank + 1.
                IF first-OF(wrk-ink.i-code) THEN v-ink-seq = v-ink-seq + 1.
                wrk-ink.i-seq = v-ink-seq.
          END.
        /* end of building wrk-ink*/

        /** SUM UP NUMBER OF SHEETS **/
        find first job
            where job.company eq cocode
              and job.job     eq bf-jobhdr.job
              and job.job-no  eq v-job-no
              and job.job-no2 eq v-job-no2
            no-lock no-error.
            
        if avail job then
        for each job-mch
            where job-mch.company eq cocode
              and job-mch.job     eq job.job
              and job-mch.job-no  eq job.job-no
              and job-mch.job-no2 eq job.job-no2
              AND job-mch.frm = int(tt-reftable.val[12]) /*bf-jobhdr.frm*/
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
              and job-prep.job     eq bf-jobhdr.job
              and job-prep.job-no  eq bf-jobhdr.job-no
              and job-prep.job-no2 eq bf-jobhdr.job-no2
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
            AND ef.est-no  EQ bf-jobhdr.est-no
            AND ef.form-no = tt-reftable.val[12] /*bf-jobhdr.frm*/
          break by ef.est-no by ef.form-no:
     
        v-est-qty = 0.
        if est.est-type eq 4 then
        for each eb FIELDS(yld-qty)
            WHERE eb.company  EQ ef.company
              AND eb.est-no   eq ef.est-no
              and eb.stock-no eq bf-jobhdr.i-no
            no-lock:
          v-est-qty = v-est-qty + eb.yld-qty.
        end.

        else v-fac = 1.
        v-itm-printed = 0.
        FOR EACH tt-ink:
            DELETE tt-ink.
        END.
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
             FIND FIRST itemfg WHERE itemfg.company = eb.company
                                 AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
             v-item-name = IF AVAIL itemfg THEN ITEMfg.i-name ELSE v-dsc[1].
             IF eb.blank-no > 0 AND eb.blank-no < 11 THEN 
                 ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
            ASSIGN
            v-upc-lbl = IF s-prt-sellprc THEN "Sell Price" ELSE "   UPC#"
            v-job-qty = 0
            v-stock-no = IF est.est-type >= 2 AND est.est-type <= 3 THEN bf-jobhdr.i-no ELSE eb.stock.
            for each xjob-hdr where xjob-hdr.company eq cocode
               and xjob-hdr.job     eq job-hdr.job
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.job-no2 eq job-hdr.job-no2
               and xjob-hdr.i-no    eq v-stock-no no-lock:
               v-job-qty = v-job-qty + xjob-hdr.qty.
            end.
            /** PRINT ITEM **/
            IF CAN-FIND(first oe-ordl where oe-ordl.company eq job-hdr.company
                          and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                          and oe-ordl.job-no  eq job-hdr.job-no
                          and oe-ordl.job-no2 eq job-hdr.job-no2
                          and oe-ordl.i-no    eq v-stock-no) 
            THEN v-ord-no = bf-jobhdr.ord-no.
            ELSE IF CAN-FIND(first oe-ordl where oe-ordl.company eq job-hdr.company
                               and oe-ordl.job-no  eq job-hdr.job-no
                               and oe-ordl.job-no2 eq job-hdr.job-no2
                               and oe-ordl.i-no    eq v-stock-no) THEN DO:
                 FIND first oe-ordl where oe-ordl.company eq job-hdr.company
                               and oe-ordl.job-no  eq job-hdr.job-no
                               and oe-ordl.job-no2 eq job-hdr.job-no2
                               and oe-ordl.i-no    eq v-stock-no NO-LOCK NO-ERROR.
                 v-ord-no = oe-ordl.ord-no.
            END.
            ELSE v-ord-no = bf-jobhdr.ord-no.

            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq v-ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq v-stock-no /*job-hdr.i-no*/
                no-lock no-error.
            IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                  and oe-ordl.i-no    eq v-stock-no /*job-hdr.i-no*/
                no-lock no-error.
            IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                no-lock no-error.
            if avail oe-ordl then do:
              IF oe-ordl.i-no EQ v-stock-no THEN v-est-qty = oe-ordl.qty.
              find first oe-ord of oe-ordl no-lock.
              v-ovund = trim(string(oe-ordl.over-pct,">>9.99")) + "/" +
                        trim(string(oe-ordl.under-pct,">>9.99")).
            end.
            else v-est-qty = v-job-qty.
            
            release w-lo.
            find first w-lo no-error.
            v-case-size = string(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
                         string(eb.cas-dep).

            ASSIGN vs-len = ""
                   vs-wid = ""
                   vs-dep = "".
            IF eb.cas-len <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-len,32,OUTPUT vs-len).
            IF eb.cas-wid <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-wid,32,OUTPUT vs-wid).
            IF eb.cas-dep <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-dep,32,OUTPUT vs-dep).
            v-case-size = (IF vs-len <> "" THEN trim(vs-len) + "x" ELSE "") +
                          (IF vs-wid <> "" THEN TRIM(vs-wid) + "x" ELSE "") +
                          trim(vs-dep).

            ASSIGN vs-len = ""
                   vs-wid = ""
                   vs-dep = "".
            IF eb.len <> 0 THEN RUN sys/inc/dec-frac.p (eb.len,32,OUTPUT vs-len).
            IF eb.wid <> 0 THEN RUN sys/inc/dec-frac.p (eb.wid,32,OUTPUT vs-wid).
            IF eb.dep <> 0 THEN RUN sys/inc/dec-frac.p (eb.dep,32,OUTPUT vs-dep).
            v-size[1] = (IF vs-len <> "" THEN trim(vs-len) + "x" ELSE "") +
                        (IF vs-wid <> "" THEN TRIM(vs-wid) + "x" ELSE "") +
                        trim(vs-dep).
            IF ef.trim-l <> 0 THEN RUN sys/inc/dec-frac.p (ef.trim-l,32,OUTPUT vs-len).
            IF ef.trim-w <> 0 THEN RUN sys/inc/dec-frac.p (ef.trim-w,32,OUTPUT vs-wid).
            IF ef.trim-d <> 0 THEN RUN sys/inc/dec-frac.p (ef.trim-d,32,OUTPUT vs-dep).
            
            ASSIGN
            v-up = eb.num-up
            v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE ""
            v-spc-no = eb.spc-no
            v-ink-list = "".
            FOR EACH tt-ink:
                DELETE tt-ink.
            END.
            
            do i = 1 to 12:
              if eb.i-code2[i] <> "" then do:
                
                 FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
                                      AND wrk-ink.blank-no = eb.blank-no
                                      AND wrk-ink.i-code = eb.i-code2[i]
                                      AND wrk-ink.i-pass = eb.i-ps2[i]
                                      NO-LOCK BREAK BY wrk-ink.i-code:
                    IF FIRST-OF(wrk-ink.i-code) AND
                       NOT CAN-FIND( FIRST tt-ink WHERE tt-ink.i-code = wrk-ink.i-code) 
                    THEN DO:
                       CREATE tt-ink.
                       ASSIGN tt-ink.i-code = eb.i-code2[i]
                              tt-ink.i-seq = wrk-ink.i-seq.
                    END.
                 END.
              END.
            END.
            FOR EACH tt-ink BREAK BY tt-ink.i-seq:
                v-ink-list =  IF LOOKUP(string(tt-ink.i-seq),v-ink-list) > 0 
                              THEN v-ink-list
                              ELSE v-ink-list + string(tt-ink.i-seq) + ",".
            END.
            IF LENGTH(v-ink-list) > 1 AND
                SUBSTRING(v-ink-list,LENGTH(v-ink-list),1) = "," 
                  THEN v-ink-list = substring(v-ink-list,1,LENGTH(v-ink-list) - 1).                    
            ELSE IF v-ink-list = "," THEN v-ink-list = "".

            v-alloc = v-ink-list.            
            
            if num-entries(v-ink-list) gt 1 then do:
               v-alloc = "".
               do j = 1 to num-entries(v-ink-list):
                  if j eq 1 or j eq num-entries(v-ink-list) THEN v-alloc = v-alloc + entry(j,v-ink-list) + ",".
                  else do:
                       if int(entry(j,v-ink-list)) - int(entry(j - 1,v-ink-list)) le 1 then
                                substr(v-alloc,length(trim(v-alloc)),1) = "-".
                       else do:
                            if substr(v-alloc,length(trim(v-alloc)),1) eq "-" then
                                   v-alloc = v-alloc + entry(j - 1,v-ink-list) + ",".
                            v-alloc = v-alloc + entry(j,v-ink-list) + ",".
                       END.
                  end.
                  
               end.                    
               if v-alloc ne "" then substr(v-alloc,length(trim(v-alloc)),1) = "".
            end.
            /* ==============*/
            ASSIGN
            v-ord-qty = IF AVAIL oe-ordl AND oe-ordl.i-no EQ v-stock-no THEN
                          (IF eb.cust-% > 0 THEN (oe-ordl.qty * eb.cust-%)
                           ELSE oe-ordl.qty)
                        ELSE v-est-qty
            v-job-qty = v-job-qty * (IF eb.cust-% > 0 THEN eb.cust-% ELSE 1)
            v-item-name = IF AVAIL oe-ordl THEN oe-ordl.i-name ELSE v-dsc[1]
            v-dsc[1] = IF AVAIL oe-ordl THEN oe-ordl.part-dscr1 ELSE v-dsc[1].

            FIND FIRST po-ordl WHERE po-ordl.company = job-hdr.company
                                 AND po-ordl.job-no = job-hdr.job-no
                                 AND po-ordl.job-no2 = job-hdr.job-no2
                                 AND po-ordl.i-no = eb.cas-no NO-LOCK NO-ERROR.
            v-case-due-date = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.
            IF eb.est-type eq 4 then v-fac = eb.yld-qty / v-est-qty.
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
            v-prev-job = "".
            FOR EACH bf-fg-bin NO-LOCK where bf-fg-bin.company eq eb.company
                                       and bf-fg-bin.i-no    eq eb.stock-no
                     BY bf-fg-bin.job-no BY bf-fg-bin.job-no2 :
                IF bf-fg-bin.job-no = job-hdr.job-no AND
                   bf-fg-bin.job-no2 = job-hdr.job-no2 THEN .
                ELSE v-prev-job = v-prev-job + trim(bf-fg-bin.job-no) + ",".
            END.
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
                    trim(string(eb.cas-len) + "x" + string(eb.cas-wid) + "x" + string(eb.cas-dep))
                    /*v-cas-no WHEN v-cas-no <> "" @ eb.cas-no
                    v-case-due-date*/
                    skip
                    v-po-no FORM "x(15)" AT 9
                    v-est-qty FORM  "->>,>>>,>>9"         AT 28 SPACE(7)
                    v-dsc[1] FORM "x(25)"  
                    /*"PREV:"  v-prev-job FORM "x(21)"*/ v-stypart FORM "x(20)"
                    v-job-cnt FORM ">>>>9" AT 97
                    
                with stream-io width 150 no-labels no-box frame line-det1.

            v-itm-printed = v-itm-printed + 1.    
        END. /* eb */
      END.   /*ef */
       IF NOT LAST(tt-reftable.val[12]) 
             OR tt-reftable.est-type = 4  THEN  put v-fill3  skip.
    END. /*first-of(tt-reftable.val[12]*/
    END. /*tt-reftable*/
        
        END. /* last-of(bf-jobhdr.frm) */
     END. /* each bf-jobhdr*/
     PUT v-fill SKIP.
     /* Print Board*/
     PUT "<P10>"
         "<B>F#  BOARD CODE                SHEETS    HOUSE SIZE         HG   MIN SHEET SIZE      CAD #      DUE DATE    VEN NAME </B>" 
         SKIP.

     FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                             BREAK BY bf-jobhdr.frm:
        IF LAST-OF(bf-jobhdr.frm) THEN DO:
           FOR  EACH tt-reftable NO-LOCK WHERE tt-reftable.reftable EQ "jc/jc-calc.p"
                AND tt-reftable.company  EQ job-hdr.company
                AND tt-reftable.loc      EQ ""
                AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")
               AND (tt-reftable.est-type <> 4 or
                tt-reftable.val[12] = bf-jobhdr.frm)
                   BREAK BY tt-reftable.val[12].
        IF FIRST-OF(tt-reftable.val[12]) THEN DO:
          for each ef
          WHERE ef.company EQ job-hdr.company
            AND ef.est-no  EQ bf-jobhdr.est-no
            AND ef.form-no = tt-reftable.val[12]
          break by ef.est-no by ef.form-no:
         v-up = 0.
         for each eb
            WHERE eb.company     EQ ef.company
              AND eb.est-no      eq ef.est-no
              and eb.form-no     eq ef.form-no NO-LOCK
            break by eb.form-no BY eb.blank-no:
        
            v-up = v-up + eb.num-up.
            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".

          /* end. /* last-of(eb.form-no) */      */
          IF LAST-OF(eb.form-no) THEN DO:
          
          find first oe-ordl
                where oe-ordl.company eq bf-jobhdr.company
                  and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                  and oe-ordl.job-no  eq bf-jobhdr.job-no
                  and oe-ordl.job-no2 eq bf-jobhdr.job-no2
                  and oe-ordl.i-no    eq bf-jobhdr.i-no
                no-lock no-error.
          IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
          find first oe-ordl
                where oe-ordl.company eq bf-jobhdr.company
                  and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                  and oe-ordl.i-no    eq bf-jobhdr.i-no
                no-lock no-error.
          IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
          find first oe-ordl
                where oe-ordl.company eq bf-jobhdr.company
                  and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                no-lock no-error.
             /* Number of sheets */
             run oe/rep/ticket2.p (recid(ef), recid(job-hdr)).
             find first wrk-sheet where recid(wrk-sheet) eq save_id.
             IF AVAIL oe-ordl THEN 
               FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                            AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
             ASSIGN
             v-vend = IF AVAIL oe-ordl THEN oe-ordl.vend-no ELSE ""
             v-board-po = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0.
             IF AVAIL po-ord THEN
                FIND FIRST po-ordl WHERE
                     po-ordl.company EQ po-ord.company AND
                     po-ordl.po-no   EQ po-ord.po-no AND
                     po-ordl.i-no = ef.board
                     NO-LOCK NO-ERROR.

             v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.   
            /** PRINT SHEET **/
             for each wrk-sheet WHERE wrk-sheet.form-no = tt-reftable.val[12] break by wrk-sheet.form-no:
                 ASSIGN vs-len = ""
                        vs-wid = ""
                        vs-dep = "".
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
                    ASSIGN v-hg = "<B>YES</B>" 
                         v-net-size = string(ef.nsh-len) + "x" + string(ef.nsh-wid).
                 ELSE ASSIGN v-hg = "   "
                             v-net-size = string(ef.nsh-wid) + "x" + string(ef.nsh-len).
                 
               PUT UNFORMATTED  wrk-sheet.form-no  FORM ">9" " "
                    " " wrk-sheet.brd-dscr  FORM "x(22)" " "
                       /*v-cat FORM "x(5)"*/
                    wrk-sheet.gsh-qty  FORM ">,>>>,>>9" "    "
                    v-sht-size  format "x(16)" "   "
                    v-hg  "  "
                    v-net-size  FORM "x(16)" "   "
                    " "
                    eb.cad-no  FORM "x(10)" " " 
                    v-po-duedate  FORM "99/99/9999" " " 
                    v-vend   FORM "x(10)" " "         SKIP
                    /*v-die-size FORM "x(16)" AT 78*/
                    SKIP.
             end. /* each wrk-sheet */
          END. /* last-of(eb.form*/
          END. /* eb */
           END. /* ef */  
          IF NOT LAST(tt-reftable.val[12]) 
             OR tt-reftable.est-type = 4  THEN  put v-fill3  skip.
       END. /*first-of(tt-reftable.val[12] */
        END.  /* tt-reftable*/   

        END. /* last-of(bf-jobhdr.frm) */
        END.  /* bf-jobhdr */

        PUT v-fill AT 1 SKIP.     
          /** PRINT INK **/
        PUT "<B>INK CODE           INK NAME                        LBS           INK CODE           INK NAME                        LBS </B>"
              SKIP.

      ASSIGN
         v-ink1 = ""
         v-ink2 = "".

      for each wrk-ink WHERE /*wrk-ink.form-no = eb.form-no*/
                break /*by wrk-ink.i-pass*/
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no:

                IF FIRST(wrk-ink.i-code) THEN ASSIGN i = 1
                                                        v-item[i] = ""
                                                        v-i-qty = 0.
                                                        
                IF FIRST-OF(wrk-ink.blank-no) THEN v-ink-use-per-blank = 0.

                v-ink-use-per-blank = v-ink-use-per-blank + 1.
                IF first-OF(wrk-ink.i-code) /*OR v-ink-use-per-blank > 1 */ THEN DO: 
                   v-i-qty = 0.
                   FOR EACH bf-ink WHERE bf-ink.form-no = wrk-ink.form-no 
                                     /*AND bf-ink.i-pass = wrk-ink.i-pass*/
                                      AND bf-ink.i-code = wrk-ink.i-code:
                       v-i-qty = v-i-qty + bf-ink.i-qty.
                   END.
         
                   ASSIGN v-ink1[i] = string(wrk-ink.i-code,"X(15)") + " " + 
                                      string(wrk-ink.i-dscr,"x(29)") + "  " +
                                      STRING(v-i-qty,">>>,>>9")
                          i = i + 1. 
                END. 
                /*delete wrk-ink.*/
      end. /* each wrk-ink */
      v-skip = NO.
      DO j = 1 TO 24:
         IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
         IF v-ink1[j] <> "" THEN do:
            IF v-skip THEN do:
                PUT  v-ink1[j] FORM "x(65)" .
                PUT SKIP.
            END.
            ELSE PUT v-ink1[j] FORM "x(65)" .                                                             
            v-skip = NOT v-skip.             
         END.
      END.
      put v-fill at 1 skip.
      IF s-run-speed THEN
           PUT "<B>FORM MACHINE             MR WASTE   MR HRS  RUN SPEED   SPOIL%     SHEETS        DATE       FINISHED SHEETS/CARTONS "
                "</B>"     
                SKIP(1).
      else PUT "<B>FORM MACHINE             MR WASTE   MR HRS   RUN HOUR    SPOIL%     SHEETS       DATE       FINISHED SHEETS/CARTONS "
               "</B>"                    
               SKIP(1).
      FOR EACH tt-size:
          DELETE tt-size.
      END.
      FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                             BREAK BY bf-jobhdr.frm:
      if last-of(bf-jobhdr.frm) then do:
    FOR  EACH tt-reftable NO-LOCK WHERE tt-reftable.reftable EQ "jc/jc-calc.p"
           AND tt-reftable.company  EQ job-hdr.company
           AND tt-reftable.loc      EQ ""
           AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")
        AND (tt-reftable.est-type <> 4 or
                tt-reftable.val[12] = bf-jobhdr.frm)
         BREAK BY tt-reftable.val[12].
    IF FIRST-OF(tt-reftable.val[12]) THEN DO:     
       v-tt-seq = 0.
       FOR EACH b-eb NO-LOCK WHERE b-eb.company = job-hdr.company
                               AND b-eb.est-no = job-hdr.est-no
                               AND b-eb.form-no = tt-reftable.val[12]
                               AND b-eb.blank-no > 0:
           FIND FIRST tt-size WHERE tt-size.frm = int(tt-reftable.val[12])
                                AND tt-size.cad# = b-eb.cad-no NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-size THEN do:
              FIND FIRST ITEM NO-LOCK WHERE ITEM.company = job-hdr.company
                                        AND ITEM.i-no = b-eb.cas-no NO-ERROR.

              v-stock-no2 = IF est.est-type >= 2 AND est.est-type <= 3 THEN bf-jobhdr.i-no ELSE b-eb.stock.
              find first oe-ordl where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                  and oe-ordl.job-no  eq bf-jobhdr.job-no
                  and oe-ordl.job-no2 eq bf-jobhdr.job-no2
                  and oe-ordl.i-no    eq v-stock-no2
                  no-lock no-error.
              IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
                 find first oe-ordl where oe-ordl.company eq job-hdr.company
                               and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                               and oe-ordl.i-no    eq v-stock-no2
                               no-lock no-error.
              
              
              CREATE tt-size.
              ASSIGN v-tt-seq = v-tt-seq + 1
                     tt-size.frm = tt-reftable.val[12]
                     tt-size.seq = v-tt-seq
                     tt-size.blank-no = b-eb.blank-no
                     tt-size.cad# = b-eb.cad-no
                     tt-size.cad-SIZE = string(b-eb.cas-len) + "x" + string(b-eb.cas-wid) + "x" +
                                     string(b-eb.cas-dep)
                     tt-size.COUNT = IF AVAIL oe-ordl AND oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt ELSE b-eb.cas-cnt 
                     tt-size.vend-part = IF AVAIL ITEM THEN ITEM.i-dscr ELSE "".
           END.
       END.
       
         FIND FIRST tt-size USE-INDEX tt-size WHERE tt-size.frm = int(tt-reftable.val[12]) NO-LOCK NO-ERROR.                     
         i = 0.
         FOR EACH wrk-op WHERE wrk-op.s-num = tt-reftable.val[12] BREAK by wrk-op.d-seq by wrk-op.b-num:
             v-mat-for-mach = "".
             IF lookup(wrk-op.dept,lv-mat-dept-list) > 0 THEN DO:
                 
                FOR EACH xjob-mat WHERE xjob-mat.company eq cocode
                                       and xjob-mat.job     eq job-hdr.job
                                       and xjob-mat.job-no  eq job-hdr.job-no
                                       and xjob-mat.job-no2 eq job-hdr.job-no2
                                       AND xjob-mat.frm = wrk-op.s-num
                                       /*AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0)*/  NO-LOCK,
                     first ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND
                                      ITEM.mat-type = SUBSTRING(wrk-op.dept,1,1) NO-LOCK :
                     v-mat-for-mach = ITEM.i-name + fill(" ", 30 - LENGTH(ITEM.i-name))  /*"       " */ +
                                      string(string(xjob-mat.wid) + "x" + STRING(xjob-mat.len),"x(13)") +
                                      " " + string(xjob-mat.qty).                   
                     LEAVE.                 
                END.                            
             END.
     
             IF s-prt-mstandard THEN DO:
                IF s-run-speed THEN
                   PUT wrk-op.s-num "  " wrk-op.m-dscr   SPACE(2)
                       wrk-op.mr-waste[wrk-op.s-num]   SPACE(5)
                       wrk-op.mr[wrk-op.s-num]         SPACE(5)
                       wrk-op.speed[wrk-op.s-num]      SPACE(5)
                       wrk-op.spoil[wrk-op.s-num]      SPACE(2)
                       wrk-op.num-sh[wrk-op.s-num] SPACE(3).
               ELSE
                   PUT wrk-op.s-num "  " wrk-op.m-dscr   SPACE(2)
                       wrk-op.mr-waste[wrk-op.s-num]   SPACE(5)
                       wrk-op.mr[wrk-op.s-num]         SPACE(5)
                       wrk-op.run-hr[wrk-op.s-num]     SPACE(5)
                       wrk-op.spoil[wrk-op.s-num]      SPACE(2)
                       wrk-op.num-sh[wrk-op.s-num] SPACE(3).

             END.
             ELSE PUT wrk-op.s-num "  " wrk-op.m-dscr   SPACE(3)
                      SPACE(10)
                      SPACE(11)
                      SPACE(18)
                    wrk-op.num-sh[wrk-op.s-num] SPACE(3)
                      .
             i = i + 1.
             /*IF i MOD 2 <> 0 THEN*/ PUT UNFORMATTED "__________________________________________________" .
             PUT SKIP(1).       
             IF LINE-COUNTER > 46 THEN PAGE.
             FIND NEXT tt-size USE-INDEX tt-size WHERE tt-size.frm = int(tt-reftable.val[12]) NO-LOCK NO-ERROR.
        end. /* each wrk-op*/
        
       /*IF NOT last(tt-reftable.val[12]) THEN*/  PUT  v-fill AT 1 SKIP.
    END. /*first-of(tt-reftable.val[12] */
    END. /*tt-reftable*/
      
        /** PRINT JOB INSTRUCTIONS **/
      END. /* last-of(bf-jobhdr.frm) */
      END. /* bf-johdr*/

    FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                             BREAK BY bf-jobhdr.frm:
        lv-text = "".

    if FIRST(bf-jobhdr.frm) then do:
       lv-line-chars = 60.
        FIND FIRST job OF bf-jobhdr NO-LOCK NO-ERROR.       
        ASSIGN v-inst2 = ""
               v-tmp-lines = 0
               j = 0
               K = 0
               lv-got-return = 0
               lv-note-cnt = 0.
        IF first(bf-jobhdr.frm) THEN PUT "<B>FORM    DEPARTMENT                                       INSTRUCTION NOTES</B>" SKIP.
        
        
        FOR EACH notes WHERE notes.rec_key = job.rec_key  and notes.note_code <> ''  NO-LOCK :
         v-inst2 = "".
            IF v-prev-note-rec <> ? AND
               v-prev-note-rec <> RECID(notes) THEN v-prev-extent = lv-note-cnt.
            
            lv-form-note = /*notes.note_title + " " +*/ notes.note_text .

            DO i = 1 TO LENGTH(lv-form-note) :        
                   IF i - j >= lv-line-chars THEN ASSIGN j = i
                                                         lv-got-return = lv-got-return + 1.
                   v-tmp-lines = ( i - j ) / lv-line-chars.
                   {SYS/INC/ROUNDUP.I v-tmp-lines}
                   k = v-tmp-lines + lv-got-return.

                   IF k > 0 AND k <= 70 THEN 
                       v-inst2[k] = v-inst2[k] +
                                    IF SUBSTRING(lv-form-note,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                                    THEN SUBSTRING(lv-form-note,i,1)
                                    ELSE "" .              

                   IF SUBSTRING(lv-form-note,i,1) = CHR(10) OR SUBSTRING(lv-form-note,i,1) = CHR(13) THEN
                      ASSIGN
                         lv-got-return = lv-got-return + 1
                         j = i.
            END.

            ASSIGN v-prev-note-rec = RECID(notes)
                   j = 0
                   lv-got-return = 0
                   lv-note-cnt = lv-note-cnt + k.
            IF lv-note-cnt > 70 THEN LEAVE.
            /* PRINT*/
            /*<ADJUST=LPI>*/
            PUT "<LPI=4.3><P16>" notes.note_form_no "  " "<B>" + caps(trim(SUBSTRING(notes.note_title,1,13))) + "</B>" FORM "x(22)"
                 v-inst2[1] FORM "x(60)" "<P10>" SKIP.
            DO i = 2 TO k /*cnt*/ :
               PUT "<P16>" "                  " v-inst2[i] FORM "x(60)" "<P10>"  SKIP.
               IF LINE-COUNTER > 34 THEN do:
                  PAGE.
                  PUT "  <B>FORM   DEPARTMENT                                       INSTRUCTION NOTES</B>" SKIP.
               END.
            END.
            PUT "<LPI=6>" SKIP. 
            ASSIGN k = 0
                   v-tmp-lines = 0
                   .
        END.        
    END.
  END.

     /* PUT v-fill AT 1 SKIP. */
      PAGE.
      FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                             BREAK BY bf-jobhdr.frm:
       /* dept notes*/
        lv-line-chars = 55.
        FIND FIRST job OF bf-jobhdr NO-LOCK NO-ERROR.       
        ASSIGN v-inst2 = ""
               v-tmp-lines = 0
               j = 0
               K = 0
               lv-got-return = 0
               lv-note-cnt = 0.
        IF first(bf-jobhdr.frm) THEN PUT "<B>SPEC    DEPARTMENT                                       INSTRUCTION NOTES</B>" SKIP.
        FIND FIRST itemfg
            WHERE itemfg.company EQ bf-jobhdr.company
            AND itemfg.i-no    EQ bf-jobhdr.i-no NO-LOCK NO-ERROR .
           
        FOR EACH notes WHERE notes.rec_key = itemfg.rec_key and notes.note_code <> '' AND LOOKUP(notes.note_code,spec-list) NE 0 NO-LOCK :
            v-inst2 = "".
            IF v-prev-note-rec <> ? AND
               v-prev-note-rec <> RECID(notes) THEN v-prev-extent = lv-note-cnt.
            
            lv-form-note = /*notes.note_title + " " +*/ notes.note_text .

            DO i = 1 TO LENGTH(lv-form-note) :        
                   IF i - j >= lv-line-chars THEN ASSIGN j = i
                                                         lv-got-return = lv-got-return + 1.
                   v-tmp-lines = ( i - j ) / lv-line-chars.
                   {SYS/INC/ROUNDUP.I v-tmp-lines}
                   k = v-tmp-lines + lv-got-return.

                   IF k > 0 AND k <= 70 THEN 
                       v-inst2[k] = v-inst2[k] +
                                    IF SUBSTRING(lv-form-note,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                                    THEN SUBSTRING(lv-form-note,i,1)
                                    ELSE "" .              

                   IF SUBSTRING(lv-form-note,i,1) = CHR(10) OR SUBSTRING(lv-form-note,i,1) = CHR(13) THEN
                      ASSIGN
                         lv-got-return = lv-got-return + 1
                         j = i.
            END.

            ASSIGN v-prev-note-rec = RECID(notes)
                   j = 0
                   lv-got-return = 0
                   lv-note-cnt = lv-note-cnt + k.
            IF lv-note-cnt > 70 THEN LEAVE.
            /* PRINT*/
            /*<ADJUST=LPI>*/
            PUT "<LPI=4.3><P16>" notes.note_code "  " "<B>" + caps(trim(notes.note_title)) + "</B>" FORM "x(22)"
                 v-inst2[1] FORM "x(60)" "<P10>" SKIP.
            DO i = 2 TO k /*cnt*/ :
               PUT "<P16>" "                  " v-inst2[i] FORM "x(60)" "<P10>"  SKIP.
               IF LINE-COUNTER > 34 THEN do:
                  PAGE.
                  PUT "  <B>SPEC   DEPARTMENT                                       INSTRUCTION NOTES</B>" SKIP.
               END.
            END.
            PUT "<LPI=6>" SKIP. 
            ASSIGN k = 0
                   v-tmp-lines = 0
                   .
        END.

        /*end. /* first bf-jobhdr.frm */*/
      END. /* bf-jobhdr */

      /* print die# image */
      IF print-box THEN DO:          
        FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                             BREAK BY bf-jobhdr.frm:
          IF FIRST-OF(bf-jobhdr.frm) THEN DO:
           FIND FIRST b-ef WHERE b-ef.company = bf-jobhdr.company
                           AND b-ef.est-no = bf-jobhdr.est-no
                           AND b-ef.form-no = bf-jobhdr.frm NO-LOCK NO-ERROR.
           FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                            AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
           IF AVAIL b-ef AND b-ef.cad-image <> "" 
              AND LOOKUP(b-ef.cad-image,lv-cad-image-list) <= 0
           THEN DO:             
             lv-cad-image =  (IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") +
                          b-ef.cad-image + ".JPG".
             lv-cad-image-list = lv-cad-image-list + b-ef.cad-image + ",".
             PAGE.
             PUT "<R3><C2><#21>".
             PUT unformatted "<=21><R+2><C3><#22><R+51><C+133><IMAGE#22=" lv-cad-image ">" .
           END.
          END.
        END. /*each bf-jobhdr*/
      END.  /* print-box*/
      /* print case labels */
      i = 1.
        v-fgitm = "".
        FOR EACH tt-fgitm.
            DELETE tt-fgitm.
        END.

        for each xjob-hdr where xjob-hdr.company eq cocode
                            and xjob-hdr.job     eq job-hdr.job
                            and xjob-hdr.job-no  eq job-hdr.job-no
                            and xjob-hdr.job-no2 eq job-hdr.job-no2
                            NO-LOCK BY xjob-hdr.frm BY xjob-hdr.blank-no:

           find first xoe-ordl
                where xoe-ordl.company eq xjob-hdr.company
                  and xoe-ordl.ord-no  eq xjob-hdr.ord-no
                  and xoe-ordl.job-no  eq xjob-hdr.job-no
                  and xoe-ordl.job-no2 eq xjob-hdr.job-no2
                  and xoe-ordl.i-no    eq xjob-hdr.i-no
                  no-lock no-error.
           IF AVAIL xoe-ordl THEN v-fgqty[i] = xoe-ordl.cas-cnt.
           
           FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                             AND b-eb.est-no  eq xjob-hdr.est-no
                             AND b-eb.form-no = xjob-hdr.frm
                             AND b-eb.blank-no = xjob-hdr.blank-no
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.
           IF NOT AVAIL b-eb THEN 
               FIND FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                             AND b-eb.est-no  eq xjob-hdr.est-no
                             AND b-eb.form-no = xjob-hdr.frm
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.
           FIND FIRST oe-ord WHERE oe-ord.company = xjob-hdr.company
                               AND oe-ord.ord-no = xjob-hdr.ord-no NO-LOCK NO-ERROR.
           v-cust-no = IF AVAIL oe-ord THEN oe-ord.cust-no ELSE xjob-hdr.cust-no.
           FIND FIRST cust WHERE cust.company = xjob-hdr.company AND
                                 cust.cust-no = v-cust-no NO-LOCK NO-ERROR.
           
           CREATE tt-fgitm.
           ASSIGN tt-fgitm.i-no = xjob-hdr.i-no
                  tt-fgitm.qty =  IF AVAIL xoe-ordl AND xoe-ordl.cas-cnt <> 0 THEN xoe-ordl.cas-cnt 
                                  ELSE IF AVAIL b-eb THEN b-eb.cas-cnt ELSE xjob-hdr.qty
                  tt-fgitm.i-dscr = IF AVAIL b-eb THEN b-eb.part-dscr1 ELSE xjob-hdr.i-no
                  tt-fgitm.po-no = IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE ""
                  tt-fgitm.ord-no = xjob-hdr.ord-no
                  tt-fgitm.cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                                       ELSE IF AVAIL cust THEN cust.name
                                       ELSE xjob-hdr.cust-no.
                  tt-fgitm.seq = i.
       find first oe-ordl
           where oe-ordl.company eq xjob-hdr.company
             and oe-ordl.ord-no  eq xjob-hdr.ord-no
             and oe-ordl.job-no  eq xjob-hdr.job-no
             and oe-ordl.job-no2 eq xjob-hdr.job-no2
             and oe-ordl.i-no    eq xjob-hdr.i-no no-lock no-error.
       IF xjob-hdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
          find first oe-ordl where oe-ordl.company eq xjob-hdr.company
                     and oe-ordl.ord-no  eq xjob-hdr.ord-no
                     and oe-ordl.i-no    eq xjob-hdr.i-no NO-LOCK no-error.
       IF xjob-hdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
          find first oe-ordl where oe-ordl.company eq xjob-hdr.company
                             and oe-ordl.ord-no  eq xjob-hdr.ord-no no-error.
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
              ASSIGN tt-fgitm.shipto[1] = shipto.ship-name
                     tt-fgitm.shipto[2] = shipto.ship-addr[1]
                     tt-fgitm.shipto[3] = shipto.ship-addr[2]
                     tt-fgitm.shipto[4] = trim(oe-rel.ship-city) + ", " +
                                          oe-rel.ship-state + "  " + oe-rel.ship-zip.          
       end.

           i = i + 1.
        END.

        /* label prints per item */
        ASSIGN
        i = 0
        j = 0
        k = 0.
        IF s-prt-label THEN DO:
        PAGE.
        FOR EACH tt-fgitm BY tt-fgitm.seq.
            
          IF PAGE-SIZE - LINE-COUNTER < 15 THEN PAGE.

          ASSIGN
             k =  i * 4 + 1
             i = i + 1
             v-fgitm[i] = tt-fgitm.i-no
             v-fgdsc[i] = tt-fgitm.i-dscr
             v-fgqty[i] = tt-fgitm.qty
             v-pono[i] = tt-fgitm.po-no
             v-cname[i] = tt-fgitm.cust-name
             v-shiptolabel[k] = tt-fgitm.shipto[1]
             v-shiptolabel[k + 1] = tt-fgitm.shipto[2]
             v-shiptolabel[k + 2] = tt-fgitm.shipto[3]
             v-shiptolabel[k + 3] = tt-fgitm.shipto[4]
             j = j + 1.

          IF i >= 3 THEN DO:
              ASSIGN v-cust-name = v-cname[1]
                     v-cust-name2 = v-cname[2]
                     v-cust-name3 = v-cname[3]
                     v-shipto[1] =  v-shiptolabel[1]
                     v-shipto[2] =  v-shiptolabel[2]
                     v-shipto[3] =  v-shiptolabel[3]
                     v-shipto[4] =  v-shiptolabel[4]
                     v-shipto1[1] =  v-shiptolabel[5]
                     v-shipto1[2] =  v-shiptolabel[6]
                     v-shipto1[3] =  v-shiptolabel[7]
                     v-shipto1[4] =  v-shiptolabel[8]
                     v-shipto2[1] =  v-shiptolabel[9]
                     v-shipto2[2] =  v-shiptolabel[10]
                     v-shipto2[3] =  v-shiptolabel[11]
                     v-shipto2[4] =  v-shiptolabel[12]
                     .
                    
            display v-fill skip
               "<B><U>LABEL ITEM" + trim(string(j - 2)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(string(j - 1)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 55
               "<U>LABEL ITEM" + TRIM(STRING(j)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 107
               SKIP
               "Job#:" v-job-no + "-" + string(v-job-no2)
               "Job#:" WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + string(v-job-no2)   WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + string(v-job-no2) WHEN v-fgitm[3] <> "" 
               SKIP
               "Customer:" v-cust-name 
               "Customer:"  WHEN v-fgitm[2] <> ""  AT 45 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Customer:" WHEN v-fgitm[3] <> "" AT 90  v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               "Purchase Order#:" v-pono[1]
               "Purchase Order#:"  WHEN v-fgitm[2] <> "" AT 45 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "Purchase Order#:" WHEN v-fgitm[3] <> "" AT 90  v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "<B>FG Item #:</B>" v-fgitm[1]
               "<B>FG Item #:</B>"  WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] WHEN v-fgitm[2] <> "" 
               "<B>FG Item #:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Description:" v-fgdsc[1]
               "Description:"  WHEN v-fgitm[2] <> "" AT 45 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Description:" WHEN v-fgitm[3] <> "" AT 90  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Count:   " v-fgqty[1]
               "Count:   "  WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  WHEN v-fgitm[2] <> "" 
               "Count:   "  WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] WHEN v-fgitm[3] <> ""                               
               SKIP
               "Shipto:" WHEN s-prt-shipto v-shipto[1] WHEN s-prt-shipto
               "Shipto:" WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 45 v-shipto1[1] WHEN s-prt-shipto AND v-fgitm[2] <> ""
               "Shipto:" WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 90 v-shipto2[1] WHEN s-prt-shipto AND v-fgitm[3] <> ""
               SKIP
               v-shipto[2] AT 8  WHEN s-prt-shipto
               v-shipto1[2] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[2] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               SKIP
               v-shipto[4] AT 8  WHEN s-prt-shipto
               v-shipto1[4] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[4] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
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
                   v-shipto = ""
                   v-shipto1 = ""
                   v-shipto2 = ""
                   v-cname = ""
                   v-shiptolabel = ""
                   v-last-j = j.
          END. /* i = 3 */
        END.
        IF i > 0 THEN DO:
            ASSIGN v-cust-name = v-cname[1]
                   v-cust-name2 = v-cname[2]
                   v-cust-name3 = v-cname[3]
                   v-shipto[1] =  v-shiptolabel[1]
                   v-shipto[2] =  v-shiptolabel[2]
                   v-shipto[3] =  v-shiptolabel[3]
                   v-shipto[4] =  v-shiptolabel[4]
                   v-shipto1[1] =  v-shiptolabel[5]
                   v-shipto1[2] =  v-shiptolabel[6]
                   v-shipto1[3] =  v-shiptolabel[7]
                   v-shipto1[4] =  v-shiptolabel[8]
                   v-shipto2[1] =  v-shiptolabel[9]
                   v-shipto2[2] =  v-shiptolabel[10]
                   v-shipto2[3] =  v-shiptolabel[11]
                   v-shipto2[4] =  v-shiptolabel[12]
                   .
            IF PAGE-SIZE - LINE-COUNTER < 15 THEN PAGE.
            display v-fill skip
               "<B><U>LABEL ITEM" + trim(string(v-last-j + 1)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(string(v-last-j + 2)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 55
               "<U>LABEL ITEM" + TRIM(STRING(v-last-j + 3)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 107
               SKIP
               "Job#:" v-job-no + "-" + string(v-job-no2)
               "Job#:" WHEN v-fgitm[2] <> ""  AT 45
                v-job-no + "-" + string(v-job-no2) WHEN v-fgitm[2] <> "" 
               "Job#:" WHEN v-fgitm[3] <> "" AT 90  
               v-job-no + "-" + string(v-job-no2) WHEN v-fgitm[3] <> "" 
               SKIP
               "Customer:" v-cust-name 
               "Customer:"  WHEN v-fgitm[2] <> ""  AT 45 v-cust-name2  WHEN v-fgitm[2] <> "" 
               "Customer:" WHEN v-fgitm[3] <> "" AT 90  v-cust-name3 WHEN v-fgitm[3] <> "" 
               SKIP
               "Purchase Order#:" v-pono[1]
               "Purchase Order#:"  WHEN v-fgitm[2] <> "" AT 45 v-pono[2]  WHEN v-fgitm[2] <> "" 
               "Purchase Order#:" WHEN v-fgitm[3] <> "" AT 90  v-pono[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "<B>FG Item #:</B>" v-fgitm[1]
               "<B>FG Item #:</B>"  WHEN v-fgitm[2] <> "" AT 52 v-fgitm[2] WHEN v-fgitm[2] <> "" 
               "<B>FG Item #:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgitm[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Description:" v-fgdsc[1]
               "Description:"  WHEN v-fgitm[2] <> "" AT 45 v-fgdsc[2]  WHEN v-fgitm[2] <> "" 
               "Description:" WHEN v-fgitm[3] <> "" AT 90  v-fgdsc[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Count:   " v-fgqty[1]
               "Count:   "  WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  WHEN v-fgitm[2] <> "" 
               "Count:   "  WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] WHEN v-fgitm[3] <> ""      
               SKIP
               "Shipto:" WHEN s-prt-shipto v-shipto[1] WHEN s-prt-shipto
               "Shipto:" WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 45 v-shipto1[1] WHEN s-prt-shipto AND v-fgitm[2] <> ""
               "Shipto:" WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 90 v-shipto2[1] WHEN s-prt-shipto AND v-fgitm[3] <> ""
               SKIP
               v-shipto[2] AT 8  WHEN s-prt-shipto
               v-shipto1[2] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[2] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               SKIP
               v-shipto[4] AT 8  WHEN s-prt-shipto
               v-shipto1[4] WHEN s-prt-shipto AND v-fgitm[2] <> "" AT 53
               v-shipto2[4] WHEN s-prt-shipto AND v-fgitm[3] <> "" AT 98
               SKIP
               WITH FRAME itmlbl2 NO-BOX NO-LABELS STREAM-IO WIDTH 180.
            i = 0.
          END. /* i <= 3 */
        END.  /* s-prt-label*/

end. /* first job-no */
      
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
RELEASE xjob-hdr NO-ERROR.    
OUTPUT STREAM st-st CLOSE.

/* end ---------------------------------- copr. 1994  advanced software, inc. */

