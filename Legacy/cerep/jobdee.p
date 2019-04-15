/* cerep/jobdee.p   Xprint FC Factory  Ticket for DEE */
/*  factory ticket  for folding , DEE */
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

OUTPUT STREAM st-st TO VALUE(v-dir + "job100.txt").

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
def new shared var v-fill as char format "x(260)". /*128*/
def new shared var v-frst as log.
def new shared var v-ok as log.
def new shared var v-est-qty as int format "->>,>>>,>>9".
def new shared var v-job-qty as int format "->>,>>>,>>9".
def new shared var v-itm-qty as int format "->>,>>>,>>9".
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
def var v-fill3 as char format "x(260)" NO-UNDO. /*128*/
DEF VAR lv-do-page-skip AS LOG NO-UNDO.
DEF VAR lv-page-line AS INT NO-UNDO.
DEF VAR note-count AS INT.
DEF VAR v-fdn AS CHAR NO-UNDO.
DEF VAR v-fill4 AS CHAR FORMAT "X(259)" NO-UNDO.

def TEMP-TABLE w-lo NO-UNDO
  field layout like v-layout.

def new shared buffer xjob-hdr for job-hdr.

def buffer b-eb for eb.
def buffer b-ef for ef.

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
  FIELD i-seq AS INT  
  /* gdm - 11030807 */
  field i-coldscr as char format "x(20)".

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

{custom/formtext.i NEW}     
{custom/notesdef.i}
DEF VAR v-inst2 AS cha EXTENT 100 NO-UNDO.    
DEF VAR v-dept-inst AS cha FORM "x(115)" EXTENT 100 NO-UNDO.
DEF VAR v-start-date AS DATE NO-UNDO.
DEF VAR v-req-date AS DATE NO-UNDO.
DEF VAR v-shipto AS cha FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-case-size AS cha NO-UNDO.
DEF VAR v-sht-size AS cha NO-UNDO.
DEF VAR v-die-size AS cha NO-UNDO.
DEF VAR v-vend LIKE po-ord.vend-no NO-UNDO.
DEF VAR v-item AS cha EXTENT 50 NO-UNDO.
DEF VAR v-ink1 AS cha FORMAT "x(130)" EXTENT 50 NO-UNDO.
DEF VAR v-ink2 AS cha FORMAT "x(130)" EXTENT 50 NO-UNDO.
DEF VAR v-ink3 AS cha FORMAT "x(130)" EXTENT 50 NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no NO-UNDO.
DEF VAR lv-mat-dept-list AS cha INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach AS cha NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF VAR v-fgitm AS cha FORM "x(15)" EXTENT 10 NO-UNDO.
DEF VAR v-fgdsc LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEF VAR v-fgqty LIKE job-hdr.qty EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEF VAR v-pono LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF TEMP-TABLE tt-fgitm FIELD i-no AS cha FORM "x(15)"
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
DEF VAR v-last-j AS INT NO-UNDO.
DEF VAR v-po-no2 LIKE v-po-no NO-UNDO.
DEF VAR v-po-no3 LIKE v-po-no NO-UNDO.
DEF VAR v-spc-no AS cha FORM "x(15)" NO-UNDO.
DEF VAR v-ord-qty AS INT NO-UNDO.
DEF VAR v-stock-no LIKE eb.stock-no NO-UNDO.
DEF VAR v-stock-no2 LIKE eb.stock-no NO-UNDO.
DEF VAR v-dept-note-line AS INT NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR v-po-no4 LIKE v-po-no NO-UNDO.

DEF TEMP-TABLE tt-size FIELD frm LIKE job-hdr.frm
                       FIELD blank-no LIKE eb.blank-no
                       FIELD cad# LIKE eb.cad-no FORM "x(10)"
                       FIELD cad-size AS cha FORM "x(25)"
                       FIELD COUNT LIKE eb.cas-cnt
                       FIELD vend-part AS cha FORM "x(30)"
                       FIELD cas-pal LIKE eb.cas-pal 
                       FIELD seq AS INT
                       INDEX tt-size frm seq.
DEF VAR v-tt-seq AS INT NO-UNDO.
                                          
ASSIGN                                       
  v-fill = "<||3><C1><FROM><C260><LINE><||3>" 
  v-fill3 = "<C2><FROM><C136><LINE><||3><R-1>"
  v-fill4 = "<||3><C1><FROM><C259><LINE><||3>".

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
DEF VAR v-i-qty AS dec format ">,>>9.9<" .
DEF BUFFER bf-ink FOR wrk-ink.
DEF BUFFER bf-jobhdr FOR job-hdr.

DEF BUFFER bf-jobhdr2 FOR job-hdr.
DEF BUFFER bf-jobhdr3 FOR job-hdr.
DEF TEMP-TABLE tt-job-hdr LIKE job-hdr 
    FIELD vdate AS CHAR .


DEF TEMP-TABLE tt-ink FIELD i-code LIKE wrk-ink.i-code
                      FIELD i-seq LIKE wrk-ink.i-seq.
DEF TEMP-TABLE tt-reftable LIKE reftable
                         FIELD est-type LIKE est.est-type.
DEF VAR lv-cad-image AS cha NO-UNDO.
DEF VAR lv-cad-image-list AS cha NO-UNDO.
DEF VAR v-set-fg AS cha NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-managed-order AS cha FORM "x(27)" NO-UNDO.

DEF VAR v-slsmn AS CHAR FORMAT "x(3)" NO-UNDO.
DEF VAR v-FOB   AS CHAR FORMAT "x(5)" NO-UNDO. 

DEF SHARED VAR v-newdie   AS LOG NO-UNDO.
DEF SHARED VAR v-newfilm  AS LOG NO-UNDO.
DEF SHARED VAR v-newcombo AS LOG NO-UNDO.

DEF VAR v-flgND AS CHAR FORMAT "x" NO-UNDO.
DEF VAR v-flgNF AS CHAR FORMAT "x" NO-UNDO.
DEF VAR v-flgNC AS CHAR FORMAT "x" NO-UNDO.

DEF VAR v-last-job LIKE job-hdr.job-no NO-UNDO.

format HEADER 
       "<C54><B><P12>" v-managed-order "</B>" "<C108>6/05 Job Ticket QF-130 Rev.A"  SKIP
       "<B>  JOB NUMBER:<P18><R-0.3><P14>" v-job-no space(0) "-" space(0) v-job-no2 format "99" "</B><P10><R+0.3>"
       " " v-set-fg FORM "x(31)"                                                                                          
       "<C54><B><P12>F A C T O R Y   T I C K E T<P11>"  "<C91>JOB START DATE:"  v-start-date "  PRINTED DATE:" TODAY "</B><P10>" SKIP
       v-fill
    with no-box frame head no-labels stream-io width 260 PAGE-TOP.

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

ASSIGN
    v-flgND =  IF v-newdie   EQ TRUE THEN "Y" ELSE "N"
    v-flgNF =  IF v-newfilm  EQ TRUE THEN "Y" ELSE "N"
    v-flgNC =  IF v-newcombo EQ TRUE THEN "Y" ELSE "N".

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
             tt-reftable.code = STRING(job-hdr.job,"999999999")
             tt-reftable.val[12] = job-hdr.frm
             tt-reftable.val[13] = job-hdr.blank-no
             tt-reftable.est-type = est.est-type.

   END.
   /* get whether warehous item or not */
   find first oe-ordl where oe-ordl.company eq job-hdr.company
             and oe-ordl.ord-no  eq job-hdr.ord-no
             and oe-ordl.job-no  eq job-hdr.job-no
             and oe-ordl.job-no2 eq job-hdr.job-no2
             and oe-ordl.i-no    eq job-hdr.i-no
           no-lock no-error.

    IF job-hdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
       find first oe-ordl where oe-ordl.company eq job-hdr.company
             and oe-ordl.ord-no  eq job-hdr.ord-no
             and oe-ordl.i-no    eq job-hdr.i-no NO-LOCK no-error.
    IF AVAIL oe-ordl THEN       

    v-managed-order = IF v-managed-order = "" AND oe-ordl.managed = true THEN "MANAGED   WAREHOUSE   ORDER"
                      ELSE v-managed-order.

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

    if first-of(job-hdr.job-no2) then v-first = yes.
    /** PRINT JOB HEADER **/
    if v-first then do:   
       assign
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2 .

       if avail oe-ord AND not oe-ctrl.p-fact and (oe-ord.stat eq "H" OR oe-ord.priceHold) then next.
       v-due-date = if avail oe-ord then oe-ord.due-date else ?.
       v-start-date = job-hdr.start-date.
       v-set-fg = IF est.est-type = 2 THEN "SET FG#: <B>" + job-hdr.i-no + "</B>" ELSE "".
       if not first(job-hdr.job-no) then page.
       ELSE
         put
           "<C54><B><P12>" v-managed-order "</B>" "<C108>6/05 Job Ticket QF-130 Rev.A"  SKIP
           "<B>  JOB NUMBER:<P18><R-0.3><P14>" v-job-no space(0) "-" space(0) v-job-no2 format "99" "</B><P10><R+0.3>"
           " " v-set-fg FORM "x(31)"                                                                                       
           "<C54><B><P12>F A C T O R Y   T I C K E T<P11>"  "<C91>JOB START DATE:"  v-start-date "  PRINTED DATE:" TODAY "</B><P10>" SKIP
           v-fill.

       view frame head.
       VIEW FRAME bott.
       v-shipto = "".
       find first oe-ordl
           where oe-ordl.company eq job-hdr.company
             and oe-ordl.ord-no  eq job-hdr.ord-no
             and oe-ordl.job-no  eq job-hdr.job-no
             and oe-ordl.job-no2 eq job-hdr.job-no2
             and oe-ordl.i-no    eq job-hdr.i-no
           no-lock no-error.

       IF job-hdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
       find first oe-ordl
           where oe-ordl.company eq job-hdr.company
             and oe-ordl.ord-no  eq job-hdr.ord-no
             and oe-ordl.i-no    eq job-hdr.i-no
           no-error.

       IF job-hdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
       find first oe-ordl
           where oe-ordl.company eq job-hdr.company
             and oe-ordl.ord-no  eq job-hdr.ord-no
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
       v-ovund = IF AVAIL oe-ord THEN 
                               trim(string(oe-ordl.over-pct,">>9.99")) + "/" +
                               trim(string(oe-ordl.under-pct,">>9.99"))
                 ELSE ""              .

       v-slsmn = IF AVAIL oe-ord THEN oe-ord.sman[1] ELSE "".
       v-FOB   = IF AVAIL oe-ord THEN oe-ord.fob-code ELSE "".

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
       v-spc-no = IF AVAIL eb THEN eb.spc-no ELSE "".

       PUT "  <B>CUSTOMER NAME:<P18><R-0.3>" v-cust-name  "<P10><R+0.3>"
            "<B>SALESMAN:  REQ DATE:   DUE DATE:    ESTIMATE:  SPC/QC          OVER/UNDER%" AT 74 SKIP
            "  SHIPTO:</B>" v-shipto[1] "<B>FOB:</B>" AT 55 v-slsmn AT 82           
           v-req-date AT 93 v-due-date AT 105 TRIM(job-hdr.est-no) FORMAT "x(8)" AT 118 
           v-spc-no AT 129 " " v-ovund
           
            SKIP.     

       IF v-shipto[2] <> "" THEN
          PUT v-shipto[2] AT 10.

       PUT v-fob AT 51 "<B>NEW DIE: </B>" AT 82  "<B>NEW FILM: </B>" AT 101 "<B>NEW COMBO: </B>" AT 121
           SKIP.

       IF v-shipto[4] <> "" THEN
         PUT v-shipto[4] AT 10 
           CAPS(STRING(v-newdie))  AT 84  CAPS(STRING(v-newfilm)) AT 96 CAPS(STRING(v-newcombo)) AT 109
           SKIP.
/*
       PUT "<B>NEW DIE: </B>" AT 82 v-flgND  "<B>NEW FILM: </B>" AT 101 v-flgNF  "<B>NEW COMBO: </B>" AT 121 v-flgNC 
           SKIP.           
*/
       PUT v-fill.

        v-line = if avail est                            and
                 est.est-type gt 2 and est.est-type lt 5 then 500 else 50.
        /* new */

     FOR EACH wrk-ink:
         DELETE wrk-ink.
     END.

    /* FOR EACH bf-jobhdr2 NO-LOCK 
         WHERE bf-jobhdr2.company EQ cocode
           AND bf-jobhdr2.i-no    EQ job-hdr.i-no
           AND ROWID(bf-jobhdr2)  NE ROWID(job-hdr)
           AND bf-jobhdr2.job-no  NE job-hdr.job-no
         BREAK BY bf-jobhdr2.start-date DESC:
         
         v-last-job = bf-jobhdr2.job-no.
         LEAVE.
     END.*/


     FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                                      BREAK BY bf-jobhdr.frm:
        IF FIRST(bf-jobhdr.frm) THEN
           /*PUT "  <P10><B><C2.7>F/B   FG ITEM #        ORDER QTY    FORM QTY  PO#       ITEM NAME        FOLD DEPT NOTE        STYLE  CARTON SIZE                CAD#    UP   COLORS" /*v-upc-lbl*/ "</B>" SKIP.*/
                                      
           PUT "  <P10><B><C2.7>F/B    ORDER QTY    FORM QTY  PO#       Customer P/N     Description                  LAST JOB #            STYLE  CARTON SIZE                CAD#    UP   " /*v-upc-lbl*/ "</B>" SKIP.    
                                                                                                         
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
                and item.i-no eq job-mat.i-no no-lock:

            for each eb WHERE eb.company     EQ job-hdr.company
                AND eb.est-no      eq bf-jobhdr.est-no
                and eb.form-no     eq job-mat.frm
                NO-LOCK
              break by eb.form-no BY eb.blank-no.

              do i = 1 to EXTENT(eb.i-code2):
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
              if avail wrk-ink then wrk-ink.i-qty = /*wrk-ink.i-qty +*/ job-mat.qty.
            END. /* eb*/
          end. /* JOB-MAT */
          /* get color info for the board */
          v-ink-seq = 0.            
          for each wrk-ink WHERE wrk-ink.form-no = tt-reftable.val[12] /*bf-jobhdr.frm*/
                break by wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no :

                IF FIRST-OF(wrk-ink.blank-no) THEN v-ink-use-per-blank = 0.
                v-ink-use-per-blank = v-ink-use-per-blank + 1.
                IF first-OF(wrk-ink.i-code) /*OR v-ink-use-per-blank > 1*/  THEN v-ink-seq = v-ink-seq + 1.
                wrk-ink.i-seq = v-ink-seq.    
                /*PUT STREAM st-st wrk-ink.i-seq wrk-ink.i-pass wrk-ink.form-no wrk-ink.blank-no wrk-ink.i-code 
                    SKIP. */     
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
        for each eb
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

        if ef.form-no eq tt-reftable.val[12] /*bf-jobhdr.frm*/  then ebloop:
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
      
          if eb.est-type eq 4 then v-fac = eb.yld-qty / v-est-qty.
          
        
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

            v-upc-lbl = IF s-prt-sellprc THEN "Sell Price" ELSE "   UPC#".

            v-job-qty = 0.
            v-stock-no = IF est.est-type >= 2 AND est.est-type <= 3 THEN bf-jobhdr.i-no ELSE eb.stock.
            for each xjob-hdr where xjob-hdr.company eq cocode
               and xjob-hdr.job     eq job-hdr.job
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.job-no2 eq job-hdr.job-no2
               and xjob-hdr.i-no    eq v-stock-no no-lock:
               v-job-qty = v-job-qty + xjob-hdr.qty.
            end.
            v-itm-qty = 0.
            for each xjob-hdr where xjob-hdr.company eq cocode
               and xjob-hdr.job     eq job-hdr.job
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.job-no2 eq job-hdr.job-no2
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.frm     eq eb.form-no
               and xjob-hdr.i-no    eq v-stock-no no-lock:
               v-itm-qty = v-itm-qty + xjob-hdr.qty.
            end.
            IF v-itm-qty EQ 0 THEN v-itm-qty = v-job-qty.
            /** PRINT ITEM **/
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq v-stock-no
                no-lock no-error.
            IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                  and oe-ordl.i-no    eq v-stock-no
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
            
            ASSIGN vs-len = ""
                   vs-wid = ""
                   vs-dep = "".
            
            IF eb.len <> 0 THEN RUN sys/inc/dec-frac.p (eb.len,32,OUTPUT vs-len).
            IF eb.wid <> 0 THEN RUN sys/inc/dec-frac.p (eb.wid,32,OUTPUT vs-wid).
            IF eb.dep <> 0 THEN RUN sys/inc/dec-frac.p (eb.dep,32,OUTPUT vs-dep).
            v-size[1] = (IF vs-len <> "" THEN trim(vs-len) + "x" ELSE "") +
                        (IF vs-wid <> "" THEN TRIM(vs-wid) + "x" ELSE "") +
                        trim(vs-dep).
          
            v-up = eb.num-up.
            v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".
            v-spc-no = eb.spc-no.
      
            v-ink-list = "".
            FOR EACH tt-ink:
                DELETE tt-ink.
            END.
            
            do i = 1 to EXTENT(eb.i-code2):
              if eb.i-code2[i] <> "" then do:
                /* FIND FIRST wrk-ink WHERE wrk-ink.form-no = eb.form-no
                                      AND wrk-ink.blank-no = eb.blank-no
                                      AND wrk-ink.i-code = eb.i-code2[i] 
                                      NO-LOCK NO-ERROR.
                 IF AVAIL wrk-ink THEN DO: */
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
              
               v-ord-qty = v-ord-qty * (v-itm-qty / v-job-qty)
               v-job-qty = v-itm-qty.

            FIND FIRST itemfg WHERE itemfg.company = eb.company AND
                 itemfg.i-no = eb.stock-no
                 NO-LOCK NO-ERROR.

            IF AVAIL itemfg THEN
            DO:
               FIND FIRST notes WHERE
                    notes.rec_key EQ itemfg.rec_key AND
                    notes.note_type EQ "S" AND
                    notes.note_code EQ "FDN"
                    NO-LOCK NO-ERROR.

               IF AVAIL notes THEN
               DO:
                  v-fdn = SUBSTRING(notes.note_title,1,21).
                  RELEASE notes.
               END.
               ELSE
                  v-fdn = "".

               RELEASE itemfg.
            END.
            ELSE
               v-fdn = "".

            FIND FIRST oe-rel WHERE oe-rel.company EQ oe-ordl.company AND 
               oe-rel.ord-no  EQ oe-ordl.ord-no AND 
               oe-rel.i-no    EQ oe-ordl.i-no   AND 
               oe-rel.line    EQ oe-ordl.line NO-LOCK NO-ERROR.
            IF AVAIL oe-rel THEN ASSIGN
                v-po-no4 = oe-rel.po-no . 

            IF v-po-no4 = "" THEN
               v-po-no4 = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".
             FOR EACH tt-job-hdr NO-LOCK:
                 DELETE tt-job-hdr.
             END.

             FOR EACH bf-jobhdr2 NO-LOCK 
                 WHERE bf-jobhdr2.company EQ cocode
                 AND bf-jobhdr2.i-no    EQ oe-ordl.i-no
                 AND bf-jobhdr2.est-no  EQ FILL(" ",8 - LENGTH(TRIM(job-hdr.est-no))) + TRIM(job-hdr.est-no)
                 BREAK BY bf-jobhdr2.start-date DESC:
                
                 CREATE tt-job-hdr.
                 BUFFER-COPY bf-jobhdr2 TO tt-job-hdr .
                 ASSIGN tt-job-hdr.vdate = STRING(bf-jobhdr2.start-date) .
             END.
                FOR EACH tt-job-hdr NO-LOCK:
                    IF tt-job-hdr.vdate = "?" THEN ASSIGN tt-job-hdr.vdate = "".
                END.

              IF job-hdr.start-date  NE ? THEN do:
              FOR EACH tt-job-hdr WHERE tt-job-hdr.est-no = FILL(" ",8 - LENGTH(TRIM(job-hdr.est-no))) + TRIM(job-hdr.est-no)
                        AND tt-job-hdr.job-no  NE job.job-no AND tt-job-hdr.i-no = oe-ordl.i-no AND tt-job-hdr.vdate < string(job-hdr.start-date)  NO-LOCK BY tt-job-hdr.vdate DESC BY tt-job-hdr.job-no  DESC:
                            v-last-job = tt-job-hdr.job-no.
                            LEAVE.
              END.
              END.

              IF job-hdr.start-date  EQ ? THEN do:
              FOR EACH tt-job-hdr WHERE tt-job-hdr.est-no = FILL(" ",8 - LENGTH(TRIM(job-hdr.est-no))) + TRIM(job-hdr.est-no)
                        AND tt-job-hdr.job-no  NE job.job-no AND tt-job-hdr.i-no = oe-ordl.i-no NO-LOCK BY tt-job-hdr.vdate :
                            v-last-job = tt-job-hdr.job-no.
                            LEAVE.
              END.
              END.
           
          
            display 
            " " trim(string(eb.form-no,">>9")) + "-" +
                    trim(string(eb.blank-no,">>9")) FORMAT "x(5)"
                    v-ord-qty  
                    v-job-qty format "->>,>>>,>>9"                     
                    v-po-no4 FORM "x(10)"
                  /*  eb.stock-no @ job-hdr.i-no */
                    eb.part-no
                    v-dsc[2] FORM "x(30)"
                    v-last-job FORM "X(21)"
                    eb.style
                    v-size[1] FORM "x(26)"
                    eb.cad-no FORM "x(7)"
                    v-up FORM ">9"
                    SPACE(3)
                   /* v-alloc FORM "x(15)" */
                    skip
                with stream-io width 210 no-labels no-box frame line-det1.

            v-itm-printed = v-itm-printed + 1.    
        END. /* eb */
      END.   /*ef */    
       IF NOT LAST(tt-reftable.val[12]) 
             OR tt-reftable.est-type = 4  THEN  put v-fill3  skip.
    END. /*first-of(tt-reftable.val[12]*/
    END. /*tt-reftable*/
        
        END. /* last-of(bf-jobhdr.frm) */
     END. /* each bf-jobhdr*/
     PUT v-fill /*SKIP*/ .
     /* Print Board*/
     PUT "<P10>" /*v-fill SKIP*/                       /*REQ'D*/
                 "<B><C1>FORM BOARD CODE          DUE DATE   SHEETS             SHEET SIZE                                                       DIE#                TOTAL UP</B>" 
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
              and eb.form-no     eq ef.form-no  NO-LOCK
            break by eb.form-no BY eb.blank-no:
        
            v-up = v-up + eb.num-up.
            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".

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
             run oe/rep/ticket1.p (recid(ef), recid(job-hdr)).
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
                 v-die-size = (IF vs-wid <> "" THEN trim(vs-wid) + "x" ELSE "") +
                              trim(vs-len).

                 IF AVAIL po-ord THEN
                    FIND FIRST po-ordl WHERE
                         po-ordl.company EQ po-ord.company AND
                         po-ordl.po-no   EQ po-ord.po-no AND
                         po-ordl.i-no = wrk-sheet.i-no
                         NO-LOCK NO-ERROR.

                 v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

                 PUT                
                       wrk-sheet.form-no
                          " " wrk-sheet.brd-dscr " "
                       v-po-duedate " " wrk-sheet.gsh-qty SPACE(13)
                       v-sht-size format "x(64)" " "
                      /* v-die-size FORM "x(20)"  " " */
                       eb.die-no   format "x(20)"
                       v-up
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

        PUT v-fill.

        /* gdm - 11030807 */
        /*PUT "<B><C1>Customer P/N           COLOR DESCRIPTION                  Customer P/N           COLOR DESCRIPTION                </B>"
                SKIP.

        FOR EACH bf-jobhdr NO-LOCK 
            WHERE bf-jobhdr.company = job-hdr.company
              AND bf-jobhdr.job-no = job-hdr.job-no
              AND bf-jobhdr.job-no2 = job-hdr.job-no2
            BREAK BY bf-jobhdr.frm:


            IF LAST-OF(bf-jobhdr.frm) THEN DO: 

                FOR EACH tt-reftable NO-LOCK 
                    WHERE tt-reftable.reftable EQ "jc/jc-calc.p"
                      AND tt-reftable.company  EQ job-hdr.company
                      AND tt-reftable.loc      EQ ""
                      AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")
                      AND (tt-reftable.est-type <> 4 or
                           tt-reftable.val[12] = bf-jobhdr.frm)
                    BREAK BY tt-reftable.val[12].                    

                    IF FIRST-OF(tt-reftable.val[12]) THEN DO:
                        FOR each ef
                            WHERE ef.company EQ job-hdr.company
                              AND ef.est-no  EQ bf-jobhdr.est-no
                              AND ef.form-no = tt-reftable.val[12]
                            BY  ef.est-no BY ef.form-no:
                            j = 0.
                            FOR EACH eb NO-LOCK
                                WHERE eb.company EQ ef.company
                                  AND eb.est-no    EQ ef.est-no
                                  AND eb.form-no   EQ ef.form-no
                                BY eb.form-no BY eb.blank-no:

                                j = j + 1.

                                PUT 
                                    eb.part-no FORMAT "x(20)"
                                    eb.i-coldscr FORMAT "x(35)".

                                IF j EQ 2 THEN DO:
                                    PUT SKIP.
                                    j = 0.
                                END.

                            END. /* eb */
                        END. /* ef */                
                    END. /* FIRST-OF(tt-reftable.val[12]) */
                END. /* FOR EACH tt-reftable */                       
            END. /* LAST-OF(bf-jobhdr.frm)*/
        END. /* FOR EACH bf-jobhdr */*/
        
        PUT v-fill /* AT 1 
                SKIP */.


          /** PRINT INK ***/
        PUT "<B><C1>FORM INK PASS           INK NAME                          LBS          FORM INK PASS           INK NAME                          LBS </B>"
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

             ASSIGN
               x = 2
               v-ink1 = ""
               v-ink2 = ""
               v-ink3 = "".
             
             for each wrk-ink WHERE wrk-ink.form-no EQ tt-reftable.val[12]
                break by wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no
                       :
                IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.

                IF FIRST-OF(wrk-ink.blank-no) THEN v-ink-use-per-blank = 0.

                v-item[i] = IF LOOKUP(string(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ",".
                v-ink-use-per-blank = v-ink-use-per-blank + 1.
                
                IF FIRST-OF(wrk-ink.i-code) /*OR v-ink-use-per-blank > 1 */ THEN DO: 
                   v-i-qty = 0.
                   FOR EACH bf-ink WHERE bf-ink.form-no = wrk-ink.form-no
                                     AND bf-ink.i-pass = wrk-ink.i-pass
                                     AND bf-ink.i-code = wrk-ink.i-code:
                       v-i-qty = v-i-qty + bf-ink.i-qty.
                   END.

                   IF wrk-ink.i-pass = 1 THEN
                       ASSIGN v-ink1[i] = STRING(wrk-ink.i-seq,">9") + "  " + "1  " +
                                          string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(21)") + " " + /*trim(v-alloc) + " " + */
                                          STRING(v-i-qty,">>>,>>9.99")
                                            /*v-item[i]*/
                                          /*+ (IF i = 1 THEN "  " + eb.plate-no ELSE "") */
                              i = i + 1         . 
                    ELSE IF wrk-ink.i-pass = 2 THEN
                       ASSIGN v-ink2[i] = STRING(wrk-ink.i-seq,">9") + "  " + "2  " + 
                                   string(wrk-ink.i-code,"X(15)") + " " + 
                                   string(wrk-ink.i-dscr,"x(21)") + " " /*+ trim(v-alloc) + " "*/ +
                                   STRING(v-i-qty,">>>,>>9.99") /*v-item[i]*/
                              i = i + 1.

                    ELSE IF wrk-ink.i-pass = 3 THEN
                       ASSIGN v-ink3[i] = STRING(wrk-ink.i-seq,">9") + "  " + "3  " + 
                                   string(wrk-ink.i-code,"X(15)") + " " + 
                                   string(wrk-ink.i-dscr,"x(21)") + " " +
                                   STRING(v-i-qty,">>>,>>9.99")
                              i = i + 1.
                END. 
                /*delete wrk-ink.*/
             end. /* each wrk-ink */
 
             v-skip = NO.
             v-plate-printed = NO.
             DO j = 1 TO EXTENT(v-ink1):
                IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
                IF v-ink1[j] <> "" THEN DO:
                   PUT tt-reftable.val[12] FORMAT ">>"
                       "    " v-ink1[j] FORM "x(65)" .
                   IF v-skip THEN DO:
                      IF NOT v-plate-printed THEN DO:
                         PUT eb.plate-no AT 150. /*155*/
                         v-plate-printed = YES. 
                      END.
                      PUT SKIP.
                   END.            
                   v-skip = NOT v-skip.
                END.
             END.
             /*IF NOT v-plate-printed THEN PUT eb.plate-no AT 106 SKIP.  */
            
             /*v-skip = NO.*/
             DO j = 1 TO EXTENT(v-ink2):
                IF TRIM(v-ink2[j]) = "-" THEN v-ink2[j] = "".                 
                IF v-ink2[j] <> "" THEN DO:
                   PUT tt-reftable.val[12] FORMAT ">>"
                       "    " v-ink2[j] FORM "x(65)" .
                   IF v-skip THEN PUT SKIP.             
                   v-skip = NOT v-skip.
                END.                
             END.

             DO j = 1 TO EXTENT(v-ink3):
                IF TRIM(v-ink3[j]) = "-" THEN v-ink3[j] = "".                 
                IF v-ink3[j] <> "" THEN DO:
                   PUT tt-reftable.val[12] FORMAT ">>"
                       "    " v-ink3[j] FORM "x(65)" .
                   IF v-skip THEN PUT SKIP.             
                   v-skip = NOT v-skip.
                END.                
             END.

             put v-fill at 1 skip .
    END. /*first-of(tt-reftable.val[12]*/
    END. /*tt-reftable*/
      END. /* last-of(bf-jobhdr.frm) */
          END. /* eadh bf-jobhdr*/

      /*PUT v-fill SKIP.*/
      IF s-run-speed THEN
         PUT "<B><C1>FORM MACHINE                 MR WASTE   MR HRS  RUN SPEED   SPOIL%     SHEETS  " /* MATERIAL FOR MACHINE          SIZE   TOTAL REQUIRED</B>"  */
               "<C72> CAD#         COUNT CASE SIZE                  CASES REQUIRED</B>"     
                SKIP.
      else PUT "<B><C1>FORM MACHINE                 MR WASTE   MR HRS   RUN HOUR    SPOIL%     SHEETS  " /* MATERIAL FOR MACHINE          SIZE   TOTAL REQUIRED</B>" */
               "<C72> CAD#         COUNT CASE SIZE                  CASES REQUIRED</B>"                    
               SKIP.
                 

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
                  and oe-ordl.i-no    eq v-stock-no2 /*job-hdr.i-no*/
                  no-lock no-error.
              IF bf-jobhdr.ord-no NE 0 AND NOT AVAIL oe-ordl THEN
                 find first oe-ordl where oe-ordl.company eq job-hdr.company
                               and oe-ordl.ord-no  eq bf-jobhdr.ord-no
                               and oe-ordl.i-no    eq v-stock-no2 /*job-hdr.i-no*/
                               no-lock no-error.
              
              v-tt-seq = v-tt-seq + 1.
              CREATE tt-size.
              ASSIGN tt-size.frm = tt-reftable.val[12]
                     tt-size.seq = v-tt-seq
                     tt-size.blank-no = b-eb.blank-no
                     tt-size.cad# = b-eb.cad-no /*cas-no*/
                     tt-size.cad-SIZE = string(b-eb.cas-len) + "x" + string(b-eb.cas-wid) + "x" +
                                     string(b-eb.cas-dep)
                     tt-size.COUNT = /*IF AVAIL oe-ordl AND oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt ELSE*/
                                     b-eb.cas-cnt 
                     tt-size.vend-part = IF AVAIL ITEM THEN ITEM.i-dscr ELSE ""
                    tt-size.cas-pal = b-eb.cas-pal.
           END.
       END.
       
         FIND FIRST tt-size USE-INDEX tt-size WHERE tt-size.frm = int(tt-reftable.val[12]) NO-LOCK NO-ERROR.                     
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
                   PUT /*string(line-counter)*/
                       wrk-op.s-num "  " wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[wrk-op.s-num]   SPACE(5)
                       wrk-op.mr[wrk-op.s-num]         SPACE(5)
                       wrk-op.speed[wrk-op.s-num]      SPACE(5)
                       wrk-op.spoil[wrk-op.s-num]      SPACE(2)
                       wrk-op.num-sh[wrk-op.s-num] SPACE(3) "  |    "
                       /*v-mat-for-mach FORM "x(60)" */.
                
               ELSE
                   PUT /*string(line-counter)*/
                      wrk-op.s-num "  " wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[wrk-op.s-num]   SPACE(5)
                       wrk-op.mr[wrk-op.s-num]         SPACE(5)
                       wrk-op.run-hr[wrk-op.s-num]     SPACE(5)
                       wrk-op.spoil[wrk-op.s-num]      SPACE(2)                       
                       wrk-op.num-sh[wrk-op.s-num] SPACE(3) "  |    "
                     /*  v-mat-for-mach FORM "x(60)" */.
                   END.
             ELSE 
                 PUT wrk-op.s-num "  " wrk-op.m-dscr   SPACE(5)
                     /* wrk-op.mr-waste[job-hdr.frm] */  SPACE(10)
                      /*wrk-op.mr[job-hdr.frm] >>9.99 */  SPACE(11)
                      /*wrk-op.speed[job-hdr.frm] >>>>9  */    SPACE(10)
                      /*wrk-op.spoil[job-hdr.frm]   >>9.99*/   SPACE(19) "  |    "
                    /*  v-mat-for-mach FORM "x(60)" */ .
                 
                 IF LINE-COUNTER > 48 THEN do:
                  PAGE.
                 END.
              
               
             IF AVAIL tt-size THEN
                PUT /*trim(string(tt-size.frm,">>9")) + "-" +
                    trim(string(tt-size.blank-no,">>9")) form "X(5)" " "*/
                    tt-size.cad# "  " tt-size.COUNT "  "
                    tt-size.cad-size "  "
                    /*tt-size.vend-part */
                   tt-size.cas-pal.

             PUT SKIP.     
             FIND NEXT tt-size USE-INDEX tt-size WHERE tt-size.frm = int(tt-reftable.val[12]) NO-LOCK NO-ERROR.
        end. /* each wrk-op*/

        DO WHILE AVAIL tt-size:
           IF AVAIL tt-size THEN
                PUT "|" AT 100 /*trim(string(tt-size.frm,">>9")) + "-" +
                    trim(string(tt-size.blank-no,">>9")) form "X(5)" " "*/
                    tt-size.cad# "  " tt-size.COUNT " "
                    "  " tt-size.cad-size "  " /*tt-size.vend-part */ tt-size.cas-pal
                     SKIP.

           FIND NEXT tt-size USE-INDEX tt-size WHERE tt-size.frm = int(tt-reftable.val[12]) NO-LOCK NO-ERROR.
        END.

      
                                                 /*need skip here otherwise wipes out first line of next section*/
       /*IF NOT last(tt-reftable.val[12]) THEN*/  PUT v-fill4 SKIP /*AT 1*/  /*SKIP*/.
       
    END. /*first-of(tt-reftable.val[12] */
    END. /*tt-reftable*/
 
        /** PRINT JOB INSTRUCTIONS **/
      END. /* last-of(bf-jobhdr.frm) */
      END. /* bf-johdr*/

      FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                             BREAK BY bf-jobhdr.frm:
       IF FIRST(bf-jobhdr.frm) THEN DO:
          FOR EACH tt-formtext:
              DELETE tt-formtext.
          END.
         /*PUT "<P14><B><C1>FORM DEPARTMENT      INSTRUCTION NOTES</B></P14>" SKIP. */
        
          FOR EACH notes WHERE notes.rec_key = job.rec_key 
                           and notes.note_code <> ''
                           AND LOOKUP(notes.note_code,v-exc-depts) EQ 0 NO-LOCK,
              FIRST dept NO-LOCK WHERE dept.code = notes.note_code
              BY notes.note_form_no BY dept.fc BY notes.note_date BY notes.note_time:
              
              lv-text = lv-text + 
                        string(notes.note_form_no,">9") + "     " + "<B>" + caps(dept.dscr) + ":</B>" + "     " + 
                        /*notes.note_title + "   " +*/ notes.note_text + CHR(10).
          END.
       END.

       FIND FIRST itemfg WHERE itemfg.company = bf-jobhdr.company
                           AND itemfg.i-no = bf-jobhdr.i-no NO-LOCK NO-ERROR.
       IF AVAIL itemfg then
          FOR EACH notes NO-LOCK WHERE notes.rec_key   EQ itemfg.rec_key
                                   AND notes.note_type EQ "S"
                                   AND CAN-DO(v-spec-list,notes.note_code),
              FIRST ITEM-spec NO-LOCK WHERE item-spec.company = itemfg.company
                                 AND item-spec.i-no = ""
                                 AND item-spec.code = notes.note_code
              BY item-spec.code BY notes.note_date BY notes.note_time:

           lv-text = lv-text + 
                     STRING(bf-jobhdr.frm,">9") + "     " +
                     "<B>" + CAPS(notes.note_code) + ":</B>" + "   " + 
                     /*notes.note_title + "   " + */ notes.note_text + CHR(10) .
       END.

       IF LAST(bf-jobhdr.frm) THEN DO:
          DO li = 1 TO 100:
              CREATE tt-formtext.
              ASSIGN tt-line-no = li
                     tt-length  = 115. 
          END.
          RUN custom/formtext.p (lv-text).
          i = 0.
          v-inst2 = "".

          note-count = 0.

          FOR EACH tt-formtext:
              i = i + 1.
              IF  i <= 100 THEN
                v-inst2[i] = tt-formtext.tt-text.

              IF v-inst2[i] <> "" THEN note-count = i.
          END.

          ASSIGN
            lv-do-page-skip = NO
            lv-page-line = PAGE-SIZE - LINE-COUNT.

          IF LINE-COUNT > PAGE-SIZE OR PAGE-SIZE - LINE-COUNT >= note-count THEN 
              DO i = 1 TO 100:
                 v-dept-inst[i] = IF v-inst2[i] BEGINS "<B>" THEN v-inst2[i]
                                ELSE "   " + v-inst2[i].
            END.
          ELSE DO:
              lv-do-page-skip = NO.
              DO i = 1 TO note-count:
                 v-dept-inst[i] = IF v-inst2[i] BEGINS "<B>" THEN v-inst2[i]
                                  ELSE "   " + v-inst2[i].
              END.
          END.

          IF lv-do-page-skip THEN DO:
              PAGE.
              PUT UNFORMATTED "    Continued..." SKIP.
              lv-do-page-skip = NO.
          END.

          PUT "<P14><B><C1>FORM DEPARTMENT      INSTRUCTION NOTES</B></P14>" SKIP.
          DO i = 1 TO note-count:
             IF v-dept-inst[i] NE "" THEN
                PUT "<P14>" v-dept-inst[i] "</P14>" FORM "x(115)" SKIP.

             IF LINE-COUNTER > 48 THEN do:
              PAGE.
             END. 
          END.
        end. /* first bf-jobhdr.frm */
      END. /* bf-jobhdr */

              

      PUT "<P10>".

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
             lv-cad-image = /*(IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") +*/  b-ef.cad-image .
                            /*b-ef.cad-image + ".JPG".*/
             lv-cad-image-list = lv-cad-image-list + b-ef.cad-image + ",".
             IF lv-cad-image > "" THEN DO:
               PAGE.
               PUT "<R3><C2><#21>".
               PUT unformatted "<=21><R+2><C3><#22><R+51><C+133><IMAGE#22=" lv-cad-image ">".
             END.
           END.
          END.
        END. /*each bf-jobhdr*/
      END.  /* print-box*/
      
      /* == task 072900514 */
      /*IF LAST(job-hdr.frm) THEN  DO:
         IF PAGE-SIZE > LINE-COUNT THEN PUT skip(PAGE-SIZE - LINE-COUNT - 1 ) "6/05 Job Ticket QF-130 Rev.A"   to 150.
         ELSE PUT skip(PAGE-SIZE - LINE-COUNT - 1) "6/05 Job Ticket QF-130 Rev.A"  to 150.
      END. */
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
    
OUTPUT STREAM st-st CLOSE.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
