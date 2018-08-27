/* ------------------------------------------------- cerep/jobppi.p  */
/*  factory ticket  for folding , Preferred                                   */
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
DEF VAR v-case-count LIKE eb.cas-cnt NO-UNDO.
DEF VAR v-case-qty AS dec NO-UNDO.

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
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-total-up AS INT NO-UNDO.
DEF VAR v-stacks-dec AS DEC NO-UNDO.
DEF VAR v-stacks-int AS INT NO-UNDO.
DEF VAR v-board-po AS INT NO-UNDO.
DEF VAR v-rct-qty LIKE po-ordl.t-rec-qty NO-UNDO.
DEF VAR v-carton AS CHAR FORMAT "X(26)" EXTENT 3 NO-UNDO.
DEF VAR v-spec-qc AS CHAR FORMAT "X(12)" NO-UNDO.
DEF VAR v-i-name AS CHAR FORMAT "X(26)" EXTENT 3 NO-UNDO.
DEF VAR v-case-size2 AS CHAR FORMAT "X(26)" EXTENT 3 NO-UNDO.

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER bf-item FOR ITEM.

DEF VAR v-cas-siz AS CHAR FORMAT "x(17)" NO-UNDO.
DEF VAR v-vertln  AS CHAR NO-UNDO.
DEF VAR v-vertln1 AS CHAR NO-UNDO.
DEF VAR v-vertln2 AS CHAR NO-UNDO.
DEF VAR v-vertln3 AS CHAR NO-UNDO.
DEF VAR v-vertln4 AS CHAR NO-UNDO.
DEF VAR v-vertln5 AS CHAR NO-UNDO.
DEF VAR v-vertln6 AS CHAR NO-UNDO.
DEF VAR v-vertln7 AS CHAR NO-UNDO.


def workfile w-lo
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
  FIELD spoil LIKE job-mch.wst-prct EXTENT 20
  FIELD mr-waste LIKE job-mch.mr-waste EXTENT 20    .

def new shared workfile wrk-die
  field die-no like eb.die-no
  FIELD cad-no LIKE eb.cad-no
  field form-no like eb.form-no
  field die-size as char format "x(17)".

def new shared workfile wrk-sheet
  field gsh-qty like ef.gsh-qty
  field cal like ef.cal
  FIELD i-no LIKE ITEM.i-no
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
  field i-pass as dec.

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

{custom/formtext.i NEW}     
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
DEF VAR v-i-qty AS DEC EXTENT 20 NO-UNDO.
DEF VAR v-ink1 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink2 AS cha EXTENT 20 NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no NO-UNDO.
DEF VAR lv-mat-dept-list AS cha INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEF VAR v-mat-for-mach AS cha NO-UNDO.
DEF BUFFER xjob-mat FOR job-mat.
DEF VAR v-fgitm AS cha FORM "x(15)" EXTENT 10 NO-UNDO.
DEF VAR v-fgprt AS cha FORM "x(15)" EXTENT 10 NO-UNDO.
DEF VAR v-count LIKE eb.tr-cnt EXTENT 10 NO-UNDO.
DEF VAR v-pono LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEF VAR v-num-of-fgitm AS INT NO-UNDO.
DEF TEMP-TABLE tt-fgitm FIELD i-no AS cha FORM "x(15)"
                        FIELD part-no AS cha FORM "x(15)"
                        FIELD seq AS INT
                        FIELD qty AS INT 
                        FIELD i-dscr AS cha
                        FIELD po-no AS cha
                        FIELD COUNT AS INT
                        FIELD i-name AS CHAR
                        FIELD case-size AS CHAR.
DEF VAR v-plate-printed AS LOG NO-UNDO.
DEF BUFFER xoe-ordl FOR oe-ordl.
DEF VAR v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name2 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-cust-name3 LIKE oe-ord.cust-name NO-UNDO.
DEF VAR v-last-j AS INT NO-UNDO.
DEF VAR v-po-no2 LIKE v-po-no NO-UNDO.
DEF VAR v-po-no3 LIKE v-po-no NO-UNDO.
/*DEF VAR v-lbs AS DEC FORM ">>,>>>,>>9" NO-UNDO.*/
DEF VAR v-dept-title AS cha NO-UNDO.
DEF VAR v-dept-note-printed AS LOG.
def new shared frame head.
DEF SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF VAR v-po-duedate LIKE po-ordl.due-date NO-UNDO.
DEF VAR v-shipto1 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shipto2 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF VAR v-pass-count AS INT NO-UNDO.
DEF VAR v-stock-no LIKE eb.stock NO-UNDO.
DEF TEMP-TABLE tt-reftable LIKE reftable
                         FIELD est-type LIKE est.est-type.

DEF VAR v-lncnt AS INT NO-UNDO.

v-fill = "<||3><C1><FROM><C108><LINE><||3>".
format header
       "<B>JOB NUMBER:<P18><R-0.3><P14>" v-job-no space(0) "-" space(0) v-job-no2 format "99" "</B><P10><R+0.3>"
       "<B><P12>F A C T O R Y   T I C K E T</B><P10>" at 52  "JOB START DATE:" at 110 v-start-date skip
       v-fill                                                                       /*123*/
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

        first est NO-LOCK where est.company  eq job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          and est.est-type le 4,  
    EACH tt-reftable NO-LOCK /*WHERE tt-reftable.tt-reftable EQ "jc/jc-calc.p"
           AND tt-reftable.company  EQ job-hdr.company
           AND tt-reftable.loc      EQ ""
           AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")*/
        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2
              BY tt-reftable.val[12] /*job-hdr.frm*/ :

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

if first-of(tt-reftable.val[12]) then v-first = yes.

/** PRINT JOB HEADER **/
if v-first then do:
assign
v-job-no  = job-hdr.job-no
v-job-no2 = job-hdr.job-no2.


if avail oe-ord then
if not oe-ctrl.p-fact and (oe-ord.stat eq "H" OR oe-ord.priceHold) then next.

v-due-date = if avail oe-ord then oe-ord.due-date else ?.
v-start-date = job-hdr.start-date.

if not first(job-hdr.job-no) then page.
view frame head.

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
        FIND FIRST cust WHERE cust.company = job-hdr.company AND
                              cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.
        v-req-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?.
        v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                      ELSE IF AVAIL cust THEN cust.name
                      ELSE job-hdr.cust-no.

        PUT "<B>Customer Name:<P18><R-0.3>" v-cust-name "<P10><R+0.3>"
            "<B>REQ DATE:   DUE DATE:   Estimate:" SKIP
            "Shipto:</B>" v-shipto[1] v-req-date AT 73 v-due-date AT 85 TRIM(job-hdr.est-no) FORMAT "x(8)" AT 97
            SKIP                                    
            v-shipto[2] AT 7 SKIP
            v-shipto[4] AT 7 SKIP
            v-fill SKIP.     

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
              AND job-mch.frm = int(tt-reftable.val[12]) /*job-hdr.frm*/
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
           wrk-op.run-hr[job-mch.frm] = job-mch.run-hr    .
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

          /*if est.est-type eq 4 and eb.stock-no ne job-hdr.i-no then next ebloop. */

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
          /*if (est.est-type eq 4 and eb.stock-no eq job-hdr.i-no) OR (est.est-type <> 4) THEN??? */
          for each job-mat
              where job-mat.company eq cocode
                and job-mat.job     eq job-hdr.job
                and job-mat.frm     eq eb.form-no
              no-lock,
              first item
              {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no
              no-lock:

            do i = 1 to 15:
              if eb.i-code2[i] eq job-mat.i-no then do:

                find first wrk-ink
                    where wrk-ink.i-code   eq eb.i-code2[i]
                      and wrk-ink.form-no  eq eb.form-no
                      and wrk-ink.blank-no eq eb.blank-no
                      AND wrk-ink.i-pass = eb.i-ps2[i]
                    no-error.

                if not avail wrk-ink then do:
                  create wrk-ink.
                  assign
                   wrk-ink.i-code   = eb.i-code2[i]
                   wrk-ink.form-no  = eb.form-no
                   wrk-ink.blank-no = eb.blank-no
                   wrk-ink.i-dscr   = eb.i-dscr2[i]
                   wrk-ink.i-pass   = eb.i-ps2[i].
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
               wrk-ink.i-pass   = 1.
            end.
            if avail wrk-ink AND
               ((est.est-type eq 4 and eb.form-no = job-mat.frm AND eb.blank-no = job-mat.blank-no) OR
                 est.est-type <> 4 ) 
                 then wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.            
            
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
             v-dsc[2] = eb.part-dscr2.
             
            IF FIRST-OF(eb.form-no) THEN
            DO:
              PUT "<P9><B>F/B FG Item #       #UP   Job Qty PO#             Description          Style Spc/QC File# CAD#         Case Size         Count  HT  Pack  Tot</B>" SKIP.
              v-total-up = 0.
            END.
              
           ASSIGN
              v-job-qty = 0
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
                  and oe-ordl.i-no    eq v-stock-no /*job-hdr.i-no*/
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

            ASSIGN
               v-case-size = string(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
                            string(eb.cas-dep)
               v-up = eb.num-up
               v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE ""
               v-case-count = IF AVAIL oe-ordl AND oe-ordl.cas-cnt <> 0 THEN oe-ordl.cas-cnt
                              ELSE eb.cas-cnt
               v-case-qty = v-job-qty / v-case-count
               v-total-up = v-total-up + v-up.

            {sys/inc/roundup.i v-case-qty}

            /*strip decimals*/
            IF eb.tr-cas NE 0 THEN
               v-stacks-dec = eb.cas-pal / eb.tr-cas.
            ELSE
               v-stacks-dec = 0.

            IF v-stacks-dec - INTEGER(v-stacks-dec) < 0 THEN
               v-stacks-int = INTEGER(v-stacks-dec) - 1.
            ELSE
               v-stacks-int = INTEGER(v-stacks-dec).

            FIND FIRST b-itemfg WHERE b-itemfg.company EQ eb.company
                                AND b-itemfg.i-no    EQ eb.stock-no
                                AND eb.stock-no    NE "" NO-LOCK NO-ERROR.

            FIND FIRST bf-ITEM WHERE bf-ITEM.company = eb.company
                                 AND TRIM(bf-ITEM.i-no) = TRIM(eb.cas-no) NO-LOCK NO-ERROR.
            IF AVAIL bf-ITEM 
              THEN ASSIGN v-cas-siz = STRING(bf-ITEM.case-l) + "X" + 
                                      STRING(bf-ITEM.case-w) + "X" + 
                                      STRING(bf-ITEM.case-d).
              ELSE ASSIGN v-cas-siz = "".
             
            display trim(string(eb.form-no,">>9")) + "-" +
                    trim(string(eb.blank-no,">>9")) FORM "x(3)" 
                    eb.stock-no FORMAT "X(15)" @ job-hdr.i-no 
                    v-up
                    v-job-qty /** v-fac*/ format ">,>>>,>>9"
                    oe-ordl.po-no FORMAT "X(15)" when avail oe-ordl
                    v-dsc[1]  FORM "x(20)"
                    eb.style  FORM "X(5)" /*v-stypart */
                    SUBSTR(eb.spc-no,1,12) FORM "X(12)" /* gdm */
                    eb.cad-no FORM "X(12)"
                    v-cas-siz FORM "X(17)" 
                    eb.cas-cnt 
                    eb.tr-cas SPACE(2)
                    b-itemfg.prod-notes FORMAT "X(5)" WHEN AVAIL b-itemfg
/*                     SPACE(5) */
/*                     "<C99.5>"STRING(v-stacks-int,">>>>9") */
                    "<C104>"STRING(eb.cas-pal,">>9")
                    skip
                with stream-io width 175 no-labels no-box frame line-det1.

            

            v-itm-printed = v-itm-printed + 1.    
            

            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".
          /* end. /* last-of(eb.form-no) */      */
          IF LAST-OF(eb.form-no) THEN DO:

             IF v-itm-printed < 4 THEN PUT SKIP(4 - v-itm-printed).

             /* Number of sheets ticket1.p - single board, ticket2.p - multi board */
             run oe/rep/ticket2.p (recid(ef), recid(job-hdr)).
             
             IF AVAIL oe-ordl THEN
               FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                            AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
             v-vend = IF AVAIL oe-ordl THEN oe-ordl.vend-no ELSE "".
             v-board-po = IF AVAIL oe-ordl THEN oe-ordl.po-no-po ELSE 0.
             IF AVAIL po-ord THEN
                 FIND FIRST po-ordl WHERE
                      po-ordl.company EQ po-ord.company and
                      po-ordl.po-no EQ po-ord.po-no AND
                      po-ordl.i-no = ef.board
                      NO-LOCK NO-ERROR.

             ASSIGN
               v-po-duedate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

             IF AVAIL po-ordl THEN
                RUN qty-in-ord-uom(OUTPUT v-rct-qty).
             ELSE
                v-rct-qty = 0.       
                
              ASSIGN 
                     v-vertln2 = "<R+" + STRING(v-itm-printed + 2) + "><LINE>"
                     v-vertln1 = "<C15.5><R" + STRING(9 - v-lncnt) + "><FROM><C15.5>" + "<R+" + STRING(v-itm-printed + 2 + v-lncnt) + "><LINE>"
                     v-vertln2 = "<C18.5><R" + STRING(9 - v-lncnt) + "><FROM><C18.5>" + "<R+" + STRING(v-itm-printed + 2 + v-lncnt) + "><LINE>"
                     v-vertln3 = "<C26><R"   + STRING(9 - v-lncnt) + "><FROM><C26>"   + "<R+" + STRING(v-itm-printed + 2 + v-lncnt) + "><LINE>"
                     v-vertln4 = "<C54><R"   + STRING(9 - v-lncnt) + "><FROM><C54>"   + "<R+" + STRING(v-itm-printed + 2 + v-lncnt) + "><LINE>"
                     v-vertln5 = "<C68><R"   + STRING(9 - v-lncnt) + "><FROM><C68>"   + "<R+" + STRING(v-itm-printed + 2 + v-lncnt) + "><LINE>"
                     v-vertln6 = "<C78><R"   + STRING(9 - v-lncnt) + "><FROM><C78>"   + "<R+" + STRING(v-itm-printed + 2 + v-lncnt) + "><LINE>"
                     v-vertln7 = "<C96.5><R" + STRING(9 - v-lncnt) + "><FROM><C96.5>" + "<R+" + STRING(v-itm-printed + 2 + v-lncnt) + "><LINE>"
                  .

             PUT v-vertln1 FORMAT "x(50)"
                 v-vertln2 FORMAT "x(50)"
                 v-vertln3 FORMAT "x(50)"
                 v-vertln4 FORMAT "x(50)"
                 v-vertln5 FORMAT "x(50)"
                 v-vertln6 FORMAT "x(50)"
                 v-vertln7 FORMAT "x(50)"
                 "<P10>" v-fill SKIP                       /*REQ'D*/
                   "<B>BOARD CODE            SHEETS       SHEET SIZE    DIE SIZE          DIE#             TOTAL #UP </B>" 
/*                 "<B>BOARD CODE            VENDOR      SHEETS       SHEET SIZE    DIE#             TOTAL #UP BOARD PO# QTY RECEIVED</B>" */
                 SKIP.
            /** PRINT SHEET **/

             v-lncnt = 1.
             x = 2.
             for each wrk-sheet WHERE wrk-sheet.form-no = ef.form-no
                 /*break by wrk-sheet.form-no*/:
               find first ITEM where item.company eq cocode
                                 and item.i-no    eq wrk-sheet.i-no no-lock no-error.
               /*v-lbs = wrk-sheet.gsh-qty * (wrk-sheet.sh-wid * wrk-sheet.sh-len / 144) / 1000 * ITEM.basis-w.*/
               FIND first notes WHERE notes.rec_key = job.rec_key AND
                                      notes.note_code = "BS" AND
                                      notes.note_form_no = wrk-sheet.form-no NO-LOCK NO-ERROR.
               v-dept-title = IF AVAIL notes THEN notes.note_title ELSE "".
                  
               display wrk-sheet.brd-dscr
/*                        v-dept-title FORM "x(10)" */
                    wrk-sheet.gsh-qty 
                    /*v-lbs*/ SPACE(8)
                    string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len) format "x(13)"
                    STRING(ef.trim-w) + "x" + STRING(ef.trim-l) FORM "x(16)"
/*                     wrk-die.die-size FORMAT "x(13)" */
                    eb.die-no FORMAT "x(15)" 
                    STRING(v-total-up,">>>>>9")
                   /*
                    v-board-po FORM ">>>>>>" SPACE(2)
                    v-rct-qty FORM "->>>,>>>,>>9.9<<<<<"
                    */
                    with stream-io width 170 no-labels no-box frame sheet.
               x = 1.
             end. /* each wrk-sheet */
             if x ne 2 then put v-fill at 1 skip.
                     
             /** PRINT INK **/
             PUT "<B>PASS    LBS CODE       INK NAME           ITEMS     PASS    LBS CODE       INK NAME           ITEMS       PLATE #</B>"
                 SKIP.
             x = 2.
             i = 1.
             v-ink1 = "".
             v-ink2 = "".
             v-pass-count = 0.
             for each wrk-ink WHERE wrk-ink.form-no = eb.form-no
                 break by wrk-ink.i-pass:
                 IF FIRST-OF(wrk-ink.i-pass) THEN v-pass-count = v-pass-count + 1.
             END.
             for each wrk-ink WHERE wrk-ink.form-no = eb.form-no
                break by wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no
                       :
                IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.
                IF FIRST-OF(wrk-ink.i-code) THEN ASSIGN v-item[i] = ""
                                                        v-i-qty[i] = 0.

                v-item[i] = IF LOOKUP(string(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ",".
                v-i-qty[i] = v-i-qty[i] + wrk-ink.i-qty.
                IF LAST-OF(wrk-ink.i-code) THEN DO:
                    IF SUBSTRING(v-item[i],LENGTH(v-item[i]),1) = "," THEN v-item[i] = substring(v-item[i],1,LENGTH(v-item[i]) - 1).                    
                    v-alloc = v-item[i].
                    if num-entries(v-item[i]) gt 1 then do:
                       v-alloc = "".
                       do j = 1 to num-entries(v-item[i]):
                          if j eq 1 THEN v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                          ELSE IF j eq num-entries(v-item[i]) THEN
                          DO:
                            if substr(v-alloc,length(trim(v-alloc)),1) eq "-" AND
                               int(entry(j,v-item[i])) - int(entry(j - 1,v-item[i])) GT 1 THEN
                               v-alloc = v-alloc + entry(j - 1,v-item[i]) + ",".

                            v-alloc = v-alloc + entry(j,v-item[i]) + ",".
                          END.
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
                    IF wrk-ink.i-pass = 1 THEN
                       ASSIGN v-ink1[i] = (IF v-pass-count = 1 THEN "F    " ELSE "B    ") +
                                          /*string(wrk-ink.i-code,"X(15)")*/ STRING(v-i-qty[i],">>9.99") + " " + string(wrk-ink.i-code,"x(10)") + " " +
                                          string(wrk-ink.i-dscr,"x(20)") + " " + trim(v-alloc) /*v-item[i]*/
                                          /*+ (IF i = 1 THEN "  " + eb.plate-no ELSE "") */
                              i = i + 1         . 
                    ELSE IF wrk-ink.i-pass = 2 THEN
                       ASSIGN v-ink2[i] = "F    " + /*string(wrk-ink.i-code,"X(15)")*/ STRING(v-i-qty[i],">>9.99") + " " + string(wrk-ink.i-code,"x(10)") + " " +
                                   string(wrk-ink.i-dscr,"x(20)") + " " + trim(v-alloc) /*v-item[i]*/
                              i = i + 1.     
                END.
                delete wrk-ink.
             end. /* each wrk-ink */
             v-skip = NO.
             v-plate-printed = NO.
             DO j = 1 TO 20:
                IF TRIM(v-ink1[j]) = "-" THEN v-ink1[j] = "".               
                IF v-ink1[j] <> "" THEN do:
                   IF v-skip THEN do:
                       PUT  v-ink1[j] FORM "x(52)" .
                       IF j = 2 THEN do:
                           PUT eb.plate-no AT 107.
                           v-plate-printed = YES.
                       END.
                       PUT SKIP.
                   END.
                   ELSE PUT v-ink1[j] FORM "x(52)".                                                             
                   v-skip = NOT v-skip.             
                END.
             END.
             IF NOT v-plate-printed THEN PUT eb.plate-no AT 107  SKIP.
             DO j = 1 TO 20:
                 IF TRIM(v-ink2[j]) = "-" THEN v-ink2[j] = "".                 
                IF v-ink2[j] <> "" THEN do:
                   IF v-skip THEN PUT v-ink2[j] FORM "x(52)" SKIP.
                   ELSE PUT v-ink2[j] FORM "x(52)".
                   v-skip = NOT v-skip.
                END.                
             END.
             put v-fill at 1 skip.
          END. /* last-of(eb.form-no) */
        end. /* each eb */
      end. /* each ef */
      end. /* first job-no */

      if last-of(tt-reftable.val[12]) then do:
         IF s-run-speed THEN
            PUT "<B>MACHINE                 MR WASTE   MR HRS  RUN SPEED   SPOIL%    SHEETS PER MACHINE   SIZE  TOTAL REQUIRED  PALLET</B>"
                SKIP.
         else
            PUT "<B>MACHINE                 MR WASTE   MR HRS   RUN HOUR    SPOIL%    SHEETS PER MACHINE   SIZE  TOTAL REQUIRED  PALLET</B>"
                SKIP.

         FOR EACH wrk-op WHERE wrk-op.s-num = tt-reftable.val[12] BREAK by wrk-op.d-seq by wrk-op.b-num:
             v-mat-for-mach = "".
             IF lookup(wrk-op.dept,lv-mat-dept-list) > 0 THEN DO:                 
                FOR EACH xjob-mat WHERE xjob-mat.company eq cocode
                                       and xjob-mat.job     eq job-hdr.job
                                       and xjob-mat.job-no  eq job-hdr.job-no
                                       and xjob-mat.job-no2 eq job-hdr.job-no2
                                       AND xjob-mat.frm = int(tt-reftable.val[12]) /*job-hdr.frm*/
                                       /*AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0)*/  NO-LOCK,
                     first ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND
                                      ITEM.mat-type = SUBSTRING(wrk-op.dept,1,1) NO-LOCK :
                     v-mat-for-mach = /*ITEM.i-name + fill(" ", 33 - LENGTH(ITEM.i-name))  /*"       " */ + */
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
                                       AND xjob-mat.frm = int(tt-reftable.val[12])
                                       AND (xjob-mat.blank-no = job-hdr.blank-no
                                            OR xjob-mat.blank-no = 0) NO-LOCK,
                     first ITEM WHERE ITEM.company = cocode AND
                                      ITEM.i-no = xjob-mat.rm-i-no AND 
                                      ITEM.mat-type = "D" NO-LOCK :
                     v-mat-for-mach = v-mat-for-mach +
                                      (IF LENGTH(v-mat-for-mach) <= 20 THEN FILL(" ", 20 - LENGTH(v-mat-for-mach))
                                      ELSE "  " ) +
                                      ITEM.i-name.
                 END.
             END.          
             IF s-prt-mstandard THEN DO:
                IF s-run-speed THEN
                   PUT wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[int(tt-reftable.val[12])]   SPACE(5)
                       wrk-op.mr[int(tt-reftable.val[12])]         SPACE(5)
                       wrk-op.speed[int(tt-reftable.val[12])]      SPACE(5)
                       wrk-op.spoil[int(tt-reftable.val[12])]      SPACE(5)
                       wrk-op.num-sh[int(tt-reftable.val[12])]     SPACE(11)  /*9+9*/
                       v-mat-for-mach FORM "x(40)"    /*60*/
                       SKIP.
               ELSE
                   PUT wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[int(tt-reftable.val[12])]   SPACE(5)
                       wrk-op.mr[int(tt-reftable.val[12])]         SPACE(5)
                       wrk-op.run-hr[int(tt-reftable.val[12])]     SPACE(5)
                       wrk-op.spoil[int(tt-reftable.val[12])]      SPACE(5)
                       wrk-op.num-sh[int(tt-reftable.val[12])]     SPACE(11)  /* 9 + 9 */
                       v-mat-for-mach FORM "x(40)"    /*60*/
                       SKIP.
             END.
             ELSE PUT wrk-op.m-dscr   SPACE(5)
                     /* wrk-op.mr-waste[job-hdr.frm]   */ SPACE(10)
                      /*wrk-op.mr[job-hdr.frm] >>9.99 */   SPACE(11)
                      /*wrk-op.speed[job-hdr.frm] >>>>9*/      SPACE(10)
                      /*wrk-op.spoil[job-hdr.frm]   >>9.99*/   SPACE(12)
                      wrk-op.num-sh[int(tt-reftable.val[12])]     SPACE(11)  /* 9 + 9 */
                      v-mat-for-mach FORM "x(40)" /*60*/
                      SKIP.

        end. /* each wrk-op*/
        PUT v-fill AT 1 SKIP.
        /** PRINT JOB INSTRUCTIONS **/
        /* dept notes*/
        lv-line-chars = 128.
        FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
        FOR EACH tt-formtext:
          DELETE tt-formtext.
        END.
        lv-text = "".
        v-dept-inst = "".
        v-exc-depts = v-exc-depts + (IF v-exc-depts <> "" THEN ",BS" ELSE "BS").
        v-dept-note-printed = NO.
        FOR EACH notes WHERE notes.rec_key = job.rec_key
                    AND (notes.note_form_no = tt-reftable.val[12] /*job-hdr.frm*/ /*OR notes.note_form_no = 0*/)
                    AND LOOKUP(notes.note_code,v-exc-depts) EQ 0 NO-LOCK
                    BREAK BY notes.note_code:
            IF FIRST-OF(notes.note_code) THEN DO:
               lv-text = "".
               FOR EACH tt-formtext:
                   DELETE tt-formtext.
               END.
               FIND FIRST dept WHERE dept.CODE = notes.note_code NO-LOCK NO-ERROR.
               lv-text = (IF AVAIL dept THEN dept.dscr ELSE notes.note_code) + "     " + 
                         notes.note_title + " " + notes.note_text + CHR(10).
               DO li = 1 TO 4:
                  CREATE tt-formtext.
                  ASSIGN tt-line-no = li
                         tt-length  = 128.
               END.
               RUN custom/formtext.p (lv-text).
               i = 0.
               v-dept-inst = "".
               FOR EACH tt-formtext:
                   i = i + 1.
                   IF  i <= 4 THEN v-dept-inst[i] = tt-formtext.tt-text.      
               END.
               IF PAGE-SIZE - LINE-COUNTER < 2 THEN PAGE.
               IF first(notes.note_code) THEN PUT "<B>DEPARTMENT   INSTRUCTION NOTES</B>" SKIP.

               IF TRIM(v-dept-inst[1]) NE "" THEN PUT v-dept-inst[1] FORM "x(128)" SKIP.
               IF TRIM(v-dept-inst[2]) NE "" THEN PUT v-dept-inst[2] FORM "x(128)" SKIP.
               IF TRIM(v-dept-inst[3]) NE "" THEN PUT v-dept-inst[3] FORM "x(128)" SKIP.
               IF TRIM(v-dept-inst[4]) NE "" THEN PUT v-dept-inst[4] FORM "x(128)" SKIP.
               
            END. /* first-of(notes.note_code) */
            v-dept-note-printed = YES.
        END. /* for each notes */
       /*==== note ======*/
    /*  IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */ */
        IF NOT v-dept-note-printed THEN DO:
          IF PAGE-SIZE - LINE-COUNTER < 2 THEN PAGE.
          PUT "<B>DEPARTMENT   INSTRUCTION NOTES</B>" SKIP.

          IF TRIM(v-dept-inst[1]) NE "" THEN PUT v-dept-inst[1] FORM "x(128)" SKIP.
          IF TRIM(v-dept-inst[2]) NE "" THEN PUT v-dept-inst[2] FORM "x(128)" SKIP.
          IF TRIM(v-dept-inst[3]) NE "" THEN PUT v-dept-inst[3] FORM "x(128)" SKIP.
          IF TRIM(v-dept-inst[4]) NE "" THEN PUT v-dept-inst[4] FORM "x(128)" SKIP.
        END.
            
        FIND first ef WHERE ef.company EQ job-hdr.company
                        AND ef.est-no  EQ job-hdr.est-no
                        AND ef.form-no = tt-reftable.val[12] /*job-hdr.frm*/ NO-LOCK NO-ERROR.
        IF AVAIL ef THEN DO:
           IF PAGE-SIZE - LINE-COUNTER < 2 THEN PAGE.
           PUT v-fill skip
               "<B>Special Materials             Miscellaneous - SubContract                                  Prep</B>"
               SKIP.
           j = 1.
           DO i = 1 TO 8:
              IF ef.spec-no[i] <> "" THEN ASSIGN  v-spec-no[j] = ef.spec-no[i]
                                                  j = j + 1.              
           END.
           j = 1.
           DO i = 1 TO 6:
              IF ef.mis-cost[i] <> "" THEN
                 ASSIGN  v-misc[j] = string(ef.mis-snum[i]) + "-" + STRING(ef.mis-bnum[i]) +
                                     ef.mis-cost[i]
                         j = j + 1.                          
           END.
           j = 1.
           v-prep = "".
           FOR EACH wrk-prep WHERE wrk-prep.s-num = ef.form-no NO-LOCK BREAK BY wrk-prep.CODE:
               
               IF first-of(wrk-prep.code) AND  wrk-prep.CODE <> "" AND j < 9 THEN 
                     ASSIGN v-prep[j] = wrk-prep.CODE + " " + wrk-prep.dscr
                            j = j + 1.
           END.
        END.  /* avail ef */
        
        DO i = 1 TO 6:
            IF v-spec-no[i] <> "" THEN PUT v-spec-no[i].
            IF v-misc[i] <> "" THEN PUT v-misc[i] AT 32 FORM "x(30)" .
            IF v-prep[i] <> "" THEN PUT v-prep[i] AT 90.
            IF v-spec-no[i] <> "" OR v-misc[i] <> "" OR v-prep[i] <> "" THEN PUT SKIP.
        END.
        DO i = 7 TO 8:
            IF v-spec-no[i] <> "" THEN PUT v-spec-no[i].
            IF v-prep[i] <> "" THEN PUT v-prep[i] AT 90.
            IF v-spec-no[i] <> "" OR v-prep[i] <> "" THEN PUT SKIP.
        END.
        
        i = 1.
        v-fgitm = "".
        v-fgprt = "".
        FOR EACH tt-fgitm.
            DELETE tt-fgitm.
        END.

        for each xjob-hdr where xjob-hdr.company eq cocode
                            and xjob-hdr.job     eq job-hdr.job
                            and xjob-hdr.job-no  eq job-hdr.job-no
                            and xjob-hdr.job-no2 eq job-hdr.job-no2
                            and xjob-hdr.frm     eq int(tt-reftable.val[12]) /*job-hdr.frm*/ NO-LOCK BY xjob-hdr.blank-no:

           find first xoe-ordl
                where xoe-ordl.company eq xjob-hdr.company
                  and xoe-ordl.ord-no  eq xjob-hdr.ord-no
                  and xoe-ordl.job-no  eq xjob-hdr.job-no
                  and xoe-ordl.job-no2 eq xjob-hdr.job-no2
                  and xoe-ordl.i-no    eq xjob-hdr.i-no
                  no-lock no-error.
           
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

           /*v-count[i] = IF AVAIL b-eb THEN b-eb.cas-cnt ELSE 0.*/
           FIND FIRST itemfg NO-LOCK
               WHERE itemfg.company EQ xjob-hdr.company
                 AND itemfg.i-no    EQ xjob-hdr.i-no
               NO-ERROR.

           FIND FIRST ITEM WHERE ITEM.company = xjob-hdr.company
                             AND trim(ITEM.i-no) = TRIM(b-eb.cas-no) NO-LOCK NO-ERROR.

           CREATE tt-fgitm.
           ASSIGN tt-fgitm.i-no = xjob-hdr.i-no
                  tt-fgitm.part-no = IF AVAIL xoe-ordl THEN xoe-ordl.part-no
                                     ELSE
                                     IF AVAIL b-eb THEN b-eb.part-no
                                     ELSE
                                     IF AVAIL itemfg THEN itemfg.part-no
                                     ELSE xjob-hdr.i-no
                  tt-fgitm.qty = IF AVAIL xoe-ordl AND xoe-ordl.cas-cnt <> 0 THEN xoe-ordl.cas-cnt 
                                 ELSE IF AVAIL b-eb THEN b-eb.cas-cnt ELSE 0 /*xjob-hdr.qty*/
                  tt-fgitm.i-dscr = IF AVAIL b-eb THEN string(b-eb.len) + "x" + string(b-eb.wid)
                                  + "x" + string(b-eb.dep) ELSE ""
                  tt-fgitm.po-no = IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE ""
                  tt-fgitm.seq = i
                  tt-fgitm.count = IF AVAIL b-eb THEN b-eb.cas-cnt ELSE 0
                  tt-fgitm.i-name = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
                  tt-fgitm.case-size = IF AVAIL ITEM THEN STRING(ITEM.case-l) + "X" + STRING(ITEM.case-w) + "X" + STRING(ITEM.case-d) ELSE "".

           IF tt-fgitm.qty = 0 AND
              CAN-FIND(FIRST b-eb WHERE b-eb.company EQ xjob-hdr.company
                                AND b-eb.est-no  eq xjob-hdr.est-no
                                AND b-eb.form-no = 0) 
           THEN DO:
              FOR EACH b-eb WHERE b-eb.company EQ xjob-hdr.company
                              AND b-eb.est-no  eq xjob-hdr.est-no 
                              AND b-eb.form-no > 0 NO-LOCK:
                  tt-fgitm.qty = tt-fgitm.qty + b-eb.cas-cnt.
              END.
           END.

           i = i + 1.
        END.
        IF s-prt-shipto THEN DO i = 1 TO 4:
            ASSIGN v-shipto1[i] = v-shipto[i]
                   v-shipto2[i] = v-shipto[i].
        END.

        /* label prints per item */
        ASSIGN v-cust-name2 = v-cust-name 
               v-cust-name3 = v-cust-name
               i = 0
               j = 0.
        
        FOR EACH tt-fgitm BY tt-fgitm.seq.
            
          IF PAGE-SIZE - LINE-COUNTER < 16 THEN DO: 
             PUT "<C85><R48>QF-CS02-01 Rev C. 10/09".
             PAGE.
          END.

          i = i + 1.
          ASSIGN v-fgitm[i] = tt-fgitm.i-no
                 v-fgprt[i] = tt-fgitm.part-no
                 v-pono[i] = tt-fgitm.po-no
                 v-count[i] = tt-fgitm.COUNT
                 v-carton[i] = tt-fgitm.i-dscr
                 v-i-name[i] = tt-fgitm.i-name
                 v-case-size2[i] = tt-fgitm.case-size.

          j = j + 1.
          IF i >= 3 THEN DO:
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
               "<B>Part Name:</B>" v-fgprt[1]
               "<B>Part Name:</B>"  WHEN v-fgitm[2] <> "" AT 52 v-fgprt[2] WHEN v-fgitm[2] <> "" 
               "<B>Part Name:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgprt[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "<B>FG Name:</B>" v-i-name[1]
               "<B>FG Name:</B>" WHEN v-fgitm[2] <> "" AT 52 v-i-name[2]  WHEN v-fgitm[2] <> "" 
               "<B>FG Name:</B>" WHEN v-fgitm[3] <> "" AT 104  v-i-name[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Carton Size:" v-carton[1]
               "Carton Size:"  WHEN v-fgitm[2] <> "" AT 45 v-carton[2]  WHEN v-fgitm[2] <> "" 
               "Carton Size:" WHEN v-fgitm[3] <> "" AT 90  v-carton[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Case Size:" v-case-size2[1]
               "Case Size:"  WHEN v-fgitm[2] <> "" AT 45 v-case-size2[2]  WHEN v-fgitm[2] <> "" 
               "Case Size:" WHEN v-fgitm[3] <> "" AT 90  v-case-size2[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Count:   " v-count[1]
               "Count:   "  WHEN v-fgitm[2] <> "" AT 45 v-count[2]  WHEN v-fgitm[2] <> "" 
               "Count:   "  WHEN v-fgitm[3] <> "" AT 90  v-count[3] WHEN v-fgitm[3] <> ""                               
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

            i = 0.
            ASSIGN v-fgitm = ""
                   v-fgprt = ""
                   v-carton = ""
                   v-fgitm = ""
                   v-pono = ""
                   v-count = 0
                   v-case-size2 = ""
                   v-i-name = "" .
            v-last-j = j.
          END. /* i = 3 */
        END.
        IF i > 0 THEN DO:

            IF PAGE-SIZE - LINE-COUNTER < 16 THEN DO:
             PUT "<C85><R48>QF-CS02-01 Rev C. 10/09".
             PAGE.
            END.

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
               "<B>Part Name:</B>" v-fgprt[1]
               "<B>Part Name:</B>"  WHEN v-fgitm[2] <> "" AT 52 v-fgprt[2] WHEN v-fgitm[2] <> "" 
               "<B>Part Name:</B>" WHEN v-fgitm[3] <> "" AT 104 v-fgprt[3] WHEN v-fgitm[3] <> ""
               SKIP
               "<B>FG Name:</B>" v-i-name[1]
               "<B>FG Name:</B>" WHEN v-fgitm[2] <> "" AT 52 v-i-name[2]  WHEN v-fgitm[2] <> "" 
               "<B>FG Name:</B>" WHEN v-fgitm[3] <> "" AT 104  v-i-name[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Carton Size:" v-carton[1]
               "Carton Size:"  WHEN v-fgitm[2] <> "" AT 45 v-carton[2]  WHEN v-fgitm[2] <> "" 
               "Carton Size:" WHEN v-fgitm[3] <> "" AT 90  v-carton[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Case Size:" v-case-size2[1]
               "Case Size:"  WHEN v-fgitm[2] <> "" AT 45 v-case-size2[2]  WHEN v-fgitm[2] <> "" 
               "Case Size:" WHEN v-fgitm[3] <> "" AT 90  v-case-size2[3] WHEN v-fgitm[3] <> "" 
               SKIP
               "Count:   " v-count[1]
               "Count:   "  WHEN v-fgitm[2] <> "" AT 45 v-count[2] WHEN v-fgitm[2] <> "" 
               "Count:   "  WHEN v-fgitm[3] <> "" AT 90 v-count[3] WHEN v-fgitm[3] <> ""      
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
          PUT "<C85><R48>QF-CS02-01 Rev C. 10/09". 
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
/****************************************************************************/
PROCEDURE qty-in-ord-uom:

  DEFINE OUTPUT PARAMETER op-qty AS DEC DECIMALS 10.

  DEF VAR ld AS DEC DECIMALS 10 EXTENT 2 NO-UNDO.

  DEF BUFFER b-po-ordl FOR po-ordl.
  
  FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ROWID(po-ordl) NO-LOCK NO-ERROR.

  ld[2] = b-po-ordl.t-rec-qty.

  IF b-po-ordl.item-type EQ YES                 AND
     b-po-ordl.pr-qty-uom NE b-po-ordl.cons-uom THEN DO:
    ld[2] = 0.

    FOR EACH rm-rcpth
        WHERE rm-rcpth.company   EQ b-po-ordl.company
          AND rm-rcpth.po-no     EQ STRING(b-po-ordl.po-no)
          AND rm-rcpth.i-no      EQ b-po-ordl.i-no
          AND rm-rcpth.rita-code EQ "R" NO-LOCK,
        EACH rm-rdtlh
        WHERE rm-rdtlh.r-no    EQ rm-rcpth.r-no
          AND rm-rdtlh.job-no  EQ b-po-ordl.job-no
          AND rm-rdtlh.job-no2 EQ b-po-ordl.job-no2
          AND rm-rdtlh.s-num   EQ b-po-ordl.s-num
        NO-LOCK:

      ld[1] = rm-rdtlh.qty.

      IF rm-rcpth.pur-uom NE b-po-ordl.pr-qty-uom THEN DO:
        FIND FIRST item
            WHERE item.company EQ b-po-ordl.company
              AND item.i-no    EQ b-po-ordl.i-no
            NO-LOCK NO-ERROR.

        RUN custom/convquom.p(cocode, rm-rcpth.pur-uom, b-po-ordl.pr-qty-uom,
                              (IF AVAIL item THEN item.basis-w ELSE 0),
                              b-po-ordl.s-len, b-po-ordl.s-wid,
                              (IF AVAIL item THEN item.s-dep ELSE 0),
                              ld[1], OUTPUT ld[1]).
      END.

      ld[2] = ld[2] + ld[1].
    END.
  END.

  IF b-po-ordl.pr-qty-uom EQ "EA" THEN DO:
    {sys/inc/roundup.i ld[2]}
  END.

  op-qty = ld[2].

END PROCEDURE.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
