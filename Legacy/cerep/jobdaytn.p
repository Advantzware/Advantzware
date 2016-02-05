/* cerep/jobdaytn.p 
    Almost same as jobintpk.p but more dept notes, not special mat info */
/*  factory ticket  for folding , Dayton                                   */
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
DEF VAR ld AS DEC NO-UNDO.
DEF VAR v-net-sheet-size AS cha NO-UNDO.
DEF VAR v-die-size AS cha NO-UNDO.
DEF VAR li AS INT NO-UNDO.

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

v-fill = /*fill("=",128)*/ "<||3><C1><FROM><C108><LINE><||3>".

def new shared frame head.

DEF SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF VAR v-po-duedate LIKE po-ordl.due-date NO-UNDO.
DEF VAR v-upc-lbl AS cha FORM "x(10)" NO-UNDO.
DEF VAR v-shipto1 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shipto2 AS cha FORM "x(30)" EXTENT 4 NO-UNDO.
DEF SHARED VAR s-run-speed AS LOG NO-UNDO.

format header
       "JOB NUMBER:<B>" v-job-no space(0) "-" space(0) v-job-no2 format "99" "</B>"
       "<B><P12>F A C T O R Y   T I C K E T</B><P10>" at 52  "JOB START DATE:" at 123 v-start-date skip
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

      if first-of(job-hdr.frm) then v-first = yes.

      /** PRINT JOB HEADER **/
      if v-first then do:
        assign
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2.


        if avail oe-ord then
          if not oe-ctrl.p-fact and oe-ord.stat eq "H" then next.

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

        v-req-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?.
        FIND FIRST cust WHERE cust.company = job-hdr.company AND
                              cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.
        v-cust-name = IF AVAIL oe-ord THEN oe-ord.cust-name 
                      ELSE IF AVAIL cust THEN cust.name
                      ELSE job-hdr.cust-no.

        PUT "<B>Customer Name:</B>" v-cust-name 
            "<B>REQ DATE:   DUE DATE:   Estimate:" SKIP
            "Shipto:</B>" v-shipto[1] v-req-date AT 49 v-due-date AT 61 job-hdr.est-no FORMAT "x(8)" AT 74
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
              AND job-mch.frm = job-hdr.frm
            no-lock,

            first mach
            {sys/ref/mach.w}
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
              {sys/look/itemiv.w}
                and item.i-no eq job-mat.i-no
              no-lock:

            do i = 1 to 12:
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
            v-upc-lbl = IF s-prt-sellprc THEN "Sell Price" ELSE "   UPC#".
            IF FIRST-OF(eb.form-no) THEN
              PUT /*"<R-1>" "Case" AT 97 SKIP */
                "<P9><B>F/B   FG Item #           Job Qty  PO#            Description           Style  Carton Size            Count Case Size       #UP     " v-upc-lbl "</B>" SKIP
              /*put " Item # / Qty         Description" "Size / Style" at 50
                  "Est #" at 72 " Case Cnt Code" "Layout # On Per Form" at 106
                  skip "--------------   -------------------------"
                  "-------------------------" at 44 "-----" at 72
                  " -------- ---------------"
                  "------------------------------" at 103.*/
                 .
            /*else
              put fill("-",132) format "x(132)". */
            v-job-qty = 0.
            for each xjob-hdr where xjob-hdr.company eq cocode
               and xjob-hdr.job     eq job-hdr.job
               and xjob-hdr.job-no  eq job-hdr.job-no
               and xjob-hdr.job-no2 eq job-hdr.job-no2
               and xjob-hdr.i-no    eq eb.stock-no no-lock:
               v-job-qty = v-job-qty + xjob-hdr.qty.
            end.
            /** PRINT ITEM **/
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq eb.stock-no /*job-hdr.i-no*/
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
            display trim(string(eb.form-no,">>9")) + "-" +
                    trim(string(eb.blank-no,">>9")) FORM "x(5)" 
                    eb.stock-no @ job-hdr.i-no 
                    v-job-qty /** v-fac*/ format "->>,>>>,>>9"
                    oe-ordl.po-no when avail oe-ordl 
                    v-dsc[1] FORM "x(21)"
                    eb.style /*v-stypart */
                    v-size[1] FORM "x(22)"
                    oe-ordl.cas-cnt when avail oe-ordl
                    eb.cas-cnt when (not avail oe-ordl) or oe-ordl.cas-cnt eq 0 @ oe-ordl.cas-cnt
                    /*PACE(4)*/
                    v-case-size  FORM "x(15)"
                    v-up   
                    eb.upc-no 
                    string(oe-ordl.price,">>>,>>9.99<<<<") WHEN AVAIL oe-ordl AND s-prt-sellprc @ eb.upc-no
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

             /* Number of sheets */
             run oe/rep/ticket1.p (recid(ef), recid(job-hdr)).
             find first wrk-sheet where recid(wrk-sheet) eq save_id.
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

             PUT "<P10>" v-fill SKIP                       /*REQ'D*/                 
                 "<B>BOARD CODE            DUE DATE    SHEETS SHEET SIZE   NET SHEET  BOARD PO# VENDOR#  Die Size     Die#            CAD#</B>"
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
        /* task# 09190503
               IF ef.xgrain EQ "S" THEN
                 ASSIGN
                  ld               = wrk-sheet.sh-wid
                  wrk-sheet.sh-wid = wrk-sheet.sh-len
                  wrk-sheet.sh-len = ld.
        */     /*task 09190503*/
               ASSIGN v-net-sheet-size = IF ef.xgrain EQ "S" THEN string(ef.nsh-len) + "x" + string(ef.nsh-wid)
                                         ELSE string(ef.nsh-wid) + "x" + string(ef.nsh-len)
                      v-die-size = IF ef.xgrain EQ "S" THEN string(ef.trim-l) + "x" + string(ef.trim-w)
                                   ELSE string(ef.trim-w) + "x" + string(ef.trim-l).
               display wrk-sheet.brd-dscr
                    v-po-duedate wrk-sheet.gsh-qty 
                    string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len)
                    format "x(13)"
                    v-net-sheet-size FORM "x(13)"
                    v-board-po  
                    v-vend  
                    v-die-size FORM "x(12)"
                    eb.die-no 
                    eb.cad-no 
                    /*"Caliper:" AT 68 wrk-sheet.cal space(5) "Board:"
                    wrk-sheet.brd-dscr "Form:" AT 123 wrk-sheet.form-no*/
                    with stream-io width 170 no-labels no-box frame sheet.
               x = 1.
             end. /* each wrk-sheet */
             if x ne 2 then put v-fill at 1 skip.
                     
             /** PRINT INK **/
             PUT "<B>INK PASS           INK NAME            ITEMS        INK PASS           INK NAME            ITEMS         PLATE #</B>"
                 SKIP.
             x = 2.
             i = 1.
             v-ink1 = "".
             v-ink2 = "".
             for each wrk-ink WHERE wrk-ink.form-no = eb.form-no
                break by wrk-ink.i-pass
                      BY wrk-ink.i-code
                      BY wrk-ink.blank-no
                       :
                IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.
                IF FIRST-OF(wrk-ink.i-code) THEN v-item[i] = "".

                v-item[i] = IF LOOKUP(string(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ",".
                /*IF LAST-OF(wrk-ink.i-pass) THEN DO:
                   IF wrk-ink.i-pass = 1 THEN
                      v-ink1[i] = wrk-ink.i-code + " " + wrk-ink.i-dscr + " " + v-item[i].
                   ELSE IF wrk-ink.i-pass = 2 THEN
                        v-ink2[i] = wrk-ink.i-code + " " + wrk-ink.i-dscr + " " + v-item[i].
                   i = i + 1.
                END. */

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
                       ASSIGN v-ink1[i] = "1  " + string(wrk-ink.i-code,"X(15)") + " " + 
                                          string(wrk-ink.i-dscr,"x(20)") + " " + trim(v-alloc) /*v-item[i]*/
                                          /*+ (IF i = 1 THEN "  " + eb.plate-no ELSE "") */
                              i = i + 1         . 
                    ELSE IF wrk-ink.i-pass = 2 THEN
                       ASSIGN v-ink2[i] = "2  " + string(wrk-ink.i-code,"X(15)") + " " + 
                                   string(wrk-ink.i-dscr,"x(20)") + " " + trim(v-alloc) /*v-item[i]*/
                              i = i + 1.           
                END.
                   

              
                /*delete wrk-ink.*/
             end. /* each wrk-ink */
             v-skip = NO.
             v-plate-printed = NO.
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
             
          END. /* last-of(eb.form-no) */
          
        end. /* each eb */
      end. /* each ef */
      end. /* first job-no */

      if last-of(job-hdr.frm) then do:
         IF s-run-speed THEN
            PUT "<B>MACHINE                 MR WASTE   MR HRS  RUN SPEED   SPOIL%    MATERIAL FOR MACHINE             SIZE  TOTAL REQUIRED  PALLET</B>"
                SKIP.
         else
            PUT "<B>MACHINE                 MR WASTE   MR HRS   RUN HOUR    SPOIL%    MATERIAL FOR MACHINE             SIZE  TOTAL REQUIRED  PALLET</B>"
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
              IF s-prt-mstandard THEN DO:
                IF s-run-speed THEN
                   PUT wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                       wrk-op.mr[job-hdr.frm]         SPACE(5)
                       wrk-op.speed[job-hdr.frm]      SPACE(5)
                       wrk-op.spoil[job-hdr.frm]      SPACE(5)
                       v-mat-for-mach FORM "x(60)"
                       SKIP.
               ELSE
                   PUT wrk-op.m-dscr   SPACE(5)
                       wrk-op.mr-waste[job-hdr.frm]   SPACE(5)
                       wrk-op.mr[job-hdr.frm]         SPACE(5)
                       wrk-op.run-hr[job-hdr.frm]     SPACE(5)
                       wrk-op.spoil[job-hdr.frm]      SPACE(5)
                       v-mat-for-mach FORM "x(60)"
                       SKIP.
             END.
             ELSE PUT wrk-op.m-dscr   SPACE(5)
                     /* wrk-op.mr-waste[job-hdr.frm]   */ SPACE(10)
                      /*wrk-op.mr[job-hdr.frm] >>9.99 */   SPACE(11)
                      /*wrk-op.speed[job-hdr.frm] >>>>9*/      SPACE(10)
                      /*wrk-op.spoil[job-hdr.frm]   >>9.99*/   SPACE(12)
                      v-mat-for-mach FORM "x(60)"
                      SKIP.
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
        
    /*  IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */ */
        PUT "<B>DEPARTMENT   INSTRUCTION NOTES</B>" SKIP
            v-dept-inst[1] FORM "x(128)" SKIP
            v-dept-inst[2] FORM "x(128)" SKIP
            v-dept-inst[3] FORM "x(128)" SKIP
            v-dept-inst[4] FORM "x(128)" SKIP
            v-dept-inst[5] FORM "x(128)" SKIP
            v-dept-inst[6] FORM "x(128)" SKIP
            v-dept-inst[7] FORM "x(128)" SKIP
            v-dept-inst[8] FORM "x(128)" SKIP
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
/* maybe later
        /* spec note */        
        FIND FIRST itemfg WHERE itemfg.company = job-hdr.company
                            AND itemfg.i-no = job-hdr.i-no NO-LOCK NO-ERROR.
        {custom/notesprt.i itemfg v-inst2 10 }
        DO i = 1 TO 10:
             v-spec-inst[i] = v-inst2[i].
        END.

        
        PUT "Spec Notes: " v-spec-inst[1] SKIP
            "            " v-spec-inst[2] SKIP
            "            " v-spec-inst[3] SKIP
            "            " v-spec-inst[4] SKIP
            "            " v-spec-inst[5] SKIP
            "            " v-spec-inst[6] SKIP
            "            " v-spec-inst[7] SKIP
            "            " v-spec-inst[8] SKIP
            "            " v-spec-inst[9] SKIP
            "            " v-spec-inst[10] SKIP
            .

*/
  /*      
        /** PRINT MISC/SUBCONTRACT **/
        x = 2.
        for each wrk-misc break by wrk-misc.form-no:
          x = (if x eq 1 then 0 else 1).
          if first(wrk-misc.form-no) then
          put v-fill format "x(50)" at 1 "  Miscellaneous - Subcontract  "
              v-fill format "x(50)" skip.
          put "Form:" wrk-misc.form-no space(2) wrk-misc.cost.
          if x eq 0 then put skip.
          else put space(16).
        end. /* each wrk-misc */
   */

/*=== not for dayton 
        FIND first ef WHERE ef.company EQ job-hdr.company
                        AND ef.est-no  EQ job-hdr.est-no
                        AND ef.form-no = job-hdr.frm NO-LOCK NO-ERROR.
        IF AVAIL ef THEN DO:
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
            IF v-misc[i] <> "" THEN PUT v-misc[i] AT 50.
            IF v-prep[i] <> "" THEN PUT v-prep[i] AT 90.
            IF v-spec-no[i] <> "" OR v-misc[i] <> "" OR v-prep[i] <> "" THEN PUT SKIP.
        END.
        DO i = 7 TO 8:
            IF v-spec-no[i] <> "" THEN PUT v-spec-no[i].
            IF v-prep[i] <> "" THEN PUT v-prep[i] AT 90.
            IF v-spec-no[i] <> "" OR v-prep[i] <> "" THEN PUT SKIP.
        END.
====================*/

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
                             AND b-eb.form-no = xjob-hdr.frm
                             AND b-eb.blank-no = xjob-hdr.blank-no
                             AND b-eb.stock-no = xjob-hdr.i-no
                             NO-LOCK NO-ERROR.
           CREATE tt-fgitm.
           ASSIGN tt-fgitm.i-no = xjob-hdr.i-no
                  tt-fgitm.qty = xjob-hdr.qty
                  tt-fgitm.i-dscr = IF AVAIL b-eb THEN b-eb.part-dscr1 ELSE xjob-hdr.i-no
                  tt-fgitm.po-no = IF AVAIL xoe-ordl THEN xoe-ordl.po-no ELSE ""
                  tt-fgitm.seq = i.

           i = i + 1.          
           /* IF i > 10 THEN LEAVE.*/
        END.
        /*
        PUT v-fill skip
               "<B><U>LABEL ITEM 1</U>"
               "<U>LABEL ITEM 2</U>" AT 55
               "<U>LABEL ITEM 3</U></B>" AT 107
               SKIP
               "Job#:" v-job-no + "-" + string(v-job-no2)
               "Job#:" AT 45 v-job-no + "-" + string(v-job-no2) 
               "Job#:" AT 90  v-job-no + "-" + string(v-job-no2) 
               SKIP
               "Customer:" oe-ord.cust-name 
               "Customer:" AT 45 oe-ord.cust-name 
               "Customer:" AT 90  oe-ord.cust-name 
               SKIP
               "Purchase Order#:" v-po-no
               "Purchase Order#:" AT 45 v-po-no 
               "Purchase Order#:" AT 90  v-po-no 
               SKIP
               "<B>FG Item #:</B>" v-fgitm[1]
               "<B>FG Item #:</B>" AT 52 v-fgitm[2]
               "<B>FG Item #:</B>" AT 104 v-fgitm[3]
               SKIP
               "Description:" v-fgdsc[1]
               "Description:" AT 45 v-fgdsc[2] 
               "Description:" AT 90  v-fgdsc[3] 
               SKIP
               "Quantity:" v-fgqty[1]
               "Quantity:" AT 45 v-fgqty[2] 
               "Quantity:" AT 90  v-fgqty[3] 
               .
          */
        
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

          IF PAGE-SIZE - LINE-COUNTER < 7 THEN PAGE.

          i = i + 1.
          ASSIGN v-fgitm[i] = tt-fgitm.i-no
                 v-fgdsc[i] = tt-fgitm.i-dscr
                 v-fgqty[i] = tt-fgitm.qty
                 v-pono[i] = tt-fgitm.po-no.
          j = j + 1.
          IF i >= 3 THEN DO:
             display v-fill skip
                "<B><U>LABEL ITEM" + trim(string(j - 2)) + "</U>"  FORM "x(22)"
               "<U>LABEL ITEM" + trim(string(j - 1)) + "</U>" FORM "x(20)" WHEN v-fgitm[2] <> "" AT 55
               "<U>LABEL ITEM" + TRIM(STRING(j)) + "</U></B>" FORM "x(23)" WHEN v-fgitm[3] <> "" AT 107
               SKIP
               "Job#:" v-job-no + "-" + string(v-job-no2)
               "Job#:"  WHEN v-fgitm[2] <> ""  AT 45
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
               "Quantity:" v-fgqty[1]
               "Quantity:"  WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  WHEN v-fgitm[2] <> "" 
               "Quantity:"  WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] WHEN v-fgitm[3] <> ""                               
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
            ASSIGN v-fgitm[1] = ""
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
                   v-pono[3] = "".
           v-last-j = j.
          END.
        END.
        IF i > 0 THEN DO:
            IF PAGE-SIZE - LINE-COUNTER < 7 THEN PAGE.
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
               "Quantity:" v-fgqty[1]
               "Quantity:"  WHEN v-fgitm[2] <> "" AT 45 v-fgqty[2]  WHEN v-fgitm[2] <> "" 
               "Quantity:"  WHEN v-fgitm[3] <> "" AT 90  v-fgqty[3] WHEN v-fgitm[3] <> ""                               
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
    
    if v-format eq "Fibre" then page.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
