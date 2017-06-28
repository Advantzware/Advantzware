/* ------------------------------------------------- jc/rep/ticket.p 10/94 gb */
/*  factory ticket                                                            */
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
def new shared var v-fill as char format "x(132)".
def new shared var v-frst as log.
def new shared var v-ok as log.
def new shared var v-skip as log.
def new shared var v-est-qty as int format "->>,>>>,>>9".
def new shared var v-job-qty as int format "->>,>>>,>>9".
def new shared var v-fac as dec.
def new shared var v-job-no like oe-ordl.job-no.
def new shared var v-job-no2 like oe-ordl.job-no2.
def new shared var v-due-date like oe-ord.due-date.
def new shared var v-reprint as log.
def new shared var v-up like eb.num-up.
def new shared var v-tandem as log.
def new shared var v-form-no like eb.form-no.
def new shared var v-fup as char.
def new shared var v-layout as char format "x(27)".
def new shared var v-out1-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */
def new shared var v-out2-id       as   recid    no-undo.  /* YSK 06/08/01  was~ local var */

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
DEF VAR v-prev-ext-gap AS INT NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no NO-UNDO.

def workfile w-lo
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
  field num-sh like est-op.num-sh extent 100.

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
  
DEF TEMP-TABLE tt-fgitem FIELD i-no AS cha.
DEF TEMP-TABLE tt-eb FIELD eb-recid AS RECID.

form header
     skip(1)
     "07/22/02 Job Ticket QF-130"   to 132
    with no-box no-attr-space frame bott page-bottom stream-io width 132.

{custom/formtext.i NEW}
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-is-set AS LOG NO-UNDO.

DEF VAR ld-yld AS DEC NO-UNDO.
DEF VAR ld-sqin AS DEC NO-UNDO.
DEF VAR ld-msf AS DEC NO-UNDO.
DEFINE VARIABLE ls-fgitem-img AS CHARACTER FORM "x(150)" NO-UNDO.
DEFINE VARIABLE v-lines AS INTEGER NO-UNDO .
DEFINE  SHARED VARIABLE s-prt-fgimage AS LOGICAL NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
{cec/msfcalc.i}
DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-jobhdr FOR job-hdr.
v-fill = fill("=",132).

def new shared frame head.

format header
       "F A C T O R Y   T I C K E T" at 52  "DATE:" at 117 v-today skip
       "===========================" at 52 skip(1)
      "[  ] JACKET" at 5 space(10) "[  ] PRODUCTION" space(10) "[  ] PLATE ROOM"
       space(10) "[  ] DIE ROOM" space(10) "[  ] PURCHASING" skip
       "JOB NUMBER:" v-job-no space(0) "-" space(0)
       v-job-no2 format "99"
       space(10) "COMPLETION DATE:" space(10)
       "BOARD P.O. #:" space(10)  "VENDOR #:"
       "DUE DATE:" at 113 v-due-date skip
       v-fill
    with no-box frame head no-labels stream-io width 132.

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
          and (job-hdr.ftick-prnt            eq v-reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and can-find(first job where job.company eq cocode
                                   and job.job     eq job-hdr.job
                                   and job.job-no  eq job-hdr.job-no
                                   and job.job-no2 eq job-hdr.job-no2
                                   and job.stat    ne "H")
        use-index job-no,

        first est
        where est.company  eq job-hdr.company
          AND est.est-no   EQ job-hdr.est-no
          and est.est-type le 4  
        no-lock

        break by job-hdr.job
              by job-hdr.job-no
              by job-hdr.job-no2:

      IF NOT job-hdr.ftick-prnt THEN DO WHILE TRUE:
        li = li + 1.
        FIND xjob-hdr EXCLUSIVE-LOCK
            WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
            NO-ERROR NO-WAIT.
        IF AVAIL xjob-hdr THEN xjob-hdr.ftick-prnt = YES.
        IF li GE 1000 OR xjob-hdr.ftick-prnt THEN LEAVE.
      END.
      
      v-est-qty = if avail est then est.est-qty[1] else 0.

      FIND FIRST tt-fgitem WHERE tt-fgitem.i-no EQ job-hdr.i-no NO-ERROR.
      IF NOT AVAIL tt-fgitem THEN DO:
         CREATE tt-fgitem.
         tt-fgitem.i-no = job-hdr.i-no.
      END.
      if first-of(job-hdr.job-no2) then v-first = yes.
      
      /** PRINT JOB HEADER **/
      if v-first then do:
        assign
         v-job-no  = job-hdr.job-no
         v-job-no2 = job-hdr.job-no2.

        find first oe-ord
            where oe-ord.company eq job-hdr.company
              and oe-ord.ord-no  eq job-hdr.ord-no
            no-lock no-error.

        if avail oe-ord then
          if not oe-ctrl.p-fact and oe-ord.stat eq "H" then next.

        v-due-date = if avail oe-ord then oe-ord.due-date else ?.

        if not first(job-hdr.job-no) then page.
        view frame head.
        v-printline = 5 .
        if v-format eq "Fibre" then view frame bott.

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
           wrk-op.num-sh[job-mch.frm] = job-mch.run-qty.
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
      end. /* first job-no */

      for each ef
          WHERE ef.company EQ job-hdr.company
            AND ef.est-no  EQ job-hdr.est-no
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

        FOR EACH tt-eb:
            DELETE tt-eb.
        END.
        if ef.form-no eq job-hdr.frm then  
        DO:
        /* temp table to print all for set too*/
        lv-is-set = NO.
        IF est.est-type = 2 AND
           CAN-FIND(FIRST eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no
                              AND eb.form-no = 0) 
        THEN DO:
           lv-is-set = YES.
           for each eb WHERE eb.company     EQ ef.company
               AND eb.est-no      eq ef.est-no
              AND eb.form-no > 0 NO-LOCK
             /*and ((eb.stock-no  eq job-hdr.i-no and
                    eb.stock-no  ne "") or
                   (eb.blank-no  eq job-hdr.blank-no or
                    (eb.blank-no eq 1 and
                     (est.est-type eq 2 or est.est-type eq 6)) and
                    eb.stock-no  eq "")) */
              break by eb.form-no:
                 CREATE tt-eb.
                 ASSIGN tt-eb.eb-recid = RECID(eb).
           END.
        END.
        ELSE do:
            for each eb WHERE eb.company     EQ ef.company
                 AND eb.est-no      eq ef.est-no
                 and eb.form-no     eq ef.form-no
                 and ((eb.stock-no  eq job-hdr.i-no and
                    eb.stock-no  ne "") or
                   (eb.blank-no  eq job-hdr.blank-no or
                    (eb.blank-no eq 1 and
                     (est.est-type eq 2 or est.est-type eq 6)) and
                    eb.stock-no  eq ""))
                  break by eb.form-no.

                 CREATE tt-eb.
                 ASSIGN tt-eb.eb-recid = RECID(eb).

             END.
        END.
        /*==========*/
        ebloop:
        for EACH tt-eb,
            each eb WHERE RECID(eb) = tt-eb.eb-recid
            /*WHERE eb.company     EQ ef.company
              AND eb.est-no      eq ef.est-no
              and eb.form-no     eq ef.form-no
              and ((eb.stock-no  eq job-hdr.i-no and
                    eb.stock-no  ne "") or
                   (eb.blank-no  eq job-hdr.blank-no or
                    (eb.blank-no eq 1 and
                     (est.est-type eq 2 or est.est-type eq 6)) and
                    eb.stock-no  eq "")) */
            break by eb.form-no BY eb.blank-no.

          if est.est-type eq 4 and eb.stock-no ne job-hdr.i-no then next ebloop.

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
              no-lock,
              first item
              {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no
              no-lock:

            do i = 1 to 12:
              if eb.i-code2[i] eq job-mat.i-no then do:

                find first wrk-ink
                    where wrk-ink.i-code   eq eb.i-code2[i]
                      and wrk-ink.form-no  eq eb.form-no
                      and wrk-ink.blank-no eq eb.blank-no
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
            end.

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
          
          /*if last-of(eb.form-no) then do:ysk*/
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
            
              /* set header print*/            
            IF lv-is-set AND v-first THEN DO:
               ld-msf = 0.
                /* FOR EACH bf-eb WHERE bf-eb.company EQ eb.company
                                   AND bf-eb.est-no  EQ eb.est-no
                                   AND bf-eb.form-no NE 0
                                   /*AND ROWID(bf-eb)  NE ROWID(eb)*/ NO-LOCK
                                   BREAK BY bf-eb.est-no:
                       ASSIGN ld-yld   = IF bf-eb.yld-qty LT 0 THEN -1 / bf-eb.yld-qty ELSE bf-eb.yld-qty
                              ld-sqin  = est.est-qty[1] * ld-yld * bf-eb.t-sqin
                              ld-msf   = ld-msf + ((IF v-corr THEN (ld-sqin * .007) ELSE (ld-sqin / 144)) / 1000).
                 END.
                 */
                 FIND FIRST bf-eb WHERE bf-eb.company = ef.company
                                    AND bf-eb.est-no = ef.est-no
                                    AND bf-eb.form-no = 0 NO-LOCK NO-ERROR.
                 IF AVAIL bf-eb THEN do:
                     PUT "SET Header:" AT 5 
                         "  Item#:" bf-eb.stock-no
                         "  Customer Part#:" bf-eb.part-no
                         "  Description:" bf-eb.part-dscr1
                         bf-eb.part-dscr2 SKIP
                         "F.G. Length:" AT 20 bf-eb.len 
                         "    Width:" bf-eb.wid
                         "    Depth:" bf-eb.dep
                         "    Qty:" eb.eqty /*  "   MSF:" ld-msf FORMAT "->>,>>9.999" */ SKIP
                         FILL("-",115) AT 5 FORM "x(115)" SKIP.
                     v-printline = v-printline + 3.
                 END.
            END.

            if v-first THEN do:
              put " Item # / Qty         Description" "Size / Style" at 50
                  "   Est #" at 72 " Case Cnt Code" "Layout # On Per Form" at 106
                  skip "--------------   -------------------------"
                  "-------------------------" at 44 "--------" at 72
                  " -------- ---------------"
                  "---------------------------" at 106 SKIP.
              v-printline = v-printline + 2.
            END.
            ELSE do:
              put fill("-",132) format "x(132)"  SKIP.
              v-printline = v-printline + 1.
            END.

            IF lv-is-set THEN v-first = NO.

            /** PRINT ITEM **/
            find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq job-hdr.i-no
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
            
            display job-hdr.i-no
                    eb.stock-no WHEN lv-is-set @ job-hdr.i-no
                    v-dsc[1] at 17 format "x(26)" space(1)
                    v-size[1] space(2) job-hdr.est-no FORMAT "x(8)" space(4)
                    oe-ordl.cas-cnt when avail oe-ordl
                    eb.cas-cnt when (not avail oe-ordl) or oe-ordl.cas-cnt eq 0
                      @ oe-ordl.cas-cnt
                    eb.cas-no space(6)
                    w-lo.layout when avail w-lo @ v-layout
                    skip
                with stream-io width 135 no-labels no-box frame line-det1.
                v-printline = v-printline + 1.
            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".

            find next w-lo no-error.

            display "Job:" v-job-qty * v-fac format "->>,>>>,>>9"
                    v-dsc[2] at 20 format "x(26)"
                    space(1) v-stypart space(6)
                    v-cas-dscr format "x(24)"
                    w-lo.layout when avail w-lo @ v-layout
                    skip
                with stream-io width 135 no-labels no-box frame line-det2.
                v-printline = v-printline + 1.
            find next w-lo no-error.

            do while avail w-lo:
              put w-lo.layout at 103 skip.
              v-printline = v-printline + 1.
              find next w-lo no-error.
            end.

            for each w-lo: delete w-lo. end.

           FIND FIRST oe-rel WHERE oe-rel.company EQ oe-ordl.company AND 
               oe-rel.ord-no  EQ oe-ordl.ord-no AND 
               oe-rel.i-no    EQ oe-ordl.i-no   AND 
               oe-rel.line    EQ oe-ordl.line NO-LOCK NO-ERROR.
           IF AVAIL oe-rel THEN ASSIGN
                v-po-no = oe-rel.po-no . 

            IF v-po-no = "" THEN
               v-po-no = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".

            display "Ord:" v-est-qty @ v-fac format "->>,>>>,>>9" space(3)
                    "Req Date:" oe-ordl.req-date when avail oe-ordl
                    space(3) "Prm Date:"
                    oe-ordl.prom-date when avail oe-ordl space(3)
                    "P.O.:" /*oe-ordl.po-no*/ v-po-no /*when avail oe-rel*/ skip
                    "Customer Name: " oe-ord.cust-name when avail oe-ord
                    "Sold To: " oe-ord.sold-id when avail oe-ord
                    "Order#: " oe-ord.ord-no when avail oe-ord
                    space(3)
                    "Cust Part#: " oe-ordl.part-no when avail oe-ord
                    eb.part-no when not avail oe-ordl @ oe-ordl.part-no
                    skip
                    "Plate#: "  eb.plate-no
                    "Die#: "    eb.die-no
                    "CAD#: "    eb.cad-no
                    "UPC#: "    eb.upc-no
                    "SPC#: "    eb.spc-no
                    skip
                    v-ovund
                with stream-io width 135 no-labels no-box frame line-det3.
                v-printline = v-printline + 4.
          /*end. /* last-of(eb.form-no) */ ysk*/
        end. /* each eb , ebloop*/
        END. /* do: */

        /* Number of sheets */
        run oe/rep/ticket1.p (recid(ef), recid(job-hdr)).
        find first wrk-sheet where recid(wrk-sheet) eq save_id.

        if last(ef.form-no) and last-of(job-hdr.job-no2) then do:
          put v-fill at 1 skip.
          v-printline = v-printline + 1.
          /** PRINT DIE **/
          x = 2.
          for each wrk-die:
            x = if x eq 1 then 0 else 1.
            put "Die Number:" wrk-die.die-no
                "  Size:"  wrk-die.die-size
                space(17).
            if x eq 0 THEN do:
                 put skip.
                 v-printline = v-printline + 1.
            END.
          end. /* each wrk-die */
          if x ne 2 THEN do:
               put v-fill at 1 skip.
               v-printline = v-printline + 1.
          END.

          /** PRINT INK **/
          x = 2.
          for each wrk-ink
              break by wrk-ink.form-no
                    by wrk-ink.blank-no
                    by wrk-ink.i-code:

            v-frm-blk = string(wrk-ink.form-no,">>9") + "-" +
                        string(wrk-ink.blank-no,"99").

            do while substr(v-frm-blk,1,1) eq " ":
              v-frm-blk = substr(v-frm-blk,2,5).
            end.

            if first-of(wrk-ink.blank-no) then do:
              put v-frm-blk
                  space(3)
                  "Ink#:"
                  space(1).
              x = 0.
            end.
            else put space(15).

            x = if x eq 1 then 0 else 1.
            
            put "P-" + string(wrk-ink.i-pass,"99") format "x(4)"
                space(2)
                wrk-ink.i-code
                space(2)
                wrk-ink.i-dscr
                space(2).

            if wrk-ink.i-qty ne 0 then
              put wrk-ink.i-qty.
            else
              put space(7).

            if x eq 0 or last-of(wrk-ink.blank-no) THEN do:
              put skip.
              v-printline = v-printline + 1.
            END.
            else
              put space(6).

            if last-of(wrk-ink.blank-no) THEN do:
                put skip(1).
                v-printline = v-printline + 2.
            END.

            delete wrk-ink.
          end. /* each wrk-ink */

          if x ne 2 THEN do:
               put v-fill at 1 skip.
               v-printline = v-printline + 1.
          END.

          /** PRINT LEAF/FILM **/
          x = 2.
          for each wrk-film:
            put "Form: " wrk-film.snum space(0) "/" space(0)
                wrk-film.bnum space(3)
                " Leaf/Window Film: " wrk-film.leaf  space(2)
                "Length: " at 68 wrk-film.leaf-l " stream-io width: "
                wrk-film.leaf-w skip.
            v-printline = v-printline + 1.
            x = 1.
          end. /* each wrk-film */
          if x ne 2 THEN do:
              put v-fill at 1 skip.
              v-printline = v-printline + 1.
          END.

          IF v-printline >= 40 THEN DO:
                v-printline = 0.
                PAGE {1}.
            END.

          /** PRINT PREP MATERIAL **/
          x = 2.
          for each wrk-prep where wrk-prep.ml:
            x = (if x eq 1 then 0 else 1).
            put "Form: "
                wrk-prep.s-num space(0) "/" space(0)
                wrk-prep.b-num
                space(2) "Prep Material: " wrk-prep.code space(1)
                wrk-prep.dscr.
            if x eq 0 THEN do:
                 put skip.
                 v-printline = v-printline + 1.
            END.
            else put space(13).
          end. /* each wrk-prep */
          if x ne 2 THEN do:
              put v-fill at 1 skip.
              v-printline = v-printline + 1.
          END.

          /** PRINT SPECIAL MATERIAL **/
          x = 2.
          for each wrk-spec:
            put "Form:" wrk-spec.form-no
                space(2) "Special Material: " wrk-spec.spec-no space(2)
                wrk-spec.dscr "Qty:" at 68 wrk-spec.qty wrk-spec.uom.
            x = 1.
          end. /* each wrk-spec */
          if x ne 2 THEN do:
               put v-fill at 1 skip.
               v-printline = v-printline + 1.
          END.

          IF v-printline >= 40 THEN DO:
                v-printline = 0.
                PAGE {1}.
            END.

          /** PRINT SHEET **/
          x = 2.
          for each wrk-sheet break by wrk-sheet.form-no:
            display "Sheets Required:" wrk-sheet.gsh-qty FORMAT ">>>,>>>,>>9" space(3) "Size:"
                    string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len)
                    format "x(25)"
                    "Caliper:" AT 68 wrk-sheet.cal space(5) "Board:"
                    wrk-sheet.brd-dscr "Form:" AT 123 wrk-sheet.form-no
                with stream-io width 132 no-labels no-box frame sheet.
            x = 1.
            v-printline = v-printline + 1.
          end. /* each wrk-sheet */
          if x ne 2 THEN do: 
              PUT v-fill at 1 skip.
              v-printline = v-printline + 1.
          END.

          /** PRINT PREP LABOR **/
          x = 2.
          for each wrk-prep where not wrk-prep.ml:
            x = if x eq 1 then 0 else 1.
            put "Form: " wrk-prep.s-num space(0) "/" space(0)
                wrk-prep.b-num
                space(2) "Prep Labor: " wrk-prep.code space(1)
                wrk-prep.dscr.
            if x eq 0 THEN do: 
                put skip.
                v-printline = v-printline + 1.
            END.
            else put space(13).
          end. /* each wrk-prep */
          if x ne 2 THEN do:
               put v-fill at 1 skip.
               v-printline = v-printline + 1.
          END.
        end. /* last(ef.form-no) */
      end. /* each ef */

      if last-of(job-hdr.job-no2) then do:
        /** PRINT JOB INSTRUCTIONS **/

        /* dept notes*/
        FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.

        lv-text = "".

        FOR EACH notes
            WHERE notes.rec_key   EQ job.rec_key
              AND notes.note_code NE ""
              AND LOOKUP(notes.note_code,v-exc-depts) EQ 0
            NO-LOCK:
          /*IF lv-text = "" THEN lv-text = notes.note_title + CHR(10).*/
          lv-text = lv-text + " " + notes.note_title + CHR(10) + 
                    TRIM(notes.note_text) + CHR(10).
        END.

        IF lv-text NE "" THEN DO:
          FOR EACH tt-formtext:
            DELETE tt-formtext.
          END.

          DO li = 1 TO 20:
            CREATE tt-formtext.
            ASSIGN
             tt-line-no = li
             tt-length  = 80.
          END.

          RUN custom/formtext.p (lv-text).

          FOR EACH tt-formtext WHERE tt-text NE "" BREAK BY tt-line-no:
            IF FIRST(tt-line) THEN PUT "Instructions:".
            PUT tt-formtext.tt-text FORMAT "x(80)" AT 15 SKIP.
            v-printline = v-printline + 1.
            IF LAST(tt-line) THEN do:
                PUT v-fill SKIP.
                v-printline = v-printline + 1.
            END.
          END.
        END.

        /* spec notes */ 
        lv-text = "".

        FOR EACH tt-fgitem
            WHERE v-format       EQ "Fibre"
               OR tt-fgitem.i-no EQ job-hdr.i-no,
            FIRST itemfg
            WHERE itemfg.company EQ job-hdr.company
              AND itemfg.i-no    EQ tt-fgitem.i-no
            NO-LOCK,
            EACH notes
            WHERE notes.rec_key   EQ itemfg.rec_key
              AND notes.note_type EQ "S"
              AND LOOKUP(notes.note_code,v-spec-list) GT 0
            NO-LOCK:
          lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
        END.

        IF lv-text NE "" THEN DO:
          FOR EACH tt-formtext:
            DELETE tt-formtext.
          END.

          DO li = 1 TO 10:
            CREATE tt-formtext.
            ASSIGN
             tt-line-no = li
             tt-length  = 80.
          END.

          RUN custom/formtext.p (lv-text).

          FOR EACH tt-formtext WHERE tt-text NE "" BREAK BY tt-line-no:
            IF FIRST(tt-line) THEN PUT "Spec Notes:".
            PUT tt-formtext.tt-text FORMAT "x(80)" AT 13 SKIP.
            v-printline = v-printline + 1.
            IF LAST(tt-line) THEN do:
                PUT v-fill SKIP.
                v-printline = v-printline + 1.
            END.
          END.
        END.

        do i = 1 to 10:
          assign
           v-frst = no
           v-ok   = no
           v-skip = no
           j      = ((i - 1) * 10) + 1.

          for each wrk-op break by wrk-op.s-num by wrk-op.d-seq by wrk-op.b-num:
            do k = 0 to 9:
              if first(wrk-op.d-seq) then v-frst = yes.
              if wrk-op.num-sh[j + k] ne 0 then v-ok = yes.
            end.
            if not v-ok then next.

            IF v-printline >= 40 THEN DO:
                v-printline = 0.
                PAGE {1}.
            END.
            if v-frst THEN do:
            put "Machine Description"
                "Form " at 23 j       format ">>9"
                "Form " at 34 (j + 1) format ">>9"
                "Form " at 45 (j + 2) format ">>9"
                "Form " at 56 (j + 3) format ">>9"
                "Form " at 67 (j + 4) format ">>9"
                "Form " at 78 (j + 5) format ">>9"
                "Form " at 89 (j + 6) format ">>9"
                "Form " at 100 (j + 7) format ">>9"
                "Form " at 111 (j + 8) format ">>9"
                "Form " at 122 (j + 9) format ">>9" skip
                " M I N I M U M   R E Q U I R E D   S T A R T I N G " at 20
                " C O U N T   P E R   O P E R A T I O N" skip(1).
             v-printline = v-printline + 3.
            END.

            assign
             v-frst = no
             v-skip = yes.
             
            put wrk-op.m-dscr at 1
                wrk-op.num-sh[j for 10] format ">>>,>>>,>>>".
          end.

          if v-skip THEN do: 
              put skip(1).
              v-printline = v-printline + 2.
          END.
        end.

        do i = 1 to 10:
          assign
           v-frst = no
           v-ok   = no
           v-skip = no
           j      = ((i - 1) * 10) + 1.
          
          for each wrk-op break by wrk-op.s-num by wrk-op.d-seq by wrk-op.b-num:
            do k = 0 to 9:
              if first(wrk-op.d-seq) then v-frst = yes.
              if wrk-op.num-sh[j + k] ne 0 then v-ok = yes.
            end.
            if not v-ok then next.

            IF v-printline >= 40 THEN DO:
                v-printline = 0.
                PAGE {1}.
              END.
             
            if v-frst THEN DO:
            put "Machine Description"
                "Form " at 23 j       format ">>9"
                "Form " at 34 (j + 1) format ">>9"
                "Form " at 45 (j + 2) format ">>9"
                "Form " at 56 (j + 3) format ">>9"
                "Form " at 67 (j + 4) format ">>9"
                "Form " at 78 (j + 5) format ">>9"
                "Form " at 89 (j + 6) format ">>9"
                "Form " at 100 (j + 7) format ">>9"
                "Form " at 111 (j + 8) format ">>9"
                "Form " at 122 (j + 9) format ">>9" skip
                " E S T I M A T E D   M A K E   R E A D Y   H O U R " at 20
                " S   A N D   R U N   S P E E D" skip(1).
              v-printline = v-printline + 3.
            END.

            assign
             v-frst = no
             v-skip = yes.

            put wrk-op.m-dscr at 1.

            do k = 1 to j + 9:
              v-mrhr = if wrk-op.mr[k] eq 0 then ""
                       else string(wrk-op.mr[k],">9.99").

              put v-mrhr        
                  space(1)
                  wrk-op.speed[k] format ">>>>>".
              v-printline = v-printline + 1.
            end.
          end.

          if v-skip THEN do:
               put skip(1).
               v-printline = v-printline + 2.
          END.
        end.

        /** PRINT MISC/SUBCONTRACT **/
        x = 2.
        for each wrk-misc break by wrk-misc.form-no:
          x = (if x eq 1 then 0 else 1).
          if first(wrk-misc.form-no) THEN do:
          put v-fill format "x(50)" at 1 "  Miscellaneous - Subcontract  "
              v-fill format "x(50)" skip.
          v-printline = v-printline + 1.
          END.
          put "Form:" wrk-misc.form-no space(2) wrk-misc.cost.
          if x eq 0 THEN do:
              put skip.
              v-printline = v-printline + 1.
          END.
          else put space(16).
        end. /* each wrk-misc */

      end. /* last-of job-hdr.job-no2 */


     IF LAST(job-hdr.job-no2) THEN do:

         FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
                             AND bf-jobhdr.job-no = job-hdr.job-no
                             AND bf-jobhdr.job-no2 = job-hdr.job-no2
                             BREAK BY bf-jobhdr.blank-no:
           IF FIRST-OF(bf-jobhdr.blank-no) THEN DO:
                 IF s-prt-fgimage THEN DO:  
                     FIND FIRST itemfg NO-LOCK
                          WHERE itemfg.company EQ job-hdr.company 
                            AND itemfg.i-no    EQ bf-jobhdr.i-no NO-ERROR.
                     ls-fgitem-img = IF AVAIL itemfg THEN itemfg.box-image ELSE "" .
                     PAGE.
                     PUT UNFORMATTED "<#12><C1><FROM><C100><R+48><RECT><||3><C100>" /*v-qa-text*/ SKIP
                         "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                         "<=12><R+3><C1><FROM><C100><LINE><||3>"
                         "<=12><R+5><C5><#21><R+42><C+80><IMAGE#21=" ls-fgitem-img ">" SKIP. 
                     PAGE.
                 END.
           END. /* FIRST-OF(bf-jobhdr.frm) */
         END. /* bf-jobhdr */

     END. /* last(job-hdr.job-no2)*/

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
