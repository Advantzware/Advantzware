/* ------------------------------------------------- cerep/jobtick  */
/*  factory ticket  for folding                                                          */
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
DEF VAR li AS INT NO-UNDO.

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
  field num-sh like est-op.num-sh extent 100.

def new shared workfile wrk-die
  field die-no like eb.die-no
  field form-no like eb.form-no
  field die-size as char format "x(17)".

def new shared workfile wrk-sheet
  field gsh-qty like ef.gsh-qty
  field cal like ef.cal
  field i-no LIKE ef.board
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
DEF VAR v-shipto AS cha EXTENT 4 NO-UNDO.
DEF VAR v-case-size AS cha NO-UNDO.
DEF VAR v-vend LIKE po-ord.vend-no NO-UNDO.
DEF VAR v-item AS cha EXTENT 20 NO-UNDO.
DEF VAR v-ink1 AS cha EXTENT 10 NO-UNDO.
DEF VAR v-ink2 AS cha EXTENT 10 NO-UNDO.
DEF VAR v-po-no LIKE oe-ordl.po-no NO-UNDO.

v-fill = fill("=",132).

def new shared frame head.

format header
       "JOB NUMBER:" v-job-no space(0) "-" space(0) v-job-no2 format "99"
       "F A C T O R Y   T I C K E T" at 52  "JOB START DATE:" at 117 v-start-date skip
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
          and job-hdr.ftick-prnt            eq v-reprint
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
              by job-hdr.job-no2
              BY job-hdr.frm:

      IF NOT job-hdr.ftick-prnt THEN DO WHILE TRUE:
        li = li + 1.
        FIND xjob-hdr EXCLUSIVE-LOCK
            WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
            NO-ERROR NO-WAIT.
        IF AVAIL xjob-hdr THEN xjob-hdr.ftick-prnt = YES.
        IF li GE 1000 OR xjob-hdr.ftick-prnt THEN LEAVE.
      END.
      
      v-est-qty = if avail est then est.est-qty[1] else 0.

      if first-of(job-hdr.frm) then v-first = yes.
      
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
        v-start-date = job-hdr.start-date.

        if not first(job-hdr.job-no) then page.
        view frame head.
     
        find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq job-hdr.i-no
                no-lock no-error.
        v-req-date = IF AVAIL oe-ordl THEN oe-ordl.req-date ELSE ?.
        PUT "<B>Customer Name:</B>" oe-ord.cust-name 
            "<B>REQ DATE:   DUE DATE:   Estimate:" SKIP
            "Shipto:</B>" v-shipto[1] v-req-date v-due-date job-hdr.est-no FORMAT "x(8)"
            SKIP
            v-shipto[2] SKIP
            v-shipto[3] SKIP
            v-fill SKIP.

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

        if ef.form-no eq job-hdr.frm then ebloop:
        for each eb
            WHERE eb.company     EQ ef.company
              AND eb.est-no      eq ef.est-no
              and eb.form-no     eq ef.form-no
              and ((eb.stock-no  eq job-hdr.i-no and
                    eb.stock-no  ne "") or
                   (eb.blank-no  eq job-hdr.blank-no or
                    (eb.blank-no eq 1 and
                     (est.est-type eq 2 or est.est-type eq 6)) and
                    eb.stock-no  eq ""))
            break by eb.form-no.

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
              wrk-die.die-size = string(ef.trim-l) + "x" +
              string(ef.trim-w).
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
          
          if last-of(eb.form-no) then do:
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
             
            if v-first then
              PUT "<B>F/B  FG Item #       Job Qty      PO#             Description                  Style         Carton Size     Case Count  Case Size #UP  UPC#:</B>" SKIP
              /*put " Item # / Qty         Description" "Size / Style" at 50
                  "Est #" at 72 " Case Cnt Code" "Layout # On Per Form" at 106
                  skip "--------------   -------------------------"
                  "-------------------------" at 44 "-----" at 72
                  " -------- ---------------"
                  "------------------------------" at 103.*/
                 .
            else
              put fill("-",132) format "x(132)". 

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

            display trim(string(eb.form-no,">>9")) + "-" +
                    trim(string(eb.blank-no,">>9"))
                    job-hdr.i-no 
                    v-job-qty * v-fac format "->>,>>>,>>9"
                    oe-ordl.po-no when avail oe-ordl 
                    v-dsc[1] 
                    v-stypart
                    v-size[1]
                    oe-ordl.cas-cnt when avail oe-ordl
                    eb.cas-cnt when (not avail oe-ordl) or oe-ordl.cas-cnt eq 0 @ oe-ordl.cas-cnt
                    v-case-size
                    v-up
                    eb.upc-no 
                    skip
                with stream-io width 135 no-labels no-box frame line-det1.
                
            find first item
                where item.company eq cocode
                  and item.i-no    eq eb.cas-no
                no-lock no-error.
            v-cas-dscr = if avail item then item.i-name else "".
          end. /* last-of(eb.form-no) */
        end. /* each eb */

        /* Number of sheets */
        run oe/rep/ticket1.p (recid(ef), recid(job-hdr)).
        find first wrk-sheet where recid(wrk-sheet) eq save_id.
        FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                            AND po-ord.po-no = int(oe-ordl.po-no) NO-LOCK NO-ERROR.
        v-vend = IF AVAIL po-ord THEN po-ord.vend-no ELSE "".
        v-po-no = oe-ordl.po-no.
        PUT v-fill SKIP
            "<B>BOARD CODE             SHEETS REQ'D   SHEET SIZE   BOARD PO# VENDOR#   Die Size    Die#:  CAD#:</B>"
            SKIP.
        /** PRINT SHEET **/
          x = 2.
          for each wrk-sheet break by wrk-sheet.form-no:
            display wrk-sheet.brd-dscr
                    wrk-sheet.gsh-qty space(5)
                    string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len)
                    format "x(25)"
                    oe-ordl.po-no when avail oe-ordl
                    v-vend
                    string(ef.trim-w) + "x" + string(ef.trim-l)
                    eb.die-no
                    eb.cad-no
                    /*"Caliper:" AT 68 wrk-sheet.cal space(5) "Board:"
                    wrk-sheet.brd-dscr "Form:" AT 123 wrk-sheet.form-no*/
                with stream-io width 132 no-labels no-box frame sheet.
            x = 1.
          end. /* each wrk-sheet */
          if x ne 2 then put v-fill at 1 skip.

                           
        if last(ef.form-no) /*and last-of(job-hdr.job-no2)*/  then do:
          put v-fill at 1 skip.
         /*
          /** PRINT DIE **/
          x = 2.
          for each wrk-die:
            x = if x eq 1 then 0 else 1.
            put "Die Number:" wrk-die.die-no
                "  Size:"  wrk-die.die-size
                space(17).
            if x eq 0 then put skip.
          end. /* each wrk-die */
          if x ne 2 then put v-fill at 1 skip.
       */

          /** PRINT INK **/
          PUT "<B>INK PASS 1   INK NAME         ITEMS     INK PASS 2(BACK)     INK NAME        ITEMS          PLATE #</B>"
              SKIP.
          x = 2.
          i = 0.
          for each wrk-ink
              break by wrk-ink.form-no
                    by wrk-ink.blank-no
                    by wrk-ink.i-pass
                    :
              IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.
                
            v-item[i] = IF LOOKUP(string(wrk-ink.blank-no),v-item[i]) > 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ",".
            IF LAST-OF(wrk-ink.i-pass) THEN DO:
               IF wrk-ink.i-pass = 1 THEN
                  v-ink1[i] = wrk-ink.i-code + " " + wrk-ink.i-dscr + " " + v-item[i].
               ELSE IF wrk-ink.i-pass = 2 THEN
                   v-ink2[i] = wrk-ink.i-code + " " + wrk-ink.i-dscr + " " + v-item[i].
               i = i + 1.
            END.
               

            delete wrk-ink.
          end. /* each wrk-ink */

          DO j = 1 TO 7:
             PUT v-ink1[j] FORM "x(50)" v-ink2[j] FORM "x(50)" .
             IF j = 1 THEN PUT eb.plate-no.
             PUT SKIP.
          END.
          
          put v-fill at 1 skip.

          /** PRINT LEAF/FILM **/
          x = 2.
          for each wrk-film:
            put "Form: " wrk-film.snum space(0) "/" space(0)
                wrk-film.bnum space(3)
                " Leaf/Window Film: " wrk-film.leaf  space(2)
                "Length: " at 68 wrk-film.leaf-l " stream-io width: "
                wrk-film.leaf-w skip.
            x = 1.
          end. /* each wrk-film */
          if x ne 2 then put v-fill at 1 skip.

          /** PRINT PREP MATERIAL **/
          x = 2.
          for each wrk-prep where wrk-prep.ml:
            x = (if x eq 1 then 0 else 1).
            put "Form: "
                wrk-prep.s-num space(0) "/" space(0)
                wrk-prep.b-num
                space(2) "Prep Material: " wrk-prep.code space(1)
                wrk-prep.dscr.
            if x eq 0 then put skip.
            else put space(13).
          end. /* each wrk-prep */
          if x ne 2 then put v-fill at 1 skip.

          /** PRINT SPECIAL MATERIAL **/
          x = 2.
          for each wrk-spec:
            put "Form:" wrk-spec.form-no
                space(2) "Special Material: " wrk-spec.spec-no space(2)
                wrk-spec.dscr "Qty:" at 68 wrk-spec.qty wrk-spec.uom.
            x = 1.
          end. /* each wrk-spec */
          if x ne 2 then put v-fill at 1 skip.
          /*==
          /** PRINT SHEET **/
          x = 2.
          for each wrk-sheet break by wrk-sheet.form-no:
            display "Sheets Required:" wrk-sheet.gsh-qty space(5) "Size:"
                    string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len)
                    format "x(25)"
                    "Caliper:" AT 68 wrk-sheet.cal space(5) "Board:"
                    wrk-sheet.brd-dscr "Form:" AT 123 wrk-sheet.form-no
                with stream-io width 132 no-labels no-box frame sheet.
            x = 1.
          end. /* each wrk-sheet */
          if x ne 2 then put v-fill at 1 skip.

          /** PRINT PREP LABOR **/
          x = 2.
          for each wrk-prep where not wrk-prep.ml:
            x = if x eq 1 then 0 else 1.
            put "Form: " wrk-prep.s-num space(0) "/" space(0)
                wrk-prep.b-num
                space(2) "Prep Labor: " wrk-prep.code space(1)
                wrk-prep.dscr.
            if x eq 0 then put skip.
            else put space(13).
          end. /* each wrk-prep */
          if x ne 2 then put v-fill at 1 skip.
          ========= */
        end. /* last(ef.form-no) */
      end. /* each ef */

      if last-of(job-hdr.frm) then do:

         PUT "<B>MACHINE    MR WASTE   MR HRS  RUN SPEED   SPOIL%   MATERIAL FOR MACHINE   SIZE   TOTAL REQUIRED    PALLET</B>"
             SKIP.
         FOR EACH wrk-op WHERE wrk-op.s-num = job-hdr.frm:
         
             PUT wrk-op.m-dscr
                 wrk-op.mr
                 wrk-op.speed.

        end.

        /** PRINT JOB INSTRUCTIONS **/

        /* dept notes*/
        FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
        {custom/notespr4.i job v-inst2 20 "notes.rec_key = job.rec_key and notes.note_code <> '' AND LOOKUP(notes.note_code,v-exc-depts) EQ 0"}
        DO i = 1 TO 20:
           v-dept-inst[i] = v-inst2[i].
        END.
        
    /*  IF v-ship <> "" THEN v-dept-inst[6] = v-ship.  /* shipto notes */ */
        PUT "<B>DEPARTMENT          INSTRUCTION NOTES" SKIP
            v-dept-inst[1] SKIP
            v-dept-inst[2] SKIP
            v-dept-inst[3] SKIP
            v-dept-inst[4] SKIP.
            /* "            " v-dept-inst[5] SKIP
            "            " v-dept-inst[6] SKIP
            "            " v-dept-inst[7] SKIP
            "            " v-dept-inst[8] SKIP
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
        PUT v-fill skip
            "<B><U>LABEL ITEM 1</U>"
            "<U>LABEL ITEM 2</U>" AT 40
            "<U>LABEL ITEM 3</U></B>" AT 80
            SKIP
            "Job#:" v-job-no + "-" + string(v-job-no2)
            "Job#:" v-job-no + "-" + string(v-job-no2) AT 40
            "Job#:" v-job-no + "-" + string(v-job-no2) AT 80
            SKIP
            "Customer:" oe-ord.cust-name 
            "Customer:" oe-ord.cust-name AT 40
            "Customer:" oe-ord.cust-name AT 80
            SKIP
            "Purchase Order#:" v-po-no
            "Purchase Order#:" v-po-no AT 40
            "Purchase Order#:" v-po-no AT 80
            SKIP
            "<B>FG Item #:</B>" job-hdr.i-no  
            "<B>FG Item #:</B>" job-hdr.i-no AT 40
            "<B>FG Item #:</B>" job-hdr.i-no AT 80
            SKIP
            "Description:" v-dsc[1]
            "Description:" v-dsc[1] AT 40
            "Description:" v-dsc[1] AT 80
            SKIP
            "Quantity:" job-hdr.qty
            "Quantity:" job-hdr.qty AT 40
            "Quantity:" job-hdr.qty AT 80
            .
             
      end. /* last-of job-hdr.job-no2 */

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
