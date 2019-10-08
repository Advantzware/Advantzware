/* ------------------------------------------------- cec/rep/ticket.p 10/94 gb*/
/* Factory Ticket                                                             */
/* -------------------------------------------------------------------------- */

def input parameter v-format like sys-ctrl.char-fld.

{sys/inc/var.i shared}


if lookup(v-format,"10 Pitch,ASI") eq 0 then do:
  run cec/rep/jobtick.p (v-format).
  return.
end.

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
def new shared var v-layout as int format "99" extent 20.
def new shared var v-frst as log.
def new shared var v-ok as log.
def new shared var v-numcopy as int format ">9" init 1.
def new shared var v-est-qty as int format "->>,>>>,>>9".
def new shared var v-job-no like oe-ordl.job-no.
def new shared var v-job-no2 like oe-ordl.job-no2.
def new shared var v-due-date like oe-ord.due-date.
def new shared var v-reprint as log.

def var v-line as int init 1 no-undo.
def var v-gsh-qty as int no-undo.
def var cnt as int init 1 no-undo.
def var v-frm-blk as char format "x(6)" no-undo.
def var v-dec as dec.
DEF VAR help-id AS CHAR NO-UNDO.

def new shared buffer xjob-hdr for job-hdr.

def new shared workfile wrk-op
  field m-dscr like est-op.m-dscr
  field m-code like est-op.m-code
  field d-seq like est-op.d-seq
  field dept like est-op.dept
  field num-sh like est-op.num-sh extent 100.

def new shared workfile wrk-die
  field die-no like eb.die-no
  field form-no like eb.form-no
  field die-size as char format "x(17)".

def new shared workfile wrk-sheet
  field gsh-qty like ef.gsh-qty
  field cal like ef.cal
  field brd-dscr like ef.brd-dscr
  field form-no like ef.form-no
  field sh-wid like ef.nsh-len
  field sh-len like ef.nsh-wid.

def new shared workfile wrk-film
  field form-no like ef.form-no
  field snum as int format "9"
  field bnum as int format "99"
  field leaf as char format "x(10)"
  field leaf-l as dec format ">9.9999"
  field leaf-w as dec format ">9.9999".

def new shared workfile wrk-ink
  field i-code as char format "x(10)"
  field form-no like eb.form-no
  field blank-no like eb.blank-no
  field i-dscr as char format "x(20)"
  field i-qty as dec format ">99.999".

def new shared workfile wrk-prep
  field code like est-prep.code
  field dscr like est-prep.dscr
  field s-num as int format "9"
  field b-num as int format "99"
  field ml like est-prep.ml.

def new shared workfile wrk-spec
  field form-no like ef.form-no
  field spec-no as char format "x(10)"
  field dscr as char format "x(20)"
  field qty as dec format ">>>,>>9.9<<<<"
  field uom as char format "x(3)".

def new shared workfile wrk-inst
  field d-seq like dept.fc
  field line like est-inst.line-no
  field dscr like est-inst.dscr
  field rec-id as recid.

def new shared workfile wrk-misc
  field form-no like ef.form-no
  field snum as int format "9"
  field bnum as int format "99"
  field cost as char format "x(20)".

assign help-id = program-name(1)
  v-fill = fill("=",132).
{sys/form/s-top.f}
def new shared frame head.

format skip(1)
  "   Form Job #: " v-job[1] space(0) "-" space(0) v-job2[1]
  "  To Job #: " v-job[2] space(0) "-" space(0) v-job2[2] skip(1)
  "   Number Of Copies:" v-numcopy
  "    Reprint Tickets:" v-reprint
  with title "  FACTORY TICKET  "
  frame selec row 3 centered overlay no-labels width 55.

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
  with no-box frame head no-labels /*page-top*/ width 200.

format
  "Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
  "Salesman:" at 68 oe-ord.sname[1] "Order#:" at 113 oe-ord.ord-no
  with no-box frame line-head no-labels width 200.
  

{sys/inc/notes.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

outers:
repeat on error undo outers, leave outers with frame head:

  pause 0.
  display v-job v-job2 v-numcopy with frame selec.
  do j = 1 to 2:
    update v-job[j] v-job2[j] with frame selec.
     v-bld-job = "".
    do i = 1 to 6:       /** REMOVE BLANKS IN JOB NUMBER **/
      if substr(input v-job[j],i,1) ne " " then
       v-bld-job  = trim(v-bld-job +
        substr(input v-job[j],i,1)).
    end. /* 1 - 6 */
     v-job[j] =
      string(fill(" ",6 - length(v-bld-job))) + (trim(v-bld-job)).
    display v-job[j] with frame selec.
  end. /* 1 - 2 */
  update v-numcopy v-reprint with frame selec.

  if keyfunction(lastkey) = "end-error" then
  undo outers, leave outers.
  /*if choice then
  do:
    {sys/msg/print.i print}
    pause 0.
    {sys/inc/outprint.i 58}.

    do cnt = 1 to v-numcopy:

    for each job-hdr
        where job-hdr.company    eq cocode
          and job-hdr.job-no     ge v-job[1]
          and job-hdr.job-no     le v-job[2]
          and job-hdr.job-no2    ge v-job2[1]
          and job-hdr.job-no2    le v-job2[2]
          and job-hdr.ftick-prnt eq v-reprint
          and can-find(first job where job.company eq cocode
                                   and job.job     eq job-hdr.job
                                   and job.job-no  eq job-hdr.job-no
                                   and job.job-no2 eq job-hdr.job-no2
                                   and job.stat    ne "H")
        use-index job-no exclusive-lock
        
        break by job-hdr.job-no by job-hdr.job-no2:

      job-hdr.ftick-prnt = true.

      find first est where est.e-num = job-hdr.e-num no-lock no-error.
      if avail est then
       v-est-qty = est.est-qty[1].
      else
      v-est-qty = 0.
      /** PRINT JOB HEADER INFORMATION **/
      if first-of(job-hdr.job-no2) then
      do:
        assign v-job-no  = job-hdr.job-no
          v-job-no2 = job-hdr.job-no2.
        find first oe-ord where oe-ord.company = job-hdr.company and
          oe-ord.ord-no = job-hdr.ord-no
          no-lock no-error.

        if avail oe-ord then
        do:
          if NOT oe-ctrl.p-fact and oe-ord.stat eq "H" then next.
          v-due-date = oe-ord.due-date.
        end.
        else
         v-due-date = ?.
        if not first(job-hdr.job-no) then
        page.
        view frame head.

        if /* CTS -check avail */ avail est and est.est-type gt 2 then
         v-line = 500.
        else
         v-line = 50.

        /** SUM UP NUMBER OF SHEETS for EACH OPERATION **/
        for each job-mch where job-mch.company eq cocode and
            job-mch.job     eq job-hdr.job
            break by job-mch.frm by job-mch.line:

          find mach where mach.company eq cocode and
            mach.loc     eq locode and
            mach.m-code  eq job-mch.m-code
            no-lock.
          /* CTS
          find first wrk-op where wrk-op.m-code eq job-mch.m-code no-error.
          if not avail wrk-op then
          ***/
          do:
            create wrk-op.
            assign wrk-op.m-code = job-mch.m-code
              wrk-op.m-dscr = mach.m-dscr
              wrk-op.d-seq = job-mch.line
              wrk-op.dept = job-mch.dept.
          end.
           wrk-op.num-sh[job-mch.frm] = wrk-op.num-sh[job-mch.frm]
            + job-mch.run-qty.
        end.

        /** BUILD PREP WORK FILE **/
        for each job-prep where job-prep.company eq cocode and
            job-prep.job     eq job-hdr.job:
          find first prep where prep.company eq cocode AND
            prep.code    eq job-prep.code
            no-lock no-error.
          create wrk-prep.
          assign wrk-prep.code = job-prep.code
            wrk-prep.dscr = if avail prep then prep.dscr else ""
            wrk-prep.s-num = job-prep.frm
            wrk-prep.b-num = job-prep.blank-no
            wrk-prep.ml = job-prep.ml.
        end. /* each job-prep */

        if avail est then
        for each est-prep of est
            where index("SON",est-prep.simon) gt 0
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
        for each oe-ordm of oe-ord no-lock:
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

      /** PRINT CUSTOMER INFORMATION **/
      if first-of(job-hdr.job-no) and avail oe-ord then
      display oe-ord.cust-name oe-ord.sold-id
        oe-ord.sname[1]  oe-ord.ord-no
        with frame line-head.
      /* first ord-no */

      for each ef where ef.e-num eq job-hdr.e-num
          break by ef.e-num by ef.form-no:

        for each eb
            where eb.e-num     eq job-hdr.e-num
              and eb.form-no   eq ef.form-no
              and (eb.blank-no eq job-hdr.blank-no or
                   (avail est and est.est-type eq 2))
            use-index eb break by form-no:

           v-layout[eb.form-no] = v-layout[eb.form-no] + eb.num-up.
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
              if eb.i-code2[i] eq job-mat.i-no and
                 eb.i-ps2[i]   eq job-mat.pass then do:

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
                   wrk-ink.i-dscr   = eb.i-dscr2[i].
                end.
              end.
            end.

            find first wrk-ink
                where wrk-ink.i-code   eq job-mat.i-no
                  and wrk-ink.form-no  eq job-mat.frm
                  and wrk-ink.blank-no eq job-mat.blank-no
                no-error.

            if avail wrk-ink then wrk-ink.i-qty = job-mat.qty.
          end. /* JOB-MAT */

          if eb.est-type eq 3 then
           v-est-qty = eb.bl-qty.
          else
          if eb.est-type eq 4 then
           v-est-qty = eb.yld-qty.

          if last-of(eb.form-no) then do:
            find first style where style.company eq eb.company and
              style.style eq eb.style no-lock no-error.
            if avail style then
             v-stypart = style.dscr.
            assign v-dsc[1] = eb.part-dscr1
              v-dsc[2] = eb.part-dscr2
              v-size[1] = string(eb.len) + "x" + string(eb.wid) + "x" +
              string(eb.dep)
              v-size[2] = eb.i-coldscr.
            if first-of(job-hdr.job-no2) then
              put " Item # / Qty         Description" "Size / Style" at 50
                "Est #" at 72 " Case Cnt Code" "Layout # On Per Form" at 106
                skip "--------------   -------------------------"
                "-------------------------" at 44 "-----" at 72
                " -------- ----------" "------------------------------" at 103.
            else
              put fill("-",132) format "x(132)".

            /** PRINT ITEM INFORMATION **/
            find first oe-ordl where                /* Added CTS */
              oe-ordl.company eq job-hdr.company and
              oe-ordl.ord-no  eq job-hdr.ord-no  and
              oe-ordl.job-no  eq job-hdr.job-no  and
              oe-ordl.job-no2 eq job-hdr.job-no2 and
              oe-ordl.i-no    eq job-hdr.i-no    no-lock no-error.

            if avail oe-ordl then v-est-qty = oe-ordl.qty.

            display job-hdr.i-no v-dsc[1] at 17 format "x(26)" space(1)
              v-size[1] space(2) job-hdr.est-no space(4)
              oe-ordl.cas-cnt when avail(oe-ordl)
              eb.cas-no space(6) v-layout[1 for 10] skip
              "Job:" job-hdr.qty format "->>,>>>,>>9"
              v-dsc[2] at 20 format "x(26)"
              space(1) v-stypart space(31) v-layout[11 for 10]
              skip
              "Ord:" v-est-qty format "->>,>>>,>>9" space(3)
              /* CTS re-added oe-ordl referances with avail() */
              "Req Date:" oe-ordl.req-date when avail(oe-ordl)
              space(3) "Prm Date:"
              oe-ordl.prom-date when avail(oe-ordl) space(3)
              "P.O.:" oe-ordl.po-no when avail(oe-ordl) skip(1)
              with width 135 no-labels no-box frame line-det.
          end. /* last-of(eb.form-no) */
        end. /* each eb */

        if last-of(job-hdr.job-no2) then
        put v-fill skip.
        /** BUILD SHEET INFORMATION **/
        for each job-mat where job-mat.company eq cocode and
            job-mat.job     eq job-hdr.job and
            job-mat.frm     eq ef.form-no
            use-index job no-lock:
          find item where item.company eq cocode and
            item.i-no    eq job-mat.i-no
            no-lock no-error.
          if avail item and item.mat-type eq "B" then
          do:
            create wrk-sheet.
            assign wrk-sheet.gsh-qty = job-mat.qty
              wrk-sheet.cal = ef.cal
              wrk-sheet.brd-dscr = if   item.i-dscr ne ""
                                   then item.i-dscr
                                   else ef.brd-dscr /* CTS */
              wrk-sheet.form-no = ef.form-no
              wrk-sheet.sh-wid = if   job-mat.wid gt 0
                                 then job-mat.wid
                                 else ef.gsh-wid  /* CTS */
              wrk-sheet.sh-len = if   job-mat.len gt 0
                                 then job-mat.len
                                 else ef.gsh-len. /* CTS */
            leave.
          end.
        end.

        /** BUILD FILM INFORMATION **/
        for each job-mat where job-mat.company eq cocode and
            job-mat.job     eq job-hdr.job and
            job-mat.frm     eq job-hdr.frm
            no-lock:
          find item where item.company eq cocode and
            item.i-no    eq job-mat.i-no
            no-lock no-error.
          if not avail item or index("WF", item.mat-type) eq 0 then
          next.

          create wrk-film.
          assign wrk-film.form-no = job-hdr.frm
            wrk-film.snum = job-hdr.frm
            wrk-film.bnum = job-hdr.blank-no
            wrk-film.leaf = job-mat.i-no
            wrk-film.leaf-l = item.s-len
            wrk-film.leaf-w = item.s-wid.
        end.

        /** BUILD SPECIAL MATERIAL INFORMATION **/
        do i = 1 to 8:
          if ef.spec-no[i] ne "" then
          do:
            create wrk-spec.
            assign wrk-spec.form-no = ef.form-no
              wrk-spec.spec-no = ef.spec-no[i]
              wrk-spec.dscr = ef.spec-dscr[i]
              /*wrk-spec.qty = ef.spec-qty[i]*/
              wrk-spec.uom = ef.spec-uom[i].

            RUN custom/extradec.p (.0001, ef.spec-qty[i],
                                   OUTPUT wrk-spec.qty).
          end.
        end.
        /** BUILD MISC/SUBCONTRACT INFORMATION **/
        do i = 1 to 6:
          if ef.mis-cost[i] ne "" then
          do:
            create wrk-misc.
            assign wrk-misc.form-no = ef.form-no
              wrk-misc.snum = ef.mis-snum[i]
              wrk-misc.bnum = ef.mis-bnum[i]
              wrk-misc.cost = ef.mis-cost[i].
          end.
        end.

        if last(ef.form-no) and last-of(job-hdr.job-no2) then
        do:
          /** PRINT DIE INFORMATION **/
           x = 2.
          for each wrk-die:
            x = (if x eq 1 then
            0 else
            1).
            put "Die Number:" wrk-die.die-no "  Size:"  wrk-die.die-size
              space(17).
            if x eq 0 then
            put skip.
          end. /* each wrk-die */
          if x ne 2 then
          put v-fill at 1 skip.

          /** PRINT INK INFORMATION **/
          x = 2.
          /*
          for each wrk-ink
              break by wrk-ink.i-code
                    by wrk-ink.form-no
                    by wrk-ink.blank-no:
            if not first-of(wrk-ink.i-code) then wrk-ink.i-qty = 0.
          end.
          */
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
                  "Item #:"
                  space(1).
              x = 0.
            end.
            else put space(17).

            x = if x eq 1 then 0 else 1.
            put wrk-ink.i-code
                space(2)
                wrk-ink.i-dscr
                space(2).

            if wrk-ink.i-qty ne 0 then
              put wrk-ink.i-qty.
            else
              put space(7).

            if x eq 0 or last-of(wrk-ink.blank-no) then
              put skip.
            else
              put space(16).

            if last-of(wrk-ink.blank-no) then put skip(1).

            delete wrk-ink.
          end. /* each wrk-ink */

          if x ne 2 then
          put v-fill at 1 skip.

          /** PRINT LEAF/FILM INFORMATION **/
           x = 2.
          for each wrk-film:
            put "Form: " wrk-film.snum space(0) "/" space(0)
              wrk-film.bnum space(3)
              " Leaf/Window Film: " wrk-film.leaf  space(3)
              "Length: " at 68 wrk-film.leaf-l " Width: "
              wrk-film.leaf-w skip.
             x = 1.
          end. /* each wrk-film */
          if x ne 2 then
          put v-fill at 1 skip.

          /** PRINT PREP MATERIAL INFORMATION **/
           x = 2.
          for each wrk-prep where wrk-prep.ml:
            x = (if x eq 1 then
            0 else
            1).
            put "Form: " wrk-prep.s-num space(0) "/" space(0)
              wrk-prep.b-num
              space(2) "Prep Material: " wrk-prep.code space(2)
              wrk-prep.dscr.
            if x eq 0 then
            put skip.
            else
            put space(13).
          end. /* each wrk-prep */
          if x ne 2 then
          put v-fill at 1 skip.

          /** PRINT SPECIAL MATERIAL INFORMATION **/
           x = 2.
          for each wrk-spec:
            put "Form:" wrk-spec.form-no
              space(2) "Special Material: " wrk-spec.spec-no space(2)
              wrk-spec.dscr "Qty:" at 68 wrk-spec.qty wrk-spec.uom.
            x = 1.
          end. /* each wrk-spec */
          if x ne 2 then
          put v-fill at 1 skip.

          /** PRINT SHEET INFORMATION **/
           x = 2.
          for each wrk-sheet break by wrk-sheet.form-no:
            v-gsh-qty = 0.
            wrkoploop:
            for each wrk-op break by wrk-op.d-seq:
              repeat counter = 1 to 100:
                v-gsh-qty = v-gsh-qty +
                            if   wrk-op.num-sh[counter] ne ?
                            then wrk-op.num-sh[counter]
                            else 0.
              end.
              leave wrkoploop.
            end.
            display "Sheets Required:"
            /* CTS wrk-sheet.gsh-qty */ v-gsh-qty /* CTS end */
            space(5) "Size:"
              string(wrk-sheet.sh-wid) + "x" + string(wrk-sheet.sh-len)
              format "x(25)"
              "Caliper:" at 68 wrk-sheet.cal space(5) "Board:"
              wrk-sheet.brd-dscr "Form:" at 123 wrk-sheet.form-no
              with width 132 no-labels no-box frame sheet.
             x = 1.
          end. /* each wrk-sheet */
          if x ne 2 then
          put v-fill at 1 skip.

          /** PRINT PREP LABOR INFORMATION **/
           x = 2.
          for each wrk-prep where not wrk-prep.ml:
            x = (if x eq 1 then
            0 else
            1).
            put "Form: " wrk-prep.s-num space(0) "/" space(0)
              wrk-prep.b-num
              space(2) "Prep Labor: " wrk-prep.code space(2)
              wrk-prep.dscr.
            if x eq 0 then
            put skip.
            else
            put space(13).
          end. /* each wrk-prep */
          if x ne 2 then
          put v-fill at 1 skip.
        end. /* last(ef.form-no) */
      end. /* each ef */

      if last-of(job-hdr.job-no2) then
      do:
        /** PRINT JOB INSTRUCTIONS **/
        for each est-inst
            where est-inst.e-num eq job-hdr.e-num
              and lookup(est-inst.dept,v-exc-depts) eq 0
            no-lock:
          find first dept where dept.code eq est-inst.dept no-lock no-error.
          create wrk-inst.
          assign
           wrk-inst.d-seq  = if avail dept then dept.fc else 0
           wrk-inst.dscr   = if avail dept then dept.dscr else est-inst.dept
           wrk-inst.line   = est-inst.line-no
           wrk-inst.rec-id = recid(est-inst).
        end.

        for each wrk-inst,
            each est-inst where recid(est-inst) eq wrk-inst.rec-id no-lock
            break by wrk-inst.d-seq
                  by wrk-inst.line:

          if first-of(wrk-inst.d-seq) then
            put wrk-inst.dscr + " Instructions:" format "x(25)".
          do i = 1 to 3:
            if est-inst.inst[i] ne "" then
            put space(2) est-inst.inst[i] at 30 skip.
          end.
          
          if last(wrk-inst.d-seq) then put v-fill at 1 skip.
        end. /* each est-inst */

        j = 1.
        do i = 1 to 2:
          assign v-frst = no
            v-ok = no.
          for each wrk-op break by wrk-op.d-seq:
            do k = 0 to 9:
              if first(wrk-op.d-seq) then
               v-frst = yes.
              if wrk-op.num-sh[j + k] ne 0 then
               v-ok = yes.
            end.

            if not v-ok then
            next.
            if v-frst then
            put "Machine Description" "Form " at 23 j format ">>9"
              "Form " at 34 (j + 1) format ">>9"
              "Form " at 45 (j + 2) format ">>9"
              "Form " at 56 (j + 3) format ">>9"
              "Form " at 67 (j + 4) format ">>9"
              "Form " at 78 (j + 5) format ">>9"
              "Form " at 89 (j + 6) format ">>9"
              "Form " at 100 (j + 7) format ">>9"
              "Form " at 111 (j + 8) format ">>9"
              "Form " at 122 (j + 9) format ">>9" skip
              " M I N M U M   R E Q U I R E D   S T A R T I N G "
              at 20
              " C O U N T   P E R   O P E R A T I O N" skip(1).
             v-frst = no.
            put wrk-op.m-dscr at 1
              wrk-op.num-sh[j for 10] format ">>>,>>>,>>z".
          end.
           j = j + 10.
          put skip(1).
        end.

        /** PRINT MISC/SUBCONTRACT INFORMATION **/
         x = 2.
        for each wrk-misc break by wrk-misc.form-no:
          x = (if x eq 1 then
          0 else
          1).
          if first(wrk-misc.form-no) then
          put v-fill format "x(50)" at 1 "  Miscellaneous - Subcontract  "
            v-fill format "x(50)" skip.
          put "Form:" wrk-misc.form-no space(2) wrk-misc.cost.
          if x eq 0 then
          put skip.
          else
          put space(16).
        end. /* each wrk-misc */
      end. /* last-of job-hdr.job-no2 */

      /** PRINT MULT COPIES OF TICKETS **/
       save_id = recid(job-hdr).
      if last-of(job-hdr.job-no2) then
      do:
        if v-numcopy gt 1 then page.
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
       v-layout = 0.
    end. /* each job-hdr */
    end.  /* do 1 to v-numcopy */
    {sys/inc/close.i}
  end. /* choice */ */

  leave outers. /* fake loop */
end.      /* outers */

output to terminal.
hide all no-pause.

/* end ---------------------------------- copr. 1994  advanced software, inc. */
