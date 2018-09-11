/* -------------------------------------------------- po/pomidsex.p 01/97 JLF */
/*                                                                            */
/* Purchase Order Print Program - P/O Module - Middlesex                      */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}


{po/po-print.i}

def var v-wid like po-ordl.s-wid format ">>9.99" no-undo.
def var v-len like po-ordl.s-len format ">>9.99" no-undo.
def var pol-counter as int no-undo. /* CTS */
def var save_id as recid.
def var time_stamp as char.
def var v-print-lines as int.
def var v-inst-lines as int.
def var v-lastpage-num as int.
def var v-sname like shipto.ship-name.
def var v-saddr like shipto.ship-addr.
def var v-scity like shipto.ship-city.
def var v-sstate like shipto.ship-state.
def var v-szip like shipto.ship-zip.
def var v-job as char format "x(9)".
def var v-po-tot like po-ord.t-cost.
def var v-sqft as dec.
def var v-tot-sqft as dec.
def var v-ratio as dec.
def var xg-flag as log initial no no-undo.
def buffer xjob-mat for job-mat.
def buffer xitem for item.
def var same-score as char no-undo.
def var v-test-scr as log no-undo.
def var v-change-dscr as char format "x(7)" no-undo.
def var v-change-ord as char format "x(35)" no-undo.
def var v-num-copies as int no-undo.
def var v-repeat as int no-undo.

def var len-score as char.
def var v-space as log initial yes.
def var v-adder like item.i-no extent 2 no-undo.
def var v-num-add as int initial 0 no-undo.
def var v-counter as int initial 0 no-undo.

form po-ordl.line
     po-ordl.ord-qty        to 10   format "->>>,>>9"
     po-ordl.i-no           at 15
     v-adder[1]                    at 31   format "x(10)"
     v-job                  at 45
     po-ordl.cost            to 65   format ">>,>>9.999"
     po-ordl.pr-uom            to 70
     po-ordl.t-cost         to 80   format ">>,>>9.99"
     skip
/*
     po-ordl.i-name         at 17
*/
     v-adder[2]                    at 31   format "x(10)"
     po-ordl.due-date       at 55 FORMAT "99/99/99"
     v-change-dscr            to 80
     skip

    with frame po-line no-box no-labels no-underline stream-io down.

form header
     "** Continued **"      to 80
     skip(1)

    with frame po-cont page-bottom no-box no-labels no-underline stream-io.

form header
     v-po-tot                           to 80   format ">>,>>>,>>9.99"
     skip(1)

    with frame po-tots page-bottom no-box no-labels no-underline stream-io.

find first company where company.company eq cocode no-lock.
assign
 v-sname    = company.name
 v-saddr[1] = company.addr[1]
 v-saddr[2] = company.addr[2]
 v-scity    = company.city
 v-sstate   = company.state
 v-szip     = company.zip.

{ce/msfcalc.i}
       
find first po-ctrl where po-ctrl.company eq cocode no-lock.

    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        break by po-ord.po-no:

      {po/exportpo.i}

      v-change-ord = "".

      if po-ord.stat eq "N" then
        assign po-ord.stat = "O".
      else
      if po-ord.stat eq "U" then
        v-change-ord = "C H A N G E D   O R D E R   O N L Y".

      if po-ord.type eq "D" then
        assign v-sname    = po-ord.ship-name
               v-saddr[1] = po-ord.ship-addr[1]
               v-saddr[2] = po-ord.ship-addr[2]
               v-scity    = po-ord.ship-city
               v-sstate   = po-ord.ship-state
               v-szip     = po-ord.ship-zip.

      find first vend
          where vend.company eq po-ord.company
            and vend.vend-no eq po-ord.vend-no
          no-lock.

      find first terms
          where terms.company eq po-ord.company
            and terms.t-code  eq po-ord.terms
          no-lock no-error.

      if first(po-ord.po-no) then
        form header
             skip (7)
             space(22) v-change-ord                skip
             "P U R C H A S E  O R D E R"       at 51
             "**************************"       at 51
             company.name                       at 7
             company.addr[1]                    at 7
             company.addr[2]                    at 7
             po-ord.po-no                       to 61
             page-number - v-lastpage-num       to 71   format ">>9"
             string(trim(company.city)  +
                    ", "                +
                    trim(company.state) +
                    "  "                +
                    trim(company.zip))          at 7    format "x(30)"
             skip(1)
             po-ord.po-change-date              at 51   FORMAT "99/99/99"
             skip(4)
             vend.name                          at 11
             v-sname                            at 50
             vend.add1                          at 11
             v-saddr[1]                         at 50
             vend.add2                          at 11
             v-saddr[2]                         at 50
             string(trim(vend.city)  +
                    ", "             +
                    trim(vend.state) +
                    "  "             +
                    trim(vend.zip))             at 11   format "x(30)"
             string(trim(v-scity)  +
                    ", "           +
                    trim(v-sstate) +
                    "  "           +
                    trim(v-szip))               at 50   format "x(30)"
             skip(2)
             fill("-",80)                               format "x(80)"
             "TERMS:"                           at 7
             terms.dscr
             fill("-",80)                               format "x(80)"
             "QUANTITY"                         to 10
             "IDENT"                            at 15
             "ADDER"                            at 31
             "JOB #"                            at 45
             "COST/DUE DATE"                    at 55
             "TOTAL"                            at 76
             fill("-",80)                               format "x(80)"

            with frame po-head page-top no-box no-labels no-underline stream-io width 80.

      view frame po-head.
      page.

      hide frame po-tots.
      view frame po-cont.

      v-po-tot = 0.

      for each po-ordl WHERE 
         po-ordl.company EQ po-ord.company AND
         po-ordl.po-no EQ po-ord.po-no AND
         (v-printde-po or
          (not po-ordl.deleted))
          exclusive
          with frame po-line:

        assign
         v-print-lines = 6
         v-job         = fill(" ",6 - length(trim(po-ordl.job-no))) +
                         trim(po-ordl.job-no)
         v-adder[1]    = ""
         v-adder[2]    = ""
         xg-flag       = no.

        if po-ordl.stat eq "A" then
        assign v-change-dscr = "Added".
        else
        if po-ordl.stat eq "U" then
        assign v-change-dscr = "Updated".
        else
        if po-ordl.stat eq "O" then
        assign v-change-dscr = "Open".
        else
        if po-ordl.stat eq "P" then
        assign v-change-dscr = "Partial".
        else
        if po-ordl.stat eq "C" then
        assign v-change-dscr = "Closed".

        if po-ordl.deleted eq yes then
        assign v-change-dscr = "Deleted".

        find first item
            where item.company eq po-ordl.company
              and item.i-no    eq po-ordl.i-no
              and po-ordl.item-type
            no-lock no-error.

        if v-job ne "" then
          find last oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.job-no  eq v-job
                and oe-ordl.job-no2 eq po-ordl.job-no2
              use-index job no-lock no-error.

        if avail item and item.mat-type eq "B" then do:
           if po-ordl.s-len gt 0 and po-ordl.s-wid gt 0 then
             v-print-lines = v-print-lines + 3.

           if v-job ne "" and avail oe-ordl then
             v-print-lines = v-print-lines + 2.
        end.

        v-inst-lines = 0.
        do pol-counter = 1 to 4:
          if po-ordl.spec-i[pol-counter] ne "" then
            v-inst-lines = v-print-lines + 1.
        end.
        if v-inst-lines gt 0 then v-inst-lines = v-inst-lines + 1.

        v-print-lines = v-print-lines + v-inst-lines + 1.

        if (line-counter + v-print-lines) gt page-size then page.

        v-job = trim(v-job) + "-" + trim(string(po-ordl.job-no2,"99")).
        if trim(v-job) begins "-" then v-job = "".

        v-po-tot = v-po-tot + po-ordl.t-cost.

        if avail item then do:
          if item.mat-type eq "B" then do:
            assign
             v-wid = po-ordl.s-wid - trunc(po-ordl.s-wid,0)
             v-wid = ( v-wid * 16 ) / 100
             v-wid = trunc(po-ordl.s-wid,0) + v-wid
             v-len = po-ordl.s-len - trunc(po-ordl.s-len,0)
             v-len = ( v-len * 16 ) / 100
             v-len = trunc(po-ordl.s-len,0) + v-len.

            FIND FIRST job WHERE job.company = cocode AND
                                      job.job-no = STRING(FILL(" ",6 - LENGTH(
                                                  TRIM(po-ordl.job-no)))) +
                                                  TRIM(po-ordl.job-no) AND
                                      job.job-no2 = po-ordl.job-no2
                                 NO-LOCK NO-ERROR.
            IF AVAIL job THEN
            do:
              for each job-mat
                  where job-mat.company  eq cocode
                    and job-mat.job      eq job.job
                    and job-mat.job-no   eq job.job-no
                    and job-mat.job-no2  eq job.job-no2
                    and job-mat.i-no     eq po-ordl.i-no
                    and job-mat.frm      eq po-ordl.s-num
                  use-index job no-lock
                  break by job-mat.blank-no desc:
                if last(job-mat.blank-no)            or
                   job-mat.blank-no eq po-ordl.b-num then leave.
              end.

              if avail job-mat then 
              do:
                assign v-num-add = 0.
                for each xjob-mat where xjob-mat.company  eq cocode
                                        and xjob-mat.job      eq job-mat.job
                                        and xjob-mat.job-no   eq job-mat.job-no
                                        and xjob-mat.job-no2  eq job-mat.job-no2
                                        and xjob-mat.frm             eq job-mat.frm
                                        and xjob-mat.blank-no eq job-mat.blank-no
                                        and xjob-mat.i-no            ne job-mat.i-no
                                    no-lock:
                    find first xitem where xitem.company         eq cocode 
                                         and xitem.i-no         eq xjob-mat.i-no
                                         and xitem.mat-type        eq "A" no-lock no-error.
                  if avail xitem then
                  do:
                    assign v-num-add = v-num-add + 1.
                    if v-num-add eq 1 then
                      assign v-adder[1] = xitem.i-no.
                    else if v-num-add eq 2 then
                      assign v-adder[2] = xitem.i-no.
                  end.
                end.

                display po-ordl.line
                        po-ordl.ord-qty
                        po-ordl.i-no
                        v-adder[1]
                        v-job
                        po-ordl.cost
                        po-ordl.pr-uom
                        po-ordl.t-cost
                        v-adder[2]
                        po-ordl.due-date
                        v-change-dscr.

                /* Adder i-no and i-name to po of exist */
                assign v-counter = 0.
                for each xjob-mat where xjob-mat.company  eq cocode
                                        and xjob-mat.job      eq job-mat.job
                                        and xjob-mat.job-no   eq job-mat.job-no
                                        and xjob-mat.job-no2  eq job-mat.job-no2
                                        and xjob-mat.frm             eq job-mat.frm
                                        and xjob-mat.blank-no eq job-mat.blank-no
                                        and xjob-mat.i-no            ne job-mat.i-no
                                    no-lock:
                    find first xitem where xitem.company         eq cocode 
                                         and xitem.i-no         eq xjob-mat.i-no
                                         and xitem.mat-type        eq "A" no-lock no-error.

                  if avail xitem then
                  do:
                    assign v-counter = v-counter + 1.
                    if v-counter gt 2 then
                      put xitem.i-no at 31. 
                  end.
                end.

                    FIND FIRST ef WHERE ef.e-num = job.e-num AND
                                      ef.form-no = job-mat.frm NO-LOCK NO-ERROR.
                if avail ef and (ef.xgrain = "S" or ef.xgrain = "B") then
                    assign xg-flag = yes.

              end. /* avail job-mat */
              else
                      display po-ordl.line
                              po-ordl.ord-qty
                              po-ordl.i-no
                              v-job
                              po-ordl.cost
                              po-ordl.pr-uom
                              po-ordl.t-cost
                              po-ordl.due-date
                        v-change-dscr.
            end. /* avail job */
            else
              display po-ordl.line
                      po-ordl.ord-qty
                      po-ordl.i-no
                      v-job
                      po-ordl.cost
                      po-ordl.pr-uom
                      po-ordl.t-cost
                      po-ordl.due-date
                      v-change-dscr.

            put "W: " at 15 v-wid space(2) "L: " v-len
                space(2) "Flute: " item.flute space(2) item.reg-no.

            run po/po-ordls.p (recid(po-ordl)).
            
            {po/poprints.i}
            
                if not v-test-scr then
                  put skip
                      space(14)
                      "Score: "
                      len-score format "x(50)".
          
                else
                if dec(trim(len-score)) ne v-wid then
                  put skip
                      space(14)
                      "Score: "
                      len-score format "x(50)".
              end.
              END.
            end.
            
            if po-ordl.s-len gt 0 and po-ordl.s-wid gt 0 then do:
              assign
               v-sqft = if v-corr then (po-ordl.s-len * po-ordl.s-wid) * .007
                                  else (po-ordl.s-len * po-ordl.s-wid) / 144
               v-tot-sqft = v-sqft * po-ordl.ord-qty.

              put skip(1)
                  "SQFT/ITEM"       at 34
                  v-sqft            to 62   format ">>>,>>9.9999"
                  skip
                  "TOT SQFT ORDER"  at 34
                  v-tot-sqft        to 62   format ">>>,>>9.9999".
            end.

            if v-job ne "" and avail oe-ordl then do:
              v-ratio = (po-ordl.t-cost / oe-ordl.t-price) * 100.

              put skip(1)
                  "*"               at 34
                  v-ratio           to 62   format ">>>,>>9.9999".
            end.

          end. /* if item-type = B */
          else
            display po-ordl.line
                    po-ordl.ord-qty
                    po-ordl.i-no
                    v-job
                    po-ordl.cost
                    po-ordl.pr-uom
                    po-ordl.t-cost
                    po-ordl.due-date        
                    v-change-dscr.

        end. /* if avail item */

        else do:

        display po-ordl.line
                po-ordl.ord-qty
                po-ordl.i-no
                v-job
                po-ordl.cost
                po-ordl.pr-uom
                po-ordl.t-cost
                po-ordl.due-date
                v-change-dscr.
        end.

        if v-inst-lines gt 1 then do:
          put skip(1).
          do pol-counter = 1 to 4:
            if po-ordl.spec-i[pol-counter] ne "" then
              put po-ordl.spec-i[pol-counter] format "x(65)" at 10 skip.
          end.
        end.

        put skip(1).
      end. /* for each po-ordl record */

      v-lastpage-num = page-number.

      hide frame po-cont.
      view frame po-tots.
    end. /* for each po-ord record */

    page.

/* END ----------------------------------- Copr. 1997  Advanced Software Inc. */


