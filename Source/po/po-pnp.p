/* ---------------------------------------------------- po/po-pnp.p 12/00 FWK */
/* Purchase Order Print Program for S-8-POPRINT = P&P                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1  for reftable.
def buffer b-ref2  for reftable.

{po/po-print.i}

def var v-wid like po-ordl.s-wid format ">>9.99" no-undo.
def var v-len like po-ordl.s-len format ">>9.99" no-undo.
def var pol-counter as int no-undo.
def var save_id as recid.
def var time_stamp as char.
def var v-exp-limit as int no-undo init 10.
def var v-line-number as int.
def var v-page-counter as int format ">>9".
def var v-lines-to-skip as int.
def var v-sname like shipto.ship-name.
def var v-saddr like shipto.ship-addr.
def var v-scity like shipto.ship-city.
def var v-sstate like shipto.ship-state.
def var v-szip like shipto.ship-zip.
def var v-po-type as char format "x(10)".
def var v-freight-dscr as char format "x(7)".
def var v-change-dscr as char format "x(7)".
def var v-dash-line as char format "x(80)" extent 3.
def var v-adders as log.
def var xg-flag as log init no no-undo.
def var v-space as log init yes.
def var len-score as char.
def buffer xjob-mat for job-mat.
def buffer xitem for item.
def var same-score as ch no-undo.
def var v-test-scr as log no-undo.
def var v-hdr as char format "x(15)" initial "" no-undo.
def var v-ino-job as char format "x(15)" initial "" no-undo.
def var v-change-ord as char format "x(35)" initial "" no-undo.
def var v-fax as cha no-undo.

v-dash-line = fill ("_",80).

{po/po-print.f}


assign v-hdr = "VEND ITEM".
       
find first company where company.company eq cocode NO-LOCK.

    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
        BREAK BY PO-ORD.PO-NO: 
    
      if po-ord.ship-id ne "" then
        assign v-sname     = po-ord.ship-name
               v-saddr[1]  = po-ord.ship-addr[1]
               v-saddr[2]  = po-ord.ship-addr[2]
               v-scity     = po-ord.ship-city
               v-sstate    = po-ord.ship-state
               v-szip      = po-ord.ship-zip.
      else
        assign
         v-sname     = company.name
         v-saddr [1] = company.addr [1]
         v-saddr [2] = company.addr [2]
         v-scity     = company.city
         v-sstate    = company.state
         v-szip      = company.zip.

      assign po-ord.printed = yes
             v-page-counter  = 1
             v-change-ord    = "".

      if po-ord.stat eq "N" then
        assign po-ord.stat = "O".
      else
      if po-ord.stat eq "U" then
        v-change-ord = "C H A N G E D   O R D E R   O N L Y".

      find first vend where vend.company eq po-ord.company 
                        and vend.vend-no eq po-ord.vend-no no-lock no-error.
      find first terms where terms.t-code eq po-ord.terms no-lock no-error.
      find first carrier where carrier.company eq po-ord.company 
                           and carrier.carrier eq po-ord.carrier no-lock no-error.

      if po-ord.type eq "R" then
        assign v-po-type = "Regular".
      else
        assign v-po-type = "Drop Ship".

      if po-ord.frt-pay eq "P" then
        assign v-freight-dscr = "Prepaid".
      else if po-ord.frt-pay eq "C" then
        assign v-freight-dscr = "Collect".
      else
        assign v-freight-dscr = "Bill".

      ASSIGN v-phone = "PH:  " + vend.area-code + "-" + string(vend.phone,"xxx-xxxx")
             v-fax = "FAX: " + vend.fax-area + "-" + string(vend.fax,"xxx-xxxx").

      if v-pre-printed-forms eq yes then do:
        display po-ord.po-no v-page-counter po-ord.po-date v-po-type
                po-ord.po-change-date vend.name vend.add1 vend.add2 vend.city
                vend.state vend.zip v-sname v-saddr [1] v-saddr [2] v-scity
                v-sstate v-szip po-ord.buyer po-ord.contact
                terms.dscr when avail terms po-ord.acknowledge
                po-ord.fob-code carrier.dscr po-ord.frt-pay
                with frame po-head.

        assign v-line-number = 24.
      end.
      else
      do:
        if v-company eq yes then
        do:
          display v-change-ord company.name company.addr [1] company.addr [2] po-ord.po-no
                  v-page-counter company.city company.state company.zip
                  po-ord.po-date v-po-type po-ord.po-change-date vend.name
                  vend.add1 vend.add2 vend.city vend.state vend.zip v-sname
                  v-saddr [1] v-saddr [2] v-scity v-sstate v-szip
                  v-dash-line [1]
                  po-ord.buyer po-ord.contact
                  terms.dscr when avail terms
                  po-ord.acknowledge po-ord.fob-code
                  carrier.dscr v-freight-dscr v-dash-line [2] v-dash-line [3]
                  v-hdr v-phone v-fax with frame po-head-2.
          assign v-line-number = 26.
        end.
        else
        do:
          display v-change-ord po-ord.po-no v-page-counter
                  po-ord.po-date v-po-type po-ord.po-change-date vend.name
                  vend.add1 vend.add2 vend.city vend.state vend.zip v-sname
                  v-saddr [1] v-saddr [2] v-scity v-sstate v-szip
                  v-dash-line [1]
                  po-ord.buyer po-ord.contact
                  terms.dscr when avail terms
                  po-ord.acknowledge po-ord.fob-code
                  carrier.dscr v-freight-dscr v-dash-line [2] v-dash-line [3]
                  v-hdr with frame po-head-3.
          assign v-line-number = 26.
        end.
      end.

      for each po-ordl WHERE
          po-ordl.company EQ po-ord.company AND
          po-ordl.po-no EQ po-ord.po-no by po-ordl.line:
        assign xg-flag = no.
        if not v-printde-po and po-ordl.deleted then next.
       
        if v-line-number gt 49 then
        do:
          assign v-page-counter = v-page-counter + 1.
          page.
          if v-pre-printed-forms eq yes then
            display
              po-ord.po-no v-page-counter po-ord.po-date v-po-type
              po-ord.po-change-date vend.name vend.add1 vend.add2
              vend.city vend.state vend.zip v-sname v-saddr [1]
              v-saddr [2] v-scity v-sstate v-szip po-ord.buyer po-ord.contact
              terms.dscr when avail terms
              po-ord.acknowledge po-ord.fob-code carrier.dscr
              po-ord.frt-pay
              with frame po-head.
          else
            display
              company.name company.addr [1] company.addr [2]
              po-ord.po-no v-page-counter company.city company.state
              company.zip po-ord.po-date v-po-type po-ord.po-change-date
              vend.name vend.add1 vend.add2 vend.city vend.state
              vend.zip v-sname v-saddr [1] v-saddr [2] v-scity v-sstate
              v-szip v-dash-line [1] po-ord.buyer po-ord.contact 
              terms.dscr when avail terms
              po-ord.acknowledge po-ord.fob-code carrier.dscr
              v-freight-dscr v-dash-line [2] v-dash-line [3]
              v-hdr v-phone v-fax with frame po-head-2.
          assign v-line-number  = 28.
        end. /* if v-line-number gt 49 */

        assign v-change-dscr = "".

        if po-ordl.stat eq "A" then
          assign v-change-dscr = "Added".
        else if po-ordl.stat eq "U" then
          assign v-change-dscr = "Updated".
        else if po-ordl.stat eq "O" then
          assign v-change-dscr = "Open".
        else if po-ordl.stat eq "P" then
          assign v-change-dscr = "Partial".
        else if po-ordl.stat eq "C" then
          assign v-change-dscr = "Closed".

        if po-ordl.deleted eq yes then
          assign v-change-dscr = "Deleted".

        assign v-ino-job = po-ordl.vend-i-no.

        if v-pre-printed-forms eq yes then
        do:
          display
            po-ordl.line po-ordl.ord-qty po-ordl.pr-qty-uom po-ordl.i-no
            /* po-ordl.vend-i-no */ v-ino-job
            po-ordl.cost po-ordl.t-cost v-change-dscr
            po-ordl.i-name po-ordl.due-date po-ordl.pr-uom
            with frame po-line.
        end.
        else
        do:
          display
            po-ordl.line po-ordl.ord-qty po-ordl.pr-qty-uom po-ordl.i-no
            /* po-ordl.vend-i-no */ v-ino-job
            po-ordl.cost po-ordl.t-cost
            /* CTS */ po-ordl.over-pct po-ordl.under-pct /* CTS end */
            po-ordl.i-name po-ordl.due-date v-change-dscr po-ordl.pr-uom
            with frame po-line-2.
        end.
        assign v-line-number = v-line-number + 3.
        find item where item.company eq po-ordl.company
                    and item.i-no    eq po-ordl.i-no
                    and po-ordl.item-type
                  no-lock no-error.

        if po-ordl.dscr[1] ne "" then do:
          put po-ordl.dscr[1] format "x(50)" at 21 skip.
          v-line-number = v-line-number + 1.
        end.

        if po-ordl.dscr[2] ne "" then do:
          put po-ordl.dscr[2] format "x(50)" at 21 skip.
          v-line-number = v-line-number + 1.
        end.

        if avail item and item.mat-type eq "B" then do:
          if v-shtsiz then do:
            assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0).
            assign v-wid = ( v-wid * 16 ) / 100.
            assign v-wid = truncate(po-ordl.s-wid,0) + v-wid.
            assign v-len = po-ordl.s-len - truncate(po-ordl.s-len,0).
            assign v-len = ( v-len * 16 ) / 100.
            assign v-len = truncate(po-ordl.s-len,0) + v-len.

            find first job where job.company eq cocode 
                             and job.job-no eq string(fill(" ",6 - length(
                                                trim(po-ordl.job-no)))) +
                                                trim(po-ordl.job-no) 
                             and job.job-no2 eq po-ordl.job-no2
                           no-lock no-error.
            if avail job then
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
                /* Adder i-no and i-name to po of exist */
                for each xjob-mat where xjob-mat.company  eq cocode
                                    and xjob-mat.job      eq job-mat.job
                                    and xjob-mat.job-no   eq job-mat.job-no
                                    and xjob-mat.job-no2  eq job-mat.job-no2
                                    and xjob-mat.frm      eq job-mat.frm
                                    and xjob-mat.blank-no eq job-mat.blank-no
                                    and xjob-mat.i-no     ne job-mat.i-no
                                  no-lock:
                  find first xitem where xitem.company        eq cocode
                                     and xitem.i-no      eq xjob-mat.i-no
                                     and xitem.mat-type  eq "A" no-lock no-error.
                  if avail xitem then
                  do:
                    put xitem.i-no at 21 xitem.i-name at 38.
                    assign v-line-number = v-line-number + 1.
                  end.

                end.

                find first ef where EF.COMPANY EQ JOB.COMPANY
                                AND ef.est-no  EQ job.est-no
                                and ef.form-no eq job-mat.frm
                              no-lock no-error.
                if avail ef and (ef.xgrain eq "S" or ef.xgrain eq "B") then
                  assign xg-flag = yes.
              end. /* avail job-mat */
            end. /* avail job */
            put "W: " at 21 v-wid space(2) "L: " v-len
                space(2) "Flute: " item.flute space(2) item.reg-no.
            
            assign v-line-number = v-line-number + 1.
            
            run po/po-ordls.p (recid(po-ordl)).
            
            {po/poprints.i}
            
                if not v-test-scr then do:
                  put skip
                      space(20)
                      "Score: "
                      len-score format "x(50)".
                      
                  v-line-number = v-line-number + 1.
                end.
          
                else
                if dec(trim(len-score)) ne v-wid then do:
                  put skip
                      space(14)
                      "Score: "
                      len-score format "x(50)".
                      
                  v-line-number = v-line-number + 1.
                end.
              end.
              END.
            end.
          end. /* v-shtsiz */        
        end. /* avail item and item.mat-type eq "B" */
        
        repeat pol-counter = 1 to 4:
          if po-ordl.spec-i[pol-counter] ne "" then
          do:
            put po-ordl.spec-i[pol-counter] format "x(65)" at 10 skip.
            assign v-line-number = v-line-number + 1.
          end.
        end.

        put skip(1).
        assign v-line-number = v-line-number + 1.

      end. /* for each po-ordl record */

      assign v-lines-to-skip = 49 - v-line-number.
      put skip (v-lines-to-skip).
      if v-pre-printed-forms eq yes then
        display po-ord.t-cost po-ord.spec-i [1] po-ord.spec-i [2]
                po-ord.spec-i [3] po-ord.spec-i [4]
                with frame po-totals.
      else
        display v-dash-line [1] po-ord.t-cost po-ord.spec-i [1]
                po-ord.spec-i [2] po-ord.spec-i [3] po-ord.spec-i [4]
                po-ord.t-freight /* RLL */
                with frame po-totals-2.
      page.
    end. /* for each po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */

