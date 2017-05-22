/* --------------------------------------------------- po/po-cent.p 06/99 FWK */
/*                                                                            */
/* Purchase Order Print Program - P/O Module - Century                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1 for reftable.
def buffer b-ref2 for reftable.

{po/po-print.i}
{custom/notesdef.i}
DEF VAR v-inst AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

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
def var v-tot-sqft as dec extent 2.
def var v-ratio as dec.
def var xg-flag as log initial no no-undo.
def buffer xjob-mat for job-mat.
def buffer xitem for item.
def var same-score as char no-undo.
def var v-test-scr as log no-undo.
def var v-bottom as int initial 0 no-undo.
def var v-change-dscr as char format "x(7)" no-undo.
def var v-change-ord as char format "x(35)" no-undo.
def var v-contact like po-ord.contact.

def var len-score as char.
def var v-space as log initial yes.
def var v-adder like item.i-name extent 2 no-undo.
def var v-num-add as int initial 0 no-undo.
def var v-counter as int initial 0 no-undo.


form po-ordl.ord-qty        to 6    format ">>>>>9"
     po-ordl.i-name         at 8    format "x(15)"
     v-adder[1]             at 25   format "x(15)"
     v-job                  at 42
     po-ordl.cost           at 59   format ">>,>>9.99"
     po-ordl.pr-uom         /* to 70 */
     v-sqft                 to 80   format ">>>9.9"
     skip
     po-ordl.dscr[1]        at 8    format "x(15)"
     v-adder[2]             at 25   format "x(15)"
     po-ordl.due-date       at 59   FORMAT "99/99/99"
     v-change-dscr            to 80
     skip

    with frame po-line width 80 no-box no-labels no-underline stream-io down.

form header
     "** Continued **"      to 80
     skip(1)

    with frame po-cont page-bottom no-box no-labels no-underline stream-io.

form header
     "TOTAL ORDER MSF:"                 to 66
     v-tot-sqft[2]                      to 80   format ">>,>>>,>>9.99"
     skip(1)

    with frame po-tots page-bottom no-box no-labels no-underline stream-io.


def stream last-page.
output stream last-page to value("po-cent.txt") page-size VALUE(v-lines-per-page).

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

      /*djk*/
      v-contact = po-ord.contact.

      {po/exportpo.i}

      v-change-ord = "".
      if po-ord.stat eq "N" then
        assign po-ord.stat = "O".

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
             skip (1)
             space (22) v-change-ord                 skip
             "P U R C H A S E  O R D E R"       at 51
             "**************************"       at 51
             company.name                       at 7
             "PAGE"                             at 72
             company.addr[1]                    at 7
             company.addr[2]                    at 7
             "PO#:"                             at 52
             po-ord.po-no                       to 63
             page-number - v-last-page          to 71   format ">>9"
             "OF" v-page-tot FORM ">>9"
             string(trim(company.city)  +
                    ", "                +
                    trim(company.state) +
                    "  "                +
                    trim(company.zip))           at 7   format "x(30)"
             skip
             "ATTN:" at 7 v-contact 
             skip
             "DATE:"                            at 51
             po-ord.po-date                     at 58   FORMAT "99/99/99"
             skip(3)
             "TO:"                              at 11
             "SHIP TO:"                         at 50
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
             "QTY"                              to 5
             "TEST"                             at 8
             "ADDER"                            at 25
             " JOB #"                           at 42
             "COST/DUE DATE"                    at 59
             "MSF"                              to 80
             fill("-",80)                               format "x(80)"

            with frame po-head page-top no-box no-labels no-underline stream-io width 80.

        view frame po-head.
        view stream last-page frame po-head.

        {po/po-cent.i "stream last-page" "last-page"}

        v-page-tot = page-number (last-page) - v-last-page.       

        {po/po-cent.i}

        v-last-page = page-number.

    end. /* for each po-ord record */

    output stream last-page close.

    page.

/* END ----------------------------------- Copr. 1997  Advanced Software Inc. */





