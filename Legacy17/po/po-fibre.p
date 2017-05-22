/* -------------------------------------------------- po/po-fibre.p 10/00 JLF */
/*                                                                            */
/* Purchase Order Print Program - P/O Module - Fibre                          */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xjob-mat for job-mat.
def buffer xitem for item.
def buffer b-ref1 for reftable.
def buffer b-ref2 for reftable.

{po/po-print.i}

def var v-wid like po-ordl.s-wid format ">>9.99" no-undo.
def var v-len like po-ordl.s-len format ">>9.99" no-undo.
def var save_id as recid NO-UNDO.
def var time_stamp as char NO-UNDO.
def var v-print-lines as int NO-UNDO.
def var v-inst-lines as int NO-UNDO.
def var v-sname like shipto.ship-name NO-UNDO.
def var v-saddr like shipto.ship-addr NO-UNDO.
def var v-scity like shipto.ship-city NO-UNDO.
def var v-sstate like shipto.ship-state NO-UNDO.
def var v-szip like shipto.ship-zip NO-UNDO.
def var v-job as char format "x(9)" NO-UNDO.
def var v-po-tot like po-ord.t-cost extent 2 NO-UNDO.
def var v-t-freight like po-ord.t-freight extent 2 NO-UNDO.
def var v-sqft as dec NO-UNDO.
def var v-tot-sqft as dec extent 2 NO-UNDO.
def var v-ratio as dec NO-UNDO.
def var xg-flag as log init no no-undo.
def var same-score as char no-undo.
def var v-test-scr as log no-undo.
def var v-bottom as int init 0 no-undo.
def var v-change-dscr as char format "x(7)" no-undo.
def var v-change-ord as char format "x(35)" no-undo.
def var v-contact like po-ord.contact NO-UNDO.
def var v-mach as char extent 4 no-undo.

def var len-score as char NO-UNDO.
def var v-space as log init yes NO-UNDO.
def var v-adder like item.i-no extent 3 no-undo.
def var v-num-add as int init 0 no-undo.
def var v-counter as int init 0 no-undo.
def var v-tax like po-ordl.t-cost extent 3 NO-UNDO.
def var v-ord-qty as char NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 10 NO-UNDO.
DEF VAR v-tmp-lines AS dec NO-UNDO.
DEF VAR lv-got-return AS int NO-UNDO.
DEF VAR lv-text AS CHAR NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.

{custom/formtext.i NEW}

form v-ord-qty              to 6    format "x(6)"
     po-ordl.i-name         at 8    format "x(30)"
     v-adder[1]             at 39   format "x(10)"
     v-job                  at 50
     po-ordl.cost           at 59   format ">>>,>>9.99<"
     po-ordl.pr-uom
     v-sqft                 to 80   format ">,>>9.9<<"
     po-ordl.dscr[1]        at 8    format "x(30)"
     v-adder[2]             at 39   format "x(10)"
     v-mach[1]              at 50   format "x(6)"
     v-mach[2]              at 57   format "x(1)"
     po-ordl.due-date       at 60   FORMAT "99/99/99"
     po-ordl.t-cost         to 80   format "->,>>>,>>9.99"
     po-ordl.dscr[2]        at 8    format "x(30)"
     v-adder[3]             at 39   format "x(10)"
     v-tax[1]               to 80   format "->,>>>,>>9.99"

    with frame po-line stream-io width 80 no-box no-labels NO-UNDERLINE down.

form header
     skip(5)
     "** Continued **"      to 80
     skip(1)

    with frame po-cont page-bottom no-box no-labels NO-UNDERLINE STREAM-IO.

form header
     "Freight:"             to 66
     v-t-freight[2]         to 80   format ">,>>>,>>9.999"
     skip(1)
     "MSF:"                 to 66
     v-tot-sqft[2]          to 80   format ">,>>>,>>9.999"
     skip(1)
     "TAX:"                 to 66   
     v-tax[2]               to 80   format ">>,>>>,>>9.99"
     "AUTHORIZED BY: ________________________________"
     "GRAND TOTAL:"         to 66
     v-po-tot[2]            to 80   format ">>,>>>,>>9.99"
     skip(1)

    with frame po-tots page-bottom no-box no-labels NO-UNDERLINE STREAM-IO.

def stream last-page.


{ce/msfcalc.i}

find first po-ctrl where po-ctrl.company eq cocode no-lock.

find first company where company.company eq cocode no-lock.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

output stream last-page to value("po-fibre.txt") page-size VALUE(v-lines-per-page).

print-po-blok:
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
    break by po-ord.po-no:

  assign
   v-contact      = po-ord.contact
   v-change-ord   = "".
   
  {po/exportpo.i}
   
  if po-ord.stat eq "N" then po-ord.stat = "O".

  assign
   v-sname    = company.name
   v-saddr[1] = company.addr[1]
   v-saddr[2] = company.addr[2]
   v-scity    = company.city
   v-sstate   = company.state
   v-szip     = company.zip.
 
  if po-ord.type eq "D" then
    assign
     v-sname    = po-ord.ship-name
     v-saddr[1] = po-ord.ship-addr[1]
     v-saddr[2] = po-ord.ship-addr[2]
     v-scity    = po-ord.ship-city
     v-sstate   = po-ord.ship-state
     v-szip     = po-ord.ship-zip.
  
  find first vend
      where vend.company eq cocode
        and vend.vend-no eq po-ord.vend-no
      no-lock.

  find first terms
      where terms.company eq cocode
        and terms.t-code  eq po-ord.terms
      no-lock no-error.

  if first(po-ord.po-no) then
    form header
         skip(2)
         v-change-ord
         "P U R C H A S E  O R D E R"       to 80
         "**************************"       to 80
         skip(8)
         "PAGE: " + string(trim(string(page-number - v-last-page,">9")) +
                    " OF " + trim(string(v-page-tot,">9")))
                                            at 51 format "x(14)"
         skip(1)
         "DATE:"                            to 55
         po-ord.po-date                           FORMAT "99/99/99"
         skip(2)
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
         "ATTN:"                            at 7
         v-contact
         "PO#:"                             to 55
         po-ord.po-no
         fill("-",80)                               format "x(80)"
         "QTY"                              to 6
         "DESC/NOTES"                       at 8
         "ADDER"                            at 39
         "JOB #"                            at 50
         "COST/DUE"                         at 60
         "TOTALS/MSF"                       to 80
         fill("-",80)                               format "x(80)"

        with frame po-head page-top no-box no-labels no-underline stream-io width 80.

  view frame po-head.
  view stream last-page frame po-head.
  
  v-tax[3] = 0.
  for each po-ordl WHERE
      po-ordl.company EQ po-ord.company AND
      po-ordl.po-no EQ po-ord.po-no AND
      po-ordl.tax AND
      (v-printde-po or not po-ordl.deleted)
      no-lock:
      
    v-tax[3] = v-tax[3] + po-ordl.t-cost.  
  end.    

  {po/po-fibre.i "stream last-page" "(last-page)"}
  
  v-page-tot = page-number (last-page) - v-last-page.
  
  {po/po-fibre.i}

  v-last-page = page-number.
end. /* for each po-ord record */

output stream last-page close.

page.

/* end ----------------------------------- Copr. 2000  Advanced Software Inc. */
