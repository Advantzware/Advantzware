/* ---------------------------------------------- oe/rep/bolprem.p  01/98 FWK */
/* Print Premier BOL                                                          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.

def workfile w-cas field w-qty-case like oe-boll.qty-case
                   field w-cases    like oe-boll.cases.

{oe/rep/oe-lad.i}
DEF VAR list-name AS CHAR NO-UNDO.
DEF VAR init-dir AS CHAR NO-UNDO.
{sys/inc/print1.i}
def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format "->>>>>9" no-undo.
def var v-tot-pal like oe-bolh.tot-pallets no-undo.
def var v-tot-wt AS INT FORMAT "->>>>>9" /*like oe-bolh.tot-wt*/ no-undo.
def var v-coname like cust.NAME no-undo.
def var v-printlines as int no-undo.
def var v-qty-uom like oe-ordl.pr-uom no-undo.
def var v-to-ship like oe-boll.qty no-undo.

def var v-shiplines  as   int no-undo.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-job-no     as   char format "x(9)" no-undo.
def var v-line-tot   as   int format "->>>>>9" no-undo.
def var v-po-tot     as   int format "->>>>>9" no-undo.

find first oe-bolh no-lock no-error.
find first cust no-lock no-error.

format header
       skip(6)
       "BILL TO:" at 1 /* cust.cust-no to 30 */
       "SHIP TO:" at 41 /* shipto.ship-id to 70 */
       cust.name at 6
       v-ship-name  at 46
       cust.addr[1] at 6
       v-ship-addr[1] at 46
       cust.addr[2] at 6
       v-ship-addr[2] at 46
       cust.city at 6 cust.state cust.zip
       v-ship-city at 46
       v-ship-state
       v-ship-zip
       skip(2)
       "DUE ON" at 55 oe-bolh.bol-date at 63 skip
       "BOL#" oe-bolh.bol-no format "999999"
       "TRAILER #" at 35 oe-bolh.trailer v-frt-pay-dscr
       "PAGE " + TRIM(STRING(PAGE-NUMBER - v-last-page,">>>>>")) +
           " OF " + TRIM(STRING(v-page-tot,">>>>>")) AT 63 FORMAT "x(19)"
       tmpstore at 1 skip
       "ITEM NUMBER" at 1
       "P.O.#" at 17 "PER" at 67 skip
       "ORDER NUMBER" at 1 "JOB#" at 17
       "PRODUCT DESCRIPTION" at 33
       "UNIT" at 62 "UNIT" at 67 "TOTAL" at 74 skip
       "---------------" at 1 "---------------" at 17
       "----------------------------" at 33
       "----" at 62 "------" at 67 "------" at 74 skip
    with frame hd-top-comp no-box no-labels page-top stream-io width 200.

form
  oe-boll.i-no format "x(15)" at 1
  oe-rel.po-no format "x(15)" at 17
  itemfg.i-name format "x(28)" at 33
  oe-boll.cases format "->>>>" to 65
  oe-boll.qty-case format "->>>>>>" to 72
  v-line-tot format "->>>>>>" to 79
  with frame ln-s down no-box no-labels stream-io width 200.

def stream last-page.


tmpstore = fill("-",80).

{sa/sa-sls01.i}

output stream last-page to value(tmp-dir + "\bolrfc.txt" + string(time)) page-size VALUE(v-lines-per-page).

find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

view frame hd-top-comp.
view stream last-page frame hd-top-comp.

for each xreport  where xreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)  eq xreport.rec-id
    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-bolh.cust-no
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    find first carrier
        where carrier.company eq cocode
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.
    v-carrier = if avail carrier then carrier.dscr else "".
   
    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no no-lock,
        first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-boll.ord-no
        no-lock,
        
        first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no
          AND oe-ordl.i-no    EQ oe-boll.i-no
          AND oe-ordl.LINE    EQ oe-boll.LINE
        NO-LOCK:

      v-frt-pay-dscr = if oe-ord.frt-pay eq "p" then "PREPAID"
                       else
                       if oe-ord.frt-pay eq "c" then "COLLECT"
                       else
                       if oe-ord.frt-pay eq "b" then "PPD/CHG" else "".

      find first uom where uom.uom eq oe-ordl.pr-uom no-lock no-error.
      v-qty-uom = if avail uom then substr(uom.dscr,1,8)
                  else oe-ordl.pr-uom.

      LEAVE.
    end.
    v-time = string(time,"hh:mm am").

    if oe-ctrl.pr-broker and avail cust and shipto.broker then
      v-coname = cust.name.

    else v-coname = company.name.

    if avail shipto then
      assign
       v-ship-name    = shipto.ship-name
       v-ship-addr[1] = shipto.ship-addr[1]
       v-ship-addr[2] = shipto.ship-addr[2]
       v-ship-city    = shipto.ship-city
       v-ship-state   = shipto.ship-state
       v-ship-zip     = shipto.ship-zip.
    else
      assign
       v-ship-name    = ""
       v-ship-addr[1] = ""
       v-ship-addr[2] = ""
       v-ship-city    = ""
       v-ship-state   = ""
       v-ship-zip     = "".

    page.
    page stream last-page.
  end. /* first-of(oe-bolh.bol-no) */

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.qty     NE 0:
        
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.po-no
     report.key-02   = oe-boll.i-no
     report.key-03   = string(oe-boll.ord-no,"9999999999")
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes
     v-tot-pkgs      = v-tot-pkgs + oe-boll.cases.
     
    if oe-boll.partial gt 0 then v-tot-pkgs = v-tot-pkgs + 1.
  end.

  assign
   v-tot-pal = v-tot-pal + oe-bolh.tot-pallets
   v-tot-wt  = v-tot-wt  + oe-bolh.tot-wt.

  if last-of(oe-bolh.bol-no) then do:
    {oe/rep/bolrfc.i "stream last-page"}

    v-page-tot = page-number (last-page) - v-last-page.

    {oe/rep/bolrfc.i}

    assign
     v-tot-pkgs  = 0
     v-tot-pal   = 0
     v-tot-wt    = 0.
  end.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

output stream last-page close.

hide frame hd-top-comp no-pause.
page.

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

