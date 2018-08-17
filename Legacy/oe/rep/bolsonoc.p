/* ---------------------------------------------- oe/rep/bolprem.p  01/98 FWK */
/* Print Premier BOL                                                          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format ">>>>>9" no-undo.
def var v-tot-pal like oe-bolh.tot-pallets no-undo.
def var v-tot-wt like oe-bolh.tot-wt no-undo.
def var v-coname like cust.NAME no-undo.
def var v-printlines as int no-undo.
def var v-qty-uom like oe-ordl.pr-uom no-undo.
def var v-to-ship like oe-boll.qty no-undo.

def var v-shiplines as int no-undo.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-job-no   as char format "x(9)" no-undo.
def var v-partial  as char format "x" no-undo.
def var v-line-tot as int format ">>>>>9" no-undo.

find first oe-bolh no-lock no-error.
find first cust no-lock no-error.

format header
       skip(5)
       cust.name at 4
       v-ship-name  at 36
       cust.addr[1] at 4
       v-ship-addr[1] at 36
       cust.addr[2] at 4
       v-ship-addr[2] at 36
       cust.city at 4 cust.state cust.zip
       v-ship-city at 36
       v-ship-state
       v-ship-zip
       skip(4)
       "CARRIER" at 4 oe-bolh.carrier
       "DUE ON" at 55 oe-bolh.bol-date at 63 skip
       "BOL#" at 4 oe-bolh.bol-no format "999999"
       "TRAILER #" at 35 oe-bolh.trailer v-frt-pay-dscr
       "PAGE" at 63 page-number - v-last-page to 69 format "99"
       "OF" at 71 v-page-tot to 75 format "99" skip
       tmpstore at 1 skip
       "P.O.#" at 21 "PER" at 67 skip
       "ITEM NUMBER" at 4 "JOB#" at 21
       "PRODUCT DESCRIPTION" at 37
       "UNIT" at 62 "UNIT" at 67 "TOTAL" at 74 skip
       "---------------" at 4 "---------------" at 21
       "-----------------------" at 37
       "----" at 62 "------" at 67 "---------" at 77 skip
    with frame hd-top-comp no-box no-labels page-top stream-io width 86.

form
  oe-boll.i-no format "x(15)" at 4
  oe-boll.po-no format "x(15)" at 21
  oe-ordl.i-name format "x(24)" at 37
  oe-boll.cases format ">>>9" to 65
  oe-boll.qty-case format ">>>>>9" to 72
  v-line-tot format ">>>>>>>>9" to 82
  with frame ln-s down no-box no-labels stream-io width 90.

form
  v-job-no format "x(9)" at 21
  oe-ordl.part-dscr1 format "x(28)" at 37
  v-partial format "x" at 65
  oe-boll.partial format ">>>>>>" to 72
  v-line-tot format ">>>>>>>>9" to 82
  oe-ordl.part-dscr2 format "x(28)" at 37
  skip(1)
  with frame ln-s2 down no-box no-labels stream-io width 82.


tmpstore = fill(" ",80). /* was a hyphen */

{sa/sa-sls01.i}

find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

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
  end. /* first-of(oe-bolh.bol-no) */

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"999999")
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes
     v-tot-pkgs      = v-tot-pkgs + oe-boll.cases
     v-page-tot      = v-page-tot + 3.
    if oe-boll.partial gt 0 then v-tot-pkgs = v-tot-pkgs + 1.

    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-boll.i-no
        no-lock no-error.
    if avail itemfg and itemfg.part-dscr1 eq "" then
      v-page-tot = v-page-tot - 1.
  end.

  assign
   v-tot-pal = v-tot-pal + oe-bolh.tot-pallets
   v-tot-wt  = v-tot-wt  + oe-bolh.tot-wt.

  if last-of(oe-bolh.bol-no) then do:
    VIEW frame hd-top-comp.

    v-page-tot = v-page-tot / /*26*/ 21.
    {sys/inc/roundup.i v-page-tot}

    v-printlines = 1.

    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-boll.i-no
        no-lock

        break by report.key-01
              by report.key-02:

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-boll.ord-no
            and oe-ordl.i-no    eq oe-boll.i-no
            and oe-ordl.line    eq oe-boll.line
          no-lock no-error.

      if v-printlines gt 21 then do:  /* Gives seven lines of print. */
        page.
        v-printlines = 0.
      end.

      if avail oe-ordl then
      find first oe-ord
          where oe-ord.company eq cocode
            and oe-ord.ord-no  eq oe-ordl.ord-no
          no-lock no-error.

      if oe-ordl.pr-uom eq "CS" then
        v-to-ship = oe-boll.cases +
                    (if oe-boll.partial gt 0 then 1 else 0).
      else
        v-to-ship = oe-boll.qty.

      v-line-tot = oe-boll.cases * oe-boll.qty-case.

      display oe-boll.i-no
              oe-boll.po-no
              oe-ordl.i-name
              oe-boll.cases
              oe-boll.qty-case
              v-line-tot when oe-boll.partial eq 0
          with frame ln-s.
      down with frame ln-s.

      v-job-no = if oe-ordl.job-no eq "" then ""
                 else (trim(oe-ordl.job-no) + "-" +
                       string(oe-ordl.job-no2,"99")).

      if oe-boll.partial ne 0 then do:
        v-line-tot = v-line-tot + oe-boll.partial.

        display v-job-no
                oe-ordl.part-dscr1
                "1" @ v-partial
                oe-boll.partial
                v-line-tot
                oe-ordl.part-dscr2 with frame ln-s2.
      end.

      else do:
        v-line-tot = oe-boll.cases * oe-boll.qty-case.

        display v-job-no
                oe-ordl.part-dscr1
                " " @ v-partial
                0 @ oe-boll.partial
                oe-ordl.part-dscr2 with frame ln-s2.
      end.

      down with frame ln-s2.

      v-printlines = v-printlines + 4.

      delete report.
    end. /* for each oe-boll */

    v-shiplines = 0.
    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then v-shiplines = v-shiplines + 1.
    end.

    if (v-shiplines + v-printlines) gt 21 then do:
      page {1}.
      v-printlines = 0.
    end.

    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then put {1} oe-bolh.ship-i[i] at 11 skip.
    end.
    v-printlines = v-printlines + v-shiplines.

    if v-printlines > 21 then do:
      page {1}.
      v-printlines = 0.
    end.

    put skip(21 - v-printlines)
        "TOTAL CARTONS" at 6  v-tot-pkgs          to 25
        "TOTAL PALLETS" at 32 v-tot-pal           to 51
        "NET WEIGHT"    at 58 v-tot-wt            to 75 skip.

    assign
     v-tot-pkgs  = 0
     v-tot-pal   = 0
     v-tot-wt    = 0
     v-page-tot  = 0
     v-last-page = page-number.
  end.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

