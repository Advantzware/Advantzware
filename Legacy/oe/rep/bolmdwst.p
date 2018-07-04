/* ---------------------------------------------- oe/rep/bolmdwst.p 01/98 JLF */
/* Print Midwest BOL                                                          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer xoe-boll for oe-boll.

def workfile w-cas field w-qty-case like oe-boll.qty-case
                   field w-cases    like oe-boll.cases.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format ">>>>>>" no-undo.
def var v-tot-pal like oe-bolh.tot-pallets no-undo.
def var v-tot-wt like oe-bolh.tot-wt no-undo.
def var v-tot-sqft as dec format ">>>>>>>>" no-undo.
def var v-coname like company.name no-undo.
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
def var v-line-tot   as   int format ">>>>>9" no-undo.
def var v-po-tot     as   int no-undo.

find first oe-bolh no-lock no-error.
find first cust no-lock no-error.

format header
       skip(4)
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
       "BOL#" oe-bolh.bol-no format "999999"
       "Trailer #" at 23 oe-bolh.trailer FORM "x(8)" v-frt-pay-dscr
       "SHIP DATE" at 52 oe-bolh.bol-date at 63 SKIP
       "Carrier #" at 1 oe-bolh.carrier v-frt-pay-dscr 
       "PAGE" at 63 page-number - v-last-page to 69 format "99"
       "OF" at 71 v-page-tot to 75 format "99" skip
       tmpstore at 1 FORMAT "x(80)" skip
       "ITEM NUMBER" at 1
       "P.O.#" at 17 "PER" at 67 skip
       "ORDER NUMBER" at 1 "JOB#" at 17
       "PRODUCT DESCRIPTION" at 33
       "UNIT" at 62 "UNIT" at 67 "TOTAL" at 74 skip
       "---------------" at 1 "---------------" at 17
       "----------------------------" at 33
       "----" at 62 "------" at 67 "--------" at 74 skip
    with frame hd-top-comp no-box no-labels page-top stream-io width 82.

form
  oe-boll.i-no format "x(15)" at 1
  oe-rel.po-no format "x(15)" at 17
  itemfg.i-name format "x(28)" at 33
  oe-boll.cases format ">>>>" to 65
  oe-boll.qty-case format ">>>>>>" to 72
  v-line-tot format ">>>>>>>>" to 81
  with frame ln-s down no-box no-labels stream-io width 90.

tmpstore = fill("-",80).

{sa/sa-sls01.i}

find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

view frame hd-top-comp.

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

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,

        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      v-frt-pay-dscr = if oe-ord.frt-pay eq "p" then "PREPAID"
                       else
                       if oe-ord.frt-pay eq "c" then "COLLECT"
                       else
                       if oe-ord.frt-pay eq "b" then "PPD/CHG" else "".
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock no-error.
      if avail oe-ordl then do:
        find first uom where uom.uom eq oe-ordl.pr-uom no-lock no-error.
        v-qty-uom = if avail uom then substr(uom.dscr,1,8)
                    else oe-ordl.pr-uom.
      end.

      LEAVE.
    END.

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

  for each oe-boll
      where oe-boll.company eq cocode
        and oe-boll.b-no    eq oe-bolh.b-no
        and oe-boll.qty     ne 0:
        
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.po-no
     report.key-02   = oe-boll.i-no
     report.key-03   = string(oe-boll.ord-no,"9999999999")
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
    v-page-tot = v-page-tot / 22.
    {sys/inc/roundup.i v-page-tot}

    v-printlines = 1.

    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
        first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-boll.i-no
        no-lock

        break by report.key-01
              by report.key-02
              by report.key-03:

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-boll.ord-no
            and oe-ordl.i-no    eq oe-boll.i-no
            and oe-ordl.line    eq oe-boll.line
          no-lock no-error.
          
      v-tot-sqft = v-tot-sqft + (itemfg.t-sqft * oe-boll.qty).
      
      if v-printlines gt 23  then do:
          
        page.
        v-printlines = 1.
      end.

      if avail oe-ordl then
      find first oe-ord
          where oe-ord.company eq cocode
            and oe-ord.ord-no  eq oe-ordl.ord-no
          no-lock no-error.

      v-line-tot = v-line-tot + oe-boll.qty.
      
      find first w-cas where w-qty-case eq oe-boll.qty-case no-error.
      if not avail w-cas then create w-cas.
      assign
       w-qty-case = oe-boll.qty-case
       w-cases    = w-cases + oe-boll.cases.
      
      find first w-cas where w-qty-case eq oe-boll.partial no-error.
      if not avail w-cas then create w-cas.
      assign
       w-qty-case = oe-boll.partial
       w-cases    = w-cases + 1.
      
      if last-of(report.key-02) then do:
        assign
         i        = 0
         j        = 0
         v-job-no = if oe-ordl.job-no eq "" then ""
                    else (trim(oe-ordl.job-no) + "-" +
                          string(oe-ordl.job-no2,"99")).
                          
        for each w-cas break by w-cases * w-qty-case desc:
          j = j + 1.
          
          if w-cases * w-qty-case eq 0 then do:
            if j gt 2 then delete w-cas.
            j = j - 1.
          end.
        end.
                         
        for each w-cas break by w-cases * w-qty-case desc:
          i = i + 1.
        
          display oe-boll.i-no          when i eq 1
                    fill(" ",12 -
                         length(trim(string(oe-boll.ord-no,">>>>>>>>")))) +
                    trim(string(oe-boll.ord-no,">>>>>>>>"))
                                        when i eq 2
                                        @ oe-boll.i-no
                  report.key-01         when i eq 1
                                        @ oe-rel.po-no
                    v-job-no            when i eq 2
                                        @ oe-rel.po-no
                  itemfg.i-name         when i eq 1
                    itemfg.part-dscr1   when i eq 2
                                        @ itemfg.i-name
                  w-cases               when w-cases * w-qty-case ne 0
                                        @ oe-boll.cases
                  w-qty-case            when w-cases * w-qty-case ne 0
                                        @ oe-boll.qty-case
                  v-line-tot            when i eq j  
             
              with frame ln-s.

          down with frame ln-s.
          
          v-printlines = v-printlines + 1.
        
          delete w-cas.
        end.
        
        put skip(1).
        
        v-printlines = v-printlines + 1.
        
        v-line-tot = 0.
      end.

      v-po-tot = v-po-tot + oe-boll.qty.
      
      if last-of(report.key-01) then do:
       /* if report.key-01 ne "" then do:
          display "         P.O.#:" @ oe-boll.i-no
                  report.key-01     @ oe-rel.po-no
                  "Total"           @ itemfg.i-name
                  v-po-tot          @ v-line-tot
              with frame ln-s.
          down with frame ln-s.
          
          put skip(1).
          
          v-printlines = v-printlines + 2.
        end.   */
        
        v-po-tot = 0.
      end.  

      delete report.
    end. /* for each oe-boll */

    v-shiplines = 0.
    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then v-shiplines = v-shiplines + 1.
    end.

    if (v-shiplines + v-printlines) gt 23 /*20 */ then do:
        
      page {1}.
      v-printlines = 1.
    end.

    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then put {1} oe-bolh.ship-i[i] at 11 skip.
    end.
    v-printlines = v-printlines + v-shiplines.
            /*23*/
    put skip(25 - v-printlines)
        "TOTAL UNITS"                               at 1
        trim(string(v-tot-pkgs))                    to 20
        "TOTAL PALLETS"                             at 22
        trim(string(v-tot-pal))                     to 43
        "WEIGHT"                                    at 45
        trim(string(v-tot-wt))                      to 59
        "TOTAL SQFT"                                at 61
        trim(string(v-tot-sqft,">>>>>>>>>"))        to 79
        skip(3)
        "RECEIVED BY:__________________________"    at 1 
        "PALLETS EXCHANGED:____________________"    to 80
        skip.

    assign
     v-tot-pkgs  = 0
     v-tot-pal   = 0
     v-tot-wt    = 0
     v-tot-sqft  = 0
     v-page-tot  = 0
     v-last-page = page-number.
  end.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
