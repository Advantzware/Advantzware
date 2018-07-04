/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/test/asi/oe/re
**       By: Chris Heins, 08.08.95
** Descript: Bill of Lading Print - Standard format
**
*****************************************************************************
\***************************************************************************/

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll FOR oe-boll.
def buffer tmp-oe-boll FOR oe-boll.
def buffer tmp-oe-bolh FOR oe-bolh.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format "->>>>9" no-undo.
def var v-coname LIKE coname no-undo.
def var v-printlines as int no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-tot-wgt    like oe-bolh.tot-wt FORMAT "->,>>>".
def var v-tot-cwt    like oe-bolh.cwt.

DEF VAR li AS INT NO-UNDO.

find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

FORM
  oe-ordl.qty                   format "->>>>>>>>"
  tt-boll.qty                   format "->>>>>>>>"
  space(0)
  tt-boll.cases                 format "->>>"
  space(0)
  tt-boll.qty-case              format ">>>>>>"
  space(2)
  tt-boll.po-no
  space(0) "/" space(0)
  itemfg.i-no                   format "x(20)"
  space(2)
  tt-boll.ord-no
  space(2)
  oe-ord.frt-pay
  tt-boll.weight                format "->>>>"                  skip
  with frame ln-1 down no-box no-labels stream-io width 90.

FORM
  oe-ordl.qty                   format "->>>>>>>>"
  tt-boll.qty                   format "->>>>>>>>"
  space(0)
  tt-boll.cases                 format "->>>"
  space(0)
  tt-boll.qty-case              format ">>>>>>"
  itemfg.i-name                                     at 28       skip
  with frame ln-2 down no-box no-labels stream-io width 90.

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id
    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    FOR EACH tt-boll:
      DELETE tt-boll.
    END.

    find first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-bolh.cust-no
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    find first carrier
        where carrier.company eq oe-bolh.company
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.
    v-carrier = if avail carrier then carrier.dscr else "".

    v-frt-pay-dscr = "".
    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,

        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

	  CASE oe-ord.frt-pay:
        WHEN "P" THEN v-frt-pay-dscr = "PREPAID".
        WHEN "C" THEN v-frt-pay-dscr = "COLLECT".
        WHEN "B" THEN v-frt-pay-dscr = "PPD/CHG".
        WHEN "T" THEN v-frt-pay-dscr = "3rdPARTY".
      END CASE.
	
      LEAVE.
    END.
    v-time = string(time,"hh:mm am").

    assign
     v-tot-pkgs = 0
     v-tot-wgt  = 0
     v-tot-cwt  = 0.

    for each xoe-boll
        where xoe-boll.company eq cocode
          and xoe-boll.bol-no  eq oe-bolh.bol-no 
        no-lock:
          
      assign
       v-tot-pkgs = v-tot-pkgs + xoe-boll.cases
       v-tot-wgt  = v-tot-wgt  + xoe-boll.weight
       v-tot-cwt  = v-tot-cwt  + xoe-boll.qty.
         
      if xoe-boll.partial gt 0 then v-tot-pkgs = v-tot-pkgs + 1.
    end.
      
    v-tot-cwt = v-tot-wgt / (v-tot-cwt / 100).

    if oe-ctrl.pr-broker and avail cust and shipto.broker then
      v-coname = cust.name.
    else
    do:
      find first company where company.company eq cocode no-lock no-error.
      if avail company then v-coname = company.name.
    end.

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

    if v-print-hdgs then do:
      format header
             skip(1)
             "Date:" oe-bolh.bol-date "/" v-time
             " Company:" v-coname format "x(25)"
             " BOL #:" oe-bolh.bol-no skip(2)
             "Carrier:" v-carrier "Trailer:" at 52
             oe-bolh.trailer
             skip(1)
             "SHIP TO:" at 12
             v-ship-name  at 12
             v-ship-addr[1] at 12
             v-ship-addr[2] at 12
             v-ship-city at 12
             v-ship-state
             v-ship-zip
             skip(2)
             v-frt-pay-dscr at 72 skip
             "# Pkgs:" v-tot-pkgs
/*
                 " Weight:" oe-bolh.tot-wt
*/
             " Weight:" v-tot-wgt
             " Cwt:" v-tot-cwt
             " Frt Pay:" oe-ord.frt-pay skip
             oe-bolh.tot-pallets FORMAT "->,>>9" AT 16
             " Total Pallets"
             skip(1)
             "Shipping Instructions:" at 16 skip
             oe-bolh.ship-i[1] at 16 format "x(60)" skip
             oe-bolh.ship-i[2] at 16 format "x(60)" skip
             oe-bolh.ship-i[3] at 16 format "x(60)" skip
             oe-bolh.ship-i[4] at 16 format "x(60)"
             skip(4)
             "  Qty Ord   Qty Ship Cs Pcs/Cs      "
             "P.O. No. / Item / Description  "
             "Order# P/C  Wt." skip
             "--------- ---------- -- ------"
             "-------------------------------------"
             "------ --- ----" skip
          with frame hd-top-comp no-box no-labels page-top stream-io width 90.
      view frame hd-top-comp.
    end.

    else do:
      format header
             skip(3)
             v-time at 4
             oe-bolh.bol-date at 16
             v-coname at 29 format "x(20)"
             oe-bolh.bol-no at 71
             skip(4)
             v-carrier at 8
             oe-bolh.trailer at 69
             skip(1)
             v-ship-name  at 12
             v-ship-addr[1] at 12
             v-ship-addr[2] at 12
             v-ship-city at 12
             v-ship-state
             v-ship-zip
             skip(4)
             v-frt-pay-dscr at 72
             v-tot-pkgs at 6
/*
                 oe-bolh.tot-wt at 55
*/
             v-tot-wgt at 55
             v-tot-cwt to 67
             oe-ord.frt-pay skip
             oe-bolh.tot-pallets FORMAT "->,>>9" at 16 " Total Pallets"
             skip(1)
             oe-bolh.ship-i[1] at 16 format "x(60)" skip
             oe-bolh.ship-i[2] at 16 format "x(60)" skip
             oe-bolh.ship-i[3] at 16 format "x(60)" skip
             oe-bolh.ship-i[4] at 16 format "x(60)"
             skip(7)
          with frame hd-top no-box no-labels page-top stream-io width 90.
      view frame hd-top.
    end.

    page.
  end. /* first-of(oe-bolh.bol-no) */

  FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
      RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).

    IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
      RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).

    oe-boll.printed = YES.
  END.

  IF LAST-OF(oe-bolh.bol-no) THEN DO:
    ASSIGN
     v-printlines = 0
     li           = 0.

    FOR EACH tt-boll
        BREAK BY tt-boll.i-no
              BY tt-boll.po-no
              BY tt-boll.ord-no
              BY tt-boll.line
              BY tt-boll.cases DESC:

      li = li + 1.

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq tt-boll.ord-no
            and oe-ordl.i-no    eq tt-boll.i-no
            and oe-ordl.line    eq tt-boll.line
          no-lock no-error.

      if v-printlines ge 27 then do:
        page.
        v-printlines = 0.
      end.

      if avail oe-ordl then
      find first oe-ord
          where oe-ord.company eq oe-ordl.company
            and oe-ord.ord-no  eq oe-ordl.ord-no
          no-lock no-error.

      find first itemfg
          where itemfg.company eq tt-boll.company
            and itemfg.i-no    eq tt-boll.i-no
          no-lock no-error.

      IF li EQ 1 THEN DO WITH FRAME ln-1:
        FOR EACH b-tt-boll
            WHERE b-tt-boll.i-no   EQ tt-boll.i-no
              AND b-tt-boll.po-no  EQ tt-boll.po-no
              AND b-tt-boll.ord-no EQ tt-boll.ord-no
              AND b-tt-boll.line   EQ tt-boll.line
              AND ROWID(b-tt-boll) NE ROWID(tt-boll):
          ASSIGN
           tt-boll.qty    = tt-boll.qty + b-tt-boll.qty
           tt-boll.weight = tt-boll.weight + b-tt-boll.weight.
        END.

        DISPLAY oe-ordl.qty
                tt-boll.qty
                tt-boll.cases
                tt-boll.qty-case
                tt-boll.po-no 
                itemfg.i-no WHEN AVAIL itemfg
                tt-boll.ord-no
                oe-ord.frt-pay WHEN AVAIL oe-ord
                tt-boll.weight.
        DOWN.
      END.

      IF li GE 2 OR (LAST-OF(tt-boll.line) AND li EQ 1) THEN DO WITH FRAME ln-2:
        IF li NE 1 THEN
          DISPLAY tt-boll.cases
                  tt-boll.qty-case.

        IF li LE 2 AND AVAIL itemfg THEN
          DISPLAY itemfg.i-name.

        DOWN.
      END.

      v-printlines = v-printlines + 1.

      IF LAST-OF(tt-boll.line) THEN DO:
        PUT SKIP(1).
        ASSIGN
         v-printlines = v-printlines + 1
         li           = 0.
      END.
    END. /* for each tt-boll */
  END.

  oe-bolh.printed = YES.
END. /* for each oe-bolh */

RETURN.

PROCEDURE create-tt-boll.
  DEF INPUT PARAM ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-boll.cases NO-UNDO.


  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-boll
      WHERE tt-boll.i-no     EQ oe-boll.i-no
        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.line
        AND tt-boll.qty-case EQ ip-qty-case
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-boll THEN DO:
    CREATE tt-boll.
    BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
    ASSIGN
     tt-boll.qty-case = ip-qty-case
     tt-boll.cases    = 0
     tt-boll.qty      = 0
     tt-boll.weight   = 0
     tt-boll.partial  = 0.
  END.

  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
   tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight).

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.
