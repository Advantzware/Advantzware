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
def var v-tot-pkgs as int format ">>>>>9" no-undo.
def var v-coname LIKE coname no-undo.
def var v-printlines as int no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-tot-wgt    like oe-bolh.tot-wt.
def var v-tot-cwt    like oe-bolh.cwt.

find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

FORM
  oe-ordl.qty format ">>>>>>>"
  space(1) oe-boll.qty  format ">>>>>>>"
  space(0) oe-boll.cases format ">>>>"
  space(0) oe-boll.qty-case format ">>>>>>"
  space(2) oe-boll.po-no space(0) "/" space(0) itemfg.i-no format "x(20)"
  space(2) oe-boll.ord-no
  space(2) oe-ord.frt-pay
  space(1) oe-boll.weight skip
  itemfg.i-name at 28 skip
  with frame ln-s down no-box no-labels stream-io width 90.

if v-print-mode eq "align" then do:
                   /*
  find printer where recid(printer) eq save_id no-lock.

  if printer.pr-port begins "Print" or
    printer.pr-port begins "Com" or
    printer.pr-port begins "Lp" then

  /************************   PRINT ALIGNMENT TEST  *************************/
  if not(v-print-hdgs) then /* Dont Print bill-of-lading headers */
  repeat:
    def var v-ans as log no-undo.
    assign v-ans = NO.
    update skip space(4) v-ans format "Yes/No"
      label "Do You Want To Print Alignment Test?" space(3) skip
      with ROW 20 SIDE-labels centered frame align no-box OVERLAY
      COLOR VALUE("white/red") PROMPT-FOR VALUE("yellow/red").
    if v-ans then
    do:
      if opsys eq "UNIX" then
      output thru value(printer.pr-port) page-size
        /* CTS 66 */ 62 /* CTS end */ .
      else /* if opsys eq "MSDOS" then */
      output to VALUE(printer.pr-port) PAGE-SIZE
        /* CTS 66 */ 62 /* CTS end */ .
      display
        "99999999" @ oe-bolh.bol-no
        "99:99 XX" @ v-time
        "99/99/99" @ oe-bolh.bol-date
        "XXXXXXXXXXXXXXXXXXX" @ v-coname
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" @ v-carrier
        "XXXXXXXX" @ oe-bolh.trailer
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" @ shipto.ship-name
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" @ shipto.ship-addr[1]
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" @ shipto.ship-addr[2]
        "XXXXXXXXXXXXXXX" @ shipto.ship-city
        "XX" @ shipto.ship-state  "99999-9999" @ shipto.ship-zip
        "XXXXXXXX" @ v-frt-pay-dscr
        "999999" @ v-tot-pkgs
        "99999" @ oe-bolh.tot-wt
        "999.99" @ oe-bolh.cwt
        "X" @ oe-ord.frt-pay
        "9999" @ oe-bolh.tot-pallets
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" @ oe-bolh.ship-i[1]
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" @ oe-bolh.ship-i[2]
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" @ oe-bolh.ship-i[3]
        "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" @ oe-bolh.ship-i[4]
        with frame hd-top no-box no-labels stream-io width 90.

      do i = 1 to 12:
        display
          "9999999" @ oe-ordl.qty
          "9999999" @ oe-boll.qty
          "999" @ oe-boll.cases
          "99999" @ oe-boll.qty-case
          "XXXXXXXX" @ oe-bolh.po-no
          "XXXXXXXXXXXXXXXXXXX" @ itemfg.i-no
          "999999" @ oe-boll.ord-no
          "X" @ oe-ord.frt-pay
          "99999" @ oe-boll.weight
          with frame ln-s down no-box no-labels stream-io width 90.
        put skip(1).
      end. /* 1 to 25 */
      /* put control v-end-compress.   /** RESET PRINTER PITCH **/ */
      output close.
      output to terminal.
    end. /* v-ans */
    else
    leave.
  end. /* repeat */
  hide frame align no-pause.
  return.

  */
end.    /* alignment mod */
else
do:     /* production mode */

  for each report   where report.term-id eq v-term-id,
      first oe-bolh where recid(oe-bolh) eq report.rec-id
      break by oe-bolh.bol-no:

    if first-of(oe-bolh.bol-no) then do:
      find first cust where cust.company eq cocode and
        cust.cust-no eq oe-bolh.cust-no no-lock no-error.

      find first shipto where shipto.company eq cocode and
        shipto.cust-no eq oe-bolh.cust-no and
        shipto.ship-id eq oe-bolh.ship-id
        use-index ship-id no-lock no-error.

      find carrier where carrier.company eq oe-bolh.company and
        carrier.carrier eq oe-bolh.carrier no-lock no-error.
      if avail carrier then
      assign v-carrier = carrier.dscr.
      else
      assign v-carrier = "".

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
      assign v-time = string(time,"hh:mm am").

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

      if first-of(oe-bolh.bol-no) then do:

        if oe-ctrl.pr-broker and avail cust and shipto.broker then
          v-coname = cust.name.
        else
        do:
          find first company where company.company eq cocode no-lock no-error.
          if avail company then
            v-coname = company.name.
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
                 oe-bolh.tot-pallets at 16
                 " Total Pallets"
                 skip(1)
                 "Shipping Instructions:" at 16 skip
                 oe-bolh.ship-i[1] at 16 format "x(60)" skip
                 oe-bolh.ship-i[2] at 16 format "x(60)" skip
                 oe-bolh.ship-i[3] at 16 format "x(60)" skip
                 oe-bolh.ship-i[4] at 16 format "x(60)"
                 skip(4)
                 "Qty Ord Qty Ship Cs Pcs/Cs      "
                 "P.O. No. / Item / Description  "
                 "Order# P/C  Wt." skip
                 "------- -------- -- ------"
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
                 oe-bolh.tot-pallets at 16 " Total Pallets"
                 skip(1)
                 oe-bolh.ship-i[1] at 16 format "x(60)" skip
                 oe-bolh.ship-i[2] at 16 format "x(60)" skip
                 oe-bolh.ship-i[3] at 16 format "x(60)" skip
                 oe-bolh.ship-i[4] at 16 format "x(60)"
                 skip(7)
              with frame hd-top no-box no-labels page-top stream-io width 90.
          view frame hd-top.
        end.
      end.

      page.
    end. /* first-of(oe-bolh.bol-no) */

    v-printlines = 0.
    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
      find first oe-ordl where oe-ordl.company eq cocode and
        oe-ordl.ord-no eq oe-boll.ord-no and
        oe-ordl.i-no   eq oe-boll.i-no   and
        oe-ordl.line   eq oe-boll.line  no-lock no-error.

      if v-printlines ge 30 then do:
        page.
        v-printlines = 0.
      end.

      if avail oe-ordl then
        find first oe-ord where oe-ord.company eq oe-ordl.company and
          oe-ord.ord-no eq oe-ordl.ord-no no-lock no-error.
      find itemfg where itemfg.company eq oe-boll.company and
        itemfg.i-no eq oe-boll.i-no no-lock no-error.
      release oe-rel.
      /*  old
      find first oe-rell where oe-rell.company eq oe-boll.company and
                               oe-rell.r-no    eq oe-boll.r-no and
                               oe-rell.i-no    eq oe-boll.i-no and
                               oe-rell.line    eq oe-boll.line
                               USE-INDEX r-no no-lock no-error.

      if avail oe-rell then do:
        find first oe-relh of oe-rell no-lock.
        find first oe-rel where oe-rel.company eq oe-relh.company   and
                                oe-rel.ord-no  eq oe-rell.ord-no    and
                                oe-rel.line    eq oe-rell.line      and
                                oe-rel.link-no eq oe-rell.r-no      and
                                oe-rel.ship-no eq oe-relh.ship-no   and
                                oe-rel.i-no    eq oe-rell.i-no
                                no-lock no-error.

        if not avail oe-rel then
          find first oe-rel where oe-rel.company  eq oe-relh.company   and
                                  oe-rel.ord-no   eq oe-rell.ord-no    and
                                  oe-rel.line     eq oe-rell.line      and
                                  oe-rel.rel-date eq oe-relh.rel-date  and
                                  oe-rel.ship-no  eq oe-relh.ship-no   and
                                  oe-rel.i-no     eq oe-rell.i-no
                                  no-lock no-error.
      end.
      */  
      display oe-ordl.qty
              oe-boll.qty
              oe-boll.cases
                1 when oe-boll.cases eq 0 @ oe-boll.cases
              oe-boll.qty-case
                oe-boll.partial when oe-boll.cases eq 0 @ oe-boll.qty-case
              /*oe-bolh.po-no
              oe-rel.po-no
                when avail oe-rel and oe-rel.po-no ne "" @ oe-bolh.po-no */
              oe-boll.po-no 
              itemfg.i-no
                when avail itemfg
              oe-boll.ord-no
              oe-ord.frt-pay
                when avail oe-ord
              oe-boll.weight
              itemfg.i-name when avail itemfg
          with frame ln-s.

      v-printlines = v-printlines + 2.
      
      /** PRINT PARTIAL SHIPMENT LINE **/
      if oe-boll.cases gt 0 and oe-boll.partial gt 0 then do:
        put skip.
        clear frame ln-s.
        display
          "" @ oe-ordl.qty
          "" @ oe-boll.qty
          1 @ oe-boll.cases
          oe-boll.partial @ oe-boll.qty-case
          itemfg.i-no when avail itemfg
          with frame ln-s.
        v-printlines = v-printlines + 1.
      end.
      
      if v-print-hdgs and avail itemfg then
        put space(30) itemfg.i-name skip
            space(30) itemfg.part-dscr1 skip.
      put skip(1).
      v-printlines = v-printlines + 3.
      oe-boll.printed = yes.
    end. /* for each oe-boll */
    oe-bolh.printed = yes.
  end. /* for each oe-bolh */
end.    /* production mode */

