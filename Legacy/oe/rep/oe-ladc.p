/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/test/asi/oe/rep/oe-ladc.p
**       By: Chris Heins, 08.08.95
** Descript: Bill of Lading Print - Carton Craft Format
**
*****************************************************************************
\***************************************************************************/

{sys/inc/var.i SHARED}
{sys/form/r-top.i}

DEF BUFFER xoe-boll FOR oe-boll.

{oe/rep/oe-lad.i}

DEF VAR v-time AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR v-carrier LIKE carrier.dscr NO-UNDO.
DEF VAR v-frt-pay-dscr AS CHAR NO-UNDO.
DEF VAR v-tot-pkgs AS INT FORMAT ">>>>>9" NO-UNDO.
def var v-tot-wgt    like oe-bolh.tot-wt.
def var v-tot-cwt    like oe-bolh.cwt.

def var v-bolh-ship-i as char format "x(30)" no-undo.
def var v-scnt as int no-undo.
def var v-send as int no-undo.

FIND FIRST company WHERE
  company.company = cocode NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE
  oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FORM
  SKIP(3)
  v-time AT 4 oe-bolh.bol-date AT 16 coname AT 29 FORMAT "x(20)"
  oe-bolh.bol-no AT 71 SKIP(4)
  v-carrier AT 8 oe-bolh.trailer AT 69 SKIP(1)
  shipto.ship-name  AT 12
  shipto.ship-addr[1] AT 12
  shipto.ship-addr[2] AT 12
  shipto.ship-city AT 12 shipto.ship-state  shipto.ship-zip SKIP(3)
    "No. Cases" at 6 " Kind of Packages " at 19 "Weight" at 55 "Rate P/C" at 63
    "---------" at 6 "------------------" at 19 "------" at 55 "--------" at 63
        skip
  v-tot-pkgs AT 6
    oe-bolh.tot-pallets AT 16 " Total Pallets"
        v-tot-wgt AT 55 v-tot-cwt TO 67
            oe-ord.frt-pay
                v-frt-pay-dscr to 80 skip(1)
  WITH FRAME hd-top NO-BOX NO-LABELS stream-io width 90.

  def var v-desc as char no-undo format 'x(28)'.

FORM
  oe-ordl.qty FORMAT ">>>>>>>"
  space(1) oe-boll.qty  FORMAT ">>>>>>>"
  space(6) oe-boll.cases FORMAT ">>>>"
  space(4) oe-boll.qty-case FORMAT ">>>>>>"
  space(2) v-desc
  space(1) oe-boll.ord-no
  space(1) oe-ord.frt-pay
  space(0) oe-boll.weight
  WITH FRAME ln-s DOWN NO-BOX NO-LABELS stream-io width 90.

FORM
  v-bolh-ship-i AT 16 FORMAT "x(37)" SKIP
  WITH FRAME rel-text NO-BOX NO-LABELS stream-io width 90.


DO:     /* production mode */

  for each report   where report.term-id eq v-term-id,
      first oe-bolh where recid(oe-bolh) eq report.rec-id
      break by oe-bolh.bol-no:

    IF FIRST-OF(oe-bolh.bol-no) THEN
    DO:
      FIND FIRST cust WHERE cust.company = cocode AND
        cust.cust-no = oe-bolh.cust-no NO-LOCK NO-ERROR.

      FIND FIRST shipto WHERE shipto.company = cocode AND
        shipto.cust-no = oe-bolh.cust-no AND
        shipto.ship-id = oe-bolh.ship-id
        USE-INDEX ship-id NO-LOCK NO-ERROR.

      FIND carrier WHERE carrier.company = oe-bolh.company AND
        carrier.carrier = oe-bolh.carrier NO-LOCK NO-ERROR.
      IF AVAILABLE carrier THEN
      ASSIGN v-carrier = carrier.dscr.
      ELSE
      ASSIGN v-carrier = "".

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

      ASSIGN v-time = STRING(TIME,"HH:MM AM").
      
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

      IF AVAILABLE(cust) THEN
      DO:
        DISPLAY
          oe-bolh.bol-no v-time oe-bolh.bol-date
          cust.name @ coname
          v-carrier oe-bolh.trailer
          shipto.ship-name    WHEN AVAILABLE shipto
          shipto.ship-addr[1] WHEN AVAILABLE shipto
          shipto.ship-addr[2] WHEN AVAILABLE shipto
          shipto.ship-city    WHEN AVAILABLE shipto
          shipto.ship-state   WHEN AVAILABLE shipto
          shipto.ship-zip     WHEN AVAILABLE shipto
          v-frt-pay-dscr v-tot-pkgs v-tot-wgt v-tot-cwt
          oe-ord.frt-pay oe-bolh.tot-pallets
          WITH FRAME hd-top.
      END.
      ELSE
      DO:
        DISPLAY
          oe-bolh.bol-no v-time oe-bolh.bol-date
          "Unknown Customer" @ coname
          v-carrier oe-bolh.trailer
          shipto.ship-name    WHEN AVAILABLE shipto
          shipto.ship-addr[1] WHEN AVAILABLE shipto
          shipto.ship-addr[2] WHEN AVAILABLE shipto
          shipto.ship-city    WHEN AVAILABLE shipto
          shipto.ship-state   WHEN AVAILABLE shipto
          shipto.ship-zip     WHEN AVAILABLE shipto
          v-frt-pay-dscr v-tot-pkgs v-tot-wgt v-tot-cwt
          oe-ord.frt-pay oe-bolh.tot-pallets
          WITH FRAME hd-top.
      END. /* pr-broker */
    END. /* first-of(oe-bolh.bol-no) */

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
      FIND FIRST oe-ordl WHERE oe-ordl.company = cocode AND
        oe-ordl.ord-no = oe-boll.ord-no AND
        oe-ordl.i-no   = oe-boll.i-no   AND
        oe-ordl.line   = oe-boll.line  NO-LOCK NO-ERROR.

      IF AVAILABLE oe-ordl THEN
      FIND FIRST oe-ord WHERE oe-ord.company = oe-ordl.company AND
        oe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR.
      FIND itemfg WHERE itemfg.company = oe-boll.company AND
        itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
      v-desc = oe-bolh.po-no + "/" + oe-boll.i-no.

      if line-counter > page-size - 7 then
      do:
      page.
      IF AVAILABLE(cust) THEN
      DO:
        DISPLAY
          oe-bolh.bol-no v-time oe-bolh.bol-date
          cust.name @ coname
          v-carrier oe-bolh.trailer
          shipto.ship-name    WHEN AVAILABLE shipto
          shipto.ship-addr[1] WHEN AVAILABLE shipto
          shipto.ship-addr[2] WHEN AVAILABLE shipto
          shipto.ship-city    WHEN AVAILABLE shipto
          shipto.ship-state   WHEN AVAILABLE shipto
          shipto.ship-zip     WHEN AVAILABLE shipto
          v-frt-pay-dscr v-tot-pkgs v-tot-wgt v-tot-cwt
          oe-ord.frt-pay oe-bolh.tot-pallets
          WITH FRAME hd-top.
      END.
      ELSE
      DO:
        DISPLAY
          oe-bolh.bol-no v-time oe-bolh.bol-date
          "Unknown Customer" @ coname
          v-carrier oe-bolh.trailer
          shipto.ship-name    WHEN AVAILABLE shipto
          shipto.ship-addr[1] WHEN AVAILABLE shipto
          shipto.ship-addr[2] WHEN AVAILABLE shipto
          shipto.ship-city    WHEN AVAILABLE shipto
          shipto.ship-state   WHEN AVAILABLE shipto
          shipto.ship-zip     WHEN AVAILABLE shipto
          v-frt-pay-dscr v-tot-pkgs v-tot-wgt v-tot-cwt
          oe-ord.frt-pay oe-bolh.tot-pallets
          WITH FRAME hd-top.
      END. /* pr-broker */
      end.                 /* line counter more than page-size */

      IF oe-boll.cases GT 0 THEN
      DO:
        DISPLAY
          oe-ordl.qty oe-boll.qty oe-boll.cases oe-boll.qty-case
          v-desc
          oe-boll.ord-no oe-ord.frt-pay WHEN AVAILABLE oe-ord
          oe-boll.weight
          WITH FRAME ln-s.
      END.
      /** PRINT PARTIAL SHIPMENT LINE **/
      IF oe-boll.partial GT 0 THEN
      DO:
        PUT SKIP.
        DISPLAY
          1 @ oe-boll.cases
          oe-boll.partial @ oe-boll.qty-case
          itemfg.i-no  WHEN AVAILABLE itemfg @ v-desc
          WITH FRAME ln-s.
      END.
      IF AVAIL(itemfg) THEN do:
        if itemfg.i-name > ' ' then do:
            down 1 with frame ln-s.
            display itemfg.i-name @ v-desc
            with frame ln-s.
        end.
        if itemfg.part-dscr1 > ' ' then do:
            down 1 with frame ln-s.
            display itemfg.part-dscr1 @ v-desc
            with frame ln-s.
        end.
      end.
                      /* variable number of instruction lines to print */
      v-send = 4.
      do v-scnt = 4 to 1 by -1:
        if oe-bolh.ship-i[v-scnt] <= " " then
          v-send = v-scnt - 1.
        else
          leave.
      end.
      do v-scnt = 1 to v-send:
        display
          oe-bolh.ship-i[v-scnt] @ v-bolh-ship-i
          with frame rel-text.
      end.

      PUT SKIP(1).
      ASSIGN oe-boll.printed = TRUE.
    END. /* for each oe-boll */
    ASSIGN oe-bolh.printed = TRUE.
    PAGE.
  END. /* for each oe-bolh */
END.    /* production mode */
