/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\ASI\i850.P
**       By: Chris Heins (c) 1997 Report Concepts, Inc.
** Descript: Interface PO's to ASI Order Processing
05.20.99 by CAH on \\ricky\robj8 Log#0000:
1.  Display of shipto corrected to use side-labels.
03.18.99 by CAH on \\ricky\asi\patch Log#0000:
1.  Moved where purchase order comments are stored.   Previously they were 
put into oe-ord.bill-i[1 TO 4] from edpoaddon with order-line = 0.  Now they
are in oe-ordl.ship-i[1 TO 4].  First we look for an edpoaddon where the line 
number = edpoline.line.  If not found we fallback to one with order-line = 0.
Note that the latter is presently the only comments supporded by the 850 
import programs, that is, line item level comments are not being created.
2.  When printing order header fields, program now checks for edpoaddon 
with order-line = 0 and if found then it displays all 9 lines.
3.  Per Jack Lechner the order line notes are only populated on the first 
line item otherwise they would make the BOL print too long.
4.  Added line item level notes, where edpoaddon.order-line = integer(edpoline.cust-po-line).
Note that if every line item has notes, and there are header notes, the header
notes will not be added to a line item.  The only reference to them will be
in the header portion of the edit list.
10.06.98 by CAH on \\ricky\rv8 Log#0000:
1.  Updated for Initial Sterling TDF release, added bill-i from NTE segs.
08.21.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added code to set the edpoline.unit-price to be the itemfg.sell-price when
it is blank or unknown.  Warning message is printed. 
2.  Added code to fallback from edshipto.cust to edpotran.cust to edmast.cust
when blank fields are encountered.
05.16.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added incrementation of itemfg.q-alloc field.
02.02.10.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Commented out the logic to determine packing from f-bin; program 
will use fg-item instead, per Jack Lechner.
2.  Added default of request-date when not specified as today + 1,
per Jack Lechner.
10.22.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Program was only processing match on ws_partner, which was not being
    set by caller.  This program should be partner independent, it updates
    and PO's which have not yet been updated.  Qualification of ws_partner
    removed.
10.16.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Restored the 06.10.97 change which somehow was lost ...
2.  Changed the stream name on that change to s-out from s-err.
06.10.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Added check for selling price vs. itemfg.
05.12.97 by CAH on ricky@812<rprodev> Log#0000:
1.  Added creation of item-no from prefix + cust-item-no + suffix.
05.05.97 by CAH on ricky@812<rprodev> Log#0000:
1.  Added lookup of item-no using itemfg.part-no.
2.  Corrected oe-ord.last-date = today + cust.ship-day.
3.  Added qty uom conversions inbound for C,M,CS,CT,CWT,LB
4.  Added weight calcs using itemfg.weight-100.
04.25.97 by CAH on ricky@812<rprodev> Log#0000:
1.  Inbound uom-code of "CT" is coerced to "CS" in asi, as this is specificcally
tested for in various places in the asi system for conversion purposes.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
DEF SHARED STREAM s-out.
DEF SHARED FRAME hdg-std.
DEF BUFFER dc FOR edshipto.
{rc/hdg-wide.i "ed/asi/i850.p" "PURCHASE ORDER EDIT LIST" "(s-out)"}
{rc/stats.i}
{rc/fcurrent.i "row 3"}
{rc/stringv.i}
{rc/ercvars.i 50 " " NEW}
DEF VAR i AS INT NO-UNDO.
DEF VAR save_erclist LIKE erclist NO-UNDO.
DEF VAR pass AS INT NO-UNDO.
DEF VAR stat_msg AS CHAR NO-UNDO FORMAT 'x(20)'.
DEF VAR by_list AS CHAR NO-UNDO INITIAL "001,010,015,038,063,410".
DEF VAR on_list AS CHAR NO-UNDO INITIAL "002,007,037,064,077,018".
DEF VAR mh_list AS CHAR NO-UNDO INITIAL "".
def var conv_fact as decimal no-undo format "-9999999999.9999999999".
{rc/hdg-noco.i}
VIEW STREAM s-out FRAME hdg-std.
start = TIME.
VIEW FRAME f-current.
VIEW FRAME f-stats.
FOR EACH eddoc
    WHERE eddoc.fgid = "PO"
    /* AND eddoc.partner BEGINS ws_partner */
    AND eddoc.posted = FALSE:
  {rc/incr.i ws_recs_read}.
  ws_partner = eddoc.partner.
  page stream s-out.
  DISPLAY
    eddoc.partner
    eddoc.docid
    eddoc.error-count format ">>9"
    WITH FRAME f-current.
  DISPLAY STREAM s-out
    eddoc.partner
    eddoc.seq
    eddoc.setid
    eddoc.fgid
    eddoc.docid
    eddoc.error-count format ">>9"
    WITH FRAME f-doc WIDTH 132 SIDE-LABELS.
  _passes:
  REPEAT pass = 1 TO 2 ON ERROR UNDO, LEAVE: /* 1 = edit, 2 = post */
    ASSIGN erclist = "" save_erclist = "".
    IF pass = 1 THEN
    eddoc.error-count = 0.
    DISPLAY pass WITH FRAME f-current.
    FIND edco WHERE edco.company = ws_company NO-LOCK NO-ERROR.
    IF NOT AVAIL edco THEN
    DO:
      {rc/listadd.i erclist 101}
      erctoken[1] = ws_company.
    END.
    FIND edmast WHERE edmast.partner = eddoc.partner EXCLUSIVE NO-ERROR.
    IF NOT AVAIL edmast THEN
    DO:
      {rc/listadd.i erclist 201}
      erctoken[2] = eddoc.partner.
    END.
    FIND edpotran
      WHERE edpotran.partner = eddoc.partner
      AND edpotran.seq = eddoc.seq
      EXCLUSIVE.
    IF NOT AVAIL edpotran THEN
    DO:
      {rc/listadd.i erclist 1001}
      erctoken[6] = STRING(eddoc.seq).
    END.
    IF erclist > "" THEN
    LEAVE _passes.
    {rc/incr.i ws_recs_selected}.
    IF edpotran.by-code = "" AND edpotran.bt-code > ""
      THEN
    edpotran.by-code = edpotran.bt-code.
    IF edpotran.st-code = "" AND edpotran.bt-code > ""
      THEN
    edpotran.st-code = edpotran.bt-code.
    FIND edshipto
        WHERE edshipto.partner = edpotran.partner
        AND edshipto.ref-type = "BY"
        AND edshipto.by-code = edpotran.by-code NO-LOCK NO-ERROR.
      IF NOT AVAIL edshipto THEN
      DO:
        erctoken[4] = edpotran.by-code.
        {rc/listadd.i erclist 401}
      END.
      else do:
    /* 9904 CAH: Per Jack Lechner, Sears is not sending us the correct
        ST code in the PO's.   They are sending ST = BY even though
        the order SHOULD go through a DC.  This code forces the
        insertion of the ST code from edshipto, which is the only 
        place in the system where the routing guide appears */
    
    IF edpotran.st-code = "" AND edpotran.by-code > ""
      OR edpotran.st-code = edpotran.by-code 
      THEN
    DO:
        edpotran.st-code =
        IF edshipto.st-code > ""
          THEN
        edshipto.st-code
        ELSE
        edpotran.by-code.
    END.
      END.
    FIND dc
      WHERE dc.partner = edpotran.partner
      AND dc.ref-type = "ST"
      AND dc.by-code = edpotran.st-code NO-LOCK NO-ERROR.
    IF NOT AVAIL dc THEN
    FIND dc
      WHERE dc.partner = edpotran.partner
      AND dc.ref-type = "BY"
      AND dc.by-code = edpotran.st-code NO-LOCK NO-ERROR.
    IF NOT AVAIL dc THEN
    DO:
      erctoken[4] = edpotran.st-code.
      {rc/listadd.i erclist 403}
    END.
    IF edpotran.cust = "" AND AVAIL edshipto AND edshipto.cust > ""
      THEN
    ASSIGN edpotran.cust = edshipto.cust.
    ELSE
    ASSIGN edpotran.cust = edmast.cust.
    FIND oe-ctrl
      WHERE oe-ctrl.company = ws_company EXCLUSIVE-LOCK
      NO-ERROR.
    IF NOT AVAIL oe-ctrl THEN
    DO:
      erctoken[1] = ws_company.
      {rc/listadd.i erclist 101}.
    END.
    FIND FIRST asi.cust
      WHERE cust.company = ws_company
      AND   cust.cust-no = edmast.cust NO-LOCK NO-ERROR.
    IF NOT AVAIL asi.cust THEN
    DO:
      {rc/listadd.i erclist 107}
      erctoken[7] = edmast.cust.
    END.
    ELSE
    DO:
      FIND FIRST asi.terms WHERE terms.company = ws_company AND
        terms.t-code = cust.terms NO-LOCK NO-ERROR.
      IF NOT AVAIL terms THEN
      DO:
        erctoken[8] = cust.terms.
        {rc/listadd.i erclist 108}
      END.
      FIND FIRST asi.sman WHERE sman.company = ws_company AND
        sman.sman = cust.sman NO-LOCK NO-ERROR.
      IF NOT AVAIL sman THEN
      DO:
        {rc/listadd.i erclist 109}
        erctoken[9] = cust.sman.
      END.
    END.
    IF pass = 1 AND AVAIL edpotran
      THEN
    DO:
        /* 9801 CAH Per Jack Lechner */
      if edpotran.request-date = ?
      then edpotran.request-date = today + 1.
      DISPLAY STREAM s-out
        edpotran.purpose-code
        edpotran.order-type
        edpotran.lines
            skip
        edpotran.order-date
        edpotran.by-code
        edpotran.st-code
            skip
        edpotran.cust-dept
        edpotran.cust-div
        edpotran.promo-code
            skip
        edpotran.contract   format 'x(20)'
        edpotran.release-no format 'x(15)'
        edpotran.ship-stat
            skip
        edpotran.request-date
        edpotran.cancel-date
        edpotran.cancel-date-code
            skip
        edpotran.terms
        edpotran.ship-method
        edpotran.ship-pay-code
        WITH FRAME f-header WIDTH 132 3 COLUMNS.
      IF EDPOTRAN.SHIP-name > "" THEN
      DISPLAY stream s-out
        edpotran.ship-name SKIP
        edpotran.ship-address[1] SKIP
        edpotran.ship-address[2] SKIP
        edpotran.ship-address[3] SKIP
        edpotran.ship-city edpotran.ship-st
        edpotran.ship-zip
        edpotran.ship-country
        WITH FRAME f-shipto side-labels.
      IF edpotran.routing[1] > "" THEN
      DISPLAY STREAM s-out
        edpotran.routing WITH FRAME f-routing 1 COLUMN.
        
        /* 9903 CAH: Display header level comments */
        for each edpoaddon of edpotran
        where edpoaddon.order-line = 0 no-lock:
        if edpoaddon.note[1] > "" 
        then do:
            put stream s-out skip(1) "Notes -" at 30 skip.
            do i = 1 to 9:
                if edpoaddon.note[i] > "" then put stream s-out 
                skip i at 30 ") " edpoaddon.note[i] skip.
            end.    
        end.
        end.    /* for each */
    END. /* pass = 1 */
    {rc/accum.i eddoc.error-count NUM-ENTRIES(erclist)}.
    {rc/ercput.i "stream s-out"}
    IF pass = 2 THEN
    DO:
      IF eddoc.error-count = 0 THEN
      DO:
        CREATE oe-ord.
        ASSIGN
          oe-ord.company       = oe-ctrl.company
          oe-ord.loc           = oe-ctrl.loc
          oe-ctrl.n-ord        = oe-ctrl.n-ord + 1
          oe-ord.ord-no        = oe-ctrl.n-ord
            + edmast.order-no-mask /* 9705 CAH testing */
          oe-ord.type          = "O"
          oe-ord.ord-date      = edpotran.order-date
          oe-ord.po-no         = edpotran.cust-po
          oe-ord.bill-to       = edmast.cust
          oe-ord.cust-no   =
          IF AVAIL edshipto AND edshipto.cust > ""
          THEN edshipto.cust
          ELSE cust.cust-no
          oe-ord.cust-name = cust.name
          oe-ord.addr[1]   = cust.addr[1]
          oe-ord.addr[2]   = cust.addr[2]
          oe-ord.city      = cust.city
          oe-ord.state     = cust.state
          oe-ord.zip       = cust.zip
          oe-ord.contact   = cust.contact
          oe-ord.lead-days = cust.ship-days
          oe-ord.last-date =
          IF edpotran.cancel-date <> ?
          and edpotran.cancel-date < (today + cust.ship-days)
          then edpotran.cancel-date else today + cust.ship-day
          oe-ord.due-date  = IF EDPOTRAN.REQUEST-DATE = ? /* 9706 cah */
          THEN TODAY + 1 /* <9801 CAH */ ELSE edpotran.request-date
          oe-ord.terms     = cust.terms
          oe-ord.over-pct  = cust.over-pct
          oe-ord.under-pct = cust.under-pct
          oe-ord.fob-code  = cust.fob-code  /* dest or orig */
          oe-ord.frt-pay   = cust.frt-pay /* (P)repaid, (C)ollect or (B)ill */
          oe-ord.tax-gr    = cust.tax-gr
          oe-ord.carrier   = cust.carrier
          oe-ord.ship-id   = cust.cust-no
          oe-ord.sman[1]       = cust.sman
          oe-ord.sman[2]       = ""
          oe-ord.sman[3]       = ""
          oe-ord.due-code      =  /* could be BY, ON or MH (make & hold) */
          IF CAN-DO(by_list, edpotran.ship-date-code) THEN "BY" ELSE
          IF CAN-DO(on_list, edpotran.ship-date-code) THEN "ON" ELSE
          IF CAN-DO(mh_list, edpotran.ship-date-code) THEN "MH" ELSE "ON"
          oe-ord.terms-d = terms.dscr
          oe-ord.sold-id = IF AVAIL dc THEN dc.ship-to
          ELSE edpotran.st-code.
        FIND FIRST soldto WHERE soldto.company = ws_company AND
          soldto.cust-no = oe-ord.cust-no
          AND TRIM(soldto.sold-id) BEGINS TRIM(oe-ord.sold-id)
          USE-INDEX sold-id NO-LOCK NO-ERROR.
        IF AVAILABLE soldto
          THEN
        ASSIGN oe-ord.sold-no = soldto.sold-no
          oe-ord.sold-id      = soldto.sold-id
          oe-ord.sold-name    = soldto.sold-name
          oe-ord.sold-addr[1] = soldto.sold-addr[1]
          oe-ord.sold-addr[2] = soldto.sold-addr[2]
          oe-ord.sold-city    = soldto.sold-city
          oe-ord.sold-state   = soldto.sold-state
          oe-ord.sold-zip     = soldto.sold-zip.
        FIND FIRST asi.shipto WHERE shipto.company = ws_company AND
          shipto.cust-no = cust.cust-no NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN
        ASSIGN
          oe-ord.ship-i[1] = shipto.notes[1]
          oe-ord.ship-i[2] = shipto.notes[2]
          oe-ord.ship-i[3] = shipto.notes[3]
          oe-ord.ship-i[4] = shipto.notes[4].
        .
        FIND FIRST sman
          WHERE sman.company = oe-ord.company
          AND
          sman.sman = oe-ord.sman[1] NO-LOCK NO-ERROR.
        IF AVAIL sman THEN
        ASSIGN oe-ord.sman[1]   = CAPS(sman.sman)
          oe-ord.sname[1]  = sman.sname
          oe-ord.s-comm[1]  = sman.scomm
          oe-ord.s-pct[1] =      100
          .
      END.    /*  no errors on header */
    END.    /* pass 2 */
    CLEAR FRAME f-det ALL NO-PAUSE.
    FOR EACH edpoline OF edpotran EXCLUSIVE
    break by edpoline.seq by edpoline.line  /* 9903 CAH to support first of */
        WITH FRAME f-det DOWN WIDTH 144:
      erclist = "".
      IF edpoline.item-no = "" THEN
      _xitem:
      DO:
        IF edpoline.cust-item-no > "" THEN
        DO:
          FIND FIRST edicxref
            WHERE edicxref.partner = edmast.partner
            AND edicxref.cust-item = edpoline.cust-item-no
            NO-LOCK NO-ERROR.
          IF AVAIL edicxref THEN do:
            ASSIGN edpoline.item-no = edicxref.item-no.
            leave _xitem.
          end.
          else do:
            find first itemfg
            where itemfg.company = ws_company
            and   itemfg.part-no = edpoline.cust-item-no
            and   itemfg.cust-no = edmast.cust no-lock no-error.
            if avail itemfg then do:
                assign edpoline.item-no = itemfg.i-no.
                leave _xitem.
            end.
            else
            if edmast.item-prefix > "" or edmast.item-suffix > "" then do:
                ws_char = edmast.item-prefix
                    + edpoline.cust-item-no
                    + edmast.item-suffix.
                find first itemfg
                where itemfg.company = ws_company
                and   itemfg.i-no = ws_char no-lock no-error.
                if avail itemfg then do:
                    assign edpoline.item-no = itemfg.i-no.
                    leave _xitem.
                end.
            end.
          end.
        END.
        IF edpoline.upc > "" THEN
        DO:
          FIND FIRST itemfg
            WHERE itemfg.upc = edpoline.upc NO-LOCK NO-ERROR.
          IF AVAIL itemfg THEN
          ASSIGN edpoline.item-no = itemfg.i-no.
          LEAVE _xitem.
        END.
      END.
      IF edpoline.item-no = "" THEN
      DO:
        erctoken[5] = (IF edpoline.cust-item-no > "" THEN
        edpoline.cust-item-no
        ELSE
        edpoline.upc).
        {rc/listadd.i erclist 501}
      END.
      ELSE
      DO:
        FIND FIRST itemfg
          WHERE itemfg.company = ws_company
          AND   itemfg.i-no = edpoline.item-no NO-LOCK NO-ERROR.
        IF NOT AVAIL itemfg THEN
        DO:
          erctoken[5] = edpoline.item-no.
          {rc/listadd.i erclist 120}.
        END.
        ELSE
        DO:
          IF edpoline.cust-item-no = ""
            THEN
          edpoline.cust-item-no = itemfg.part-no.
          IF edpoline.color-desc = ""
            THEN
          edpoline.color-desc = "".
          IF edpoline.description[1] = ""
            THEN
          edpoline.description[1] =
          IF itemfg.part-dscr1 > ""
            THEN
          itemfg.part-dscr1
          ELSE
          itemfg.i-dscr.
          IF edpoline.description[2] = "" THEN edpoline.description[2] =
          IF itemfg.part-dscr2 > "" THEN
          itemfg.part-dscr2 ELSE itemfg.i-name.
          if edpoline.upc = "" and itemfg.upc-no > ""
          then edpoline.upc = itemfg.upc-no.
          IF edpoline.description[2] > "" and edpoline.description[1] = ""
          then assign edpoline.description[1] = edpoline.description[2]
            edpoline.description[2] = "".
          if edpoline.description[2] = edpoline.description[1]
          then edpoline.description[2] = "".
        END.
      END.
      FIND FIRST uom
        WHERE uom.uom = edpoline.uom-code NO-LOCK NO-ERROR.
      IF NOT AVAIL uom THEN
      DO:
        IF edpoline.uom-code = "CT" THEN
        DO:
        END.
        ELSE
        DO:
          {rc/listadd.i erclist 503}
          erctoken[9] = edpoline.uom-code.
        END.
      END.
      if avail edshipto then do:
      FIND FIRST shipto
        WHERE shipto.company = ws_company
        AND shipto.cust-no = /* 9808 CAH: Expanded this logic */
            (if edshipto.cust > "" then edshipto.cust else 
            if edpotran.cust > "" then edpotran.cust 
            else edmast.cust)
        AND shipto.ship-id = edshipto.ship-to NO-LOCK NO-ERROR.
      IF NOT AVAIL shipto THEN
      DO:
        {rc/listadd.i erclist 502}
        erctoken[1] = edshipto.cust.
        erctoken[2] = edshipto.ship-to.
      END. end. else
      DO:
        erctoken[4] = edpotran.by-code.
        {rc/listadd.i erclist 401}
      end.
      IF pass = 1 THEN
      DO:
        DISPLAY STREAM s-out
          edpoline.line
          edpoline.item-no
          edpoline.description[1]
          edpoline.qty-orig-ord
          edpoline.unit-price
          edpoline.uom-code
          EDPOLine.UPC
          edpoline.cust-item-no format 'x(10)'
          EDPOLine.cust-po-line FORMAT "x(04)"
          WITH FRAME f-det DOWN WIDTH 144.
        IF EDPOLine.qty-change <> 0 AND edpoline.qty-orig-ord = 0
          THEN
        DISPLAY STREAM s-out
          edpoline.qty-change @ edpoline.qty-orig-ord.
        DOWN STREAM s-out.
        IF EDPOLine.Description[2] > "" THEN
        DO:
          DISPLAY STREAM s-out
            edpoline.description[2] @ edpoline.description[1].
          DOWN STREAM s-out.
        END.
        if avail itemfg
        and itemfg.sell-price <> edpoline.unit-price
        then do:
            if edpoline.unit-price = 0 or edpoline.unit-price = ?
            then do:
                assign edpoline.unit-price = itemfg.sell-price.
                put stream s-out skip(1) "*** Warning: Price set to itemfg price: " string(itemfg.sell-price,"$>>>>.99") " Per " itemfg.sell-uom skip.
            end.
            else put stream s-out skip(1) "*** Warning: Price in itemfg: "
            string(itemfg.sell-price,"$>>>>.99") " Per " itemfg.sell-uom skip.
        end.
        /* 9903 CAH: Added Line item level notes support */
        for each edpoaddon no-lock of edpotran
        where edpoaddon.order-line = integer(edpoline.cust-po-line):
        
         if edpoaddon.note[1] > "" then do:
         put stream s-out skip(1) "Notes -" at 30 skip.
         do i = 1 to 9:
                if edpoaddon.note[i] > "" then put stream s-out 
                skip i at 30 ") " edpoaddon.note[i] skip.
         end.   /* nonblank notes */
         end.   /* some notes */
         end.   /* for each */
      END.
      IF erclist > "" THEN
      DO:
        {rc/accum.i  eddoc.error-count NUM-ENTRIES(erclist)}.
        {rc/ercput.i "stream s-out"}
      END.
      ELSE
      DO:
        IF pass = 2 THEN
        DO:
          {rc/incr.i ws_recs_selected}.
        /* 9805 CAH added this code to lookup itemfg in order to 
            update q-alloc ... */
          FIND FIRST itemfg
            WHERE itemfg.company = oe-ord.company
            AND   itemfg.i-no = edpoline.item-no EXCLUSIVE-LOCK NO-ERROR.
            
          FIND FIRST fg-bin
            WHERE fg-bin.company  = oe-ord.company
            AND fg-bin.i-no     = edpoline.item-no
            AND fg-bin.loc      =
            (if edpotran.sf-code > "" then edpotran.sf-code else oe-ord.loc)
            AND fg-bin.qty      >= edpoline.qty-orig-ord
            NO-LOCK NO-ERROR.
          IF NOT AVAIL fg-bin THEN
          FIND FIRST fg-bin
            WHERE fg-bin.company  = oe-ord.company
            AND fg-bin.i-no     = edpoline.item-no
            AND fg-bin.qty      >= edpoline.qty-orig-ord
            NO-LOCK NO-ERROR.
          IF NOT AVAIL fg-bin THEN
          FIND FIRST fg-bin
            WHERE fg-bin.company  = oe-ord.company
            AND fg-bin.i-no     = edpoline.item-no
            NO-LOCK NO-ERROR.
          /* 9802 CAH: Disable bin per Jack Lechner ...
            if edpoline.uom-code = "CT" and avail fg-bin
          and fg-bin.case-count > 0 then conv_fact = fg-bin.case-count.
          else */ if edpoline.uom-code = "CT" and avail itemfg
          and itemfg.case-count > 0 then conv_fact = itemfg.case-count.
          else if edpoline.uom-code = "M" then conv_fact = 1000.
          else if edpoline.uom-code = "C" then conv_fact = 100.
          else if edpoline.uom-code = "CWT" and avail itemfg
            and itemfg.weight-100 > 0 then conv_fact = (1 / itemfg.weight-100).
          else if edpoline.uom-code begins "L"  and avail itemfg
            and itemfg.weight-100 > 0 then conv_fact =
                (1 / (itemfg.weight-100 / 100)).
          else conv_fact = 1.
          CREATE oe-ordl.
          ASSIGN
            oe-ordl.company       = oe-ord.company
            oe-ordl.ord-no        = oe-ord.ord-no
            oe-ordl.cust-no       = oe-ord.cust-no
            oe-ordl.line          = edpoline.line
            oe-ordl.part-no       = edpoline.cust-item-no
            oe-ordl.i-no          = edpoline.item-no
            oe-ordl.i-name        = itemfg.i-name
            oe-ordl.est-no        = itemfg.est-no
            oe-ordl.pr-uom        =
            IF edpoline.uom-code = "CT" THEN "CS"
            ELSE edpoline.uom-code
            oe-ordl.qty           = round(edpoline.qty-orig-ord * conv_fact,0)
            oe-ordl.i-dscr        = itemfg.i-dscr
            oe-ordl.price         = edpoline.unit-price
            oe-ordl.po-no         = oe-ord.po-no
            oe-ordl.cost          = itemfg.std-tot-cost
            oe-ordl.tax           = edpoline.tax
            oe-ordl.t-weight = round((oe-ordl.qty / 100) * itemfg.weight-100,0)
            oe-ordl.req-date      = oe-ord.due-date
            oe-ordl.req-code      = oe-ord.due-code
            oe-ordl.prom-date     = edpotran.cancel-date
            oe-ordl.over-pct      = oe-ord.over-pct
            oe-ordl.under-pct     = oe-ord.under-pct
            oe-ordl.prom-code     = IF edpotran.cancel-date <> ? THEN
            IF CAN-DO(by_list, edpotran.cancel-date-code) THEN "BY" ELSE
            IF CAN-DO(on_list, edpotran.cancel-date-code) THEN "ON" ELSE
            IF CAN-DO(mh_list, edpotran.cancel-date-code) THEN "MH" ELSE "BY"
            ELSE "BY"
            oe-ordl.t-price       = oe-ordl.qty * (oe-ordl.price / conv_fact)
            oe-ordl.part-dscr1    = itemfg.part-dscr1
            oe-ordl.part-dscr2    = itemfg.part-dscr2
            oe-ordl.cas-cnt       = itemfg.case-count.
          /*
          display stream s-out
            edpoline.uom-code
            conv_fact
            itemfg.case-count
            fg-bin.case-count when avail fg-bin
            oe-ordl.cas-cnt.
            .
          */
          IF AVAIL fg-bin THEN
          DO:
            ASSIGN
              oe-ordl.job-no        = fg-bin.job-no
              oe-ordl.job-no2       = fg-bin.job-no2 .
          END.
        if oe-ordl.pr-uom Begins "L" then
          assign oe-ordl.t-price =  oe-ordl.price -
                  round( (oe-ordl.price * oe-ordl.disc) / 100, 2).
        else if oe-ordl.pr-uom = "CS" and avail itemfg and
            itemfg.case-count ne 0 then
          assign oe-ordl.t-price = ((oe-ordl.qty / itemfg.case-count) *
                          oe-ordl.price) - round((((oe-ordl.qty /
                          itemfg.case-count) * oe-ordl.price) *
                          oe-ordl.disc) / 100, 2).
        else if oe-ordl.pr-uom = "C" then
          assign oe-ordl.t-price = (( oe-ordl.qty / 100) *
                     oe-ordl.price) - round(((( oe-ordl.qty / 100) *
                     oe-ordl.price) *  oe-ordl.disc) / 100, 2).
        else if  oe-ordl.pr-uom = "M" then
          assign oe-ordl.t-price = (( oe-ordl.qty / 1000) *
                 oe-ordl.price) - round(( (( oe-ordl.qty / 1000) *
                 oe-ordl.price) *  oe-ordl.disc) / 100, 2).
        else /** DEFAULT TO EACH **/
          assign oe-ordl.t-price = (( oe-ordl.qty ) *
                 oe-ordl.price) -
                round(( (( oe-ordl.qty ) *
                 oe-ordl.price) *  oe-ordl.disc) / 100, 2).
        /* 9903 CAH: Added Line item level notes support */
        find first edpoaddon of edpotran
        where edpoaddon.order-line = integer(edpoline.cust-po-line)
        and edpoaddon.note[1] > "" no-lock no-error.
        ws_logical = if avail edpoaddon then true else false.
        /* 9903 CAH: Moved this from header bill-i to line item ship-i */
        
        if first-of (edpoline.seq) then do:
        if not avail edpoaddon 
        then find first edpoaddon of edpotran
        where edpoaddon.order-line = 0
        and edpoaddon.note[1] > "" no-lock no-error.
        
        end.    /* first of seq */
        if avail edpoaddon then 
        ASSIGN
          oe-ordl.ship-i[1] = edpoaddon.note[1]
          oe-ordl.ship-i[2] = edpoaddon.note[2]
          oe-ordl.ship-i[3] = edpoaddon.note[3]
          oe-ordl.ship-i[4] = edpoaddon.note[4].
        .
        
        assign
    oe-ord.t-weight  = oe-ord.t-weight  + oe-ordl.t-weight
    oe-ord.t-freight = oe-ord.t-freight + oe-ordl.t-freight
    oe-ordl.t-cost   = oe-ordl.cost * (oe-ordl.qty / 1000)
    oe-ord.t-cost    = oe-ord.t-cost  + oe-ordl.t-cost.
          DEF BUFFER xrel FOR oe-rel.
          FIND FIRST xrel USE-INDEX seq-no EXCLUSIVE-LOCK NO-ERROR.
          ws_int = IF AVAIL xrel THEN
          xrel.r-no + 1 ELSE
          1.
          CREATE oe-rel.
          ASSIGN
            oe-rel.r-no         = ws_int
            oe-rel.company      = oe-ord.company
            oe-rel.ship-id      = shipto.ship-id
            oe-rel.ship-no      = shipto.ship-no
            oe-rel.sold-no      = oe-ord.sold-no
            oe-rel.qty          = oe-ordl.qty
            oe-rel.po-no        = oe-ord.po-no
            oe-rel.i-no         = oe-ordl.i-no
            oe-rel.rel-date     = edpotran.request-date
            oe-rel.partial      = 0
            oe-rel.cust-no      = oe-ord.cust-no
            oe-rel.line         = oe-ordl.line
            oe-rel.o-no         = STRING(oe-ord.ord-no,"99999999")
            oe-rel.w-ord        = TRUE
            oe-rel.ship-date    = ?
            oe-rel.loc          =
            IF AVAIL fg-bin THEN fg-bin.loc ELSE oe-ord.loc
            oe-rel.loc-bin      =
            IF AVAIL fg-bin THEN fg-bin.loc-bin ELSE oe-rel.loc-bin
            oe-rel.tag          =
            IF AVAIL fg-bin THEN fg-bin.tag ELSE oe-rel.tag
            oe-rel.qty-case     =
            IF AVAIL fg-bin AND fg-bin.case-count > 0
            THEN fg-bin.case-count
            ELSE IF AVAIL itemfg AND itemfg.case-count > 0
            THEN itemfg.case-count
            ELSE 1
            oe-rel.cases        = TRUNC(oe-rel.qty / oe-rel.qty-case, 0)
            oe-rel.sman[1]      = oe-ord.sman[1]
            oe-rel.sman[2]      = oe-ord.sman[2]
            oe-rel.sman[3]      = oe-ord.sman[3]
            oe-rel.s-pct[1]     = oe-ord.s-pct[1]
            oe-rel.s-pct[2]     = oe-ord.s-pct[2]
            oe-rel.s-pct[3]     = oe-ord.s-pct[3]
            oe-rel.s-name[1]    = oe-ord.sname[1]
            oe-rel.s-name[2]    = oe-ord.sname[2]
            oe-rel.s-name[3]    = ''
            oe-rel.s-comm[1]    = oe-ord.s-comm[1]
            oe-rel.s-comm[2]    = oe-ord.s-comm[2]
            oe-rel.s-comm[3]    = oe-ord.s-comm[3]
            oe-rel.ord-no       = oe-ord.ord-no
            oe-rel.carrier      =
            IF shipto.carrier > "" THEN shipto.carrier ELSE oe-ord.carrier
            oe-rel.ship-addr[1] = shipto.ship-addr[1]
            oe-rel.ship-addr[2] = shipto.ship-addr[2]
            oe-rel.ship-city    = shipto.ship-city
            oe-rel.ship-state   = shipto.ship-state
            oe-rel.ship-zip     = shipto.ship-zip
            oe-rel.ship-i[1]    = oe-ord.ship-i[1]
            oe-rel.ship-i[2]    = oe-ord.ship-i[2]
            oe-rel.ship-i[3]    = oe-ord.ship-i[3]
            oe-rel.ship-i[4]    = oe-ord.ship-i[4]
            oe-rel.deleted      = oe-ord.deleted
            oe-ordl.rel-qty     = oe-ordl.rel-qty + oe-rel.qty.
          IF oe-rel.ship-i[1] = "" AND shipto.notes[1] > ""
            THEN
          DO i = 1 TO 4:
            oe-rel.ship-i[i] = shipto.notes[i].
          END.
          IF AVAIL xrel THEN
          RELEASE xrel.
          IF AVAIL itemfg then do:  
        /* 9805 CAH: update the itemfg.q-alloc field, per Joe Hentz ... */
          Assign itemfg.q-alloc = itemfg.q-alloc + oe-ordl.qty.
          END.
        END. /* pass 2 */
      END.  /* no errors */
    END.
    FOR EACH edpoaddon OF edpotran:
      erclist = "".
      IF pass = 2 THEN
      DO:
        {rc/incr.i ws_recs_selected}.
        IF edpoaddon.order-line > 0 THEN
        FIND oe-ordl OF oe-ord
          WHERE
          oe-ordl.line = edpoaddon.order-line NO-LOCK NO-ERROR.
        CREATE oe-ordm.
        ASSIGN
          oe-ordm.company = oe-ord.company
          oe-ordm.ord-no  = oe-ord.ord-no
          oe-ordm.est-no  = IF AVAIL oe-ordl THEN oe-ordl.est-no ELSE ""
          oe-ordm.line    = edpoaddon.line
          oe-ordm.amt     = edpoaddon.amount
          oe-ordm.dscr    = edpoaddon.description[1]
          oe-ordm.posted  = FALSE
          .
      END.    /* pass 2 */
    END.
    DISPLAY eddoc.error-count WITH FRAME f-current.
    IF pass = 1 AND eddoc.error-count > 0 THEN
    DO:
      LEAVE _passes.
    END.
    IF pass = 2 THEN
    DO:
      IF eddoc.error-count > 0 THEN
      DO:
        LEAVE _passes.
      END.
      ELSE
      DO:
        display stream s-out "Created Order#" oe-ord.ord-no
        with frame f-create no-labels.
        ASSIGN
          eddoc.posted = TRUE
          eddoc.openitem = TRUE
          eddoc.unique-order-no = oe-ord.ord-no
          eddoc.st-code = edpotran.st-code
          eddoc.stat = 1
          eddoc.status-flag = "WIP"
          {rc/stampcht.i eddoc}
          {rc/incr.i ws_recs_changed}
          eddoc.interpdate = today
          eddoc.interptime = time
          eddoc.userref = "BY-CODE: " + edpotran.by-code.
      END. END. END.  /* passes */
  IF erclist > "" THEN
  DO:
    {rc/accum.i eddoc.error-count NUM-ENTRIES(erclist)}.
    {rc/ercput.i "stream s-out"}
  END.
  IF eddoc.error-count > 0 THEN {rc/incr.i ws_recs_inerror}.
  /* {rc/statsdis.i} 9809 CAH caused flashing */
  pause 0.
END.
{rc/statsdis.i}
PAGE STREAM s-out.
VIEW STREAM s-out FRAME f-stats.
page stream s-out.
PAUSE 5 NO-MESSAGE.
HIDE FRAME f-current NO-PAUSE.
HIDE FRAME f-stats NO-PAUSE.
RETURN.
