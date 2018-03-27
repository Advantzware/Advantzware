{ed/sharedv.i}
DEF SHARED STREAM s-out.
DEF SHARED FRAME hdg-std.
DEF BUFFER dc FOR edshipto.
{rc/hdg-wide.i "ed/i860.p" "PURCHASE ORDER CHANGE EDIT LIST" "(s-out)"}
{rc/stats.i}
{rc/fcurrent.i "row 3"}
{rc/stringv.i}
{rc/ercvars.i 50 " " NEW}
DEF VAR pass AS INT NO-UNDO.
DEF VAR save_erclist LIKE erclist NO-UNDO.
{rc/hdg-noco.i}
VIEW STREAM s-out FRAME hdg-std.
start = TIME.
VIEW FRAME f-current.
VIEW FRAME f-stats.
FOR EACH eddoc
    WHERE eddoc.fgid = "PC"
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
    IF pass = 1 AND AVAIL edpotran
      THEN
    DO:
        /* 9801 CAH Per Jack Lechner */
        
        CASE edpotran.purpose-code:
        WHEN "00" THEN WS_CHAR = "* ORIGINAL *".
        WHEN "01" THEN ws_char = "* CANCELLATION *".
        WHEN "04" then ws_char = "* PO CHANGE *".
        OTHERWISE WS_CHAR = "".
        END CASE.
        
      DISPLAY STREAM s-out
        edpotran.purpose-code 
        edpotran.order-type
        edpotran.cust-po
            skip
        edpotran.order-date label "Changed On"
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
            skip
        edpotran.misc-date1 label "Orig PO Date"    
        ws_char             format "x(20)" label "NOTE"
        WITH FRAME f-header WIDTH 132 3 COLUMNS.
      IF EDPOTRAN.SHIP-name > "" THEN
      DISPLAY
        edpotran.ship-name SKIP
        edpotran.ship-address[1] SKIP
        edpotran.ship-address[2] SKIP
        edpotran.ship-address[3] SKIP
        edpotran.ship-city edpotran.ship-st
        edpotran.ship-zip
        edpotran.ship-country
        WITH FRAME f-shipto.
      IF edpotran.routing[1] > "" THEN
      DISPLAY STREAM s-out
        edpotran.routing WITH FRAME f-routing 1 COLUMN.
    END. /* pass = 1 */
    {rc/accum.i eddoc.error-count NUM-ENTRIES(erclist)}.
    {rc/ercput.i "stream s-out"}
    IF pass = 2 THEN
    DO:
      IF eddoc.error-count = 0 THEN
      DO:
      END.    /*  no errors on header */
    END.    /* pass 2 */
    CLEAR FRAME f-det ALL NO-PAUSE.
    FOR EACH edpoline OF edpotran EXCLUSIVE
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
            if edmast.item-prefix > "" or edmast.item-suffix > "" then do:
                ws_char = edmast.item-prefix
                    + edpoline.cust-item-no
                    + edmast.item-suffix.
            end.
          end.
        END.
        IF edpoline.upc > "" THEN
        DO:
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
      END.
      if avail edshipto then do:
      end.
      IF pass = 1 THEN
      DO:
        DISPLAY STREAM s-out
          edpoline.line
          edpoline.item-no
          edpoline.description[1]
          edpoline.qty-orig-ord
          edpoline.qty-change
          edpoline.unit-price
          edpoline.uom-code
          EDPOLine.UPC
          edpoline.cust-item-no format 'x(10)'
          EDPOLine.cust-po-line FORMAT "x(04)"
          WITH FRAME f-det DOWN WIDTH 144.
          
/*
        IF EDPOLine.qty-change <> 0 AND edpoline.qty-orig-ord = 0
          THEN
        DISPLAY STREAM s-out
          edpoline.qty-change @ edpoline.qty-orig-ord.
*/          
        DOWN STREAM s-out.
        IF EDPOLine.Description[2] > "" THEN
        DO:
          DISPLAY STREAM s-out
            edpoline.description[2] @ edpoline.description[1].
          DOWN STREAM s-out.
        END.
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
        END. /* pass 2 */
      END.  /* no errors */
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
        ASSIGN
          eddoc.posted = TRUE
          eddoc.openitem = TRUE
          eddoc.unique-order-no = ?
          eddoc.st-code = edpotran.st-code
          eddoc.stat = 1
          eddoc.status-flag = "WIP"
          {rc/stampcht.i eddoc}
          {rc/incr.i ws_recs_changed}
          eddoc.interpdate = today
          eddoc.interptime = time
          eddoc.userref = "BY-CODE: " + edpotran.by-code.
      END. 
    END. 
  END.  /* passes */
  IF erclist > "" THEN
  DO:
    {rc/accum.i eddoc.error-count NUM-ENTRIES(erclist)}.
    {rc/ercput.i "stream s-out"}
  END.
  IF eddoc.error-count > 0 THEN {rc/incr.i ws_recs_inerror}.
  {rc/statsdis.i}
END.
{rc/statsdis.i}
PAGE STREAM s-out.
VIEW STREAM s-out FRAME f-stats.
page stream s-out.
PAUSE 5 NO-MESSAGE.
HIDE FRAME f-current NO-PAUSE.
HIDE FRAME f-stats NO-PAUSE.
RETURN.
