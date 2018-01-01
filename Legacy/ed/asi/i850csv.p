/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\ed\asi\i850c
**       By: Chris Heins, RCI (c) 1998
** Descript: Import Orders from Spreadsheet, Royal Pioneer format.
Fields are:
1 = Store#
2 = Purchase Order#
3 = Ship Date
... repeating block ...
4 = Item#
5 = Quantity 
6 = Price
10.10.98 by CAH on \\ricky\rv8 Log#0000:
1.  Added column for unit price following item and quantity.
2.  Added accumulation of edpotran.order-amount.
**
*****************************************************************************
\***************************************************************************/
DEF STREAM s-in.
DEF SHARED STREAM s-out.
{ed/sharedv.i}
{ed/edivars.i}
{rc/stats.i}
{rc/stringv.i}
{rc/datev.i}
{rc/timev.i}
{rc/ercvars.i 60 "initial ''" "new"}
{rc/fcurrent.i "row 3"}
DEF var ws_version AS char NO-UNDO.
DEF VAR temp_fid      AS CHAR NO-UNDO.
DEF VAR ws_fext AS CHAR NO-UNDO.
DEF VAR err_fid AS CHAR NO-UNDO.
DEF VAR curr_fid     AS CHAR NO-UNDO.  /* used for deletion when done */
DEF VAR n AS INT NO-UNDO FORMAT "9" INITIAL 1.
DEF VAR fid AS CHAR FORMAT 'x(12)' NO-UNDO EXTENT 1 INITIAL
  [ "" ].
DEF VAR archive_fid AS CHAR NO-UNDO.
DEF VAR skip_doc AS LOGICAL NO-UNDO.
/*
DEF VAR ws_customer LIKE edmast.cust NO-UNDO.
DEF VAR ws_isa  AS INT NO-UNDO.
DEF VAR ws_gs   AS INT NO-UNDO.
DEF VAR ws_st   AS INT NO-UNDO.
/* set true if we have to skip to the next header */
def var ws_message as char format 'x(40)' no-undo label "Message".
*/
{ed/getpath.i}
ASSIGN
  temp_fid = "t_" + STRING(TIME,"99999") + ".q"
  .
RUN rc/dt2fext.p (TODAY, OUTPUT ws_fext).
err_fid = edco.path-err + dirsep + "edierr" + ws_fext.
start = TIME.
n = 1.
_files:
REPEAT n = 1 TO 1:
  curr_fid = ws_edi_path + (IF fid[n] > "" THEN
  dirsep + LC(fid[n]) ELSE ""
  ).
  IF top-debug THEN
  DO:
    RUN rc/debugmsg.p ("Searching for file: " + curr_fid).
  END.
  curr_fid = SEARCH(curr_fid).
  IF SEARCH(curr_fid) = ? THEN
  DO:
    IF n <= 1 /* no header file */ THEN
    DO:
      PUT STREAM s-out UNFORMATTED SKIP(1)
        "Input file: " curr_fid " was not found, cannot continue" SKIP.
      RETURN.
    END.
    ELSE
    NEXT _files.
  END.
  /* assigment required to put the archive in the same directory */
  ELSE
  curr_fid = SEARCH(curr_fid).
  archive_fid = curr_fid.
  ws_int = R-INDEX(archive_fid, ".").
  IF ws_int > 0          /* strip off extension */
    THEN
  archive_fid = SUBSTRING(archive_fid, 1, ws_int - 1).
  archive_fid = archive_fid + ws_fext.  /* add MDD extension */
  IF top-debug THEN
  DO:
    RUN rc/debugmsg.p ("Processing file: " + curr_fid).
    MESSAGE "running quoter".
    PAUSE.
  END.
  RUN rc/osquoter.p
    (curr_fid, ",", ?, temp_fid).
  INPUT STREAM s-in FROM VALUE(temp_fid) NO-ECHO.
  PUT STREAM s-out UNFORMATTED SKIP(1) "Processing from file: " curr_fid SKIP.
  DEF var c AS char NO-UNDO extent 50.
  ws_edpotran_rec = ?.
  PAUSE 0.
  _main:
  REPEAT:
    ASSIGN str_buffa = '' erclist = '' c = ''.
    IMPORT STREAM s-in c.
    {rc/incr.i ws_recs_read}.
    IF top-debug THEN
    DO:
      DISPLAY
        c[1 FOR 7]
        NO-LABEL /* LABEL "Current Record (Leading 50 Chars)" */
        WITH FRAME f-debug.
      PAUSE 1.
      ws_char = c[1] + '/' + c[2] + '/' + c[3] + '/' + c[4] + '/' + c[5] + '/'
      + c[6] + '/' + c[7].
      RUN rc/debugmsg.p (ws_char).
    END.
    IF c[1] <= " " or c[2] <= " " 
    or c[1] = "STORE" THEN  /* store and po# must be nonblank */
    NEXT _main.
    
    ws_date = ?.
    ws_date = date(c[3]) no-error.
    if error-status:error = false then 
    _process: DO:
        {rc/incr.i ws_recs_selected}.
        
        /*
      ASSIGN
        yr = integer(c[2])
        mo = LOOKUP(c[3], month_abbrev)
        da = integer(c[4])
        */
      ordering_store_number = c[1].
      ship_date# = ws_date.
      ws_docid = string(ship_date#) + "-" + ordering_store_number
        /* 9810 CAH: Added PO# to doc ID ... */ 
         + (if c[2] > '' then '-' + trim(c[2]) else "").
      purchase_order_number = if c[2] > "" then trim(c[2]) else ws_docid.
      
      FIND FIRST eddoc
        WHERE eddoc.partner = ws_partner
        AND eddoc.setid = ws_setid
        AND eddoc.docid = ws_docid EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL eddoc THEN
      DO:
          PUT STREAM s-out UNFORMATTED SKIP
            "Duplicate document detected for partner: " eddoc.partner
            " purchase order: " eddoc.docid.
          IF eddoc.stat > 0 THEN
          DO:
            /* duplicate ??? */
            PUT STREAM s-out UNFORMATTED
              " has already been processed - duplicate skipped" SKIP.
            {rc/listadd.i erclist 1002}
            erctoken[7] = ws_docid.
            skip_doc = TRUE.
            leave _process.
          END.
          ELSE
          DO:
            PUT STREAM s-out UNFORMATTED
              " not yet processed - duplicate deleted" SKIP.
            RUN ed/fm850del.p (RECID(eddoc)).
          DELETE eddoc.
          {rc/incr.i ws_amt_changed}.
          {rc/decr.i ws_amt_added}.
        END.
      END.
      IF NOT AVAIL eddoc
        THEN
      DO:
        FIND edshipto
          WHERE edshipto.partner = ws_partner
          AND edshipto.ref-type = "BY"
          AND edshipto.by-code = ordering_store_number NO-LOCK NO-ERROR.
        RUN ed/gendoc.p (ws_edcode_rec, ws_docid, OUTPUT ws_eddoc_rec).
        {rc/incr.i ws_amt_added}.
        FIND eddoc WHERE RECID(eddoc) = ws_eddoc_rec
          EXCLUSIVE-LOCK NO-ERROR.
        CREATE edpotran.
        ASSIGN
          eddoc.interpdate = TODAY
          eddoc.interptime = TIME
          eddoc.st-code =
          IF AVAIL edshipto THEN edshipto.st-code ELSE ""
          eddoc.fgid = "PO"
          eddoc.version = ws_version
          edpotran.partner = eddoc.partner
          edpotran.seq = eddoc.seq
          edpotran.cust     = edmast.cust
          edpotran.cust-po = purchase_order_number  /* 9808 was ws_docid */
          edpotran.order-date = TODAY
          edpotran.request-date = ship_date#
          edpotran.sf-code = 
                          if edmast.sf-code > "" then edmast.sf-code else edco.sf-code
          edpotran.by-code = ordering_store_number
          edpotran.vn-code = edmast.we-vend-no
          edpotran.st-code = IF AVAIL edshipto THEN edshipto.st-code ELSE ""
          /*
          edpotran.cust-div = if avail edshipto then edshipto.sales-div else ""
          edpotran.cust-div = if avail edshipto then edshipto.cust-div else ""
          */
          edpotran.cust-div = IF AVAIL edshipto THEN edshipto.cust-region ELSE ""
          .
      END.
      ELSE
      DO:
        ASSIGN ws_eddoc_rec = RECID(eddoc).
        FIND edpotran OF eddoc NO-LOCK NO-ERROR.
      END.
      ws_edpotran_rec = RECID(edpotran).
      DEF var i AS int NO-UNDO.
      i = 4.
      FIND edpotran WHERE RECID(edpotran) = ws_edpotran_rec EXCLUSIVE-LOCK.
      REPEAT:
        customer_item_number = c[i].
        IF customer_item_number = "" THEN
        LEAVE.
        vendor_item_number = edmast.item-prefix + customer_item_number
        + edmast.item-suffix.
        quantity_ordered = integer(c[i + 1]).
        unit_price       = decimal(c[i + 2]).   /* 9810 CAH */
        IF quantity_ordered > 0 THEN
        DO:
          IF top-debug THEN
          DO:
            RUN rc/debugmsg.p 
            ("Order for " + vendor_item_number 
            + " Quantity " + string(quantity_ordered) 
            + " Price: " + string(unit_price,"->>>>.99") ).
          END.
          FIND FIRST edpoline OF edpotran
            WHERE edpoline.item-no = vendor_item_number EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAIL edpoline THEN
          DO:
            CREATE edpoline.
            ASSIGN edpoline.partner = edpotran.partner
              edpoline.seq = edpotran.seq
              {rc/incr.i edpotran.last-line}
              {rc/incr.i edpotran.lines}
              edpoline.line = edpotran.last-line
              edpoline.item-no = vendor_item_number
              {rc/incr.i ws_recs_added}
              {rc/decr.i ws_recs_changed}.
          END.
          ASSIGN
            edpoline.cust-item-no = customer_item_number
            edpoline.cust-po-line = string(edpoline.line,"99")
            edpoline.qty-orig-ord = quantity_ordered
            edpoline.by-code = edpotran.by-code            
            edpoline.st-code = edpotran.st-code
            edpoline.sf-code = edpotran.sf-code
            edpoline.uom-code = "CT"
            edpoline.unit-price = unit_price
            edpotran.order-amount = edpotran.order-amount 
                + round(edpoline.qty-orig-ord * edpoline.unit-price, 2)
            {rc/incr.i ws_recs_changed}.
          IF top-debug THEN
          DISPLAY
            edpoline.line
            edpoline.cust-item-no edpoline.cust-po-line edpoline.qty-orig-ord
            WITH FRAME f-line DOWN.
        END.    /* quantity > 0 */
        i = i + 3.
      END.        /* repeat line items */
    END.    /* _process good line */
    IF erclist > '' THEN
    DO:
      {rc/incr.i ws_recs_inerror}.
      DISPLAY STREAM s-out
        ws_partner
        ws_direction
       ws_setid
       c[1 for 4]
        WITH FRAME f-det DOWN WIDTH 185.
      {rc/ercput.i "stream s-out"}
    END.
  END. /* repeat */
  PUT STREAM s-out UNFORMATTED
    SKIP(1) "Purchase orders added: " ws_amt_added  FORMAT "-99999" SKIP.
  IF ws_amt_inerror > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Duplicates rejected  : " ws_amt_inerror FORMAT "-99999" SKIP.
  IF ws_amt_changed > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Duplicated recycled  : " ws_amt_changed FORMAT "-99999" SKIP.
  INPUT STREAM s-in CLOSE.
    if not error-status:error then do:
  RUN rc/oscopy.p (curr_fid, archive_fid).  /* save the input file as .MDD */
  ws_char = temp_fid.                       /* delete quoter temp file */
  IF SEARCH(archive_fid) <> ? THEN
  DO:      /* verify file is backed up */
  PUT STREAM s-out UNFORMATTED
  SKIP(1) "Input file saved as: " archive_fid SKIP.
  {rc/listadd.i ws_char curr_fid}         /*  add it to deletion list */
  END.
  RUN rc/osdel.p (ws_char).                 /* deletes list of files */
  end.
END.
IF top-debug THEN
HIDE FRAME f-debug NO-PAUSE.
/*
----*----1----*----2----*----3----*----4----*----5----*----6----*----7----*----8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
Store,Year,Month,,2,2,5,5,6,6,12,12,16,16,19,19,20,20,21,21,22,22,23,23,24,24,25,25,Total,Control,Proof
101,1998,OCT,15,2,6,5,7,6,10,12,20,16,25,19,7,20,5,21,160,22,65,23,20,24,95,25,12,432,432,0
101,1998,NOV,15,2,6,5,9,6,10,12,20,16,10,19,7,20,10,21,160,22,80,23,30,24,110,25,12,464,464,0
101,1998,DEC,15,2,8,5,9,6,15,12,30,16,30,19,6,20,25,21,180,22,80,23,30,24,120,25,12,545,545,0
,,,15,,,,,,,,,,,,,,,,,,,,,,,,,,,
107,1998,OCT,15,2,9,5,9,6,0,12,9,16,0,19,0,20,9,21,18,22,8,23,0,24,10,25,0,72,72,0
107,1998,NOV,15,2,9,5,9,6,9,12,9,16,9,19,0,20,9,21,144,22,58,23,28,24,87,25,18,389,389,0
107,1998,DEC,15,2,9,5,9,6,9,12,9,16,18,19,5,20,9,21,144,22,66,23,28,24,98,25,18,422,422,0
,,,15,,,,,,,,,,,,,,,,,,,,,,,,,,,
*/
