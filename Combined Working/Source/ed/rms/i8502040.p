/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\RMS\i8502040.p
**       By: Chris Heins (c) 1997 Report Concepts, Inc.
** Descript: EDI import procedure, RMS 850 2040
**
12.17.97 by CAH on \\ricky\robj8\ Log#0000:
Updated to reflect changes in input file format for SEARS 3060:
1.  Added substring from input file and assignment to edpotran of 
transaction purpose code and purchase order type.
2.  Added substring from input file and assignment to edpoline of
UPC code.
3.  Added save of REF AE Authorization of expense in ref3 with qual AE.
4.  Added save of N1/4 Mark For Store in ref2 with qual Z9.
*****************************************************************************
\***************************************************************************/
DEF STREAM s-in.
DEF SHARED STREAM s-out.
{ed/sharedv.i}
{ed/edivars.i}
{ed/rms/sharedv.i "new shared"}
{rc/stats.i}
{rc/stringv.i}
{rc/datev.i}
{rc/timev.i}
{rc/ercvars.i 60 "initial ''" "new"}
{rc/fcurrent.i "row 3"}
DEF VAR fid AS CHAR FORMAT 'x(12)' NO-UNDO EXTENT 1 INITIAL
  [ "" ].
DEF VAR temp_fid      AS CHAR NO-UNDO.
DEF VAR err_fid AS CHAR NO-UNDO.
DEF VAR curr_fid     AS CHAR NO-UNDO.  /* used for deletion when done */
DEF VAR n AS INT NO-UNDO FORMAT "9" INITIAL 1.
DEF VAR debug AS LOGICAL NO-UNDO INITIAL FALSE.
DEF VAR debug_msg AS CHAR NO-UNDO.
DEF VAR ws_customer LIKE edmast.cust NO-UNDO.
DEF VAR ws_isa  AS INT NO-UNDO.
DEF VAR ws_gs   AS INT NO-UNDO.
DEF VAR ws_st   AS INT NO-UNDO.
DEF VAR archive_fid AS CHAR NO-UNDO.
DEF VAR ws_fext AS CHAR NO-UNDO.
/* set true if we have to skip to the next header */
DEF VAR skip_doc AS LOGICAL NO-UNDO.
def var ws_message as char format 'x(40)' no-undo label "Message".
def var ws_version as char no-undo.
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
  IF debug THEN
  DO:
    DISPLAY
      ws_rec_code WITH FRAME f-debug.
  END.
  curr_fid = ws_edi_path + (IF fid[n] > "" THEN
  dirsep + LC(fid[n]) ELSE ""
  ).
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
  IF debug THEN
  DO:
    MESSAGE "running quoter".
    PAUSE.
  END.
  RUN rc/osquoter.p
    (curr_fid, ?, ?, temp_fid).
  INPUT STREAM s-in FROM VALUE(temp_fid) NO-ECHO.
  PUT STREAM s-out UNFORMATTED SKIP(1) "Processing from file: " curr_fid SKIP.
  PAUSE 0.
  _main:
  REPEAT:
    ASSIGN str_buffa = '' erclist = ''.
    IMPORT STREAM s-in str_buffa.
    {rc/incr.i ws_recs_read}.
    IF debug THEN
    DO:
      DISPLAY
        STR_BUFFA FORMAT "X(50)"  LABEL "Current Record (Leading 50 Chars)"
        WITH FRAME f-debug.
      PAUSE 1.
    END.
    IF str_buffa <= " " THEN
    NEXT _main.
    IF SUBSTRING(str_buffa,1,5) = rms_sep_code THEN
    DO:
      ASSIGN
        ws_rec_code = 0
        last_rec_code = 0
        skip_doc = FALSE. /* reset at start of new document */
    END.
    ELSE
    DO:
      IF skip_doc THEN
      NEXT _main.
      ws_rec_code = INTEGER(SUBSTRING(str_buffa,1,2)).
    END.
    IF debug THEN
    DISPLAY ws_rec_code WITH FRAME f-debug.
    IF ws_rec_code = 0 THEN
    DO: /* header record */
      ASSIGN
        {rc/incr.i ws_recs_selected}
        {rc/substr.i rms_header_partner         6  5}
        {rc/substr.i rms_header_setid         11  3}
        {rc/substr.i rms_header_std-ver        14 12}
        {rc/substr.i rms_header_int-cd         26 30}
        {rc/substr.i rms_header_company-id     56  5}
        .
      IF debug THEN
      DO:
        DISPLAY
          rms_header_partner
          rms_header_setid
          rms_header_std-ver
          rms_header_int-cd
          rms_header_company-id
          .
      END.
      find first edcode
      where edcode.partner =
      (if rms_header_company-id > "" then rms_header_company-id
       else rms_header_partner)
        and edcode.setid   = rms_header_setid
        and integer(edcode.version) = integer(rms_header_std-ver)
        and edcode.direction = "I" no-lock no-error.
      if avail edcode then ws_edcode_rec = recid(edcode).
      else do:
        display stream s-out
          rms_header_partner
          rms_header_setid
          rms_header_std-ver
          rms_header_int-cd
          rms_header_company-id
          "No corresponding edcode for RMS header"  @ ws_message
        with frame f-rms-header 1 down width 132.
        skip_doc = true. /* unknown partner or set or version */
        next _main.
      END.
      ASSIGN
        dtl_rec_code = 40
        ws_edpotran_rec = ?
        ws_edpoline_rec = ?
        ws_edpoaddon_rec = ?
        ws_eddoc_rec = ?
        ws_docid = ""
        ws_edcode_rec = recid(edcode)
        ws_partner = edcode.partner
        ws_setid = edcode.setid
        .
    END.    /* separator rec, code = 0  */
    /* check to see if the current record is the first detail line.
    if so, create headers from data stored in variables.
    */
    IF ws_rec_code >= dtl_rec_code THEN
    DO:
      IF last_rec_code < dtl_rec_code THEN
      _doc_header:
      DO:
        FIND FIRST eddoc
          WHERE eddoc.partner = ws_partner
          AND eddoc.setid = ws_setid
          AND eddoc.docid = ws_docid
          /*
          AND eddoc.docseq = INTEGER(location_number)
          AND eddoc.isa = ws_isa
          AND eddoc.gs  = ws_gs
          AND eddoc.st  = ws_st
          AND eddoc.stat = 0
          */
          NO-ERROR.
        IF NOT AVAIL eddoc THEN
        DO:
          FIND edmast
            WHERE RECID(edmast) = ws_edmast_rec EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAIL edmast THEN
          DO:
            {rc/listadd.i erclist 201}
            erctoken[1] = ws_partner.
            LEAVE _doc_header.
          END.
          ASSIGN
            ws_customer = edmast.cust
            ws_isa = ?.
          FIND edcode WHERE RECID(edcode) = ws_edcode_rec EXCLUSIVE-LOCK
            NO-ERROR.
          IF NOT AVAIL edcode THEN
          DO:
            {rc/listadd.i erclist 301}
            erctoken[3] = "850".
            LEAVE _doc_header.
          END.
          run ed/gendoc.p (recid(edcode), ws_docid, output ws_eddoc_rec).
          find eddoc where recid(eddoc) = ws_eddoc_rec exclusive.
          ASSIGN
            eddoc.docseq        = INTEGER(location_number)
            eddoc.st-code       = location_number
            eddoc.status-flag   = "RCV"
            eddoc.isa           = ws_isa
            eddoc.gs            = ws_gs
            eddoc.st            = ws_st
            eddoc.fgsender      = rms_header_partner
            eddoc.setid         = rms_header_setid
            eddoc.version       = rms_header_std-ver
            eddoc.userref       = rms_header_int-cd
            eddoc.fgrecvid      = rms_header_company-id
            .
          IF debug THEN
          DO:
            MESSAGE "after create eddoc".
            PAUSE.
          END.
          {rc/incr.i ws_amt_added}.
        END.
        ELSE
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
            {rc/incr.i ws_amt_inerror}.
            skip_doc = TRUE.
          END.
          ELSE
          DO:
            PUT STREAM s-out UNFORMATTED
              " not yet processed - duplicate deleted" SKIP.
            RUN ed/fm850del.p (RECID(eddoc)).
            {rc/incr.i ws_amt_changed}.
          END.
        END.
        ws_eddoc_rec = RECID(eddoc).
        IF erclist = '' THEN
        DO:
          CREATE edpotran.
          ASSIGN
            edpotran.partner    = eddoc.partner
            edpotran.seq        = eddoc.seq
            edpotran.cust    = ws_customer
            edpotran.cust-po = purchase_order_number
            edpotran.cust-dept = department_number
            edpotran.scheduled-code1 = scheduled_code_1
            edpotran.ship-method-code = transportation_method_code
            edpotran.routing[1] = routing
            edpotran.vn-code = vendor_number
            edpotran.cust-div = sales_division /* 9704 CAH */
            edpotran.purpose-code   = transaction_purpose_code /* 9712 CAH */
            edpotran.order-type     = purchase_order_type      /* 9712 CAH */
            edpotran.ref3           = reference_3              /* 9712 CAH */
            edpotran.ref3-code      = reference_3_qualifier    /* 9712 CAH */
            ws_edpotran_rec = RECID(edpotran)
            {rc/incr.i ws_recs_added}
            edpotran.sf-code =
                if edmast.sf-code > "" then edmast.sf-code else edco.sf-code
            .
          RUN ed/fixidin.p (RECID(edmast), shipto_store_number,
            OUTPUT edpotran.st-code).
          RUN ed/fixidin.p (RECID(edmast), ordering_store_number,
            OUTPUT edpotran.by-code).
          if reference_2_qualifier begins "Z" then do:      /* 9712 CAH */
            run ed/fixidin.p (recid(edmast), reference_2, 
            output edpotran.ref2).
            ref2-code = reference_2_qualifier.
          end.
          {rc/xyymmdd.i purchase_order_date edpotran.order-date}
          {rc/xyymmdd.i cancel_date edpotran.cancel-date}
          {rc/xyymmdd.i ship_date   edpotran.request-date}
          ASSIGN
            cancel-date-code = IF cancel-date <> ? THEN "001" ELSE ""
            ship-date-code   = IF request-date <> ? THEN "010" ELSE ""
            .
        END.
      END.  /* first detail line, create headers */
      IF erclist = "" THEN
      DO:
        /* find headers if into detail lines */
        {rc/incr.i ws_recs_selected}.
        FIND edpotran WHERE RECID(edpotran) = ws_edpotran_rec EXCLUSIVE.
        FIND eddoc    WHERE RECID(eddoc)    = ws_eddoc_rec EXCLUSIVE.
        IF ws_rec_code = 40 THEN
        DO:
          ASSIGN
            {rc/substr.i  customer_line_number    03 3 integer} /* 9712 CAH */
            {rc/substr.i  quantity_ordered        32 5 INTEGER}
            {rc/substr.i  unit_of_measure         37 2}
            {rc/substr.i  unit_price              39 7 DECIMAL} / 100
            {rc/substr.i  customer_item_number    46 13}
            {rc/substr.i  item_description        59 20}
            {rc/substr.i  upc_code                101 13}       /* 9712 CAH */
            .
          CREATE edpoline.
          ASSIGN
            edpoline.partner = edpotran.partner
            edpoline.seq = edpotran.seq
            edpotran.lines = edpotran.lines + 1
            edpotran.last-line = edpotran.last-line + 1
            edpoline.line = edpotran.last-line
            edpoline.cust-po-line =
            IF customer_line_number > 0
            THEN STRING(customer_line_number)
            ELSE STRING(edpoline.line)
            edpoline.cust-item-no = customer_item_number
            edpoline.qty-orig-ord   = quantity_ordered
            edpoline.uom-code = unit_of_measure
            edpoline.unit-price = unit_price
            edpoline.description[1] = item_description
            edpoline.upc            = upc_code          /* 9712 CAH */
            ws_edpoline_rec = RECID(edpoline).
        END.
        ELSE
        DO:
          erctoken[9] = STRING(ws_rec_code).
          {rc/listadd.i erclist 2001}.
        END.
      END.
    END.
    ELSE
    DO:    /* header rec types accum into variables */
      IF ws_rec_code = 16 THEN
      DO:
        ASSIGN
          {rc/substr.i    purchase_order_number       5  8}
          {rc/substr.i    purchase_order_date         13 6}
          {rc/substr.i    department_number           42 3}
          {rc/substr.i    vendor_number               33 9}
          {rc/substr.i    sales_division              45 3}
          {rc/substr.i    cancel_date                 19 6}
          {rc/substr.i    ship_date                   25 6}
          {rc/substr.i    scheduled_code_1            31 2}
          {rc/substr.i    transaction_purpose_code    50 2} /* 9712 CAH */
          {rc/substr.i    purchase_order_type         55 2} /* 9712 CAH */
          {rc/substr.i    reference_3                 70 15} /* 9712 CAH */
          {rc/substr.i    ws_century                108 2 integer} /* 9712 */
          ws_docid = purchase_order_number
          .
          reference_3_qualifier =  /* 9712 CAH */
            if reference_3 > "" then "AE" else "".
      END.
      ELSE
      IF ws_rec_code = 30 THEN
      DO:
        ASSIGN
          {rc/substr.i    routing                       5 35}
          {rc/substr.i    transportation_method_code   54  2}
          {rc/substr.i    ordering_store_number   40 7}
          {rc/substr.i    shipto_store_number     47 7}
          {rc/substr.i    reference_2             70 7} /* 9712 CAH */
          .
          reference_2_qualifier =       /* 9712 CAH */
            if reference_2 > "" then "Z7" else "".
      END.
    END.
    IF erclist > '' THEN
    DO:
      {rc/incr.i ws_recs_inerror}.
      DISPLAY STREAM s-out
        ws_partner
        COLUMN-LABEL "PARTNER"
        purchase_order_number
        COLUMN-LABEL "PURCHASE ORDER"
        location_number  FORMAT 'X(08)'
        COLUMN-LABEL "LOCATION"
        record_type
        COLUMN-LABEL "RT"
        unique_order_number
        COLUMN-LABEL "UNIQUE ORD#"
        line_sequence_number
        COLUMN-LABEL "SEQ#"
        edi_transmission_control_number
        COLUMN-LABEL "ISA#"
        edi_funct_group_control_number
        COLUMN-LABEL "FG#"
        edi_set_control_number
        COLUMN-LABEL "SET#"
        edi_standard
        COLUMN-LABEL "STD"
        edi_version
        COLUMN-LABEL "VERSION"
        ws_setid
        COLUMN-LABEL "DOC"
        WITH FRAME f-det DOWN WIDTH 185.
      {rc/ercput.i "stream s-out"}
    END.
    last_rec_code = ws_rec_code.
  END. /* repeat */
  PUT STREAM s-out UNFORMATTED
    SKIP(1) "Purchase orders added: " ws_recs_added  FORMAT "-99999" SKIP.
  IF ws_amt_inerror > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Duplicates rejected  : " ws_amt_inerror FORMAT "-99999" SKIP.
  IF ws_amt_changed > 0 THEN
  PUT STREAM s-out UNFORMATTED
    SKIP    "Duplicated recycled  : " ws_amt_changed FORMAT "-99999" SKIP.
  INPUT STREAM s-in CLOSE.
  RUN rc/oscopy.p (curr_fid, archive_fid).  /* save the input file as .MDD */
  ws_char = temp_fid.                       /* delete quoter temp file */
  IF SEARCH(archive_fid) <> ? THEN
  DO:      /* verify file is backed up */
    PUT STREAM s-out UNFORMATTED
      SKIP(1) "Input file saved as: " archive_fid SKIP.
    {rc/listadd.i ws_char curr_fid}         /*  add it to deletion list */
  END.
  RUN rc/osdel.p (ws_char).                 /* deletes list of files */
END.
IF debug THEN
HIDE FRAME f-debug NO-PAUSE.
/*
----*----1----*----2----*----3----*----4----*----5----*----6----*----7----*----8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
*****SEARB850002040                                    SEARA
16  00201860970106      970115  002268340806092
30                                     00013870001387
40                             00001CT000260093315
40                             00001CT000277293316
*****SEARB850002040                                    SEARA
16  00201861970106      970115  002268340806092
30                                     00014050001405
40                             00001CT000239493314
40                             00001CT000277293316
40                             00001CT000315093317
*****SEARB850002040                                    SEARA
16  00201862970106      970109  002268340806092
30                                     00022240002224
40                             00001CT000116093271
40                             00001CT000150893272
*****SEARB850002040                                    SEARA
16  00771225970106      970109  002268340806092
30                                     00010140001014
40                             00030CT000116093271
40                             00050CT000150893272
40                             00007CT000106093284
40                             00025CT000178093285
40                             00020CT000122593291
*****SEARB850002040                                    SEARA
16  00771226970106      970115  002268340806092
30                                     00010140001014
40                             00005CT000239493314
40                             00005CT000260093315
40                             00005CT000277293316
40                             00005CT000315093317
40                             00005CT000239493318
*/
