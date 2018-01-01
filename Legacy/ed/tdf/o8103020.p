/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\tdf\o8103020.p
**       By: Chris Heins (c) 1998 Report Concepts, Inc.
** Descript: Output 810 to Sterling TDF
10.08.98 by CAH on \\ricky\rv8 Log#0000:
1.  sales_division goes through REF SD not REF 19 as in 3060 per sears.
09.01.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added line to work table to allow for correct segment sequencing within
then detail area.
07.15.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added edit list report, summary, invoice level.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i       "new shared"}
{ed/tdf/sharedv.i   "new shared"}
{rc/stringv.i       "new shared"}
{rc/fcurrent.i}
{rc/stats.i}
DEF SHARED STREAM s-out.
DEF SHARED STREAM s-err.
DEF STREAM s-edi.
DEF VAR n_recs AS INT NO-UNDO.
DEF VAR ws_tax  AS DECIMAL NO-UNDO.
DEF VAR ws_misc AS DECIMAL NO-UNDO.
DEF var ws_line AS int NO-UNDO.
DEF VAR header_segment_list AS CHAR NO-UNDO.
DEF VAR detail_segment_list AS CHAR NO-UNDO.
DEF VAR trailer_segment_list AS CHAR NO-UNDO.
DEF VAR partner_segment_list AS CHAR NO-UNDO.
DEF var ctt_count_segment_list      AS char NO-UNDO.
ASSIGN
  header_segment_list =
  "000,000,BIG,002,CUR,003,REF,004,PER,005,N1,006,N2,007,N3,008,N4,009,ITD,010,DTM,011,FOB,012,PID,017"
  detail_segment_list =
  "IT1,013,IT3,014,CTP,015,MEA,016,MEA,018,PO4,019,REF,020,ITA,021,SLN,022,TC2,023"
  trailer_segment_list =
  "TDS,024,CAD,025,ITA,026,ISS,027,CTT,028"
  ctt_count_segment_list = "IT1".
CASE ws_partner:
WHEN "SEARS" OR WHEN "SEARS" THEN
ASSIGN
  partner_segment_list = "000,ST,BIG,REF,N1,ITD,DTM,PID,IT1,SAC,TDS,ITA,CTT,SE".
OTHERWISE ASSIGN
  partner_segment_list = "*".
END CASE.
{ed/getpath.i}
ASSIGN
  ws_version        = STRING(edcode.version,"9999")
  header_int-cd     = ws_docid
  header_fgid       = "IV"
  header_isa        = 0
  header_gs         = 0
  header_st         = 0
  header_std-ver    = STRING(edcode.version,"999999")
  + IF edcode.agency = "V" THEN "VICS" ELSE ""
  header_std-rcvd   = ""
  header_std-used   =
  IF edcode.agency = "" OR edcode.agency = "V"
  THEN "X" ELSE CAPS(edcode.agency)
  header_rcvd-test-prod = ''
  header_part-test-prod = STRING(edcode.test-prod,"T/P")
  header_setid      = ws_setid
  header_partner    = ws_partner + ws_direction + ws_setid + header_part-test-prod
  .
/* 9808 CAH: To prevent superfluous page headings */
FIND FIRST eddoc NO-LOCK
  WHERE eddoc.setid = edcode.setid
  AND eddoc.partner = edcode.partner
  AND eddoc.error-count = 0
  AND eddoc.posted = FALSE
  AND NOT eddoc.status-flag = "DEL"   /* 9809 CAH */
  AND eddoc.direction = edcode.direction NO-ERROR.
IF NOT AVAIL eddoc THEN
RETURN. /* nothing to do */
{rc/hdg-wide.i "ed/tdf/o8103020.p" "OUTBOUND INVOICE EDIT LIST" "(s-out)"}
{rc/hdg-noco.i}
ASSIGN
  {rc/ctrtext.i hdg_name 40}
  {rc/ctrtext.i hdg_desc 40}
  hdg_text = "".
FORM
  WITH FRAME f-det DOWN WIDTH 144.
OUTPUT STREAM s-edi TO VALUE(ws_edi_path) APPEND.
START = TIME.
FOR EACH eddoc EXCLUSIVE
    WHERE eddoc.setid = edcode.setid
    AND eddoc.partner = edcode.partner
    AND eddoc.error-count = 0
    AND eddoc.posted = FALSE
    AND eddoc.direction = edcode.direction
    AND NOT eddoc.status-flag = "DEL"   /* 9809 CAH */:
  ws_section = 1.
  DISPLAY
    eddoc.partner
    eddoc.setid
    eddoc.direction
    eddoc.seq
    eddoc.docid     FORMAT "x(22)"
    eddoc.userref   FORMAT "x(22)"
    WITH FRAME f-current.
  {rc/incr.i ws_recs_read}.
  FIND edivtran OF eddoc
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edivtran THEN
  DO:
    ws_erc = 3001.
    ws_erc_desc = ",,,,," + string(eddoc.seq).
    {rc/incr.i ws_recs_inerror}.
    RETURN.
  END.
  IF edivtran.cust-po > "" THEN
  DO:
    FIND FIRST edpotran
      WHERE edpotran.partner = edivtran.partner
      AND edpotran.cust-po = edivtran.cust-po NO-LOCK NO-ERROR.
    IF AVAIL edpotran
      THEN
    DO:
      CASE ws_partner:
      WHEN "SEARS" THEN
      ASSIGN
        afe_number =
        IF edpotran.ref2-code = "AE" THEN edpotran.ref2 ELSE "14257680".
      /* 9810 CAH no zz-code in ASI schema ...
      edpotran.zz-code.    /* there is no afe-code right now */
      */
    END CASE.
  END.    /* avail edpotran */
END.  /* lookup po */
ASSIGN
  /* BIG */
  invoice_number                  = edivtran.invoice-no
  invoice_date#                   = edivtran.invoice-date
  purchase_order_number           = edivtran.cust-po
  purchase_order_date#            = edivtran.cust-po-date
  alt_invoice_number              = ""
  /* REF */
  division_number                 = edivtran.cust-div
  sales_division                  = edivtran.cust-div
  department_number               = edivtran.cust-dept
  vendor_number                   = edivtran.vn-code
  promotion_event_code            = edivtran.promo-code
  remit_number                    =
  IF edivtran.re-code > "" THEN edivtran.re-code ELSE edmast.re-code
  ordering_store_number           = edivtran.by-code
  weight_unit_measure             = edivtran.wght-uom
  /* N1 */
  entity_id                       = "BY"
  id_code_qualifier               = "92"
  id_code                         = ordering_store_number
  /* ITD */
  terms_type                      = edivtran.terms-type
  terms_basis                     = edivtran.terms-basis
  discount_percent                = edivtran.terms-disc-pct * 1000
  discount_due_date#              = edivtran.terms-disc-date
  discount_due_days               = edivtran.terms-disc-days
  net_date#                       = edivtran.terms-net-date
  net_days                        = edivtran.terms-net-days
  discount_amount                 = edivtran.terms-disc-amt * 100
  /* 000 */
  ws_docid                        = edivtran.invoice-no
  header_int-cd     = ws_docid
  /* TDS */
  total_amount1                   = edivtran.tot-gross
  number_of_line_items            = 0
  .
CASE ws_partner:
WHEN "SEARS" THEN
DO:
  ASSIGN
    /* for PID */
    item_description_type           = "S"
    product_characteristic_code     = ""
    sac_agency_qualifier            = "AB"
    product_description_code        = "FL" /* Fair Labor Standards Act */
    ref_qual                        = "DP"
    ref_number                      = department_number
    ref_desc                        = ""
    sales_division = 
        if edivtran.cust-div = "" and edivtran.cust-dept = "806"
        then "092" 
        else sales_division.
    .
END.  /* SEARS */
END CASE.
{rc/incr.i ws_recs_selected}.
error-status:error = FALSE.
RUN write_segments.ip (header_segment_list).
CASE ws_partner:
WHEN "SEARS" THEN
DO:
  ASSIGN
    ref_qual = "IA"
    ref_number = vendor_number.
  RUN write_segments.ip ("REF,004").
  IF promotion_event_code > "" THEN
  DO:
    ASSIGN
      ref_qual = "PD"
      ref_number = promotion_event_code.
    RUN write_segments.ip ("REF,004").
  END.
  IF sales_division > "" THEN
  DO:
    ASSIGN
      ref_qual = "SD"
      ref_number = sales_division.
    RUN write_segments.ip ("REF,004").
  END.
  IF afe_number > "" THEN
  DO:
    ASSIGN
      ref_qual = "AE"
      ref_number = afe_number.
    RUN write_segments.ip ("REF,004").
  END.
  ASSIGN
    entity_id                  = "RE"
    id_code_qualifier               = "92"
    id_code                         = remit_number.
  RUN write_segments.ip ("N1,006").
END. /* SEARS */
END CASE.
ASSIGN ref_qual = "" ref_number = "".
ws_section = 2.
ws_line = 0.
FOR EACH edivline OF edivtran EXCLUSIVE
    WHERE edivline.qty-shipped <> 0:
  ASSIGN
    customer_item_number        = edivline.cust-item-no
    product_id                  = customer_item_number
    item_product_qualifier      = "CB"
    quantity_invoiced           = edivline.qty-shipped
    unit_price                  = edivline.unit-price
    unit_of_measure             = edivline.uom-code
    weight_unit_measure         = "LB"
    volume_unit_measure         = "CF"
    ws_line = ws_line + 1
    .
    
  /* 9810 CAH ... */
  if ws_partner begins "SEAR" then do:
    if unit_of_measure = "" 
    or unit_of_measure = "CT" 
    or unit_of_measure = "CS"
    then unit_of_measure = "EA".
  end.
  RUN write_segments.ip (detail_segment_list).
END.
ws_section = 3.
ws_line = 0.
n_recs = 0.
FOR EACH edivaddon OF edivtran EXCLUSIVE:
  RUN write_segments.ip ("SAC,027").
END.
RUN write_segments.ip (trailer_segment_list).
RUN write_tdf.ip.
ASSIGN
  {rc/stampcht.i eddoc}
  eddoc.openitem = FALSE
  eddoc.stat = 9
  eddoc.status-flag = "SNT"
  eddoc.posted = TRUE
  {rc/incr.i ws_recs_changed}
  ws_line = ws_line + 1
  .
DISPLAY STREAM s-out
  eddoc.partner               COLUMN-LABEL "Partner"
  eddoc.setid                 COLUMN-LABEL "Set"
  eddoc.seq                   COLUMN-LABEL "Seq#"
  edivtran.invoice-no
  edivtran.invoice-date       COLUMN-LABEL "Date"
  edivtran.cust-po            COLUMN-LABEL "Customer PO#"
  edivtran.tot-gross          COLUMN-LABEL "Amount"
  ws_tax                      COLUMN-LABEL "Tax"
  edivtran.tot-frt            COLUMN-LABEL "Freight"
  ws_misc                     COLUMN-LABEL "Misc."
  WITH FRAME f-det DOWN.
DOWN STREAM s-out WITH FRAME f-det.
ACCUMULATE
  edivtran.invoice-no (count)
  edivtran.tot-gross (total)
  edivtran.tot-frt (total)
  ws_misc (total).
IF error-status:error OR ws_erc <> 0
  THEN
{rc/incr.i ws_recs_inerror}.
{rc/accum.i ws_amt_changed edivtran.tot-gross}.
/* 9810 CAH: was causing screen flip ...
{rc/statsdis.i}
*/
END.
IF (ACCUM count edivtran.invoice-no) > 0 THEN
DO WITH FRAME f-det:
  UNDERLINE STREAM s-out
    edivtran.invoice-no
    edivtran.tot-gross
    edivtran.tot-frt
    ws_misc.
  DOWN STREAM s-out.
  DISPLAY STREAM s-out
    ACCUM count edivtran.invoice-no @ edivtran.invoice-no
    ACCUM total edivtran.tot-gross  @ edivtran.tot-gross
    ACCUM total edivtran.tot-frt    @ edivtran.tot-frt
    ACCUM total ws_misc             @ ws_misc.
END.
OUTPUT STREAM s-edi CLOSE.
HIDE FRAME f-current NO-PAUSE.
IF ws_recs_selected > 0 THEN
DO:
  DOWN STREAM s-out WITH FRAME f-det.
  PAGE.
END.
{rc/statsdis.i}
VIEW STREAM s-out FRAME f-stats.
HIDE FRAME f-stats NO-PAUSE.
PAGE STREAM s-out.
RETURN.
{ed/tdf/writeseg.i}
