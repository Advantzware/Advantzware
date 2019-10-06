/***************************************************************************\
*****************************************************************************
**  Program: E:\robj8\patch\ed\tdf\o8563020.p
**       By: Chris Heins, Report Concepts, Inc. (c) 1998 All rights reserved.
** Descript: Create Sterling TDF file for outbound Advance Ship Notice
**
10.23.98 by CAH on \\ricky\rv8 Log#0000:
1.  Added touchup of equipment-code to "TL" when trailer-num is nonblank.  
10.13.98 by CAH on \\ricky\rv8 Log#0000:
1.  Cleared line numbering in LIN on SEARS.
2.  Moved Item level HL into loop.  Required on each one.
10.08.98 by CAH on \\ricky\rv8 Log#0000:
1.  Initial write from o8563020.
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i       "new shared"}
{ed/tdf/sharedv.i   "new shared"}
{rc/ercvars.i       10 " " NEW}
{rc/stringv.i       "new shared"}
{rc/fcurrent.i}
{rc/stats.i}
DEF SHARED STREAM s-out.
DEF SHARED STREAM s-err.
DEF STREAM s-edi.
DEF VAR n_recs AS INT NO-UNDO.
DEF var ws_tax  AS decimal NO-UNDO.
DEF var ws_misc AS decimal NO-UNDO.
DEF var ws_line AS int NO-UNDO.
FORM
  WITH FRAME f-det DOWN width 144.
DEF var header_segment_list1        AS char NO-UNDO.
DEF var header_segment_list2        AS char NO-UNDO.
DEF var order_segment_list          AS char NO-UNDO.
DEF var detail_segment_list         AS char NO-UNDO.
DEF var tare_segment_list           AS char NO-UNDO.
DEF var pack_segment_list           AS char NO-UNDO.
DEF var trailer_segment_list        AS char NO-UNDO.
DEF var partner_segment_list        AS char NO-UNDO.
DEF var ctt_count_segment_list      AS char NO-UNDO.
def var shipment_HL                 as char no-undo.
def var order_HL                    as char no-undo.
/* 9810 CAH: REMOVED TD5,013 FROM DETAIL SEGMENTS, THIS IS OPTIONALLY USED
BY SEARS TO CANCEL REMAINING ORDER BALANCE.  NOT IMPLEMENTED YET 
10.08.98 by CAH on \\ricky\rv8 Log#0000:
1.  Removed PRF from detail segment list, not necessary.
2.  Removed SLN,006 from detail segment list, not necessary. 
*/
ASSIGN
  header_segment_list1
  = "000,000,BSN,002,HL,003,TD1,012,TD5,013,TD3,014,TD4,015"
  header_segment_list2 = "REF,016,PER,017,DTM,019,N1,021,N2,022,N3,023,N4,024"
  order_segment_list  = "HL,003,PRF,007,PID,009,MEA,010,PKG,011,TD1,012,REF,016,N1,021"
  tare_segment_list   = "HL,003,MAN,018"
  pack_segment_list   = "HL,003,PO4,008,MAN,018"
  detail_segment_list = "HL,003,LIN,004,SN1,005,PO4,008,REF,016"
  trailer_segment_list ="CTT,027"
  ctt_count_segment_list = "HL".
CASE ws_partner:
WHEN "SEARS" THEN
ASSIGN
  partner_segment_list =
  "000,ST,BSN,HL,TD5,TD3,REF,DTM,N1,N3,N4,PRF,TD1,MAN,LIN,SN1,SLN,CTT,SE".
OTHERWISE ASSIGN partner_segment_list = "*".
END CASE.
{ed/getpath.i}
ASSIGN
  ws_version = string(edcode.version,"9999")
  header_int-cd     = ws_docid
  header_isa        = 0
  header_gs         = 0
  header_st         = 0
  header_std-ver    = string(edcode.version,"999999")
  + IF edcode.agency = "V" THEN "VICS" ELSE ""
  header_std-rcvd   = ""
  header_std-used   =
  IF edcode.agency = "" OR edcode.agency = "V"
  THEN "X" ELSE caps(edcode.agency)
  header_rcvd-test-prod = ''
  header_part-test-prod = string(edcode.test-prod,"T/P")
  header_setid      = ws_setid
  header_partner    = ws_partner + ws_direction + ws_setid + header_part-test-prod
  vendor_number     = edmast.we-vend-no
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
{rc/hdg-wide.i "ed/tdf/o8563020.p" "OUTBOUND ASN EDIT LIST" "(s-out)"}
{rc/hdg-noco.i}
ASSIGN
  {rc/ctrtext.i hdg_name 40}
  {rc/ctrtext.i hdg_desc 40}
  hdg_text = "".
/* VIEW STREAM s-out FRAME hdg-std. */
OUTPUT STREAM s-edi TO VALUE(ws_edi_path) APPEND.
start = TIME.
FOR EACH eddoc EXCLUSIVE
    WHERE eddoc.setid = edcode.setid
    AND eddoc.partner = edcode.partner
    AND eddoc.error-count = 0
    AND eddoc.posted = FALSE
    AND NOT eddoc.status-flag = "DEL"   /* 9809 CAH */
    AND eddoc.direction = edcode.direction:
  ws_section = 0.
  ws_line = 0.
  DISPLAY
    eddoc.partner
    eddoc.setid
    eddoc.direction
    eddoc.seq
    eddoc.docid     FORMAT "x(22)"
    eddoc.userref   FORMAT "x(22)"
    WITH FRAME f-current.
  DISPLAY STREAM s-out
    eddoc.partner
    eddoc.seq
    eddoc.docid
    eddoc.userref
    WITH FRAME f-doc side-labels width 144.
  {rc/incr.i ws_recs_read}.
  FIND edshtran OF eddoc
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edshtran THEN
  DO:
    {rc/listadd.i erclist 1005}
    erctoken[6] = string(eddoc.seq).
  END.
  ELSE
  DO:
    /* 9705 CAH: Option to suppress ASN's on direct shipment to ordering store*/
    IF edmast.asn-on-ds = FALSE THEN
    DO:
      FIND FIRST edshline
        WHERE edshline.partner = edshtran.partner
        AND   edshline.seq = edshtran.seq
        AND   edshline.by-code <> edshtran.st-code NO-LOCK NO-ERROR.
      IF NOT AVAIL edshline THEN
      DO:
        {rc/listadd.i erclist 1006}
      END.
    END.
  END.
  IF erclist > '' THEN
  DO:
    {rc/ercput.i "stream s-out"}
    {rc/incr.i ws_recs_inerror}.
    NEXT.
  END.
  IF edshtran.carrier-code = ""
    THEN
  edshtran.carrier-code = "5014".
  
  /* 10.23.98 CAH: equipment code is mandatory field */
  if edshtran.trailer-number > ""
  and edshtran.equipment-code = ""
  then edshtran.equipment-code = "TL".  /* trailer */
  /* 9801 CAH: Added following code to make BSN unique on retransmission */
  shipment_id = IF EDSHTRAN.BOL-NO > "" THEN
  edshtran.bol-no
  ELSE
  STRING(EDSHTRAN.SEQ).
  IF eddoc.c-fatime > 0 THEN
  DO:
    shipment_id = shipment_id + "-" + string(eddoc.c-fatime).
  END.
  ship_time = string(edshtran.bol-addtime,"HH:MM").
  {rc/depunct.i ship_time}
  ASSIGN
    /* BSN */
    ws_docid                        = shipment_id
    header_int-cd                   = shipment_id
    transaction_purpose_code        =
    IF edshtran.purpose-code > "" THEN edshtran.purpose-code ELSE "00"
    manifest_create_date#           =
    IF edshtran.bol-adddate <> ? THEN edshtran.bol-adddate ELSE eddoc.adddate
    manifest_create_time            =
    IF ship_time > "" THEN ship_time ELSE "0000" /* HHMM */
    hl_structure_code               = "0004"    /* shipment order item */
    transaction_type_code           = ""
    status_reason_code              = ""
    /* HL Shipment */
    hl_id                           = "1"
    shipment_hl                     = hl_id
    hl_parent_id                    = ""
    hl_level_code                   = "S"
    hl_child_id                     = /* "Y" */ ""
    /* TD1 TD5 TD3 Carrier Details */
    packaging_code                  = ""
    lading_quantity                 = edshtran.tot-cartons
    weight_qual                     = "G"
    total_weight                    = edshtran.tot-wght
    weight_unit_measure             = "LB"
    total_volume                    = edshtran.tot-volume
    volume_unit_measure             = "CF"
    id_code_qualifier               = "2"   /* scac */
    carrier_scac_code               = "5014"
    /* edshtran.carrier-code /* 9810 CAH */ <= data no good ... */
    equipment_code                  = edshtran.equipment-code
    equipment_initial               = edshtran.equipment-initial
    trailer_number                  = edshtran.trailer-number
    number_of_line_items            = 0
    ws_seq = 0  /* 9810 CAH */
    .
  error-status:error = FALSE.
  RUN write_segments.ip (header_segment_list1).
  /* 9810 CAH: Had to break the segments as the both td5 and n1 use
  id code and qualifier  */
  ASSIGN
    /* N1 N2 N3 N4 */
    entity_id                       = "ST"
    company_name                    = ""
    id_code_qualifier               = "92"
    id_code                         = edshtran.st-code
    .
  error-status:error = FALSE.
  RUN write_segments.ip ("N1,021").
  ASSIGN
    /* REF */
    ref_qual                        = "BM"
    pro_number                      = edshtran.pro-number
    bill_of_lading_number           = edshtran.bol-no
    /* DTM */
    date_qual                       = "011"
    /*
    IF edshtran.ship-date-code > ""
    THEN edshtran.ship-date-code
    ELSE "011"
    */
    manifest_create_date#           = edshtran.ship-date
    ship_date#                      = edshtran.ship-date    /* 9810 CAH */
    /* n1 SF ship from company name */
    entity_id                       = "SF"
    company_name                    = WS_CO_NAME
    id_code_qualifier               = ""
    id_code                         = ""
    .
  {rc/incr.i ws_recs_selected}.
  DISPLAY STREAM s-out
    EDSHTran.purpose-code
    EDSHTran.BOL-No
    string(eddoc.c-fatime) LABEL "Reset Cnt"
    EDSHTran.BOL-Adddate
    string(EDSHTran.BOL-Addtime,"HH:MM") LABEL "BOL-Addtime"
    EDSHTran.lines
    EDSHTran.sf-code
    EDSHTran.Ship-Date
    EDSHTran.carrier
    EDSHTran.carrier-code
    EDSHTran.Trailer-Number
    EDSHTran.st-code
    EDSHTran.Ship-name
    EDSHTran.Ship-address[1]
    EDSHTran.Ship-address[2]
    EDSHTran.Ship-address[3]
    EDSHTran.Ship-city
    EDSHTran.Ship-st
    EDSHTran.Ship-zip
    EDSHTran.Ship-country
    EDSHTran.routing[1]
    EDSHTran.routing[2]
    EDSHTran.routing[3]
    EDSHTran.routing[4]
    EDSHTran.Pro-Number
    EDSHTran.del-date
    EDSHTran.tot-wght
    EDSHTran.tot-cartons
    EDSHTran.tot-volume
    EDSHTran.Package-Code
    EDSHTran.vn-code
    EDSHTran.Ship-Stat
    WITH FRAME f-edshtran 3 COLUMNS width 144.
  error-status:error = FALSE.
  RUN write_segments.ip (header_segment_list2).
  IF pro_number > "" THEN
  DO:
    ref_qual = "CN".
    RUN write_segments.ip ("REF,016").
  END.
  ASSIGN ref_qual = "" ref_number = "".
  FOR EACH edshline OF edshtran EXCLUSIVE
      WHERE edshline.qty-shipped <> 0
      BREAK BY edshline.cust-po BY edshline.cust-po-line:
    ws_line = ws_line + 1.
    IF FIRST-OF (edshline.cust-po) THEN
    DO: /* order level segments */
      FIND FIRST edshord
        WHERE edshord.partner   = edshline.partner
        AND edshord.seq       = edshline.seq
        AND edshord.cust-po   = edshline.cust-po
        AND edshord.order-no  = edshline.order-no
        EXCLUSIVE-LOCK /* 10.22.98 CAH WAS NO LOCK BUT THIS RECORD
            MIGHT BE TOUCHED UP IF IT IS MISSING DIV OR DEPT */
        NO-ERROR.
      IF edshline.cust-po > "" THEN
      DO:
        FIND FIRST edpotran
          WHERE edpotran.partner = edshord.partner
          AND edpotran.cust-po = edshord.cust-po NO-LOCK NO-ERROR.
      END.  /* lookup po */
      IF AVAIL edshord THEN
      DO:
        IF AVAIL edpotran then do:  /* 981013 CAH */
            if edshord.cust-div = '' and edpotran.cust-div > ''
            then edshord.cust-div = edpotran.cust-div.
            if edshord.cust-dept = '' and edpotran.cust-dept > ''
            then edshord.cust-dept = edpotran.cust-dept.
        end.
        ASSIGN
          /* HL order level */
          hl_parent_id                = shipment_hl
          hl_id                       = string(integer(hl_id) + 1)
          order_HL                    = hl_id
          hl_level_code               = "O"
          hl_child_id                 = /* "Y" */ ""
          /* PRF */
          purchase_order_number       = edshord.cust-po
          purchase_order_date#        = edshord.cust-po-date
          /* TD1 Quantity and Weight */
          packaging_code                  = ""
          lading_quantity                 = edshord.tot-cartons
          weight_qual                     = "G"
          total_weight                    = edshord.tot-wght
          weight_unit_measure             = "LB"
          total_volume                    = edshord.tot-volume
          volume_unit_measure             = "CF"
          /* REF */
          /* N1 */
          ordering_store_number           = edshord.by-code
          company_name                    = ""
          entity_id                       = "BY"
          id_code_qualifier               = "92"
          id_code                         = ordering_store_number
          .
        CASE ws_partner:
        WHEN "SEARS" THEN do:
          if edshord.cust-dept = "" then edshord.cust-dept = "806".
          if edshord.cust-div  = "" then edshord.cust-div  = "092".
        
        ASSIGN
          /* for PID */
          item_description_type           = "S"
          product_characteristic_code     = ""
          sac_agency_qualifier            = "AB"
          product_description_code        = "FL" /* Fair Labor Standards Act */
          /* REF */
          ref_qual                        = "DP"
          department_number               =
          IF edshord.cust-dept = "806" 
          and edshord.cust-div = "092"
          THEN edshord.cust-div
          else if edshord.cust-div > ""
          then edshord.cust-div
          else "092"
          /* ELSE edshord.cust-dept */
          ref_desc                        = ""
          .
        end.    /* do when sears */  
      END CASE.
      DISPLAY STREAM s-out
        EDSHOrd.Cust-po
        EDSHOrd.Cust-po-date
        EDSHOrd.By-code
        EDSHOrd.Cust-div
        EDSHOrd.Cust-dept
        EDSHOrd.Lines
        EDSHOrd.Last-line
        EDSHOrd.Order-no
        EDSHOrd.Tot-wght
        EDSHOrd.Tot-cartons
        EDSHOrd.Tot-volume
        EDSHOrd.Package-Code
        EDSHOrd.Ship-Stat
        WITH FRAME f-edshord 3 COLUMNS COLUMN 6 width 144.
      ws_section = ws_section + 1.
      ws_line = 0.
      RUN write_segments.ip (order_segment_list).
    END. /* avail order */
    CASE ws_partner:
    WHEN "SEARS" THEN
    DO:
      ASSIGN
        ref_qual = "IA"
        ref_number = vendor_number.
      RUN write_segments.ip ("REF,016").
      /*
      ASSIGN
      ref_qual                        = "19"
      sales_division                  = edshord.cust-div.
      RUN write_segments.ip ("REF,016").
      */
      ASSIGN
        /* n1 ZZ MARK-FOR STORE, SAME AS BY STORE */
        entity_id                       = "ZZ"
        company_name                    = ""
        id_code_qualifier               = "92"
        id_code                         = ordering_store_number
        .
      error-status:error = FALSE.
      RUN write_segments.ip ("N1,021").
    END. /* SEARS */
  END CASE.
  ws_section = ws_section + 1.
END.    /* order break */
ASSIGN
  /* HL Item level */
  hl_parent_id                = order_hl
  hl_id                       = string(integer(hl_id) + 1)
  hl_level_code               = "I"
  hl_child_id                 = /* "N" */ ""
  /* LIN */
  item_assigned_id                    = edshline.cust-po-line
  item_product_qualifier              = "CB"
  customer_item_number                = edshline.cust-item-no
  product_id                          = edshline.cust-item-no
  upc_code                            = edshline.upc
  /* SN1 */
  quantity_shipped                    = edshline.qty-shipped
  quantity_ordered                    = edshline.qty-orig-ord
  quantity_cumulative                 = edshline.qty-shipped
  unit_of_measure                     = edshline.uom-code
  unit_of_measure2                    = edshline.uom-code
  item_status_code                    = edshline.ship-stat
  /* SLN ... not implemented */
  ws_line = ws_line + 1
  /* REF */
  ref_qual = ""          /* clear this else it generates */
  .
if ws_partner = "SEARS" then do:
    /* 9810 CAH per Dunia Grasso of Sears: */
    if unit_of_measure = "CS" 
    then assign unit_of_measure = "EA" unit_of_measure2 = "EA".
    item_assigned_id = "".
end.    
DISPLAY STREAM s-out
  EDSHLine.cust-po-line
  EDSHLine.Item-no
  EDSHLine.Qty-orig-ord
  EDSHLine.qty-shipped
  EDSHLine.unit-price
  EDSHLine.Uom-code
  EDSHLine.pack-size
  EDSHLine.case-wght
  EDSHLine.cust-item-no
  EDSHLine.UPC
  EDSHLine.tot-wght
  EDSHLine.tot-cartons
  EDSHLine.tot-volume
  EDSHLine.Ship-Stat
  EDSHLine.Carton-mark
  EDSHLine.Pallet-mark
  WITH FRAME f-edshline 3 COLUMNS COLUMN 11 width 144.
RUN write_segments.ip (detail_segment_list).
END.
ws_section = ws_section + 1.
RUN write_segments.ip (trailer_segment_list).
RUN write_tdf.ip.
ASSIGN
  {rc/stampcht.i eddoc}
  eddoc.openitem = FALSE
  eddoc.stat = 9
  eddoc.status-flag = "SNT"
  eddoc.posted = TRUE
  {rc/incr.i ws_recs_changed}
  .
PUT STREAM s-out SKIP(1) "Document header marked: " eddoc.status-flag SKIP.
IF error-status:error OR ws_erc <> 0
  THEN
{rc/incr.i ws_recs_inerror}.
{rc/accum.i ws_amt_changed edshtran.tot-cartons}.
/* 9810 CAH ... to prevent screen flasing ...
{rc/statsdis.i}
*/
END.
OUTPUT STREAM s-edi CLOSE.
/* top-debug = FALSE. */
HIDE FRAME f-current NO-PAUSE.
{rc/statsdis.i}
VIEW STREAM s-out FRAME f-stats.
HIDE FRAME f-stats NO-PAUSE.
PAGE STREAM s-out.
RETURN.
{ed/tdf/writeseg.i}
