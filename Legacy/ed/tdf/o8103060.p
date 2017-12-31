/****************************************************************************
*****************************************************************************
**  Program: D:\RPRODEV\ED\tdf\o8103060.p
**       By: Chris Heins (c) 1998 Report Concepts, Inc.
** Descript: Output 810 to Sterling TDF
10.09.98 by CAH on \\ricky\rv8 Log#0000:
1.  Replaced division_number with sales_division which is the field
in the tdf/ref thread procedure.
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
DEFINE SHARED STREAM s-out.
DEFINE SHARED STREAM s-err.
DEFINE STREAM s-edi.
DEFINE VARIABLE n_recs                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE ws_tax                 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ws_misc                AS DECIMAL NO-UNDO.
DEFINE VARIABLE ws_line                AS INTEGER     NO-UNDO.
DEFINE VARIABLE header_segment_list    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE detail_segment_list    AS CHARACTER    NO-UNDO.
DEFINE VARIABLE trailer_segment_list   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE partner_segment_list   AS CHARACTER    NO-UNDO.
DEFINE VARIABLE ctt_count_segment_list AS CHARACTER    NO-UNDO.
DEFINE VARIABLE lPathIsGood AS LOGICAL NO-UNDO.

ws_filetype = "EDI".


ASSIGN
  header_segment_list =
  "000,000,BIG,002,CUR,003,REF,004,PER,005,N1,006,N2,007,N3,008,N4,009,ITD,010,DTM,011,FOB,012,PID,017"
  detail_segment_list =
  "IT1,013,PID,014,IT3,015,CTP,016,MEA,017,MEA,019,PO4,020,REF,021,SDQ,022,SAC,023,SLN,024,TC2,025"
  trailer_segment_list =
  "TDS,025,CAD,026,SAC,027,TXI,028,ISS,029"
    ctt_count_segment_list = "IT1".
/*CASE ws_partner:                                                          */
/*WHEN "AMAZ" OR WHEN "SEARS" THEN                                          */
/*ASSIGN                                                                    */
/*  partner_segment_list = "000,ST,BIG,CUR,N1,N3,N4,IT1,SAC,TDS,SAC,TXI,SE".*/
/*OTHERWISE ASSIGN                                                          */
/*  partner_segment_list = "*".                                             */
/*END CASE.                                                
                 */
                
FIND FIRST EDMast NO-LOCK WHERE EDMast.Partner EQ ws_partner NO-ERROR.
IF NOT AVAILABLE EDMast THEN 
    RETURN. 
FIND FIRST ediPartnerSegment NO-LOCK  
    WHERE ediPartnerSegment.partnerGrp EQ EDMast.partnerGrp
    NO-ERROR.
IF NOT AVAILABLE ediPartnerSegment THEN 
    partner_segment_list = "*".
ELSE 
DO:
    partner_segment_list = "000". 
    FOR EACH ediPartnerSegment NO-LOCK  
        WHERE ediPartnerSegment.partnerGrp EQ EDMast.partnerGrp 
        BREAK BY ediPartnerSegment.segmentCode  :
        IF FIRST-OF(ediPartnerSegment.segmentCode) THEN 
            partner_segment_list = partner_segment_list + "," + ediPartnerSegment.segmentCode.  
    END.
END. 

{ed/getpath.i}
ASSIGN
    ws_version            = STRING(edcode.version,"9999")
    header_int-cd         = ws_docid
    header_fgid           = "IV"
    header_isa            = 0
    header_gs             = 0
    header_st             = 0
    header_std-ver        = STRING(edcode.version,"999999")
  + IF edcode.agency = "V" THEN "VICS" ELSE ""
    header_std-rcvd       = ""
    header_std-used       = IF edcode.agency = "" OR edcode.agency = "V"
  THEN "X" ELSE CAPS(edcode.agency)
    header_rcvd-test-prod = ''
    header_part-test-prod = STRING(edcode.test-prod,"T/P")
    header_setid          = ws_setid
    header_partner        = ws_partner + ws_direction + ws_setid + header_part-test-prod
    .
/* 9808 CAH: To prevent superfluous page headings */
FIND FIRST eddoc NO-LOCK
    WHERE eddoc.setid = edcode.setid
    AND eddoc.partner = edcode.partner
    AND eddoc.error-count = 0
    AND eddoc.posted = FALSE
    AND NOT eddoc.status-flag = "DEL"   /* 9809 CAH */
    AND eddoc.direction = edcode.direction NO-ERROR.
IF NOT AVAILABLE eddoc THEN
    RETURN. /* nothing to do */
{rc/hdg-wide.i "ed/tdf/o8103060.p" "OUTBOUND INVOICE EDIT LIST" "(s-out)"}
{rc/hdg-noco.i}
ASSIGN
  {rc/ctrtext.i hdg_name 40}
  {rc/ctrtext.i hdg_desc 40}
    hdg_text = "".
FORM
    WITH FRAME f-det DOWN WIDTH 144.
    
ws_edi_path = ws_edi_path +  fOutputFileName().
lPathIsGood = fCheckFolderOfPath(ws_edi_path).
IF NOT lPathIsGood THEN 
    RETURN.
OUTPUT STREAM s-edi TO VALUE(ws_edi_path).    

START = TIME.
FOR EACH eddoc EXCLUSIVE
    WHERE eddoc.setid = edcode.setid
      AND eddoc.partner = ws_partner
      AND eddoc.error-count = 0
      AND eddoc.posted = FALSE
      AND eddoc.direction = edcode.direction
AND NOT eddoc.status-flag = "DEL"   /* 9809 CAH */:
    ws_section = 10.
    /*
    DISPLAY
      eddoc.partner
      eddoc.setid
      eddoc.direction
      eddoc.seq
      eddoc.docid     FORMAT "x(22)"
      eddoc.userref   FORMAT "x(22)"
      WITH FRAME f-current.
      */
     
    /* Notify UI to display current being processed */
    PUBLISH "EDIEVENT" ( "Partner: " + eddoc.partner + " SetID:  " + eddoc.setid 
           + " Direction:  " + eddoc.direction + " Seq:  " +    string(eddoc.seq)) .
    
  {rc/incr.i ws_recs_read}.
    FIND edivtran OF eddoc
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE edivtran THEN
    DO:
        ws_erc = 3001.
        ws_erc_desc = ",,,,," + string(eddoc.seq).
        {rc/incr.i ws_recs_inerror}.
        RETURN.
    END.
    FIND FIRST edshipto
        WHERE edshipto.partner = EDIVTran.Partner
        AND edshipto.ref-type = "BT"
        AND edshipto.cust = EDIVTran.Cust
        /* AND edshipto.ship-to = EDIVTran.By-code */
        NO-LOCK NO-ERROR.
  
    IF edivtran.cust-po > "" THEN
    DO:
        FIND FIRST edpotran
            WHERE edpotran.partner = edivtran.partner
            AND edpotran.cust-po = edivtran.cust-po NO-LOCK NO-ERROR.
        IF AVAILABLE edpotran
            THEN
        DO:
            CASE ws_partner:
                WHEN "SEARS" THEN
                    ASSIGN
                        afe_number = IF edpotran.ref2-code = "AE" THEN edpotran.ref2 ELSE "14257680".
            /* 9810 CAH no zz-code in ASI schema ...
            edpotran.zz-code.    /* there is no afe-code right now */
            */
            END CASE.
        END.    /* avail edpotran */
    END.  /* lookup po */

    ASSIGN
        /* BIG */
        invoice_number        = edivtran.invoice-no
        invoice_date#         = edivtran.invoice-date
        /* temp purchase_order_number           = edivtran.cust-po
         purchase_order_date#            = edivtran.cust-po-date */
        alt_invoice_number    = ""
        /* CUR */
        currency_denom_seller = IF edivTran.curr-seller EQ "" THEN "USD"
                                    ELSE EDIVTran.Curr-seller
  
        /* REF */
        division_number       = edivtran.cust-div
        sales_division        = edivtran.cust-div
        department_number     = edivtran.cust-dept
        vendor_number         = edivtran.vn-code
        promotion_event_code  = edivtran.promo-code
        remit_number          = IF edivtran.re-code > "" THEN edivtran.re-code ELSE edmast.re-code
        ordering_store_number = edivtran.by-code
        weight_unit_measure   = edivtran.wght-uom
  
        /* N1 */
        entity_id             = "BT"
        company_name          = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE ""
        id_code_qualifier     = "92"
        id_code               = IF AVAILABLE EDShipto THEN EDShipto.cust ELSE ordering_store_number

        /* N2 */
        /* name2 = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE "" */
  
        /* N3 */
        address1              = IF AVAILABLE EDShipto THEN EDShipto.Addr1 ELSE ""
        address2              = IF AVAILABLE EDShipto THEN EDShipto.Addr2 ELSE ""
  
        /* N4 */
        city                  = IF AVAILABLE EDShipto THEN EDShipto.City ELSE ""
        state                 = IF AVAILABLE EDShipto THEN EDShipto.State ELSE ""    
        country               = IF AVAILABLE EDShipto THEN EDShipto.Country ELSE ""
        zip                   = IF AVAILABLE EDShipto THEN EDShipto.Zip ELSE ""
  
        /* ITD */
        terms_type            = edivtran.terms-type
        terms_basis           = edivtran.terms-basis
        discount_percent      = edivtran.terms-disc-pct * 1000
        discount_due_date#    = edivtran.terms-disc-date
        discount_due_days     = edivtran.terms-disc-days
        net_date#             = edivtran.terms-net-date
        net_days              = edivtran.terms-net-days
        discount_amount       = edivtran.terms-disc-amt * 100
        /* 000 */
        ws_docid              = edivtran.invoice-no
        header_int-cd         = ws_docid
        /* TDS */
        total_amount1         = edivtran.tot-gross
        number_of_line_items  = 0
        .
    CASE ws_partner:
        WHEN "SEARS" THEN
            DO:
                ASSIGN
                    /* for PID */
                    item_description_type       = "S"
                    product_characteristic_code = ""
                    sac_agency_qualifier        = "AB"
                    product_description_code    = "FL" /* Fair Labor Standards Act */
                    ref_qual                    = "DP"
                    ref_number                  = department_number
                    ref_desc                    = ""
                    sales_division              = IF edivtran.cust-div = "" AND edivtran.cust-dept = "806"
                                                    THEN "092" 
                                                    ELSE sales_division
                    .
                .
            END.  /* SEARS */
    END CASE.
    /* top-debug = TRUE. */
    {rc/incr.i ws_recs_selected}.
    ERROR-STATUS:ERROR = FALSE.

    RUN write_segments.ip (header_segment_list).
    CASE ws_partner_grp:
        WHEN "AMAZ" THEN
            DO:
                FIND FIRST EDShipto NO-LOCK WHERE EDShipto.cust EQ EDIVTran.cust
                    AND EDShipto.Partner EQ EDIVTran.partner
                    AND EDShipto.ship-to EQ EDIVTran.By-code 
                    NO-ERROR.
                ASSIGN
                    entity_id         = "RI"
                    id_code_qualifier = "92" 
                    id_code           = remit_number
                    company_name      = 'PREMIER PACKAGING'
                    .
    
                IF edshipto.siteID GT "" THEN 
                    ASSIGN 
                        id_code_qualifier = "92"
                        id_code           = edshipto.siteID
                        .
    
                ASSIGN 
                    name2    = 'PREMIER PACKAGING'
    
                    /* N3 */
                    address1 = '3254 RELIABLE PARKWAY'
    
                    /* N4 */
                    city     = 'CHICAGO'
                    state    = 'IL'
                    country  = 'US'
                    zip      = '60686' 
                    .    
                IF id_code EQ "" THEN 
                    id_code_qualifier = "".
                ws_section = 21.
                RUN write_segments.ip ("N1,006").
                RUN write_segments.ip ("N3,008").
                RUN write_segments.ip ("N4,009").
    
                FIND FIRST EDShipto NO-LOCK WHERE EDShipto.cust EQ EDIVTran.cust
                    AND EDShipto.Partner EQ EDIVTran.partner
                    AND EDShipto.ship-to EQ EDIVTran.By-code 
                    NO-ERROR.
                /*    ASSIGN                                               */
                /*        entity_id         = "PE"                         */
                /*        id_code_qualifier = "92"                         */
                /*        id_code           = edIVtran.by-code             */
                /*    company_name                    = 'PREMIER PACKAGING'*/
                /*        .                                                */
    
                /* Should only be in 'RI' */
                ASSIGN 
                    entity_relationship_code = ""
                    entity_identifier_code   = ""
                    id_code_qualifier        = ""
                    id_code                  = ""        
                    .
        
                /* N1 */
                ASSIGN 
                    entity_id  = "PE"
                    /* company_name                    = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE ""
                     id_code_qualifier               = "" /* 92" */ 
                      id_code                         = "" /* edIVtran.by-code */ */ 

                    /* N2 */
                    /* name2 = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE "" */
                    /*
                      /* N3 */
                      address1 = IF AVAILABLE EDShipto THEN EDShipto.Addr1 ELSE ""
                      address2 = IF AVAILABLE EDShipto THEN EDShipto.Addr2 ELSE ""
                      /* N4 */
                      city = IF AVAILABLE EDShipto THEN EDShipto.City ELSE ""
                      country = IF AVAILABLE EDShipto THEN EDShipto.Country ELSE ""
                      zip = IF AVAILABLE EDShipto THEN EDShipto.Zip ELSE ""
                      */
                    ws_section = 22.    
                RUN write_segments.ip ("N1,006").
                RUN write_segments.ip ("N3,008").
                RUN write_segments.ip ("N4,009").    
                /*
                FIND FIRST EDShipto NO-LOCK WHERE EDShipto.cust EQ EDIVTran.cust
                    AND EDShipto.Partner EQ EDIVTran.partner
                    AND EDShipto.ship-to EQ EDIVTran.By-code 
                    NO-ERROR.
            
                ASSIGN
                    entity_id         = "BY"
                    id_code_qualifier = "92"
                    id_code           = edIVtran.by-code
                    company_name                    = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE ""
                    .
                ASSIGN 
                    name2    = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE ""
                    
                    /* N3 */
                    address1 = IF AVAILABLE EDShipto THEN EDShipto.Addr1 ELSE ""
                    address2 = IF AVAILABLE EDShipto THEN EDShipto.Addr2 ELSE ""
                    /* N4 */
                    city     = IF AVAILABLE EDShipto THEN EDShipto.City ELSE ""
                    country  = IF AVAILABLE EDShipto THEN EDShipto.Country ELSE ""
                    state    = IF AVAILABLE EDShipto THEN EDShipto.state ELSE ""
                    zip      = IF AVAILABLE EDShipto THEN EDShipto.Zip ELSE "".
                
                ws_section = 23.    
                RUN write_segments.ip ("N1,006").    
                RUN write_segments.ip ("N3,008").
                RUN write_segments.ip ("N4,009").
            */
                FIND FIRST EDShipto NO-LOCK WHERE EDShipto.cust EQ EDIVTran.cust
                    AND EDShipto.Partner EQ EDIVTran.partner
                    AND EDShipto.ship-to EQ EDIVTran.By-code 
                    AND EDShipto.Ref-type EQ "BY"
                    NO-ERROR.
                ASSIGN
                    entity_id         = "ST"
                    id_code_qualifier = "" /* "92" */ 
                    id_code           = "" /* edIVtran.by-code */
                    company_name      = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE ""
                    .
                ASSIGN 
                    name2    = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE ""
                    name3    = IF AVAILABLE EDShipto THEN EDShipto.Name ELSE ""
                    /* N3 */
                    address1 = IF AVAILABLE EDShipto THEN EDShipto.Addr1 ELSE ""
                    address2 = IF AVAILABLE EDShipto THEN EDShipto.Addr2 ELSE ""
                    /* N4 */
                    city     = IF AVAILABLE EDShipto THEN EDShipto.City ELSE ""
                    country  = IF AVAILABLE EDShipto THEN EDShipto.Country ELSE ""
                    state    = IF AVAILABLE EDShipto THEN EDShipto.state ELSE ""
                    zip      = IF AVAILABLE EDShipto THEN EDShipto.Zip ELSE "".
                ws_section = 24.    
                RUN write_segments.ip ("N1,006").
                RUN write_segments.ip ("N3,008").
                RUN write_segments.ip ("N4,009").
      

          
            END. /* SEARS */
    END CASE.

    ASSIGN 
        ref_qual   = "" 
        ref_number = "".
    /* Amazon does not have this on BIG */
    ASSIGN 
        purchase_order_number = edivtran.cust-po
        purchase_order_date#  = edivtran.cust-po-date
        . 
    ws_section = 30.
    ws_line = 0.
    FOR EACH edivline OF edivtran EXCLUSIVE
       WHERE edivline.qty-shipped <> 0:
        ASSIGN
            product_characteristic_code = "08"
            item_description_type       = "F"
            item_description            = EDIVLine.description[1] 
            customer_item_number        = edivline.cust-item-no
            product_id                  = customer_item_number
            item_product_qualifier      = "CB"
            quantity_invoiced           = edivline.qty-shipped
            unit_price                  = edivline.unit-price
            unit_of_measure             = edivline.uom-code
            weight_unit_measure         = "LB"
            volume_unit_measure         = "CF"
            ws_line                     = ws_line + 1
            item_assigned_id            = STRING(ws_line)
            .
    
        /* 9810 CAH ... */
        CASE ws_partner_grp:
            WHEN "SEARS" THEN 
                DO:
                    IF unit_of_measure = "" 
                        OR unit_of_measure = "CT" 
                        OR unit_of_measure = "CS"
                        THEN unit_of_measure = "EA".
                END.
            WHEN "AMAZ" THEN 
                DO:
                    ASSIGN 
                        item_product_qualifier = "PO"
                        product_id             = purchase_order_number
                        .
                    IF unit_of_measure EQ "M" THEN 
                        ASSIGN
                            unit_of_measure = "EA"
                            unit_price      = unit_price / 1000
                            .
                END.
        END CASE.
        RUN write_segments.ip (detail_segment_list).
    END.

    ws_section = 40.
    ws_line = 0.
    n_recs = 0.
    /* Freight, etc */
    FOR EACH edivaddon OF edivtran EXCLUSIVE WHERE EDIVAddon.Agency-code NE "TAX":  
     
        ASSIGN
            allow_charge_indicator = "C"
            misc_elem[2]           = "D240"
            sac_agency_qualifier   = ""
            sac_agency_code        = ""
            sac_reference_id       = STRING(EDIVAddon.Amount)
            . 
        RUN write_segments.ip ("SAC,027").
    END.

    /* Tax */
    FOR EACH edivaddon OF edivtran EXCLUSIVE WHERE EDIVAddon.Agency-code EQ "TAX":    
        
        ASSIGN     
            tax_type          = "ST"
            total_tax_dollars = EDIVAddon.amount
            tax_pct           = EDIVAddon.rate
            .              
        RUN write_segments.ip ("TXI,028").
    END.
    /* Blank out values so that they aren't genered again in trailer list  */
    ASSIGN     
        tax_type               = ""
        total_tax_dollars      = 0
        tax_pct                = 0
        allow_charge_indicator = ""
        misc_elem[2]           = ""
        sac_agency_qualifier   = ""
        sac_agency_code        = ""
        sac_reference_id       = ""
        .  
    RUN write_segments.ip (trailer_segment_list).
    RUN write_tdf.ip.

    ASSIGN
        {rc/stampcht.i eddoc}
        eddoc.openitem    = FALSE
        eddoc.stat        = 9
        eddoc.status-flag = "SNT"
        eddoc.posted      = TRUE
        {rc/incr.i ws_recs_changed}
        ws_line           = ws_line + 1
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
        edivtran.invoice-no (COUNT)
        edivtran.tot-gross (total)
        edivtran.tot-frt (total)
        ws_misc (total).
    IF ERROR-STATUS:ERROR OR ws_erc <> 0
        THEN
{rc/incr.i ws_recs_inerror}.
{rc/accum.i ws_amt_changed edivtran.tot-gross}.
/* 9810 CAH: was causing screen flip ...
{rc/statsdis.i}
*/
END.
IF (ACCUM COUNT edivtran.invoice-no) > 0 THEN
DO WITH FRAME f-det:
    UNDERLINE STREAM s-out
        edivtran.invoice-no
        edivtran.tot-gross
        edivtran.tot-frt
        ws_misc.
    DOWN STREAM s-out.
    DISPLAY STREAM s-out
        ACCUM COUNT edivtran.invoice-no @ edivtran.invoice-no
        ACCUM TOTAL edivtran.tot-gross  @ edivtran.tot-gross
        ACCUM TOTAL edivtran.tot-frt    @ edivtran.tot-frt
        ACCUM TOTAL ws_misc             @ ws_misc.
END.
OUTPUT STREAM s-edi CLOSE.
HIDE FRAME f-current NO-PAUSE.
IF ws_recs_selected > 0 THEN
DO:
    DOWN STREAM s-out WITH FRAME f-det.
    PAGE.
END.
/*{rc/statsdis.i} */
VIEW STREAM s-out FRAME f-stats.
HIDE STREAM s-out FRAME f-stats NO-PAUSE.
PAGE STREAM s-out.

IF ws_filetype EQ "EDI" THEN 
    RUN ed/postProcessEDI.p (INPUT "810", ws_edi_path, ws_partner, ws_edi_path + ".edi").
  
RETURN.
{ed/tdf/writeseg.i}
