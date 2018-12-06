
/*------------------------------------------------------------------------
    File        : ttToEdiTab.p
    Purpose     : 

    Syntax      :

    Description : Temp tables to EDI tables

    Author(s)   : WK
    Created     : Wed Nov 28 16:59:35 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
{sys/inc/var.i "new shared"}

ASSIGN
    cocode = g_company
    locode = g_loc.
{ed/sharedv.i "new"}
{ed/edivars.i "new shared"}
{ed/tdf/sharedv.i   "NEW SHARED"}

{cxml\cxmldefs.i}
define input parameter table for ttNodes.
define input-output parameter table for ttOrdHead.
define input-output parameter table for ttOrdLines.
define input-output parameter table for ttOrdSchedShipments.

DEFINE VARIABLE ws_customer        AS CHARACTER NO-UNDO.
DEFINE VARIABLE needs_header       AS LOGICAL   INITIAL TRUE NO-UNDO.
DEFINE VARIABLE needs_detail       AS LOGICAL   INITIAL FALSE NO-UNDO.
DEFINE VARIABLE has_shipto_address AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE note_array         AS CHARACTER NO-UNDO EXTENT 9.    /* 9810 CAH */
DEFINE TEMP-TABLE ttRecsCreated 
 FIELD saveRow AS ROWID .
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ws_company = cocode.
RUN createEdiRecs.
FIND FIRST ttRecsCreated NO-ERROR.
IF AVAILABLE ttRecsCreated THEN 
  RUN process860.
  
  
PROCEDURE createEdiRecs:
/* Program is for GE Only */
FIND FIRST EDMast NO-LOCK 
    WHERE EDMast.partnerGrp EQ "GE"
    NO-ERROR.
IF NOT AVAILABLE edmast THEN 
  RETURN.
ws_edmast_rec = RECID(edmast).   

FIND FIRST EDCode NO-LOCK
    WHERE  edcode.setid EQ "850"
    AND EDCode.Partner EQ edmast.partner 
    NO-ERROR.
IF NOT AVAILABLE edcode THEN 
    FIND FIRST EDCode NO-LOCK
        WHERE  edcode.setid EQ "850"
        AND EDCode.Partner EQ edmast.partnerGrp 
        NO-ERROR.    
IF NOT AVAILABLE edcode THEN 
   RETURN.    

 
/*   FIRST eddoc WHERE eddoc.partner EQ edmast.partner
   AND eddoc.posted = NO
   . */
       
IF AVAILABLE EDMast THEN 
    ws_partner_grp = EDMast.PartnerGrp.
    
ASSIGN
    ws_partner    = edmast.Partner 
    ws_edcode_rec = RECID(edcode)
    .
  
FOR EACH ttOrdHead:
            
    RUN ed/gendoc.p (RECID(edcode), ws_docid, OUTPUT ws_eddoc_rec).
    /* creates eddoc, assigns opening values */
    FIND eddoc WHERE RECID(eddoc) = ws_eddoc_rec EXCLUSIVE.
    MESSAGE "doc type for eddoc" ttOrdHead.ttDoctype
    VIEW-AS ALERT-BOX.
    ASSIGN
        eddoc.docseq      = INTEGER(location_number)
        eddoc.st-code     = ttOrdHead.ttshipToID
        eddoc.status-flag = "RCV"
        eddoc.isa         = header_isa
        eddoc.gs          = header_gs
        eddoc.st          = header_st
        eddoc.fgsender    = header_partner
        eddoc.setid       = ttOrdHead.ttDocType /* header_setid */
        eddoc.version     = header_std-ver
        eddoc.userref     = header_int-cd
        eddoc.fgrecvid    = header_partner
        eddoc.fgid        = (If ttOrdHead.ttDocType EQ "850" then "PO" ELSE "PC")
        EDDoc.Partner     = ws_partner
        .
        
    IF eddoc.setid EQ "860" THEN DO:
        CREATE ttRecsCreated.
        ASSIGN  
          ttRecsCreated.saveRow = ROWID(eddoc).
    END.
    
    ws_eddoc_rec = RECID(eddoc).
    FIND edpotran
        WHERE edpotran.partner = eddoc.partner
        AND edpotran.seq = eddoc.seq EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE edpotran THEN
    DO:
        FIND edmast WHERE RECID(edmast) = ws_edmast_rec NO-LOCK NO-ERROR.
        FIND edco   WHERE edco.company = ws_company NO-LOCK NO-ERROR.
        CREATE edpotran.
        ASSIGN
            edpotran.partner = eddoc.partner
            edpotran.seq     = eddoc.seq.
    END.
    ASSIGN
        edpotran.cust             = ws_customer
        edpotran.cust-po          = ttOrdHead.ttorderID
        edpotran.cust-dept        = ""
        edpotran.purpose-code     = ""
        edpotran.order-type       = ""
        edpotran.scheduled-code1  = ""
        edpotran.ship-method-code = ""
        edpotran.routing[1]       = ""
        edpotran.vn-code          = ""
        EDPOTran.st-code          = ttOrdHead.ttshipToID
        /* edpotran.zz-code = afe_number 9810 CAH: Not in Schema */
        edpotran.cust-div         = "" /* 9704 CAH */
        ws_edpotran_rec           = RECID(edpotran)
        
        edpotran.sf-code          = IF edmast.sf-code > "" THEN edmast.sf-code ELSE edco.sf-code
        .
    ASSIGN
        edpotran.order-date       = date(ttOrdHead.ttorderDate)
        edpotran.cancel-date      = ?
        edpotran.cancel-date-code = cancel_date_qualifier
        edpotran.request-date     = ship_date#
        edpotran.ship-date-code   = ship_date_qualifier
        /* DTM procedure saves these qualifiers ...
        cancel-date-code = IF cancel-date <> ? THEN "001" ELSE ""
        ship-date-code   = IF request-date <> ? THEN "010" ELSE ""
        */
        eddoc.st-code             = edpotran.st-code    /* 9809 CAH */
        ordering_store_number     = edpotran.by-code
        shipto_store_number       = edpotran.st-code
        /* ITD */
        edpotran.terms            = terms_type
        edpotran.terms-desc[1]    = (IF terms_description > "" 
        THEN ENTRY(1, terms_description) ELSE "")
        edpotran.terms-desc[2]    = (IF NUM-ENTRIES(terms_description) > 1 
        THEN ENTRY(2, terms_description) ELSE "")
        .
    IF ttOrdHead.ttbillToID GT "" THEN 
      has_shipto_address = TRUE.
    /* 9810 CAH per federated */
    IF has_shipto_address THEN
        ASSIGN
            edpotran.ship-name       = ttOrdHead.ttshipToName
            edpotran.ship-address[1] = ttOrdHead.ttshipToAddress1 
            edpotran.ship-address[2] = (IF  ttOrdHead.ttshipToAddress2 = "" AND ws_contact_name > ""
                                        THEN ws_contact_name ELSE  ttOrdHead.ttshipToAddress2)
            edpotran.ship-address[3] = contact_name
            edpotran.ship-city       = ttOrdHead.ttshipToCity
            edpotran.ship-st         = ttOrdHead.ttshipToState
            edpotran.ship-zip        = ttOrdHead.ttshipToZip
            shipto_name              = ''
            shipto_address1          = ''
            shipto_address2          = ''
            contact_name             = ''
            shipto_city              = ''
            shipto_state             = ''
            shipto_zip               = ''
            ws_contact_name          = ''
            has_shipto_address       = FALSE
            .

    FOR EACH ttOrdLines WHERE 
        ttOrdLines.ttpayLoadID EQ ttOrdHead.ttPayLoadID:
            
        IF NOT AVAILABLE edpoline THEN
        DO:
            CREATE edpoline.
            ASSIGN
                edpoline.partner   = edpotran.partner
                edpoline.seq       = edpotran.seq
                edpotran.lines     = edpotran.lines + 1
                edpotran.last-line = edpotran.last-line + 1
                edpoline.line      = edpotran.last-line
                edpoline.st-code   = edpotran.st-code
                edpoline.by-code   = ordering_store_number
                .
        END.

        ASSIGN
            /* PO1 */
            edpoline.cust-po-line   = IF INTEGER(ttOrdLines.ttItemLineNumber)  > 0
                                        THEN STRING(ttOrdLines.ttItemLineNumber )
                                        ELSE STRING(edpoline.line)
            edpoline.cust-item-no   = (IF ttOrdLines.ttItemSupplierPartID > '' THEN ttOrdLines.ttItemSupplierPartID ELSE
                                    IF customer_sku_number > '' THEN customer_sku_number ELSE
                                        IF vendor_item_number > '' THEN vendor_item_number
                                            ELSE cust-item-no)
            edpoline.item-no        = (IF vendor_item_number > '' THEN vendor_item_number
                                        ELSE edpoline.item-no)
            edpoline.upc            = (IF upc_code > '' THEN upc_code ELSE
                                      IF ean_code > '' THEN ean_code
                                      ELSE edpoline.upc)
            edpoline.qty-orig-ord   = INTEGER(ttOrdLines.ttItemQuantity) 
            edpoline.uom-code       = ttOrdLines.ttItemUnitOfMeasure
            edpoline.unit-price     = DECIMAL(ttOrdLines.ttItemMoney)
            ws_edpoline_rec         = RECID(edpoline)
            /* PID */
            edpoline.description[1] = item_description
            edpoline.description[2] = second_description
            edpoline.size-desc      = item_size
            edpoline.color-desc     = item_color
            .
    END. /* EACH ttOrdLines*/
END. /* EACH ttOrdHead */
END PROCEDURE.  /* CREATE EDI recs */

PROCEDURE process860:
    DEFINE VARIABLE cBufferDiff AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ix AS INTEGER NO-UNDO. 
    DEFINE VARIABLE bufEdPOTran AS HANDLE NO-UNDO.
    DEFINE VARIABLE qryEdPoTran AS HANDLE NO-UNDO.
    DEFINE VARIABLE fh AS HANDLE NO-UNDO EXTENT 100.
    DEFINE VARIABLE rMatchRow AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-edPoTran FOR edPoTran.
    DEFINE BUFFER bf-edPOline FOR edPoLine.
    DEFINE BUFFER bf-eddoc FOR eddoc.
   

    FOR EACH ttRecsCreated, 
      FIRST eddoc WHERE ROWID(eddoc) EQ ttRecsCreated.saveRow:
      
      IF AVAILABLE eddoc THEN DO:
          MESSAGE "avail ttrecs" eddoc.partner SKIP eddoc.docID SKIP eddoc.seq
              VIEW-AS ALERT-BOX.
          FIND FIRST bf-eddoc NO-LOCK 
            WHERE bf-eddoc.setID EQ "850"
              AND bf-eddoc.partner EQ eddoc.partner
              AND bf-eddoc.docID EQ eddoc.docId
           NO-ERROR.
      IF AVAILABLE bf-eddoc THEN 
         FIND FIRST edPOTran no-lock
            WHERE edpotran.partner EQ  eddoc.partner
              AND edpotran.seq EQ eddoc.seq
            NO-ERROR.
            
            MESSAGE "eddoc 850 found" AVAILABLE(eddoc) skip
            "ws_part" ws_partner skip
            "custspo" edPoTran.cust-po
            VIEW-AS ALERT-BOX.
          FIND FIRST bf-edPoTran NO-LOCK 
            WHERE bf-edPoTran.seq EQ bf-eddoc.seq
              AND bf-edPOTran.partner EQ bf-eddoc.partner
            NO-ERROR.
            MESSAGE "bfedpotran found" avail(bf-edpotran)
            VIEW-AS ALERT-BOX.
          IF AVAILABLE bf-edPoTran THEN DO:
             BUFFER-COMPARE edPoTran TO bf-edPoTran SAVE RESULT IN cBufferDiff.
              CREATE BUFFER bufEdPOTran FOR TABLE "edPoTran".
              CREATE QUERY qryEdPoTran.
              qryEdPoTran:SET-BUFFERS(bufEdPoTran).
              rMatchRow= bf-edPoTran.seq.
              qryEdPoTran:QUERY-PREPARE("FOR EACH edPoTran WHERE edPoTran.seq = " + STRING(rMatchRow)).
              qryEdPoTran:QUERY-OPEN().
              qryEdPoTran:GET-FIRST().            
             OUTPUT TO c:\temp\860Report.
             PUT UNFORMATTED SESSION:TEMP-DIRECTORY skip.
             PUT UNFORMATTED "Sequence: " edPoTran.seq SKIP.
             PUT UNFORMATTED "PO#: " edPOTran.cust-po SKIP.
             PUT UNFORMATTED "Cust#: " ws_partner SKIP.
             PUT UNFORMATTED cBufferDiff SKIP.
             DO ix = 1 TO bufEdPOTran:NUM-FIELDS:
                 fh[ix] = bufEdPOTran:buffer-field(ix).
                 IF LOOKUP(fh[ix]:name, cBufferDiff) > 0 THEN
                   PUT UNFORMATTED fh[ix]:NAME  + ": " + STRING(fh[ix]:BUFFER-VALUE) SKIP.
             END.
             OUTPUT CLOSE.
              qryEdPoTran:QUERY-CLOSE.
              bufEdPOTran:BUFFER-RELEASE.
              DELETE OBJECT bufEdPOTran.
              DELETE OBJECT qryEdPoTran. 
             FOR EACH edPoLine NO-LOCK 
                WHERE edPoLine.partner EQ edPoTran.partner
                  AND edPoLine.seq EQ edPOTran.seq:
                 FIND FIRST bf-edPoLine no-lock
                    WHERE bf-edPoLine.Seq EQ bf-edPoTran.seq
                      AND bf-edPOLine.partner EQ bf-edPoTran.partner
                      AND bf-edPoLine.Cust-po-line EQ edPoLine.cust-po-line
                    NO-ERROR.
                 IF AVAILABLE bf-edPoLine THEN DO:
                     BUFFER-COMPARE edPoLine TO bf-edPoLine SAVE RESULT IN cBufferDiff.
                     CREATE BUFFER bufEdPOTran FOR TABLE "edPoLine".
                     CREATE QUERY qryEdPoTran.
                     qryEdPoTran:SET-BUFFERS(bufEdPoTran).
                     rMatchRow = bf-edPoLine.seq.
                     qryEdPoTran:QUERY-PREPARE("FOR EACH edPoLine WHERE edPoLine.seq = " + STRING(rMatchRow)).
                     qryEdPoTran:QUERY-OPEN().
                     qryEdPoTran:GET-FIRST().                     
                     OUTPUT TO c:\temp\860Report append.
                     PUT UNFORMATTED cBufferDiff SKIP.
                     DO ix = 1 TO bufEdPOTran:NUM-FIELDS:
                         fh[ix] = bufEdPOTran:buffer-field(ix).
                         IF LOOKUP(fh[ix]:name, cBufferDiff) > 0 THEN
                             PUT UNFORMATTED fh[ix]:NAME  + ": " + STRING(fh[ix]:BUFFER-VALUE) SKIP.
                     END.                 
                     OUTPUT CLOSE.
                     qryEdPoTran:QUERY-CLOSE.
                     bufEdPOTran:BUFFER-RELEASE.
                     DELETE OBJECT bufEdPOTran.
                     DELETE OBJECT qryEdPoTran. 
                 END. /* if avail matching edPoline */
             END. /* each edPoLine */
          END. /* if avail matching edPOTran */
      END. /* if bf-eddoc is found */
    END. /* each ttrecsCreated */
END PROCEDURE.

