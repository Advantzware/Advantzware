/*------------------------------------------------------------------------
    File        : api/SendInvoice.p
    Purpose     : Returns the request data for X12 Invoice (810)

    Syntax      :

    Description : Returns the request data for X12 Invoice (810)

    Author(s)   : Wade Kaldawi
    Created     : Wed Apr 01 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    /* Variables to store invoice line's request data */
    DEFINE VARIABLE lcLineData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineData AS LONGCHAR  NO-UNDO.

    /* Variables to store invoice Addon's request data */
    DEFINE VARIABLE lcLineAddonData       AS LONGCHAR  NO-UNDO.
    DEFINE VARIABLE lcConcatLineAddonData AS LONGCHAR  NO-UNDO.
    
    /* Variables to store invoice address data */
    DEFINE VARIABLE lcConcatAddressData AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcAddressData AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcAddress2Data AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcConcatAddress2Data AS CHARACTER NO-UNDO.
    
    /* Variables to store Tax request data */
    DEFINE VARIABLE lcConcatTaxData AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcTaxData AS CHARACTER NO-UNDO.         
       
    /* Invoice Header Variables */
    DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIsaDate         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIsaTime         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIsaControlSeq   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGsControlSeq    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStControlSeq    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBigDate         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBigDocID        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtInvoiceDate    AS DATE NO-UNDO.
           
    DEFINE VARIABLE cTotalAmount     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dInvoiceTotalAmt AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLineTotalAmt    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cSELineCount     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dSELineCount     AS INTEGER NO-UNDO.
    
    /* Invoice Line Variables */
    DEFINE VARIABLE cItemLineNum   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemQty       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemUom       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemPrice     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemBuyerPart AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPartQualifier AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemDesc      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustCountry   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cShipToCode    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInvNotes      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLineCounter   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBolNum        AS INTEGER NO-UNDO.
    DEFINE VARIABLE cUomCode       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iQtyShipped    AS INTEGER NO-UNDO.
    DEFINE VARIABLE dUnitPrice     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyShipped    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cCustPart      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoNum AS CHARACTER NO-UNDO.
    
    /* Invoice Addon Variables */
    DEFINE VARIABLE cAddonAllowCharge     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMiscElem             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSacAgencyQualifier   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSacAgencyCode        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSacReferenceId       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAddonTaxType         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTotalTaxDollars      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTaxPct               AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE iCurrentAddonNumber   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTaxRate              AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxRateFreight       AS DECIMAL NO-UNDO.
    
    /* Invoice Address Variables */
    DEFINE VARIABLE cN1Code       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iAddressOrder AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCode         LIKE ar-inv.sold-id NO-UNDO.
    DEFINE VARIABLE cName         LIKE shipto.ship-name NO-UNDO.
    DEFINE VARIABLE cAddress1     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAddress2     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCity         LIKE shipto.ship-city NO-UNDO.
    DEFINE VARIABLE cState        LIKE shipto.ship-state NO-UNDO.
    DEFINE VARIABLE cZip          LIKE shipto.ship-zip NO-UNDO.
    DEFINE VARIABLE cCountry      LIKE shipto.country NO-UNDO.

    DEFINE VARIABLE cWhsCode           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQtyPerPack        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPurchaseUnit      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hFormatProcs       AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE iIndex             AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-APIOutboundDetail1 FOR APIOutboundDetail.
    DEFINE BUFFER bf-APIOutboundDetail2 FOR APIOutboundDetail. 
    DEFINE BUFFER bf-APIOutboundDetail3 FOR APIOutBoundDetail.
    DEFINE BUFFER bf-APIOutboundDetail4 FOR APIOutBoundDetail.
    DEFINE BUFFER bf-reftable1          FOR reftable.
    DEFINE BUFFER bf-reftable2          FOR reftable.
    DEFINE BUFFER bf-ar-invl            FOR ar-invl.
    
    DEFINE TEMP-TABLE ttAddons LIKE edivAddon.
    
    DEFINE TEMP-TABLE ttN1Address
        FIELD N1Code       AS CHARACTER 
        FIELD AddressOrder AS INTEGER   
        FIELD AddressCode         LIKE ar-inv.sold-id
        FIELD AddressName         LIKE shipto.ship-name 
        FIELD Address1     AS CHARACTER 
        FIELD Address2     AS CHARACTER 
        FIELD City         LIKE shipto.ship-city 
        FIELD State        LIKE shipto.ship-state 
        FIELD Zip          LIKE shipto.ship-zip 
        FIELD Country      LIKE shipto.country 
        INDEX iOrder AddressOrder.
        
    DEFINE TEMP-TABLE ttLines
        FIELD rLineDetailRow AS ROWID 
        FIELD iLine          AS INTEGER
        .
/* ************************  Function Prototypes ********************** */

FUNCTION fnGetGSControl RETURNS INTEGER 
    (  ) FORWARD.

FUNCTION fnGetISAControl RETURNS INTEGER 
    (  ) FORWARD.
          
    /* This is to run client specific request handler to fetch request data */
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE ttArgs,
            INPUT ipiAPIOutboundID,
            INPUT ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:    
        FIND FIRST APIOutboundDetail NO-LOCK
             WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND APIOutboundDetail.detailID      EQ "detail"
               AND APIOutboundDetail.parentID      EQ "SendInvoice"
             NO-ERROR.
        
        IF NOT AVAILABLE APIOutboundDetail THEN DO:
            ASSIGN
                opcMessage = "No APIOutboundDetail record found for [ detail ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
   
        FIND FIRST bf-APIOutboundDetail1 NO-LOCK
             WHERE bf-APIOutboundDetail1.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail1.detailID      EQ "Tax"
               AND bf-APIOutboundDetail1.parentID      EQ "SendInvoice"
             NO-ERROR.
                     
        FIND FIRST bf-APIOutboundDetail2 NO-LOCK
             WHERE bf-APIOutboundDetail2.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail2.detailID      EQ "N1Addresses"
               AND bf-APIOutboundDetail2.parentID      EQ "SendInvoice"
             NO-ERROR.
             
        FIND FIRST bf-APIOutboundDetail3 NO-LOCK
             WHERE bf-APIOutboundDetail3.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail3.detailID      EQ "Addons"
               AND bf-APIOutboundDetail3.parentID      EQ "SendInvoice"
             NO-ERROR.
             
        FIND FIRST bf-APIOutboundDetail4 NO-LOCK
             WHERE bf-APIOutboundDetail4.apiOutboundID EQ ipiAPIOutboundID
               AND bf-APIOutboundDetail4.detailID      EQ "N3Address2"
               AND bf-APIOutboundDetail4.parentID      EQ "N1Addresses"
             NO-ERROR.             
             
        FIND FIRST ttArgs
             WHERE ttArgs.argType  EQ "ROWID"
               AND ttArgs.argKey   EQ "inv-head"
             NO-ERROR.
        IF NOT AVAIL ttArgs THEN 
            FIND FIRST ttArgs
                 WHERE ttArgs.argType  EQ "ROWID"
                   AND ttArgs.argKey   EQ "ar-inv"
                 NO-ERROR.
                          
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid inv-head or ar-inv record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST inv-head NO-LOCK
             WHERE ROWID(inv-head) EQ TO-ROWID(ttArgs.argValue)
             NO-ERROR.
        IF NOT AVAIL inv-head THEN 
            FIND FIRST ar-inv NO-LOCK
                 WHERE ROWID(ar-inv) EQ TO-ROWID(ttArgs.argValue)
                 NO-ERROR.        
        IF NOT AVAILABLE inv-head AND NOT AVAIL ar-inv THEN DO:
            ASSIGN
                opcMessage = "Invalid inv-head or ar-inv ROWID passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        IF AVAILABLE inv-head THEN DO:
            IF NOT CAN-FIND(FIRST inv-line
                 WHERE inv-line.r-no  EQ inv-head.r-no) THEN DO:
                ASSIGN
                    opcMessage = "No inv-line records available for invoice [ " + STRING(inv-head.inv-no) + " ]"
                    oplSuccess = FALSE
                    .
                RETURN.
            END.
        END. 
        ELSE DO:
            IF NOT CAN-FIND(FIRST ar-invl
                 WHERE ar-invl.x-no EQ ar-inv.x-no) THEN DO:
                ASSIGN
                    opcMessage = "No ar-invl records available for invoice [ " + STRING(ar-inv.inv-no) + " ]"
                    oplSuccess = FALSE
                    .
                RETURN.
            END.            
        END.
        
        RUN system/formatProcs.p PERSISTENT SET hFormatProcs.
        
        dSELineCount = 3.
        IF AVAIL inv-head THEN DO:
            
            cCompany = inv-head.company.
            FIND FIRST cust NO-LOCK 
              WHERE cust.company EQ inv-head.company
                AND cust.cust-no EQ inv-head.cust-no
              NO-ERROR.
            IF AVAILABLE cust AND cust.country GT "" THEN 
              cCustCountry = cust.country.
            ELSE 
              cCustCountry = "US".
              
            FIND FIRST shipto NO-LOCK WHERE shipto.company EQ inv-head.company
                AND shipto.cust-no EQ inv-head.cust-no
                AND shipto.ship-id EQ IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to
                NO-ERROR.
            /*
            IF AVAILABLE shipto THEN 
                    RUN pCreateAddress("ST", 4, IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to,
                                        shipto.ship-name, shipto.ship-addr[1], shipto.ship-addr[2], 
                                        shipto.ship-city, shipto.ship-state,
                                        shipto.ship-zip, shipto.country).
            ELSE */ DO:
                RUN pCreateAddress("ST", 4, IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to,
                                    inv-head.sold-name, inv-head.sold-addr[1], inv-head.sold-addr[2], inv-head.sold-city,
                                    inv-head.sold-state,inv-head.sold-zip, cCustCountry ).
            END.    
            
            RUN pCreateAddress('PE', 3, '0000','PREMIER PACKAGING', '3254 RELIABLE PARKWAY', '', 'CHICAGO',
                               'IL','60686', 'US' ).                                    
            /* Not used for Amazon */
            cShipToCode    = IF inv-head.sold-no NE "" THEN inv-head.sold-no ELSE inv-head.bill-to.
            RUN format_date IN hFormatProcs (inv-head.inv-date, "YYYYMMDD", OUTPUT cBigDate).
            ASSIGN                 
             cBigDocID        = STRING(inv-head.inv-no)
             cTotalAmount     = TRIM(IF inv-head.t-inv-rev GT 0 THEN STRING(inv-head.t-inv-rev, ">>>>>>>>.99") ELSE "0")
             cTotalAmount     = REPLACE(ctotalAmount, ".", "")
             dtInvoiceDate    = inv-head.inv-date
             .

             RUN pCreateAddress("BT", 1, inv-head.bill-to, inv-head.cust-name, inv-head.addr[1], 
                                inv-head.addr[2], inv-head.city, inv-head.state, inv-head.zip, 
                                cCustCountry).
             /* Fetch invoice notes from notes table */    
             FOR EACH notes NO-LOCK
                WHERE notes.rec_key EQ inv-head.rec_key:
                cInvNotes = cInvNotes + STRING(notes.note_text).
             END.
        END. /* end using inv-head */
        ELSE DO: /* Using ar-inv */
           cCompany = ar-inv.company.
            FIND FIRST cust NO-LOCK 
              WHERE cust.company EQ ar-inv.company
                AND cust.cust-no EQ ar-inv.cust-no
              NO-ERROR.
            IF AVAILABLE cust AND cust.country GT "" THEN 
              cCustCountry = cust.country.
            ELSE 
              cCustCountry = "US".
              /*
            FIND FIRST shipto NO-LOCK WHERE shipto.company EQ ar-inv.company
                AND shipto.cust-no EQ ar-inv.cust-no
                AND shipto.ship-id EQ IF ar-inv.sold-id NE "" THEN ar-inv.sold-id ELSE ar-inv.ship-id
                NO-ERROR.
            IF AVAILABLE shipto THEN 
                    RUN pCreateAddress("ST", 4, IF ar-inv.sold-id NE "" THEN ar-inv.sold-id ELSE ar-inv.bill-to,
                                        shipto.ship-name, shipto.ship-addr[1], shipto.ship-addr[2], 
                                        shipto.ship-city, shipto.ship-state,
                                        shipto.ship-zip, shipto.country).
            ELSE */ DO:
                RUN pCreateAddress("ST", 4, IF ar-inv.sold-id NE "" THEN ar-inv.sold-id ELSE ar-inv.bill-to,
                                    ar-inv.sold-name, ar-inv.sold-addr[1], ar-inv.sold-addr[2], ar-inv.sold-city,
                                    ar-inv.sold-state,ar-inv.sold-zip, cCustCountry ).
            END.    
            RUN pCreateAddress('PE', 3, '0000','PREMIER PACKAGING', '3254 RELIABLE PARKWAY', '', 'CHICAGO',
                                   'IL','60686', 'US' ).  
            dLineTotalAmt = 0.
            FOR EACH bf-ar-invl NO-LOCK  
               WHERE bf-ar-invl.x-no = ar-inv.x-no
                AND (bf-ar-invl.inv-qty NE 0 OR bf-ar-invl.misc) :
               dLineTotalAmt = dLineTotalAmt + bf-ar-invl.amt.
            END.
            ASSIGN 
                cBigDocID        = STRING(ar-inv.inv-no)
                dInvoiceTotalAmt = dLineTotalAmt + ar-inv.tax-amt + (IF ar-inv.f-bill THEN ar-inv.freight ELSE 0)
                cTotalAmount     = TRIM(IF dInvoiceTotalAmt GT 0 THEN STRING(dInvoiceTotalAmt, ">>>>>>>>.99") ELSE "0")
                cTotalAmount     = REPLACE(ctotalAmount, ".", "")
                dtInvoiceDate    = ar-inv.inv-date
                .
             /*
             IF AVAIL cust THEN 
                 RUN pCreateAddress("BT", 1, cust.cust-no, cust.name, cust.addr[1], 
                        cust.addr[2], cust.city, cust.state, cust.zip, 
                        cCustCountry).
             ELSE
             */         
                /* Testing */       
                FIND FIRST shipto NO-LOCK WHERE shipto.company EQ ar-inv.company
                    AND shipto.cust-no EQ ar-inv.cust-no
                    AND shipto.ship-id EQ ar-inv.cust-no
                    NO-ERROR.
                 RUN pCreateAddress("BT", 1, ar-inv.bill-to, IF AVAIL shipto THEN shipto.ship-name ELSE ar-inv.cust-name, 
                    IF AVAIL shipto THEN shipto.ship-addr[1] ELSE ar-inv.addr[1], 
                                    IF AVAIL shipto THEN shipto.ship-addr[2] ELSE ar-inv.addr[2], 
                                    IF AVAIL shipto THEN shipto.ship-city ELSE ar-inv.city, 
                                    IF AVAIL shipto THEN shipto.ship-state ELSE ar-inv.state, 
                                    IF AVAIL shipto THEN shipto.ship-zip ELSE ar-inv.zip, 
                                    cCustCountry).            
            /* Fetch invoice notes from notes table */    
            FOR EACH notes NO-LOCK
               WHERE notes.rec_key EQ ar-inv.rec_key:
                cInvNotes = cInvNotes + STRING(notes.note_text).
            END.
        END.
        RUN format_date IN hFormatProcs (dtInvoiceDate, "YYYYMMDD", OUTPUT cBigDate).
        RUN format_date IN hFormatProcs (TODAY, "YYMMDD", OUTPUT cISADate).
        RUN format_time IN hFormatProcs (time, "hhmm", OUTPUT cIsaTime).
        
        ASSIGN 
             cIsaControlSeq   = STRING(fnGetISAControl())  /* TBD */
             cGsControlSeq    = STRING(fnGetGSControl())  /* TBD Have been using the same number with no problem */
             cStControlSeq    = STRING(1, "9999")   /* TBD */
             .

        RUN pCreateAddress('RI', 2, '0000','PREMIER PACKAGING', '3254 RELIABLE PARKWAY', '', 'CHICAGO',
                               'IL','60686', 'US' ).
                               
        /* Fetch Address Details for the invoice */
        FOR EACH ttN1Address                                 
            :          
            /* Address section has 3 lines per iteration */
            dSELineCount = dSELineCount + 3.
            ASSIGN  
                lcAddressData = STRING(bf-APIOutboundDetail2.data)
                cN1code       = ttN1Address.N1Code      
                cCode         = ttN1Address.addressCode 
                cName         = ttN1Address.addressName 
                cAddress1     = ttN1Address.address1    
                cAddress2     = ttN1Address.address2   
                cState        = ttN1Address.state 
                cCity         = ttN1Address.city        
                cZip          = ttN1Address.zip         
                cCountry      = ttN1Address.country
                .    
              IF cAddress1 EQ "" AND cAddress2 GT "" THEN 
                ASSIGN cAddress1 = cAddress2
                       cAddress2 = ""
                       . 
            IF AVAILABLE bf-APIOutboundDetail4 THEN 
                lcAddress2Data = STRING(bf-APIOutboundDetail4.data).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N1Qual", cN1Code).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N1Name", cName).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N3Address1", cAddress1).                       
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N4City", cCity).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N4State", cState).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N4Zip", cZip).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N4Country", cCountry).
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "linefeed", "~n").
            IF cAddress2 NE "" THEN 
                RUN updateRequestData(INPUT-OUTPUT lcAddress2Data, "N3Address2", cAddress2).
            ELSE 
                lcAddress2Data = "".
            RUN updateRequestData(INPUT-OUTPUT lcAddressData, "N3Address2nd", lcAddress2Data). 
            lcConcatAddressData = lcConcatAddressData + "" + lcAddressData.
        END.
        
        /* Fetch line details for the Invoice */         
        IF AVAIL inv-head THEN DO:
            
            lcConcatLineData = "".
            EMPTY TEMP-TABLE ttLines.
            FOR EACH inv-line
                WHERE inv-line.r-no   EQ inv-head.r-no
                  AND inv-line.inv-qty GT 0
                BY inv-line.line:    
                
                /* find order line for line number */
                FIND FIRST oe-ordl NO-LOCK 
                    WHERE oe-ordl.company EQ inv-line.company
                    AND oe-ordl.ord-no  EQ inv-line.ord-no 
                    AND oe-ordl.i-no EQ inv-line.i-no
                    NO-ERROR.
                IF NOT AVAIL oe-ordl THEN                 
                FIND FIRST oe-ordl NO-LOCK 
                    WHERE oe-ordl.company EQ inv-line.company
                      AND oe-ordl.po-no EQ inv-line.po-no
                      AND oe-ordl.i-no EQ inv-line.i-no 
                      USE-INDEX po-no
                      NO-ERROR.
                ASSIGN iLineCounter = iLineCounter + 1.
                CREATE ttLines.
                ASSIGN ttLines.rLineDetailRow = ROWID(inv-line)
                       ttLines.iLine = IF AVAILABLE oe-ordl THEN oe-ordl.line ELSE iLineCounter
                       .                       
            END.             
            FOR EACH ttLines,
              FIRST inv-line
                WHERE ROWID(inv-line) EQ ttLines.rLineDetailRow
                BY ttLines.iLine:    
                
                 ASSIGN 
                      iLineCounter = iLineCounter + 1
                      iBolNum = INTEGER(inv-line.b-no)               
                      dQtyShipped = (IF inv-line.inv-qty NE 0 OR iBolNum GT 0 THEN inv-line.inv-qty ELSE 1)
                      dUnitPrice       = inv-line.price
                      cCustPart        = inv-line.part-no
                      cUomCode =
                        (IF inv-line.pr-qty-uom > "" THEN
                        inv-line.pr-qty-uom
                        ELSE "EA"
                        )                      
                      .
                RUN pConvQtyPriceUOM (                                         
                     inv-line.inv-qty,
                     inv-line.cas-cnt,
                     inv-line.pr-uom,
                     inv-line.pr-qty-uom,
                    INPUT-OUTPUT cUomCode,
                    INPUT-OUTPUT dQtyShipped,
                    INPUT-OUTPUT dUnitprice). 
                                          
                /* Line number from inbound 850 if available, otherwise incremented */
                ASSIGN
                    lcLineData            = STRING(APIOutboundDetail.data)
                    cItemLineNum          = STRING(ttLInes.iLine)                     
                    cItemQty              = STRING(dQtyShipped)
                    cItemUom              = string(cUomCode)
                    cItemPrice            = STRING(dUnitPrice)
                    cItemID               = STRING(inv-line.i-no)
                    cItemBuyerPart        = STRING(TRIM(inv-line.part-no))
                    cItemDesc             = STRING(inv-line.i-name)
                    cPoNum                = STRING(inv-line.po-no)
                    cPartQualifier        = STRING(IF cItemBuyerPart NE "" THEN "BP" ELSE "")
                    .
                /* Detail section has 2 lines per iteration */
                dSELineCount = dSELineCount + 2.
    
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemLineNum", cItemLineNum).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemQty", cItemQty).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemUOM", cItemUom).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemPrice", cItemPrice).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "PartQualifier", cPartQualifier).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "BuyerPart", cItemBuyerPart).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDescription", cItemDesc).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "PoNum", cPoNum).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "linefeed", "~n").  
                lcConcatLineData = lcConcatLineData +  lcLineData + "~n". 

            END.      
        END. /* Process lines if using inv-head */
        ELSE DO:
            /* Process lines if using ar-inv */
            lcConcatLineData = "".

            EMPTY TEMP-TABLE ttLines.
            FOR EACH ar-invl
                WHERE ar-invl.x-no   EQ ar-inv.x-no
                  AND ar-invl.inv-qty GT 0
                BY ar-invl.line:    
                
                /* find order line for line number */
                FIND FIRST oe-ordl NO-LOCK 
                    WHERE oe-ordl.company EQ ar-invl.company
                    AND oe-ordl.ord-no  EQ ar-invl.ord-no 
                    AND oe-ordl.i-no EQ ar-invl.i-no
                    NO-ERROR.
                IF NOT AVAIL oe-ordl THEN                 
                FIND FIRST oe-ordl NO-LOCK 
                    WHERE oe-ordl.company EQ ar-inv.company
                      AND oe-ordl.po-no EQ ar-invl.po-no
                      AND oe-ordl.i-no EQ ar-invl.i-no 
                      USE-INDEX po-no
                      NO-ERROR.
                ASSIGN iLineCounter = iLineCounter + 1.
                CREATE ttLines.
                ASSIGN ttLines.rLineDetailRow = ROWID(ar-invl)
                       ttLines.iLine = IF AVAILABLE oe-ordl THEN oe-ordl.line ELSE iLineCounter
                       .                       
            END.            
            FOR EACH ttLines,
                FIRST ar-invl
                  WHERE ROWID(ar-invl) EQ ttLines.rLineDetailrow                    
                BY ttLines.iLine:    
                
                ASSIGN 
                      iLineCounter = iLineCounter + 1
                      iBolNum = INTEGER(ar-invl.b-no)               
                      cUomCode =  (IF ar-invl.pr-qty-uom > "" THEN
                                ar-invl.pr-qty-uom
                                ELSE "EA")
                      dQtyShipped = (IF ar-invl.inv-qty NE 0 OR iBolNum GT 0 THEN ar-invl.inv-qty ELSE 1)
                      dUnitPrice       = ar-invl.unit-pr
                      cCustPart        = ar-invl.part-no
                      // edivline.unit-price       = ar-invl.unit-pr
                      cUomCode =
                        (IF ar-invl.pr-qty-uom > "" THEN
                        ar-invl.pr-qty-uom
                        ELSE "EA"
                        )                      
                      .
                RUN pConvQtyPriceUOM (                                         
                     ar-invl.inv-qty,
                     ar-invl.cas-cnt,
                     ar-invl.pr-uom,
                     ar-invl.pr-qty-uom,
                    INPUT-OUTPUT cUomCode,
                    INPUT-OUTPUT dQtyShipped,
                    INPUT-OUTPUT dUnitprice). 
                          
                
                /* Line number from inbound 850 if available, otherwise incremented */
                ASSIGN
                    lcLineData            = STRING(APIOutboundDetail.data)
                    cItemLineNum          = STRING(ttLines.iLine)                     
                    cItemQty              = STRING(dQtyShipped)
                    cItemUom              = string(cUomCode)
                    cItemPrice            = STRING(dUnitPrice)
                    cItemID               = STRING(ar-invl.i-no)
                    cItemBuyerPart        = STRING(ar-invl.part-no)
                    cItemDesc             = STRING(ar-invl.i-name)
                    cPoNum                = STRING(ar-invl.po-no)
                    cPartQualifier        = STRING(IF cItemBuyerPart NE "" THEN "BP" ELSE "")
                    .
                /* Detail section has 2 lines per iteration */
                dSELineCount = dSELineCount + 2.
    
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemLineNum", cItemLineNum).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemQty", cItemQty).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemUOM", cItemUom).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemPrice", cItemPrice).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "PartQualifier", cPartQualifier).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "PoNum", cPoNum).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "BuyerPart", cItemBuyerPart).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDescription", cItemDesc).
                RUN updateRequestData(INPUT-OUTPUT lcLineData, "linefeed", "~n").
                
                lcConcatLineData = lcConcatLineData + "" + lcLineData.  

            END.                 
        END.
        
        /* Added charges section, i.e. Freight, Tax, Etc */
        IF AVAIL inv-head THEN DO:
            FOR EACH inv-misc OF inv-head NO-LOCK
                WHERE inv-misc.deleted = FALSE:              
            
                RUN pCreateAddonRecord (
                    RECID(inv-head), 
                    inv-misc.inv-line,      
                    inv-misc.charge,
                    inv-misc.Dscr,
                    inv-misc.amt,
                    0,             /* rate */
                    inv-misc.bill,
                    "ESTIMATE# " + inv-misc.est-no,
                    inv-head.cust-no,
                    iCurrentAddonNumber                    
                    ).
            
            END.    /* each inv-misc of inv-head */
        
            IF inv-head.t-inv-freight <> 0 THEN 
            DO:
                RUN pCreateAddonRecord (
                    RECID(inv-head), 
                    0,          /* line # */
                    "FRT",
                    "FREIGHT",
                    inv-head.t-inv-freight,
                    0,            /* rate */
                    "Y",
                    "Invoice Level Freight Charge",
                    inv-head.cust-no,
                    iCurrentAddonNumber                    
                    ).
            END.
        
            IF inv-head.t-inv-tax <> 0 THEN 
            DO:
                FIND stax-group
                    WHERE stax-group.company = inv-head.company
                    AND stax-group.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.
                RUN ar/cctaxrt.p (INPUT inv-head.company, inv-head.tax-gr ,
                    OUTPUT dTaxRate, OUTPUT dTaxRateFreight).   
                RUN pCreateAddonRecord (
                    RECID(inv-head), 
                    0,          /* line # */
                    "TAX",
                    (IF AVAILABLE stax-group THEN stax-group.tax-dscr ELSE "TAXES"),
                    inv-head.t-inv-tax,
                    dTaxRate,          /* rate */
                    YES,
                    "Invoice Level Taxes",
                    inv-head.cust-no,
                    iCurrentAddonNumber
                    ).
            END.
        END.
        ELSE DO:

                IF ar-inv.freight <> 0 THEN 
                DO:
                    RUN pCreateAddonRecord (
                        0 /* eddoc.rec */, 
                        0,          /* line # */
                        "FRT",
                        "FREIGHT",
                        ar-inv.freight,
                        0,          /* rate */    
                        "Y",
                        "Invoice Level Freight Charge",
                        ar-inv.cust-no,
                        iCurrentAddonNumber
                        ).
                END.
            
                IF ar-inv.tax-amt <> 0 THEN 
                DO:
                    FIND stax-group NO-LOCK
                        WHERE stax-group.company = ar-inv.company
                          AND stax-group.tax-group = ar-inv.tax-code  
                        NO-ERROR.
              
                    RUN ar/cctaxrt.p (INPUT ar-inv.company, ar-inv.tax-code /* oe-ord.tax-gr */,
                        OUTPUT dTaxRate, OUTPUT dTaxRateFreight).  
                    RUN pCreateAddonRecord (
                        0 /* eddoc.rec*/, 
                        0,          /* line # */
                        "TAX",
                        (IF AVAILABLE stax-group THEN stax-group.tax-dscr ELSE "TAXES"),
                        ar-inv.tax-amt,
                        dTaxRate,          /* rate */ 
                        "Y",
                        "Invoice Level Taxes",
                        ar-inv.cust-no,
                        iCurrentAddonNumber
                        ).
                END.
                
                /* Process the misc records that came from inv-misc */
                FOR EACH ar-invl 
                    WHERE ar-invl.x-no = ar-inv.x-no
                    AND  ar-invl.misc:
                    
                       
                    RUN pCreateAddonRecord (
                        0 /* eddoc.rec */, 
                        0,      
                        ar-invl.prep-charge,
                        ar-invl.Dscr[1],
                        ar-invl.amt,
                        0,             /* rate */
                        (IF ar-invl.billable THEN "Y" ELSE "N"),
                        "ESTIMATE# " + ar-invl.est-no,
                        ar-inv.cust-no,
                        iCurrentAddonNumber
                        ).            
                END.
        END.
        
        lcConcatLineAddonData = "".
        IF AVAILABLE bf-APIOutboundDetail3 THEN DO:
            /* Fetch Addon details for the invoice addons */           
            FOR EACH ttAddons 
                WHERE ttAddons.Agency-code NE "TAX"                  
                :             
                /* Added Charge section has 1 line per iteration */
                dSELineCount = dSELineCount + 1.
                ASSIGN
                    lcLineAddonData       = STRING(bf-APIOutboundDetail3.data)
                    cAddonAllowCharge     = "C"
                    cMiscElem             = "D240"
                    cSacAgencyQualifier   = ""
                    cSacAgencyCode        = ""
                    cSacReferenceId       = STRING(ttAddons.Amount)
                   .

                RUN updateRequestData(INPUT-OUTPUT lcLineAddonData, "AddonAllowCharge", cAddonAllowCharge).
                RUN updateRequestData(INPUT-OUTPUT lcLineAddonData, "AddonMiscElem", cMiscElem).
                RUN updateRequestData(INPUT-OUTPUT lcLineAddonData, "SacReferenceId", cSacReferenceId).
                RUN updateRequestData(INPUT-OUTPUT lcLineAddonData, "linefeed", "~n").
                lcConcatLineAddonData = lcConcatLineAddonData + "" + lcLineAddonData.
            END.
                      
        END.
        lcConcatTaxData = "".
        IF AVAILABLE bf-APIOutboundDetail1 THEN DO:
            FOR EACH ttAddons 
                WHERE ttAddons.Agency-code EQ "TAX"                  
                :
                /* Added Charge section has 1 line per iteration */
                dSELineCount = dSELineCount + 1.
                ASSIGN     
                    lcTaxData  = STRING(bf-APIOutboundDetail1.data)                    
                    cAddonTaxType    = "ST"
                    cTotalTaxDollars = STRING(ttAddons.amount)
                    cTaxPct          = STRING(ttAddons.rate)
                    .              
                
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxType", cAddonTaxType).
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TotalTaxDollars", cTotalTaxDollars).
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "TaxPct", cTaxPct).
                RUN updateRequestData(INPUT-OUTPUT lcTaxData, "linefeed", "~n").
                lcConcatTaxData = lcConcatTaxData + "" + lcTaxData.                    
                             
            END.  
        END.        
           
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IsaDate", cIsaDate ). 
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IsaTime", cIsaTime ). 
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IsaControlNum", cIsaControlSeq).   
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GsDate", cIsaDate ). 
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GsTime", cIsaTime ).           
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GsControlNum", cIsaControlSeq).   
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "StControlNum", cStControlSeq). 
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BigDate", cBigDate ). 
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BigDocID", cBigDocID ). 
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Currency", "USD" ).
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalAmount", cTotalAmount ).
          /* Add one for TDS segment - total amount, one for SE */
          dSELineCount = dSELineCount + 2.          
          cSELineCount = STRING(dSELineCount).
          RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SECount", cSELineCount ).
          
        RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "invNotes", cInvNotes).
    
        /* If the previous section was not blank, it ended with CR so don't need to start with one */
     
        lcConcatLineData = TRIM(lcConcatLineData, "~n").
        
        lcConcatLineAddonData = TRIM(lcConcatLineAddonData,"~n"). 
        
        lcConcatAddressData = TRIM(lcConcatAddressData,"~n").   
        
        lcConcatTaxData = TRIM(lcConcatTaxData,"~n").     
        //lcConcatTaxData = lcConcatTaxData + "~n".

        ioplcRequestData = REPLACE(ioplcRequestData, "[$N1Addresses$]", (IF lcConcatAddressData ne "" THEN "~n" ELSE "") + lcConcatAddressData).
          
        ioplcRequestData = REPLACE(ioplcRequestData, "[$Detail$]", (if lcConcatLineData ne "" THEN "~n" ELSE "") + lcConcatLineData).
                                                                 
        ioplcRequestData = REPLACE(ioplcRequestData, "[$Addons$]", (IF lcConcatLineAddonData ne "" THEN  "~n" ELSE "") + lcConcatLineAddonData).
        
        ioplcRequestData = REPLACE(ioplcRequestData, "[$Tax$]", (IF lcConcatTaxData ne "" THEN  "~n" ELSE "") + lcConcatTaxData).
                           
        RELEASE bf-APIOutboundDetail1.
        RELEASE bf-APIOutboundDetail2.
        RELEASE bf-APIOutboundDetail3.
        
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
             .
    END.
    
    /* End of Main Code */
    

/* **********************  Internal Procedures  *********************** */


PROCEDURE pConvQtyPriceUOM:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ipdInvoiceQty AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCaseCount AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPriceUom AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPriceqtyUom AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcUomCode AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyShipped LIKE dQtyShipped NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdUnitPrice AS DECIMAL NO-UNDO.
   

                        
                CASE iopcUomCode :
                    WHEN 'CS' THEN 
                        DO:
                            iopcUomCode = "CT". 
                            IF ipiCaseCount > 0 THEN
                                iopdQtyShipped = ipdInvoiceQty / ipiCaseCount.
                        END.
                    WHEN "M" THEN 
                        DO:
                            ASSIGN 
                                iopcUomCode      = "EA"
                                iopdUnitPrice     = iopdUnitPrice  / 1000
                                .
                        END.
                    OTHERWISE 
                    DO:
                    END. 
                END CASE.
                    

                       
                IF ipcPriceUom = "CS" AND ipiCaseCount > 0 THEN
                DO:  /* scale qty by case count */
                    iopdQtyShipped = ipdInvoiceQty / ipiCaseCount.
                    
                END.
                ELSE
                DO:
                    /* quantity unit */
                    iopcUomCode =
                        (IF ipcPriceqtyUom > "" THEN
                        ipcPriceqtyUom
                        ELSE "EA"
                        ).
        
       
                END.
                
                IF iopcUomCode = "CS"
                    THEN
                    iopcUomCode = "CT".    /* 9705 CAH */   

END PROCEDURE.

    PROCEDURE pCreateAddonRecord:
    DEFINE INPUT PARAMETER rInvRecid AS RECID NO-UNDO.
    DEFINE INPUT PARAMETER iInvLine AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipAgencyCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipChargeDesc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipAmount AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipRate AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipBillIableCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipReference AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipCustNum AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipNextAddonNum AS INTEGER NO-UNDO.

    DEFINE BUFFER bAddon FOR ttAddons.

    FIND FIRST ttAddons EXCLUSIVE-LOCK
        WHERE ttAddons.partner     = ipCustNum
          AND ttAddons.Description[1] = ipChargeDesc
        NO-ERROR.
    IF NOT AVAILABLE ttAddons THEN
    DO:
        ipNextAddonNum = 0.
        FOR each bAddon NO-LOCK 
           WHERE bAddon.partner = ipCustNum 
           :
           ipNextAddonNum = ipNextAddonNum + 1.
        END.
        ipNextAddonNum = ipNextAddonNum + 1.
        IF iInvLine EQ 0 THEN 
          iInvLine = ipNextAddonNum.

        /* ### if both of the above fail then this addon will be an orphan */
        CREATE ttAddons.
        ASSIGN
            ttAddons.partner    = ipCustNum
/*            ttAddons.company    = edivtran.company   */
/*            ttAddons.invoice-no = edivtran.invoice-no*/
            ttAddons.line       = ipNextAddonNum /* iInvLine */
            ttAddons.Addon-line = ipNextAddonNum
            .
    END.

    ASSIGN
        ttAddons.Description[1]   = ipChargeDesc
        ttAddons.Description[2]   = ""
        ttAddons.allow-charge     = IF ipAmount >= 0 THEN FALSE ELSE TRUE
        ttAddons.Amount           = ipAmount
        ttAddons.hand-meth        = IF ipBillIableCode = "Y" THEN "02" /* off invoice */ ELSE
    IF ipBillIableCode = "N" THEN "05" /* paid by vendor */
    ELSE "02"
        ttAddons.Agency-qual      = ""
        ttAddons.agency-code      = ipAgencyCode /* ### requires xlate */
        ttAddons.Ref-Num          = ipReference
        ttAddons.special-svc-code = ""
        ttAddons.Rate             = ipRate
        .

  
    RELEASE ttAddons.

END PROCEDURE.

PROCEDURE pCreateAddress PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcN1Code       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiAddressOrder AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcCode         LIKE ar-inv.sold-id NO-UNDO.
    DEFINE INPUT PARAMETER ipcName         LIKE shipto.ship-name NO-UNDO.
    DEFINE INPUT PARAMETER ipcAddress1     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAddress2     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCity         LIKE shipto.ship-city NO-UNDO.
    DEFINE INPUT PARAMETER ipcState        LIKE shipto.ship-state NO-UNDO.
    DEFINE INPUT PARAMETER ipcZip          LIKE shipto.ship-zip NO-UNDO.
    DEFINE INPUT PARAMETER ipcCountry      LIKE shipto.country NO-UNDO.
    
    CREATE ttN1Address.
    ASSIGN 
        ttN1Address.N1Code     = ipcN1code
        ttN1Address.addressOrder = ipiAddressOrder
        ttN1Address.addressCode  = ipcCode
        ttN1Address.addressName  = ipcName
        ttN1Address.address1     = ipcAddress1
        ttN1Address.address2     = ipcAddress2
        ttN1Address.state        = ipcState
        ttN1Address.city         = ipcCity
        ttN1Address.zip          = ipcZip
        ttN1Address.country      = (IF ipcCountry NE "" THEN ipcCountry ELSE "US")
        .
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fnGetGSControl RETURNS INTEGER 
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE result AS INTEGER NO-UNDO.
        RESULT = 1.
		RETURN result.


		
END FUNCTION.

FUNCTION fnGetISAControl RETURNS INTEGER 
	(  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE result AS INTEGER NO-UNDO.
        RESULT = 1.
		RETURN result.


		
END FUNCTION.
    
