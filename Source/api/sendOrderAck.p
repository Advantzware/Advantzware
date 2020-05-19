/*------------------------------------------------------------------------
    File        : sendOrderAck.p
    Purpose     : Returns the request data for x12 Order Acknowlegement 
                  (855)
    Syntax      :

    Description : 

    Author(s)   : Wade Kaldawi
    Created     : Wed Apr 29 09:46:01 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{api/ttArgs.i}
{api/CommonAPIProcs.i}
    
DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
/* Variables to store Order line's request data */
DEFINE VARIABLE lcLineData            AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineData      AS LONGCHAR  NO-UNDO.

/* Variables to store Order MiscCharge's request data */
DEFINE VARIABLE lcLineMiscChargeData       AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE lcConcatLineMiscChargeData AS LONGCHAR  NO-UNDO.
    
/* Variables to store Order address data */
DEFINE VARIABLE lcConcatAddressData   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAddressData         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAddress2Data        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcConcatAddress2Data  AS CHARACTER NO-UNDO.
    
/* Variables to store Tax request data */
DEFINE VARIABLE lcConcatTaxData       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTaxData             AS CHARACTER NO-UNDO.         
       
/* Order Header Variables */
DEFINE VARIABLE cCompany              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIsaDate              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIsaTime              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIsaControlSeq        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cGsControlSeq         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStControlSeq         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBigDate              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBigDocID             AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtOrderDate           AS DATE      NO-UNDO.
           
DEFINE VARIABLE cTotalAmount          AS CHARACTER NO-UNDO.
DEFINE VARIABLE dOrderTotalAmt        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dLineTotalAmt         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iLineCount            AS INTEGER   NO-UNDO.
DEFINE VARIABLE cSELineCount          AS CHARACTER NO-UNDO.
DEFINE VARIABLE dSELineCount          AS INTEGER   NO-UNDO.
    
/* Order Line Variables */
DEFINE VARIABLE cLineNum              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQtyOrdered           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQtyAlloc             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQtyShipped           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQtyUOM              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemPrice            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemID               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBuyerPart            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPartQualifier        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemDesc             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustCountry          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToCode           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInvNotes             AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLineCounter          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cUomCode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE iQtyShipped           AS INTEGER   NO-UNDO.
DEFINE VARIABLE dUnitPrice            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dQtyShipped           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dQtyOrdered           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dQtyAlloc             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCustPart             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemPoNum            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAckPrnt              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cAckPrntDate          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cAddr                 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cApprovedDate         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cApprovedId           AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cBankCode             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cBillI                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cBillTo               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cBillFreight          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCarrier              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcActnum             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcAddr               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcAuth               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcBank               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcCity               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcCompany            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcCountry            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcCvvCode            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcExpiration         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcName               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcNum                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcPhone              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcState              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCType                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCcZip                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cchType               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCity                 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCloseChecked         AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCloseDate            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE closeTime             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCloseUser            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cContact              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCsrUserId            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCurrCode             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCustName             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cCustNo               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDelZone              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDeleted              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDueCode              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDueDate              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cDueTime              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cENum                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEdiOrdDate           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEmailAddr            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEmailName            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEnteredDate          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEnteredId            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEstNo                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cEstType              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExRate               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFBill                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFobCode              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFrtPay               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFtckPrnt             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cFuelBill             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cInvDate              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cInvNo                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJNo                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cJobNo2               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLastDate             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cLeadDays             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLoc                  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cManaged              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOpened               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrdDate              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrdNo                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOverPct              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoNo                 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPoNum                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPoNo2                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPordNo               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoReceivedDate       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPosted               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPriceHold            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cPriceHoldReason      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cProdDate             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cQNo                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRec_key              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSComm                AS CHARACTER NO-UNDO EXTENT 3.
DEFINE VARIABLE cSPct                 AS CHARACTER NO-UNDO EXTENT 3.
DEFINE VARIABLE cShipI                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cShipId               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSman                 AS CHARACTER NO-UNDO EXTENT 3. 
DEFINE VARIABLE cSmanNo               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSname                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSoldAddr             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSoldCity             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSoldId               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSoldName             AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSoldNo               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSoldState            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSoldZip              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cSpareChar1           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareChar2           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareChar3           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareChar4           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareChar5           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareDec1            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareDec2            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareDec3            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareDec4            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareDec5            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareInt1            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareInt2            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareInt3            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareInt4            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSpareInt5            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStat                 AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cTComm                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTCost                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTFreight             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTFuel                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTInvCost             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTInvFreight          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTInvRev              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTInvTax              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTInvWeight           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTRevenue             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTWeight              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTax                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTaxGr                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cTerms                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cTermsD               AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cTotOrd               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType                 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cUnderPct             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUpdDate              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cUpdTime              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUpdatedDate          AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cUpdatedId            AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cUserId               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhsRder              AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cWhsed                AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cLineCnt              AS CHARACTER NO-UNDO.

DEFINE VARIABLE cAdder                AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cBlankNo              AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cCasCnt               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cCases                AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cCases-unit           AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cCost                 AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cDScore               AS CHARACTER NO-UNDO.         
DEFINE VARIABLE cDisc                 AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cFixoh                AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cFlute                AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cFormNo               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cFtickPrnt            AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cIDscr                AS CHARACTER NO-UNDO.      
DEFINE VARIABLE cInvQty               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cIsAComponent         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobStartDate         AS CHARACTER NO-UNDO.        
DEFINE VARIABLE cJobStartTime         AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cLScore               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cLab                  AS CHARACTER NO-UNDO.          
DEFINE VARIABLE cLotNo                AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cMatl                 AS CHARACTER NO-UNDO.       
DEFINE VARIABLE cOrdqNo               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cPalChg               AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cPalINo               AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cPartDscr1            AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cPartDscr2            AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cPartDscr3            AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cPartNo               AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cPartial              AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cPoNoPo               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cPriceUom             AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cPrice                AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cPromCode             AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cPromDate             AS CHARACTER NO-UNDO.        
DEFINE VARIABLE cPrUom                AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQLine                AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cOrdQty               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cQRel                 AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cQty                  AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cQtyOnBo              AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cRaInvNo              AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cRaNo                 AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cRel                  AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cRelQty               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cRelStat              AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cReqCode              AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cReqDate              AS CHARACTER NO-UNDO.        
DEFINE VARIABLE cReqTime              AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cSBasis               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cScore                AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cSetHdrLine           AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cSetup                AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cSheetLen             AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cSheetWid             AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cShipQty              AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cStackCode            AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cStyle                AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cTDep                 AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cTFreightP            AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cTInvPallets          AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cTInvQty              AS CHARACTER NO-UNDO.         
DEFINE VARIABLE cTLen                 AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cTPrice               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cTRelQty              AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cTShipQty             AS CHARACTER NO-UNDO.      
DEFINE VARIABLE cTWid                 AS CHARACTER NO-UNDO.       
DEFINE VARIABLE cTest                 AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cTypeCode             AS CHARACTER NO-UNDO.    
DEFINE VARIABLE cUnitCount            AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cUnitsPallet          AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cVaroh                AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cVendNo               AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cWScore               AS CHARACTER NO-UNDO.     
DEFINE VARIABLE cWhsItem              AS CHARACTER NO-UNDO.      
DEFINE VARIABLE cOrdDateYMD           AS CHARACTER NO-UNDO.



/* Order MiscCharge Variables */
DEFINE VARIABLE cOrd-no               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLine                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCharge               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAmt                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cActnum               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDscr                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEst-no               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMiscChargeTax             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMiscChargeCost            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBill                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrdINo               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrdLine              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCommbasis            AS CHARACTER NO-UNDO EXTENT 10.
DEFINE VARIABLE cEqty                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstLine              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrepType             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstExtent            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoNoNo               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstPrepEqty          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstPrepLine          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMiscType             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMiscInd              AS CHARACTER NO-UNDO.

    
/* Order Address Variables */
DEFINE VARIABLE cCode                 LIKE ar-inv.sold-id NO-UNDO.
DEFINE VARIABLE cName                 LIKE shipto.ship-name NO-UNDO.
DEFINE VARIABLE cAddress1             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAddress2             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cState                LIKE shipto.ship-state NO-UNDO.
DEFINE VARIABLE cZip                  LIKE shipto.ship-zip NO-UNDO.
DEFINE VARIABLE cCountry              LIKE shipto.country NO-UNDO.

DEFINE VARIABLE cWhsCode              AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQtyPerPack           AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPurchaseUnit         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFormatProcs          AS HANDLE    NO-UNDO.
    
DEFINE VARIABLE iIndex                AS INTEGER   NO-UNDO.
    
DEFINE BUFFER bf-APIOutboundDetail2 FOR APIOutboundDetail. 

    
DEFINE BUFFER bf-reftable1          FOR reftable.
DEFINE BUFFER bf-reftable2          FOR reftable.
DEFINE BUFFER bf-ar-invl            FOR ar-invl.
    
    
        
DEFINE TEMP-TABLE ttLines
    FIELD rLineDetailRow AS ROWID 
    FIELD iLine          AS INTEGER


    .
/* ************************  Function Prototypes ********************** */

FUNCTION fnGetGSControl RETURNS INTEGER 
    ( ) FORWARD.

FUNCTION fnGetISAControl RETURNS INTEGER 
    ( ) FORWARD.


/* ***************************  Main Block  *************************** */
          
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
ELSE 
DO:    
    FIND FIRST APIOutboundDetail NO-LOCK
        WHERE APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
        AND APIOutboundDetail.detailID      EQ "detail"
        AND APIOutboundDetail.parentID      EQ "SendOrderAck"
        NO-ERROR.
        
    IF NOT AVAILABLE APIOutboundDetail THEN 
    DO:
        ASSIGN
            opcMessage = "No APIOutboundDetail record found for [ detail ]"
            oplSuccess = FALSE
            .
        RETURN.
    END.
   
                     
    FIND FIRST bf-APIOutboundDetail2 NO-LOCK
        WHERE bf-APIOutboundDetail2.apiOutboundID EQ ipiAPIOutboundID
        AND bf-APIOutboundDetail2.detailID      EQ "MiscCharge"
        AND bf-APIOutboundDetail2.parentID      EQ "SendOrderAck"
        NO-ERROR.
             

           
             
    FIND FIRST ttArgs
        WHERE ttArgs.argType  EQ "ROWID"
        AND ttArgs.argKey   EQ "oe-ord"
        NO-ERROR.

                          
    IF NOT AVAILABLE ttArgs THEN 
    DO:
        ASSIGN
            opcMessage = "No valid oe-ord record passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
        
    FIND FIRST oe-ord NO-LOCK
        WHERE ROWID(oe-ord) EQ TO-ROWID(ttArgs.argValue)
        NO-ERROR.
     
    IF NOT AVAILABLE oe-ord THEN 
    DO:
        ASSIGN
            opcMessage = "Invalid oe-ord ROWID passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    IF AVAILABLE oe-ord THEN 
    DO:
        IF NOT CAN-FIND(FIRST oe-ordl
            WHERE oe-ord.company EQ oe-ord.company
            and oe-ordl.ord-no  EQ oe-ord.ord-no) THEN 
        DO:
            ASSIGN
                opcMessage = "No oe-ordl records available for Order [ " + STRING(oe-ord.ord-no) + " ]"
                oplSuccess = FALSE
                .
            RETURN.
        END.
    END. 
END.
        
RUN system/formatProcs.p PERSISTENT SET hFormatProcs.
        

IF AVAILABLE oe-ord THEN 
DO:
            
    cCompany = oe-ord.company.
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
        NO-ERROR.
    IF AVAILABLE cust AND cust.country GT "" THEN 
        cCustCountry = cust.country.
    ELSE 
        cCustCountry = "US".
              
    FIND FIRST shipto NO-LOCK WHERE shipto.company EQ oe-ord.company
        AND shipto.cust-no EQ oe-ord.cust-no
        AND shipto.ship-id EQ oe-ord.ship-id
        NO-ERROR.

            
    /* Fetch Order notes from notes table */    
    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ oe-ord.rec_key:
        cInvNotes = cInvNotes + STRING(notes.note_text).
    END.
END. /* end using oe-ord */

RUN format_date IN hFormatProcs (dtOrderDate, "YYYYMMDD", OUTPUT cBigDate).
RUN format_date IN hFormatProcs (TODAY, "YYMMDD", OUTPUT cISADate).
RUN format_time IN hFormatProcs (TIME, "hhmm", OUTPUT cIsaTime).
        
ASSIGN 
    cIsaControlSeq = STRING(fnGetISAControl( ))  /* TBD */
    cGsControlSeq  = STRING(fnGetGSControl( ))  /* TBD Have been using the same number with no problem */
    cStControlSeq  = STRING(1, "9999")   /* TBD */
    .      

            
ASSIGN lcConcatLineData = ""
       iLineCount = 0
       .         

FOR EACH oe-ordl
    WHERE oe-ordl.company EQ oe-ord.company
    AND oe-ordl.ord-no EQ oe-ord.ord-no:    
                                          
    /* Line number from inbound 850 if available, otherwise incremented */
    ASSIGN
        lcLineData     = STRING(APIOutboundDetail.data)
        iLineCount     = iLineCount + 1
        cLineNum   = STRING(oe-ordl.line)                     
        cQtyOrdered    = IF oe-ordl.spare-dec-1 NE 0 THEN STRING(oe-ordl.spare-dec-1) + '"' ELSE STRING(oe-ordl.qty)
        cQtyUOM        = STRING(IF oe-ordl.spare-char-2 NE '' THEN oe-ordl.spare-char-2 ELSE 'EA')
        cPriceUom      = STRING(oe-ordl.pr-uom)
        cItemPrice     = STRING(oe-ordl.price)
        cItemID        = STRING(oe-ordl.i-no)
        cBuyerPart = STRING(TRIM(oe-ordl.part-no))
        cItemDesc      = STRING(oe-ordl.i-name)
        cPoNum         = STRING(oe-ordl.po-no)
        cPartQualifier = STRING(IF cBuyerPart NE "" THEN "BP" ELSE "")
        cAdder         = STRING(oe-ordl.adder[1]) 
        cBlankNo       = STRING(oe-ordl.blank-no)
        cCarrier       = STRING(oe-ordl.carrier)
        cCasCnt        = STRING(oe-ordl.cas-cnt)
        cCases         = STRING(oe-ordl.cases)
        cCases-unit    = STRING(oe-ordl.cases-unit)
        cCompany       = STRING(oe-ordl.company)
        cCost          = STRING(oe-ordl.cost)
        cCustNo        = STRING(oe-ordl.cust-no)
        cDScore        = STRING(oe-ordl.d-score[1]) 
        cDelZone       = STRING(oe-ordl.del-zone)
        cDeleted       = STRING(oe-ordl.deleted)
        cDisc          = STRING(oe-ordl.disc)
        cENum          = STRING(oe-ordl.e-num)
        cEstNo         = STRING(oe-ordl.est-no)
        cEstType       = STRING(oe-ordl.est-type)
        cFixoh         = STRING(oe-ordl.fixoh)
        cFlute         = STRING(oe-ordl.flute)
        cFormNo        = STRING(oe-ordl.form-no)
        cFrtPay        = STRING(oe-ordl.frt-pay)
        cFtickPrnt     = STRING(oe-ordl.ftick-prnt)
        cIDscr         = STRING(oe-ordl.i-dscr)
        cInvQty        = STRING(oe-ordl.inv-qty)
        cIsAComponent  = STRING(oe-ordl.is-a-component)
        cJNo           = STRING(oe-ordl.j-no)
        cJobNo         = STRING(oe-ordl.job-no)
        cJobNo2        = STRING(oe-ordl.job-no2)
        cJobStartDate  = STRING(oe-ordl.job-start-date) 
        cJobStartTime  = STRING(oe-ordl.job-start-time)
        cLScore        = STRING(oe-ordl.l-score[1]) 
        cLab           = STRING(oe-ordl.lab)
        cLotNo         = STRING(oe-ordl.lot-no)
        cManaged       = STRING(oe-ordl.managed)
        cMatl          = STRING(oe-ordl.matl)
        cOpened        = STRING(oe-ordl.opened)
        cOrdNo         = STRING(oe-ordl.ord-no)
        cOrdqNo        = STRING(oe-ordl.ordq-no)
        cOverPct       = STRING(oe-ordl.over-pct)
        cPalChg        = STRING(oe-ordl.pal-chg)
        cPalINo        = STRING(oe-ordl.pal-i-no)
        cPartDscr1     = STRING(oe-ordl.part-dscr1)
        cPartDscr2     = STRING(oe-ordl.part-dscr2)
        cPartDscr3     = STRING(oe-ordl.part-dscr3)
        cPartNo        = STRING(oe-ordl.part-no)
        cPartial       = STRING(oe-ordl.partial)
        cPoNoPo        = STRING(oe-ordl.po-no-po)
        cPoNo2         = STRING(oe-ordl.po-no2)
        cPrUom         = STRING(oe-ordl.pr-uom)
        cPromCode      = STRING(oe-ordl.prom-code)
        cPromDate      = STRING(oe-ordl.prom-date) 
        cQLine         = STRING(oe-ordl.q-line)
        cQNo           = STRING(oe-ordl.q-no)
        cOrdQty        = STRING(oe-ordl.q-qty)
        cQRel          = STRING(oe-ordl.q-rel)
        cQtyOnBo       = STRING(oe-ordl.qty-on-bo)
        cRaInvNo       = STRING(oe-ordl.ra-inv-no)
        cRaNo          = STRING(oe-ordl.ra-no)
        cRec_key       = STRING(oe-ordl.rec_key)
        cRel           = STRING(oe-ordl.rel)
        cRelQty        = STRING(oe-ordl.rel-qty)
        cRelStat       = STRING(oe-ordl.rel-stat)
        cReqCode       = STRING(oe-ordl.req-code)
        cReqDate       = STRING(oe-ordl.req-date) 
        cReqTime       = STRING(oe-ordl.req-time)
        cSBasis        = STRING(oe-ordl.s-basis[1]) 
        cSComm[1]        = STRING(oe-ordl.s-comm[1]) 
        cSComm[2]        = STRING(oe-ordl.s-comm[2])
        cSComm[3]        = STRING(oe-ordl.s-comm[3])
        cSMan[1]          = STRING(oe-ordl.s-man[1]) 
        cSMan[2]          = STRING(oe-ordl.s-man[1])
        cSMan[3]          = STRING(oe-ordl.s-man[2])
        cSPct[1]          = STRING(oe-ordl.s-pct[3]) 
        cSPct[2]          = STRING(oe-ordl.s-pct[2])
        cSPct[3]         = STRING(oe-ordl.s-pct[3])
        cScore         = STRING(oe-ordl.score[1]) 
        cSetHdrLine    = STRING(oe-ordl.set-hdr-line)
        cSetup         = STRING(oe-ordl.setup)
        cSheetLen      = STRING(oe-ordl.sheet-len)
        cSheetWid      = STRING(oe-ordl.sheet-wid)
        cShipI         = STRING(oe-ordl.ship-i[1]) 
        cShipId        = STRING(oe-ordl.ship-id)
        cShipQty       = STRING(oe-ordl.ship-qty)
        cSpareChar1    = STRING(oe-ordl.spare-char-1)
        cSpareChar2    = STRING(oe-ordl.spare-char-2)
        cSpareChar3    = STRING(oe-ordl.spare-char-3)
        cSpareChar4    = STRING(oe-ordl.spare-char-4)
        cSpareChar5    = STRING(oe-ordl.spare-char-5)
        cSpareDec1     = STRING(oe-ordl.spare-dec-1)
        cSpareDec2     = STRING(oe-ordl.spare-dec-2)
        cSpareDec3     = STRING(oe-ordl.spare-dec-3)
        cSpareDec4     = STRING(oe-ordl.spare-dec-4)
        cSpareDec5     = STRING(oe-ordl.spare-dec-5)
        cSpareInt1     = STRING(oe-ordl.spare-int-1)
        cSpareInt2     = STRING(oe-ordl.spare-int-2)
        cSpareInt3     = STRING(oe-ordl.spare-int-3)
        cSpareInt4     = STRING(oe-ordl.spare-int-4)
        cSpareInt5     = STRING(oe-ordl.spare-int-5)
        cStackCode     = STRING(oe-ordl.stack-code)
        cStat          = STRING(oe-ordl.stat)
        cStyle         = STRING(oe-ordl.style)
        cTCost         = STRING(oe-ordl.t-cost)
        cTDep          = STRING(oe-ordl.t-dep)
        cTFreight      = STRING(oe-ordl.t-freight)
        cTFreightP     = STRING(oe-ordl.t-freight-p)
        cTFuel         = STRING(oe-ordl.t-fuel)
        cTInvPallets   = STRING(oe-ordl.t-inv-pallets)
        cTInvQty       = STRING(oe-ordl.t-inv-qty)
        cTInvWeight    = STRING(oe-ordl.t-inv-weight)
        cTLen          = STRING(oe-ordl.t-len)
        cTPrice        = STRING(oe-ordl.t-price)
        cTRelQty       = STRING(oe-ordl.t-rel-qty)
        cTShipQty      = STRING(oe-ordl.t-ship-qty)
        cTWeight       = STRING(oe-ordl.t-weight)
        cTWid          = STRING(oe-ordl.t-wid)
        cTax           = STRING(oe-ordl.tax)
        cTest          = STRING(oe-ordl.test)
        cTypeCode      = STRING(oe-ordl.type-code)
        cUnderPct      = STRING(oe-ordl.under-pct)
        cUnitCount     = STRING(oe-ordl.unit-count)
        cUnitsPallet   = STRING(oe-ordl.units-pallet)
        cUpdDate       = STRING(oe-ordl.upd-date) 
        cUpdTime       = STRING(oe-ordl.upd-time)
        cUserId        = STRING(oe-ordl.user-id)
        cVaroh         = STRING(oe-ordl.varoh)
        cVendNo        = STRING(oe-ordl.vend-no)
        cWScore        = STRING(oe-ordl.w-score[1]) 
        cWhsItem       = STRING(oe-ordl.whs-item)
        cWhsed         = STRING(oe-ordl.whsed)
        .

             
    RUN updateRequestData(INPUT-OUTPUT lcLineData, "LineNum", cLineNum).
    RUN updateRequestData(INPUT-OUTPUT lcLineData, "QtyUOM", cQtyUOM).
    RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemPrice", cItemPrice).
    RUN updateRequestData(INPUT-OUTPUT lcLineData, "PartQualifier", cPartQualifier).
    RUN updateRequestData(INPUT-OUTPUT lcLineData, "BuyerPart", cBuyerPart).
    RUN updateRequestData(INPUT-OUTPUT lcLineData, "itemDescription", cItemDesc).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Adder", cAdder).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"BlankNo", cBlankNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Carrier", cCarrier).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"CasCnt", cCasCnt).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Cases", cCases).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Cases-unit", cCases-unit).        
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Company", cCompany).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Cost", cCost).     
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"CustNo", cCustNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"DScore", cDScore).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"DelZone", cDelZone).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Deleted", cDeleted).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Disc", cDisc).     
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"ENum", cENum).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"EstNo", cEstNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"EstType", cEstType).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Fixoh", cFixoh).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Flute", cFlute).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"FormNo", cFormNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"FrtPay", cFrtPay).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"FtickPrnt", cFtickPrnt).         
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"IDscr", cIDscr).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"InvQty", cInvQty).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"IsAComponen", cIsAComponent).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"JNo", cJNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"JobNo", cJobNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"JobNo2", cJobNo2).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"JobStartDat", cJobStartDate). 
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"JobStartTim", cJobStartTime).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"LScore", cLScore).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Lab", cLab).       
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"LotNo", cLotNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Managed", cManaged).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Matl", cMatl).     
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Opened", cOpened).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"OrdNo", cOrdNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"OrdqNo", cOrdqNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"OverPct", cOverPct).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PalChg", cPalChg).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PalINo", cPalINo).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PartDscr1", cPartDscr1).         
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PartDscr2", cPartDscr2).         
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PartDscr3", cPartDscr3).         
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PartNo", cPartNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Partial", cPartial).        
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PoNoPo", cPoNoPo).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PoNo2", cPoNo2).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PromCode", cPromCode).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PromDate", cPromDate).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"PriceUom", cPrUom).     
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"QLine", cQLine).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"QNo", cQNo).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"OrdQty", cQtyOrdered).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"QRel", cQRel).         
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"QtyOnBo", cQtyOnBo).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"RaInvNo", cRaInvNo).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"RaNo", cRaNo).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Rec_key", cRec_key).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Rel", cRel).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"RelQty", cRelQty).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"RelStat", cRelStat).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"ReqCode", cReqCode).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"ReqDate", cReqDate).     
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"ReqTime", cReqTime).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SBasis", cSBasis).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SCommission1", cSComm[1]).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SCommission2", cSComm[2]).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SCommission3", cSComm[3]).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Salesman1", cSMan[1]).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Salesman2", cSMan[2]).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Salesman3", cSMan[3]).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SPct1", cSPct[1]).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SPct2", cSPct[2]).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SPct3", cSPct[3]).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Score", cScore).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SetHdrLine", cSetHdrLine).
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Setup", cSetup).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SheetLen", cSheetLen).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SheetWid", cSheetWid).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"ShipI", cShipI).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"ShipId", cShipId).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"ShipQty", cShipQty).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareChar1", cSpareChar1).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareChar2", cSpareChar2).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareChar3", cSpareChar3).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareChar4", cSpareChar4).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareChar5", cSpareChar5).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareDec1", cSpareDec1).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareDec2", cSpareDec2).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareDec3", cSpareDec3).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareDec4", cSpareDec4).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareDec5", cSpareDec5).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareInt1", cSpareInt1).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareInt2", cSpareInt2).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareInt3", cSpareInt3).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareInt4", cSpareInt4).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"SpareInt5", cSpareInt5).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"StackCode", cStackCode).         
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Stat", cStat).     
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Style", cStyle).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TCost", cTCost).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TDep", cTDep).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TFreight", cTFreight).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TFreightP", cTFreightP).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TFuel", cTFuel).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TInvPallets", cTInvPallets).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TInvQty", cTInvQty).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TInvWeight", cTInvWeight).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TLen", cTLen).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TPrice", cTPrice).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TRelQty", cTRelQty).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TShipQty", cTShipQty).    
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TWeight", cTWeight).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TWid", cTWid).      
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Tax", cTax).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Test", cTest).     
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"TypeCode", cTypeCode).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"UnderPct", cUnderPct).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"UnitCount", cUnitCount).         
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"UnitsPallet", cUnitsPallet).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"UpdDate", cUpdDate).     
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"UpdTime", cUpdTime).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"UserId", cUserId).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Varoh", cVaroh).  
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"VendNo", cVendNo).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"WScore", cWScore).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"WhsItem", cWhsItem).   
    RUN updateRequestData(INPUT-OUTPUT lcLineData,"Whsed", cWhsed).  
       
    RUN updateRequestData(INPUT-OUTPUT lcLineData, "linefeed", "~n").  
    lcConcatLineData = lcConcatLineData +  lcLineData + "~n". 

END.      

        
             
                    
lcConcatLineMiscChargeData = "".
IF AVAILABLE bf-APIOutboundDetail2 THEN 
DO:
    /* Fetch MiscCharge details for the Order MiscCharge */           
    /* Added charges section, i.e. Freight, Tax, Etc */

    FOR EACH oe-ordm OF oe-ord NO-LOCK
        WHERE oe-ordm.deleted = FALSE               
        :             
        /* Added Charge section has 1 line per iteration */

        ASSIGN
            lcLineMiscChargeData = STRING(bf-APIOutboundDetail2.data)
            iLineCount      = iLineCount + 1
            cLineCnt        = STRING(iLineCount)            
            cCompany        = STRING(oe-ordm.company)
            cOrdNo          = STRING(oe-ordm.ord-no)
            cLine           = STRING(oe-ordm.line)
            cCharge         = STRING(oe-ordm.charge)
            cAmt            = STRING(oe-ordm.amt )
            cActnum         = STRING(oe-ordm.actnum)
            cDscr           = STRING(oe-ordm.dscr)
            cEstNo          = STRING(oe-ordm.est-no)
            cMiscChargeTax       = STRING(oe-ordm.tax )
            cMiscChargeCost      = STRING(oe-ordm.cost)
            cBill           = STRING(oe-ordm.bill)
            cPosted         = STRING(oe-ordm.posted)
            cDeleted        = STRING(oe-ordm.deleted)
            cOrdINo         = STRING(oe-ordm.ord-i-no)
            cOrdLine        = STRING(oe-ordm.ord-line)
            cRec_key        = STRING(oe-ordm.rec_key)
            cUpdDate        = STRING(oe-ordm.upd-date)
            cUpdTime        = STRING(oe-ordm.upd-time)
            cSMan[1]        = STRING(oe-ordm.s-man[1])
            cSMan[2]        = STRING(oe-ordm.s-man[2])
            cSMan[3]        = STRING(oe-ordm.s-man[3])
            cSPct[1]        = STRING(oe-ordm.s-pct[1])
            cSPct[2]        = STRING(oe-ordm.s-pct[2])
            cSPct[3]        = STRING(oe-ordm.s-pct[3])
            cSComm[1]       = STRING(oe-ordm.s-comm[1])
            cSComm[2]       = STRING(oe-ordm.s-comm[2]) 
            cSComm[3]       = STRING(oe-ordm.s-comm[3])
            cCommbasis[ 1]  = STRING(oe-ordm.commbasis[ 1])
            cCommbasis[ 2]  = STRING(oe-ordm.commbasis[ 2])
            cCommbasis[ 3]  = STRING(oe-ordm.commbasis[ 3])
            cCommbasis[ 4]  = STRING(oe-ordm.commbasis[ 4])
            cCommbasis[ 5]  = STRING(oe-ordm.commbasis[ 5])
            cCommbasis[ 6]  = STRING(oe-ordm.commbasis[ 6])
            cCommbasis[ 7]  = STRING(oe-ordm.commbasis[ 7])
            cCommbasis[ 8]  = STRING(oe-ordm.commbasis[ 8])
            cCommbasis[ 9]  = STRING(oe-ordm.commbasis[ 9])
            cCommbasis[10]  = STRING(oe-ordm.commbasis[10])
            cEqty           = STRING(oe-ordm.eqty)
            cEstLine        = STRING(oe-ordm.est-line)
            cPrepType       = STRING(oe-ordm.prep-type)
            cEstExtent      = STRING(oe-ordm.est-extent)
            cPoNo           = STRING(oe-ordm.po-no)
            cPoNoPo         = STRING(oe-ordm.po-no-po)
            cFormNo         = STRING(oe-ordm.form-no)
            cBlankNo        = STRING(oe-ordm.blank-no)
            cSpareChar1     = STRING(oe-ordm.spare-char-1)
            cSpareChar2     = STRING(oe-ordm.spare-char-2)
            cSpareChar3     = STRING(oe-ordm.spare-char-3)
            cSpareChar4     = STRING(oe-ordm.spare-char-4)
            cSpareChar5     = STRING(oe-ordm.spare-char-5)
            cSpareDec1      = STRING(oe-ordm.spare-dec-1)
            cSpareDec2      = STRING(oe-ordm.spare-dec-2)
            cSpareDec3      = STRING(oe-ordm.spare-dec-3)
            cSpareDec4      = STRING(oe-ordm.spare-dec-4)
            cSpareDec5      = STRING(oe-ordm.spare-dec-5)
            cSpareInt1      = STRING(oe-ordm.spare-int-1)
            cSpareInt2      = STRING(oe-ordm.spare-int-2)
            cSpareInt3      = STRING(oe-ordm.spare-int-3)
            cSpareInt4      = STRING(oe-ordm.spare-int-4)
            cSpareInt5       = STRING(oe-ordm.spare-int-5)
            cEstPrepEqty    = STRING(oe-ordm.estPrepEqty)
            cEstPrepLine    = STRING(oe-ordm.estPrepLine)
            cMiscType       = STRING(oe-ordm.miscType)
            cMiscInd        = STRING(oe-ordm.miscInd)
            .            
        /*                     oe-ordm.charge oe-ordm.dscr  oe-ordm.po-no 
                    oe-ordm.amt . */
        /* this section in case the misc charges are using the same fields as the detail */
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"LineNum",      cLineCnt).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"ItemPrice",      cAmt).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"PartNo",      cCharge).
        
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"company",      cCompany).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"ord-no",        cOrdNo).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"line",          cLine).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"charge",        cCharge).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"amt",           cAmt).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"actnum",        cActnum).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"dscr",          cDscr).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"est-no",        cEstNo).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"tax",           cMiscChargeTax).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"cost",          cMiscChargeCost).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"bill",          cBill).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"posted",        cPosted).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"deleted",       cDeleted).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"ord-i-no",      cOrdINo).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"ord-line",      cOrdLine).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"rec_key",       cRec_key).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"upd-date",      cUpdDate).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"upd-time",      cUpdTime).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-man[1]",      cSMan[1]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-man[2]",      cSMan[2]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-man[3]",      cSMan[3]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-pct[1]",      cSPct[1]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-pct[2]",      cSPct[2]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-pct[3]",      cSPct[3]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-comm[1]",     cSComm[1]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-comm[2]",     cSComm[2]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"s-comm[3]",     cSComm[3]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 1]", cCommbasis[ 1]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 2]", cCommbasis[ 2]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 3]", cCommbasis[ 3]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 4]", cCommbasis[ 4]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 5]", cCommbasis[ 5]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 6]", cCommbasis[ 6]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 7]", cCommbasis[ 7]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 8]", cCommbasis[ 8]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[ 9]", cCommbasis[ 9]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"commbasis[10]", cCommbasis[10]).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"eqty",          cEqty).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"est-line",      cEstLine).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"prep-type",     cPrepType).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"est-extent",    cEstExtent).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"po-no",         cPoNo).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"po-no-po",      cPoNoPo).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"form-no",       cFormNo).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"blank-no",      cBlankNo).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-char-1", cSpareChar1).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-char-2", cSpareChar2).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-char-3", cSpareChar3).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-char-4", cSpareChar4).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-char-5", cSpareChar5).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-dec-1",  cSpareDec1).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-dec-2",  cSpareDec2).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-dec-3",  cSpareDec3).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-dec-4",  cSpareDec4).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-dec-5",  cSpareDec5).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData, "spare-int-1",  cSpareInt1).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-int-2",  cSpareInt2).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-int-3",  cSpareInt3).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-int-4",  cSpareInt4).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"spare-int-5",  cSpareInt5).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"estPrepEqty",  cEstPrepEqty).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"estPrepLine",  cEstPrepLine).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"miscType",     cMiscType).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData,"miscInd",      cMiscInd).
        RUN updateRequestData(INPUT-OUTPUT lcLineMiscChargeData, "linefeed", "~n").
        lcConcatLineMiscChargeData = lcConcatLineMiscChargeData + "" + lcLineMiscChargeData.
    END.
                      

END.      

/* EDI X12 related */
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IsaDate", cIsaDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IsaTime", cIsaTime). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "IsaControlNum", cIsaControlSeq).   
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GsDate", cIsaDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GsTime", cIsaTime).           
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "GsControlNum", cIsaControlSeq).   
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "StControlNum", cStControlSeq). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BigDate", cBigDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BigDocID", cBigDocID). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "LineCount", iLineCount).

RUN format_date IN hFormatProcs (oe-ord.ord-date, "YYYYMMDD", OUTPUT cOrdDateYMD).
ASSIGN 
    cAckPrnt         = STRING(oe-ord.ack-prnt)
    cAckPrntDate     = STRING(oe-ord.ack-prnt-date)
    cAddr            = STRING(oe-ord.addr[1])
    cApprovedDate    = STRING(oe-ord.approved-date)
    cApprovedId      = STRING(oe-ord.approved-id)
    cBankCode        = STRING(oe-ord.bank-code)
    cBillI           = STRING(oe-ord.bill-i[1])
    cBillTo          = STRING(oe-ord.bill-to)
    cBillFreight     = STRING(oe-ord.billFreight)
    cCarrier         = STRING(oe-ord.carrier)
    cCcActnum        = STRING(oe-ord.cc-actnum)
    cCcAddr          = STRING(oe-ord.cc-addr[1]) 
    cCcAuth          = STRING(oe-ord.cc-auth)
    cCcBank          = STRING(oe-ord.cc-bank)
    cCcCity          = STRING(oe-ord.cc-city) 
    cCcCompany       = STRING(oe-ord.cc-company)
    cCcCountry       = STRING(oe-ord.cc-country)
    cCcCvvCode       = STRING(oe-ord.cc-cvv-code)
    cCcExpiration    = STRING(oe-ord.cc-expiration)
    cCcName          = STRING(oe-ord.cc-name)
    cCcNum           = STRING(oe-ord.cc-num)
    cCcPhone         = STRING(oe-ord.cc-phone)
    cCcState         = STRING(oe-ord.cc-state)
    cCType           = STRING(oe-ord.cc-type)
    cCcZip           = STRING(oe-ord.cc-zip)
    cchType          = STRING(oe-ord.ch-type)
    cCity            = STRING(oe-ord.city)
    cCloseChecked    = STRING(oe-ord.close-checked)
    cCloseDate       = STRING(oe-ord.closeDate)
    closeTime        = STRING(oe-ord.closeTime)
    cCloseUser       = STRING(oe-ord.closeUser)
    ccompany         = STRING(oe-ord.company)
    cContact         = STRING(oe-ord.contact) 
    cCsrUserId       = STRING(oe-ord.csrUser_id) 
    cCurrCode        = STRING(oe-ord.curr-code[1]) 
    cCustName        = STRING(oe-ord.cust-name)
    cCustNo          = STRING(oe-ord.cust-no) 
    cDelZone         = STRING(oe-ord.del-zone)
    cDeleted         = STRING(oe-ord.deleted)
    cDueCode         = STRING(oe-ord.due-code)
    cDueDate         = STRING(oe-ord.due-date)
    cDueTime         = STRING(oe-ord.due-time)
    cENum            = STRING(oe-ord.e-num  ) 
    cEmailAddr       = STRING(oe-ord.email-addr)
    cEmailName       = STRING(oe-ord.email-name)
    cEnteredDate     = STRING(oe-ord.entered-date)
    cEnteredId       = STRING(oe-ord.entered-id)
    cEstNo           = STRING(oe-ord.est-no)
    cEstType         = STRING(oe-ord.est-type)
    cExRate          = STRING(oe-ord.ex-rate)
    cFBill           = STRING(oe-ord.f-bill)
    cFobCode         = STRING(oe-ord.fob-code)
    cFrtPay          = STRING(oe-ord.frt-pay)
    cFtckPrnt        = STRING(oe-ord.ftck-prnt) 
    cFuelBill        = STRING(oe-ord.fuel-bill)
    cInvDate         = STRING(oe-ord.inv-date)
    cInvNo           = STRING(oe-ord.inv-no )
    cJNo             = STRING(oe-ord.j-no   )
    cJobNo           = STRING(oe-ord.job-no)
    cJobNo2          = STRING(oe-ord.job-no2)
    cLastDate        = STRING(oe-ord.last-date)
    cLeadDays        = STRING(oe-ord.lead-days)
    cLoc             = STRING(oe-ord.loc )
    cManaged         = STRING(oe-ord.managed)
    cOpened          = STRING(oe-ord.opened) 
    cOrdDate         = STRING(oe-ord.ord-date)
    cOrdNo           = STRING(oe-ord.ord-no )
    cOverPct         = STRING(oe-ord.over-pct)
    cPoNo            = STRING(oe-ord.po-no)
    cPoNo2           = STRING(oe-ord.po-no2)
    cPordNo          = STRING(oe-ord.pord-no) 
    cPoReceivedDate  = STRING(oe-ord.poReceivedDate)
    cPosted          = STRING(oe-ord.posted)
    cPriceHold       = STRING(oe-ord.priceHold) 
    cPriceHoldReason = STRING(oe-ord.priceHoldReason)
    cProdDate        = STRING(oe-ord.prod-date)
    cQNo             = STRING(oe-ord.q-no   )
    cRec_key         = STRING(oe-ord.rec_key) 
    cSComm[1]           = STRING(oe-ord.s-comm[1] )
    cSPct            = STRING(oe-ord.s-pct[1]  )
    cShipI           = STRING(oe-ord.ship-i[1]) 
    cShipId          = STRING(oe-ord.ship-id)
    cSman[1]            = STRING(oe-ord.sman[1]) 
    cSmanNo          = STRING(oe-ord.sman-no)
    cSname           = STRING(oe-ord.sname[1])
    cSoldAddr        = STRING(oe-ord.sold-addr[1])
    cSoldCity        = STRING(oe-ord.sold-city)
    cSoldId          = STRING(oe-ord.sold-id)
    cSoldName        = STRING(oe-ord.sold-name)
    cSoldNo          = STRING(oe-ord.sold-no)
    cSoldState       = STRING(oe-ord.sold-state)
    cSoldZip         = STRING(oe-ord.sold-zip)
    cSpareChar1      = STRING(oe-ord.spare-char-1)
    cSpareChar2      = STRING(oe-ord.spare-char-2)
    cSpareChar3      = STRING(oe-ord.spare-char-3)
    cSpareChar4      = STRING(oe-ord.spare-char-4)
    cSpareChar5      = STRING(oe-ord.spare-char-5)
    cSpareDec1       = STRING(oe-ord.spare-dec-1)
    cSpareDec2       = STRING(oe-ord.spare-dec-2)
    cSpareDec3       = STRING(oe-ord.spare-dec-3)
    cSpareDec4       = STRING(oe-ord.spare-dec-4)
    cSpareDec5       = STRING(oe-ord.spare-dec-5)
    cSpareInt1       = STRING(oe-ord.spare-int-1)
    cSpareInt2       = STRING(oe-ord.spare-int-2)
    cSpareInt3       = STRING(oe-ord.spare-int-3)
    cSpareInt4       = STRING(oe-ord.spare-int-4)
    cSpareInt5       = STRING(oe-ord.spare-int-5)
    cStat            = STRING(oe-ord.stat)
    cState           = STRING(oe-ord.state)
    cTComm           = STRING(oe-ord.t-comm )
    cTCost           = STRING(oe-ord.t-cost )
    cTFreight        = STRING(oe-ord.t-freight)
    cTFuel           = STRING(oe-ord.t-fuel )
    cTInvCost        = STRING(oe-ord.t-inv-cost )
    cTInvFreight     = STRING(oe-ord.t-inv-freight)
    cTInvRev         = STRING(oe-ord.t-inv-rev  )
    cTInvTax         = STRING(oe-ord.t-inv-tax  )
    cTInvWeight      = STRING(oe-ord.t-inv-weight)
    cTRevenue        = STRING(oe-ord.t-revenue)
    cTWeight         = STRING(oe-ord.t-weight)
    cTax             = STRING(oe-ord.tax    )
    cTaxGr           = STRING(oe-ord.tax-gr)
    cTerms           = STRING(oe-ord.terms)
    cTermsD          = STRING(oe-ord.terms-d)
    cTotOrd          = STRING(oe-ord.tot-ord)
    cType            = STRING(oe-ord.type)
    cUnderPct        = STRING(oe-ord.under-pct)
    cUpdDate         = STRING(oe-ord.upd-date)
    cUpdTime         = STRING(oe-ord.upd-time)
    cUpdatedDate     = STRING(oe-ord.updated-date)
    cUpdatedId       = STRING(oe-ord.updated-id)
    cUserId          = STRING(oe-ord.user-id)
    cWhsRder         = STRING(oe-ord.whs-order)
    cWhsed           = STRING(oe-ord.whsed)
    cZip             = STRING(oe-ord.zip ) 
    .       
    RUN format_date IN hFormatProcs (oe-ord.ord-date, "YYYYMMDD", OUTPUT cEdiOrdDate).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "AckPrnt", cAckPrnt). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "AckPrntDate", cAckPrntDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Addr", cAddr). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ApprovedDat", cApprovedDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ApprovedId", cApprovedId). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BankCode", cBankCode). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillI", cBillI). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillTo", cBillTo). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "BillFreight", cBillFreight). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Carrier", cCarrier). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcActnum", cCcActnum). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcAddr", cCcAddr). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcAuth", cCcAuth). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcBank", cCcBank). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcCity", cCcCity). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcCompany", cCcCompany). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcCountry", cCcCountry). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcCvvCode", cCcCvvCode). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcExpiratio", cCcExpiration). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcName", cCcName). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcNum", cCcNum). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcPhone", cCcPhone). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcState", cCcState). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CType", cCType). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CcZip", cCcZip). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "chType", cchType). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "City", cCity). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CloseChecked", cCloseChecked). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CloseDate", cCloseDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "loseTime", closeTime). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CloseUser", cCloseUser). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "company", ccompany). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Contact", cContact). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CsrUserId", cCsrUserId). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CurrCode", cCurrCode). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustName", cCustName). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "CustNo", cCustNo). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "DelZone", cDelZone). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Deleted", cDeleted). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "DueCode", cDueCode). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "DueDate", cDueDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "EdiOrdDate", cEdiOrdDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "EmailAddr", cEmailAddr). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "EmailName", cEmailName). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "EnteredDate", cEnteredDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "EnteredId", cEnteredId). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "EstNo", cEstNo). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ExRate", cExRate).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FBill", cFBill). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FobCode", cFobCode). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FrtPay", cFrtPay). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FtckPrnt", cFtckPrnt). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "FuelBill", cFuelBill). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvDate", cInvDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "InvNo", cInvNo).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "JNo", cJNo).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "JobNo", cJobNo). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "JobNo2", cJobNo2).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "LastDate", cLastDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "LeadDays", cLeadDays).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Loc", cLoc). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Managed", cManaged). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Opened", cOpened). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "OrdDate", cOrdDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "OrdNo", cOrdNo).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "OverPct", cOverPct).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PurchaseOrderNum", cPoNo). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PoNo2", cPoNo2). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PordNo", cPordNo).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PoReceivedDt", cPoReceivedDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Posted", cPosted). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PriceHold", cPriceHold). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "PriceHoldRes", cPriceHoldReason). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ProdDate", cProdDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "QNo", cQNo).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Rec_key", cRec_key). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SComm", cSComm[1]).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SPct", cSPct[1]).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipI", cShipI). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "ShipId", cShipId). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Sman", cSman[1]). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SmanNo", cSmanNo).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Sname", cSname). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SoldAddr", cSoldAddr). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SoldCity", cSoldCity). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SoldId", cSoldId). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SoldName", cSoldName). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SoldNo", cSoldNo).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SoldState", cSoldState). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SoldZip", cSoldZip). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareChar1", cSpareChar1).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareChar2", cSpareChar2).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareChar3", cSpareChar3).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareChar4", cSpareChar4).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareChar5", cSpareChar5).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareDec1", cSpareDec1).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareDec2", cSpareDec2).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareDec3", cSpareDec3).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareDec4", cSpareDec4).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareDec5", cSpareDec5).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareInt1", cSpareInt1).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareInt2", cSpareInt2).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareInt3", cSpareInt3).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareInt4", cSpareInt4).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SpareInt5", cSpareInt5).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Stat", cStat). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "State", cState). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TComm", cTComm).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TCost", cTCost).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TFreight", cTFreight).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TFuel", cTFuel).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TInvCost", cTInvCost).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TInvFreight", cTInvFreight).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TInvRev", cTInvRev).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TInvTax", cTInvTax).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TInvWeight", cTInvWeight).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TRevenue", cTRevenue).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TWeight", cTWeight).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Tax", cTax).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TaxGr", cTaxGr). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Terms", cTerms). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TermsD", cTermsD). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotOrd", cTotOrd).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Type", cType). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "UnderPct", cUnderPct).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "UpdDate", cUpdDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "UpdTime", cUpdTime).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "UpdatedDate", cUpdatedDate). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "UpdatedId", cUpdatedId). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "UserId", cUserId).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "WhsRder", cWhsRder). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Whsed", cWhsed). 
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Zip", cZip).  
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "Currency", "USD").
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "TotalAmount", cTotalAmount).
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "OrderDateYMD", cEdiOrdDate).


          
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "invNotes", cInvNotes).
    
/* If the previous section was not blank, it ended with CR so don't need to start with one */
     
lcConcatLineData = TRIM(lcConcatLineData, "~n").
        
lcConcatLineMiscChargeData = TRIM(lcConcatLineMiscChargeData,"~n").         

          
ioplcRequestData = REPLACE(ioplcRequestData, "[$Detail$]", (IF lcConcatLineData NE "" THEN "~n" ELSE "") + lcConcatLineData).
                                                                 
ioplcRequestData = REPLACE(ioplcRequestData, "[$MiscCharge$]", (IF lcConcatLineMiscChargeData NE "" THEN  "~n" ELSE "") + lcConcatLineMiscChargeData).
ASSIGN 
  dSELineCount = NUM-ENTRIES(ioplcRequestData, "~~") - 1  
  /* Subtract lines before ST and after SE segments */
  dSELineCount = dSELineCount - 4
  cSELineCount = STRING(dSELineCount)
  .
RUN updateRequestData(INPUT-OUTPUT ioplcRequestData, "SECount", STRING(cSELineCount)).
                           
RELEASE bf-APIOutboundDetail2.

        
ASSIGN
    opcMessage = ""
    oplSuccess = TRUE
    .

    
/* End of Main Code */
    

/* **********************  Internal Procedures  *********************** */




/* ************************  Function Implementations ***************** */

FUNCTION fnGetGSControl RETURNS INTEGER 
    ( ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE result AS INTEGER NO-UNDO.
    RESULT = 1.
    RETURN result.


        
END FUNCTION.

FUNCTION fnGetISAControl RETURNS INTEGER 
    ( ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE result AS INTEGER NO-UNDO.
    RESULT = 1.
    RETURN result.


        
END FUNCTION.
    
