
/*------------------------------------------------------------------------
    File        : BolProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Oct 31 EST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE giBolFmtIntegerValue AS INTEGER NO-UNDO .
DEFINE VARIABLE glBolFmtLogicalValue AS LOGICAL NO-UNDO .
            
{oe/ttBolHeader.i}
{oe/ttBolItem.i}     

DEFINE WORKFILE w2 NO-UNDO
    FIELD cases            AS   INTEGER FORMAT ">9"
    FIELD cas-cnt          AS   INTEGER FORMAT ">>>>9".
        
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */



/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddBolHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-bolh FOR oe-bolh.
    DEFINE VARIABLE cCarrier      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSalesman     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cTerms        AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cFrtTerms     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cPoNo         AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cJobNo        AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cFob          AS CHARACTER NO-UNDO.     
    DEFINE VARIABLE cCusxAdd1     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cCusxAdd2     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cCusxAdd3     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cCusxAdd4     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCusxAdd5     AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cCusxEmail    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCusxName     AS CHARACTER NO-UNDO.     
    DEFINE VARIABLE cCompanyAdd1  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cCompanyAdd2  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cCompanyAdd3  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cCompanyAdd4  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyAdd5  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cCompanyEmail AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompanyName  AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE cSoldtoName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSoldtoAddr   AS CHARACTER EXTENT 2 NO-UNDO. 
    DEFINE VARIABLE cSoldtoAddr3  AS CHARACTER NO-UNDO. 
    
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    DEFINE BUFFER bf-cust FOR cust.           
    
    IF NOT AVAILABLE ipbf-oe-bolh THEN
        RETURN.
    
    FIND FIRST ttBolHeader
        WHERE ttBolHeader.riBol EQ ROWID(ipbf-oe-bolh) 
        NO-ERROR.
    IF AVAILABLE ttBolHeader THEN
        RETURN.
           
    CREATE ttBolHeader.
    ASSIGN
        ttBolHeader.riBol      = ROWID(ipbf-oe-bolh)
        ttBolHeader.company    = ipbf-oe-bolh.company
        ttBolHeader.locationID = ipbf-oe-bolh.loc
        ttBolHeader.BolID      = ipbf-oe-bolh.bol-no         
        ttBolHeader.customerID = ipbf-oe-bolh.cust-no        
        ttBolHeader.carrier    = ipbf-oe-bolh.carrier
        .
           
    IF glBolFmtLogicalValue THEN 
    DO:
        FIND FIRST bf-cust WHERE bf-cust.company = ipbf-oe-bolh.company AND
            bf-cust.active = "X" NO-LOCK NO-ERROR.
        IF AVAILABLE bf-cust THEN
            ASSIGN cCompanyAdd1  = bf-cust.addr[1]
                cCompanyAdd2  = bf-cust.addr[2]
                cCompanyAdd3  = bf-cust.city + "  " + bf-cust.state + "  " + bf-cust.zip
                cCompanyAdd4  = "Phone:  " + string(bf-cust.area-code,"(999)") + string(bf-cust.phone,"999-9999") 
                cCompanyAdd5  = "Fax     :  " + string(bf-cust.fax,"(999)999-9999") 
                cCompanyEmail = "Email:  " + bf-cust.email 
                cCompanyName  = bf-cust.NAME   
                cCusxAdd1     = cCompanyAdd1
                cCusxAdd2     = cCompanyAdd2
                cCusxAdd3     = cCompanyAdd3
                cCusxAdd4     = cCompanyAdd4
                cCusxAdd5     = cCompanyAdd5
                cCusxEmail    = cCompanyEmail
                cCusxName     = cCompanyName
                .            
    END.    
            
    FIND FIRST carrier NO-LOCK
        WHERE carrier.company EQ ipbf-oe-bolh.company
        AND carrier.carrier EQ ipbf-oe-bolh.carrier
        NO-ERROR.
    cCarrier = IF AVAILABLE carrier THEN carrier.carrier ELSE "".
    
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ ipbf-oe-bolh.company
        AND cust.cust-no EQ ipbf-oe-bolh.cust-no
        NO-ERROR. 
    FIND FIRST oe-boll NO-LOCK
        WHERE oe-boll.company EQ ipbf-oe-bolh.company 
        AND oe-boll.b-no EQ ipbf-oe-bolh.b-no
        AND oe-boll.ord-no NE 0 NO-ERROR.
    IF AVAILABLE oe-boll THEN
    DO:
        FIND FIRST oe-ord NO-LOCK 
            WHERE oe-ord.company EQ oe-boll.company
            AND oe-ord.ord-no  EQ oe-boll.ord-no
            NO-ERROR.
                     
        IF NOT AVAILABLE carrier AND AVAILABLE oe-ord THEN 
        DO:
            FIND FIRST carrier NO-LOCK
                WHERE carrier.company EQ oe-ord.company
                AND carrier.carrier EQ oe-ord.carrier  NO-ERROR.
            cCarrier = IF AVAILABLE carrier THEN carrier.carrier ELSE "".
        END.
        IF AVAILABLE oe-ord THEN
        DO iCount = 1 TO 3:
            IF oe-ord.sman[iCount] NE "" THEN
                cSalesman = TRIM(cSalesman) + " " + oe-ord.sman[iCount] + ",".
        END.  
              
        ASSIGN 
            cTerms    = oe-ord.terms-d
            cFrtTerms = IF ipbf-oe-bolh.frt-pay EQ "P" THEN "Prepaid"
                           ELSE IF ipbf-oe-bolh.frt-pay EQ "B" THEN "Bill"
                           ELSE IF ipbf-oe-bolh.frt-pay EQ "C" THEN "Collect"
                           ELSE IF ipbf-oe-bolh.frt-pay EQ "T" THEN "Third Party"
                           ELSE ""  .
                           
        IF cTerms EQ "" AND AVAILABLE oe-ord THEN
        DO:
            FIND FIRST terms WHERE terms.t-code EQ oe-ord.terms NO-LOCK NO-ERROR.
            IF AVAILABLE terms THEN
                ASSIGN cTerms = terms.t-code.
        END.
    END. 
                
    cSalesman = TRIM(cSalesman).
    cPoNo = oe-boll.po-no.
    cJobNo = IF oe-boll.job-no = "" THEN "" ELSE 
        TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-boll.job-no, oe-boll.job-no2))).
                        
    IF cSalesman GT '' THEN
        IF substr(cSalesman,LENGTH(TRIM(cSalesman)),1) EQ "," THEN
            substr(cSalesman,LENGTH(TRIM(cSalesman)),1) = "".

    cFob = IF AVAILABLE oe-ord AND oe-ord.fob-code BEGINS "ORIG" THEN "Origin" ELSE "Destination".
    
    RUN oe/custxship.p (ipbf-oe-bolh.company,
        ipbf-oe-bolh.cust-no,
        ipbf-oe-bolh.ship-id,
        BUFFER shipto).
    ASSIGN
        ttBolHeader.shipName      = shipto.ship-name
        ttBolHeader.shipAddr[1]   = shipto.ship-addr[1]
        ttBolHeader.shipAddr[2]   = shipto.ship-addr[2]
        ttBolHeader.shipAddr3     = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
        ttBolHeader.custPhoneNum  = cust.area-code + cust.phone
        ttBolHeader.shipPhone     = IF shipto.area-code + shipto.phone <> "" THEN
                      "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx")
                      ELSE ""
        ttBolHeader.bolPhone      = IF ipbf-oe-bolh.area-code + ipbf-oe-bolh.phone <> "" THEN 
              "(" + ipbf-oe-bolh.area-code + ")" + string(ipbf-oe-bolh.phone,"xxx-xxxx")
              ELSE ""
        ttBolHeader.shiptoContact = ipbf-oe-bolh.contact.    
    
    IF ttBolHeader.bolPhone = "" THEN ttBolHeader.bolPhone = "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx").
    IF ttBolHeader.shiptoContact = "" THEN ttBolHeader.shiptoContact = shipto.contact.
    
    IF shipto.broker THEN 
    DO:
        ASSIGN 
            cCompanyAdd1  = cust.addr[1]
            cCompanyAdd2  = cust.addr[2]
            cCompanyAdd3  = cust.city + "  " +
                        cust.state + "  " +
                        cust.zip
            cCompanyAdd4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            cCompanyAdd5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            cCompanyEmail = "Email:  " + cust.email   
            cCompanyName  = cust.NAME .  
                        
        IF AVAILABLE oe-boll AND AVAILABLE oe-ord THEN 
        DO:             
            ASSIGN 
                cCompanyName = oe-ord.sold-name
                cCompanyAdd1 = oe-ord.sold-addr[1]
                cCompanyAdd2 = oe-ord.sold-addr[2]
                cCompanyAdd3 = oe-ord.sold-city + "  " +
                                  oe-ord.sold-state + "  " +
                                  oe-ord.sold-zip.  
        END.                        
    END.
    ELSE ASSIGN cCompanyAdd1  = cCusxAdd1
            cCompanyAdd2  = cCusxAdd2    
            cCompanyAdd3  = cCusxAdd3    
            cCompanyAdd4  = cCusxAdd4                
            cCompanyAdd5  = cCusxAdd5
            cCompanyEmail = cCusxEmail
            cCompanyName  = cCusxName.
                     
    FIND FIRST oe-boll WHERE oe-boll.company EQ ipbf-oe-bolh.company AND oe-boll.b-no EQ ipbf-oe-bolh.b-no NO-LOCK NO-ERROR.
    IF AVAILABLE oe-boll THEN 
    DO:
        FIND FIRST oe-ord WHERE oe-ord.company = ipbf-oe-bolh.company
            AND oe-ord.ord-no = oe-boll.ord-no NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ord THEN
            ASSIGN
                cSoldtoName    = oe-ord.sold-name
                cSoldtoAddr[1] = oe-ord.sold-addr[1]
                cSoldtoAddr[2] = oe-ord.sold-addr[2]
                cSoldtoAddr3   = oe-ord.sold-city + ", " +
                             oe-ord.sold-state + "  " +
                             oe-ord.sold-zip.
    END.
    
    IF TRIM(cSoldtoAddr3) EQ "," THEN cSoldtoAddr3 = "".               
    IF cSoldtoAddr[2] EQ "" THEN
        ASSIGN
            cSoldtoAddr[2] = cSoldtoAddr3
            cSoldtoAddr3   = "".
    IF ttBolHeader.shipAddr[2] EQ "" THEN
        ASSIGN
            ttBolHeader.shipAddr[2] = ttBolHeader.shipAddr3
            ttBolHeader.shipAddr3   = "".

    IF TRIM(ttBolHeader.shipAddr3) EQ "," THEN ttBolHeader.shipAddr3 = "".     
                       
    ASSIGN
        ttBolHeader.carrier       = cCarrier
        ttBolHeader.salesMan      = cSalesman
        ttBolHeader.terms         = cTerms
        ttBolHeader.frtTerms      = cFrtTerms
        ttBolHeader.poNo          = cPoNo
        ttBolHeader.jobNo         = cJobNo
        ttBolHeader.fob           = cFob           
        ttBolHeader.companyAdd1   = cCompanyAdd1
        ttBolHeader.companyAdd2   = cCompanyAdd2
        ttBolHeader.companyAdd3   = cCompanyAdd3
        ttBolHeader.companyAdd4   = cCompanyAdd4
        ttBolHeader.companyAdd5   = cCompanyAdd5
        ttBolHeader.companyEmail  = cCompanyEmail
        ttBolHeader.companyName   = cCompanyName          
        ttBolHeader.soldtoName    = cSoldtoName
        ttBolHeader.soldtoAddr[1] = cSoldtoAddr[1]
        ttBolHeader.soldtoAddr[2] = cSoldtoAddr[2]
        ttBolHeader.soldtoAddr3   = cSoldtoAddr3                                                               
        .         
        
END PROCEDURE.

PROCEDURE pAddBolItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-bolh FOR oe-bolh.
    DEFINE           BUFFER bf-oe-boll   FOR oe-boll.
    DEFINE OUTPUT PARAMETER opdTotalWeight AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTotCases    AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iShipQty  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dWeight   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iOrdQty   AS INTEGER   NO-UNDO.      
    DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cPartDscr AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cJobPo    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTotPkgs  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotCases AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dTotWt    AS DECIMAL   NO-UNDO.
    
    ASSIGN
        dTotWt    = 0
        iTotCases = 0
        .
    
    FOR EACH bf-oe-boll NO-LOCK
        WHERE bf-oe-boll.company EQ ipbf-oe-bolh.company
        AND bf-oe-boll.b-no EQ ipbf-oe-bolh.b-no,
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ bf-oe-boll.company
        AND itemfg.i-no    EQ bf-oe-boll.i-no
        BREAK BY bf-oe-boll.i-no 
        BY bf-oe-boll.ord-no
        BY bf-oe-boll.line
        BY bf-oe-boll.po-no
        BY bf-oe-boll.job-no
        BY bf-oe-boll.job-no2:   
        
        ASSIGN
            iTotPkgs = iTotPkgs + bf-oe-boll.cases +
                       IF bf-oe-boll.partial GT 0 THEN 1 ELSE 0 .
                
        IF FIRST-OF(bf-oe-boll.ord-no) AND integer(giBolFmtIntegerValue) EQ 1 THEN 
        DO:
            ASSIGN 
                iShipQty = 0
                dWeight   = 0
                iOrdQty  = 0.
        END.                                      
                  
        FIND FIRST oe-ordl WHERE oe-ordl.company EQ bf-oe-boll.company
            AND oe-ordl.ord-no  EQ int(bf-oe-boll.ord-no)
            AND (oe-ordl.line    EQ bf-oe-boll.line OR bf-oe-boll.line EQ 0)
            AND oe-ordl.i-no    EQ bf-oe-boll.i-no
            NO-LOCK NO-ERROR.
            
        ASSIGN 
            iShipQty = iShipQty + bf-oe-boll.qty
            dWeight  = dWeight + bf-oe-boll.weight
            iOrdQty  = iOrdQty + oe-ordl.qty.       
                    
        IF INTEGER(giBolFmtIntegerValue) EQ 1 THEN
        DO:                         
            IF bf-oe-boll.qty-case NE 0 AND bf-oe-boll.cases NE 0 THEN 
            DO:
                FIND FIRST w2 WHERE w2.cas-cnt EQ bf-oe-boll.qty-case NO-ERROR.
                IF NOT AVAILABLE w2 THEN CREATE w2.
                ASSIGN
                    w2.cas-cnt = bf-oe-boll.qty-case
                    w2.cases   = w2.cases + bf-oe-boll.cases.
            END.

            IF bf-oe-boll.partial NE 0 THEN 
            DO:
                FIND FIRST w2 WHERE w2.cas-cnt EQ bf-oe-boll.partial NO-ERROR.
                IF NOT AVAILABLE w2 THEN CREATE w2.
                ASSIGN
                    w2.cas-cnt = bf-oe-boll.partial
                    w2.cases   = w2.cases + 1.
            END.
                                                                                                         
                      
            IF LAST-OF(bf-oe-boll.ord-no) THEN 
            DO:
                iCount = 0.
                FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESCENDING:
                    iCount = iCount + 1.
                    IF iCount EQ 1 THEN ASSIGN cPartDscr = oe-ordl.part-no
                            cJobPo    = bf-oe-boll.po-no.
                    ELSE
                        IF iCount EQ 2 THEN ASSIGN cPartDscr = oe-ordl.part-dscr1 /*i-name*/
                                cJobPo    = IF oe-ordl.job-no EQ "" THEN "" ELSE 
                                                        TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', oe-ordl.job-no, oe-ordl.job-no2))).                                                
                        ELSE
                            IF iCount EQ 3 THEN cPartDscr = oe-ordl.part-dscr1.
                            ELSE
                                IF iCount EQ 4 THEN cPartDscr = oe-ordl.part-dscr2.                       

                    IF FIRST(w2.cases * w2.cas-cnt) THEN 
                    DO: 
                        CREATE ttBolItem.
                        ASSIGN
                            ttBolItem.riBolItm      = ROWID(bf-oe-boll)
                            ttBolItem.company       = bf-oe-boll.company
                            ttBolItem.locationID    = bf-oe-boll.loc
                            ttBolItem.bolID         = bf-oe-boll.bol-no
                            ttBolItem.lineID        = bf-oe-boll.line
                            ttBolItem.partID        = oe-ordl.part-no
                            ttBolItem.custPo        = cJobPo
                            ttBolItem.itemID        = bf-oe-boll.i-no                              
                            ttBolItem.itemName      = oe-ordl.i-name                                  
                            ttBolItem.ItemUnit      = w2.cases
                            ttBolItem.ItemQtyUnit   = w2.cas-cnt
                            ttBolItem.ItemPC        = ""
                            ttBolItem.ItemWeight    = 0  
                            ttBolItem.firstLineItem = YES
                            ttBolItem.cRecKey       = IF AVAILABLE oe-ordl  THEN oe-ordl.rec_key ELSE ""
                            ttBolItem.bolSummaryQty = iShipQty
                            ttBolItem.OrdSummaryQty = iOrdQty
                            ttBolItem.bolSummPart   = cPartDscr
                            .                    
                    END.
                    ELSE 
                    DO:
                        CREATE ttBolItem.
                        ASSIGN
                            ttBolItem.riBolItm    = ROWID(bf-oe-boll)
                            ttBolItem.company     = bf-oe-boll.company
                            ttBolItem.locationID  = bf-oe-boll.loc
                            ttBolItem.bolID       = bf-oe-boll.bol-no
                            ttBolItem.lineID      = bf-oe-boll.line
                            ttBolItem.partID      = STRING(oe-ordl.ord-no)
                            ttBolItem.custPo      = ""
                            ttBolItem.itemID      = ""                               
                            ttBolItem.itemName    = oe-ordl.part-dscr1      
                            ttBolItem.ItemUnit    = w2.cases
                            ttBolItem.ItemQtyUnit = w2.cas-cnt
                            ttBolItem.ItemPC      = ""
                            ttBolItem.ItemWeight  = 0 
                            ttBolItem.cRecKey     = IF AVAILABLE oe-ordl  THEN oe-ordl.rec_key ELSE ""
                            ttBolItem.bolSummaryQty = iShipQty
                            ttBolItem.OrdSummaryQty = iOrdQty
                            ttBolItem.bolSummPart   = cPartDscr
                            .                       
                    END.  
                        
                    IF LAST(w2.cases * w2.cas-cnt) THEN 
                    DO:
                        IF FIRST(w2.cases * w2.cas-cnt) THEN 
                        DO:                          
                            CREATE ttBolItem.
                            ASSIGN
                                ttBolItem.riBolItm    = ROWID(bf-oe-boll)
                                ttBolItem.company     = bf-oe-boll.company
                                ttBolItem.locationID  = bf-oe-boll.loc
                                ttBolItem.bolID       = bf-oe-boll.bol-no
                                ttBolItem.lineID      = bf-oe-boll.line
                                ttBolItem.partID      = STRING(oe-ordl.ord-no)
                                ttBolItem.custPo      = ""
                                ttBolItem.itemID      = ""
                                ttBolItem.itemName    = oe-ordl.part-dscr1                                   
                                ttBolItem.ItemUnit    = 0
                                ttBolItem.ItemQtyUnit = 0
                                ttBolItem.ItemPC      = ""
                                ttBolItem.ItemWeight  = 0  
                                ttBolItem.cRecKey     = IF AVAILABLE oe-ordl  THEN oe-ordl.rec_key ELSE ""
                                ttBolItem.bolSummaryQty = iShipQty
                                ttBolItem.OrdSummaryQty = iOrdQty
                                ttBolItem.bolSummPart   = cPartDscr
                                .                       
                        END.
                      
                        ASSIGN                             
                            ttBolItem.ItemPC       = IF bf-oe-boll.p-c THEN "P" ELSE "C"
                            ttBolItem.ItemWeight   = dWeight
                            ttBolItem.bolQty       = bf-oe-boll.qty
                            ttBolItem.totalPkgs    = iTotPkgs
                            ttBolItem.lastLineItem = YES                            
                            .                          

                        ASSIGN                              
                            iTotPkgs = 0.                      
                    END.
                    iTotCases = iTotCases + w2.cases.
                    DELETE w2.
                END.
                  
            END.  /* Summary */
        END.            
        ELSE 
        DO:                   
            CREATE ttBolItem.
            ASSIGN
                ttBolItem.riBolItm      = ROWID(bf-oe-boll)
                ttBolItem.company       = bf-oe-boll.company
                ttBolItem.locationID    = bf-oe-boll.loc
                ttBolItem.bolID         = bf-oe-boll.bol-no
                ttBolItem.lineID        = bf-oe-boll.line
                ttBolItem.partID        = IF AVAILABLE oe-ordl  THEN oe-ordl.part-no  ELSE ""
                ttBolItem.custPo        = bf-oe-boll.po-no
                ttBolItem.itemID        = bf-oe-boll.i-no
                ttBolItem.itemName      = oe-ordl.i-name
                ttBolItem.itemPartDesc  = oe-ordl.part-dscr1  
                ttBolItem.bolSummPart   = oe-ordl.part-dscr2  
                ttBolItem.ItemUnit      = bf-oe-boll.cases
                ttBolItem.ItemQtyUnit   = bf-oe-boll.qty-case
                ttBolItem.ItemPC        = IF bf-oe-boll.p-c THEN "P" ELSE "C"
                ttBolItem.ItemWeight    = bf-oe-boll.weight
                ttBolItem.bolQty        = bf-oe-boll.qty
                ttBolItem.totalPkgs     = iTotPkgs
                ttBolItem.firstLineItem = YES
                ttBolItem.lastLineItem  = YES
                ttBolItem.cRecKey       = IF AVAILABLE oe-ordl  THEN oe-ordl.rec_key ELSE ""
                ttBolItem.OrdSummaryQty = oe-ordl.qty
                . 
                
            ASSIGN
                iTotCases = iTotCases + iTotPkgs
                iTotPkgs  = 0.
                                 
        END. /* detail*/
                
                
        dTotWt = dTotWt + bf-oe-boll.weight.

        IF bf-oe-boll.weight EQ 0 THEN
            dTotWt = dTotWt + (bf-oe-boll.qty / 100 * itemfg.weight-100).    
    END.
    opiTotCases = iTotCases.
    opdTotalWeight = dTotWt.
END PROCEDURE.


PROCEDURE Bol_BuildBol:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiBolID   AS INTEGER   NO-UNDO.     
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttBolHeader.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttBolItem.    
 
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    
    FIND FIRST bf-oe-bolh NO-LOCK
        WHERE bf-oe-bolh.company EQ ipcCompany
        AND bf-oe-bolh.b-no    EQ ipiBolID
        NO-ERROR.
    IF NOT AVAILABLE bf-oe-bolh THEN
        RETURN.
        
    RUN pSetGlobalSettings(bf-oe-bolh.company,bf-oe-bolh.cust-no ).     
     
    RUN pBol_BuildBol (
        BUFFER bf-oe-bolh,           
        INPUT-OUTPUT TABLE ttBolHeader,
        INPUT-OUTPUT TABLE ttBolItem        
        ).
END PROCEDURE.

PROCEDURE pBol_BuildBol PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-bolh FOR oe-bolh.     
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttBolHeader.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttBolItem.
    
    DEFINE VARIABLE dBolWeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iTotCases  AS INTEGER NO-UNDO.
        
    EMPTY TEMP-TABLE ttBolItem.
    EMPTY TEMP-TABLE ttBolHeader.           
        
    RUN pAddBolHeader (BUFFER ipbf-oe-bolh).
    
    RUN pAddBolItem (BUFFER ipbf-oe-bolh, OUTPUT dBolWeight, OUTPUT iTotCases).
    
    RUN pUpdateBolHeader (BUFFER ipbf-oe-bolh, INPUT dBolWeight, INPUT iTotCases).
    
END PROCEDURE.


PROCEDURE pUpdateBolHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-bolh FOR oe-bolh.     
    DEFINE INPUT PARAMETER ipdBolWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiTotCases  AS INTEGER NO-UNDO.
    
    FIND FIRST ttBolHeader NO-LOCK
        WHERE ttBolHeader.riBol EQ ROWID(ipbf-oe-bolh) NO-ERROR.
    IF AVAILABLE ttBolHeader THEN
    DO:
        ASSIGN
            ttBolHeader.totalWeight = ipdBolWeight.
            ttBolHeader.totalCases = ipiTotCases.
    END.
       
END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets the NK1 setting global variables that are pertinent to th
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomer AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
               
    RUN sys/ref/nk1look.p (ipcCompany, "BOLFMT", "I" , YES, YES, "" , "", OUTPUT cReturn, OUTPUT lFound).          
    IF lFound THEN giBolFmtIntegerValue = INTEGER(cReturn). 
    
    RUN sys/ref/nk1look.p (ipcCompany, "BOLFMT", "L" , YES, YES, "" , "", OUTPUT cReturn, OUTPUT lFound).          
    IF lFound THEN glBolFmtLogicalValue = LOGICAL(cReturn) NO-ERROR.
    
            
END PROCEDURE.  


/* ************************  Function Implementations ***************** */


