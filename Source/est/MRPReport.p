
/*------------------------------------------------------------------------
    File        : MRPReport.p
    Purpose     : Ticket 101951 - Replace ProductListXLS.p

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Mon Oct 04 10:18:00 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEb AS ROWID  NO-UNDO.

DEFINE TEMP-TABLE ttHeader
    FIELD company        AS CHARACTER 
    FIELD estimateNo     AS CHARACTER
    FIELD customerID     AS CHARACTER
    FIELD customerName   AS CHARACTER 
    FIELD customerPart   AS CHARACTER
    FIELD datePrinted    AS DATE
    FIELD dateCalculated AS DATE 
    FIELD plateID        AS CHARACTER 
    . 
    
DEFINE TEMP-TABLE ttQuantity
    FIELD quantity        AS INTEGER 
    FIELD estCostHeaderID AS INT64 
    FIELD indexID         AS INTEGER
    . 
    
DEFINE TEMP-TABLE ttMaterial
    FIELD itemID AS CHARACTER 
    FIELD itemName AS CHARACTER 
    FIELD sizeDesc AS CHARACTER 
    FIELD quantityOnHand AS DECIMAL 
    FIELD quantityRequired AS DECIMAL EXTENT 5
    FIELD quantityUOM AS CHARACTER
    FIELD quantityWasted AS DECIMAL EXTENT 5
    .

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fIsSet RETURNS LOGICAL PRIVATE
    (ipiEstType AS INTEGER) FORWARD.


/* ***************************  Main Block  *************************** */

RUN pBuildHeader(ipriEb).
RUN pBuildData.
RUN pWriteData.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the Quantity and Materials tables
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE dtLatestProbe AS DATE NO-UNDO. 
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    DEFINE VARIABLE cSizeDesc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQtyRequired AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyWasted AS DECIMAL NO-UNDO.
    
    FOR FIRST ttHeader,
        EACH estCostHeader NO-LOCK 
        WHERE estCostHeader.company EQ ttHeader.company
        AND estCostHeader.estimateNo EQ ttHeader.estimateNo
        BREAK BY estCostHeader.calcDateTime DESCENDING:
        
        IF FIRST-OF(estCostHeader.calcDateTime) THEN 
        DO: 
            ASSIGN 
                dtLatestProbe           = DATE(estCostHeader.calcDateTime)
                ttHeader.dateCalculated = dtLatestProbe
                ttHeader.datePrinted    = TODAY
                .                
        END.
        FIND FIRST ttQuantity NO-LOCK
            WHERE ttQuantity.quantity EQ estCostHeader.quantityMaster
            NO-ERROR.
        IF NOT AVAILABLE ttQuantity THEN 
        DO:
            CREATE ttQuantity.
            ASSIGN 
                ttQuantity.quantity = estCostHeader.quantityMaster
                ttQuantity.estCostHeaderID = estCostHeader.estCostHeaderID
                .
        END.  
    END.  //Each estCostHeader
    iIndex = 0.
    FOR EACH ttQuantity        
        BY ttQuantity.quantity:
        ASSIGN 
            iIndex = iIndex + 1
            ttQuantity.indexID = iIndex.
        FOR EACH estCostMaterial NO-LOCK 
            WHERE estCostMaterial.estCostHeaderID EQ ttQuantity.estCostHeaderID,
                FIRST item NO-LOCK 
                    WHERE item.company EQ estCostMaterial.company
                        AND item.i-no EQ estCostMaterial.itemID:
            
            IF estCostMaterial.isPrimarySubstrate THEN 
                cSizeDesc = TRIM(STRING(estCostMaterial.dimLength,">>>9.99")) + " x " + TRIM(STRING(estCostMaterial.dimWidth,">>>9.99")).
            ELSE 
                cSizeDesc = "".
                
            FIND FIRST ttMaterial
                WHERE ttMaterial.itemID EQ estCostMaterial.itemID
                AND ttMaterial.sizeDesc EQ cSizeDesc
                NO-ERROR.
            IF NOT AVAILABLE ttMaterial THEN DO: 
                CREATE ttMaterial.
                ASSIGN 
                    ttMaterial.itemID = estCostMaterial.itemID
                    ttMaterial.itemName = estCostMaterial.itemName
                    ttMaterial.quantityOnHand = item.q-onh
                    ttMaterial.sizeDesc = cSizeDesc
                    ttMaterial.quantityUOM = estCostMaterial.quantityUOM
                    .
            END.
            ASSIGN 
                dQtyRequired = estCostMaterial.quantityRequiredTotal
                dQtyWasted = estCostMaterial.quantityRequiredRunWaste + estCostMaterial.quantityRequiredSetupWaste
                .
            
            IF ttMaterial.quantityUOM NE estCostMaterial.quantityUOM THEN DO:
                //Convert 
            END.
                             
            ASSIGN 
                ttMaterial.quantityRequired[iIndex] = ttMaterial.quantityRequired[iIndex] + dQtyRequired
                ttMaterial.quantityWasted[iIndex] = ttMaterial.quantityWasted[iIndex] + dQtyWasted
                .
                
        END.
    END.
    
END PROCEDURE.

PROCEDURE pBuildHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given ROWID for eb, builds all related header information
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEB AS ROWID NO-UNDO.

    DEFINE BUFFER bf-eb FOR eb.
    
    FIND FIRST eb NO-LOCK
        WHERE ROWID(eb) EQ ipriEB NO-ERROR.

    IF AVAILABLE eb THEN 
    DO: 
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ eb.company
            AND cust.cust-no EQ eb.cust-no
            NO-ERROR.
        
        IF fIsSet(eb.est-type) THEN 
        DO:
            FIND FIRST bf-eb NO-LOCK 
                WHERE bf-eb.company EQ eb.company
                AND bf-eb.est-no EQ eb.est-no
                AND bf-eb.form-no EQ 0
                NO-ERROR .
        END.
        IF NOT AVAILABLE bf-eb THEN  
            FIND bf-eb NO-LOCK 
                WHERE ROWID(bf-eb) EQ ROWID(eb).
        
        CREATE ttHeader.
        ASSIGN 
            ttHeader.company      = bf-eb.company
            ttHeader.estimateNo   = bf-eb.est-no
            ttHeader.customerID   = bf-eb.cust-no
            ttHeader.customerName = IF AVAILABLE cust THEN cust.name ELSE bf-eb.cust-no
            ttHeader.customerPart = bf-eb.part-no
            ttHeader.plateID      = eb.plate-no
            .
            
    END.
END PROCEDURE.

PROCEDURE pWriteData PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Outputs the data from the temptables to the report
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hdOutput AS HANDLE NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
    
    RUN Output_TempTableToCSV IN hdOutput (TEMP-TABLE ttMaterial:HANDLE, "c:\tmp\MRP.csv", YES, YES, OUTPUT lError, OUTPUT cMessage).
    DELETE OBJECT hdOutput. 

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fIsSet RETURNS LOGICAL PRIVATE
    (ipiEstType AS INTEGER):
    /*------------------------------------------------------------------------------
    Purpose:  Given estimate type number, determine if set
    Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE cEstType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIsSet   AS LOGICAL   NO-UNDO.
    
    cEstType = DYNAMIC-FUNCTION("fEstimate_GetEstimateType", ipiEstType,"" ).

    IF cEstType NE "" THEN 
        lIsSet = DYNAMIC-FUNCTION("fEstimate_IsSetType", cEstType).
        
    RETURN lIsSet. 
		
END FUNCTION.

