
/*------------------------------------------------------------------------
    File        : FreightProcsTester.p
    Purpose     : 

    Syntax      :

    Description : Tester procedures for testing FreightProcs.p

    Author(s)   : BV
    Created     : Fri May 10 11:44:40 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hFreightProcs AS HANDLE.
DEFINE VARIABLE gcCompany     AS CHARACTER INIT '001'.
DEFINE VARIABLE gcEstimate1   AS CHARACTER INIT '10001'.
DEFINE VARIABLE gcEstimate2   AS CHARACTER INIT '10002'.
DEFINE VARIABLE gdQty1        AS DECIMAL   INIT 1000.
DEFINE VARIABLE gdQty2        AS DECIMAL   INIT 100.
DEFINE VARIABLE giDeleteForm  AS INTEGER   INIT 3.
DEFINE VARIABLE giDeleteBlankForm AS INTEGER   INIT 2.
DEFINE VARIABLE giDeleteBlank AS INTEGER   INIT 2.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */



/* ***************************  Main Block  *************************** */
RUN system\FreightProcs.p PERSISTENT SET hFreightProcs.

/*RUN DeleteAllEstReleasesForEstimate IN hFreightProcs (gcCompany, gcEstimate1).                                       */
/*RUN DeleteAllEstReleasesForEstimate IN hFreightProcs (gcCompany, gcEstimate2).                                       */
/*                                                                                                                     */
/*RUN pCreateSampleEstReleases.                                                                                        */
/*RUN DeleteAllEstReleasesForEstimate IN hFreightProcs (gcCompany, gcEstimate2).                                       */
/*IF CAN-FIND(FIRST estRelease WHERE estRelease.company EQ gcCompany                                                   */
/*    AND estRelease.estimateNo EQ gcEstimate2) THEN                                                                   */
/*    MESSAGE "Deletion Error - Estimate"                                                                              */
/*        VIEW-AS ALERT-BOX.                                                                                           */
/*RUN DeleteAllEstReleasesForEstimateQuantity IN hFreightProcs (gcCompany, gcEstimate1, gdQty1).                       */
/*IF CAN-FIND(FIRST estRelease WHERE estRelease.company EQ gcCompany                                                   */
/*    AND estRelease.estimateNo EQ gcEstimate1                                                                         */
/*    AND estRelease.quantity EQ gdQty1) THEN                                                                          */
/*    MESSAGE "Deletion Error - Estimate Quantity"                                                                     */
/*        VIEW-AS ALERT-BOX.                                                                                           */
/*RUN DeleteAllEstReleasesForEstimateForm IN hFreightProcs (gcCompany, gcEstimate1, giDeleteForm).                     */
/*IF CAN-FIND(FIRST estRelease WHERE estRelease.company EQ gcCompany                                                   */
/*    AND estRelease.estimateNo EQ gcEstimate1                                                                         */
/*    AND estRelease.formNo EQ giDeleteForm) THEN                                                                      */
/*    MESSAGE "Deletion Error - Estimate Form"                                                                         */
/*        VIEW-AS ALERT-BOX.                                                                                           */
/*                                                                                                                     */
/*RUN DeleteAllEstReleasesForEstimateBlank IN hFreightProcs (gcCompany, gcEstimate1, giDeleteBlankForm, giDeleteBlank).*/
/*IF CAN-FIND(FIRST estRelease WHERE estRelease.company EQ gcCompany                                                   */
/*    AND estRelease.estimateNo EQ gcEstimate1                                                                         */
/*    AND estRelease.formNo EQ giDeleteBlankForm                                                                       */
/*    AND estRelease.blankNo EQ giDeleteBlank) THEN                                                                    */
/*    MESSAGE "Deletion Error - Estimate Blank"                                                                        */
/*        VIEW-AS ALERT-BOX.                                                                                           */
/*RUN DeleteAllEstReleasesForEstimate IN hFreightProcs (gcCompany, gcEstimate1).                                       */
/*RUN DeleteAllEstReleasesForEstimate IN hFreightProcs (gcCompany, gcEstimate2).                                       */
/*                                                                                                                     */
/*FOR EACH estRelease EXCLUSIVE-LOCK:                                                                                  */
/*    DELETE estRelease.                                                                                               */
/*END.                                                                                                                 */
/*RUN pCreateTestEstReleases.                                                                                          */

RUN pTestFreightForBOL.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pCreateSampleEstReleases PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iEstReleaseID   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.


    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 1, 1, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 1, 2, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 2, 1, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 2, 2, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).
    
    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 3, 1, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 3, 2, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 1, 1, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 1, 2, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 2, 1, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 2, 2, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).
    
    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 3, 1, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate1, 3, 2, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 1, 1, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 1, 2, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 2, 1, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 2, 2, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).
    
    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 3, 1, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 3, 2, gdQty1,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 1, 1, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 1, 2, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 2, 1, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 2, 2, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).
    
    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 3, 1, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).

    RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 3, 2, gdQty2,
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage ).


END PROCEDURE.

PROCEDURE pCreateTestEstReleases PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iEstReleaseID   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    
    FIND FIRST eb NO-LOCK 
    WHERE eb.company EQ '001'
    AND eb.cust-no EQ 'ZOV100'
    AND eb.est-no EQ '   13798'
    AND eb.est-type EQ 5
    .
    RUN CreateEstReleaseForEstBlank IN hFreightProcs (ROWID(eb), 
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage).
    
    IF lError THEN
        MESSAGE cMessage VIEW-AS ALERT-BOX.       
    ELSE DO:
        /*Simulate user edits*/
        FIND FIRST estRelease EXCLUSIVE-LOCK 
        WHERE estRelease.estReleaseID EQ iEstReleaseID
        NO-ERROR.
        IF AVAILABLE estRelease THEN
            ASSIGN 
                estRelease.monthsAtShipFrom = 3
                estRelease.stackHeight = 2
                .
            RUN GetStorageAndHandlingForLocation IN hFreightProcs (estRelease.company, estRelease.shipFromLocationID, estRelease.stackHeight,
                OUTPUT estRelease.storageCost, OUTPUT estRelease.handlingCost, OUTPUT lError, OUTPUT cMessage). 
        RUN CalcStorageAndHandlingForEstRelease IN hFreightProcs (iEstReleaseID, OUTPUT lError, OUTPUT cMessage).
        IF lError THEN 
            MESSAGE cMessage VIEW-AS ALERT-BOX.
        RUN CalcFreightForEstRelease IN hFreightProcs (iEstReleaseID, OUTPUT lError, OUTPUT cMessage).
        IF lError THEN 
            MESSAGE cMessage VIEW-AS ALERT-BOX.            
    END.
    RUN pDisplayEstReleases.
    RUN pTestFreightCalc(BUFFER eb).
    
END PROCEDURE.

PROCEDURE pDisplayEstReleases PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Displays all EstReleases 
     Notes:
    ------------------------------------------------------------------------------*/
    
    FOR EACH estRelease NO-LOCK :
       DISPLAY 
                estRelease.estReleaseID 
                estRelease.quantity
                estRelease.quantityRelease
                estRelease.quantityPerSubUnit
                estRelease.quantitySubUnitsPerUnit
                estRelease.quantityOfUnits
                estRelease.handlingCost
                estRelease.handlingCostTotal
                estRelease.storageCost
                estRelease.storageCostTotal
                .
    END.
END PROCEDURE.

PROCEDURE pTestFreightCalc PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE dPalletCount AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotalWeight AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotalMSF AS DECIMAL NO-UNDO.
DEFINE VARIABLE dFreightTotal AS DECIMAL NO-UNDO.
DEFINE VARIABLE dFreightMin AS DECIMAL NO-UNDO.

ASSIGN
    dPalletCount = 6
    dTotalWeight = 500
    dTotalMSF = 300
    . 
RUN GetFreightForCarrierZone IN hFreightProcs (eb.company, eb.loc, eb.carrier, eb.dest-code, eb.ship-zip,
    dPalletCount, dTotalWeight, dTotalMSF, 
    OUTPUT dFreightTotal, OUTPUT dFreightMin,
    OUTPUT lError, OUTPUT cMessage).  

END PROCEDURE.

PROCEDURE pTestFreightForBOL PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dFreight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    FOR EACH oe-bolh NO-LOCK 
        WHERE oe-bolh.company EQ '001'
        //AND oe-bolh.bol-no EQ 9550
        AND NOT oe-bolh.posted
        AND CAN-FIND(FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no)       
        : 
        RUN GetFreightForBOL IN hFreightProcs (ROWID(oe-bolh), NO, OUTPUT dFreight, OUTPUT lError, OUTPUT cMessage).
        IF ROUND(dFreight,2) NE ROUND(oe-bolh.freight,2) THEN 
            MESSAGE "BOL" oe-bolh.bol-no SKIP 
            "New" dFreight SKIP 
            "Old" oe-bolh.freight
            VIEW-AS ALERT-BOX.    
    END.
    

END PROCEDURE.

/* ************************  Function Implementations ***************** */


