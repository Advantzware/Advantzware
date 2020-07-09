
/*------------------------------------------------------------------------
    File        : cXMLTester.p
    Purpose     : 

    Syntax      :

    Description : Simple Import Tester for cXML Files		

    Author(s)   : BV
    Created     : Fri May 22 11:36:14 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN pExecuteImport("C:\tmp\test.xml").


/* **********************  Internal Procedures  *********************** */

PROCEDURE pExecuteImport PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO INITIAL "001".
    DEFINE VARIABLE cWarehouseID AS CHARACTER NO-UNDO INITIAL "MAIN".
    DEFINE VARIABLE cPayloadID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cInternalException AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lcRequestData AS LONGCHAR NO-UNDO.
    RUN pGetFileContents(ipcFile, YES, OUTPUT lcRequestData).
    
    RUN cXML/gencXMLOrder.p (
        INPUT  lcRequestData,
        INPUT  NO, /* Add records to temp-table only */
        INPUT  cCompany, 
        INPUT  cWarehouseID,
        OUTPUT cPayLoadID,
        OUTPUT cOrderID,
        OUTPUT lSuccess,
        OUTPUT cMessage,
        OUTPUT cInternalException
        ).
        MESSAGE "Order " cOrderID SKIP
        lSuccess SKIP 
        cMessage SKIP
        cInternalException
        VIEW-AS ALERT-BOX.

END PROCEDURE.

PROCEDURE pGetFileContents PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplShow AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcContents AS LONGCHAR NO-UNDO.
    
    COPY-LOB FROM FILE ipcFile TO oplcContents.
    IF iplShow THEN 
        MESSAGE STRING(oplcContents) VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

END PROCEDURE.

