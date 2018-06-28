
/*------------------------------------------------------------------------
    File        : FixCxmlDuplicates.p
    Purpose     : remove duplicate cXML Orders with no Order Lines

    Syntax      : RUN util/fixcXMLDuplicates.p

    Description : Removes the partial imports for cXML/Ariba order importing.


    Author(s)   : Brad Vigrass
    Created     : Mon Nov 14 21:10:17 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttOrdersToDelete
    FIELD riOrder AS ROWID 
    FIELD cPONo   AS CHARACTER 
    FIELD iOrdNo  AS INTEGER 
    .
DEFINE VARIABLE lDelete AS LOGICAL NO-UNDO.
DEFINE BUFFER bf-oe-ord FOR oe-ord.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN pBuildList.
RUN pAskToDelete (OUTPUT lDelete).
IF lDelete THEN RUN pDeleteList.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pAskToDelete:
    /*------------------------------------------------------------------------------
     Purpose: Builds a summary of 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER iplDelete AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO. 

    IF CAN-FIND(FIRST ttOrdersToDelete) THEN 
    DO:
        cMessage = "The following duplicate and invalid orders have been found:" + CHR(10) + CHR(10).
        FOR EACH ttOrdersToDelete:
            cMessage = cMessage + " PO#: " + ttOrdersToDelete.cPONo 
                + " - Order #: " + STRING(ttOrdersToDelete.iOrdNo) + CHR(10).  
        END.
        cMessage = cMessage + CHR(10) + "Delete These Orders?".
        MESSAGE cMessage
        VIEW-AS ALERT-BOX BUTTONS YES-NO TITLE "cXML Duplicate Invalid Orders w/o Order Lines"
        UPDATE iplDelete.
    END.
/*    ELSE                                                                     */
/*    DO:                                                                      */
/*        MESSAGE "No duplicate records found for deletion." VIEW-AS ALERT-BOX.*/
/*        iplDelete = NO.                                                      */
/*    END.                                                                     */

END PROCEDURE.

PROCEDURE pBuildList:
    /*------------------------------------------------------------------------------
     Purpose:  Builds the list of orders to delete in temp-table
     Notes: Must have a payload id and have status of W to qualify
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrdersToDelete.

    FOR EACH company NO-LOCK,
        EACH oe-ord NO-LOCK 
        WHERE oe-ord.company      EQ company.company
          AND oe-ord.stat         EQ 'W'
          AND oe-ord.spare-char-3 NE ''
          AND NOT CAN-FIND(FIRST oe-ordl OF oe-ord)
        :        
        CREATE ttOrdersToDelete.
        ASSIGN 
            ttOrdersToDelete.riOrder = ROWID(oe-ord)
            ttOrdersToDelete.cPONo   = oe-ord.po-no
            ttOrdersToDelete.iOrdNo  = oe-ord.ord-no
            .
    END.
    
    /* Found a case where an order with stat 'A' and same Po was found */
    /* with no line items                                              */
    FOR EACH company NO-LOCK,
        EACH oe-ord NO-LOCK 
        WHERE oe-ord.company      EQ company.company
        AND oe-ord.stat         EQ 'W'
        AND oe-ord.spare-char-3 NE '',
        
        FIRST bf-oe-ord NO-LOCK 
           WHERE bf-oe-ord.company EQ company.company             
             AND bf-oe-ord.po-no EQ oe-ord.po-no    
              AND ROWID(bf-oe-ord) NE ROWID(oe-ord)
              AND NOT CAN-FIND(FIRST oe-ordl OF bf-oe-ord)        
             USE-INDEX pono  
    :          
        CREATE ttOrdersToDelete.
        ASSIGN 
            ttOrdersToDelete.riOrder = ROWID(bf-oe-ord)
            ttOrdersToDelete.cPONo   = bf-oe-ord.po-no
            ttOrdersToDelete.iOrdNo  = bf-oe-ord.ord-no
            .
    END.

END PROCEDURE.

PROCEDURE pDeleteList:
    /*------------------------------------------------------------------------------
     Purpose: Processes the temp table and deletes the linked orders
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    FOR EACH ttOrdersToDelete:
        FIND FIRST oe-ord EXCLUSIVE-LOCK 
             WHERE ROWID(oe-ord) EQ ttOrdersToDelete.riOrder
             NO-ERROR.
        IF AVAILABLE oe-ord THEN DO:
            iCount = iCount + 1.
            DELETE oe-ord.
        END.
        RELEASE oe-ord.
    END.
    MESSAGE iCount " hidden duplicate orders have been deleted." VIEW-AS ALERT-BOX.

END PROCEDURE.
