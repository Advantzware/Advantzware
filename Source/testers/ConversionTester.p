
/*------------------------------------------------------------------------
    File        : ConversionTester.p
    Purpose     : 

    Syntax      :

    Description : Tester for ConversionProcs.p

    Author(s)   : BV
    Created     : Fri Mar 13 00:39:19 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE ghSession           AS HANDLE.
DEFINE VARIABLE ghConversionProcs   AS HANDLE.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
RUN system\ConversionProcs.p PERSISTENT SET ghConversionProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (ghConversionProcs).

RUN pTestConversionForItem.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pTestConversionForItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dOldValue AS DECIMAL NO-UNDO INITIAL 6.
    DEFINE VARIABLE dNewValue AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.    
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ '001'
        AND itemfg.i-no EQ 'ZOV10X10X10'
        NO-ERROR.
    RUN Conversion_ValueFromUOMToEAForItem(ROWID(itemfg), dOldValue, "ZV", OUTPUT dNewValue, OUTPUT lError, OUTPUT cMessage).   
    MESSAGE dNewValue SKIP(2)
        cMessage
    VIEW-AS ALERT-BOX.
END PROCEDURE.
