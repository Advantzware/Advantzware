
/*------------------------------------------------------------------------
    File        : activeLoc.p
    Purpose     : 

    Syntax      :

    Description : Returns loc active status

    Author(s)   : WFK
    Created     : Tue Mar 05 17:42:27 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE SHARED VARIABLE cocode AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE getActiveBin:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcLoc AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBin AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidBin AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lActiveLoc AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lActiveBin AS LOGICAL NO-UNDO.

    RUN getActiveLoc IN THIS-PROCEDURE (INPUT ipcLoc, OUTPUT lActiveLoc).
    
    lActiveBin = CAN-FIND(FIRST fg-bin NO-LOCK 
        WHERE fg-bin.company EQ cocode  
          AND fg-bin.loc     EQ ipcLoc 
          AND fg-bin.loc-bin EQ ipcBin
          AND fg-bin.i-no    EQ ""
          AND fg-bin.active  EQ TRUE).
    
    oplValidBin = lActiveLoc AND lActiveBin.
    
END PROCEDURE.

PROCEDURE getActiveLoc:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcLoc AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidLoc AS LOGICAL NO-UNDO.
    
    oplValidLoc = CAN-FIND(FIRST loc NO-LOCK 
        WHERE loc.company EQ cocode  
        AND loc.loc     EQ ipcLoc 
        AND loc.active  EQ TRUE).
        
END PROCEDURE.

