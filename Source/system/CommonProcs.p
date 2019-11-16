/*------------------------------------------------------------------------
    File        : CommonProcs.p
    Purpose     : 

    Syntax      :

    Description : This will hold procedures and functions like “RoundUp” and some other common,
                  non-DB related functions

    Author(s)   : Sewa Singh
    Created     : Thur sept 12 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Property Variables*/
            
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

 FUNCTION Common_GetNumberOfDaysInMonth RETURNS INTEGER 
    (ipcMonth AS INTEGER) FORWARD.

/* ***************************  Main Block  *************************** */
/*Initialize Constants and Property Defaults*/

/* **********************  Internal Procedures  *********************** */

PROCEDURE Common_ValidateValueByDataType:
    /*------------------------------------------------------------------------------
     Purpose: Validate if a given value can be converted to given data type.
              Returns error in case of failure
     Notes:
    ------------------------------------------------------------------------------*/	
                
    DEFINE INPUT  PARAMETER ipcValue    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDataType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid    AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE dDecimalValidator AS DECIMAL NO-UNDO.
    DEFINE VARIABLE daDateValidator   AS DATE    NO-UNDO.
    
    CASE ipcDataType:
        WHEN "DECIMAL" OR
        WHEN "INTEGER" THEN DO:
            dDecimalValidator = DECIMAL(ipcValue) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                oplValid   = FALSE.
                RETURN.
            END.
        END.
        WHEN "DATE" THEN DO:
            daDateValidator = DATE(ipcValue) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                oplValid = FALSE.
                RETURN.
            END.
        END.
    END CASE.
    
    oplValid = TRUE.
END PROCEDURE.
    
/* ************************  Function Implementations ***************** */

FUNCTION Common_GetNumberOfDaysInMonth RETURNS INTEGER 
    ( ipcMonth AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose: return day in a month
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cDaya AS INTEGER EXTENT 12 INIT [31,28,31,30,31,30,31,31,30,31,30,31] NO-UNDO .
    DEFINE VARIABLE iReturn AS INTEGER NO-UNDO .

    if (ipcMonth NE 2) THEN  iReturn =  cDaya[ipcMonth ].
    ELSE if (year(TODAY) / 4 NE 0) THEN  iReturn =  cDaya[2].
    ELSE if (year(TODAY) / 100 EQ 0 AND year(TODAY) / 400 NE 0) THEN  iReturn = cDaya[2].
    ELSE iReturn = cDaya[2] + 1.
 
    RETURN iReturn .   
		
END FUNCTION.


