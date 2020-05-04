/*------------------------------------------------------------------------
    File        : CreditProcs.p
    Purpose     : 

    Syntax      :

    Description : This will hold procedures and functions like Geterms and some other common
                  functions

    Author(s)   : Sewa Singh
    Created     : Thur sept 19 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Property Variables*/
            
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

 
 FUNCTION GetInvDueDate RETURNS DATE 
    (ipdtInvDate AS DATE,
     ipcCompany AS CHARACTER ,
     ipcTerms AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
/*Initialize Constants and Property Defaults*/

/* **********************  Internal Procedures  *********************** */

PROCEDURE Credit_GetTerms:
    /*------------------------------------------------------------------------------
     Purpose: Calculates a given estReleaseID's Freight Cost
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTermsCode  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiDueOnMonth AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiDueOnDay   AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiNetDays    AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDiscRate   AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiDiscDays   AS DECIMAL NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplError      AS LOGICAL NO-UNDO.

   FIND FIRST terms NO-LOCK
       WHERE terms.company = ipcCompany
         AND terms.t-code  = ipcTermsCode 
       NO-ERROR. 
   IF AVAIL terms THEN DO:
       ASSIGN
           opiDueOnMonth = terms.dueOnMonth
           opiDueOnDay   = terms.dueOnDay
           opiNetDays    = terms.net-days
           opdDiscRate   = terms.disc-rate 
           opiDiscDays   = terms.disc-days
           .
   END.
   ELSE DO:
       oplError = YES .
   END.

END PROCEDURE.


/* ************************  Function Implementations ***************** */


FUNCTION GetInvDueDate RETURNS DATE 
    ( ipdtInvDate AS DATE,
     ipcCompany AS CHARACTER ,
     ipcTerms AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: return due date on invoice 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dtReturn AS DATE NO-UNDO .
    DEFINE VARIABLE iDueOnMonth AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDueOnDay   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNetDays    AS INTEGER NO-UNDO.
    DEFINE VARIABLE dDiscRate   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iDiscDays   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lGetError      AS LOGICAL NO-UNDO.

    RUN Credit_GetTerms( ipcCompany,ipcTerms, OUTPUT iDueOnMonth,OUTPUT iDueOnDay,OUTPUT iNetDays,OUTPUT dDiscRate, OUTPUT iDiscDays, OUTPUT lGetError) .

    IF NOT lGetError THEN do:
        IF iDueOnMonth GT 0 AND ipdtInvDate  GT  DATE(string(iDueOnMonth,"99") + "/" + STRING(iDueOnDay,"99") + "/" + STRING(YEAR(TODAY),"9999"))   THEN
            ASSIGN dtReturn = DATE(string(iDueOnMonth,"99") + "/" + STRING(iDueOnDay,"99") + "/" + STRING(YEAR(TODAY) + 1,"9999")).
        ELSE IF iDueOnMonth GT 0  THEN
            ASSIGN dtReturn = DATE(string(iDueOnMonth,"99") + "/" + STRING(iDueOnDay,"99") + "/" + STRING(YEAR(TODAY),"9999")).
        ELSE
            ASSIGN dtReturn = DATE(date(ipdtInvDate) + iNetDays).
            
    END.
    ELSE DO:
         ASSIGN dtReturn = DATE(ipdtInvDate).
    END.

    RETURN dtReturn .   
		
END FUNCTION.


