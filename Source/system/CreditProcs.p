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
 
FUNCTION Credit_fAmountPaid RETURNS DECIMAL (
    ipcCompany AS CHARACTER,
    ipcCustNo  AS CHARACTER 
    ) FORWARD.

FUNCTION Credit_fBalanceDue RETURNS DECIMAL (
    ipcCompany AS CHARACTER,
    ipcCustNo  AS CHARACTER 
    ) FORWARD.

FUNCTION GetInvDueDate RETURNS DATE (
    ipdtInvDate AS DATE,
    ipcCompany AS CHARACTER ,
    ipcTerms AS CHARACTER
    ) FORWARD.

/* ***************************  Main Block  *************************** */
/*Initialize Constants and Property Defaults*/

/* **********************  Internal Procedures  *********************** */

PROCEDURE Credit_CustBalances:
    DEFINE PARAMETER BUFFER cust FOR cust.

    DEFINE OUTPUT PARAMETER opdDue                AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBalanceCurrent     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdBalanceWithinGrace AS DECIMAL NO-UNDO. 
    DEFINE OUTPUT PARAMETER opdBalancePastDue     AS DECIMAL NO-UNDO.

    DEFINE VARIABLE iGraceDays AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNetDays   AS INTEGER NO-UNDO.

    FIND FIRST terms NO-LOCK
         WHERE terms.company EQ cust.company
           AND terms.t-code  EQ cust.terms
         NO-ERROR.
    IF AVAILABLE terms THEN
    iNetDays = terms.net-days.
    iGraceDays = cust.cr-hold-invdays.
    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company EQ cust.company
          AND ar-inv.posted  EQ YES
          AND ar-inv.cust-no EQ cust.cust-no
        USE-INDEX posted-due
        :    
        opdDue = opdDue + ar-inv.due.
        IF ar-inv.due-date + iNetDays GE TODAY THEN
        opdBalanceCurrent = opdBalanceCurrent + ar-inv.due.
        ELSE
        IF ar-inv.due-date + iNetDays + iGraceDays GE TODAY THEN
        opdBalanceWithinGrace = opdBalanceWithinGrace + ar-inv.due.
        ELSE
        opdBalancePastDue = opdBalancePastDue + ar-inv.due.
    END. /* each ar-inv */

END PROCEDURE.

PROCEDURE Credit_GetTerms:
/*------------------------------------------------------------------------------
 Purpose: Calculates a given estReleaseID's Freight Cost
 Notes:
 Syntax:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTermsCode  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiDueOnMonth AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiDueOnDay   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiNetDays    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDiscRate   AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiDiscDays   AS DECIMAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplError      AS LOGICAL   NO-UNDO.

   FIND FIRST terms NO-LOCK
        WHERE terms.company EQ ipcCompany
          AND terms.t-code  EQ ipcTermsCode 
        NO-ERROR. 
   IF AVAIL terms THEN
   ASSIGN
       opiDueOnMonth = terms.dueOnMonth
       opiDueOnDay   = terms.dueOnDay
       opiNetDays    = terms.net-days
       opdDiscRate   = terms.disc-rate 
       opiDiscDays   = terms.disc-days
       .
   ELSE oplError = YES.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION Credit_fAmountPaid RETURNS DECIMAL (
    ipcCompany AS CHARACTER,
    ipcCustNo  AS CHARACTER 
    ):
    DEFINE VARIABLE dAmountPaid AS DECIMAL NO-UNDO.
    
    FOR EACH ar-cashl NO-LOCK
        WHERE ar-cashl.company    EQ ipcCompany
          AND ar-cashl.posted     EQ YES
          AND ar-cashl.cust-no    EQ ipcCustNo
          AND ar-cashl.on-account EQ YES
        USE-INDEX inv-no:
        dAmountPaid = dAmountPaid + ar-cashl.amt-paid.
    END. /* each ar-cashl */
    RETURN dAmountPaid.
END FUNCTION.

FUNCTION Credit_fBalanceDue RETURNS DECIMAL (
    ipcCompany AS CHARACTER,
    ipcCustNo  AS CHARACTER 
    ):
    DEFINE VARIABLE dDue AS DECIMAL NO-UNDO.

    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company EQ ipcCompany
          AND ar-inv.posted  EQ YES
          AND ar-inv.cust-no EQ ipcCustNo
        USE-INDEX posted-due:
        dDue = dDue + ar-inv.due.
    END. /* each ar-inv */
    RETURN dDue.
END FUNCTION.

FUNCTION GetInvDueDate RETURNS DATE (
    ipdtInvDate AS DATE,
    ipcCompany  AS CHARACTER,
    ipcTerms    AS CHARACTER
    ):
/*------------------------------------------------------------------------------
 Purpose: return due date on invoice 
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dtReturn    AS DATE    NO-UNDO.
    DEFINE VARIABLE iDueOnMonth AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDueOnDay   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNetDays    AS INTEGER NO-UNDO.
    DEFINE VARIABLE dDiscRate   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iDiscDays   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lGetError   AS LOGICAL NO-UNDO.

    RUN Credit_GetTerms (
        ipcCompany,
        ipcTerms,
        OUTPUT iDueOnMonth,
        OUTPUT iDueOnDay,
        OUTPUT iNetDays,
        OUTPUT dDiscRate,
        OUTPUT iDiscDays,
        OUTPUT lGetError
        ).
    IF NOT lGetError THEN DO:
        IF iDueOnMonth GT 0 AND ipdtInvDate GT DATE(STRING(iDueOnMonth,"99") + "/" + STRING(iDueOnDay,"99") + "/" + STRING(YEAR(TODAY),"9999")) THEN
        dtReturn = DATE(STRING(iDueOnMonth,"99") + "/" + STRING(iDueOnDay,"99") + "/" + STRING(YEAR(TODAY) + 1,"9999")).
        ELSE IF iDueOnMonth GT 0 THEN
        dtReturn = DATE(STRING(iDueOnMonth,"99") + "/" + STRING(iDueOnDay,"99") + "/" + STRING(YEAR(TODAY),"9999")).
        ELSE
        dtReturn = DATE(ipdtInvDate) + iNetDays.            
    END.
    ELSE dtReturn = ipdtInvDate.

    RETURN dtReturn.   
		
END FUNCTION.
