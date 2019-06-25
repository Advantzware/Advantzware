/*------------------------------------------------------------------------
    File        : CheckUserCust.p
    Purpose     : Customer
    mainfile    : viewers/usercust.w
    Syntax      :

    Description : Return a Output value for User Cust

    Author(s)   : 
    Created     : april 15 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER prmCompany   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER DefaultCust   AS CHARACTER    NO-UNDO.

IF prmCompany = ?  THEN ASSIGN prmCompany = "".
IF prmUser    = ?  THEN ASSIGN prmUser    = "".

IF prmCompany = "" THEN DO:

    /*    Company logic***************************/
    FIND FIRST usercomp WHERE
         usercomp.user_id = prmUser AND
         usercomp.loc = '' AND
         usercomp.company_default = YES
         NO-LOCK NO-ERROR.

    prmCompany = IF AVAIL usercomp THEN usercomp.company ELSE "001".

END.

 FIND FIRST usercust WHERE  usercust.company = prmCompany AND usercust.USER_ID = prmUser
       AND usercust.cust_default = YES NO-LOCK NO-ERROR.
 IF AVAIL usercust THEN DO:
     ASSIGN
        DefaultCust = usercust.cust-no .
 END.

