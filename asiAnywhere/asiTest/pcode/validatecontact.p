
                                 
/*------------------------------------------------------------------------
    File        : RfqShipping.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : Return a Dataset of all Orders

    Author(s)   : Sewa
    Created     : mon March 10 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmtitle         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcustcode      AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmcompany       AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmaddr          AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmcity          AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmstate         AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmterr          AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmsman          AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmcustype       AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER prmzip           AS CHARACTER NO-UNDO. 
                                    
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ?   THEN ASSIGN prmAction = "".


IF prmtitle      = ?  THEN ASSIGN prmtitle = "".
IF prmcustcode   = ? THEN ASSIGN   prmcustcode      = "".
IF prmcompany    = ? THEN ASSIGN   prmcompany       = "".
IF prmaddr       = ? THEN ASSIGN   prmaddr          = "".
IF prmcity       = ? THEN ASSIGN   prmcity          = "".
IF prmstate      = ? THEN ASSIGN   prmstate         = "".
IF prmterr       = ? THEN ASSIGN   prmterr          = "".
IF prmsman       = ? THEN ASSIGN   prmsman          = "".
IF prmcustype    = ? THEN ASSIGN   prmcustype       = "".
IF prmzip        = ? THEN ASSIGN   prmzip           = "".
                                                 
IF prmAction = ""  THEN ASSIGN prmAction = "Select".

DEF VAR prmLoc AS CHAR NO-UNDO.

IF prmComp EQ "" THEN
DO:
   FIND FIRST usercomp WHERE
        usercomp.user_id = prmUser AND
        usercomp.loc = '' AND
        usercomp.company_default = YES
        NO-LOCK NO-ERROR.

   prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

IF prmAction = "Validate" THEN DO:
    FIND FIRST cust WHERE cust.cust-no = prmcustcode AND cust.company = prmComp NO-LOCK NO-ERROR.
    IF AVAIL cust THEN DO:
        ASSIGN 
            cError = "Customer already exists, Try other number".
        RETURN.
    END.
    FIND FIRST titlcode WHERE titlcode.titlcode = prmtitle NO-LOCK NO-ERROR.
    IF NOT AVAILABLE titlcode THEN DO:
        ASSIGN cError  = "Invalid Title".
       RETURN. 
    END.
     
    FIND FIRST custype WHERE custype.custype = prmcustype NO-LOCK NO-ERROR.
    IF NOT AVAILABLE custype THEN DO:
        ASSIGN cError  = "Invalid Type".
        RETURN. 
    END.
    FIND FIRST sman WHERE sman.sman = prmsman NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
        ASSIGN cError  = "Invalid Salesman".
        RETURN. 
    END.
    FIND FIRST zipcode WHERE zipcode.city = prmcity NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid City".
        RETURN. 
    END.
    FIND FIRST statecod WHERE statecod.statecod = prmstate NO-LOCK NO-ERROR.
    IF NOT AVAILABLE statecod THEN DO:
        ASSIGN cError  = "Invalid State".
        RETURN. 
    END.
    FIND FIRST zipcode WHERE zipcode.zipcode = prmzip NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid zip".
        RETURN. 
    END.
     FIND FIRST terr WHERE terr.terr = prmterr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE terr THEN DO:
        ASSIGN cError  = "Invalid Territory".
        RETURN.
    END.
     
END.

IF prmAction = "UpdateValidate" THEN DO:
   /* FIND FIRST cust WHERE cust.cust-no = prmcustcode AND cust.company = prmComp NO-LOCK NO-ERROR.
    IF NOT AVAIL cust THEN DO:
        ASSIGN 
            cError = "Can not change the exists customer".
        RETURN.
    END.*/
    FIND FIRST titlcode WHERE titlcode.titlcode = prmtitle NO-LOCK NO-ERROR.
    IF NOT AVAILABLE titlcode THEN DO:
        ASSIGN cError  = "Invalid Title".
       RETURN. 
    END.
     
    FIND FIRST custype WHERE custype.custype = prmcustype NO-LOCK NO-ERROR.
    IF NOT AVAILABLE custype THEN DO:
        ASSIGN cError  = "Invalid Type".
        RETURN. 
    END.
    FIND FIRST sman WHERE sman.sman = prmsman NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
        ASSIGN cError  = "Invalid Salesman".
        RETURN. 
    END.
    FIND FIRST zipcode WHERE zipcode.city = prmcity NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid City".
        RETURN. 
    END.
    FIND FIRST statecod WHERE statecod.statecod = prmstate NO-LOCK NO-ERROR.
    IF NOT AVAILABLE statecod THEN DO:
        ASSIGN cError  = "Invalid State".
        RETURN. 
    END.
    FIND FIRST zipcode WHERE zipcode.zipcode = prmzip NO-LOCK NO-ERROR.
    IF NOT AVAILABLE zipcode THEN DO:
        ASSIGN cError  = "Invalid zip".
        RETURN. 
    END.
     FIND FIRST terr WHERE terr.terr = prmterr NO-LOCK NO-ERROR.
    IF NOT AVAILABLE terr THEN DO:
        ASSIGN cError  = "Invalid Territory".
        RETURN.
    END.
     
END.


/*************************************/
/*****************************************PROCEDURE assign-RfqShipping******************************/


