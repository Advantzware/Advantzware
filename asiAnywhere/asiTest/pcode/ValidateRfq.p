

/*------------------------------------------------------------------------
    File        : ValidateRfq.p
    Purpose     : RFQS Maintenance

    Syntax      :

    Description : 

    Author(s)   : Sewa Singh
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
def NEW SHARED {1} {2} var cocode     as   char  format "x(3)"  no-undo.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER RfqStock     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER Rfqstyle     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER RfqProcat    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER RfqBoard     AS CHAR  NO-UNDO.

DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmComp   = ?   THEN ASSIGN prmComp = "".
IF prmUser   = ?   THEN ASSIGN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF RfqStock  = ?  THEN ASSIGN RfqStock = "".
IF Rfqstyle  = ?  THEN ASSIGN Rfqstyle = "".
IF RfqProcat  = ?  THEN ASSIGN RfqProcat = "".
IF RfqBoard  = ?  THEN ASSIGN RfqBoard = "".

DEF VAR prmLoc AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
cocode = prmComp.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

/**************************************************************************/


IF prmAction = "ValidateRfq" THEN DO:
    IF RfqStyle <> "" THEN DO:
        FIND FIRST style WHERE style.company = prmComp AND style.style = RfqStyle NO-LOCK NO-ERROR.
        IF NOT AVAIL style THEN DO:
            cError = "Invalide Style".
            RETURN.
        END.
    END.    
    IF RfqProcat <> "" THEN DO:
        FIND FIRST fgcat WHERE fgcat.company = prmComp AND fgcat.procat = RfqProcat NO-LOCK NO-ERROR.
        IF NOT AVAIL fgcat THEN DO:
            ASSIGN 
            cError =  "Invalid Product Category. " .
            RETURN.        
        END.
    END. 
    IF RfqStock <> "" THEN DO:
        find first itemfg where itemfg.company = prmComp AND itemfg.i-no = RfqStock no-lock no-error.        
        if not avail itemfg then do:
            ASSIGN 
            cError = "Invalid FG Item#. Try Help.".
            RETURN. 
        end.  
    END.
    IF RfqBoard <> "" THEN DO:
        FIND FIRST item WHERE item.company = prmComp and item.i-no = RfqBoard  AND
        (item.mat-type = "B" or item.mat-type = "P" or item.mat-type = "1" 
         OR item.mat-type = "2" or item.mat-type = "3" or item.mat-type = "4") NO-LOCK NO-ERROR.
        IF NOT AVAIL item then do:        
            ASSIGN cError = "Invalid Board".
            RETURN. 
        end. 
    END.
    
END.
