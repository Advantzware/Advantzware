
/*------------------------------------------------------------------------
    File        : LocationLook.p
    Purpose     : Location Look up

    Syntax      :

    Description : Return a Dataset of all Location Look

    Author(s)   : Sewa
    Created     : Apr 01 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLocationLook NO-UNDO 
    FIELD vLoc  LIKE loc.loc
    FIELD vName  LIKE loc.dscr.


DEFINE DATASET dsLocationLook FOR ttLocationLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsLocationLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    
if prmAction <> "search" then do:
    FOR EACH loc WHERE loc.company = prmComp NO-LOCK:
        create ttLocationLook.
            assign                                         
                ttLocationLook.vLoc        = loc.loc
                ttLocationLook.vName       = loc.dscr
                .
    END.	 /* FOR EACH locat */
    
END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH loc WHERE loc.company = prmComp no-lock:

                IF loc.loc = prmText OR loc.dscr = prmText THEN
                DO:
                   create ttLocationLook.
                   assign
                       ttLocationLook.vLoc      = loc.loc
                       ttLocationLook.vName     = loc.dscr.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH loc WHERE loc.company = prmComp no-lock:

                IF loc.loc BEGINS prmText OR loc.dscr begins prmText THEN
                DO:
                   create ttLocationLook.
                   assign
                      ttLocationLook.vLoc      = loc.loc
                      ttLocationLook.vName     = loc.dscr.
                END.
             END.  /*FOR EACH loc where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "loc"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH loc WHERE loc.loc = prmText AND loc.company = prmComp no-lock:
                create ttLocationLook.
                assign 
                    ttLocationLook.vLoc        = loc.loc
                    ttLocationLook.vName       = loc.dscr

                    .
            end. /*FOR EACH loc where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH loc WHERE loc.loc = prmText AND loc.company = prmComp no-lock:
                create ttLocationLook.
                assign
                ttLocationLook.vLoc            = loc.loc
                ttLocationLook.vName           = loc.dscr
                
                    
                    .
            end.  /*FOR EACH loc wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
    IF prmField = "name" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH loc WHERE loc.dscr = prmText AND loc.company = prmComp no-lock:
                create ttLocationLook.
                assign
                    ttLocationLook.vLoc        = loc.loc
                    ttLocationLook.vName       = loc.dscr
                
                    
                    .
            END. /*FOR EACH loc where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH loc  WHERE loc.dscr begins prmText AND loc.company = prmComp no-lock:
                create ttLocationLook.
                assign 
                    ttLocationLook.vLoc        = loc.loc
                    ttLocationLook.vName       = loc.dscr
                
                    
                    .
            END. /*FOR EACH loc where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = name */
   
    
END.  /* IF prmAction = search then do: */

