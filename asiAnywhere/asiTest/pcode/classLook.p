

/*------------------------------------------------------------------------
    File         : CLASS Lookup
    Purpose     :  ITEMFG CLASS lookup

    Syntax      :

    Description : Return a Dataset For Class Inventory Report

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttClassLook NO-UNDO 
        FIELD vClass AS CHARACTER
                  .
                                           
    
DEFINE DATASET dsClassLook FOR ttClassLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsClassLook.

DEFINE VAR vfgclass AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
ASSIGN vfgclass =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .
IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
         NO-LOCK:
        OE-LOOP:
        FOR EACH itemfg WHERE itemfg.company = prmComp  no-lock:
            FIND FIRST ttClassLook WHERE ttClassLook.vClass = itemfg.CLASS NO-LOCK NO-ERROR.
            IF AVAIL ttClassLook THEN NEXT OE-LOOP.
            create ttClassLook.
            assign                                     
                ttClassLook.vClass      = itemfg.CLASS
                .                       
        END.	 /* FOR EACH itemfg */
    END.
 END.  /*if prmAction <> "search" */
IF prmAction = "search" then do:
    if prmField = "class" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH itemfg where itemfg.company = prmComp AND itemfg.CLASS = vfgclass no-lock:
                create ttClassLook.
                assign                                     
                    ttClassLook.vClass      = itemfg.CLASS.                     
           END.  /*FOR EACH itemfg*/
        END.   /*if prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:
            FOR EACH itemfg where itemfg.company = prmComp AND itemfg.CLASS begins vfgclass no-lock:
                create ttClassLook.
                assign                                                                  
                    ttClassLook.vClass      = itemfg.CLASS.
            END.  /*FOR EACH itemfg*/
        END.  /*if prmCondition = "BEGIN"*/       
    END.   /*  if prmField = "class" then do:*/
 END. /* IF prmAction = search then do: */


