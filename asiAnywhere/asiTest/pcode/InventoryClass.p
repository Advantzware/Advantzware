




/*------------------------------------------------------------------------
    File         : InventoryClass.p
    Purpose     :  InventoryClass

    Syntax      :

    Description : Return a Dataset of InventoryClass

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInventoryClass NO-UNDO 
    FIELD vClass           AS CHARACTER
    FIELD gh AS CHAR
    .
DEFINE DATASET dsInveClassLook FOR ttInventoryClass .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInveClassLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
        
    FOR EACH itemfg WHERE itemfg.company = prmComp NO-LOCK:
        IF itemfg.CLASS <> "" THEN DO:
       FIND FIRST  ttInventoryClass WHERE ttInventoryClass.vClass = itemfg.CLASS NO-LOCK NO-ERROR.
       IF AVAIL ttInventoryClass THEN NEXT.
        create ttInventoryClass.
        assign                                     
            ttInventoryClass.vClass      = itemfg.CLASS.
        END.
        
        END.
END.  /*ifif prmAction <> "search" */

    
