
/*------------------------------------------------------------------------
    File        : GroupLookup.p
    Purpose     : TopDeptLook

    Syntax      :

    Description : Return a Dataset of all Dept

    Author(s)   : 
    Created     : july 31, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTopGroupLook NO-UNDO
    FIELD vGroup AS CHAR 
    FIELD vGroupDscr AS CHAR
    FIELD pnhfj AS INT .

DEFINE DATASET dsTopGroupLook FOR ttTopGroupLook.

DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopGroupLook.

IF prmUser  = ? THEN ASSIGN prmUser = "".
IF prmAction = ?  THEN ASSIGN prmAction = "".
IF prmField      =  ?  THEN ASSIGN prmField      =  "".
IF prmCondition  =  ?  THEN ASSIGN prmCondition  =  "".
IF prmText       = ?   THEN ASSIGN prmText       =  "".


DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
MESSAGE "action" prmAction.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
IF prmAction <> "Search" THEN DO:
    FOR EACH usergrps NO-LOCK:
        CREATE ttTopGroupLook.
        ASSIGN 
            ttTopGroupLook.vGroup     = usergrps.usergrps
            ttTopGroupLook.vGroupDscr = usergrps.dscr
            
            .
     END.
END.
