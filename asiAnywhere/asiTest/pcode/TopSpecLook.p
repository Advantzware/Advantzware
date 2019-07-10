
/*------------------------------------------------------------------------
    File        : TopSpecLook.p
    Purpose     : TopDeptLook

    Syntax      :

    Description : Return a Dataset of all Dept

    Author(s)   : 
    Created     : july 31, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTopSpecLook NO-UNDO
    FIELD vCode AS CHAR 
    FIELD vDscr AS CHAR
    FIELD hjgkjhgjh  AS INT .

DEFINE DATASET dsTopSpecLook FOR ttTopSpecLook.

DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopSpecLook.

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
    FOR EACH item-spec WHERE  item-spec.company = prmComp
        AND item-spec.i-no = '' NO-LOCK :
        
        CREATE ttTopSpecLook.
        ASSIGN 
            ttTopSpecLook.vCode = item-spec.CODE
            ttTopSpecLook.vDscr = item-spec.notes[1]
            .
     END.
END.

IF prmAction = "Search" THEN DO:
    if prmField = "code" then do:
    if prmCondition = "EQUAL" then do:
       FOR EACH item-spec WHERE  item-spec.company = prmComp
        AND item-spec.i-no = '' AND item-spec.CODE = prmText NO-LOCK :
        CREATE ttTopSpecLook.
        ASSIGN 
            ttTopSpecLook.vCode = item-spec.CODE
            ttTopSpecLook.vDscr = item-spec.notes[1]
                .
         END.
    END.
    if prmCondition = "BEGIN" then do:
         FOR EACH item-spec WHERE  item-spec.company = prmComp
        AND item-spec.i-no = '' AND item-spec.CODE BEGINS prmText NO-LOCK :
        CREATE ttTopSpecLook.
        ASSIGN 
            ttTopSpecLook.vCode = item-spec.CODE
            ttTopSpecLook.vDscr = item-spec.notes[1]
                .
         END.
    END.
    END.

    if prmField = "dscr" then do:
    if prmCondition = "EQUAL" then do:
         FOR EACH item-spec WHERE  item-spec.company = prmComp
        AND item-spec.i-no = '' AND item-spec.notes[1] = prmText NO-LOCK :
        CREATE ttTopSpecLook.
        ASSIGN 
            ttTopSpecLook.vCode = item-spec.CODE
            ttTopSpecLook.vDscr = item-spec.notes[1]
                .
         END.
    END.
    if prmCondition = "BEGIN" then do:
         FOR EACH item-spec WHERE  item-spec.company = prmComp
        AND item-spec.i-no = '' AND item-spec.notes[1] BEGINS prmText NO-LOCK :
        CREATE ttTopSpecLook.
        ASSIGN 
            ttTopSpecLook.vCode = item-spec.CODE
            ttTopSpecLook.vDscr = item-spec.notes[1]
                .
         END.
    END.
    END.
END.
