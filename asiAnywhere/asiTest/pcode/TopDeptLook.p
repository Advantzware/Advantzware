
/*------------------------------------------------------------------------
    File        : TopDeptLook.p
    Purpose     : TopDeptLook

    Syntax      :

    Description : Return a Dataset of all Dept

    Author(s)   : 
    Created     : july 31, 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTopDeptLook NO-UNDO
    FIELD vCode AS CHAR 
    FIELD vDscr AS CHAR
    FIELD vFold AS INT .

DEFINE DATASET dsTopDeptLook FOR ttTopDeptLook.

DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCode      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDscr      AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopDeptLook.

IF prmUser  = ? THEN ASSIGN prmUser = "".
IF prmCode = ?  THEN ASSIGN prmCode = "".
IF prmDscr = ?  THEN ASSIGN prmDscr = "".
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
    FOR EACH dept  NO-LOCK:
        CREATE ttTopDeptLook.
        ASSIGN 
            ttTopDeptLook.vCode = dept.CODE
            ttTopDeptLook.vDscr = dept.dscr
            ttTopDeptLook.vFold = dept.fc
            .
     END.
END.

IF prmAction = "Search" THEN DO:
    if prmField = "code" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH dept WHERE dept.CODE = prmText  NO-LOCK:
            CREATE ttTopDeptLook.
            ASSIGN 
                ttTopDeptLook.vCode = dept.CODE
                ttTopDeptLook.vDscr = dept.dscr
                ttTopDeptLook.vFold = dept.fc
                .
         END.
    END.
    if prmCondition = "BEGIN" then do:
        FOR EACH dept WHERE dept.CODE BEGINS prmText  NO-LOCK:
            CREATE ttTopDeptLook.
            ASSIGN 
                ttTopDeptLook.vCode = dept.CODE
                ttTopDeptLook.vDscr = dept.dscr
                ttTopDeptLook.vFold = dept.fc
                .
         END.
    END.
    END.

    if prmField = "dscr" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH dept WHERE dept.dscr = prmText  NO-LOCK:
            CREATE ttTopDeptLook.
            ASSIGN 
                ttTopDeptLook.vCode = dept.CODE
                ttTopDeptLook.vDscr = dept.dscr
                ttTopDeptLook.vFold = dept.fc
                .
         END.
    END.
    if prmCondition = "BEGIN" then do:
        FOR EACH dept WHERE dept.dscr BEGINS prmText  NO-LOCK:
            CREATE ttTopDeptLook.
            ASSIGN 
                ttTopDeptLook.vCode = dept.CODE
                ttTopDeptLook.vDscr = dept.dscr
                ttTopDeptLook.vFold = dept.fc
                .
         END.
    END.
    END.
END.
