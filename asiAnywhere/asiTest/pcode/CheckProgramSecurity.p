/*------------------------------------------------------------------------
    File        : CheckProgramSecurity.p
    Purpose     : Category

    Syntax      :

    Description : Return a Output value for program security

    Author(s)   : Kuldeep
    Created     : Oct 09 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER programName  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER CanCreate   AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER CanRun      AS LOGICAL    NO-UNDO.

DEFINE OUTPUT PARAMETER CanUpdate   AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER CanDelete   AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER prmComp   AS CHAR    NO-UNDO.
DEFINE OUTPUT PARAMETER vUsers   AS CHAR    NO-UNDO.


FIND FIRST prgrms WHERE  prgrms.prgmname = programName NO-LOCK NO-ERROR.
IF AVAILABLE prgrms  THEN DO:

    IF LOOKUP(prmUser, prgrms.can_update) > 0 or prgrms.can_update = "*" THEN
        ASSIGN CanUpdate = TRUE.
    else
        ASSIGN CanUpdate = FALSE.

    IF LOOKUP(prmUser, prgrms.can_run) > 0 or prgrms.can_run = "*" THEN
        ASSIGN CanRun = TRUE.
    else
        ASSIGN CanRun = FALSE.

    IF LOOKUP(prmUser, prgrms.can_delete) > 0 or prgrms.can_delete = "*" THEN
        ASSIGN CanDelete = TRUE.
    else
        ASSIGN CanDelete = FALSE.

    IF LOOKUP(prmUser, prgrms.can_create) > 0 or prgrms.can_create = "*" THEN
        ASSIGN CanCreate = TRUE.
    else
        ASSIGN CanCreate = FALSE.
        

END.
/*    Company logic***************************/
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


/****************** internal or external user logic***************************/
FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
IF AVAILABLE users THEN DO:
    IF users.internal-user = NO THEN DO:
        ASSIGN vUsers = "external".
    END. /*IF users.internal-user = NO*/
    IF users.internal-user = YES THEN DO:
        ASSIGN vUsers = "internal".
    END. /*IF users.internal-user = yes*/
END. /*IF AVAILABLE users*/
