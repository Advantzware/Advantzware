
/*------------------------------------------------------------------------
    File        : userman.p
    Purpose     : 
    Main File   : 
    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUserManTable NO-UNDO
    FIELD sman        AS CHAR
    FIELD sname       AS CHAR
    FIELD sdefault    AS CHAR
    FIELD reckey      AS CHAR
    FIELD smrec       AS CHAR 
    .

DEFINE DATASET dsUserManTable FOR ttUserManTable.
    

DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCondition  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmText       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmsman       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmsname      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmsdefault   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmReckey     AS CHAR NO-UNDO.   

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserManTable .
DEFINE OUTPUT PARAMETER cError   AS CHARACTER.
    

IF prmAction          = ?  THEN ASSIGN prmAction      = "Select".
IF prmComp            = ?  THEN ASSIGN prmComp        = "".
IF prmUser            = ?  THEN ASSIGN prmUser        = "".
IF prmCondition       = ?  THEN ASSIGN prmCondition   = "".
IF prmText            = ?  THEN ASSIGN prmText        = "".
IF prmsman            = ?  THEN ASSIGN prmsman        = "".
IF prmsname           = ?  THEN ASSIGN prmsname       = "".
IF prmsdefault        = ?  THEN ASSIGN prmsdefault    = "".
IF prmReckey          = ?  THEN ASSIGN prmReckey      = "".




DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEFINE NEW SHARED VAR g_loc  AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEFINE VAR vUsers AS CHAR NO-UNDO.
     IF prmComp EQ "" THEN
     DO:
        FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.

        prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
     END.

ASSIGN
    cocode = prmComp
    g_company = prmComp
    g_user    = prmUser  .



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
  ASSIGN g_loc = locode .

FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
IF AVAILABLE users THEN DO:
    IF users.internal-user = NO THEN DO:
        ASSIGN vUsers = "external".
    END. /*IF users.internal-user = NO*/
    IF users.internal-user = YES THEN DO:
        ASSIGN vUsers = "internal".
    END. /*IF users.internal-user = yes*/
END.

IF prmAction = "Select" THEN DO:
    IF vUsers = "external" THEN DO:
     FOR EACH usersman WHERE 
         usersman.user_id EQ prmUser
         AND usersman.company EQ cocode NO-LOCK,
         FIRST sman WHERE sman.company EQ usersman.company 
         AND sman.sman EQ usersman.sman NO-LOCK:
         

        CREATE ttUserManTable.
           ASSIGN 
                 ttUserManTable.sman          = usersman.sman
                 ttUserManTable.sname         = sman.sname
                 ttUserManTable.sdefault      = STRING(usersman.sman_default) .
     END.
    END.
     
     ELSE DO: /* vUsers = "internal"*/
        FOR EACH sman WHERE sman.company EQ cocode NO-LOCK:
            CREATE ttUserManTable.
            ASSIGN 
                 ttUserManTable.sman          = sman.sman
                 ttUserManTable.sname         = sman.sname
                 .
        END.

     END.
            
   
END. /*IF prmAction = "Select" THEN DO:*/

IF prmAction = "Customer" THEN DO:
    FOR EACH usercust WHERE usercust.user_id EQ prmUser 
         AND usercust.company EQ cocode NO-LOCK, 
             FIRST cust WHERE cust.company EQ usercust.company 
                          AND cust.cust-no EQ usercust.cust-no NO-LOCK:
        CREATE ttUserManTable.
           ASSIGN 
                 ttUserManTable.sman          = usercust.cust-no
                 ttUserManTable.sname         = cust.NAME .
    END. /* FOR EACH usercust */

END. /* prmAction */

/******************Search***********************************/

IF prmAction = "Search" then do:
    IF vUsers = "external" THEN DO:
         if prmCondition = "EQUAL" then do:
             FOR EACH usersman WHERE
                 usersman.user_id EQ prmUser
                 AND usersman.company EQ cocode 
                 AND usersman.sman EQ prmText NO-LOCK,
                 FIRST sman WHERE sman.company EQ usersman.company 
                 AND sman.sman EQ usersman.sman NO-LOCK:
                 

                 CREATE ttUserManTable.
                 ASSIGN 
                     ttUserManTable.sman          = usersman.sman
                     ttUserManTable.sname         = sman.sname
                     ttUserManTable.sdefault      = STRING(usersman.sman_default) .
             END. /*FOR EACH usersman  */
         END. /*prmCondition*/
         if prmCondition = "BEGIN" then do:
             FOR EACH usersman WHERE
                 usersman.user_id EQ prmUser
                 AND usersman.company EQ cocode
                 AND usersman.sman BEGINS prmText NO-LOCK,
                 FIRST sman WHERE sman.company EQ usersman.company 
                 AND sman.sman EQ usersman.sman NO-LOCK:
                 

                 CREATE ttUserManTable.
                 ASSIGN 
                     ttUserManTable.sman          = usersman.sman
                     ttUserManTable.sname         = sman.sname
                     ttUserManTable.sdefault      = STRING(usersman.sman_default) .
             END. /*FOR EACH usersman  */
         END. /*prmCondition*/
    END. /* auser = "external"*/

    ELSE DO: /*auser = "internal"  */
        if prmCondition = "EQUAL" then do:
             FOR EACH sman WHERE  sman.company EQ cocode
                 AND sman.sman EQ prmText NO-LOCK:
                 
                 CREATE ttUserManTable.
                 ASSIGN 
                     ttUserManTable.sman          = sman.sman
                     ttUserManTable.sname         = sman.sname
                     .
             END. /*FOR EACH usersman  */
         END. /*prmCondition*/
         if prmCondition = "BEGIN" then do:
             FOR EACH sman WHERE  sman.company EQ cocode
                 AND sman.sman BEGINS prmText NO-LOCK:

                 CREATE ttUserManTable.
                 ASSIGN 
                     ttUserManTable.sman          = sman.sman
                     ttUserManTable.sname         = sman.sname
                      .
             END. /*FOR EACH usersman  */
         END. /*prmCondition*/

    END.
END. /*Search*/



