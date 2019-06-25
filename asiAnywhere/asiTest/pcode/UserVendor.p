



/*------------------------------------------------------------------------
    File        : UserVendor.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all user Maintenance

    Author(s)   : 
    Created     : Apr 02 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserVend NO-UNDO 
    BEFORE-TABLE beforeUserVend
    FIELD User_id    as char
    FIELD user_name  AS CHAR
    FIELD Vend      AS CHARACTER FORMAT  "x(8)"
    FIELD VendName  AS CHARACTER FORMAT  "x(30)"
    FIELD DefVend   AS LOGICAL INITIAL TRUE.

DEFINE DATASET dsUserVendor FOR ttUserVend .

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmVend      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVendName  AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmVendDef   AS CHARACTER  NO-UNDO.     


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserVendor.

DEFINE BUFFER bf-UserVend FOR uservend.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmAction   = ? THEN ASSIGN prmAction = "Select".
IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmVend     = ? THEN ASSIGN prmVend     = "".
IF prmVendDef   = ?  THEN ASSIGN prmVendDef = "".

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */

MESSAGE "vendor" prmUser prmComp prmAction prmVend prmVendDef.

FOR EACH ttUserVend:
    DELETE ttUserVend.
END.

IF prmAction = "delete" THEN DO:
    FIND FIRST bf-UserVend WHERE bf-UserVend.user_id = prmUser 
                             AND bf-UserVend.company = prmComp 
                             AND  bf-UserVend.vend-no = prmVend EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-UserVend  THEN DO:
        DELETE bf-UserVend.        
    END.  /*IF AVAIL bf_UserVend */
    ASSIGN prmAction = "Select".
END. /*IF prmAction = "delete"*/

IF prmAction = "Update" THEN DO:
    FIND FIRST bf-UserVend WHERE bf-UserVend.user_id = prmUser 
                            AND bf-UserVend.company = prmComp 
                            AND bf-UserVend.vend-no = prmVend EXCLUSIVE-LOCK NO-ERROR.
   IF  AVAILABLE bf-UserVend  THEN DO:
       ASSIGN
           bf-UserVend.vend_default =  IF prmVendDef = "yes" THEN  TRUE ELSE FALSE.
           RELEASE bf-UserVend.
   END.
   ASSIGN prmAction = "Select".
END.

IF prmAction = "AddVendor" THEN DO:
   FIND FIRST bf-UserVend WHERE bf-UserVend.user_id = prmUser 
                            AND bf-UserVend.company = prmComp 
                            AND bf-UserVend.vend-no = prmVend NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bf-UserVend  THEN DO:
       CREATE bf-UserVend.
       ASSIGN
           bf-UserVend.user_id         =  prmUser
           bf-UserVend.company         =  prmComp
           bf-UserVend.vend-no         =  prmVend
           bf-UserVend.vend_default =  IF prmVendDef = "yes" THEN TRUE ELSE FALSE.
           RELEASE bf-UserVend.
   END.
   ASSIGN prmAction = "Select".
END.       

IF prmAction = "Search" THEN DO:
    FOR EACH uservend WHERE uservend.user_id = prmUser
                        AND uservend.company  = prmComp 
                        AND (uservend.vend-no = prmVend OR prmVend = "") NO-LOCK :
       FIND FIRST vend WHERE vend.company = uservend.company
                         AND vend.vend-no = uservend.vend-no  
                         AND (vend.NAME = prmVendName OR prmVendName = "")  NO-LOCK NO-ERROR.
       CREATE ttuservend.
       assign
           ttUserVend.User_id     = uservend.user_id
           ttUserVend.Vend        = uservend.vend-no
           ttUserVend.VendName    = vend.name
           ttUserVend.DefVend     = uservend.vend_default    .
    END.
END.

IF prmAction = "Select" THEN DO:
    FOR EACH uservend WHERE uservend.user_id = prmUser 
                        AND uservend.company  = prmComp NO-LOCK :
       FIND FIRST vend WHERE vend.company = uservend.company
                         AND vend.vend-no = uservend.vend-no NO-LOCK NO-ERROR.
        RUN CreateVendor.
   END.  

END.  /*IF prmAction = "Select"*/

IF prmAction = "View" THEN DO:
    FIND FIRST uservend WHERE uservend.user_id = prmUser 
                          AND uservend.company  = prmComp 
                          AND uservend.vend-no = prmVend NO-LOCK NO-ERROR.
    IF AVAIL uservend THEN DO:
    FIND FIRST vend WHERE vend.company = uservend.company 
                      AND vend.vend-no = uservend.vend-no NO-LOCK NO-ERROR.
       RUN CreateVendor.
   END.        
END.  /*IF prmAction = "View  "*/

PROCEDURE  CreateVendor:
    CREATE ttUserVend.
    assign
        ttUserVend.User_id     = uservend.user_id
        ttUserVend.Vend        = uservend.vend-no
        ttUserVend.VendName    = vend.name
        ttUserVend.DefVend     = uservend.vend_default    .
END PROCEDURE.



