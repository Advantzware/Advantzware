




/*------------------------------------------------------------------------
    File        : SalesRep.p
    Purpose     : UserMaintenance

    Syntax      :

    Description : Return a Dataset of all user Maintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserSales NO-UNDO 
    BEFORE-TABLE beforeUserSales
    FIELD Suser_id    as char
    FIELD sales      AS CHARACTER FORMAT  "x(8)"
    FIELD SName  AS CHARACTER FORMAT  "x(30)"
    FIELD DefSman   AS LOGICAL INITIAL TRUE.

DEFINE DATASET dsUserSales FOR ttUserSales .

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmSman      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSName     AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmSmanDef   AS CHARACTER  NO-UNDO.     

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserSales.
DEFINE BUFFER bf-UserSales FOR usersman.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmAction   = ? THEN ASSIGN prmAction = "Select".
IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmSman     = ? THEN ASSIGN prmSman     = "".
IF prmSName     = ? THEN ASSIGN prmSName     = "".
IF prmSmanDef   = ?  THEN ASSIGN prmSmanDef = "".

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */

 
FOR EACH ttUserSales:
    DELETE ttUserSales.
END.

IF prmAction = "delete" THEN DO:
    FIND FIRST bf-UserSales WHERE bf-UserSales.user_id = prmUser 
                           AND bf-UserSales.company = prmComp 
                           AND  bf-UserSales.sman = prmSman EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL bf-UserSales  THEN DO:
        DELETE bf-UserSales.        
  END.  /*IF AVAIL bf_UserSales */
  

   ASSIGN prmAction = "Select".
END. /*IF prmAction = "delete"*/
IF prmAction = "Update" THEN DO:
   FIND FIRST bf-UserSales WHERE bf-UserSales.user_id = prmUser 
                            AND bf-UserSales.company = prmComp 
                            AND bf-UserSales.sman = prmSman EXCLUSIVE-LOCK NO-ERROR.
   IF  AVAILABLE bf-UserSales  THEN DO:
       ASSIGN
          
          bf-UserSales.sman_default =  IF prmSmanDef = "yes" THEN  TRUE ELSE FALSE.

     RELEASE bf-UserSales.
   END.

   ASSIGN prmAction = "Select".

END.
IF prmAction = "AddSman" THEN DO:
   FIND FIRST bf-UserSales WHERE bf-UserSales.user_id = prmUser 
                            AND bf-UserSales.company = prmComp 
                            AND bf-UserSales.sman = prmSman NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bf-UserSales  THEN DO:
       CREATE bf-UserSales.
       ASSIGN
          bf-UserSales.user_id         =  prmUser
          bf-UserSales.company         =  prmComp
          bf-UserSales.sman         =  prmSman
          bf-UserSales.sman_default =  IF prmSmanDef = "yes" THEN TRUE ELSE FALSE.
RELEASE bf-UserSales.
   END.
   ASSIGN prmAction = "Select".
END.


IF prmAction = "Search" THEN DO:

    FOR EACH usersman WHERE usersman.user_id = prmUser
                        AND usersman.company  = prmComp 
                        AND (usersman.sman = prmSman OR prmSman = "") NO-LOCK :
       FIND FIRST sman WHERE sman.company = usersman.company
                         AND sman.sman = usersman.sman  
                         AND (sman.sname = prmSName OR prmSName = "")  NO-LOCK NO-ERROR.
       CREATE ttUserSales.
           assign
               ttUserSales.Suser_id     = usersman.user_id
               ttUserSales.sales        = usersman.sman
               ttUserSales.SName        = sman.sname
               ttUserSales.Defsman     = usersman.sman_default    .
    END.
END.
     
    IF prmAction = "Select" THEN DO:
    
    FOR EACH usersman WHERE usersman.user_id = prmUser 
                        AND usersman.company  = prmComp NO-LOCK :
       FIND FIRST sman WHERE sman.company = usersman.company
                         AND sman.sman = usersman.sman NO-LOCK NO-ERROR.
        RUN Createsman.
    END.  

END.  /*IF prmAction = "Select"*/

IF prmAction = "View" THEN DO:
    FIND FIRST usersman WHERE usersman.user_id = prmUser 
                          AND usersman.company  = prmComp 
                          AND usersman.sman = prmSman NO-LOCK NO-ERROR.
    IF AVAIL usersman THEN DO:
    FIND FIRST sman WHERE sman.company = usersman.company 
                      AND sman.sman = usersman.sman NO-LOCK NO-ERROR.
       RUN Createsman.
   END.        
END.  /*IF prmAction = "View  "*/

PROCEDURE  Createsman:
    CREATE ttUserSales.
    assign
        ttUserSales.Suser_id    = usersman.user_id
        ttUserSales.sales       = usersman.sman 
        ttUserSales.SName       = sman.sname
        ttUserSales.DefSman     = usersman.sman_default    .
END PROCEDURE.



