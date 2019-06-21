

/*------------------------------------------------------------------------
    File        : UserComp.p
    Purpose     : User Company

    Syntax      :

    Description : Return a Dataset of user MAintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserComp NO-UNDO 
    BEFORE-TABLE beforeUserComp
    FIELD UID as char
    FIELD Comp    AS CHARACTER
    FIELD CompName  AS CHARACTER
    FIELD DefComp AS LOGICAL.

DEFINE DATASET dsUserComp FOR ttUserComp .

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmName      AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmCompDef   AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER vComp        AS CHARACTER  NO-UNDO.     

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserComp.

DEFINE BUFFER bf-usercomp FOR usercomp.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmAction   = ? THEN ASSIGN prmAction = "Select".

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCompDef   = ?  THEN ASSIGN prmCompDef = "".

IF prmName     = ? THEN ASSIGN prmName     = "".

IF vComp     = ? THEN ASSIGN vComp     = "".

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */

 
FOR EACH ttUserComp:
    DELETE ttUserComp.
END.
MESSAGE "bajaj" vComp prmAction prmUser prmComp  prmCompDef.

IF prmAction = "delete" THEN DO:
  FIND FIRST bf-usercomp WHERE bf-usercomp.user_id = prmUser AND 
                                bf-usercomp.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL bf-usercomp  THEN DO:
      DELETE bf-usercomp.        
  END.  /*IF AVAIL bf_usercomp */
  

   ASSIGN prmAction = "Select".
END. /*IF prmAction = "delete"*/
IF prmAction = "Update" THEN DO:
   FIND FIRST bf-usercomp WHERE bf-usercomp.user_id = prmUser AND bf-usercomp.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
   IF  AVAILABLE bf-usercomp  THEN DO:
       ASSIGN
          /*bf-usercomp.company         =  prmComp*/
          bf-usercomp.company_default =  IF prmCompDef = "yes" THEN  TRUE ELSE FALSE.

     RELEASE bf-usercomp.
   END.

   ASSIGN prmAction = "Select".

END.
IF prmAction = "AddCompany" THEN DO:
   FIND FIRST bf-usercomp WHERE bf-usercomp.user_id = prmUser AND bf-usercomp.company = prmComp  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bf-usercomp  THEN DO:
       CREATE bf-usercomp.
       ASSIGN
          bf-usercomp.user_id         =  prmUser
          bf-usercomp.company         =  prmComp
          bf-usercomp.company_default =  IF prmCompDef = "yes" THEN TRUE ELSE FALSE.
RELEASE bf-usercomp.
   END.
   ASSIGN prmAction = "Select".
END.


IF prmAction = "SearchCompany" THEN DO:
    FOR EACH usercomp WHERE usercomp.user_id = prmUser AND
                           (usercomp.company = prmComp OR prmComp = "") NO-LOCK:
        FIND FIRST company WHERE company.company = usercomp.company and 
                                 (company.NAME = prmName OR prmName = "") NO-LOCK NO-ERROR.
       CREATE ttUserComp.
           assign
               ttUserComp.UID         = usercomp.user_id
               ttUserComp.Comp        = usercomp.company
               ttUserComp.CompName    = company.NAME
               ttUserComp.DefComp     =  usercomp.company_default . 


   END.
END.
IF prmAction = "Select" THEN DO:
    COMP:
    FOR EACH usercomp WHERE usercomp.user_id = prmUser NO-LOCK :
       Find FIRST company WHERE company.company = usercomp.company NO-LOCK NO-ERROR.
       IF usercomp.company = "" THEN NEXT COMP.
       RUN CreateCompany.
   END.        
END.  /*IF prmAction = "Select"*/

IF prmAction = "View" THEN DO:
    FIND FIRST usercomp WHERE usercomp.user_id = prmUser AND usercomp.company = prmComp NO-LOCK NO-ERROR.
    IF AVAIL usercomp THEN DO:
    Find FIRST company WHERE company.company = usercomp.company NO-LOCK NO-ERROR.
       RUN CreateCompany.
   END.        
END.  /*IF prmAction = "View  "*/

PROCEDURE  CreateCompany:
    CREATE ttUserComp.
    assign
        ttUserComp.UID         = usercomp.user_id
        ttUserComp.Comp        = usercomp.company
        ttUserComp.CompName    = company.NAME
        ttUserComp.DefComp     = usercomp.company_default .
END PROCEDURE.
