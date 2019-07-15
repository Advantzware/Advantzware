


/*------------------------------------------------------------------------
    File        : UserLoc.p
    Purpose     : Users Location Maintenance

    Syntax      :

    Description : Return a Dataset of all Loaction
    Author(s)   : 
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUserLoc NO-UNDO 
    BEFORE-TABLE beforeUserComp
    FIELD ULID as char
    FIELD Loc    AS CHARACTER
    FIELD Dscr  AS CHARACTER
    FIELD DefLoc AS LOGICAL.

DEFINE DATASET dsUserloc FOR ttUserLoc .
DEFINE INPUT PARAMETER prmComp   AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmLoc      AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmDscr      AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER prmLocDef   AS CHARACTER  NO-UNDO.     
DEFINE INPUT PARAMETER vLoc   AS CHARACTER  NO-UNDO.     



DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserloc.

DEFINE BUFFER buff-usercomp FOR usercomp.

    IF prmComp     = ? THEN ASSIGN prmComp     = "".
       IF vLoc     = ? THEN ASSIGN vLoc     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmLocDef   = ? THEN ASSIGN prmLocDef     = "".
IF prmAction   = ? THEN ASSIGN prmAction     = "".
IF prmLoc      = ? THEN ASSIGN prmLoc     = "".
IF prmDscr     = ? THEN ASSIGN prmDscr     = "".
IF prmAction   = "" THEN ASSIGN prmAction = "Select".



/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
 
 MESSAGE "location" vLoc prmAction prmUser prmComp prmLoc prmLocDef.
FOR EACH ttUserLoc:
    DELETE ttUserLoc.
END.

IF prmAction = "delete" THEN DO:
  FIND FIRST buff-usercomp WHERE buff-usercomp.user_id = prmUser AND buff-usercomp.company = prmComp AND 
                                 buff-usercomp.loc = prmLoc EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL buff-usercomp  THEN DO:
      DELETE buff-usercomp.        
  END.  /*IF AVAIL bf_usercomp */
  

   ASSIGN prmAction = "Select".
END. /*IF prmAction = "delete"*/
IF prmAction = "Update" THEN DO:
    FIND FIRST buff-usercomp WHERE buff-usercomp.user_id = prmUser AND 
                                 buff-usercomp.loc = prmLoc AND buff-usercomp.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    IF  AVAILABLE buff-usercomp  THEN DO:
        ASSIGN
          /* buff-usercomp.loc         =  prmLoc*/
           buff-usercomp.loc_default =  IF prmLocDef = "yes" THEN TRUE ELSE FALSE.
        
      RELEASE buff-usercomp.
    END.
       
    ASSIGN prmAction = "Select".

END.

IF prmAction = "AddLoc" THEN DO:
    FIND FIRST buff-usercomp WHERE buff-usercomp.user_id = prmUser AND buff-usercomp.company = prmComp AND buff-usercomp.loc = prmLoc  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE buff-usercomp  THEN DO:
        CREATE buff-usercomp.
        ASSIGN
             buff-usercomp.user_id         =  prmUser
            buff-usercomp.company         =  prmComp
           buff-usercomp.loc               =  prmLoc
           buff-usercomp.loc_default       =   IF prmLocDef = "yes" THEN TRUE ELSE FALSE.

    END.
    ASSIGN prmAction = "Select".
END.

 

IF prmAction = "SearchLoc" THEN DO:
MESSAGE "locuser" prmAction   prmUser prmComp prmLoc  .
    FOR EACH usercomp WHERE usercomp.user_id = prmUser AND usercomp.company = prmComp AND 
                            (usercomp.loc = prmLoc OR prmLoc = "") NO-LOCK:
                             
        FIND FIRST loc WHERE loc.company = usercomp.company AND loc.loc = usercomp.loc and 
                               (loc.dscr = prmDscr OR prmDscr = "") NO-LOCK NO-ERROR.
        CREATE ttUserLoc.
            assign
                ttUserLoc.ULID       = usercomp.user_id
                ttUserLoc.Loc        = usercomp.loc
                ttUserLoc.Dscr       = loc.dscr
                ttUserLoc.DefLoc     =  usercomp.loc_default.
    END.
END.

IF prmAction = "Select" THEN DO:
    LOCGROUP:
    FOR EACH usercomp WHERE usercomp.user_id = prmUser  AND usercomp.company = prmComp  NO-LOCK :
        Find FIRST loc WHERE loc.company = usercomp.company AND loc.loc = usercomp.loc NO-LOCK NO-ERROR.
        IF usercomp.loc = "" THEN NEXT LOCGROUP.
        RUN CreateLoc.    
    END.        
END.  /*IF prmAction = "Select"*/
IF prmAction = "View" THEN DO:
    IF prmLoc <> "" THEN DO:
        FIND FIRST usercomp WHERE usercomp.user_id = prmUser AND
                             usercomp.company = prmComp AND
                             usercomp.loc = prmLoc NO-LOCK NO-ERROR.
    IF AVAIL usercomp THEN DO:
    Find FIRST loc WHERE loc.company = usercomp.company AND loc.loc = usercomp.loc NO-LOCK NO-ERROR.
       RUN CreateLoc.
   END.    /*IF AVAIL usercomp THEN*/
   END.   /* IF prmLoc <> "" THEN*/
END.  /*IF prmAction = "View  "*/

PROCEDURE CreateLoc:
CREATE ttUserLoc.
            assign
                ttUserLoc.ULID         = usercomp.user_id
                ttUserLoc.Loc          = usercomp.loc
                ttUserLoc.Dscr         = loc.dscr  
                ttUserLoc.DefLoc       =  usercomp.loc_default.
  MESSAGE "userlocation"  ttUserLoc.Loc.             
END PROCEDURE.
       
