

/*------------------------------------------------------------------------
    File        : TerrLook.p
    Purpose     : Territory

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTerrotLook NO-UNDO 
    FIELD VTerro AS CHARACTER
    FIELD VDscr AS CHARACTER
    .
                                           
    
DEFINE DATASET dsTerrotLook FOR ttTerrotLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTerrotLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

/*prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".*/

prmComp = "001".
MESSAGE "company" prmComp.
if prmAction <> "search" then do:
    FOR EACH terr WHERE terr.company = prmComp 
            NO-LOCK:
                 create ttTerrotLook.
                 assign                                     
                    ttTerrotLook.VTerro = terr.terr  
                    ttTerrotLook.VDscr = terr.dscr  
                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH terr WHERE terr.company = prmComp NO-LOCK:
                    IF (terr.terr = prmText OR terr.dscr = prmText) THEN
                        DO:
                        create ttTerrotLook.
                        assign                                     
                            ttTerrotLook.VTerro = terr.terr  
                            ttTerrotLook.VDscr = terr.dscr .
                    END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH terr WHERE terr.company = prmComp  NO-LOCK :
                IF (terr.terr BEGINS prmText OR terr.dscr BEGINS prmText) THEN
                    DO:
                    create ttTerrotLook.
                    assign                                     
                        ttTerrotLook.VTerro = terr.terr  
                        ttTerrotLook.VDscr = terr.dscr .
                    END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "terr"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH terr WHERE terr.company = prmComp AND terr.terr = prmText  NO-LOCK :
                 create ttTerrotLook.
                 assign                                     
                     ttTerrotLook.VTerro = terr.terr  
                     ttTerrotLook.VDscr = terr.dscr. 
             END.

          END. /*FOR EACH terr*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH terr WHERE terr.company = prmComp AND terr.terr BEGINS prmText NO-LOCK :
                      create ttTerrotLook.
                      assign                                     
                          ttTerrotLook.VTerro = terr.terr  
                          ttTerrotLook.VDscr = terr.dscr. 
                  end.  /*FOR EACH terr wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = terr  */
           
END.  /* IF prmAction = search then do: */

