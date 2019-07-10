


/*------------------------------------------------------------------------
    File        : StateLook.p
    Purpose     : State

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttStateLook NO-UNDO 
    FIELD vStateCode AS CHARACTER
    FIELD vDscrp AS CHARACTER
    FIELD vFif AS INTEGER
    .
                                           
    
DEFINE DATASET dsStateLook FOR ttStateLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsStateLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

IF prmField      = ? THEN ASSIGN prmField      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
MESSAGE "state" prmAction prmUser prmCondition prmText prmField.
if prmAction <> "search" then do:
      
    FOR EACH statecod NO-LOCK:
                 create ttStateLook.
                 assign                                     
                    ttStateLook.vStateCode = statecod.statecod
                    ttStateLook.vDscrp = statecod.description
                    ttStateLook.vFif = statecod.fips_code
                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
         MESSAGE "Test".
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH statecod  NO-LOCK:
                    IF (statecod.statecod = prmText OR statecod.description = prmText) THEN
                        DO:
                        create ttStateLook.
                        assign                                     
                            ttStateLook.vStateCode = statecod.statecod
                            ttStateLook.vDscrp = statecod.description
                            ttStateLook.vFif = statecod.fips_code.
                    END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH statecod NO-LOCK :
                IF (statecod.statecod BEGINS prmText OR statecod.description BEGINS prmText) THEN
                    DO:
                    create ttStateLook.
                    assign                                     
                        ttStateLook.vStateCode = statecod.statecod
                        ttStateLook.vDscrp = statecod.description
                        ttStateLook.vFif = statecod.fips_code.
                    END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "statecod"  then do:
        if prmCondition = "EQUAL" then do:
             MESSAGE "Test2".
             FOR EACH statecod WHERE  statecod.statecod = prmText  NO-LOCK :
                 create ttStateLook.
                 assign                                     
                     ttStateLook.vStateCode = statecod.statecod
                     ttStateLook.vDscrp = statecod.description
                     ttStateLook.vFif = statecod.fips_code.
                 MESSAGE "jyoti"  statecod.statecod  ttStateLook.vStateCode.
             END.
             

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH statecod WHERE statecod.statecod BEGINS prmText NO-LOCK :
                      create ttStateLook.
                      assign   
                          ttStateLook.vStateCode = statecod.statecod
                          ttStateLook.vDscrp = statecod.description
                          ttStateLook.vFif = statecod.fips_code.

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */

