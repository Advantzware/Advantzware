



/*------------------------------------------------------------------------
    File         : CadLook.p
    Purpose     :  CadLook

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCadLook NO-UNDO 
    FIELD vcadno           AS CHARACTER
    FIELD vitemno          AS CHARACTER
    FIELD vitemname        AS CHARACTER
    FIELD vSpc             AS CHAR
    FIELD vUpc             AS CHAR
   
    .
                                           
    
DEFINE DATASET dsCadItemLook FOR ttCadLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCadItemLook.
       
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

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
        
    FOR EACH itemfg WHERE itemfg.company = prmComp NO-LOCK:
        IF itemfg.cad-no <> "" THEN DO:
                 create ttCadLook.
                 assign                                     
                    ttCadLook.vcadno      = itemfg.cad-no
                    ttCadLook.vitemno     = itemfg.i-no
                    ttCadLook.vitemname   = itemfg.i-name
                    ttCadLook.vSpc        = itemfg.spc-no
                    ttCadLook.vUpc        = itemfg.upc-no
                        .
                     
        END.
   END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
       
     if prmField = "cadcode"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cad-no = prmText  NO-LOCK :
                  IF itemfg.cad-no <> "" THEN DO:
                 create ttCadLook.
                 assign                                     
                    ttCadLook.vcadno      = itemfg.cad-no
                    ttCadLook.vitemno     = itemfg.i-no
                    ttCadLook.vitemname   = itemfg.i-name 
                    ttCadLook.vSpc        = itemfg.spc-no
                    ttCadLook.vUpc        = itemfg.upc-no .
             END.
             END.
          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.cad-no BEGINS prmText NO-LOCK :
                       IF itemfg.cad-no <> "" THEN DO:
                      create ttCadLook.
                      assign   
                          ttCadLook.vcadno      = itemfg.cad-no
                          ttCadLook.vitemno     = itemfg.i-no
                          ttCadLook.vitemname   = itemfg.i-name 
                          ttCadLook.vSpc        = itemfg.spc-no
                          ttCadLook.vUpc        = itemfg.upc-no.
                       END.
                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */

   if prmField = "spc"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.spc-no = prmText  NO-LOCK :
                  IF itemfg.cad-no <> "" THEN DO:
                 create ttCadLook.
                 assign                                     
                    ttCadLook.vcadno      = itemfg.cad-no
                    ttCadLook.vitemno     = itemfg.i-no
                    ttCadLook.vitemname   = itemfg.i-name
                    ttCadLook.vSpc        = itemfg.spc-no
                     ttCadLook.vUpc        = itemfg.upc-no.
             END.
             END.
          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.spc-no BEGINS prmText NO-LOCK :
                       IF itemfg.cad-no <> "" THEN DO:
                      create ttCadLook.
                      assign   
                          ttCadLook.vcadno      = itemfg.cad-no
                          ttCadLook.vitemno     = itemfg.i-no
                          ttCadLook.vitemname   = itemfg.i-name 
                          ttCadLook.vSpc        = itemfg.spc-no
                          ttCadLook.vUpc        = itemfg.upc-no.
                       END.
                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           

         if prmField = "upc"  then do:
              if prmCondition = "EQUAL" then do:
                  FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.upc-no = prmText  NO-LOCK :
                       IF itemfg.cad-no <> "" THEN DO:
                      create ttCadLook.
                      assign                                     
                         ttCadLook.vcadno      = itemfg.cad-no
                         ttCadLook.vitemno     = itemfg.i-no
                         ttCadLook.vitemname   = itemfg.i-name
                         ttCadLook.vSpc        = itemfg.spc-no
                          ttCadLook.vUpc        = itemfg.upc-no.
                  END.
                  END.
               END. /*FOR EACH state*/
               IF prmCondition = "BEGIN" then do:
                       FOR EACH itemfg WHERE itemfg.company = prmComp AND itemfg.upc-no BEGINS prmText NO-LOCK :
                            IF itemfg.cad-no <> "" THEN DO:
                           create ttCadLook.
                           assign   
                               ttCadLook.vcadno      = itemfg.cad-no
                               ttCadLook.vitemno     = itemfg.i-no
                               ttCadLook.vitemname   = itemfg.i-name 
                               ttCadLook.vSpc        = itemfg.spc-no
                               ttCadLook.vUpc        = itemfg.upc-no.
                            END.
                       end.  /*FOR EACH state wher*/
                 end.    /*if prmCondition = BEGIN*/    
              end.  /* if prmField = state  */

END.  /* IF prmAction = search then do: */



