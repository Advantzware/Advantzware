



/*------------------------------------------------------------------------
    File        :FgAdjLook.p
    Purpose     : fg-bin

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFgAbjLook 
    FIELD vItem   AS CHARACTER
    FIELD vName   AS CHARACTER
    FIELD vPartno AS CHARACTER
    FIELD vJob1   AS CHARACTER
    FIELD vJob2   AS INTEGER
    FIELD vLoc    AS CHARACTER
    FIELD vLocbin AS CHARACTER
    FIELD vtag    AS CHARACTER
    FIELD vcust   AS CHARACTER
    FIELD vqty    AS DECIMAL
    .
                                           
    
DEFINE DATASET dsFgAbjLook FOR ttFgAbjLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFgAbjLook.
       
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
        
    FOR EACH itemfg WHERE  itemfg.company = prmComp NO-LOCK:
       FOR EACH  fg-bin WHERE  fg-bin.company = prmComp
                     AND fg-bin.i-no = itemfg.i-no  and fg-bin.qty <> 0 NO-LOCK :
         FIND FIRST ttFgAbjLook WHERE  ttFgAbjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
           IF AVAIL ttFgAbjLook THEN NEXT.
                 create ttFgAbjLook.
                 assign                                     
                     ttFgAbjLook.vItem    = itemfg.i-no
                     ttFgAbjLook.vName    = itemfg.i-name
                     ttFgAbjLook.vPartno  = itemfg.part-no
                     ttFgAbjLook.vJob1   = fg-bin.job-no        
                     ttFgAbjLook.vJob2   = fg-bin.job-no2   
                     ttFgAbjLook.vLoc    = fg-bin.loc 
                     ttFgAbjLook.vLocbin = fg-bin.loc-bin    
                     ttFgAbjLook.vtag    = fg-bin.tag  
                     ttFgAbjLook.vcust   = fg-bin.cust-no 
                     ttFgAbjLook.vqty    = fg-bin.qty
                            .
               END. /* FOR EACH fg-bin */

        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */



    IF prmAction = "search" then do:
     if prmField = "itemno"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH itemfg WHERE  itemfg.company = prmComp AND itemfg.i-no = prmText NO-LOCK:
                 FOR EACH  fg-bin WHERE  fg-bin.company = prmComp
                     AND fg-bin.i-no = itemfg.i-no  and fg-bin.qty <> 0 NO-LOCK :
                      FIND FIRST ttFgAbjLook WHERE  ttFgAbjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
           IF AVAIL ttFgAbjLook THEN NEXT.
                 create ttFgAbjLook.
                 assign                                     
                     ttFgAbjLook.vItem    = itemfg.i-no
                     ttFgAbjLook.vName    = itemfg.i-name
                     ttFgAbjLook.vPartno  = itemfg.part-no
                     ttFgAbjLook.vJob1   = fg-bin.job-no        
                     ttFgAbjLook.vJob2   = fg-bin.job-no2   
                     ttFgAbjLook.vLoc    = fg-bin.loc 
                     ttFgAbjLook.vLocbin = fg-bin.loc-bin    
                     ttFgAbjLook.vtag    = fg-bin.tag  
                     ttFgAbjLook.vcust   = fg-bin.cust-no 
                     ttFgAbjLook.vqty    = fg-bin.qty
                            .

                 END. /* FOR EACH fg-bin */

        END.  /*FOR EACH Itemfg*/

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH itemfg WHERE  itemfg.company = prmComp AND itemfg.i-no BEGINS  prmText NO-LOCK:
                  FOR EACH  fg-bin WHERE  fg-bin.company = prmComp
                     AND fg-bin.i-no = itemfg.i-no  and fg-bin.qty <> 0 NO-LOCK :
                       FIND FIRST ttFgAbjLook WHERE  ttFgAbjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
                        IF AVAIL ttFgAbjLook THEN NEXT.
                 create ttFgAbjLook.
                 assign                                     
                     ttFgAbjLook.vItem    = itemfg.i-no
                     ttFgAbjLook.vName    = itemfg.i-name
                     ttFgAbjLook.vPartno  = itemfg.part-no
                     ttFgAbjLook.vJob1   = fg-bin.job-no        
                     ttFgAbjLook.vJob2   = fg-bin.job-no2   
                     ttFgAbjLook.vLoc    = fg-bin.loc 
                     ttFgAbjLook.vLocbin = fg-bin.loc-bin    
                     ttFgAbjLook.vtag    = fg-bin.tag  
                     ttFgAbjLook.vcust   = fg-bin.cust-no 
                     ttFgAbjLook.vqty    = fg-bin.qty
                            .

                 END. /* FOR EACH fg-bin */

        END.  /*FOR EACH Itemfg*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
/**************************************************************/

          if prmField = "partno"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH itemfg WHERE  itemfg.company = prmComp AND itemfg.part-no = prmText NO-LOCK:
                  FOR EACH  fg-bin WHERE  fg-bin.company = prmComp
                     AND fg-bin.i-no = itemfg.i-no  and fg-bin.qty <> 0 NO-LOCK :
                       FIND FIRST ttFgAbjLook WHERE  ttFgAbjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
                        IF AVAIL ttFgAbjLook THEN NEXT.
                 create ttFgAbjLook.
                 assign                                     
                     ttFgAbjLook.vItem    = itemfg.i-no
                     ttFgAbjLook.vName    = itemfg.i-name
                     ttFgAbjLook.vPartno  = itemfg.part-no
                     ttFgAbjLook.vJob1   = fg-bin.job-no        
                     ttFgAbjLook.vJob2   = fg-bin.job-no2   
                     ttFgAbjLook.vLoc    = fg-bin.loc 
                     ttFgAbjLook.vLocbin = fg-bin.loc-bin    
                     ttFgAbjLook.vtag    = fg-bin.tag  
                     ttFgAbjLook.vcust   = fg-bin.cust-no 
                     ttFgAbjLook.vqty    = fg-bin.qty
                            .

                 END. /* FOR EACH fg-bin */

        END.  /*FOR EACH Itemfg*/

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH itemfg WHERE  itemfg.company = prmComp AND itemfg.part-no BEGINS  prmText NO-LOCK:
                  FOR EACH  fg-bin WHERE  fg-bin.company = prmComp
                     AND fg-bin.i-no = itemfg.i-no  and fg-bin.qty <> 0 NO-LOCK :
                       FIND FIRST ttFgAbjLook WHERE  ttFgAbjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
                        IF AVAIL ttFgAbjLook THEN NEXT.
                 create ttFgAbjLook.
                 assign                                     
                     ttFgAbjLook.vItem    = itemfg.i-no
                     ttFgAbjLook.vName    = itemfg.i-name
                     ttFgAbjLook.vPartno  = itemfg.part-no
                     ttFgAbjLook.vJob1   = fg-bin.job-no        
                     ttFgAbjLook.vJob2   = fg-bin.job-no2   
                     ttFgAbjLook.vLoc    = fg-bin.loc 
                     ttFgAbjLook.vLocbin = fg-bin.loc-bin    
                     ttFgAbjLook.vtag    = fg-bin.tag  
                     ttFgAbjLook.vcust   = fg-bin.cust-no 
                     ttFgAbjLook.vqty    = fg-bin.qty
                            .

                 END.    /* FOR EACH fg-bin */

        END.  /*FOR EACH Itemfg*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */
