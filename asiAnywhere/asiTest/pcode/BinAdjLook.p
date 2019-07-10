



/*------------------------------------------------------------------------
    File        :BinAdjLook.p
    Purpose     : fg-bin

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBinAdjLook 
    FIELD vItem   AS CHARACTER
    FIELD vName    AS CHARACTER
    FIELD vJob1   AS CHARACTER
    FIELD vJob2   AS INTEGER
    FIELD vLoc    AS CHARACTER
    FIELD vLocbin AS CHARACTER
    FIELD vtag    AS CHARACTER
    FIELD vcust   AS CHARACTER
    FIELD vqty    AS DECIMAL
    .
                                           
    
DEFINE DATASET dsBinAdjLook FOR ttBinAdjLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBinAdjLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmItem      = ? THEN ASSIGN prmItem      ="".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
  
if prmAction <> "search" then do:
        
    FOR EACH itemfg WHERE itemfg.company = prmComp NO-LOCK:
    
       FOR EACH  fg-bin WHERE  fg-bin.company = prmComp
                AND fg-bin.i-no = itemfg.i-no  and fg-bin.qty <> 0 NO-LOCK :
           FIND FIRST ttBinAdjLook WHERE  ttBinAdjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
           IF AVAIL ttBinAdjLook THEN NEXT.
          create ttBinAdjLook.
                 assign                                     
                     ttBinAdjLook.vItem   = itemfg.i-no
                     ttBinAdjLook.vName   = itemfg.i-name
                     ttBinAdjLook.vJob1   = fg-bin.job-no        
                     ttBinAdjLook.vJob2   = fg-bin.job-no2   
                     ttBinAdjLook.vLoc    = fg-bin.loc 
                     ttBinAdjLook.vLocbin = fg-bin.loc-bin    
                     ttBinAdjLook.vtag    = fg-bin.tag  
                     ttBinAdjLook.vcust   = fg-bin.cust-no 
                     ttBinAdjLook.vqty    = fg-bin.qty
                            .
               END. /* FOR EACH fg-bin */
               END.
END.  /*ifif prmAction <> "search" */



    IF prmAction = "search" then do:
     if prmField = "jobno"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH  fg-bin WHERE  fg-bin.company = prmComp AND fg-bin.job-no = prmText 
                     AND fg-bin.i-no = prmItem  and fg-bin.qty <> 0 NO-LOCK :
                 FIND FIRST ttBinAdjLook WHERE  ttBinAdjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
           IF AVAIL ttBinAdjLook THEN NEXT.
                 create ttBinAdjLook.
                 assign                                     
                     ttBinAdjLook.vItem   = itemfg.i-no
                      ttBinAdjLook.vName   = itemfg.i-name
                     ttBinAdjLook.vJob1   = fg-bin.job-no        
                     ttBinAdjLook.vJob2   = fg-bin.job-no2   
                     ttBinAdjLook.vLoc    = fg-bin.loc 
                     ttBinAdjLook.vLocbin = fg-bin.loc-bin    
                     ttBinAdjLook.vtag    = fg-bin.tag  
                     ttBinAdjLook.vcust   = fg-bin.cust-no 
                     ttBinAdjLook.vqty    = fg-bin.qty
                            .
               END. /* FOR EACH fg-bin */

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH  fg-bin WHERE  fg-bin.company = prmComp AND fg-bin.job-no BEGINS prmText 
                     AND fg-bin.i-no = prmItem  and fg-bin.qty <> 0 NO-LOCK :
                      FIND FIRST ttBinAdjLook WHERE  ttBinAdjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
           IF AVAIL ttBinAdjLook THEN NEXT.
                 create ttBinAdjLook.
                 assign                                     
                     ttBinAdjLook.vItem   = itemfg.i-no
                      ttBinAdjLook.vName   = itemfg.i-name
                     ttBinAdjLook.vJob1   = fg-bin.job-no        
                     ttBinAdjLook.vJob2   = fg-bin.job-no2   
                     ttBinAdjLook.vLoc    = fg-bin.loc 
                     ttBinAdjLook.vLocbin = fg-bin.loc-bin    
                     ttBinAdjLook.vtag    = fg-bin.tag  
                     ttBinAdjLook.vcust   = fg-bin.cust-no 
                     ttBinAdjLook.vqty    = fg-bin.qty
                            .
               END. /* FOR EACH fg-bin */

            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
/**************************************************************/

          if prmField = "loc"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH  fg-bin WHERE  fg-bin.company = prmComp AND fg-bin.loc = prmText 
                     AND fg-bin.i-no = prmItem  and fg-bin.qty <> 0 NO-LOCK :
                 FIND FIRST ttBinAdjLook WHERE  ttBinAdjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
           IF AVAIL ttBinAdjLook THEN NEXT.
                 create ttBinAdjLook.
                 assign                                     
                     ttBinAdjLook.vItem   = itemfg.i-no
                      ttBinAdjLook.vName   = itemfg.i-name
                     ttBinAdjLook.vJob1   = fg-bin.job-no        
                     ttBinAdjLook.vJob2   = fg-bin.job-no2   
                     ttBinAdjLook.vLoc    = fg-bin.loc 
                     ttBinAdjLook.vLocbin = fg-bin.loc-bin    
                     ttBinAdjLook.vtag    = fg-bin.tag  
                     ttBinAdjLook.vcust   = fg-bin.cust-no 
                     ttBinAdjLook.vqty    = fg-bin.qty
                            .
               END. /* FOR EACH fg-bin */

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH  fg-bin WHERE  fg-bin.company = prmComp AND fg-bin.job-no begins prmText 
                     AND fg-bin.i-no = prmItem  and fg-bin.qty <> 0 NO-LOCK :
                      FIND FIRST ttBinAdjLook WHERE  ttBinAdjLook.vItem = itemfg.i-no NO-LOCK NO-ERROR.
           IF AVAIL ttBinAdjLook THEN NEXT.
                 create ttBinAdjLook.
                 assign                                     
                     ttBinAdjLook.vItem   = itemfg.i-no
                      ttBinAdjLook.vName   = itemfg.i-name
                     ttBinAdjLook.vJob1   = fg-bin.job-no        
                     ttBinAdjLook.vJob2   = fg-bin.job-no2   
                     ttBinAdjLook.vLoc    = fg-bin.loc 
                     ttBinAdjLook.vLocbin = fg-bin.loc-bin    
                     ttBinAdjLook.vtag    = fg-bin.tag  
                     ttBinAdjLook.vcust   = fg-bin.cust-no 
                     ttBinAdjLook.vqty    = fg-bin.qty
                            .
               END. /* FOR EACH fg-bin */

            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */
