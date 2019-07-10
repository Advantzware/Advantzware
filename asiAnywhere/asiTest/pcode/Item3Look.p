


/*------------------------------------------------------------------------
    File        : Item3Look.p
    Purpose     : item

    Syntax      :

    Description : Return a Dataset of all item

    Author(s)   : Jyoti
    Created     : dec 04 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttMainItem3Look NO-UNDO 
    FIELD itemno       LIKE item.i-no
    FIELD iname  LIKE item.i-name
    FIELD idscr   LIKE item.i-dscr
    FIELD retname    LIKE item.i-name .

DEFINE DATASET dsMainItem3Look FOR ttMainItem3Look.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMainItem3Look .
       
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
    FOR EACH item WHERE  item.company eq prmComp  NO-LOCK :
        create ttMainItem3Look.
            assign                                         
               ttMainItem3Look.itemno   =  item.i-no  
               ttMainItem3Look.iname    =  item.i-name                         
               ttMainItem3Look.idscr    =  item.i-dscr
               ttMainItem3Look.retname  =  SUBSTR(ITEM.i-name,1,5)  
                 .
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
             FOR EACH item where  item.company eq prmComp  AND item.i-no = prmText NO-LOCK BY item.i-no:
                create ttMainItem3Look.
                assign                                         
               ttMainItem3Look.itemno   =  item.i-no  
               ttMainItem3Look.iname    =  item.i-name                         
               ttMainItem3Look.idscr    =  item.i-dscr
               ttMainItem3Look.retname  =  SUBSTR(ITEM.i-name,1,5)  
                 .
            END. /*FOR EACH item where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
             FOR EACH item where  item.company EQ prmComp  AND item.i-no BEGINS prmText NO-LOCK BY item.i-no:
                create ttMainItem3Look.
                 assign    
                     ttMainItem3Look.itemno   =  item.i-no  
                     ttMainItem3Look.iname    =  item.i-name                         
                     ttMainItem3Look.idscr    =  item.i-dscr
                     ttMainItem3Look.retname  =  SUBSTR(ITEM.i-name,1,5)  
                     .
                             end.  /*FOR EACH item wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       
     IF prmField = "i-name" then do:
             if prmCondition = "EQUAL" then do:
                 FOR EACH item where  item.company eq prmComp  AND item.i-name = prmText NO-LOCK BY item.i-no:
                     create ttMainItem3Look.
                     assign
                        ttMainItem3Look.itemno   =  item.i-no  
                        ttMainItem3Look.iname    =  item.i-name                         
                         ttMainItem3Look.idscr    =  item.i-dscr
                        ttMainItem3Look.retname  =  SUBSTR(ITEM.i-name,1,5)  
                           .
                 END. /*FOR EACH item where*/
             END. /*if prmCondition = EQUAL*/
             IF prmCondition = "BEGIN" then do:
                 FOR EACH item where  item.company EQ prmComp  AND item.i-name BEGINS prmText NO-LOCK BY item.i-no:
                     create ttMainItem3Look.
                     assign 
                         ttMainItem3Look.itemno   =  item.i-no  
                        ttMainItem3Look.iname    =  item.i-name                         
                        ttMainItem3Look.idscr    =  item.i-dscr
                        ttMainItem3Look.retname  =  SUBSTR(ITEM.i-name,1,5) 
                        
                         .
                 END. /*FOR EACH item where*/
             END.  /*if prmCondition = BEGIN*/
         END.  /*IF prmField = stock-no */

    
END.  /* IF prmAction = search then do: */


