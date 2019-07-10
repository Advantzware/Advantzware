



/*------------------------------------------------------------------------
    File        : PrintLook.p
    Purpose     : Return Code 

    Syntax      :

    Description : Return a Dataset of all PrintLook

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPrintLook NO-UNDO 
    FIELD Prinno       LIKE item.i-no
    FIELD PrintName  LIKE item.i-name
    FIELD InkType AS CHARACTER
    .
 


DEFINE DATASET dsPrintLook FOR ttPrintLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPrintLook.
       
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
    FOR EACH item WHERE  item.company eq prmComp NO-LOCK BY item.i-no:

        IF item.mat-type = "V" OR ITEM.mat-type = "I" THEN
        DO:
           create ttPrintLook.
           assign                                         
              ttPrintLook.Prinno       =  item.i-no  
              ttPrintLook.PrintName    =  item.i-name
              ttPrintLook.InkType    =  item.ink-type.
        END.
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH item where  item.company eq prmComp NO-LOCK BY item.i-no:

                IF (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                   (item.i-no = prmText OR item.i-name = prmText OR 
                    ITEM.press-type = prmText ) THEN
                DO:
                   create ttPrintLook.
                   assign
                       ttPrintLook.Prinno     =  item.i-no  
                       ttPrintLook.PrintName  =  item.i-name
                       ttPrintLook.InkType    =  item.ink-type.
                END. 
                
            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH item where  item.company eq prmComp NO-LOCK BY item.i-no:

                IF (item.mat-type = "V" OR ITEM.mat-type = "I") AND
                   (item.i-no BEGINS prmText OR ITEM.i-name BEGINS prmText OR 
                    item.press-type BEGINS prmText) THEN
                DO:
                   create ttPrintLook.
                   assign
                       ttPrintLook.Prinno       =  item.i-no  
                       ttPrintLook.PrintName     =  item.i-name
                        ttPrintLook.InkType    =  item.ink-type.
                END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
             FOR EACH item where item.company eq prmComp AND item.i-no = prmText NO-LOCK BY item.i-no:

                IF item.mat-type = "V" OR ITEM.mat-type = "I" THEN
                DO:
                   create ttPrintLook.
                   assign 
                       ttPrintLook.Prinno       =  item.i-no  
                       ttPrintLook.PrintName     =  item.i-name
                        ttPrintLook.InkType    =  item.ink-type.
                END.
            END. /*FOR EACH item where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
             FOR EACH item where  item.company BEGINS prmComp AND item.i-no BEGINS prmText NO-LOCK BY item.i-no:

                IF item.mat-type = "V" OR ITEM.mat-type = "I" THEN
                DO:
                   create ttPrintLook.
                   assign
                       ttPrintLook.Prinno       =  item.i-no  
                       ttPrintLook.PrintName     =  item.i-name
                        ttPrintLook.InkType    =  item.ink-type.
                END.
            end.  /*FOR EACH item wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "press-type" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH item where  item.company eq prmComp AND item.press-type = prmText NO-LOCK BY item.i-no:

                 IF item.mat-type = "V" OR ITEM.mat-type = "I" THEN
                 DO:
                    create ttPrintLook.
                    assign
                        ttPrintLook.Prinno       =  item.i-no  
                        ttPrintLook.PrintName     =  item.i-name
                         ttPrintLook.InkType    =  item.ink-type.
                 END.
             END. /*FOR EACH item where*/
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
             FOR EACH item where  item.company EQ prmComp AND ITEM.mat-type = "V" AND item.press-type BEGINS prmText NO-LOCK BY item.i-no:
                 create ttPrintLook.
                 assign 
                     ttPrintLook.Prinno       =  item.i-no  
                     ttPrintLook.PrintName     =  item.i-name
                      ttPrintLook.InkType    =  item.ink-type
                                         .
             END. /*FOR EACH item where*/
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = i-no */
     IF prmField = "i-name" then do:
             if prmCondition = "EQUAL" then do:
                 FOR EACH item where  item.company eq prmComp AND item.i-name = prmText NO-LOCK BY item.i-no:

                     IF item.mat-type = "V" OR ITEM.mat-type = "I" THEN
                     DO:
                        create ttPrintLook.
                        assign
                            ttPrintLook.Prinno       =  item.i-no  
                            ttPrintLook.PrintName     =  item.i-name
                             ttPrintLook.InkType    =  item.ink-type.
                     END.
                 END. /*FOR EACH item where*/
             END. /*if prmCondition = EQUAL*/
             IF prmCondition = "BEGIN" then do:
                 FOR EACH item where  item.company BEGINS prmComp AND item.i-name BEGINS prmText NO-LOCK BY item.i-no:

                     IF (item.mat-type = "V" OR ITEM.mat-type = "I") THEN
                     DO:
                        create ttPrintLook.
                        assign 
                            ttPrintLook.Prinno       =  item.i-no  
                            ttPrintLook.PrintName     =  item.i-name
                             ttPrintLook.InkType    =  item.ink-type.
                     END.
                 END. /*FOR EACH item where*/
             END.  /*if prmCondition = BEGIN*/
         END.  /*IF prmField = stock-no */

    
END.  /* IF prmAction = search then do: */





