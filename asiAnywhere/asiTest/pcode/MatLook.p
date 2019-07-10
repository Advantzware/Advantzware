


/*------------------------------------------------------------------------
    File        : MatLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all MatLook

    Author(s)   : Jyoti
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttMatLook NO-UNDO 
    FIELD Ino       LIKE item.i-no
    FIELD Iname  LIKE item.i-name
    FIELD Idscr   LIKE item.i-dscr
    FIELD PressType    LIKE item.press-type.

DEFINE DATASET dsMatLook FOR ttMatLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER MatType   AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMatLook.
       
IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    
if prmAction <> "search" then do:
    FOR EACH item WHERE  item.company eq prmComp AND item.mat-type = MatType NO-LOCK BY item.i-no:
        create ttMatLook.
            assign                                         
               ttMatLook.Ino       =  item.i-no  
               ttMatLook.Iname    =  item.i-name                         
               ttMatLook.Idscr      =  item.i-dscr
               ttMatLook.PressType         =  item.press-type  
                 .
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH item where  item.company eq prmComp AND ITEM.mat-type = MatType NO-LOCK BY item.i-no:
                
                IF (item.i-no = prmText OR item.i-name = prmText OR 
                    item.press-type = prmText ) THEN
                DO:
                   create ttMatLook.
                   assign
                       ttMatLook.Ino     =  item.i-no  
                       ttMatLook.Iname   =  item.i-name
                       ttMatLook.Idscr    =  item.i-dscr                        
                       ttMatLook.PressType       =  item.press-type.
                END.
                
            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH item where  item.company eq prmComp  AND ITEM.mat-type = MatType NO-LOCK BY item.i-no:

                IF (item.i-no BEGINS prmText OR item.i-name BEGINS prmText OR 
                    item.press-type BEGINS prmText) THEN
                DO:
                   create ttMatLook.
                   assign
                       ttMatLook.Ino       =  item.i-no  
                       ttMatLook.Iname     =  item.i-name
                       ttMatLook.Idscr      =  item.i-dscr
                       ttMatLook.PressType  =  item.press-type.
                END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
             FOR EACH item where  item.company eq prmComp AND ITEM.mat-type = MatType AND item.i-no = prmText NO-LOCK BY item.i-no:
                create ttMatLook.
                assign 
                    ttMatLook.Ino       =  item.i-no  
                    ttMatLook.Iname     =  item.i-name  
                    ttMatLook.Idscr      =  item.i-dscr                        
                    ttMatLook.PressType         =  item.press-type  
                      .
            END. /*FOR EACH item where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
             FOR EACH item where  item.company EQ prmComp AND ITEM.mat-type = MatType AND item.i-no BEGINS prmText NO-LOCK BY item.i-no:
                create ttMatLook.
                assign
                    ttMatLook.Ino       =  item.i-no  
                    ttMatLook.Iname     =  item.i-name  
                    ttMatLook.Idscr      =  item.i-dscr                       
                    ttMatLook.PressType         =  item.press-type  
                      .
            end.  /*FOR EACH item wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "press-type" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH item where  item.company eq prmComp AND ITEM.mat-type = MatType AND item.press-type = prmText NO-LOCK BY item.i-no:
                 create ttMatLook.
                 assign
                     ttMatLook.Ino       =  item.i-no  
                     ttMatLook.Iname     =  item.i-name  
                     ttMatLook.Idscr      =  item.i-dscr                         
                     ttMatLook.PressType         =  item.press-type  
                       .
             END. /*FOR EACH item where*/
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
             FOR EACH item where  item.company EQ prmComp AND ITEM.mat-type = MatType AND item.press-type BEGINS prmText NO-LOCK BY item.i-no:
                 create ttMatLook.
                 assign 
                     ttMatLook.Ino       =  item.i-no  
                     ttMatLook.Iname     =  item.i-name  
                     ttMatLook.Idscr      =  item.i-dscr                        
                     ttMatLook.PressType         =  item.press-type  
                       .
                     .
             END. /*FOR EACH item where*/
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = i-no */
     IF prmField = "i-name" then do:
             if prmCondition = "EQUAL" then do:
                 FOR EACH item where  item.company eq prmComp AND ITEM.mat-type = MatType AND item.i-name = prmText NO-LOCK BY item.i-no:
                     create ttMatLook.
                     assign
                         ttMatLook.Ino       =  item.i-no  
                         ttMatLook.Iname     =  item.i-name  
                         ttMatLook.Idscr      =  item.i-dscr                         
                         ttMatLook.PressType         =  item.press-type  
                           .
                 END. /*FOR EACH item where*/
             END. /*if prmCondition = EQUAL*/
             IF prmCondition = "BEGIN" then do:
                 FOR EACH item where  item.company EQ prmComp AND ITEM.mat-type = MatType AND item.i-name BEGINS prmText NO-LOCK BY item.i-no:
                     create ttMatLook.
                     assign 
                         ttMatLook.Ino       =  item.i-no  
                         ttMatLook.Iname     =  item.i-name  
                         ttMatLook.Idscr      =  item.i-dscr                        
                         ttMatLook.PressType         =  item.press-type  
                           .
                         .
                 END. /*FOR EACH item where*/
             END.  /*if prmCondition = BEGIN*/
         END.  /*IF prmField = stock-no */

    
END.  /* IF prmAction = search then do: */


