



/*------------------------------------------------------------------------
    File        : PackingLook.p
    Purpose     : Packing Code

    Syntax      :

    Description : Return a Dataset of Quote Request

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPackingLook NO-UNDO 
    FIELD Ino         LIKE item.i-no
    FIELD Iname       LIKE item.i-name
    FIELD Idscr       LIKE item.i-dscr
    FIELD BoxCase    AS INTEGER
    FIELD CaseLen    AS DECIMAL
    FIELD CaseWid    AS DECIMAL
    FIELD CaseDep    AS DECIMAL
    FIELD CasePal    AS INTEGER
    FIELD Avgw       AS INTEGER
    FIELD CaseL      AS DECIMAL
    FIELD CaseW      AS DECIMAL
    FIELD CaseD      AS DECIMAL
    .

DEFINE DATASET dsPackingLook FOR ttPackingLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER MatType   AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPackingLook.
       
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
    FOR EACH item WHERE  item.company eq prmComp AND item.mat-type = MatType  NO-LOCK BY item.i-no:
        create ttPackingLook.
            assign                                         
               ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w   
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d     
                                                       
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
                   create ttPackingLook.
                   assign
                      ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w      
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d                                         
                 .

                END.
                
            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH item where  item.company eq prmComp  AND ITEM.mat-type = MatType NO-LOCK BY item.i-no:

                IF (item.i-no BEGINS prmText OR item.i-name BEGINS prmText OR 
                    item.press-type BEGINS prmText) THEN
                DO:
                   create ttPackingLook.
                   assign
                       ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w      
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d                                         
                 .

                END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
             FOR EACH item where  item.company eq prmComp AND ITEM.mat-type = MatType AND item.i-no = prmText NO-LOCK BY item.i-no:
                create ttPackingLook.
                assign 
                    ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w      
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d                                         
                 .
            END. /*FOR EACH item where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
             FOR EACH item where  item.company EQ prmComp AND ITEM.mat-type = MatType AND item.i-no BEGINS prmText NO-LOCK BY item.i-no:
                create ttPackingLook.
                assign
                   ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w      
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d                                        
                 .
            end.  /*FOR EACH item wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "press-type" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH item where  item.company eq prmComp AND ITEM.mat-type = MatType AND item.press-type = prmText NO-LOCK BY item.i-no:
                 create ttPackingLook.
                 assign
                    ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w      
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d                                         
                 .

             END. /*FOR EACH item where*/
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
             FOR EACH item where  item.company EQ prmComp AND ITEM.mat-type = MatType AND item.press-type BEGINS prmText NO-LOCK BY item.i-no:
                 create ttPackingLook.
                 assign 
                     ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w      
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d                                        
                 .

             END. /*FOR EACH item where*/
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = i-no */
     IF prmField = "i-name" then do:
             if prmCondition = "EQUAL" then do:
                 FOR EACH item where  item.company eq prmComp AND ITEM.mat-type = MatType AND item.i-name = prmText NO-LOCK BY item.i-no:
                     create ttPackingLook.
                     assign
                         ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w      
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d                                        
                 .

                 END. /*FOR EACH item where*/
             END. /*if prmCondition = EQUAL*/
             IF prmCondition = "BEGIN" then do:
                 FOR EACH item where  item.company EQ prmComp AND ITEM.mat-type = MatType AND item.i-name BEGINS prmText NO-LOCK BY item.i-no:
                     create ttPackingLook.
                     assign 
                         ttPackingLook.Ino        =  item.i-no  
               ttPackingLook.Iname      =  item.i-name                         
               ttPackingLook.Idscr      =  item.i-dscr
               ttPackingLook.BoxCase    =  item.box-case  
               ttPackingLook.CaseLen    =  item.case-l                             
               ttPackingLook.CaseWid    =  item.case-w    
               ttPackingLook.CaseDep    =  item.case-d    
               ttPackingLook.CasePal    =  item.case-pall 
               ttPackingLook.Avgw       =  item.avg-w      
               ttPackingLook.CaseL      =  item.case-l          
               ttPackingLook.CaseW      =  item.case-w   
               ttPackingLook.CaseD      =  item.case-d                                        
                 .

                 END. /*FOR EACH item where*/
             END.  /*if prmCondition = BEGIN*/
         END.  /*IF prmField = stock-no */

    
END.  /* IF prmAction = search then do: */



