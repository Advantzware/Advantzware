
/*------------------------------------------------------------------------
    File        : CategoryLook.p
    Purpose     : CategoryLook

    Syntax      :

    Description : Return a Dataset of all CategoryLook

    Author(s)   : Kuldeep
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCategoryLook NO-UNDO 
    FIELD vProcat LIKE procat.procat
    FIELD vDscr  LIKE fgcat.dscr
    .


DEFINE DATASET dsCategoryLook FOR ttCategoryLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCategoryLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    
if prmAction <> "search" then do:
    FOR EACH fgcat WHERE  fgcat.company eq prmComp NO-LOCK:
        create ttCategoryLook.
            assign                                         
                ttCategoryLook.vProcat        = fgcat.procat
                ttCategoryLook.vDscr    = fgcat.dscr
                .
    END.	 /* FOR EACH procat */
    
END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH fgcat where  fgcat.company eq prmComp no-lock:

                IF fgcat.procat = prmText OR fgcat.dscr = prmText THEN
                DO:
                   create ttCategoryLook.
                   assign
                       ttCategoryLook.vProcat  = fgcat.procat
                       ttCategoryLook.vDscr    = fgcat.dscr.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH fgcat WHERE fgcat.company eq prmComp no-lock:

                IF fgcat.procat BEGINS prmText OR fgcat.dscr begins prmText THEN
                DO:
                   create ttCategoryLook.
                   assign
                      ttCategoryLook.vProcat  = fgcat.procat
                      ttCategoryLook.vDscr    = fgcat.dscr.
                END.
             END.  /*FOR EACH procat where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "procat"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH fgcat WHERE fgcat.company eq prmComp AND fgcat.procat = prmText no-lock:
                create ttCategoryLook.
                assign 
                    ttCategoryLook.vProcat        = fgcat.procat
                ttCategoryLook.vDscr    = fgcat.dscr

                    .
            end. /*FOR EACH procat where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH fgcat WHERE fgcat.procat BEGINS prmText AND fgcat.company EQ prmComp no-lock:
                create ttCategoryLook.
                assign
                    ttCategoryLook.vProcat        = fgcat.procat
                ttCategoryLook.vDscr    = fgcat.dscr
                
                    
                    .
            end.  /*FOR EACH procat wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
    IF prmField = "dscr" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH fgcat WHERE fgcat.company eq prmComp AND fgcat.dscr = prmText no-lock:
                create ttCategoryLook.
                assign
                    ttCategoryLook.vProcat        = fgcat.procat
                ttCategoryLook.vDscr    = fgcat.dscr
                
                    
                    .
            END. /*FOR EACH procat where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH fgcat  WHERE fgcat.company EQ prmComp AND fgcat.dscr begins prmText no-lock:
                create ttCategoryLook.
                assign 
                    ttCategoryLook.vProcat        = fgcat.procat
                ttCategoryLook.vDscr    = fgcat.dscr
                
                    
                    .
            END. /*FOR EACH procat where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = dscr */
   
    
END.  /* IF prmAction = search then do: */

