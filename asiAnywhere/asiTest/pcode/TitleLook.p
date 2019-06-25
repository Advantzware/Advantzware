

/*------------------------------------------------------------------------
    File        : TitleLook.p
    Purpose     :
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTitleLook NO-UNDO 
    FIELD vCode     AS CHARACTER
    FIELD vDscr     AS CHARACTER
    .
                                           
    
DEFINE DATASET dsTitleLook FOR ttTitleLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTitleLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
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
    FOR EACH titlcode NO-LOCK:
           create ttTitleLook.
                 assign                                     
                    ttTitleLook.vCode = titlcode.titlcode
                    ttTitleLook.vDscr = titlcode.description 
                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH titlcode NO-LOCK:
                    IF (titlcode.titlcode = prmText OR titlcode.description = prmText) THEN
                        DO:
                        create ttTitleLook.
                        assign                                     
                            ttTitleLook.vCode = titlcode.titlcode  
                            ttTitleLook.vDscr = titlcode.description.
                    END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH titlcode NO-LOCK:
                IF (titlcode.titlcode BEGINS prmText OR titlcode.description BEGINS prmText) THEN
                    DO:
                    create ttTitleLook.
                    assign                                     
                        ttTitleLook.vCode = titlcode.titlcode  
                        ttTitleLook.vDscr = titlcode.description
                        .
                    END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "titlcode"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH titlcode WHERE (titlcode.titlcode = prmText OR titlcode.description = prmText) NO-LOCK :
                 create ttTitleLook.
                 assign                                     
                     ttTitleLook.vCode = titlcode.titlcode  
                     ttTitleLook.vDscr = titlcode.description. 
             END.

          END. /*FOR EACH titlcode*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH titlcode WHERE (titlcode.titlcode BEGINS prmText OR titlcode.description BEGINS prmText) NO-LOCK :
                      create ttTitleLook.
                      assign                                     
                          ttTitleLook.vCode = titlcode.titlcode  
                          ttTitleLook.vDscr = titlcode.description. 
                  end.  /*FOR EACH titlcode wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = titlcode  */
           
END.  /* IF prmAction = search then do: */



