



/*------------------------------------------------------------------------
    File        : TermsLook.p
    Purpose     : Terms Code

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTermsLook NO-UNDO 
    FIELD vcode AS CHARACTER
    FIELD vdscr AS CHARACTER
   
    .
                                           
    
DEFINE DATASET dstcodeLook FOR ttTermsLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dstcodeLook.
       
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
        
    FOR EACH terms NO-LOCK:
                 create ttTermsLook.
                 assign                                     
                    ttTermsLook.vcode      = terms.t-code
                    ttTermsLook.vdscr      = terms.dscr
                   

                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH terms  NO-LOCK:
                    IF (terms.t-code = prmText ) THEN
                        DO:
                        create ttTermsLook.
                        assign                                     
                            ttTermsLook.vcode      = terms.t-code
                            ttTermsLook.vdscr      = terms.dscr
                             .
                        END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH terms NO-LOCK :
                IF (terms.t-code BEGINS prmText ) THEN
                    DO:
                    create ttTermsLook.
                    assign                                     
                       ttTermsLook.vcode      = terms.t-code
                       ttTermsLook.vdscr      = terms.dscr
                     . 
                END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "code"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH terms WHERE  terms.t-code = prmText  NO-LOCK :
                 create ttTermsLook.
                 assign                                     
                     ttTermsLook.vcode      = terms.t-code
                     ttTermsLook.vdscr      = terms.dscr  .
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH terms WHERE terms.t-code BEGINS prmText NO-LOCK :
                      create ttTermsLook.
                      assign   
                           ttTermsLook.vcode      = terms.t-code
                     ttTermsLook.vdscr      = terms.dscr  .

                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */



