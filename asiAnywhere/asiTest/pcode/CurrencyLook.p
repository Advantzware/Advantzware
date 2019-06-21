



/*------------------------------------------------------------------------
    File        : CurrencyLook.p
    Purpose     : CurrencyCode

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : praveen sharma
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCurrencyCodeLook NO-UNDO 
    FIELD vcode AS CHARACTER
    FIELD vdesc AS CHARACTER
    FIELD vex-rate AS DECIMAL.
   
    
                                          
    
DEFINE DATASET dsCurrencyCodeLook FOR ttCurrencyCodeLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCurrencyCodeLook.
       
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
        
    FOR EACH currency NO-LOCK:
                 create ttCurrencyCodeLook.
                 assign                                     
                    ttCurrencyCodeLook.vcode      = currency.c-code
                   ttCurrencyCodeLook.vdesc       = currency.c-desc
                   ttCurrencyCodeLook.vex-rate    = currency.ex-rate .
                  
                   
        END.  /*FOR EACH Currency*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        
            IF prmCondition = "EQUAL" then do:
                FOR EACH currency  NO-LOCK:
                    IF currency.c-code = prmText OR currency.c-desc = prmText  THEN 
                         
                       
                        DO:
                        create ttCurrencyCodeLook.
                        assign                                     
                            ttCurrencyCodeLook.vcode       = currency.c-code
                            ttCurrencyCodeLook.vdesc       = currency.c-desc
                            ttCurrencyCodeLook.vex-rate    = currency.ex-rate .
                        END.
              

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH currency NO-LOCK :
                 IF currency.c-code = prmText OR currency.c-desc = prmText   THEN
                    DO:
                    create ttCurrencyCodeLook.
                    assign                                     
                        ttCurrencyCodeLook.vcode       = currency.c-code
                        ttCurrencyCodeLook.vdesc       = currency.c-desc
                        ttCurrencyCodeLook.vex-rate    = currency.ex-rate.
                END.
            END.  /*FOR EACH currency where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "c-code"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH currency WHERE currency.c-code  = prmText  NO-LOCK :
                 create ttCurrencyCodeLook.
                 assign                                     
                   ttCurrencyCodeLook.vcode        = currency.c-code
                   ttCurrencyCodeLook.vdesc        = currency.c-desc
                   ttCurrencyCodeLook.vex-rate     = currency.ex-rate.
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH currency WHERE currency.c-code BEGINS prmText NO-LOCK :
                      create ttCurrencyCodeLook.
                      assign   
                   ttCurrencyCodeLook.vcode           = currency.c-code
                   ttCurrencyCodeLook.vdesc           = currency.c-desc
                   ttCurrencyCodeLook.vex-rate        = currency.ex-rate.
                  end.  
            end.    /*if prmCondition = BEGIN*/    
         end.  
           
END.  


