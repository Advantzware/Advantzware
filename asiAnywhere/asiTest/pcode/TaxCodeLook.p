




/*------------------------------------------------------------------------
    File        : TaxCodeLook.p
    Purpose     : Tax Code

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTaxCodeLook NO-UNDO 
    FIELD vgroup    AS CHARACTER
    FIELD vtaxcode  AS CHARACTER 
    FIELD vdscr     AS CHARACTER
    FIELD vtaxcode1 AS CHARACTER
    FIELD vtaxcode2 AS CHARACTER
    .
                                           
    
DEFINE DATASET dstaxcodeLook FOR ttTaxCodeLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dstaxcodeLook.
       
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
        
    FOR EACH stax NO-LOCK:
                 create ttTaxCodeLook.
                 assign                                     
                    ttTaxCodeLook.vgroup      = stax.tax-group
                    ttTaxCodeLook.vtaxcode    = stax.tax-code[1]
                    ttTaxCodeLook.vdscr       = stax.tax-dscr[1]
                    ttTaxCodeLook.vtaxcode1   = stax.tax-code1[2]
                    ttTaxCodeLook.vtaxcode2   = stax.tax-code1[3]
                   
                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH stax  NO-LOCK:
                    IF (stax.tax-group = prmText OR stax.tax-code[1] = prmText ) THEN
                        DO:
                        create ttTaxCodeLook.
                        assign                                     
                            ttTaxCodeLook.vgroup      = stax.tax-group
                    ttTaxCodeLook.vtaxcode    = stax.tax-code[1]
                    ttTaxCodeLook.vdscr       = stax.tax-dscr[1]
                    ttTaxCodeLook.vtaxcode1   = stax.tax-code1[2]
                    ttTaxCodeLook.vtaxcode2   = stax.tax-code1[3] .
                        END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH stax NO-LOCK :
                IF (stax.tax-group BEGINS prmText OR stax.tax-code[1] BEGINS prmText  ) THEN
                   
                    DO:
                    create ttTaxCodeLook.
                    assign                                     
                          ttTaxCodeLook.vgroup      = stax.tax-group
                    ttTaxCodeLook.vtaxcode    = stax.tax-code[1]
                    ttTaxCodeLook.vdscr       = stax.tax-dscr[1]
                    ttTaxCodeLook.vtaxcode1   = stax.tax-code1[2]
                    ttTaxCodeLook.vtaxcode2   = stax.tax-code1[3] .

                END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "group"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH stax WHERE  stax.tax-group = prmText  NO-LOCK :
                 create ttTaxCodeLook.
                 assign                                     
                    ttTaxCodeLook.vgroup      = stax.tax-group
                    ttTaxCodeLook.vtaxcode    = stax.tax-code[1]
                    ttTaxCodeLook.vdscr       = stax.tax-dscr[1]
                    ttTaxCodeLook.vtaxcode1   = stax.tax-code1[2]
                    ttTaxCodeLook.vtaxcode2   = stax.tax-code1[3] .

             END.
         END.
         if prmCondition = "BEGIN" then do:
             FOR EACH stax WHERE  stax.tax-group Begins prmText  NO-LOCK :
                 create ttTaxCodeLook.
                 assign                                     
                    ttTaxCodeLook.vgroup      = stax.tax-group
                    ttTaxCodeLook.vtaxcode    = stax.tax-code[1]
                    ttTaxCodeLook.vdscr       = stax.tax-dscr[1]
                    ttTaxCodeLook.vtaxcode1   = stax.tax-code1[2]
                    ttTaxCodeLook.vtaxcode2   = stax.tax-code1[3] .

             END.
         END.

         END. /*FOR EACH state*/

             if prmField = "code"  then do:
                if prmCondition = "EQUAL" then do:
                     FOR EACH stax WHERE  stax.tax-code[1] = prmText  NO-LOCK :
                          create ttTaxCodeLook.
                            assign                                     
                               ttTaxCodeLook.vgroup      = stax.tax-group
                               ttTaxCodeLook.vtaxcode    = stax.tax-code[1]
                               ttTaxCodeLook.vdscr       = stax.tax-dscr[1]
                               ttTaxCodeLook.vtaxcode1   = stax.tax-code1[2]
                               ttTaxCodeLook.vtaxcode2   = stax.tax-code1[3] .

             END.


          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH stax WHERE stax.tax-code[1] BEGINS prmText NO-LOCK :
                      create ttTaxCodeLook.
                      assign   
                            ttTaxCodeLook.vgroup      = stax.tax-group
                    ttTaxCodeLook.vtaxcode    = stax.tax-code[1]
                    ttTaxCodeLook.vdscr       = stax.tax-dscr[1]
                    ttTaxCodeLook.vtaxcode1   = stax.tax-code1[2]
                    ttTaxCodeLook.vtaxcode2   = stax.tax-code1[3] .
                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */


