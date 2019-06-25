




/*------------------------------------------------------------------------
    File        : TaxGroupLook.p
    Purpose     : Tax Code

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTaxGroupLook NO-UNDO 
    FIELD vgroup     AS CHARACTER
    FIELD rate       AS CHARACTER
    FIELD taxfrt     AS CHAR
    FIELD taxacc     AS CHAR 
    FIELD vtaxcode1  AS CHARACTER 
    FIELD vtaxcode2  AS CHARACTER
    FIELD vtaxcode3  AS CHARACTER
    FIELD vtaxcode4  AS CHARACTER
    FIELD vtaxcode5  AS CHARACTER
    FIELD vdscr      AS CHARACTER
    FIELD vdscr2     AS CHARACTER
    FIELD vdscr3     AS CHARACTER
    FIELD vdscr4     AS CHARACTER
    FIELD vdscr5     AS CHARACTER
    .
                                           
    
DEFINE DATASET dsttTaxGroupLook FOR ttTaxGroupLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsttTaxGroupLook.
       
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
        
    FOR EACH stax WHERE stax.company = prmComp  AND 
        stax.tax-code1[1] EQ stax.tax-group AND
        stax.tax-group NE ""  NO-LOCK:
                 create ttTaxGroupLook.
                 assign                                     
                    ttTaxGroupLook.vgroup      = stax.tax-group
                    ttTaxGroupLook.vtaxcode1    = stax.tax-code1[1]
                    ttTaxGroupLook.vtaxcode2   = stax.tax-code1[2]
                    ttTaxGroupLook.vtaxcode3   = stax.tax-code1[3]
                    ttTaxGroupLook.vtaxcode4   = stax.tax-code1[4]
                    ttTaxGroupLook.vtaxcode5   = stax.tax-code1[5]
                    ttTaxGroupLook.rate   = string(stax.tax-rate1[1])
                    ttTaxGroupLook.taxfrt = string(stax.tax-frt1[1])
                    ttTaxGroupLook.taxacc = stax.tax-acc1[1]
                    ttTaxGroupLook.vdscr       = stax.tax-dscr1[1]
                    ttTaxGroupLook.vdscr2      = stax.tax-dscr1[2]
                    ttTaxGroupLook.vdscr3      = stax.tax-dscr1[3]
                    ttTaxGroupLook.vdscr4      = stax.tax-dscr1[4]
                    ttTaxGroupLook.vdscr5      = stax.tax-dscr1[5]

                   
                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH stax  WHERE stax.company = prmComp  AND 
                    stax.tax-code1[1] EQ stax.tax-group AND
                    stax.tax-group NE ""  NO-LOCK:
                    IF (stax.tax-group = prmText OR stax.tax-code[1] = prmText ) THEN
                        DO:
                        create ttTaxGroupLook.
                        assign                                     
                             ttTaxGroupLook.vgroup      = stax.tax-group
                    ttTaxGroupLook.vtaxcode1    = stax.tax-code1[1]
                    ttTaxGroupLook.vtaxcode2   = stax.tax-code1[2]
                    ttTaxGroupLook.vtaxcode3   = stax.tax-code1[3]
                    ttTaxGroupLook.vtaxcode4   = stax.tax-code1[4]
                    ttTaxGroupLook.vtaxcode5   = stax.tax-code1[5]
                    ttTaxGroupLook.rate   = string(stax.tax-rate1[1])
                    ttTaxGroupLook.taxfrt = string(stax.tax-frt1[1])
                    ttTaxGroupLook.taxacc = stax.tax-acc1[1]
                    ttTaxGroupLook.vdscr       = stax.tax-dscr1[1]
                    ttTaxGroupLook.vdscr2      = stax.tax-dscr1[2]
                    ttTaxGroupLook.vdscr3      = stax.tax-dscr1[3]
                    ttTaxGroupLook.vdscr4      = stax.tax-dscr1[4]
                    ttTaxGroupLook.vdscr5      = stax.tax-dscr1[5].
                        END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH stax  WHERE stax.company = prmComp  AND 
                stax.tax-code1[1] EQ stax.tax-group AND
                stax.tax-group NE "" NO-LOCK :
                IF (stax.tax-group BEGINS prmText OR stax.tax-code[1] BEGINS prmText  ) THEN
                   
                    DO:
                    create ttTaxGroupLook.
                    assign                                     
                          ttTaxGroupLook.vgroup      = stax.tax-group
                    ttTaxGroupLook.vtaxcode1    = stax.tax-code1[1]
                    ttTaxGroupLook.vtaxcode2   = stax.tax-code1[2]
                    ttTaxGroupLook.vtaxcode3   = stax.tax-code1[3]
                    ttTaxGroupLook.vtaxcode4   = stax.tax-code1[4]
                    ttTaxGroupLook.vtaxcode5   = stax.tax-code1[5]
                    ttTaxGroupLook.rate   = string(stax.tax-rate1[1])
                    ttTaxGroupLook.taxfrt = string(stax.tax-frt1[1])
                    ttTaxGroupLook.taxacc = stax.tax-acc1[1]
                    ttTaxGroupLook.vdscr       = stax.tax-dscr1[1]
                    ttTaxGroupLook.vdscr2      = stax.tax-dscr1[2]
                    ttTaxGroupLook.vdscr3      = stax.tax-dscr1[3]
                    ttTaxGroupLook.vdscr4      = stax.tax-dscr1[4]
                    ttTaxGroupLook.vdscr5      = stax.tax-dscr1[5] .

                END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "group"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH stax WHERE  stax.tax-group = prmText AND stax.company = prmComp  AND 
                 stax.tax-code1[1] EQ stax.tax-group AND
                 stax.tax-group NE ""  NO-LOCK :
                 create ttTaxGroupLook.
                 assign                                     
                     ttTaxGroupLook.vgroup      = stax.tax-group
                    ttTaxGroupLook.vtaxcode1    = stax.tax-code1[1]
                    ttTaxGroupLook.vtaxcode2   = stax.tax-code1[2]
                    ttTaxGroupLook.vtaxcode3   = stax.tax-code1[3]
                    ttTaxGroupLook.vtaxcode4   = stax.tax-code1[4]
                    ttTaxGroupLook.vtaxcode5   = stax.tax-code1[5]
                    ttTaxGroupLook.rate   = string(stax.tax-rate1[1])
                    ttTaxGroupLook.taxfrt = string(stax.tax-frt1[1])
                    ttTaxGroupLook.taxacc = stax.tax-acc1[1]
                    ttTaxGroupLook.vdscr       = stax.tax-dscr1[1]
                    ttTaxGroupLook.vdscr2      = stax.tax-dscr1[2]
                    ttTaxGroupLook.vdscr3      = stax.tax-dscr1[3]
                    ttTaxGroupLook.vdscr4      = stax.tax-dscr1[4]
                    ttTaxGroupLook.vdscr5      = stax.tax-dscr1[5] .

             END.
         END.
         if prmCondition = "BEGIN" then do:
             FOR EACH stax WHERE  stax.tax-group Begins prmText 
                  AND stax.company = prmComp  AND 
                 stax.tax-code1[1] EQ stax.tax-group AND
                 stax.tax-group NE ""  NO-LOCK :
                 create ttTaxGroupLook.
                 assign                                     
                    ttTaxGroupLook.vgroup      = stax.tax-group
                    ttTaxGroupLook.vtaxcode1    = stax.tax-code1[1]
                    ttTaxGroupLook.vtaxcode2   = stax.tax-code1[2]
                    ttTaxGroupLook.vtaxcode3   = stax.tax-code1[3]
                    ttTaxGroupLook.vtaxcode4   = stax.tax-code1[4]
                    ttTaxGroupLook.vtaxcode5   = stax.tax-code1[5]
                    ttTaxGroupLook.rate   = string(stax.tax-rate1[1])
                    ttTaxGroupLook.taxfrt = string(stax.tax-frt1[1])
                    ttTaxGroupLook.taxacc = stax.tax-acc1[1]
                    ttTaxGroupLook.vdscr       = stax.tax-dscr1[1]
                    ttTaxGroupLook.vdscr2      = stax.tax-dscr1[2]
                    ttTaxGroupLook.vdscr3      = stax.tax-dscr1[3]
                    ttTaxGroupLook.vdscr4      = stax.tax-dscr1[4]
                    ttTaxGroupLook.vdscr5      = stax.tax-dscr1[5] .

             END.
         END.

         END. /*FOR EACH state*/

             if prmField = "code"  then do:
                if prmCondition = "EQUAL" then do:
                     FOR EACH stax WHERE  stax.tax-code[1] = prmText AND 
                         stax.company = prmComp  AND 
                         stax.tax-code1[1] EQ stax.tax-group AND
                         stax.tax-group NE ""  NO-LOCK :
                          create ttTaxGroupLook.
                            assign                                     
                               ttTaxGroupLook.vgroup      = stax.tax-group
                    ttTaxGroupLook.vtaxcode1    = stax.tax-code1[1]
                    ttTaxGroupLook.vtaxcode2   = stax.tax-code1[2]
                    ttTaxGroupLook.vtaxcode3   = stax.tax-code1[3]
                    ttTaxGroupLook.vtaxcode4   = stax.tax-code1[4]
                    ttTaxGroupLook.vtaxcode5   = stax.tax-code1[5]
                    ttTaxGroupLook.rate   = string(stax.tax-rate1[1])
                    ttTaxGroupLook.taxfrt = string(stax.tax-frt1[1])
                    ttTaxGroupLook.taxacc = stax.tax-acc1[1]
                    ttTaxGroupLook.vdscr       = stax.tax-dscr1[1]
                    ttTaxGroupLook.vdscr2      = stax.tax-dscr1[2]
                    ttTaxGroupLook.vdscr3      = stax.tax-dscr1[3]
                    ttTaxGroupLook.vdscr4      = stax.tax-dscr1[4]
                    ttTaxGroupLook.vdscr5      = stax.tax-dscr1[5] .

             END.


          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH stax WHERE stax.tax-code[1] BEGINS prmText AND
                      stax.company = prmComp  AND 
                      stax.tax-code1[1] EQ stax.tax-group AND
                      stax.tax-group NE "" NO-LOCK :
                      create ttTaxGroupLook.
                      assign   
                         ttTaxGroupLook.vgroup      = stax.tax-group
                    ttTaxGroupLook.vtaxcode1    = stax.tax-code1[1]
                    ttTaxGroupLook.vtaxcode2   = stax.tax-code1[2]
                    ttTaxGroupLook.vtaxcode3   = stax.tax-code1[3]
                    ttTaxGroupLook.vtaxcode4   = stax.tax-code1[4]
                    ttTaxGroupLook.vtaxcode5   = stax.tax-code1[5]
                    ttTaxGroupLook.rate   = string(stax.tax-rate1[1])
                    ttTaxGroupLook.taxfrt = string(stax.tax-frt1[1])
                    ttTaxGroupLook.taxacc = stax.tax-acc1[1]
                    ttTaxGroupLook.vdscr       = stax.tax-dscr1[1]
                    ttTaxGroupLook.vdscr2      = stax.tax-dscr1[2]
                    ttTaxGroupLook.vdscr3      = stax.tax-dscr1[3]
                    ttTaxGroupLook.vdscr4      = stax.tax-dscr1[4]
                    ttTaxGroupLook.vdscr5      = stax.tax-dscr1[5].
                  end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */


