

/*------------------------------------------------------------------------
    File        : TypeLookUp.p
    Purpose     : 
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTypeLookUp NO-UNDO 
    FIELD vcst  AS CHARACTER
    FIELD vCustype  AS CHARACTER
    FIELD vDscr     AS CHARACTER
    FIELD vCommrate AS DECIMAL
    FIELD vDiscount AS DECIMAL
    .
                                           
    
DEFINE DATASET dsTypeLookUp FOR ttTypeLookUp .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTypeLookUp.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmField     = ? THEN  ASSIGN prmField  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
MESSAGE "Type" prmAction  prmUser  prmCondition   prmText   prmField.

if prmAction <> "search" then do:
    FOR EACH custype  NO-LOCK:
                 create ttTypeLookUp.
                 assign                                     
                    ttTypeLookUp.vCustype   = custype.custype  
                    ttTypeLookUp.vDscr      = custype.dscr
                    ttTypeLookUp.vCommrate  = custype.commrate
                    ttTypeLookUp.vDiscount  = custype.discount
                   .
                 MESSAGE "tttype"  ttTypeLookUp.vCustype. 
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH custype WHERE custype.company = prmComp NO-LOCK:
                    IF (custype.custype = prmText OR custype.dscr = prmText) THEN
                        DO:
                        create ttTypeLookUp.
                        assign                                     
                           ttTypeLookUp.vCustype   = custype.custype  
                           ttTypeLookUp.vDscr      = custype.dscr
                           ttTypeLookUp.vCommrate  = custype.commrate
                           ttTypeLookUp.vDiscount  = custype.discount
                           .
                    END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH custype WHERE custype.company = prmComp  NO-LOCK :
                IF (custype.custype BEGINS prmText OR custype.dscr BEGINS prmText) THEN
                    DO:
                    create ttTypeLookUp.
                    assign                                     
                       ttTypeLookUp.vCustype   = custype.custype  
                       ttTypeLookUp.vDscr      = custype.dscr
                       ttTypeLookUp.vCommrate  = custype.commrate
                       ttTypeLookUp.vDiscount  = custype.discount
                       .
                    END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  

     if prmField = "custype"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH custype WHERE custype.company = prmComp AND custype.custype = prmText  NO-LOCK :
                 create ttTypeLookUp.
                 assign                                     
                    ttTypeLookUp.vCustype   = custype.custype  
                    ttTypeLookUp.vDscr      = custype.dscr
                    ttTypeLookUp.vCommrate  = custype.commrate
                    ttTypeLookUp.vDiscount  = custype.discount
                    .
             END.

          END. /*FOR EACH custype*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH custype WHERE custype.company = prmComp AND custype.custype BEGINS prmText NO-LOCK :
                      create ttTypeLookUp.
                      assign                                     
                        ttTypeLookUp.vCustype   = custype.custype  
                        ttTypeLookUp.vDscr      = custype.dscr
                        ttTypeLookUp.vCommrate  = custype.commrate
                        ttTypeLookUp.vDiscount  = custype.discount
                        .
                  end.  /*FOR EACH custype wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = custype  */
           
END.  /* IF prmAction = search then do: */



