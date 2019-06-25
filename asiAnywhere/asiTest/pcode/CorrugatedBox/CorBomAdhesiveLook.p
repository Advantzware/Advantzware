/*------------------------------------------------------------------------
    File      : CorBomAdhesiveLook.p
    Purpose   :  Corrugated Bill of Material for Adhesive
    Syntax    :

    Description : Return a Dataset of Corrugated Box Adhesive

    Author(s)   : 
    Created     : 22 sep 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorrBomAdhesiveLookup NO-UNDO 
        FIELD vItemNo         AS character
        FIELD vItemName       AS CHARACTER
        FIELD vER       AS CHAR
        FIELD vCal       AS DECIMAL
        FIELD vWid       AS DECIMAL
        FIELD vLen       AS DECIMAL
        FIELD vQOH       AS DECIMAL
        FIELD vComit       AS DECIMAL
        FIELD vAvail       AS DECIMAL
        FIELD vSqInch       AS DECIMAL

        .
    
DEFINE DATASET dsCorrBomAdhesiveLookup FOR ttCorrBomAdhesiveLookup .

DEFINE INPUT PARAMETER prmAction        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition     AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmIndustry       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMatType       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmItem          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm          AS INT         NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrBomAdhesiveLookup.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmComp        = ? THEN ASSIGN prmComp        = "".
IF prmIndustry     = ? THEN ASSIGN prmIndustry     = "".
IF prmMatType     = ? THEN ASSIGN prmMatType     = "".
IF prmItem        = ? THEN ASSIGN prmItem        = "".
IF  prmEstimate    = ?  THEN ASSIGN prmEstimate = "".
IF prmForm        = ?  THEN ASSIGN prmForm  = 0.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


if prmAction <> "search" then do:
    
    FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and lookup(item.mat-type,prmMatType) > 0 NO-LOCK :
             create ttCorrBomAdhesiveLookup.
             assign  

                 ttCorrBomAdhesiveLookup.vItemNo          = item.i-no
                 ttCorrBomAdhesiveLookup.vItemName        = item.i-name
                 ttCorrBomAdhesiveLookup.vER              = item.i-code 
                 ttCorrBomAdhesiveLookup.vCal             = item.cal
                 ttCorrBomAdhesiveLookup.vWid             = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomAdhesiveLookup.vLen             = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomAdhesiveLookup.vQOH             = item.q-onh
                 ttCorrBomAdhesiveLookup.vComit           = item.q-comm
                 ttCorrBomAdhesiveLookup.vAvail           = item.q-avail 
                 
                 .
            
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "item" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and lookup(item.mat-type,prmMatType) > 0 AND ITEM.i-no = prmText NO-LOCK :
            IF AVAIL item THEN
            create ttCorrBomAdhesiveLookup.
             assign  

                 ttCorrBomAdhesiveLookup.vItemNo          = item.i-no
                 ttCorrBomAdhesiveLookup.vItemName        = item.i-name
                 ttCorrBomAdhesiveLookup.vER              = item.i-code 
                 ttCorrBomAdhesiveLookup.vCal             = item.cal
                 ttCorrBomAdhesiveLookup.vWid             = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomAdhesiveLookup.vLen             = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomAdhesiveLookup.vQOH             = item.q-onh
                 ttCorrBomAdhesiveLookup.vComit           = item.q-comm
                 ttCorrBomAdhesiveLookup.vAvail           = item.q-avail
                     .

        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and lookup(item.mat-type,prmMatType) > 0 AND ITEM.i-no BEGINS prmText NO-LOCK :
         IF AVAIL ITEM THEN     
        create ttCorrBomAdhesiveLookup.
             assign  

                 ttCorrBomAdhesiveLookup.vItemNo          = item.i-no
                 ttCorrBomAdhesiveLookup.vItemName        = item.i-name
                 ttCorrBomAdhesiveLookup.vER              = item.i-code 
                 ttCorrBomAdhesiveLookup.vCal             = item.cal
                 ttCorrBomAdhesiveLookup.vWid             = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomAdhesiveLookup.vLen             = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomAdhesiveLookup.vQOH             = item.q-onh
                 ttCorrBomAdhesiveLookup.vComit           = item.q-comm
                 ttCorrBomAdhesiveLookup.vAvail           = item.q-avail
                     .
           
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "name" then do:
    if prmCondition = "EQUAL" then do:
       FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and lookup(item.mat-type,prmMatType) > 0 AND ITEM.i-name = prmText NO-LOCK :
            IF AVAIL ITEM THEN  
           create ttCorrBomAdhesiveLookup.
             assign  

                 ttCorrBomAdhesiveLookup.vItemNo          = item.i-no
                 ttCorrBomAdhesiveLookup.vItemName        = item.i-name
                 ttCorrBomAdhesiveLookup.vER              = item.i-code 
                 ttCorrBomAdhesiveLookup.vCal             = item.cal
                 ttCorrBomAdhesiveLookup.vWid             = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomAdhesiveLookup.vLen             = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomAdhesiveLookup.vQOH             = item.q-onh
                 ttCorrBomAdhesiveLookup.vComit           = item.q-comm
                 ttCorrBomAdhesiveLookup.vAvail           = item.q-avail
                     .
             
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
   FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and lookup(item.mat-type,prmMatType) > 0 AND ITEM.i-name BEGINS prmText NO-LOCK :
        IF AVAIL ITEM THEN      
       create ttCorrBomAdhesiveLookup.
             assign  

                ttCorrBomAdhesiveLookup.vItemNo          = item.i-no
                 ttCorrBomAdhesiveLookup.vItemName        = item.i-name
                 ttCorrBomAdhesiveLookup.vER              = item.i-code 
                 ttCorrBomAdhesiveLookup.vCal             = item.cal
                 ttCorrBomAdhesiveLookup.vWid             = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomAdhesiveLookup.vLen             = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomAdhesiveLookup.vQOH             = item.q-onh
                 ttCorrBomAdhesiveLookup.vComit           = item.q-comm
                 ttCorrBomAdhesiveLookup.vAvail           = item.q-avail
                     .
           
   END.
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


