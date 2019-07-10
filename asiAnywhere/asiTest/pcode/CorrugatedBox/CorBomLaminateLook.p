/*------------------------------------------------------------------------
    File      : CorBomLaminateLook.p
    Purpose   :  Corrugated Bom Laminate Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Bom Laminate 

    Author(s)   : 
    Created     : 19 sept 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorrBomLaminateLookup NO-UNDO 
        FIELD vItem         AS character
        FIELD vItemName     AS CHARACTER
        FIELD vEr           AS CHAR
        FIELD vCaliper      AS DECIMAL
        FIELD vWidth        AS DECIMAL
        FIELD vLength       AS DECIMAL
        FIELD vQOH          AS DECIMAL
        FIELD vCommitted    AS DECIMAL
        FIELD vAvail        AS DECIMAL
        FIELD vSqInch       AS DECIMAL
        
        FIELD vItem2         AS character
        FIELD vItemName2     AS CHARACTER
        .
    
DEFINE DATASET dsCorrBomLaminateLookup FOR ttCorrBomLaminateLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMatType        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmIndustry        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm          AS INT         NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrBomLaminateLookup.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmComp        = ? THEN ASSIGN prmComp    = "".
IF prmMatType     = ? THEN ASSIGN prmMatType    = "".
IF prmIndustry     = ? THEN ASSIGN prmIndustry    = "".
IF  prmEstimate    = ?  THEN ASSIGN prmEstimate = "".
IF prmForm        = ?  THEN ASSIGN prmForm  = 0.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
DEF VAR v-lam-in AS DEC DECIMALS 4 NO-UNDO.
def var k_frac as dec init 6.25 no-undo.

FOR EACH ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK:
 ASSIGN v-lam-in = ef.gsh-len * ef.gsh-wid.
END.
MESSAGE "indus" prmIndustry.
if prmAction <> "search" then do:
    
    FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and ( item.mat-type = prmMatType or prmMatType eq "") NO-LOCK :
             create ttCorrBomLaminateLookup.
             assign  

                 ttCorrBomLaminateLookup.vItem          = item.i-no
                 ttCorrBomLaminateLookup.vItemName      = item.i-name
                 ttCorrBomLaminateLookup.vEr            = item.i-code 
                 ttCorrBomLaminateLookup.vCaliper       = item.cal
                 ttCorrBomLaminateLookup.vWidth         = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomLaminateLookup.vLength        = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomLaminateLookup.vQOH           = item.q-onh
                 ttCorrBomLaminateLookup.vCommitted     = item.q-comm
                 ttCorrBomLaminateLookup.vAvail         = item.q-avail
                 ttCorrBomLaminateLookup.vSqInch        = round(trunc((v-lam-in),0) + (((v-lam-in) - trunc((v-lam-in),0)) / K_FRAC),2)
                 
                 ttCorrBomLaminateLookup.vItem2          = item.i-no
                 ttCorrBomLaminateLookup.vItemName2      = item.i-name
                     
                     .

         
    END. /* for each item */

    FOR EACH ttCorrBomLaminateLookup NO-LOCK:
        IF INDEX(ttCorrBomLaminateLookup.vItem2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLaminateLookup.vItem2  = REPLACE(ttCorrBomLaminateLookup.vItem2 ,'"',":").
        IF INDEX(ttCorrBomLaminateLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLaminateLookup.vItemName2  = REPLACE(ttCorrBomLaminateLookup.vItemName2 ,'"',":").

    END.
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "item" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and ( item.mat-type = prmMatType or prmMatType eq "") AND ITEM.i-no = prmText NO-LOCK :
            IF AVAIL ITEM THEN
            create ttCorrBomLaminateLookup.
             assign  

                 ttCorrBomLaminateLookup.vItem          = item.i-no
                 ttCorrBomLaminateLookup.vItemName      = item.i-name
                 ttCorrBomLaminateLookup.vEr            = item.i-code 
                 ttCorrBomLaminateLookup.vCaliper       = item.cal
                 ttCorrBomLaminateLookup.vWidth         = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomLaminateLookup.vLength        = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomLaminateLookup.vQOH           = item.q-onh
                 ttCorrBomLaminateLookup.vCommitted     = item.q-comm
                 ttCorrBomLaminateLookup.vAvail         = item.q-avail
                 ttCorrBomLaminateLookup.vSqInch        = round(trunc((v-lam-in),0) + (((v-lam-in) - trunc((v-lam-in),0)) / K_FRAC),2)

                 ttCorrBomLaminateLookup.vItem2          = item.i-no
                 ttCorrBomLaminateLookup.vItemName2      = item.i-name
                 .
             
        END. /*FOR EACH item where*/

        FOR EACH ttCorrBomLaminateLookup NO-LOCK:
            IF INDEX(ttCorrBomLaminateLookup.vItem2 ,'"',1) > 0 THEN ASSIGN
                ttCorrBomLaminateLookup.vItem2  = REPLACE(ttCorrBomLaminateLookup.vItem2 ,'"',":").
            IF INDEX(ttCorrBomLaminateLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
                ttCorrBomLaminateLookup.vItemName2  = REPLACE(ttCorrBomLaminateLookup.vItemName2 ,'"',":").

        END.

    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and ( item.mat-type = prmMatType or prmMatType eq "") AND ITEM.i-no BEGINS prmText NO-LOCK :
         IF AVAIL ITEM THEN     
        create ttCorrBomLaminateLookup.
             assign  

                 ttCorrBomLaminateLookup.vItem          = item.i-no
                 ttCorrBomLaminateLookup.vItemName      = item.i-name
                 ttCorrBomLaminateLookup.vEr            = item.i-code 
                 ttCorrBomLaminateLookup.vCaliper       = item.cal
                 ttCorrBomLaminateLookup.vWidth         = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomLaminateLookup.vLength        = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomLaminateLookup.vQOH           = item.q-onh
                 ttCorrBomLaminateLookup.vCommitted     = item.q-comm
                 ttCorrBomLaminateLookup.vAvail         = item.q-avail 
                 ttCorrBomLaminateLookup.vSqInch        = round(trunc((v-lam-in),0) + (((v-lam-in) - trunc((v-lam-in),0)) / K_FRAC),2)

                 ttCorrBomLaminateLookup.vItem2          = item.i-no
                 ttCorrBomLaminateLookup.vItemName2      = item.i-name
                 .
           
     END.  /*for each item */

     FOR EACH ttCorrBomLaminateLookup NO-LOCK:
        IF INDEX(ttCorrBomLaminateLookup.vItem2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLaminateLookup.vItem2  = REPLACE(ttCorrBomLaminateLookup.vItem2 ,'"',":").
        IF INDEX(ttCorrBomLaminateLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLaminateLookup.vItemName2  = REPLACE(ttCorrBomLaminateLookup.vItemName2 ,'"',":").

    END.

END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "name" then do:
    if prmCondition = "EQUAL" then do:
       FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and ( item.mat-type = prmMatType or prmMatType eq "") AND ITEM.i-name = prmText NO-LOCK :
            IF AVAIL ITEM THEN  
           create ttCorrBomLaminateLookup.
             assign  

                ttCorrBomLaminateLookup.vItem          = item.i-no
                 ttCorrBomLaminateLookup.vItemName      = item.i-name
                 ttCorrBomLaminateLookup.vEr            = item.i-code 
                 ttCorrBomLaminateLookup.vCaliper       = item.cal
                 ttCorrBomLaminateLookup.vWidth         = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomLaminateLookup.vLength        = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomLaminateLookup.vQOH           = item.q-onh
                 ttCorrBomLaminateLookup.vCommitted     = item.q-comm
                 ttCorrBomLaminateLookup.vAvail         = item.q-avail
                 ttCorrBomLaminateLookup.vSqInch        = round(trunc((v-lam-in),0) + (((v-lam-in) - trunc((v-lam-in),0)) / K_FRAC),2)

                 ttCorrBomLaminateLookup.vItem2          = item.i-no
                 ttCorrBomLaminateLookup.vItemName2      = item.i-name
                 .
             
        END. /*FOR EACH item where*/

        FOR EACH ttCorrBomLaminateLookup NO-LOCK:
            IF INDEX(ttCorrBomLaminateLookup.vItem2 ,'"',1) > 0 THEN ASSIGN
                ttCorrBomLaminateLookup.vItem2  = REPLACE(ttCorrBomLaminateLookup.vItem2 ,'"',":").
            IF INDEX(ttCorrBomLaminateLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
                ttCorrBomLaminateLookup.vItemName2  = REPLACE(ttCorrBomLaminateLookup.vItemName2 ,'"',":").

        END.

    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
   FOR EACH item WHERE  item.company = prmComp and ( item.industry = prmIndustry OR prmIndustry = "" ) and ( item.mat-type = prmMatType or prmMatType eq "") AND ITEM.i-name BEGINS prmText NO-LOCK :
        IF AVAIL ITEM THEN      
       create ttCorrBomLaminateLookup.
             assign  

                 ttCorrBomLaminateLookup.vItem          = item.i-no
                 ttCorrBomLaminateLookup.vItemName      = item.i-name
                 ttCorrBomLaminateLookup.vEr            = item.i-code 
                 ttCorrBomLaminateLookup.vCaliper       = item.cal
                 ttCorrBomLaminateLookup.vWidth         = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                 ttCorrBomLaminateLookup.vLength        = IF item.r-wid EQ 0 THEN item.s-len ELSE 12 
                 ttCorrBomLaminateLookup.vQOH           = item.q-onh
                 ttCorrBomLaminateLookup.vCommitted     = item.q-comm
                 ttCorrBomLaminateLookup.vAvail         = item.q-avail 
                 ttCorrBomLaminateLookup.vSqInch        = round(trunc((v-lam-in),0) + (((v-lam-in) - trunc((v-lam-in),0)) / K_FRAC),2)

                 ttCorrBomLaminateLookup.vItem2          = item.i-no
                 ttCorrBomLaminateLookup.vItemName2      = item.i-name
                 .

           
   END.

   FOR EACH ttCorrBomLaminateLookup NO-LOCK:
        IF INDEX(ttCorrBomLaminateLookup.vItem2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLaminateLookup.vItem2  = REPLACE(ttCorrBomLaminateLookup.vItem2 ,'"',":").
        IF INDEX(ttCorrBomLaminateLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLaminateLookup.vItemName2  = REPLACE(ttCorrBomLaminateLookup.vItemName2 ,'"',":").

    END.

END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


