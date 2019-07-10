/*------------------------------------------------------------------------
    File      : CorBomLook.p
    Purpose   :  Corrugated Bill of Material
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 19 sep 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorrBomLookup NO-UNDO 
        FIELD vItemNo         AS character
        FIELD vItemName       AS CHARACTER
        FIELD vShrink       AS CHAR
        FIELD vSqInch       AS DECIMAL

        FIELD vItemNo2         AS character
        FIELD vItemName2       AS CHARACTER
        .
    
DEFINE DATASET dsCorrBomLookup FOR ttCorrBomLookup .

DEFINE INPUT PARAMETER prmAction        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition     AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMatType       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmItem          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm          AS INT         NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrBomLookup.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmComp        = ? THEN ASSIGN prmComp        = "".
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

DEF VAR v-sq-inch-1 AS DEC DECIMALS 4 NO-UNDO.

def var k_frac as dec init 6.25 no-undo.

FOR EACH ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK:
 
END.

if prmAction <> "search" then do:
    
    FOR EACH item WHERE  item.company = prmComp and ( item.mat-type = prmMatType ) and ( item.i-no >= prmItem or prmItem eq "") NO-LOCK :
             create ttCorrBomLookup.
             assign  

                 ttCorrBomLookup.vItemNo          = item.i-no
                 ttCorrBomLookup.vItemName        = item.i-name
                 ttCorrBomLookup.vShrink          = string(item.shrink)

                 ttCorrBomLookup.vItemNo2          = item.i-no
                 ttCorrBomLookup.vItemName2        = item.i-name
                 
                 .
            IF AVAIL ef THEN
            v-sq-inch-1 = IF DECIMAL(item.shrink) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(item.shrink) / 100)))
                                     ELSE 0.
        ASSIGN ttCorrBomLookup.vSqInch          = round(trunc((v-sq-inch-1),0) + (((v-sq-inch-1) - trunc((v-sq-inch-1),0)) / K_FRAC),2) .
         
    END. /* for each usercust */

    FOR EACH ttCorrBomLookup NO-LOCK:
        IF INDEX(ttCorrBomLookup.vItemNo2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLookup.vItemNo2  = REPLACE(ttCorrBomLookup.vItemNo2 ,'"',":").
        IF INDEX(ttCorrBomLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLookup.vItemName2  = REPLACE(ttCorrBomLookup.vItemName2 ,'"',":").

    END.

END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "item" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH item WHERE  item.company = prmComp and ( item.mat-type = prmMatType ) and ( item.i-no >= prmItem or prmItem eq "") AND ITEM.i-no = prmText NO-LOCK :
            IF AVAIL item THEN
            create ttCorrBomLookup.
             assign  

                 ttCorrBomLookup.vItemNo          = item.i-no
                 ttCorrBomLookup.vItemName        = item.i-name
                 ttCorrBomLookup.vShrink          = string(item.shrink)

                 ttCorrBomLookup.vItemNo2          = item.i-no
                 ttCorrBomLookup.vItemName2        = item.i-name
                  
                 .
             IF AVAIL ef THEN
              v-sq-inch-1 = IF DECIMAL(item.shrink) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(item.shrink) / 100)))
                                     ELSE 0.
        ASSIGN ttCorrBomLookup.vSqInch          = round(trunc((v-sq-inch-1),0) + (((v-sq-inch-1) - trunc((v-sq-inch-1),0)) / K_FRAC),2) .
        END. /*FOR EACH item where*/

        FOR EACH ttCorrBomLookup NO-LOCK:
            IF INDEX(ttCorrBomLookup.vItemNo2 ,'"',1) > 0 THEN ASSIGN
                ttCorrBomLookup.vItemNo2  = REPLACE(ttCorrBomLookup.vItemNo2 ,'"',":").
            IF INDEX(ttCorrBomLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
                ttCorrBomLookup.vItemName2  = REPLACE(ttCorrBomLookup.vItemName2 ,'"',":").

        END.

    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH item WHERE  item.company = prmComp and ( item.mat-type = prmMatType ) and ( item.i-no >= prmItem or prmItem eq "") AND ITEM.i-no BEGINS prmText NO-LOCK :
         IF AVAIL ITEM THEN     
        create ttCorrBomLookup.
             assign  

                 ttCorrBomLookup.vItemNo          = item.i-no
                 ttCorrBomLookup.vItemName        = item.i-name
                 ttCorrBomLookup.vShrink          = string(item.shrink)

                 ttCorrBomLookup.vItemNo2          = item.i-no
                 ttCorrBomLookup.vItemName2        = item.i-name
                  
                 .
             IF AVAIL ef THEN
             v-sq-inch-1 = IF DECIMAL(item.shrink) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(item.shrink) / 100)))
                                     ELSE 0.
        ASSIGN ttCorrBomLookup.vSqInch          = round(trunc((v-sq-inch-1),0) + (((v-sq-inch-1) - trunc((v-sq-inch-1),0)) / K_FRAC),2) .
           
     END.  /*for each usercust */

     FOR EACH ttCorrBomLookup NO-LOCK:
        IF INDEX(ttCorrBomLookup.vItemNo2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLookup.vItemNo2  = REPLACE(ttCorrBomLookup.vItemNo2 ,'"',":").
        IF INDEX(ttCorrBomLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLookup.vItemName2  = REPLACE(ttCorrBomLookup.vItemName2 ,'"',":").

    END.

END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "name" then do:
    if prmCondition = "EQUAL" then do:
       FOR EACH item WHERE  item.company = prmComp and ( item.mat-type = prmMatType ) and ( item.i-no >= prmItem or prmItem eq "") AND ITEM.i-name = prmText NO-LOCK :
            IF AVAIL ITEM THEN  
           create ttCorrBomLookup.
             assign  

                 ttCorrBomLookup.vItemNo          = item.i-no
                 ttCorrBomLookup.vItemName        = item.i-name
                 ttCorrBomLookup.vShrink          = string(item.shrink) 
                  
                 ttCorrBomLookup.vItemNo2          = item.i-no
                 ttCorrBomLookup.vItemName2        = item.i-name
                 .
             IF AVAIL ef THEN
             v-sq-inch-1 = IF DECIMAL(item.shrink) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(item.shrink) / 100)))
                                     ELSE 0.
        ASSIGN ttCorrBomLookup.vSqInch          = round(trunc((v-sq-inch-1),0) + (((v-sq-inch-1) - trunc((v-sq-inch-1),0)) / K_FRAC),2) .
             
        END. /*FOR EACH item where*/

        FOR EACH ttCorrBomLookup NO-LOCK:
            IF INDEX(ttCorrBomLookup.vItemNo2 ,'"',1) > 0 THEN ASSIGN
                ttCorrBomLookup.vItemNo2  = REPLACE(ttCorrBomLookup.vItemNo2 ,'"',":").
            IF INDEX(ttCorrBomLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
                ttCorrBomLookup.vItemName2  = REPLACE(ttCorrBomLookup.vItemName2 ,'"',":").

        END.

    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
   FOR EACH item WHERE  item.company = prmComp and ( item.mat-type = prmMatType ) and ( item.i-no >= prmItem or prmItem eq "") AND ITEM.i-name BEGINS prmText NO-LOCK :
        IF AVAIL ITEM THEN      
       create ttCorrBomLookup.
             assign  

                 ttCorrBomLookup.vItemNo          = item.i-no
                 ttCorrBomLookup.vItemName        = item.i-name
                 ttCorrBomLookup.vShrink          = string(item.shrink)
                  
                 ttCorrBomLookup.vItemNo2          = item.i-no
                 ttCorrBomLookup.vItemName2        = item.i-name
                 .
             IF AVAIL ef THEN
            v-sq-inch-1 = IF DECIMAL(item.shrink) NE 100 THEN
                                        ef.gsh-wid *
                                        (ef.gsh-len / (1 - (DECIMAL(item.shrink) / 100)))
                                     ELSE 0.
        ASSIGN ttCorrBomLookup.vSqInch          = round(trunc((v-sq-inch-1),0) + (((v-sq-inch-1) - trunc((v-sq-inch-1),0)) / K_FRAC),2) .
           
   END.
    
   FOR EACH ttCorrBomLookup NO-LOCK:
        IF INDEX(ttCorrBomLookup.vItemNo2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLookup.vItemNo2  = REPLACE(ttCorrBomLookup.vItemNo2 ,'"',":").
        IF INDEX(ttCorrBomLookup.vItemName2 ,'"',1) > 0 THEN ASSIGN
            ttCorrBomLookup.vItemName2  = REPLACE(ttCorrBomLookup.vItemName2 ,'"',":").

    END.

END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


