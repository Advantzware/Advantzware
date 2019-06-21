/*------------------------------------------------------------------------
    File      : ItemLookup.p
    Purpose   :  ItemLookup
    Syntax    :

    Description : Return a Dataset of ItemLookup

    Author(s)   : 
    Created     : 12 Mar 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemSpecLookup NO-UNDO 
        FIELD vIno         AS character
        FIELD vName        AS CHARACTER
        FIELD vDscr        AS CHARACTER
        FIELD vPressType   AS CHARACTER
        
        .
    
DEFINE DATASET dsItemSpecLookup FOR ttItemSpecLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmType      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmIndustry  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate        AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemSpecLookup.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmIndustry    = ? THEN ASSIGN prmIndustry    = "".
IF prmEstimate    = ? THEN ASSIGN prmEstimate    = "".

DEFINE VAR v-count AS INT NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
MESSAGE "valtest" prmComp prmType prmIndustry.
FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
IF AVAIL eb THEN
    MESSAGE "style" eb.style.
          
if prmAction <> "search" then do:
    v-count = 0 . 
    FOR EACH item WHERE  item.company = prmComp and  (item.industry = prmIndustry or prmIndustry = "") 
        and lookup(item.mat-type,prmType) > 0 NO-LOCK :
         IF AVAIL item THEN DO:
             create ttItemSpecLookup.
             assign  

                 ttItemSpecLookup.vIno         = ITEM.i-no
                 ttItemSpecLookup.vName        = ITEM.i-name 
                 ttItemSpecLookup.vDscr        = item.i-dscr
                 ttItemSpecLookup.vPressType   = ITEM.press-type
                 
                 .
                 
             v-count = v-count + 1 .
             IF v-count > 100 THEN LEAVE.

         END.	 /* IF AVAIL eb */
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "item" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH item WHERE  item.company = prmComp and  (item.industry = prmIndustry or prmIndustry = "") 
            AND ITEM.i-no = prmText  and lookup(item.mat-type,prmType) > 0 NO-LOCK :
         IF AVAIL item THEN DO:
             create ttItemSpecLookup.
             assign  

                 ttItemSpecLookup.vIno         = ITEM.i-no
                 ttItemSpecLookup.vName        = ITEM.i-name 
                 ttItemSpecLookup.vDscr        = item.i-dscr
                 ttItemSpecLookup.vPressType   = ITEM.press-type 
                 
                 .
                
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH item WHERE  item.company = prmComp and  (item.industry = prmIndustry or prmIndustry = "") 
            AND ITEM.i-no BEGINS prmText  and lookup(item.mat-type,prmType) > 0 NO-LOCK :
         IF AVAIL item THEN DO:
             create ttItemSpecLookup.
             assign  

                 ttItemSpecLookup.vIno         = ITEM.i-no
                 ttItemSpecLookup.vName        = ITEM.i-name 
                 ttItemSpecLookup.vDscr        = item.i-dscr
                 ttItemSpecLookup.vPressType   = ITEM.press-type 
                 
                 .

           END. /*if avail item*/
     END.  /*for each item */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "adh" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH item WHERE  item.company = prmComp and  (item.industry = prmIndustry or prmIndustry = "") 
            AND ITEM.press-type = prmText  and lookup(item.mat-type,prmType) > 0 NO-LOCK :
         IF AVAIL item THEN DO:
             create ttItemSpecLookup.
             assign  

                 ttItemSpecLookup.vIno         = ITEM.i-no
                 ttItemSpecLookup.vName        = ITEM.i-name 
                 ttItemSpecLookup.vDscr        = item.i-dscr
                 ttItemSpecLookup.vPressType   = ITEM.press-type 
                 
                 .
                
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH item WHERE  item.company = prmComp and  (item.industry = prmIndustry or prmIndustry = "") 
            AND ITEM.press-type BEGINS prmText  and lookup(item.mat-type,prmType) > 0 NO-LOCK :
         IF AVAIL item THEN DO:
             create ttItemSpecLookup.
             assign  

                 ttItemSpecLookup.vIno         = ITEM.i-no
                 ttItemSpecLookup.vName        = ITEM.i-name 
                 ttItemSpecLookup.vDscr        = item.i-dscr
                 ttItemSpecLookup.vPressType   = ITEM.press-type 
                 
                 .

           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/



