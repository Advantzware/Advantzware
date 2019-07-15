/*------------------------------------------------------------------------
    File      : AdhesiveLook
    Purpose   :  Corrugated Customer Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 2 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttAdderLookup NO-UNDO 
        FIELD vIno         AS character
        FIELD vName        AS CHARACTER
        
        
        .
    
DEFINE DATASET dsAdderLookup FOR ttAdderLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmIndustry  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate        AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsAdderLookup.

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
MESSAGE "val" prmEstimate.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
IF AVAIL eb THEN
            find style where style.company = prmComp and
                            style.style = eb.style
                            no-lock no-error.   
           if avail style then prmIndustry = style.industry.
           else prmIndustry = "".  

if prmAction <> "search" then do:
    v-count = 0 . 
    FOR EACH item WHERE item.company = prmComp  and (item.industry = prmIndustry or prmIndustry = "") 
        and item.mat-type = "A" NO-LOCK :
        IF AVAIL item THEN DO:
             create ttAdderLookup.
             assign  

                 ttAdderLookup.vIno         = ITEM.i-no
                 ttAdderLookup.vName        = ITEM.i-name 
                  
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
            AND ITEM.i-no = prmText  and item.mat-type = "A" NO-LOCK :
         IF AVAIL item THEN DO:
             create ttAdderLookup.
             assign  

                 ttAdderLookup.vIno         = ITEM.i-no
                 ttAdderLookup.vName        = ITEM.i-name 
                  
                 .
                
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH item WHERE  item.company = prmComp and  (item.industry = prmIndustry or prmIndustry = "") 
            AND ITEM.i-no BEGINS prmText  and item.mat-type = "A" NO-LOCK :
         IF AVAIL item THEN DO:
             create ttAdderLookup.
             assign  

                 ttAdderLookup.vIno         = ITEM.i-no
                 ttAdderLookup.vName        = ITEM.i-name 
                  
                 .

           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "name" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH item WHERE  item.company = prmComp and  (item.industry = prmIndustry or prmIndustry = "") 
            AND ITEM.i-name = prmText  and item.mat-type = "A" NO-LOCK :
         IF AVAIL item THEN DO:
             create ttAdderLookup.
             assign  

                 ttAdderLookup.vIno         = ITEM.i-no
                 ttAdderLookup.vName        = ITEM.i-name 
                  
                 .
                
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH item WHERE  item.company = prmComp and  (item.industry = prmIndustry or prmIndustry = "") 
            AND ITEM.i-name BEGINS prmText  and item.mat-type = "A" NO-LOCK :
         IF AVAIL item THEN DO:
             create ttAdderLookup.
             assign  

                 ttAdderLookup.vIno         = ITEM.i-no
                 ttAdderLookup.vName        = ITEM.i-name 
                 
                 .

           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


