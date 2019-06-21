/*------------------------------------------------------------------------
    File      : PrepLook
    Purpose   :  Corrugated Prep Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 2 march 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttPrepLook NO-UNDO 
        FIELD vCode         AS character
        FIELD vDscr        AS CHARACTER
        FIELD vMattype        AS CHARACTER
        FIELD vCost         AS DECIMAL
        FIELD vM            AS CHAR
        FIELD vSimon       AS CHAR
        FIELD vMkup        AS DECIMAL
        FIELD vAmtz        AS DECIMAL
        .
    
DEFINE DATASET dsPrepLook FOR ttPrepLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPrepLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".

DEFINE VAR v-count AS INT NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    v-count = 0 . 
    FOR EACH prep WHERE  prep.company = prmComp NO-LOCK :
         IF AVAIL prep THEN DO:
             create ttPrepLook.
             assign  

                 ttPrepLook.vCode            = prep.code
                 ttPrepLook.vDscr            = prep.dscr 
                 ttPrepLook.vMattype         = prep.mat-type
                 ttPrepLook.vCost            = prep.cost     
                 ttPrepLook.vM               = IF  prep.ml  = TRUE THEN "M" ELSE "L"  
                 ttPrepLook.vSimon           = prep.simon
                 ttPrepLook.vMkup            = prep.mkup     
                 ttPrepLook.vAmtz            = prep.amtz     .
                                            
             v-count = v-count + 1 .
             IF v-count > 100 THEN LEAVE.

         END.	 /* IF AVAIL eb */
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "code" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH prep WHERE  prep.company = prmComp AND prep.CODE = prmText NO-LOCK :
         IF AVAIL prep THEN DO:
             create ttPrepLook.
             assign  

                 ttPrepLook.vCode            = prep.code
                 ttPrepLook.vDscr            = prep.dscr 
                 ttPrepLook.vMattype         = prep.mat-type
                 ttPrepLook.vCost            = prep.cost     
                 ttPrepLook.vM               = IF  prep.ml  = TRUE THEN "M" ELSE "L"  
                 ttPrepLook.vSimon           = prep.simon
                 ttPrepLook.vMkup            = prep.mkup     
                 ttPrepLook.vAmtz            = prep.amtz     .

                 
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH prep WHERE  prep.company = prmComp AND prep.CODE BEGINS prmText NO-LOCK :
         IF AVAIL prep THEN DO:
             create ttPrepLook.
             assign  

                ttPrepLook.vCode            = prep.code
                 ttPrepLook.vDscr            = prep.dscr 
                 ttPrepLook.vMattype         = prep.mat-type
                 ttPrepLook.vCost            = prep.cost     
                 ttPrepLook.vM               = IF  prep.ml  = TRUE THEN "M" ELSE "L"  
                 ttPrepLook.vSimon           = prep.simon
                 ttPrepLook.vMkup            = prep.mkup     
                 ttPrepLook.vAmtz            = prep.amtz     .


           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "dscr" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH prep WHERE  prep.company = prmComp AND prep.dscr = prmText NO-LOCK :
         IF AVAIL prep THEN DO:
             create ttPrepLook.
             assign  

                ttPrepLook.vCode            = prep.code
                 ttPrepLook.vDscr            = prep.dscr 
                 ttPrepLook.vMattype         = prep.mat-type
                 ttPrepLook.vCost            = prep.cost     
                 ttPrepLook.vM               = IF  prep.ml  = TRUE THEN "M" ELSE "L"  
                 ttPrepLook.vSimon           = prep.simon
                 ttPrepLook.vMkup            = prep.mkup     
                 ttPrepLook.vAmtz            = prep.amtz     .

                
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH prep WHERE  prep.company = prmComp AND prep.dscr BEGINS prmText NO-LOCK :
         IF AVAIL prep THEN DO:
             create ttPrepLook.
             assign  

                ttPrepLook.vCode            = prep.code
                 ttPrepLook.vDscr            = prep.dscr 
                 ttPrepLook.vMattype         = prep.mat-type
                 ttPrepLook.vCost            = prep.cost     
                 ttPrepLook.vM               = IF  prep.ml  = TRUE THEN "M" ELSE "L"  
                 ttPrepLook.vSimon           = prep.simon
                 ttPrepLook.vMkup            = prep.mkup     
                 ttPrepLook.vAmtz            = prep.amtz     .

           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


