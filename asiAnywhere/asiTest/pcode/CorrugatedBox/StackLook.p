/*------------------------------------------------------------------------
    File      : StackLook
    Purpose   :  Corrugated Stack Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 18 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttStackLook NO-UNDO 
        FIELD vCode         AS character
        FIELD vDscr        AS CHARACTER
        FIELD vStacks        AS CHARACTER
        FIELD vStraps     AS CHARACTER
        FIELD vStrapCode  AS CHAR
        FIELD vFormula    AS CHAR
        FIELD vPatten       AS CHAR
        
        .
    
DEFINE DATASET dsStackLook FOR ttStackLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsStackLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmComp        = ? THEN ASSIGN prmComp    = "".

DEFINE VAR v-count AS INT NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    v-count = 0 . 
     FOR EACH reftable WHERE reftable.reftable = "STACK" and reftable.company = "" AND reftable.loc = "" NO-LOCK, 
        first strap  where strap.reftable = "STACKSTRAP" and strap.company = "" 
         and strap.loc = ""  and strap.code = reftable.code no-lock, 
         FIRST pattern OUTER-JOIN where pattern.reftable = "STACKPAT"  
           and pattern.company = ""  and pattern.loc = ""  and pattern.code = reftable.code NO-LOCK :
             create ttStackLook.
             assign  

                 ttStackLook.vCode         = reftable.code
                 ttStackLook.vDscr         = reftable.dscr 
                 ttStackLook.vStacks       = reftable.val[1]
                 ttStackLook.vStraps       = strap.val[1] 
                 ttStackLook.vStrapCode    = strap.code2
                 ttStackLook.vFormula      = strap.dscr 
                 ttStackLook.vPatten       = pattern.dscr
                 
                 .
                 
             v-count = v-count + 1 .
             IF v-count > 100 THEN LEAVE.

         END.	 /* IF AVAIL reftable */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "stcode" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH reftable WHERE reftable.reftable = "STACK" and reftable.company = ""
             AND reftable.loc = "" AND reftable.CODE = prmText NO-LOCK, 
        first strap  where strap.reftable = "STACKSTRAP" and strap.company = "" 
         and strap.loc = ""  and strap.code = reftable.code no-lock, 
         FIRST pattern OUTER-JOIN where pattern.reftable = "STACKPAT"  
           and pattern.company = ""  and pattern.loc = ""  and pattern.code = reftable.code NO-LOCK :
             create ttStackLook.
             assign  

                 ttStackLook.vCode         = reftable.code
                 ttStackLook.vDscr         = reftable.dscr 
                 ttStackLook.vStacks       = reftable.val[1]
                 ttStackLook.vStraps       = strap.val[1] 
                 ttStackLook.vStrapCode    = strap.code2
                 ttStackLook.vFormula      = strap.dscr 
                 ttStackLook.vPatten       = pattern.dscr
                 
                 .
                
             END. /*if avail item*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH reftable WHERE reftable.reftable = "STACK" and reftable.company = ""
         AND reftable.loc = ""  AND reftable.CODE BEGINS prmText NO-LOCK, 
        first strap  where strap.reftable = "STACKSTRAP" and strap.company = "" 
         and strap.loc = ""  and strap.code = reftable.code no-lock, 
         FIRST pattern OUTER-JOIN where pattern.reftable = "STACKPAT"  
           and pattern.company = ""  and pattern.loc = ""  and pattern.code = reftable.code NO-LOCK :
             create ttStackLook.
             assign  

                 ttStackLook.vCode         = reftable.code
                 ttStackLook.vDscr         = reftable.dscr 
                 ttStackLook.vStacks       = reftable.val[1]
                 ttStackLook.vStraps       = strap.val[1] 
                 ttStackLook.vStrapCode    = strap.code2
                 ttStackLook.vFormula      = strap.dscr 
                 ttStackLook.vPatten       = pattern.dscr
                 
                 .

           END. /*if avail eb*/
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "dscr" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH reftable WHERE reftable.reftable = "STACK" and reftable.company = ""
             AND reftable.loc = "" AND reftable.dscr = prmText  NO-LOCK, 
        first strap  where strap.reftable = "STACKSTRAP" and strap.company = "" 
         and strap.loc = ""  and strap.code = reftable.code no-lock, 
         FIRST pattern OUTER-JOIN where pattern.reftable = "STACKPAT"  
           and pattern.company = ""  and pattern.loc = ""  and pattern.code = reftable.code NO-LOCK :
             create ttStackLook.
             assign  

                 ttStackLook.vCode         = reftable.code
                 ttStackLook.vDscr         = reftable.dscr 
                 ttStackLook.vStacks       = reftable.val[1]
                 ttStackLook.vStraps       = strap.val[1] 
                 ttStackLook.vStrapCode    = strap.code2
                 ttStackLook.vFormula      = strap.dscr 
                 ttStackLook.vPatten       = pattern.dscr
                 
                 .
                
             END. /*if avail item*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH reftable WHERE reftable.reftable = "STACK" and reftable.company = "" 
        AND reftable.loc = "" AND reftable.dscr BEGINS prmText  NO-LOCK, 
        first strap  where strap.reftable = "STACKSTRAP" and strap.company = "" 
         and strap.loc = ""  and strap.code = reftable.code no-lock, 
         FIRST pattern OUTER-JOIN where pattern.reftable = "STACKPAT"  
           and pattern.company = ""  and pattern.loc = ""  and pattern.code = reftable.code NO-LOCK :
             create ttStackLook.
             assign  

                 ttStackLook.vCode         = reftable.code
                 ttStackLook.vDscr         = reftable.dscr 
                 ttStackLook.vStacks       = reftable.val[1]
                 ttStackLook.vStraps       = strap.val[1] 
                 ttStackLook.vStrapCode    = strap.code2
                 ttStackLook.vFormula      = strap.dscr 
                 ttStackLook.vPatten       = pattern.dscr
                 .

           END. /*if avail reftable*/
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/


