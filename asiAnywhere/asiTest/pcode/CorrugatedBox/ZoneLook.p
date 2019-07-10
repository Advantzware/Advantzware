/*------------------------------------------------------------------------
    File      : ZoneLook
    Purpose   :  Corrugated DelZone Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 17 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttDelZoneLook NO-UNDO 
        FIELD vZon         AS character
        FIELD vZonDscr        AS CHARACTER
       .
    
DEFINE DATASET dsDelZoneLook FOR ttDelZoneLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCarrier      AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsDelZoneLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmCarrier     = ? THEN ASSIGN prmCarrier     = "".


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
    FOR EACH carr-mtx WHERE  carr-mtx.company = prmComp /*and 
        carr-mtx.loc = ip-loc and*/ AND  (carr-mtx.carrier = prmCarrier OR prmCarrier = "") NO-LOCK :
         IF AVAIL carr-mtx THEN DO:
             create ttDelZoneLook.
             assign  

                 ttDelZoneLook.vZon           = carr-mtx.del-zone
                 ttDelZoneLook.vZonDscr       = carr-mtx.del-dscr 
                .
                 
             v-count = v-count + 1 .
             IF v-count > 100 THEN LEAVE.

         END.	 /* IF AVAIL carr-mtx */
    END. /* for each carr-mtx */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "zon" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH carr-mtx WHERE  carr-mtx.company = prmComp /*and 
        carr-mtx.loc = ip-loc and*/ AND (carr-mtx.carrier = prmCarrier OR prmCarrier = "") AND carr-mtx.del-zone = prmText  NO-LOCK :
         IF AVAIL carr-mtx THEN DO:
             create ttDelZoneLook.
             assign  
                 ttDelZoneLook.vZon           = carr-mtx.del-zone
                 ttDelZoneLook.vZonDscr       = carr-mtx.del-dscr 
                . 
                
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH carr-mtx WHERE  carr-mtx.company = prmComp /*and 
        carr-mtx.loc = ip-loc and*/ AND (carr-mtx.carrier = prmCarrier OR prmCarrier = "") AND carr-mtx.del-zone BEGINS prmText  NO-LOCK :
         IF AVAIL carr-mtx THEN DO:
             create ttDelZoneLook.
             assign  

                 ttDelZoneLook.vZon           = carr-mtx.del-zone
                 ttDelZoneLook.vZonDscr       = carr-mtx.del-dscr 
                .

           END. /*if avail carr-mtx*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = zon */

IF prmField = "dscr" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH carr-mtx WHERE carr-mtx.company = prmComp /*and 
        carr-mtx.loc = ip-loc and*/ AND (carr-mtx.carrier = prmCarrier OR prmCarrier = "") AND carr-mtx.del-dscr = prmText NO-LOCK :
         IF AVAIL carr-mtx THEN DO:
             create ttDelZoneLook.
             assign  

                 ttDelZoneLook.vZon           = carr-mtx.del-zone
                 ttDelZoneLook.vZonDscr       = carr-mtx.del-dscr 
                .
             END. /*if avail item*/
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH carr-mtx WHERE  carr-mtx.company = prmComp /*and 
        carr-mtx.loc = ip-loc and*/ AND (carr-mtx.carrier = prmCarrier OR prmCarrier = "") AND carr-mtx.del-dscr BEGINS prmText NO-LOCK :
         IF AVAIL carr-mtx THEN DO:
             create ttDelZoneLook.
             assign  

                 ttDelZoneLook.vZon           = carr-mtx.del-zone
                 ttDelZoneLook.vZonDscr       = carr-mtx.del-dscr 
                .

           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = dscr */


END. /*ir prmAction = "search"*/


