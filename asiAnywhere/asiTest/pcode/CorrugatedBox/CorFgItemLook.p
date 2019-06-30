
/*------------------------------------------------------------------------
    File      : CorFgItemLook
    Purpose   :  Corrugated Customer Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 2 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorFgItem NO-UNDO 
        FIELD vEst      AS character
        FIELD vCust      AS CHARACTER
        FIELD vPartNum    AS CHARACTER
        FIELD vPartDscr   AS CHARACTER
        FIELD vFgItem     AS CHARACTER
       
        .
    
DEFINE DATASET dsCorFgItem FOR ttCorFgItem .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorFgItem.

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
    FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
      EACH  eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND eb.stock-no <> "" NO-LOCK:
        
         IF AVAIL eb THEN DO:
             create ttCorFgItem.
             assign                                         
                 ttCorFgItem.vEst       = eb.est-no
                 ttCorFgItem.vCust      = eb.cust-no 
                 ttCorFgItem.vPartNum   = eb.part-no
                 ttCorFgItem.vPartDscr  = eb.part-dscr1 
                 ttCorFgItem.vFgItem    = eb.stock-no .
                 
             v-count = v-count + 1 .
             MESSAGE "count" v-count.
             IF v-count > 100 THEN LEAVE.
         END.	 /* IF AVAIL cust */
    END. /* for each cust */
END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" THEN DO:
    IF prmField = "item-no" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
            EACH  eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND eb.stock-no <> "" AND eb.stock-no = prmText NO-LOCK:
         IF AVAIL eb THEN DO:
             create ttCorFgItem.
             assign                                         
                 ttCorFgItem.vEst       =  eb.est-no
                 ttCorFgItem.vCust      = eb.cust-no 
                 ttCorFgItem.vPartNum   = eb.part-no
                 ttCorFgItem.vPartDscr  = eb.part-dscr1 
                 ttCorFgItem.vFgItem    = eb.stock-no .
             
             
             END. /*if avail cust*/
        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL*/


 IF prmCondition = "BEGIN" then do:
    FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
     EACH  eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND eb.stock-no <> "" AND eb.stock-no BEGINS prmText NO-LOCK:
         IF AVAIL eb THEN DO:
             create ttCorFgItem.
             assign                                         
                 ttCorFgItem.vEst       =  eb.est-no
                 ttCorFgItem.vCust      = eb.cust-no 
                 ttCorFgItem.vPartNum   = eb.part-no
                 ttCorFgItem.vPartDscr  = eb.part-dscr1 
                 ttCorFgItem.vFgItem    = eb.stock-no .
             
           END. /*if avail cust*/
     END.  /*for each cust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = cust-no */
END. /*if prmAction = "search"*/


