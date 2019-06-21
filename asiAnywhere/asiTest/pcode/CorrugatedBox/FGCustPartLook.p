/*------------------------------------------------------------------------
    File      : FGCustPartLook
    Purpose   :  Corrugated Customer Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 2 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorFgCustpart NO-UNDO 
        FIELD vEst      AS character
        FIELD vCust      AS CHARACTER
        FIELD vPartNum    AS CHARACTER
        FIELD vPartDscr   AS CHARACTER
        
        .
    
DEFINE DATASET dsCorFgCustpart FOR ttCorFgCustpart .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorFgCustpart.

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
      EACH  eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no NO-LOCK:
        
         IF AVAIL eb THEN DO:
             create ttCorFgCustpart.
             assign  

                 ttCorFgCustpart.vEst       = eb.est-no
                 ttCorFgCustpart.vCust      = eb.cust-no 
                 ttCorFgCustpart.vPartNum   = eb.part-no
                 ttCorFgCustpart.vPartDscr  = eb.part-dscr1 
                 .
                 
             v-count = v-count + 1 .
             MESSAGE "count" v-count.
             IF v-count > 100 THEN LEAVE.

         END.	 /* IF AVAIL eb */
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "part-no" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
            EACH  eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND eb.part-no = prmText NO-LOCK:
         IF AVAIL eb THEN DO:
             create ttCorFgCustpart.
             assign                                         
                 ttCorFgCustpart.vEst       =  eb.est-no
                 ttCorFgCustpart.vCust      = eb.cust-no 
                 ttCorFgCustpart.vPartNum   = eb.part-no
                 ttCorFgCustpart.vPartDscr  = eb.part-dscr1 
                .
             END. /*if avail cust*/
        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
            EACH  eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND eb.part-no BEGINS prmText NO-LOCK:
         IF AVAIL eb THEN DO:
              create ttCorFgCustpart.
                 assign                                         
                 ttCorFgCustpart.vEst       =  eb.est-no
                 ttCorFgCustpart.vCust      = eb.cust-no 
                 ttCorFgCustpart.vPartNum   = eb.part-no
                 ttCorFgCustpart.vPartDscr  = eb.part-dscr1 
                 .

           END. /*if avail eb*/
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */
END. /*ir prmAction = "search"*/


