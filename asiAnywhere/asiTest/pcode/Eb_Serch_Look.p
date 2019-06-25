
/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttEbSerchLook NO-UNDO 
        FIELD Estimate AS CHARACTER
        FIELD Customer AS CHARACTER
        FIELD Type     AS INTEGER  
        FIELD hnjdkl   AS CHAR 
        FIELD mnnnbkkl  AS CHAR
          .
DEFINE DATASET dsEbSerchLook  FOR ttEbSerchLook .

DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmStat      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEbSerchLook.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .
IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmCust        = ? THEN ASSIGN prmCust        = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


if prmAction <> "search" then do:
MESSAGE "user" prmUser.
    FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
        IF prmStat = "open" THEN DO:
        OE-LOOP:
        FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.est-no <> "" AND (oe-ordl.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) OR prmText = "")
                AND oe-ordl.opened = YES NO-LOCK,
            first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes AND oe-ordl.stat NE "C",
         EACH eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND  eb.est-no = oe-ordl.est-no  no-lock:
            FIND FIRST ttEbSerchLook WHERE ttEbSerchLook.Estimate = eb.est-no NO-LOCK NO-ERROR.
            IF AVAIL ttEbSerchLook THEN NEXT OE-LOOP.
            create ttEbSerchLook.
            assign                                     
                ttEbSerchLook.Estimate    = eb.est-no
                ttEbSerchLook.Customer    = eb.cust-no 
                ttEbSerchLook.Type        = eb.est-type
                .                       
        END.	 /* FOR EACH eb */
        
        END. /* prmStat = open*/
         IF prmStat = "closed" THEN DO:
        OE-LOOP:
        FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.est-no <> "" AND (oe-ordl.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) OR prmText = "")
                AND oe-ordl.opened = NO NO-LOCK,
            first oe-ord of oe-ordl no-lock where oe-ord.opened eq no OR oe-ordl.stat EQ "C",
         EACH eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND  eb.est-no = oe-ordl.est-no  no-lock:
            FIND FIRST ttEbSerchLook WHERE ttEbSerchLook.Estimate = eb.est-no NO-LOCK NO-ERROR.
            IF AVAIL ttEbSerchLook THEN NEXT OE-LOOP.
            create ttEbSerchLook.
            assign                                     
                ttEbSerchLook.Estimate    = eb.est-no
                ttEbSerchLook.Customer    = eb.cust-no 
                ttEbSerchLook.Type        = eb.est-type
                .                       
        END.	 /* FOR EACH eb */
        
        END. /* prmStat = open*/
         IF prmStat = "pending" THEN DO:
        OE-LOOP:
        FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.est-no <> "" AND (oe-ordl.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) OR prmText = "")
                AND oe-ordl.opened = YES   NO-LOCK,  
            first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes and oe-ord.stat eq "w",
            EACH eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND  eb.est-no = oe-ordl.est-no  no-lock:
            FIND FIRST ttEbSerchLook WHERE ttEbSerchLook.Estimate = eb.est-no NO-LOCK NO-ERROR.
            IF AVAIL ttEbSerchLook THEN NEXT OE-LOOP.
            create ttEbSerchLook.
            assign                                     
                ttEbSerchLook.Estimate    = eb.est-no
                ttEbSerchLook.Customer    = eb.cust-no 
                ttEbSerchLook.Type        = eb.est-type
                .                       
        END.	 /* FOR EACH eb */
        
        END. /* prmStat = open*/
         IF prmStat = "any" THEN DO:
        OE-LOOP:
        FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.est-no <> "" AND (oe-ordl.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) OR prmText = "")
                 NO-LOCK,
         EACH eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no AND  eb.est-no = oe-ordl.est-no  no-lock:
            FIND FIRST ttEbSerchLook WHERE ttEbSerchLook.Estimate = eb.est-no NO-LOCK NO-ERROR.
            IF AVAIL ttEbSerchLook THEN NEXT OE-LOOP.
            create ttEbSerchLook.
            assign                                     
                ttEbSerchLook.Estimate    = eb.est-no
                ttEbSerchLook.Customer    = eb.cust-no 
                ttEbSerchLook.Type        = eb.est-type
                .                       
        END.	 /* FOR EACH eb */
        
        END. /* prmStat = open*/
    END. /*FOR EACH usercust WHERE*/
 END.  /*if prmAction <> "search" */





