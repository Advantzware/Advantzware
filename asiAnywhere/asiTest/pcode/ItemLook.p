
/*------------------------------------------------------------------------
    File         : itemLookup
    Purpose     : item lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ItemLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmStat      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmStat      = ? THEN ASSIGN prmStat      = "".
IF prmField      = ? THEN ASSIGN prmField      = "".
IF prmCust      = ? THEN ASSIGN prmCust      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE custcount AS CHARACTER NO-UNDO .
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

if prmAction <> "search" then do:
    FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
        IF prmStat = "open" THEN DO:
            OE-LOOP:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no
                AND oe-ordl.opened = YES NO-LOCK:
                IF oe-ordl.i-no = "" THEN NEXT OE-LOOP.
            FIND FIRST ttItemLook WHERE ttItemLook.Item = oe-ordl.i-no NO-LOCK NO-ERROR.
            IF AVAIL ttItemLook THEN NEXT OE-LOOP.
                create ttItemLook.
                assign                                         
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                .
            END.	 /* FOR EACH itemfg */
        END.   /*if prmStat = ""open*/
        IF prmStat = "closed" THEN DO:
            OE-LOOP:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no 
                AND oe-ordl.opened = NO NO-LOCK:
                IF oe-ordl.i-no = "" THEN NEXT OE-LOOP.
                FIND FIRST ttItemLook WHERE ttItemLook.Item = oe-ordl.i-no NO-LOCK NO-ERROR.
               IF AVAIL ttItemLook THEN NEXT OE-LOOP.
                create ttItemLook.
                assign                                         
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                    .
            END.	 /* FOR EACH itemfg */
        END.
        IF prmStat = "pending" THEN DO:
            OE-LOOP:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no 
                AND oe-ordl.opened = YES NO-LOCK:
                FIND FIRST oe-ord WHERE oe-ord.ord-no = oe-ordl.ord-no 
                    AND oe-ord.stat = 'w' NO-LOCK NO-ERROR.
                FIND FIRST ttItemLook WHERE ttItemLook.Item = oe-ordl.i-no NO-LOCK NO-ERROR.
                IF AVAIL ttItemLook THEN NEXT OE-LOOP.
                create ttItemLook.
                assign                                         
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                    .
            END.	 /* FOR EACH itemfg */
        END.
        IF prmStat = "any" THEN DO:
            OE-LOOP:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no NO-LOCK:
                FIND FIRST ttItemLook WHERE ttItemLook.Item = oe-ordl.i-no NO-LOCK NO-ERROR.
                IF AVAIL ttItemLook THEN NEXT OE-LOOP.
                create ttItemLook.
                assign                                         
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no.
            END.	 /* FOR EACH itemfg */
        END.
    END.
END.  /*if prmAction <> "search" then do*/

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH oe-ordl WHERE
                oe-ordl.company = prmComp AND (oe-ordl.cust-no = prmCust OR prmCust = "")
                AND LOOKUP(oe-ordl.cust-no, custcount ) NE 0
                no-lock:

                IF (oe-ordl.i-no = prmText or
                    oe-ordl.i-name = prmText or 
                    oe-ordl.i-dscr = prmText) THEN
                DO:
                   create ttItemLook.
                   assign                                                            
                       ttItemLook.Item = oe-ordl.i-no
                       ttItemLook.Name = oe-ordl.i-name 
                       ttItemLook.Dscr = oe-ordl.i-dscr
                       ttItemLook.cust-no = oe-ordl.cust-no.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH oe-ordl WHERE
                oe-ordl.company = prmComp AND(oe-ordl.cust-no = prmCust OR prmCust = "")
                AND LOOKUP(oe-ordl.cust-no, custcount ) NE 0
                no-lock:

                IF (oe-ordl.i-no begins prmText or
                    oe-ordl.i-name begins prmText or 
                    oe-ordl.i-dscr begins prmText) THEN
                DO:
                   create ttItemLook.
                   assign                 
                       ttItemLook.Item = oe-ordl.i-no
                       ttItemLook.Name = oe-ordl.i-name 
                       ttItemLook.Dscr = oe-ordl.i-dscr
                       ttItemLook.cust-no = oe-ordl.cust-no.
                END.
             END.  /*FOR EACH oe-ordl where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/      
    
    IF prmField = "i-no" then do:       

        if prmCondition = "EQUAL" then do:     
            FOR EACH oe-ordl WHERE
                oe-ordl.company = prmComp AND
                oe-ordl.i-no = prmText AND 
                (oe-ordl.cust-no = prmCust OR prmCust = "") AND
                LOOKUP(oe-ordl.cust-no, custcount ) NE 0 no-lock:
                create ttItemLook.
                assign                 
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                    
                    .
            END. /*FOR EACH oe-ordl where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp AND
                oe-ordl.i-no begins prmText AND (oe-ordl.cust-no BEGINS prmCust OR prmCust = "")
                AND LOOKUP(oe-ordl.cust-no, custcount ) NE 0 no-lock:
                create ttItemLook.
                assign                                                      
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                    .
            END. /*FOR EACH oe-ordl where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
    if prmField = "i-name"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp AND
                oe-ordl.i-name = prmText AND (oe-ordl.cust-no = prmCust OR prmCust = "")
                AND LOOKUP(oe-ordl.cust-no, custcount ) NE 0 no-lock:
                create ttItemLook.
                assign                                         
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                    .
            end. /*FOR EACH item-fg where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp AND  oe-ordl.i-name begins prmText
                              AND (oe-ordl.cust-no = prmCust OR prmCust = "")
                              AND LOOKUP(oe-ordl.cust-no, custcount ) NE 0 no-lock:
                create ttItemLook.
                assign                                          
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                    .
            end.  /*FOR EACH item-fg wher*/
        end.    /*if prmCondition = BEGIN*/    
    end.  /* if prmField = name  */
    if prmField = "i-dscr"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp AND  oe-ordl.i-dscr = prmText
                 AND (oe-ordl.cust-no = prmCust OR prmCust = "") 
                 AND LOOKUP(oe-ordl.cust-no, custcount ) NE 0 no-lock:
                create ttItemLook.
                assign                                         
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr = oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                    .
            end. /*FOR EACH oe-ordl where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH oe-ordl WHERE oe-ordl.company = prmComp AND  oe-ordl.i-name begins prmText
                 AND (oe-ordl.cust-no = prmCust OR prmCust = "")
                 AND LOOKUP(oe-ordl.cust-no, custcount ) NE 0 no-lock:
                create ttItemLook.
                assign                                          
                    ttItemLook.Item = oe-ordl.i-no
                    ttItemLook.Name = oe-ordl.i-name 
                    ttItemLook.Dscr	= oe-ordl.i-dscr
                    ttItemLook.cust-no = oe-ordl.cust-no
                    .
            end.  /*FOR EACH oe-ordl wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
     
END.  /* IF prmAction = search then do: */



