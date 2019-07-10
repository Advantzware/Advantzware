
/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{PartLook.i}
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPartLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCust        = ? THEN ASSIGN prmCust        = "".
IF prmItem        = ? THEN ASSIGN prmItem = "".

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
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
    MAIN-LOOP:
   FOR EACH cust-part WHERE  cust-part.company = prmComp AND  (cust-part.cust-no = prmCust OR prmCust = "" )
        AND (cust-part.i-no = prmItem OR prmItem = "")  NO-LOCK:
        IF LOOKUP(cust-part.cust-no, custcount) = 0 THEN NEXT MAIN-LOOP .
        FIND FIRST ttPartLook WHERE ttPartLook.ino = cust-part.i-no NO-LOCK NO-ERROR.
        IF AVAIL ttPartLook THEN  NEXT.
           create ttPartLook.
            assign                                     
                
                ttPartLook.ino          = cust-part.i-no             
                ttPartLook.iname        = cust-part.i-name 
                ttPartLook.cust-no      = cust-part.cust-no          
                ttPartLook.Part         = cust-part.part-no 
                ttPartLook.Part2         = cust-part.part-no 
                    .                       
         END.	 /* FOR EACH oe-ordl */
         FOR EACH ttPartLook NO-LOCK:
        IF INDEX(ttPartLook.Part ,'"',1) > 0 THEN ASSIGN
            ttPartLook.Part  = REPLACE(ttPartLook.Part ,'"',":").
         END.
   
    
END.  /*ifif prmAction <> "search" */

IF prmAction = "search" then do:
    
if prmField = "part-no" then do:
     if prmCondition = "EQUAL" then do:
         MAIN-LOOP:
           FOR EACH cust-part WHERE  cust-part.company = prmComp AND (cust-part.cust-no = prmCust OR prmCust = "") AND
                (cust-part.i-no = prmItem OR prmItem = "") AND cust-part.part-no EQ prmText  NO-LOCK:
               IF LOOKUP(cust-part.cust-no, custcount) = 0 THEN NEXT MAIN-LOOP .
         create ttPartLook.
            assign                                     
                ttPartLook.ino          = cust-part.i-no             
                ttPartLook.iname        = cust-part.i-name 
                ttPartLook.cust-no      = cust-part.cust-no          
                ttPartLook.Part         = cust-part.part-no
                ttPartLook.Part2         = cust-part.part-no 
                           .    
           END.  /*FOR EACH oe-ordl*/
           FOR EACH ttPartLook NO-LOCK:
        IF INDEX(ttPartLook.Part ,'"',1) > 0 THEN ASSIGN
            ttPartLook.Part  = REPLACE(ttPartLook.Part ,'"',":").
         END.
         
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
        MAIN-LOOP:
           FOR EACH cust-part WHERE  cust-part.company = prmComp AND (cust-part.cust-no EQ prmCust OR prmCust = "") AND
                (cust-part.i-no = prmItem OR prmItem = "") AND cust-part.part-no BEGINS prmText  NO-LOCK:
               IF LOOKUP(cust-part.cust-no, custcount) = 0 THEN NEXT MAIN-LOOP .
            create ttPartLook.
            assign                                     
                
                    
                ttPartLook.ino          = cust-part.i-no             
                ttPartLook.iname        = cust-part.i-name 
                ttPartLook.cust-no      = cust-part.cust-no          
                ttPartLook.Part         = cust-part.part-no
                ttPartLook.Part2         = cust-part.part-no 
                           . 
           END.
           FOR EACH ttPartLook NO-LOCK:
        IF INDEX(ttPartLook.Part ,'"',1) > 0 THEN ASSIGN
            ttPartLook.Part  = REPLACE(ttPartLook.Part ,'"',":").
         END.
       
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "part-no" then do:*/


END. /* IF prmAction = search then do: */







