
/*------------------------------------------------------------------------
    File         : ItemPartLook
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemPartLook NO-UNDO 
    FIELD Part AS CHARACTER
    FIELD cust-no AS CHAR
    FIELD partdscr1 AS CHARACTER
    FIELD qtyhand AS DECIMAL
    FIELD kkkddddf AS CHAR
   .

DEFINE DATASET dsItemPartLook FOR ttItemPartLook .

DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustPart  AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemPartLook.

IF prmAction      = ? THEN ASSIGN prmAction    = "".
IF prmUser        = ? THEN ASSIGN prmUser      = "".
IF prmCust        = ? THEN ASSIGN prmCust      = "".
IF prmCustPart    = ? THEN ASSIGN prmCustPart  = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
       
if prmAction = "search" then do:
    MESSAGE "part" prmCust prmComp.
    FOR EACH cust WHERE ( cust.company = prmComp  AND cust.cust-no = prmCust) /* OR cust.ACTIVE = "X"*/  NO-LOCK :
    FOR EACH cust-part WHERE  cust-part.company = prmComp AND  cust-part.cust-no = cust.cust-no AND (cust-part.i-no = prmCustPart )  NO-LOCK:
        /* FIND FIRST ttItemPartLook WHERE ttItemPartLook.Part = cust-part.part-no NO-LOCK NO-ERROR.
                     IF AVAIL ttItemPartLook THEN  NEXT.*/
            create ttItemPartLook.
            assign  
                ttItemPartLook.cust-no      = cust-part.cust-no          
                ttItemPartLook.Part         = cust-part.part-no  
                ttItemPartLook.partdscr1    = cust-part.part-dscr1   
                                    .                       
         END.	 /* FOR EACH cust-part */
    END.  /* for each cust*/
   
END.  /*ifif prmAction <> "search" */

