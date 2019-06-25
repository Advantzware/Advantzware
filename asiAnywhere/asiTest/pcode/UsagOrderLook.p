


/*------------------------------------------------------------------------
    File        : UsagOrderLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all PartLook

    Author(s)   : Jyoti
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUsagOrderLook NO-UNDO 
    FIELD OrderNo        AS INT
    FIELD LineNo          AS INT
    FIELD PartNo       AS CHAR
    FIELD ItemNo        AS CHAR
    FIELD usagQty       AS INT
    FIELD Job           AS CHAR
    FIELD Job2          AS INT
    FIELD Price         AS DECIMAL
   FIELD ffssk AS CHAR
    .


DEFINE DATASET dsUsagOrderLook FOR ttUsagOrderLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrder     AS INT NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUsagOrderLook.


IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmItem      = ? THEN ASSIGN prmItem      = "".
IF prmOrder     = ? THEN ASSIGN prmOrder     = 0.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    MESSAGE "test" prmComp prmItem prmOrder.
   FOR EACH oe-ordl WHERE  oe-ordl.company = prmComp
       AND oe-ordl.ord-no >= prmOrder AND oe-ordl.part-no BEGINS prmItem   NO-LOCK:
            create ttUsagOrderLook.
                assign
                            ttUsagOrderLook.OrderNo   = oe-ordl.ord-no
                            ttUsagOrderLook.LineNo    = oe-ordl.LINE
                            ttUsagOrderLook.PartNo    = oe-ordl.part-no
                            ttUsagOrderLook.ItemNo    = oe-ordl.i-no
                            ttUsagOrderLook.usagQty   = oe-ordl.qty 
                            ttUsagOrderLook.Job     = oe-ordl.job-no
                            ttUsagOrderLook.Job2    = oe-ordl.job-no2
                            ttUsagOrderLook.Price   = oe-ordl.t-price .
                           
  
    END.	 /* FOR EACH eoe- */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "Order"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH oe-ordl WHERE  oe-ordl.company = prmComp
       AND oe-ordl.ord-no >= prmOrder AND oe-ordl.part-no BEGINS prmItem AND oe-ordl.ord-no = prmOrder  NO-LOCK:
            create ttUsagOrderLook.
                assign
                            ttUsagOrderLook.OrderNo   = oe-ordl.ord-no
                            ttUsagOrderLook.LineNo    = oe-ordl.LINE
                            ttUsagOrderLook.PartNo    = oe-ordl.part-no
                            ttUsagOrderLook.ItemNo    = oe-ordl.i-no
                            ttUsagOrderLook.usagQty   = oe-ordl.qty 
                            ttUsagOrderLook.Job     = oe-ordl.job-no
                            ttUsagOrderLook.Job2    = oe-ordl.job-no2
                            ttUsagOrderLook.Price   = oe-ordl.t-price .
                           
  
    END.	 /* FOR EACH eoe- */      
        END. /*if prmCondition = EQUAL */
         
         
     END.  /*IF prmField = est-no */
END.  /* IF prmAction = search then do: */



