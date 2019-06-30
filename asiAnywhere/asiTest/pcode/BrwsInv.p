


/*------------------------------------------------------------------------
    File        : Inventory.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{BrwsInv.i}
DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  as Character  NO-UNDO.
DEFINE INPUT PARAMETER prmInvoice   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAccount   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPart      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustPo    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBOL       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDate       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOpen       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPaid       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBrwsInv.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VARIABLE prmComp        AS CHARACTER NO-UNDO.

IF prmUser = ? THEN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmInvoice = ? THEN ASSIGN prmInvoice = "".
IF prmCustomer = ? THEN ASSIGN prmCustomer = "".
IF prmItem = ? THEN ASSIGN prmItem = "".
IF prmAccount = ? THEN ASSIGN prmAccount = "".
IF prmPart = ? THEN ASSIGN prmPart = "".
IF prmCustPo = ? THEN ASSIGN prmCustPo = "".
IF prmBOL = ? THEN ASSIGN prmBOL = "".
IF prmEstimate = ? THEN ASSIGN prmEstimate = "".
IF prmDate = ? THEN ASSIGN prmDate = "".
IF prmOpen = ? THEN ASSIGN prmOpen = "".
IF prmPaid = ? THEN ASSIGN prmPaid = "".  
/* ********************  Preprocessor Definitions  ******************** */

FOR EACH ttBrwsInv NO-LOCK:
    DELETE ttBrwsInv.
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "select" THEN DO:
    FIND FIRST oe-ordl where prmComp EQ prmComp AND
        oe-ordl.ord-no = int(prmOrderNum) NO-LOCK NO-ERROR.    
        FOR EACH ar-invl WHERE ar-invl.company = oe-ordl.company
                           AND ar-invl.ord-no  = oe-ordl.ord-no
                         NO-LOCK, 
            EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no NO-LOCK:
            
            
            create ttBrwsInv.
            assign 
                ttBrwsInv.inv-no         = ar-invl.inv-no
                ttBrwsInv.bol-no         = ar-invl.bol-no
                ttBrwsInv.cust-no        = ar-invl.cust-no
                ttBrwsInv.inv-date       = ar-inv.inv-date
                ttBrwsInv.act-num        = ar-invl.actnum
                ttBrwsInv.i-no           = ar-invl.i-no
                ttBrwsInv.part-no        = ar-invl.part-no
                ttBrwsInv.ord-no         = ar-invl.ord-no
                ttBrwsInv.po-no          = ar-invl.po-no
                ttBrwsInv.est-no         = ar-invl.est-no
                ttBrwsInv.price          = oe-ordl.price
                ttBrwsInv.costs          = ar-invl.cost
                .

        END.  /*FOR EACH ar-invl */
  
END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/

/*make into dynamic inquiry*/
IF prmAction = "Search" THEN DO:
    FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
             oe-ordl.ord-no = int(prmOrderNum) NO-LOCK:    
        FOR EACH ar-invl WHERE ar-invl.company EQ prmComp AND
                           (ar-invl.inv-no   = int(prmInvoice)    or int(prmInvoice)  = 0 )
                           AND (ar-invl.cust-no  >= prmCustomer    or prmCustomer = "" )
                           AND (ar-invl.i-no     BEGINS prmItem        or prmItem = "" )
                           AND (ar-invl.actnum   >= prmAccount     or prmAccount  = "" )
                           AND (ar-invl.part-no  >= prmPart        or prmPart     = "" )
                           AND (ar-invl.po-no  >= prmCustPo        or prmCustPo     = "" )
                           AND (ar-invl.bol-no   = int(prmBOL)         or int(prmBOL) = 0 )
                           AND (ar-invl.est-no   >= prmEstimate         or prmEStimate = "" )
                           AND ar-invl.company = oe-ordl.company
                           AND ar-invl.ord-no = oe-ordl.ord-no 
                           AND ar-invl.i-no   = oe-ordl.i-no NO-LOCK, 
            FIRST ar-inv WHERE ar-inv.x-no = ar-invl.x-no 
                          AND (ar-inv.inv-date = date(prmDate)       or prmDate     = "" ) 
                          AND ((ar-inv.due > 0 AND  prmOpen = "yes")OR (ar-inv.due < 0 AND  prmPaid = "yes" )) NO-LOCK:
            create ttBrwsInv.
            assign 
                ttBrwsInv.inv-no         = ar-invl.inv-no
                ttBrwsInv.bol-no         = ar-invl.bol-no
                ttBrwsInv.cust-no        = ar-invl.cust-no
                ttBrwsInv.inv-date       = ar-inv.inv-date
                ttBrwsInv.act-num        = ar-invl.actnum
                ttBrwsInv.i-no           = ar-invl.i-no
                ttBrwsInv.part-no        = ar-invl.part-no
                ttBrwsInv.ord-no         = ar-invl.ord-no
                ttBrwsInv.po-no          = ar-invl.po-no
                ttBrwsInv.est-no         = ar-invl.est-no
                ttBrwsInv.price          = oe-ordl.price
                ttBrwsInv.costs          = ar-invl.cost
                .
        END.  /*FOR EACH ar-invl */
    END. /* For Each oe-ordl*/
END.   /*if prmAction = "Search"*/
/*************************************************/
