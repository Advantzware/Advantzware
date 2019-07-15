

 



/*------------------------------------------------------------------------
    File        : InvoicePo.p
    Purpose     : PoInquiry

    Syntax      :

    Description : Return a Dataset of all PO Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : jan 03,2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{InvoicePo.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  as Character  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPo        AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvoicePo.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

IF prmItem = ? THEN ASSIGN prmItem = "".
IF prmPo = ? THEN ASSIGN prmPo = "".
IF prmUser = ? THEN ASSIGN prmUser = "".

/* ********************  Preprocessor Definitions  ******************** */

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "select" THEN DO:
    FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
        oe-ordl.ord-no = int(prmOrderNum) AND oe-ordl.i-no = prmItem NO-LOCK,
        FIRST po-ordl WHERE po-ordl.company = oe-ordl.company
                             AND po-ordl.i-no = oe-ordl.i-no 
                              AND po-ordl.po-no = int(prmPo) NO-LOCK,
        FIRST po-ord WHERE po-ord.po-no = po-ordl.po-no 
                          AND po-ord.company = po-ordl.company NO-LOCK:
      
        create ttInvoicePo.
        assign 
            ttInvoicePo.po-no           = po-ord.po-no
            ttInvoicePo.po-date         = po-ord.po-date
            ttInvoicePo.TYPE            = po-ord.type 
            ttInvoicePo.stat            = po-ord.stat
            ttInvoicePo.msf             = v-tot-msf
            ttInvoicePo.loc             = po-ord.loc
            ttInvoicePo.i-no            = po-ordl.i-no
            ttInvoicePo.i-name          = po-ordl.i-name
            ttInvoicePo.cust-no         = po-ordl.cust-no
            ttInvoicePo.part            = fi_cust-part
            ttInvoicePo.due-date1       = po-ordl.due-date
            ttInvoicePo.ord-qty         = po-ordl.ord-qty
            ttInvoicePo.cost            = po-ordl.cost
            ttInvoicePo.tot-cost        = po-ordl.t-cost
            ttInvoicePo.job-no          = po-ordl.job-no
            ttInvoicePo.job-no2         = po-ordl.job-no2
            ttInvoicePo.s-num           = po-ordl.s-num
           
              .
         FOR EACH reftable WHERE reftable.reftable = "AP-INVL" AND reftable.company = ""
                         and reftable.loc = "" NO-LOCK,
      EACH ap-invl WHERE ap-invl.company EQ prmComp AND
           ap-invl.i-no eq int(reftable.code2) and
           ap-invl.po-no = po-ordl.po-no AND
           (ap-invl.line + (ap-invl.po-no * -1000)) eq po-ordl.line   NO-LOCK, 
      FIRST ap-inv WHERE ap-inv.i-no EQ ap-invl.i-no NO-LOCK:
             ASSIGN
                 ttInvoicePo.inv-no          = ap-inv.inv-no
                 ttInvoicePo.inv-date        = ap-inv.inv-date
                 ttInvoicePo.Dscr            = get-acct-dscr()
                 ttInvoicePo.qty             = ap-invl.qty 
                 ttInvoicePo.uom             = ap-invl.cons-uom 
                 ttInvoicePo.amt             = ap-invl.amt
         .
       END.
    END. /* For Each oe-ordl*/
END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/



