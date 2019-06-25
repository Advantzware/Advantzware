
 



/*------------------------------------------------------------------------
    File        : ViewPo.p
    Purpose     : PoInquiry

    Syntax      :

    Description : Return a Dataset of all PO Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : jan 03,2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ViewPo.i}
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  as Character  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPo        AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewPo.


DEF VAR prmComp AS CHAR NO-UNDO.

IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

IF prmItem = ? THEN ASSIGN prmItem = "".
IF prmPo = ? THEN ASSIGN prmPo = "".
IF prmUser = ? THEN ASSIGN prmUser = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */

IF prmAction = "select" THEN DO:
    FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
        oe-ordl.ord-no = int(prmOrderNum) AND oe-ordl.i-no = prmItem NO-LOCK,
        FIRST po-ordl WHERE po-ordl.company = oe-ordl.company
                             AND po-ordl.i-no = oe-ordl.i-no 
                              AND po-ordl.po-no = int(prmPo) NO-LOCK,
        FIRST po-ord WHERE po-ord.po-no = po-ordl.po-no 
                          AND po-ord.company = po-ordl.company NO-LOCK, 
        FIRST vend WHERE vend.company EQ po-ord.company 
                        AND vend.vend-no EQ po-ord.vend-no 
                        NO-LOCK:
        create ttViewPo.
        assign 
            ttViewPo.po-no           = po-ord.po-no
            ttViewPo.po-date         = po-ord.po-date
            ttViewPo.TYPE            = po-ord.type 
            ttViewPo.stat            = po-ord.stat
            ttViewPo.vend-no         = po-ord.vend-no
            ttViewPo.vend-name       = vend.name  
            ttViewPo.Vaddr1          = vend.add1  
            ttViewPo.Vaddr2          = vend.add2  
            ttViewPo.Vcity           = vend.city  
            ttViewPo.Vstate          = vend.state 
            ttViewPo.Vzip            = vend.zip
            ttViewPo.VArea-code      = vend.area-code            
            ttViewPo.VPhone          = vend.phone
            ttViewPo.ship-id         = po-ord.ship-id
            ttViewPo.ship-name       = po-ord.ship-name
            ttViewPo.SAddr1          = po-ord.ship-addr[1]
            ttViewPo.SAddr2          = po-ord.ship-addr[2]
            ttViewPo.SCity           = po-ord.ship-city 
            ttViewPo.SState          = po-ord.ship-state
            ttViewPo.SZip            = po-ord.ship-zip  
            ttViewPo.buyer           = po-ord.buyer
            ttViewPo.contact         = po-ord.contact
            ttViewPo.due-date        = po-ord.due-date
            ttViewPo.last-dat        = po-ord.last-ship-date
            ttViewPo.under-pc        = po-ord.under-pct
            ttViewPo.over-pct        = po-ord.over-pct
            ttViewPo.carrier         = po-ord.carrier
            ttViewPo.tax-gr          = po-ord.tax-gr
            ttViewPo.terms           = po-ord.terms
            ttViewPo.frt-pay         = po-ord.frt-pay 
            ttViewPo.fob-code        = po-ord.fob-code
            ttViewPo.t-freight       = po-ord.t-freight
            ttViewPo.tax             = po-ord.tax
            ttViewPo.t-cost          = po-ord.t-cost 
            ttViewPo.i-no            = po-ordl.i-no
            ttViewPo.i-name          = po-ordl.i-name
            ttViewPo.cust-no         = po-ordl.cust-no
            ttViewPo.part            = fi_cust-part
            ttViewPo.due-date1       = po-ordl.due-date
            ttViewPo.ord-qty         = po-ordl.ord-qty
            ttViewPo.cost            = po-ordl.cost
            ttViewPo.tot-cost        = po-ordl.t-cost
            ttViewPo.job-no          = po-ordl.job-no
            ttViewPo.job-no2         = po-ordl.job-no2
            ttViewPo.s-num           = po-ordl.s-num

              .
        
    END. /* For Each oe-ordl*/
END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/


