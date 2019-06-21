 



/*------------------------------------------------------------------------
    File        : BrwsPo.p
    Purpose     : PoInquiry

    Syntax      :

    Description : Return a Dataset of all PO Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : jan 01,2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{BrwsPo.i}
DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  as Character  NO-UNDO.

DEFINE INPUT PARAMETER prmLine  as Character  NO-UNDO.
DEFINE INPUT PARAMETER prmPo   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVend  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmVendItem   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmDate      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmJob2       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOpen       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmClosed       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBrwsPo.

IF prmUser = ? THEN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmPo = ? THEN ASSIGN prmPo = "".
IF prmVend = ? THEN ASSIGN prmVend = "".
IF prmItem = ? THEN ASSIGN prmItem = "".
IF prmVendItem = ? THEN ASSIGN prmVendItem = "".
IF prmDate = ? THEN ASSIGN prmDate = "".
IF prmJob = ? THEN ASSIGN prmJob = "".
IF prmJob2 = ? THEN ASSIGN prmJob2 = "".
IF prmOpen = ? THEN ASSIGN prmOpen = "".
IF prmClosed = ? THEN ASSIGN prmClosed = "".
/* ********************  Preprocessor Definitions  ******************** */

DEF VAR prmComp AS CHAR NO-UNDO.

FOR EACH ttBrwsPo NO-LOCK:
    DELETE ttBrwsPo.
END.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction = "select" THEN DO:
  FIND FIRST oe-ordl WHERE oe-ordl.ord-no = int(prmOrderNum) AND oe-ordl.i-no = prmItem AND oe-ordl.LINE = int(prmLine) NO-LOCK NO-ERROR. 
   FOR EACH po-ordl WHERE po-ordl.company EQ prmComp  AND
       po-ordl.i-no = oe-ordl.i-no NO-LOCK: 
    FOR EACH  po-ord WHERE po-ordl.po-no = po-ord.po-no AND po-ord.stat <> "c" NO-LOCK : 
 
            create ttBrwsPo.
            assign 
                ttBrwsPo.po-no           = po-ordl.po-no
                ttBrwsPo.vend-no         = po-ord.vend-no
                ttBrwsPo.due-date        = po-ordl.due-date
                ttBrwsPo.job-no          = po-ordl.job-no
                ttBrwsPo.job-no2         = po-ordl.job-no2
                ttBrwsPo.i-no            = po-ordl.i-no
                ttBrwsPo.s-num           = po-ordl.s-num
                ttBrwsPo.i-name          = po-ordl.i-name
                ttBrwsPo.s-wid           = po-ordl.s-wid
                ttBrwsPo.s-len           = po-ordl.s-len
                ttBrwsPo.vend-i-no       = po-ordl.vend-i-no
                ttBrwsPo.ord-qty         = po-ordl.ord-qty
               /* ttBrwsPo.rece-qty        = */
                ttBrwsPo.Orduom          = po-ordl.pr-qty-uom
                ttBrwsPo.recqty          = po-ordl.t-rec-qty
                ttBrwsPo.cost            = po-ordl.cost
                ttBrwsPo.uom             = po-ordl.pr-uom
                ttBrwsPo.buyer           = po-ord.buyer
                ttBrwsPo.stat            = po-ord.stat
                .
        END.  /*FOR EACH po-ordl */
   END.
END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/
 
/*dynamic query*/
IF prmAction = "Search" THEN DO:
   
    FOR EACH po-ordl WHERE  po-ordl.company EQ prmComp AND
                           (po-ordl.po-no        = INT(prmPo)    or int(prmPo)  = 0 )
                           AND (po-ordl.i-no      BEGINS prmItem        or prmItem = "" )
                           AND (po-ordl.vend-i-no BEGINS prmVendItem     or prmVendItem  = "" )
                           AND (po-ordl.due-date  >= DATE(prmDate)        or prmDate     = "" )
                           AND (po-ordl.job-no    = prmJob        or prmJob     = "" )
                           AND (po-ordl.job-no2   = int(prmJob2)         or int(prmJob2) = 0 )
                           AND ((po-ordl.opened AND prmOpen = "yes")  OR  (NOT po-ordl.opened AND prmClosed = "Yes"))
                          
                             NO-LOCK, 
            FIRST po-ord WHERE
             po-ord.company EQ po-ordl.company AND
             po-ord.po-no   EQ po-ordl.po-no AND
             (po-ord.vend-no = prmVend OR prmVend = "") NO-LOCK:
            create ttBrwsPo.
            assign 
                ttBrwsPo.po-no           = po-ordl.po-no
                ttBrwsPo.vend-no         = po-ord.vend-no
                ttBrwsPo.due-date        = po-ordl.due-date
                ttBrwsPo.job-no          = po-ordl.job-no
                ttBrwsPo.job-no2         = po-ordl.job-no2
                ttBrwsPo.i-no            = po-ordl.i-no
                ttBrwsPo.s-num           = po-ordl.s-num
                ttBrwsPo.i-name          = po-ordl.i-name
                ttBrwsPo.s-wid           = po-ordl.s-wid
                ttBrwsPo.s-len           = po-ordl.s-len
                ttBrwsPo.vend-i-no       = po-ordl.vend-i-no
                ttBrwsPo.ord-qty         = po-ordl.ord-qty
               /* ttBrwsPo.rece-qty        = */
                ttBrwsPo.uom             = po-ordl.pr-qty-uom
                ttBrwsPo.recqty          = po-ordl.t-rec-qty
                ttBrwsPo.cost            = po-ordl.cost
                ttBrwsPo.uom             = po-ordl.pr-uom
                ttBrwsPo.buyer           = po-ord.buyer
                ttBrwsPo.stat            = po-ord.stat
                .
        END.  /*FOR EACH po-ordl */
    END.   /*if prmAction = "Search"*/
/*************************************************/

  

