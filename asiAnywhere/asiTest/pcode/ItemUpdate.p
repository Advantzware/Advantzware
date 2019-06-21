


/*------------------------------------------------------------------------
    File        : UpdateItem.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUpdateItem NO-UNDO
    FIELD a AS CHAR
    FIELD OrdNo         AS INT 
    FIELD est-no LIKE oe-ordl.est-no
    FIELD job-no LIKE oe-ordl.job-no
    FIELD vLine LIKE oe-ordl.LINE
    FIELD CustPart      AS CHAR FORMAT "x(15)"
    FIELD Item1         AS CHAR FORMAT "x(15)"
    FIELD Name1         AS CHAR FORMAT "x(30)"
    FIELD Dscr          AS CHAR FORMAT "x(30)"
    FIELD quantity      AS DECIMAL FORMAT "->>,>>>,>>99.9<<"
    FIELD price         AS DECIMAL FORMAT "->>,>>>,>>9.99<<"
    FIELD uom           AS CHAR FORMAT "X(4)"
    FIELD counter       AS INTEGER 
    FIELD custpo        AS CHAR FORMAT "x(15)"
    FIELD taxable       AS LOGICAl Initial true 
    FIELD discount      AS DECIMAL FORMAT ">>>,>>9.99" 
    FIELD requested     AS CHAR FORMAT "x(2)"
    FIELD requestdate   AS DATE FORMAT "99/99/9999"
    FIELD extprice      AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD promised      AS CHAR FORMAT ">>>>9"
    FIELD promisdate    AS DATE FORMAT "99/99/9999"
    FIELD shipqty       AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD alloc         AS LOGICAL Initial true 
    FIELD ord-level     AS Decimal FORMAT ">>>,>>>,>>9.999"
    FIELD q-ono         AS Decimal FORMAT  "->>,>>>,>>9.999 "  
    FIELD q-onh         AS Decimal FORMAT "->>,>>>,>>9.999"  
    FIELD q-alloc       AS Decimal FORMAT "->>,>>>,>>9.999"  
    FIELD q-avail       AS Decimal FORMAT "->>,>>>,>>9.999"     
    FIELD q-back        AS Decimal FORMAT "->>,>>>,>>9.999"     

   . 
DEFINE DATASET dsUpdateItem FOR ttUpdateItem .

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as INT no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemName as Character no-undo.
DEFINE INPUT PARAMETER prmLine as INT no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUpdateItem.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = 0.
IF prmItemNum = ? THEN ASSIGN prmItemNum = "".

IF prmLine = ? THEN ASSIGN prmLine = 0.
IF prmItemName = ? THEN ASSIGN prmItemName = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
        
IF prmAction = "Select" THEN DO:
FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
                       oe-ordl.ord-no = prmOrderNum NO-LOCK :
    create ttUpdateItem.
    assign 
        ttUpdateItem.OrdNo          = oe-ordl.ord-no
        ttUpdateItem.vLine          = oe-ordl.LINE
        ttUpdateItem.est-no         = oe-ordl.est-no
        ttUpdateItem.Item1          = oe-ordl.i-no
        ttUpdateItem.CustPart       = oe-ordl.part-no
        ttUpdateItem.quantity       = oe-ordl.qty
        ttUpdateItem.Name1          = oe-ordl.i-name
        ttUpdateItem.Dscr           = string(oe-ordl.part-dscr1 + ", " + oe-ordl.part-dscr2 + ", " + oe-ordl.part-dscr3)
        ttUpdateItem.price          = oe-ordl.price
        ttUpdateItem.uom            = oe-ordl.pr-uom
        ttUpdateItem.taxable        = oe-ordl.tax
        ttUpdateItem.custpo         = oe-ordl.po-no
        ttUpdateItem.job-no         = oe-ordl.job-no
        ttUpdateItem.discount       = oe-ordl.disc
        ttUpdateItem.requested      = oe-ordl.req-code
        ttUpdateItem.requestdate    = oe-ordl.req-date
        ttUpdateItem.extprice       = oe-ordl.t-price
        ttUpdateItem.promised       = oe-ordl.prom-code
        ttUpdateItem.promisdate     = oe-ordl.prom-date
        ttUpdateItem.shipqty        = oe-ordl.ship-qty
        ttUpdateItem.counter        = oe-ordl.cas-cnt
        .
              /* END.  /*FOR EACH  itemfg*/*/
        
    END.   /*FOR EACH oe-ordl*/
   
END.
/**********************************************************************************************/
IF prmAction = "Search" THEN DO:
    FOR EACH oe-ordl where oe-ordl.company EQ prmComp AND
                            oe-ordl.ord-no = prmOrderNum AND
                           (oe-ordl.i-no  BEGINS prmItemNum  OR prmItemNum = "" )
                           AND (oe-ordl.LINE =      prmLine OR  prmLine  = 0 )
                           AND (oe-ordl.i-name  BEGINS prmItemName  or prmItemName = "" ) NO-LOCK :
        create ttUpdateItem.
        assign 
            ttUpdateItem.OrdNo          = oe-ordl.ord-no
            ttUpdateItem.vLine          = oe-ordl.LINE
            ttUpdateItem.est-no         = oe-ordl.est-no
            ttUpdateItem.Item1          = oe-ordl.i-no
            ttUpdateItem.CustPart       = oe-ordl.part-no
            ttUpdateItem.quantity       = oe-ordl.qty
            ttUpdateItem.Name1          = oe-ordl.i-name
            ttUpdateItem.Dscr           = string(oe-ordl.part-dscr1 + ", " + oe-ordl.part-dscr2 + ", " + oe-ordl.part-dscr3)
            ttUpdateItem.price          = oe-ordl.price
            ttUpdateItem.uom            = oe-ordl.pr-uom
            ttUpdateItem.taxable        = oe-ordl.tax
            ttUpdateItem.custpo         = oe-ordl.po-no
            ttUpdateItem.job-no         = oe-ordl.job-no
            ttUpdateItem.discount       = oe-ordl.disc
            ttUpdateItem.requested      = oe-ordl.req-code
            ttUpdateItem.requestdate    = oe-ordl.req-date
            ttUpdateItem.extprice       = oe-ordl.t-price
            ttUpdateItem.promised       = oe-ordl.prom-code
            ttUpdateItem.promisdate     = oe-ordl.prom-date
            ttUpdateItem.shipqty        = oe-ordl.ship-qty
            ttUpdateItem.counter        = oe-ordl.cas-cnt
            .
              /* END.  /*FOR EACH  itemfg*/*/
        
    END.   /*FOR EACH oe-ordl*/
   
END.




