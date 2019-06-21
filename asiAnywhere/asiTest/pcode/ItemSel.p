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
{ItemSel.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemSel.

DEFINE VARIABLE v-use-rel      AS LOG NO-UNDO.
DEFINE BUFFER buff_itemfg FOR itemfg.
DEFINE VARIABLE li-onh AS DECIMAL.
DEFINE VARIABLE li-ono AS DECIMAL.
DEFINE VARIABLE li-all AS DECIMAL.
DEFINE VARIABLE li-bor AS DECIMAL.
DEFINE VARIABLE li-ava AS DECIMAL.
DEFINE VARIABLE li-reo AS DECIMAL.
 
DEF VAR v-type AS CHAR NO-UNDO.
DEF VARIABLE op-alloc AS DECIMAL NO-UNDO.
DEF var oereordr-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF var oereordr-cha LIKE sys-ctrl.char-fld NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum  = ? THEN ASSIGN prmOrderNum  = "".
IF prmItemNum = ? THEN ASSIGN prmItemNum = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
 CASE prmAction:
     WHEN "select" THEN DO:
        
        FIND FIRST oe-ordl where oe-ordl.company EQ prmComp AND
             oe-ordl.ord-no = int(prmOrderNum) AND
             oe-ordl.LINE = INT(prmItemNum)
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ordl THEN
        DO:
           FIND FIRST itemfg where
                itemfg.company = oe-ordl.company AND
                itemfg.i-no = oe-ordl.i-no
                NO-LOCK NO-ERROR.

           IF AVAIL itemfg THEN
           DO:
              create ttItemSel.
              assign 
                ttItemSel.Item1          = oe-ordl.i-no
                ttItemSel.Name           = oe-ordl.i-name
                ttItemSel.Order          = oe-ordl.ord-no
                ttItemSel.Estimate       = oe-ordl.est-no
                ttItemSel.ord-level      = itemfg.ord-level
                ttItemSel.q-ono          = itemfg.q-ono
                ttItemSel.q-onh          = itemfg.q-onh.
           END.
           
           FIND FIRST sys-ctrl
               WHERE sys-ctrl.company eq itemfg.company
               AND sys-ctrl.name    eq "OEREORDR"
               NO-LOCK NO-ERROR.
           
           IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
              CREATE sys-ctrl.
              ASSIGN
                  sys-ctrl.company = itemfg.company
                  sys-ctrl.name    = "OEREORDR"
                  sys-ctrl.descrip = "Use Actual Releases to calculate Qty Allocated in OE?"
                  sys-ctrl.log-fld = NO.
           END.
           
           ASSIGN
              oereordr-log = sys-ctrl.log-fld
              oereordr-cha = sys-ctrl.char-fld.
           
/********************************************************************************************************************************************************/
           FIND FIRST buff_itemfg WHERE buff_itemfg.company = oe-ordl.company 
                                     AND buff_itemfg.i-no = oe-ordl.i-no
                                     NO-LOCK NO-ERROR.                          
           IF NOT AVAIL buff_itemfg THEN DO:
              ASSIGN 
                  li-onh = 0
                  li-ono = 0
                  li-all = 0
                  li-bor = 0
                  li-ava = 0
                  li-reo = 0.
              RETURN ERROR.
           END.
           
           IF oereordr-log THEN RUN oi/oereordr.p (BUFFER itemfg, OUTPUT li-all).
            
           ELSE li-all = buff_itemfg.q-alloc. 
           
           IF AVAIL buff_itemfg THEN
               ASSIGN
               li-onh = buff_itemfg.q-onh
               li-ono = buff_itemfg.q-ono
               li-bor = buff_itemfg.q-back
               li-ava = buff_itemfg.q-onh +
               (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE buff_itemfg.q-ono) -
               li-all
               li-reo = buff_itemfg.ord-level.
           ELSE
              ASSIGN
               li-onh = 0
               li-ono = 0
               li-all = 0
               li-bor = 0
               li-ava = 0
               li-reo = 0.
           
           ASSIGN       
               ttItemSel.q-alloc        = li-all
               ttItemSel.q-avail        = li-ava
               ttItemSel.q-back         = li-bor.
                     
        END. /* avail oe-ordl*/
 END. /*WHEN "select"**/
     
 END CASE.





