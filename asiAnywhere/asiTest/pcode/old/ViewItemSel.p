


/*------------------------------------------------------------------------
    File        : ViewItemSel.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ViewItemSel.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewItems.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
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

DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEFINE STREAM s1.

IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:

WHEN "select" THEN DO:        

FOR EACH oe-ordl where oe-ordl.i-no = prmOrderNum NO-LOCK: 
    for each itemfg where itemfg.company = oe-ordl.company AND itemfg.i-no = oe-ordl.i-no no-lock :
        create ttViewItems.
        assign 
            ttViewItems.Item1          = oe-ordl.i-no
            ttViewItems.ord-level      = itemfg.ord-level
            ttViewItems.q-ono          = itemfg.q-ono
            ttViewItems.q-onh          = itemfg.q-onh
            .
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
            
            MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
            END.
            ASSIGN
                oereordr-log = sys-ctrl.log-fld
                oereordr-cha = sys-ctrl.char-fld.

/****************************************************************************************************************************************************/
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
                  ELSE ASSIGN li-onh = 0
                      li-ono = 0
                      li-all = 0
                      li-bor = 0
                      li-ava = 0
                      li-reo = 0.

                 ASSIGN       
                     ttViewItems.q-alloc        = li-all
                     ttViewItems.q-avail        = li-ava
                     ttViewItems.q-back         = li-bor.
                 MESSAGE "jyoti" li-all  li-ava li-bor.
             
        END.  /*FOR EACH  itemfg*/
    END.   /*FOR EACH oe-ordl*/
END.
END CASE.   




