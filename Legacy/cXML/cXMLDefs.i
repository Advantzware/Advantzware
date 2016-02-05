/* cXMLDefs.i */

DEFINE VARIABLE cXMLASNDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLInvoiceDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLOrderDir AS CHARACTER NO-UNDO.

DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xef FOR ef.
DEFINE NEW SHARED BUFFER xeb FOR eb.

DEFINE BUFFER oe-ord-whs-order FOR reftable.
DEFINE BUFFER oe-ordl-whs-item FOR reftable.
DEFINE BUFFER bf-oe-rel FOR oe-rel.

{XMLOutput/ttNodes.i NEW}
{cXML/cXMLOrderFunc.i}

DO TRANSACTION:
  {sys/inc/oereleas.i}
  {sys/inc/oeimport.i}
END.

DEF NEW SHARED BUFFER xoe-ord FOR oe-ord.
DEF NEW SHARED VAR save_id AS RECID NO-UNDO.
DEF NEW SHARED VAR v-i-item LIKE oe-ordl.i-no NO-UNDO.
DEF NEW SHARED VAR v-i-qty LIKE oe-ordl.qty NO-UNDO.
DEF NEW SHARED VAR v-qty-mod AS LOG NO-UNDO.
DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.

DEFINE TEMP-TABLE cXMLDir NO-UNDO
  FIELD cXMLName AS CHARACTER
  FIELD cXMLDir AS CHARACTER
  .

FOR EACH sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ g_company
      AND sys-ctrl.name BEGINS 'cXML'
      AND sys-ctrl.log-fld EQ YES
      AND CAN-FIND(FIRST sys-ctrl-shipto OF sys-ctrl
    WHERE sys-ctrl-shipto.log-fld EQ YES):
  CREATE cXMLDir.
  ASSIGN
    cXMLDir.cXMLName = sys-ctrl.name
    cXMLDir.cXMLDir  = sys-ctrl.char-fld
    .
END. /* each sys-ctrl */
