/* cXMLDefs.i */

DEFINE VARIABLE cXMLASNDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLInvoiceDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMLOrderDir   AS CHARACTER NO-UNDO.

DEFINE NEW SHARED BUFFER xest  FOR est.
DEFINE NEW SHARED BUFFER xef   FOR ef.
DEFINE NEW SHARED BUFFER xeb   FOR eb.

DEFINE BUFFER oe-ord-whs-order FOR reftable.
DEFINE BUFFER oe-ordl-whs-item FOR reftable.
DEFINE BUFFER bf-oe-rel        FOR oe-rel.

{XMLOutput/ttNodes.i NEW}
{cXML/cXMLOrderFunc.i}

DO TRANSACTION:
  {sys/inc/oereleas.i}
  {sys/inc/oeimport.i}
END.

DEFINE NEW SHARED BUFFER   xoe-ord   FOR oe-ord.
DEFINE NEW SHARED VARIABLE save_id   AS RECID          NO-UNDO.
DEFINE NEW SHARED VARIABLE v-i-item  LIKE oe-ordl.i-no NO-UNDO.
DEFINE NEW SHARED VARIABLE v-i-qty   LIKE oe-ordl.qty  NO-UNDO.
DEFINE NEW SHARED VARIABLE v-qty-mod AS LOGICAL        NO-UNDO.
DEFINE NEW SHARED VARIABLE lv-qty    AS INTEGER        NO-UNDO.

DEFINE TEMP-TABLE cXMLDir NO-UNDO
  FIELD cXMLName AS CHARACTER
  FIELD cXMLDir  AS CHARACTER
  .

DEFINE TEMP-TABLE ttOrdHead NO-UNDO
    FIELD ttSelectedOrder     AS LOGICAL 
    FIELD ttProcessed         AS LOGICAL
    FIELD ttpayLoadID         AS CHARACTER
    FIELD ttfromIdentity      AS CHARACTER 
    FIELD tttoIdentity        AS CHARACTER 
    FIELD ttsenderIdentity    AS CHARACTER 
    FIELD ttorderDate         AS CHARACTER 
    FIELD ttorderID           AS CHARACTER 
    FIELD tttotalMoney        AS CHARACTER 
    FIELD ttpaymentPCard      AS CHARACTER 
    FIELD ttpaymentExpiration AS CHARACTER 
    FIELD ttshipToID          AS CHARACTER 
    FIELD ttshipToName        AS CHARACTER 
    FIELD ttshipToAddress1    AS CHARACTER 
    FIELD ttshipToAddress2    AS CHARACTER 
    FIELD ttshipToCity        AS CHARACTER 
    FIELD ttshipToState       AS CHARACTER 
    FIELD ttshipToZip         AS CHARACTER 
    FIELD ttshipToContact     AS CHARACTER 
    FIELD ttshiptoCountry     AS CHARACTER 
    FIELD ttshipToEmail       AS CHARACTER 
    FIELD ttshipToPhone       AS CHARACTER 
    FIELD ttshipToAreaCode    AS CHARACTER 
    FIELD ttbillToID          AS CHARACTER 
    FIELD ttbillToName        AS CHARACTER 
    FIELD ttbillToAddress1    AS CHARACTER 
    FIELD ttbillToAddress2    AS CHARACTER 
    FIELD ttbillToCity        AS CHARACTER 
    FIELD ttbillToState       AS CHARACTER 
    FIELD ttbillToZip         AS CHARACTER 
    FIELD ttcustNo            AS CHARACTER 
    .
DEFINE TEMP-TABLE ttOrdLines NO-UNDO
    FIELD ttpayLoadID                   AS CHARACTER
    FIELD ttitemLineNumber              AS CHARACTER
    FIELD ttitemQuantity                AS CHARACTER 
    FIELD ttitemSupplierPartID          AS CHARACTER 
    FIELD ttitemManufacturerPartID      AS CHARACTER 
    FIELD ttitemSupplierPartAuxiliaryID AS CHARACTER 
    FIELD ttitemMoney                   AS CHARACTER 
    FIELD ttitemDescription             AS CHARACTER 
    FIELD ttitemUnitOfMeasure           AS CHARACTER 
    FIELD ttItemDueDate                 AS CHARACTER
    .
DEFINE TEMP-TABLE ttOrdSchedShipments NO-UNDO
    FIELD ttpayLoadID         AS CHARACTER
    FIELD ttitemLineNumber    AS CHARACTER
    FIELD ttOrdShipmentNumber AS CHARACTER
    FIELD ttShipTo            AS CHARACTER
    FIELD ttQty               AS CHARACTER.
FOR EACH sys-ctrl NO-LOCK
    WHERE sys-ctrl.company  EQ g_company
      AND sys-ctrl.name BEGINS 'cXML'
      AND sys-ctrl.log-fld  EQ YES
      AND CAN-FIND(FIRST sys-ctrl-shipto OF sys-ctrl
    WHERE sys-ctrl-shipto.log-fld EQ YES):
  CREATE cXMLDir.
  ASSIGN
    cXMLDir.cXMLName = sys-ctrl.name
    cXMLDir.cXMLDir  = sys-ctrl.char-fld
    .
END. /* each sys-ctrl */
