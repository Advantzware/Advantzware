
/*------------------------------------------------------------------------
    File        : ttOrd.i
    Purpose     : Temp-table records needed for GE 850 orders

    Syntax      :

    Description : Needed in programs that include cXml/cXmlOrderProc.i

    Author(s)   : WFK
    Created     : Fri Dec 21 11:35:09 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
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
    FIELD ttDocType           AS CHARACTER 
    FIELD setPurpose          AS CHARACTER 
    FIELD ttRelease           AS CHARACTER
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
    FIELD ttChangePurpose               AS CHARACTER
    .
DEFINE TEMP-TABLE ttOrdSchedShipments NO-UNDO
    FIELD ttpayLoadID         AS CHARACTER
    FIELD ttitemLineNumber    AS CHARACTER
    FIELD ttOrdShipmentNumber AS CHARACTER
    FIELD ttShipTo            AS CHARACTER
    FIELD ttQty               AS CHARACTER.



