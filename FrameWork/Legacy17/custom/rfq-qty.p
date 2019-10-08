/* rfq-qty.p */

DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipEstNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipRfqNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipPartNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipQty AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipQtyPrice AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER ipQtyUom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipQtyDate AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipDelivery AS INTEGER NO-UNDO.

DEFINE VARIABLE rfqNo AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.

rfqNo = INTEGER(ipRfqNo) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

FIND FIRST rfqitem EXCLUSIVE-LOCK
     WHERE rfqitem.company EQ ipCompany
       AND rfqitem.loc EQ ipLoc
       AND rfqitem.est-no EQ ipEstNo
       AND rfqitem.rfq-no EQ rfqNo
       AND rfqitem.part-no EQ ipPartNo NO-ERROR.
IF NOT AVAILABLE rfqitem THEN RETURN.

DO i = 1 TO EXTENT(rfqitem.qty):
  IF rfqitem.qty[i] EQ ipQty THEN LEAVE.
  IF rfqitem.qty[i] EQ 0 AND j EQ 0 THEN j = i.
END.
IF i GT EXTENT(rfqitem.qty) THEN i = j.
IF rfqitem.qty-price[i] NE ipQtyPrice OR
   rfqitem.qty-uom[i] NE ipQtyUom OR
   rfqitem.delivery[i] NE ipDelivery THEN
ASSIGN
  rfqitem.qty[i] = ipQty
  rfqitem.qty-price[i] = ipQtyPrice
  rfqitem.qty-uom[i] = ipQtyUom
  rfqitem.qty-date[i] = ipQtyDate
  rfqitem.delivery[i] = ipDelivery.
FIND CURRENT rfqitem NO-LOCK.
