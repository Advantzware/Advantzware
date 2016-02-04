/* getrfq.p */

DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipLoc AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipEstNo AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER opRfqNo AS INTEGER NO-UNDO.

FIND LAST rfqitem NO-LOCK
     WHERE rfqitem.company EQ ipCompany
       AND rfqitem.loc EQ ipLoc
       AND rfqitem.est-no EQ ipEstNo NO-ERROR.
IF AVAILABLE rfqitem THEN opRfqNo = rfqitem.rfq-no.
