/* ttblProdAce.i - rstark - 9.9.2019 */

DEFINE TEMP-TABLE ttblProdAce NO-UNDO
    FIELD prodAceResource      LIKE mach.m-code 
    FIELD prodAceDMIID         LIKE dmiTrans.dmiID
    FIELD prodAceJob           LIKE dmiTrans.jobID
    FIELD prodAceItem          LIKE dmiTrans.productID
    FIELD prodAceSeq           LIKE dmiTrans.seq
    FIELD prodAceShift         LIKE dmiTrans.shift
    FIELD prodAceShiftDate     LIKE dmiTrans.shiftDate
    FIELD prodAceStartDate     LIKE dmiTrans.startDate
    FIELD prodAceStartTime     LIKE dmiTrans.startTime
    FIELD prodAceEndDate       AS DATE      LABEL "End Date"      FORMAT "99/99/9999"
    FIELD prodAceEndTime       AS INTEGER   LABEL "End Time"      FORMAT ">>>>9"
    FIELD prodAceDuration      AS INTEGER   LABEL "Duration"      FORMAT ">>>>9"
    FIELD prodAceTranRunQty    LIKE dmiTrans.tranRunQty 
    FIELD prodAceTranRejectQty LIKE dmiTrans.tranRejectQty
    FIELD prodAceQtyDue        LIKE dmiTrans.qtyDue
    FIELD prodAceState         LIKE dmiTrans.transState
    FIELD prodAceChargeCode    AS CHARACTER LABEL "Charge Code"   FORMAT "x(3)"
    FIELD prodAceRunComplete   AS LOGICAL   LABEL "Run Complete"
    FIELD deleteFlag           AS LOGICAL   LABEL "Delete"
    FIELD prodAceOperator      AS CHARACTER LABEL "Operator"      EXTENT 10
    FIELD prodAceSelected      AS LOGICAL   LABEL "Selected"      INITIAL YES
    FIELD tempSelected         AS LOGICAL   LABEL "Temp Selected" INITIAL YES
    FIELD prodAceData          AS CHARACTER LABEL "ProdAce Data"  FORMAT "x(256)"
        INDEX ttblProdAceDetail IS PRIMARY
              prodAceResource
              prodAceJob
              prodAceItem
              prodAceSeq
              .
