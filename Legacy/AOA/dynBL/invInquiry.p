/*------------------------------------------------------------------------
  File:         invInquiry.p
  Description:  Inventory Inquiry BL
  Author:       Ron Stark
  Date Created: 5.2.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD transNumber  AS CHARACTER LABEL "Transaction Number"    FORMAT "x(15)"
    FIELD brewery      AS INTEGER   LABEL "Brewery Code"          FORMAT ">9"            INITIAL 99
    FIELD matNumber    AS CHARACTER LABEL "Material Numbers"      FORMAT "x(15)"
    FIELD qtyQualifier AS INTEGER   LABEL "Quantity Qualifier"    FORMAT ">9"
    FIELD inventoryQty AS INTEGER   LABEL "Inventory Quantity"    FORMAT "->>>>>>>9"
    FIELD uom          AS CHARACTER LABEL "Unit of Measurement"   FORMAT "x(2)"          INITIAL "EA"
    FIELD unitPrice    AS DECIMAL   LABEL "Unit Price"            FORMAT ">>>>>9.9<<<<<"
    FIELD genDate      AS CHARACTER LABEL "Date in YYMMDD Format" FORMAT "x(6)"
    FIELD genTime      AS CHARACTER LABEL "Time in HHMMSS Format" FORMAT "x(6)"
    .
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 5010
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cGenDate   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cGenTime   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQualifier AS INTEGER   NO-UNDO EXTENT 3 INITIAL [27,33,37].
    
    ASSIGN
        cGenDate = STRING(TODAY,"999999")
        cGenDate = SUBSTRING(cGenDate,5,2) + SUBSTRING(cGenDate,1,4)
        cGenTime = REPLACE(STRING(TIME,"HH:MM:SS"),":","")
        .
    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ cCompany
          AND itemfg.cust-no EQ cCustNo
          AND itemfg.stat    EQ "A",
        FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ itemfg.company
          AND oe-ordl.part-no EQ itemfg.part-no,
        FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-ordl.company
          AND oe-ord.ord-no  EQ oe-ordl.ord-no
          AND oe-ord.cust-no EQ itemfg.cust-no
          AND oe-ord.opened  EQ YES
        :
        DO idx = 1 TO EXTENT(iQualifier):
            CREATE ttTempTable.
            ASSIGN
                ttTempTable.transNumber  = itemfg.i-no
                ttTempTable.matNumber    = itemfg.part-no
                ttTempTable.qtyQualifier = iQualifier[idx]
                ttTempTable.unitPrice    = itemfg.sell-price
                ttTempTable.genDate      = cGenDate
                ttTempTable.genTime      = cGenTime
                .
            CASE iQualifier[idx]:
                WHEN 27 THEN
                ttTempTable.inventoryQty = itemfg.q-alloc.
                WHEN 33 THEN
                ttTempTable.inventoryQty = itemfg.q-avail.
                WHEN 37 THEN
                ttTempTable.inventoryQty = itemfg.q-ono.
            END CASE.
        END. /* do idx */
    END. /* each itemfg */
END PROCEDURE.
