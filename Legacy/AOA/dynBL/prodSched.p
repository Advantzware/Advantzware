/*------------------------------------------------------------------------
  File:         prodSched.p
  Description:  Production Schedule BL
  Author:       Ron Stark
  Date Created: 5.2.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD transNumber       AS INTEGER   LABEL "Transaction Number"                FORMAT ">>>>>>>9"
    FIELD brewery           AS INTEGER   LABEL "Brewery Code"                      FORMAT ">9"         INITIAL 99
    FIELD startDate         AS DATE      LABEL "Forecast Horizon Start Date"       FORMAT "99/99/9999" INITIAL 1/1/1950
    FIELD endDate           AS DATE      LABEL "Forecast Horizon End Date"         FORMAT "99/99/9999" INITIAL 12/31/2049
    FIELD genDate           AS CHARACTER LABEL "Forecast Generated"                FORMAT "x(6)"
    FIELD genTime           AS CHARACTER LABEL "Forecast Generation Time"          FORMAT "x(6)"
    FIELD matNumber         AS CHARACTER LABEL "Material Numbers"                  FORMAT "x(15)"
    FIELD forecastQty       AS INTEGER   LABEL "Forecast Quantity"                 FORMAT "->>>>>>>9"
    FIELD forecastQualifier AS CHARACTER LABEL "Forecast Timing Qualifier"         FORMAT "X"          INITIAL "W"
    FIELD prodStartDate     AS CHARACTER LABEL "Production Start Date of Quantity" FORMAT "x(6)"
    FIELD uom               AS CHARACTER LABEL "Unit of Measurement"               FORMAT "x(2)"       INITIAL "EA"
    .
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 5011
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cGenDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE cGenTime AS CHARACTER NO-UNDO.

ASSIGN
    cGenDate = STRING(TODAY,"999999")
    cGenDate = SUBSTRING(cGenDate,5,2) + SUBSTRING(cGenDate,1,4)
    cGenTime = REPLACE(STRING(TIME,"HH:MM:SS"),":","")
    .

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE lRelease  AS LOGICAL   NO-UNDO.

    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company EQ cCompany
          AND oe-ord.cust-no EQ cCustNo
          AND oe-ord.opened  EQ YES,
        EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ oe-ord.company
          AND oe-ordl.ord-no  EQ oe-ord.ord-no
          AND oe-ordl.part-no GT ""
        :
        lRelease = NO.
        FOR EACH oe-rel NO-LOCK
             WHERE oe-rel.company EQ oe-ordl.company
               AND oe-rel.ord-no  EQ oe-ordl.ord-no
               AND oe-rel.i-no    EQ oe-ordl.i-no
               AND oe-rel.line    EQ oe-ordl.line
               AND oe-rel.stat    NE "C"
               AND oe-rel.stat    NE "Z"
             :
             RUN pCreateTempTable (oe-rel.tot-qty).
             lRelease = YES.
        END. /* each oe-rel */
        IF lRelease EQ NO THEN
        RUN pCreateTempTable (oe-ordl.qty).
    END. /* each oe-ord */
END PROCEDURE.

PROCEDURE pCreateTempTable:
    DEFINE INPUT PARAMETER ipiForecastQty AS INTEGER NO-UNDO.

    DEFINE VARIABLE cProdDate AS CHARACTER NO-UNDO.

    CREATE ttTempTable.
    ASSIGN
        ttTempTable.transNumber   = oe-ord.ord-no
        ttTempTable.genDate       = cGenDate
        ttTempTable.genTime       = cGenTime
        ttTempTable.matNumber     = oe-ordl.part-no
        ttTempTable.forecastQty   = ipiForecastQty
        cProdDate                 = STRING(oe-ord.due-date - 10,"999999")
        cProdDate                 = SUBSTRING(cProdDate,5,2) + SUBSTRING(cProdDate,1,4)
        ttTempTable.prodStartDate = cProdDate
        .
END PROCEDURE.
