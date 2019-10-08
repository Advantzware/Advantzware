/* ttAgedReceivablesTotals.i */

/* Aged Receivables Totals.rpa */
DEFINE TEMP-TABLE ttAgedReceivablesTotals NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD totCustNo      AS CHARACTER LABEL "Cust No"     FORMAT "x(8)" 
    FIELD totCustName    AS CHARACTER LABEL "Name"        FORMAT "x(30)"
    FIELD totSalesRep    AS CHARACTER LABEL "SalesRep"    FORMAT "x(25)"
    FIELD totDescription AS CHARACTER LABEL "Description" FORMAT "x(30)"
    FIELD totAmount      AS DECIMAL   LABEL "Amt"         FORMAT "->,>>>,>>>,>>9.99"
    FIELD totCurrent     AS DECIMAL   LABEL "Curr Amt"    FORMAT "->,>>>,>>>,>>9.99"
    FIELD totPeriodDay1  AS DECIMAL   LABEL "Period 1"    FORMAT "->,>>>,>>>,>>9.99"
    FIELD totPeriodDay2  AS DECIMAL   LABEL "Period 2"    FORMAT "->,>>>,>>>,>>9.99"
    FIELD totPeriodDay3  AS DECIMAL   LABEL "Period 3"    FORMAT "->,>>>,>>>,>>9.99"
    FIELD totPeriodDay4  AS DECIMAL   LABEL "Period 4"    FORMAT "->,>>>,>>>,>>9.99"
    FIELD xxSort         AS CHARACTER LABEL "Sort"        FORMAT "x(100)"
        INDEX ttAgedReceivablesTotals IS PRIMARY rowType xxSort
    .
