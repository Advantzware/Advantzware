/* ttRecapProductCategory.i */

/* Recap Product Category.rpa */
DEFINE TEMP-TABLE ttRecapProductCategory NO-UNDO
    {AOA/tempTable/ttFields.i}
    FIELD proCat          AS CHARACTER LABEL "Cat"                  FORMAT "x(5)"
    FIELD catDscr         AS CHARACTER LABEL "Category Description" FORMAT "x(20)"
    FIELD numOrders       AS INTEGER   LABEL "Orders"               FORMAT ">>>>9"
    FIELD sqFtCurrent     AS DECIMAL   LABEL "SqFt Current"         FORMAT ">>>9.999<<<"
    FIELD tonsCurrent     AS DECIMAL   LABEL "Tons Current"         FORMAT "->,>>9.99"
    FIELD amountCurrent   AS DECIMAL   LABEL "Amount Current"       FORMAT "->>>,>>>,>>9.99"
    FIELD priceMSFCurrent AS DECIMAL   LABEL "Price MSF Current"    FORMAT "->>>,>>>,>>9.99"
    FIELD priceTonCurrent AS DECIMAL   LABEL "Price Ton Current"    FORMAT "->>>,>>>,>>9.99"
    FIELD numDaysCurrent  AS INTEGER   LABEL "Num Days Current"     FORMAT ">,>>9"
    FIELD sqFtPeriod      AS DECIMAL   LABEL "SqFt Period"          FORMAT ">>>9.999<<<"
    FIELD tonsPeriod      AS DECIMAL   LABEL "Tons Period"          FORMAT "->,>>9.99"
    FIELD amountPeriod    AS DECIMAL   LABEL "Amount Period"        FORMAT "->>>,>>>,>>9.99"
    FIELD priceMSFPeriod  AS DECIMAL   LABEL "Price MSF Period"     FORMAT "->>>,>>>,>>9.99"
    FIELD priceTonPeriod  AS DECIMAL   LABEL "Price Ton Period"     FORMAT "->>>,>>>,>>9.99"
    FIELD numDaysPeriod   AS INTEGER   LABEL "Num Days Period"      FORMAT ">,>>9"
    FIELD dscr            AS CHARACTER LABEL "Description"          FORMAT "x(20)"
        INDEX ttRecapProductCategory IS PRIMARY rowType proCat
        .
DEFINE BUFFER bttRecapProductCategory FOR ttRecapProductCategory.

RUN spSetSessionParam ("SummaryTables", "1").
RUN spSetSessionParam ("SummaryHandle1", TEMP-TABLE ttRecapProductCategory:HANDLE).
RUN spSetSessionParam ("SummaryTitle1", "Recap Product Category").
