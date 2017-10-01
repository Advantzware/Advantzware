/* ttRecapProductCategory.i */

/* Recap Product Category.rpa */
DEFINE TEMP-TABLE ttRecapProductCategory NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD proCat          AS CHARACTER LABEL "Cat"                  FORMAT "x(5)"
    FIELD catDscr         AS CHARACTER LABEL "Category Description" FORMAT "x(20)"
    FIELD numOrders       AS INTEGER   LABEL "Orders"               FORMAT ">>>>9"
    FIELD amountCurrent   AS DECIMAL   LABEL "Amount Current"       FORMAT "->>>,>>>,>>9.99"
    FIELD sqFtCurrent     AS DECIMAL   LABEL "SqFt Current"         FORMAT ">>>9.999<<<"
    FIELD priceMSFCurrent AS DECIMAL   LABEL "MSF Current"          FORMAT "->>>,>>>,>>9.99"
    FIELD numDaysCurrent  AS INTEGER   LABEL "Num Days Current"     FORMAT ">,>>9"
    FIELD amountPeriod    AS DECIMAL   LABEL "Amount Period"        FORMAT "->>>,>>>,>>9.99"
    FIELD sqFtPeriod      AS DECIMAL   LABEL "SqFt Period"          FORMAT ">>>9.999<<<"
    FIELD priceMSFPeriod  AS DECIMAL   LABEL "MSF Period"           FORMAT "->>>,>>>,>>9.99"
    FIELD numDaysPeriod   AS INTEGER   LABEL "Num Days Period"      FORMAT ">,>>9"
    FIELD dscr            AS CHARACTER LABEL "Description"          FORMAT "x(20)"
        INDEX ttRecapProductCategory IS PRIMARY rowType proCat
        .
