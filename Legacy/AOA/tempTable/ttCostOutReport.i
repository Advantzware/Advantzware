/* ttCostOutReport.i */

/* Cost Out Report.rpa */
DEFINE TEMP-TABLE ttCostOutReport NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD jobNo        AS CHARACTER     LABEL "Job"            FORMAT "x(6)"
    FIELD dieNo      LIKE itemfg.die-no LABEL "Die No"
    FIELD salesRep   LIKE sman.sname
    FIELD custName   LIKE cust.name
    FIELD itemNo     LIKE itemfg.i-no   LABEL "Item No"
    FIELD sellingPrice AS DECIMAL       LABEL "Selling Price"  FORMAT "->>,>>>,>>9.99"
    FIELD qtyOrdered   AS INTEGER       LABEL "Ordered"
    FIELD estLaborCost AS DECIMAL       LABEL "Est Labor Cost" FORMAT "->>,>>>,>>9.99"
    FIELD actLaborCost AS DECIMAL       LABEL "Act Labor Cost" FORMAT "->>,>>>,>>9.99"
    FIELD estMatCost   AS DECIMAL       LABEL "Est Mat Cost"   FORMAT "->>,>>>,>>9.99"
    FIELD actMatCost   AS DECIMAL       LABEL "Act Mat Cost"   FORMAT "->>,>>>,>>9.99"
    FIELD totalEstCost AS DECIMAL       LABEL "Total Est Cost" FORMAT "->>,>>>,>>9.99"
    FIELD totalActCost AS DECIMAL       LABEL "Total Act Cost" FORMAT "->>,>>>,>>9.99"
    FIELD difference   AS DECIMAL       LABEL "Difference"     FORMAT "->>,>>>,>>9.99"
    FIELD sales        AS DECIMAL       LABEL "Sales"          FORMAT "->>,>>>,>>9.99"
    FIELD bookedCF     AS DECIMAL       LABEL "Bookded CF"     FORMAT "->>,>>>,>>9.99"
    FIELD estimatedDF  AS DECIMAL       LABEL "Estimated DF"   FORMAT "->>,>>>,>>9.99"
    FIELD actualCF     AS DECIMAL       LABEL "Actual CF"      FORMAT "->>,>>>,>>9.99"
    FIELD category     AS CHARACTER     LABEL "Category"       FORMAT "x(8)"
    FIELD qtyProduced  AS DECIMAL       LABEL "Qty Produced"   FORMAT "->>,>>>,>>9.99"
    FIELD xxSort       AS CHARACTER     LABEL "Sort By"        FORMAT "x(20)"
        INDEX ttCostOutReport IS PRIMARY xxSort
    .
