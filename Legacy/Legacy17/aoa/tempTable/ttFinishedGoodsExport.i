/* ttFinishedGoodsExport.i */

DEFINE TEMP-TABLE ttFinishedGoodsExport NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD itemNo             AS CHARACTER LABEL "Item No"              FORMAT "x(15)"
    FIELD itemName           AS CHARACTER LABEL "Item Name"            FORMAT "x(30)"
    FIELD custPartNo         AS CHARACTER LABEL "Customer Part No"     FORMAT "x(15)"
    FIELD custNo             AS CHARACTER LABEL "Customer"             FORMAT "x(8)"
    FIELD custName           AS CHARACTER LABEL "Customer Name"        FORMAT "x(30)"
    FIELD estimate           AS CHARACTER LABEL "Estimate"             FORMAT "x(8)"
    FIELD style              AS CHARACTER LABEL "Style"                FORMAT "x(8)"
    FIELD category           AS CHARACTER LABEL "Category"             FORMAT "x(8)"
    FIELD categoryDescr      AS CHARACTER LABEL "Category Description" FORMAT "x(30)"
    FIELD categoryDescr1     AS CHARACTER LABEL "Description 1"        FORMAT "x(30)"
    FIELD categoryDescr2     AS CHARACTER LABEL "Description 2"        FORMAT "x(30)"
    FIELD categoryDescr3     AS CHARACTER LABEL "Description 3"        FORMAT "x(30)"
    FIELD stockCust          AS CHARACTER LABEL "Stock/Custom"         FORMAT "x(8)"
    FIELD die                AS CHARACTER LABEL "Die No"               FORMAT "x(15)"
    FIELD plate              AS CHARACTER LABEL "Plate No"             FORMAT "x(15)"
    FIELD upc                AS CHARACTER LABEL "UPC No"               FORMAT "x(15)"
    FIELD cad                AS CHARACTER LABEL "Cad No"               FORMAT "x(15)"
    FIELD qualitySpc         AS CHARACTER LABEL "Quality/SPC No"       FORMAT "x(15)"
    FIELD stocked            AS LOGICAL   LABEL "Stocked 1"            FORMAT "x(3)"
    FIELD stocked2           AS LOGICAL   LABEL "Stocked 2"            FORMAT "x(3)"
    FIELD setHeader          AS LOGICAL   LABEL "Set Header"           FORMAT "x(3)"
    FIELD aGroup             AS CHARACTER LABEL "Group"                FORMAT "x(8)" 
    FIELD exemptFromDisc     AS LOGICAL   LABEL "Exempt from Disc"     FORMAT "x(3)"
    FIELD pm                 AS LOGICAL   LABEL "P/M"                  FORMAT "x(3)"
    FIELD sellPrice          AS DECIMAL   LABEL "Sell Price"           FORMAT "->>>,>>9.99"
    FIELD sellPriceUom       AS CHARACTER LABEL "Sell Price UOM"       FORMAT "x(3)"
    FIELD typeCode           AS CHARACTER LABEL "Type Code"            FORMAT "x(15)"
    FIELD currency           AS CHARACTER LABEL "Currency"             FORMAT "x(3)"
    FIELD warehouse          AS CHARACTER LABEL "Warehouse"            FORMAT "x(15)"
    FIELD bin                AS CHARACTER LABEL "Bin"                  FORMAT "x(15)"
    FIELD inventoryClass     AS CHARACTER LABEL "Inventory Class"      FORMAT "x(15)"
    FIELD cycleCountCode     AS CHARACTER LABEL "Cycle Count Code"     FORMAT "x(8)"
    FIELD prodcode           AS CHARACTER LABEL "Production Code"      FORMAT "x(8)"
    FIELD aCount             AS INTEGER   LABEL "Count"                FORMAT "->>>>>>9"
    FIELD weight             AS DECIMAL   LABEL "Weight"               FORMAT "->>>,>>9.99"
    FIELD freezeWeight       AS INTEGER   LABEL "Freeze Weight"        FORMAT "->>>,>>9.99"
    FIELD pknote             AS CHARACTER LABEL "Pk Note"              FORMAT "x(8)"
    FIELD freightClass       AS CHARACTER LABEL "Freight Class"        FORMAT "x(15)"
    FIELD freightClassDesc   AS CHARACTER LABEL "Freight Class Desc"   FORMAT "x(30)"
    FIELD stdMaterialCost     AS DECIMAL  LABEL "Std Material Cos"    FORMAT "->>>,>>9.99"
    FIELD stdLaborCost       AS DECIMAL   LABEL "Std Labor Cost"       FORMAT "->>>,>>9.99"
    FIELD stdVarOHCost       AS DECIMAL   LABEL "Std Var OH Cost"      FORMAT "->>>,>>9.99"
    FIELD stdFixOHCost       AS DECIMAL   LABEL "Std Fix OH Cost"      FORMAT "->>>,>>9.99"
    FIELD totalStdCost       AS DECIMAL   LABEL "Total Std Cost"       FORMAT "->>>,>>9.99"
    FIELD averageCost        AS DECIMAL   LABEL "Average Cost"         FORMAT "->>>,>>9.99"
    FIELD lastCost           AS DECIMAL   LABEL "Last Cost"            FORMAT "->>>,>>9.99"
    FIELD costUOM            AS CHARACTER LABEL "Cost UOM"             FORMAT "x(3)" 
    FIELD fullCost           AS DECIMAL   LABEL "Full Cost"            FORMAT "->>>,>>9.99"
    FIELD varied             AS CHARACTER LABEL "Varied"               FORMAT "x(8)"
    FIELD taxable            AS LOGICAL   LABEL "Taxable"
    FIELD astatus            AS CHARACTER LABEL "Status"               FORMAT "x(20)"
    FIELD shipMeth           AS CHARACTER LABEL "Ship Method"          FORMAT "x(8)"
    FIELD vendor1            AS CHARACTER LABEL "Vendor 1"             FORMAT "x(8)"
    FIELD vendor1Item        AS CHARACTER LABEL "Vendor 1 Item No"     FORMAT "x(15)"
    FIELD vendor2            AS CHARACTER LABEL "Vendor 2"             FORMAT "x(8)"
    FIELD vendor2Item        AS CHARACTER LABEL "Vendor 2 Item No"     FORMAT "x(15)"
    FIELD setAllocation      AS CHARACTER LABEL "Set Allocation"       FORMAT "x(15)"
    FIELD reorderPolicy      AS LOGICAL   LABEL "Reorder Policy"       FORMAT "x(3)"
    FIELD reorderLevel       AS DECIMAL   LABEL "Reorder Level"        FORMAT "->>>,>>9.99"
    FIELD minOrder           AS DECIMAL   LABEL "Minimum Order"        FORMAT "->>>,>>9.99"
    FIELD maxOrder           AS DECIMAL   LABEL "Maximum Order"        FORMAT "->>>,>>9.99"
    FIELD purchasedQtyUOM    AS CHARACTER LABEL "Purchased Qty UOM"    FORMAT "x(3)" 
    FIELD leadTimeDays       AS INTEGER   LABEL "Lead Time Days"       FORMAT ">>9"
    FIELD begDate            AS DATE      LABEL "Beginning Date"       FORMAT 99/99/9999
    FIELD begBalance         AS DECIMAL   LABEL "Beginning Balance"    FORMAT "->>>,>>9.99"
    FIELD qtyOnHand          AS DECIMAL   LABEL "Qty On-hand"          FORMAT "->>>,>>9.99"
    FIELD qtyOnOrd           AS DECIMAL   LABEL "Qty On Ord"           FORMAT "->>>,>>9.99"
    FIELD qtyAllocated       AS DECIMAL   LABEL "Qty Allocated"        FORMAT "->>>,>>9.99"
    FIELD qtyBackordered     AS DECIMAL   LABEL "Qty Backordered"      FORMAT "->>>,>>9.99"
    FIELD qtyAvailable       AS DECIMAL   LABEL "Qty Available"        FORMAT "->>>,>>9.99"
    FIELD qtyOrderedPTD      AS INTEGER   LABEL "Qty Ordered PTD"      FORMAT "->>,>>>,>>9"
    FIELD qtyOrderedYTD      AS INTEGER   LABEL "Qty Ordered YTD"      FORMAT "->>,>>>,>>9"
    FIELD qtyOrderedLastYr   AS INTEGER   LABEL "Qty Ordered Last Yr"  FORMAT "->>,>>>,>>9"
    FIELD qtyProducedPTD     AS INTEGER   LABEL "Qty Produced PTD"     FORMAT "->>,>>>,>>9"
    FIELD qtyProducedYTD     AS INTEGER   LABEL "Qty Produced YTD"     FORMAT "->>,>>>,>>9"
    FIELD qtyProducedLastYr  AS INTEGER   LABEL "Qty Produced Last Yr" FORMAT "->>,>>>,>>9"
    FIELD qtyShippedPTD      AS INTEGER   LABEL "Qty Shipped PTD"      FORMAT "->>,>>>,>>9"
    FIELD qtyShippedYTD      AS INTEGER   LABEL "Qty Shipped YTD"      FORMAT "->>,>>>,>>9"
    FIELD qtyShippedLastYr   AS INTEGER   LABEL "Qty Shipped Last Yr"  FORMAT "->>,>>>,>>9"
    FIELD qtyInvoicedPTD     AS INTEGER   LABEL "Qty Invoiced PTD"     FORMAT "->>,>>>,>>9"
    FIELD qtyInvoicedYTD     AS INTEGER   LABEL "Qty Invoiced YTD"     FORMAT "->>,>>>,>>9"
    FIELD qtyInvoicedLastYr  AS INTEGER   LABEL "Qty Invoiced Last Yr" FORMAT "->>,>>>,>>9"
    FIELD totalMsfPTD        AS DECIMAL   LABEL "Total MSF PTD"        FORMAT "->>>,>>9.99"
    FIELD totalMsfYTD        AS INTEGER   LABEL "Total MSF YTD"        FORMAT "->>,>>>,>>9"
    FIELD totalMsfLastYr     AS INTEGER   LABEL "Total MSF Last Yr"    FORMAT "->>,>>>,>>9"
    FIELd boxLength          AS DECIMAL   LABEL "Box Length"           FORMAT "->>>,>>9.99"
    FIELD boxWidth           AS DECIMAL   LABEL "Box Width"            FORMAT "->>>,>>9.99"
    FIELD boxDepth           AS DECIMAL   LABEL "Box Depth"            FORMAT "->>>,>>9.99"
    FIELD blankLength        AS DECIMAL   LABEL "Blank Length"         FORMAT "->>>,>>9.99"
    FIELD blankWidth         AS DECIMAL   LABEL "Blank Width"          FORMAT "->>>,>>9.99"
    FIELD totalSqIn          AS DECIMAL   LABEL "Total Sq In"          FORMAT "->>>,>>9.99"
    FIELD totalSqFt          AS DECIMAL   LABEL "Total Sq Ft"          FORMAT "->>>,>>9.99"
    FIELD aColor1            AS CHARACTER LABEL "Color1"               FORMAT "x(8)"
    FIELD aColor2            AS CHARACTER LABEL "Color2"               FORMAT "x(8)"
    FIELD aColor3            AS CHARACTER LABEL "Color3"               FORMAT "x(8)"
    FIELD aColor4            AS CHARACTER LABEL "Color4"               FORMAT "x(8)"
    FIELD aColor5            AS CHARACTER LABEL "Color5"               FORMAT "x(8)"
    FIELD aColor6            AS CHARACTER LABEL "Color6"               FORMAT "x(8)"
    FIELD aColor7            AS CHARACTER LABEL "Color7"               FORMAT "x(8)"
    FIELD aColor8            AS CHARACTER LABEL "Color8"               FORMAT "x(8)"
    FIELD aColor9            AS CHARACTER LABEL "Color9"               FORMAT "x(8)"
    FIELD aColor10           AS CHARACTER LABEL "Color10"              FORMAT "x(8)"
    FIELD aCoating1          AS CHARACTER LABEL "Coating1"             FORMAT "x(8)"
    FIELD aCoating2          AS CHARACTER LABEL "Coating2"             FORMAT "x(8)"
    FIELD aCoating3          AS CHARACTER LABEL "Coating3"             FORMAT "x(8)"
    FIELD boardCode          AS CHARACTER LABEL "Board Code"           FORMAT "x(15)"
    FIELD boardName          AS CHARACTER LABEL "Board Name"           FORMAT "x(15)"
    FIELD caliper            AS DECIMAL   LABEL "Caliper"              FORMAT "->>>,>>9.99"
    FIELD caseCode           AS CHARACTER LABEL "Case Code"            FORMAT "x(15)"
    FIELD caseName           AS CHARACTER LABEL "Case Name"            FORMAT "x(15)"
    FIELD caseQty            AS INTEGER   LABEL "Case Qty"             FORMAT "->>,>>>,>>9"
    FIELD skidCode           AS CHARACTER LABEL "Skid Code"            FORMAT "x(15)"
    FIELD skidName           AS CHARACTER LABEL "Skid Name"            FORMAT "x(15)"
    FIELD skidQty            AS INTEGER   LABEL "Skid Qty"             FORMAT "->>,>>>,>>9"
    FIELD specCode1          AS CHARACTER LABEL "Spec Code1"           FORMAT "x(15)"
    .
