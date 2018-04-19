/* ttInventoryValue.i */

/* Inventory Value.rpa */
DEFINE TEMP-TABLE ttInventoryValue NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD custNo       AS CHARACTER LABEL "Customer"
    FIELD custName     AS CHARACTER LABEL "Cust Name"    FORMAT "x(30)"
    FIELD salesRep     AS CHARACTER LABEL "Rep"          FORMAT "X(3)"
    FIELD iNo          AS CHARACTER LABEL "Item"         FORMAT "x(15)"
    FIELD iName        AS CHARACTER LABEL "Description"  FORMAT "X(30)"
    FIELD tagNo        AS CHARACTER LABEL "Tag"          FORMAT "x(6)"
    FIELD tag          AS CHARACTER LABEL "Full Tag"     FORMAT "x(24)"    
    FIELD fgLotVal     AS CHARACTER LABEL "FG LOT#"      FORMAT "x(20)"
    FIELD partNo       AS CHARACTER LABEL "Cust Part"    FORMAT "X(15)"    
    FIELD procat       AS CHARACTER LABEL "FG Cat"       FORMAT "X(5)"         
    FIELD loc          AS CHARACTER LABEL "Whse"         FORMAT "X(5)"
    FIELD bin          AS CHARACTER LABEL "Bin"          FORMAT "X(8)" 
    FIELD jobNo        AS CHARACTER LABEL "Job"          FORMAT "x(10)"
    FIELD msfOnHand    AS DECIMAL   LABEL "MSF OH"       FORMAT "->>9.999" DECIMALS 3
    FIELD qtyOnHand    AS INTEGER   LABEL "Qty On Hand"  FORMAT "->>,>>>,>>9" 
    FIELD relQty       AS INTEGER   LABEL "Rel Qty"      FORMAT "->>,>>>,>>9"     
    FIELD sellPrice    AS DECIMAL   LABEL "FG Price"     FORMAT "->>>,>>9.99" 
    FIELD ordPr        AS DECIMAL   LABEL "Order Price"  FORMAT "->>>,>>9.99"     
    FIELD uomCost      AS DECIMAL   LABEL "UOM Cost"     FORMAT "->>>>>9.999" DECIMALS 3
    FIELD totCost      AS DECIMAL   LABEL "Total Cost"   FORMAT "->>>,>>9.99" 
    FIELD matCost      AS DECIMAL   LABEL "Mat Cost"     FORMAT "->>>,>>9.99" 
    FIELD labCost      AS DECIMAL   LABEL "Labor Cost"   FORMAT "->>>,>>9.99" 
    FIELD costUom      AS CHARACTER LABEL "C-UOM"        FORMAT "x(5)" 
    FIELD sellValueFg  AS DECIMAL   LABEL "Sell Val FG"  FORMAT "->>,>>>,>>9.99" 
    FIELD sellValueOrd AS DECIMAL   LABEL "Sell Val Ord" FORMAT "->>,>>>,>>9.99"  
    FIELD lastSale     AS DATE      LABEL "Last Sale"    FORMAT 99/99/9999
    FIELD viewPo       AS CHARACTER LABEL "View PO"      FORMAT "x(11)" 
    FIELD linePo       AS CHARACTER LABEL "Line PO"      FORMAT "x(10)" 
    FIELD relPo        AS CHARACTER LABEL "Rel PO"       FORMAT "x(11)"      
    FIELD daysOld      AS INTEGER   LABEL "Days Old"     FORMAT "->>>>>>9" 
    FIELD custNoOwned  AS CHARACTER LABEL "Cust Own"     FORMAT "X(8)" 
    FIELD setHeader    AS CHARACTER LABEL "Set Header"   FORMAT "X(15)" 
    FIELD qtyPerSet    AS DECIMAL   LABEL "Qty Per Set"  FORMAT "->>,>>9.99<<<<" 
    FIELD recDate      AS DATE      LABEL "Rec Date"     FORMAT 99/99/9999
    FIELD xxSort       AS CHARACTER LABEL "Sort"         FORMAT "x(500)"
        INDEX sortBy IS PRIMARY rowType xxSort
        .
