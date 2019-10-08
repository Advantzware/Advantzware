/* ttReOrderingAdvice.i */

/* ReOrdering Advice.rpa */
DEFINE TEMP-TABLE ttReOrderingAdvice NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD itemNo          LIKE item.i-no      LABEL "Item Number"
    FIELD productCategory LIKE item.procat    LABEL "Cat"
    FIELD uom             LIKE item.cons-uom  LABEL "UOM"
    FIELD location        LIKE item.loc       LABEL "Loc"
    FIELD reorderLevel    LIKE item.ord-level LABEL "Reorder"       FORMAT "->>>>9.99"
    FIELD qtyOnHand       LIKE item.q-onh     LABEL "Qty On Hand"   FORMAT "->>>>9.999"
    FIELD qtyAlloc        LIKE item.q-comm    LABEL "Qty Allocated" FORMAT "->>>>9.999"
    FIELD qtyOnORder      LIKE item.q-ono     LABEL "Qty On Order"  FORMAT "->>>>9.999"
    FIELD minOrder        LIKE item.ord-min   LABEL "Min Order"
    FIELD vendorNO        LIKE item.vend-no   LABEL "Vendor"
    FIELD vendorItem      LIKE item.vend-item LABEL "Vendor Item"
        INDEX itemLoc IS PRIMARY 
            itemNo location
    .