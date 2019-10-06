DEFINE {1} SHARED TEMP-TABLE ttPOGroups
    FIELD VendNo LIKE po-ordl.vend-no
    FIELD INo LIKE po-ordl.i-no
    FIELD Len LIKE po-ordl.s-len
    FIELD Wid LIKE po-ordl.s-wid
    FIELD Scores AS CHAR
    FIELD Adders AS CHAR
    FIELD AdderCost AS DECIMAL
    FIELD TotalQty AS DECIMAL
    FIELD TotalQtyUOM AS CHAR
    FIELD NewCost LIKE po-ordl.cost
    FIELD NewCostUOM LIKE po-ordl.pr-uom
    FIELD NewSetup LIKE po-ordl.setup
    FIELD Multi AS LOG
    FIELD UpdateCost AS LOG
    FIELD BasisWeight LIKE ITEM.basis-w
    FIELD LineCount AS INT.

DEFINE {1} SHARED TEMP-TABLE ttPOLineXRef /*allows for easy re-finding of po-ordl*/
    FIELD POGroupRowId AS ROWID
    FIELD PoOrdlRowId AS ROWID
    FIELD OldCost LIKE po-ordl.cost
    FIELD NewCost LIKE po-ordl.cost
    FIELD CostUom LIKE po-ordl.pr-uom
    FIELD OldSetup LIKE po-ordl.setup
    FIELD NewSetup LIKE po-ordl.setup.