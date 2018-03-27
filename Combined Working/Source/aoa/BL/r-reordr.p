/*------------------------------------------------------------------------
  File: r-reordr.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Reordering Advice.rpa */
{aoa/tempTable/ttReOrderingAdvice.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttReOrderingAdvice.
{aoa/includes/pReOrderingAdvice.i}

/* local variables */
DEFINE VARIABLE idx AS INTEGER NO-UNDO.

/* subject business logic */
FOR EACH item NO-LOCK
    WHERE item.company EQ ipcCompany
      AND item.i-code  EQ "R"
      AND item.i-no    GE cStartItemNo
      AND item.i-no    LE cEndItemNo
      AND item.loc     GE cStartLoc
      AND item.loc     LE cEndLoc
      AND item.procat  GE cStartProdCategory
      AND item.procat  LE cEndProdCategory
    :
    IF item.q-onh + (IF includeQtyOnHand THEN item.q-ono ELSE 0) - item.q-comm GE item.ord-level THEN 
    NEXT.
    CREATE ttReOrderingAdvice.
    ASSIGN 
        ttReOrderingAdvice.itemNo          = item.i-no
        ttReOrderingAdvice.productCategory = item.procat
        ttReOrderingAdvice.uom             = item.cons-uom
        ttReOrderingAdvice.location        = item.loc
        ttReOrderingAdvice.reorderLevel    = item.ord-level
        ttReOrderingAdvice.qtyOnHand       = item.q-onh
        ttReOrderingAdvice.qtyAlloc        = item.q-comm
        ttReOrderingAdvice.qtyOnORder      = item.q-ono
        ttReOrderingAdvice.minOrder        = item.ord-min
        ttReOrderingAdvice.vendorNO        = item.vend-no
        ttReOrderingAdvice.vendorItem      = item.vend-item
        .
END. /* each item */
