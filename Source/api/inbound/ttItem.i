DEFINE TEMP-TABLE ttItem NO-UNDO
    FIELD WarehouseID              AS CHARACTER COLUMN-LABEL "Warehouse"
    FIELD LocationID               AS CHARACTER COLUMN-LABEL "Location"
    FIELD PrimaryID                AS CHARACTER COLUMN-LABEL "Item #"
    FIELD itemDesc                 AS CHARACTER COLUMN-LABEL "Item Description"
    FIELD InventoryStockID         AS CHARACTER COLUMN-LABEL "Tag #"
    FIELD Quantity                 AS DECIMAL COLUMN-LABEL "Quantity"
    FIELD ItemType                 AS CHARACTER  
    FIELD tag                      AS CHARACTER
    FIELD QuantityUOM              AS CHARACTER
    FIELD QuantityPerSubUnit       AS INTEGER
    FIELD QuantitySubUnitsPerUnit  AS INTEGER
    FIELD QuantityPartial          AS INTEGER
    FIELD Units                    AS INTEGER
    FIELD JobNo                    AS CHARACTER
    FIELD JobNo2                   AS INTEGER
    FIELD customerID               AS CHARACTER COLUMN-LABEL "Customer"
    FIELD POID                     AS INTEGER COLUMN-LABEL "PO #"
    FIELD UnitLength               AS DECIMAL
    FIELD UnitHeight               AS DECIMAL
    FIELD UnitWidth                AS DECIMAL
    FIELD StackHeight              AS INTEGER
    FIELD TagStatus                AS CHARACTER COLUMN-LABEL "Tag Status"
    FIELD StatusDescription        AS CHARACTER COLUMN-LABEL "Tag Description"
    FIELD OnHold                   AS LOGICAL COLUMN-LABEL "On Hold"
    FIELD sourceRowID              AS ROWID
    .
