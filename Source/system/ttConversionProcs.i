
/*------------------------------------------------------------------------
    File        : ttConversionProcs.i
    Purpose     : 

    Syntax      :

    Description : Holds Temp-table for Conversion Procs in order to return as parameter

    Author(s)   : BV
    Created     : Tue Feb 16 20:56:54 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUOM NO-UNDO
    FIELD uom                 AS CHARACTER LABEL "UOM" FORMAT "x(4)"
    FIELD uomBase             AS CHARACTER LABEL "Base UOM" FORMAT "x(4)"
    FIELD multiplierToBase    AS DECIMAL   LABEL "Multiplier" FORMAT ">>>,>>>,>>9.9999"
    FIELD uomDescription      AS CHARACTER LABEL "Description" FORMAT "x(30)"
    FIELD canUseOrderQuantity AS LOGICAL   LABEL "Use Order Qty"
    FIELD canUsePOQuantity    AS LOGICAL   LABEL "Use PO Qty"
    FIELD canUseStockQuantity AS LOGICAL   LABEL "Use Stock Qty"
    FIELD canUsePricePerUnit  AS LOGICAL   LABEL "Use Price Per Unit"
    FIELD canUseCostPerUnit   AS LOGICAL   LABEL "Use Cost Per Unit"
    FIELD isBaseConverter     AS LOGICAL   LABEL "Base Converter"
    FIELD isOverridden        AS LOGICAL   LABEL "Overridden"
    FIELD uomSource           AS CHARACTER LABEL "UOM Source"
    FIELD iSourceLevel        AS INTEGER   LABEL "Source Level"
    . 

DEFINE TEMP-TABLE ttUOMEffective NO-UNDO 
    LIKE ttUOM.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
