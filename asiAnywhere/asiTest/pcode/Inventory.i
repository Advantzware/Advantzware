
/*------------------------------------------------------------------------
    File        OrdView.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order Inventoryory Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttInventory NO-UNDO
BEFORE-TABLE beforeInventory
        FIELD Item AS char
        FIELD Name AS char
        FIELD Dscr AS char
        FIELD Vendor1 AS char
        FIELD VendItem1 AS char
        FIELD Vendor2 AS char
        FIELD VendItem2 AS char
        FIELD OrdPolicy AS LOGICAl  
        FIELD Stocked  AS LOGICAl 
        FIELD Purchased AS LOGICAl 
        FIELD IsSet AS Logical 
        FIELD alloc AS LOGICAL 
        FIELD ord-level AS Decimal 
        FIELD ord-min AS Decimal
        FIELD ord-max AS Decimal
        FIELD pur-uom AS char   
        FIELD lead-days AS Integer   
        FIELD beg-date AS date
        FIELD beg-bal AS Decimal   
        FIELD q-ono AS Decimal   
        FIELD q-onh AS Decimal   
        FIELD q-alloc AS Decimal   
        FIELD q-avail AS Decimal   
        FIELD q-back AS Decimal   

   . 
DEFINE DATASET dsInventory FOR ttInventory .

DEFINE QUERY q-InventoryQuery FOR ttInventory.

DEFINE DATA-SOURCE src-Inventory  FOR QUERY q-InventoryQuery.

BUFFER ttInventory :ATTACH-DATA-SOURCE(DATA-SOURCE src-Inventory  :HANDLE).
