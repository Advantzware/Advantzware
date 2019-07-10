
/*------------------------------------------------------------------------
    File        OrdView.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order OrdItemory Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrdItem NO-UNDO
BEFORE-TABLE beforeOrdItem
        FIELD Item1 AS char
        FIELD Part AS Char
        FIELD Name1 AS char
        FIELD Dscr AS char
        FIELD Dscr2 AS char
        FIELD Dscr3 AS char
        FIELD IsSet AS Logical
        FIELD Cust AS char
        FIELD CustName AS char
        FIELD Tax AS Logical
        FIELD Purchase AS Logical   
        FIELD Estimate  as Character
        FIELD Style as Character
        Field die as Char
        FIELD Plate as Char
        FIELD Cad  as Char
        FIELD SPC as Char
        FIELD UPC as Char
        FIELD Sell as Decimal
        FIELD UOM as Char
        FIELD Curr as Char
        FIELD Categ as Char
        FIELD Rpt as Character
        FIELD WareHouse as Char
        FIELD Bin as Char
        FIELD Count as Integer
        FIELD Weight as Decimal
        FIELD Freight as Char
        FIELD FreClass as Char
        FIELD Inventory as Char
        FIELD Cycle as Char
        FIELD Production as Char
        FIELD Packing as Char
        FIELD StdMat as Decimal
        FIELD StdLab as  Decimal
        FIELD StdVar as Decimal
        FIELD StdFix as Decimal
        FIELD StdTot as Decimal
        FIELD Average as Decimal
        FIELD LastCost as Decimal
        FIELD LastUom as Char
        FIELD Exempt AS LOGICAL INITIAL NO
        FIELD Stat AS CHARACTER INITIAL "A"
        FIELD stock AS CHARACTER INITIAL "S"
        FIELD casepal LIKE itemfg.ship-meth
        FIELD typecode LIKE itemfg.type-code
                                    
   . 
DEFINE DATASET dsOrdItem FOR ttOrdItem .

DEFINE QUERY q-OrdItemQuery FOR ttOrdItem.

DEFINE DATA-SOURCE src-OrdItem  FOR QUERY q-OrdItemQuery.

BUFFER ttOrdItem :ATTACH-DATA-SOURCE(DATA-SOURCE src-OrdItem  :HANDLE).
/***********************************************************************************************/




