


/*------------------------------------------------------------------------
    File        : RfqMaterial.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Rfq Maintenance

    Author(s)   : Sewa Singh
    Created     : Sat Feb 16, 2008
    Notes       :
  ----------------------------------------------------------------------*/
 
/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttRfqMaterial NO-UNDO 
    FIELD vBoard          AS CHAR
    FIELD vBrdDscr        AS CHAR
    FIELD vCal             AS DECIMAL FORMAT "9.99999"
    FIELD vGshwid        AS DECIMAL FORMAT ">>9.9999"
    FIELD vGshlen        AS DECIMAL FORMAT ">>9.9999"
    FIELD vLeaf1          AS CHAR FORMAT "x(10)"
    FIELD vLeafw1        AS DECIMAL FORMAT ">9.9999"
    FIELD vLeafl1        AS DECIMAL FORMAT ">9.9999"
    FIELD vLeaf2          AS CHAR FORMAT "x(10)" 
    FIELD vLeafw2        AS DECIMAL FORMAT ">9.9999"
    FIELD vLeafl2        AS DECIMAL FORMAT ">9.9999"
    FIELD vLeaf3          AS CHAR FORMAT "x(10)"
    FIELD vLeafw3        AS DECIMAL FORMAT ">9.9999"
    FIELD vLeafl3        AS DECIMAL FORMAT ">9.9999"
    FIELD vLeaf4          AS CHAR FORMAT "x(10)"
    FIELD vLeafw4        AS DECIMAL FORMAT ">9.9999"
    FIELD vLeafl4        AS DECIMAL FORMAT ">9.9999"
    FIELD vSpecdscr1   AS CHAR FORMAT "x(20)"
    FIELD vSpecdscr2   AS CHAR FORMAT "x(20)"
    FIELD vSpecdscr3   AS CHAR FORMAT "x(20)"
    FIELD vSpecdscr4   AS CHAR FORMAT "x(20)"
    FIELD vSpecdscr5   AS CHAR FORMAT "x(20)"
    FIELD vSpecdscr6   AS CHAR FORMAT "x(20)"
    FIELD vAdder1    AS CHAR FORMAT "x(10)"
    FIELD vAdder2   AS CHAR FORMAT "x(10)" 
    FIELD vAdder3    AS CHAR FORMAT "x(10)"
    FIELD vAdder4    AS CHAR FORMAT "x(10)"
    FIELD vAdder5    AS CHAR FORMAT "x(10)"
    FIELD vAdder6    AS CHAR FORMAT "x(10)"
    FIELD vAdder7    AS CHAR FORMAT "x(10)"
    FIELD vAdder8    AS CHAR FORMAT "x(10)"
    FIELD vAdder9    AS CHAR FORMAT "x(10)"
    FIELD vAdder10    AS CHAR FORMAT "x(10)"
    FIELD vAdder11       AS CHAR FORMAT "x(10)"
    FIELD vAdder12       AS CHAR FORMAT "x(10)"
    FIELD vSpecno1     AS CHAR FORMAT "x(10)"
    FIELD vSpecno2     AS CHAR FORMAT "x(10)"
    FIELD vSpecno3     AS CHAR FORMAT "x(10)"
    FIELD vSpecno4     AS CHAR FORMAT "x(10)"
    FIELD vSpecno5     AS CHAR FORMAT "x(10)"
    FIELD vSpecno6     AS CHAR FORMAT "x(10)"
    FIELD vLeafdscr     AS CHAR FORMAT "x(20)"   
    FIELD vLeafdscr2     AS CHAR FORMAT "x(20)"    
    FIELD vLeafdscr3     AS CHAR FORMAT "x(20)"   
    FIELD vLeafdscr4     AS CHAR FORMAT "x(20)"   
    FIELD flute          AS CHAR FORMAT "xxx"
    FIELD test           AS CHAR FORMAT "x(6)"
    FIELD vSpecQty       AS DECIMAL FORMAT ">>>9.9<<<"
    FIELD vSpecQty2       AS DECIMAL FORMAT ">>>9.9<<<"
    FIELD vSpecQty3       AS DECIMAL FORMAT ">>>9.9<<<"
    FIELD vSpecQty4       AS DECIMAL FORMAT ">>>9.9<<<"
    FIELD vSpecQty5       AS DECIMAL FORMAT ">>>9.9<<<"
    FIELD vSpecQty6       AS DECIMAL FORMAT ">>>9.9<<<"
    FIELD MatRowid AS RECID
     .

DEFINE DATASET dsRfqMaterial FOR ttRfqMaterial.
DEFINE QUERY q-RfqMaterialQuery FOR ttRfqMaterial.
DEFINE DATA-SOURCE src-RfqMaterial  FOR QUERY q-RfqMaterialQuery.
BUFFER ttRfqMaterial :ATTACH-DATA-SOURCE(DATA-SOURCE src-RfqMaterial  :HANDLE).


