
/*------------------------------------------------------------------------
    File        Releases.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order Releases Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttRel NO-UNDO 
        FIELD Part AS char
        FIELD print AS CHAR
        FIELD ord-no AS char
        FIELD line AS char
        FIELD SI AS char
        FIELD ShipTo  AS char
        FIELD Vstatus AS char
        FIELD Via AS char
        FIELD qty AS Decimal
        FIELD SQty AS Decimal
        FIELD po-no AS Char
        FIELD Vdate AS DATE FORMAT "99/99/9999"
        FIELD addr AS char
        FIELD city AS char
        FIELD disc AS Decimal
        FIELD state AS char
        FIELD sell-price AS DECIMAL
        FIELD price AS Decimal
        FIELD lot-no AS CHAR
        FIELD vRowid AS RECID
        FIELD aRowid AS RECID
        FIELD lvmailto AS CHAR 
        FIELD lvmailsubject AS CHAR 
        FIELD lvmailbody AS CHAR 
        FIELD frtpay AS CHAR
        FIELD fob AS CHAR.

DEF TEMP-TABLE tt-report LIKE report FIELD phantom AS LOG
                                     FIELD po-no LIKE oe-rel.po-no
                                     FIELD qty LIKE oe-rel.qty
                                     FIELD printed AS LOG
                                     FIELD s-code AS CHAR
                                     FIELD lot-no AS CHAR
                                     FIELD sell-price AS DEC.
DEFINE DATASET dsRel FOR ttRel .

DEFINE QUERY q-RelQuery FOR ttRel.

DEFINE DATA-SOURCE src-Rel  FOR QUERY q-RelQuery.

BUFFER ttRel :ATTACH-DATA-SOURCE(DATA-SOURCE src-Rel  :HANDLE).

