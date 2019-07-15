
/*------------------------------------------------------------------------
    File        OrdView.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order ItemTotalory Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemTotal NO-UNDO 
BEFORE-TABLE beforeItemTotal
    FIELD i-no AS CHAR
    FIELD i-name AS CHAR
    FIELD q-ptd      AS Decimal
    FIELD q-ord-ytd  AS Decimal
    FIELD u-ord      AS Decimal
    FIELD q-prod-ptd AS Decimal
    FIELD q-prod-ytd AS Decimal
    FIELD u-prod     AS Decimal
    FIELD q-ship-ptd AS Decimal
    FIELD q-ship-ytd AS Decimal
    FIELD u-ship     AS Decimal
    FIELD q-inv-ptd  AS Decimal
    FIELD q-inv-ytd  AS Decimal
    Field u-inv      AS Decimal
    FIELD ptd-msf    AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD ytd-msf    AS Decimal
    FIELD lyytd-msf  AS Decimal
    FIELD l-score    AS Decimal
    FIELD t-len      AS Decimal
    FIELD w-score    AS Decimal
    FIELD t-wid      AS Decimal
    FIELD d-score    AS Decimal
    FIELD t-sqin     AS Decimal
    FIELD t-sqft     AS Decimal
    FIELD factor     AS CHARACTER
    FIELD cust-no    AS CHARACTER
    FIELD part-no    AS CHARACTER
    . 
DEFINE DATASET dsItemTotal FOR ttItemTotal .

DEFINE QUERY q-ItemTotalQuery FOR ttItemTotal.

DEFINE DATA-SOURCE src-ItemTotal  FOR QUERY q-ItemTotalQuery.

BUFFER ttItemTotal :ATTACH-DATA-SOURCE(DATA-SOURCE src-ItemTotal  :HANDLE).
