

/*------------------------------------------------------------------------
    File        BinItem.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order ItemJobUp Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemJobUp NO-UNDO 
    BEFORE-TABLE beforeItemJobUp
    FIELD i-no LIKE itemfg.i-no
    FIELD i-name like itemfg.i-name
    FIELD cust LIKE itemfg.cust-no
    FIELD q-ono LIKE itemfg.q-ono
    FIELD q-onh LIKE itemfg.q-onh
    FIELD q-alloc LIKE itemfg.q-alloc
    FIELD q-back LIKE itemfg.q-back
    FIELD q-avail LIKE itemfg.q-avail
    FIELD ord-level LIKE itemfg.ord-level
    FIELD ord-min LIKE itemfg.ord-min
    FIELD ord-max LIKE itemfg.ord-max
    . 
DEFINE DATASET dsItemJobUp FOR ttItemJobUp .

DEFINE QUERY q-ItemJobUpQuery FOR ttItemJobUp.

DEFINE DATA-SOURCE src-ItemJobUp  FOR QUERY q-ItemJobUpQuery.

BUFFER ttItemJobUp :ATTACH-DATA-SOURCE(DATA-SOURCE src-ItemJobUp  :HANDLE).

