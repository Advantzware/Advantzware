
/*------------------------------------------------------------------------
    File        OrderOrderRel.i
    Purpose     : 

    Syntax      :

    Description : Dataset and Temp-Table definitions for Order OrderReleases Detail

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :

  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttOrderRel NO-UNDO
        FIELD rec-id AS CHAR
        FIELD printed AS char
        FIELD s-code AS char
        FIELD ship-id AS char
        FIELD Vstatus AS char
        FIELD carrier AS char
        FIELD tot-qty AS Decimal
        FIELD qty AS Decimal
        FIELD po-no AS CHAR
        FIELD lot-no AS CHAR
        FIELD Vdate AS DATE
        FIELD ship-addr AS char
        FIELD ship-city AS char
        FIELD ship-state AS char
        FIELD disc AS Decimal
        FIELD t-price AS Decimal
  .
DEFINE DATASET dsOrderRel FOR ttOrderRel .

DEFINE QUERY q-OrderRelQuery FOR ttOrderRel.

DEFINE DATA-SOURCE src-OrderRel  FOR QUERY q-OrderRelQuery.

BUFFER ttOrderRel :ATTACH-DATA-SOURCE(DATA-SOURCE src-OrderRel  :HANDLE).




/*DEF VAR char-hdl AS CHAR NO-UNDO.
def var ls-rel-stat as cha label "" form "x" no-undo.
def var lv-rel-recid as recid no-undo.
FUNCTION get-tot-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF NOT AVAIL oe-rel THEN DO:
    FIND oe-rel WHERE RECID(oe-rel) EQ lv-rel-recid NO-LOCK NO-ERROR.
    IF AVAIL oe-rel THEN
      FIND FIRST ttOrderRel WHERE ttOrderRel.rec-id EQ RECID(oe-rel) NO-ERROR.
  END.

  RETURN IF NOT oereleas-log                AND
            AVAIL ttOrderRel                 AND
            INDEX("AB",get-rel-stat()) GT 0 THEN ttOrderRel.qty
         ELSE
         IF AVAIL oe-rel THEN oe-rel.tot-qty
         ELSE INT(oe-rel.tot-qty:SCREEN-VALUE IN BROWSE {&browse-name}).

END FUNCTION.
  */
