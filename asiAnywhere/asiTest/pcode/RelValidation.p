
/*------------------------------------------------------------------------
    File        : rel.p
    Purpose     : Release

    Syntax      :

    Description : Return a Dataset of all Order Inquiry release

    Author(s)   : kuldeep gill
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */




DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vRowid      AS RECID  NO-UNDO.

DEFINE INPUT PARAMETER AShipTo      AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER cError      AS CHARACTER  NO-UNDO.


DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR relstat AS CHARACTER.
DEFINE VAR relout AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEF VAR ll-transfer AS LOG NO-UNDO.
DEF BUFFER ref-lot-no FOR reftable.
DEF BUFFER s-code FOR reftable.
DEF BUFFER bf-rel FOR oe-rel.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF VAR choice AS LOG NO-UNDO INITIAL YES.
DEF BUFFER cust-po-mand FOR reftable.
DEF VAR ld-date as DATE NO-UNDO.
def var li-ship-no as int no-undo.  /* if ship-to is changed */
DEF BUFFER ref-sell-price FOR reftable.

FIND oe-rel WHERE RECID(oe-rel) = vRowid. 

    IF AShipTo  <> "" THEN do:
        FIND FIRST oe-ord WHERE oe-ord.company EQ oe-rel.company 
                            AND oe-ord.ord-no  EQ oe-rel.ord-no
                             NO-LOCK.
        RUN oe/custxship.p (oe-rel.company,
                            oe-ord.cust-no, AShipTo,
                            BUFFER shipto).
        IF AVAIL shipto THEN li-ship-no = shipto.ship-no.
        ELSE DO:
            cError = "Invalid ship Id ".
            
            RETURN ERROR.
        END.
    END. /*IF AShipTo  <> "" THEN do:*/
