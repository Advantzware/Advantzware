/*addon/bol/gettaglist.p*/
/*------------------------------------------------------------------------
    File        : gettaglist.p
    Purpose     : 

    Syntax      :

    Description : Get tag list for release no 

    Author(s)   : Sewa Singh
    Created     : Mon Aug 27 19:29:35 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiReleaseNo AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opcList AS CHARACTER NO-UNDO.

FIND FIRST oe-relh NO-LOCK WHERE
      oe-relh.company  EQ ipCompany AND
      oe-relh.release# EQ ipiReleaseNo
      NO-ERROR.
  
  IF AVAIL oe-relh THEN
      FOR EACH oe-rell NO-LOCK
         WHERE oe-rell.company EQ ipCompany 
           AND oe-rell.r-no EQ oe-relh.r-no BREAK BY oe-rell.tag:
         IF LAST(oe-rell.tag) THEN
             opcList = opcList + oe-rell.tag  .
         ELSE 
             opcList = opcList + oe-rell.tag + "," . 
      END.


