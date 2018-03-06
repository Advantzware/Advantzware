/* TagMonDefs.i */



DEFINE TEMP-TABLE ttHeader NO-UNDO
  FIELD company           AS CHARACTER
  FIELD AsnCreateDateTime AS CHARACTER
  FIELD TotalQtyCount     AS INTEGER 
  FIELD RecNum            AS INTEGER
  .
DEFINE TEMP-TABLE ttBody NO-UNDO
  FIELD company          AS CHARACTER
  FIELD TheirCompanyCode AS CHARACTER         
  FIELD PalletID         AS CHARACTER
  FIELD DateInTransit    AS CHARACTER
  FIELD TheirVendor      AS CHARACTER
  FIELD PoNumber         AS CHARACTER
  FIELD poLineNumber     AS CHARACTER 
  FIELD OrderNumber      AS CHARACTER    
  FIELD QuanityOnPallet  AS CHARACTER
  FIELD BolNumber        AS CHARACTER 
  FIELD TrailerID        AS CHARACTER   
  FIELD NewCorOrder      AS CHARACTER
  FIELD RecNum            AS INTEGER
  . 
