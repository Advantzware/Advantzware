/*------------------------------------------------------------------------
    File        : CopyEnhancedPack.p
    Purpose     : 

    Syntax      :

    Description : Copy folding enhanced material 

    Author(s)   : Sewa
    Created     : tue Aug 6 19:29:35 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE INPUT PARAMETER ipRowidEb AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipRowidBffEb AS ROWID NO-UNDO.

DEFINE VARIABLE lPackCodeCopy AS LOGICAL NO-UNDO .
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.
DEFINE BUFFER bf-estPacking FOR estPacking .
DEFINE BUFFER b-eb FOR eb .

  FIND FIRST eb WHERE ROWID(eb) EQ ipRowidEb NO-LOCK NO-ERROR .
  IF NOT AVAIL eb THEN RETURN .

  FIND FIRST b-eb NO-LOCK WHERE ROWID(b-eb) EQ ipRowidBffEb NO-ERROR .

  RUN sys/ref/nk1look.p (INPUT eb.company, "CePackEnhanced", "L" /* Logical */, NO /* check by cust */, 
                         INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
                         OUTPUT cRtnChar, OUTPUT lRecFound).

  IF lRecFound THEN
      lPackCodeCopy = LOGICAL(cRtnChar) NO-ERROR.

/* ***************************  Main Block  *************************** */

  IF lPackCodeCopy THEN DO:
      FOR EACH estPacking NO-LOCK 
          WHERE estPacking.company EQ eb.company 
           AND estPacking.estimateNo EQ eb.est-no 
           AND estPacking.FormNo  EQ eb.form-no 
           AND estPacking.BlankNo  EQ eb.blank-No :
         FIND FIRST bf-estPacking EXCLUSIVE-LOCK 
              WHERE bf-estPacking.company EQ b-eb.company
              AND bf-estPacking.estimateNo EQ b-eb.est-no
              AND bf-estPacking.FormNo EQ b-eb.form-no
              AND bf-estPacking.BlankNo EQ b-eb.blank-No
              AND bf-estPacking.rmItemID EQ estPacking.rmItemID NO-ERROR.
         IF NOT AVAIL bf-estPacking THEN do:     
             CREATE bf-estPacking .
             ASSIGN
                bf-estPacking.company      = eb.company 
                bf-estPacking.estimateNo   = b-eb.est-no
                bf-estPacking.FormNo       = b-eb.form-no
                bf-estPacking.BlankNo      = b-eb.blank-No 
                bf-estPacking.rmItemID     = estPacking.rmItemID.
         END.
         BUFFER-COPY estPacking EXCEPT company estimateNo FormNo  BlankNo estPackingID rmItemID TO bf-estPacking.
      END.
      RELEASE bf-estPacking NO-ERROR .
  END.

