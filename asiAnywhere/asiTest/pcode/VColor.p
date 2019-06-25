

/*------------------------------------------------------------------------
    File        : ItemColor.p
    Purpose     : ItemColors

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{VColor.i}


DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemCol.
DEFINE VARIABLE lv_i-coat AS INTEGER FORMAT ">9" INITIAL 0    .
DEFINE VARIABLE lv_i-coat-p AS INTEGER FORMAT ">9" INITIAL 0 .
DEFINE VARIABLE lv_i-col AS INTEGER FORMAT ">9" INITIAL 0   .
DEFINE VARIABLE lv_i-pass AS INTEGER FORMAT ">9" INITIAL 1 .
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR v-board AS CHARACTER.
DEFINE VAR cocode AS CHARACTER.
DEFINE STREAM s1.


IF prmComp     = ? THEN ASSIGN prmComp     = "".
IF prmCust     = ? THEN ASSIGN prmCust     = "".
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction:
    WHEN "select" THEN DO:
        RUN build-qry .
        DATASET dsItemCol:FILL().
    END.
WHEN "delete" THEN DO:
    OUTPUT STREAM s1 TO ItemCol.txt.
    FOR EACH beforeItemCol TRANSACTION:
        ASSIGN v-return-value = BUFFER beforeItemCol:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
    END.
    IF NOT v-return-value THEN 
        DO:
        EXPORT STREAM s1 ERROR-STATUS:GET-NUMBER(1) ":" ERROR-STATUS:GET-MESSAGE(1).
        END.
        ELSE DO:
            EXPORT STREAM s1 "Deleted.".    
        END.
        OUTPUT STREAM s1 CLOSE.
    END.
    WHEN "update" THEN DO:
        FOR EACH beforeItemCol TRANSACTION:
            ASSIGN v-return-value = BUFFER beforeItemCol:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
               
        END.
        
    END.
    WHEN "insert" THEN DO:
        FOR EACH beforeItemCol TRANSACTION: 
            ASSIGN v-return-value = BUFFER beforeItemCol:SAVE-ROW-CHANGES("prgrms") NO-ERROR.
        END. 
    END.
END CASE.

/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
         DEFINE VARIABLE j AS INTEGER NO-UNDO.
         DEFINE VARIABLE rtRowID AS ROWID NO-UNDO.

         DEFINE BUFFER b-eb FOR eb.

         ASSIGN
           lv_i-col = 0
           lv_i-pass = 0
           lv_i-coat = 0
           lv_i-coat-p = 0.
         EMPTY TEMP-TABLE ebfg.

FOR EACH oe-ordl where oe-ordl.ord-no = int(prmOrderNum)  no-lock: 
for each itemfg where itemfg.i-no = oe-ordl.i-no no-lock :

    
           FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
          AND eb.stock-no EQ itemfg.i-no AND eb.est-no EQ itemfg.est-no
          AND eb.est-no EQ itemfg.est-no /*AND eb.form-no EQ itemfg.form-no AND eb.blank-no EQ itemfg.blank-no*/
          NO-ERROR.
      IF AVAILABLE eb THEN DO:
        DO i = 1 TO 20:
          IF i EQ 1 OR i EQ 13 THEN DO:
            j = IF i EQ 1 THEN 0 ELSE 1.
            RUN getRefTable (ROWID(eb),j,OUTPUT rtRowID,NO).
            FIND reftable NO-LOCK WHERE ROWID(reftable) EQ rtRowID NO-ERROR.
          END. /* i eq 1 or 13 */
          IF eb.i-code2[i] EQ '' THEN NEXT.
          CREATE ebfg.
          ASSIGN
            ebfg.i-row = i
            ebfg.unit# = IF AVAIL(reftable) THEN reftable.val[i - (j * 12)] ELSE 0
            ebfg.i-ps2 = eb.i-ps2[i]
            ebfg.i-code2 = eb.i-code2[i]
            ebfg.i-dscr2 = eb.i-dscr2[i]
            ebfg.i-%2 = eb.i-%2[i].
        END. /* do i */
        ASSIGN
          lv_i-col = eb.i-col
          lv_i-pass = eb.i-pass
          lv_i-coat = eb.i-coat
          lv_i-coat-p = eb.i-coat-p.
      END. /* avail eb */
        
             END.   /*FOR EACH Itemfg*/
    END.   /*FOR EACH oe-ordl*/

    END PROCEDURE.
/******************************************  Procedure getRefTable***************************************************/
PROCEDURE getRefTable:
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipUnit# AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipCreate AS LOGICAL NO-UNDO.

  DEFINE BUFFER ebBuffer FOR eb.

  FIND ebBuffer NO-LOCK WHERE ROWID(ebBuffer) EQ ipRowID NO-ERROR.
  IF NOT AVAILABLE ebBuffer THEN RETURN.
  FIND FIRST reftable NO-LOCK
       WHERE reftable.reftable EQ 'ce/v-est3.w Unit#' + TRIM(STRING(ipUnit#,'>'))
         AND reftable.company EQ ebBuffer.company
         AND reftable.loc EQ ebBuffer.est-no
         AND reftable.code EQ STRING(ebBuffer.form-no,'9999999999')
         AND reftable.code2 EQ STRING(ebBuffer.blank-no,'9999999999') NO-ERROR.
  IF NOT AVAILABLE reftable AND ipCreate THEN DO:
    CREATE reftable.
    ASSIGN
      reftable.reftable = 'ce/v-est3.w Unit#' + TRIM(STRING(ipUnit#,'>'))
      reftable.company = ebBuffer.company
      reftable.loc = ebBuffer.est-no
      reftable.code = STRING(ebBuffer.form-no,'9999999999')
      reftable.code2 = STRING(ebBuffer.blank-no,'9999999999').
    FIND CURRENT reftable NO-LOCK.
  END. /* if not avail */
  opRowID = ROWID(reftable).
END PROCEDURE.

