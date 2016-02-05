/* b-ebfg.i - used in fg/b-ebfg.w and oe/b-ebfg.w */

PROCEDURE ebfgBuild:
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE rtRowID AS ROWID NO-UNDO.

  DEFINE BUFFER b-eb FOR eb.
  /* {1} must be available here */
  IF NOT AVAIL {1} THEN 
    RETURN.
  ASSIGN
    {&BROWSE-NAME}:TITLE IN FRAME {&FRAME-NAME} =
                   REPLACE(browseTitle,'%',LEFT-TRIM({1}.est-no))
    lv_i-col = 0
    lv_i-pass = 0
    lv_i-coat = 0
    lv_i-coat-p = 0.
  EMPTY TEMP-TABLE ebfg.
  FIND FIRST eb NO-LOCK WHERE eb.company EQ {1}.company
  &IF '{1}' EQ 'itemfg' &THEN
    AND eb.stock-no EQ {1}.i-no AND eb.est-no EQ {1}.est-no
  &ELSEIF '{1}' EQ 'oe-ordl' &THEN
    AND eb.est-no EQ {1}.est-no AND eb.form-no EQ {1}.form-no AND eb.blank-no EQ {1}.blank-no
  &ENDIF NO-ERROR.
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
  DISPLAY {&DISPLAYED-OBJECTS} WITH FRAME {&FRAME-NAME}.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

PROCEDURE ebfgSave:
  DEFINE INPUT PARAMETER ipBtnHandle AS HANDLE NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE j AS INTEGER NO-UNDO.
  DEFINE VARIABLE k AS INTEGER NO-UNDO.
  DEFINE VARIABLE rtRowID AS ROWID NO-UNDO.
  DEFINE VARIABLE unit#1 AS DECIMAL NO-UNDO EXTENT 20.
  DEFINE VARIABLE unit#2 AS DECIMAL NO-UNDO EXTENT 20.

  DEFINE BUFFER b-eb FOR eb.

  IF NOT CAN-FIND(FIRST ebfg) THEN RETURN.

  IF ipBtnHandle:LABEL EQ '&Update' THEN DO:
    ipBtnHandle:LABEL = '&Save'.
    APPLY 'ENTRY':U TO unit# IN BROWSE {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END. /* if ipbtnhandle */
  APPLY 'ROW-LEAVE':U TO BROWSE {&BROWSE-NAME}.

  FOR EACH ebfg NO-LOCK:
    IF ebfg.i-row EQ 1 THEN RUN getRefTable (ROWID(eb),0,OUTPUT rtRowID,YES).
    IF ebfg.i-row EQ 13 THEN RUN getRefTable (ROWID(eb),1,OUTPUT rtRowID,YES).
    IF ebfg.i-row EQ 1 OR ebfg.i-row EQ 13 THEN
    FIND reftable EXCLUSIVE-LOCK WHERE ROWID(reftable) EQ rtRowID NO-ERROR.
    IF AVAILABLE reftable THEN
    ASSIGN
      j = IF ebfg.i-row LE 12 THEN 0 ELSE 1
      reftable.val[ebfg.i-row - (j * 12)] = ebfg.unit#
      unit#1[ebfg.i-row] = ebfg.unit#.
  END. /* each ebfg */
  
  FOR EACH ef OF eb NO-LOCK,
      EACH b-eb OF ef NO-LOCK WHERE ROWID(b-eb) NE ROWID(eb):
    
    unit#2 = 0.
    DO j = 0 TO 1:
      RUN getRefTable (ROWID(b-eb),j,OUTPUT rtRowID,NO).
      FIND reftable NO-LOCK WHERE ROWID(reftable) EQ rtRowID NO-ERROR.
      IF NOT AVAILABLE reftable THEN NEXT.
      k = IF j EQ 0 THEN 12 ELSE 8.
      DO i = 1 TO k:
        unit#2[i + (j * 12)] = reftable.val[i].
      END. /* do i */
    END. /* do j */

    DO i = 1 TO EXTENT(unit#1):
      IF eb.i-code2[i] NE '' AND unit#1[i] NE 0 THEN
      DO j = 1 TO EXTENT(unit#1):
        IF b-eb.i-code2[j] EQ eb.i-code2[i] AND
           b-eb.i-ps2[j] EQ eb.i-ps2[i] THEN DO:
          unit#2[j] = unit#1[i].
          LEAVE.
        END.
      END. /* do j */
    END. /* do i */

    DO j = 0 TO 1:
      RUN getRefTable (ROWID(b-eb),j,OUTPUT rtRowID,YES).
      FIND reftable EXCLUSIVE-LOCK WHERE ROWID(reftable) EQ rtRowID NO-ERROR.
      IF NOT AVAILABLE reftable THEN NEXT.
      k = IF j EQ 0 THEN 12 ELSE 8.
      DO i = 1 TO k:
        reftable.val[i] = unit#2[i + (j * 12)].
      END. /* do i */
    END. /* do j */

  END. /* each ef */
  ipBtnHandle:LABEL = '&Update'.
END PROCEDURE.

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
