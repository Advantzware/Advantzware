/* d-cadcam.i */

    /* Update new eb record with info from CADCAM */
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    
    IF cadcamValue NE '' THEN
    DO:
      FIND b-eb EXCLUSIVE-LOCK WHERE RECID(b-eb) EQ lv-eb-recid.
      FIND FIRST b-ef OF b-eb EXCLUSIVE-LOCK.
      DO i = 1 TO NUM-ENTRIES(cadcamValue) - 1 BY 2:
        /* these values loaded in est/d-cadcam.w */
        CASE ENTRY(i,cadcamValue):
          WHEN 'board' THEN
          b-ef.board = ENTRY(i + 1,cadcamValue).
          WHEN 'cad-no' THEN
          b-eb.cad-no = ENTRY(i + 1,cadcamValue).
          WHEN 'cal' THEN
          b-ef.cal = DECIMAL(ENTRY(i + 1,cadcamValue)).
          WHEN 'dep' THEN
          b-eb.dep = DECIMAL(ENTRY(i + 1,cadcamValue)).
          WHEN 'die-no' THEN
          b-eb.die-no = ENTRY(i + 1,cadcamValue).
          WHEN 'len' THEN
          b-eb.len = DECIMAL(ENTRY(i + 1,cadcamValue)).
          WHEN 'lin-in' THEN
          b-eb.lin-in = DECIMAL(ENTRY(i + 1,cadcamValue)).
          WHEN 'style' THEN
          b-eb.style = ENTRY(i + 1,cadcamValue).
          WHEN 't-len' THEN
          b-eb.t-len = DECIMAL(ENTRY(i + 1,cadcamValue)).
          WHEN 't-sqin' THEN
          b-eb.t-sqin = DECIMAL(ENTRY(i + 1,cadcamValue)).
          WHEN 't-wid' THEN
          b-eb.t-wid = DECIMAL(ENTRY(i + 1,cadcamValue)).
          WHEN 'weight' THEN
          b-ef.weight = DECIMAL(ENTRY(i + 1,cadcamValue)).
          WHEN 'wid' THEN
          b-eb.wid = DECIMAL(ENTRY(i + 1,cadcamValue)).
        END CASE.
      END. /* do i */
      FIND CURRENT b-eb NO-LOCK.
      FIND CURRENT b-ef NO-LOCK.
      cadcamValue = ''.
    END.
