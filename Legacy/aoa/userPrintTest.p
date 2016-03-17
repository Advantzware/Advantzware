&SCOPED-DEFINE deleteRecords NO
    
FOR EACH user-print
    WHERE user-print.company EQ '001'
      AND (user-print.program-id EQ 'custaoa.'
       OR  user-print.program-id BEGINS 'aoaAR')
      AND user-print.USER-ID EQ 'nosweat'
    :
  DISP
    company
    program-id
    USER-ID
    BATCH
    batch-seq
    prog-seq
    field-name[1 FOR 3]
    field-label[1 FOR 3]
    field-value[1 FOR 3]
    PRINTER-NAME
    prog-title
    FREQUENCY
    next-date
    STRING(next-time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Next Time'
      WITH 1 COL TITLE 'AOA user-print'.
    &IF '{&deleteRecords}' EQ 'yes' &THEN
    DELETE user-print.
    &ENDIF
END.

FOR EACH user-batch:
    DISP user-batch WITH 1 COL TITLE 'user-batch'.
    &IF '{&deleteRecords}' EQ 'yes' &THEN
    DELETE user-batch.
    &ENDIF
END.

FOR EACH reftable WHERE reftable EQ 'aoaReport':
    DISP
        reftable.reftable
        reftable.code FORMAT 'x(20)'
        reftable.code2
         WITH TITLE 'reftable'.
    &IF '{&deleteRecords}' EQ 'yes' &THEN
    DELETE reftable.
    &ENDIF
END.

FOR EACH user-print NO-LOCK
    WHERE user-print.company EQ '001'
      AND user-print.program-id BEGINS 'oerep\s-book':
    DISP
      company
      program-id
      USER-ID
      BATCH
      batch-seq
      prog-seq
      field-name[1 FOR 3]
      field-label[1 FOR 3]
      field-value[1 FOR 3]
      PRINTER-NAME
      prog-title
      FREQUENCY
      next-date
      STRING(next-time,'hh:mm:ss am') FORMAT 'x(11)' LABEL 'Next Time'
        WITH 1 COL TITLE 'ASI user-print'.
END.
