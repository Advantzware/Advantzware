/* ttblJob.i - used in includes/ttblJob.i */

/* used to check for job conflicts */
DEFINE BUFFER buffJob FOR ttblJob.
DEFINE BUFFER bufferJob FOR ttblJob.
DEFINE BUFFER buffPending FOR pendingJob.

&IF DEFINED(noTempTables) EQ 0 &THEN

DEFINE {1} SHARED TEMP-TABLE jobNotes NO-UNDO
  FIELD jobRowID AS ROWID
  FIELD noteDate AS DATE FORMAT '99/99/9999' LABEL 'Note Date'
  FIELD noteTime AS INTEGER
  FIELD noteText AS CHARACTER
  FIELD noteKey AS CHARACTER
  FIELD jobStatus AS LOGICAL
  FIELD deleteNote AS LOGICAL
    INDEX jobNotes IS PRIMARY UNIQUE
          jobRowID jobStatus deleteNote noteDate DESCENDING noteTime DESCENDING
    INDEX rptOrder
          jobRowID jobStatus deleteNote noteDate noteTime.

/* used to auto schedule jobs */
DEFINE {1} SHARED TEMP-TABLE jobStacker NO-UNDO
  FIELD sortOrder AS INTEGER LABEL 'Sort' FORMAT '>>>9'
  FIELD resource AS CHARACTER LABEL 'Resource'
  FIELD jobSequence AS INTEGER LABEL 'jobSeq'
  FIELD job AS CHARACTER LABEL 'Job' FORMAT 'X(20)'
  FIELD jobChanged AS LOGICAL LABEL 'Chg'
  FIELD startDate AS DATE  LABEL 'Start Date'
  FIELD startTime AS INTEGER LABEL 'Start Time' FORMAT '>>>>9'
  FIELD startDateTime AS DECIMAL FORMAT '>>>>>>>9.99999'
  FIELD endDate AS DATE LABEL 'End Date'
  FIELD endTime AS INTEGER LABEL 'End Time' FORMAT '>>>>9'
  FIELD endDateTime AS DECIMAL FORMAT '>>>>>>>9.99999'
  FIELD downtimeSpan AS INTEGER
  FIELD lagTime AS INTEGER
  FIELD ttblRowID AS ROWID
    INDEX jobStacker IS PRIMARY sortOrder
    INDEX resource resource endDateTime
    INDEX jobSequence resource jobSequence
    INDEX job job endDateTime.

DEFINE BUFFER buffStack FOR jobStacker.

/* stores data used when moving existing jobs from drop/drag operation */
DEFINE TEMP-TABLE moveExisting NO-UNDO
  FIELD job AS CHARACTER
  FIELD resourceSequence AS INTEGER
    INDEX moveExisting IS PRIMARY UNIQUE job resourceSequence.

&ENDIF
