/* ttblJob.i - any changes here, check btnSave.i */

DEFINE {1} SHARED TEMP-TABLE ttblJob NO-UNDO
  {{&includes}/ttblJobFields.i}
  {{&includes}/ttblJobIndex.i}.

DEFINE {1} SHARED TEMP-TABLE pendingJob NO-UNDO LIKE ttblJob.

DEFINE {1} SHARED TEMP-TABLE jobColors NO-UNDO
  FIELD priority AS INTEGER
  FIELD idx AS INTEGER
  FIELD bgColorValue AS INTEGER
  FIELD fgColorValue AS INTEGER
  FIELD colorLabel AS CHARACTER
  FIELD colorOn AS LOGICAL INIT YES
  FIELD jobValue AS CHARACTER
  FIELD timeColor AS LOGICAL
    INDEX priority IS PRIMARY priority
    INDEX idx idx.

DEFINE {1} SHARED TEMP-TABLE ttblResource NO-UNDO
  FIELD order AS INTEGER
  FIELD resource AS CHARACTER
  FIELD resourceDescription AS CHARACTER
  FIELD sortOrder AS INTEGER
  FIELD priority AS INTEGER
  FIELD department AS CHARACTER
  FIELD kicks AS INTEGER
  FIELD dmiID AS INTEGER
    INDEX ttblResource IS PRIMARY UNIQUE order sortOrder resource
    INDEX priority priority
    INDEX resource resource.

DEFINE {1} SHARED TEMP-TABLE ttblDowntime NO-UNDO
  FIELD dayID AS INTEGER
  FIELD resource AS CHARACTER
  FIELD startDate AS DATE
  FIELD startTime AS INTEGER
  FIELD endTime AS INTEGER
  FIELD startDateTime AS DECIMAL
  FIELD endDateTime AS DECIMAL
    INDEX ttblDowntime IS PRIMARY UNIQUE
          resource startDate dayID startTime endTime
    INDEX dateTimeIdx
          resource startDateTime endDateTime dayID.

DEFINE BUFFER buffDowntime FOR ttblDowntime.

DEFINE {1} SHARED TEMP-TABLE boardDowntime NO-UNDO
  FIELD resource AS CHARACTER
  FIELD startDate AS DATE
  FIELD startTime AS INTEGER
  FIELD endDate AS DATE
  FIELD endTime AS INTEGER
  FIELD startDateTime AS DECIMAL
  FIELD endDateTime AS DECIMAL
  FIELD downtimeSpan AS INTEGER
    INDEX boardDowntime IS PRIMARY UNIQUE
          resource startDateTime endDateTime
    INDEX descendDowntime
          resource startDateTime DESCENDING endDateTime DESCENDING
    INDEX startDate resource startDate.

/* one time dummy record for reading in downtime data */
DEFINE TEMP-TABLE tempDowntime LIKE ttblDowntime.
CREATE tempDowntime.

{{&includes}/{&Board}/ttblJob.i {1}}
