/* ttblDowntime.i */

DEFINE {1} SHARED TEMP-TABLE ttblDowntime NO-UNDO
  FIELD dayID AS INTEGER
  FIELD resource AS CHARACTER
  FIELD startDate AS DATE
  FIELD startTime AS INTEGER
  FIELD endTime AS INTEGER
  FIELD startDateTime AS DECIMAL
  FIELD endDateTime AS DECIMAL
    INDEX ttblDowntime IS PRIMARY
          resource startDate dayID startTime endTime
    INDEX dateTimeIdx
          resource startDateTime endDateTime dayID
    INDEX dayID
          dayID resource startDate startTime endTime
    .
DEFINE BUFFER buffDowntime FOR ttblDowntime.

/* one time dummy record for reading in downtime data */
DEFINE TEMP-TABLE tempDowntime LIKE ttblDowntime.
CREATE tempDowntime.
