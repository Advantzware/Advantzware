/* scheduleTempTables.i */

{schedule/scopDir.i}
{{&includes}/ttblDowntime.i NEW}

DEFINE TEMP-TABLE ttJob NO-UNDO LIKE job-mch 
  FIELD rRowID AS ROWID 
  FIELD d-seq  AS INTEGER
  FIELD m-seq  AS INTEGER
  FIELD m-dscr AS CHARACTER
    INDEX ttJob IS PRIMARY frm blank-no d-seq m-seq pass m-code
    .
DEFINE TEMP-TABLE ttblJob NO-UNDO
  FIELD m-code        AS CHARACTER 
  FIELD job           AS CHARACTER
  FIELD frm           AS INTEGER 
  FIELD blank-no      AS INTEGER 
  FIELD pass          AS INTEGER 
  FIELD startDateTime AS DECIMAL
  FIELD endDateTime   AS DECIMAL
  FIELD startDate     AS DATE
  FIELD startTime     AS INTEGER
  FIELD endDate       AS DATE
  FIELD endTime       AS INTEGER
  FIELD newJob        AS LOGICAL 
    INDEX dateTimeIdx IS PRIMARY startDateTime endDateTime
    INDEX startDate startDate
    INDEX endDate endDate
    .
DEFINE BUFFER bTtblJob FOR ttblJob.
DEFINE BUFFER bJobMch  FOR job-mch.

{{&includes}/htmlDefs.i m-code}

DEFINE TEMP-TABLE ttMachine NO-UNDO
  FIELD m-code AS CHARACTER FORMAT "x(10)" LABEL "Machine"
  FIELD m-dscr AS CHARACTER FORMAT "x(24)" LABEL "Description"
  FIELD d-seq  LIKE mach.d-seq
  FIELD m-seq  LIKE mach.m-seq
    INDEX ttMachine IS PRIMARY m-code
    .
DEFINE TEMP-TABLE ttFolder NO-UNDO
  FIELD folderName AS CHARACTER
  FIELD searched   AS LOGICAL
  .
DEFINE TEMP-TABLE ttResource NO-UNDO
  FIELD resource AS CHARACTER
  .
