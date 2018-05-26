
/*------------------------------------------------------------------------
    File        : RefTableConversion.p
    Purpose     : Ref Table Data Conversion

    Syntax      :

    Description : This will initiate RefTableMigration Class and Run Methods    

    Author(s)   : Jitender Gill
    Created     : Fri Feb 09 09:35:08 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
DEFINE VARIABLE oRefTableMigration AS RefTableMigration.
DEFINE VARIABLE iCount             AS INTEGER           NO-UNDO.
DEFINE VARIABLE iRecordLimit       AS INTEGER           NO-UNDO INITIAL 10000000.
DEFINE VARIABLE iProcessCount      AS INTEGER           NO-UNDO.
DEFINE VARIABLE cOutputFile        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cOutFile2          AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cError             AS CHARACTER         NO-UNDO.
DEFINE VARIABLE startTime          AS INTEGER           NO-UNDO.

DEF STREAM sText.
DEF STREAM sReport.

DEFINE TEMP-TABLE ttResults NO-UNDO
    FIELD cReftable      AS CHARACTER
    FIELD iTotalCount    AS INTEGER
    FIELD iChangeCount   AS INTEGER
    FIELD cConvError     AS CHARACTER
    FIELD timetaken      AS INTEGER
    .
disable triggers for load of reftable.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    cOutputFile = "c:\tmp\reftableconversion" 
                   + STRING(YEAR(TODAY)) 
                   + STRING(MONTH(TODAY)) 
                   + STRING(DAY(TODAY))
                   + "_"
                   + STRING(TIME)
                   + ".csv".
ASSIGN 
    cOutFile2 = "c:\tmp\reftableGroup" 
                   + STRING(YEAR(TODAY)) 
                   + STRING(MONTH(TODAY)) 
                   + STRING(DAY(TODAY))
                   + "_"
                   + STRING(TIME)
                   + ".txt".

FIND FIRST company NO-LOCK NO-ERROR.

OUTPUT STREAM sText TO VALUE(cOutFile2).
PUT company.company SPACE(5)
    company.name SPACE(5)
    pdbname(1) SKIP(2).
OUTPUT STREAM sText CLOSE.

ASSIGN 
    oRefTableMigration = NEW RefTableMigration().

    {refTableConv.i "FgRctdUserId" 'fg-rctd.user-id'}
    {refTableConv.i "DArtios" 'cecrep/d-artios.w'}
    {refTableConv.i "Flute" 'Flute'}


/* Deferred for 16.7.0                                              */
/*    {refTableConv.i "STYFLU" 'STYFLU'}                            */
/*    {refTableConv.i "STYSCORE" 'STYSCORE'}                        */
/*    {refTableConv.i "EstOpLock" 'est.op-lock'}                    */
/*                                                                  */

OUTPUT STREAM sReport TO cOutputFile.
PUT STREAM sReport "RefTable,Total Records,Records Converted,Error,Time Taken (Secs)" SKIP.
FOR EACH ttResults: 
    EXPORT STREAM sReport DELIMITER "," ttResults.
END.
OUTPUT STREAM sReport CLOSE.        
     
MESSAGE 
    "Reftable data migration 1 complete."
    VIEW-AS ALERT-BOX INFO. 
