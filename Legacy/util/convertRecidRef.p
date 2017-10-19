
/*------------------------------------------------------------------------
    File        : convertRecidRef.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Oct 17 16:02:38 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE STREAM sExport.
DEFINE STREAM sReport.
DEFINE TEMP-TABLE ttCount
  FIELD refname AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Reftable Name"
  FIELD iCntBefore AS INTEGER COLUMN-LABEL "Cnt Before" 
  FIELD iCntAfter AS INTEGER COLUMN-LABEL "Cnt After"
  FIELD iErrors AS INTEGER COLUMN-LABEL "Err Recs Deleted".
DEFINE VARIABLE lDumpRecords AS LOGICAL NO-UNDO.
DEFINE VARIABLE cDumpFile AS CHARACTER NO-UNDO FORMAT "x(50)" INIT "c:\tmp\recidConv.txt".
DEFINE VARIABLE lExportReport AS LOGICAL NO-UNDO.
DEFINE VARIABLE cReportFile AS CHARACTER NO-UNDO FORMAT "x(50)" INIT "c:\temp\reftable-report.txt".

/* ********************  Preprocessor Definitions  ******************** */

DEFINE BUFFER bf-reftable FOR reftable.
/* ***************************  Main Block  *************************** */
disable triggers for load of reftable.
  
OUTPUT STREAM sExport TO VALUE(cDumpFile) APPEND.

CREATE ttCount.
ttCount.refname = "ts/jobdata.p".



FOR EACH reftable NO-LOCK WHERE reftable.reftable = "ts/jobdata.p"
/* AND reftable.company  = cocode*/ :
       EXPORT STREAM sExport reftable. 
    ttCount.iCntBefore = ttCount.iCntBefore + 1.
    FIND FIRST job-hdr NO-LOCK WHERE RECID(job-hdr) EQ INTEGER(reftable.code) NO-ERROR.
    FIND FIRST fg-bin NO-LOCK WHERE RECID(fg-bin) EQ INTEGER(reftable.code2) NO-ERROR.
    IF AVAILABLE job-hdr AND AVAILABLE fg-bin THEN DO:
        FIND FIRST bf-reftable WHERE RECID(bf-reftable) EQ RECID(reftable) EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf-reftable THEN DO:
            ASSIGN bf-reftable.code     = job-hdr.rec_key
                   bf-reftable.code2    = fg-bin.rec_key
                   .
        END.
    END. 
    ELSE DO:
        ttCount.iErrors = ttCount.iErrors + 1.
        FIND FIRST bf-reftable WHERE RECID(bf-reftable) EQ RECID(reftable) EXCLUSIVE-LOCK NO-ERROR.
        DELETE bf-reftable.
    END.
END. 

CREATE ttCount.
ttCount.refname = "empAlert".

FOR EACH empalert NO-LOCK, EACH reftable NO-LOCK WHERE reftable.rec_key = STRING(RECID(empalert)):
     
        EXPORT STREAM sExport reftable. 
    
    ttCount.iCntBefore = ttCount.iCntBefore + 1.
    FIND FIRST bf-reftable WHERE RECID(bf-reftable) EQ RECID(reftable) EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bf-reftable THEN 
    DO:
        ASSIGN 
            bf-reftable.rec_key = empalert.rec_key.
            /* bf-reftable.CODE    = emailcod.emailcod. */
    END.     
END.

CREATE ttCount.
ttCount.refname = "oe-boll.lot-no".

FOR EACH reftable NO-LOCK WHERE reftable.reftable = "oe-boll.lot-no":
     
        EXPORT STREAM sExport reftable. 
    
    ttCount.iCntBefore = ttCount.iCntBefore + 1.
    FIND FIRST oe-boll WHERE RECID(oe-boll) EQ INTEGER(reftable.rec_key) NO-ERROR.
    IF AVAILABLE oe-boll THEN DO:
      FIND FIRST bf-reftable EXCLUSIVE-LOCK WHERE RECID(bf-reftable) EQ RECID(reftable)  NO-ERROR.
      IF AVAILABLE bf-reftable THEN DO:
        ASSIGN bf-reftable.rec_key  = oe-boll.rec_key.
      END.
    END.
    ELSE DO:
        ttCount.iErrors = ttCount.iErrors + 1.        
        FIND FIRST bf-reftable EXCLUSIVE-LOCK WHERE RECID(bf-reftable) EQ RECID(reftable)  NO-ERROR.
        DELETE bf-reftable.
    END. 
END. 

CREATE ttCount.
ttCount.refname = "oe-boll.sell-price".
 
FOR EACH reftable NO-LOCK WHERE reftable.reftable = "oe-boll.sell-price":
     
        EXPORT STREAM sExport reftable. 
    
    ttCount.iCntBefore = ttCount.iCntBefore + 1.
    FIND FIRST oe-boll WHERE RECID(oe-boll) EQ INTEGER(reftable.rec_key) NO-ERROR.
    IF AVAILABLE oe-boll THEN 
    DO:
        FIND FIRST bf-reftable EXCLUSIVE-LOCK WHERE RECID(bf-reftable) EQ RECID(reftable)  NO-ERROR.
        IF AVAILABLE bf-reftable THEN 
        DO:
            ASSIGN 
                bf-reftable.rec_key = oe-boll.rec_key.
        END.
    END.
    ELSE DO:
        ttCount.iErrors = ttCount.iErrors + 1.        
        FIND FIRST bf-reftable EXCLUSIVE-LOCK WHERE RECID(bf-reftable) EQ RECID(reftable)  NO-ERROR.
        DELETE bf-reftable.
    END.
    
END. 
/*5. Reftable                                                                                                                                                                                                         */
/*for empalert.table_rec_key and empalert.rec_key is created in viewers/empalert.w.  Did not find this to be used but should be removed along with other email related reftables since there are several similar ones.*/

FIND FIRST ttCount WHERE ttCount.refname = "ts/jobdata.p".
FOR EACH reftable NO-LOCK WHERE reftable.reftable = "ts/jobdata.p":
    
    
    FIND FIRST job-hdr NO-LOCK WHERE job-hdr.rec_key EQ reftable.code NO-ERROR.
    FIND FIRST fg-bin NO-LOCK WHERE fg-bin.rec_key EQ reftable.code2 NO-ERROR.
    IF  AVAILABLE(job-hdr) AND AVAILABLE(fg-bin) THEN 
        ttCount.iCntAfter = ttCount.iCntAfter + 1.
END.

FIND FIRST ttCount WHERE ttCount.refname = "empalert".
FOR EACH empalert NO-LOCK, EACH reftable NO-LOCK WHERE reftable.rec_key = empalert.rec_key:
    ttCount.iCntAfter = ttCount.iCntAfter + 1.
END.

FIND FIRST ttCount WHERE ttCount.refname = "oe-boll.lot-no".
FOR EACH reftable NO-LOCK WHERE reftable.reftable = "oe-boll.lot-no":
    FIND FIRST oe-boll WHERE oe-boll.rec_key EQ reftable.rec_key NO-ERROR.
    IF AVAILABLE oe-boll THEN 
      ttCount.iCntAfter = ttCount.iCntAfter + 1.
END.

FIND FIRST ttCount WHERE ttCount.refname = "oe-boll.sell-price".
FOR EACH reftable NO-LOCK WHERE reftable.reftable = "oe-boll.sell-price":
    FIND FIRST oe-boll WHERE oe-boll.rec_key EQ reftable.rec_key NO-ERROR.
    IF AVAILABLE oe-boll THEN 
        ttCount.iCntAfter = ttCount.iCntAfter + 1.
END.


  FOR EACH ttCount: 
    DISPLAY STREAM sExport ttCount WITH WIDTH 200 STREAM-IO  .
  END.
