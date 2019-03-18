
/*------------------------------------------------------------------------
    File        : reftable-PREPCAD.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Mar 08 17:36:38 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
DEFINE BUFFER reftable1 FOR reftable1.
DEFINE BUFFER Breftable FOR reftable. 
DEFINE BUFFER prep      FOR prep.

DISABLE TRIGGERS FOR LOAD OF reftable.        
DISABLE TRIGGERS FOR LOAD OF prep.
FOR EACH reftable WHERE reftable.reftable = "PREPCADFILE"
    NO-LOCK:
    DO TRANSACTION:    
        FIND FIRST prep WHERE prep.rec_key = reftable.rec_key
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE prep THEN    
        DO: 
            IF prep.cadNo EQ "" THEN ASSIGN prep.cadNo = reftable.code.
            IF prep.fileNo EQ "" THEN ASSIGN prep.fileNo = reftable.code2.
        END. 
        FIND CURRENT prep NO-LOCK NO-ERROR.    
        RELEASE prep.      
        CREATE reftable1.
        BUFFER-COPY reftable TO reftable1.
        RELEASE reftable1.
        FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Breftable THEN DELETE Breftable. 
        RELEASE Breftable.   
        iCnt = iCnt + 1.
    END.              
END.  /*FOR EACH reftable*/  
