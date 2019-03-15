
/*------------------------------------------------------------------------
    File        : Reftable-MSFLIMIT.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sat Mar 09 14:24:54 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE BUFFER reftable1 FOR reftable1.
DEFINE BUFFER Breftable FOR reftable. 
DEFINE BUFFER truck  FOR truck.

DISABLE TRIGGERS FOR LOAD OF reftable.        
DISABLE TRIGGERS FOR LOAD OF truck.
FOR EACH reftable WHERE reftable.reftable = "msf-limit"
    NO-LOCK:
    DO TRANSACTION:    
        FOR EACH truck WHERE truck.carrier    = reftable.code
                         AND truck.company    = reftable.company
                         AND truck.loc        = reftable.loc
                         AND truck.truck-code = reftable.code2
            EXCLUSIVE-LOCK:
            IF truck.msfLimit EQ 0 THEN ASSIGN truck.msfLimit = reftable.val[1]. 

        END.
        FIND CURRENT truck NO-LOCK NO-ERROR.    
        RELEASE truck.      
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
