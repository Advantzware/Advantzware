
/*------------------------------------------------------------------------
    File        : ttPurge.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Sep 20 17:01:38 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} SHARED TEMP-TABLE ttFileList
    FIELD cFileName AS CHARACTER 
    FIELD rRowID AS ROWID 
    FIELD cRule AS CHARACTER 
    FIELD cError AS CHARACTER  
    FIELD cMessage AS CHARACTER
    FIELD lPurge AS LOG
    FIELD cRec_key AS CHARACTER
    FIELD cKeyValues AS CHARACTER 
    .
    
DEFINE {1} SHARED TEMP-TABLE ttPurgeList
    FIELD cTable AS CHARACTER 
    FIELD cCompany AS CHAR 
    FIELD cKey1 AS CHAR 
    FIELD cKey2 AS CHAR 
    FIELD cKey3 AS CHAR 
    FIELD cKey4 AS CHAR 
    FIELD cKey5 AS CHAR 
    FIELD rRowid AS ROWID 
    INDEX iTableRow cTable
    .     
    
DEFINE {1} SHARED TEMP-TABLE ttGLHistList
    FIELD lPosted AS LOG FORMAT "POSTED/UNPOSTED"
    FIELD iYear AS INTEGER
    FIELD iPeriod AS INTEGER 
    FIELD cAccount AS CHARACTER 
    FIELD cJournal AS CHARACTER 
    FIELD daTxnDate AS DATE 
    FIELD deAmount AS DECIMAL 
    FIELD cCurrency AS CHARACTER 
    FIELD cDescription AS CHARACTER 
    FIELD cType AS CHARACTER 
    FIELD cCreatedBy AS CHARACTER 
    FIELD cFileName AS CHARACTER 
    FIELD cReckey AS CHARACTER 
    FIELD rRowID AS ROWID.        
    
DEFINE {1} SHARED TEMP-TABLE ttPurgeByGroup
    FIELD ttcGroup AS CHAR 
    FIELD ttcPurge AS CHAR
    FIELD ttcProgram AS CHAR 
    FIELD ttcMasterTable AS CHAR 
    FIELD ttcUserList AS CHAR 
    FIELD ttiSecurityLevel AS INT.
    
DEFINE {1} SHARED TEMP-TABLE ttParmsByPurge
    FIELD ttcPurge AS CHAR 
    FIELD ttcParm AS CHAR 
    FIELD ttcLabel AS CHAR 
    FIELD ttcDataType AS CHAR
    FIELD ttcFormat AS CHAR 
    FIELD ttlRange AS LOG 
    FIELD ttcStartValue AS CHAR 
    FIELD ttcEndValue AS CHAR
    . 


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
