
/*------------------------------------------------------------------------
    File        : CompanyProcs.i
    Purpose     : 

    Syntax      :

    Description : Temp-table definitions for util\CompanyProcs.i

    Author(s)   : BV
    Created     : Wed Oct 17 16:44:18 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {1} TEMP-TABLE ttTables
    FIELD cTable             AS CHARACTER 
    FIELD cTableDesc         AS CHARACTER
    FIELD iRecordCount       AS INTEGER 
    FIELD lHasCompany        AS LOGICAL 
    FIELD lHasRecKey         AS LOGICAL 
    FIELD lHasINo            AS LOGICAL 
    FIELD lHasCustID         AS LOGICAL 
    FIELD lHasVendID         AS LOGICAL 
    FIELD lHasOrderNo        AS LOGICAL 
    FIELD lHasJobNo          AS LOGICAL 
    FIELD lHasAcctNo         AS LOGICAL
    FIELD lHasSequence       AS LOGICAL  
    .
    
DEFINE {1} TEMP-TABLE ttTablesToMerge
    FIELD cTable             AS CHARACTER
    FIELD cStatusNote        AS CHARACTER 
    FIELD iRecordsCompany1   AS INTEGER 
    FIELD iRecordsCompany2   AS INTEGER 
    FIELD iRecordsCollisions AS INTEGER 
    FIELD lProcess           AS LOGICAL
    .
DEFINE {1} TEMP-TABLE ttMergeCollisions
    FIELD cTable             AS CHARACTER 
    FIELD cKeyElements       AS CHARACTER 
    FIELD riRowIDMain        AS ROWID 
    FIELD riRowIDToMerge     AS ROWID
    .
     

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
