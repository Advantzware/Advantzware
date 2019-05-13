
/*------------------------------------------------------------------------
    File        : convProdToMex.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun May 12 09:32:35 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR cMasterList AS CHAR NO-UNDO.
DEF VAR cCompSpecificList AS CHAR NO-UNDO.
DEF VAR cNextRecKey AS CHAR NO-UNDO.
DEF VAR iCtr AS CHAR NO-UNDO.
DEF VAR cExportDir AS CHAR NO-UNDO.

DEF VAR tth AS HANDLE EXTENT 30 NO-UNDO.
DEF VAR bh AS HANDLE EXTENT 30 NO-UNDO.
DEF VAR qh AS HANDLE EXTENT 30 NO-UNDO.



/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FOR EACH _field WHERE _field._field-name EQ "company" OR _field._field-name EQ "cocode":
    FIND _file OF _field NO-LOCK NO-ERROR.
    IF AVAIL _file THEN ASSIGN 
        cCompSpecificList = cCompSpecificList + _file._file-name + ",".
END.
ASSIGN 
    cCompSpecificList = TRIM(cCompSpecificList,",").
MESSAGE ccompspecificlist VIEW-AS ALERT-BOX.