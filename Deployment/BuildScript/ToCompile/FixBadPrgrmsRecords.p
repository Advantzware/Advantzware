
/*------------------------------------------------------------------------
    File        : FixBadPrgrmsRecords.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : MYT
    Created     : Tue Jul 02 19:28:56 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE ttPrgrms LIKE prgrms.
DEF VAR i AS INT NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

&SCOPED-DEFINE tablename prgrms

DISABLE TRIGGERS FOR LOAD OF {&tablename}.

OUTPUT TO c:\tmp\prgrmsbak.txt.
FOR EACH {&tablename} NO-LOCK:
    EXPORT {&tablename}.
END.
OUTPUT CLOSE.
    
INPUT FROM VALUE("..\..\Updates\{&tablename}.d") NO-ECHO.
REPEAT:
    CREATE tt{&tablename}.
    IMPORT tt{&tablename}.

    FIND FIRST {&tablename} EXCLUSIVE WHERE 
            {&tablename}.prgmname EQ tt{&tablename}.prgmname 
            NO-ERROR.
    IF NOT AVAIL {&tablename} THEN 
    DO:
        CREATE {&tablename}.
        BUFFER-COPY tt{&tablename} TO {&tablename}
            ASSIGN 
            {&tablename}.can_run = '*'
            {&tablename}.can_create = '*'
            {&tablename}.can_update = '*'
            {&tablename}.can_delete = '*'.
    END.
    ELSE 
    DO:
        BUFFER-COPY tt{&tableName} EXCEPT
            can_run
            can_create
            can_update
            can_delete
            TO {&tableName}. 
    END.
END.
INPUT CLOSE.
        
/* Delete records no longer used */
FOR EACH {&tablename} EXCLUSIVE WHERE 
        NOT CAN-FIND(FIRST tt{&tablename} WHERE tt{&tablename}.prgmname = {&tablename}.prgmname ):
    DELETE {&tablename}.
END.
    
EMPTY TEMP-TABLE tt{&tablename}.