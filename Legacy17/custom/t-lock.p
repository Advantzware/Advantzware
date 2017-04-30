/* t-lock.p  show user-id for the locked record */
   
FIND FIRST cust NO-LOCK NO-ERROR.

FIND _file WHERE _file._file-name = "cust" NO-LOCK NO-ERROR.

FIND first _Lock WHERE   
    _Lock._Lock-Usr  <> ? AND                                                            
    _lock._lock-table = _file._file-number AND
    _Lock._Lock-Recid = INT(RECID(cust)) NO-LOCK NO-ERROR.

IF AVAIL _lock THEN
   DISP _Lock._Lock-Usr
        _Lock._Lock-Name
        _Lock._Lock-Recid
        _Lock._Lock-Table
        _file._file-name
    _file._desc.

/*   

   /******************************************************************************/ 
DEFINE TEMP-TABLE  ttLock NO-UNDO
    FIELD iUSER AS INTEGER
    FIELD cName AS CHARACTER
    FIELD rRECID AS INTEGER
    FIELD iTABLE AS INTEGER
    FIELD cTABLE AS CHARACTER
    INDEX UserName cName ASCENDING.
 
FOR EACH _Lock WHERE                                                                              
    _Lock._Lock-Usr   <> ? AND                                                            
    _Lock._Lock-Recid <> ?                                                                           
    NO-LOCK:
/* Comment the next two statements to also list individual locked records */
    FIND FIRST ttLock WHERE
        ttLock.iUSER  = _Lock._Lock-Usr AND
        ttLock.iTABLE = _Lock._Lock-Table
        NO-LOCK NO-ERROR.
    IF AVAILABLE ttLock THEN NEXT.
 
    CREATE ttLock.
    ASSIGN
        ttLock.iUSER = _Lock._Lock-Usr
        ttLock.cName = _Lock._Lock-Name
        ttLock.rRECID = _Lock._Lock-Recid
        ttLock.iTABLE = _Lock._Lock-Table.
END.
 
FOR EACH ttLock NO-LOCK:
    FIND FIRST _File WHERE _File._File-Number = iTABLE NO-LOCK NO-ERROR.
    ASSIGN
        cTABLE = _File._File-Name.
    DISPLAY
        ttLock.iUSER  LABEL "User Number"
        ttLock.cName  LABEL "User Name"
        ttLock.cTABLE LABEL "Table Name".
END.
 
*/
