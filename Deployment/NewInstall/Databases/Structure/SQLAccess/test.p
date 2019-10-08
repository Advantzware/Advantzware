/* Easy */
OUTPUT TO c:\temp\allfiles.SQL.
FOR EACH _file WHERE _file._file-num GT 0 AND _file._file-num LT 32000 NO-LOCK:
     PUT UNFORMATTED 'grant select on pub."' + _file._file-name + '" to public ~;'
     SKIP.
     PUT UNFORMATTED 'COMMIT;'
     SKIP.
END.

/* Hard 
OUTPUT TO VALUE("createDbaUsers.sql") APPEND.
FOR EACH _User NO-LOCK:
     FOR EACH _Sysdbauth WHERE _Sysdbauth._grantee = _User._Userid NO-LOCK: 
          PUT UNFORMATTED "GRANT DBA TO " + _GRANTEE + "; " SKIP.
          PUT UNFORMATTED "COMMIT WORK;" SKIP.
     END.
END.
OUTPUT CLOSE.
*/
