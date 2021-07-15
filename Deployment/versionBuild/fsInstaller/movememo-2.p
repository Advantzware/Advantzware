DEF VAR i AS INT.
DEF VAR j AS INT.
DEF VAR old-key LIKE z_memo.key-list.
DEF VAR old-file LIKE z_memo.FILE-NAME.
DEF VAR old-page-no AS INT.

FIND FIRST z_memo NO-LOCK NO-ERROR.
REPEAT:
if not avail z_memo then leave.
    IF z_memo.page-no = 1  THEN DO:
        CREATE z_memos.
        ASSIGN z_memos.CAN-READ = z_memo.CAN-READ
             z_memos.create-date = z_memo.last-update 
            z_memos.create-time = "0000"
            z_memos.create-user = entry(1,userid("ptdb"),"@")
            z_memos.File-name = z_memo.FILE-NAME
            z_memos.Form-list = z_memo.form-list
            z_memos.key-list = z_memo.key-list
            z_memos.memo-type = IF CAN-DO("c",z_memo.form-list) OR
                                 CAN-DO("i",z_memo.form-list) THEN "E"
                                 ELSE "I"
            z_memos.must-see = z_memo.must-see
            z_memos.Page-no  = 1
            old-page-no = 1
            z_memos.Update-Date = z_memo.last-update
            z_memos.update-time = "0000"
            z_memos.update-user = entry(1,userid("ptdb"),"@")
            z_memos.memo = z_memo.memo[1].
            DO j = 2 TO 10:
               z_memos.memo = z_memos.memo + "~n" + 
                   z_memo.memo[j].
            END.
    END.
    ELSE DO:
        IF z_memo.page-no MOD 6 <> 0 THEN DO:
         FIND z_memos WHERE z_memos.FILE-NAME = z_memo.FILE-NAME
             AND z_memos.key-list = z_memo.key-list
             AND z_memos.page-no =  INTEGER ((z_memo.page-no / 6)  + .5)
             EXCLUSIVE-LOCK NO-ERROR.
        if not avail z_memos then do:
        create z_memos.
              ASSIGN z_memos.CAN-READ = z_memo.CAN-READ
             z_memos.create-date = z_memo.last-update 
            z_memos.create-time = "0000"
            z_memos.create-user = entry(1,userid("ptdb"),"@")
            z_memos.File-name = z_memo.FILE-NAME
            z_memos.Form-list = z_memo.form-list
            z_memos.key-list = z_memo.key-list
            z_memos.memo-type = IF CAN-DO("c",z_memo.form-list) OR
                                 CAN-DO("i",z_memo.form-list) THEN "E"
                                 ELSE "I"
            z_memos.must-see = z_memo.must-see
            z_memos.Page-no  = integer((z_memo.page-no / 6) + .5)
            old-page-no = z_memos.page-no
            z_memos.Update-Date = z_memo.last-update
            z_memos.update-time = "0000"
            z_memos.update-user = entry(1,userid("ptdb"),"@").
        end.
        DO j = 1 TO 10:
        ASSIGN z_memos.memo = z_memos.memo + "~n" + z_memo.memo[j].
        END.
       END.
        IF z_memo.page-no MOD 6 = 0 THEN DO:
        find z_memos where z_memos.file-name = z_memo.file-name
        and z_memos.key-list = z_memo.key-list
        and z_memos.page-no = integer(z_memo.page-no / 6)
        exclusive-lock no-error.
        if not avail z_memos then do:
            CREATE z_memos.
       ASSIGN z_memos.CAN-READ = z_memo.CAN-READ
             z_memos.create-date = z_memo.last-update 
            z_memos.create-time = "0000"
            z_memos.create-user = entry(1,userid("ptdb"),"@")
            z_memos.File-name = z_memo.FILE-NAME
            z_memos.Form-list = z_memo.form-list
            z_memos.key-list = z_memo.key-list
            z_memos.memo-type = IF CAN-DO("c",z_memo.form-list) OR
                                 CAN-DO("i",z_memo.form-list) THEN "E"
                                 ELSE "I"
            z_memos.must-see = z_memo.must-see
            z_memos.Page-no  = integer(z_memo.page-no / 6)
            old-page-no = z_memos.page-no
            z_memos.Update-Date = z_memo.last-update
            z_memos.update-time = "0000"
            z_memos.update-user = entry(1,userid("ptdb"),"@").
            end.
            z_memos.memo = z_memo.memo[1].
            DO j = 2 TO 10:
               z_memos.memo = z_memos.memo + "~n" + 
                   z_memo.memo[j].
            END.
        END.
    END.

    FIND NEXT z_memo NO-LOCK NO-ERROR.
    IF NOT AVAIL z_memo  THEN LEAVE.
END.
