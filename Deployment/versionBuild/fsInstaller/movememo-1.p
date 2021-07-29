DEF VAR i AS INT.
DEF VAR j AS INT.
DEF VAR old-key LIKE memo.memo-code.
DEF VAR old-file AS CHAR.
DEF VAR old-memo-type LIKE memo.memo-type.
DEF VAR old-page-no AS INT.
DEF VAR file-to-use AS CHAR.
DEF VAR old-LINE-COUNT AS INT.
DEF BUFFER bzm FOR z_memos.
def var cKey-list as char.

FIND FIRST memo NO-LOCK NO-ERROR.
    RUN getFileToDo (INPUT memo.memo-source,
                     INPUT memo.memo-code,
                     OUTPUT file-to-use,
                     output ckey-list).
REPEAT:
    IF memo.memo-code MATCHES "*!A*" OR 
        file-to-use = "NO" THEN .
    ELSE DO:
     IF memo.line-no = 1  THEN DO:
         FIND LAST bzm WHERE bzm.FILE-NAME = file-to-use
             AND bzm.key-list = cKey-list
             NO-LOCK NO-ERROR.
        CREATE z_memos.
        ASSIGN z_memos.CAN-READ = "*"
             z_memos.create-date = memo.dt-last-mod
            z_memos.create-time = memo.tm-last-mod
            z_memos.create-user = memo.usr-last-mod
            z_memos.File-name = file-to-use
            z_memos.Form-list = "*"
            z_memos.key-list = cKey-list
            z_memos.memo-type = memo.memo-type
            z_memos.must-see = NO
            z_memos.Page-no  = 1 + IF AVAIL bzm THEN bzm.page-no ELSE 0
            old-page-no = z_memos.page-no
            old-LINE-COUNT = 1
            OLD-file = file-to-use
            old-key = z_memos.key-list
            old-memo-type = z_memos.memo-type
            z_memos.Update-Date = memo.dt-last-mod
            z_memos.update-time = memo.tm-last-mod
            z_memos.update-user = memo.usr-last-mod
            z_memos.memo = memo.memo-text.
     END.
    ELSE DO:
        IF REPLACE(memo.memo-code,"!",",") <> old-key 
            OR memo.memo-type <> old-memo-type OR
            old-file <> file-to-use THEN DO:
            FIND LAST bzm WHERE bzm.FILE-NAME = file-to-use
             AND bzm.key-list = cKey-list
             NO-LOCK NO-ERROR.

            CREATE z_memos.
               ASSIGN z_memos.CAN-READ = "*"
                    z_memos.create-date = memo.dt-last-mod
                   z_memos.create-time = memo.tm-last-mod
                   z_memos.create-user = memo.usr-last-mod
                   z_memos.File-name = file-to-use
                   z_memos.Form-list = "*"
                   z_memos.key-list = cKey-list
                   z_memos.memo-type = memo.memo-type
                   z_memos.must-see = NO
                   z_memos.Page-no  = 1 + IF AVAIL bzm THEN bzm.page-no ELSE 0
                   old-page-no = z_memos.page-no
                   old-line-count = 1
                   OLD-file = file-to-use
                   old-key = z_memos.key-list
                   old-memo-type = z_memos.memo-type
                   z_memos.Update-Date = memo.dt-last-mod
                   z_memos.update-time = memo.tm-last-mod
                   z_memos.update-user = memo.usr-last-mod
                   z_memos.memo = memo.memo-text.
 
        END.
        ELSE DO:

        IF old-line-count < 34  THEN DO:
         FIND z_memos WHERE z_memos.FILE-NAME = old-file
             AND z_memos.key-list = old-key
             AND z_memos.page-no = old-page-no
             EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAIL z_memos THEN DO:
            FIND LAST bzm WHERE bzm.FILE-NAME = file-to-use
         AND bzm.key-list = cKey-list
         NO-LOCK NO-ERROR.
        CREATE z_memos.
        ASSIGN z_memos.CAN-READ = "*"
        z_memos.create-date = memo.dt-last-mod
        z_memos.create-time = memo.tm-last-mod
        z_memos.create-user = memo.usr-last-mod
        z_memos.File-name = file-to-use
        z_memos.Form-list = "*"
        z_memos.key-list = cKey-list
        z_memos.memo-type = memo.memo-type
        z_memos.must-see = NO
        z_memos.Page-no  = 1 + IF AVAIL bzm THEN bzm.page-no ELSE 0
        old-page-no = z_memos.page-no
        old-line-count = 1
        OLD-file = file-to-use
        old-key = z_memos.key-list
        old-memo-type = z_memos.memo-type
        z_memos.Update-Date = memo.dt-last-mod
        z_memos.update-time = memo.tm-last-mod
        z_memos.update-user = memo.usr-last-mod
        z_memos.memo = memo.memo-text.
        END.
      ELSE   do:  ASSIGN z_memos.memo = z_memos.memo + "~n" + memo.memo-text
            old-line-count = old-line-count + 1.
            end.
       END.
        IF old-line-count >= 34 THEN DO:
            FIND LAST bzm WHERE bzm.FILE-NAME = file-to-use
             AND bzm.key-list = cKey-list
             NO-LOCK NO-ERROR.
        CREATE z_memos.
       ASSIGN z_memos.CAN-READ = "*"
            z_memos.create-date = memo.dt-last-mod
            z_memos.create-time = memo.tm-last-mod
            z_memos.create-user = memo.usr-last-mod
            z_memos.File-name = file-to-use
            z_memos.Form-list = "*"
            z_memos.key-list = cKey-list
            z_memos.memo-type = memo.memo-type
            z_memos.must-see = NO
            z_memos.Page-no  = 1 + IF AVAIL bzm THEN bzm.page-no ELSE 0
            old-page-no = z_memos.page-no
           old-line-count = 1
            OLD-file = file-to-use
            old-key = z_memos.key-list
            old-memo-type = z_memos.memo-type
            z_memos.Update-Date = memo.dt-last-mod
            z_memos.update-time = memo.tm-last-mod
            z_memos.update-user = memo.usr-last-mod
            z_memos.memo = memo.memo-text.
        END.  /* more than 34 lines on one memo */
        END.  /* else do (continuation of same memo) */
    END.
       END.  /* not an Audit record */
    FIND NEXT memo NO-LOCK NO-ERROR.
    IF NOT AVAIL memo  THEN LEAVE.
    RUN getFileToDo (INPUT memo.memo-source,
                     INPUT memo.memo-code,
                     OUTPUT file-to-use,
                     output cKey-list).
END.

PROCEDURE GetFileToDo:
DEF INPUT PARAMETER cMemo-Source LIKE memo.memo-source.
DEF INPUT PARAMETER cMemo-code LIKE memo.memo-code.
DEF OUTPUT PARAMETER cFile-to-use AS CHAR.
def output parameter cKey-list as char.
DEF VAR convmemo-code AS CHAR.
convmemo-code = REPLACE(cMemo-code,"!",",").
CASE cmemo-source:
    WHEN "CACA":U THEN DO:
       IF  cMemo-code MATCHES "*!A*" THEN cFile-to-use = "NO".
       ELSE DO:
        IF NUM-ENTRIES(convMemo-code) = 2 THEN cFile-to-use = "CONTRACT".
        IF NUM-ENTRIES(convMemo-code) = 3 THEN cFile-to-use = "CONTRACT-LIN".
       END.
    END.
    WHEN "CACP" THEN cFile-to-use = "CONTRACT-PRI".
    WHEN "CACT" THEN cFile-to-use = "FF-CODES".
    WHEN "ININ" THEN cFile-to-use = "TECH-IND".
    WHEN "PMPF" THEN cFile-to-use = "PM-LINE".
    WHEN "PMPL" THEN cFile-to-use = "PM-LINE".
    WHEN "PMPM" THEN cFile-to-use = "PM-HEADER".
    WHEN "PMPP" THEN cFile-to-use = "PM-LINE".
    WHEN "PMPC" THEN cFile-to-use = "PM-LINE".
    WHEN "RVRV" THEN cFile-to-use = "RV".
    WHEN "MBME" THEN cFile-to-use = "METER-CARD".
    WHEN "SOCF" THEN cFile-to-use = "FF-CONTROL".
    WHEN "SOCO" THEN cFile-to-use = "CODES".
    WHEN "SOLH" THEN cFile-to-use = "CODES".
    WHEN "SOSE" THEN cFile-to-use = "SERIAL".
    WHEN "SOSO" THEN DO:
        IF  cMemo-code MATCHES "*!A*" THEN cFile-to-use = "NO".
        ELSE DO:
         IF NUM-ENTRIES(convMemo-code) = 1 THEN cFile-to-use = "SERVICE".
         IF NUM-ENTRIES(convMemo-code) = 3 THEN cFile-to-use = "SERVICE-LINE".
       END.
    END.
    WHEN "SOST" THEN cFile-to-use = "FF-CODES".
    WHEN "SOSY" THEN cFile-to-use = "BOM".
    WHEN "SOTE" THEN DO:
        IF NUM-ENTRIES(convMemo-code) = 1  THEN cFile-to-use = "TECH".
        IF NUM-ENTRIES(convMemo-code) = 3 THEN cFile-to-use  = "TECH-CAL".
        IF NUM-ENTRIES(convMemo-code) = 2 THEN DO:
            FIND vendor WHERE vendor.vendor-code = ENTRY(2,convMemo-code)
                NO-LOCK NO-ERROR.
            FIND FIRST ITEM WHERE ITEM.item-no = ENTRY(2,convMemo-code)
                NO-LOCK NO-ERROR.
            FIND svc-terr WHERE svc-terr.terr-code = ENTRY(2,convMemo-code)
                NO-LOCK NO-ERROR.
            IF AVAIL vendor THEN cFile-to-use = "TECH-VEND".
            IF AVAIL ITEM  THEN cFile-to-use = "TECH-ITEM".
            IF AVAIL svc-terr THEN cFile-to-use = "TECH-TERR".
        END.

    END.
    WHEN "SOWV" THEN cFile-to-use = "WARRANTY".
    WHEN "SOTY" THEN do:
        IF num-entries(convMemo-code) = 1 THEN cFile-to-use = "SVC-TERR".
        IF NUM-ENTRIES(convMemo-code) = 2 THEN cFile-to-use = "TERR-ZIP".
    END.
    WHEN "TRTR" THEN cFile-to-use = "INV-TFRH".
    WHEN "TRTL" THEN cFile-to-use = "INV-TFR".
        OTHERWISE cFile-to-use = "NO".
END CASE.
if not can-do( "PMPF,PMPL,PMPP,PMPC",cmemo-source) then 
cKey-list = convmemo-code.
else ckey-list = entry(1,convmemo-code) + "," + substring(cmemo-source, 4,1) + "," + 
                     entry(2,convmemo-code).
END PROCEDURE.
