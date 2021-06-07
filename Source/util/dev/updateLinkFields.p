DEFINE VARIABLE bh AS HANDLE.
DEFINE VARIABLE qh AS HANDLE.
DEFINE VARIABLE bfh AS HANDLE.
DEFINE VARIABLE cFileList AS CHARACTER.
DEFINE VARIABLE cFieldList AS CHARACTER.
DEFINE VARIABLE iIndex AS INTEGER.
DEFINE VARIABLE iCtr AS INTEGER.
DEFINE VARIABLE jCtr AS INTEGER.
DEFINE VARIABLE kCtr AS INTEGER.
DEFINE VARIABLE cFromValue AS CHAR NO-UNDO.
DEFINE VARIABLE cToValue AS CHAR NO-UNDO.
DEFINE VARIABLE lCheckAndMove AS LOG NO-UNDO.
DEFINE VARIABLE cTestDir AS CHAR NO-UNDO.
DEFINE VARIABLE cTestFile AS CHAR NO-UNDO.
DEFINE VARIABLE iPos AS INT NO-UNDO.
DEFINE VARIABLE cOldFileName AS CHAR NO-UNDO.
DEFINE VARIABLE err-status AS INT NO-UNDO.
DEFINE VARIABLE cNewDir AS CHAR NO-UNDO. 

ASSIGN 
    lCheckAndMove = TRUE 
    cFromValue = "N:"
    cToValue = "M:"
    cFileList = "attach" + "," +
                "ap-invl" + "," +
                "emailconfig" + "," +
                "box-design-hdr" + "," +
                "box-design-hdr" + "," +
                "config" + "," +
                "ef" + "," +
                "employee" + "," +
                "employee" + "," +
                "itemfg" + "," +
                "itemfg" + "," +
                "mach" + "," +
                "mach" + "," +
                "notes" + "," +
                "notes" + "," +
                "oe-bolh" + "," +
                "po-ord" + "," +
                "prep" + "," +
                "prgrms" + "," +
                "prgrms" + "," +
                "stackPattern" + "," +
                "taskEmail" + "," +
                "userLanguage" + "," +
                "users" + "," +
                "users" + "," +
                "xmllayoutsform" + "," +
                "xmllayoutsformdesign" + "," +
                "sys-ctrl" + "," +
                "sys-ctrl" + "," +
                "sys-ctrl-shipto"
                .
    cFieldList = "attach-file" + "," +
                "scanned-image" + "," +
                "attachment" + "," +
                "box-3d-image" + "," +
                "box-image" + "," +
                "image_filename" + "," +
                "cad-image" + "," +
                "employeeImage" + "," +
                "image" + "," +
                "box-3d-image" + "," +
                "box-image" + "," +
                "image" + "," +
                "machineImage" + "," +
                "attachment" + "," +
                "attachmentName" + "," +
                "scanned-image" + "," +
                "scanned-image" + "," +
                "cad-image" + "," +
                "image" + "," +
                "menuImage" + "," +
                "stackImage" + "," +
                "attachment" + "," +
                "flagImage" + "," +
                "image_filename" + "," +
                "userImage" + "," +
                "imageName" + "," +
                "oneUpImageName" + "," +
                "descrip" + "," +
                "char-fld" + "," +
                "char-fld"
                .
                
DO iIndex = 1 TO NUM-ENTRIES(cFileList):
    CREATE BUFFER bh FOR TABLE ENTRY(iIndex,cFileList).
    bh:DISABLE-LOAD-TRIGGERS(FALSE).
    bfh = bh:BUFFER-FIELD(ENTRY(iIndex,cFieldList)).

    CREATE QUERY qh.
    qh:SET-BUFFERS(bh).
    IF bfh:EXTENT LT 2 THEN 
        qh:QUERY-PREPARE ("FOR EACH " + ENTRY(iIndex,cFileList) +
                        " EXCLUSIVE WHERE INDEX(" + ENTRY(iIndex,cFileList) + "." + 
                        ENTRY(iIndex,cFieldList) + ",'" + cFromValue + "') NE 0").
    ELSE 
        qh:QUERY-PREPARE ("FOR EACH " + ENTRY(iIndex,cFileList) + 
                        " EXCLUSIVE WHERE INDEX(" + ENTRY(iIndex,cFileList) + "." + 
                        ENTRY(iIndex,cFieldList) + "[1]" + ",'" + cFromValue + "') NE 0").
    qh:QUERY-OPEN.
    
    REPEAT TRANSACTION ON ERROR UNDO, NEXT:
        qh:GET-NEXT().
        IF qh:QUERY-OFF-END THEN LEAVE.

        IF bfh:EXTENT LT 2 THEN DO:
            ASSIGN 
                cOldFileName = bfh:BUFFER-VALUE 
                bfh:BUFFER-VALUE = REPLACE(bfh:BUFFER-VALUE,cFromValue,cToValue)
                bfh:BUFFER-VALUE = REPLACE(bfh:BUFFER-VALUE,"/","\").
            IF lCheckAndMove THEN DO:                          /* Move files? */
                IF SEARCH(bfh:BUFFER-VALUE) EQ ? 
                AND SEARCH(cOldFileName) NE ? THEN DO:
                    ASSIGN
                        iPos = NUM-ENTRIES(bfh:BUFFER-VALUE,"\")
                        cTestFile =  ENTRY(iPos,bfh:BUFFER-VALUE,"\")
                        cTestDir = REPLACE(bfh:BUFFER-VALUE,"\" + cTestfile,"")
                        kCtr = NUM-ENTRIES(cTestDir,"\")
                        cNewDir = ENTRY(1,cTestDir,"\").
                    DO jCtr = 2 TO kCtr:
                        ASSIGN 
                            cNewDir = cNewDir + "\" + ENTRY(jCtr,cTestDir,"\"). 
                        OS-CREATE-DIR VALUE(cNewDir).
                    END.
                    OS-COPY VALUE(cOldFileName) VALUE(cTestDir).
                END.
            END.
        END.
        ELSE DO iCtr = 1 TO bfh:EXTENT:
            ASSIGN 
                cOldFileName = bfh:BUFFER-VALUE(iCtr)
                bfh:BUFFER-VALUE(iCtr) = REPLACE(bfh:BUFFER-VALUE(iCtr),cFromValue,cToValue)
                bfh:BUFFER-VALUE(iCtr) = REPLACE(bfh:BUFFER-VALUE(iCtr),"/","\").
            IF lCheckAndMove THEN DO:                          /* Move files? */
                IF SEARCH(bfh:BUFFER-VALUE(iCtr)) EQ ? 
                AND SEARCH(cOldFileName) NE ? THEN DO:
                    ASSIGN
                        iPos = NUM-ENTRIES(bfh:BUFFER-VALUE(iCtr),"\")
                        cTestFile =  ENTRY(iPos,bfh:BUFFER-VALUE(iCtr),"\")
                        cTestDir = REPLACE(bfh:BUFFER-VALUE(iCtr),"\" + cTestfile,"")
                        kCtr = NUM-ENTRIES(cTestDir,"\")
                        cNewDir = ENTRY(1,cTestDir,"\").
                    DO jCtr = 2 TO kCtr:
                        ASSIGN 
                            cNewDir = cNewDir + "\" + ENTRY(jCtr,cTestDir,"\"). 
                        OS-CREATE-DIR VALUE(cNewDir).
                    END.
                    OS-COPY VALUE(cOldFileName) VALUE(cTestDir).
                END.
            END.
        END.
    END.
    qh:QUERY-CLOSE().

    bh:BUFFER-RELEASE().
    DELETE OBJECT bh.
END.
