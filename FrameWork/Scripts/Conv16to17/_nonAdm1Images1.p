/* _nonAdmImage1Images1.p */

{_base.i}

DEFINE VARIABLE nonAdmImage AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile1      AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTrigger     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButton     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLabel      AS CHARACTER NO-UNDO.

{_ttFileCode.i}

DEFINE BUFFER bttFileCode FOR ttFileCode.

DEFINE TEMP-TABLE ttButton NO-UNDO
    FIELD buttonName   AS CHARACTER
    FIELD buttonLabel  AS CHARACTER
    FIELD ribbonButton AS LOGICAL
    .

DEFINE STREAM sNonAdmImage.

INPUT STREAM sNonAdmImage FROM {&datLocation}\nonAdm1Images1{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sNonAdmImage nonAdmImage.
    IF SEARCH("{&v16}" + nonAdmImage) EQ ? THEN NEXT.
    EMPTY TEMP-TABLE ttFileCode.
    EMPTY TEMP-TABLE ttButton.
    INPUT FROM VALUE("{&v16}" + nonAdmImage) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        IF cFile BEGINS "DEFINE BUTTON" THEN DO:
            CREATE ttButton.
            ttButton.buttonName = ENTRY(3,cFile," ").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            IMPORT UNFORMATTED cFile.
            cFile1 = TRIM(cFile).
            ttButton.buttonLabel = REPLACE(REPLACE(ENTRY(2,cFile1," "),"~&",""),"~"","").
        END.
        ELSE IF INDEX(cFile,":PRIVATE-DATA") NE 0 THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile1 = ENTRY(1,TRIM(cFile),":").
            IMPORT UNFORMATTED cFile.
            IF INDEX(cFile,"ribbon-button") NE 0 THEN DO:
                FIND FIRST ttButton
                     WHERE ttButton.buttonName EQ cFile1
                     NO-ERROR.
                IF AVAILABLE ttButton THEN
                ttButton.ribbonButton = YES.
            END.
        END.
        ELSE IF cFile BEGINS "ON CHOOSE OF" THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cTrigger = ENTRY(4,cFile," ").
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF INDEX(cFile,"LABEL") NE 0 AND
                   INDEX(nonAdmImage,"contlbl") EQ 0 THEN DO:
                    cLabel = ENTRY(NUM-ENTRIES(cFile," "),cFile," ").
                    IF INDEX(cLabel,"~"") NE 0 THEN DO:
                        ASSIGN
                            cButton = TRIM(ENTRY(1,cFile,":"))
                            cButton = REPLACE(cButton,"~{&SELF-NAME}",cTrigger)
                            cLabel = REPLACE(REPLACE(REPLACE(cLabel,"~"",""),"~&",""),".","")
                            cFile = fAddComment("    ~{methods/setButton.i " + cButton + " ~"" + cLabel + "~"}")
                            .
                        FIND FIRST bttFileCode
                             WHERE bttFileCode.ln EQ idx
                               AND TRIM(bttFileCode.lnText) EQ "ASSIGN"
                             NO-ERROR.
                        IF AVAILABLE bttFileCode THEN
                        DELETE bttFileCode.
                    END.
                END.
                IF cFile EQ "/* _UIB-CODE-BLOCK-END */" THEN
                LEAVE.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
        END.
        ELSE IF INDEX(cFile,"embedfinalize-nonadm.i") NE 0 THEN DO:
            FOR EACH ttButton WHERE ttButton.ribbonButton EQ YES:
                cFile1 = fAddComment("    ~{methods/setButton.i " + ttButton.buttonName + " ~"" + ttButton.buttonLabel + "~"}").
                RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
            END.
        END.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + nonAdmImage).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sNonAdmImage CLOSE.

{_pCodeFile.i}
