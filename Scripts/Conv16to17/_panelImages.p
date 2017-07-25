/* _panelImages.p */

{_base.i}

DEFINE VARIABLE panelImage AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile1     AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTrigger   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButton    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLabel     AS CHARACTER NO-UNDO.

{_ttFileCode.i}

DEFINE BUFFER bttFileCode FOR ttFileCode.

DEFINE TEMP-TABLE ttButton NO-UNDO
    FIELD buttonName  AS CHARACTER
    FIELD buttonLabel AS CHARACTER
    FIELD panelImage  AS LOGICAL
    .

DEFINE STREAM sPanelImage.

INPUT STREAM sPanelImage FROM {&datLocation}\panelImages{&test}.dat NO-ECHO.
REPEAT:
    IMPORT STREAM sPanelImage panelImage.
    IF SEARCH("{&v16}" + panelImage) EQ ? THEN NEXT.
    EMPTY TEMP-TABLE ttFileCode.
    EMPTY TEMP-TABLE ttButton.
    INPUT FROM VALUE("{&v16}" + panelImage) NO-ECHO.
    REPEAT:
        IMPORT UNFORMATTED cFile.
        cFile = REPLACE(cFile,"SmartViewer","SmartPanel").
        IF cFile BEGINS "DEFINE BUTTON" THEN DO:
            CREATE ttButton.
            ttButton.buttonName = ENTRY(3,cFile," ").
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            IMPORT UNFORMATTED cFile.
            ASSIGN
                cFile1 = TRIM(cFile)
                cFile1 = SUBSTR(cFile1,INDEX(cFile1,"~"") + 1)
                cFile1 = REPLACE(cFile1,"~"","")
                ttButton.buttonLabel = REPLACE(cFile1,"~&","")
                .
        END.
        ELSE IF INDEX(cFile,":PRIVATE-DATA") NE 0 THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            cFile1 = ENTRY(1,TRIM(cFile),":").
            IMPORT UNFORMATTED cFile.
            IF INDEX(cFile,"panel-image") NE 0 THEN DO:
                FIND FIRST ttButton
                     WHERE ttButton.buttonName EQ cFile1
                     NO-ERROR.
                IF AVAILABLE ttButton THEN
                ttButton.panelImage = YES.
            END.
        END.
        ELSE IF INDEX(cFile,"/* ***************************  Main Block") NE 0 THEN DO:
            RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            REPEAT:
                IMPORT UNFORMATTED cFile.
                IF cFile BEGINS "/* _UIB-CODE-BLOCK-END */" THEN DO:
                    FOR EACH ttButton
                        WHERE ttButton.panelImage EQ YES
                        BY ttButton.buttonName:
                        cFile1 = fAddComment("  ~{methods/setButton.i " + ttButton.buttonName + " ~"" + ttButton.buttonLabel + "~"}").
                        RUN pCodeFile (INPUT-OUTPUT idx, cFile1).
                    END.
                    LEAVE.
                END.
                RUN pCodeFile (INPUT-OUTPUT idx, cFile).
            END.
            RUN pCodeFile (INPUT-OUTPUT idx, "").
        END.
        RUN pCodeFile (INPUT-OUTPUT idx, cFile).
    END.
    INPUT CLOSE.
    OUTPUT TO VALUE("{&v17}" + panelImage).
    FOR EACH ttFileCode:
        IF ttFileCode.lnText NE "" THEN
        PUT UNFORMATTED ttFileCode.lnText SKIP.
        ELSE
        PUT UNFORMATTED SKIP(1).
    END.
    OUTPUT CLOSE.
END.
INPUT STREAM sPanelImage CLOSE.

{_pCodeFile.i}
