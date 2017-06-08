        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* ************************  Frame Definitions  *********************** */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED 'DEFINE FRAME F-Main' + CHR(10).
        PUT UNFORMATTED '     br_table AT ROW 1 COL 1' + CHR(10).
        PUT UNFORMATTED '    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY ' + CHR(10).
        PUT UNFORMATTED '         SIDE-LABELS NO-UNDERLINE THREE-D ' + CHR(10).
        PUT UNFORMATTED '         AT COL 1 ROW 1 SCROLLABLE .' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* *********************** Procedure Settings ************************ */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _PROCEDURE-SETTINGS' + CHR(10).
        PUT UNFORMATTED '/* Settings for THIS-PROCEDURE' + CHR(10).
        PUT UNFORMATTED '   Type: SmartDataBrowser' + CHR(10).

    IF rsVersion:SCREEN-VALUE = "1" THEN
        PUT UNFORMATTED '   Data Source: "' + LOWER(REPLACE(cShortFileName,"brw","sdo")) + '"' + CHR(10).
    ELSE
        PUT UNFORMATTED '   Data Source: "' + LOWER(REPLACE(cShortFileName,"\b","\sdo")) + '"' + CHR(10).

        PUT UNFORMATTED '   Allow: Basic,Browse' + CHR(10).
        PUT UNFORMATTED '   Frames: 1' + CHR(10).
        PUT UNFORMATTED '   Add Fields to: Neither' + CHR(10).
        PUT UNFORMATTED '   Other Settings: PERSISTENT-ONLY COMPILE' + CHR(10).
        PUT UNFORMATTED ' */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* This procedure should always be RUN PERSISTENT.  Report the error,  */' + CHR(10).
        PUT UNFORMATTED '/* then cleanup and return.                                            */' + CHR(10).
        PUT UNFORMATTED 'IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:' + CHR(10).
        PUT UNFORMATTED '  MESSAGE "~{&FILE-NAME} should only be RUN PERSISTENT.":U' + CHR(10).
        PUT UNFORMATTED '          VIEW-AS ALERT-BOX ERROR BUTTONS OK.' + CHR(10).
        PUT UNFORMATTED '  RETURN.' + CHR(10).
        PUT UNFORMATTED 'END.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME _END-PROCEDURE-SETTINGS' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* *************************  Create Window  ************************** */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _CREATE-WINDOW' + CHR(10).
        PUT UNFORMATTED '/* DESIGN Window definition (used by the UIB) ' + CHR(10).
        PUT UNFORMATTED '  CREATE WINDOW bTableWin ASSIGN' + CHR(10).
        PUT UNFORMATTED '         HEIGHT             = 6.84' + CHR(10).
        PUT UNFORMATTED '         WIDTH              = 138.6.' + CHR(10).
        PUT UNFORMATTED '/* END WINDOW DEFINITION */' + CHR(10).
        PUT UNFORMATTED '                                                                        */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB bTableWin ' + CHR(10).
        PUT UNFORMATTED '/* ************************* Included-Libraries *********************** */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{src/adm2/browser.i}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* ***********  Runtime Attributes and AppBuilder Settings  *********** */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES' + CHR(10).
        PUT UNFORMATTED '/* SETTINGS FOR WINDOW bTableWin' + CHR(10).
        PUT UNFORMATTED '  NOT-VISIBLE,,RUN-PERSISTENT                                           */' + CHR(10).
        PUT UNFORMATTED '/* SETTINGS FOR FRAME F-Main' + CHR(10).
        PUT UNFORMATTED '   NOT-VISIBLE Size-to-Fit                                              */' + CHR(10).
        PUT UNFORMATTED '/* BROWSE-TAB br_table 1 F-Main */' + CHR(10).
        PUT UNFORMATTED 'ASSIGN ' + CHR(10).
        PUT UNFORMATTED '       FRAME F-Main:SCROLLABLE       = FALSE' + CHR(10).
        PUT UNFORMATTED '       FRAME F-Main:HIDDEN           = TRUE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED 'ASSIGN ' + CHR(10).
        PUT UNFORMATTED '       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _RUN-TIME-ATTRIBUTES-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* Setting information for Queries and Browse Widgets fields            */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table' + CHR(10).
        PUT UNFORMATTED '/* Query rebuild information for BROWSE br_table' + CHR(10).
        PUT UNFORMATTED '     _TblList          = "rowObject"' + CHR(10).
        PUT UNFORMATTED '     _Options          = "NO-LOCK INDEXED-REPOSITION"' + CHR(10).

        ASSIGN
            iCtr = 1.
        FOR EACH ttFile BREAK BY ttFile.ttOrder:
            IF ttFile.ttNumExtents < 2 THEN DO:
                PUT UNFORMATTED '     _FldNameList[' + string(ictr) + ']   > _<SDO>.rowobject.' + ttFile.ttFieldName + CHR(10).
                PUT UNFORMATTED '"rowObject.' + ttFile.ttFieldName + '" "' + ttFile.ttLabel + '" ? "' + ttFile.ttDataType + '" ? ? ? ? ? ? no ? ' + 
                                'no no "' + TRIM(STRING(ttFile.ttObjWidth,">>9.9")) + '" yes no no "U" "" ""' + CHR(10).
                ASSIGN
                    iCtr = iCtr + 1.
            END.
            ELSE DO:
                DO jCtr = 1 TO ttFile.ttNumExtents:
                    PUT UNFORMATTED '     _FldNameList[' + string(ictr) + ']   > _<SDO>.rowobject.' + ttFile.ttFieldName + STRING(jCtr) + CHR(10).
                    PUT UNFORMATTED '"rowObject.' + ttFile.ttFieldName + STRING(jCtr) + '" "' + ttFile.ttLabel + STRING(jCtr) + '" ? "' + ttFile.ttDataType + '" ? ? ? ? ? ? no ? ' + 
                                    'no no "' + TRIM(STRING(ttFile.ttObjWidth,">>9.9")) + '" yes no no "U" "" ""' + CHR(10).
                    ASSIGN
                        iCtr = iCtr + 1.
                END.
            END.
        END.

        PUT UNFORMATTED '     _Query            is NOT OPENED' + CHR(10).
        PUT UNFORMATTED '*/  /* BROWSE br_table */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main' + CHR(10).
        PUT UNFORMATTED '/* Query rebuild information for FRAME F-Main' + CHR(10).
        PUT UNFORMATTED '     _Options          = "NO-LOCK"' + CHR(10).
        PUT UNFORMATTED '     _Query            is NOT OPENED' + CHR(10).
        PUT UNFORMATTED '*/  /* FRAME F-Main */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&Scoped-define BROWSE-NAME br_table' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK bTableWin ' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* ***************************  Main Block  *************************** */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          ' + CHR(10).
        PUT UNFORMATTED 'RUN initializeObject.        ' + CHR(10).
        PUT UNFORMATTED '&ENDIF' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).

    IF rsVersion:SCREEN-VALUE = "1" THEN
        PUT UNFORMATTED '~{sys/_com/brwComProcs.i}' + CHR(10).
    ELSE
        PUT UNFORMATTED '~{br/comprocs.i 0}' + CHR(10).

        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* **********************  Internal Procedures  *********************** */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Include  _DEFAULT-DISABLE' + CHR(10).
        PUT UNFORMATTED 'PROCEDURE disable_UI :' + CHR(10).
        PUT UNFORMATTED '/*------------------------------------------------------------------------------' + CHR(10).
        PUT UNFORMATTED '  Purpose:     DISABLE the User Interface' + CHR(10).
        PUT UNFORMATTED '  Parameters:  <none>' + CHR(10).
        PUT UNFORMATTED '  Notes:       Here we clean-up the user-interface by deleting' + CHR(10).
        PUT UNFORMATTED '               dynamic widgets we have created and/or hide ' + CHR(10).
        PUT UNFORMATTED '               frames.  This procedure is usually called when' + CHR(10).
        PUT UNFORMATTED '               we are ready to "clean-up" after running.' + CHR(10).
        PUT UNFORMATTED '------------------------------------------------------------------------------*/' + CHR(10).
        PUT UNFORMATTED '  /* Hide all frames. */' + CHR(10).
        PUT UNFORMATTED '  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.' + CHR(10).
        PUT UNFORMATTED 'END PROCEDURE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipspecific bTableWin ' + CHR(10).
        PUT UNFORMATTED 'PROCEDURE ipspecific :' + CHR(10).
        PUT UNFORMATTED '/*------------------------------------------------------------------------------' + CHR(10).
        PUT UNFORMATTED '  Purpose:     ' + CHR(10).
        PUT UNFORMATTED '  Parameters:  <none>' + CHR(10).
        PUT UNFORMATTED '  Notes:       ' + CHR(10).
        PUT UNFORMATTED '------------------------------------------------------------------------------*/' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '    ASSIGN' + CHR(10).
        PUT UNFORMATTED '        cBaseFileName = "' + ttFile.ttFileName + '".' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED 'END PROCEDURE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).


