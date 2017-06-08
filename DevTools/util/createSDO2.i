        PUT UNFORMATTED '/* Setting information for Queries and Browse Widgets fields            */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main' + CHR(10).
        PUT UNFORMATTED '/* Query rebuild information for SmartDataObject Query-Main' + CHR(10).
        PUT UNFORMATTED '     _TblList          = "' + LDBNAME("DICTDB") + '.' + ttFile.ttFileName + '"' + CHR(10).
        PUT UNFORMATTED '     _Options          = "NO-LOCK INDEXED-REPOSITION"' + CHR(10).
        
        ASSIGN
            iCtr = 1.
        FOR EACH ttFile BREAK BY ttFile.ttOrder:
            IF INDEX(ttFile.ttFormat,"x") > 0 THEN DO:
                IF INDEX(ttFile.ttFormat,"(") > 0 THEN ASSIGN
                    iPosStart = INDEX(ttFile.ttFormat,"(") + 1
                    iPosEnd = INDEX(ttFile.ttFormat,")") 
                    deWidth = DECIMAL(SUBSTRING(ttFile.ttFormat,iPosStart,iPosEnd - iPosStart)).
                ELSE ASSIGN
                    deWidth = DECIMAL(LENGTH(ttFile.ttFormat)).
            END.
            ELSE IF INDEX(ttFile.ttFormat,"/") > 0 THEN ASSIGN
                deWidth = DECIMAL(LENGTH(SUBSTRING(ttFile.ttFormat,1,INDEX(ttFile.ttFormat,"/") - 1))).
            ELSE ASSIGN 
                deWidth = DECIMAL(LENGTH(ttFile.ttFormat)).
            ASSIGN
                deWidth = MAXIMUM(deWidth,DECIMAL(LENGTH(ttFile.ttLabel,"COLUMN"))).
            IF ttFile.ttNumExtents < 2 THEN DO:
                PUT UNFORMATTED '     _FldNameList[' + string(ictr) + ']   > ' + LDBNAME("DICTDB") + '.' + ttFile.ttFileName + '.' + ttFile.ttFieldName + CHR(10).
                PUT UNFORMATTED '"' + ttFile.ttFieldName + '" "' + ttFile.ttFieldName + '" ? ? "' + ttFile.ttDataType + '" ? ? ? ? ? ? yes ? ' + 
                                STRING(ttFile.ttMandatory) + ' ' + TRIM(STRING(deWidth,">>9.9")) + ' yes' + CHR(10).
                ASSIGN
                    iCtr = iCtr + 1.
            END.
            ELSE DO:
                DO jCtr = 1 TO ttFile.ttNumExtents:
                    PUT UNFORMATTED '     _FldNameList[' + string(ictr) + ']   > ' + LDBNAME("DICTDB") + '.' + ttFile.ttFileName + '.' + ttFile.ttFieldName + '[' + STRING(jCtr) + ']' + CHR(10).
                    PUT UNFORMATTED '"' + ttFile.ttFieldName + '[' + STRING(jCtr) + ']' + '" "' + ttFile.ttFieldName + STRING(jCtr) + '" ? ? "' + ttFile.ttDataType + '" ? ? ? ? ? ? yes ? ' + 
                                    STRING(ttFile.ttMandatory) + ' ' + TRIM(STRING(deWidth,">>9.9")) + ' yes' + CHR(10).
                    ASSIGN
                        iCtr = iCtr + 1.
                END.
            END.
        END.
        PUT UNFORMATTED '     _Design-Parent    is WINDOW dTables @ ( 1.16 , 2.6 )' + CHR(10).
        PUT UNFORMATTED '*/  /* QUERY Query-Main */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables ' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* ***************************  Main Block  *************************** */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          ' + CHR(10).
        PUT UNFORMATTED '    RUN initializeObject.' + CHR(10).
        PUT UNFORMATTED '  &ENDIF' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-START}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* **********************  Internal Procedures  *********************** */' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE zRefQry dTables  _DB-REQUIRED' + CHR(10).
        PUT UNFORMATTED 'PROCEDURE chgQry :' + CHR(10).
        PUT UNFORMATTED '/*------------------------------------------------------------------------------' + CHR(10).
        PUT UNFORMATTED '  Purpose:     ' + CHR(10).
        PUT UNFORMATTED '  Parameters:  <none>' + CHR(10).
        PUT UNFORMATTED '  Notes:       ' + CHR(10).
        PUT UNFORMATTED '------------------------------------------------------------------------------*/ ' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION(~'closeQuery~':U ).' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION(~'openQuery~':U ).' + CHR(10).
        PUT UNFORMATTED 'END PROCEDURE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-END}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-START}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE zChgQry dTables  _DB-REQUIRED' + CHR(10).
        PUT UNFORMATTED 'PROCEDURE chgQry1 :' + CHR(10).
        PUT UNFORMATTED '/*------------------------------------------------------------------------------' + CHR(10).
        PUT UNFORMATTED '  Purpose:     For backwards compatibility with 10.6' + CHR(10).
        PUT UNFORMATTED '  Parameters:  ipQueryString' + CHR(10).
        PUT UNFORMATTED '  Notes:       entry 1 of ipQueryString is new where clause (WHERE keyword not reqd)' + CHR(10).
        PUT UNFORMATTED '               entry 2 (optional) is new sort order (BY keyword IS optional)' + CHR(10).
        PUT UNFORMATTED '------------------------------------------------------------------------------*/ ' + CHR(10).
        PUT UNFORMATTED '    DEF INPUT PARAMETER ipQueryString AS CHAR NO-UNDO.' + CHR(10).
        PUT UNFORMATTED '    DEF VAR cQueryWhere AS CHAR NO-UNDO.' + CHR(10).
        PUT UNFORMATTED '    DEF VAR cQuerySort AS CHAR NO-UNDO.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '    IF NUM-ENTRIES(ipQueryString) > 1 THEN DO:' + CHR(10).
        PUT UNFORMATTED '        ASSIGN' + CHR(10).
        PUT UNFORMATTED '            cQueryWhere = ENTRY(1,ipQueryString)' + CHR(10).
        PUT UNFORMATTED '            cQuerySort = ENTRY(2,ipQueryString).' + CHR(10).
        PUT UNFORMATTED '        IF SUBSTRING(cQuerySort,1,2) <> "BY"' + CHR(10).
        PUT UNFORMATTED '        AND cQuerySort <> "" THEN ASSIGN' + CHR(10).
        PUT UNFORMATTED '            cQuerySort = "BY " + cQuerySort.' + CHR(10).
        PUT UNFORMATTED '        DYNAMIC-FUNCTION(~'setQueryWhere~':U IN THIS-PROCEDURE, cQueryWhere).' + CHR(10).
        PUT UNFORMATTED '        DYNAMIC-FUNCTION(~'setQuerySort~':U IN THIS-PROCEDURE, cQuerySort).' + CHR(10).
        PUT UNFORMATTED '    END.' + CHR(10).
        PUT UNFORMATTED '    ELSE DO:' + CHR(10).
        PUT UNFORMATTED '        ASSIGN' + CHR(10).
        PUT UNFORMATTED '            cQueryWhere = ipQueryString.' + CHR(10).
        PUT UNFORMATTED '        DYNAMIC-FUNCTION(~'setQueryWhere~':U IN THIS-PROCEDURE, cQueryWhere).' + CHR(10).
        PUT UNFORMATTED '    END.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION(~'closeQuery~':U ).' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION(~'openQuery~':U ).' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED 'END PROCEDURE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-END}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE' + CHR(10).
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
        PUT UNFORMATTED '~{&DB-REQUIRED-START}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject dTables  _DB-REQUIRED' + CHR(10).
        PUT UNFORMATTED 'PROCEDURE initializeObject :' + CHR(10).
        PUT UNFORMATTED '/*------------------------------------------------------------------------------' + CHR(10).
        PUT UNFORMATTED '  Purpose:     Super Override' + CHR(10).
        PUT UNFORMATTED '  Parameters:  ' + CHR(10).
        PUT UNFORMATTED '  Notes:       ' + CHR(10).
        PUT UNFORMATTED '------------------------------------------------------------------------------*/' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '  RUN SUPER.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED 'END PROCEDURE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-END}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-START}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE zMoveIt dTables  _DB-REQUIRED' + CHR(10).
        PUT UNFORMATTED 'PROCEDURE moveIt :' + CHR(10).
        PUT UNFORMATTED '/*------------------------------------------------------------------------------' + CHR(10).
        PUT UNFORMATTED '  Purpose:     ' + CHR(10).
        PUT UNFORMATTED '  Parameters:  <none>' + CHR(10).
        PUT UNFORMATTED '  Notes:       ' + CHR(10).
        PUT UNFORMATTED '------------------------------------------------------------------------------*/' + CHR(10).
        PUT UNFORMATTED '    DEF INPUT PARAMETER ipRowid AS ROWID NO-UNDO.' + CHR(10).
        PUT UNFORMATTED '    ~{set RebuildOnRepos TRUE}.' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION("fetchrowident":U IN TARGET-PROCEDURE, STRING(ipRowid), "").' + CHR(10).
        PUT UNFORMATTED 'END PROCEDURE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-END}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-START}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE zChgQry dTables  _DB-REQUIRED' + CHR(10).
        PUT UNFORMATTED 'PROCEDURE zChgQry :' + CHR(10).
        PUT UNFORMATTED '/*------------------------------------------------------------------------------' + CHR(10).
        PUT UNFORMATTED '  Purpose:     Changes where-clause and (optional) sort-order of query' + CHR(10).
        PUT UNFORMATTED '  Parameters:  ipQueryString' + CHR(10).
        PUT UNFORMATTED '  Notes:       entry 1 of ipQueryString is new where clause (WHERE keyword not reqd)' + CHR(10).
        PUT UNFORMATTED '               entry 2 (optional) is new sort order (BY keyword IS optional)' + CHR(10).
        PUT UNFORMATTED '------------------------------------------------------------------------------*/ ' + CHR(10).
        PUT UNFORMATTED '    DEF INPUT PARAMETER ipQueryString AS CHAR NO-UNDO.' + CHR(10).
        PUT UNFORMATTED '    DEF VAR cQueryWhere AS CHAR NO-UNDO.' + CHR(10).
        PUT UNFORMATTED '    DEF VAR cQuerySort AS CHAR NO-UNDO.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '    IF NUM-ENTRIES(ipQueryString) > 1 THEN DO:' + CHR(10).
        PUT UNFORMATTED '        ASSIGN' + CHR(10).
        PUT UNFORMATTED '            cQueryWhere = ENTRY(1,ipQueryString)' + CHR(10).
        PUT UNFORMATTED '            cQuerySort = ENTRY(2,ipQueryString).' + CHR(10).
        PUT UNFORMATTED '        IF SUBSTRING(cQuerySort,1,2) <> "BY"' + CHR(10).
        PUT UNFORMATTED '        AND cQuerySort <> "" THEN ASSIGN' + CHR(10).
        PUT UNFORMATTED '            cQuerySort = "BY " + cQuerySort.' + CHR(10).
        PUT UNFORMATTED '        DYNAMIC-FUNCTION(~'setQueryWhere~':U IN THIS-PROCEDURE, cQueryWhere).' + CHR(10).
        PUT UNFORMATTED '        DYNAMIC-FUNCTION(~'setQuerySort~':U IN THIS-PROCEDURE, cQuerySort).' + CHR(10).
        PUT UNFORMATTED '    END.' + CHR(10).
        PUT UNFORMATTED '    ELSE DO:' + CHR(10).
        PUT UNFORMATTED '        ASSIGN' + CHR(10).
        PUT UNFORMATTED '            cQueryWhere = ipQueryString.' + CHR(10).
        PUT UNFORMATTED '        DYNAMIC-FUNCTION(~'setQueryWhere~':U IN THIS-PROCEDURE, cQueryWhere).' + CHR(10).
        PUT UNFORMATTED '    END.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION(~'closeQuery~':U ).' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION(~'openQuery~':U ).' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED 'END PROCEDURE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-END}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-START}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE zRefQry dTables  _DB-REQUIRED' + CHR(10).
        PUT UNFORMATTED 'PROCEDURE zRefQry :' + CHR(10).
        PUT UNFORMATTED '/*------------------------------------------------------------------------------' + CHR(10).
        PUT UNFORMATTED '  Purpose:     ' + CHR(10).
        PUT UNFORMATTED '  Parameters:  <none>' + CHR(10).
        PUT UNFORMATTED '  Notes:       ' + CHR(10).
        PUT UNFORMATTED '------------------------------------------------------------------------------*/ ' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION(~'closeQuery~':U ).' + CHR(10).
        PUT UNFORMATTED '    DYNAMIC-FUNCTION(~'openQuery~':U ).' + CHR(10).
        PUT UNFORMATTED 'END PROCEDURE.' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '/* _UIB-CODE-BLOCK-END */' + CHR(10).
        PUT UNFORMATTED '&ANALYZE-RESUME' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        PUT UNFORMATTED '~{&DB-REQUIRED-END}' + CHR(10).
        PUT UNFORMATTED '' + CHR(10).
        
        
        
        

