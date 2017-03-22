/* aoaParam.i */

/* security & set paramStr */
{aoa/includes/aoaDefs.i}

&IF "{&aoaParam}" EQ "YES" &THEN
    RUN aoa/aoaParam.w (g_company,g_loc,paramStr).
&ELSE
    /* with no param screen, aoaParamDefs.i, needs this var */
    DEFINE VARIABLE ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ipcLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ipcParamStr AS CHARACTER NO-UNDO.
    ASSIGN
        ipcCompany  = g_company
        ipcLocation = g_loc
        ipcParamStr = paramStr
        .
    /* parse ipcParamStr, set param screen, set webpage URL */
    {aoa/includes/aoaParamDefs.i}
    /* launch web browser page */
    {aoa/includes/aoaURL.i}
&ENDIF
