/* aoaParam.i */

/* security & set paramStr */
{aoa/includes/aoaDefs.i}

DEFINE VARIABLE h_aoaParam AS HANDLE NO-UNDO.

&IF "{&aoaParam}" EQ "YES" &THEN
    IF Consultingwerk.WindowIntegrationKit.WinKitSettings:WinKitActive EQ TRUE THEN DO:
        RUN AOA/aoaParam.w PERSISTENT SET h_aoaParam (g_company,g_loc,paramStr).
        RUN dispatch IN h_aoaParam ("initialize") .
    END.
    ELSE
    RUN AOA/aoaParam.w (g_company,g_loc,paramStr) .
&ELSE
    /* with no param screen, aoaParamDefs.i, needs these vars */
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
