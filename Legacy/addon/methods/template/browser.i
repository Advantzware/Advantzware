&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : browser.i
    Purpose     : Browser Triggers and Internal Procedures

    Syntax      : {methods/template/browser.i}

    Description : Browser Triggers and Internal Procedures

    Author(s)   : Ron Stark
    Created     : 02/06/98
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE current-rowid AS ROWID NO-UNDO.

&IF "{&IAMWHAT}" = "" &THEN
DEFINE VARIABLE save-rowid AS ROWID NO-UNDO.
DEFINE VARIABLE find-auto AS LOGICAL NO-UNDO.
{methods/defines/hndlset.i}
&ENDIF

{methods/defines/noreckey.i}
{methods/high_low.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 16.86
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

ON CTRL-A OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
    DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
    
    hTable = BUFFER {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}:HANDLE.
    RUN system/CallAudit.p ("{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}",hTable,"Browser",PROGRAM-NAME(1)).
END.

&IF "{&IAMWHAT}" = "" &THEN
  &IF DEFINED(BRWSDEFS) NE 0 &THEN
{methods/browsers/{&BRWSDEFS}.i}
  &ELSE
{methods/browsers/{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.i}
  &ENDIF
&ENDIF
{methods/template/brwsrtrg.i}
{methods/enhance.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Change-Order Include 
PROCEDURE Change-Order :
/*------------------------------------------------------------------------------
  Purpose:     Change browser display & sort order.
  Parameters:  Change order value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER b-order AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      browse-order:SCREEN-VALUE = b-order
      browse-order.
    CASE browse-order:
      {methods/template/chngord.i 1}
      {methods/template/chngord.i 2}
      {methods/template/chngord.i 3}
      {methods/template/chngord.i 4}
      {methods/template/chngord.i 5}
      {methods/template/chngord.i 6}
      {methods/template/chngord.i 7}
      {methods/template/chngord.i 8}
      {methods/template/chngord.i 9}
      {methods/template/chngord.i 10}
      {methods/template/chngord.i 11}
      {methods/template/chngord.i 12}
      {methods/template/chngord.i 13}
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Clear_Auto_Find Include 
PROCEDURE Clear_Auto_Find :
/*------------------------------------------------------------------------------
  Purpose:     Clear Auto Find Field to prevent problems when returning from viewer
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    auto_find:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''
    auto_find.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation Include 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     Disable Navigation Panel when viewer in Update mode
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('disable':U) NO-ERROR"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation Include 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     Enable Navigation Panel when viewer not in Update mode
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('enable':U) NO-ERROR"}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Find-Record Include 
PROCEDURE Find-Record :
/*------------------------------------------------------------------------------
  Purpose:     Find Record
  Parameters:  Change order value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER b-order AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      browse-order:SCREEN-VALUE = b-order
      browse-order.
    CASE browse-order:
      {methods/template/findrecd.i 1}
      {methods/template/findrecd.i 2}
      {methods/template/findrecd.i 3}
      {methods/template/findrecd.i 4}
      {methods/template/findrecd.i 5}
      {methods/template/findrecd.i 6}
      {methods/template/findrecd.i 7}
      {methods/template/findrecd.i 8}
      {methods/template/findrecd.i 9}
      {methods/template/findrecd.i 10}
      {methods/template/findrecd.i 11}
      {methods/template/findrecd.i 12}
      {methods/template/findrecd.i 13}
    END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-first Include 
PROCEDURE local-get-first :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-first':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-last Include 
PROCEDURE local-get-last :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-last':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-next Include 
PROCEDURE local-get-next :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-next':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-prev Include 
PROCEDURE local-get-prev :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-prev':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize Include 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  &IF LOOKUP("{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}",
             "address note phone {custom/brwsinit.i}"," ") NE 0 &THEN
  {methods/browsers/initial/{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.i}
  &ENDIF

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/brwsinit.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view Include 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Run-Search Include 
PROCEDURE Run-Search :
/*------------------------------------------------------------------------------
  Purpose:     Called from Smart Window to apply run search program.
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE search-rowid AS ROWID NO-UNDO.

  RUN "searches/{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.p" (OUTPUT search-rowid).
  IF search-rowid = ? THEN
  RETURN.
  REPOSITION {&BROWSE-NAME} TO ROWID search-rowid.
  APPLY "VALUE-CHANGED" TO {&BROWSE-NAME} IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus Include 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Called from Smart Window to apply focus to brower widget.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i {&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

