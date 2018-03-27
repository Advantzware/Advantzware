&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : methods/listrqst.i
    Purpose     : internal procedure for listrqst.w

    Syntax      : {methods/listrqst.i}

    Description : List Request internal procedures

    Author(s)   : Ron Stark
    Created     : 07/25/98
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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
         HEIGHT             = 12.57
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE List-Name Include 
PROCEDURE List-Name :
/*------------------------------------------------------------------------------
  Purpose:     List Name supplied to listrqst.w (frame container)
  Parameters:  listname
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER list-name AS CHARACTER NO-UNDO.

  FIND prgrms WHERE prgrms.prgmname = listname NO-LOCK NO-ERROR.
  request-header = IF AVAILABLE prgrms THEN prgrms.prgtitle
                   ELSE "* * List Title Missing * *".
  RUN Get_Procedure IN Persistent-Handle (listname,OUTPUT list-name,no).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Output-Name Include 
PROCEDURE Output-Name :
/*------------------------------------------------------------------------------
  Purpose:     Supply calling program output file name
  Parameters:  OUTPUT request-header.
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER output-name AS CHARACTER NO-UNDO.

  output-name = request-header + ".rpt".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Program-Name Include 
PROCEDURE Program-Name :
/*------------------------------------------------------------------------------
  Purpose:     Program Name supplied to listrqst.w (frame container)
  Parameters:  listname
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER progname AS CHARACTER NO-UNDO.

  progname = listname.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus Include 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus called by initiating program
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(h_lstframe) THEN
  RUN Set-Focus IN h_lstframe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

