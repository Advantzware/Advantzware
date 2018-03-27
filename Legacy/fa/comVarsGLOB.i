&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : pt/comVarsGLOB.i
    Purpose     : provice definition for variables used by all programs of all modules
    Syntax      :
    Description :
    Author(s)   : MYT
    Created     : 8/17/16
    Notes       : note: browsers and sdos have their own dedicated comVar includes
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{pt/probase1.i}
{pt/probase3.i}
{pt/shared.i}
{rules/genfunct.i}

DEF TEMP-TABLE tt-editor no-undo
    FIELD tt-line-no AS INT
    FIELD tt-text AS CHAR FORMAT 'x(80)'
    INDEX tt-editor-id IS PRIMARY UNIQUE 
          tt-line-no.

DEF BUFFER wznumgen FOR z_numgen.
DEF BUFFER wentbuf FOR entity.

DEF VAR add-new AS LOG NO-UNDO.
DEF VAR call-pgm AS CHAR NO-UNDO.
DEF VAR cFunctType AS CHAR NO-UNDO.
DEF VAR copyrowid AS ROWID NO-UNDO.
DEF VAR do-copy AS LOG.
DEF VAR entered-wkey AS CHAR NO-UNDO.
DEF VAR first-time AS LOG INIT YES.
DEF VAR h-help AS HANDLE NO-UNDO.
DEF VAR h-helps AS HANDLE EXTENT 100 NO-UNDO.
DEF VAR h-rowobject AS HANDLE.
DEF VAR h-save AS HANDLE NO-UNDO.
DEF VAR handleToBrowse AS HANDLE NO-UNDO.
DEF VAR i AS INT.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR ipvResolver AS CHAR NO-UNDO.
DEF VAR kase AS INT NO-UNDO.
DEF VAR l-begin AS LOG INIT YES.
DEF VAR l-ok AS LOG NO-UNDO.
DEF VAR lAddNew AS LOG NO-UNDO.
DEF VAR last-selection AS CHAR NO-UNDO.
DEF VAR lBegin AS LOG INIT YES.
DEF VAR lError AS LOG NO-UNDO.
DEF VAR lInquiry AS LOG NO-UNDO.
DEF VAR lModified AS LOG.
DEF VAR log1# AS LOG NO-UNDO.
DEF VAR lsave AS LOG.
DEF VAR lValidate AS LOG NO-UNDO.
DEF VAR origValue as CHAR NO-UNDO.
DEF VAR pgm AS CHAR FORMAT "x(13)".
DEF VAR r-rowid AS ROWID NO-UNDO.
DEF VAR recordList AS CHAR NO-UNDO.
DEF VAR result-value# AS CHAR NO-UNDO.
DEF VAR savefunctionname AS CHAR NO-UNDO.
DEF VAR sort-choice AS CHAR INIT "somm-code".
DEF VAR sortchg AS LOG.
DEF VAR tmpFunctName AS CHAR NO-UNDO.
DEF VAR trashLogical AS LOGICAL NO-UNDO.
DEF VAR v-char AS CHAR NO-UNDO.
DEF VAR v-dest AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR v-update AS LOG INIT NO.
DEF VAR wentered AS LOG.
DEF VAR wlock AS LOG.
DEF VAR z_global-rowid AS ROWID.
DEF VAR old-Rowid AS ROWID NO-UNDO.
DEF VAR cAudAction AS CHAR NO-UNDO.
DEF VAR cAudFileName AS CHAR NO-UNDO.
DEF VAR cAudRecImg AS CHAR NO-UNDO.
DEF VAR cAudTempFile AS CHAR NO-UNDO.
DEF VAR deAudSeq AS DECIMAL NO-UNDO.
DEF VAR rAudRowid AS ROWID NO-UNDO.
DEF VAR hAudBuffer AS HANDLE.
DEF VAR hAudQuery AS HANDLE.

IF NOT VALID-HANDLE(hGenRules) THEN
    RUN rules/GenRule.p PERSISTENT SET hGenRules.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


