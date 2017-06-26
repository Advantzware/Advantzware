&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF SHARED VAR do-audit AS LOG NO-UNDO.
DEF SHARED VAR from-Frame AS LOG.
DEF SHARED VAR lrecno AS ROWID NO-UNDO.
DEF SHARED VAR llrecno AS ROWID NO-UNDO .
DEF SHARED VAR llrecno1 AS ROWID NO-UNDO .
DEF SHARED VAR ochoice AS CHAR NO-UNDO.
DEF SHARED VAR recno AS ROWID NO-UNDO .

DEF VAR all-ok AS LOG NO-UNDO.
DEF VAR auto-appl AS LOG NO-UNDO.
DEF VAR cAction AS CHAR NO-UNDO.
DEF VAR cString AS CHAR NO-UNDO.
DEF VAR done AS INT NO-UNDO.
DEF VAR eresponse AS LOG NO-UNDO.
DEF VAR first-time AS LOG INIT YES.
DEF VAR handleToBrowse AS HANDLE NO-UNDO.
DEF VAR h-help AS HANDLE NO-UNDO.
DEF VAR h-helps AS HANDLE EXTENT 100 NO-UNDO.
DEF VAR hBrowseHdl AS HANDLE NO-UNDO.
DEF VAR h_columns AS HANDLE NO-UNDO EXTENT 40.
DEF VAR h_sdb AS HANDLE NO-UNDO.
DEF VAR hcol AS HANDLE NO-UNDO.
DEF VAR hColumn1 AS HANDLE NO-UNDO.
DEF VAR hColumn2 AS HANDLE NO-UNDO.
DEF VAR hColumn3 AS HANDLE NO-UNDO.
DEF VAR hColumn4 AS HANDLE NO-UNDO.
DEF VAR hColumn5 AS HANDLE NO-UNDO.
DEF VAR hColumn6 AS HANDLE NO-UNDO.
DEF VAR hColumn7 AS HANDLE NO-UNDO.
DEF VAR hColumn8 AS HANDLE NO-UNDO.
DEF VAR hColumn9 AS HANDLE NO-UNDO.
DEF VAR hMyParent AS HANDLE NO-UNDO.
DEF VAR i AS INT.
DEF VAR ii AS INT NO-UNDO.
DEF VAR last-selection AS CHAR NO-UNDO.
DEF VAR lCont AS LOG NO-UNDO.
DEF VAR old-Rowid AS ROWID NO-UNDO.
DEF VAR proceed3 AS LOG NO-UNDO.
DEF VAR r-rowid AS ROWID NO-UNDO.
DEF VAR recordlist AS CHAR.
DEF VAR response AS LOG NO-UNDO.
DEF VAR result-value# AS CHAR NO-UNDO.
DEF VAR savefunctionname AS CHAR.
DEF VAR sortchg AS LOG.
DEF VAR tmpFunctName AS CHAR NO-UNDO.
DEF VAR v-ok AS LOG.

{pt/getControlRecord.i "{&cControl}"}

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


