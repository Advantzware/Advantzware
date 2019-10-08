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
{fa/comVarsGlob.i {1}}

DEF BUFFER entity# FOR entity.
DEF BUFFER fa-mast# FOR fa-mast.
DEF BUFFER bfa-mast-rep FOR fa-mast-rep.

DEF {1} SHARED VAR asset-code# AS CHAR NO-UNDO.
DEF {1} SHARED VAR fa-entity# AS CHAR NO-UNDO. 
DEF {1} SHARED VAR tag-no# AS INT NO-UNDO.
DEF {1} SHARED VAR fa-mast-rowid AS ROWID NO-UNDO.

DEF VAR origAsset-Code AS CHAR NO-UNDO.
DEF VAR old-status AS CHAR NO-UNDO.
DEF VAR nb-tags AS INT NO-UNDO.
DEF VAR m AS INT NO-UNDO.
DEF VAR loop1 AS INT NO-UNDO.
DEF VAR basis# AS INT NO-UNDO.
DEF VAR iTest AS INT NO-UNDO.

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


