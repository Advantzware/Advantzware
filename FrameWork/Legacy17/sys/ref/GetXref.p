&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sys/ref/GetXRef.p
    Purpose     : Retrieve a cross reference value for a given table

    Syntax      : GetXref.p (cocode,"Customer","ZOV",output cValue)

    Description :

    Author(s)   : BV
    Created     : 09/21/2015
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcType AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcLookup AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opcValue AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEFINE BUFFER xref-table FOR reftable.

FIND FIRST xref-table
    WHERE xref-table.company EQ ipcCompany
      AND xref-table.reftable EQ 'Xref'
      AND xref-table.CODE EQ ipcLookup
      AND xref-table.loc EQ ipcType
    NO-LOCK NO-ERROR.
IF AVAIL xref-table THEN
    opcValue = xref-table.code2.
ELSE
    opcValue = ''.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


