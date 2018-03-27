&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : GetItemfgActInact.p
    Purpose     : Centralize the query for determining if a given i-no is 
                active or inactive.  Also helps in conversion from reftable to 
                itemfg.stat field

    Syntax      :

    Description :

    Author(s)   : BV
    Created     : 2/3/2014
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcINo AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplActive AS LOGICAL NO-UNDO.

DEFINE BUFFER bf-reftable FOR reftable.
DEFINE BUFFER bf-itemfg FOR itemfg.

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


/* FIND FIRST bf-reftable                       */
/*     WHERE bf-reftable.reftable EQ "FGSTATUS" */
/*       AND bf-reftable.company EQ ipcCompany  */
/*       AND bf-reftable.loc EQ ""              */
/*       AND bf-reftable.CODE EQ ipcINo         */
/*     NO-LOCK NO-ERROR.                        */
/* IF AVAIL bf-reftable THEN                    */
/*     oplActive = bf-reftable.code2 NE "I".    */

oplActive = NOT CAN-FIND(FIRST bf-itemfg 
    WHERE bf-itemfg.company EQ ipcCompany
      AND bf-itemfg.stat EQ "I"
      AND bf-itemfg.i-no EQ ipcINo).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


