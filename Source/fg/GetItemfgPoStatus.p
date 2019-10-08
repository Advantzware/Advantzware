&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : GetItemfgPoStatus.p
    Purpose     : Centralize the query for determining if a given i-no is 
                active or inactive.  Also helps in conversion from reftable to 
                itemfg.stat field

    Syntax      :

    Description :

    Author(s)   : BV
    Created     : 7/25/2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcINo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcPartNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplCheckManual AS LOGICAL NO-UNDO .
DEFINE OUTPUT PARAMETER oplActive AS LOGICAL NO-UNDO.


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
    FIND FIRST bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ ipcCompany
          AND bf-itemfg.i-no EQ ipcINo
          AND (bf-itemfg.part-no EQ ipcPartNo OR ipcPartNo EQ "") NO-ERROR .
    IF AVAIL bf-itemfg THEN
      oplActive = IF (bf-itemfg.poStatus EQ "Default" OR bf-itemfg.poStatus EQ "") THEN YES ELSE NO .

     IF AVAIL bf-itemfg AND iplCheckManual AND (bf-itemfg.poStatus EQ "Default" OR bf-itemfg.poStatus EQ "NoAuto")  THEN
         oplActive = YES .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


