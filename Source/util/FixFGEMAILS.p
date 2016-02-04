&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : util/FixFGEMAILS.p
    Purpose     : N-K FGEMAIL (no "S") was erroneously created.  The integer
    value of this sys-ctrl should be converted to FGEMAILS (with "S") integer 
    value and the N-K FGEMAIL record should be deleted.

    Syntax      :

    Description :

    Author(s)   :  BV
    Created     :  10/21/2013
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE BUFFER gbf-sys-ctrl-FGEMAILS FOR sys-ctrl.
DEFINE BUFFER gbf-sys-ctrl-FGEMAIL FOR sys-ctrl.
DEFINE BUFFER gbf-sys-ctrl-shipto-FGEMAIL FOR sys-ctrl-shipto.

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
/*move int-val over to FGEMAILS and delete*/
FOR EACH gbf-sys-ctrl-FGEMAILS 
    WHERE gbf-sys-ctrl-FGEMAILS.NAME EQ "FGEMAILS" EXCLUSIVE-LOCK,
    FIRST gbf-sys-ctrl-FGEMAIL 
        WHERE gbf-sys-ctrl-FGEMAIL.company EQ gbf-sys-ctrl-FGEMAILS.company
          AND gbf-sys-ctrl-FGEMAIL.NAME EQ "FGEMAIL" EXCLUSIVE-LOCK:
    
    gbf-sys-ctrl-FGEMAILS.int-fld = gbf-sys-ctrl-FGEMAIL.int-fld.
    DELETE gbf-sys-ctrl-FGEMAIL.
    
END.

/*clean up any other FGEMAIL records that exist*/
FOR EACH gbf-sys-ctrl-FGEMAIL
    WHERE gbf-sys-ctrl-FGEMAIL.NAME EQ "FGEMAIL" EXCLUSIVE-LOCK:

    DELETE gbf-sys-ctrl-FGEMAIL.
END.
FOR EACH gbf-sys-ctrl-shipto-FGEMAIL
    WHERE gbf-sys-ctrl-shipto-FGEMAIL.NAME EQ "FGEMAIL" EXCLUSIVE-LOCK:

    DELETE gbf-sys-ctrl-shipto-FGEMAIL.
END.


MESSAGE "FixFGEMAILS Process Complete."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


