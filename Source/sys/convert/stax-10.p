&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
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
DEFINE VARIABLE li AS INTEGER NO-UNDO.

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

find first reftable no-lock where reftable.reftable = "v10-TaxCode-Upgrade"
                              and reftable.code     = "10 Extents" no-error.
IF AVAILABLE reftable THEN RETURN "Tax Code Upgrade Already Completed".

DO TRANSACTION:
    FOR EACH stax EXCLUSIVE-LOCK:
        DO li = 1 to extent(stax.tax-code): /* synch original with new */
          Assign stax.tax-code1[li] = stax.tax-code[li]
                 stax.tax-dscr1[li] = stax.tax-dscr[li]
                 stax.tax-rate1[li] = stax.tax-rate[li]
                 stax.tax-acc1[li]  = stax.tax-acc[li]
                 stax.tax-frt1[li]  = stax.tax-frt[li].
        END.
    END.
    CREATE reftable.
    ASSIGN reftable.reftable = "v10-TaxCode-Upgrade"
           reftable.CODE     = "10 Extents"
           reftable.code2    = STRING(NOW) + " " + USERID("nosweat").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


