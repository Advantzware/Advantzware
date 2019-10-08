&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : ar/calctax2.p
    Purpose     : Calculate tax

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       : Copied from calctax.p and restructured to accommodate 
                  new tax calculation logic.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER v-tax-code     LIKE stax.tax-gr NO-UNDO.
DEF INPUT  PARAMETER v-frt          AS LOG NO-UNDO.
DEF INPUT  PARAMETER v-price-in     AS DEC NO-UNDO. /* item total price */
DEF INPUT  PARAMETER v-company      LIKE inv-head.company NO-UNDO.
DEF INPUT  PARAMETER v-i-no         LIKE itemfg.i-no NO-UNDO.
DEF OUTPUT PARAMETER v-tax-amt-out  AS DEC NO-UNDO INIT 0. /* total tax amount */

DEF VAR v-tax-amt AS DEC NO-UNDO INIT 0. /* Tax amount */

DEFINE BUFFER buf-itemfg FOR itemfg.


{sys/inc/var.i shared}

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


    ASSIGN v-tax-amt-out = 0.
    
    FIND FIRST stax WHERE stax.company = cocode AND
                          stax.tax-group = v-tax-code NO-LOCK NO-ERROR.
    
    
    IF NOT AVAIL stax THEN RETURN.


  FIND FIRST buf-itemfg WHERE 
             buf-itemfg.company = v-company AND 
             buf-itemfg.i-no = v-i-no NO-LOCK NO-ERROR.

  /* Determine which tax program to run (standard or varied tax calculation). */
  /* If the itemfg varied tax flag is set... */
  IF AVAIL buf-itemfg AND buf-itemfg.spare-char-2 = "YES" AND
      /* And the dollar limit is setup in tax code 5... */
      stax.tax-code1[5] = "" AND stax.tax-dscr1[5] = "Dollar Limit" AND 
           stax.tax-rate1[5] > 0 AND
      /* and the invoice price exceeds the dollar limit... */
      v-price-in > stax.tax-rate1[5] THEN 
        /* then run the varied tax rate calculation program. */
        RUN varied-tax.
  ELSE
      RUN standard-tax.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-standard-tax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE standard-tax Procedure 
PROCEDURE standard-tax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

              
    DO i = 1 TO EXTENT(stax.tax-rate1):
    
        /* Skip if tax code is blank. */
      IF stax.tax-code1[i] = "" THEN NEXT.
    
      IF NOT v-frt or stax.tax-frt1[i] THEN DO:
        ASSIGN v-tax-amt = ROUND(v-price-in * stax.tax-rate1[i] / 100,2).
    
        /*if stax.company eq "yes" then v-in = v-in + v-amt. */
        IF stax.accum-tax THEN 
            ASSIGN v-price-in  = (v-price-in + v-tax-amt).
    
        /* Accumulate total amount out. */
        ASSIGN v-tax-amt-out = (v-tax-amt-out + v-tax-amt).
      END.
    END.
    
    /* Round the tax amount out. */
    ASSIGN v-tax-amt-out = ROUND(v-tax-amt-out,2).
              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-varied-tax) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE varied-tax Procedure 
PROCEDURE varied-tax :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR v-amt-1 AS DEC NO-UNDO INIT 0. /* Dollar limit amount */
    DEF VAR v-amt-2 AS DEC NO-UNDO INIT 0. /* Exceeded amount */

    DEF VAR v-tax-amt-1 AS DEC NO-UNDO INIT 0. /* Tax amount for dollar limit */
    DEF VAR v-tax-amt-2 AS DEC NO-UNDO INIT 0. /* Tax amount for exceeded. */


    /* Separate the dollar limit amt from the exceeded amount. */
    ASSIGN v-amt-1 = stax.tax-rate1[5] /* Set to dollar limit. */
           v-amt-2 = (v-price-in - stax.tax-rate1[5]). /* exceeded amt */

    /* Dollar limit amt taxed with all taxes (first 4). */
    DO i = 1 TO 4:
      /* Skip if tax code is blank. */
      IF stax.tax-code1[i] = "" THEN NEXT.
      /* If v-frt = no or stax frt1 = yes... */
      IF NOT v-frt OR stax.tax-frt1[i] THEN DO:
        /* Calculate the tax amount. */
        ASSIGN v-tax-amt-1 = ROUND(v-amt-1 * stax.tax-rate1[i] / 100,2).

        /* if stax.accum-tax eq "yes" then add tax amount to item total price. */
        IF stax.accum-tax THEN 
            ASSIGN v-price-in  = (v-amt-1 + v-tax-amt-1).

        /* Accumulate total amount out. */
        ASSIGN v-tax-amt-out = v-tax-amt-out + v-tax-amt-1.
      END.
    END.

    /* Tax the exceeded amount at first rate. */
    ASSIGN v-tax-amt-2 = ROUND(v-amt-2 * stax.tax-rate1[1] / 100,2).


    /* Accumulate total tax amount out. */
    ASSIGN v-tax-amt-out = v-tax-amt-out + v-tax-amt-2.

    /* Round the tax amount out. */
    ASSIGN v-tax-amt-out = ROUND(v-tax-amt-out,2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

