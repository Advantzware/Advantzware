&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : ap/ap-ckassi.p
    Purpose     : Generic utility to print AP checks.

    Syntax      :

    Description : Format: STUB/CHECK/STUB

    Author(s)   : Stacey Brooks
    Created     : Nov 2011
    Notes       : To maintain proper line counting control:
                  Always use function incrementLineCount(1) after a PUT statement.
                  Always use function skipLines(n) to skip blank lines.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{sys/inc/var.i shared}

{ap/ap-chk.i}

/* Variables defined by the user or programmer. */
DEF VAR viLn-Section-1  AS INT NO-UNDO INIT 2.  /* Line to start printing top section details. */
DEF VAR viLn-Check      AS INT NO-UNDO INIT 22. /* Line to start printing the check. */
DEF VAR viLn-Void       AS INT NO-UNDO INIT 27. /* Line to start printing VOID. */
DEF VAR viLn-Section-2  AS INT NO-UNDO INIT 40. /* Line to start printing bottom section details. */
DEF VAR viLn-Detail-Max AS INT NO-UNDO INIT 9.  /* Max number of detail lines in a section. */
DEF VAR viPageLines     AS INT NO-UNDO INIT 66. /* Number of available lines on the page. */

/* Private variables controlled by the program (not set by the user) */
DEF VAR viCurrentSection     AS INT NO-UNDO INIT 1.  /* 1 or 2 (private) */
DEF VAR viCurrentLine        AS INT NO-UNDO INIT 1.  /* Current line (private). */
DEF VAR viCurrentPage        AS INT NO-UNDO INIT 1.  /* Current page (private) */
DEF VAR viNumDetailsPrinted  AS INT NO-UNDO INIT 0.  /* Num detail lines printed (private). */
DEF VAR viNumChecks          AS INT NO-UNDO INIT 0.  /* Num checks processed (private)*/
DEF VAR vPrintLineNumbers    AS LOG NO-UNDO INIT NO. /* Indicates to print line numbers (for testing)*/

DEFINE VARIABLE vcAdd1 AS CHAR FORMAT "x(60)" NO-UNDO INIT "".
DEFINE VARIABLE vcAdd2 AS CHAR FORMAT "x(60)" NO-UNDO INIT "".
DEFINE VARIABLE vcAdd3 AS CHAR FORMAT "x(60)" NO-UNDO INIT "".

/* Temp-table for processing check invoice details. */
DEF TEMP-TABLE wrk-chk
  field inv-no      like ap-sel.inv-no
  field po-no       like ap-inv.po-no
  field inv-date    like ap-inv.inv-date
  field inv-amt     like ap-inv.net
  field amt-paid    like ap-sel.amt-paid
  field disc-amt    like ap-sel.disc-amt
  field line-amt    as   dec format "->>>,>>9.99".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-goto-Line) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD goto-Line Procedure 
FUNCTION goto-Line RETURNS LOGICAL
  ( INPUT piLineNum AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-goto-section) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD goto-section Procedure 
FUNCTION goto-section RETURNS LOGICAL
  ( INPUT viSection AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-incrementLineCount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD incrementLineCount Procedure 
FUNCTION incrementLineCount RETURNS LOGICAL
  ( INPUT piLines AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LineNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LineNum Procedure 
FUNCTION LineNum RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-maxDetailsPrinted) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD maxDetailsPrinted Procedure 
FUNCTION maxDetailsPrinted RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-next-line) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD next-line Procedure 
FUNCTION next-line RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-next-page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD next-page Procedure 
FUNCTION next-page RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-skipLines) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD skipLines Procedure 
FUNCTION skipLines RETURNS LOGICAL
  ( INPUT piLines AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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
         HEIGHT             = 17.52
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

  /* If align mode, then exit. */
  IF v-print-mode = "ALIGN" THEN RETURN.         /* production mode */


   /* Process each "A/P Checks" for selected range of vendors. */
  check-block:
   FOR EACH ap-chk
      WHERE ap-chk.company   EQ cocode
        AND ap-chk.vend-no   GE wvend-no
        AND ap-chk.vend-no   LE evend-no
        AND ap-chk.man-check EQ NO
        AND CAN-FIND(FIRST ap-sel
                     WHERE ap-sel.company   EQ cocode
                       AND ap-sel.vend-no   EQ ap-chk.vend-no
                       AND ap-sel.man-check EQ NO),
      /* Sort by vendor name and number. */
      FIRST vend WHERE vend.company eq cocode AND 
                       vend.vend-no eq ap-chk.vend-no
       BREAK BY (IF v-sort-name THEN vend.name ELSE "")
             BY ap-chk.vend-no:

       /* Get vendor remit address. */
       RUN Get-Remit-Address.

       /* Assign ap-chk fields. */
       ASSIGN
        ap-chk.check-date = wdate
        ap-chk.bank-code  = x-bank
        v-vend-no         = vend.vend-no
        v-vend-name       = vend.name.



       /* Process "List of invoices selected for payment". */
       invoice-block:
       FOR EACH ap-sel WHERE 
           ap-sel.company   EQ cocode AND 
           ap-sel.vend-no   EQ ap-chk.vend-no AND 
           ap-sel.man-check EQ NO BREAK BY ap-sel.inv-no:  
    
         /* Get a/p invoices header */
         FIND FIRST ap-inv WHERE 
             ap-inv.company EQ cocode AND 
             ap-inv.vend-no EQ ap-sel.vend-no AND 
             ap-inv.inv-no  EQ ap-sel.inv-no USE-INDEX inv-no.
    
         /* Assign invoice pay date. */
         ASSIGN ap-inv.pay-date = wdate.

          
         /* Accumulate totals. */
         ASSIGN
          ctot              = ctot + ap-sel.amt-paid
          cdis              = cdis + ap-sel.disc-amt
          ap-sel.check-date = wdate
          cgrossl           = cgrossl + ap-sel.amt-paid /*+ ap-sel.disc-amt) */
          cgross            = cgross + cgrossl.
    
         /* Create temp-table records for each selected invoice to be paid. */
         CREATE wrk-chk.
         ASSIGN
          wrk-chk.inv-no   = ap-sel.inv-no
          wrk-chk.po-no    = ap-inv.po-no
          wrk-chk.inv-date = ap-inv.inv-date
          wrk-chk.inv-amt  = (IF ap-inv.gross gt 0 then ap-inv.gross else ap-inv.net)
          wrk-chk.amt-paid = ap-sel.amt-paid
          wrk-chk.disc-amt = ap-sel.disc-amt
          wrk-chk.line-amt = cgrossl.

         IF FIRST(ap-sel.inv-no) THEN
             /* Go to the first detail section. */
             goto-section(1).


         /* Print a detail line. */ 
         IF NOT maxDetailsPrinted() THEN
             RUN Print-Detail.

         /* If max detail lines printed OR we finished printing them all... */
         IF maxDetailsPrinted() OR LAST(ap-sel.inv-no) THEN DO:

             /* If last detail line, print totals */
             IF LAST(ap-sel.inv-no) THEN
                 RUN Print-Totals.

             /* If section 1, then print check or VOID and process bottom section. */
             IF viCurrentSection = 1  THEN DO:

                 /* If all detail lines are printed, then print check. */
                 IF LAST(ap-sel.inv-no) THEN DO:
                      RUN Print-Check.
                      ASSIGN ap-chk.check-no = stnum.
                      ASSIGN stnum = stnum + 1.
                      /* Accumulate number of checks processed. */
                      ASSIGN viNumChecks = viNumChecks + 1.
                 END.
                    
                 /* Else if there are more to print, then print VOID. */
                 ELSE
                     RUN Print-Void.

                 /* Go to the second detail section. */
                 goto-section(2).

                 /* Reprint the detail lines again (deletes them). */
                 RUN Reprint-Details.

                 /* If last detail line, print totals */
                 IF LAST(ap-sel.inv-no) THEN
                     RUN Print-Totals.

                 /* Go to next page (if more to print). */
                 IF NOT LAST(ap-sel.inv-no) THEN DO:
                     next-page().
                     goto-section(1).
                 END.
                     

             END. /* If section 1 */

         END. /*  IF maxDetailsPrinted() OR LAST(ap-sel.inv-no) */

       END. /* for each ap-sel (invoice-block) */
   END. /* for each ap-chk  (check block) */

   /* If no checks were processed, message the user. */
   IF viNumChecks = 0 THEN
       PUT "No checks found ready to print!".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Get-Remit-Address) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Get-Remit-Address Procedure 
PROCEDURE Get-Remit-Address :
/*------------------------------------------------------------------------------
  Purpose:     Get the vendor remit address.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   ASSIGN vcAdd1 = ""
          vcAdd2 = ""
          vcAdd3 = "".

   IF vend.r-add1 EQ " " THEN DO:   /*if no remit-to address*/
     IF LENGTH(vend.r-zip) GT 5 THEN
       ASSIGN csz = vend.city + ", " + vend.state + " " +
                    SUBSTR(vend.zip,1,5) + "-" + SUBSTR(vend.zip,6,4).
     ELSE
       ASSIGN csz = vend.city + ", " + vend.state + " " + vend.zip.

     ASSIGN vcAdd1 = vend.add1
            vcAdd2 = vend.add2
            vcAdd3 = csz.
   END.

   ELSE DO: /*if a remit-to address exists  GEH */
     IF LENGTH(vend.r-zip) GT 5 THEN
       ASSIGN csz = vend.r-city + ", " + vend.r-state + " " +
             SUBSTR(vend.r-zip,1,5) + "-" + SUBSTR(vend.r-zip,6,4).
     ELSE
         ASSIGN csz = vend.r-city + ", " + vend.r-state + " " + vend.r-zip.

     ASSIGN
      vcAdd1 = vend.r-add1
      vcAdd2 = vend.r-add2
      vcAdd3 = csz.
   END.

   /* Eliminate blank address lines. */
   IF vcAdd1 = "" THEN
       ASSIGN vcAdd1 = vcAdd2
              vcAdd2 = csz
              vcAdd3 = "".

   RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Print-Check) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Print-Check Procedure 
PROCEDURE Print-Check :
/*------------------------------------------------------------------------------
  Purpose:     Print the check
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Convert dollar amt to text. */
    RUN ap/apchks.p (input ctot, input 70, output dol).

    ASSIGN dol = trim(dol) + fill("*",70) .  

    /* Go to check printing line. */
    Goto-Line(viLn-Check).


    /* Go to date line. */
    Goto-Line(22).

    /* Print check date. */
    PUT LineNum() FORMAT "99" AT 1 
        ap-chk.check-date AT 65.
    /* Increment line count */
    incrementLineCount(1).

   
    /* Go to line. */
    Goto-Line(25).
    /* Print check amount. */
    PUT LineNum() FORMAT "99" AT 1 
        ctot              AT 65.
    /* Increment line count */
    incrementLineCount(1).


    /* Go to line. */
    Goto-Line(27).
    /* Print text dollar amount. */
    PUT LineNum() FORMAT "99" AT 1
        CAPS(dol) FORMAT "x(60)" AT 10.
    /* Increment line count */
    incrementLineCount(1).

    /* Go to line. */
    Goto-Line(30).
    /* Print vendor remit info and increment line count. */
    PUT LineNum() FORMAT "99" AT 1 
        CAPS(vend.remit)  FORMAT "x(60)" AT 10.
    /* Increment line count */
    incrementLineCount(1).

    PUT LineNum() FORMAT "99" AT 1 
        CAPS(vcAdd1)  FORMAT "x(60)"  AT 10.
    /* Increment line count */
    incrementLineCount(1).

    PUT LineNum() FORMAT "99" AT 1 
        CAPS(vcAdd2) FORMAT "x(60)"   AT 10.
    /* Increment line count */
    incrementLineCount(1).

    PUT LineNum() FORMAT "99" AT 1 
        CAPS(vcAdd3)  FORMAT "x(60)"   AT 10.
    /* Increment line count */
    incrementLineCount(1).

    /* Go to line. */
    Goto-Line(35).
    /* Print memo line. */
    PUT LineNum() FORMAT "99" AT 1 
        "Memo:" AT 4
        vend.check-memo FORMAT "x(60)" AT 11.
    /* Increment line count */
    incrementLineCount(1).


  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Print-Detail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Print-Detail Procedure 
PROCEDURE Print-Detail :
/*------------------------------------------------------------------------------
  Purpose:     Print a detail line.
  Parameters:  <none>
  Notes:       Uncomment line number when testing line numbers.
               Even when not testing, it's throwing the lines off.
------------------------------------------------------------------------------*/

     /* Print a detail line. */
     PUT /*LineNum() FORMAT "99" AT 1 */
         wrk-chk.inv-no                TO 12 FORMAT "x(12)"
         trim(string(wrk-chk.po-no,">>>>>>")) to 25 format "x(12)"
         wrk-chk.inv-date              TO 34 FORMAT "99/99/99"
         wrk-chk.inv-amt               TO 45 FORMAT "->>>,>>9.99"
         wrk-chk.amt-paid              TO 56 FORMAT "->>>,>>9.99"
         wrk-chk.disc-amt              TO 67 FORMAT "->>>,>>9.99"
         wrk-chk.line-amt              TO 79 FORMAT "->>>>,>>9.99".

     /* Update the current line and num detail lines printed. */
     ASSIGN viNumDetailsPrinted  = (viNumDetailsPrinted + 1).

     /* Increment line count */
     incrementLineCount(1).

     /* Clear gross line amt. */
     ASSIGN  cgrossl = 0.


     RETURN.


/* Test Data */
    /*         "123456789012"     TO 15 format "x(12)"             */
    /*             TODAY              TO 30 format "99/99/99"      */
    /*             -153789.99         TO 43 format "->>>,>>9.99"   */
    /*             -153789.99         TO 55 FORMAT "->>>,>>9.99"   */
    /*             -153789.99         TO 67 format "->>>,>>9.99"   */
    /*             -153789.99         TO 79 format "->>>,>>9.99".  */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Print-Totals) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Print-Totals Procedure 
PROCEDURE Print-Totals :
/*------------------------------------------------------------------------------
  Purpose:     Print detail totals.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  PUT /*LineNum() FORMAT "99" AT 1*/
      "-----------"      to 79.

  /* Increment line count. */
  incrementLineCount(1).

  PUT LineNum() FORMAT "99" AT 1
      "Check Date: " AT 5 
      ap-chk.check-date
      "Net Check Amount" to 67
      ctot               to 79.


  /* Zero out check total when section 2 (last stub). */
  IF viCurrentSection = 2 THEN
      ASSIGN ctot   = 0.

  /* Increment line count. */
  incrementLineCount(1).

  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Print-Vendor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Print-Vendor Procedure 
PROCEDURE Print-Vendor :
/*------------------------------------------------------------------------------
  Purpose:     Print the vendor info line.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Print Vendor Info. */
  PUT LineNum() FORMAT "99" AT 1 
      "Vendor ID: " v-vend-no 
      SPACE(8)
      "Vendor Name: "
      v-vend-name
      SKIP(1).

      PUT "Invoice No."     at 2
          "Reference"       at 16
          "Date"            at 29
          "Inv Amt"         at 37
          "Amt Paid"        at 48
          "Disc Taken"      at 58
          "Net Amt"         at 71.

      PUT "============"    at 1
          "============"    at 14
          "========"        at 27
          "=========="      at 36
          "=========="      at 47
          "=========="      at 58
          "==========="     at 69.

  incrementLineCount(4).


  RETURN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Print-Void) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Print-Void Procedure 
PROCEDURE Print-Void :
/*------------------------------------------------------------------------------
  Purpose:     Print the VOID check.
  Parameters:  <none>
  Notes:       Increment line counter after each line.
------------------------------------------------------------------------------*/

  /* Go to void line and print VOID. */
  Goto-Line(viLn-Void).
  
  PUT LineNum() FORMAT "99" AT 1 "V   V      OOO       III      DDDD"   at 25. 
  /* Increment line count. */
  incrementLineCount(1).
  PUT LineNum() FORMAT "99" AT 1 "V   V     O   O       I       D   D"  at 25. 
  /* Increment line count. */
  incrementLineCount(1).
  PUT LineNum() FORMAT "99" AT 1 "V   V     O   O       I       D   D"  at 25. 
  /* Increment line count. */
  incrementLineCount(1).
  PUT LineNum() FORMAT "99" AT 1 " V V      O   O       I       D   D"  at 25.
  /* Increment line count. */
  incrementLineCount(1).
  PUT LineNum() FORMAT "99" AT 1 " V V      O   O       I       D   D"  at 25. 
  /* Increment line count. */
  incrementLineCount(1).
  PUT LineNum() FORMAT "99" AT 1 "  V        OOO       III      DDDD "  at 25.
  /* Increment line count. */
  incrementLineCount(1).

  ASSIGN stnum = stnum + 1.


  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Reprint-Details) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reprint-Details Procedure 
PROCEDURE Reprint-Details :
/*------------------------------------------------------------------------------
  Purpose:     Reprint the detail lines after the check has been printed.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH wrk-chk BY wrk-chk.inv-no:

      /* If max details not reached, then. */ 
      IF NOT maxDetailsPrinted() THEN DO:
          /* Print the detail line. */
          RUN Print-Detail.
          /* Delete this work record. */
          DELETE wrk-chk.
      END.
      
      /* If max details reached, then exit. */
      IF maxDetailsPrinted() THEN RETURN.

  END.


  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-goto-Line) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION goto-Line Procedure 
FUNCTION goto-Line RETURNS LOGICAL
  ( INPUT piLineNum AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Go to a requested line on the page.
    Notes:  current line = 1, piLineNum = 3 (skip 2)
------------------------------------------------------------------------------*/
  DEF VAR viSkip AS INT NO-UNDO INIT 0.


  IF piLineNum <= 0 THEN RETURN FALSE.


  /* If 'goto line' is less than the current line, then page first. */
  IF piLineNum < viCurrentLine THEN 
      next-page().


  /* Calculate how many lines to skip. */
  ASSIGN viSkip = (piLineNum - viCurrentLine). /* 2 */


  /* If number of lines to skip is greater than page size, then page first. */
  IF (viCurrentLine + viSkip) >= viPageLines THEN next-page().

  IF viSkip > 0 THEN DO:
       PUT   SKIP(viSkip).
      ASSIGN viCurrentLine = (viCurrentLine + viSkip).
  END.

      
  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-goto-section) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION goto-section Procedure 
FUNCTION goto-section RETURNS LOGICAL
  ( INPUT viSection AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Go to the requested section.
    Notes:  
------------------------------------------------------------------------------*/

  /* If section 1 requested, go there. */
  IF viSection = 1 THEN
      Goto-Line(viLn-Section-1).
  /* Else go to section 2. */
  ELSE
      Goto-Line(viLn-Section-2).

  /* Reset num detail lines printed and current section. */
  ASSIGN viNumDetailsPrinted = 0
         viCurrentSection = viSection.

  /* Print Vendor info. */
  RUN Print-Vendor.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-incrementLineCount) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION incrementLineCount Procedure 
FUNCTION incrementLineCount RETURNS LOGICAL
  ( INPUT piLines AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Increment line counter by number of lines printed.
    Notes:  
------------------------------------------------------------------------------*/

  ASSIGN viCurrentLine = viCurrentLine + piLines.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LineNum) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LineNum Procedure 
FUNCTION LineNum RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Return the current line number.
    Notes:  
------------------------------------------------------------------------------*/

  IF vPrintLineNumbers = YES THEN
      RETURN STRING(viCurrentLine,"99").
  ELSE
      RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-maxDetailsPrinted) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION maxDetailsPrinted Procedure 
FUNCTION maxDetailsPrinted RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Determine if the maximum number of detail lines have been printed.
    Notes:  If section is full, then go to the next section.
------------------------------------------------------------------------------*/

  IF viNumDetailsPrinted >= viLn-Detail-Max THEN
      RETURN TRUE.
  ELSE
      RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-next-line) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION next-line Procedure 
FUNCTION next-line RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Skip to the next line.
    Notes:  
------------------------------------------------------------------------------*/

  /* If number of lines is greater than page size, then page first. */
  IF (viCurrentLine + 1) >= viPageLines THEN next-page().

  /* Skip to next line. */
  PUT SKIP(1).

  /* Increment current line. */
  ASSIGN viCurrentLine = viCurrentLine + 1.


  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-next-page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION next-page Procedure 
FUNCTION next-page RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Go to the next page.
    Notes:  See notes below on issue with line position on secondary pages.
------------------------------------------------------------------------------*/

  /* Go to the next page. */
  PUT CONTROL CHR(12).  

  /* Increment page number. */
  ASSIGN viCurrentPage = viCurrentPage + 1.

  /* Reset current line based on page number. For some reason,
     the 'next page' control is forcing it to line 2 instead of line 1. 
     So this logic is rigged to set the current line to line 2 if after
     the first page. */
  IF viCurrentPage > 1 THEN
      ASSIGN viCurrentLine = 2.
  ELSE
      ASSIGN viCurrentLine = 1.

  /* Go to the first detail section. */
/*   goto-section(1).  */


  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-skipLines) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION skipLines Procedure 
FUNCTION skipLines RETURNS LOGICAL
  ( INPUT piLines AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  Skip a requested number of lines.
    Notes:  current line = 1, piLines = 3, goto line = 4
------------------------------------------------------------------------------*/

  /* If requested lines to skip is zero or less, then ignore. */
  IF piLines <= 0 THEN RETURN FALSE.

  /* If number of skip lines goes over the page size, then page first. */
  IF (viCurrentLine + piLines) >= viPageLines THEN
      next-page().
  
  /* Skip requested lines. */
  PUT SKIP(piLines).

  /* Increment current line number. */
  ASSIGN viCurrentLine = (viCurrentLine + piLines).

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

