/* custom/prntbat.p   Print procedure in Batch */
 DEF INPUT PARAM ip-filename AS cha NO-UNDO /* print file name */.
 DEF INPUT PARAM ip-font AS INT NO-UNDO.
 DEF INPUT PARAM ip-orientation AS cha NO-UNDO.
 DEF INPUT PARAM ip-copies AS INT NO-UNDO.
 DEF INPUT PARAM ip-prtname AS cha NO-UNDO.

 DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
 DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
 DEFINE VARIABLE result AS LOGICAL NO-UNDO.
 DEF VAR lv-ornt AS INT NO-UNDO.
 DEF VAR i AS INT NO-UNDO.
 DEF VAR v-copies AS INT NO-UNDO.
 {CUSTOM/Xprint.i}

 IF ip-orientation BEGINS "L" THEN lv-ornt = 2.
 ELSE lv-ornt = 0.  /* No printer setup dialog window */

 /* Use Progress Print. Always use Font#9 in Registry (set above) */

/* goes to  a default printer */

/* SESSION:PRINTER-NAME = ip-prtname. NOT WORKDING 
 RUN 'adecomm/_osprint.p' (INPUT ?, INPUT ip-filename,
                           INPUT ip-font, INPUT lv-ornt, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */
*/                           
 FILE-INFO:FILE-NAME = ip-filename.
 RUN printfile (FILE-INFO:FILE-NAME).


/*  for print specific
 v-copies = ip-copies.
 IF v-copies = 0 THEN v-copies = 1.

 IF ip-prtname BEGINS "\\" THEN DO i = 1 TO v-copies:
     OS-COPY VALUE(ip-filename) VALUE(ip-prtname).      
 END.
 ELSE IF ip-prtname BEGINS "usb" THEN DO:
      /*
      def VAR lv-input AS cha FORM "x(80)" NO-UNDO.
      OUTPUT TO PRINTER VALUE(ip-prtname) .
      INPUT FROM VALUE(ip-print-file) NO-ECHO.
      REPEAT:
          IMPORT UNFORMATTED lv-input.
          PUT lv-input SKIP. /* with unformatted - make worse */
      END.
      input close.
      output close.
      */
      /*
      def VAR lv-input AS cha FORM "x(80)" NO-UNDO.
      OUTPUT TO VALUE(ip-prtname).
      INPUT FROM VALUE(ip-print-file) NO-ECHO.
      REPEAT:
          IMPORT UNFORMATTED lv-input.
          PUT lv-input SKIP. /* with unformatted - make worse */
      END.
      INPUT CLOSE.
      output close.
        */
  END.
  ELSE DO i = 1 TO v-copies:
      /*  OS-COMMAND VALUE(lv-prt-cmd). */   
      
      /*OS-COPY VALUE(ip-print-file) VALUE(lv-prt-port). 
              not working local printer on window 98/me */
      OS-COMMAND SILENT VALUE("copy " + ip-filename + " " + ip-prtname).
   END.
===========*/
