/* custom/prntproc.p   Print procedure */
 DEF INPUT PARAM ip-prtname AS cha NO-UNDO.
 DEF INPUT PARAM ip-font AS INT NO-UNDO.
 DEF INPUT PARAM ip-orientation AS cha NO-UNDO.

 DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
 DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
 DEFINE VARIABLE result AS LOGICAL NO-UNDO.
 DEF VAR lv-ornt AS INT NO-UNDO.
  
 IF ip-orientation BEGINS "L" THEN lv-ornt = 3.
 ELSE lv-ornt = 1.

 /* Use Progress Print. Always use Font#9 in Registry (set above) */
 RUN 'adecomm/_osprint.p' (INPUT ?, INPUT ip-prtname,
                           INPUT ip-font, INPUT lv-ornt, INPUT 0, INPUT 0, OUTPUT result).
                                    /* use-dialog(1) and landscape(2) */

 /*     SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
     IF NOT printok THEN
     RETURN NO-APPLY.
*/
