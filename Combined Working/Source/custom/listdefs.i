/* listdefs.i */

{custom/gcompany.i}
{custom/gloc.i}

{custom/getcmpny.i}
{custom/getloc.i}

/* for addon touch posting program */
&IF '{&PROGNAME}' = 'post_.' or  '{&PROGNAME}' = 'postre_.'&THEN
DEFINE TEMP-TABLE ttbl_pc-prdd NO-UNDO LIKE pc-prdd
       INDEX ttbl_pc-prdd IS PRIMARY
             company m-code op-date shift job frm blank-no.
DEFINE TEMP-TABLE ttbl_pc-prdh NO-UNDO LIKE pc-prdh
       INDEX ttbl_pc-prdh IS PRIMARY
             company m-code trans-date shift.
DEFINE TEMP-TABLE ttbl_rowid NO-UNDO
  FIELD pc-prdd_rowid AS ROWID
  FIELD total_time AS INTEGER.
DEFINE VARIABLE machtotaltime AS DECIMAL NO-UNDO.
DEFINE VARIABLE shiftpct AS DECIMAL NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE waste-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE run-qty AS DECIMAL NO-UNDO.
&elseif '{&PROGNAME}' = 'shrpt1_.' &THEN
 /* define var rd-mat-type as int form ">9" no-undo.
  define var is-real-mat as log no-undo.
  define var is-est-mat as log no-undo.
  define var is-detail as log no-undo.
  */
&endif
