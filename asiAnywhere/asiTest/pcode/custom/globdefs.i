/* globdefs.i */

DEFINE {&NEW} SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_sysdate AS DATE NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_period AS INTEGER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_init AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE g_batch-rowid AS rowid NO-UNDO.

&IF '{&NEW}' = 'NEW GLOBAL' &THEN
ASSIGN
  g_init = yes
  g_sysdate = TODAY.
&ENDIF
