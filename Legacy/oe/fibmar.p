DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

DEFINE INPUT PARAMETER ip-est-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-est-type AS INT NO-UNDO.
DEFINE INPUT PARAMETER ip-qty AS INT NO-UNDO.
DEFINE INPUT PARAMETER ip-cocode AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-locode AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER op-margin AS DEC NO-UNDO.

ASSIGN
   cocode = ip-cocode
   locode = ip-locode
   qty = ip-qty.

IF ip-est-type LT 5 THEN
   RUN oe\foldmar.p(INPUT ip-est-no,
                    INPUT qty,
                    OUTPUT op-margin).
ELSE
   RUN oe\corrmar.p(INPUT ip-est-no,
                    INPUT qty,
                    OUTPUT op-margin).
