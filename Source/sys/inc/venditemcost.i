/* sys/inc/vendItemCost.i  include file to use new tables for vendor cost     */

DEF VAR lNewVendorItemCost AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR lFound  AS LOG  NO-UNDO.

/* RUN NK1LOOKUP.P instead of using sys/inc/.i */
RUN sys/ref/nk1look.p (cocode, "VendItemCost", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
IF lFound THEN lNewVendorItemCost = IF cReturn = "Yes" THEN YES ELSE No.
