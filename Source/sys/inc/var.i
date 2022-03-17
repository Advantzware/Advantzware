/* -------------------------------------------------- sys/inc/var.i 01/02 JLF */
/*                                                                            */
/* vars & constants include file                                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */

/* should be {1} {2} {3}, out of order for backward compatibility */

DEFINE {1} {3} {2} VARIABLE cocode   AS CHARACTER NO-UNDO FORMAT "x(3)".
DEFINE {1} {3} {2} VARIABLE locode   AS CHARACTER NO-UNDO FORMAT "x(5)".
DEFINE {1} {3} {2} VARIABLE x        AS INTEGER   NO-UNDO.
DEFINE {1} {3} {2} VARIABLE y        AS INTEGER   NO-UNDO.
DEFINE {1} {3} {2} VARIABLE k        AS INTEGER   NO-UNDO.
DEFINE             VARIABLE i        AS INTEGER   NO-UNDO.
DEFINE             VARIABLE j        AS INTEGER   NO-UNDO.
DEFINE             VARIABLE z        AS INTEGER   NO-UNDO.
DEFINE             VARIABLE xxx      AS DECIMAL   NO-UNDO.
DEFINE             VARIABLE yyy      AS DECIMAL   NO-UNDO.
DEFINE             VARIABLE zzz      AS DECIMAL   NO-UNDO.
DEFINE             VARIABLE tmpstore AS CHARACTER NO-UNDO.

/* end ---------------------------------- copr. 2001  advanced software, inc. */
