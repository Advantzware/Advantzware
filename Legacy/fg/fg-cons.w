/* fg/fg-cons.w*/

DEF NEW SHARED VAR choice AS LOG NO-UNDO. /* for post fg */

LAST-EVENT:SET-LASTKEY(0, -1).
RUN addon/fg/fg-cons.w.
