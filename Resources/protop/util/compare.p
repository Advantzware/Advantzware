/* compare.p
 *
 * compare old and new tabanalys output
 *
 * grep ^PUB old.tba | grep -v "^PUB._" > old.tbx
 * grep ^PUB new.tba | grep -v "^PUB._" > new.tbx
 *
 * pro -p compare.p -param "old.tbx,new.tbx"
 *
 *
 */

define temp-table dtab
  field tname  as character format "x(18)" label "Table"
  field reccnt as integer   format ">,>>>,>>>,>>9" label "Records"
.

define temp-table ltab
  field tname  as character format "x(30)"
  field reccnt as integer   format ">,>>>,>>>,>>9"
.

define variable xnote  as character no-undo format "x(40)" label "Note".

define variable i as integer no-undo.
define variable v as integer no-undo format "->,>>>,>>>,>>9" label "Variance".

/* read the dump data
 *
 */

input from value ( entry( 1, session:parameter )).

repeat:
  create dtab.
  import dtab.tname dtab.reccnt.
end.

input close.
delete dtab.	/* stray empty record... */

/* read the load data
 *
 */

input from value ( entry( 2, session:parameter )).

repeat:
  create ltab.
  import ltab.tname ltab.reccnt.
end.

input close.
delete ltab.	/* stray empty record... */

form dtab.tname v dtab.reccnt xnote
  with frame a down.

for each dtab no-lock:
  find ltab where ltab.tname = dtab.tname no-error.
  if not available ltab then
    do:
      i = i + 1.
      display dtab.tname "is missing from the load." @ xnote with frame a.
      down with frame a.
    end.
   else if ltab.reccnt <> dtab.reccnt then
    do:
      i = i + 1.
      display dtab.tname ( ltab.reccnt - dtab.reccnt ) @ v dtab.reccnt with frame a.
      down with frame a.
    end.
end.

for each ltab no-lock:
  find dtab where dtab.tname = ltab.tname no-error.
  if not available dtab then
    do:
      i = i + 1.
      display ltab.tname @ dtab.tname "is missing from the dump." @ xnote with frame a.
      down with frame a.
    end.
end.

if i = 0 then
  display "The record counts match.".
 else
  display
    "A negative variance means that fewer records were loaded than expected." skip
    "A positive variance means that more records were loaded than expected." skip
  .

pause.

quit.

