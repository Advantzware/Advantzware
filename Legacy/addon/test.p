
def var result as log no-undo.

run adecomm/_osprint.p (INPUT ?,INPUT "r:\asiaddon\development\source\users\nosweat\contact list.rpt",
INPUT 10, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).


/*
def var i as int.

do i = 1 to font-table:num-entries:
 message
   "font" i
   font-table:get-text-height-chars(i)
   font-table:get-text-height-chars(i)
   font-table:get-text-height-chars(i)
   font-table:get-text-height-chars(i)
   view-as alert-box.
end.
*/
