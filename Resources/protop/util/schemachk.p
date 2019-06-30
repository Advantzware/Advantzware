/* schemachk.p
 *
 * look for RECID and LOB fields -- they can be troublesome when dumping & loading
 *
 */
  
define variable i as integer no-undo format ">>9".
  
form
  i _file._file-name _field._field-name _field._data-type
 with
  frame a
  down
.
         
for each _file no-lock where _hidden = no:
  for each _field no-lock of _file:
    if _field._data-type = "recid" or _field._data-type matches "*LOB" then
      do:
        i = i + 1.
        display i _file._file-name _field._field-name _field._data-type with frame a.
        down with frame a.
      end.
  end.
end.

pause.

hide frame a.

return.
