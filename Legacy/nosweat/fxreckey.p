/* fxreckey.p */

output to "custom/fxreckey.p".
if can-find(_file where _file-name = "rec_key") then
put unformatted
  "for each rec_key exclusive:" skip
  "  delete rec_key." skip
  "end." skip.
for each _file where not _file-name begins "_"
                 and not can-do("mfvalues,notes,rec_key,sys-ctrl",_file-name) no-lock:
  if not can-find(_field of _file where _field-name = "rec_key") then
  next.
  put unformatted
  "run " _file-name "_rec_key." skip
  "procedure " _file-name "_rec_key:" skip
  "  for each " _file-name " exclusive:" skip
  "    create rec_key." skip
  "    assign" skip
  "      " _file-name ".rec_key = STRING(TODAY,~"99999999~") + " skip
  "                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),~"99999999~")" skip
  "      rec_key.rec_key = " _file-name ".rec_key" skip
  "      rec_key.table_name = ~"" _file-name "~"." skip
  "  end." skip
  "end procedure." skip.
end.
output close.
