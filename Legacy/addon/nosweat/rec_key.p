def var i as int no-undo.

output to 'rec_key.df'.
for each _file no-lock where not _file-name begins '_':
  for each _field of _file by _order:
    i = _order.
  end.
  put unformatted
    'ADD FIELD "rec_key" OF "' _file-name '" AS character ' skip
    '  FORMAT "X(20)"' skip
    '  INITIAL ""' skip
    '  LABEL "Record Key"' skip
    '  HELP "Enter Record Key Value"' skip
    '  ORDER ' i + 10 skip(1)
    'ADD INDEX "si-rec_key" ON "' _file-name '" ' skip
    '  DESCRIPTION "Completed Jobs Secondary Index by rec_key"' skip
    '  INDEX-FIELD "rec_key" ASCENDING ' skip(1).
end.
put unformatted
  '.' skip
  'PSC' skip
  'codepage=iso8859-1' skip
  '.' skip
  '0000001505' skip.
output close.
