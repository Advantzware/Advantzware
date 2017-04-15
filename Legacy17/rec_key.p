output to 'asi.df'.
for each _file no-lock where not _file-name begins '_':
  if _file-name = 'cust' then
  next.
  find last _field of _file no-lock use-index _field-position.
  put unformatted
    'ADD FIELD "rec_key" OF "' _file-name '" AS character ' skip
    '  FORMAT "X(20)"' skip
    '  INITIAL ""' skip
    '  LABEL "Rec Key"' skip
    '  HELP "Enter Rec Key"' skip
    '  ORDER ' _order + 10 skip(1)
    'ADD INDEX "rec_key" ON "' _file-name '" ' skip
    '  DESCRIPTION "Secondary Index by rec_key"' skip
    '  INDEX-FIELD "rec_key" ASCENDING ' skip(1).
end.
put unformatted
  '.' skip
  'PSC' skip
  'codepage=iso8859-1' skip
  '.' skip
  '0000016362' skip.
