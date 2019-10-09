/* ttblWidgetCreate.i - used to create widget-handle temp-table record */

CREATE {1}.
ASSIGN
  {1}.idx = {2}
  {1}.{1} = {3}:HANDLE.
  &IF '{4}' NE '' &THEN
  {1}.{4} = {5}.
  &ENDIF
  &IF '{6}' NE '' &THEN
  {1}.{6} = {7}.
  &ENDIF
