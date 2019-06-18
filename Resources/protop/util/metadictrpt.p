/* metadictrpt.p
 *
 * simple, concise dictionary report
 *
 * a variant of util/dictrpt.p that only reports on the metaschema and VST tables
 *
 * december 9, 1988 */

define new global shared variable pt_rptdir      as character no-undo initial ".".
define new global shared variable pt_shortname   as character no-undo.

define variable i          as integer   no-undo.
define variable idx-member as character no-undo.
define variable rptOut     as character no-undo.

form header
     "Table                           Indexes Field                           Order  Type         Extent   Format           " skip
     "----------------------------------------------------------------------------------------------------------------------" skip
  with frame page-header
       no-labels
       page-top
       width 132
       no-attr-space.

form _file._file-name   format "x(30)"
     idx-member
     _field._field-name format "x(30)"
     _field._order      space( 4 )
     _field._data-type
     _field._extent     space( 8 )
     _field._format
  with frame field-list
       down
       no-box
       no-labels
       width 132
       no-attr-space.

rptOut = substitute( "&1/&2.metadict.rpt", pt_rptdir, max( pt_shortname, ldbname(1))).

output to value( rptOut ) page-size 80.

view frame page-header.

for each _file no-lock where
      _file._file-name begins "_"
    by _file._file-name:

  for each _field no-lock where
        _field._file-recid = recid( _file )
      break by _field._file-recid by _field._order:

    if ( line-counter > page-size ) then page.
    if ( first-of( _field._file-recid ) and ( line-counter > ( page-size - 4 ))) then page.  /* don't start a table unless we can print a few lines.... */

    if ( first-of( _field._file-recid ) or
         (( line-counter > ( page-size - 4 )) and ( first-of( _field._file-recid ))) or
         ( line-counter <= 1 )) then display _file._file-name with frame field-list.

    /* index membership of the field	*/

    i = 0.
    idx-member = "".
    for each _index where _index._file-recid = recid( _file ) no-lock:
      for each _index-field where _index-field._index-recid = recid( _index ) and _index-field._field-recid = recid( _field ) no-lock:
        if _index._unique then
          if recid( _index ) = _file._prime-index then
            substring( idx-member, 8 - i, 1 ) = "U".
           else
            substring( idx-member, 8 - i, 1 ) = "u".
         else
          if recid( _index ) = _file._prime-index then
            substring( idx-member, 8 - i, 1 ) = "+".
           else
            substring( idx-member, 8 - i, 1 ) = "*".
      end.
      i = i + 1.
      if i = 8 then leave.
    end.

    display idx-member			/* display field attributes	 */
            _field._field-name
            _field._order
            _field._data-type
            _field._extent
            _field._format
      with frame field-list.

    down 1 with frame field-list.

  end.

  down 1 with frame field-list.		/* a blank line between tables	*/

end.

hide frame page-header.
page.

output close.

/** message "Output is in" rptOut view-as alert-box. **/

return.
