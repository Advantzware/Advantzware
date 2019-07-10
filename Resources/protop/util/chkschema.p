/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2010 Tom Bascom, Greenfield Technologies                  **
 **  http://www.greenfieldtech.com                                            **
 **                                                                           **
 **  The utilities in this directory are NOT part of the publicly             **
 **  distributed ProTop release.                                              **
 **                                                                           **
 **  These utilities are provided as part of an active White Star Software,   **
 **  LLC consulting engagement or in support of a DBAppraise, LLC monitoring  **
 **  and alerting subscription.                                               **
 **                                                                           **
 **  Use or distribution of these utilities outside of that context is        **
 **  prohibited.                                                              **
 **                                                                           **
 **                                                                           **
 *******************************************************************************
 *******************************************************************************
 *
 * chkschema.p
 *
 * Check for objects in the schema area (there shouldn't be any...)
 *
 */

define new global shared variable pt_logdir    as character no-undo initial ".".
define new global shared variable pt_shortName as character no-undo.

define variable monname     as character no-undo initial "chkschema".
define variable logFileName as character no-undo.

define variable dummy1      as character format "x(20)" label "Object Name".
define variable dummy2      as character format "x(20)" label "Parent Object".

define variable i as integer no-undo.

define buffer stobuf  for _storageobject.
define buffer areabuf for _area.

form
    _storageobject._object-number format "->>>>9" label "Obj#"
    _storageobject._object-type   format ">>>9"   label "Type"
    dummy1
    dummy2
    areabuf._area-name format "x(20)" label "Parent Area Name"
  with frame a down.

if pt_shortName = "" then pt_shortName = ldbname( 1 ).

logFileName = substitute( "&1/&2.&3.&4", pt_logdir, monName, pt_shortname, "log" ).

output to value ( logFileName ).

find first dictdb._DB no-lock.
find _area no-lock where _area-name = "Schema Area".

display today " " string( time, "hh:mm:ss" ) " Schema Check for DB: " pdbname(1) format "x(30)" skip.

for each _storageobject no-lock
   where _storageobject._DB-Recid = recid( _db )
     and _storageobject._area-number = _area._area-number
     and _storageobject._object-num > 7:

    /***
     and _storageobject._object-num > 0
     and _storageobject._object-associate > 0:
     ***/

  case _storageobject._object-type:

    when 1 then		/* table	*/
      do:
        find _file no-lock where _file._file-num = _storageobject._object-num.
        if not _file._file-name begins "_" then
          do:
            display 
              _storageobject._object-number format "->>>>9" label "Obj#"
              _storageobject._object-type   format ">>>9"   label "Type"
              _file._file-name @ dummy1
            with frame a.
            down with frame a.
            i = i + 1.
          end.
      end.

    when 2 then		/* index	*/
      do:
        find _index no-lock where _index._idx-num = _storageobject._object-num.
        find _file no-lock where _file._file-num = _storageobject._object-associate.
        find stobuf no-lock where stobuf._object-num = _file._file-num and stobuf._object-type = 1.
        find areabuf no-lock where areabuf._area-number = stobuf._area-number.
        if not _index._index-name begins "_" and
           not _file._file-name   begins "_" then
          do:
            display
              _storageobject._object-number format "->>>>9" label "Obj#"
              _storageobject._object-type   format ">>>9"   label "Type"
              _index._index-name @ dummy1
              _file._file-name @ dummy2
              areabuf._area-name
            with frame a.
            down with frame a.
            i = i + 1.
          end.
      end.

    when 3 then		/* LOB	*/
      do:
        find _field no-lock where _field._fld-stlen = _storageobject._object-number.
        find _file no-lock where _file._file-num = _storageobject._object-associate.
        find stobuf no-lock where stobuf._object-num = _file._file-num and stobuf._object-type = 1.
        find areabuf no-lock where areabuf._area-number = stobuf._area-number.
        if not _index._index-name begins "_" and
           not _file._file-name   begins "_" then
          do:
            display
              _storageobject._object-number format "->>>>9" label "Obj#"
              _storageobject._object-type   format ">>>9"   label "Type"
              _field._field-name @ dummy1
              _file._file-name @ dummy2
              areabuf._area-name
            with frame a.
            down with frame a.
            i = i + 1.
          end.
      end.

  end case.

end.

if i = 0 then display "The schema area is clean.".

output close.

return.
