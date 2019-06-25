/* lib/getdba.p
 *
 * read proutil dbanalys data, cache the results in dbname.dbx
 *
 * there is a lot of legacy cruft and sloppy coding hanging around in here -- it needs to be cleaned up some day.
 *
 * pro dbname -RO ; run lib/getdba.p ( "dbanalys/dbname.dba" )
 *
 */

{lib/protop.i}

{lib/dba.i}

define new global shared variable dbgMode as integer no-undo initial 1.

define variable r          as integer   no-undo.
define variable x          as character no-undo.
define variable y          as decimal   no-undo.
define variable z          as decimal   no-undo.
define variable xline      as character no-undo extent 512.
define variable curr_area  as character no-undo.
define variable doFixed    as logical   no-undo initial no.
define variable bSize      as integer   no-undo initial 4096.

define variable ztblName as character no-undo.
define variable zidxName as character no-undo.

define stream inStrm.


procedure getTableInfo:

  define input   parameter tblName as character no-undo.
  define output  parameter numRecs as decimal   no-undo.
  define output  parameter numRM   as decimal   no-undo.
  define output  parameter fragPct as decimal   no-undo.
  define output  parameter scatter as decimal   no-undo.
  define output  parameter avgRow  as decimal   no-undo.

  find tblist where tbl = tblName no-error.
  if available tblist then
    assign
      numRecs = tblist.recs
      numRM   = tblist.rm
      fragPct = ( if tblist.recs > 0 then ((( tblist.frag / tblist.recs ) - 1 ) * 100 ) else 0 )
      scatter = tblist.scat
      avgRow  = ( if tblist.recs > 0 then ( tblist.tsz / tblist.recs ) else 0 )
    .

  return.

end.


procedure getIndexInfo:

  define input   parameter idxName     as character no-undo.
  define output  parameter idxLevels   as decimal   no-undo.
  define output  parameter numBlocks   as decimal   no-undo.
  define output  parameter pctUtilized as decimal   no-undo.

  find ixlist where idx = idxName no-error.
  if available ixlist then
    assign
      idxLevels   = ixlist.lvls
      numBlocks   = ixlist.blks
      pctUtilized = ixlist.pctut
    .

  return.

end.


define variable dbxDate as date    no-undo.
define variable dbxTime as integer no-undo.

procedure loadDBX:

  define input parameter dbxOutFile as character no-undo.

  file-info:file-name = right-trim( dbxOutFile, "dba" ) + "dbx".
  if file-info:full-pathname = ? then
    do:
      if true /* dbgMode >= 5 */ then
        message {&NOW} "Unable to find:" dbxOutfile view-as alert-box.
      return.
    end.

  if dbxDate = file-info:file-mod-date and dbxTime = file-info:file-mod-time then
    do:
      /* message {&NOW} "already loaded:" file-info:file-name. pause. */
      return.									/* we have already loaded this so just return	*/
    end.

  assign
    dbxDate = file-info:file-mod-date
    dbxTime = file-info:file-mod-time
  .

  if dbgMode >= 2 then message {&NOW} "Loading dbanalys data".

  /* message {&NOW} "loading:" right-trim( dbxOutFile, "dba" ) + "dbx". pause. */

  empty temp-table tblist.
  empty temp-table ixlist.

  input stream inStrm from value( right-trim( dbxOutFile, "dba" ) + "dbx" ).
  repeat:
    create tblist.
    import stream inStrm tblist.
  end.
  input stream inStrm close.

  input stream inStrm from value( right-trim( dbxOutFile, "dba" ) + "idx" ).
  repeat:
    create ixlist.
    import stream inStrm ixlist.
  end.
  input stream inStrm close.

  for each tblist where tbl = "":
    delete tblist.
  end.

  for each ixlist where idx = "":
    delete ixlist.
  end.

  if dbgMode >= 4 then message {&NOW} "Done loading dbanalys data".

  return.

end.


define variable dbaDate as date    no-undo.
define variable dbaTime as integer no-undo.

define variable dbaNag  as logical no-undo.

procedure checkDBX:

  define variable dbaOutFile as character no-undo.

  dbaOutFile = search( "dbanalys/" + pt_shortname + ".dba" ).			/* first look for dbanalys/pt_shortname.dba	*/
  if dbaOutFile = ? then
    dbaOutFile = search( "dbanalys/" + ldbname(1) + ".dba" ).			/* then look for dbanalys/ldbname(1).dba	*/
  if dbaOutFile = ? then
    dbaOutFile = search( ldbname(1) + ".dba" ).					/* then look everywhere				*/

  /*** message {&NOW} "dbanalys:" dbaOutFile. pause. ***/

  file-info:file-name = dbaOutFile.
  if file-info:full-pathname = ? then
    do:

      if dbaNag = no then
        do:

          dbaNag = yes.

          if session:batch = yes then
            message {&NOW} "Unable to find dbanalys:" pt_shortname "or" ldbname(1).
           else
            message color red
              skip(1)
              "   Unable to find dbanalys for:" pt_shortname "or" ldbname(1)  skip
              skip(1)
              "   ProTop uses this data to enrich the table and index activity screens.   " skip
              "   Without dbanalys data some features are limited.                        " skip
              skip(1)
              "   Ideally you will routinely run a fresh dbanalys at least weekly.        " skip
              skip(1)
              "   To run it offline use a command similar to:                             " skip
              skip(1)
              substitute( "  proutil &1 -C dbanalys > dbanalys/&2.dba  ", ldbname(1), ( if pt_shortname <> "" then pt_shortname else ldbname(1) )) skip
              skip(1)
              "   You can also run dbanalys directly within ProTop with the ~"A~" command.  " skip
              "   (But be aware that this command is IO intense - you may prefer to run   " skip
              "   it during non-peak hours.)                                              " skip
              skip(1)
              view-as alert-box
              title " No DB Analys "
            .

        end.

      return.

    end.

  assign
    dbaDate = file-info:file-mod-date
    dbaTime = file-info:file-mod-time
  .

  /* which is more recent:  .dba or .dbx?
   */

  file-info:file-name = right-trim( dbaOutFile, "dba" ) + "dbx".
  if file-info:full-pathname = ? or
     dbaDate > file-info:file-mod-date or
   ( dbaDate = file-info:file-mod-date and dbaTime > file-info:file-mod-time ) then
    do:
      /* message {&NOW} "parseDBA:" dbaOutFile. pause. */
      run parseDBA( dbaOutFile ).
    end.
   else
    do:
      /* message {&NOW} "loadDBX:" dbaOutFile. pause. */
      run loadDBX( dbaOutFile ).
    end.

  return.

end.


&IF DEFINED( OE10 ) &THEN
{ssg/sausage20.i}
&ENDIF

/*** main block (constructor)
 ***
 ***/

session:add-super-procedure( this-procedure ).

run checkDBX.

return.
