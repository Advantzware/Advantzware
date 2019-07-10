/* dumpdotd.p
 *
 * dump a table to a dot-d file in a given directory
 *
 * bpro dbname -p ./dumpdotd.p -param "tableName|/dumpdir" > logfile 2>&1
 *
 * magic:
 *
 * "all"	will dump all tables
 * "tblName"    will dump one specific table
 * "_user"	will dump users to _User.d (note casing)
 * "_sequence"	will dump sequences to _seqvals.d
 *
 */

define variable tblName as character no-undo.
define variable dumpDir as character no-undo.

assign
  tblName = entry( 1, session:parameter, "|" )
  dumpDir = entry( 2, session:parameter, "|" )
no-error.

if dumpDir = "" or dumpDir = ? then dumpDir = ".".

if lookup( tblName, "_seqvals,_sequence" ) < 1 then
  do:
    run prodict/dump_d( tblName, dumpDir, "" ).
    find _file no-lock where _file-name = tblName no-error.
    if available _file and _dump-name <> _file-name then
      do:
        os-rename value( substitute( "&1/&2.d", dumpDir, _dump-name )) value( substitute( "&1/&2.d", dumpDir, _file-name )).
      end.
  end.
 else
  do:
    output to value( substitute( "&2/&1.d", "_seqvals", dumpDir )).		/* _seqvals.d for compatibility with the data dictionary */
    for each _sequence no-lock:
      export _seq-Num _seq-Name dynamic-current-value( _seq-name, "dictdb" ).
    end.
    output close.
  end.

return.
