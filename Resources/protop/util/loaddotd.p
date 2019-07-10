/* loaddotd.p
 *
 * load a dot-d file in a given directory into a table
 *
 * bpro dbname -p ./loaddotd.p -param "tableName|/loadDir/tableName.d" > logfile 2>&1
 *
 */

if entry( 1, session:parameter, "|" ) = "_seqvals" then
  run util/loadseq.p.
 else
  run prodict/load_d.p (
    input entry( 1, session:parameter, "|" ),
    input entry( 2, session:parameter, "|" )
  ).

return.
