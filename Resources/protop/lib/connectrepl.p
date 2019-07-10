/* connectrepl.p
 *
 */

{lib/v9.i}

/***
define new global shared variable pt_shortname as character no-undo.

file-info:file-name = substitute( "etc/&1.repl.pf", pt_shortname ).
if file-info:full-pathname <> ? then
  do:
    connect value( "-pf " + file-info:full-pathname ).
  end.
 ***/

define variable agentSection    as logical   no-undo.
define variable agentSectionTag as character no-undo.
define variable lineInKey       as character no-undo.

define variable replDB   as character no-undo.
define variable replHost as character no-undo.
define variable replPort as character no-undo.
define variable xtra     as character no-undo.

define stream inStrm.

find first dictdb._Repl-AgentControl no-lock no-error.
if not available _Repl-AgentControl then
  do:

    message {&NOW} "No _Repl-AgentControl, I must be the target".
    return.

  /*
   *    find first dictdb._repl-Agent no-lock no-error.
   *    if available _repl-Agent then
   *      do:
   *        message {&NOW} "Using _Repl-Agent".
   *        create alias replTarget for database dictdb no-error.
   *      end.
   *
   *    return.
   */

  end.

agentSectionTag = substitute( "[control-agent.&1]", _ReplAgtCtl-agentName ).

define variable lineIn as character no-undo.

file-info:file-name = pdbname(1) + ".repl.properties".
if file-info:full-pathname = ? then
  do:
    message {&NOW} "could not find:" pdbname(1) + ".repl.properties".
    message {&NOW} "replication is not configured for this database".
    /* pause. */
    return.
  end.

input stream inStrm from value( file-info:full-pathname ).
repeat:

  lineIn = "".

  import stream inStrm unformatted lineIn.
  if lineIn = "" or lineIn begins "#" then next.

  if agentSection = yes and trim( lineIn ) begins "[" then leave.

  if trim( lineIn ) begins agentSectionTag then
    do:
      agentSection = yes.
      next.
    end.

  if agentSection = no then next.

  if num-entries( lineIn, "=" ) < 2 then next.

  lineInKey = trim( entry( 1, lineIn, "=" )).

  /* message lineIn. */
  /* message lineInKey. */

  case lineInKey:
    when "database" then replDB   = trim( entry( 2, lineIn, "=" )).
    when "host"     then replHost = trim( entry( 2, lineIn, "=" )).
    when "port"     then replPort = trim( entry( 2, lineIn, "=" )).
  end.

  /* if agentSection = yes and trim( lineIn ) begins "[" then leave. */		/* this belongs *before* we process lineInKey... */

end.
input stream inStrm close.

/* full db path names do not work with client/server connections
 */

if index( replDB, "/" ) > 0 then
  replDB = substring( replDB, r-index( replDB, "/" ) + 1 ).

if index( replDB, "~\" ) > 0 then
  replDB = substring( replDB, r-index( replDB, "~\" ) + 1 ).

/* message replDB replHost replPort. */
/* pause. */

file-info:file-name = "etc/replx.pf".
if file-info:full-pathname <> ? then xtra = substitute( "-pf &1", file-info:full-pathname ).

do on error  undo, leave
   on endkey undo, leave
   on stop   undo, leave
   on quit   undo, leave:

  connect value( substitute( "-H &2 -S &3 -ld replTarget &4 -db &1", replDB, replHost, replPort, xtra )) no-error.

end.

return.
