/* gentba.p
 *
 * generates a script to run tabanalys on each storage area in parallel
 *
 * bpro dbName -p util/gentba.p -param "old|new"
 *
 * rm dbname.old.tba ; grep '^PUB.' *.ptba >> dbname.old.tba
 *
 */

define variable oldNew as character no-undo.

if session:parameter <> "" and session:parameter <> ? then
  oldNew = session:parameter.
 else
  oldNew = "xxx".

if opsys = "unix" then
  do:

    output to value( substitute( "ptba.&1.sh", ldbname(1) )).

    put unformatted "#!/bin/sh" skip.
    put unformatted "#" skip.
    put skip(1).

    put unformatted ". $~{HOME~}/bin/dlenv" skip.
    put skip(1).

    put unformatted "TMPTBA=$~{DL~}/tmp/" + oldNew skip.
    put skip(1).
    put unformatted "mkdir $~{TMPTBA~} 2>/dev/null" skip.
    put skip(1).
    put unformatted "rm -f $~{TMPTBA~}/* 2>/dev/null # " skip.
    put skip(1).

    put unformatted "cd $~{SRCDIR~}" skip.
    put skip(1).

  end.
 else
  do:

    output to value( substitute( "ptba.&1.bat", ldbname(1) )).

    put unformatted "@echo off" skip.
    put skip(1).
    put unformatted "call %PROTOP%~\dlbin~\dlenv.bat" skip.

  end.

put skip(1).

for each _Area no-lock where _Area-Num >= 6 and _Area._Area-type <> 7:	/* skip control area, bi area and after image areas		*/

  if opsys = "unix" then
    put unformatted substitute( 'proutil $~{DB~} -C tabanalys "&1" > $~{TMPTBA~}/&2.ptba && ', _Area-Name, replace( _Area-Name, " ", "_" )) skip.
   else
    put unformatted substitute( 'call proutil $~{DB~} -C tabanalys "&1" > %DL_LGDIR%~\&2.ptba && ', _Area-Name, replace( _Area-Name, " ", "_" )) skip.

end.

put skip(1).


if opsys = "unix" then
  do:

    put unformatted "wait" skip.
    put skip(1).

    put unformatted "grep -h ^PUB $~{TMPTBA~}~/" + '*.ptba | grep -v "^PUB._" ' + "| awk '~{print $1, $2~}' | sort -f > $~{DL_LGDIR~}/$~{DB~}." + oldNew + ".tbx" skip.
    put skip(1).

  end.

output close.

return.
