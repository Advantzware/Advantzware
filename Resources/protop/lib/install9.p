/* install.p
 *
 * tailor a ProTop version 3 install
 *
 */

define variable envDLC     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "    OpenEdge".
define variable envPT3     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "    ProTop 3".
define variable envLOG     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "   Log Files".
define variable envTMP     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "  Temp Files".

define variable custId     as character no-undo format "x(250)" view-as fill-in size 20 by 1 label " Customer Id".

define variable PT3env     as character no-undo.		/* bin/protopenv -- not $PROTOP		*/

define variable dirSep     as character no-undo.
define variable f          as character no-undo.
define variable p          as character no-undo.

define variable stubDir    as character no-undo.
define variable stubName   as character no-undo.
define variable baseName   as character no-undo.
define variable scriptName as character no-undo.

define variable scriptBody as character no-undo.

define stream stubStream.
define stream scriptStream.
define stream inStrm.

/* logMsg
 *
 * three ways to call it:
 *
 * logMsg( logLevel, messageText ).
 * publish "logMsg" ( logLevel, messageText ).
 * run doLogMsg ( logLevel, messageText ).
 *
 */

define new global shared variable logLevel as integer no-undo initial 5.

define stream logStream.
output stream logStream to value( "pt3inst.log" ).

subscribe to "logMsg" anywhere run-procedure "doLogMsg".

function logMsg returns logical ( input msgLevel as integer, input msgText as character ):

  run doLogMsg( msgLevel, msgText ).

  return true.

end.


procedure doLogMsg:

  define input parameter msgLevel as integer   no-undo.
  define input parameter msgText  as character no-undo.

  if msgLevel <= logLevel then
    put stream logStream unformatted today " " string( time, "hh:mm:ss" ) " " msgText skip.

  return.

end.


dirSep = ( if opsys begins "win" then "~\" else "/" ).

f = "lib/install.p".

if envPT3 = "" then
  do:

    /* the windows wix installer cannot set the current directory to the install directory
     * so we have to figure it out based on the absolute path to lib/install.p (this program)
     *
     */

    file-info:file-name = search( f ).
    p = file-info:full-pathname.
    if p = ? then
      do:
        logMsg( 0, substitute( "search( &1 ) failed -- trying: &2", f, "{&FILE-NAME}" )).
        message "search(" f ") failed -- trying {&FILE-NAME}".
        file-info:file-name = "{&FILE-NAME}".
        p = file-info:full-pathname.
      end.

    if p = ? then
      do:
        logMsg( 0, "Unknown install path, shamefully quitting." ).
        message "Unknown install path".
        quit.
      end.

    envPT3 = substring( p, 1, length( p ) - length( f ) - 1 ).

  end.

message "ProTop 3 is being installed in:" envPT3.
logMsg( 0, substitute( "ProTop 3 is being installed in: &1, dirSep = &2", envPT3, dirSep )).

if session:parameter = "" then
  assign
    envDLC = os-getenv( "DLC" )
    envLOG = os-getenv( "LOGDIR" )
    envTMP = os-getenv( "TMPDIR" )
    custId = os-getenv( "CUSTID" )
  .
 else
  assign
    envDLC = entry( 1, session:parameter )
    envLOG = entry( 2, session:parameter )
    envTMP = entry( 3, session:parameter )
    custId = entry( 4, session:parameter )
  .

if envDLC = ? then envDLC = "".

logMsg( 0, ( if session:parameter = "" then "environment variables" else "session:parameter" )).
logMsg( 0, substitute( "Values provided: [DLC=&1] [LOGDIR=&2] [TMPDIR=&3] [CUSTID=&4] [ENVPT3=&5]", envDLC, envLOG, envTMP, custId, envPT3 )).

if opsys = "unix" then
  do:

 /* if envLOG = ? then envLOG = "/tmp".
  * if envTMP = ? then envTMP = "/tmp".
  */

    if envLOG = ? then envLOG = envPT3 + "/log".
    if envTMP = ? then envTMP = envPT3 + "/tmp".

  end.
 else
  do:

 /* if envLOG = ? then envLOG = "c:~\temp".
  * if envTMP = ? then envTMP = "c:~\temp".
  */

    if envLOG = ? then envLOG = envPT3 + "~\log".
    if envTMP = ? then envTMP = envPT3 + "~\tmp".

  end.

if custID = ? then
  do:
    file-info:file-name = envPT3 + dirSep + "etc" + dirSep + "custid.cfg".
    if file-info:full-pathname <> ? then
      do:
        input stream inStrm from value( file-info:full-pathname ).
        import stream inStrm unformatted custId.
        input stream inStrm close.
      end.
  end.

logMsg( 0, substitute( "Initial values: [DLC=&1] [LOGDIR=&2] [TMPDIR=&3] [CUSTID=&4] [ENVPT3=&5]", envDLC, envLOG, envTMP, custId, envPT3 )).

do on error undo, leave
   on endkey undo, leave
   on stop undo, leave:

  do while session:batch = no:

    update
      skip(1)
      envDLC skip
      envPT3 skip
      envLOG skip
      envTMP skip
      skip(1)
      "    A custId is needed to enable data transfer to support the web  " skip
      "    browser user interface.  Your firewall must have port 80 open  " skip
      "    for outbound traffic on the db server for this to work.        " skip
      skip(1)
      "    Leave custId as ? if you do NOT want ProTop to communicate     " skip
      "    with our cloud based monitoring tools.                         " skip
      skip(1)
      custId " (optional)" skip
      skip(1)
      "    Contact tom@wss.com to obtain a custId.                        " skip
      skip(1)
     with
      frame updEnv
      title " ProTop 3 Configuration "
      centered
      side-labels
    .

    logMsg( 0, substitute( "User values: [DLC=&1] [LOGDIR=&2] [TMPDIR=&3] [CUSTID=&4] [ENVPT3=&5]", envDLC, envLOG, envTMP, custId, envPT3 )).

    if envDLC = "" or envPT3 = "" or envLOG = "" or envTMP = "" or envDLC = ? or envPT3 = ? or envLOG = ? or envTMP = ? then
      next.
     else
      leave.

  end.

  envDLC = right-trim( envDLC, dirSep ).
  envPT3 = right-trim( envPT3, dirSep ).
  envLOG = right-trim( envLOG, dirSep ).
  envTMP = right-trim( envTMP, dirSep ).

  logMsg( 0, substitute( "Working values: [DLC=&1] [LOGDIR=&3] [TMPDIR=&3] [CUSTID=&4] [ENVPT3=&5]", envDLC, envLOG, envTMP, custId, envPT3 )).

  if custId <> "" and custId <> ? then
    do:
      output to value( envPT3 + dirSep + "etc" + dirSep + "custid.cfg" ).
      put unformatted custId skip.
      output close.
    end.

  if opsys = "unix" then
    do:
      PT3env = envPT3 + "/bin/protopenv".
      stubDir = envPT3 + "/ustub".
    end.
   else
    do:
      PT3env = envPT3 + "~\bin~\protopenv.bat".
      stubDir = envPT3 + "~\wstub".
    end.

  logMsg( 0, substitute( "Working values: [DLC=&1] [LOGDIR=&2] [TMPDIR=&3] [CUSTID=&4] [ENVPT3=&5]", envDLC, envLOG, envTMP, custId, envPT3 )).

  os-create-dir value( envPT3 + "/bin" ) value( envLOG ) value( envTMP ).
  os-create-dir value( envPT3 + "/rpt" ).

  if os-error <> 0 then
    do:
      message
        "Error:" os-error "occurred when creating directories:" skip
        envPT3 + "/bin" envLOG envTMP skip
        skip(1)
        "Canceling installation" skip
        view-as alert-box
      .
      quit.
    end.

  logMsg( 0, substitute( "Created &1, &2, &3 and &4",  envPT3 + "/bin", envLog, envTMP, envPT3 + "/rpt" )).

  logMsg( 0, substitute( "Copying stub files from: &1", stubDir )).

  input stream inStrm from os-dir( stubDir ).

  repeat:

    import stream inStrm ^ stubName.

    if substring( stubName, length( stubName ), 1 ) = "." then next.	/* if it ends in "." it is either "." or ".." or	*/
									/* crazily named and unworthy of our attention		*/

    logMsg( 0, substitute( "Processing: &1", stubName )).

    if opsys = "unix" then
      do:
        scriptName = replace( stubName, "ustub", "bin" ).
      end.
     else
      do:

        if r-index( stubName, ".BAT" ) = ( length( stubName ) - 3 ) then
          scriptName = replace( stubName, "wstub", "bin" ).
         else if r-index( stubName, ".INI" ) = ( length( stubName ) - 3 ) then
          scriptName = replace( stubName, "wstub", "etc" ).
         else
          next.

      end.

    /*
    copy-lob from file stubName to scriptBody.
    scriptBody = replace( scriptBody, "#PT3ENV#", PT3env ).
    scriptBody = replace( scriptBody, "#ENVDLC#", envDLC ).
    scriptBody = replace( scriptBody, "#ENVPT3#", envPT3 ).
    scriptBody = replace( scriptBody, "#ENVLOG#", envLOG ).
    scriptBody = replace( scriptBody, "#ENVTMP#", envTMP ).
    copy-lob from scriptBody to file scriptName.
     */

    input stream stubStream from value( stubName ).
    output stream scriptStream to value( scriptName ).
    repeat:
      scriptBody = "".
      import stream stubStream unformatted scriptBody.
      scriptBody = replace( scriptBody, "#PT3ENV#", PT3env ).
      scriptBody = replace( scriptBody, "#ENVDLC#", envDLC ).
      scriptBody = replace( scriptBody, "#ENVPT3#", envPT3 ).
      scriptBody = replace( scriptBody, "#ENVLOG#", envLOG ).
      scriptBody = replace( scriptBody, "#ENVTMP#", envTMP ).
      put stream scriptStream unformatted scriptBody skip.
      if scriptBody = "" then put stream scriptStream unformatted skip(1).
    end.
    input stream stubStream close.
    output stream scriptStream close.

    logMsg( 0, substitute( "Finished: &1", scriptName )).

  end.

  input stream inStrm close.

  logMsg( 0, "Looking for estub" ).

  /* copy files from estub to etc if they do NOT already exist in etc
   */

  file-info:file-name = "estub".
  if file-info:full-pathname <> ? then
    do:

      input stream inStrm from os-dir( "estub" ).

      repeat:

        import stream inStrm ^ stubName.

        if substring( stubName, length( stubName ), 1 ) = "." then next. /* if it ends in "." it is either "." or ".." or	*/
									 /* crazily named and unworthy of our attention		*/

        baseName = substring( stubName, r-index( stubName, "estub" ) + 6 ).

        file-info:file-name = "etc/" + baseName.
        if file-info:full-pathname <> ? then
          logMsg( 0, substitute( "&1 already exists in etc, skipping.", baseName )).
         else
          do:
            logMsg( 0, substitute( "Copying &1 to etc", baseName )).
            os-copy value( stubName ) value( "etc" ).
          end.

      end.

      input stream inStrm close.

    end.

end.

if opsys = "unix" then
  do:
    logMsg( 0, "Fixing permissions" ).
    os-command silent value( "chmod 755 " + envPT3 + "/bin/*" ).
  end.

logMsg( 0, "Finished!" ).

quit.
