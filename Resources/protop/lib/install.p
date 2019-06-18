/* lib/install.p
 *
 * tailor a ProTop version 3 install
 *
 */

define new global shared variable dbgMode   as integer no-undo initial 3.
define new global shared variable waitLimit as integer no-undo.

define variable envDLC     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "    OpenEdge".
define variable envPT3     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "    ProTop 3".
define variable envLOG     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "   Log Files".
define variable envTMP     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "  Temp Files".
define variable envRPT     as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "     Reports".

define variable envPROXY   as character no-undo.
define variable envHOST    as character no-undo.
define variable envUPD     as character no-undo.

define variable srvName    as character no-undo.
define variable pvers      as character no-undo.

define variable custId     as character no-undo format "x(250)" view-as fill-in size 15 by 1 label " Customer ID".

define new global shared
       variable pt_proxy   as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "       Proxy".

define variable PT3env     as character no-undo.		/* bin/protopenv -- not $PROTOP		*/

define variable ok         as logical   no-undo.
define variable useSockets as logical   no-undo.
define variable DS         as character no-undo.
define variable f          as character no-undo.
define variable p          as character no-undo.

define variable bkupName   as character no-undo.
define variable stubDir    as character no-undo.
define variable stubName   as character no-undo.
define variable baseName   as character no-undo.
define variable saveName   as character no-undo.
define variable scriptName as character no-undo.

define stream instStream.
define stream inStrm.

{lib/v9.i}
{lib/osinfo.i}

if os-getenv( "USESOCKETS" ) = "no" then
  useSockets = no.
 else
  useSockets = yes.

run ssg/sausage02.p persistent.
run ssg/sausage06.p persistent.


/* allow the debug mode to be set externally
 */

define variable x as integer no-undo.

x = integer( os-getenv( "DBGMODE" )).

if x <> ? and x > 0 and x < 9 then dbgMode = x.


/*****/


/* logMsg
 *
 * three ways to call it:
 *
 * logMsg( msgLevel, messageText ).
 * publish "logMsg" ( msgLevel, messageText ).
 * run doLogMsg ( msgLevel, messageText ).
 *
 */

define new global shared variable logLevel as integer no-undo initial 5.

define stream logStream.

subscribe to "logMsg" anywhere run-procedure "doLogMsg".

function logMsg returns logical ( input msgLevel as integer, input msgText as character ):

  run doLogMsg( msgLevel, msgText ).

  return true.

end.


procedure doLogMsg:

  define input parameter msgLevel as integer   no-undo.
  define input parameter msgText  as character no-undo.

  if msgLevel <= logLevel then
    do:
      output stream logStream to value( "pt3inst.log" ) append.
      put stream logStream unformatted today " " string( time, "hh:mm:ss" ) " " msgText skip.
      output stream logStream close.
    end.

  return.

end.


define stream stubStream.
define stream scriptStream.

procedure tailor:

  define input parameter srcFile as character no-undo.
  define input parameter dstFile as character no-undo.

  define variable scriptBody as character no-undo.

  if session:batch = no and dbgMode > 3 then message "Tailoring:" srcFile "==>" dstFile.

  /* copy-lob would be nice but we are still supporting v9...
   */

  input stream stubStream from  value( srcFile ).
  output stream scriptStream to value( dstFile ).

  repeat:

    scriptBody = "".
    import stream stubStream unformatted scriptBody.

    scriptBody = replace( scriptBody, "#PT3ENV#", PT3env ).
    scriptBody = replace( scriptBody, "#ENVDLC#", envDLC ).
    scriptBody = replace( scriptBody, "#ENVPT3#", envPT3 ).
    scriptBody = replace( scriptBody, "#ENVLOG#", envLOG ).
    scriptBody = replace( scriptBody, "#ENVTMP#", envTMP ).
    scriptBody = replace( scriptBody, "#ENVRPT#", envRPT ).
    scriptBody = replace( scriptBody, "#PTHOST#", envHOST ).
    scriptBody = replace( scriptBody, "#PTUPD#",  envUPD ).

    if index( stubName, "protop.cfg" ) > 0 and pt_proxy <> "" then
      scriptBody = replace( scriptBody, "#PROXY#",  "proxy	" + pt_proxy ).
     else
      scriptBody = replace( scriptBody, "#PROXY#",  pt_proxy ).

    put stream scriptStream unformatted scriptBody skip.
    if scriptBody = "" then put stream scriptStream unformatted skip(1).

  end.
  input stream stubStream close.
  output stream scriptStream close.

  return.

end.


/* main body
 */

pause 0 before-hide.

if session:batch = no then run lib/chkscreen.p.

DS = ( if opsys begins "win" then "~\" else "/" ).

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
        if session:batch = no then message "search(" f ") failed -- trying {&FILE-NAME}".
        file-info:file-name = "{&FILE-NAME}".
        p = file-info:full-pathname.
      end.

    if p = ? then
      do:
        logMsg( 0, "Unknown install path, shamefully quitting." ).
        if session:batch = no then message "Unknown install path, shamefully quitting.".
        quit.
      end.

    envPT3 = substring( p, 1, length( p ) - length( f ) - 1 ).

  end.

if session:batch = no then 
  do:
    message "ProTop 3 is being installed in:" envPT3.
    message "Detailed log is: pt3inst.log".
  end.

logMsg( 0, substitute( "ProTop 3 is being installed in: &1, DS = &2", envPT3, DS )).

assign
  envDLC   = os-getenv( "DLC" )
  envLOG   = os-getenv( "LOGDIR" )
  envTMP   = os-getenv( "TMPDIR" )
  envRPT   = os-getenv( "RPTDIR" )
  custId   = os-getenv( "CUSTID" )
  envHOST  = os-getenv( "PTHOST" )
  envUPD   = os-getenv( "PTUPD" )
  envPROXY = os-getenv( "PROXY" )
.

if envDLC   = ? then envDLC   = "".
if envPROXY = ? then envPROXY = "".

pt_proxy = envPROXY.

if envHOST  = ? or envHOST = "" then envHOST = "demo.wss.com".
if envUPD   = ? or envUPD  = "" then envUPD  = "demo.wss.com".

logMsg( 0, ( if session:parameter = "" then "environment variables" else "session:parameter" )).
logMsg( 0, substitute( "Values provided: [DLC=&1] [LOGDIR=&2] [TMPDIR=&3] [RPTDIR=&4] [CUSTID=&5] [ENVPT3=&6] [PTHOST=&7] [PTUPD=&8] [PROXY=&9]", envDLC, envLOG, envTMP, envRPT, custId, envPT3, envHOST, envUPD, envPROXY )).

/* set some sensible initial values
 */

if envLOG = ? or envLOG = "" then envLOG = envPT3 + DS + "log".
if envTMP = ? or envTMP = "" then envTMP = envPT3 + DS + "tmp".
if envRPT = ? or envRPT = "" then envRPT = envPT3 + DS + "rpt".

if custID = ? or custId = "" then
  do:
    file-info:file-name = envPT3 + DS + "etc" + DS + "custid.cfg".
    if file-info:full-pathname <> ? then
      do on error undo, leave
         on endkey undo, leave:
        input stream inStrm from value( file-info:full-pathname ).
        import stream inStrm unformatted custId.
        input stream inStrm close.
      end.
  end.

if custId = ? then custId = "".

logMsg( 0, substitute( "Initial values: [DLC=&1] [LOGDIR=&2] [TMPDIR=&3] [RPTDIR=&4] [CUSTID=&5] [ENVPT3=&6]", envDLC, envLOG, envTMP, envRPT, custId, envPT3 )).

run osInfo.

find first tt_osInfo no-error.
if available tt_osInfo then
  logMsg( 0, substitute( "OSInfo: [host=&1] [os=&3] [osVersion=&3] [osKernel=&4] [cpu=&5] [clock=&6] [numcpu=&7] [RAM=&8]", tt_osInfo.hostName, tt_osInfo.osName, tt_osInfo.osVersion, tt_osInfo.keVersion, tt_osInfo.modelCPU, tt_osInfo.clkCPU, tt_osInfo.numCPU, tt_osInfo.memTotal )).
 else
  logMsg( 0, substitute( "OSInfo: <not available>" )).

file-info:file-name = substitute( "&1&2version", envDLC, DS ).
if file-info:full-pathname = ? then
  pvers = proversion.
 else
  do:
    input stream inStrm from value( file-info:full-pathname ).
    do on error undo, leave
       on endkey undo, leave:
      import stream inStrm ^ ^ pvers.
    end.
    input stream inStrm close.
  end.

&IF "{&PROCESS-ARCHITECTURE}" = "64" &THEN
logMsg( 0, substitute( "proInfo: [proversion=&1] [bits=&2]", pvers, 64 )).
&ELSE
logMsg( 0, substitute( "proInfo: [proversion=&1] [bits=&2]", pvers, 32 )).
&ENDIF


/* prompt the user for information if necessary
 *
 */

run lib/inschui.p (
  input-output custId,
  input-output envHOST,
  input-output envPT3,
  input-output envDLC,
  input-output envLOG,
  input-output envTMP,
  input-output envRPT
).


/* the actual work of installing protop
 */

envDLC = right-trim( envDLC, DS ).
envPT3 = right-trim( envPT3, DS ).
envLOG = right-trim( envLOG, DS ).
envTMP = right-trim( envTMP, DS ).
envRPT = right-trim( envRPT, DS ).

if session:batch = no then message "Tailoring...".

logMsg( 0, substitute( "Initial working values: [DLC=&1] [LOGDIR=&2] [TMPDIR=&3] [RPTDIR=&4] [CUSTID=&5] [ENVPT3=&6]", envDLC, envLOG, envTMP, envRPT, custId, envPT3 )).

custId = trim( custId ).

if custId = "" or custId = ? then
  do:
    output stream instStream to value( envPT3 + DS + "etc" + DS + "custid.cfg" ).
    output stream instStream close.
  end.
 else
  do:
    output stream instStream to value( envPT3 + DS + "etc" + DS + "custid.cfg" ).
    put stream instStream unformatted custId skip.
    output stream instStream close.
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

logMsg( 0, substitute( "Tailored working values: [pt3env=&1] [stubdir=&2]", pt3env, stubdir )).

os-create-dir value( envPT3 + "/bin" ) value( envLOG ) value( envTMP ) value( envRPT ).

/*
if os-error <> 0 then
  do:
    message
      "Error:" os-error "occurred when creating required directories:" skip
      envPT3 + "/bin" envLOG envTMP envRPT skip
      skip(1)
      "Canceling installation" skip
      view-as alert-box
    .
    quit.
  end.
 */

logMsg( 0, substitute( "Created &1, &2, &3 and &4",  envPT3 + "/bin", envLog, envTMP, envRPT )).

bkupName = substitute( "etc.&1.&2.tar", string( month( today ), "99" ), string( day( today ), "99" )).
file-info:file-name = bkupName.
if file-info:full-pathname = ? then
  do:
    logMsg( 0, substitute( "Backing up etc to &1", bkupName )).
    os-command silent value( "tar cvf " + bkupName + " etc" ).
  end.

logMsg( 0, substitute( "Copying stub files from: &1", stubDir )).

input stream instStream from os-dir( stubDir ).

binstub_loop: repeat:

  import stream instStream ^ stubName.

  if substring( stubName, length( stubName ), 1 ) = "." then		/* if it ends in "." it is either "." or ".." or	*/
    next binstub_loop.							/* crazily named and unworthy of our attention		*/

  logMsg( 0, substitute( "Tailoring: &1", stubName )).

  if opsys = "unix" then
    do:

      scriptName = replace( stubName, "ustub", "bin" ).
      if stubName matches "*uemacs*" then
        do:
          os-copy value( stubName ) value( "bin/uemacs" ).
          next binstub_loop.
        end.

    end.
   else
    do:

      if r-index( stubName, ".INI" ) = ( length( stubName ) - 3 ) then
        scriptName = replace( stubName, "wstub", "etc" ).
       else
        scriptName = replace( stubName, "wstub", "bin" ).

    end.

  run tailor( stubName, scriptName ).

  logMsg( 0, substitute( " Finished: &1", scriptName )).

end.		/* binstub_loop	*/

input stream instStream close.

logMsg( 0, "Looking for estub" ).

/* copy files from estub to etc /* ~~~ if they do NOT already exist in etc ~~~ */
 */

file-info:file-name = "estub".
if file-info:full-pathname = ? then
  do:
    logMsg( 0, "No estub directory was found" ).
  end.
 else
  do:

    input stream instStream from os-dir( "estub" ).

    estub_loop: repeat:

      import stream instStream ^ stubName.

      if substring( stubName, length( stubName ), 1 ) = "." then	/* if it ends in "." it is either "." or ".." or	*/
        next estub_loop.						/* crazily named and unworthy of our attention		*/

      baseName = substring( stubName, r-index( stubName, "estub" ) + 6 ).

      file-info:file-name = "etc/" + baseName.
      if file-info:full-pathname <> ? then
        do:

          if lookup( baseName, "custid.cfg,dblist.cfg" ) > 0 then next.	/* skip these, do NOT copy or over-write */

          if lookup( baseName, "protop.pf" ) > 0 then
            do:
              saveName = file-info:full-pathname + substitute( ".&1.&2", string( month( today ), "99" ), string( day( today ), "99" )).
              logMsg( 0, substitute( "&1 already exists in etc, saving a copy as &2", baseName, saveName )).
              file-info:file-name = saveName.
              if file-info:full-pathname = ? then
              os-copy value( "etc/" + baseName ) value( saveName ).
            end.

          /* do NOT tailor (binary files)
           */

          if lookup( baseName, "protop.ico" ) > 0 then
            do:
              os-copy value( "estub/" + baseName ) value( "etc/" + baseName ).
              next estub_loop.
            end.

        end.

      logMsg( 0, substitute( "Tailoring: &1", stubName )).
      scriptName = replace( stubName, "estub", "etc" ).
      run tailor( stubName, scriptName ).

    end.	/* estub_loop	*/

    input stream instStream close.

  end.

/* end. */


if opsys = "unix" then
  do:
    logMsg( 0, "Fixing permissions" ).
    os-command silent value( "chmod 755 " + envPT3 + "/bin/*" ).
  end.
 else
  do:
    /* ... (PROVERSION BEGINS "10.2B" OR INTEGER(ENTRY (1, PROVERSION, ".")) GE 11) ... */
    if integer( entry( 1, proversion, "." )) >= 11 then
      do:
        logMsg( 0, substitute( "Updating registry with ProTop install directory: &1", envPT3 )).
        run lib/setpt3dir2.p ( envPT3 ).			/* set registry key to locate ProTop install directory -- contributed by Mike @ Consultingwerk	*/
      end.
  end.


file-info:file-name = envTMP + "/dbmonitor.flg".

if file-info:full-pathname = ? then
  do:

    if opsys = "unix" then
      do:
        logMsg( 0, "Starting dbmonitor" ).
        os-command silent value( "bin/dbmonitor.sh" ). 
      end.
     else
      do:

        logMsg( 0, "Installing dbmonitor" ).

        /* nssm_setup needs to run as admin -- right now I don't have a good way to do that
         *
         * 	os-command value( "runas /user:administrator bin~\nssm_setup.bat" ).
         *
         * is close but it prompts for a password even if you are already admin
         *
         * so, for now, I'm going to assume most people are installing as admin
         * and just go ahead and run it
         */

	os-command silent value( "bin~\nssm_setup.bat" ).

      end.

  end.
 else
  do:

    logMsg( 0, "Restarting monitors" ).

    if opsys = "unix" then
      os-command silent value( "rm -f "  + envTMP +  "/*.flg" ).
     else
      os-command silent value( "del /q " + envTMP + "~\*.flg 2>NUL" ).

  end.


logMsg( 0, "Finished!" ).

if session:batch then
  return.
 else
  quit.
