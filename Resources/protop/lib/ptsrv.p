/* lib/ptsrv.p
 *
 */

{lib/v9.i}

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2 &THEN
&global-define	BIGINT	int64
&ELSE
&global-define	BIGINT	decimal
&ENDIF

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2 AND PROVERSION >= "10.2B" &THEN
&global-define	SINIT	"JSON"
&ELSE
&global-define	SINIT	"XML"
&ENDIF

&IF DEFINED( OE10 ) = 0 &THEN
define new global shared temp-table tt_dataset no-undo
  field ttHandle as handle
.
&ENDIF

define new global shared variable hDataset   as handle    no-undo.	/* ugly hack :( +++ */

define variable serverType as character no-undo.

define variable dcXID    as integer   no-undo.
define variable result   as character no-undo.

define variable monDir   as character no-undo initial "dc".

define variable fifo_in  as character no-undo.
define variable fifo_out as character no-undo.

define variable qName    as character no-undo.

define variable sendType as character no-undo initial {&SINIT}.

define variable sendPretty  as logical   no-undo initial false.
define variable sendSchema  as logical   no-undo initial false.			/* false = no 4GL specific stuff like FORMAT	*/
define variable omitInitVal as logical   no-undo initial false.

define variable ok        as logical     no-undo.
define variable i         as integer     no-undo.
define variable trxNum    as integer     no-undo.
define variable cmd       as character   no-undo.
define variable UID       as character   no-undo.
define variable dcList    as character   no-undo.
define variable oldNow    as integer     no-undo.

define new global shared variable dbgMode  as integer no-undo initial 1.
define new global shared variable rawMode  as integer no-undo initial 5.
define new global shared variable timeMode as integer no-undo initial 2.
define new global shared variable rowLimit as integer no-undo.

define new global shared variable custId       as character no-undo.
define new global shared variable pt_shortname as character no-undo.
define new global shared variable zmonName     as character no-undo.

define new global shared variable x as integer no-undo initial 5.

define stream inStrm.

{ssg/sausage16.i}

define temp-table tt_client no-undo
  field clientId    as character
  field lastContact as {&DTZ}
  field sType       as character initial {&SINIT}			/* XML or JSON						*/
  field rMode       as integer   initial 5
  field tMode       as integer   initial 2
  field rLimit      as integer
.

define temp-table tt_dataCollector no-undo
  field xId      as integer
  field clientId as character
  field xActive  as logical
  field dcName   as character
  field dcHandle as handle
.

run ssg/sausage04.p persistent.						/* sausage lib						*/

function dcLaunch returns logical ( dcClient as character, dcProg as character, dcEnable as logical ):

  dcXID = dcXID + 1.

  create tt_dataCollector.
  assign
    tt_dataCollector.xId      = dcXID
    tt_dataCollector.clientId = dcClient
    tt_dataCollector.xActive  = dcEnable 
  .

  if not connected( "dictdb" ) then
    do:
      if lookup( substring( dcProg, length( monDir ) + 2 ), "apsv.p,dbid.p,df.p,netstat.p,osinfo.p,pasoe.p,appmon.p" ) <= 0 then
        do:
          if dbgMode >= 0 then message {&NOW} dcProg "requires a db connection to launch".
          return yes.
        end.
    end.

  if dbgMode >= 4 then message {&NOW} "launching data collector:" dcProg.

  run value( dcProg ) persistent set tt_dataCollector.dcHandle ( output tt_dataCollector.dcName ).

  if valid-handle( dcHandle ) then run "mon-init" in dcHandle.

  if dbgMode >= 0 then message {&NOW} dcProg "has been initialized as" tt_dataCollector.dcName.

  return yes.

end.

function clientLaunch returns logical ( clientId as character ):

  define variable pname as character no-undo.
  define variable dname as character no-undo.

  find tt_client no-lock where tt_client.clientId = clientId no-error.
  if available( tt_client ) then
    do:
      message "Client:" clientId "exists!".
      return false.
    end.
   else
    do:

      /* message "Launching client:" clientId "!".
       */

      create tt_client.
      assign
        tt_client.clientId = clientId
&IF DEFINED( OE10 ) &THEN
        tt_client.lastContact = now
&ELSE
        tt_client.lastContact = time
&ENDIF
      .

      if dbgMode >= 0 then message {&NOW} "Launching data collectors" clientId.

      input stream inStrm from os-dir( monDir ).

      repeat:
        import stream inStrm ^ pname.
        pname = substring( pname, r-index( pname, monDir )).	/* run as a relative path */

        if substring( pname, length( pname) - 1, 2 ) = ".p" then dcLaunch( clientId, pname, yes ).

      end.

      input stream inStrm close.

/*      /* load monitoring modules from an OS specific directory (if any)
 *       *
 *       */
 *    
 *      dname = "".
 * 
 *      if opsys begins "WIN" then
 *        dname = "WIN32".
 *       else if opsys = "unix" then
 *        do:
 *          input stream inStrm through value( "uname -a" ).
 *          import stream inStrm dname.
 *          input stream inStrm close.
 *        end.
 *  
 *      dname = monDir + "/os/" + dname.
 *
 *      file-info:file-name = dname.
 *
 *      if file-info:full-pathname <> ? and dname <> "" and dname <> ? then
 *        do:
 *
 *          input stream inStrm from os-dir( dname ).
 *
 *          repeat:
 *            import stream inStrm ^ pname.
 *            pname = substring( pname, index( pname, dname )).	/* run as a relative path */
 *            if substring( pname, length( pname) - 1, 2 ) = ".p" then
 *              dcLaunch( clientId, pname, yes ).
 *          end.
 * 
 *          input stream inStrm close.
 * 
 *        end.
 */

      message {&NOW} "all data collectors have been initialized".

      return true.

    end.

end.


procedure initProTopServer:

  define input parameter srvType as character no-undo.				/* "direct", "http" or "stomp"			*/
  define input parameter p1      as character no-undo.				/* server name					*/
  define input parameter p2      as character no-undo.				/* db name					*/

  serverType = srvType.

  file-info:file-name = monDir.
  if ( file-info:full-pathname <> ? ) and index( file-info:file-type, "d" ) > 0 then
    . /* message "Monitor data collectors:" monDir. */
   else
    do:
      message "Cannot find directory:" monDir.
      quit.
    end.

  if serverType <> "direct" then                                        	/* initialize protop environment                */
    do:
      run lib/protoplib.p persistent.
      run lib/vstlib.p persistent.
      run lib/protop-cfg.p persistent.
    end.

&IF DEFINED( OE10 ) &THEN
  create dataset hDataset.
&ELSE
  empty temp-table tt_dataset.
&ENDIF

  return.

end.


procedure doRequest:

  define input  parameter cmd    as character no-undo.
  define output parameter result as {&LNGCR}  no-undo.

  define variable dcName  as character no-undo.
  define variable xList   as character no-undo.
  define variable argVal  as character no-undo.
  define variable argList as character no-undo.

  define variable cscCmd  as character no-undo.
  define variable dbaCmd  as character no-undo.

  define variable my_dbAccess as {&BIGINT} no-undo.
  define variable my_dbRead   as {&BIGINT} no-undo.

  define variable winPID as integer no-undo.

  define variable i as integer no-undo.

  assign
    UID     = ""
    dcList  = ""
    xList   = ""
    argList = ""
  .

/* +++db
 *  find first _myconnection no-lock.
 *  find _userio no-lock where _userio-id = _myconn-userid + 1.
 *  assign
 *    my_dbAccess = _userio-dbAccess
 *    my_dbRead   = _userio-dbRead
 *  .
 */

&IF DEFINED( OE10 ) &THEN
  assign
    ok = hDataset:set-buffers( "" )
  no-error.
&ELSE
  empty temp-table tt_dataset.
&ENDIF

  if num-entries( cmd, "|" ) = 1 then
    xList = cmd.
   else
    assign
      UID    = entry( 1, cmd, "|" )
      xList  = entry( 2, cmd, "|" )
    .

  if xList begins "xcmd=" then
    do:

      find tt_client no-lock where tt_client.clientId = UID.
      assign
        sendType = tt_client.sType
        rawMode  = tt_client.rMode
        timeMode = tt_client.tMode
        rowLimit = tt_client.rLimit
      .

      if num-entries( entry( 2, xList, "=" ), "+" ) > 1 then
        argList = entry( 2, entry( 2, xList, "=" ), "+" ).

      if opsys = "unix" then
        do:
          cscCmd = "$DLC/bin/promon  &1 < $PROTOP/etc/promon.csc_&2 > /dev/null 2>&&1".
          dbaCmd = "$DLC/bin/_proutil &1 -C dbanalys -Bp 10 > $PROTOP/dbanalys/&2.dba 2>&&1 &&".
        end.
       else
        do:
          cscCmd = "%DLC%~\bin~\promon  &1 < %PROTOP%~\etc~\promon.csc_&2 > nul 2>&&1".
          dbaCmd = "%DLC%~\bin~\_proutil &1 -C dbanalys -Bp 10 > %PROTOP%~\dbanalys~\&2.dba 2>&&1 &&".
       /* dbaCmd = "%DLC%~\bin~\_proutil &1 -C dbanalys -Bp 10 > %PROTOP%~\dbanalys~\&2.dba 2>&&1".    */
       /* dbaCmd = "%PROTOP%~\bin~\dbanalys.bat &1 %PROTOP%~\dbanalys~\&2.dba". */
       /* dbaCmd = "start /b %DLC%~\bin~\_proutil &1 -C dbanalys -Bp 10 > %PROTOP%~\dbanalys~\&2.dba 2>&&1". */
       /* dbaCmd = "start /b %PROTOP%~\bin~\dbanalys.bat &1 %PROTOP%~\dbanalys~\&2.dba". */
       /* dbaCmd = "%PROTOP%~\bin~\dbanalys.bat &1 %PROTOP%~\dbanalys~\&2.dba". */
        end.

      case entry( 1, entry( 2, xList, "=" ), "+" ):

        /* these impact all client sessions
         */

        when "csc_on"   then os-command silent value( substitute( cscCmd, pdbname( 1 ), "on"  )).
        when "csc_off"  then os-command silent value( substitute( cscCmd, pdbname( 1 ), "off" )).

        when "dba"      then
          do:
            if opsys = "unix" then
              os-command silent value( substitute( dbaCmd, pdbname( 1 ), pt_shortname )).
             else
              os-command silent value( substitute( dbaCmd, pdbname( 1 ), pt_shortname )).
              /* os-command no-wait value( substitute( dbaCmd, pdbname( 1 ), pt_shortname )). */
              /* run spawn ( substitute( dbaCmd, pdbname( 1 ), pt_shortname ), "", output winPID ). */

          end.

        /* these are specific to a client session
         */

        when "raw_Interval"   then rawMode = 5.
        when "raw_Cumulative" then rawMode = 4.
        when "raw_Raw"        then rawMode = 3.

        when "time_Summary"   then timeMode = 1.
        when "time_Rate"      then timeMode = 2.

        when "rowLimit" then rowLimit = integer( entry( 2, entry( 2, xList, "=" ), "+" )).
        when "sendType" then
          do:
            if lookup( argList, "XML,JSON" ) > 0 then
              sendType = argList.
          end.

      end.

      assign
        tt_client.sType  = sendType
        tt_client.rMode  = rawMode
        tt_client.tMode  = timeMode
        tt_client.rLimit = rowLimit
      .

      result = "".

      return.

    end.
   else
    do:

      /* get a dcList that is only dcNames -- pull any args (specified by ":") out of the command string
       */

      do i = 1 to num-entries( xList ):

        assign
          dcName = entry( i, xList )
          argVal = ""
        .

        if num-entries( dcName, ":" ) > 1 then
          assign
            argVal  = entry( 2, dcName, ":" )
            dcName  = entry( 1, dcName, ":" )
          .

        assign
          argList = argList + ( if dcList > "" then ":" else "" ) + argVal		/* dcList is deliberate...		*/
          dcList  = dcList +  ( if dcList > "" then "," else "" ) + dcName
        .

      end.

      find first tt_dataCollector where tt_dataCollector.clientId = UID no-error.
      if not available( tt_dataCollector ) then clientLaunch( UID ).

      for each tt_dataCollector where tt_dataCollector.clientId = UID:
        tt_dataCollector.xActive = ( dcList = "" or dcList = ? or dcList = "all" ).
      end.

      if num-entries( dcList ) >= 1 and dcList <> "all" then
        for each tt_dataCollector where tt_dataCollector.clientId = UID:
          tt_dataCollector.xActive = ( lookup( tt_dataCollector.dcName, dcList ) > 0 ).
        end.

      for each tt_dataCollector no-lock where tt_dataCollector.clientId = UID and tt_dataCollector.xActive = true:

        /* oldNow = mtime. */
        if num-entries( dcList ) > 0 then
          do:
            argVal = entry( lookup( tt_dataCollector.dcName, dcList ), argList, ":" ).
            if valid-handle( dcHandle ) then run "mon-update" in dcHandle ( argVal ).
          end.

      /*  message tt_dataCollector.dcName ( mtime - oldNow ). */

      end.

    end.

  /* oldNow = mtime. */

/* +++db
  find _userio no-lock where _userio-id = _myconn-userid + 1.
  assign
    my_dbAccess = _userio-dbAccess - my_dbAccess
    my_dbRead   = _userio-dbRead   - my_dbRead
  .
 */

  publish "updPTStats" ( my_dbAccess, my_dbRead ).

  {ssg/sausage08.i}

  return.

end.


procedure getRequest:

  define output parameter cmd as character no-undo.

&IF DEFINED( OE10 ) &THEN
  assign
    ok = hDataset:set-buffers( "" )
  no-error.
&ELSE
  empty temp-table tt_dataset.
&ENDIF

  cmd = "".

  get_req: do while true
    on error  undo, retry get_req
    on endkey undo, retry get_req:

    if not retry then
      i = 0.
     else	/* maybe we should accumulate incomplete cmd strings? */
      do:
        i = i + 1.
        message {&NOW} i "retrying get_req: interrupted cmd read".
        if cmd > "" then
          do:
            message {&NOW} i "cmd interrupted:" cmd.
            leave get_req.
          end.
        if i >= 5 then leave get_req.
      end.

    /***
    if serverType = "http" then
      do:
        input stream inStrm from value( fifo_in ) no-echo.
        import stream inStrm unformatted cmd.
        input stream inStrm close.
      end.
     else if serverType = "stomp" then
      do:
        message {&NOW} "waiting on q" qName "for stomp message".
        wait-for "U1" of this-procedure.
        message {&NOW} "stomp message received".
        cmd = string( result ).				/* "result" is populated by newStompMessage()	*/
      end.
     ***/

    leave get_req.

  end. /* get_req */

  return.

end.


function add2ds returns logical ( input h as handle ):

&IF DEFINED( OE10 ) &THEN
  hDataset:add-buffer( h ).
&ELSE
  create tt_dataset.
  ttHandle = h.
&ENDIF

  return true.

end.


function getTempTableHandle returns handle ( input n as character ):

&IF DEFINED( OE10 ) &THEN
  define variable i as integer no-undo.
  define variable b as handle  no-undo.
  
  do i = 1 to hDataset:num-buffers:
    b = hDataset:get-buffer-handle( i ).
    if b:name = n then return b.
  end.
&ELSE
  for each tt_dataset:
    if ttHandle:name = n then return ttHandle.   
  end.
&ENDIF

  return ?.

end.


/*** main body
 ***
 ***/

/* plain old protop users might not have a custid and that's ok
 *
 */

file-info:file-name = "etc/custid.cfg".
if file-info:full-pathname = ? then
  custId = "".
 else
  do on error undo, leave
     on endkey undo, leave:
    input stream inStrm from value( file-info:full-pathname ).
    import stream inStrm unformatted custId.
    input stream inStrm close.
  end.

if os-getenv( "SENDTYPE" ) <> ? and lookup( os-getenv( "SENDTYPE" ), "JSON,XML" ) > 0 then sendType = os-getenv( "SENDTYPE" ).

session:add-super-procedure( this-procedure ).

return.
