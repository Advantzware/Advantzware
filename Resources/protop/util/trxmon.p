/* trxmon.p
 *
 * mpro -p util/trxmon.p -param "friendlyName"
 *
 */

{lib/protop.i}

/* dbgMode = 1  minimal
 * dbgMode = 3  errors only
 * dbgMode = 4  + success
 * dbgMode = 5  verbose details (usually overkill)
 */

dbgMode = 3.

define variable monName      as character   no-undo.
define variable tick         as decimal     no-undo initial 5.0.	/* clock tick - 1/2 sec rounds up to 1 prior to OE11    */
define variable monint       as integer     no-undo initial 300.	/* monitor refresh interval                             */
									/* 10 is useful for testing 300 is typical for PROD	*/

define variable logFileName  as character   no-undo.
define variable flgFileName  as character   no-undo.
define variable dbgFileName  as character   no-undo.

/* config file definition
 */

define temp-table tt_config no-undo
  field cfgId    as character format "x(20)"				/* standard first field, must be unique			*/
  field cfgValue as character format "x(60)"				/* the value						*/
  index cfgId-idx is primary unique cfgId				/* standard primary, unique index			*/
.

/********************************************************************************************************************************/
/* this is the business end of things...
 *
 * all code specific to this monitor should go in here
 *
 */

define variable emailList    as character no-undo.
define variable stuckList    as character no-undo.
define variable mailxBody    as {&LNGCR}  no-undo.
define variable inLine       as character no-undo.
define variable trxThreshold as integer   no-undo initial 3600.		/* how old is an "old" transaction?			*/
define variable trxZapAfter  as integer   no-undo initial 1800.		/* how long must it be idle before being zapped?	*/
define variable trxZapBatch  as integer   no-undo initial  900.		/* how often do we actually zap things?			*/

define variable vlogdir      as character no-undo.
define variable exListDate   as date      no-undo.
define variable exListTime   as integer   no-undo.

define temp-table tt_userExclude no-undo
  field exName as character
  index exName-idx is primary exName
.

define temp-table tt_trx no-undo

  field xid       as integer   format ">>>>>>>>>9"    label "Id"
  field xValid    as logical
  field zap       as logical
  field trxNum    as integer   format ">>>>>>>>>9"    label "TRX#"
  field userEx    as character format "x(6)"          label "Exempt"
  field userNum   as integer   format ">>>>9"         label "Usr#"
  field userName  as character format "x(15)"         label "Name"
  field userPID   as character format "x(8)"          label "PID"
  field userFlags as character format "x(5)"          label "Flags"
  field userDev   as character format "x(16)"         label "Device"
  field trx-rl    as integer   format ">>>>>>>>9"     label "BI Clstr"
  field trx-st    as character format "x(4)"          label "Stat"
  field xtime     as character format "x(13)"         label "Start"
  field duraStr   as character format "x(12)"         label "    Duration"
  field trx-wait  as character format "x(34)"         label "Wait Resource (dbkey)"
  field duration  as integer   format ">>>>>9"        label " Len"

  field dbAccess  as int64      format ">>>>>>>>>>>9" label "db Access"
  field lastAct   as datetime                         label "Last Active"

  index xid-idx is unique xid
  index duraStr-idx is primary duraStr descending
  index zap-idx zap

.

define stream zapMail.


procedure checkExList:

  file-info:file-name = "etc/exclude.trx".
  if file-info:full-pathname = ? then
    do:
      empty temp-table tt_userExclude.
      return.
    end.

  if exListDate <> file-info:file-mod-date or exListTime <> file-info:file-mod-time then
    do:

      empty temp-table tt_userExclude.

      input stream inStrm from value( file-info:full-pathname ).
      repeat:
        create tt_userExclude.
        import stream inStrm tt_userExclude.
      end.
      input stream inStrm close.

      for each tt_userExclude:
        exName = trim( exName ).
        if exName = "" then delete tt_userExclude.
      end.

      assign
        exListDate = file-info:file-mod-date
        exListTime = file-info:file-mod-time
      .

      /*
      for each tt_userExclude:
        display tt_userExclude.
      end.
      pause.
       */

    end.

  return.

end.


procedure updTRXlist:

  for each tt_TRX:
    xvalid = no.
  end.

  /* Get transaction data from _Trans VST
   *
   */

  for each dictdb._Trans no-lock
     where _Trans-usrNum <> ?
       and _Trans-state <> "allocated":

    /* and _Trans-duration <> ?        */
    /* by _Trans-duration descending:  */

    find tt_TRX where tt_TRX.xid = _Trans-id no-error.
    if not available tt_TRX then create tt_trx.

    assign
      tt_trx.xid      = _Trans-id
      tt_trx.xValid   = yes
      tt_trx.xtime    = ( if _Trans-txtime <> ? then substring( _Trans-txtime, 12 ) else "" )
      tt_trx.userNum  = _Trans-usrnum
      tt_trx.trxNum   = _Trans-num
      tt_trx.trx-rl   = _Trans-counter
      tt_trx.trx-st   = _Trans-state
      tt_trx.duration = ( if _Trans-duration <> ? then _Trans-duration else 0 )
      tt_trx.duraStr  = string( integer( truncate( tt_trx.duration / 86400, 0 )), ">>>" ) + " " + string( tt_trx.duration, "hh:mm:ss" )
    .

    case tt_trx.trx-st:
      when "ALLOCATED"  then tt_trx.trx-st = "ALLO".
      when "ACTIVE"     then tt_trx.trx-st = "ACTV".
      when "DEAD"       then tt_trx.trx-st = "DEAD".
      when "COMMITTING" then tt_trx.trx-st = "COMM".
      when "PREPARING"  then tt_trx.trx-st = "PRPG".
      when "PREPARED"   then tt_trx.trx-st = "PRPD".
    end.

    /* get some info about the session that started the trx
     *
     */

    /* About usr#
     *
     * PROMON = usr#
     * .lg file = usr#
     * _myconnection.myconn-userid = usr#
     * _connection._connect-usr = usr#
     * _connection._connection-id = usr# + 1
     *
     * _connection-id is a *much* more efficient way to access _connect -- thus we jump through hoops (add one to the usr#)
     *
     */

    find dictdb._Connect no-lock where _Connect-id = _Trans-usrnum + 1 no-error.

    if available _Connect then
      assign
        tt_trx.userFlags = connectFlags( _Connect-Id )
        tt_trx.userName  = connectName( _Connect-Id, tt_trx.userFlags )
        tt_trx.userPID   = string( _connect-PID )
        tt_trx.userDev   = _connect-Device
        tt_trx.trx-wait  = ( if available _Connect then ( string( _Connect-wait ) + " " + string( _Connect-wait1 )) else "" )
      .

    find _userio where _userio-id = _connect-id.			/* has there been a db activity since we last looked?	*/

    if tt_TRX.dbAccess <> _userio-dbAccess then				/* if it is different then update the last activity 	*/
      assign								/* time stamp						*/
        tt_trx.dbAccess = _userio-dbAccess
        tt_trx.lastAct  = now
      .

    find tt_userExclude where exName = tt_trx.userName no-error.	/* is this an exempt user?				*/
      tt_trx.userEx = ( if available( tt_userExclude ) then "*" else "" ).

  end.

  for each tt_TRX where xvalid = no:					/* remove transactions that are no longer "on the radar" */
    delete tt_TRX.
  end.

  return.

end.


/* "monitor" is called by the main body
 *
 */

define variable zapTimer as integer no-undo.

procedure monitor:

  define variable i as integer no-undo.

  define variable scratchFile as character no-undo.
  define variable subjectLine as character no-undo.

  if dbgMode >= 3 then message now substitute( "&1...", monName ).

  run chkConfig.
  run checkExList.
  run updTRXlist.

  zapTimer = zapTimer + monInt.

  /* check for zapping candidates -- active transactions older than the threshold
   */

  i = 0.
  for each tt_trx where tt_trx.duration >= trxThreshold break by tt_trx.duration descending:

    /* is the last activity older than the zapAfter limit?
     */

    if ( abs( interval( tt_trx.lastAct, now, "seconds" )) < trxZapAfter ) then
      tt_trx.zap = no.
     else								/* a candidate has been found!				*/
      do:

        assign
          i = i + 1
          tt_trx.zap = yes
        .

        /* log the details of sessions about to be zapped
         */

        message now "disconnect:"
          tt_trx.userNum
          tt_trx.userEx
          tt_trx.userName
          tt_trx.userPID
          tt_trx.userFlags
          tt_trx.userDev
          tt_trx.trx-st
          tt_trx.duraStr
          tt_trx.dbAccess
          abs( interval( tt_trx.lastAct, now, "seconds" ))
        .

      end.

  end.

  /* if there is at least one session that needs to be disconnected...
   */

  if i >= 1 then
    do:

      if zapTimer < trxZapBatch then return.

      /* send out an email to interested parties
       */

      assign
        subjectLine = substitute( "Disconnecting &1 very old transaction&2", i, ( if i > 1 then "s" else "" ))
        scratchFile = pt_tmpdir + "/disconnect.tmp"
      .

      if opsys begins "win" then scratchFile = replace( scratchFile, "/", "~\" ).

      os-command value( substitute( 'echo "" > &1', scratchFile )).

      output stream zapMail close.
      output stream zapMail to value( scratchFile ) append unbuffered.

      put stream zapMail unformatted now " " subjectLine ":" skip.
      put stream zapMail skip(1).

      output stream zapMail close.

      for each tt_trx where tt_trx.zap = yes by tt_trx.duration descending:

        output stream zapMail to value( scratchFile ) append.

        put stream zapMail unformatted " Usr# Name            PID      Flags Device           Stat     Duration       Idle" skip.
        put stream zapMail unformatted "----- --------------- -------- ----- ---------------- ---- ------------ ----------" skip.

        put stream zapMail
            tt_trx.userNum   format ">>>>9" space(1)
            tt_trx.userName  format "x(15)" space(1)
            tt_trx.userPID   format "x(8)"  space(1)
            tt_trx.userFlags format "x(5)"  space(1)
            tt_trx.userDev   format "x(16)" space(1)
            tt_trx.trx-st    format "x(4)"  space(1)
            tt_trx.duraStr   format "x(12)" space(1)
            abs( interval( tt_trx.lastAct, now, "seconds" )) format ">>>>>>>>>9"
            ( if tt_trx.userEx = "*" then " EXEMPT" else "" )
          skip
        .

        output stream zapMail close.

        if tt_trx.userEx = "*" then next.						/* skip over Excluded users	*/

        pause 0.5.

        if tt_trx.userFlags begins "S" then
          os-command value( substitute( "bin/disconnect &1 &2 &3", pdbname(1), tt_trx.userNum, tt_trx.userPID )).
         else
          os-command value( substitute( "bin/disconnect &1 &2", pdbname(1), tt_trx.userNum )).

        /* check for _connect-disconnect = 1
         */

        find _Connect no-lock where _Connect-Id = tt_trx.userNum + 1 no-error.

        if available _Connect and _Connect-Disconnect = 1 then
          do:
            pause 5.	/* perhaps it will become unstuck... */
            find _Connect no-lock where _Connect-Id = tt_trx.userNum + 1 no-error.
          end.

	/* the session is still connected but "stuck" so try harder to disconnect them
         */

        if available _Connect and _Connect-Disconnect = 1 then
          do:

            /* stuck users get a special email
             */

            output stream zapMail to value( pt_tmpdir + "/disconnectx.tmp" ) append.

            put stream zapMail unformatted " Usr# Name            PID      Flags Device           Stat     Duration       Idle" skip.
            put stream zapMail unformatted "----- --------------- -------- ----- ---------------- ---- ------------ ----------" skip.

            put stream zapMail
                tt_trx.userNum   format ">>>>9" space(1)
                tt_trx.userName  format "x(15)" space(1)
                tt_trx.userPID   format "x(8)"  space(1)
                tt_trx.userFlags format "x(5)"  space(1)
                tt_trx.userDev   format "x(16)" space(1)
                tt_trx.trx-st    format "x(4)"  space(1)
                tt_trx.duraStr   format "x(12)" space(1)
                abs( interval( tt_trx.lastAct, now, "seconds" )) format ">>>>>>>>>9"
              skip
            .

            put stream zapMail unformatted skip(2).
            put stream zapMail unformatted "disconnect flag is set!" skip.
            put stream zapMail unformatted skip(2).

            output stream zapMail close.

            if tt_trx.userFlags begins "S" then
              os-command value( substitute( "bin/disconnectx &1 &2 &3", pdbname(1), tt_trx.userNum, tt_trx.userPID )).
             else
              os-command value( substitute( "bin/disconnectx &1 &2", pdbname(1), tt_trx.userNum )).

            /* stuck users get a special email -- regardless of NOTRXRAG
             */

            if stuckList <> "" and stuckList <> ? then
              do:

                /* os-command value( substitute( 'cat &3 | mailx -s "&4 &1" &2', pdbname(1), stuckList,  pt_tmpdir + "/disconnectx.tmp", "Stuck disconnection" )). */

                mailxBody = "".
                file-info:file-name = pt_tmpdir + "/disconnectx.tmp".
                if file-info:full-pathname <> ? then
                  do:
                    mailxBody = mailxBody + substitute( "=== &1 ===", file-info:full-pathname ) + chr(10) + chr(10).
                    input stream inStrm from value( file-info:full-pathname ).
                    repeat:
                      inLine = "".
                      import stream inStrm unformatted inLine.
                      mailxBody = mailxBody + chr(10) + inLine.
                    end.
                    mailxBody = mailxBody + chr(10).
                    input stream inStrm close.
                  end.

                run ptSendMail ( stuckList, "", substitute( "Stuck disconnection &1", pdbname(1)), mailxBody, "" ).

              end.

          end.

        output stream zapMail to value( scratchFile ) append.

        put stream zapMail unformatted skip(2).
        put stream zapMail unformatted fill( "=", 60 ) skip.
        put stream zapMail unformatted skip(2).

        output stream zapMail close.

      end.

      output stream zapMail close.

      os-append
        value( scratchFile )
        value( substitute( "&1/disconnect.&2.&3", vlogdir, string( month( today ), "99" ), string( day( today ), "99" )))
      .

      if emailList <> "" and emailList <> ? then
        do:

          /* os-command value( substitute( 'cat &3 | mailx -s "&4 &1" &2', pdbname(1), emailList, scratchFile, subjectLine )). */

          mailxBody = "".
          file-info:file-name = scratchFile.
          if file-info:full-pathname <> ? then
            do:
              mailxBody = mailxBody + substitute( "=== &1 ===", file-info:full-pathname ) + chr(10) + chr(10).
              input stream inStrm from value( file-info:full-pathname ).
              repeat:
                inLine = "".
                import stream inStrm unformatted inLine.
                mailxBody = mailxBody + chr(10) + inLine.
              end.
              mailxBody = mailxBody + chr(10).
              input stream inStrm close.
            end.

          run ptSendMail ( emailList, "", substitute( "&1 &2", subjectLine, pdbname(1)), mailxBody, "" ).

        end.

      /* if we have actually zapped something then
       * the timer needs to be reset
       */

      zapTimer = 0.

    end.

  return.

end.

/********************************************************************************************************************************/

define variable cfgFile as character no-undo.
define variable cfgDate as date      no-undo.
define variable cfgTime as integer   no-undo.

/* chkConfig
 *
 * reads a generic etc/monName.cfg file into tt_config as defined above
 *   - will notice if the file time-stamp changes and re-read the config
 *   - eliminates blank lines and treates lines starting with "#" as comments
 *   - only requires the first field to be a unique char id field called "cfgId"
 *
 */

define variable firstCfgCheck as logical initial yes.

procedure chkConfig:

  define variable i as integer no-undo.

  if cfgFile = "" or cfgFile = ? then
    do:

      if dbgMode > 3 then message now "looking for:" substitute( "&1/&2.&3.&4", "etc", monName, pt_shortname, "cfg" ).
      file-info:file-name = substitute( "&1/&2.&3.&4", "etc", monName, pt_shortname, "cfg" ).
      if file-info:full-pathname = ? then
        do:
          if dbgMode > 3 then message now "looking for:" substitute( "&1/&2.&3", "etc", monName, "cfg" ).
          file-info:file-name = substitute( "&1/&2.&3", "etc", monName, "cfg" ).
        end.

      if file-info:full-pathname <> ? then
        do:
          cfgFile = file-info:full-pathname.
          if dbgMode > 3 then message now "using:" file-info:full-pathname.
          firstCfgCheck = yes.
        end.
       else
        do:
          if firstCfgCheck then
            do:
              if dbgMode > 3 then message now "no config file found, using internal default settings".
              firstCfgCheck = no.
            end.
          return.
        end.
    end.

  file-info:file-name = cfgFile.				/* no cfgFile was found or it has disappeared while running	*/
  if file-info:full-pathname = ? then
    do:
      cfgFile = "".
      return.
    end.

  if cfgDate <> file-info:file-mod-date or cfgTime <> file-info:file-mod-time then
    do:

      assign
        cfgDate = file-info:file-mod-date
        cfgTime = file-info:file-mod-time
      .

      empty temp-table tt_config.

      if file-info:full-pathname = ? then
        do:
          if dbgMode >= 3 then message now "No config file:" cfgFile.
          return.
        end.

      if dbgMode >= 3 then
        message now "Parsing config file:" cfgFile.

      input stream inStrm from value( file-info:full-pathname ).
      load_cfg: repeat:

        do on error undo, leave load_cfg:

          create tt_config.
          tt_config.cfgId = "".

          import stream inStrm tt_config no-error.

          if error-status:num-messages > 0 then
            do:
              do i = 1 to error-status:num-messages:
                if dbgMode >= 0 then message now error-status:get-message(i).
              end.
              delete tt_config.
              next load_cfg.
            end.

        end.

        if tt_config.cfgId = "" or tt_config.cfgId begins "#" then
          delete tt_config.

      end.
      input stream inStrm close.

      for each tt_config where tt_config.cfgId = "" or tt_config.cfgId begins "#":
        delete tt_config.
      end.

      /*
       * message now "Config file," cfgFile "contents:".
       * for each tt_config:
       *   display tt_config with width 132.
       * end.
       */

      for each tt_config:

        if dbgMode >= 0 then message now substitute( "&1 = [&2]", tt_config.cfgId, tt_config.cfgValue ).

        case tt_config.cfgId:
          when "monInt"       then monInt       = integer( tt_config.cfgValue ).
          when "trxThreshold" then trxThreshold = integer( tt_config.cfgValue ).
          when "trxZapAfter"  then trxZapAfter  = integer( tt_config.cfgValue ).
          when "trxZapBatch"  then trxZapBatch  = integer( tt_config.cfgValue ).
          when "emailList"    then emailList    = tt_config.cfgValue.
          when "stuckList"    then stuckList    = tt_config.cfgValue.
        end.

      end.

    end.

  return.

end.

/* main body
 *
 */

define variable lastRefresh  as datetime-tz no-undo.			/* when was the last refresh?                           */

if num-entries( session:parameter, "|" ) >= 1 then pt_shortname = entry( 1, session:parameter, "|" ).

run lib/protoplib.p  persistent.					/* load protop infrastructure library                   */
run lib/vstlib.p     persistent.					/* db related infrastructure				*/
run lib/protop-cfg.p persistent.					/* initialize protop environment                        */
run lib/mailx.p      persistent.					/* handle mail						*/

run getMonName ( input-output monName ).

file-info:file-name = pt_tmpdir.					/* make certain that we have a temp directory!          */
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.					/* make certain that we have a log directory!           */
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_logdir ).

message now "Initializing" monName pt_shortname "pdbname:" pdbname(1).

assign
  logFileName = substitute( "&1/&2.&3.&4", pt_logdir, monName, pt_shortname, "log" )
  flgFileName = substitute( "&1/&2.&3.&4", pt_tmpdir, monName, pt_shortname, "flg" )
  dbgFileName = substitute( "&1/&2.&3.&4", pt_tmpdir, monName, pt_shortname, "dbg" )
.

vlogdir = ( if pt_vstmon <> "" then pt_vstmon else pt_logdir ).

run mkFlag ( flgFileName ).

output to value( logFileName ) unbuffered append.			/* if a 2nd copy starts, try to make it obvious		*/

lastRefresh = add-interval( now, monInt * -2, "seconds" ).		/* don't wait on the first iteration			*/

message now "Starting" monName pt_shortname "pdbname:" pdbname(1).

/* the monitoring loop
 *
 */

do while true:

  pause tick no-message.						/* check flag and debug level while waiting		*/

  if lastkey > 0 and lastkey <> 32 then leave.				/* useful if we are running interactively		*/

  file-info:file-name = flgFileName.					/* are we being politely asked to stop?			*/
  if file-info:full-pathname = ? then					/* (if the flag disappears we are bing asked to stop)	*/
    do:
      message now flgFileName "has disappeared.".
      message now substitute( "Gracefully shutting down &1.", monName ).
      leave.
    end.

  run chkDbgMode ( dbgFileName, input-output dbgMode ).			/* has the requested debug level changed?		*/

  if abs( interval( now, lastRefresh, "seconds" )) < monint then	/* if the monitoring interval hasn't been reached yet	*/
    next.								/* then loop						*/

  lastRefresh = now.							/* the last time the monitored data was refreshed	*/

  run monitor.

end.

/* just in case...
 */

file-info:file-name = flgFileName.
if file-info:full-pathname <> ? then os-delete value( file-info:full-pathname ).

output close.

if dbgMode >= 3 then
  do:
    output to value( logFileName ) unbuffered append.
    message now "==Quit==".
    output close.
  end.

quit.
