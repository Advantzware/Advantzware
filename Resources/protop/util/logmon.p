/* logmon.p
 *
 * mpro -p util/logmon.p
 *
 */

{lib/protop.i}

/* dbgMode = 1  minimal
 * dbgMode = 3  errors
 * dbgMode = 4  + success
 * dbgMode = 5  verbose details (usually overkill)
 */

dbgMode = 4.

define new global shared variable custId as character no-undo.

define variable monName      as character   no-undo.
define variable tick         as decimal     no-undo initial 0.5.	/* clock tick - 1/2 sec rounds up to 1 prior to OE11    */
define variable monint       as integer     no-undo initial 5.		/* monitor refresh interval                             */

define variable logFileName  as character   no-undo.
define variable flgFileName  as character   no-undo.
define variable dbgFileName  as character   no-undo.

/* generic config file definition
 */

define temp-table tt_config no-undo

  field cfgId   as character						/* standard first field, must be unique			*/

  /* user defined config fields
   *
   * if these are *all* character fields then comments in the config file
   * will not throw specious "Invalid character..." errors -- if you need
   * other data types just be aware that some of the parse errors may be
   * false alarms
   */

  field logType as character						/* text, dblog, appsrv...				*/
  field logTemplate as character format "x(60)"				/* the full path name of the target log file		*/
  field strtPad as character						/* how far to look backwards at startup?		*/

  field logName as character						/* the actual log name found with os-dir		*/

  index cfgId-idx is primary unique cfgId				/* standard primary, unique index			*/

  /* user defined config indexes (if any)
   */

  index logTemplate-idx is unique logTemplate					/* logTemplate must be unique				*/

.

/********************************************************************************************************************************/

/* this is the business end of things... all code specific to this monitor should go in here
 *
 */

run util/log_oedb.p persistent.						/* log handler for openedge db .lg files		*/
run util/log_ubrk.p persistent.						/* log handler for ubroker managed logs			*/
run util/log_text.p persistent.						/* log handler for generic text logs			*/


/*** super-procedure? ***/

define temp-table tt_logInfo no-undo
  field logName   as character						/* the log file being monitored				*/
  field logOffset as int64						/* offset that we have read up to			*/
  field strtPad   as integer						/* how far to look backwards at startup?		*/
  index logName-idx is primary unique logName
.

procedure scanLog:

  define input parameter lgName as character no-undo.
  define input parameter lgType as character no-undo.

  define variable eof      as {&BIGINT} no-undo.
  define variable offset   as {&BIGINT} no-undo.
  define variable lgLine   as character no-undo.
  define variable xStatus  as character no-undo.

  define variable i as integer no-undo.

  find first tt_logInfo where tt_logInfo.logName = lgName no-error.
  if not available tt_logInfo then
    do:
      find tt_config where tt_config.logTemplate = lgName no-error.
      create tt_logInfo.
      assign
        tt_logInfo.logOffset = -1					/* if set to -1 we are in initialization mode		*/
        tt_logInfo.logName   = lgName
        tt_logInfo.strtPad   = ( if available tt_config then integer( tt_config.strtPad ) else 0 )
      .
    end.

  file-info:file-name = lgName.
  if file-info:full-pathname = ? then
    do:
      message now substitute( "scanLog: &1 is missing!", lgName ).
      return.
    end.

  if (( index( file-info:file-type, "F" ) < 1 ) or ( index( file-info:file-type, "R" ) < 1 )) then
    do:
      message now substitute( "scanLog: &1 is not readable a log file: &2", lgName, file-info:file-type ).
      return.
    end.

  /* make sure that the rule set for this type of log is up to date
   */

  publish lgType + "_rules".

  /* ready to scan!
   */

  if dbgMode >= 7 then message now substitute( "Scanning &1:", lgName ).

  input stream inStrm from value( lgName ).

  offset = tt_logInfo.logOffset.
  seek stream inStrm to end.
  eof = seek( inStrm ).

  /* when we first start, backup from eof -- otherwise we may
   * not have a complete line of text to work with.  it can
   * also be helpful to potentially review messages that might
   * have appeared just prior to starting
   */

  if offset < 0 then							/* will be -1 on initial startup			*/
    offset = max( 0, eof - max( 2048, tt_logInfo.strtPad )).		/* if possible backup at least 2048 bytes on init	*/

  if offset > eof then							/* lg file must have been truncated...			*/
    do:
      offset = 0.
      message now substitute( "scanLog: &1 seems to have been truncated", lgName ).
    end.

  if offset < eof then							/* catch up!						*/
    do:

      seek stream inStrm to offset.						/* reposition to last read offset			*/

      i = 0.
      read_lg: do while true:						/* read log lines					*/

        lgLine = "".

        do on error  undo, leave read_lg on endkey undo, leave read_lg:
          import stream inStrm unformatted lgLine.
        end.

        i = i + 1.
        if i modulo 100000 = 0 then message now substitute( "Scanning &1.lg line: &2", lgName, i ).
        offset = seek( inStrm ).

        /* if this is the initial reading of the log file then
         * catch up to the present without looking at everything
         */

        if tt_logInfo.logOffset = -1					/* -1 means that startup is happening			*/
          and offset <= ( eof - tt_logInfo.strtPad ) then next read_lg.

        if i >= 10000 then leave read_lg.                               /* read no more than 10,000 lines per scanLog() call    */

        /* parse the log file line
         *
         */

        publish lgType + "_line" ( lgLine, lgType, lgName, input-output xStatus ).

      end.

      tt_logInfo.logOffset = offset.

    end.

  input stream inStrm close.

  if dbgMode >= 6 then message now substitute( "Scanned &1 lines in &2", i, lgName ).

  return.

end.

/*** end super-procedure candidate ***/



function chkLogTemplate returns character ( template as character ):

  define variable fileName     as character no-undo.
  define variable logPath      as character no-undo.
  define variable xName        as character no-undo format "x(60)".

  if index( template, "*" ) = 0 then
    return template.

  logPath  = substring( template, 1, r-index( template, "/" ) - 1 ).

  input stream inStrm from os-dir( logPath ) no-attr-list.
  repeat:
    import stream inStrm ^ xName.
    if xName matches template then
      fileName = max( xName, fileName ).
  end.
  input stream inStrm close.

  return fileName.

end.


procedure monitor:

  define variable oldName     as character no-undo.
  define variable logName     as character no-undo.
  define variable logBaseName as character no-undo.

  define variable i as integer no-undo.

  run chkConfig.

  for each tt_config:
    i = i + 1.
  end.

  if dbgMode >= 6 then message now substitute( "monitor() &1 config records found", i ).

  oldName = pt_shortName.

  for each tt_config:

    logName = chkLogTemplate( tt_config.logTemplate ).
    logBaseName = substring( logName, r-index( logName, "/" ) + 1 ).

    pt_shortName = logBaseName.
    pt_shortName = "logmon".

    if dbgMode >= 5 then
      message now substitute( "monitoring &1 &2 &3", tt_config.cfgId, tt_config.logType, tt_config.logTemplate ).

    run scanLog( logName, tt_config.logType ).

  end.

  pt_shortName = oldName.

  return.

end.


/********************************************************************************************************************************/

/* most code below is standard and should not need modification
 */

define variable cfgFile as character no-undo.
define variable cfgDate as date      no-undo.
define variable cfgTime as integer   no-undo.

/* chkConfig
 *
 * reads a generic etc/monName.cfg file into tt_config as defined above
 *   - will notice if the file time-stamp changes and re-read the config
 *   - eliminates blank lines and treats lines starting with "#" as comments
 *   - only requires the first field to be a unique char id field called "cfgId"
 *
 */

define variable firstCfgCheck as logical initial yes.

procedure chkConfig:

  define variable i as integer no-undo.

  if cfgFile = "" or cfgFile = ? then
    do:
      if dbgMode >= 3 then message now "looking for:" substitute( "&1/&2.&3.&4", "etc", monName, pt_shortname, "cfg" ).
      file-info:file-name = substitute( "&1/&2.&3.&4", "etc", monName, pt_shortname, "cfg" ).
      if file-info:full-pathname = ? then
        do:
          if dbgMode >= 3 then message now "looking for:" substitute( "&1/&2.&3", "etc", monName, "cfg" ).
          file-info:file-name = substitute( "&1/&2.&3", "etc", monName, "cfg" ).
        end.
      if file-info:full-pathname <> ? then
        do:
          cfgFile = file-info:full-pathname.
          if dbgMode >= 3 then message now "using:" file-info:full-pathname.
          firstCfgCheck = yes.
        end.
       else
        do:
          if firstCfgCheck then
            do:
              if dbgMode >= 3 then message now "no config file found, using internal default settings".
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

  if cfgDate <> file-info:file-mod-date or cfgTime <> file-info:file-mod-time or file-info:full-pathname = ? then
    do:

      assign
        cfgDate = file-info:file-mod-date
        cfgTime = file-info:file-mod-time
      .

      empty temp-table tt_config.

      if file-info:full-pathname = ? then
        do:
          message now "No config file:" cfgFile.
          return.
        end.

      if dbgMode >= 3 then
        message now "Parsing config file:" cfgFile.

      input stream  inStrm from value( file-info:full-pathname ).
      load_cfg: repeat:

        do on error undo, leave load_cfg:

          create tt_config.
          tt_config.cfgId = "".

          import stream inStrm
            tt_config.cfgId
            tt_config.logType
            tt_config.logTemplate
            tt_config.strtPad
          no-error.

        if tt_config.cfgId = "" or tt_config.cfgId begins "#" then
          do:
            delete tt_config.
            next load_cfg.
          end.

          if error-status:num-messages > 0 then
            do:
              do i = 1 to error-status:num-messages:
                message now error-status:get-message(i).
              end.
              delete tt_config.
              next load_cfg.
            end.

        end.

        tt_config.logName = "".					/*  this field is not valid when read from a .cfg file		*/

      end.
      input stream inStrm close.

      for each tt_config where tt_config.cfgId = "" or tt_config.cfgId begins "#":
        delete tt_config.
      end.

      message now "Config file," cfgFile "contents:".
      for each tt_config:
        display tt_config with width 132.
      end.

    end.

  return.

end.


/* main body
 *
 */

define variable lastRefresh  as datetime-tz no-undo.			/* when was the last refresh?                           */

define variable XID          as character   no-undo.			/* eXtended unique ID					*/

run lib/protoplib.p persistent.						/* load protop infrastructure library                   */
run lib/protop-cfg.p persistent.					/* initialize protop environment                        */

run getMonName ( input-output monName ).

file-info:file-name = pt_tmpdir.					/* make certain that we have a temp directory!          */
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.					/* make certain that we have a log directory!           */
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_logdir ).

run ssg/sausage02.p persistent.                                         /* sausage lib                                          */
run ssg/sausage04.p persistent.                                         /* sausage lib                                          */

/*** unlike most monitors "logmon" does not have a db connection...	/* XID is therefore blank!				*/
 ***
run lib/vstlib.p persistent.						/* db related infrastructure				*/
run dbGUID ( pt_shortname, output XID ).				/* every db needs a unique id                           */
 ***
 ***/

run custId( output custId ).

if XID <> "" then XID = "." + XID.					/* just in case						*/

assign
  logFileName = substitute( "&1/&2&3.&4", pt_logdir, monName, XID, "log" )
  flgFileName = substitute( "&1/&2&3.&4", pt_tmpdir, monName, XID, "flg" )
  dbgFileName = substitute( "&1/&2&3.&4", pt_tmpdir, monName, XID, "dbg" )
.

{ssg/sausage05.i}							/* alert.cfg & alert library                            */

run lib/mailx.p persistent.

run mkFlag ( flgFileName ).						/* create a flag file without a db connection		*/

output to value( logFileName ) unbuffered append.			/* if a 2nd copy starts, try to make it obvious		*/

lastRefresh = now.
message now substitute( "Starting &1...", monName ).

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

if dbgMode >= 5 then
  do:
    output to value( logFileName ) unbuffered append.
    message now "==Quit==".
    output close.
  end.

quit.
