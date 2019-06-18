/* tasklib.p
 *
 *
 *	*     *     *   *    *        command to be executed
 *	-     -     -   -    -
 *	|     |     |   |    |
 *	|     |     |   |    +----- day of week (0 - 6) (Sunday=0) 	4gl weekday() function is based at 1
 *	|     |     |   +------- month (1 - 12)
 *	|     |     +--------- day of month (1 - 31)
 *	|     +----------- hour (0 - 23)
 *	+------------- min (0 - 59)
 *
 * this scheduler is a single thread - be careful not to launch blocking processes
 * as they will prevent dbmonitor from doing its job! if necessary use "&" at the
 * end of commands to put them in the background
 *
 * since tasks are launched from within the context of dbmonitor.p all of the	
 * protopenv variables and path settings are available to the command line
 *	
 * for example:
 *
 * 5,20,35,50 * * * * ${PROTOP}/bin/syncio.sh >> ${LOGDIR}/tasksched.log 2>&1
 *
 */

{lib/protop.i}

define temp-table tt_schedule no-undo
  field tx_min as character
  field tx_hr  as character
  field tx_dom as character
  field tx_mth as character
  field tx_dow as character
  field tx_cmd as character
.

define stream inStrm.

session:add-super-procedure( this-procedure ).

return.


/* check the config file for changes
 */

define variable taskCfgDate as date      no-undo.
define variable taskCfgTime as integer   no-undo.

procedure checkTaskConfig:

  define variable cfgName as character no-undo.

  define variable xLine   as character no-undo.   
  define variable inLine  as character no-undo extent 8.
  define variable cmdLine as character no-undo.

  define variable ii as integer no-undo.
  define variable xx as integer no-undo.

  run findCfgName( "schedule", input-output cfgName ).

  file-info:file-name = cfgName.
  if file-info:full-pathname = ? then
    do:
      if dbgMode >= 5 then message {&NOW} "no schedule was found".
      empty temp-table tt_schedule.
      return.
    end.
   else
    do:
      if dbgMode >= 4 then message {&NOW} "checking ts:" file-info:full-pathname.
    end.

  if file-info:file-mod-date = taskCfgDate and file-info:file-mod-time = taskCfgTime then
    do:
      if dbgMode >= 5 then message {&NOW} "schedule config ts:" file-info:file-mod-date "=" taskCfgDate file-info:file-mod-time "=" taskCfgTime.
      return.
    end.

  assign
    taskCfgDate = file-info:file-mod-date
    taskCfgTime = file-info:file-mod-time
  .

  if dbgMode >= 3 then message {&NOW} "Loading" file-info:full-pathname.

  empty temp-table tt_schedule.

  input stream inStrm from value( file-info:full-pathname ).

  repeat on endkey undo, leave:

   assign
      xLine   = "" 
      inLine  = "" 
      cmdLine = "" 
    .
    
    import stream inStrm unformatted xLine. 
    
    xLine = replace( xLine, chr(9), chr(32)).
    xLine = trim( xLine ). 
    
    do ii = 1 to 8:
      
      xx = index( xLine, " " ).
      if xx <= 0 then xx = length( xLine ).

      inLine[ii] = substring( xLine, 1, xx ) no-error.

      if inLine[ii] begins "#" or inLine[ii] = "" then
        leave.

      xLine = trim( substring( xLine, xx + 1 )) no-error.

      if ii >= 5 then
        do:
          cmdLine = xLine.
          leave.
        end.

    end.

    if ii < 5 or inLine[1] = "" or cmdLine = "" then next.	/* not a valid schedule line	*/

    create tt_schedule.
    assign
      tt_schedule.tx_min = inLine[1]
      tt_schedule.tx_hr  = inLine[2]
      tt_schedule.tx_dom = inLine[3]
      tt_schedule.tx_mth = inLine[4]
      tt_schedule.tx_dow = inLine[5]
      tt_schedule.tx_cmd = cmdLine
    .

  end.

  input stream inStrm close.

  if dbgMode >= 3 then
    do:
      ii = 0.
      for each tt_schedule:
        ii = ii + 1.
        message {&NOW} tx_min tx_hr tx_dom tx_mth tx_dow tx_cmd.
      end.
      if ii = 0 then
        message {&NOW} "no valid schedule lines were loaded.".
    end.

  return.

end.


function eligible returns logical ( currVal as integer, valList as character ):

  define variable n as integer no-undo.
  define variable i as integer no-undo.

  if valList = "*" then
    return yes.

  n = num-entries( valList ).

  do i = 1 to n:
    if integer( entry( i, valList )) = currVal then
      return yes.
  end.

  return no.

end.

/* check to see what tasks need to run now
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 10.0 &THEN

procedure scheduledTasks:
  return.
end.

&ELSE

define variable lastDT as datetime no-undo.

procedure scheduledTasks:

  define variable workDT as datetime no-undo.

  define variable workMin as integer no-undo.
  define variable workHr  as integer no-undo.
  define variable workDOM as integer no-undo.
  define variable workMth as integer no-undo.
  define variable workDow as integer no-undo.

  define variable nn      as integer no-undo.
  define variable xx      as integer no-undo.

  define variable xMin    as logical no-undo.
  define variable xHr     as logical no-undo.
  define variable xDOM    as logical no-undo.
  define variable xMth    as logical no-undo.
  define variable xDow    as logical no-undo.

  define variable cmd     as character no-undo.

  if dbgMode >= 5 then message {&NOW} "checking scheduled tasks".

  run checkTaskConfig.

  /* advance the clock 1 minute at a time in order to make sure that we do not
   * miss anything due to other dbmonitor tasks taking a long time
   */

  if lastDT = ? then lastDT = {&NOW}.

  if dbgMode >= 5 then message {&NOW} "lastDT:" lastDT.

  workDT = add-interval( lastDT, 1, "minute" ).

  do while workDT < now:

    if dbgMode >= 5 then message {&NOW} "working date:" workDT.

    lastDT = workDT.		/* this is the last time we actually might have done anything */

    assign
      workMin = integer( substring( string( workDT ), 15, 2 ))
      workHr  = integer( substring( string( workDt ), 12, 2 ))
      workDOM = day( workDT )
      workMth = month( workDT )
      workDOW = weekday( workDT ) - 1	/*  Progress weekday() starts at 1 rather than 0, schedule.cfg starts at 0 */
    .

    if dbgMode >= 5 then message {&NOW} "work parameters:" workMin workHr workDOM workMth workDOW.

    /* is there anything that we should be doing?
     */

    for each tt_schedule:

      if dbgMode >= 5 then message {&NOW} "tt_schedule:" tx_min tx_hr tx_dom tx_mth tx_dow tx_cmd.

      assign
        xDOW = no
        xMth = no
        xDOM = no
        xHr  = no
        xMin = no
      .

      /* is this an eligible moment of some sort?
       */

      assign
        xHr  = eligible( workhr,  tx_hr  )
        xMin = eligible( workMin, tx_min )
        xDOW = eligible( workDOW, tx_dow )
        xMth = eligible( workMth, tx_mth )
        xDOM = eligible( workDOM, tx_dom )
      .

      if dbgMode >= 4 then message {&NOW} "eligible?" xMin xHr xDOM xMth xDOW tx_cmd.

      if xHr = yes and xMin = yes and xDOM = yes and xMth = yes and xDOW = yes then
        do:

          /**** there is something interesting for us to do! ****/

          if dbgMode >= 1 then message {&NOW} "eligible task:" tx_min tx_hr tx_dom tx_mth tx_dow tx_cmd.

          cmd = tx_cmd.
          nn = index( cmd, "[NOALERT]" ). 
          if nn = 0 then
            publish "info" ( "runTask", substitute( "runTask: &1", cmd )).
           else
            cmd = substring( cmd, 1, nn - 1 ).

          if opsys = "unix" then
            os-command silent value( cmd ).
           else if opsys begins "win" then
            run spawn( cmd, "", output xx ).

        end.

    end.

    /* "tick"
     */

    workDT = add-interval( lastDT, 1, "minute" ).

  end.

  return.

end.

&ENDIF
