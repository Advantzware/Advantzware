/* log_oedb.p
 *
 * rule set for oedb
 *
 */

{lib/protop.i}

/* standard handlers
 *
 */

subscribe to "oedb_rules"    anywhere run-procedure "chkRuleCfg".	/* load the rules related to this log type		*/
subscribe to "oedb_line"     anywhere run-procedure "parse_line".	/* parse the raw log line				*/

/* handlers specific to the log type (procedures to be define below)
 *
 */

subscribe to "oedb_Login"    anywhere run-procedure "oedb_Login".
subscribe to "oedb_Logout"   anywhere run-procedure "oedb_Logout".
subscribe to "oedb_Unknown"  anywhere run-procedure "oedb_Unknowmn".
subscribe to "oedb_ignore"   anywhere run-procedure "oedb_Ignore".

/* the rule set
 *
 */

define temp-table tt_ruleCfg
  field ruleId     as character						/* oe message number					*/
  field ruleAction as character						/* publish this!					*/
  field nagFreq    as character						/* how often should we nag?				*/
  index ruleId-idx is primary unique ruleId
.

define variable ruleCfgFile as character no-undo			/* name of the config file holding the rules		*/
  initial "etc/log_oedb.cfg"
.

define variable ruleCfgDate as date      no-undo.			/* last date that the rules changed			*/
define variable ruleCfgTime as integer   no-undo.			/* last date that the rules changed			*/

define variable alertTempl as character no-undo
  initial "[&1]~n[server: &2]~n[&3]"
.

session:add-super-procedure( this-procedure ).

return.


/* does a string end with another string?
 *
 */

function ends returns logical ( input s as character, input t as character ):
  return ( r-index( s, t ) = (( length( s ) - length( t )) + 1 )).
end.


/* is it ok to act on this?  or have we nagged enough?
 *
 */

define temp-table tt_nagTracker
  field ruleId     as character
  field lastNag    as datetime
  index ruleId-idx is primary unique ruleId
.

function chkNag returns logical ( input rid as character, input n as integer ):

  if n <= 0  then return yes.						/* always report this event				*/

  find tt_nagTracker where tt_nagTracker.ruleId = rid no-error.
  if not available tt_nagTracker then
    do:
      create tt_nagTracker.
      assign
        tt_nagTracker.ruleId  = rid
        tt_nagTracker.lastNag = now
      .
      return yes.
    end.

  if interval( now, tt_nagTracker.lastNag, "seconds" ) < n then
    return no.
   else
    do:
      tt_nagTracker.lastNag = now.
      return yes.
    end.

end.


/* check the rule configuration, (re)load if necessary
 *
 * this will work with any rule configuration file that follows the standard pattern:
 *   - first column is ruleId, character, unique
 *   - as many additional fields as needed
 *   - # is for comments
 *   - will be reloaded on any change
 */

procedure chkRuleCfg:

  define variable i as integer no-undo.

  file-info:file-name = ruleCfgFile.

  if ruleCfgDate <> file-info:file-mod-date or ruleCfgTime <> file-info:file-mod-time or file-info:full-pathname = ? then
    do:

      assign
        ruleCfgDate = file-info:file-mod-date
        ruleCfgTime = file-info:file-mod-time
      .

      empty temp-table tt_ruleCfg.

      if file-info:full-pathname = ? then
        do:
          message now "No config file:" ruleCfgFile.
          return.
        end.

      if dbgMode >= 3 then
        message now "Parsing config file:" ruleCfgFile.

      input from value( file-info:full-pathname ).
      load_cfg: repeat:

        do on error undo, leave load_cfg:

          create tt_ruleCfg.
          tt_ruleCfg.ruleId = "".

          import tt_ruleCfg no-error.

          if error-status:num-messages > 0 then
            do:
              do i = 1 to error-status:num-messages:
                message now error-status:get-message(i).
              end.
              delete tt_ruleCfg.
              next load_cfg.
            end.

        end.

        if tt_ruleCfg.ruleId = "" or tt_ruleCfg.ruleId begins "#" then
          delete tt_ruleCfg.

      end.
      input close.

      for each tt_ruleCfg where tt_ruleCfg.ruleId = "" or tt_ruleCfg.ruleId begins "#":
        delete tt_ruleCfg.
      end.

/*
 *    for each tt_ruleCfg where tt_ruleCfg.ruleAction = "ignore" or tt_ruleCfg.ruleAction = "":
 *      delete tt_ruleCfg.
 *    end.
 */

      if dbgMode >= 3 then
        do:
          message now "Config file," ruleCfgFile "contents:".
          for each tt_ruleCfg:
            display tt_ruleCfg with width 132.
          end.
        end.

    end.

  return.

end.


/* handle log messages - taking advantage of the known structure of those messages
 *
 */

procedure parse_line:

  define input        parameter xLine   as character no-undo.
  define input        parameter lgType  as character no-undo.
  define input        parameter lgName  as character no-undo.
  define input-output parameter xStatus as character no-undo.

  define variable m as character no-undo.				/* message #						*/
  define variable w as character no-undo.				/* message text						*/
  define variable p as character no-undo.				/* process-id						*/
  define variable u as character no-undo.				/* progress usr#					*/

  if dbgMode >= 9 then message now lgType lgName xLine.

  /* valid oedb .lg file lines have a structure to them:
   *   - they start with a timestamp: 
   *   - position 30 is "]"
   *   - the message number is in a known position and format
   *   - the process-id is in a known position and format
   *   - the user number is in a "sort of" known position and format
   */

  if substring( xLine, 30, 1 ) <> "]" then				/* verify that this is a line from an oe10+ .lg file	*/
    return.
   else
    assign								/* find the message number -- i.e. "(334)"		*/
      w = entry( 2, xLine, "(" )
      m = entry( 1, w, ")" )						/* just the message number				*/
      w = trim( substring( xLine, index( xLine, "(" )))			/* "(334)   Multi-user session end." etc.		*/
      p = trim( substring( xLine, 34, 10 ))				/* process-id						*/
      u = trim( substring( xLine, 64, 15 ))				/* i.e. "ABL   764: (45"				*/
      u = trim( entry( 1, u, ":" ))					/* i.e." ABL   764"					*/
      u = trim( substring( u, r-index( u, " " )))			/* usr#							*/
    no-error.

  if dbgMode >= 8 then message now p u m w.				/* the payload...					*/

  find tt_ruleCfg where tt_ruleCfg.ruleId = m no-error.
  if not available tt_ruleCfg then
    publish "oedb_Unknown" ( p, u, m, w, input-output xStatus ).
   else
    do:
      if tt_ruleCfg.ruleAction = "ignore" or tt_ruleCfg.ruleAction = "" then
        .
       else
        if chkNag( tt_ruleCfg.ruleId, integer( tt_ruleCfg.nagFreq )) = yes then
          publish "oedb_" + tt_ruleCfg.ruleAction ( tt_ruleCfg.ruleId, lgName, input-output xStatus, p, u, m, w ).
    end.

  return.

end.


/* a tt to hold login/logout info
 *
 * this is a silly example to show how to store context so that the logic behind log events can 
 * have knowledge of previous events in order to build more complex analysis
 *
 */

define temp-table tt_login no-undo
  field usrNum as character
  field PID    as character
  field inTS   as datetime
  index usrNumPID-idx is primary unique usrNum PID
.


/* login event
 *
 * record the login time for this session
 *
 */

procedure oedb_Login:

  define input        parameter rid     as character no-undo.
  define input        parameter lgName  as character no-undo.
  define input-output parameter xStatus as character no-undo.
  define input        parameter p       as character no-undo.
  define input        parameter u       as character no-undo.
  define input        parameter m       as character no-undo.
  define input        parameter w       as character no-undo.

  if dbgMode >= 9 then message now "login:" p u m w.			/* the payload...					*/

  /* usr# and PID are both reused -- sometimes rapidly so it is
   * not safe to rely on only one of them as a unique key, the
   * combination  is reasonably safe though
   */

  find tt_login where tt_login.usrNum = u and tt_login.PID = p no-error.
  if not available tt_login then create tt_login.
  assign
    tt_login.usrNum = u
    tt_login.PID    = p
    tt_login.inTS   = now
  .

  return.

end.


/* logout event
 *
 */

procedure oedb_Logout:

  define input        parameter rid     as character no-undo.
  define input        parameter lgName  as character no-undo.
  define input-output parameter xStatus as character no-undo.
  define input        parameter p       as character no-undo.
  define input        parameter u       as character no-undo.
  define input        parameter m       as character no-undo.
  define input        parameter w       as character no-undo.

  define variable msgTxt as character no-undo.

  if dbgMode >= 9 then message now "logout:" p u m w.

  /* the login may have occurred before we started looking
   */

  find tt_login where tt_login.usrNum = u and tt_login.PID = p no-error.
  if not available tt_login then return.

  msgTxt = substitute(  "usr &1, pid &2, logged in for: &3", u, p, string( interval( now, tt_login.inTS, "seconds" ), "hh:mm:ss" )).

  msgTxt = msgTxt + "~n" + substitute( alertTempl, now, pt_server, lgName ).

  publish "alert" ( rid, msgTxt ).

  delete tt_login.

  return.

end.


/* unknown event
 *
 */

procedure oedb_Unknown:

  define input        parameter rid     as character no-undo.
  define input        parameter lgName  as character no-undo.
  define input-output parameter xStatus as character no-undo.
  define input        parameter p       as character no-undo.
  define input        parameter u       as character no-undo.
  define input        parameter m       as character no-undo.
  define input        parameter w       as character no-undo.

  if dbgMode >= 9 then message now "unknown:" p u m w.

  /* what do we want to do with unknown events?
   */

  return.

end.


procedure oedb_Ignore:

  define input        parameter rid     as character no-undo.
  define input        parameter lgName  as character no-undo.
  define input-output parameter xStatus as character no-undo.
  define input        parameter p       as character no-undo.
  define input        parameter u       as character no-undo.
  define input        parameter m       as character no-undo.
  define input        parameter w       as character no-undo.

  if dbgMode >= 9 then message now "ignoring:" p u m w.

  message now "This shouldn't be happening!  'ignoring:'" p u m w.	/* "ignore" is supposed to be suppressed		*/

  return.

end.
