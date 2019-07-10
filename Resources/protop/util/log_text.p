/* log_text.p
 *
 * rule set for generic text logs
 *
 * i.e.
 *  001 17 begins   trax       alert			# starting at position 17 if the line begins "trax"
 *  002  0 index    "audit "   alert			# starting at position 0 if the line contains "audit "
 *  003  0 contains " 2*rhsmd" alert			# starting at position 0 if the line matches "* 2*rhsmd*"
 *
 */

{lib/protop.i}

/* standard handlers
 *
 */

subscribe to "text_rules"    anywhere run-procedure "chkRuleCfg".	/* load the rules related to this log type		*/
subscribe to "text_line"     anywhere run-procedure "parse_line".	/* parse the raw log line				*/

/* the rule set
 *
 */

define temp-table tt_ruleCfg
  field ruleId     as character						/* unique id						*/
  field ruleOffset as character						/* skip N chars into line				*/
  field ruleOper   as character						/* matching operation					*/
  field ruleText   as character format "x(30)"				/* target text to compare				*/
  field ruleAction as character						/* publish this!					*/
  field nagFreq    as character						/* how often should we nag?				*/
  index ruleId-idx is primary unique ruleId
.

define variable ruleCfgFile as character no-undo			/* name of the config file holding the rules		*/
  initial "etc/log_text.cfg"
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

      for each tt_ruleCfg where tt_ruleCfg.ruleAction = "ignore" or tt_ruleCfg.ruleAction = "":
        delete tt_ruleCfg.
      end.

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

  define variable m as character no-undo.				/* message being examined				*/
  define variable x as logical   no-undo.				/* pass or fail?					*/

  if dbgMode >= 9 then message now lgType lgName xLine.

  rule_loop: for each tt_ruleCfg:

    assign
      x = no
      m = substring( xLine, max( 1, integer( tt_ruleCfg.ruleOffset )))
    .

    case tt_ruleCfg.ruleOper:

      when "begins" then
        do:
          x = ( m begins tt_ruleCfg.ruleText ).
          if dbgMode >= 8 then message now lgType lgName "begins" tt_ruleCfg.ruleText x skip xLine skip m.
        end.

      when "ends" then
        do:
          x = ends( trim( m ), tt_ruleCfg.ruleText ).
          if dbgMode >= 8 then message now lgType lgName "ends" tt_ruleCfg.ruleText x skip xLine skip m.
        end.

      when "index" then
        do:
          x = ( index( m, tt_ruleCfg.ruleText ) > 0 ).
          if dbgMode >= 8 then message now lgType lgName "index" tt_ruleCfg.ruleText x skip xLine skip m.
        end.

      when "contains" then
        do:
          x = ( m matches ( "*" + tt_ruleCfg.ruleText + "*" )).
          if dbgMode >= 8 then message now lgType lgName "contains" tt_ruleCfg.ruleText x skip xLine skip m.
        end.

    end.

    if x = yes and chkNag( tt_ruleCfg.ruleId, integer( tt_ruleCfg.nagFreq )) = yes then
      do:

	run doAlert ( tt_ruleCfg.ruleAction, tt_ruleCfg.ruleId, lgName, input-output xStatus, xLine, tt_ruleCfg.ruleOper, tt_ruleCfg.ruleText ).

        /* leave rule_loop. */	/* first one wins? or process them all? */

      end.

  end.

  return.

end.


procedure doAlert:

  define input        parameter rAction as character no-undo.
  define input        parameter rid     as character no-undo.
  define input        parameter lgName  as character no-undo.
  define input-output parameter xStatus as character no-undo.
  define input        parameter xLine   as character no-undo.
  define input        parameter xOper   as character no-undo.
  define input        parameter xText   as character no-undo.

  define variable msgTxt as character no-undo.

  if dbgMode >= 4 then message now rAction + ":" lgName xOper xText xLine.

  msgTxt = substitute(  'log text &1 "&2"~n&3', xOper, xText, xLine ).

  msgTxt = msgTxt + "~n" + substitute( alertTempl, now, pt_server, lgName ).

  publish rAction ( rid, msgTxt ).

  return.

end.
