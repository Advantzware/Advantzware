/* ptsetvar.p
 *
 * prompt the user for various settings that do not require a db connection
 *
 */

{lib/protop.i}

session:add-super-procedure( this-procedure ).

subscribe to "protop_getMonInt"      anywhere run-procedure "getMonInt".
subscribe to "protop_getMailInfo"    anywhere run-procedure "getMailInfo".
subscribe to "protop_getRawMode"     anywhere run-procedure "getRawMode".
subscribe to "protop_getRowLimit"    anywhere run-procedure "getRowLimit".
subscribe to "protop_getSendType"    anywhere run-procedure "getSendType".
subscribe to "protop_getDbgMode"     anywhere run-procedure "getDbgMode".

return.


procedure getMonInt:

  define input-output parameter monInt as integer no-undo label "  monInt" format ">>>9".

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      '  The time, in seconds, between auto-refreshes when in "auto" mode.' skip
      skip(1)
      monInt skip
      skip(1)
      '  Use the "S" command to toggle Auto and OnDemand sampling mode.' skip
      skip(1)
     with
      frame getVar
      title " Monitoring Interval "
      row 3
      centered
      width 80
      side-labels
      overlay
    .

  end.

  hide frame getVar.

  return.

end.


procedure getMailInfo:

  define input-output parameter mailTo      as character no-undo label "  Send To" format "x(80)".
  define input-output parameter replyTo     as character no-undo label " Reply To" format "x(80)".
  define input-output parameter mailSubject as character no-undo label "  Subject" format "x(80)".
  define input-output parameter mailBody    as character no-undo label "     Note" format "x(80)".
  define input-output parameter mailAttach  as character no-undo label "   Attach" format "x(80)".

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      mailTo      skip(1)
      replyTo     skip(1)
      mailSubject skip(1)
      skip(1)
      mailBody    view-as editor size 80 by 10
      skip(1)
      "           (ProTtop screen output will be automatically appended to this note.)" skip
      skip(1)
      "           <F1> or ^X to Send,  <F4> or ^E to Abort" skip
      skip(1)
     with
      frame getVar
      title " ProTop e-Mail Message "
      row 3
      centered
      width 102
      side-labels
      overlay
    .

    run ptSendMail ( mailTo, replyTo, mailSubject, mailBody, mailAttach ).

  end.

  hide frame getVar.

  return.

end.


procedure getRawMode:

  define input-output parameter xRawMode  as integer no-undo.
  define input-output parameter xTimeMode as integer no-undo.

  define variable sampleData as character no-undo format "x(12)"
    view-as combo-box list-item-pairs
      "Raw",        "Raw",
      "Cumulative", "Cumulative",
      "Interval",   "Interval"
   .

  define variable summaryRate as character no-undo format "x(12)"
    view-as combo-box list-item-pairs
      "Summary",     "Summary",
      "Rate",        "Rate"
   .

  assign
    sampleData  = entry( xRawMode,  "Initial,Previous,Raw,Cumulative,Interval" )
    summaryRate = entry( xTimeMode, "Summary,Rate" )
  no-error.

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      '  Normal behavior is for ProTop to automatically display the RATE at which'         skip
      '  metrics are changing during the sample INTERVAL.  (Where that makes sense.)'      skip
      '  Thus "Interval" and "Rate" are the defaults below.'                               skip
      skip(1)
      '  Sometimes it is useful to view the raw numbers.  In those cases it might be'      skip
      '  helpful to try some of the other modes.  You may also want to switch to'          skip
      '  "OnDemand" sampling (the "S" command).'                                           skip
      skip(1)
      '  The default sampling is to return the data gathered in the interval between'      skip
      '  samples.  Alternatives are data since ProTop started ("Cumulative") or since'     skip
      '  the db started ("Raw").'                                                          skip
      skip(1)
      '      Select type of sampling:' sampleData skip
      skip(1)
      '  The default divisor is the number of seconds in the sample interval.  When'       skip
      '  done this way the *rate* (per second) is displayed.  If the "summary" option'     skip
      '  is selected the numbers are whatever they are -- no division is performed.'       skip
      skip(1)
      '      Select type of division:' summaryRate skip
      skip(1)
     with
      frame getVar
      title " Sample Type "
      row 3
      centered
      width 100
      no-labels
      overlay
    .


  end.

  hide frame getVar.

  xRawMode = lookup( sampleData, "Initial,Previous,Raw,Cumulative,Interval" ).
  if xRawMode < 3 or xRawMode > 5 or xRawMode = ? then
    do:
      /* message "oops!" xRawMode sampleData. pause. */
      xRawMode = 5.
    end.

  xTimeMode = lookup( summaryRate, "Summary,Rate" ).
  if xTimeMode < 0 or xTimeMode > 1 or xTimeMode = ? then
    do:
      /* message "oops!" xRawMode sampleData. pause. */
      xTimeMode = 2.
    end.

  return.

end.


procedure getRowLimit:

  define input-output parameter rowLimit as integer no-undo label "    Row Limit" format "->>>>>9".

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      '  The limit on the number of rows returned by large data sets (uio, table activity, etc.).' skip
      skip(1)
      rowLimit skip
      skip(1)
      '  This only limits the rows returned to the user-interface.  Data collectors still' skip
      '  have to process the same amount of data.  A potential downside to limiting rows'  skip
      '  is that the limit is enforced by the primary index -- so if you are sorting by'   skip
      '  non-default columns (the "z" command) you might not see everything you expect.'   skip
      skip(1)
     with
      frame getVar
      title " Row Limit "
      row 3
      centered
      width 100
      side-labels
      overlay
    .

  end.

  hide frame getVar.

  return.

end.


procedure getSendType:

  define input-output parameter sendType as character no-undo.

  define variable serializeAs as logical format "JSON/XML" label "    sendType".

  serializeAs = ( if sendType = "JSON" then yes else no ).

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      '  Serialize data as either XML or JSON for transfer from data collectors to user-interface.' skip
      skip(1)
      serializeAs " J = JSON, X = XML " skip
      skip(1)
      '  JSON is much more compact and thus faster but support is newer and /might/ have some'      skip
      '  (unknown) glitches.  You must be on 10.2B or better to use JSON.'                          skip
      skip(1)
     with
      frame getVar
      title " Serialize Data As "
      row 3
      centered
      width 100
      side-labels
      overlay
    .

  end.

  hide frame getVar.

  sendType = ( if serializeAs then "JSON" else "XML" ).

  return.

end.


procedure getDbgMode:

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      '  A debug level of 1 is the default, 3 is a moderate level of detail and includes  '  skip
      '  occasional PAUSEs.  5 can be very annoying and includes a lot of messages that   '  skip
      '  flash by quite rapidly.  Values greater than 5 are not likely to be very useful. '  skip
      skip(1)
      '  Debug Level = ' dbgMode format "9" skip
      skip(1)
     with
      frame getVar
      title " Debug Level "
      row 3
      centered
      width 100
      side-labels
      overlay
    .

  end.

  hide frame getVar.

  return.

end.

