/* ------------------------------------------------ sys/inc/period.i  7/94 RM */
/* Include File Used To Assign Transaction Dates/GL Periods to GL Transactions*/
/* -------------------------------------------------------------------------- */

define buffer xperiod for period.
def var v-answer as log.
assign choice  = yes
       uperiod = period.

if "{1}" eq "" then udate = today.

form
   skip(1) "   Transaction Date:" udate space(3) "Period:" uperiod space(3)
   skip(1)
with frame uperiod row 5 no-labels centered overlay no-box
     color value(col-warn) prompt value(col-input).
form
   skip (1) " Is the date you entered correct ?:" v-answer skip (1)
with frame verify-date row 10 width 45 no-labels centered overlay no-box
   color value(col-warn) prompt value(col-input).

form skip(1)
 "  The Period for" udate "is closed. Reopen this Period? " choice "  " skip(1)
with frame closed-period row 10 no-labels no-box centered overlay
     color value(col-error) prompt value(col-input).

form skip(1)
"  Multiple Periods after" udate "are closed. Reopen all Periods? " choice "  "
skip(1)
with frame closed-periods row 10 no-labels no-box centered overlay
     color value(col-error) prompt value(col-input).

blok:
repeat on error undo with frame uperiod:
   update
   udate
   editing:
      readkey.
      if keyfunction(lastkey) = "end-error" then leave blok.
      else apply lastkey.
   end.

   find first period where period.company  =  cocode  and
                               period.pst  <= udate   and
                               period.pend >= udate no-lock no-error.
   if available period and period.pstat
   then do:
      uperiod  = period.pnum.
      display uperiod.
   end.
   else do:
      if not available period
      then do:
         repeat:
            display
            skip(1)
      "  No defined Period exists for" udate space(0) ".  " skip
            skip(1)
            with frame no-period-avail row 10 no-labels no-box
                 centered overlay color value(col-warn).
            pause.
            leave.
         end.
         hide frame no-period-avail no-pause.
         undo blok, retry blok.
      end.
      if available period and not period.pstat then
      do:
         repeat:
            display
            skip(1)
     "  The Period for" udate "is closed. Can't Post to Closed Period. " skip
            skip(1)
            with frame no-post-avail row 10 no-labels no-box
                 centered overlay color value(col-warn).
            pause.
            leave.
         end.
         hide frame no-post-avail no-pause.
         undo blok, retry blok.
      end.
/* Removed open periods
      do with frame closed-period:
         find first xperiod where xperiod.company = period.company and
                                  xperiod.pend    > period.pend    and
                                  not xperiod.pstat no-lock no-error.
         if not available xperiod then do:
            display udate with frame closed-period.
            update choice.
            if choice then
            for each period where period.pend >= udate and
                                  period.company =  cocode:
               period.pstat = true.
            end.
            else do:
                 hide frame closed-period no-pause.
                 undo blok, retry blok.
            end.
            hide frame closed-period   no-pause.
         end.
         else do with frame closed-periods:
            view frame closed-periods.
            display udate.
            update choice.
            if choice then
            for each period where period.pend >= udate and
                                  period.company =  cocode:
               period.pstat = true.
            end.
            else do:
                 hide frame closed-periods no-pause.
                 undo blok, retry blok.
            end.
            hide frame closed-periods   no-pause.
         end.
      end. /*do with frame */
*/
   end.

   /* verify the date entered is correct */
   update v-answer with frame verify-date
   editing:
      readkey.
      if keyfunction(lastkey) = "end-error" then leave blok.
      else apply lastkey.
   end.
   hide frame verify-date no-pause.
   if not v-answer then do:
      undo blok, retry blok.
   end.

/*   hide frame uperiod no-pause. */
   leave.
end.
if keyfunction(lastkey) = "end-error" or not choice then leave.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
