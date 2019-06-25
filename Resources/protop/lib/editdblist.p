/* lib/editdblist.p
 *
 * allow the user to review the dbList and optionally remove entries from it
 *
 */

{lib/v9.i}
{lib/tt_dblist.i}

define input-output parameter table for tt_dbList.

define new global shared variable logLevel as integer no-undo initial 5.

define query    q for tt_dbList.

define variable n as integer no-undo.
define variable z as handle  no-undo.

define variable infoText1 as character no-undo format "x(60)".
define variable infoText2 as character no-undo format "x(60)".

infoText1 = '  Use the spacebar to toggle the "keep" field.'. 
infoText2 = substitute( "  Press &1 to save and exit, &2 to quit without saving.", kblabel( "GO" ), kblabel( "END-ERROR" )).

define browse b
  query q
  display
    tt_dbList.xValid format "yes/no" label "Keep?"
    tt_dbList.friendlyName	/* x(20) */
    tt_dbList.dbPath		/* x(60) */
/*  tt_dbList.monitorDB	     */ /* x(4)	 */
   with
    /* no-box */
    /* width 132 */
    /* 20 down */
    title " Discovered Databases "
    size 82 by 20
    no-scrollbar-vertical
.

form b with frame dbList width 82 row 16 centered no-box.		/* need "width" or else:  **BROWSE b will not fit in FRAME dbList in PROGRAM /home/protop/lib/editdblist.p. (4028) */


/* toggle a row
 */

on " " of b in frame dbList do:
  publish "logMsg" ( 0, substitute( "&1: toggling &2", "lib/editdblist", tt_dbList.friendlyName )).
  tt_dbList.xvalid = not tt_dbList.xvalid.
  z:refresh().
  return no-apply.
end.


/* /* enable monitoring -- sample code
 *  */
 * 
 * on "m" of b in frame dbList do:
 *   assign
 *     tt_dbList.monitorDB   = not( tt_dbList.monitorDB )
 *   .
 *   z:refresh().
 *   return no-apply.
 * end.
 * 
 * 
 * /* delete a row
 *  */
 * 
 * on "delete-character", "delete-char", "delete-line", "delete", "ctrl-d", "d" of b in frame dbList do:
 *   /* delete tt_dbList. */
 *   publish "logMsg" ( 0, substitute( "&1: removing &2", "lib/editdblist", tt_dbList.friendlyName )).
 *   tt_dbList.xvalid = no.	/* just set a flag so that we can un-delete... */
 *   z:delete-current-row().
 *   return no-apply.
 * end.
 * 
 * 
 * /* un-delete deleted rows
 *  */
 * 
 * on "u" of b in frame dbList do:
 *   publish "logMsg" ( 0, substitute( "&1: undeleting", "lib/editdblist" )).
 *   for each tt_dbList:
 *     tt_dbList.xvalid = yes.
 *   end.
 *   close query q.
 *   open query q for each tt_dbList where tt_dbList.xvalid = yes.
 *   z:refresh().
 *   return no-apply.
 * end.
 * 
 */


/* exit
 */

on "s", "x", "go", "ctrl-x", "pf1", "f1" of b in frame dbList do:
  apply "close" to this-procedure.
  return no-apply.
end.


/* quit -- save nothing, delete tt_dbList
 */

on "q", "end-error", "ctrl-e", "pf4", "f4" of b in frame dbList do:

  define variable ok as logical no-undo.

  /* message "quit:" key-label( lastkey ) key-function( lastkey ). */

  ok = no.

  message
      skip(1)
      "Do you really want to quit without keeping any discovered databases?"
      skip(1)
    view-as alert-box question buttons yes-no-cancel
    title " Quit? "
    update ok
  .

  if ok then
    do:
      for each tt_dbList:
        delete tt_dbList.
      end.
      publish "logMsg" ( 0, substitute( "&1: quit, no save", "lib/editdblist" )).
      apply "close" to this-procedure.
    end.

  return no-apply.

end.


/* main block
 */

for each tt_dbList:
  tt_dbList.xvalid = yes.
  n = n + 1.
end.

publish "logMsg" ( 0, substitute( "&1: &2", "lib/editdblist", n )).

if n = 0 then return.

display
  skip(1)
  infoText1 skip
  infoText2 skip
  skip(1)
 with
  frame info
  no-box
  no-labels
  row 36
  centered
  width 82
.

z = browse b:handle.

open query q for each tt_dbList where tt_dbList.xvalid = yes.

/* z:multiple = yes. */

enable b with frame dbList.

/* z:select-all(). */
z:refresh().

apply "entry" to b in frame dbList.

do on error undo, leave on endkey undo, leave on stop undo, leave:

  wait-for "close" of this-procedure.

end.

publish "logMsg" ( 0, substitute( "&1: cleanup", "lib/editdblist" )).

for each tt_dbList where tt_dbList.xvalid = no:
  delete tt_dbList.
end.

hide frame dbList no-pause.
hide frame info   no-pause.

return.
