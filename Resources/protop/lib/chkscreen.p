/* lib/chkscreen.p
 *
 */

define variable w as integer no-undo.

w = current-window:width-chars.

/* the last solely 80x24 terminal was manufactured in the stone-age -- every
 * reasonable device and emulator is capable of larger screens -- even the
 * ssh client on my phone can handle 132x48 (actually only 128 x 43 -- which
 * is the limit that is *actually* enforced)
 *
 * update - 160 colums is now the minimum as of 3.3q (May 2017). that is the
 * equivalent of 4 old style 80x24 terminals
 *
 * 48 rows is still the minimum height although 72 or even 96 are much better.
 *
 */

if /* true or */ screen-lines > 200 and opsys begins "WIN" then
  do:

/* view-as alert-box will wrap text according to screen size -- this gives an idea of
 * how many lines that might result in on an 80 column screen
 *
 *      '         1         2         3         4         5         6         7         8'
 *      '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
 */

    message
    /*  '  Your window size is 201 rows.  That probably means that your shortcut~'s   '		*/
        '  Your window size is' screen-lines 'rows.  That probably means that your shortcut~'s   '
        '  "layout" properties are not properly set.                                 '
        skip(1)
        '  Right-click the "OE" Icon in the upper-left corner of the window, select  '
        '  "Properties" from the drop-down menu and then select the "Layout" tab.    '
        '  The "width" must be at least 160 and the "height" must be at least 48 in  '
        '  *both* the "Screen Buffer Size" and the "Window Size" sections.           '
        skip(1)
        '  The default Screen Buffer Height of 300 is known to be too large.  The    '
        '  actual limit is unknown but ProTop is known to work with 96 lines and     '
        '  probably works with 128 but I do not have a screen that large right now   '
        '  (hardware donations are happily accepted!)                                '
      view-as alert-box
    .

    quit.

  end.

if /* true or */ w < 160 or screen-lines < 40 then	/* 'screen-lines' is 3 less than window size due to the message & status lines  */
  do:

/* view-as alert-box will wrap text according to screen size -- this gives an idea of
 * how many lines that might result in on an 80 column screen
 *
 *      '         1         2         3         4         5         6         7         8'
 *      '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
 *
 *        _____        _______            ____   ____  _          
 *       |  __ \      |__   __|          |___ \ |___ \| |         
 *       | |__) | __ ___ | | ___  _ __     __) |  __) | | ____  __
 *       |  ___/ '__/ _ \| |/ _ \| '_ \   |__ <  |__ <| |/ /\ \/ /
 *       | |   | | | (_) | | (_) | |_) |  ___) | ___) |   <  >  < 
 *       |_|   |_|  \___/|_|\___/| .__/  |____(_)____/|_|\_\/_/\_\
 *                               | |                              
 *                               |_|                              
 */

    message
    /*  ' ProTop thinks that your screen is 80 columns by 24 lines. Is it 1978?    ' */
        ' Is it 1978? ProTop thinks that your screen is' w 'columns by' screen-lines + 3 'lines.    '
        skip(1)
        ' This window size is inadequate to be used with ProTop. You must have a   '
        ' window that has at least 160 columns and 48 lines.                       '
        skip(1)

&IF opsys begins "unix" &THEN
        ' Use "stty -a" to check your columns & rows settings.  You may have       '
        ' dynamically resized your window to be too small.                         '
        skip(1)
        ' The supplied etc/protermcap.protop will dynamically handle XTERM screens '
        ' on Linux, HP-UX and Solaris.                                             '
        skip(1)
        ' AIX requires pre-defined sizes. The default xterm on AIX is 160x48,      '
        ' xterm60 is 160x60, xterm72 is 160x72, xterm84 is 160x84 and xterm96 is   '
        ' 160x96.  Or you can supply your own protermcap entires if you prefer.    '

&ENDIF

&IF opsys begins "win" &THEN
        ' Right-click the "OE" Icon in the upper-left corner of the window, select '
        ' "Properties" from the drop-down menu and then select the "Layout" tab.   '
        ' The "width" must be at least 160 and the "height" must be at least 48 in '
        ' *both* the "Screen Buffer Size" and the "Window Size" sections.          '
        skip(1)
        ' The default Screen Buffer Height of 300 is known to be too large.  The   '
        ' actual limit is unknown but ProTop is known to work with 96 lines and    '
        ' probably works with 128 but I do not have a screen that large right now  '
        ' (hardware donations are happily accepted!)                               '
&ENDIF

        skip(1)
        ' Progress does not recognize screen size changes after a session is       '
        ' started so you must resize *before* starting ProTop.                     '
      view-as alert-box
    .

    quit.

  end.

return.
