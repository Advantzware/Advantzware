/* lib/ptsplash.p
 *
 */

define temp-table tt_motd no-undo
  field xid         as integer
  field xMessage    as character format "x(80)"
  index xid-idx is primary unique xid
.

define variable ii as integer no-undo.
define variable nn as integer no-undo.

define variable ptLogo  as character no-undo extent 8 format "x(66)".
define variable partner as character no-undo format "x(66)".

define stream inStrm.


procedure loadMOTD:

  define variable cfgName as character no-undo.
  define variable xLine   as character no-undo.

  define variable ii as integer no-undo.

  run findCfgName( "motd", input-output cfgName ).

  file-info:file-name = cfgname.
  if file-info:full-pathname <> ? then
    do:

      input from value( file-info:full-pathname ).

      repeat:

        xLine = "".
        import unformatted xLine.

        if xLine = "" or xLine begins "#" then next.

        create tt_motd.
        assign
          ii = ii + 1
          tt_motd.xid = ii
          tt_motd.xMessage = replace( xLine, "~~n", "~n" )
        .

      end.

      input close.
      if tt_motd.xMessage = "" then delete tt_motd.

    end.

  return.

end.


/* main block
 *
 */

run lib/chkscreen.p.

input stream inStrm from etc/ptlogo.txt.
do on endkey undo, leave:
  import stream inStrm unformatted ptLogo[1].
  import stream inStrm unformatted ptLogo[2].
  import stream inStrm unformatted ptLogo[3].
  import stream inStrm unformatted ptLogo[4].
  import stream inStrm unformatted ptLogo[5].
  import stream inStrm unformatted ptLogo[6].
  import stream inStrm unformatted ptLogo[7].
  import stream inStrm unformatted ptLogo[8].
end.
input stream inStrm close.

file-info:file-name = "etc/partner.cfg".
if file-info:full-pathname <> ? then
  do:
    input stream inStrm from value( file-info:full-pathname ).
    do on error undo, leave
       on endkey undo, leave:
      import stream inStrm unformatted partner.
    end.
    input stream inStrm close.
  end.

if opsys = "unix" then
  color display value( "blue" )
    ptLogo[1]
    ptLogo[2]
    ptLogo[3]
    ptLogo[4]
    ptLogo[5]
    ptLogo[6]
    ptLogo[7]
    ptLogo[8]
   with
    frame splash
  .

display
  skip(1)

  /* these banners are created with "figlet -f big"				*/

  ptLogo[1] dcolor 9 at 10 skip
  ptLogo[2] dcolor 9 at 10 skip
  ptLogo[3] dcolor 9 at 10 skip
  ptLogo[4] dcolor 9 at 10 skip
  ptLogo[5] dcolor 9 at 10 skip
  ptLogo[6] dcolor 9 at 10 skip
  ptLogo[7] dcolor 9 at 10 skip
  ptLogo[8] dcolor 9 at 10 skip

  skip(1)
  "         Brought to you by the OpenEdge experts at:                "    skip
  skip(1)
  "           White Star Software, LLC      http://wss.com            "    skip
  "          " partner                                                     skip
  skip(1)
  "                              Copyright 2003 -" year( today ) format "9999" "Tom Bascom" skip
 with
  frame splash
  row 3
  centered
  no-box
  no-labels
  overlay
.

run loadMOTD.

ii = 0.
for each tt_motd:
  ii = ii + 1.
end.

if os-getenv( "NAGME" ) = "no" then
  do:
    pause 10.
    hide frame splash.
    return.
  end.

motd: do while true:

  nn = time + 20.
  if nn > 86400 then leave motd.		/* skip if we happen to hit midnite	*/

  find first tt_motd where tt_motd.xid >= random( 1, ii ) no-error.
  if not available tt_motd then
    return.

  display
    skip(1)
    tt_motd.xMessage view-as editor size 80 by 20 dcolor 7
    skip(1)
   with
    frame nag
    row 20
    centered
    no-box
    no-labels
    overlay
    width 84
 .

  /* F4 or CTRL-E will dismiss the countdown and continue
   */

  motd_wait: do while time < nn
     on error undo, leave motd
     on endkey undo, leave motd:

    case ( nn - time ):
     when  1 then pause 1 message "ProTop will continue in 1 second".
     when  2 then pause 1 message "ProTop will continue in 2 seconds".
     when  3 then pause 1 message "ProTop will continue in 3 seconds".
     when  4 then pause 1 message "ProTop will continue in 4 seconds".
     when  5 then pause 1 message "ProTop will continue in 5 seconds".
     when  6 then pause 1 message "ProTop will continue in 6 seconds".
     when  7 then pause 1 message "ProTop will continue in 7 seconds".
     when  8 then pause 1 message "ProTop will continue in 8 seconds".
     when  9 then pause 1 message "ProTop will continue in 9 seconds".
     when 10 then pause 1 message "ProTop will continue in 10 seconds".
     when 11 then pause 1 message "ProTop will continue in 11 seconds".
     when 12 then pause 1 message "ProTop will continue in 12 seconds".
     when 13 then pause 1 message "ProTop will continue in 13 seconds".
     when 14 then pause 1 message "ProTop will continue in 14 seconds".
     when 15 then pause 1 message "ProTop will continue in 15 seconds".
     when 16 then pause 1 message "ProTop will continue in 16 seconds".
     when 17 then pause 1 message "ProTop will continue in 17 seconds".
     when 18 then pause 1 message "ProTop will continue in 18 seconds".
     when 19 then pause 1 message "ProTop will continue in 19 seconds".
     when 20 then pause 1 message "ProTop will continue in 20 seconds".
     when 21 then pause 1 message "ProTop will continue in 21 seconds".
     when 22 then pause 1 message "ProTop will continue in 22 seconds".
     when 23 then pause 1 message "ProTop will continue in 23 seconds".
     when 24 then pause 1 message "ProTop will continue in 24 seconds".
     when 25 then pause 1 message "ProTop will continue in 25 seconds".
     when 26 then pause 1 message "ProTop will continue in 26 seconds".
     when 27 then pause 1 message "ProTop will continue in 27 seconds".
     when 28 then pause 1 message "ProTop will continue in 28 seconds".
     when 29 then pause 1 message "ProTop will continue in 29 seconds".
     when 30 then pause 1 message "ProTop will continue in 30 seconds".
    end.

    if nn - time <= 0 then
      leave motd.

    if keyfunction( lastkey ) = "go" then
      next motd.

    if lastkey = -1 then
      next motd_wait.
     else
      leave motd.

  end.

end.

hide frame nag.

hide frame splash.

return.
