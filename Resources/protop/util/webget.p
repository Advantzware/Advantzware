/* webget.p
 *
 * simple program for testing socket communications code
 *
 */

define new global shared variable dbgMode  as integer   no-undo.
define new global shared variable pt_proxy as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "Optional Proxy".
define new global shared variable pt_port  as character no-undo initial "80".

define variable host    as character no-undo format "x(30)" initial "demo.wss.com".
define variable path    as character no-undo format "x(60)" initial "/proxy.test".

define variable fName   as character no-undo format "x(30)" initial "proxy.test".

define variable rLen    as integer   no-undo label "result length".
define variable fLen    as integer   no-undo label "file length".
define variable result  as longchar  no-undo.
define variable xstatus as character no-undo format "x(60)".
define variable xrspHdr as character no-undo view-as editor size 110 by 10.
define variable xresult as character no-undo view-as editor size 110 by 30.

run ssg/sausage02.p persistent.

/* pt_port = 443. */

dbgMode = 7.	/* 7 is about as high as you really want to go */

pause 0 before-hide.

do while true on endkey undo, leave:

  update
    dbgMode label "debug level" skip
    skip(1)
    host  skip
    path  skip
    pt_port  skip
    skip(1)
    pt_proxy skip
    skip(1)
    fName skip
   with
    1 column
    side-labels
    no-box
    width 132
  .

  output to value( "webget.log" ).
  run getURL( host, path, fName, output result, output xrspHdr ).
  output close.

  xstatus = entry( 1, xrspHdr, "~n" ).

  xresult = substring( result, 1, 4096 ).

  rLen = length( result ).

  if fName <> "" and fName <> ? then
    do:
      file-info:file-name = fName.
      fLen = file-info:file-size.
    end.

  display
    rLen skip
    fLen skip
    skip(1)
    xrspHdr skip
    skip(1)
    xresult skip
  .

end.

return.
