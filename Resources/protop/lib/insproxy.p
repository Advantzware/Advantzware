/* lib/insproxy.p
 *
 */

define input  parameter envHost   as character no-undo.
define output parameter proxyStat as logical   no-undo.

define new global shared
       variable pt_proxy   as character no-undo format "x(250)" view-as fill-in size 40 by 1 label "       Proxy".

define variable useSockets as logical   no-undo.

if os-getenv( "USESOCKETS" ) = "no" then
  useSockets = no.
 else
  useSockets = yes.

run ssg/sausage02.p persistent.
run ssg/sausage06.p persistent.

proxyStat = false.

run proxyTest.

return.


procedure proxyTest:

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 10.0 &THEN
  define variable result   as character no-undo.
  define variable payload  as character no-undo.
&ELSE
  define variable result   as longchar  no-undo.
  define variable payload  as longchar  no-undo.
&ENDIF

  define variable respHeaders as character no-undo.
  define variable resphdrX    as character no-undo.
  define variable httpHeaders as character no-undo.

  define variable ptVerStr    as character no-undo initial "{lib/ptversion.i}".
  define variable NL          as character no-undo.

  ptVerStr = trim( entry( 1, ptVerStr, "~n" )).

  NL = chr( 13 ) + chr( 10 ).						/* official http spec is CR + LF			*/

  proxy_loop: do while useSockets = yes
    on error undo, leave
    on endkey undo, leave
    on stop undo, leave:

    message "Checking communications with portal:" envHost pt_proxy.

    /* curl -X GET http://dashboard.wss.com/proxy.test
     */

    run getURL(
      envHOST,		/* "demo.wss.com", */
      substitute( "/proxy.test" ),
      "",
      output result,
      output respHeaders
    ).

  if result begins "test passed" then
    do:
      if pt_proxy <> "" then
        message "Success: " pt_proxy view-as alert-box title " proxyTest passed ".
      proxyStat = true.
      leave proxy_loop.
    end.
   else
    do:

      /*** message substring( result, 1, 4096 ) view-as alert-box title " proxyTest failed ". ***/

      display
        skip(1)
        "      The install process was unable to contact the portal server at" string( envHost ) format "x(30)"      skip
        skip(1)
        "      You may need to use a proxy server or have your firewall rules modified in order to communicate     " skip
        "      with the portal.                                                                                    " skip
        skip(1)
        "      ProTop can communicate with servers that use proxy configurations similar to:                       " skip
        "            proxy.xyzzy.com:8080                                                                          " skip
        skip(1)
        "      Enter your proxy server configuration or F4/^E to continue without portal communications.           " skip
        skip(1)
        pt_proxy skip
        skip(1)
       with
        frame updProxy
        title " Proxy Server Configuration "
        centered
        row 3
        width 110
        side-labels
      .

      update pt_proxy with frame updProxy.

    end.

  end.

  return.

end.
