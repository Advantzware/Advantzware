/* lib/inschui.p
 *
 *
 *
 */

{lib/v9.i}
{lib/tt_dblist.i}

function hexEncode returns character ( input src as character ) in super.

define input-output parameter custId  as character no-undo.
define input-output parameter envHOST as character no-undo.

define input-output parameter envPT3  as character no-undo format "x(250)".
define input-output parameter envDLC  as character no-undo format "x(250)".
define input-output parameter envLOG  as character no-undo format "x(250)".
define input-output parameter envTMP  as character no-undo format "x(250)".
define input-output parameter envRPT  as character no-undo format "x(250)".

define variable installType as integer no-undo initial 1
  view-as radio-set radio-buttons
    " Create a new custId", 1,
    " Use an existing custId", 2,
    " Local install (no portal)", 3
.

define variable dbDirList  as character no-undo format "x(250)" view-as fill-in size 60 by 1.
define variable nameList   as character no-undo format "x(250)" view-as fill-in size 60 by 1.
define variable pathList   as character no-undo format "x(250)" view-as fill-in size 60 by 1.
define variable dirList    as character no-undo.
define variable dName      as character no-undo.

define variable DS         as character no-undo.

DS = ( if opsys begins "win" then "~\" else "/" ).

define variable useSockets as logical   no-undo.


function leftMessage returns character ( input t as character, input w as integer ):

  define variable m as character no-undo.
  define variable s as character no-undo.
  define variable i as integer   no-undo.
  define variable n as integer   no-undo.

  n = num-entries( t, "~n" ).
  do i = 1 to n:
    assign
      s = entry( i, t, "~n" )
      s = s + fill( " ", w - length( s ))
      s = substring( s, 1, w )
      m = m + s + "~n"
    .
  end.

  return m.

end.


/* prompt for an existing custId
 *
 */

procedure getCustId:

  define variable proxyStat as logical no-undo.
  define variable mergeStat as logical no-undo.

  publish "logMsg" ( 0, "getCustId()" ).

  custId_loop: do while true
    on error undo, leave
    on endkey undo, leave
    on stop undo, leave:

    run lib/insxcustid.p ( input-output custId ).

    if custId = "" or custId = ? then next custId_loop.

    if envHost = "" or envHost = ? or useSockets = no then
      leave custId_loop.								/* no way to check without a host	*/
     else
      do:

        publish "logMsg" ( 0, "getCustId: proxytest" ).
        run lib/insproxy.p ( envHost, output proxyStat ).
        if proxyStat = false then leave custId_loop.

        message "Verifying custId and merging resources from portal".
        publish "logMsg" ( 0, "getCustId: lib/mergedblist" ).
        run lib/mergedblist.p ( input-output custId, envHost, "POST", output mergeStat ).	/* check the portal for the custId	*/
        if mergeStat = false then next custId_loop.

      end.

    if custId = "" or custId = ? then
      next custId_loop.
     else
      do:
        message "Customer ID" custId "confirmed." view-as alert-box title " Existing custId ".
        publish "logMsg" ( 0, substitute( "getCustId: &1 confirmed", custId )).
      end.

    leave custId_loop.

  end.

  return.

end.


/* get a new custId
 *
 */

procedure getNewCustId:

  define variable emailId    as character no-undo format "x(250)" view-as fill-in size 30 by 1 label "    Email ID".
  define variable compId     as character no-undo format "x(250)" view-as fill-in size 30 by 1 label "Company Name".

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 10.0 &THEN
  define variable result   as character no-undo.
  define variable payload  as character no-undo.
&ELSE
  define variable result   as longchar  no-undo.
  define variable payload  as longchar  no-undo.
&ENDIF

  define variable proxyStat as logical no-undo.

  define variable respHeaders as character no-undo.
  define variable resphdrX    as character no-undo.
  define variable httpHeaders as character no-undo.

  define variable ok as logical.

  define variable ptVerStr    as character no-undo initial "{lib/ptversion.i}".
  define variable NL          as character no-undo.

  ptVerStr = trim( entry( 1, ptVerStr, "~n" )).

  NL = chr( 13 ) + chr( 10 ).						/* official http spec is CR + LF			*/

  define variable msgTemplate as character no-undo.
  define variable msgBody     as character no-undo.

  msgTemplate = "email=&1&&companyName=&2".

  if useSockets = no then return.

  publish "logMsg" ( 0, "getNewCustId()" ).

  email_loop: do while true:

    run lib/insemail.p ( input-output emailId, input-output compId ).

    if emailId <> "" and emailId <> ? then
      leave email_loop.
     else
      do:

        message
          color value( "red" )
          skip(1)
          'Abandon portal setup?'
          skip(1)
         view-as alert-box question buttons yes-no title " Are You Sure? "
          update ok
        .

        if ok then
          do:
            custId = ?.
            emailId = "".
            publish "logMsg" ( 0, "getNewCustId() abandoned" ).
            return.
          end.

      end.

  end.

  publish "logMsg" ( 0, "getNewCustId: proxytest" ).
  run lib/insproxy.p ( envHost, output proxyStat ).
  if proxyStat = false then return.

  /* curl -X POST --data "email=me@email.com&companyName=mycompany" http://demo.wss.com/api/newsite.php -o test.dat
   */

  httpHeaders =								/* setup http headers					*/
    'Host: ' + envHOST + NL +						/* required?						*/
    'User-Agent: protop' + ptVerStr + NL +				/* optional						*/
    'Accept: *' + '/' + '*' + NL					/* optional -- beware star-slash and slash-star!	*/
  .

/*  msgBody = "/api/newsite.php" + "?" + substitute( msgTemplate, hexEncode( emailId ), hexEncode( compId )). */

  msgBody = "/api/newsite.php" + "?" + "email=" + hexEncode( emailId ) + "&companyName=" + hexEncode( compId ).

  publish "logMsg" ( 0, substitute( "getNewCustid( &1 )", msgBody )).

  /* let the user know that we are working on getting then a custId...
   */

  display
    skip(1)
    " Obtaining a new custId..." skip
    skip(1)
    " This may take a while for slow network connections. Please be patient. " skip
    skip(1)
   with
    frame checkPortal
    title " New custId "
    centered
    row 3
    width 110
    side-labels
  .

  run postURL(
    envHOST,
    msgBody,
    httpHeaders,
    "",									/* optional data payload				*/
    "",									/* optional filename					*/
    output result,
    output respHeaders
  ).

  if /* length( result ) = 5 and */ result begins "z" then		/* this is a bit too hard-coded :(			*/
    do:
      custId = string( result ).
      /***
       * message
       *   skip(1)
       *   "Your new custId is " string( result )
       *   skip(1)
       *   "An email has been sent to " emailId "with" skip
       *   "instructions to complete your portal setup." 
       *   skip(1)
       *   view-as alert-box
       *   title " Confirmation "
       * .
       ***/
    end.
   else
    do:

      custId = ?.

      respHdrX = substring( respHeaders, 1, 2048 ).
      if respHdrX = substring( string( result ), 1, 2048 ) then respHdrX = "".

      publish "logMsg" ( 0, substitute( "getNewCustId error: &1", respHdrX )).

      if index( string( result ), "nothing to return" ) > 0 then
        do:

          message
            skip(1)
            leftMessage( "An timeout error occurred getting a custId.", 76 )  skip
            leftMessage( 'If you receive a "New ProTop custId" email you can manually enter the custId in etc/custid.cfg', 76 ) skip
            leftMessage( "Otherwise please contact protop@wss.com for support.", 76 ) skip
            skip(1)
            view-as alert-box
            title " Timeout Error "
          .

        end.
       else
        do:

          message
            skip(1)
            leftMessage( "An error occurred getting a custId.", 76 )  skip
            leftMessage( substring( "Request: " + string( msgBody ), 1, 2048 ), 76 ) skip
            leftMessage( "Please contact protop@wss.com for support.", 76 ) skip
            skip(1)
            leftMessage( substring( string( result ), 1, 2048 ), 76 ) skip
            leftMessage( respHdrX, 76 )
            skip(1)
            view-as alert-box
            title " Error "
          .

        end.

    end.

  return.

end.

define variable mergeStat as logical no-undo.

define variable i as integer no-undo.
define variable n as integer no-undo.

/* main
 *
 */

if os-getenv( "USESOCKETS" ) = "no" then
  useSockets = no.
 else
  useSockets = yes.

do on error undo, leave
   on endkey undo, leave
   on stop undo, leave:

    config_loop: do while session:batch = no:

      if ( envDLC = "" or envPT3 = "" or envLOG = "" or envTMP = "" or envRPT = "" or
           envDLC = ?  or envPT3 = ?  or envLOG = ?  or envTMP = ?  or envRPT = ? ) then
        do:
          run lib/inscfgvars.p( input-output envPT3, input-output envDLC, input-output envLOG, input-output envTMP, input-output envRPT ).
          publish "logMsg" ( 0, substitute( "User values: [DLC=&1] [LOGDIR=&2] [TMPDIR=&3] [RPTDIR=&4] [CUSTID=&5] [ENVPT3=&6]", envDLC, envLOG, envTMP, envRPT, custId, envPT3 )).
        end.

      run lib/instype.p ( input-output custId, input-output installType ).

      case installType:
        when 1 then run getNewCustId.		/* get a new custId		*/
        when 2 then run getCustId.		/* use an existing custId	*/
        when 3 then custId = "".		/* local install		*/
        otherwise
          do:
            publish "logMsg" ( 0, "Invalid install type, aborting install." ).
            quit.
          end.
      end.

      publish "logMsg" ( 0, substitute( "installType: &1 &2", entry( installType, "new custId,existing custId,local" ), custId )).

      if custId = ? and installType < 3 then
        do:
          publish "logMsg" ( 0, substitute( "install error: &1 &2", entry( installType, "new custId,existing custId,local" ), custId )).
          assign
            installType = 3
            custId = ""
          .
          publish "logMsg" ( 0, substitute( "installing locally: &1 &2", entry( installType, "new custId,existing custId,local" ), custId )).
        end.

    /* db discovery
     */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 10.0 &THEN
    run lib/readdblist.p ( output table tt_dbList ).
&ELSE
    run lib/readdblist.p ( output table tt_dbList by-reference ).
&ENDIF

    n = 0.
    for each tt_dbList:
      n = n + 1.
    end.

    if n > 0 then
      publish "logMsg" ( 0, substitute( "no db discovery, etc/dblist.cfg exists and has &1 entries", n )).
     else
      dbDiscovery_loop: do while true on endkey undo, leave on error undo, leave:

        publish "logMsg" ( 0, "auto db discovery offered" ).

        run lib/insdbdisc.p ( input-output dbDirList ).

        if dbDirList = "" then leave dbDiscovery_loop.

        dbDirList = replace( dbDirList, ",", " " ).
        n = num-entries( dbDirList, " " ).
        do i = 1 to n:

          dName = entry( i, dbDirList, " " ).

          dName = right-trim( dName, DS ).

          file-info:file-name = dName.

          if file-info:full-pathname = ? then
            do:
              message "no such directory:" dName.
              next dbDiscovery_loop.
            end.

          if index( file-info:file-type, "d" ) <= 0 then
            do:
              message dName "is not a directory".
              next dbDiscovery_loop.
            end.

          dirList = substitute( "&1 &2", dirList, dName ).

        end.

        dbDirList = trim( dirList ).

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 10.0 &THEN
        run lib/discover.p ( dbDirList, output table tt_dbList ).
&ELSE
        run lib/discover.p ( dbDirList, output table tt_dbList by-reference ).
&ENDIF

        n = 0.
        for each tt_dbList:
          n = n + 1.
        end.

        if n = 0 then
          publish "logMsg" ( 0, "discovery: no databases found" ).
         else
          do: 
            publish "logMsg" ( 0, substitute( "discovery: &1 databases discovered", n )).
            publish "logMsg" ( 0, substitute( "running lib/mergedblist( &1, &2, &3 )", custId, envHost, "POST" )).
            run lib/mergedblist.p ( input-output custId, envHost, "POST", output mergeStat ).
          end.

        leave dbDiscovery_loop.

      end.

      leave config_loop.

    end.

end.

if session:batch = no then
  do:
    case installType:
      when 1 then message "New custId" custId "has been configured.  Check your email for instructions to login to the ProTop portal!" view-as alert-box title " ProTop Install Complete ".
      when 2 then message custId "has been configured." view-as alert-box title " ProTop Install Complete ".
      when 3 then message "ProTop has been installed locally, no portal access has been configured.  If you decide you would like to use the web portal run:~n pro -p lib/install.p from a PROENV command." view-as alert-box title " ProTop Install Complete ".
      otherwise   message "Very strange ProTop3 install finished!" view-as alert-box title " ProTop Install Complete ".
    end.
  end.

return.
