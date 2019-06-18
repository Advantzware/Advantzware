/* httplib.p
 *
 * something a bit less vile than the kbase code
 *
 * (unused)
 *
 * december 28, 2012 */

define new global shared variable pt_tmpdir as character no-undo initial "/tmp".
define new global shared variable pt_logdir as character no-undo initial "/tmp".

define new global shared variable dbgMode as integer no-undo initial 1.

define variable NL as character no-undo.

NL = chr( 13 ) + chr( 10 ).					/* CR + LF					*/

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

return.

define stream outfile.

define variable vsocket as handle    no-undo.
define variable wloop   as logical   no-undo.
define variable xLOB    as longchar  no-undo.

/* break a URL into host port & path components
 */

procedure parseURL:

  define input  parameter url  as character no-undo.
  define output parameter host as character no-undo.
  define output parameter port as character no-undo.
  define output parameter path as character no-undo.

  assign
    port = "80"
    path = "/"
  .

  /* strip "http://" if it is provided
   */

  if url begins "http://" then url = substring( url, 8 ).

  host = url.

  if num-entries( url, "/" ) > 1 then
    assign
      host = entry( 1, url, "/" )
      path = substring( url, index( url, "/" ))
    .

  if num-entries( host, ":" ) = 2 then
    assign
      port = entry( 2, path, ":" )
      host = entry( 1, path, ":" )
    .
   else if num-entries( path, ":" ) = 2 then
    assign
      port = entry( 2, path, ":" )
      path = entry( 1, path, ":" )
    .

  return.

end.

procedure readHandler:

  define variable l   as integer   no-undo.
  define variable p   as integer   no-undo.
  define variable str as longchar  no-undo.
  define variable b   as memptr    no-undo.

  self:sensitive = no.							/* no interruptions!	*/

  l = vsocket:get-bytes-available().

  if self:connected() = false then
    do:
      if dbgMode >= 5 then message now "Not connected! length( xLOB ) = " length( xLOB ) "bytes-available:" vsocket:get-bytes-available() view-as alert-box.
      apply 'u9' to this-procedure.
      return.
    end.

  if l <= 0 then
    do:
      wloop = no.
      if dbgMode >= 9 then output stream outfile close.
      if dbgMode >= 5 then message "no more data..." view-as alert-box.
      apply 'u9' to this-procedure.
    end.
   else
    do:
      set-size( b ) = l + 1.
      vsocket:read( b, 1, l, 1 ).
      p = 1.
      do while ( l > 0 ):
        assign
          str  = get-string( b, p, min( l, 16384 ))
          xLOB = xLOB + str
          p    = p + 16384
          l    = l - 16384
        .
      end.
      if dbgMode >= 9 then copy-lob from xLOB to file pt_tmpdir + "/httpget.tmp" append no-convert.
      set-size( b ) = 0.
      if vsocket:get-bytes-available() < 1 then
        apply "u9" to this-procedure.
       else
        wloop = yes.
      self:sensitive = yes.
    end.

  return.

end.

procedure getURL:

  define input  parameter host   as character no-undo.
  define input  parameter port   as character no-undo.
  define input  parameter path   as character no-undo.
  define output parameter result as longchar  no-undo.

  define variable wstatus as logical   no-undo.
  define variable vstr    as character no-undo.
  define variable vbuffer as memptr    no-undo.

  assign    
    wloop = yes
    xLOB  = ""
  .

  create socket vsocket.
  vsocket:set-read-response-procedure( "readHandler", this-procedure ).
  wstatus = vsocket:connect( "-H " + host + " -S " + port ) no-error.

  if wstatus = yes then
    do:
      if dbgMode >= 5 then message "Connection to HTTP server:" host "port" port "established successfully." view-as alert-box.
    end.
   else
    do:
      if dbgMode > 0 then message "Connection to HTTP server:" host "port" port "is unavailable." view-as alert-box.
      delete object vsocket.
      return.
    end.

  if dbgMode >= 9 then output stream outfile to value( pt_tmpdir + "/httpget.tmp" ) append binary no-convert.

  vstr = "GET " + path + " HTTP/1.0" + NL + NL + NL.
  set-size( vbuffer ) = length( vstr ) + 1.
  set-byte-order( vbuffer ) = big-endian.
  put-string( vbuffer, 1 ) = vstr.
  vsocket:write( vbuffer, 1, length( vstr )).
  set-size( vbuffer ) = 0.

  wait-for "u9" of this-procedure.

  /* cleanup
   */

  vsocket:disconnect().
  delete object vsocket.
  if dbgMode >= 9 then output stream outfile close.

  /**
  copy-lob from file pt_tmpdir + "/httpget.tmp" to result.
   **/

  result = xLOB.

  return.

end.
