/* chkcs.p
 *
 * client/server will "work" but performance may be cringe-worthy
 *
 */

find _myconnection no-lock.
find _connect no-lock where _connect-id = _myconn-userId + 1.

if _connect-type = "remc" then
  message
    color value( "red" )
    skip(1)
    '  You are running in client/server mode.  ' skip
    '  ProTop generates quite a lot of network traffic.  ' skip
    '  Performance may be "unimpressive".  ' skip   
    '  You might want to reconsider this decision.  ' skip   
    skip(1)
    view-as alert-box title " Client/Server Connection "
  .

return.
