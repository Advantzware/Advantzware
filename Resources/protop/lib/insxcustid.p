/* lib/insxcustid.p
 *
 * prompt for an existing custId
 *
 */

define input-output parameter custId   as character no-undo format "x(250)".

custId_loop: do while true:

/***
  on error undo, leave
  on endkey undo, leave
  on stop undo, leave:
 ***/

  display
    skip(1)
    "      A Customer ID is needed to enable communications with the web portal.  Your firewall must have      " skip
    "      port 80 open for outbound traffic from the db server for this to work.                              " skip
    skip(1)
    "      Enter your previously assigned custId below.                                                        " skip
    skip(1)
    "        " custId view-as fill-in size 15 by 1 label " Customer ID" skip
    skip(1)
   with
    frame updCustId
    title " Existing ProTop CustId "
    centered
    row 3
    width 110
    side-labels
  .

  update custId with frame updCustId.

  if custId = "" or custId = ? then
    next custId_loop.
   else
    leave custId_loop.

end.

return.
