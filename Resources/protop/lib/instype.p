/* lib/instype.p
 *
 */

define input-output parameter custId      as character no-undo format "x(250)".
define input-output parameter installType as integer   no-undo initial 1.


/* check to see if we have a known, default, custId
 */

if custId <> "" and custId <> ? then installType = 2.

installType_loop: do while true:

  display
    skip(1)
    "      A Customer ID is needed to enable communications with the web portal.  Your firewall must have      " skip
    "      port 80 open for outbound traffic from the db server for this to work.                              " skip
    skip(1)
    '      If you do not wish to use the web portal choose the "Local" install type.                           ' skip
    skip(1)
    installType at 15   view-as radio-set radio-buttons
      " Create a new custId", 1,
      " Use an existing custId", 2,
      " Local install (no portal)", 3
    skip(1)
   with
    frame updInstallType
    title " ProTop 3 Install Type "
    centered
    row 3
    width 110
    no-labels
  .

  update
    installType
   go-on( "enter", "return" )
   with
    frame updInstallType
  .

  leave installType_loop.

end.

return.
