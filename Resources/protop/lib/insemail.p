/* lib/insemail.p
 *
 */

define input-output parameter emailId as character no-undo format "x(250)".
define input-output parameter compId  as character no-undo format "x(250)".

emailId_loop: do while true
  on error undo, leave
  on endkey undo, leave
  on stop undo, leave:

  update
    skip(1)
    " A valid email address is required in order to send instructions to complete your portal setup. " skip
    skip(1)
    emailId view-as fill-in size 30 by 1 label "    Email ID" skip
    compId  view-as fill-in size 30 by 1 label "Company Name" skip
    skip(1)
   with
    frame updEmailId
    title " New custId email address "
    centered
    row 3
    width 110
    side-labels
  .

  if emailId = "" or emailId = ? or length( emailId ) > 1 and index( emailId, "@" ) = 0 then
    do:
      message 'That does not look like a valid email. At least try something with an "@".'.
      next emailId_loop.
    end.

  if emailId <> "" and  emailId <> ? then leave.

end.

return.
