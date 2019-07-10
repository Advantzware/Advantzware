/* badtrx.p
 *
 * example of bad TRX scoping that will result in bi growth if some OTHER process is doing work
 *
 */

define variable flgName as character no-undo initial "badtrx&1.flg".
define variable lastCust as integer no-undo.

find last customer no-error.
lastCust = customer.custNum.

flgName = substitute( flgName, session:parameter ).
output to value( flgName ) unbuffered append.
message now flgName.
output close.

loop: do while true:

  find first customer where customer.custNum > random( 1, lastCust ) no-error.

  if not available customer then next.

  repeat on error undo, retry:

    /*
      find first billTo.
      postalCode = "03047".
     */

      create billTo.
      assign
        billTo.custNum = customer.custNum
        billTo.billToId = random( 1, 10000 )
      .

      do while true:

        pause 0.5.

        file-info:file-name = flgName.
        if file-info:full-pathname = ? then leave loop.

      end.

  end.

end.

quit.
