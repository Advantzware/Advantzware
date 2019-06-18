/* sports2000.p
 *
 * initialized by lib/usermon.p IF sports2000 or s2k are connected
 *
 */


session:add-super-procedure( this-procedure ).

subscribe to "usermon" anywhere run-procedure "userMon".

return.


/* respond to requests for user-defined application specific metrics
 *
 * dc/dashboard.p PUBLISHes the request
 *
 * the label can be 13 characters wide and should include a final ":"
 *
 */

procedure userMon:

  define output parameter ufld1 as decimal   no-undo.
  define output parameter ulbl1 as character no-undo.

  define output parameter ufld2 as decimal   no-undo.
  define output parameter ulbl2 as character no-undo.

  define output parameter ufld3 as decimal   no-undo.
  define output parameter ulbl3 as character no-undo.

  define output parameter ufld4 as decimal   no-undo.
  define output parameter ulbl4 as character no-undo.

  define output parameter ufld5 as decimal   no-undo.
  define output parameter ulbl5 as character no-undo.

  define output parameter ufld6 as decimal   no-undo.
  define output parameter ulbl6 as character no-undo.

  define output parameter ufld7 as decimal   no-undo.
  define output parameter ulbl7 as character no-undo.

  define output parameter ufld8 as decimal   no-undo.
  define output parameter ulbl8 as character no-undo.

  assign
    ufld1 = random( 0, 99 )
    ulbl1 =  "randomLabel:"
  .

  return.

end.
