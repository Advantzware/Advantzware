/* lib/setptname.p
 *
 */

{lib/vstlib.i}

define variable i as integer   no-undo.
define variable k as character no-undo.

define buffer upd_Connect for _Connect.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.7 &THEN

if isReplTarget() = no then
  do:

    find _myconnection no-lock no-error.
    if available _myconnection then        /* we might not be connected to a db...        */
      do:

         i = 1.
         do while program-name( i ) <> ?:
           k = program-name( i ).
           i = i + 1.
         end.

         if num-entries( k,  "/" ) > 1 then k = entry( num-entries( k,  "/" ), k,  "/" ).
         if num-entries( k, "~\" ) > 1 then k = entry( num-entries( k, "~\" ), k, "~\" ).
         if num-entries( k,  "." ) = 2 then k = entry( 1, k, "." ).

         do for upd_Connect transaction:
           find upd_connect where upd_Connect._connect-id = _myconn-userid + 1.
           buffer upd_connect:handle:buffer-field( "_connect-usermisc" ):buffer-value = "pt3:" + k no-error.
           /*** upd_Connect._connect-usermisc = "pt3:" + k.  ***/
         end.

      end.

  end.

&ENDIF

return.
