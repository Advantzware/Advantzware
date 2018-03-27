/* fa/rtaxx.p   Tax Table List (from pt/driver.m)
		System Generated 05/29/97
*/
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

{fa/rtax.y new}
{pt/shared}
{pt/globals.i}
define variable loop-count as integer initial 1.
define variable Z_date as char format "X(30)" label "System-date" no-undo.
define new shared variable answer as char.

define shared var w-$pgm as char format "x(8)" no-undo.
define var frame-cnt as int.

if condition = "edit params":U then loop-count = 0.
r-memo = "Please forward printout to " + entry(1,userid("ptdb":U),"@").

w-$pgm = "fa/rtaxx":U.   /* added for report help */

repeat with side-labels no-box 1 down:   /* USED FOR ENDKEY ERROR PROCESSING */
  input-loop: repeat:

    /* ENTER OUTPUT DEVICE */

     run value("pt/report0.p":U).

    if keyfunction(lastkey) = "endkey":U
    or keyfunction(lastkey) = "end-error":U then return.


    /* ENTER PARAMETERS */

    frame-cnt = 1.
    status input "Enter Page-Up and Page-Down for navigation".
    repeat on endkey  undo, next input-loop :

       pause 0.
       if frame-cnt = 1 then repeat
              with side-labels 1 down three-d
	        title " Tax Table List  (page  1)" centered :   /* page 1 */
         update
fr-method colon 32 view-as fill-in
help "Enter the starting tax method for the range."
to-method colon 32 view-as fill-in
help "Enter the ending tax method for the range."
	   go-on(page-up page-down)
	 with {&UGUI-SCR}.

         if keyfunction(lastkey) = "page-up":U then do:
                frame-cnt = frame-cnt - 1.
                leave.
         end.


         frame-cnt = frame-cnt + 1.
         leave.
       end.  /* of frame-cnt */










        /* end of change mi */

       if frame-cnt = 0 then next  input-loop. /* do at the very end */
       if keyfunction(lastkey) ne "page-up":U then leave.
    end. /* of repeat loop for update */


    frame-cnt = 1.
    if keyfunction(lastkey) = "endkey":U then return.
    if keyfunction(lastkey) = "end-error":U then next.

    if loop-count > 0 then leave.
    loop-count = 1.
    input from terminal.  /* TO ALLOW UPDATE OF PARAMS IN STACKER */
  end.  /* OF INPUT LOOP */

  if keyfunction(lastkey) = "endkey":U then return.

  /* OUTPUT PARAMS TO WORKFILE */

  output to value(z_tempfile).
  export r-date r-output p-output r-memo.
  export
fr-method
to-method
                .	
  export
	.
  export
	 .
  export
	 .
  export
	 .
  export
	 .
  export
	 .
  export
	 .
  export
	 .
  export
	 .

  output close.

  if condition = "edit params":U then return.

  /* ENTER PROCEED QUESTION */

  run value("pt/report9.p":U).

  frame-cnt = 1.
  if keyfunction(lastkey) = "endkey":U or lastkey = -1 then return.
  if keyfunction(lastkey) = "end-error":U then next.
  if answer ne "p":U then return.

  /* PRINT REPORT FRONT-END, OPEN OUTPUT, DO REPORT ... */

  z_date = substr("SunMonTueWedThuFriSat",(weekday(today) - 1 ) * 3 + 1,3)
	   + " " + substr("JanFebMarAprMayJunJulAugSepOctNovDec",
	   (month(today) - 1 ) * 3 + 1,3)
	   + " " + string (day(today),">9")
	   + " " + string(time,"hh:mm:ss")
	   + " " + string (year(today),"9999").

  output to value(z_tempfile).

  find  z_funct where  z_funct.funct-name = functionname no-lock.

  display systemname   no-label
	  (if r-memo > "" then " Memo: " + r-memo else "") format "x(42)" at 40
	  "Page:"                     to 116 space(8) 1 format "9"
                skip(1)

	  functdesc    no-label       at 1
	  "(Report Parameters)"       at 40
	  "As-of date:"               to 116
	  r-date       no-label
	  string(time,"hh:mm") format "x(5)"
                skip(1)

	  fill("-",132) format "x(132)"
                skip(1)

	  z_date colon 32  skip(1)
	  dbname label "Database Name" format "x(30)" colon 32  skip(1)
	  entry(1,userid("ptdb":U),"@") colon 32 label "User Id"  skip(1)
	  terminalid colon 32 label "Terminal Id" skip(1)
	   z_funct.funct-name  colon 32 label "Function name" skip(1)
	  " Parameter list =" skip (1)
	  p-output colon 32  skip(1)
	  r-output colon 32 skip(1)

fr-method colon 32  skip(1)
to-method colon 32  skip(1)
	  with {&UGUI-RPT} width 132 no-box no-attr-space use-text.

  output close.
  run value("pt/report1.p":U).
  leave.
end.
return.
return.
