/* usermonx.p
 *
 * User Defined Metrics
 *
 * This module allows you to collect and monitor user defined and application 
 * specific 
 *
 * edit and rename lib/usermonx.p to lib/usermon.p to activate
 *
 * The simplest metrics to monitor are counters that monotonically increase
 * and which bear an interesting relationship to business activity.  "Widgets
 * Shipped" might, for instance, be such a metric in your system.
 *
 * IF YOU INTEND TO USE THIS CAPABILITY IN A MULTI-DB ENVIRONMENT YOU MUST
 * USE LDBNAME(1) TO RUN SUB PROGRAMS SPECIFIC TO THE CONNECTED DATABASE
 *
 */


/* launch db specific routines as needed
 *
 * SAMPLE CODE - replace with real database names and real user-defined functions
 *
 */

if ldbname(1) = "sports2000" or ldbname(1) = "s2k" then
  run value( "lib/sports2000.p" ) persistent.				/* contains code that is tied to the sports2000 db	*/
 else  
  do:									/* otherwise just use the current procedure		*/
    session:add-super-procedure( this-procedure ).
    subscribe to "usermon" anywhere run-procedure "userMon".
  end.

return.


/* the real work
 *
 * dc/dashboard.p PUBLISHes the request
 *
 * this example is NOT tied to any particular database schema so it is  ok to run within lib/usermon.p
 *
 */

define variable i as integer no-undo.

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

  /***
  ufld1 = ?.
  ulbl1 = "User Defined:".
   ***/

  /***
  i = i + 1.
  ufld1 = i.
  ufld8 = random( 1, 100 ).
   ***/

  return.

end.
