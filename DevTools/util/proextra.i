/* ====================================================================
   file      ProExtra.i
   by        Jurjen Dijkstra, 1997
             mailto:jurjen.dijkstra@wxs.nl
             http://www.pugcentral.org/api
   language  Progress 8.2A
   purpose   "uses ProExtra"
   ==================================================================== */
&IF DEFINED(PROEXTRA_I)=0 &THEN
&GLOBAL-DEFINE PROEXTRA_I
   
def new global shared var hpExtra as handle no-undo.
if not valid-handle(hpExtra) then run util/ProExtra.p persistent set hpExtra.

&ENDIF  /* &IF DEFINED(PROEXTRA_I)=0 */

