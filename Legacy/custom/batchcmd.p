/*custom/batchcmd.p */

/* for asi
CONNECT -pf z:\asi_gui9\pco410\nosweat.pf NO-ERROR .
CONNECT -pf z:\asi_gui9\pco410\asi.pf NO-ERROR.
*/            

CONNECT -pf n:\rcode\nosweat.pf NO-ERROR .
CONNECT -pf n:\rcode\asi.pf NO-ERROR.


IF CONNECTED('nosweat') AND CONNECTED('asi') THEN RUN ./custom/exporder.p.
