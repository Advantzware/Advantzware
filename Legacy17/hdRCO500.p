/* hdRCO500.p */

&SCOPED-DEFINE dbDir p:/asi9test/rco500

/* commented out by DGD 04/01/2007 to remove error messages. */
/* CONNECT -pf {&dbDir}/nosweat.pf.            */
/* CONNECT -pf {&dbDir}/asi.pf.                */
/* CONNECT -pf {&dbDir}/asihelp.pf.            */
/* CONNECT -pf {&dbDir}/addon/emptrack-tst.pf. */
/* CONNECT -pf {&dbDir}/addon/jobs-tst.pf.     */
/* CONNECT -pf {&dbDir}/addon/rfq-tst.pf.      */
/* commented out by DGD 04/01/2007 to remove error messages. */

/* revised by DGD on 04/01/2007 */
CONNECT -H asisbs -N TCP -S 3703 -db nosweat   -ld NOSWEAT.
CONNECT -H asisbs -N tcp -S 3701 -db asi       -ld ASI.
CONNECT -H asisbs -N tcp -S 3714 -db asihelp   -ld ASIHLP.
CONNECT -H asisbs -N tcp -S 3706 -db emptrack  -ld EMPTRACK.
CONNECT -H asisbs -N tcp -S 3705 -db jobs      -ld JOBS.
CONNECT -H asisbs -N tcp -S 3704 -db rfq       -ld RFQ.
/* revised by DGD on 04/01/2007 */

RUN hdCompiler.w ('500').
QUIT.
