/* hdRCO1010.p */

&SCOPED-DEFINE dbDir p:/asi10test/rco1010

/* commented out by DGD 04/01/2007 to remove error messages. */
/* CONNECT -pf {&dbDir}/nosweat.pf.            */
/* CONNECT -pf {&dbDir}/asi.pf.                */
/* CONNECT -pf {&dbDir}/asihelp.pf.            */
/* CONNECT -pf {&dbDir}/addon/emptrack-tst.pf. */
/* CONNECT -pf {&dbDir}/addon/jobs-tst.pf.     */
/* CONNECT -pf {&dbDir}/addon/rfq-tst.pf.      */
/* commented out by DGD 04/01/2007 to remove error messages. */

CONNECT -H asisbs -N TCP -S 3802 -db nosweat   -ld NOSWEAT.
CONNECT -H asisbs -N tcp -S 3800 -db asi       -ld ASI.
CONNECT -H asisbs -N tcp -S 3801 -db asihelp   -ld ASIHLP.
CONNECT -H asisbs -N tcp -S 3808 -db emptrack  -ld EMPTRACK.
CONNECT -H asisbs -N tcp -S 3809 -db jobs      -ld JOBS.
CONNECT -H asisbs -N tcp -S 3811 -db rfq       -ld RFQ.

RUN hdCompiler.w ('1010').
QUIT.
