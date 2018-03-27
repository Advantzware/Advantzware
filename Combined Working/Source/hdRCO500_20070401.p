/* hdRCO410.p */

&SCOPED-DEFINE dbDir z:/asi9test/rco500

CONNECT -pf {&dbDir}/nosweat.pf.
CONNECT -pf {&dbDir}/asi.pf.
CONNECT -pf {&dbDir}/asihelp.pf.
CONNECT -pf {&dbDir}/addon/emptrack-tst.pf.
CONNECT -pf {&dbDir}/addon/jobs-tst.pf.
CONNECT -pf {&dbDir}/addon/rfq-tst.pf.

RUN hdCompiler.w ('500').
QUIT.
