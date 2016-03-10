/* custaoa.p */

&SCOPED-DEFINE test
&SCOPED-DEFINE aoaID 0FF3B5A72400430EA11513E1BA45B6B5
&SCOPED-DEFINE aoaName custaoa
&SCOPED-DEFINE aoaTitle Customers
&SCOPED-DEFINE aoaType Document
&SCOPED-DEFINE aoaParam YES

DEFINE VARIABLE hAppSrv AS HANDLE  NO-UNDO.
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.
CREATE SERVER hAppSrv.
lReturn = hAppSrv:CONNECT("-AppService asAOA -H localhost -S 5162","","").

{aoa/aoaParam.i}

lReturn = hAppSrv:DISCONNECT().
DELETE OBJECT hAppSrv.
