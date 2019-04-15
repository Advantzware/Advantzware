/* startUp.p - used to create custom startUp code */

DEFINE OUTPUT PARAMETER opContinue AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opCommaList AS CHARACTER NO-UNDO.

/* custom code (security check, etc) here, PROGRAM-NAME(2) is
   name of calling program (sbPro.p, sbView.p or sbBasic.p */

DEFINE VARIABLE sbName AS CHARACTER NO-UNDO.

{schedule/scopDir.i}

sbName = IF INDEX(PROGRAM-NAME(2),'sbPro.')    NE 0 THEN 'sbPro.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbView.')   NE 0 THEN 'sbView.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbBasic.')  NE 0 THEN 'sbBasic.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbHTML.')   NE 0 THEN 'sbHTML.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbReport.') NE 0 THEN 'sbReport.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbStatus.') NE 0 THEN 'sbStatus.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbJScan.')  NE 0 THEN 'sbJScan.'
    ELSE IF INDEX(PROGRAM-NAME(2),'sbDMI.')    NE 0 THEN 'sbDMI.'
    ELSE ''
    .
{methods/defines/hndldefs.i}
{custom/prgsecur.i &vprgmname=sbName}

ASSIGN
  opContinue = NOT access-close /* output YES or NO */
  opCommaList  = g_company + "," + g_loc
  .
