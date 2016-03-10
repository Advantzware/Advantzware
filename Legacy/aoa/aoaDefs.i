/* aoaDefs.i */

&IF DEFINED(test) EQ 0 &THEN
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
&ELSE
DEFINE VARIABLE g_company AS CHARACTER NO-UNDO INITIAL "001".
&ENDIF

DEFINE VARIABLE paramStr AS CHARACTER NO-UNDO
    INITIAL "{&aoaID},{&aoaName},{&aoaTitle},{&aoaType}".
                                                 .
