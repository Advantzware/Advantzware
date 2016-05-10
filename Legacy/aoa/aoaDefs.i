/* aoaDefs.i */

&IF DEFINED(test) EQ 0 &THEN
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
    cocode = gcompany
    locode = gloc
    .

&IF "{&aoaCustListForm}" NE "" &THEN
{sys/inc/CustListForm.i ""{&aoaCustListForm}"" }
&ENDIF

&ELSE
DEFINE VARIABLE g_company AS CHARACTER NO-UNDO INITIAL "001".
&ENDIF

DEFINE VARIABLE paramStr AS CHARACTER NO-UNDO
    INITIAL "{&aoaID},{&aoaProgramID},{&aoaTitle},{&aoaType},{&aoaColumns},{&aoaCustListForm}".
                                                 
/* used if testing in AppBuilder */.
&IF DEFINED(test) NE 0 &THEN
paramStr = paramStr + ",{&test}".
&ENDIF
