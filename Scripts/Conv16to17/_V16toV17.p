/* _admV16toV17.p */

/* ensure _base.i scoped-define test value is set correctly */

IF INDEX(PROPATH,"Conv16to17") EQ 0 THEN
PROPATH = "C:/Advantzware/v16/Scripts/Conv16to17," + PROPATH.

RUN _globdefs.p.
RUN _lookups.p.
RUN _nonAdm1.p.
RUN _nonAdm1Images1.p.
RUN _nonAdm1Images2.p.
RUN _admBrowsers.p.
RUN _admViewers.p.
RUN _admWindows.p.
RUN _admPanels.p.
RUN _admTransPanels.p.
