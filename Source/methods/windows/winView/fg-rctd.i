/* fg-rctd.i - window local-view - rstark 9.30.2018 */

/* this included used in multiple programs because  */
/* external table is the same - rm-rctd             */
/* add scop-def in defs section of win container.w  */
/* SCOPED-DEFINE winViewPrgmName <prgm id>          */

&IF "{&winViewPrgmName}" EQ "w-relbol" &THEN
    {methods/selectTab.i 1}
    RUN auto-add IN  h_p-relbol.
&ELSEIF "{&winViewPrgmName}" EQ "w-updrel" &THEN
    {methods/selectTab.i 1}
    RUN apply-entry-rel IN h_b-updrel.
&ELSEIF "{&winViewPrgmName}" EQ "fg-cons" &THEN
    {methods/selectTab.i 1}
    RUN auto-ADD IN h_p-updbar.
&ELSEIF "{&winViewPrgmName}" EQ "fg-phys" &THEN
    {methods/selectTab.i 1}
    RUN auto-ADD IN h_p-updsav.
&ELSEIF "{&winViewPrgmName}" EQ "fg-physi" &THEN
    RUN auto-ADD IN h_p-updbar.
&ELSEIF "{&winViewPrgmName}" EQ "fg-rcpt" &THEN
    IF lvlAutoAdd THEN DO:
        {methods/selectTab.i 1}
        RUN scan-next IN h_b-rcptd.
    END.
&ELSEIF "{&winViewPrgmName}" EQ "fg-rcpts" &THEN
    IF lvlAutoAdd THEN DO:
        {methods/selectTab.i 1}
        RUN scan-next IN h_b-rcptds.
    END.
&ELSEIF "{&winViewPrgmName}" EQ "fg-trans" &THEN
    {methods/selectTab.i 1}
    RUN auto-ADD IN h_p-updbar.
&ELSEIF "{&winViewPrgmName}" EQ "fg-ucpt" &THEN
    {methods/selectTab.i 1}
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"init-entry-target", OUTPUT char-hdl).
    RUN apply-entry IN WIDGET-HANDLE(char-hdl).
&ENDIF
