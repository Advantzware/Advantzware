/* rm-rctd.i - window local-view - rstark 9.30.2018 */

/* this included used in multiple programs because  */
/* external table is the same - rm-rctd             */
/* add scop-def in defs section of win container.w  */
/* SCOPED-DEFINE winViewPrgmName <prgm id>          */

&IF "{&winViewPrgmName}" EQ "addon_w-phycnt" &THEN
    {methods/selectTab.i 1}
    RUN auto-add IN h_p-updsav.
&ELSEIF "{&winViewPrgmName}" EQ "rm-ucpt" &THEN
    {methods/selectTab.i 1}
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"init-entry-target", OUTPUT char-hdl).
    RUN apply-entry IN WIDGET-HANDLE(char-hdl).
&ELSEIF "{&winViewPrgmName}" EQ "w-issue" &THEN
    {methods/selectTab.i 1}
    RUN auto-add IN h_p-updsav.
&ELSEIF "{&winViewPrgmName}" EQ "w-jobret" &THEN
    {methods/selectTab.i 1}
    RUN auto-add IN h_p-updsav.
&ELSEIF "{&winViewPrgmName}" EQ "w-loadrm" &THEN
    RUN auto-add IN h_p-updsav.
&ELSEIF "{&winViewPrgmName}" EQ "w-rcpt" &THEN
    {methods/selectTab.i 1}
    RUN auto-add IN h_p-updsav.
&ELSEIF "{&winViewPrgmName}" EQ "w-recven" &THEN
    {methods/selectTab.i 1}
    RUN entry-vend-tag-proc IN h_v-recven.
&ELSEIF "{&winViewPrgmName}" EQ "w-phycnt" &THEN
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"focus-target",OUTPUT char-hdl).
    IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
    RUN init-focus IN WIDGET-HANDLE(char-hdl).
&ELSEIF "{&winViewPrgmName}" EQ "w-trans" &THEN
    {methods/selectTab.i 1}
    RUN auto-add IN h_p-updsav.
&ENDIF
