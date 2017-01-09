/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : LlConst
    Purpose     : Constants definitions for List & Label API's
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.07.2016 21:35:09
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.LlConst:

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LlConst ():
        SUPER ().

    END CONSTRUCTOR.

    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_DEFAULT                                                   AS INTEGER NO-UNDO INIT          -1 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_GERMAN                                                    AS INTEGER NO-UNDO INIT           0 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_ENGLISH                                                   AS INTEGER NO-UNDO INIT           1 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_ARABIC                                                    AS INTEGER NO-UNDO INIT           2 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_AFRIKAANS                                                 AS INTEGER NO-UNDO INIT           3 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_ALBANIAN                                                  AS INTEGER NO-UNDO INIT           4 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_BASQUE                                                    AS INTEGER NO-UNDO INIT           5 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_BULGARIAN                                                 AS INTEGER NO-UNDO INIT           6 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_BYELORUSSIAN                                              AS INTEGER NO-UNDO INIT           7 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_CATALAN                                                   AS INTEGER NO-UNDO INIT           8 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_CHINESE                                                   AS INTEGER NO-UNDO INIT           9 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_CROATIAN                                                  AS INTEGER NO-UNDO INIT          10 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_CZECH                                                     AS INTEGER NO-UNDO INIT          11 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_DANISH                                                    AS INTEGER NO-UNDO INIT          12 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_DUTCH                                                     AS INTEGER NO-UNDO INIT          13 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_ESTONIAN                                                  AS INTEGER NO-UNDO INIT          14 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_FAEROESE                                                  AS INTEGER NO-UNDO INIT          15 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_FARSI                                                     AS INTEGER NO-UNDO INIT          16 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_FINNISH                                                   AS INTEGER NO-UNDO INIT          17 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_FRENCH                                                    AS INTEGER NO-UNDO INIT          18 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_GREEK                                                     AS INTEGER NO-UNDO INIT          19 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_HEBREW                                                    AS INTEGER NO-UNDO INIT          20 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_HUNGARIAN                                                 AS INTEGER NO-UNDO INIT          21 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_ICELANDIC                                                 AS INTEGER NO-UNDO INIT          22 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_INDONESIAN                                                AS INTEGER NO-UNDO INIT          23 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_ITALIAN                                                   AS INTEGER NO-UNDO INIT          24 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_JAPANESE                                                  AS INTEGER NO-UNDO INIT          25 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_KOREAN                                                    AS INTEGER NO-UNDO INIT          26 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_LATVIAN                                                   AS INTEGER NO-UNDO INIT          27 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_LITHUANIAN                                                AS INTEGER NO-UNDO INIT          28 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_NORWEGIAN                                                 AS INTEGER NO-UNDO INIT          29 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_POLISH                                                    AS INTEGER NO-UNDO INIT          30 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_PORTUGUESE                                                AS INTEGER NO-UNDO INIT          31 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_ROMANIAN                                                  AS INTEGER NO-UNDO INIT          32 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_RUSSIAN                                                   AS INTEGER NO-UNDO INIT          33 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_SLOVAK                                                    AS INTEGER NO-UNDO INIT          34 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_SLOVENIAN                                                 AS INTEGER NO-UNDO INIT          35 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_SERBIAN                                                   AS INTEGER NO-UNDO INIT          36 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_SPANISH                                                   AS INTEGER NO-UNDO INIT          37 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_SWEDISH                                                   AS INTEGER NO-UNDO INIT          38 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_THAI                                                      AS INTEGER NO-UNDO INIT          39 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_TURKISH                                                   AS INTEGER NO-UNDO INIT          40 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_UKRAINIAN                                                 AS INTEGER NO-UNDO INIT          41 GET. 
    DEFINE PUBLIC STATIC PROPERTY CMBTLANG_SERBIAN_LATIN                                             AS INTEGER NO-UNDO INIT          42 GET. 
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HPOS_NONE                                                  AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HPOS_START                                                 AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HPOS_END                                                   AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HPOS_ABS                                                   AS INTEGER NO-UNDO INIT           3 GET. /* (0x0003) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HPOS_MASK                                                  AS INTEGER NO-UNDO INIT           7 GET. /* (0x0007) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VPOS_NONE                                                  AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VPOS_START                                                 AS INTEGER NO-UNDO INIT          16 GET. /* (0x0010) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VPOS_END                                                   AS INTEGER NO-UNDO INIT          32 GET. /* (0x0020) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VPOS_ABS                                                   AS INTEGER NO-UNDO INIT          48 GET. /* (0x0030) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VPOS_MASK                                                  AS INTEGER NO-UNDO INIT         112 GET. /* (0x0070) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HSIZE_NONE                                                 AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HSIZE_PROP                                                 AS INTEGER NO-UNDO INIT         256 GET. /* (0x0100) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HSIZE_INV                                                  AS INTEGER NO-UNDO INIT         512 GET. /* (0x0200) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_HSIZE_MASK                                                 AS INTEGER NO-UNDO INIT         768 GET. /* (0x0300) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VSIZE_NONE                                                 AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VSIZE_PROP                                                 AS INTEGER NO-UNDO INIT        4096 GET. /* (0x1000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VSIZE_INV                                                  AS INTEGER NO-UNDO INIT        8192 GET. /* (0x2000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_VSIZE_MASK                                                 AS INTEGER NO-UNDO INIT       12288 GET. /* (0x3000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_SIZEPOS_MASK                                               AS INTEGER NO-UNDO INIT       13175 GET. /* (0x3377) */
    DEFINE PUBLIC STATIC PROPERTY LL_LINK_SIZEOFPARENT                                               AS INTEGER NO-UNDO INIT       16384 GET. /* (0x4000) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTCALLBACK_PREVIEW_START                             AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTCALLBACK_PREVIEW_ABORT                             AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTCALLBACK_PREVIEW_FINALIZE                          AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTCALLBACK_PREVIEW_QUEST_JOBSTATE                    AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTCALLBACK_EXPORT_START                              AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTCALLBACK_EXPORT_ABORT                              AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTCALLBACK_EXPORT_FINALIZE                           AS INTEGER NO-UNDO INIT           7 GET. /* (7) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTCALLBACK_EXPORT_QUEST_JOBSTATE                     AS INTEGER NO-UNDO INIT           8 GET. /* (8) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTTHREAD_STATE_STOPPED                               AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTTHREAD_STATE_SUSPENDED                             AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNERPRINTTHREAD_STATE_RUNNING                               AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRILLDOWN_START                                                 AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRILLDOWN_FINALIZE                                              AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINTJOB_FINALIZE                                               AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_JOBOPENFLAG_NOLLXPRELOAD                                        AS INTEGER NO-UNDO INIT        4096 GET. /* (0x00001000) */
    DEFINE PUBLIC STATIC PROPERTY LL_JOBOPENFLAG_ONLYEXACTLANGUAGE                                   AS INTEGER NO-UNDO INIT        8192 GET. /* (0x00002000) do not look for '@@' LNG file */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLL                                                    AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) debug CMBTLLnn.DLL */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTDWG                                                   AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) debug CMBTDWnn.DLL */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLL_NOCALLBACKS                                        AS INTEGER NO-UNDO INIT           4 GET. /* (0x0004) */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLL_NOSTORAGE                                          AS INTEGER NO-UNDO INIT           8 GET. /* (0x0008) */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLL_NOWAITDLG                                          AS INTEGER NO-UNDO INIT          16 GET. /* (0x0010) */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLL_NOSYSINFO                                          AS INTEGER NO-UNDO INIT          32 GET. /* (0x0020) */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLL_LOGTOFILE                                          AS INTEGER NO-UNDO INIT          64 GET. /* (0x0040) */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLS                                                    AS INTEGER NO-UNDO INIT         128 GET. /* (0x0080) debug CMBTLSnn.DLL */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_PRNINFO                                                   AS INTEGER NO-UNDO INIT         256 GET. /* (0x0100) issue basic printer operations */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLL_LICINFO                                            AS INTEGER NO-UNDO INIT         512 GET. /* (0x0200) issue LIC messages */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_CMBTLL_OBJECTSTATES                                       AS INTEGER NO-UNDO INIT        1024 GET. /* (0x0400) issue object states after drawing (realdata printing only) */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_NOPRIVACYDATA                                             AS INTEGER NO-UNDO INIT        2048 GET. /* (0x0800) suppress field contents (useful if they might contain private data) */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_FORCE_SYSINFO                                             AS INTEGER NO-UNDO INIT        4096 GET. /* (0x1000) issue sysinfo even though it has been issued once already */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_EVAL2HOSTEXPRESSION                                       AS INTEGER NO-UNDO INIT        8192 GET. /* (0x2000) debug Eval2HostExpr processing */
    DEFINE PUBLIC STATIC PROPERTY LL_DEBUG_EXTENDED_DEBWINFLAGS                                      AS INTEGER NO-UNDO INIT -2147483648 GET. /* (0x80000000) internal usage */
    DEFINE PUBLIC STATIC PROPERTY LL_VERSION_MAJOR                                                   AS INTEGER NO-UNDO INIT           1 GET. /* (1) direct return of major version (f.ex. 1) */
    DEFINE PUBLIC STATIC PROPERTY LL_VERSION_MINOR                                                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) direct return of minor version (f.ex. 13) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_DRAW_USEROBJ                                               AS INTEGER NO-UNDO INIT           0 GET. /* (0) callback for LL_DRAWING_USEROBJ */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_EDIT_USEROBJ                                               AS INTEGER NO-UNDO INIT           1 GET. /* (1) callback for LL_DRAWING_USEROBJ_DLG */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_GETSIZE_USEROBJ                                            AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_GETSIZE_USEROBJ_SCM                                        AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_GETSIZE_USEROBJ_PIXEL                                      AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_TABLELINE                                                  AS INTEGER NO-UNDO INIT          10 GET. /* (10) callback for LL_CB_TABLELINE */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_LINE_HEADER                                               AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_LINE_BODY                                                 AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_LINE_FOOTER                                               AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_LINE_FILL                                                 AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_LINE_GROUP                                                AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_LINE_GROUPFOOTER                                          AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_TABLEFIELD                                                 AS INTEGER NO-UNDO INIT          11 GET. /* (11) callback for LL_CB_TABLEFIELD */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_FIELD_HEADER                                              AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_FIELD_BODY                                                AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_FIELD_FOOTER                                              AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_FIELD_FILL                                                AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_FIELD_GROUP                                               AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_FIELD_GROUPFOOTER                                         AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_EVALUATE                                                   AS INTEGER NO-UNDO INIT          12 GET. /* (12) callback for "External$" function */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_OBJECT                                                     AS INTEGER NO-UNDO INIT          20 GET. /* (20) callback of LL_CB_OBJECT */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_PAGE                                                       AS INTEGER NO-UNDO INIT          21 GET. /* (21) callback of LL_CB_PAGE */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_PROJECT                                                    AS INTEGER NO-UNDO INIT          22 GET. /* (22) callback of LL_CB_PROJECT */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_DRAW_GROUP_BEGIN                                           AS INTEGER NO-UNDO INIT          23 GET. /* (23) callback for LlPrintBeginGroup */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_DRAW_GROUP_END                                             AS INTEGER NO-UNDO INIT          24 GET. /* (24) callback for LlPrintEndGroup */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_DRAW_GROUPLINE                                             AS INTEGER NO-UNDO INIT          25 GET. /* (25) callback for LlPrintGroupLine */
    DEFINE PUBLIC STATIC PROPERTY LL_RSP_GROUP_IMT                                                   AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_RSP_GROUP_NEXTPAGE                                              AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_RSP_GROUP_OK                                                    AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_RSP_GROUP_DRAWFOOTER                                            AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_HELP                                                       AS INTEGER NO-UNDO INIT          30 GET. /* (30) lParam: HIWORD=HELP_xxx, LOWORD=Context # */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_ENABLEMENU                                                 AS INTEGER NO-UNDO INIT          31 GET. /* (31) undoc: lParam/LOWORD(lParam) = HMENU */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_MODIFYMENU                                                 AS INTEGER NO-UNDO INIT          32 GET. /* (32) undoc: lParam/LOWORD(lParam) = HMENU */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_SELECTMENU                                                 AS INTEGER NO-UNDO INIT          33 GET. /* (33) undoc: lParam=ID (return TRUE if processed) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_GETVIEWERBUTTONSTATE                                       AS INTEGER NO-UNDO INIT          34 GET. /* (34) HIWORD(lParam)=ID, LOWORD(lParam)=state */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_VARHELPTEXT                                                AS INTEGER NO-UNDO INIT          35 GET. /* (35) lParam=LPSTR(Name), returns LPSTR(Helptext) */
    DEFINE PUBLIC STATIC PROPERTY LL_INFO_METER                                                      AS INTEGER NO-UNDO INIT          37 GET. /* (37) lParam = addr(scLlMeterInfo) */
    DEFINE PUBLIC STATIC PROPERTY LL_METERJOB_LOAD                                                   AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_METERJOB_SAVE                                                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_METERJOB_CONSISTENCYCHECK                                       AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_METERJOB_PASS2                                                  AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_METERJOB_PASS3                                                  AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_FAILSFILTER                                                AS INTEGER NO-UNDO INIT        1000 GET. /* (1000) data set fails filter expression */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_VIEWERBTNCLICKED                                           AS INTEGER NO-UNDO INIT          38 GET. /* (38) user presses a preview button (action will be done). lParam=ID. result: 0=allowed, 1=not allowed */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_DLGEXPR_VARBTN                                             AS INTEGER NO-UNDO INIT          39 GET. /* (39) lParam: @scLlDlgExprVarExt, return: IDOK for ok */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_HOSTPRINTER                                                AS INTEGER NO-UNDO INIT          40 GET. /* (40) lParam: scLlPrinter */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_CREATE_DC                                                   AS INTEGER NO-UNDO INIT           1 GET. /* (1) scLlPrinter._nCmd values */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_DELETE_DC                                                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_SET_ORIENTATION                                             AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_GET_ORIENTATION                                             AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_EDIT                                                        AS INTEGER NO-UNDO INIT           5 GET. /* (5) unused */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_GET_DEVICENAME                                              AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_GET_DRIVERNAME                                              AS INTEGER NO-UNDO INIT           7 GET. /* (7) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_GET_PORTNAME                                                AS INTEGER NO-UNDO INIT           8 GET. /* (8) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_RESET_DC                                                    AS INTEGER NO-UNDO INIT           9 GET. /* (9) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_COMPARE_PRINTER                                             AS INTEGER NO-UNDO INIT          10 GET. /* (10) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_GET_PHYSPAGE                                                AS INTEGER NO-UNDO INIT          11 GET. /* (11) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_SET_PHYSPAGE                                                AS INTEGER NO-UNDO INIT          12 GET. /* (12) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_GET_PAPERFORMAT                                             AS INTEGER NO-UNDO INIT          13 GET. /* (13) fill _nPaperFormat */
    DEFINE PUBLIC STATIC PROPERTY LL_PRN_SET_PAPERFORMAT                                             AS INTEGER NO-UNDO INIT          14 GET. /* (14) _nPaperFormat, _xPaperSize, _yPaperSize */
    DEFINE PUBLIC STATIC PROPERTY LL_OEM_TOOLBAR_START                                               AS INTEGER NO-UNDO INIT          41 GET. /* (41) */
    DEFINE PUBLIC STATIC PROPERTY LL_OEM_TOOLBAR_END                                                 AS INTEGER NO-UNDO INIT          50 GET. /* (50) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_EXPRERROR                                                  AS INTEGER NO-UNDO INIT          51 GET. /* (51) lParam = LPCSTR(error text) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_CHANGE_DCPROPERTIES_CREATE                                 AS INTEGER NO-UNDO INIT          52 GET. /* (52) lParam = addr(scLlPrinter), _hDC is valid */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_CHANGE_DCPROPERTIES_DOC                                    AS INTEGER NO-UNDO INIT          53 GET. /* (53) lParam = addr(scLlPrinter), _hDC is valid */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_CHANGE_DCPROPERTIES_PAGE                                   AS INTEGER NO-UNDO INIT          54 GET. /* (54) lParam = addr(scLlPrinter), _hDC is valid */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_CHANGE_DCPROPERTIES_PREPAGE                                AS INTEGER NO-UNDO INIT          56 GET. /* (56) lParam = addr(scLlPrinter), _hDC and _pszBuffer( DEVMODE* ) are valid */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_MODIFY_METAFILE                                            AS INTEGER NO-UNDO INIT          57 GET. /* (57) lParam = handle of metafile (32 bit: enh. metafile) */
    DEFINE PUBLIC STATIC PROPERTY LL_INFO_PRINTJOBSUPERVISION                                        AS INTEGER NO-UNDO INIT          58 GET. /* (58) lParam = addr(scLlPrintJobInfo) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_DELAYEDVALUE                                               AS INTEGER NO-UNDO INIT          59 GET. /* (59) lParam = addr(scLlDelayedValue) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_SUPPLY_USERDATA                                            AS INTEGER NO-UNDO INIT          60 GET. /* (60) lParam = addr(scLlProjectUserData) */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_SAVEFILENAME                                               AS INTEGER NO-UNDO INIT          61 GET. /* (61) lParam = LPCTSTR(Filename) */
    DEFINE PUBLIC STATIC PROPERTY LL_QUERY_IS_VARIABLE_OR_FIELD                                      AS INTEGER NO-UNDO INIT          62 GET. /* (62) lParam = addr(scLlDelayDefineFieldOrVariable), must be enabled by CB mask. If returns TRUE, the var must be defined in the callback... */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_PROJECTLOADED                                              AS INTEGER NO-UNDO INIT          63 GET. /* (63) lParam = 0 */
    DEFINE PUBLIC STATIC PROPERTY LL_QUERY_DESIGNERACTIONSTATE                                       AS INTEGER NO-UNDO INIT          64 GET. /* (64) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_DESIGNERREADY                                              AS INTEGER NO-UNDO INIT          65 GET. /* (65) lParam = 0 */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_DESIGNERPRINTJOB                                           AS INTEGER NO-UNDO INIT          66 GET. /* (66) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_VIEWERDRILLDOWN                                            AS INTEGER NO-UNDO INIT          67 GET. /* (67) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_QUEST_DRILLDOWNDENIED                                      AS INTEGER NO-UNDO INIT          68 GET. /* (68) see LS_VIEWERCONTROL_QUEST_DRILLDOWNDENIED */
    DEFINE PUBLIC STATIC PROPERTY LL_QUERY_DRILLDOWN_ADDITIONAL_BASETABLES_FOR_VARIABLES             AS INTEGER NO-UNDO INIT          69 GET. /* (69) lParam = scLlDDFilterInfo* */
    DEFINE PUBLIC STATIC PROPERTY LL_QUERY_DRILLDOWN_ADDITIONAL_TABLES                               AS INTEGER NO-UNDO INIT          70 GET. /* (70) lParam = scLlDDFilterInfo* */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_DRILLDOWN_DESIGNERACTION                                   AS INTEGER NO-UNDO INIT          71 GET. /* (71) lParam = scLlDDDesignerActionW* */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_INPLACEDESIGNER_START                                      AS INTEGER NO-UNDO INIT          72 GET. /* (72) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_INPLACEDESIGNER_END                                        AS INTEGER NO-UNDO INIT          73 GET. /* (73) */
    DEFINE PUBLIC STATIC PROPERTY LL_QUERY_OWN_MENU                                                  AS INTEGER NO-UNDO INIT          74 GET. /* (74) lParam = HMENU -> return 1 if uses own menu */
    DEFINE PUBLIC STATIC PROPERTY LL_CMND_UPDATE_MENU                                                AS INTEGER NO-UNDO INIT          75 GET. /* (75) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_FRAMEHANDLE                                                AS INTEGER NO-UNDO INIT          76 GET. /* (76) lParam -> handle of layout window ("L&LFrame") */
    DEFINE PUBLIC STATIC PROPERTY LL_QUERY_DEFAULTPROJECTSTREAM                                      AS INTEGER NO-UNDO INIT          77 GET. /* (77) lParam -> IStream**. Return NONZERO when stream contains data */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_QUEST_RP_REALDATAJOBDENIED                                 AS INTEGER NO-UNDO INIT          78 GET. /* (78) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_QUEST_EXPANDABLEREGIONSJOBDENIED                           AS INTEGER NO-UNDO INIT          79 GET. /* (79) */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_QUEST_INTERACTIVESORTINGJOBDENIED                          AS INTEGER NO-UNDO INIT          80 GET. /* (80) */
    DEFINE PUBLIC STATIC PROPERTY LL_QUERY_EXPR2HOSTEXPRESSION                                       AS INTEGER NO-UNDO INIT          81 GET. /* (81) lParam = LLEXPR2HOSTEXPR*, return 0 for FAIL, 1 for OPTIMAL, 2 for INEXACT */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_REPORTPARAMETERS_COLLECTED                                 AS INTEGER NO-UNDO INIT          82 GET. /* (82) lParam = &scNtfyReportparametersCollected, return LL_ERR_USER_ABORTED to abort, 0x01 to get RP stream, 0x02 to get RP contents, 0 to leave processing */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_EXPORTERPAGEFINISHED                                       AS INTEGER NO-UNDO INIT          83 GET. /* (83) lParam = &scNtfyExporterPageFinished */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_HYPERLINK                                                  AS INTEGER NO-UNDO INIT          84 GET. /* (84) lParam = &scNtfyHyperlink */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_SAVEREPORTSTATEITEM                                        AS INTEGER NO-UNDO INIT          85 GET. /* (85) lParam = &scLLNtfyReportStateItem */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_RESTOREREPORTSTATEITEM                                     AS INTEGER NO-UNDO INIT          86 GET. /* (86) lParam = &scLLNtfyReportStateItem */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_PROGRESS                                                   AS INTEGER NO-UNDO INIT          87 GET. /* (87) lParam -> percentage of current progress */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_TRIGGEREDJOBINUITHREAD                                     AS INTEGER NO-UNDO INIT          88 GET. /* (88) lParam -> user data */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_PLEASE_TRANSLATE                                           AS INTEGER NO-UNDO INIT          89 GET. /* (89) lParam=BSTR* */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_PREVIEW_PRINT_START                                        AS INTEGER NO-UNDO INIT          99 GET. /* (99) lParam = &scViewerControlPrintData, return 1 to abort print */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_PREVIEW_PRINT_PAGE                                         AS INTEGER NO-UNDO INIT         100 GET. /* (100) lParam = &scViewerControlPrintData, return 1 to abort loop */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_PREVIEW_PRINT_END                                          AS INTEGER NO-UNDO INIT         101 GET. /* (101) lParam = &scViewerControlPrintData */
    DEFINE PUBLIC STATIC PROPERTY LL_NTFY_EMF_PAGE                                                   AS INTEGER NO-UNDO INIT         102 GET. /* (102) lParam = &scLLNtfyEMF */
    DEFINE PUBLIC STATIC PROPERTY LL_QUERY_FILENAME_FOR_EXPORTJOB                                    AS INTEGER NO-UNDO INIT         103 GET. /* (103) lParam = VARIANT* (in: old filename, out:new filename) */
    DEFINE PUBLIC STATIC PROPERTY OBJECT_LABEL                                                       AS INTEGER NO-UNDO INIT           1 GET. /* (1) old - please do not use any more */
    DEFINE PUBLIC STATIC PROPERTY OBJECT_LIST                                                        AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY OBJECT_CARD                                                        AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_PROJECT_LABEL                                                   AS INTEGER NO-UNDO INIT           1 GET. /* (1) new names... */
    DEFINE PUBLIC STATIC PROPERTY LL_PROJECT_LIST                                                    AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_PROJECT_CARD                                                    AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_PROJECT_TOC                                                     AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_PROJECT_IDX                                                     AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_PROJECT_GTC                                                     AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_PROJECT_LAST                                                    AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_PROJECT_MASK                                                    AS INTEGER NO-UNDO INIT          15 GET. /* (0x000f) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_MARKER                                                      AS INTEGER NO-UNDO INIT           0 GET. /* (0) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_TEXT                                                        AS INTEGER NO-UNDO INIT           1 GET. /* (1) the following are used in the object callback */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_RECT                                                        AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_LINE                                                        AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_BARCODE                                                     AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_DRAWING                                                     AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_TABLE                                                       AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_TEMPLATE                                                    AS INTEGER NO-UNDO INIT           7 GET. /* (7) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_ELLIPSE                                                     AS INTEGER NO-UNDO INIT           8 GET. /* (8) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_GROUP                                                       AS INTEGER NO-UNDO INIT           9 GET. /* (9) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_RTF                                                         AS INTEGER NO-UNDO INIT          10 GET. /* (10) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_LLX                                                         AS INTEGER NO-UNDO INIT          11 GET. /* (11) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_INPUT                                                       AS INTEGER NO-UNDO INIT          12 GET. /* (12) */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_LAST                                                        AS INTEGER NO-UNDO INIT          12 GET. /* (12) last object type (for loops as upper bound) - if this is changed, change the contants in object.c too! */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_REPORTCONTAINER                                             AS INTEGER NO-UNDO INIT         253 GET. /* (253) for exporter */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_PROJECT                                                     AS INTEGER NO-UNDO INIT         254 GET. /* (254) for exporter */
    DEFINE PUBLIC STATIC PROPERTY LL_OBJ_PAGE                                                        AS INTEGER NO-UNDO INIT         255 GET. /* (255) for exporter */
    DEFINE PUBLIC STATIC PROPERTY LL_DELAYEDVALUE                                                    AS INTEGER NO-UNDO INIT -2147483648 GET. /* (0x80000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_TYPEMASK                                                        AS INTEGER NO-UNDO INIT  2147418112 GET. /* (0x7fff0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_FIELDTYPEMASK                                             AS INTEGER NO-UNDO INIT       63488 GET. /* (0x0000f800) internal use */
    DEFINE PUBLIC STATIC PROPERTY LL_SUBTYPEMASK                                                     AS INTEGER NO-UNDO INIT        1023 GET. /* (0x000003ff) */
    DEFINE PUBLIC STATIC PROPERTY LL_TYPEFLAGS                                                       AS INTEGER NO-UNDO INIT -2147420160 GET. /* (0x8000f800) */
    DEFINE PUBLIC STATIC PROPERTY LL_HANDLESTREAM_TYPE_MASK                                          AS INTEGER NO-UNDO INIT        1024 GET. /* (0x00000400) */
    DEFINE PUBLIC STATIC PROPERTY LL_HANDLESTREAM_BLOB                                               AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) binary data (images only) */
    DEFINE PUBLIC STATIC PROPERTY LL_HANDLESTREAM_UTF8                                               AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) UTF8-encoded string (non-images only) */
    DEFINE PUBLIC STATIC PROPERTY LL_HANDLESTREAM_UTF16                                              AS INTEGER NO-UNDO INIT        1024 GET. /* (0x00000400) UTF16-encoded string (non-images only) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_FOOTERFIELD                                               AS INTEGER NO-UNDO INIT       32768 GET. /* (0x00008000) 'or'ed for footline-only fields // reserved also for Variables (see "$$xx$$")!!!! */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_GROUPFIELD                                                AS INTEGER NO-UNDO INIT       16384 GET. /* (0x00004000) 'or'ed for groupline-only fields */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_HEADERFIELD                                               AS INTEGER NO-UNDO INIT        8192 GET. /* (0x00002000) 'or'ed for headline-only fields */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_BODYFIELD                                                 AS INTEGER NO-UNDO INIT        4096 GET. /* (0x00001000) 'or'ed for headline-only fields */
    DEFINE PUBLIC STATIC PROPERTY LL_TABLE_GROUPFOOTERFIELD                                          AS INTEGER NO-UNDO INIT        2048 GET. /* (0x00000800) 'or'ed for group-footer-line-only fields */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE                                                         AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_METHODMASK                                              AS INTEGER NO-UNDO INIT         255 GET. /* (0x000000ff) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_WITHTEXT                                                AS INTEGER NO-UNDO INIT         256 GET. /* (0x00000100) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_WITHOUTTEXT                                             AS INTEGER NO-UNDO INIT         512 GET. /* (0x00000200) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_TEXTDONTCARE                                            AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_EAN13                                                   AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_EAN8                                                    AS INTEGER NO-UNDO INIT  1073741825 GET. /* (0x40000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GTIN13                                                  AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GTIN8                                                   AS INTEGER NO-UNDO INIT  1073741825 GET. /* (0x40000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_UPCA                                                    AS INTEGER NO-UNDO INIT  1073741826 GET. /* (0x40000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_UPCE                                                    AS INTEGER NO-UNDO INIT  1073741827 GET. /* (0x40000003) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_3OF9                                                    AS INTEGER NO-UNDO INIT  1073741828 GET. /* (0x40000004) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_25INDUSTRIAL                                            AS INTEGER NO-UNDO INIT  1073741829 GET. /* (0x40000005) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_25INTERLEAVED                                           AS INTEGER NO-UNDO INIT  1073741830 GET. /* (0x40000006) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_25DATALOGIC                                             AS INTEGER NO-UNDO INIT  1073741831 GET. /* (0x40000007) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_25MATRIX                                                AS INTEGER NO-UNDO INIT  1073741832 GET. /* (0x40000008) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_POSTNET                                                 AS INTEGER NO-UNDO INIT  1073741833 GET. /* (0x40000009) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_FIM                                                     AS INTEGER NO-UNDO INIT  1073741834 GET. /* (0x4000000A) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_CODABAR                                                 AS INTEGER NO-UNDO INIT  1073741835 GET. /* (0x4000000B) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_EAN128                                                  AS INTEGER NO-UNDO INIT  1073741836 GET. /* (0x4000000C) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GS1_128                                                 AS INTEGER NO-UNDO INIT  1073741836 GET. /* (0x4000000C) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_CODE128                                                 AS INTEGER NO-UNDO INIT  1073741837 GET. /* (0x4000000D) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_DP_LEITCODE                                             AS INTEGER NO-UNDO INIT  1073741838 GET. /* (0x4000000E) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_DP_IDENTCODE                                            AS INTEGER NO-UNDO INIT  1073741839 GET. /* (0x4000000F) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GERMAN_PARCEL                                           AS INTEGER NO-UNDO INIT  1073741840 GET. /* (0x40000010) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_CODE93                                                  AS INTEGER NO-UNDO INIT  1073741841 GET. /* (0x40000011) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_MSI                                                     AS INTEGER NO-UNDO INIT  1073741842 GET. /* (0x40000012) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_CODE11                                                  AS INTEGER NO-UNDO INIT  1073741843 GET. /* (0x40000013) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_MSI_10_CD                                               AS INTEGER NO-UNDO INIT  1073741844 GET. /* (0x40000014) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_MSI_10_10                                               AS INTEGER NO-UNDO INIT  1073741845 GET. /* (0x40000015) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_MSI_11_10                                               AS INTEGER NO-UNDO INIT  1073741846 GET. /* (0x40000016) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_MSI_PLAIN                                               AS INTEGER NO-UNDO INIT  1073741847 GET. /* (0x40000017) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_EAN14                                                   AS INTEGER NO-UNDO INIT  1073741848 GET. /* (0x40000018) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GTIN14                                                  AS INTEGER NO-UNDO INIT  1073741848 GET. /* (0x40000018) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_UCC14                                                   AS INTEGER NO-UNDO INIT  1073741849 GET. /* (0x40000019) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_CODE39                                                  AS INTEGER NO-UNDO INIT  1073741850 GET. /* (0x4000001A) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_CODE39_CRC43                                            AS INTEGER NO-UNDO INIT  1073741851 GET. /* (0x4000001B) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_PZN                                                     AS INTEGER NO-UNDO INIT  1073741852 GET. /* (0x4000001C) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_CODE39_EXT                                              AS INTEGER NO-UNDO INIT  1073741853 GET. /* (0x4000001D) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_JAPANESE_POSTAL                                         AS INTEGER NO-UNDO INIT  1073741854 GET. /* (0x4000001E) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_RM4SCC                                                  AS INTEGER NO-UNDO INIT  1073741855 GET. /* (0x4000001F) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_RM4SCC_CRC                                              AS INTEGER NO-UNDO INIT  1073741856 GET. /* (0x40000020) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_SSCC                                                    AS INTEGER NO-UNDO INIT  1073741857 GET. /* (0x40000021) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_ISBN                                                    AS INTEGER NO-UNDO INIT  1073741858 GET. /* (0x40000022) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GS1                                                     AS INTEGER NO-UNDO INIT  1073741859 GET. /* (0x40000023) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GS1_TRUNCATED                                           AS INTEGER NO-UNDO INIT  1073741860 GET. /* (0x40000024) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GS1_STACKED                                             AS INTEGER NO-UNDO INIT  1073741861 GET. /* (0x40000025) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GS1_STACKED_OMNI                                        AS INTEGER NO-UNDO INIT  1073741862 GET. /* (0x40000026) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GS1_LIMITED                                             AS INTEGER NO-UNDO INIT  1073741863 GET. /* (0x40000027) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_GS1_EXPANDED                                            AS INTEGER NO-UNDO INIT  1073741864 GET. /* (0x40000028) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_INTELLIGENT_MAIL                                        AS INTEGER NO-UNDO INIT  1073741865 GET. /* (0x40000029) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_PZN8                                                    AS INTEGER NO-UNDO INIT  1073741866 GET. /* (0x4000002A) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_CODE128_FULL                                            AS INTEGER NO-UNDO INIT  1073741867 GET. /* (0x4000002B) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_EAN128_FULL                                             AS INTEGER NO-UNDO INIT  1073741868 GET. /* (0x4000002C) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_LLXSTART                                                AS INTEGER NO-UNDO INIT  1073741888 GET. /* (0x40000040) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_PDF417                                                  AS INTEGER NO-UNDO INIT  1073741888 GET. /* (0x40000040) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_MAXICODE                                                AS INTEGER NO-UNDO INIT  1073741889 GET. /* (0x40000041) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_MAXICODE_UPS                                            AS INTEGER NO-UNDO INIT  1073741890 GET. /* (0x40000042) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_DATAMATRIX                                              AS INTEGER NO-UNDO INIT  1073741892 GET. /* (0x40000044) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_AZTEC                                                   AS INTEGER NO-UNDO INIT  1073741893 GET. /* (0x40000045) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_QRCODE                                                  AS INTEGER NO-UNDO INIT  1073741894 GET. /* (0x40000046) */
    DEFINE PUBLIC STATIC PROPERTY LL_BARCODE_DATAMATRIX_PREMIUMADRESS                                AS INTEGER NO-UNDO INIT  1073741895 GET. /* (0x40000047) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING                                                         AS INTEGER NO-UNDO INIT   536870912 GET. /* (0x20000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING_METHODMASK                                              AS INTEGER NO-UNDO INIT         255 GET. /* (0x000000ff) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING_HMETA                                                   AS INTEGER NO-UNDO INIT   536870913 GET. /* (0x20000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING_USEROBJ                                                 AS INTEGER NO-UNDO INIT   536870914 GET. /* (0x20000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING_USEROBJ_DLG                                             AS INTEGER NO-UNDO INIT   536870915 GET. /* (0x20000003) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING_HBITMAP                                                 AS INTEGER NO-UNDO INIT   536870916 GET. /* (0x20000004) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING_HICON                                                   AS INTEGER NO-UNDO INIT   536870917 GET. /* (0x20000005) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING_HEMETA                                                  AS INTEGER NO-UNDO INIT   536870918 GET. /* (0x20000006) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRAWING_HDIB                                                    AS INTEGER NO-UNDO INIT   536870919 GET. /* (0x20000007) global handle to BITMAPINFO and bits */
    DEFINE PUBLIC STATIC PROPERTY LL_META_MAXX                                                       AS INTEGER NO-UNDO INIT       10000 GET. /* (10000) */
    DEFINE PUBLIC STATIC PROPERTY LL_META_MAXY                                                       AS INTEGER NO-UNDO INIT       10000 GET. /* (10000) */
    DEFINE PUBLIC STATIC PROPERTY LL_TEXT                                                            AS INTEGER NO-UNDO INIT   268435456 GET. /* (0x10000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_TEXT_ALLOW_WORDWRAP                                             AS INTEGER NO-UNDO INIT   268435456 GET. /* (0x10000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_TEXT_DENY_WORDWRAP                                              AS INTEGER NO-UNDO INIT   268435457 GET. /* (0x10000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_TEXT_FORCE_WORDWRAP                                             AS INTEGER NO-UNDO INIT   268435458 GET. /* (0x10000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_NUMERIC                                                         AS INTEGER NO-UNDO INIT   134217728 GET. /* (0x08000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_NUMERIC_LOCALIZED                                               AS INTEGER NO-UNDO INIT   134217729 GET. /* (0x08000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_NUMERIC_INTEGER                                                 AS INTEGER NO-UNDO INIT   134217730 GET. /* (0x08000002) flag */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE                                                            AS INTEGER NO-UNDO INIT    67108864 GET. /* (0x04000000) LL's own julian */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_METHODMASK                                                 AS INTEGER NO-UNDO INIT         255 GET. /* (0x000000ff) */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_DELPHI_1                                                   AS INTEGER NO-UNDO INIT    67108865 GET. /* (0x04000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_DELPHI                                                     AS INTEGER NO-UNDO INIT    67108866 GET. /* (0x04000002) DELPHI 2, 3, 4: OLE DATE */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_MS                                                         AS INTEGER NO-UNDO INIT    67108866 GET. /* (0x04000002) MS C/Basic: OLE DATE */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_OLE                                                        AS INTEGER NO-UNDO INIT    67108866 GET. /* (0x04000002) generic: OLE DATE */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_VFOXPRO                                                    AS INTEGER NO-UNDO INIT    67108867 GET. /* (0x04000003) nearly LL's own julian, has an offset of 1! */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_DMY                                                        AS INTEGER NO-UNDO INIT    67108868 GET. /* (0x04000004) <d><sep><m><sep><yyyy>. Year MUST be 4 digits! */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_MDY                                                        AS INTEGER NO-UNDO INIT    67108869 GET. /* (0x04000005) <m><sep><d><sep><yyyy>. Year MUST be 4 digits! */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_YMD                                                        AS INTEGER NO-UNDO INIT    67108870 GET. /* (0x04000006) <yyyy><sep><m><sep><d>. Year MUST be 4 digits! */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_YYYYMMDD                                                   AS INTEGER NO-UNDO INIT    67108871 GET. /* (0x04000007) <yyyymmdd> */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_LOCALIZED                                                  AS INTEGER NO-UNDO INIT    67108872 GET. /* (0x04000008) localized (automatic VariantConversion) */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_JULIAN                                                     AS INTEGER NO-UNDO INIT    67108873 GET. /* (0x04000009) variant 'date' is a julian date */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_CLARION                                                    AS INTEGER NO-UNDO INIT    67108874 GET. /* (0x0400000a) days since 1800-12-28 (what's so special about that day?) */
    DEFINE PUBLIC STATIC PROPERTY LL_DATE_YMD_AUTO                                                   AS INTEGER NO-UNDO INIT    67108880 GET. /* (0x04000010) wither DMY, MDY or YMD, automatically detected */
    DEFINE PUBLIC STATIC PROPERTY LL_BOOLEAN                                                         AS INTEGER NO-UNDO INIT    33554432 GET. /* (0x02000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_RTF                                                             AS INTEGER NO-UNDO INIT    16777216 GET. /* (0x01000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_HTML                                                            AS INTEGER NO-UNDO INIT     8388608 GET. /* (0x00800000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PDF                                                             AS INTEGER NO-UNDO INIT     4194304 GET. /* (0x00400000) */
    DEFINE PUBLIC STATIC PROPERTY LL_INPUTOBJECT                                                     AS INTEGER NO-UNDO INIT     2097152 GET. /* (0x00200000) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_LLXOBJECT                                                       AS INTEGER NO-UNDO INIT     1048576 GET. /* (0x00100000) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_SUBTABLELIST                                                    AS INTEGER NO-UNDO INIT      524288 GET. /* (0x00080000) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_FIXEDNAME                                                       AS INTEGER NO-UNDO INIT       32768 GET. /* (0x00008000) */
    DEFINE PUBLIC STATIC PROPERTY LL_NOSAVEAS                                                        AS INTEGER NO-UNDO INIT       16384 GET. /* (0x00004000) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRCONVERTQUIET                                                AS INTEGER NO-UNDO INIT        4096 GET. /* (0x00001000) convert to new expressions without warning box */
    DEFINE PUBLIC STATIC PROPERTY LL_NONAMEINTITLE                                                   AS INTEGER NO-UNDO INIT        2048 GET. /* (0x00000800) no file name appended to title */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNER_OVER_CHILD                                             AS INTEGER NO-UNDO INIT        8192 GET. /* (0x00002000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRVOPT_PRN_USEDEFAULT                                           AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRVOPT_PRN_ASKPRINTERIFNEEDED                                   AS INTEGER NO-UNDO INIT           1 GET. /* (0x00000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRVOPT_PRN_ASKPRINTERALWAYS                                     AS INTEGER NO-UNDO INIT           2 GET. /* (0x00000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRVOPT_PRN_ALWAYSUSEDEFAULT                                     AS INTEGER NO-UNDO INIT           3 GET. /* (0x00000003) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRVOPT_PRN_ASSIGNMASK                                           AS INTEGER NO-UNDO INIT           3 GET. /* (0x00000003) used by L&L */
    DEFINE PUBLIC STATIC PROPERTY LL_PRVOPT_FLAG_STANDALONEVIEWER                                    AS INTEGER NO-UNDO INIT          16 GET. /* (0x00000010) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_COPIES                                                   AS INTEGER NO-UNDO INIT           0 GET. /* (0) compatibility only, please use LL_PRNOPT_... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_STARTPAGE                                                AS INTEGER NO-UNDO INIT           1 GET. /* (1) compatibility only, please use LL_PRNOPT_PAGE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PAGE                                                     AS INTEGER NO-UNDO INIT           1 GET. /* (1) compatibility only, please use LL_PRNOPT_... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_OFFSET                                                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) compatibility only, please use LL_PRNOPT_... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_COPIES_SUPPORTED                                         AS INTEGER NO-UNDO INIT           3 GET. /* (3) compatibility only, please use LL_PRNOPT_... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FIRSTPAGE                                                AS INTEGER NO-UNDO INIT           5 GET. /* (5) compatibility only, please use LL_PRNOPT_... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_LASTPAGE                                                 AS INTEGER NO-UNDO INIT           6 GET. /* (6) compatibility only, please use LL_PRNOPT_... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_JOBPAGES                                                 AS INTEGER NO-UNDO INIT           7 GET. /* (7) compatibility only, please use LL_PRNOPT_... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRINTORDER                                               AS INTEGER NO-UNDO INIT           8 GET. /* (8) compatibility only, please use LL_PRNOPT_... */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_COPIES                                                   AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_COPIES_HIDE                                                     AS INTEGER NO-UNDO INIT      -32768 GET. /* (-32768) anything negative... */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_STARTPAGE                                                AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_PAGE                                                     AS INTEGER NO-UNDO INIT           1 GET. /* (1) alias; please do not use STARTPAGE any more... */
    DEFINE PUBLIC STATIC PROPERTY LL_PAGE_HIDE                                                       AS INTEGER NO-UNDO INIT      -32768 GET. /* (-32768) must be exactly this value! */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_OFFSET                                                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_COPIES_SUPPORTED                                         AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_UNITS                                                    AS INTEGER NO-UNDO INIT           4 GET. /* (4) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_UNITS_MM_DIV_10                                                 AS INTEGER NO-UNDO INIT           0 GET. /* (0) for LL_PRNOPT_UNITS, LL_OPTION_UNITS and LL_OPTION_UNITS_DEFAULT */
    DEFINE PUBLIC STATIC PROPERTY LL_UNITS_INCH_DIV_100                                              AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_UNITS_INCH_DIV_1000                                             AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_UNITS_SYSDEFAULT_LORES                                          AS INTEGER NO-UNDO INIT           3 GET. /* (3) mm/10, in/100 (depending on regional settings of the system) */
    DEFINE PUBLIC STATIC PROPERTY LL_UNITS_SYSDEFAULT                                                AS INTEGER NO-UNDO INIT           4 GET. /* (4) mm/100, in/1000 (depending on regional settings of the system) */
    DEFINE PUBLIC STATIC PROPERTY LL_UNITS_MM_DIV_100                                                AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_UNITS_MM_DIV_1000                                               AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_UNITS_SYSDEFAULT_HIRES                                          AS INTEGER NO-UNDO INIT           7 GET. /* (7) mm/100, in/1000 (depending on regional settings of the system) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_FIRSTPAGE                                                AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_LASTPAGE                                                 AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_JOBPAGES                                                 AS INTEGER NO-UNDO INIT           7 GET. /* (7) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_PRINTORDER                                               AS INTEGER NO-UNDO INIT           8 GET. /* (8) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINTORDER_HORZ_LTRB                                            AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINTORDER_VERT_LTRB                                            AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINTORDER_HORZ_RBLT                                            AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINTORDER_VERT_RBLT                                            AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_DEFPRINTERINSTALLED                                      AS INTEGER NO-UNDO INIT          11 GET. /* (11) returns 0 for no default printer, 1 for default printer present */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_PRINTDLG_DESTMASK                                        AS INTEGER NO-UNDO INIT          12 GET. /* (12) any combination of the ones below... Default: all. Outdated, please use LL_OPTIONSTR_EXPORTS_ALLOWED */
    DEFINE PUBLIC STATIC PROPERTY LL_DESTINATION_PRN                                                 AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESTINATION_PRV                                                 AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESTINATION_FILE                                                AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESTINATION_EXTERN                                              AS INTEGER NO-UNDO INIT           8 GET. /* (8) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESTINATION_MSFAX                                               AS INTEGER NO-UNDO INIT          16 GET. /* (16) reserved */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_PRINTDLG_DEST                                            AS INTEGER NO-UNDO INIT          13 GET. /* (13) default destination; outdated, please use LL_PRNOPTSTR_EXPORT */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_PRINTDLG_ONLYPRINTERCOPIES                               AS INTEGER NO-UNDO INIT          14 GET. /* (14) show copies option in dialog only if they are supported by the printer. default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_JOBID                                                    AS INTEGER NO-UNDO INIT          17 GET. /* (17) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_PAGEINDEX                                                AS INTEGER NO-UNDO INIT          18 GET. /* (18) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_USES2PASS                                                AS INTEGER NO-UNDO INIT          19 GET. /* (19) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_PAGERANGE_USES_ABSOLUTENUMBER                            AS INTEGER NO-UNDO INIT          20 GET. /* (20) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_USEMEMORYMETAFILE                                        AS INTEGER NO-UNDO INIT          22 GET. /* (22) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_PARTIALPREVIEW                                           AS INTEGER NO-UNDO INIT          23 GET. /* (23) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_ADDITIONALPAGES_FOR_TOTAL                                AS INTEGER NO-UNDO INIT          24 GET. /* (24) internal */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPT_HAS_TOTALPAGES                                           AS INTEGER NO-UNDO INIT          25 GET. /* (25) internal */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_PRINTDST_FILENAME                                     AS INTEGER NO-UNDO INIT           0 GET. /* (0) print to file: default filename (LlGet/SetPrintOptionString) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_EXPORTDESCR                                           AS INTEGER NO-UNDO INIT           1 GET. /* (1) r/o, returns the description of the export chosen */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_EXPORT                                                AS INTEGER NO-UNDO INIT           2 GET. /* (2) sets default exporter to use / returns the name of the export chosen */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_PRINTJOBNAME                                          AS INTEGER NO-UNDO INIT           3 GET. /* (3) set name to be given to StartDoc() (lpszMessage of LlPrintWithBoxStart() */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_PRESTARTDOCESCSTRING                                  AS INTEGER NO-UNDO INIT           4 GET. /* (4) sent before StartDoc() */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_POSTENDDOCESCSTRING                                   AS INTEGER NO-UNDO INIT           5 GET. /* (5) sent after EndDoc() */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_PRESTARTPAGEESCSTRING                                 AS INTEGER NO-UNDO INIT           6 GET. /* (6) sent before StartPage() */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_POSTENDPAGEESCSTRING                                  AS INTEGER NO-UNDO INIT           7 GET. /* (7) sent after EndPage() */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_PRESTARTPROJECTESCSTRING                              AS INTEGER NO-UNDO INIT           8 GET. /* (8) sent before first StartPage() of project */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_POSTENDPROJECTESCSTRING                               AS INTEGER NO-UNDO INIT           9 GET. /* (9) sent after last EndPage() of project */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_PAGERANGES                                            AS INTEGER NO-UNDO INIT          10 GET. /* (10) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_ISSUERANGES                                           AS INTEGER NO-UNDO INIT          11 GET. /* (11) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRNOPTSTR_PREVIEWTITLE                                          AS INTEGER NO-UNDO INIT          12 GET. /* (12) default: language dependent */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_V1POINTX                                                  AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_NORMAL                                                    AS INTEGER NO-UNDO INIT         256 GET. /* (0x00000100) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_PREVIEW                                                   AS INTEGER NO-UNDO INIT         512 GET. /* (0x00000200) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_STORAGE                                                   AS INTEGER NO-UNDO INIT         512 GET. /* (0x00000200) same as LL_PRINT_PREVIEW */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_FILE                                                      AS INTEGER NO-UNDO INIT        1024 GET. /* (0x00000400) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_USERSELECT                                                AS INTEGER NO-UNDO INIT        2048 GET. /* (0x00000800) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_EXPORT                                                    AS INTEGER NO-UNDO INIT        2048 GET. /* (0x00000800) same as LL_PRINT_USERSELECT */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_MODEMASK                                                  AS INTEGER NO-UNDO INIT        3840 GET. /* (0x00000f00) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_MULTIPLE_JOBS                                             AS INTEGER NO-UNDO INIT        4096 GET. /* (0x00001000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_KEEPJOB                                                   AS INTEGER NO-UNDO INIT        8192 GET. /* (0x00002000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_IS_DOM_CALLER                                             AS INTEGER NO-UNDO INIT       16384 GET. /* (0x00004000) internal */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_DOM_NOCREATEDC                                            AS INTEGER NO-UNDO INIT       65536 GET. /* (0x00010000) internal */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_DOM_NOOBJECTLOAD                                          AS INTEGER NO-UNDO INIT      131072 GET. /* (0x00020000) internal */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINT_REMOVE_UNUSED_VARS                                        AS INTEGER NO-UNDO INIT       32768 GET. /* (0x00008000) optimization flag */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_BOXTYPEMASK                                             AS INTEGER NO-UNDO INIT         255 GET. /* (0x000000ff) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_NONE                                                    AS INTEGER NO-UNDO INIT         255 GET. /* (0x000000ff) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_FLAG_ALLOWSUSPEND                                       AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_FLAG_USEMARQUEE                                         AS INTEGER NO-UNDO INIT -2147483648 GET. /* (0x80000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_NORMALMETER                                             AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_BRIDGEMETER                                             AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_NORMALWAIT                                              AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_BRIDGEWAIT                                              AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_EMPTYWAIT                                               AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_EMPTYABORT                                              AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_STDWAIT                                                 AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_STDABORT                                                AS INTEGER NO-UNDO INIT           7 GET. /* (7) */
    DEFINE PUBLIC STATIC PROPERTY LL_BOXTYPE_MAX                                                     AS INTEGER NO-UNDO INIT           7 GET. /* (7) */
    DEFINE PUBLIC STATIC PROPERTY LL_FILE_ALSONEW                                                    AS INTEGER NO-UNDO INIT       32768 GET. /* (0x8000) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_DOUBLE                                              AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_DATE                                                AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_STRING                                              AS INTEGER NO-UNDO INIT           4 GET. /* (0x0004) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_BOOL                                                AS INTEGER NO-UNDO INIT           8 GET. /* (0x0008) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_DRAWING                                             AS INTEGER NO-UNDO INIT          16 GET. /* (0x0010) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_BARCODE                                             AS INTEGER NO-UNDO INIT          32 GET. /* (0x0020) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_ALL                                                 AS INTEGER NO-UNDO INIT          63 GET. /* (0x003f) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_PARA1                                               AS INTEGER NO-UNDO INIT       32769 GET. /* (0x8001) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_PARA2                                               AS INTEGER NO-UNDO INIT       32770 GET. /* (0x8002) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_PARA3                                               AS INTEGER NO-UNDO INIT       32771 GET. /* (0x8003) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_PARA4                                               AS INTEGER NO-UNDO INIT       32772 GET. /* (0x8004) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_SAME                                                AS INTEGER NO-UNDO INIT       32831 GET. /* (0x803f) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPE_MASK                                                AS INTEGER NO-UNDO INIT       36863 GET. /* (0x8fff) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPEFLAG_NONULLCHECK                                     AS INTEGER NO-UNDO INIT       65536 GET. /* (0x00010000) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPEFLAG_MULTIDIM_ALLOWED                                AS INTEGER NO-UNDO INIT      131072 GET. /* (0x00020000) internal */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPEFLAG_RAW                                             AS INTEGER NO-UNDO INIT      524288 GET. /* (0x00080000) parameter passed as string without evaluation */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPEFLAG_RAW_WITH_SYNTAXCHECK                            AS INTEGER NO-UNDO INIT      262144 GET. /* (0x00040000) parameter passed as string without evaluation */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPEFLAG_KEEP_LINEBREAKS                                 AS INTEGER NO-UNDO INIT     1048576 GET. /* (0x00100000) */
    DEFINE PUBLIC STATIC PROPERTY LL_FCTPARATYPEFLAG_EXECUTE_ON_SYNTAXCHECK_AT_LOADTIME              AS INTEGER NO-UNDO INIT     2097152 GET. /* (0x00200000) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRTYPE_DOUBLE                                                 AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRTYPE_DATE                                                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRTYPE_STRING                                                 AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRTYPE_BOOL                                                   AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRTYPE_DRAWING                                                AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRTYPE_BARCODE                                                AS INTEGER NO-UNDO INIT           6 GET. /* (6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NEWEXPRESSIONS                                           AS INTEGER NO-UNDO INIT           0 GET. /* (0) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ONLYONETABLE                                             AS INTEGER NO-UNDO INIT           1 GET. /* (1) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TABLE_COLORING                                           AS INTEGER NO-UNDO INIT           2 GET. /* (2) default: LL_COLORING_LL */
    DEFINE PUBLIC STATIC PROPERTY LL_COLORING_LL                                                     AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_COLORING_PROGRAM                                                AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_COLORING_DONTCARE                                               AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPERVISOR                                               AS INTEGER NO-UNDO INIT           3 GET. /* (3) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_UNITS                                                    AS INTEGER NO-UNDO INIT           4 GET. /* (4) default: see LL_OPTION_METRIC */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TABSTOPS                                                 AS INTEGER NO-UNDO INIT           5 GET. /* (5) default: LL_TABS_DELETE */
    DEFINE PUBLIC STATIC PROPERTY LL_TABS_DELETE                                                     AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_TABS_EXPAND                                                     AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CALLBACKMASK                                             AS INTEGER NO-UNDO INIT           6 GET. /* (6) default: 0x00000000 */
    DEFINE PUBLIC STATIC PROPERTY LL_CB_PAGE                                                         AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) callback for each page */
    DEFINE PUBLIC STATIC PROPERTY LL_CB_PROJECT                                                      AS INTEGER NO-UNDO INIT   536870912 GET. /* (0x20000000) callback for each label */
    DEFINE PUBLIC STATIC PROPERTY LL_CB_OBJECT                                                       AS INTEGER NO-UNDO INIT   268435456 GET. /* (0x10000000) callback for each object */
    DEFINE PUBLIC STATIC PROPERTY LL_CB_HELP                                                         AS INTEGER NO-UNDO INIT   134217728 GET. /* (0x08000000) callback for HELP (F1/Button) */
    DEFINE PUBLIC STATIC PROPERTY LL_CB_TABLELINE                                                    AS INTEGER NO-UNDO INIT    67108864 GET. /* (0x04000000) callback for table line */
    DEFINE PUBLIC STATIC PROPERTY LL_CB_TABLEFIELD                                                   AS INTEGER NO-UNDO INIT    33554432 GET. /* (0x02000000) callback for table field */
    DEFINE PUBLIC STATIC PROPERTY LL_CB_QUERY_IS_VARIABLE_OR_FIELD                                   AS INTEGER NO-UNDO INIT    16777216 GET. /* (0x01000000) callback for delayload (LL_QUERY_IS_VARIABLE_OR_FIELD) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CALLBACKPARAMETER                                        AS INTEGER NO-UNDO INIT           7 GET. /* (7) default: 0 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_HELPAVAILABLE                                            AS INTEGER NO-UNDO INIT           8 GET. /* (8) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SORTVARIABLES                                            AS INTEGER NO-UNDO INIT           9 GET. /* (9) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPPORTPAGEBREAK                                         AS INTEGER NO-UNDO INIT          10 GET. /* (10) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SHOWPREDEFVARS                                           AS INTEGER NO-UNDO INIT          11 GET. /* (11) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USEHOSTPRINTER                                           AS INTEGER NO-UNDO INIT          13 GET. /* (13) default: false // use host printer via callback */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_EXTENDEDEVALUATION                                       AS INTEGER NO-UNDO INIT          14 GET. /* (14) allows expressions in chevrons (amwin mode) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TABREPRESENTATIONCODE                                    AS INTEGER NO-UNDO INIT          15 GET. /* (15) default: 247 (0xf7) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SHOWSTATE                                                AS INTEGER NO-UNDO INIT          16 GET. /* (16) r/o, */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_METRIC                                                   AS INTEGER NO-UNDO INIT          18 GET. /* (18) default: depends on Windows defaults */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ADDVARSTOFIELDS                                          AS INTEGER NO-UNDO INIT          19 GET. /* (19) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MULTIPLETABLELINES                                       AS INTEGER NO-UNDO INIT          20 GET. /* (20) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CONVERTCRLF                                              AS INTEGER NO-UNDO INIT          21 GET. /* (21) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_WIZ_FILENEW                                              AS INTEGER NO-UNDO INIT          22 GET. /* (22) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RETREPRESENTATIONCODE                                    AS INTEGER NO-UNDO INIT          23 GET. /* (23) default: LL_CHAR_NEWLINE (182) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVZOOM_PERC                                             AS INTEGER NO-UNDO INIT          25 GET. /* (25) initial preview zoom */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVRECT_LEFT                                             AS INTEGER NO-UNDO INIT          26 GET. /* (26) initial preview position */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVRECT_TOP                                              AS INTEGER NO-UNDO INIT          27 GET. /* (27) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVRECT_WIDTH                                            AS INTEGER NO-UNDO INIT          28 GET. /* (28) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVRECT_HEIGHT                                           AS INTEGER NO-UNDO INIT          29 GET. /* (29) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_STORAGESYSTEM                                            AS INTEGER NO-UNDO INIT          30 GET. /* (30) DEPRECATED. Do not change. 0=LX4-compatible, 1=STORAGE (default) */
    DEFINE PUBLIC STATIC PROPERTY LL_STG_COMPAT4                                                     AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_STG_STORAGE                                                     AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_COMPRESSSTORAGE                                          AS INTEGER NO-UNDO INIT          31 GET. /* (31) 0, 1, 10..17 */
    DEFINE PUBLIC STATIC PROPERTY LL_STG_COMPRESS_THREADED                                           AS INTEGER NO-UNDO INIT       32768 GET. /* (0x00008000) */
    DEFINE PUBLIC STATIC PROPERTY LL_STG_COMPRESS_UNTHREADED                                         AS INTEGER NO-UNDO INIT       65536 GET. /* (0x00010000) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOPARAMETERCHECK                                         AS INTEGER NO-UNDO INIT          32 GET. /* (32) you need a bit more speed? */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NONOTABLECHECK                                           AS INTEGER NO-UNDO INIT          33 GET. /* (33) don't check on "NO_TABLEOBJECT" error. Default TRUE (don't check) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DRAWFOOTERLINEONPRINT                                    AS INTEGER NO-UNDO INIT          34 GET. /* (34) delay footerline printing to LlPrint(). Default FALSE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVZOOM_LEFT                                             AS INTEGER NO-UNDO INIT          35 GET. /* (35) initial preview position in percent of screen */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVZOOM_TOP                                              AS INTEGER NO-UNDO INIT          36 GET. /* (36) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVZOOM_WIDTH                                            AS INTEGER NO-UNDO INIT          37 GET. /* (37) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVZOOM_HEIGHT                                           AS INTEGER NO-UNDO INIT          38 GET. /* (38) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SPACEOPTIMIZATION                                        AS INTEGER NO-UNDO INIT          40 GET. /* (40) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_REALTIME                                                 AS INTEGER NO-UNDO INIT          41 GET. /* (41) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_AUTOMULTIPAGE                                            AS INTEGER NO-UNDO INIT          42 GET. /* (42) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USEBARCODESIZES                                          AS INTEGER NO-UNDO INIT          43 GET. /* (43) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MAXRTFVERSION                                            AS INTEGER NO-UNDO INIT          44 GET. /* (44) default: 0xff00 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARSCASESENSITIVE                                        AS INTEGER NO-UNDO INIT          46 GET. /* (46) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DELAYTABLEHEADER                                         AS INTEGER NO-UNDO INIT          47 GET. /* (47) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_OFNDIALOG_EXPLORER                                       AS INTEGER NO-UNDO INIT          48 GET. /* (48) DEPRECATED. Do not change. */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_OFN_NOPLACESBAR                                          AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_EMFRESOLUTION                                            AS INTEGER NO-UNDO INIT          49 GET. /* (49) DEPRECATED. Do not change. */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SETCREATIONINFO                                          AS INTEGER NO-UNDO INIT          50 GET. /* (50) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_XLATVARNAMES                                             AS INTEGER NO-UNDO INIT          51 GET. /* (51) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_LANGUAGE                                                 AS INTEGER NO-UNDO INIT          52 GET. /* (52) returns current language (r/o) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PHANTOMSPACEREPRESENTATIONCODE                           AS INTEGER NO-UNDO INIT          54 GET. /* (54) default: LL_CHAR_PHANTOMSPACE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_LOCKNEXTCHARREPRESENTATIONCODE                           AS INTEGER NO-UNDO INIT          55 GET. /* (55) default: LL_CHAR_LOCK */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_EXPRSEPREPRESENTATIONCODE                                AS INTEGER NO-UNDO INIT          56 GET. /* (56) default: LL_CHAR_EXPRSEP */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DEFPRINTERINSTALLED                                      AS INTEGER NO-UNDO INIT          57 GET. /* (57) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CALCSUMVARSONINVISIBLELINES                              AS INTEGER NO-UNDO INIT          58 GET. /* (58) default: false - only default value if no preferences in project */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOFOOTERPAGEWRAP                                         AS INTEGER NO-UNDO INIT          59 GET. /* (59) default: false - only default value if no preferences in project */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_IMMEDIATELASTPAGE                                        AS INTEGER NO-UNDO INIT          64 GET. /* (64) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_LCID                                                     AS INTEGER NO-UNDO INIT          65 GET. /* (65) default: LOCALE_USER_DEFAULT */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TEXTQUOTEREPRESENTATIONCODE                              AS INTEGER NO-UNDO INIT          66 GET. /* (66) default: 1 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SCALABLEFONTSONLY                                        AS INTEGER NO-UNDO INIT          67 GET. /* (67) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOTIFICATIONMESSAGEHWND                                  AS INTEGER NO-UNDO INIT          68 GET. /* (68) default: NULL (parent window handle) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DEFDEFFONT                                               AS INTEGER NO-UNDO INIT          69 GET. /* (69) default: GetStockObject(ANSI_VAR_FONT) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CODEPAGE                                                 AS INTEGER NO-UNDO INIT          70 GET. /* (70) default: CP_ACP; set codepage to use for conversions. */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCEFONTCHARSET                                         AS INTEGER NO-UNDO INIT          71 GET. /* (71) default: false; set font's charset to the codepage according to LL_OPTION_LCID. Default: FALSE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_COMPRESSRTF                                              AS INTEGER NO-UNDO INIT          72 GET. /* (72) default: true; compress RTF text > 1024 bytes in project file */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ALLOW_LLX_EXPORTERS                                      AS INTEGER NO-UNDO INIT          74 GET. /* (74) default: true; allow ILlXExport extensions */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPPORTS_PRNOPTSTR_EXPORT                                AS INTEGER NO-UNDO INIT          75 GET. /* (75) default: false: hides "set to default" button in "export option" tab in designer */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DEBUGFLAG                                                AS INTEGER NO-UNDO INIT          76 GET. /* (76) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SKIPRETURNATENDOFRTF                                     AS INTEGER NO-UNDO INIT          77 GET. /* (77) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_INTERCHARSPACING                                         AS INTEGER NO-UNDO INIT          78 GET. /* (78) default: false: allows character interspacing in case of block justify */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_INCLUDEFONTDESCENT                                       AS INTEGER NO-UNDO INIT          79 GET. /* (79) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESOLUTIONCOMPATIBLETO9X                                 AS INTEGER NO-UNDO INIT          80 GET. /* (80) DEPRECATED. default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USECHARTFIELDS                                           AS INTEGER NO-UNDO INIT          81 GET. /* (81) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_OFNDIALOG_NOPLACESBAR                                    AS INTEGER NO-UNDO INIT          82 GET. /* (82) default: false; do not use "Places" bar in NT2K? */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SKETCH_COLORDEPTH                                        AS INTEGER NO-UNDO INIT          83 GET. /* (83) default: 24 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FINAL_TRUE_ON_LASTPAGE                                   AS INTEGER NO-UNDO INIT          84 GET. /* (84) default: false: internal use */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_INTERCHARSPACING_FORCED                                  AS INTEGER NO-UNDO INIT          86 GET. /* (86) default: false: forces character interspacing calculation in TEXT objects (possibly dangerous and slow) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RTFAUTOINCREMENT                                         AS INTEGER NO-UNDO INIT          87 GET. /* (87) default: false, to increment RTF char pointer if nothing can be printed */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_UNITS_DEFAULT                                            AS INTEGER NO-UNDO INIT          88 GET. /* (88) default: LL_OPTION_UNITS_SYSDEFAULT. Use for contols that query the units, where we need to return "sysdefault" also */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_MAPI                                                  AS INTEGER NO-UNDO INIT          89 GET. /* (89) default: false. Inhibit MAPI load for preview */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLE                                             AS INTEGER NO-UNDO INIT          90 GET. /* (90) default: LL_OPTION_TOOLBARSTYLE_STANDARD|LL_OPTION_TOOLBARSTYLEFLAG_DOCKABLE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLE_STANDARD                                    AS INTEGER NO-UNDO INIT           0 GET. /* (0) OFFICE97 alike style */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLE_OFFICEXP                                    AS INTEGER NO-UNDO INIT           1 GET. /* (1) DOTNET/OFFICE_XP alike style */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLE_OFFICE2003                                  AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLEMASK                                         AS INTEGER NO-UNDO INIT          15 GET. /* (0x0f) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLEFLAG_GRADIENT                                AS INTEGER NO-UNDO INIT         128 GET. /* (0x80) starting with XP, use gradient style */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLEFLAG_DOCKABLE                                AS INTEGER NO-UNDO INIT          64 GET. /* (0x40) dockable toolbars? */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLEFLAG_CANCLOSE                                AS INTEGER NO-UNDO INIT          32 GET. /* (0x20) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOOLBARSTYLEFLAG_SHRINK_TO_FIT                           AS INTEGER NO-UNDO INIT          16 GET. /* (0x10) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MENUSTYLE                                                AS INTEGER NO-UNDO INIT          91 GET. /* (91) default: LL_OPTION_MENUSTYLE_STANDARD */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MENUSTYLE_STANDARD_WITHOUT_BITMAPS                       AS INTEGER NO-UNDO INIT           0 GET. /* (0) values: see CTL */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MENUSTYLE_STANDARD                                       AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MENUSTYLE_OFFICEXP                                       AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MENUSTYLE_OFFICE2003                                     AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RULERSTYLE                                               AS INTEGER NO-UNDO INIT          92 GET. /* (92) default: LL_OPTION_RULERSTYLE_FLAT */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RULERSTYLE_FLAT                                          AS INTEGER NO-UNDO INIT          16 GET. /* (0x10) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RULERSTYLE_GRADIENT                                      AS INTEGER NO-UNDO INIT         128 GET. /* (0x80) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_STATUSBARSTYLE                                           AS INTEGER NO-UNDO INIT          93 GET. /* (93) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_STATUSBARSTYLE_STANDARD                                  AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_STATUSBARSTYLE_OFFICEXP                                  AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_STATUSBARSTYLE_OFFICE2003                                AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TABBARSTYLE                                              AS INTEGER NO-UNDO INIT          94 GET. /* (94) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TABBARSTYLE_STANDARD                                     AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TABBARSTYLE_OFFICEXP                                     AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TABBARSTYLE_OFFICE2003                                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DROPWINDOWSTYLE                                          AS INTEGER NO-UNDO INIT          95 GET. /* (95) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DROPWINDOWSTYLE_STANDARD                                 AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DROPWINDOWSTYLE_OFFICEXP                                 AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DROPWINDOWSTYLE_OFFICE2003                               AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DROPWINDOWSTYLEMASK                                      AS INTEGER NO-UNDO INIT          15 GET. /* (0x0f) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DROPWINDOWSTYLEFLAG_CANCLOSE                             AS INTEGER NO-UNDO INIT          32 GET. /* (0x20) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_INTERFACEWRAPPER                                         AS INTEGER NO-UNDO INIT          96 GET. /* (96) returns IL<n>* */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FONTQUALITY                                              AS INTEGER NO-UNDO INIT          97 GET. /* (97) LOGFONT.lfQuality, default: DEFAULT_QUALITY */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FONTPRECISION                                            AS INTEGER NO-UNDO INIT          98 GET. /* (98) LOGFONT.lfOutPrecision, default: OUT_STRING_PRECIS */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_UISTYLE                                                  AS INTEGER NO-UNDO INIT          99 GET. /* (99) UI collection, w/o */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_UISTYLE_STANDARD                                         AS INTEGER NO-UNDO INIT           0 GET. /* (0) 90=0x40, 91=1, 92=0x10, 93=0, 94=0, 95=0x20 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_UISTYLE_OFFICEXP                                         AS INTEGER NO-UNDO INIT           1 GET. /* (1) 90=0x41, 91=2, 92=0x10, 93=1, 94=1, 95=0x21 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_UISTYLE_OFFICE2003                                       AS INTEGER NO-UNDO INIT           2 GET. /* (2) 90=0x42, 91=3, 92=0x10, 93=2, 94=2, 95=0x22 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOFILEVERSIONUPGRADEWARNING                              AS INTEGER NO-UNDO INIT         100 GET. /* (100) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_UPDATE_FOOTER_ON_DATALINEBREAK_AT_FIRST_LINE             AS INTEGER NO-UNDO INIT         101 GET. /* (101) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ESC_CLOSES_PREVIEW                                       AS INTEGER NO-UNDO INIT         102 GET. /* (102) shall ESC close the preview window (default: false) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VIEWER_ASSUMES_TEMPFILE                                  AS INTEGER NO-UNDO INIT         103 GET. /* (103) shall the viewer assume that the file is a temporary file (and not store values in it)? default TRUE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CALC_USED_VARS                                           AS INTEGER NO-UNDO INIT         104 GET. /* (104) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOPRINTJOBSUPERVISION                                    AS INTEGER NO-UNDO INIT         106 GET. /* (106) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CALC_SUMVARS_ON_PARTIAL_LINES                            AS INTEGER NO-UNDO INIT         107 GET. /* (107) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_BLACKNESS_SCM                                            AS INTEGER NO-UNDO INIT         108 GET. /* (108) default: 0 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PROHIBIT_USERINTERACTION                                 AS INTEGER NO-UNDO INIT         109 GET. /* (109) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PERFMON_INSTALL                                          AS INTEGER NO-UNDO INIT         110 GET. /* (110) w/o, TRUE to install, FALSE to uninstall */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESERVED111                                              AS INTEGER NO-UNDO INIT         111 GET. /* (111) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTBUCKETCOUNT                                       AS INTEGER NO-UNDO INIT         112 GET. /* (112) applied to future jobs only, default 1000 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MSFAXALLOWED                                             AS INTEGER NO-UNDO INIT         113 GET. /* (113) global flag - set at start of LL! Will allow/prohibit fax detection. Default: TRUE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_AUTOPROFILINGTICKS                                       AS INTEGER NO-UNDO INIT         114 GET. /* (114) global flag - set at start of LL! Activates LL's thread profiling */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PROJECTBACKUP                                            AS INTEGER NO-UNDO INIT         115 GET. /* (115) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ERR_ON_FILENOTFOUND                                      AS INTEGER NO-UNDO INIT         116 GET. /* (116) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOFAXVARS                                                AS INTEGER NO-UNDO INIT         117 GET. /* (117) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOMAILVARS                                               AS INTEGER NO-UNDO INIT         118 GET. /* (118) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PATTERNRESCOMPATIBILITY                                  AS INTEGER NO-UNDO INIT         119 GET. /* (119) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NODELAYEDVALUECACHING                                    AS INTEGER NO-UNDO INIT         120 GET. /* (120) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FEATURE                                                  AS INTEGER NO-UNDO INIT        1000 GET. /* (1000) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FEATURE_CLEARALL                                         AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FEATURE_SUPPRESS_JPEG_DISPLAY                            AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FEATURE_SUPPRESS_JPEG_CREATION                           AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY                                           AS INTEGER NO-UNDO INIT         121 GET. /* (121) default: LL_OPTION_VARLISTDISPLAY_FOLDERPOS_TOP | LL_OPTION_VARLISTDISPLAY_VARSORT_ALPHA */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_VARSORT_DECLARATIONORDER                  AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_VARSORT_ALPHA                             AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_VARSORT_MASK                              AS INTEGER NO-UNDO INIT          15 GET. /* (0x000f) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_FOLDERPOS_DECLARATIONORDER                AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_FOLDERPOS_ALPHA                           AS INTEGER NO-UNDO INIT          16 GET. /* (0x0010) only if LL_OPTION_VARLISTDISPLAY_VARSORT_ALPHA is set */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_FOLDERPOS_TOP                             AS INTEGER NO-UNDO INIT          32 GET. /* (0x0020) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_FOLDERPOS_BOTTOM                          AS INTEGER NO-UNDO INIT          48 GET. /* (0x0030) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_FOLDERPOS_MASK                            AS INTEGER NO-UNDO INIT         240 GET. /* (0x00f0) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_LLFOLDERPOS_BOTTOM                        AS INTEGER NO-UNDO INIT         256 GET. /* (0x0100) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_WORKAROUND_RTFBUG_EMPTYFIRSTPAGE                         AS INTEGER NO-UNDO INIT         122 GET. /* (122) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORMULASTRINGCOMPARISONS_CASESENSITIVE                   AS INTEGER NO-UNDO INIT         123 GET. /* (123) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FIELDS_IN_PROJECTPARAMETERS                              AS INTEGER NO-UNDO INIT         124 GET. /* (124) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CHECKWINDOWTHREADEDNESS                                  AS INTEGER NO-UNDO INIT         125 GET. /* (125) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ISUSED_WILDCARD_AT_START                                 AS INTEGER NO-UNDO INIT         126 GET. /* (126) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ROOT_MUST_BE_MASTERTABLE                                 AS INTEGER NO-UNDO INIT         127 GET. /* (127) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DLLTYPE                                                  AS INTEGER NO-UNDO INIT         128 GET. /* (128) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DLLTYPE_32BIT                                            AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DLLTYPE_64BIT                                            AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DLLTYPE_BITMASK                                          AS INTEGER NO-UNDO INIT          15 GET. /* (0x000f) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DLLTYPE_SDBCS                                            AS INTEGER NO-UNDO INIT          16 GET. /* (0x0010) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DLLTYPE_UNICODE                                          AS INTEGER NO-UNDO INIT          32 GET. /* (0x0020) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DLLTYPE_CHARSET                                          AS INTEGER NO-UNDO INIT         240 GET. /* (0x00f0) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_HLIBRARY                                                 AS INTEGER NO-UNDO INIT         129 GET. /* (129) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_INVERTED_PAGEORIENTATION                                 AS INTEGER NO-UNDO INIT         130 GET. /* (130) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ENABLE_STANDALONE_DATACOLLECTING_OBJECTS                 AS INTEGER NO-UNDO INIT         131 GET. /* (131) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USERVARS_ARE_CODESNIPPETS                                AS INTEGER NO-UNDO INIT         132 GET. /* (132) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_STORAGE_ADD_SUMMARYINFORMATION                           AS INTEGER NO-UNDO INIT         133 GET. /* (133) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_INCREMENTAL_PREVIEW                                      AS INTEGER NO-UNDO INIT         135 GET. /* (135) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RELAX_AT_SHUTDOWN                                        AS INTEGER NO-UNDO INIT         136 GET. /* (136) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOPRINTERPATHCHECK                                       AS INTEGER NO-UNDO INIT         137 GET. /* (137) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPPORT_HUGESTORAGEFS                                    AS INTEGER NO-UNDO INIT         138 GET. /* (138) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOAUTOPROPERTYCORRECTION                                 AS INTEGER NO-UNDO INIT         139 GET. /* (139) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOVARLISTRESET_ON_RESETPROJECTSTATE                      AS INTEGER NO-UNDO INIT         140 GET. /* (140) default: false; */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DESIGNERPREVIEWPARAMETER                                 AS INTEGER NO-UNDO INIT         141 GET. /* (141) default: NULL */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESERVED142                                              AS INTEGER NO-UNDO INIT         142 GET. /* (142) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DESIGNEREXPORTPARAMETER                                  AS INTEGER NO-UNDO INIT         143 GET. /* (143) default: NULL */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DESIGNERPRINT_SINGLETHREADED                             AS INTEGER NO-UNDO INIT         144 GET. /* (144) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ALLOW_COMMENTS_IN_FORMULA                                AS INTEGER NO-UNDO INIT         145 GET. /* (145) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USE_MLANG_LINEBREAKALGORITHM                             AS INTEGER NO-UNDO INIT         146 GET. /* (146) default: false (would use MLANG to calculate the line break algorithm) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USE_JPEG_OR_PNG_OPTIMIZATION                             AS INTEGER NO-UNDO INIT         147 GET. /* (147) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ENABLE_IMAGESMOOTHING                                    AS INTEGER NO-UNDO INIT         148 GET. /* (148) default: true (uses GDIPLUS - no smoothing on Win2000/98 if not GDIPLUS installed! Right now, applies only to JPEG.) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MAXRTFVERSION_AVAILABLE                                  AS INTEGER NO-UNDO INIT         159 GET. /* (159) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CONDREPRESENTATIONCODES_LIKE_ANSI                        AS INTEGER NO-UNDO INIT         160 GET. /* (160) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NULL_IS_NONDESTRUCTIVE                                   AS INTEGER NO-UNDO INIT         161 GET. /* (161) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DRILLDOWNPARAMETER                                       AS INTEGER NO-UNDO INIT         162 GET. /* (162) default: NULL */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ROUNDINGSTRATEGY                                         AS INTEGER NO-UNDO INIT         163 GET. /* (163) default: LL_ROUNDINGSTRATEGY_ARITHMETIC_SYMMETRIC */
    DEFINE PUBLIC STATIC PROPERTY LL_ROUNDINGSTRATEGY_BANKERSROUNDING                                AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_ROUNDINGSTRATEGY_ARITHMETIC_SYMMETRIC                           AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESERVED164                                              AS INTEGER NO-UNDO INIT         164 GET. /* (164) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESERVED165                                              AS INTEGER NO-UNDO INIT         165 GET. /* (165) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PICTURE_TRANSPARENCY_IS_WHITE                            AS INTEGER NO-UNDO INIT         166 GET. /* (166) default: false (transparent) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FLOATPRECISION                                           AS INTEGER NO-UNDO INIT         167 GET. /* (167) global (not job specific!). Default: 0 (192 bit mantissa, 32 bit exponent) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPPRESS_LRUENTRY                                        AS INTEGER NO-UNDO INIT         168 GET. /* (168) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCEFIRSTGROUPHEADER                                    AS INTEGER NO-UNDO INIT         169 GET. /* (169) default: false (group match string is an empty string) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPPORT_PDFINPUTFIELDS                                   AS INTEGER NO-UNDO INIT         170 GET. /* (170) PDF 3.0 supports text objects and check boxes as input objects - default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ENHANCED_SKIPRETURNATENDOFRTF                            AS INTEGER NO-UNDO INIT         171 GET. /* (171) default: false. */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_HIERARCHICALDATASOURCE                                   AS INTEGER NO-UNDO INIT         172 GET. /* (172) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCE_HEADER_EVEN_ON_LARGE_FOOTERLINES                   AS INTEGER NO-UNDO INIT         173 GET. /* (173) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRINTERDEVICEOPTIMIZATION                                AS INTEGER NO-UNDO INIT         174 GET. /* (174) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RTFHEIGHTSCALINGPERCENTAGE                               AS INTEGER NO-UNDO INIT         175 GET. /* (175) default: 100 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCE_DEFAULT_PRINTER_IN_PREVIEW                         AS INTEGER NO-UNDO INIT         176 GET. /* (176) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SAVE_PROJECT_IN_UTF8                                     AS INTEGER NO-UNDO INIT         178 GET. /* (178) BOOL, default 0 (meaning: project is saved as UNICODE if A API is not used) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRILLDOWNFILTERSTRATEGY_ALLOW_ONLY_SUBTABLES                    AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRILLDOWNFILTERSTRATEGY_ALLOW_ALL_TABLES                        AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRILLDOWNFILTERSTRATEGY_ALLOW_SUBTABLES_AND_UNRELATED           AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRILLDOWNFILTERSTRATEGY_ALLOW_SUBTABLES_AND_USERDEFINED         AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRILLDOWNFILTERSTRATEGY_MASK                                    AS INTEGER NO-UNDO INIT          15 GET. /* (0x0f) */
    DEFINE PUBLIC STATIC PROPERTY LL_DRILLDOWNFILTERFLAG_OFFER_BASERECORD_AS_VARIABLES               AS INTEGER NO-UNDO INIT          16 GET. /* (0x10) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DRILLDOWN_DATABASEFILTERING                              AS INTEGER NO-UNDO INIT         179 GET. /* (179) default: 0 (filter all except subtables of the base table: LL_DRILLDOWNFILTERSTRATEGY_ALLOW_ONLY_SUBTABLES) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPPRESS_TASKBARBUTTON_PROGRESSSTATE                     AS INTEGER NO-UNDO INIT         180 GET. /* (180) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRINTDLG_DEVICECHANGE_KEEPS_DEVMODESETTINGS              AS INTEGER NO-UNDO INIT         181 GET. /* (181) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DRILLDOWN_SUPPORTS_EMBEDDING                             AS INTEGER NO-UNDO INIT         182 GET. /* (182) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_VARLISTCLEARSTRATEGY_EMPTY_LIST                                 AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_VARLISTCLEARSTRATEGY_SET_NULL                                   AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_VARLISTCLEARSTRATEGY_SET_DEFAULT                                AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTCLEARSTRATEGY_ON_DEFINE_START                     AS INTEGER NO-UNDO INIT         183 GET. /* (183) default: LL_VARLISTCLEARSTRATEGY_EMPTY_LIST */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESERVED184                                              AS INTEGER NO-UNDO INIT         184 GET. /* (184) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_KEEP_EMPTY_SUM_VARS                                      AS INTEGER NO-UNDO INIT         185 GET. /* (185) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESERVED187                                              AS INTEGER NO-UNDO INIT         187 GET. /* (187) internal test flag */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DEFAULTDECSFORSTR                                        AS INTEGER NO-UNDO INIT         188 GET. /* (188) default: 5. Sets the default number of decimals for Str$ */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESETPROJECTSTATE_FORCES_NEW_PRINTJOB                    AS INTEGER NO-UNDO INIT         189 GET. /* (189) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DEFINEXXXSTART_COMPATIBLE_TO_PRE15                       AS INTEGER NO-UNDO INIT         190 GET. /* (190) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESETPROJECTSTATE_FORCES_NEW_DC                          AS INTEGER NO-UNDO INIT         191 GET. /* (191) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_BITMAP_RESOLUTION_FOR_PREVIEW                            AS INTEGER NO-UNDO INIT         192 GET. /* (192) default: 0 (leave original size), suggestions are 300 or 600. -1 to use device default. */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DRAW_EMPTY_CHARTOBJECTS                                  AS INTEGER NO-UNDO INIT         193 GET. /* (193) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PREVIOUS_DEFAULTS_TO_NULL                                AS INTEGER NO-UNDO INIT         194 GET. /* (194) default: false (for compatibility). Previous() returns NULL on first record if TRUE, otherwise some default value for the given datatype. */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCE_IMAGEEMBEDDING                                     AS INTEGER NO-UNDO INIT         195 GET. /* (195) default: false. Images added via the image dialog are always embedded. */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARKEY_MAP_SHARP_S_TO_SS                                 AS INTEGER NO-UNDO INIT         196 GET. /* (196) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_LAYERED_WINDOWS                                       AS INTEGER NO-UNDO INIT         197 GET. /* (197) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SCALED_PERCENTAGEFORMATTER                               AS INTEGER NO-UNDO INIT         198 GET. /* (198) default: false (0.1="0.1%", true: 0.1="10%") */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USE_ANTIALIAS                                            AS INTEGER NO-UNDO INIT         199 GET. /* (199) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCETABLELINECALLBACK                                   AS INTEGER NO-UNDO INIT         200 GET. /* (200) LL_CMND_TABLE_LINE is called even when COLORINGMODE_LL ist set, default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_EXPORTCONSUMER                                           AS INTEGER NO-UNDO INIT         201 GET. /* (201) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOC_IDX_ITEMID                                           AS INTEGER NO-UNDO INIT         202 GET. /* (202) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCED2PASSMODE                                          AS INTEGER NO-UNDO INIT         203 GET. /* (203) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SETVAR_ONLY_SETS_IF_CONTAINER_PRINTS                     AS INTEGER NO-UNDO INIT         204 GET. /* (204) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SHOW_PREVIEW_AFTER_PRINT_END                             AS INTEGER NO-UNDO INIT         206 GET. /* (206) "Export-ShowResult" sets this for PRV... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PROPLIST_COMBOBOX_SCROLL_WRAPS                           AS INTEGER NO-UNDO INIT         207 GET. /* (207) default: FALSE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ALWAYS_CALC_GROUPCHANGE_CONDITION                        AS INTEGER NO-UNDO INIT         208 GET. /* (208) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NULLHANDLING_SUPPORTED_IN_ENHMODE                        AS INTEGER NO-UNDO INIT         209 GET. /* (209) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESETPROJECTSTATE_FORCES_NEW_PREVIEWJOB                  AS INTEGER NO-UNDO INIT         210 GET. /* (210) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USE_LEGACY_WORDWRAPPINGALGORITHM                         AS INTEGER NO-UNDO INIT         211 GET. /* (211) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PREVIEW_USES_PRINTTHREAD                                 AS INTEGER NO-UNDO INIT         212 GET. /* (212) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_LL_SUPPLIES_MESSAGELOOP_WHILE_PRINTING_TO_PREVIEW        AS INTEGER NO-UNDO INIT         213 GET. /* (213) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRINTERDCCACHE_TIMEOUT_SEC                               AS INTEGER NO-UNDO INIT         214 GET. /* (214) default: 60 (0 -> no cache) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DESIGNER_RIBBONBACKGROUNDCOLOR                           AS INTEGER NO-UNDO INIT         215 GET. /* (215) default: undefined (system default) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_INTERNAL_EMFCLEANUP                                      AS INTEGER NO-UNDO INIT         216 GET. /* (216) no comment -> internal! */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RIBBON_DEFAULT_ENABLEDSTATE                              AS INTEGER NO-UNDO INIT         217 GET. /* (217) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PRVFILEVERSION                                           AS INTEGER NO-UNDO INIT         218 GET. /* (218) default: 0 (2 would be an optimized version, supported since LL18, usually a bit faster if printing > 5000 pages) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TRY_REDUCE_BMPSIZE_BY_CONVERTING_TO_PNG_OR_JPEG          AS INTEGER NO-UNDO INIT         219 GET. /* (219) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_IMAGEFILEOPTIMIZATION                                 AS INTEGER NO-UNDO INIT         220 GET. /* (220) default: false. Set this to TRUE if you know you're replacing an image file during printing that is used in a project using its file name */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_ENFORCED_GROUPFOOTERPRIORITY_FOR_LAST_GROUPFOOTER     AS INTEGER NO-UNDO INIT         221 GET. /* (221) default: false. Compatibility to LL 16.008. */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ALLOW_COMBINED_COLLECTING_OF_DATA_FOR_COLLECTIONCONTROLS AS INTEGER NO-UNDO INIT         222 GET. /* (222) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPPRESS_LOADERRORMESSAGES                               AS INTEGER NO-UNDO INIT         223 GET. /* (223) default: false. Please take care that this is a reference counted flag, so add (true) and subtract (false) the same number of calls! [ChK] */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_IGNOREFORMULARESULTMISMATCH_AT_LOADTIME                  AS INTEGER NO-UNDO INIT         224 GET. /* (224) default: false. Switches the r8117 (err #3535) change back to the old behavior */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MAX_SIZE_OF_PROJECTINFOCACHE                             AS INTEGER NO-UNDO INIT         225 GET. /* (225) default: 1000 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_CORRECTION_OF_UNICODE_RTF                             AS INTEGER NO-UNDO INIT         226 GET. /* (226) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MAY_RELEASE_UNNECESSARY_PROPS_AT_PRINTTIME               AS INTEGER NO-UNDO INIT         227 GET. /* (227) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DO_NOT_RESTORE_PREVSTATE_ON_FILTER_MISMATCH              AS INTEGER NO-UNDO INIT         228 GET. /* (228) default: false (LL17: implicitly TRUE until 17.006) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUPPORT_USERDEFINED_REPORTPAGELAYOUT                     AS INTEGER NO-UNDO INIT         229 GET. /* (229) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DESIGNER_RIBBONTEXTCOLOR                                 AS INTEGER NO-UNDO INIT         230 GET. /* (230) default: undefined (system default) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARTSHARINGFLAG_VARIABLES_TOC                                   AS INTEGER NO-UNDO INIT           1 GET. /* (0x01) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARTSHARINGFLAG_VARIABLES_IDX                                   AS INTEGER NO-UNDO INIT           2 GET. /* (0x02) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARTSHARINGFLAG_VARIABLES_GTC                                   AS INTEGER NO-UNDO INIT           4 GET. /* (0x04) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PARTSHARINGFLAGS                                         AS INTEGER NO-UNDO INIT         231 GET. /* (231) default: 0 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PIECHARTORDER_COMPATIBLE_TO_PRE19                        AS INTEGER NO-UNDO INIT         232 GET. /* (232) default: 1 (LL18), 0 (>= LL19) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DATABASESTRUCTURE_SORT_DECLARATIONORDER                  AS INTEGER NO-UNDO INIT         233 GET. /* (233) default: false (sorted alphabetically) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_REPORT_PARAMETERS_REALDATAJOBPARAMETER                   AS INTEGER NO-UNDO INIT         234 GET. /* (234) default: NULL */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_EXPANDABLE_REGIONS_REALDATAJOBPARAMETER                  AS INTEGER NO-UNDO INIT         235 GET. /* (235) default: NULL */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_IMPROVED_TABLELINEANCHORING                              AS INTEGER NO-UNDO INIT         236 GET. /* (236) default: FALSE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_INTERACTIVESORTING_REALDATAJOBPARAMETER                  AS INTEGER NO-UNDO INIT         237 GET. /* (237) default: NULL */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TEMPFILESTRATEGY                                         AS INTEGER NO-UNDO INIT         238 GET. /* (238) default: LL_TEMPFILESTRATEGY_SPEED */
    DEFINE PUBLIC STATIC PROPERTY LL_TEMPFILESTRATEGY_SPEED                                          AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_TEMPFILESTRATEGY_SIZE                                           AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_TEMPFILESTRATEGY_SECURITY                                       AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RTF_WHITE_BACKGROUND_IS_TRANSPARENT                      AS INTEGER NO-UNDO INIT         239 GET. /* (239) default: TRUE (!) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_DOTTED_LINE_ON_SECONDARY_AXIS                         AS INTEGER NO-UNDO INIT         240 GET. /* (240) default: FALSE */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_PREVIOUS_VARLIST                                      AS INTEGER NO-UNDO INIT         241 GET. /* (241) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_COMMIT_FILE_ON_SAVE                                      AS INTEGER NO-UNDO INIT         242 GET. /* (242) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DO_NOT_RTRIM_CELLTEXT                                    AS INTEGER NO-UNDO INIT         243 GET. /* (243) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ALLOW_FCT_TEXTWIDTH                                      AS INTEGER NO-UNDO INIT         244 GET. /* (244) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PASTEOBJECTS_TO_FIRST_VISIBLE_LAYER                      AS INTEGER NO-UNDO INIT         245 GET. /* (245) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_EMPTY_FILE_TRIGGERS_PROJECT_WIZARD                       AS INTEGER NO-UNDO INIT         246 GET. /* (246) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DELAY_UPDATE_REMAININGTABLESPACE                         AS INTEGER NO-UNDO INIT         247 GET. /* (247) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_WIZARD_ADDS_ORGNAME_TO_UI                                AS INTEGER NO-UNDO INIT         248 GET. /* (248) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PROHIBIT_EXTERNAL_FILES                                  AS INTEGER NO-UNDO INIT         249 GET. /* (249) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DRAWINGS_INLINED                                         AS INTEGER NO-UNDO INIT         250 GET. /* (250) default: 0 (1 = inlined, 2 = leave as is, but no BLOBs - for GTC) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SERIALIZE_PRINTAPI                                       AS INTEGER NO-UNDO INIT         251 GET. /* (251) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_PROJECTFILELOCKTIMEOUT_IN_MS                             AS INTEGER NO-UNDO INIT         252 GET. /* (252) default: 10000 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ILLDATAPROVIDER                                          AS INTEGER NO-UNDO INIT         253 GET. /* (253) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RTF_SUPPORTS_PARABREAKOPTIONS                            AS INTEGER NO-UNDO INIT         254 GET. /* (254) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCE_PDFEMBEDDING                                       AS INTEGER NO-UNDO INIT         255 GET. /* (255) default: false. PDF documents added via the pdf dialog are always embedded */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_IGNORE_NONSCALEABLEFONTPROPERTIES                        AS INTEGER NO-UNDO INIT         256 GET. /* (256) default: false. PDF documents added via the pdf dialog are always embedded */
    DEFINE PUBLIC STATIC PROPERTY LL_DATAPROVIDERTHREADNESS_NONE                                     AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_DATAPROVIDERTHREADNESS_ONE_INSTANCE_PER_THREAD                  AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_DATAPROVIDERTHREADNESS_DONTCARE                                 AS INTEGER NO-UNDO INIT           2 GET. /* (2) default */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DATAPROVIDER_THREADEDNESS                                AS INTEGER NO-UNDO INIT         257 GET. /* (257) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUBREPORT_BASE                                           AS INTEGER NO-UNDO INIT         258 GET. /* (258) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SUBREPORT_CLIENT                                         AS INTEGER NO-UNDO INIT         259 GET. /* (259) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_IPICTURE_SUPPORT                                      AS INTEGER NO-UNDO INIT         260 GET. /* (260) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCE_JPEG_RECOMPRESSION                                 AS INTEGER NO-UNDO INIT         261 GET. /* (261) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TEXTWRAP_TOLERANCE_PERC                                  AS INTEGER NO-UNDO INIT         262 GET. /* (262) default: 0 (no tolerance) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NO_USERVARCHECK_ON_LOAD                                  AS INTEGER NO-UNDO INIT         263 GET. /* (263) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_TOC_IDX_PAGE                                             AS INTEGER NO-UNDO INIT         264 GET. /* (264) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RTF_TAB_KEY_IS_TAB_FORMATTER                             AS INTEGER NO-UNDO INIT         265 GET. /* (265) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_VARLISTDISPLAY_LL_FOLDER_AT_END                          AS INTEGER NO-UNDO INIT         266 GET. /* (266) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DOM_DO_NOT_KILL_EMPTY_TABLE                              AS INTEGER NO-UNDO INIT         267 GET. /* (267) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_ENABLE_INPUTOBJECTS_IN_TABLES                            AS INTEGER NO-UNDO INIT         268 GET. /* (268) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MAX_ENTRIES_FOR_AUTOCOMPLETE                             AS INTEGER NO-UNDO INIT         269 GET. /* (269) default: 200 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_DEFAULT_FOR_SHADOWPAGEWRAP                               AS INTEGER NO-UNDO INIT         270 GET. /* (270) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_MAX_UNDO_STEPS                                           AS INTEGER NO-UNDO INIT         271 GET. /* (271) default: 10 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_HTML_USE_MAILFORMAT                                      AS INTEGER NO-UNDO INIT         272 GET. /* (272) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_CLIP_LABELS_TO_PROJECTAREA                               AS INTEGER NO-UNDO INIT         273 GET. /* (273) default: false (may paint beyond the border not to lose any data) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESETPROJECTSTATE_FORCES_NEW_EXPORTJOB                   AS INTEGER NO-UNDO INIT         274 GET. /* (274) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SCRIPTENGINE_ENABLED                                     AS INTEGER NO-UNDO INIT         276 GET. /* (276) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SCRIPTENGINE_TIMEOUTMS                                   AS INTEGER NO-UNDO INIT         277 GET. /* (277) default: 10000 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SCRIPTENGINE_AUTOEXECUTE                                 AS INTEGER NO-UNDO INIT         278 GET. /* (278) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SHAPEFILE_TIMEOUTMS                                      AS INTEGER NO-UNDO INIT         279 GET. /* (279) default: 1000 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_COUNTALLPRINTEDDATA_LASTPRINT                            AS INTEGER NO-UNDO INIT         280 GET. /* (280) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_SAVE_AS_ACTS_AS_EXPORT                                   AS INTEGER NO-UNDO INIT         281 GET. /* (281) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_RESETPROJECTSTATE_TRIGGERS_NEW_SHEET                     AS INTEGER NO-UNDO INIT         282 GET. /* (282) default: true */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_HIDE_EXPORT_TAB_FROM_LAYOUT_CONFIG                       AS INTEGER NO-UNDO INIT         283 GET. /* (283) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_USE_VARLIST_NAMESORTINDEXCACHE                           AS INTEGER NO-UNDO INIT         284 GET. /* (284) should be defined for job -1 */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_NOCONTRASTOPTIMIZATION                                   AS INTEGER NO-UNDO INIT         285 GET. /* (285) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_AUTORECOVERY_DISABLED                                    AS INTEGER NO-UNDO INIT         286 GET. /* (286) default: false */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_AUTORECOVERY_SAVEOPTIONS                                 AS INTEGER NO-UNDO INIT         287 GET. /* (287) default: LL_AUTORECOVERY_SAVEOPTIONS_NEWFILE (value might be combination of following) */
    DEFINE PUBLIC STATIC PROPERTY LL_AUTORECOVERY_SAVEOPTIONS_NEWFILE                                AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_AUTORECOVERY_SAVEOPTIONS_OVERWRITE                              AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_LINK_PRINTERQUEUES                                       AS INTEGER NO-UNDO INIT         288 GET. /* (288) default: false, does not work yet */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_FORCE_RTFMERGING                                         AS INTEGER NO-UNDO INIT         289 GET. /* (289) default: false, forces to merge RTF contents even if there is just one part to load, compatibility switch */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTION_W201512300001                                            AS INTEGER NO-UNDO INIT         290 GET. /* (290) do not check on empty bodylines for "ActivateNextLine" - sort of "I know what I am doing in my print loop" */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LABEL_PRJEXT                                          AS INTEGER NO-UNDO INIT           0 GET. /* (0) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LABEL_PRVEXT                                          AS INTEGER NO-UNDO INIT           1 GET. /* (1) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LABEL_PRNEXT                                          AS INTEGER NO-UNDO INIT           2 GET. /* (2) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_CARD_PRJEXT                                           AS INTEGER NO-UNDO INIT           3 GET. /* (3) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_CARD_PRVEXT                                           AS INTEGER NO-UNDO INIT           4 GET. /* (4) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_CARD_PRNEXT                                           AS INTEGER NO-UNDO INIT           5 GET. /* (5) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LIST_PRJEXT                                           AS INTEGER NO-UNDO INIT           6 GET. /* (6) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LIST_PRVEXT                                           AS INTEGER NO-UNDO INIT           7 GET. /* (7) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LIST_PRNEXT                                           AS INTEGER NO-UNDO INIT           8 GET. /* (8) internal... (compatibility to L6) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LLXPATHLIST                                           AS INTEGER NO-UNDO INIT          12 GET. /* (12) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_SHORTDATEFORMAT                                       AS INTEGER NO-UNDO INIT          13 GET. /* (13) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DECIMAL                                               AS INTEGER NO-UNDO INIT          14 GET. /* (14) decimal point, default: system */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_THOUSAND                                              AS INTEGER NO-UNDO INIT          15 GET. /* (15) thousands separator, default: system */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_CURRENCY                                              AS INTEGER NO-UNDO INIT          16 GET. /* (16) currency symbol, default: system */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_EXPORTS_AVAILABLE                                     AS INTEGER NO-UNDO INIT          17 GET. /* (17) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_EXPORTS_ALLOWED                                       AS INTEGER NO-UNDO INIT          18 GET. /* (18) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DEFDEFFONT                                            AS INTEGER NO-UNDO INIT          19 GET. /* (19) in "{(r,g,b),size,<logfont>}" */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_EXPORTFILELIST                                        AS INTEGER NO-UNDO INIT          20 GET. /* (20) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_VARALIAS                                              AS INTEGER NO-UNDO INIT          21 GET. /* (21) "<local>=<global>" */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_MAILTO                                                AS INTEGER NO-UNDO INIT          24 GET. /* (24) default TO: address for mailing from viewer */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_MAILTO_CC                                             AS INTEGER NO-UNDO INIT          25 GET. /* (25) default CC: address for mailing from viewer */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_MAILTO_BCC                                            AS INTEGER NO-UNDO INIT          26 GET. /* (26) default BCC: address for mailing from viewer */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_MAILTO_SUBJECT                                        AS INTEGER NO-UNDO INIT          27 GET. /* (27) default subject for mailing from viewer */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_SAVEAS_PATH                                           AS INTEGER NO-UNDO INIT          28 GET. /* (28) default filename for saving the LL file from viewer */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LABEL_PRJDESCR                                        AS INTEGER NO-UNDO INIT          29 GET. /* (29) "Etikett" ... */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_CARD_PRJDESCR                                         AS INTEGER NO-UNDO INIT          30 GET. /* (30) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LIST_PRJDESCR                                         AS INTEGER NO-UNDO INIT          31 GET. /* (31) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LLFILEDESCR                                           AS INTEGER NO-UNDO INIT          32 GET. /* (32) "Vorschau-Datei" */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_PROJECTPASSWORD                                       AS INTEGER NO-UNDO INIT          33 GET. /* (33) w/o, of course :) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_FAX_RECIPNAME                                         AS INTEGER NO-UNDO INIT          34 GET. /* (34) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_FAX_RECIPNUMBER                                       AS INTEGER NO-UNDO INIT          35 GET. /* (35) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_FAX_QUEUENAME                                         AS INTEGER NO-UNDO INIT          36 GET. /* (36) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_FAX_SENDERNAME                                        AS INTEGER NO-UNDO INIT          37 GET. /* (37) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_FAX_SENDERCOMPANY                                     AS INTEGER NO-UNDO INIT          38 GET. /* (38) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_FAX_SENDERDEPT                                        AS INTEGER NO-UNDO INIT          39 GET. /* (39) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_FAX_SENDERBILLINGCODE                                 AS INTEGER NO-UNDO INIT          40 GET. /* (40) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_FAX_AVAILABLEQUEUES                                   AS INTEGER NO-UNDO INIT          42 GET. /* (42) r/o (Tab-separated) [job can be -1 or a valid job] */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LOGFILEPATH                                           AS INTEGER NO-UNDO INIT          43 GET. /* (43) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_LICENSINGINFO                                         AS INTEGER NO-UNDO INIT          44 GET. /* (44) w/o, SERNO to define licensing state */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_PRINTERALIASLIST                                      AS INTEGER NO-UNDO INIT          45 GET. /* (45) multiple "PrnOld=PrnNew1[;PrnNew2[;...]]", erase with NULL or "" */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_PREVIEWFILENAME                                       AS INTEGER NO-UNDO INIT          46 GET. /* (46) path of preview file (directory will be overridden by LlPreviewSetTempPath(), if given) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_EXPORTS_ALLOWED_IN_PREVIEW                            AS INTEGER NO-UNDO INIT          47 GET. /* (47) set in preview file */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_HELPFILENAME                                          AS INTEGER NO-UNDO INIT          48 GET. /* (48) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_NULLVALUE                                             AS INTEGER NO-UNDO INIT          49 GET. /* (49) string which represents the NULL value */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DEFAULT_EXPORT                                        AS INTEGER NO-UNDO INIT          50 GET. /* (50) default export medium for new projects */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_ORIGINALPROJECTFILENAME                               AS INTEGER NO-UNDO INIT          51 GET. /* (51) fixup project path for relative paths in realdata preview/export in designer */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_HIERARCHICALDATASOURCE_ROOT                           AS INTEGER NO-UNDO INIT          52 GET. /* (52) internal use only */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_PRINTERDEFINITIONFILENAME                             AS INTEGER NO-UNDO INIT          53 GET. /* (53) override for P file name */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DOCINFO_DATATYPE                                      AS INTEGER NO-UNDO INIT          54 GET. /* (54) DOCINFO.lpszDatatype */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_IDX_PRJEXT                                            AS INTEGER NO-UNDO INIT          55 GET. /* (55) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_IDX_PRVEXT                                            AS INTEGER NO-UNDO INIT          56 GET. /* (56) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_IDX_PRNEXT                                            AS INTEGER NO-UNDO INIT          57 GET. /* (57) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_TOC_PRJDESCR                                          AS INTEGER NO-UNDO INIT          58 GET. /* (58) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_IDX_PRJDESCR                                          AS INTEGER NO-UNDO INIT          59 GET. /* (59) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_TOC_PRJEXT                                            AS INTEGER NO-UNDO INIT          60 GET. /* (60) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_TOC_PRVEXT                                            AS INTEGER NO-UNDO INIT          61 GET. /* (61) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_TOC_PRNEXT                                            AS INTEGER NO-UNDO INIT          62 GET. /* (62) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DEFAULTSCHEME                                         AS INTEGER NO-UNDO INIT          63 GET. /* (63) default: empty (COMBIT) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DEFAULTPROJECTNAME                                    AS INTEGER NO-UNDO INIT          64 GET. /* (64) DOCINFO.lpszDatatype */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_GTC_PRJEXT                                            AS INTEGER NO-UNDO INIT          65 GET. /* (65) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_GTC_PRVEXT                                            AS INTEGER NO-UNDO INIT          66 GET. /* (66) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_GTC_PRNEXT                                            AS INTEGER NO-UNDO INIT          67 GET. /* (67) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_GTC_PRJDESCR                                          AS INTEGER NO-UNDO INIT          68 GET. /* (68) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_ERRORTEXT_FROM_EXPORT                                 AS INTEGER NO-UNDO INIT          69 GET. /* (69) r/o */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DEFAULTPRJDESCR                                       AS INTEGER NO-UNDO INIT          70 GET. /* (70) default: empty (localized version of 'List & Label project file') */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DEFAULTPRINTER                                        AS INTEGER NO-UNDO INIT          71 GET. /* (71) if set, this printer is used instead of the system's default printer (applies to ALL JOBS, so job ID must be "-1"!) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_QUERY_LICENSINGINFO                                   AS INTEGER NO-UNDO INIT          72 GET. /* (72) r/o, returns serial number in return value */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_RESERVED73                                            AS INTEGER NO-UNDO INIT          73 GET. /* (73) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_REPRESENTATION_BOOL_TRUE                              AS INTEGER NO-UNDO INIT          74 GET. /* (74) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_REPRESENTATION_BOOL_FALSE                             AS INTEGER NO-UNDO INIT          75 GET. /* (75) */
    DEFINE PUBLIC STATIC PROPERTY LL_OPTIONSTR_DEFAULT_FILENAME_FOR_SAVEAS                           AS INTEGER NO-UNDO INIT          76 GET. /* (76) if set, this filename is used as a default name when "Save as" is chosen from the menu */
    DEFINE PUBLIC STATIC PROPERTY LL_SYSCOMMAND_MINIMIZE                                             AS INTEGER NO-UNDO INIT          -1 GET. /* (-1) */
    DEFINE PUBLIC STATIC PROPERTY LL_SYSCOMMAND_MAXIMIZE                                             AS INTEGER NO-UNDO INIT          -2 GET. /* (-2) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_3DBUTTONS                                            AS INTEGER NO-UNDO INIT       32768 GET. /* (0x8000) 'or'ed */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_3DFRAME2                                             AS INTEGER NO-UNDO INIT       16384 GET. /* (0x4000) 'OR'ed */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_3DFRAME                                              AS INTEGER NO-UNDO INIT        4096 GET. /* (0x1000) 'OR'ed */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_NOBITMAPS                                            AS INTEGER NO-UNDO INIT        8192 GET. /* (0x2000) 'or'ed */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_DONTCARE                                             AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) load from INI */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_SAA                                                  AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT1                                                 AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT2                                                 AS INTEGER NO-UNDO INIT           3 GET. /* (0x0003) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT3                                                 AS INTEGER NO-UNDO INIT           4 GET. /* (0x0004) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT4                                                 AS INTEGER NO-UNDO INIT           5 GET. /* (0x0005) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT5                                                 AS INTEGER NO-UNDO INIT           6 GET. /* (0x0006) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT6                                                 AS INTEGER NO-UNDO INIT           7 GET. /* (0x0007) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT7                                                 AS INTEGER NO-UNDO INIT           8 GET. /* (0x0008) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT8                                                 AS INTEGER NO-UNDO INIT           9 GET. /* (0x0009) Win95 */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT9                                                 AS INTEGER NO-UNDO INIT          10 GET. /* (0x000A) Win98 */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_ALT10                                                AS INTEGER NO-UNDO INIT          11 GET. /* (0x000B) Win98 with gray/color button bitmaps like IE4 */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGBOXMODE_TOOLTIPS98                                           AS INTEGER NO-UNDO INIT        2048 GET. /* (0x0800) DEPRECATED. Do not change. */
    DEFINE PUBLIC STATIC PROPERTY LL_CTL_ADDTOSYSMENU                                                AS INTEGER NO-UNDO INIT           4 GET. /* (0x00000004) from CTL */
    DEFINE PUBLIC STATIC PROPERTY LL_CTL_ALSOCHILDREN                                                AS INTEGER NO-UNDO INIT          16 GET. /* (0x00000010) */
    DEFINE PUBLIC STATIC PROPERTY LL_CTL_CONVERTCONTROLS                                             AS INTEGER NO-UNDO INIT       65536 GET. /* (0x00010000) */
    DEFINE PUBLIC STATIC PROPERTY LL_GROUP_ALWAYSFOOTER                                              AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINTERCONFIG_SAVE                                              AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRINTERCONFIG_RESTORE                                           AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJTYPE_OPTION_FORCEDEFAULTSETTINGS                             AS INTEGER NO-UNDO INIT       32768 GET. /* (0x8000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJTYPE_OPTION_CREATEPARTSFROMPROJECT                           AS INTEGER NO-UNDO INIT       16384 GET. /* (0x4000) */
    DEFINE PUBLIC STATIC PROPERTY LL_RTFTEXTMODE_RTF                                                 AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_RTFTEXTMODE_PLAIN                                               AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LL_RTFTEXTMODE_EVALUATED                                           AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_RTFTEXTMODE_RAW                                                 AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) */
    DEFINE PUBLIC STATIC PROPERTY LL_RTFTEXTFLAG_ALL                                                 AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_RTFTEXTFLAG_SELECTION                                           AS INTEGER NO-UNDO INIT           4 GET. /* (0x0004) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_JOBHANDLE                                               AS INTEGER NO-UNDO INIT          -1 GET. /* (-1) bad jobhandle */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_TASK_ACTIVE                                                 AS INTEGER NO-UNDO INIT          -2 GET. /* (-2) LlDefineLayout() only once in a job */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_OBJECTTYPE                                              AS INTEGER NO-UNDO INIT          -3 GET. /* (-3) nObjType must be one of the allowed values (obsolete constant) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_PROJECTTYPE                                             AS INTEGER NO-UNDO INIT          -3 GET. /* (-3) nObjType must be one of the allowed values */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_PRINTING_JOB                                                AS INTEGER NO-UNDO INIT          -4 GET. /* (-4) print job not opened, no print object */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_BOX                                                      AS INTEGER NO-UNDO INIT          -5 GET. /* (-5) LlPrintSetBoxText(...) called when no abort box exists! */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_ALREADY_PRINTING                                            AS INTEGER NO-UNDO INIT          -6 GET. /* (-6) the current operation cannot be performed while a print job is open */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOT_YET_PRINTING                                            AS INTEGER NO-UNDO INIT          -7 GET. /* (-7) LlPrintGetOptionString... */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_PROJECT                                                  AS INTEGER NO-UNDO INIT         -10 GET. /* (-10) object with requested name does not exist (former ERR_NO_OBJECT) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_PRINTER                                                  AS INTEGER NO-UNDO INIT         -11 GET. /* (-11) printer couldn't be opened */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_PRINTING                                                    AS INTEGER NO-UNDO INIT         -12 GET. /* (-12) error while printing */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_EXPORTING                                                   AS INTEGER NO-UNDO INIT         -13 GET. /* (-13) error while exporting */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NEEDS_VB                                                    AS INTEGER NO-UNDO INIT         -14 GET. /* (-14) '11...' needs VB.EXE */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_PRINTER                                                 AS INTEGER NO-UNDO INIT         -15 GET. /* (-15) PrintOptionsDialog(): no printer available */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_PREVIEWMODE                                              AS INTEGER NO-UNDO INIT         -16 GET. /* (-16) Preview functions: not in preview mode */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_PREVIEWFILES                                             AS INTEGER NO-UNDO INIT         -17 GET. /* (-17) PreviewDisplay(): no file found */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_PARAMETER                                                   AS INTEGER NO-UNDO INIT         -18 GET. /* (-18) bad parameter (usually NULL pointer) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_EXPRESSION                                              AS INTEGER NO-UNDO INIT         -19 GET. /* (-19) bad expression in LlExprEvaluate() and LlExprType() */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_EXPRMODE                                                AS INTEGER NO-UNDO INIT         -20 GET. /* (-20) bad expression mode (LlSetExpressionMode()) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_TABLE                                                    AS INTEGER NO-UNDO INIT         -21 GET. /* (-21) not used */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CFGNOTFOUND                                                 AS INTEGER NO-UNDO INIT         -22 GET. /* (-22) on LlPrintStart(), LlPrintWithBoxStart() [not found] */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_EXPRESSION                                                  AS INTEGER NO-UNDO INIT         -23 GET. /* (-23) on LlPrintStart(), LlPrintWithBoxStart() */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CFGBADFILE                                                  AS INTEGER NO-UNDO INIT         -24 GET. /* (-24) on LlPrintStart(), LlPrintWithBoxStart() [read error, bad format] */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BADOBJNAME                                                  AS INTEGER NO-UNDO INIT         -25 GET. /* (-25) on LlPrintEnableObject() - not a ':' at the beginning */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOOBJECT                                                    AS INTEGER NO-UNDO INIT         -26 GET. /* (-26) on LlPrintEnableObject() - "*" and no object in project */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNOBJECT                                               AS INTEGER NO-UNDO INIT         -27 GET. /* (-27) on LlPrintEnableObject() - object with that name not existing */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_TABLEOBJECT                                              AS INTEGER NO-UNDO INIT         -28 GET. /* (-28) LlPrint...Start() and no list in Project, or: */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_OBJECT                                                   AS INTEGER NO-UNDO INIT         -29 GET. /* (-29) LlPrint...Start() and no object in project */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_TEXTOBJECT                                               AS INTEGER NO-UNDO INIT         -30 GET. /* (-30) LlPrintGetTextCharsPrinted() and no printable text in Project! */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWN                                                     AS INTEGER NO-UNDO INIT         -31 GET. /* (-31) LlPrintIsVariableUsed(), LlPrintIsFieldUsed() */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_MODE                                                    AS INTEGER NO-UNDO INIT         -32 GET. /* (-32) LlPrintFields(), LlPrintIsFieldUsed() called on non-OBJECT_LIST */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CFGBADMODE                                                  AS INTEGER NO-UNDO INIT         -33 GET. /* (-33) on LlDefineLayout(), LlPrint...Start(): file is in wrong expression mode */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_ONLYWITHONETABLE                                            AS INTEGER NO-UNDO INIT         -34 GET. /* (-34) on LlDefinePageSeparation(), LlDefineGrouping() */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNVARIABLE                                             AS INTEGER NO-UNDO INIT         -35 GET. /* (-35) on LlGetVariableContents() */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNFIELD                                                AS INTEGER NO-UNDO INIT         -36 GET. /* (-36) on LlGetFieldContents() */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNSORTORDER                                            AS INTEGER NO-UNDO INIT         -37 GET. /* (-37) on LlGetFieldContents() */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOPRINTERCFG                                                AS INTEGER NO-UNDO INIT         -38 GET. /* (-38) on LlPrintCopyPrinterConfiguration() - no or bad file */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_SAVEPRINTERCFG                                              AS INTEGER NO-UNDO INIT         -39 GET. /* (-39) on LlPrintCopyPrinterConfiguration() - file could not be saved */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_RESERVED                                                    AS INTEGER NO-UNDO INIT         -40 GET. /* (-40) function not yet implemeted */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOVALIDPAGES                                                AS INTEGER NO-UNDO INIT         -41 GET. /* (-41) could also be that 16 bit Viewer tries to open 32bit-only storage */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOTINHOSTPRINTERMODE                                        AS INTEGER NO-UNDO INIT         -42 GET. /* (-42) cannot be done in Host Printer Mode (LlSetPrinterInPrinterFile()) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOTFINISHED                                                 AS INTEGER NO-UNDO INIT         -43 GET. /* (-43) appears when a project reset() is done, but the table not finished */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BUFFERTOOSMALL                                              AS INTEGER NO-UNDO INIT         -44 GET. /* (-44) LlXXGetOptionStr() */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BADCODEPAGE                                                 AS INTEGER NO-UNDO INIT         -45 GET. /* (-45) LL_OPTION_CODEPAGE */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CANNOTCREATETEMPFILE                                        AS INTEGER NO-UNDO INIT         -46 GET. /* (-46) cannot create temporary file */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NODESTINATION                                               AS INTEGER NO-UNDO INIT         -47 GET. /* (-47) no valid export destination */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOCHART                                                     AS INTEGER NO-UNDO INIT         -48 GET. /* (-48) no chart control present */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_TOO_MANY_CONCURRENT_PRINTJOBS                               AS INTEGER NO-UNDO INIT         -49 GET. /* (-49) WebServer: not enough print process licenses */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_WEBSERVER_LICENSE                                       AS INTEGER NO-UNDO INIT         -50 GET. /* (-50) WebServer: bad license file */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_WEBSERVER_LICENSE                                        AS INTEGER NO-UNDO INIT         -51 GET. /* (-51) WebServer: no license file */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_INVALIDDATE                                                 AS INTEGER NO-UNDO INIT         -52 GET. /* (-52) LlSystemTimeFromLocaleString(): date not valid! */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_DRAWINGNOTFOUND                                             AS INTEGER NO-UNDO INIT         -53 GET. /* (-53) only if LL_OPTION_ERR_ON_FILENOTFOUND set */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOUSERINTERACTION                                           AS INTEGER NO-UNDO INIT         -54 GET. /* (-54) a call is used which would show a dialog, but LL is in Webserver mode */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BADDATABASESTRUCTURE                                        AS INTEGER NO-UNDO INIT         -55 GET. /* (-55) the project that is loading has a table that is not supported by the database */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_UNKNOWNPROPERTY                                             AS INTEGER NO-UNDO INIT         -56 GET. /* (-56) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_INVALIDOPERATION                                            AS INTEGER NO-UNDO INIT         -57 GET. /* (-57) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_PROPERTY_ALREADY_DEFINED                                    AS INTEGER NO-UNDO INIT         -58 GET. /* (-58) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_CFGFOUND                                                    AS INTEGER NO-UNDO INIT         -59 GET. /* (-59) on LlPrjOpen() with CREATE_NEW disposition, or of project file is r/o and access flag is r/w */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_SAVECFG                                                     AS INTEGER NO-UNDO INIT         -60 GET. /* (-60) error while saving (LlProjectSave()) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_WRONGTHREAD                                                 AS INTEGER NO-UNDO INIT         -61 GET. /* (-61) internal (.NET) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_SUCH_INFORMATION                                         AS INTEGER NO-UNDO INIT         -62 GET. /* (-62) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_USER_ABORTED                                                AS INTEGER NO-UNDO INIT         -99 GET. /* (-99) user aborted printing */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_BAD_DLLS                                                    AS INTEGER NO-UNDO INIT        -100 GET. /* (-100) DLLs not up to date (CTL, DWG, UTIL) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_LANG_DLL                                                 AS INTEGER NO-UNDO INIT        -101 GET. /* (-101) no or out-of-date language resource DLL */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_MEMORY                                                   AS INTEGER NO-UNDO INIT        -102 GET. /* (-102) out of memory */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_EXCEPTION                                                   AS INTEGER NO-UNDO INIT        -104 GET. /* (-104) there was a GPF during the API execution. Any action that follows might cause problems! */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_LICENSEVIOLATION                                            AS INTEGER NO-UNDO INIT        -105 GET. /* (-105) your license does not allow this call (see LL_OPTIONSTR_LICENSINGINFO) */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NOT_SUPPORTED_IN_THIS_OS                                    AS INTEGER NO-UNDO INIT        -106 GET. /* (-106) the OS does not support this function */
    DEFINE PUBLIC STATIC PROPERTY LL_ERR_NO_MORE_DATA                                                AS INTEGER NO-UNDO INIT        -107 GET. /* (-107) */
    DEFINE PUBLIC STATIC PROPERTY LL_WRN_REPORTPARAMETERS_COLLECTION_FINISHED                        AS INTEGER NO-UNDO INIT        -994 GET. /* (-994) internal use */
    DEFINE PUBLIC STATIC PROPERTY LL_WRN_ISNULL                                                      AS INTEGER NO-UNDO INIT        -995 GET. /* (-995) LlExprEvaluate[Var]() */
    DEFINE PUBLIC STATIC PROPERTY LL_WRN_TABLECHANGE                                                 AS INTEGER NO-UNDO INIT        -996 GET. /* (-996) */
    DEFINE PUBLIC STATIC PROPERTY LL_WRN_PRINTFINISHED                                               AS INTEGER NO-UNDO INIT        -997 GET. /* (-997) LlRTFDisplay() */
    DEFINE PUBLIC STATIC PROPERTY LL_WRN_REPEAT_DATA                                                 AS INTEGER NO-UNDO INIT        -998 GET. /* (-998) notification: page is full, prepare for next page */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_TEXTQUOTE                                                  AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_PHANTOMSPACE                                               AS INTEGER NO-UNDO INIT        8203 GET. /* (0x200b) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_LOCK                                                       AS INTEGER NO-UNDO INIT        8288 GET. /* (0x2060) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_NEWLINE                                                    AS INTEGER NO-UNDO INIT         182 GET. /* (182) "¶" */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_EXPRSEP                                                    AS INTEGER NO-UNDO INIT         164 GET. /* (164) "¤" */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_TAB                                                        AS INTEGER NO-UNDO INIT         247 GET. /* (247) "÷" */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_EAN128NUL                                                  AS INTEGER NO-UNDO INIT         255 GET. /* (255) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_EAN128FNC1                                                 AS INTEGER NO-UNDO INIT         254 GET. /* (254) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_EAN128FNC2                                                 AS INTEGER NO-UNDO INIT         253 GET. /* (253) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_EAN128FNC3                                                 AS INTEGER NO-UNDO INIT         252 GET. /* (252) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_EAN128FNC4                                                 AS INTEGER NO-UNDO INIT         251 GET. /* (251) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_CODE93NUL                                                  AS INTEGER NO-UNDO INIT         255 GET. /* (255) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_CODE93EXDOLLAR                                             AS INTEGER NO-UNDO INIT         254 GET. /* (254) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_CODE93EXPERC                                               AS INTEGER NO-UNDO INIT         253 GET. /* (253) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_CODE93EXSLASH                                              AS INTEGER NO-UNDO INIT         252 GET. /* (252) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_CODE93EXPLUS                                               AS INTEGER NO-UNDO INIT         251 GET. /* (251) */
    DEFINE PUBLIC STATIC PROPERTY LL_CHAR_CODE39NUL                                                  AS INTEGER NO-UNDO INIT         255 GET. /* (255) */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGEXPR_VAREXTBTN_ENABLE                                        AS INTEGER NO-UNDO INIT           1 GET. /* (0x00000001) callback for simple Wizard extension */
    DEFINE PUBLIC STATIC PROPERTY LL_DLGEXPR_VAREXTBTN_DOMODAL                                       AS INTEGER NO-UNDO INIT           2 GET. /* (0x00000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_LLX_EXTENSIONTYPE_EXPORT                                        AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_LLX_EXTENSIONTYPE_BARCODE                                       AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_LLX_EXTENSIONTYPE_OBJECT                                        AS INTEGER NO-UNDO INIT           3 GET. /* (3) nyi */
    DEFINE PUBLIC STATIC PROPERTY LL_LLX_EXTENSIONTYPE_WIZARD                                        AS INTEGER NO-UNDO INIT           4 GET. /* (4) nyi */
    DEFINE PUBLIC STATIC PROPERTY LL_LLX_EXTENSIONTYPEFLAG_FORCE_PUBLIC                              AS INTEGER NO-UNDO INIT       65536 GET. /* (0x00010000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LLX_EXTENSIONTYPEFLAG_FORCE_PRIVATE                             AS INTEGER NO-UNDO INIT      131072 GET. /* (0x00020000) */
    DEFINE PUBLIC STATIC PROPERTY LL_LLX_EXTENSIONTYPE_TYPEMASK                                      AS INTEGER NO-UNDO INIT          15 GET. /* (0x0000000f) */
    DEFINE PUBLIC STATIC PROPERTY LL_DECLARECHARTROW_FOR_OBJECTS                                     AS INTEGER NO-UNDO INIT           1 GET. /* (0x00000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_DECLARECHARTROW_FOR_TABLECOLUMNS                                AS INTEGER NO-UNDO INIT           2 GET. /* (0x00000002) body only */
    DEFINE PUBLIC STATIC PROPERTY LL_DECLARECHARTROW_FOR_TABLECOLUMNS_FOOTERS                        AS INTEGER NO-UNDO INIT           4 GET. /* (0x00000004) */
    DEFINE PUBLIC STATIC PROPERTY LL_GETCHARTOBJECTCOUNT_CHARTOBJECTS                                AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_GETCHARTOBJECTCOUNT_CHARTOBJECTS_BEFORE_TABLE                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_GETCHARTOBJECTCOUNT_CHARTCOLUMNS                                AS INTEGER NO-UNDO INIT           3 GET. /* (3) body only */
    DEFINE PUBLIC STATIC PROPERTY LL_GETCHARTOBJECTCOUNT_CHARTCOLUMNS_FOOTERS                        AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_GRIPT_DIM_SCM                                                   AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_GRIPT_DIM_PERC                                                  AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERFLAG_PUBLIC                                            AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERFLAG_SAVEDEFAULT                                       AS INTEGER NO-UNDO INIT       65536 GET. /* (0x00010000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERFLAG_PRIVATE                                           AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERFLAG_FORMULA                                           AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERFLAG_VALUE                                             AS INTEGER NO-UNDO INIT   536870912 GET. /* (0x20000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERFLAG_GLOBAL                                            AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERFLAG_LOCAL                                             AS INTEGER NO-UNDO INIT   268435456 GET. /* (0x10000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERFLAG_MASK                                              AS INTEGER NO-UNDO INIT      -65536 GET. /* (0xffff0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERTYPE_USER                                              AS INTEGER NO-UNDO INIT           0 GET. /* (0) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERTYPE_FAX                                               AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERTYPE_MAIL                                              AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERTYPE_LLINTERNAL                                        AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_PARAMETERTYPE_MASK                                              AS INTEGER NO-UNDO INIT          15 GET. /* (0x0000000f) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_AM_READWRITE                                            AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_AM_READONLY                                             AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) default */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_AM_MASK                                                 AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_CD_OPEN_EXISTING                                        AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) fails if it does not exist - default */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_CD_CREATE_ALWAYS                                        AS INTEGER NO-UNDO INIT   268435456 GET. /* (0x10000000) open (but do not read contents) if exists, create if not */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_CD_CREATE_NEW                                           AS INTEGER NO-UNDO INIT   536870912 GET. /* (0x20000000) fails if already exists */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_CD_OPEN_ALWAYS                                          AS INTEGER NO-UNDO INIT   805306368 GET. /* (0x30000000) open (and load) if exists, create if not */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_CD_MASK                                                 AS INTEGER NO-UNDO INIT   805306368 GET. /* (0x30000000) fails if it does not exist */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_EM_IGNORE_FORMULAERRORS                                 AS INTEGER NO-UNDO INIT   134217728 GET. /* (0x08000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_EM_MASK                                                 AS INTEGER NO-UNDO INIT   134217728 GET. /* (0x08000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_FLG_NOINITPRINTER                                       AS INTEGER NO-UNDO INIT    67108864 GET. /* (0x04000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_FLG_NOOBJECTLOAD                                        AS INTEGER NO-UNDO INIT    33554432 GET. /* (0x02000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_PRJOPEN_FLG_RESERVED                                            AS INTEGER NO-UNDO INIT    16777216 GET. /* (0x01000000) internal use */
    DEFINE PUBLIC STATIC PROPERTY LL_ASSOCIATEPREVIEWCONTROLFLAG_DELETE_ON_CLOSE                     AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LL_ASSOCIATEPREVIEWCONTROLFLAG_HANDLE_IS_ATTACHINFO                AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) */
    DEFINE PUBLIC STATIC PROPERTY LL_ASSOCIATEPREVIEWCONTROLFLAG_PRV_REPLACE                         AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) */
    DEFINE PUBLIC STATIC PROPERTY LL_ASSOCIATEPREVIEWCONTROLFLAG_PRV_ADD_TO_CONTROL_STACK            AS INTEGER NO-UNDO INIT           4 GET. /* (0x0004) */
    DEFINE PUBLIC STATIC PROPERTY LL_ASSOCIATEPREVIEWCONTROLFLAG_PRV_ADD_TO_CONTROL_IN_TAB           AS INTEGER NO-UNDO INIT           8 GET. /* (0x0008) */
    DEFINE PUBLIC STATIC PROPERTY LL_ASSOCIATEPREVIEWCONTROLMASK_ATTACHLOCATION                      AS INTEGER NO-UNDO INIT          60 GET. /* (0x003c) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILEOPEN_OPEN_EXISTING                                       AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) fails if it does not exist - default */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILEOPEN_CREATE_ALWAYS                                       AS INTEGER NO-UNDO INIT   268435456 GET. /* (0x10000000) open (but do not read contents) if exists, create if not */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILEOPEN_CREATE_NEW                                          AS INTEGER NO-UNDO INIT   536870912 GET. /* (0x20000000) fails if already exists */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILEOPEN_OPEN_ALWAYS                                         AS INTEGER NO-UNDO INIT   805306368 GET. /* (0x30000000) open (and load) if exists, create if not */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILEOPEN_OPEN_IMPORT                                         AS INTEGER NO-UNDO INIT  1073741824 GET. /* (0x40000000) fails if it does not exist - only imports data */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILEOPEN_OPENMASK                                            AS INTEGER NO-UNDO INIT  1879048192 GET. /* (0x70000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILEOPENFLAG_SUPPRESS_SAVEDIALOG                             AS INTEGER NO-UNDO INIT           1 GET. /* (0x00000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILEOPENFLAG_SUPPRESS_SAVE                                   AS INTEGER NO-UNDO INIT           2 GET. /* (0x00000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESFILESAVE_DEFAULT                                             AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) default */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTIONFLAG_ADD_TO_TOOLBAR                                  AS INTEGER NO-UNDO INIT   536870912 GET. /* (0x20000000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_MENUITEM_APPEND                                     AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_MENUITEM_INSERT                                     AS INTEGER NO-UNDO INIT   268435456 GET. /* (0x10000000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_MENUITEM_POSITIONMASK                               AS INTEGER NO-UNDO INIT   268435456 GET. /* (0x10000000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_ACCEL_CONTROL                                       AS INTEGER NO-UNDO INIT       65536 GET. /* (0x00010000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_ACCEL_SHIFT                                         AS INTEGER NO-UNDO INIT      131072 GET. /* (0x00020000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_ACCEL_ALT                                           AS INTEGER NO-UNDO INIT      262144 GET. /* (0x00040000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_ACCEL_VIRTKEY                                       AS INTEGER NO-UNDO INIT      524288 GET. /* (0x00080000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_ACCEL_KEYMODIFIERMASK                               AS INTEGER NO-UNDO INIT      983040 GET. /* (0x000f0000) */
    DEFINE PUBLIC STATIC PROPERTY LLDESADDACTION_ACCEL_KEYCODEMASK                                   AS INTEGER NO-UNDO INIT       65535 GET. /* (0x0000ffff) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNEROPTSTR_PROJECTFILENAME                                  AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNEROPTSTR_WORKSPACETITLE                                   AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_DESIGNEROPTSTR_PROJECTDESCRIPTION                               AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_USEDIDENTIFIERSFLAG_VARIABLES                                   AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LL_USEDIDENTIFIERSFLAG_FIELDS                                      AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) */
    DEFINE PUBLIC STATIC PROPERTY LL_USEDIDENTIFIERSFLAG_CHARTFIELDS                                 AS INTEGER NO-UNDO INIT           4 GET. /* (0x0004) */
    DEFINE PUBLIC STATIC PROPERTY LL_USEDIDENTIFIERSFLAG_TABLES                                      AS INTEGER NO-UNDO INIT           8 GET. /* (0x0008) */
    DEFINE PUBLIC STATIC PROPERTY LL_USEDIDENTIFIERSFLAG_RELATIONS                                   AS INTEGER NO-UNDO INIT          16 GET. /* (0x0010) */
    DEFINE PUBLIC STATIC PROPERTY LL_USEDIDENTIFIERSFLAG_FILES                                       AS INTEGER NO-UNDO INIT          32 GET. /* (0x0020) */
    DEFINE PUBLIC STATIC PROPERTY LL_TEMPFILENAME_DEFAULT                                            AS INTEGER NO-UNDO INIT           0 GET. /* (0x0000) see UT */
    DEFINE PUBLIC STATIC PROPERTY LL_TEMPFILENAME_ENSURELONGPATH                                     AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) see UT */
    DEFINE PUBLIC STATIC PROPERTY LL_DICTIONARY_TYPE_STATIC                                          AS INTEGER NO-UNDO INIT           1 GET. /* (1) */
    DEFINE PUBLIC STATIC PROPERTY LL_DICTIONARY_TYPE_IDENTIFIER                                      AS INTEGER NO-UNDO INIT           2 GET. /* (2) */
    DEFINE PUBLIC STATIC PROPERTY LL_DICTIONARY_TYPE_TABLE                                           AS INTEGER NO-UNDO INIT           3 GET. /* (3) */
    DEFINE PUBLIC STATIC PROPERTY LL_DICTIONARY_TYPE_RELATION                                        AS INTEGER NO-UNDO INIT           4 GET. /* (4) */
    DEFINE PUBLIC STATIC PROPERTY LL_DICTIONARY_TYPE_SORTORDER                                       AS INTEGER NO-UNDO INIT           5 GET. /* (5) */
    DEFINE PUBLIC STATIC PROPERTY LL_DICTIONARY_TYPE_ALL                                             AS INTEGER NO-UNDO INIT           0 GET. /* (0) only valid for NULL, NULL to clear all dictionaries in one run */
    DEFINE PUBLIC STATIC PROPERTY LL_UILANGUAGETYPE_NORMAL                                           AS INTEGER NO-UNDO INIT           1 GET. /* (0x00000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_UILANGUAGETYPE_TRIAL                                            AS INTEGER NO-UNDO INIT           2 GET. /* (0x00000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_ADDTABLEOPT_SUPPORTSSTACKEDSORTORDERS                           AS INTEGER NO-UNDO INIT           1 GET. /* (0x00000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_ADDTABLEOPT_SUPPORTSADVANCEDFILTERING                           AS INTEGER NO-UNDO INIT           2 GET. /* (0x00000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_INPLACEDESIGNERINTERACTION_QUERY_CANCLOSE                       AS INTEGER NO-UNDO INIT           1 GET. /* (1) wParam = 0, lParam = &BOOL */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRXLATRESULT_OPTIMAL                                          AS INTEGER NO-UNDO INIT           0 GET. /* (0x00000000) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRXLATRESULT_PARTIAL                                          AS INTEGER NO-UNDO INIT           1 GET. /* (0x00000001) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRXLATRESULT_FAIL                                             AS INTEGER NO-UNDO INIT           2 GET. /* (0x00000002) */
    DEFINE PUBLIC STATIC PROPERTY LL_EXPRXLATRESULT_MASK                                             AS INTEGER NO-UNDO INIT           7 GET. /* (0x00000007) some reserve */
    DEFINE PUBLIC STATIC PROPERTY LLJOBOPENCOPYEXFLAG_NO_COPY_FIELDLIST                              AS INTEGER NO-UNDO INIT           1 GET. /* (0x0001) */
    DEFINE PUBLIC STATIC PROPERTY LLJOBOPENCOPYEXFLAG_NO_COPY_DBSTRUCTS                              AS INTEGER NO-UNDO INIT           2 GET. /* (0x0002) */
    DEFINE PUBLIC STATIC PROPERTY LLJOBOPENCOPYEXFLAG_NO_COPY_XLATTABLES                             AS INTEGER NO-UNDO INIT           4 GET. /* (0x0004) */

END CLASS.
