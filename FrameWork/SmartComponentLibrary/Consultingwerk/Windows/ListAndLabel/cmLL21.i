/* Progress constants and function definitions for LL21.DLL */
/*  (c) combit GmbH */
/*  [build of 2016-03-11 00:03:36] */

/*----------------------------------------------------------------------------------------*/
/* This is a generic header file. The example below uses List & Label to demonstrate the  */
/* usage of the header file for a specific DLL, i.e. List & Label.                        */
/*----------------------------------------------------------------------------------------*/
/* + this uses code from List & Label                                                     */
/* + the DLL is being preloaded because of the following preprocessor definition          */
/*                                                                                        */
/* {CMLL21.i}                                                                             */
/*                                                                                        */
/* def var hJob as integer.                                                               */
/*                                                                                        */
/* /* open job */                                                                         */
/* run LlJobOpen(output hJob, {&CMBTLANG_GERMAN}).                                        */
/* ....                                                                                   */
/* /* do what you need to do */                                                           */
/* ....                                                                                   */
/* /* close the job */                                                                    */
/* run LlJobClose(hJob).                                                                  */
/*----------------------------------------------------------------------------------------*/

/* language constants */
&GLOBAL-DEFINE CMBTLANG_DEFAULT    -1
&GLOBAL-DEFINE CMBTLANG_GERMAN     0
&GLOBAL-DEFINE CMBTLANG_ENGLISH    1
&GLOBAL-DEFINE CMBTLANG_ARABIC     2
&GLOBAL-DEFINE CMBTLANG_AFRIKAANS  3
&GLOBAL-DEFINE CMBTLANG_ALBANIAN   4
&GLOBAL-DEFINE CMBTLANG_BASQUE     5
&GLOBAL-DEFINE CMBTLANG_BULGARIAN  6
&GLOBAL-DEFINE CMBTLANG_BYELORUSSIAN  7
&GLOBAL-DEFINE CMBTLANG_CATALAN    8
&GLOBAL-DEFINE CMBTLANG_CHINESE    9
&GLOBAL-DEFINE CMBTLANG_CROATIAN   10
&GLOBAL-DEFINE CMBTLANG_CZECH      11
&GLOBAL-DEFINE CMBTLANG_DANISH     12
&GLOBAL-DEFINE CMBTLANG_DUTCH      13
&GLOBAL-DEFINE CMBTLANG_ESTONIAN   14
&GLOBAL-DEFINE CMBTLANG_FAEROESE   15
&GLOBAL-DEFINE CMBTLANG_FARSI      16
&GLOBAL-DEFINE CMBTLANG_FINNISH    17
&GLOBAL-DEFINE CMBTLANG_FRENCH     18
&GLOBAL-DEFINE CMBTLANG_GREEK      19
&GLOBAL-DEFINE CMBTLANG_HEBREW     20
&GLOBAL-DEFINE CMBTLANG_HUNGARIAN  21
&GLOBAL-DEFINE CMBTLANG_ICELANDIC  22
&GLOBAL-DEFINE CMBTLANG_INDONESIAN  23
&GLOBAL-DEFINE CMBTLANG_ITALIAN    24
&GLOBAL-DEFINE CMBTLANG_JAPANESE   25
&GLOBAL-DEFINE CMBTLANG_KOREAN     26
&GLOBAL-DEFINE CMBTLANG_LATVIAN    27
&GLOBAL-DEFINE CMBTLANG_LITHUANIAN  28
&GLOBAL-DEFINE CMBTLANG_NORWEGIAN  29
&GLOBAL-DEFINE CMBTLANG_POLISH     30
&GLOBAL-DEFINE CMBTLANG_PORTUGUESE  31
&GLOBAL-DEFINE CMBTLANG_ROMANIAN   32
&GLOBAL-DEFINE CMBTLANG_RUSSIAN    33
&GLOBAL-DEFINE CMBTLANG_SLOVAK     34
&GLOBAL-DEFINE CMBTLANG_SLOVENIAN  35
&GLOBAL-DEFINE CMBTLANG_SERBIAN    36
&GLOBAL-DEFINE CMBTLANG_SPANISH    37
&GLOBAL-DEFINE CMBTLANG_SWEDISH    38
&GLOBAL-DEFINE CMBTLANG_THAI       39
&GLOBAL-DEFINE CMBTLANG_TURKISH    40
&GLOBAL-DEFINE CMBTLANG_UKRAINIAN  41
&GLOBAL-DEFINE CMBTLANG_SERBIAN_LATIN 42

/* other constant declarations */
&GLOBAL-DEFINE LL_LINK_HPOS_NONE                                      0 /* (0x0000)  */
&GLOBAL-DEFINE LL_LINK_HPOS_START                                     1 /* (0x0001)  */
&GLOBAL-DEFINE LL_LINK_HPOS_END                                       2 /* (0x0002)  */
&GLOBAL-DEFINE LL_LINK_HPOS_ABS                                       3 /* (0x0003)  */
&GLOBAL-DEFINE LL_LINK_HPOS_MASK                                      7 /* (0x0007)  */
&GLOBAL-DEFINE LL_LINK_VPOS_NONE                                      0 /* (0x0000)  */
&GLOBAL-DEFINE LL_LINK_VPOS_START                                    16 /* (0x0010)  */
&GLOBAL-DEFINE LL_LINK_VPOS_END                                      32 /* (0x0020)  */
&GLOBAL-DEFINE LL_LINK_VPOS_ABS                                      48 /* (0x0030)  */
&GLOBAL-DEFINE LL_LINK_VPOS_MASK                                    112 /* (0x0070)  */
&GLOBAL-DEFINE LL_LINK_HSIZE_NONE                                     0 /* (0x0000)  */
&GLOBAL-DEFINE LL_LINK_HSIZE_PROP                                   256 /* (0x0100)  */
&GLOBAL-DEFINE LL_LINK_HSIZE_INV                                    512 /* (0x0200)  */
&GLOBAL-DEFINE LL_LINK_HSIZE_MASK                                   768 /* (0x0300)  */
&GLOBAL-DEFINE LL_LINK_VSIZE_NONE                                     0 /* (0x0000)  */
&GLOBAL-DEFINE LL_LINK_VSIZE_PROP                                  4096 /* (0x1000)  */
&GLOBAL-DEFINE LL_LINK_VSIZE_INV                                   8192 /* (0x2000)  */
&GLOBAL-DEFINE LL_LINK_VSIZE_MASK                                 12288 /* (0x3000)  */
&GLOBAL-DEFINE LL_LINK_SIZEPOS_MASK                               13175 /* (0x3377)  */
&GLOBAL-DEFINE LL_LINK_SIZEOFPARENT                               16384 /* (0x4000)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTCALLBACK_PREVIEW_START                 1 /* (1)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTCALLBACK_PREVIEW_ABORT                 2 /* (2)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTCALLBACK_PREVIEW_FINALIZE               3 /* (3)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTCALLBACK_PREVIEW_QUEST_JOBSTATE               4 /* (4)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTCALLBACK_EXPORT_START                  5 /* (5)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTCALLBACK_EXPORT_ABORT                  6 /* (6)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTCALLBACK_EXPORT_FINALIZE               7 /* (7)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTCALLBACK_EXPORT_QUEST_JOBSTATE               8 /* (8)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTTHREAD_STATE_STOPPED                   0 /* (0)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTTHREAD_STATE_SUSPENDED                 1 /* (1)  */
&GLOBAL-DEFINE LL_DESIGNERPRINTTHREAD_STATE_RUNNING                   2 /* (2)  */
&GLOBAL-DEFINE LL_DRILLDOWN_START                                     1 /* (1)  */
&GLOBAL-DEFINE LL_DRILLDOWN_FINALIZE                                  2 /* (2)  */
&GLOBAL-DEFINE LL_PRINTJOB_FINALIZE                                   3 /* (3)  */
&GLOBAL-DEFINE LL_JOBOPENFLAG_NOLLXPRELOAD                         4096 /* (0x00001000)  */
&GLOBAL-DEFINE LL_JOBOPENFLAG_ONLYEXACTLANGUAGE                    8192 /* (0x00002000) do not look for '@@' LNG file */
&GLOBAL-DEFINE LL_DEBUG_CMBTLL                                        1 /* (0x0001) debug CMBTLLnn.DLL */
&GLOBAL-DEFINE LL_DEBUG_CMBTDWG                                       2 /* (0x0002) debug CMBTDWnn.DLL */
&GLOBAL-DEFINE LL_DEBUG_CMBTLL_NOCALLBACKS                            4 /* (0x0004)  */
&GLOBAL-DEFINE LL_DEBUG_CMBTLL_NOSTORAGE                              8 /* (0x0008)  */
&GLOBAL-DEFINE LL_DEBUG_CMBTLL_NOWAITDLG                             16 /* (0x0010)  */
&GLOBAL-DEFINE LL_DEBUG_CMBTLL_NOSYSINFO                             32 /* (0x0020)  */
&GLOBAL-DEFINE LL_DEBUG_CMBTLL_LOGTOFILE                             64 /* (0x0040)  */
&GLOBAL-DEFINE LL_DEBUG_CMBTLS                                      128 /* (0x0080) debug CMBTLSnn.DLL */
&GLOBAL-DEFINE LL_DEBUG_PRNINFO                                     256 /* (0x0100) issue basic printer operations */
&GLOBAL-DEFINE LL_DEBUG_CMBTLL_LICINFO                              512 /* (0x0200) issue LIC messages */
&GLOBAL-DEFINE LL_DEBUG_CMBTLL_OBJECTSTATES                        1024 /* (0x0400) issue object states after drawing (realdata printing only) */
&GLOBAL-DEFINE LL_DEBUG_NOPRIVACYDATA                              2048 /* (0x0800) suppress field contents (useful if they might contain private data) */
&GLOBAL-DEFINE LL_DEBUG_FORCE_SYSINFO                              4096 /* (0x1000) issue sysinfo even though it has been issued once already */
&GLOBAL-DEFINE LL_DEBUG_EVAL2HOSTEXPRESSION                        8192 /* (0x2000) debug Eval2HostExpr processing */
&GLOBAL-DEFINE LL_DEBUG_EXTENDED_DEBWINFLAGS                -2147483648 /* (0x80000000) internal usage */
&GLOBAL-DEFINE LL_VERSION_MAJOR                                       1 /* (1) direct return of major version (f.ex. 1) */
&GLOBAL-DEFINE LL_VERSION_MINOR                                       2 /* (2) direct return of minor version (f.ex. 13) */
&GLOBAL-DEFINE LL_CMND_DRAW_USEROBJ                                   0 /* (0) callback for LL_DRAWING_USEROBJ */
&GLOBAL-DEFINE LL_CMND_EDIT_USEROBJ                                   1 /* (1) callback for LL_DRAWING_USEROBJ_DLG */
&GLOBAL-DEFINE LL_CMND_GETSIZE_USEROBJ                                2 /* (2)  */
&GLOBAL-DEFINE LL_CMND_GETSIZE_USEROBJ_SCM                            3 /* (3)  */
&GLOBAL-DEFINE LL_CMND_GETSIZE_USEROBJ_PIXEL                          4 /* (4)  */
&GLOBAL-DEFINE LL_CMND_TABLELINE                                     10 /* (10) callback for LL_CB_TABLELINE */
&GLOBAL-DEFINE LL_TABLE_LINE_HEADER                                   0 /* (0)  */
&GLOBAL-DEFINE LL_TABLE_LINE_BODY                                     1 /* (1)  */
&GLOBAL-DEFINE LL_TABLE_LINE_FOOTER                                   2 /* (2)  */
&GLOBAL-DEFINE LL_TABLE_LINE_FILL                                     3 /* (3)  */
&GLOBAL-DEFINE LL_TABLE_LINE_GROUP                                    4 /* (4)  */
&GLOBAL-DEFINE LL_TABLE_LINE_GROUPFOOTER                              5 /* (5)  */
&GLOBAL-DEFINE LL_CMND_TABLEFIELD                                    11 /* (11) callback for LL_CB_TABLEFIELD */
&GLOBAL-DEFINE LL_TABLE_FIELD_HEADER                                  0 /* (0)  */
&GLOBAL-DEFINE LL_TABLE_FIELD_BODY                                    1 /* (1)  */
&GLOBAL-DEFINE LL_TABLE_FIELD_FOOTER                                  2 /* (2)  */
&GLOBAL-DEFINE LL_TABLE_FIELD_FILL                                    3 /* (3)  */
&GLOBAL-DEFINE LL_TABLE_FIELD_GROUP                                   4 /* (4)  */
&GLOBAL-DEFINE LL_TABLE_FIELD_GROUPFOOTER                             5 /* (5)  */
&GLOBAL-DEFINE LL_CMND_EVALUATE                                      12 /* (12) callback for "External$" function */
&GLOBAL-DEFINE LL_CMND_OBJECT                                        20 /* (20) callback of LL_CB_OBJECT */
&GLOBAL-DEFINE LL_CMND_PAGE                                          21 /* (21) callback of LL_CB_PAGE */
&GLOBAL-DEFINE LL_CMND_PROJECT                                       22 /* (22) callback of LL_CB_PROJECT */
&GLOBAL-DEFINE LL_CMND_DRAW_GROUP_BEGIN                              23 /* (23) callback for LlPrintBeginGroup */
&GLOBAL-DEFINE LL_CMND_DRAW_GROUP_END                                24 /* (24) callback for LlPrintEndGroup */
&GLOBAL-DEFINE LL_CMND_DRAW_GROUPLINE                                25 /* (25) callback for LlPrintGroupLine */
&GLOBAL-DEFINE LL_RSP_GROUP_IMT                                       0 /* (0)  */
&GLOBAL-DEFINE LL_RSP_GROUP_NEXTPAGE                                  1 /* (1)  */
&GLOBAL-DEFINE LL_RSP_GROUP_OK                                        2 /* (2)  */
&GLOBAL-DEFINE LL_RSP_GROUP_DRAWFOOTER                                3 /* (3)  */
&GLOBAL-DEFINE LL_CMND_HELP                                          30 /* (30) lParam: HIWORD=HELP_xxx, LOWORD=Context # */
&GLOBAL-DEFINE LL_CMND_ENABLEMENU                                    31 /* (31) undoc: lParam/LOWORD(lParam) = HMENU */
&GLOBAL-DEFINE LL_CMND_MODIFYMENU                                    32 /* (32) undoc: lParam/LOWORD(lParam) = HMENU */
&GLOBAL-DEFINE LL_CMND_SELECTMENU                                    33 /* (33) undoc: lParam=ID (return TRUE if processed) */
&GLOBAL-DEFINE LL_CMND_GETVIEWERBUTTONSTATE                          34 /* (34) HIWORD(lParam)=ID, LOWORD(lParam)=state */
&GLOBAL-DEFINE LL_CMND_VARHELPTEXT                                   35 /* (35) lParam=LPSTR(Name), returns LPSTR(Helptext) */
&GLOBAL-DEFINE LL_INFO_METER                                         37 /* (37) lParam = addr(scLlMeterInfo) */
&GLOBAL-DEFINE LL_METERJOB_LOAD                                       1 /* (1)  */
&GLOBAL-DEFINE LL_METERJOB_SAVE                                       2 /* (2)  */
&GLOBAL-DEFINE LL_METERJOB_CONSISTENCYCHECK                           3 /* (3)  */
&GLOBAL-DEFINE LL_METERJOB_PASS2                                      4 /* (4)  */
&GLOBAL-DEFINE LL_METERJOB_PASS3                                      5 /* (5)  */
&GLOBAL-DEFINE LL_NTFY_FAILSFILTER                                 1000 /* (1000) data set fails filter expression */
&GLOBAL-DEFINE LL_NTFY_VIEWERBTNCLICKED                              38 /* (38) user presses a preview button (action will be done). lParam=ID. result: 0=allowed, 1=not allowed */
&GLOBAL-DEFINE LL_CMND_DLGEXPR_VARBTN                                39 /* (39) lParam: @scLlDlgExprVarExt, return: IDOK for ok */
&GLOBAL-DEFINE LL_CMND_HOSTPRINTER                                   40 /* (40) lParam: scLlPrinter */
&GLOBAL-DEFINE LL_PRN_CREATE_DC                                       1 /* (1) scLlPrinter._nCmd values */
&GLOBAL-DEFINE LL_PRN_DELETE_DC                                       2 /* (2)  */
&GLOBAL-DEFINE LL_PRN_SET_ORIENTATION                                 3 /* (3)  */
&GLOBAL-DEFINE LL_PRN_GET_ORIENTATION                                 4 /* (4)  */
&GLOBAL-DEFINE LL_PRN_EDIT                                            5 /* (5) unused */
&GLOBAL-DEFINE LL_PRN_GET_DEVICENAME                                  6 /* (6)  */
&GLOBAL-DEFINE LL_PRN_GET_DRIVERNAME                                  7 /* (7)  */
&GLOBAL-DEFINE LL_PRN_GET_PORTNAME                                    8 /* (8)  */
&GLOBAL-DEFINE LL_PRN_RESET_DC                                        9 /* (9)  */
&GLOBAL-DEFINE LL_PRN_COMPARE_PRINTER                                10 /* (10)  */
&GLOBAL-DEFINE LL_PRN_GET_PHYSPAGE                                   11 /* (11)  */
&GLOBAL-DEFINE LL_PRN_SET_PHYSPAGE                                   12 /* (12)  */
&GLOBAL-DEFINE LL_PRN_GET_PAPERFORMAT                                13 /* (13) fill _nPaperFormat */
&GLOBAL-DEFINE LL_PRN_SET_PAPERFORMAT                                14 /* (14) _nPaperFormat, _xPaperSize, _yPaperSize */
&GLOBAL-DEFINE LL_OEM_TOOLBAR_START                                  41 /* (41)  */
&GLOBAL-DEFINE LL_OEM_TOOLBAR_END                                    50 /* (50)  */
&GLOBAL-DEFINE LL_NTFY_EXPRERROR                                     51 /* (51) lParam = LPCSTR(error text) */
&GLOBAL-DEFINE LL_CMND_CHANGE_DCPROPERTIES_CREATE                    52 /* (52) lParam = addr(scLlPrinter), _hDC is valid */
&GLOBAL-DEFINE LL_CMND_CHANGE_DCPROPERTIES_DOC                       53 /* (53) lParam = addr(scLlPrinter), _hDC is valid */
&GLOBAL-DEFINE LL_CMND_CHANGE_DCPROPERTIES_PAGE                      54 /* (54) lParam = addr(scLlPrinter), _hDC is valid */
&GLOBAL-DEFINE LL_CMND_CHANGE_DCPROPERTIES_PREPAGE                   56 /* (56) lParam = addr(scLlPrinter), _hDC and _pszBuffer( DEVMODE* ) are valid */
&GLOBAL-DEFINE LL_CMND_MODIFY_METAFILE                               57 /* (57) lParam = handle of metafile (32 bit: enh. metafile) */
&GLOBAL-DEFINE LL_INFO_PRINTJOBSUPERVISION                           58 /* (58) lParam = addr(scLlPrintJobInfo) */
&GLOBAL-DEFINE LL_CMND_DELAYEDVALUE                                  59 /* (59) lParam = addr(scLlDelayedValue) */
&GLOBAL-DEFINE LL_CMND_SUPPLY_USERDATA                               60 /* (60) lParam = addr(scLlProjectUserData) */
&GLOBAL-DEFINE LL_CMND_SAVEFILENAME                                  61 /* (61) lParam = LPCTSTR(Filename) */
&GLOBAL-DEFINE LL_QUERY_IS_VARIABLE_OR_FIELD                         62 /* (62) lParam = addr(scLlDelayDefineFieldOrVariable), must be enabled by CB mask. If returns TRUE, the var must be defined in the callback... */
&GLOBAL-DEFINE LL_NTFY_PROJECTLOADED                                 63 /* (63) lParam = 0 */
&GLOBAL-DEFINE LL_QUERY_DESIGNERACTIONSTATE                          64 /* (64)  */
&GLOBAL-DEFINE LL_NTFY_DESIGNERREADY                                 65 /* (65) lParam = 0 */
&GLOBAL-DEFINE LL_NTFY_DESIGNERPRINTJOB                              66 /* (66)  */
&GLOBAL-DEFINE LL_NTFY_VIEWERDRILLDOWN                               67 /* (67)  */
&GLOBAL-DEFINE LL_NTFY_QUEST_DRILLDOWNDENIED                         68 /* (68) see LS_VIEWERCONTROL_QUEST_DRILLDOWNDENIED */
&GLOBAL-DEFINE LL_QUERY_DRILLDOWN_ADDITIONAL_BASETABLES_FOR_VARIABLES              69 /* (69) lParam = scLlDDFilterInfo* */
&GLOBAL-DEFINE LL_QUERY_DRILLDOWN_ADDITIONAL_TABLES                  70 /* (70) lParam = scLlDDFilterInfo* */
&GLOBAL-DEFINE LL_NTFY_DRILLDOWN_DESIGNERACTION                      71 /* (71) lParam = scLlDDDesignerActionW* */
&GLOBAL-DEFINE LL_NTFY_INPLACEDESIGNER_START                         72 /* (72)  */
&GLOBAL-DEFINE LL_NTFY_INPLACEDESIGNER_END                           73 /* (73)  */
&GLOBAL-DEFINE LL_QUERY_OWN_MENU                                     74 /* (74) lParam = HMENU -> return 1 if uses own menu */
&GLOBAL-DEFINE LL_CMND_UPDATE_MENU                                   75 /* (75)  */
&GLOBAL-DEFINE LL_NTFY_FRAMEHANDLE                                   76 /* (76) lParam -> handle of layout window ("L&LFrame") */
&GLOBAL-DEFINE LL_QUERY_DEFAULTPROJECTSTREAM                         77 /* (77) lParam -> IStream**. Return NONZERO when stream contains data */
&GLOBAL-DEFINE LL_NTFY_QUEST_RP_REALDATAJOBDENIED                    78 /* (78)  */
&GLOBAL-DEFINE LL_NTFY_QUEST_EXPANDABLEREGIONSJOBDENIED              79 /* (79)  */
&GLOBAL-DEFINE LL_NTFY_QUEST_INTERACTIVESORTINGJOBDENIED              80 /* (80)  */
&GLOBAL-DEFINE LL_QUERY_EXPR2HOSTEXPRESSION                          81 /* (81) lParam = LLEXPR2HOSTEXPR*, return 0 for FAIL, 1 for OPTIMAL, 2 for INEXACT */
&GLOBAL-DEFINE LL_NTFY_REPORTPARAMETERS_COLLECTED                    82 /* (82) lParam = &scNtfyReportparametersCollected, return LL_ERR_USER_ABORTED to abort, 0x01 to get RP stream, 0x02 to get RP contents, 0 to leave processing */
&GLOBAL-DEFINE LL_NTFY_EXPORTERPAGEFINISHED                          83 /* (83) lParam = &scNtfyExporterPageFinished */
&GLOBAL-DEFINE LL_NTFY_HYPERLINK                                     84 /* (84) lParam = &scNtfyHyperlink */
&GLOBAL-DEFINE LL_NTFY_SAVEREPORTSTATEITEM                           85 /* (85) lParam = &scLLNtfyReportStateItem */
&GLOBAL-DEFINE LL_NTFY_RESTOREREPORTSTATEITEM                        86 /* (86) lParam = &scLLNtfyReportStateItem */
&GLOBAL-DEFINE LL_NTFY_PROGRESS                                      87 /* (87) lParam -> percentage of current progress */
&GLOBAL-DEFINE LL_NTFY_TRIGGEREDJOBINUITHREAD                        88 /* (88) lParam -> user data */
&GLOBAL-DEFINE LL_NTFY_PLEASE_TRANSLATE                              89 /* (89) lParam=BSTR* */
&GLOBAL-DEFINE LL_NTFY_PREVIEW_PRINT_START                           99 /* (99) lParam = &scViewerControlPrintData, return 1 to abort print */
&GLOBAL-DEFINE LL_NTFY_PREVIEW_PRINT_PAGE                           100 /* (100) lParam = &scViewerControlPrintData, return 1 to abort loop */
&GLOBAL-DEFINE LL_NTFY_PREVIEW_PRINT_END                            101 /* (101) lParam = &scViewerControlPrintData */
&GLOBAL-DEFINE LL_NTFY_EMF_PAGE                                     102 /* (102) lParam = &scLLNtfyEMF */
&GLOBAL-DEFINE LL_QUERY_FILENAME_FOR_EXPORTJOB                      103 /* (103) lParam = VARIANT* (in: old filename, out:new filename) */
&GLOBAL-DEFINE OBJECT_LABEL                                           1 /* (1) old - please do not use any more */
&GLOBAL-DEFINE OBJECT_LIST                                            2 /* (2)  */
&GLOBAL-DEFINE OBJECT_CARD                                            3 /* (3)  */
&GLOBAL-DEFINE LL_PROJECT_LABEL                                       1 /* (1) new names... */
&GLOBAL-DEFINE LL_PROJECT_LIST                                        2 /* (2)  */
&GLOBAL-DEFINE LL_PROJECT_CARD                                        3 /* (3)  */
&GLOBAL-DEFINE LL_PROJECT_TOC                                         4 /* (4)  */
&GLOBAL-DEFINE LL_PROJECT_IDX                                         5 /* (5)  */
&GLOBAL-DEFINE LL_PROJECT_GTC                                         6 /* (6)  */
&GLOBAL-DEFINE LL_PROJECT_LAST                                        6 /* (6)  */
&GLOBAL-DEFINE LL_PROJECT_MASK                                       15 /* (0x000f)  */
&GLOBAL-DEFINE LL_OBJ_MARKER                                          0 /* (0) internal use only */
&GLOBAL-DEFINE LL_OBJ_TEXT                                            1 /* (1) the following are used in the object callback */
&GLOBAL-DEFINE LL_OBJ_RECT                                            2 /* (2)  */
&GLOBAL-DEFINE LL_OBJ_LINE                                            3 /* (3)  */
&GLOBAL-DEFINE LL_OBJ_BARCODE                                         4 /* (4)  */
&GLOBAL-DEFINE LL_OBJ_DRAWING                                         5 /* (5)  */
&GLOBAL-DEFINE LL_OBJ_TABLE                                           6 /* (6)  */
&GLOBAL-DEFINE LL_OBJ_TEMPLATE                                        7 /* (7)  */
&GLOBAL-DEFINE LL_OBJ_ELLIPSE                                         8 /* (8)  */
&GLOBAL-DEFINE LL_OBJ_GROUP                                           9 /* (9) internal use only */
&GLOBAL-DEFINE LL_OBJ_RTF                                            10 /* (10)  */
&GLOBAL-DEFINE LL_OBJ_LLX                                            11 /* (11)  */
&GLOBAL-DEFINE LL_OBJ_INPUT                                          12 /* (12)  */
&GLOBAL-DEFINE LL_OBJ_LAST                                           12 /* (12) last object type (for loops as upper bound) - if this is changed, change the contants in object.c too! */
&GLOBAL-DEFINE LL_OBJ_REPORTCONTAINER                               253 /* (253) for exporter */
&GLOBAL-DEFINE LL_OBJ_PROJECT                                       254 /* (254) for exporter */
&GLOBAL-DEFINE LL_OBJ_PAGE                                          255 /* (255) for exporter */
&GLOBAL-DEFINE LL_DELAYEDVALUE                              -2147483648 /* (0x80000000)  */
&GLOBAL-DEFINE LL_TYPEMASK                                   2147418112 /* (0x7fff0000)  */
&GLOBAL-DEFINE LL_TABLE_FIELDTYPEMASK                             63488 /* (0x0000f800) internal use */
&GLOBAL-DEFINE LL_SUBTYPEMASK                                      1023 /* (0x000003ff)  */
&GLOBAL-DEFINE LL_TYPEFLAGS                                 -2147420160 /* (0x8000f800)  */
&GLOBAL-DEFINE LL_HANDLESTREAM_TYPE_MASK                           1024 /* (0x00000400)  */
&GLOBAL-DEFINE LL_HANDLESTREAM_BLOB                                   0 /* (0x00000000) binary data (images only) */
&GLOBAL-DEFINE LL_HANDLESTREAM_UTF8                                   0 /* (0x00000000) UTF8-encoded string (non-images only) */
&GLOBAL-DEFINE LL_HANDLESTREAM_UTF16                               1024 /* (0x00000400) UTF16-encoded string (non-images only) */
&GLOBAL-DEFINE LL_TABLE_FOOTERFIELD                               32768 /* (0x00008000) 'or'ed for footline-only fields // reserved also for Variables (see "$$xx$$")!!!! */
&GLOBAL-DEFINE LL_TABLE_GROUPFIELD                                16384 /* (0x00004000) 'or'ed for groupline-only fields */
&GLOBAL-DEFINE LL_TABLE_HEADERFIELD                                8192 /* (0x00002000) 'or'ed for headline-only fields */
&GLOBAL-DEFINE LL_TABLE_BODYFIELD                                  4096 /* (0x00001000) 'or'ed for headline-only fields */
&GLOBAL-DEFINE LL_TABLE_GROUPFOOTERFIELD                           2048 /* (0x00000800) 'or'ed for group-footer-line-only fields */
&GLOBAL-DEFINE LL_BARCODE                                    1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_BARCODE_METHODMASK                                255 /* (0x000000ff)  */
&GLOBAL-DEFINE LL_BARCODE_WITHTEXT                                  256 /* (0x00000100)  */
&GLOBAL-DEFINE LL_BARCODE_WITHOUTTEXT                               512 /* (0x00000200)  */
&GLOBAL-DEFINE LL_BARCODE_TEXTDONTCARE                                0 /* (0x00000000)  */
&GLOBAL-DEFINE LL_BARCODE_EAN13                              1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_BARCODE_EAN8                               1073741825 /* (0x40000001)  */
&GLOBAL-DEFINE LL_BARCODE_GTIN13                             1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_BARCODE_GTIN8                              1073741825 /* (0x40000001)  */
&GLOBAL-DEFINE LL_BARCODE_UPCA                               1073741826 /* (0x40000002)  */
&GLOBAL-DEFINE LL_BARCODE_UPCE                               1073741827 /* (0x40000003)  */
&GLOBAL-DEFINE LL_BARCODE_3OF9                               1073741828 /* (0x40000004)  */
&GLOBAL-DEFINE LL_BARCODE_25INDUSTRIAL                       1073741829 /* (0x40000005)  */
&GLOBAL-DEFINE LL_BARCODE_25INTERLEAVED                      1073741830 /* (0x40000006)  */
&GLOBAL-DEFINE LL_BARCODE_25DATALOGIC                        1073741831 /* (0x40000007)  */
&GLOBAL-DEFINE LL_BARCODE_25MATRIX                           1073741832 /* (0x40000008)  */
&GLOBAL-DEFINE LL_BARCODE_POSTNET                            1073741833 /* (0x40000009)  */
&GLOBAL-DEFINE LL_BARCODE_FIM                                1073741834 /* (0x4000000A)  */
&GLOBAL-DEFINE LL_BARCODE_CODABAR                            1073741835 /* (0x4000000B)  */
&GLOBAL-DEFINE LL_BARCODE_EAN128                             1073741836 /* (0x4000000C)  */
&GLOBAL-DEFINE LL_BARCODE_GS1_128                            1073741836 /* (0x4000000C)  */
&GLOBAL-DEFINE LL_BARCODE_CODE128                            1073741837 /* (0x4000000D)  */
&GLOBAL-DEFINE LL_BARCODE_DP_LEITCODE                        1073741838 /* (0x4000000E)  */
&GLOBAL-DEFINE LL_BARCODE_DP_IDENTCODE                       1073741839 /* (0x4000000F)  */
&GLOBAL-DEFINE LL_BARCODE_GERMAN_PARCEL                      1073741840 /* (0x40000010)  */
&GLOBAL-DEFINE LL_BARCODE_CODE93                             1073741841 /* (0x40000011)  */
&GLOBAL-DEFINE LL_BARCODE_MSI                                1073741842 /* (0x40000012)  */
&GLOBAL-DEFINE LL_BARCODE_CODE11                             1073741843 /* (0x40000013)  */
&GLOBAL-DEFINE LL_BARCODE_MSI_10_CD                          1073741844 /* (0x40000014)  */
&GLOBAL-DEFINE LL_BARCODE_MSI_10_10                          1073741845 /* (0x40000015)  */
&GLOBAL-DEFINE LL_BARCODE_MSI_11_10                          1073741846 /* (0x40000016)  */
&GLOBAL-DEFINE LL_BARCODE_MSI_PLAIN                          1073741847 /* (0x40000017)  */
&GLOBAL-DEFINE LL_BARCODE_EAN14                              1073741848 /* (0x40000018)  */
&GLOBAL-DEFINE LL_BARCODE_GTIN14                             1073741848 /* (0x40000018)  */
&GLOBAL-DEFINE LL_BARCODE_UCC14                              1073741849 /* (0x40000019)  */
&GLOBAL-DEFINE LL_BARCODE_CODE39                             1073741850 /* (0x4000001A)  */
&GLOBAL-DEFINE LL_BARCODE_CODE39_CRC43                       1073741851 /* (0x4000001B)  */
&GLOBAL-DEFINE LL_BARCODE_PZN                                1073741852 /* (0x4000001C)  */
&GLOBAL-DEFINE LL_BARCODE_CODE39_EXT                         1073741853 /* (0x4000001D)  */
&GLOBAL-DEFINE LL_BARCODE_JAPANESE_POSTAL                    1073741854 /* (0x4000001E)  */
&GLOBAL-DEFINE LL_BARCODE_RM4SCC                             1073741855 /* (0x4000001F)  */
&GLOBAL-DEFINE LL_BARCODE_RM4SCC_CRC                         1073741856 /* (0x40000020)  */
&GLOBAL-DEFINE LL_BARCODE_SSCC                               1073741857 /* (0x40000021)  */
&GLOBAL-DEFINE LL_BARCODE_ISBN                               1073741858 /* (0x40000022)  */
&GLOBAL-DEFINE LL_BARCODE_GS1                                1073741859 /* (0x40000023)  */
&GLOBAL-DEFINE LL_BARCODE_GS1_TRUNCATED                      1073741860 /* (0x40000024)  */
&GLOBAL-DEFINE LL_BARCODE_GS1_STACKED                        1073741861 /* (0x40000025)  */
&GLOBAL-DEFINE LL_BARCODE_GS1_STACKED_OMNI                   1073741862 /* (0x40000026)  */
&GLOBAL-DEFINE LL_BARCODE_GS1_LIMITED                        1073741863 /* (0x40000027)  */
&GLOBAL-DEFINE LL_BARCODE_GS1_EXPANDED                       1073741864 /* (0x40000028)  */
&GLOBAL-DEFINE LL_BARCODE_INTELLIGENT_MAIL                   1073741865 /* (0x40000029)  */
&GLOBAL-DEFINE LL_BARCODE_PZN8                               1073741866 /* (0x4000002A)  */
&GLOBAL-DEFINE LL_BARCODE_CODE128_FULL                       1073741867 /* (0x4000002B)  */
&GLOBAL-DEFINE LL_BARCODE_EAN128_FULL                        1073741868 /* (0x4000002C)  */
&GLOBAL-DEFINE LL_BARCODE_LLXSTART                           1073741888 /* (0x40000040)  */
&GLOBAL-DEFINE LL_BARCODE_PDF417                             1073741888 /* (0x40000040)  */
&GLOBAL-DEFINE LL_BARCODE_MAXICODE                           1073741889 /* (0x40000041)  */
&GLOBAL-DEFINE LL_BARCODE_MAXICODE_UPS                       1073741890 /* (0x40000042)  */
&GLOBAL-DEFINE LL_BARCODE_DATAMATRIX                         1073741892 /* (0x40000044)  */
&GLOBAL-DEFINE LL_BARCODE_AZTEC                              1073741893 /* (0x40000045)  */
&GLOBAL-DEFINE LL_BARCODE_QRCODE                             1073741894 /* (0x40000046)  */
&GLOBAL-DEFINE LL_BARCODE_DATAMATRIX_PREMIUMADRESS           1073741895 /* (0x40000047)  */
&GLOBAL-DEFINE LL_DRAWING                                     536870912 /* (0x20000000)  */
&GLOBAL-DEFINE LL_DRAWING_METHODMASK                                255 /* (0x000000ff)  */
&GLOBAL-DEFINE LL_DRAWING_HMETA                               536870913 /* (0x20000001)  */
&GLOBAL-DEFINE LL_DRAWING_USEROBJ                             536870914 /* (0x20000002)  */
&GLOBAL-DEFINE LL_DRAWING_USEROBJ_DLG                         536870915 /* (0x20000003)  */
&GLOBAL-DEFINE LL_DRAWING_HBITMAP                             536870916 /* (0x20000004)  */
&GLOBAL-DEFINE LL_DRAWING_HICON                               536870917 /* (0x20000005)  */
&GLOBAL-DEFINE LL_DRAWING_HEMETA                              536870918 /* (0x20000006)  */
&GLOBAL-DEFINE LL_DRAWING_HDIB                                536870919 /* (0x20000007) global handle to BITMAPINFO and bits */
&GLOBAL-DEFINE LL_META_MAXX                                       10000 /* (10000)  */
&GLOBAL-DEFINE LL_META_MAXY                                       10000 /* (10000)  */
&GLOBAL-DEFINE LL_TEXT                                        268435456 /* (0x10000000)  */
&GLOBAL-DEFINE LL_TEXT_ALLOW_WORDWRAP                         268435456 /* (0x10000000)  */
&GLOBAL-DEFINE LL_TEXT_DENY_WORDWRAP                          268435457 /* (0x10000001)  */
&GLOBAL-DEFINE LL_TEXT_FORCE_WORDWRAP                         268435458 /* (0x10000002)  */
&GLOBAL-DEFINE LL_NUMERIC                                     134217728 /* (0x08000000)  */
&GLOBAL-DEFINE LL_NUMERIC_LOCALIZED                           134217729 /* (0x08000001)  */
&GLOBAL-DEFINE LL_NUMERIC_INTEGER                             134217730 /* (0x08000002) flag */
&GLOBAL-DEFINE LL_DATE                                         67108864 /* (0x04000000) LL's own julian */
&GLOBAL-DEFINE LL_DATE_METHODMASK                                   255 /* (0x000000ff)  */
&GLOBAL-DEFINE LL_DATE_DELPHI_1                                67108865 /* (0x04000001)  */
&GLOBAL-DEFINE LL_DATE_DELPHI                                  67108866 /* (0x04000002) DELPHI 2, 3, 4: OLE DATE */
&GLOBAL-DEFINE LL_DATE_MS                                      67108866 /* (0x04000002) MS C/Basic: OLE DATE */
&GLOBAL-DEFINE LL_DATE_OLE                                     67108866 /* (0x04000002) generic: OLE DATE */
&GLOBAL-DEFINE LL_DATE_VFOXPRO                                 67108867 /* (0x04000003) nearly LL's own julian, has an offset of 1! */
&GLOBAL-DEFINE LL_DATE_DMY                                     67108868 /* (0x04000004) <d><sep><m><sep><yyyy>. Year MUST be 4 digits! */
&GLOBAL-DEFINE LL_DATE_MDY                                     67108869 /* (0x04000005) <m><sep><d><sep><yyyy>. Year MUST be 4 digits! */
&GLOBAL-DEFINE LL_DATE_YMD                                     67108870 /* (0x04000006) <yyyy><sep><m><sep><d>. Year MUST be 4 digits! */
&GLOBAL-DEFINE LL_DATE_YYYYMMDD                                67108871 /* (0x04000007) <yyyymmdd> */
&GLOBAL-DEFINE LL_DATE_LOCALIZED                               67108872 /* (0x04000008) localized (automatic VariantConversion) */
&GLOBAL-DEFINE LL_DATE_JULIAN                                  67108873 /* (0x04000009) variant 'date' is a julian date */
&GLOBAL-DEFINE LL_DATE_CLARION                                 67108874 /* (0x0400000a) days since 1800-12-28 (what's so special about that day?) */
&GLOBAL-DEFINE LL_DATE_YMD_AUTO                                67108880 /* (0x04000010) wither DMY, MDY or YMD, automatically detected */
&GLOBAL-DEFINE LL_BOOLEAN                                      33554432 /* (0x02000000)  */
&GLOBAL-DEFINE LL_RTF                                          16777216 /* (0x01000000)  */
&GLOBAL-DEFINE LL_HTML                                          8388608 /* (0x00800000)  */
&GLOBAL-DEFINE LL_PDF                                           4194304 /* (0x00400000)  */
&GLOBAL-DEFINE LL_INPUTOBJECT                                   2097152 /* (0x00200000) internal use only */
&GLOBAL-DEFINE LL_LLXOBJECT                                     1048576 /* (0x00100000) internal use only */
&GLOBAL-DEFINE LL_SUBTABLELIST                                   524288 /* (0x00080000) internal use only */
&GLOBAL-DEFINE LL_FIXEDNAME                                       32768 /* (0x00008000)  */
&GLOBAL-DEFINE LL_NOSAVEAS                                        16384 /* (0x00004000)  */
&GLOBAL-DEFINE LL_EXPRCONVERTQUIET                                 4096 /* (0x00001000) convert to new expressions without warning box */
&GLOBAL-DEFINE LL_NONAMEINTITLE                                    2048 /* (0x00000800) no file name appended to title */
&GLOBAL-DEFINE LL_DESIGNER_OVER_CHILD                              8192 /* (0x00002000)  */
&GLOBAL-DEFINE LL_PRVOPT_PRN_USEDEFAULT                               0 /* (0x00000000)  */
&GLOBAL-DEFINE LL_PRVOPT_PRN_ASKPRINTERIFNEEDED                       1 /* (0x00000001)  */
&GLOBAL-DEFINE LL_PRVOPT_PRN_ASKPRINTERALWAYS                         2 /* (0x00000002)  */
&GLOBAL-DEFINE LL_PRVOPT_PRN_ALWAYSUSEDEFAULT                         3 /* (0x00000003)  */
&GLOBAL-DEFINE LL_PRVOPT_PRN_ASSIGNMASK                               3 /* (0x00000003) used by L&L */
&GLOBAL-DEFINE LL_PRVOPT_FLAG_STANDALONEVIEWER                       16 /* (0x00000010)  */
&GLOBAL-DEFINE LL_OPTION_COPIES                                       0 /* (0) compatibility only, please use LL_PRNOPT_... */
&GLOBAL-DEFINE LL_OPTION_STARTPAGE                                    1 /* (1) compatibility only, please use LL_PRNOPT_PAGE */
&GLOBAL-DEFINE LL_OPTION_PAGE                                         1 /* (1) compatibility only, please use LL_PRNOPT_... */
&GLOBAL-DEFINE LL_OPTION_OFFSET                                       2 /* (2) compatibility only, please use LL_PRNOPT_... */
&GLOBAL-DEFINE LL_OPTION_COPIES_SUPPORTED                             3 /* (3) compatibility only, please use LL_PRNOPT_... */
&GLOBAL-DEFINE LL_OPTION_FIRSTPAGE                                    5 /* (5) compatibility only, please use LL_PRNOPT_... */
&GLOBAL-DEFINE LL_OPTION_LASTPAGE                                     6 /* (6) compatibility only, please use LL_PRNOPT_... */
&GLOBAL-DEFINE LL_OPTION_JOBPAGES                                     7 /* (7) compatibility only, please use LL_PRNOPT_... */
&GLOBAL-DEFINE LL_OPTION_PRINTORDER                                   8 /* (8) compatibility only, please use LL_PRNOPT_... */
&GLOBAL-DEFINE LL_PRNOPT_COPIES                                       0 /* (0)  */
&GLOBAL-DEFINE LL_COPIES_HIDE                                    -32768 /* (-32768) anything negative... */
&GLOBAL-DEFINE LL_PRNOPT_STARTPAGE                                    1 /* (1)  */
&GLOBAL-DEFINE LL_PRNOPT_PAGE                                         1 /* (1) alias; please do not use STARTPAGE any more... */
&GLOBAL-DEFINE LL_PAGE_HIDE                                      -32768 /* (-32768) must be exactly this value! */
&GLOBAL-DEFINE LL_PRNOPT_OFFSET                                       2 /* (2)  */
&GLOBAL-DEFINE LL_PRNOPT_COPIES_SUPPORTED                             3 /* (3)  */
&GLOBAL-DEFINE LL_PRNOPT_UNITS                                        4 /* (4) r/o */
&GLOBAL-DEFINE LL_UNITS_MM_DIV_10                                     0 /* (0) for LL_PRNOPT_UNITS, LL_OPTION_UNITS and LL_OPTION_UNITS_DEFAULT */
&GLOBAL-DEFINE LL_UNITS_INCH_DIV_100                                  1 /* (1)  */
&GLOBAL-DEFINE LL_UNITS_INCH_DIV_1000                                 2 /* (2)  */
&GLOBAL-DEFINE LL_UNITS_SYSDEFAULT_LORES                              3 /* (3) mm/10, in/100 (depending on regional settings of the system) */
&GLOBAL-DEFINE LL_UNITS_SYSDEFAULT                                    4 /* (4) mm/100, in/1000 (depending on regional settings of the system) */
&GLOBAL-DEFINE LL_UNITS_MM_DIV_100                                    5 /* (5)  */
&GLOBAL-DEFINE LL_UNITS_MM_DIV_1000                                   6 /* (6)  */
&GLOBAL-DEFINE LL_UNITS_SYSDEFAULT_HIRES                              7 /* (7) mm/100, in/1000 (depending on regional settings of the system) */
&GLOBAL-DEFINE LL_PRNOPT_FIRSTPAGE                                    5 /* (5)  */
&GLOBAL-DEFINE LL_PRNOPT_LASTPAGE                                     6 /* (6)  */
&GLOBAL-DEFINE LL_PRNOPT_JOBPAGES                                     7 /* (7)  */
&GLOBAL-DEFINE LL_PRNOPT_PRINTORDER                                   8 /* (8)  */
&GLOBAL-DEFINE LL_PRINTORDER_HORZ_LTRB                                0 /* (0)  */
&GLOBAL-DEFINE LL_PRINTORDER_VERT_LTRB                                1 /* (1)  */
&GLOBAL-DEFINE LL_PRINTORDER_HORZ_RBLT                                2 /* (2)  */
&GLOBAL-DEFINE LL_PRINTORDER_VERT_RBLT                                3 /* (3)  */
&GLOBAL-DEFINE LL_PRNOPT_DEFPRINTERINSTALLED                         11 /* (11) returns 0 for no default printer, 1 for default printer present */
&GLOBAL-DEFINE LL_PRNOPT_PRINTDLG_DESTMASK                           12 /* (12) any combination of the ones below... Default: all. Outdated, please use LL_OPTIONSTR_EXPORTS_ALLOWED */
&GLOBAL-DEFINE LL_DESTINATION_PRN                                     1 /* (1)  */
&GLOBAL-DEFINE LL_DESTINATION_PRV                                     2 /* (2)  */
&GLOBAL-DEFINE LL_DESTINATION_FILE                                    4 /* (4)  */
&GLOBAL-DEFINE LL_DESTINATION_EXTERN                                  8 /* (8)  */
&GLOBAL-DEFINE LL_DESTINATION_MSFAX                                  16 /* (16) reserved */
&GLOBAL-DEFINE LL_PRNOPT_PRINTDLG_DEST                               13 /* (13) default destination; outdated, please use LL_PRNOPTSTR_EXPORT */
&GLOBAL-DEFINE LL_PRNOPT_PRINTDLG_ONLYPRINTERCOPIES                  14 /* (14) show copies option in dialog only if they are supported by the printer. default: false */
&GLOBAL-DEFINE LL_PRNOPT_JOBID                                       17 /* (17)  */
&GLOBAL-DEFINE LL_PRNOPT_PAGEINDEX                                   18 /* (18)  */
&GLOBAL-DEFINE LL_PRNOPT_USES2PASS                                   19 /* (19) r/o */
&GLOBAL-DEFINE LL_PRNOPT_PAGERANGE_USES_ABSOLUTENUMBER               20 /* (20) default: false */
&GLOBAL-DEFINE LL_PRNOPT_USEMEMORYMETAFILE                           22 /* (22) default: false */
&GLOBAL-DEFINE LL_PRNOPT_PARTIALPREVIEW                              23 /* (23) default: false */
&GLOBAL-DEFINE LL_PRNOPT_ADDITIONALPAGES_FOR_TOTAL                   24 /* (24) internal */
&GLOBAL-DEFINE LL_PRNOPT_HAS_TOTALPAGES                              25 /* (25) internal */
&GLOBAL-DEFINE LL_PRNOPTSTR_PRINTDST_FILENAME                         0 /* (0) print to file: default filename (LlGet/SetPrintOptionString) */
&GLOBAL-DEFINE LL_PRNOPTSTR_EXPORTDESCR                               1 /* (1) r/o, returns the description of the export chosen */
&GLOBAL-DEFINE LL_PRNOPTSTR_EXPORT                                    2 /* (2) sets default exporter to use / returns the name of the export chosen */
&GLOBAL-DEFINE LL_PRNOPTSTR_PRINTJOBNAME                              3 /* (3) set name to be given to StartDoc() (lpszMessage of LlPrintWithBoxStart() */
&GLOBAL-DEFINE LL_PRNOPTSTR_PRESTARTDOCESCSTRING                      4 /* (4) sent before StartDoc() */
&GLOBAL-DEFINE LL_PRNOPTSTR_POSTENDDOCESCSTRING                       5 /* (5) sent after EndDoc() */
&GLOBAL-DEFINE LL_PRNOPTSTR_PRESTARTPAGEESCSTRING                     6 /* (6) sent before StartPage() */
&GLOBAL-DEFINE LL_PRNOPTSTR_POSTENDPAGEESCSTRING                      7 /* (7) sent after EndPage() */
&GLOBAL-DEFINE LL_PRNOPTSTR_PRESTARTPROJECTESCSTRING                  8 /* (8) sent before first StartPage() of project */
&GLOBAL-DEFINE LL_PRNOPTSTR_POSTENDPROJECTESCSTRING                   9 /* (9) sent after last EndPage() of project */
&GLOBAL-DEFINE LL_PRNOPTSTR_PAGERANGES                               10 /* (10)  */
&GLOBAL-DEFINE LL_PRNOPTSTR_ISSUERANGES                              11 /* (11)  */
&GLOBAL-DEFINE LL_PRNOPTSTR_PREVIEWTITLE                             12 /* (12) default: language dependent */
&GLOBAL-DEFINE LL_PRINT_V1POINTX                                      0 /* (0x00000000)  */
&GLOBAL-DEFINE LL_PRINT_NORMAL                                      256 /* (0x00000100)  */
&GLOBAL-DEFINE LL_PRINT_PREVIEW                                     512 /* (0x00000200)  */
&GLOBAL-DEFINE LL_PRINT_STORAGE                                     512 /* (0x00000200) same as LL_PRINT_PREVIEW */
&GLOBAL-DEFINE LL_PRINT_FILE                                       1024 /* (0x00000400)  */
&GLOBAL-DEFINE LL_PRINT_USERSELECT                                 2048 /* (0x00000800)  */
&GLOBAL-DEFINE LL_PRINT_EXPORT                                     2048 /* (0x00000800) same as LL_PRINT_USERSELECT */
&GLOBAL-DEFINE LL_PRINT_MODEMASK                                   3840 /* (0x00000f00)  */
&GLOBAL-DEFINE LL_PRINT_MULTIPLE_JOBS                              4096 /* (0x00001000)  */
&GLOBAL-DEFINE LL_PRINT_KEEPJOB                                    8192 /* (0x00002000)  */
&GLOBAL-DEFINE LL_PRINT_IS_DOM_CALLER                             16384 /* (0x00004000) internal */
&GLOBAL-DEFINE LL_PRINT_DOM_NOCREATEDC                            65536 /* (0x00010000) internal */
&GLOBAL-DEFINE LL_PRINT_DOM_NOOBJECTLOAD                         131072 /* (0x00020000) internal */
&GLOBAL-DEFINE LL_PRINT_REMOVE_UNUSED_VARS                        32768 /* (0x00008000) optimization flag */
&GLOBAL-DEFINE LL_BOXTYPE_BOXTYPEMASK                               255 /* (0x000000ff)  */
&GLOBAL-DEFINE LL_BOXTYPE_NONE                                      255 /* (0x000000ff)  */
&GLOBAL-DEFINE LL_BOXTYPE_FLAG_ALLOWSUSPEND                  1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_BOXTYPE_FLAG_USEMARQUEE                   -2147483648 /* (0x80000000)  */
&GLOBAL-DEFINE LL_BOXTYPE_NORMALMETER                                 0 /* (0)  */
&GLOBAL-DEFINE LL_BOXTYPE_BRIDGEMETER                                 1 /* (1)  */
&GLOBAL-DEFINE LL_BOXTYPE_NORMALWAIT                                  2 /* (2)  */
&GLOBAL-DEFINE LL_BOXTYPE_BRIDGEWAIT                                  3 /* (3)  */
&GLOBAL-DEFINE LL_BOXTYPE_EMPTYWAIT                                   4 /* (4)  */
&GLOBAL-DEFINE LL_BOXTYPE_EMPTYABORT                                  5 /* (5)  */
&GLOBAL-DEFINE LL_BOXTYPE_STDWAIT                                     6 /* (6)  */
&GLOBAL-DEFINE LL_BOXTYPE_STDABORT                                    7 /* (7)  */
&GLOBAL-DEFINE LL_BOXTYPE_MAX                                         7 /* (7)  */
&GLOBAL-DEFINE LL_FILE_ALSONEW                                    32768 /* (0x8000)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_DOUBLE                                  1 /* (0x0001)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_DATE                                    2 /* (0x0002)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_STRING                                  4 /* (0x0004)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_BOOL                                    8 /* (0x0008)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_DRAWING                                16 /* (0x0010)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_BARCODE                                32 /* (0x0020)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_ALL                                    63 /* (0x003f)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_PARA1                               32769 /* (0x8001)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_PARA2                               32770 /* (0x8002)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_PARA3                               32771 /* (0x8003)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_PARA4                               32772 /* (0x8004)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_SAME                                32831 /* (0x803f)  */
&GLOBAL-DEFINE LL_FCTPARATYPE_MASK                                36863 /* (0x8fff)  */
&GLOBAL-DEFINE LL_FCTPARATYPEFLAG_NONULLCHECK                     65536 /* (0x00010000)  */
&GLOBAL-DEFINE LL_FCTPARATYPEFLAG_MULTIDIM_ALLOWED               131072 /* (0x00020000) internal */
&GLOBAL-DEFINE LL_FCTPARATYPEFLAG_RAW                            524288 /* (0x00080000) parameter passed as string without evaluation */
&GLOBAL-DEFINE LL_FCTPARATYPEFLAG_RAW_WITH_SYNTAXCHECK           262144 /* (0x00040000) parameter passed as string without evaluation */
&GLOBAL-DEFINE LL_FCTPARATYPEFLAG_KEEP_LINEBREAKS               1048576 /* (0x00100000)  */
&GLOBAL-DEFINE LL_FCTPARATYPEFLAG_EXECUTE_ON_SYNTAXCHECK_AT_LOADTIME         2097152 /* (0x00200000)  */
&GLOBAL-DEFINE LL_EXPRTYPE_DOUBLE                                     1 /* (1)  */
&GLOBAL-DEFINE LL_EXPRTYPE_DATE                                       2 /* (2)  */
&GLOBAL-DEFINE LL_EXPRTYPE_STRING                                     3 /* (3)  */
&GLOBAL-DEFINE LL_EXPRTYPE_BOOL                                       4 /* (4)  */
&GLOBAL-DEFINE LL_EXPRTYPE_DRAWING                                    5 /* (5)  */
&GLOBAL-DEFINE LL_EXPRTYPE_BARCODE                                    6 /* (6)  */
&GLOBAL-DEFINE LL_OPTION_NEWEXPRESSIONS                               0 /* (0) default: true */
&GLOBAL-DEFINE LL_OPTION_ONLYONETABLE                                 1 /* (1) default: false */
&GLOBAL-DEFINE LL_OPTION_TABLE_COLORING                               2 /* (2) default: LL_COLORING_LL */
&GLOBAL-DEFINE LL_COLORING_LL                                         0 /* (0)  */
&GLOBAL-DEFINE LL_COLORING_PROGRAM                                    1 /* (1)  */
&GLOBAL-DEFINE LL_COLORING_DONTCARE                                   2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_SUPERVISOR                                   3 /* (3) default: false */
&GLOBAL-DEFINE LL_OPTION_UNITS                                        4 /* (4) default: see LL_OPTION_METRIC */
&GLOBAL-DEFINE LL_OPTION_TABSTOPS                                     5 /* (5) default: LL_TABS_DELETE */
&GLOBAL-DEFINE LL_TABS_DELETE                                         0 /* (0)  */
&GLOBAL-DEFINE LL_TABS_EXPAND                                         1 /* (1)  */
&GLOBAL-DEFINE LL_OPTION_CALLBACKMASK                                 6 /* (6) default: 0x00000000 */
&GLOBAL-DEFINE LL_CB_PAGE                                    1073741824 /* (0x40000000) callback for each page */
&GLOBAL-DEFINE LL_CB_PROJECT                                  536870912 /* (0x20000000) callback for each label */
&GLOBAL-DEFINE LL_CB_OBJECT                                   268435456 /* (0x10000000) callback for each object */
&GLOBAL-DEFINE LL_CB_HELP                                     134217728 /* (0x08000000) callback for HELP (F1/Button) */
&GLOBAL-DEFINE LL_CB_TABLELINE                                 67108864 /* (0x04000000) callback for table line */
&GLOBAL-DEFINE LL_CB_TABLEFIELD                                33554432 /* (0x02000000) callback for table field */
&GLOBAL-DEFINE LL_CB_QUERY_IS_VARIABLE_OR_FIELD                16777216 /* (0x01000000) callback for delayload (LL_QUERY_IS_VARIABLE_OR_FIELD) */
&GLOBAL-DEFINE LL_OPTION_CALLBACKPARAMETER                            7 /* (7) default: 0 */
&GLOBAL-DEFINE LL_OPTION_HELPAVAILABLE                                8 /* (8) default: true */
&GLOBAL-DEFINE LL_OPTION_SORTVARIABLES                                9 /* (9) default: true */
&GLOBAL-DEFINE LL_OPTION_SUPPORTPAGEBREAK                            10 /* (10) default: true */
&GLOBAL-DEFINE LL_OPTION_SHOWPREDEFVARS                              11 /* (11) default: true */
&GLOBAL-DEFINE LL_OPTION_USEHOSTPRINTER                              13 /* (13) default: false // use host printer via callback */
&GLOBAL-DEFINE LL_OPTION_EXTENDEDEVALUATION                          14 /* (14) allows expressions in chevrons (amwin mode) */
&GLOBAL-DEFINE LL_OPTION_TABREPRESENTATIONCODE                       15 /* (15) default: 247 (0xf7) */
&GLOBAL-DEFINE LL_OPTION_SHOWSTATE                                   16 /* (16) r/o,  */
&GLOBAL-DEFINE LL_OPTION_METRIC                                      18 /* (18) default: depends on Windows defaults */
&GLOBAL-DEFINE LL_OPTION_ADDVARSTOFIELDS                             19 /* (19) default: false */
&GLOBAL-DEFINE LL_OPTION_MULTIPLETABLELINES                          20 /* (20) default: true */
&GLOBAL-DEFINE LL_OPTION_CONVERTCRLF                                 21 /* (21) default: true */
&GLOBAL-DEFINE LL_OPTION_WIZ_FILENEW                                 22 /* (22) default: false */
&GLOBAL-DEFINE LL_OPTION_RETREPRESENTATIONCODE                       23 /* (23) default: LL_CHAR_NEWLINE (182) */
&GLOBAL-DEFINE LL_OPTION_PRVZOOM_PERC                                25 /* (25) initial preview zoom */
&GLOBAL-DEFINE LL_OPTION_PRVRECT_LEFT                                26 /* (26) initial preview position */
&GLOBAL-DEFINE LL_OPTION_PRVRECT_TOP                                 27 /* (27)  */
&GLOBAL-DEFINE LL_OPTION_PRVRECT_WIDTH                               28 /* (28)  */
&GLOBAL-DEFINE LL_OPTION_PRVRECT_HEIGHT                              29 /* (29)  */
&GLOBAL-DEFINE LL_OPTION_STORAGESYSTEM                               30 /* (30) DEPRECATED. Do not change. 0=LX4-compatible, 1=STORAGE (default) */
&GLOBAL-DEFINE LL_STG_COMPAT4                                         0 /* (0)  */
&GLOBAL-DEFINE LL_STG_STORAGE                                         1 /* (1)  */
&GLOBAL-DEFINE LL_OPTION_COMPRESSSTORAGE                             31 /* (31) 0, 1, 10..17 */
&GLOBAL-DEFINE LL_STG_COMPRESS_THREADED                           32768 /* (0x00008000)  */
&GLOBAL-DEFINE LL_STG_COMPRESS_UNTHREADED                         65536 /* (0x00010000)  */
&GLOBAL-DEFINE LL_OPTION_NOPARAMETERCHECK                            32 /* (32) you need a bit more speed? */
&GLOBAL-DEFINE LL_OPTION_NONOTABLECHECK                              33 /* (33) don't check on "NO_TABLEOBJECT" error. Default TRUE (don't check) */
&GLOBAL-DEFINE LL_OPTION_DRAWFOOTERLINEONPRINT                       34 /* (34) delay footerline printing to LlPrint(). Default FALSE */
&GLOBAL-DEFINE LL_OPTION_PRVZOOM_LEFT                                35 /* (35) initial preview position in percent of screen */
&GLOBAL-DEFINE LL_OPTION_PRVZOOM_TOP                                 36 /* (36)  */
&GLOBAL-DEFINE LL_OPTION_PRVZOOM_WIDTH                               37 /* (37)  */
&GLOBAL-DEFINE LL_OPTION_PRVZOOM_HEIGHT                              38 /* (38)  */
&GLOBAL-DEFINE LL_OPTION_SPACEOPTIMIZATION                           40 /* (40) default: true */
&GLOBAL-DEFINE LL_OPTION_REALTIME                                    41 /* (41) default: false */
&GLOBAL-DEFINE LL_OPTION_AUTOMULTIPAGE                               42 /* (42) default: true */
&GLOBAL-DEFINE LL_OPTION_USEBARCODESIZES                             43 /* (43) default: false */
&GLOBAL-DEFINE LL_OPTION_MAXRTFVERSION                               44 /* (44) default: 0xff00 */
&GLOBAL-DEFINE LL_OPTION_VARSCASESENSITIVE                           46 /* (46) default: false */
&GLOBAL-DEFINE LL_OPTION_DELAYTABLEHEADER                            47 /* (47) default: true */
&GLOBAL-DEFINE LL_OPTION_OFNDIALOG_EXPLORER                          48 /* (48) DEPRECATED. Do not change. */
&GLOBAL-DEFINE LL_OPTION_OFN_NOPLACESBAR                     1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_OPTION_EMFRESOLUTION                               49 /* (49) DEPRECATED. Do not change. */
&GLOBAL-DEFINE LL_OPTION_SETCREATIONINFO                             50 /* (50) default: true */
&GLOBAL-DEFINE LL_OPTION_XLATVARNAMES                                51 /* (51) default: true */
&GLOBAL-DEFINE LL_OPTION_LANGUAGE                                    52 /* (52) returns current language (r/o) */
&GLOBAL-DEFINE LL_OPTION_PHANTOMSPACEREPRESENTATIONCODE              54 /* (54) default: LL_CHAR_PHANTOMSPACE */
&GLOBAL-DEFINE LL_OPTION_LOCKNEXTCHARREPRESENTATIONCODE              55 /* (55) default: LL_CHAR_LOCK */
&GLOBAL-DEFINE LL_OPTION_EXPRSEPREPRESENTATIONCODE                   56 /* (56) default: LL_CHAR_EXPRSEP */
&GLOBAL-DEFINE LL_OPTION_DEFPRINTERINSTALLED                         57 /* (57) r/o */
&GLOBAL-DEFINE LL_OPTION_CALCSUMVARSONINVISIBLELINES                 58 /* (58) default: false - only default value if no preferences in project */
&GLOBAL-DEFINE LL_OPTION_NOFOOTERPAGEWRAP                            59 /* (59) default: false - only default value if no preferences in project */
&GLOBAL-DEFINE LL_OPTION_IMMEDIATELASTPAGE                           64 /* (64) default: true */
&GLOBAL-DEFINE LL_OPTION_LCID                                        65 /* (65) default: LOCALE_USER_DEFAULT */
&GLOBAL-DEFINE LL_OPTION_TEXTQUOTEREPRESENTATIONCODE                 66 /* (66) default: 1 */
&GLOBAL-DEFINE LL_OPTION_SCALABLEFONTSONLY                           67 /* (67) default: true */
&GLOBAL-DEFINE LL_OPTION_NOTIFICATIONMESSAGEHWND                     68 /* (68) default: NULL (parent window handle) */
&GLOBAL-DEFINE LL_OPTION_DEFDEFFONT                                  69 /* (69) default: GetStockObject(ANSI_VAR_FONT) */
&GLOBAL-DEFINE LL_OPTION_CODEPAGE                                    70 /* (70) default: CP_ACP; set codepage to use for conversions. */
&GLOBAL-DEFINE LL_OPTION_FORCEFONTCHARSET                            71 /* (71) default: false; set font's charset to the codepage according to LL_OPTION_LCID. Default: FALSE */
&GLOBAL-DEFINE LL_OPTION_COMPRESSRTF                                 72 /* (72) default: true; compress RTF text > 1024 bytes in project file */
&GLOBAL-DEFINE LL_OPTION_ALLOW_LLX_EXPORTERS                         74 /* (74) default: true; allow ILlXExport extensions */
&GLOBAL-DEFINE LL_OPTION_SUPPORTS_PRNOPTSTR_EXPORT                   75 /* (75) default: false: hides "set to default" button in "export option" tab in designer */
&GLOBAL-DEFINE LL_OPTION_DEBUGFLAG                                   76 /* (76)  */
&GLOBAL-DEFINE LL_OPTION_SKIPRETURNATENDOFRTF                        77 /* (77) default: false */
&GLOBAL-DEFINE LL_OPTION_INTERCHARSPACING                            78 /* (78) default: false: allows character interspacing in case of block justify */
&GLOBAL-DEFINE LL_OPTION_INCLUDEFONTDESCENT                          79 /* (79) default: true */
&GLOBAL-DEFINE LL_OPTION_RESOLUTIONCOMPATIBLETO9X                    80 /* (80) DEPRECATED. default: false */
&GLOBAL-DEFINE LL_OPTION_USECHARTFIELDS                              81 /* (81) default: false */
&GLOBAL-DEFINE LL_OPTION_OFNDIALOG_NOPLACESBAR                       82 /* (82) default: false; do not use "Places" bar in NT2K? */
&GLOBAL-DEFINE LL_OPTION_SKETCH_COLORDEPTH                           83 /* (83) default: 24 */
&GLOBAL-DEFINE LL_OPTION_FINAL_TRUE_ON_LASTPAGE                      84 /* (84) default: false: internal use */
&GLOBAL-DEFINE LL_OPTION_INTERCHARSPACING_FORCED                     86 /* (86) default: false: forces character interspacing calculation in TEXT objects (possibly dangerous and slow) */
&GLOBAL-DEFINE LL_OPTION_RTFAUTOINCREMENT                            87 /* (87) default: false, to increment RTF char pointer if nothing can be printed */
&GLOBAL-DEFINE LL_OPTION_UNITS_DEFAULT                               88 /* (88) default: LL_OPTION_UNITS_SYSDEFAULT. Use for contols that query the units, where we need to return "sysdefault" also */
&GLOBAL-DEFINE LL_OPTION_NO_MAPI                                     89 /* (89) default: false. Inhibit MAPI load for preview */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLE                                90 /* (90) default: LL_OPTION_TOOLBARSTYLE_STANDARD|LL_OPTION_TOOLBARSTYLEFLAG_DOCKABLE */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLE_STANDARD                        0 /* (0) OFFICE97 alike style */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLE_OFFICEXP                        1 /* (1) DOTNET/OFFICE_XP alike style */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLE_OFFICE2003                      2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLEMASK                            15 /* (0x0f)  */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLEFLAG_GRADIENT                  128 /* (0x80) starting with XP, use gradient style */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLEFLAG_DOCKABLE                   64 /* (0x40) dockable toolbars? */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLEFLAG_CANCLOSE                   32 /* (0x20) internal use only */
&GLOBAL-DEFINE LL_OPTION_TOOLBARSTYLEFLAG_SHRINK_TO_FIT              16 /* (0x10) internal use only */
&GLOBAL-DEFINE LL_OPTION_MENUSTYLE                                   91 /* (91) default: LL_OPTION_MENUSTYLE_STANDARD */
&GLOBAL-DEFINE LL_OPTION_MENUSTYLE_STANDARD_WITHOUT_BITMAPS               0 /* (0) values: see CTL */
&GLOBAL-DEFINE LL_OPTION_MENUSTYLE_STANDARD                           1 /* (1)  */
&GLOBAL-DEFINE LL_OPTION_MENUSTYLE_OFFICEXP                           2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_MENUSTYLE_OFFICE2003                         3 /* (3)  */
&GLOBAL-DEFINE LL_OPTION_RULERSTYLE                                  92 /* (92) default: LL_OPTION_RULERSTYLE_FLAT */
&GLOBAL-DEFINE LL_OPTION_RULERSTYLE_FLAT                             16 /* (0x10)  */
&GLOBAL-DEFINE LL_OPTION_RULERSTYLE_GRADIENT                        128 /* (0x80)  */
&GLOBAL-DEFINE LL_OPTION_STATUSBARSTYLE                              93 /* (93)  */
&GLOBAL-DEFINE LL_OPTION_STATUSBARSTYLE_STANDARD                      0 /* (0)  */
&GLOBAL-DEFINE LL_OPTION_STATUSBARSTYLE_OFFICEXP                      1 /* (1)  */
&GLOBAL-DEFINE LL_OPTION_STATUSBARSTYLE_OFFICE2003                    2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_TABBARSTYLE                                 94 /* (94)  */
&GLOBAL-DEFINE LL_OPTION_TABBARSTYLE_STANDARD                         0 /* (0)  */
&GLOBAL-DEFINE LL_OPTION_TABBARSTYLE_OFFICEXP                         1 /* (1)  */
&GLOBAL-DEFINE LL_OPTION_TABBARSTYLE_OFFICE2003                       2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_DROPWINDOWSTYLE                             95 /* (95)  */
&GLOBAL-DEFINE LL_OPTION_DROPWINDOWSTYLE_STANDARD                     0 /* (0)  */
&GLOBAL-DEFINE LL_OPTION_DROPWINDOWSTYLE_OFFICEXP                     1 /* (1)  */
&GLOBAL-DEFINE LL_OPTION_DROPWINDOWSTYLE_OFFICE2003                   2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_DROPWINDOWSTYLEMASK                         15 /* (0x0f)  */
&GLOBAL-DEFINE LL_OPTION_DROPWINDOWSTYLEFLAG_CANCLOSE                32 /* (0x20)  */
&GLOBAL-DEFINE LL_OPTION_INTERFACEWRAPPER                            96 /* (96) returns IL<n>* */
&GLOBAL-DEFINE LL_OPTION_FONTQUALITY                                 97 /* (97) LOGFONT.lfQuality, default: DEFAULT_QUALITY */
&GLOBAL-DEFINE LL_OPTION_FONTPRECISION                               98 /* (98) LOGFONT.lfOutPrecision, default: OUT_STRING_PRECIS */
&GLOBAL-DEFINE LL_OPTION_UISTYLE                                     99 /* (99) UI collection, w/o */
&GLOBAL-DEFINE LL_OPTION_UISTYLE_STANDARD                             0 /* (0) 90=0x40, 91=1, 92=0x10, 93=0, 94=0, 95=0x20 */
&GLOBAL-DEFINE LL_OPTION_UISTYLE_OFFICEXP                             1 /* (1) 90=0x41, 91=2, 92=0x10, 93=1, 94=1, 95=0x21 */
&GLOBAL-DEFINE LL_OPTION_UISTYLE_OFFICE2003                           2 /* (2) 90=0x42, 91=3, 92=0x10, 93=2, 94=2, 95=0x22 */
&GLOBAL-DEFINE LL_OPTION_NOFILEVERSIONUPGRADEWARNING                100 /* (100) default: false */
&GLOBAL-DEFINE LL_OPTION_UPDATE_FOOTER_ON_DATALINEBREAK_AT_FIRST_LINE             101 /* (101) default: false */
&GLOBAL-DEFINE LL_OPTION_ESC_CLOSES_PREVIEW                         102 /* (102) shall ESC close the preview window (default: false) */
&GLOBAL-DEFINE LL_OPTION_VIEWER_ASSUMES_TEMPFILE                    103 /* (103) shall the viewer assume that the file is a temporary file (and not store values in it)? default TRUE */
&GLOBAL-DEFINE LL_OPTION_CALC_USED_VARS                             104 /* (104) default: true */
&GLOBAL-DEFINE LL_OPTION_NOPRINTJOBSUPERVISION                      106 /* (106) default: true */
&GLOBAL-DEFINE LL_OPTION_CALC_SUMVARS_ON_PARTIAL_LINES              107 /* (107) default: false */
&GLOBAL-DEFINE LL_OPTION_BLACKNESS_SCM                              108 /* (108) default: 0 */
&GLOBAL-DEFINE LL_OPTION_PROHIBIT_USERINTERACTION                   109 /* (109) default: false */
&GLOBAL-DEFINE LL_OPTION_PERFMON_INSTALL                            110 /* (110) w/o, TRUE to install, FALSE to uninstall */
&GLOBAL-DEFINE LL_OPTION_RESERVED111                                111 /* (111)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTBUCKETCOUNT                         112 /* (112) applied to future jobs only, default 1000 */
&GLOBAL-DEFINE LL_OPTION_MSFAXALLOWED                               113 /* (113) global flag - set at start of LL! Will allow/prohibit fax detection. Default: TRUE */
&GLOBAL-DEFINE LL_OPTION_AUTOPROFILINGTICKS                         114 /* (114) global flag - set at start of LL! Activates LL's thread profiling */
&GLOBAL-DEFINE LL_OPTION_PROJECTBACKUP                              115 /* (115) default: true */
&GLOBAL-DEFINE LL_OPTION_ERR_ON_FILENOTFOUND                        116 /* (116) default: false */
&GLOBAL-DEFINE LL_OPTION_NOFAXVARS                                  117 /* (117) default: false */
&GLOBAL-DEFINE LL_OPTION_NOMAILVARS                                 118 /* (118) default: false */
&GLOBAL-DEFINE LL_OPTION_PATTERNRESCOMPATIBILITY                    119 /* (119) default: false */
&GLOBAL-DEFINE LL_OPTION_NODELAYEDVALUECACHING                      120 /* (120) default: false */
&GLOBAL-DEFINE LL_OPTION_FEATURE                                   1000 /* (1000)  */
&GLOBAL-DEFINE LL_OPTION_FEATURE_CLEARALL                             0 /* (0)  */
&GLOBAL-DEFINE LL_OPTION_FEATURE_SUPPRESS_JPEG_DISPLAY                1 /* (1)  */
&GLOBAL-DEFINE LL_OPTION_FEATURE_SUPPRESS_JPEG_CREATION               2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY                             121 /* (121) default: LL_OPTION_VARLISTDISPLAY_FOLDERPOS_TOP | LL_OPTION_VARLISTDISPLAY_VARSORT_ALPHA */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_VARSORT_DECLARATIONORDER               0 /* (0x0000)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_VARSORT_ALPHA                 1 /* (0x0001)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_VARSORT_MASK                 15 /* (0x000f)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_FOLDERPOS_DECLARATIONORDER               0 /* (0x0000)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_FOLDERPOS_ALPHA              16 /* (0x0010) only if LL_OPTION_VARLISTDISPLAY_VARSORT_ALPHA is set */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_FOLDERPOS_TOP                32 /* (0x0020)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_FOLDERPOS_BOTTOM              48 /* (0x0030)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_FOLDERPOS_MASK              240 /* (0x00f0)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_LLFOLDERPOS_BOTTOM             256 /* (0x0100)  */
&GLOBAL-DEFINE LL_OPTION_WORKAROUND_RTFBUG_EMPTYFIRSTPAGE             122 /* (122)  */
&GLOBAL-DEFINE LL_OPTION_FORMULASTRINGCOMPARISONS_CASESENSITIVE             123 /* (123) default: true */
&GLOBAL-DEFINE LL_OPTION_FIELDS_IN_PROJECTPARAMETERS                124 /* (124) default: false */
&GLOBAL-DEFINE LL_OPTION_CHECKWINDOWTHREADEDNESS                    125 /* (125) default: false */
&GLOBAL-DEFINE LL_OPTION_ISUSED_WILDCARD_AT_START                   126 /* (126) default: false */
&GLOBAL-DEFINE LL_OPTION_ROOT_MUST_BE_MASTERTABLE                   127 /* (127) default: false */
&GLOBAL-DEFINE LL_OPTION_DLLTYPE                                    128 /* (128) r/o */
&GLOBAL-DEFINE LL_OPTION_DLLTYPE_32BIT                                1 /* (0x0001)  */
&GLOBAL-DEFINE LL_OPTION_DLLTYPE_64BIT                                2 /* (0x0002)  */
&GLOBAL-DEFINE LL_OPTION_DLLTYPE_BITMASK                             15 /* (0x000f)  */
&GLOBAL-DEFINE LL_OPTION_DLLTYPE_SDBCS                               16 /* (0x0010)  */
&GLOBAL-DEFINE LL_OPTION_DLLTYPE_UNICODE                             32 /* (0x0020)  */
&GLOBAL-DEFINE LL_OPTION_DLLTYPE_CHARSET                            240 /* (0x00f0)  */
&GLOBAL-DEFINE LL_OPTION_HLIBRARY                                   129 /* (129) r/o */
&GLOBAL-DEFINE LL_OPTION_INVERTED_PAGEORIENTATION                   130 /* (130) default: false */
&GLOBAL-DEFINE LL_OPTION_ENABLE_STANDALONE_DATACOLLECTING_OBJECTS             131 /* (131) default: false */
&GLOBAL-DEFINE LL_OPTION_USERVARS_ARE_CODESNIPPETS                  132 /* (132) default: false */
&GLOBAL-DEFINE LL_OPTION_STORAGE_ADD_SUMMARYINFORMATION             133 /* (133) default: false */
&GLOBAL-DEFINE LL_OPTION_INCREMENTAL_PREVIEW                        135 /* (135) default: true */
&GLOBAL-DEFINE LL_OPTION_RELAX_AT_SHUTDOWN                          136 /* (136) default: true */
&GLOBAL-DEFINE LL_OPTION_NOPRINTERPATHCHECK                         137 /* (137) default: false */
&GLOBAL-DEFINE LL_OPTION_SUPPORT_HUGESTORAGEFS                      138 /* (138) default: true */
&GLOBAL-DEFINE LL_OPTION_NOAUTOPROPERTYCORRECTION                   139 /* (139) default: false */
&GLOBAL-DEFINE LL_OPTION_NOVARLISTRESET_ON_RESETPROJECTSTATE             140 /* (140) default: false; */
&GLOBAL-DEFINE LL_OPTION_DESIGNERPREVIEWPARAMETER                   141 /* (141) default: NULL */
&GLOBAL-DEFINE LL_OPTION_RESERVED142                                142 /* (142)  */
&GLOBAL-DEFINE LL_OPTION_DESIGNEREXPORTPARAMETER                    143 /* (143) default: NULL */
&GLOBAL-DEFINE LL_OPTION_DESIGNERPRINT_SINGLETHREADED               144 /* (144) default: false */
&GLOBAL-DEFINE LL_OPTION_ALLOW_COMMENTS_IN_FORMULA                  145 /* (145) default: true */
&GLOBAL-DEFINE LL_OPTION_USE_MLANG_LINEBREAKALGORITHM               146 /* (146) default: false (would use MLANG to calculate the line break algorithm) */
&GLOBAL-DEFINE LL_OPTION_USE_JPEG_OR_PNG_OPTIMIZATION               147 /* (147) default: true */
&GLOBAL-DEFINE LL_OPTION_ENABLE_IMAGESMOOTHING                      148 /* (148) default: true (uses GDIPLUS - no smoothing on Win2000/98 if not GDIPLUS installed! Right now, applies only to JPEG.) */
&GLOBAL-DEFINE LL_OPTION_MAXRTFVERSION_AVAILABLE                    159 /* (159) r/o */
&GLOBAL-DEFINE LL_OPTION_CONDREPRESENTATIONCODES_LIKE_ANSI             160 /* (160) default: false */
&GLOBAL-DEFINE LL_OPTION_NULL_IS_NONDESTRUCTIVE                     161 /* (161) default: false */
&GLOBAL-DEFINE LL_OPTION_DRILLDOWNPARAMETER                         162 /* (162) default: NULL */
&GLOBAL-DEFINE LL_OPTION_ROUNDINGSTRATEGY                           163 /* (163) default: LL_ROUNDINGSTRATEGY_ARITHMETIC_SYMMETRIC */
&GLOBAL-DEFINE LL_ROUNDINGSTRATEGY_BANKERSROUNDING                    0 /* (0)  */
&GLOBAL-DEFINE LL_ROUNDINGSTRATEGY_ARITHMETIC_SYMMETRIC               1 /* (1)  */
&GLOBAL-DEFINE LL_OPTION_RESERVED164                                164 /* (164)  */
&GLOBAL-DEFINE LL_OPTION_RESERVED165                                165 /* (165)  */
&GLOBAL-DEFINE LL_OPTION_PICTURE_TRANSPARENCY_IS_WHITE              166 /* (166) default: false (transparent) */
&GLOBAL-DEFINE LL_OPTION_FLOATPRECISION                             167 /* (167) global (not job specific!). Default: 0 (192 bit mantissa, 32 bit exponent) */
&GLOBAL-DEFINE LL_OPTION_SUPPRESS_LRUENTRY                          168 /* (168)  */
&GLOBAL-DEFINE LL_OPTION_FORCEFIRSTGROUPHEADER                      169 /* (169) default: false (group match string is an empty string) */
&GLOBAL-DEFINE LL_OPTION_SUPPORT_PDFINPUTFIELDS                     170 /* (170) PDF 3.0 supports text objects and check boxes as input objects - default: true */
&GLOBAL-DEFINE LL_OPTION_ENHANCED_SKIPRETURNATENDOFRTF              171 /* (171) default: false. */
&GLOBAL-DEFINE LL_OPTION_HIERARCHICALDATASOURCE                     172 /* (172) default: false */
&GLOBAL-DEFINE LL_OPTION_FORCE_HEADER_EVEN_ON_LARGE_FOOTERLINES             173 /* (173) default: false */
&GLOBAL-DEFINE LL_OPTION_PRINTERDEVICEOPTIMIZATION                  174 /* (174) default: true */
&GLOBAL-DEFINE LL_OPTION_RTFHEIGHTSCALINGPERCENTAGE                 175 /* (175) default: 100 */
&GLOBAL-DEFINE LL_OPTION_FORCE_DEFAULT_PRINTER_IN_PREVIEW             176 /* (176) default: false */
&GLOBAL-DEFINE LL_OPTION_SAVE_PROJECT_IN_UTF8                       178 /* (178) BOOL, default 0 (meaning: project is saved as UNICODE if A API is not used) */
&GLOBAL-DEFINE LL_DRILLDOWNFILTERSTRATEGY_ALLOW_ONLY_SUBTABLES               0 /* (0)  */
&GLOBAL-DEFINE LL_DRILLDOWNFILTERSTRATEGY_ALLOW_ALL_TABLES               1 /* (1)  */
&GLOBAL-DEFINE LL_DRILLDOWNFILTERSTRATEGY_ALLOW_SUBTABLES_AND_UNRELATED               2 /* (2)  */
&GLOBAL-DEFINE LL_DRILLDOWNFILTERSTRATEGY_ALLOW_SUBTABLES_AND_USERDEFINED               3 /* (3)  */
&GLOBAL-DEFINE LL_DRILLDOWNFILTERSTRATEGY_MASK                       15 /* (0x0f)  */
&GLOBAL-DEFINE LL_DRILLDOWNFILTERFLAG_OFFER_BASERECORD_AS_VARIABLES              16 /* (0x10)  */
&GLOBAL-DEFINE LL_OPTION_DRILLDOWN_DATABASEFILTERING                179 /* (179) default: 0 (filter all except subtables of the base table: LL_DRILLDOWNFILTERSTRATEGY_ALLOW_ONLY_SUBTABLES) */
&GLOBAL-DEFINE LL_OPTION_SUPPRESS_TASKBARBUTTON_PROGRESSSTATE             180 /* (180) default: false */
&GLOBAL-DEFINE LL_OPTION_PRINTDLG_DEVICECHANGE_KEEPS_DEVMODESETTINGS             181 /* (181) default: true */
&GLOBAL-DEFINE LL_OPTION_DRILLDOWN_SUPPORTS_EMBEDDING               182 /* (182) default: true */
&GLOBAL-DEFINE LL_VARLISTCLEARSTRATEGY_EMPTY_LIST                     0 /* (0)  */
&GLOBAL-DEFINE LL_VARLISTCLEARSTRATEGY_SET_NULL                       1 /* (1)  */
&GLOBAL-DEFINE LL_VARLISTCLEARSTRATEGY_SET_DEFAULT                    2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_VARLISTCLEARSTRATEGY_ON_DEFINE_START             183 /* (183) default: LL_VARLISTCLEARSTRATEGY_EMPTY_LIST */
&GLOBAL-DEFINE LL_OPTION_RESERVED184                                184 /* (184)  */
&GLOBAL-DEFINE LL_OPTION_KEEP_EMPTY_SUM_VARS                        185 /* (185) default: false */
&GLOBAL-DEFINE LL_OPTION_RESERVED187                                187 /* (187) internal test flag */
&GLOBAL-DEFINE LL_OPTION_DEFAULTDECSFORSTR                          188 /* (188) default: 5. Sets the default number of decimals for Str$ */
&GLOBAL-DEFINE LL_OPTION_RESETPROJECTSTATE_FORCES_NEW_PRINTJOB             189 /* (189) default: false */
&GLOBAL-DEFINE LL_OPTION_DEFINEXXXSTART_COMPATIBLE_TO_PRE15             190 /* (190)  */
&GLOBAL-DEFINE LL_OPTION_RESETPROJECTSTATE_FORCES_NEW_DC             191 /* (191) default: true */
&GLOBAL-DEFINE LL_OPTION_BITMAP_RESOLUTION_FOR_PREVIEW              192 /* (192) default: 0 (leave original size), suggestions are 300 or 600. -1 to use device default. */
&GLOBAL-DEFINE LL_OPTION_DRAW_EMPTY_CHARTOBJECTS                    193 /* (193) default: false */
&GLOBAL-DEFINE LL_OPTION_PREVIOUS_DEFAULTS_TO_NULL                  194 /* (194) default: false (for compatibility). Previous() returns NULL on first record if TRUE, otherwise some default value for the given datatype. */
&GLOBAL-DEFINE LL_OPTION_FORCE_IMAGEEMBEDDING                       195 /* (195) default: false. Images added via the image dialog are always embedded. */
&GLOBAL-DEFINE LL_OPTION_VARKEY_MAP_SHARP_S_TO_SS                   196 /* (196) default: false */
&GLOBAL-DEFINE LL_OPTION_NO_LAYERED_WINDOWS                         197 /* (197) default: false */
&GLOBAL-DEFINE LL_OPTION_SCALED_PERCENTAGEFORMATTER                 198 /* (198) default: false (0.1="0.1%", true: 0.1="10%") */
&GLOBAL-DEFINE LL_OPTION_USE_ANTIALIAS                              199 /* (199) default: true */
&GLOBAL-DEFINE LL_OPTION_FORCETABLELINECALLBACK                     200 /* (200) LL_CMND_TABLE_LINE is called even when COLORINGMODE_LL ist set, default: false */
&GLOBAL-DEFINE LL_OPTION_EXPORTCONSUMER                             201 /* (201) internal use only */
&GLOBAL-DEFINE LL_OPTION_TOC_IDX_ITEMID                             202 /* (202) internal use only */
&GLOBAL-DEFINE LL_OPTION_FORCED2PASSMODE                            203 /* (203) default: false */
&GLOBAL-DEFINE LL_OPTION_SETVAR_ONLY_SETS_IF_CONTAINER_PRINTS             204 /* (204) default: false */
&GLOBAL-DEFINE LL_OPTION_SHOW_PREVIEW_AFTER_PRINT_END               206 /* (206) "Export-ShowResult" sets this for PRV... */
&GLOBAL-DEFINE LL_OPTION_PROPLIST_COMBOBOX_SCROLL_WRAPS             207 /* (207) default: FALSE */
&GLOBAL-DEFINE LL_OPTION_ALWAYS_CALC_GROUPCHANGE_CONDITION             208 /* (208) default: false */
&GLOBAL-DEFINE LL_OPTION_NULLHANDLING_SUPPORTED_IN_ENHMODE             209 /* (209) default: false  */
&GLOBAL-DEFINE LL_OPTION_RESETPROJECTSTATE_FORCES_NEW_PREVIEWJOB             210 /* (210) default: true */
&GLOBAL-DEFINE LL_OPTION_USE_LEGACY_WORDWRAPPINGALGORITHM             211 /* (211) default: false */
&GLOBAL-DEFINE LL_OPTION_PREVIEW_USES_PRINTTHREAD                   212 /* (212) default: true */
&GLOBAL-DEFINE LL_OPTION_LL_SUPPLIES_MESSAGELOOP_WHILE_PRINTING_TO_PREVIEW             213 /* (213) default: true */
&GLOBAL-DEFINE LL_OPTION_PRINTERDCCACHE_TIMEOUT_SEC                 214 /* (214) default: 60 (0 -> no cache) */
&GLOBAL-DEFINE LL_OPTION_DESIGNER_RIBBONBACKGROUNDCOLOR             215 /* (215) default: undefined (system default) */
&GLOBAL-DEFINE LL_OPTION_INTERNAL_EMFCLEANUP                        216 /* (216) no comment -> internal! */
&GLOBAL-DEFINE LL_OPTION_RIBBON_DEFAULT_ENABLEDSTATE                217 /* (217) default: true */
&GLOBAL-DEFINE LL_OPTION_PRVFILEVERSION                             218 /* (218) default: 0 (2 would be an optimized version, supported since LL18, usually a bit faster if printing > 5000 pages) */
&GLOBAL-DEFINE LL_OPTION_TRY_REDUCE_BMPSIZE_BY_CONVERTING_TO_PNG_OR_JPEG             219 /* (219) default: false */
&GLOBAL-DEFINE LL_OPTION_NO_IMAGEFILEOPTIMIZATION                   220 /* (220) default: false. Set this to TRUE if you know you're replacing an image file during printing that is used in a project using its file name */
&GLOBAL-DEFINE LL_OPTION_NO_ENFORCED_GROUPFOOTERPRIORITY_FOR_LAST_GROUPFOOTER             221 /* (221) default: false. Compatibility to LL 16.008. */
&GLOBAL-DEFINE LL_OPTION_ALLOW_COMBINED_COLLECTING_OF_DATA_FOR_COLLECTIONCONTROLS             222 /* (222) default: true */
&GLOBAL-DEFINE LL_OPTION_SUPPRESS_LOADERRORMESSAGES                 223 /* (223) default: false. Please take care that this is a reference counted flag, so add (true) and subtract (false) the same number of calls! [ChK] */
&GLOBAL-DEFINE LL_OPTION_IGNOREFORMULARESULTMISMATCH_AT_LOADTIME             224 /* (224) default: false. Switches the r8117 (err #3535) change back to the old behavior */
&GLOBAL-DEFINE LL_OPTION_MAX_SIZE_OF_PROJECTINFOCACHE               225 /* (225) default: 1000 */
&GLOBAL-DEFINE LL_OPTION_NO_CORRECTION_OF_UNICODE_RTF               226 /* (226) default: false */
&GLOBAL-DEFINE LL_OPTION_MAY_RELEASE_UNNECESSARY_PROPS_AT_PRINTTIME             227 /* (227) default: false */
&GLOBAL-DEFINE LL_OPTION_DO_NOT_RESTORE_PREVSTATE_ON_FILTER_MISMATCH             228 /* (228) default: false (LL17: implicitly TRUE until 17.006) */
&GLOBAL-DEFINE LL_OPTION_SUPPORT_USERDEFINED_REPORTPAGELAYOUT             229 /* (229) default: false */
&GLOBAL-DEFINE LL_OPTION_DESIGNER_RIBBONTEXTCOLOR                   230 /* (230) default: undefined (system default) */
&GLOBAL-DEFINE LL_PARTSHARINGFLAG_VARIABLES_TOC                       1 /* (0x01)  */
&GLOBAL-DEFINE LL_PARTSHARINGFLAG_VARIABLES_IDX                       2 /* (0x02)  */
&GLOBAL-DEFINE LL_PARTSHARINGFLAG_VARIABLES_GTC                       4 /* (0x04)  */
&GLOBAL-DEFINE LL_OPTION_PARTSHARINGFLAGS                           231 /* (231) default: 0 */
&GLOBAL-DEFINE LL_OPTION_PIECHARTORDER_COMPATIBLE_TO_PRE19             232 /* (232) default: 1 (LL18), 0 (>= LL19) */
&GLOBAL-DEFINE LL_OPTION_DATABASESTRUCTURE_SORT_DECLARATIONORDER             233 /* (233) default: false (sorted alphabetically) */
&GLOBAL-DEFINE LL_OPTION_REPORT_PARAMETERS_REALDATAJOBPARAMETER             234 /* (234) default: NULL */
&GLOBAL-DEFINE LL_OPTION_EXPANDABLE_REGIONS_REALDATAJOBPARAMETER             235 /* (235) default: NULL */
&GLOBAL-DEFINE LL_OPTION_IMPROVED_TABLELINEANCHORING                236 /* (236) default: FALSE */
&GLOBAL-DEFINE LL_OPTION_INTERACTIVESORTING_REALDATAJOBPARAMETER             237 /* (237) default: NULL */
&GLOBAL-DEFINE LL_OPTION_TEMPFILESTRATEGY                           238 /* (238) default: LL_TEMPFILESTRATEGY_SPEED */
&GLOBAL-DEFINE LL_TEMPFILESTRATEGY_SPEED                              0 /* (0)  */
&GLOBAL-DEFINE LL_TEMPFILESTRATEGY_SIZE                               1 /* (1)  */
&GLOBAL-DEFINE LL_TEMPFILESTRATEGY_SECURITY                           2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_RTF_WHITE_BACKGROUND_IS_TRANSPARENT             239 /* (239) default: TRUE (!) */
&GLOBAL-DEFINE LL_OPTION_NO_DOTTED_LINE_ON_SECONDARY_AXIS             240 /* (240) default: FALSE */
&GLOBAL-DEFINE LL_OPTION_NO_PREVIOUS_VARLIST                        241 /* (241)  */
&GLOBAL-DEFINE LL_OPTION_COMMIT_FILE_ON_SAVE                        242 /* (242) default: false */
&GLOBAL-DEFINE LL_OPTION_DO_NOT_RTRIM_CELLTEXT                      243 /* (243) default: false */
&GLOBAL-DEFINE LL_OPTION_ALLOW_FCT_TEXTWIDTH                        244 /* (244) default: false */
&GLOBAL-DEFINE LL_OPTION_PASTEOBJECTS_TO_FIRST_VISIBLE_LAYER             245 /* (245) default: false */
&GLOBAL-DEFINE LL_OPTION_EMPTY_FILE_TRIGGERS_PROJECT_WIZARD             246 /* (246) default: false */
&GLOBAL-DEFINE LL_OPTION_DELAY_UPDATE_REMAININGTABLESPACE             247 /* (247) default: false  */
&GLOBAL-DEFINE LL_OPTION_WIZARD_ADDS_ORGNAME_TO_UI                  248 /* (248) default: false */
&GLOBAL-DEFINE LL_OPTION_PROHIBIT_EXTERNAL_FILES                    249 /* (249) default: false */
&GLOBAL-DEFINE LL_OPTION_DRAWINGS_INLINED                           250 /* (250) default: 0 (1 = inlined, 2 = leave as is, but no BLOBs - for GTC) */
&GLOBAL-DEFINE LL_OPTION_SERIALIZE_PRINTAPI                         251 /* (251) default: false */
&GLOBAL-DEFINE LL_OPTION_PROJECTFILELOCKTIMEOUT_IN_MS               252 /* (252) default: 10000 */
&GLOBAL-DEFINE LL_OPTION_ILLDATAPROVIDER                            253 /* (253)  */
&GLOBAL-DEFINE LL_OPTION_RTF_SUPPORTS_PARABREAKOPTIONS              254 /* (254) default: false */
&GLOBAL-DEFINE LL_OPTION_FORCE_PDFEMBEDDING                         255 /* (255) default: false. PDF documents added via the pdf dialog are always embedded */
&GLOBAL-DEFINE LL_OPTION_IGNORE_NONSCALEABLEFONTPROPERTIES             256 /* (256) default: false. PDF documents added via the pdf dialog are always embedded */
&GLOBAL-DEFINE LL_DATAPROVIDERTHREADNESS_NONE                         0 /* (0)  */
&GLOBAL-DEFINE LL_DATAPROVIDERTHREADNESS_ONE_INSTANCE_PER_THREAD               1 /* (1)  */
&GLOBAL-DEFINE LL_DATAPROVIDERTHREADNESS_DONTCARE                     2 /* (2) default */
&GLOBAL-DEFINE LL_OPTION_DATAPROVIDER_THREADEDNESS                  257 /* (257)  */
&GLOBAL-DEFINE LL_OPTION_SUBREPORT_BASE                             258 /* (258)  */
&GLOBAL-DEFINE LL_OPTION_SUBREPORT_CLIENT                           259 /* (259)  */
&GLOBAL-DEFINE LL_OPTION_NO_IPICTURE_SUPPORT                        260 /* (260) default: false */
&GLOBAL-DEFINE LL_OPTION_FORCE_JPEG_RECOMPRESSION                   261 /* (261) default: false */
&GLOBAL-DEFINE LL_OPTION_TEXTWRAP_TOLERANCE_PERC                    262 /* (262) default: 0 (no tolerance) */
&GLOBAL-DEFINE LL_OPTION_NO_USERVARCHECK_ON_LOAD                    263 /* (263) default: false */
&GLOBAL-DEFINE LL_OPTION_TOC_IDX_PAGE                               264 /* (264) internal use only */
&GLOBAL-DEFINE LL_OPTION_RTF_TAB_KEY_IS_TAB_FORMATTER               265 /* (265) default: false */
&GLOBAL-DEFINE LL_OPTION_VARLISTDISPLAY_LL_FOLDER_AT_END             266 /* (266) default: false */
&GLOBAL-DEFINE LL_OPTION_DOM_DO_NOT_KILL_EMPTY_TABLE                267 /* (267) default: false */
&GLOBAL-DEFINE LL_OPTION_ENABLE_INPUTOBJECTS_IN_TABLES              268 /* (268) default: true */
&GLOBAL-DEFINE LL_OPTION_MAX_ENTRIES_FOR_AUTOCOMPLETE               269 /* (269) default: 200 */
&GLOBAL-DEFINE LL_OPTION_DEFAULT_FOR_SHADOWPAGEWRAP                 270 /* (270) default: true */
&GLOBAL-DEFINE LL_OPTION_MAX_UNDO_STEPS                             271 /* (271) default: 10 */
&GLOBAL-DEFINE LL_OPTION_HTML_USE_MAILFORMAT                        272 /* (272) default: false */
&GLOBAL-DEFINE LL_OPTION_CLIP_LABELS_TO_PROJECTAREA                 273 /* (273) default: false (may paint beyond the border not to lose any data) */
&GLOBAL-DEFINE LL_OPTION_RESETPROJECTSTATE_FORCES_NEW_EXPORTJOB             274 /* (274)  */
&GLOBAL-DEFINE LL_OPTION_SCRIPTENGINE_ENABLED                       276 /* (276) default: false */
&GLOBAL-DEFINE LL_OPTION_SCRIPTENGINE_TIMEOUTMS                     277 /* (277) default: 10000 */
&GLOBAL-DEFINE LL_OPTION_SCRIPTENGINE_AUTOEXECUTE                   278 /* (278) default: false */
&GLOBAL-DEFINE LL_OPTION_SHAPEFILE_TIMEOUTMS                        279 /* (279) default: 1000  */
&GLOBAL-DEFINE LL_OPTION_COUNTALLPRINTEDDATA_LASTPRINT              280 /* (280) r/o */
&GLOBAL-DEFINE LL_OPTION_SAVE_AS_ACTS_AS_EXPORT                     281 /* (281) default: false */
&GLOBAL-DEFINE LL_OPTION_RESETPROJECTSTATE_TRIGGERS_NEW_SHEET             282 /* (282) default: true */
&GLOBAL-DEFINE LL_OPTION_HIDE_EXPORT_TAB_FROM_LAYOUT_CONFIG             283 /* (283) default: false */
&GLOBAL-DEFINE LL_OPTION_USE_VARLIST_NAMESORTINDEXCACHE             284 /* (284) should be defined for job -1 */
&GLOBAL-DEFINE LL_OPTION_NOCONTRASTOPTIMIZATION                     285 /* (285) default: false */
&GLOBAL-DEFINE LL_OPTION_AUTORECOVERY_DISABLED                      286 /* (286) default: false */
&GLOBAL-DEFINE LL_OPTION_AUTORECOVERY_SAVEOPTIONS                   287 /* (287) default: LL_AUTORECOVERY_SAVEOPTIONS_NEWFILE (value might be combination of following) */
&GLOBAL-DEFINE LL_AUTORECOVERY_SAVEOPTIONS_NEWFILE                    1 /* (1)  */
&GLOBAL-DEFINE LL_AUTORECOVERY_SAVEOPTIONS_OVERWRITE                  2 /* (2)  */
&GLOBAL-DEFINE LL_OPTION_LINK_PRINTERQUEUES                         288 /* (288) default: false, does not work yet */
&GLOBAL-DEFINE LL_OPTION_FORCE_RTFMERGING                           289 /* (289) default: false, forces to merge RTF contents even if there is just one part to load, compatibility switch */
&GLOBAL-DEFINE LL_OPTION_W201512300001                              290 /* (290) do not check on empty bodylines for "ActivateNextLine" - sort of "I know what I am doing in my print loop" */
&GLOBAL-DEFINE LL_OPTIONSTR_LABEL_PRJEXT                              0 /* (0) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_LABEL_PRVEXT                              1 /* (1) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_LABEL_PRNEXT                              2 /* (2) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_CARD_PRJEXT                               3 /* (3) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_CARD_PRVEXT                               4 /* (4) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_CARD_PRNEXT                               5 /* (5) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_LIST_PRJEXT                               6 /* (6) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_LIST_PRVEXT                               7 /* (7) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_LIST_PRNEXT                               8 /* (8) internal... (compatibility to L6) */
&GLOBAL-DEFINE LL_OPTIONSTR_LLXPATHLIST                              12 /* (12)  */
&GLOBAL-DEFINE LL_OPTIONSTR_SHORTDATEFORMAT                          13 /* (13)  */
&GLOBAL-DEFINE LL_OPTIONSTR_DECIMAL                                  14 /* (14) decimal point, default: system */
&GLOBAL-DEFINE LL_OPTIONSTR_THOUSAND                                 15 /* (15) thousands separator, default: system */
&GLOBAL-DEFINE LL_OPTIONSTR_CURRENCY                                 16 /* (16) currency symbol, default: system */
&GLOBAL-DEFINE LL_OPTIONSTR_EXPORTS_AVAILABLE                        17 /* (17) r/o */
&GLOBAL-DEFINE LL_OPTIONSTR_EXPORTS_ALLOWED                          18 /* (18)  */
&GLOBAL-DEFINE LL_OPTIONSTR_DEFDEFFONT                               19 /* (19) in "{(r,g,b),size,<logfont>}" */
&GLOBAL-DEFINE LL_OPTIONSTR_EXPORTFILELIST                           20 /* (20)  */
&GLOBAL-DEFINE LL_OPTIONSTR_VARALIAS                                 21 /* (21) "<local>=<global>" */
&GLOBAL-DEFINE LL_OPTIONSTR_MAILTO                                   24 /* (24) default TO: address for mailing from viewer */
&GLOBAL-DEFINE LL_OPTIONSTR_MAILTO_CC                                25 /* (25) default CC: address for mailing from viewer */
&GLOBAL-DEFINE LL_OPTIONSTR_MAILTO_BCC                               26 /* (26) default BCC: address for mailing from viewer */
&GLOBAL-DEFINE LL_OPTIONSTR_MAILTO_SUBJECT                           27 /* (27) default subject for mailing from viewer */
&GLOBAL-DEFINE LL_OPTIONSTR_SAVEAS_PATH                              28 /* (28) default filename for saving the LL file from viewer */
&GLOBAL-DEFINE LL_OPTIONSTR_LABEL_PRJDESCR                           29 /* (29) "Etikett" ... */
&GLOBAL-DEFINE LL_OPTIONSTR_CARD_PRJDESCR                            30 /* (30)  */
&GLOBAL-DEFINE LL_OPTIONSTR_LIST_PRJDESCR                            31 /* (31)  */
&GLOBAL-DEFINE LL_OPTIONSTR_LLFILEDESCR                              32 /* (32) "Vorschau-Datei" */
&GLOBAL-DEFINE LL_OPTIONSTR_PROJECTPASSWORD                          33 /* (33) w/o, of course :) */
&GLOBAL-DEFINE LL_OPTIONSTR_FAX_RECIPNAME                            34 /* (34)  */
&GLOBAL-DEFINE LL_OPTIONSTR_FAX_RECIPNUMBER                          35 /* (35)  */
&GLOBAL-DEFINE LL_OPTIONSTR_FAX_QUEUENAME                            36 /* (36)  */
&GLOBAL-DEFINE LL_OPTIONSTR_FAX_SENDERNAME                           37 /* (37)  */
&GLOBAL-DEFINE LL_OPTIONSTR_FAX_SENDERCOMPANY                        38 /* (38)  */
&GLOBAL-DEFINE LL_OPTIONSTR_FAX_SENDERDEPT                           39 /* (39)  */
&GLOBAL-DEFINE LL_OPTIONSTR_FAX_SENDERBILLINGCODE                    40 /* (40)  */
&GLOBAL-DEFINE LL_OPTIONSTR_FAX_AVAILABLEQUEUES                      42 /* (42) r/o (Tab-separated) [job can be -1 or a valid job] */
&GLOBAL-DEFINE LL_OPTIONSTR_LOGFILEPATH                              43 /* (43)  */
&GLOBAL-DEFINE LL_OPTIONSTR_LICENSINGINFO                            44 /* (44) w/o, SERNO to define licensing state */
&GLOBAL-DEFINE LL_OPTIONSTR_PRINTERALIASLIST                         45 /* (45) multiple "PrnOld=PrnNew1[;PrnNew2[;...]]", erase with NULL or "" */
&GLOBAL-DEFINE LL_OPTIONSTR_PREVIEWFILENAME                          46 /* (46) path of preview file (directory will be overridden by LlPreviewSetTempPath(), if given) */
&GLOBAL-DEFINE LL_OPTIONSTR_EXPORTS_ALLOWED_IN_PREVIEW               47 /* (47) set in preview file */
&GLOBAL-DEFINE LL_OPTIONSTR_HELPFILENAME                             48 /* (48)  */
&GLOBAL-DEFINE LL_OPTIONSTR_NULLVALUE                                49 /* (49) string which represents the NULL value */
&GLOBAL-DEFINE LL_OPTIONSTR_DEFAULT_EXPORT                           50 /* (50) default export medium for new projects */
&GLOBAL-DEFINE LL_OPTIONSTR_ORIGINALPROJECTFILENAME                  51 /* (51) fixup project path for relative paths in realdata preview/export in designer */
&GLOBAL-DEFINE LL_OPTIONSTR_HIERARCHICALDATASOURCE_ROOT              52 /* (52) internal use only */
&GLOBAL-DEFINE LL_OPTIONSTR_PRINTERDEFINITIONFILENAME                53 /* (53) override for P file name */
&GLOBAL-DEFINE LL_OPTIONSTR_DOCINFO_DATATYPE                         54 /* (54) DOCINFO.lpszDatatype */
&GLOBAL-DEFINE LL_OPTIONSTR_IDX_PRJEXT                               55 /* (55)  */
&GLOBAL-DEFINE LL_OPTIONSTR_IDX_PRVEXT                               56 /* (56)  */
&GLOBAL-DEFINE LL_OPTIONSTR_IDX_PRNEXT                               57 /* (57)  */
&GLOBAL-DEFINE LL_OPTIONSTR_TOC_PRJDESCR                             58 /* (58)  */
&GLOBAL-DEFINE LL_OPTIONSTR_IDX_PRJDESCR                             59 /* (59)  */
&GLOBAL-DEFINE LL_OPTIONSTR_TOC_PRJEXT                               60 /* (60)  */
&GLOBAL-DEFINE LL_OPTIONSTR_TOC_PRVEXT                               61 /* (61)  */
&GLOBAL-DEFINE LL_OPTIONSTR_TOC_PRNEXT                               62 /* (62)  */
&GLOBAL-DEFINE LL_OPTIONSTR_DEFAULTSCHEME                            63 /* (63) default: empty (COMBIT) */
&GLOBAL-DEFINE LL_OPTIONSTR_DEFAULTPROJECTNAME                       64 /* (64) DOCINFO.lpszDatatype */
&GLOBAL-DEFINE LL_OPTIONSTR_GTC_PRJEXT                               65 /* (65)  */
&GLOBAL-DEFINE LL_OPTIONSTR_GTC_PRVEXT                               66 /* (66)  */
&GLOBAL-DEFINE LL_OPTIONSTR_GTC_PRNEXT                               67 /* (67)  */
&GLOBAL-DEFINE LL_OPTIONSTR_GTC_PRJDESCR                             68 /* (68)  */
&GLOBAL-DEFINE LL_OPTIONSTR_ERRORTEXT_FROM_EXPORT                    69 /* (69) r/o */
&GLOBAL-DEFINE LL_OPTIONSTR_DEFAULTPRJDESCR                          70 /* (70) default: empty (localized version of 'List & Label project file') */
&GLOBAL-DEFINE LL_OPTIONSTR_DEFAULTPRINTER                           71 /* (71) if set, this printer is used instead of the system's default printer (applies to ALL JOBS, so job ID must be "-1"!) */
&GLOBAL-DEFINE LL_OPTIONSTR_QUERY_LICENSINGINFO                      72 /* (72) r/o, returns serial number in return value */
&GLOBAL-DEFINE LL_OPTIONSTR_RESERVED73                               73 /* (73)  */
&GLOBAL-DEFINE LL_OPTIONSTR_REPRESENTATION_BOOL_TRUE                 74 /* (74)  */
&GLOBAL-DEFINE LL_OPTIONSTR_REPRESENTATION_BOOL_FALSE                75 /* (75)  */
&GLOBAL-DEFINE LL_OPTIONSTR_DEFAULT_FILENAME_FOR_SAVEAS              76 /* (76) if set, this filename is used as a default name when "Save as" is chosen from the menu */
&GLOBAL-DEFINE LL_SYSCOMMAND_MINIMIZE                                -1 /* (-1)  */
&GLOBAL-DEFINE LL_SYSCOMMAND_MAXIMIZE                                -2 /* (-2)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_3DBUTTONS                            32768 /* (0x8000) 'or'ed */
&GLOBAL-DEFINE LL_DLGBOXMODE_3DFRAME2                             16384 /* (0x4000) 'OR'ed */
&GLOBAL-DEFINE LL_DLGBOXMODE_3DFRAME                               4096 /* (0x1000) 'OR'ed */
&GLOBAL-DEFINE LL_DLGBOXMODE_NOBITMAPS                             8192 /* (0x2000) 'or'ed */
&GLOBAL-DEFINE LL_DLGBOXMODE_DONTCARE                                 0 /* (0x0000) load from INI */
&GLOBAL-DEFINE LL_DLGBOXMODE_SAA                                      1 /* (0x0001)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT1                                     2 /* (0x0002)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT2                                     3 /* (0x0003)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT3                                     4 /* (0x0004)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT4                                     5 /* (0x0005)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT5                                     6 /* (0x0006)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT6                                     7 /* (0x0007)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT7                                     8 /* (0x0008)  */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT8                                     9 /* (0x0009) Win95 */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT9                                    10 /* (0x000A) Win98 */
&GLOBAL-DEFINE LL_DLGBOXMODE_ALT10                                   11 /* (0x000B) Win98 with gray/color button bitmaps like IE4 */
&GLOBAL-DEFINE LL_DLGBOXMODE_TOOLTIPS98                            2048 /* (0x0800) DEPRECATED. Do not change. */
&GLOBAL-DEFINE LL_CTL_ADDTOSYSMENU                                    4 /* (0x00000004) from CTL */
&GLOBAL-DEFINE LL_CTL_ALSOCHILDREN                                   16 /* (0x00000010)  */
&GLOBAL-DEFINE LL_CTL_CONVERTCONTROLS                             65536 /* (0x00010000)  */
&GLOBAL-DEFINE LL_GROUP_ALWAYSFOOTER                         1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_PRINTERCONFIG_SAVE                                  1 /* (1)  */
&GLOBAL-DEFINE LL_PRINTERCONFIG_RESTORE                               2 /* (2)  */
&GLOBAL-DEFINE LL_PRJTYPE_OPTION_FORCEDEFAULTSETTINGS             32768 /* (0x8000)  */
&GLOBAL-DEFINE LL_PRJTYPE_OPTION_CREATEPARTSFROMPROJECT           16384 /* (0x4000)  */
&GLOBAL-DEFINE LL_RTFTEXTMODE_RTF                                     0 /* (0x0000)  */
&GLOBAL-DEFINE LL_RTFTEXTMODE_PLAIN                                   1 /* (0x0001)  */
&GLOBAL-DEFINE LL_RTFTEXTMODE_EVALUATED                               0 /* (0x0000)  */
&GLOBAL-DEFINE LL_RTFTEXTMODE_RAW                                     2 /* (0x0002)  */
&GLOBAL-DEFINE LL_RTFTEXTFLAG_ALL                                     0 /* (0x0000)  */
&GLOBAL-DEFINE LL_RTFTEXTFLAG_SELECTION                               4 /* (0x0004)  */
&GLOBAL-DEFINE LL_ERR_BAD_JOBHANDLE                                  -1 /* (-1) bad jobhandle */
&GLOBAL-DEFINE LL_ERR_TASK_ACTIVE                                    -2 /* (-2) LlDefineLayout() only once in a job */
&GLOBAL-DEFINE LL_ERR_BAD_OBJECTTYPE                                 -3 /* (-3) nObjType must be one of the allowed values (obsolete constant) */
&GLOBAL-DEFINE LL_ERR_BAD_PROJECTTYPE                                -3 /* (-3) nObjType must be one of the allowed values */
&GLOBAL-DEFINE LL_ERR_PRINTING_JOB                                   -4 /* (-4) print job not opened, no print object */
&GLOBAL-DEFINE LL_ERR_NO_BOX                                         -5 /* (-5) LlPrintSetBoxText(...) called when no abort box exists! */
&GLOBAL-DEFINE LL_ERR_ALREADY_PRINTING                               -6 /* (-6) the current operation cannot be performed while a print job is open */
&GLOBAL-DEFINE LL_ERR_NOT_YET_PRINTING                               -7 /* (-7) LlPrintGetOptionString... */
&GLOBAL-DEFINE LL_ERR_NO_PROJECT                                    -10 /* (-10) object with requested name does not exist (former ERR_NO_OBJECT) */
&GLOBAL-DEFINE LL_ERR_NO_PRINTER                                    -11 /* (-11) printer couldn't be opened */
&GLOBAL-DEFINE LL_ERR_PRINTING                                      -12 /* (-12) error while printing */
&GLOBAL-DEFINE LL_ERR_EXPORTING                                     -13 /* (-13) error while exporting */
&GLOBAL-DEFINE LL_ERR_NEEDS_VB                                      -14 /* (-14) '11...' needs VB.EXE */
&GLOBAL-DEFINE LL_ERR_BAD_PRINTER                                   -15 /* (-15) PrintOptionsDialog(): no printer available */
&GLOBAL-DEFINE LL_ERR_NO_PREVIEWMODE                                -16 /* (-16) Preview functions: not in preview mode */
&GLOBAL-DEFINE LL_ERR_NO_PREVIEWFILES                               -17 /* (-17) PreviewDisplay(): no file found */
&GLOBAL-DEFINE LL_ERR_PARAMETER                                     -18 /* (-18) bad parameter (usually NULL pointer) */
&GLOBAL-DEFINE LL_ERR_BAD_EXPRESSION                                -19 /* (-19) bad expression in LlExprEvaluate() and LlExprType() */
&GLOBAL-DEFINE LL_ERR_BAD_EXPRMODE                                  -20 /* (-20) bad expression mode (LlSetExpressionMode()) */
&GLOBAL-DEFINE LL_ERR_NO_TABLE                                      -21 /* (-21) not used */
&GLOBAL-DEFINE LL_ERR_CFGNOTFOUND                                   -22 /* (-22) on LlPrintStart(), LlPrintWithBoxStart() [not found] */
&GLOBAL-DEFINE LL_ERR_EXPRESSION                                    -23 /* (-23) on LlPrintStart(), LlPrintWithBoxStart() */
&GLOBAL-DEFINE LL_ERR_CFGBADFILE                                    -24 /* (-24) on LlPrintStart(), LlPrintWithBoxStart() [read error, bad format] */
&GLOBAL-DEFINE LL_ERR_BADOBJNAME                                    -25 /* (-25) on LlPrintEnableObject() - not a ':' at the beginning */
&GLOBAL-DEFINE LL_ERR_NOOBJECT                                      -26 /* (-26) on LlPrintEnableObject() - "*" and no object in project */
&GLOBAL-DEFINE LL_ERR_UNKNOWNOBJECT                                 -27 /* (-27) on LlPrintEnableObject() - object with that name not existing */
&GLOBAL-DEFINE LL_ERR_NO_TABLEOBJECT                                -28 /* (-28) LlPrint...Start() and no list in Project, or: */
&GLOBAL-DEFINE LL_ERR_NO_OBJECT                                     -29 /* (-29) LlPrint...Start() and no object in project */
&GLOBAL-DEFINE LL_ERR_NO_TEXTOBJECT                                 -30 /* (-30) LlPrintGetTextCharsPrinted() and no printable text in Project! */
&GLOBAL-DEFINE LL_ERR_UNKNOWN                                       -31 /* (-31) LlPrintIsVariableUsed(), LlPrintIsFieldUsed() */
&GLOBAL-DEFINE LL_ERR_BAD_MODE                                      -32 /* (-32) LlPrintFields(), LlPrintIsFieldUsed() called on non-OBJECT_LIST */
&GLOBAL-DEFINE LL_ERR_CFGBADMODE                                    -33 /* (-33) on LlDefineLayout(), LlPrint...Start(): file is in wrong expression mode */
&GLOBAL-DEFINE LL_ERR_ONLYWITHONETABLE                              -34 /* (-34) on LlDefinePageSeparation(), LlDefineGrouping() */
&GLOBAL-DEFINE LL_ERR_UNKNOWNVARIABLE                               -35 /* (-35) on LlGetVariableContents() */
&GLOBAL-DEFINE LL_ERR_UNKNOWNFIELD                                  -36 /* (-36) on LlGetFieldContents() */
&GLOBAL-DEFINE LL_ERR_UNKNOWNSORTORDER                              -37 /* (-37) on LlGetFieldContents() */
&GLOBAL-DEFINE LL_ERR_NOPRINTERCFG                                  -38 /* (-38) on LlPrintCopyPrinterConfiguration() - no or bad file */
&GLOBAL-DEFINE LL_ERR_SAVEPRINTERCFG                                -39 /* (-39) on LlPrintCopyPrinterConfiguration() - file could not be saved */
&GLOBAL-DEFINE LL_ERR_RESERVED                                      -40 /* (-40) function not yet implemeted */
&GLOBAL-DEFINE LL_ERR_NOVALIDPAGES                                  -41 /* (-41) could also be that 16 bit Viewer tries to open 32bit-only storage */
&GLOBAL-DEFINE LL_ERR_NOTINHOSTPRINTERMODE                          -42 /* (-42) cannot be done in Host Printer Mode (LlSetPrinterInPrinterFile()) */
&GLOBAL-DEFINE LL_ERR_NOTFINISHED                                   -43 /* (-43) appears when a project reset() is done, but the table not finished */
&GLOBAL-DEFINE LL_ERR_BUFFERTOOSMALL                                -44 /* (-44) LlXXGetOptionStr() */
&GLOBAL-DEFINE LL_ERR_BADCODEPAGE                                   -45 /* (-45) LL_OPTION_CODEPAGE */
&GLOBAL-DEFINE LL_ERR_CANNOTCREATETEMPFILE                          -46 /* (-46) cannot create temporary file */
&GLOBAL-DEFINE LL_ERR_NODESTINATION                                 -47 /* (-47) no valid export destination */
&GLOBAL-DEFINE LL_ERR_NOCHART                                       -48 /* (-48) no chart control present */
&GLOBAL-DEFINE LL_ERR_TOO_MANY_CONCURRENT_PRINTJOBS                 -49 /* (-49) WebServer: not enough print process licenses */
&GLOBAL-DEFINE LL_ERR_BAD_WEBSERVER_LICENSE                         -50 /* (-50) WebServer: bad license file */
&GLOBAL-DEFINE LL_ERR_NO_WEBSERVER_LICENSE                          -51 /* (-51) WebServer: no license file */
&GLOBAL-DEFINE LL_ERR_INVALIDDATE                                   -52 /* (-52) LlSystemTimeFromLocaleString(): date not valid! */
&GLOBAL-DEFINE LL_ERR_DRAWINGNOTFOUND                               -53 /* (-53) only if LL_OPTION_ERR_ON_FILENOTFOUND set */
&GLOBAL-DEFINE LL_ERR_NOUSERINTERACTION                             -54 /* (-54) a call is used which would show a dialog, but LL is in Webserver mode */
&GLOBAL-DEFINE LL_ERR_BADDATABASESTRUCTURE                          -55 /* (-55) the project that is loading has a table that is not supported by the database */
&GLOBAL-DEFINE LL_ERR_UNKNOWNPROPERTY                               -56 /* (-56)  */
&GLOBAL-DEFINE LL_ERR_INVALIDOPERATION                              -57 /* (-57)  */
&GLOBAL-DEFINE LL_ERR_PROPERTY_ALREADY_DEFINED                      -58 /* (-58)  */
&GLOBAL-DEFINE LL_ERR_CFGFOUND                                      -59 /* (-59) on LlPrjOpen() with CREATE_NEW disposition, or of project file is r/o and access flag is r/w */
&GLOBAL-DEFINE LL_ERR_SAVECFG                                       -60 /* (-60) error while saving (LlProjectSave()) */
&GLOBAL-DEFINE LL_ERR_WRONGTHREAD                                   -61 /* (-61) internal (.NET) */
&GLOBAL-DEFINE LL_ERR_NO_SUCH_INFORMATION                           -62 /* (-62)  */
&GLOBAL-DEFINE LL_ERR_USER_ABORTED                                  -99 /* (-99) user aborted printing */
&GLOBAL-DEFINE LL_ERR_BAD_DLLS                                     -100 /* (-100) DLLs not up to date (CTL, DWG, UTIL) */
&GLOBAL-DEFINE LL_ERR_NO_LANG_DLL                                  -101 /* (-101) no or out-of-date language resource DLL */
&GLOBAL-DEFINE LL_ERR_NO_MEMORY                                    -102 /* (-102) out of memory */
&GLOBAL-DEFINE LL_ERR_EXCEPTION                                    -104 /* (-104) there was a GPF during the API execution. Any action that follows might cause problems! */
&GLOBAL-DEFINE LL_ERR_LICENSEVIOLATION                             -105 /* (-105) your license does not allow this call (see LL_OPTIONSTR_LICENSINGINFO) */
&GLOBAL-DEFINE LL_ERR_NOT_SUPPORTED_IN_THIS_OS                     -106 /* (-106) the OS does not support this function */
&GLOBAL-DEFINE LL_ERR_NO_MORE_DATA                                 -107 /* (-107)  */
&GLOBAL-DEFINE LL_WRN_REPORTPARAMETERS_COLLECTION_FINISHED            -994 /* (-994) internal use */
&GLOBAL-DEFINE LL_WRN_ISNULL                                       -995 /* (-995) LlExprEvaluate[Var]() */
&GLOBAL-DEFINE LL_WRN_TABLECHANGE                                  -996 /* (-996)  */
&GLOBAL-DEFINE LL_WRN_PRINTFINISHED                                -997 /* (-997) LlRTFDisplay() */
&GLOBAL-DEFINE LL_WRN_REPEAT_DATA                                  -998 /* (-998) notification: page is full, prepare for next page */
&GLOBAL-DEFINE LL_CHAR_TEXTQUOTE                                      1 /* (1)  */
&GLOBAL-DEFINE LL_CHAR_PHANTOMSPACE                                8203 /* (0x200b)  */
&GLOBAL-DEFINE LL_CHAR_LOCK                                        8288 /* (0x2060)  */
&GLOBAL-DEFINE LL_CHAR_NEWLINE                                      182 /* (182) "¶" */
&GLOBAL-DEFINE LL_CHAR_EXPRSEP                                      164 /* (164) "¤" */
&GLOBAL-DEFINE LL_CHAR_TAB                                          247 /* (247) "÷" */
&GLOBAL-DEFINE LL_CHAR_EAN128NUL                                    255 /* (255)  */
&GLOBAL-DEFINE LL_CHAR_EAN128FNC1                                   254 /* (254)  */
&GLOBAL-DEFINE LL_CHAR_EAN128FNC2                                   253 /* (253)  */
&GLOBAL-DEFINE LL_CHAR_EAN128FNC3                                   252 /* (252)  */
&GLOBAL-DEFINE LL_CHAR_EAN128FNC4                                   251 /* (251)  */
&GLOBAL-DEFINE LL_CHAR_CODE93NUL                                    255 /* (255)  */
&GLOBAL-DEFINE LL_CHAR_CODE93EXDOLLAR                               254 /* (254)  */
&GLOBAL-DEFINE LL_CHAR_CODE93EXPERC                                 253 /* (253)  */
&GLOBAL-DEFINE LL_CHAR_CODE93EXSLASH                                252 /* (252)  */
&GLOBAL-DEFINE LL_CHAR_CODE93EXPLUS                                 251 /* (251)  */
&GLOBAL-DEFINE LL_CHAR_CODE39NUL                                    255 /* (255)  */
&GLOBAL-DEFINE LL_DLGEXPR_VAREXTBTN_ENABLE                            1 /* (0x00000001) callback for simple Wizard extension */
&GLOBAL-DEFINE LL_DLGEXPR_VAREXTBTN_DOMODAL                           2 /* (0x00000002)  */
&GLOBAL-DEFINE LL_LLX_EXTENSIONTYPE_EXPORT                            1 /* (1)  */
&GLOBAL-DEFINE LL_LLX_EXTENSIONTYPE_BARCODE                           2 /* (2)  */
&GLOBAL-DEFINE LL_LLX_EXTENSIONTYPE_OBJECT                            3 /* (3) nyi */
&GLOBAL-DEFINE LL_LLX_EXTENSIONTYPE_WIZARD                            4 /* (4) nyi */
&GLOBAL-DEFINE LL_LLX_EXTENSIONTYPEFLAG_FORCE_PUBLIC              65536 /* (0x00010000)  */
&GLOBAL-DEFINE LL_LLX_EXTENSIONTYPEFLAG_FORCE_PRIVATE            131072 /* (0x00020000)  */
&GLOBAL-DEFINE LL_LLX_EXTENSIONTYPE_TYPEMASK                         15 /* (0x0000000f)  */
&GLOBAL-DEFINE LL_DECLARECHARTROW_FOR_OBJECTS                         1 /* (0x00000001)  */
&GLOBAL-DEFINE LL_DECLARECHARTROW_FOR_TABLECOLUMNS                    2 /* (0x00000002) body only */
&GLOBAL-DEFINE LL_DECLARECHARTROW_FOR_TABLECOLUMNS_FOOTERS               4 /* (0x00000004)  */
&GLOBAL-DEFINE LL_GETCHARTOBJECTCOUNT_CHARTOBJECTS                    1 /* (1)  */
&GLOBAL-DEFINE LL_GETCHARTOBJECTCOUNT_CHARTOBJECTS_BEFORE_TABLE               2 /* (2)  */
&GLOBAL-DEFINE LL_GETCHARTOBJECTCOUNT_CHARTCOLUMNS                    3 /* (3) body only */
&GLOBAL-DEFINE LL_GETCHARTOBJECTCOUNT_CHARTCOLUMNS_FOOTERS               4 /* (4)  */
&GLOBAL-DEFINE LL_GRIPT_DIM_SCM                                       1 /* (1)  */
&GLOBAL-DEFINE LL_GRIPT_DIM_PERC                                      2 /* (2)  */
&GLOBAL-DEFINE LL_PARAMETERFLAG_PUBLIC                                0 /* (0x00000000)  */
&GLOBAL-DEFINE LL_PARAMETERFLAG_SAVEDEFAULT                       65536 /* (0x00010000)  */
&GLOBAL-DEFINE LL_PARAMETERFLAG_PRIVATE                      1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_PARAMETERFLAG_FORMULA                               0 /* (0x00000000)  */
&GLOBAL-DEFINE LL_PARAMETERFLAG_VALUE                         536870912 /* (0x20000000)  */
&GLOBAL-DEFINE LL_PARAMETERFLAG_GLOBAL                                0 /* (0x00000000)  */
&GLOBAL-DEFINE LL_PARAMETERFLAG_LOCAL                         268435456 /* (0x10000000)  */
&GLOBAL-DEFINE LL_PARAMETERFLAG_MASK                             -65536 /* (0xffff0000)  */
&GLOBAL-DEFINE LL_PARAMETERTYPE_USER                                  0 /* (0)  */
&GLOBAL-DEFINE LL_PARAMETERTYPE_FAX                                   1 /* (1)  */
&GLOBAL-DEFINE LL_PARAMETERTYPE_MAIL                                  2 /* (2)  */
&GLOBAL-DEFINE LL_PARAMETERTYPE_LLINTERNAL                            4 /* (4)  */
&GLOBAL-DEFINE LL_PARAMETERTYPE_MASK                                 15 /* (0x0000000f)  */
&GLOBAL-DEFINE LL_PRJOPEN_AM_READWRITE                       1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_PRJOPEN_AM_READONLY                                 0 /* (0x00000000) default */
&GLOBAL-DEFINE LL_PRJOPEN_AM_MASK                            1073741824 /* (0x40000000)  */
&GLOBAL-DEFINE LL_PRJOPEN_CD_OPEN_EXISTING                            0 /* (0x00000000) fails if it does not exist - default */
&GLOBAL-DEFINE LL_PRJOPEN_CD_CREATE_ALWAYS                    268435456 /* (0x10000000) open (but do not read contents) if exists, create if not */
&GLOBAL-DEFINE LL_PRJOPEN_CD_CREATE_NEW                       536870912 /* (0x20000000) fails if already exists */
&GLOBAL-DEFINE LL_PRJOPEN_CD_OPEN_ALWAYS                      805306368 /* (0x30000000) open (and load) if exists, create if not */
&GLOBAL-DEFINE LL_PRJOPEN_CD_MASK                             805306368 /* (0x30000000) fails if it does not exist */
&GLOBAL-DEFINE LL_PRJOPEN_EM_IGNORE_FORMULAERRORS             134217728 /* (0x08000000)  */
&GLOBAL-DEFINE LL_PRJOPEN_EM_MASK                             134217728 /* (0x08000000)  */
&GLOBAL-DEFINE LL_PRJOPEN_FLG_NOINITPRINTER                    67108864 /* (0x04000000)  */
&GLOBAL-DEFINE LL_PRJOPEN_FLG_NOOBJECTLOAD                     33554432 /* (0x02000000)  */
&GLOBAL-DEFINE LL_PRJOPEN_FLG_RESERVED                         16777216 /* (0x01000000) internal use */
&GLOBAL-DEFINE LL_ASSOCIATEPREVIEWCONTROLFLAG_DELETE_ON_CLOSE               1 /* (0x0001)  */
&GLOBAL-DEFINE LL_ASSOCIATEPREVIEWCONTROLFLAG_HANDLE_IS_ATTACHINFO               2 /* (0x0002)  */
&GLOBAL-DEFINE LL_ASSOCIATEPREVIEWCONTROLFLAG_PRV_REPLACE               0 /* (0x0000)  */
&GLOBAL-DEFINE LL_ASSOCIATEPREVIEWCONTROLFLAG_PRV_ADD_TO_CONTROL_STACK               4 /* (0x0004)  */
&GLOBAL-DEFINE LL_ASSOCIATEPREVIEWCONTROLFLAG_PRV_ADD_TO_CONTROL_IN_TAB               8 /* (0x0008)  */
&GLOBAL-DEFINE LL_ASSOCIATEPREVIEWCONTROLMASK_ATTACHLOCATION              60 /* (0x003c)  */
&GLOBAL-DEFINE LL_DESFILEOPEN_OPEN_EXISTING                           0 /* (0x00000000) fails if it does not exist - default */
&GLOBAL-DEFINE LL_DESFILEOPEN_CREATE_ALWAYS                   268435456 /* (0x10000000) open (but do not read contents) if exists, create if not */
&GLOBAL-DEFINE LL_DESFILEOPEN_CREATE_NEW                      536870912 /* (0x20000000) fails if already exists */
&GLOBAL-DEFINE LL_DESFILEOPEN_OPEN_ALWAYS                     805306368 /* (0x30000000) open (and load) if exists, create if not */
&GLOBAL-DEFINE LL_DESFILEOPEN_OPEN_IMPORT                    1073741824 /* (0x40000000) fails if it does not exist - only imports data */
&GLOBAL-DEFINE LL_DESFILEOPEN_OPENMASK                       1879048192 /* (0x70000000)  */
&GLOBAL-DEFINE LL_DESFILEOPENFLAG_SUPPRESS_SAVEDIALOG                 1 /* (0x00000001)  */
&GLOBAL-DEFINE LL_DESFILEOPENFLAG_SUPPRESS_SAVE                       2 /* (0x00000002)  */
&GLOBAL-DEFINE LL_DESFILESAVE_DEFAULT                                 0 /* (0x00000000) default */
&GLOBAL-DEFINE LLDESADDACTIONFLAG_ADD_TO_TOOLBAR              536870912 /* (0x20000000)  */
&GLOBAL-DEFINE LLDESADDACTION_MENUITEM_APPEND                         0 /* (0x00000000)  */
&GLOBAL-DEFINE LLDESADDACTION_MENUITEM_INSERT                 268435456 /* (0x10000000)  */
&GLOBAL-DEFINE LLDESADDACTION_MENUITEM_POSITIONMASK           268435456 /* (0x10000000)  */
&GLOBAL-DEFINE LLDESADDACTION_ACCEL_CONTROL                       65536 /* (0x00010000)  */
&GLOBAL-DEFINE LLDESADDACTION_ACCEL_SHIFT                        131072 /* (0x00020000)  */
&GLOBAL-DEFINE LLDESADDACTION_ACCEL_ALT                          262144 /* (0x00040000)  */
&GLOBAL-DEFINE LLDESADDACTION_ACCEL_VIRTKEY                      524288 /* (0x00080000)  */
&GLOBAL-DEFINE LLDESADDACTION_ACCEL_KEYMODIFIERMASK              983040 /* (0x000f0000)  */
&GLOBAL-DEFINE LLDESADDACTION_ACCEL_KEYCODEMASK                   65535 /* (0x0000ffff)  */
&GLOBAL-DEFINE LL_DESIGNEROPTSTR_PROJECTFILENAME                      1 /* (1)  */
&GLOBAL-DEFINE LL_DESIGNEROPTSTR_WORKSPACETITLE                       2 /* (2)  */
&GLOBAL-DEFINE LL_DESIGNEROPTSTR_PROJECTDESCRIPTION                   3 /* (3)  */
&GLOBAL-DEFINE LL_USEDIDENTIFIERSFLAG_VARIABLES                       1 /* (0x0001)  */
&GLOBAL-DEFINE LL_USEDIDENTIFIERSFLAG_FIELDS                          2 /* (0x0002)  */
&GLOBAL-DEFINE LL_USEDIDENTIFIERSFLAG_CHARTFIELDS                     4 /* (0x0004)  */
&GLOBAL-DEFINE LL_USEDIDENTIFIERSFLAG_TABLES                          8 /* (0x0008)  */
&GLOBAL-DEFINE LL_USEDIDENTIFIERSFLAG_RELATIONS                      16 /* (0x0010)  */
&GLOBAL-DEFINE LL_USEDIDENTIFIERSFLAG_FILES                          32 /* (0x0020)  */
&GLOBAL-DEFINE LL_TEMPFILENAME_DEFAULT                                0 /* (0x0000) see UT */
&GLOBAL-DEFINE LL_TEMPFILENAME_ENSURELONGPATH                         1 /* (0x0001) see UT */
&GLOBAL-DEFINE LL_DICTIONARY_TYPE_STATIC                              1 /* (1)  */
&GLOBAL-DEFINE LL_DICTIONARY_TYPE_IDENTIFIER                          2 /* (2)  */
&GLOBAL-DEFINE LL_DICTIONARY_TYPE_TABLE                               3 /* (3)  */
&GLOBAL-DEFINE LL_DICTIONARY_TYPE_RELATION                            4 /* (4)  */
&GLOBAL-DEFINE LL_DICTIONARY_TYPE_SORTORDER                           5 /* (5)  */
&GLOBAL-DEFINE LL_DICTIONARY_TYPE_ALL                                 0 /* (0) only valid for NULL, NULL to clear all dictionaries in one run */
&GLOBAL-DEFINE LL_UILANGUAGETYPE_NORMAL                               1 /* (0x00000001)  */
&GLOBAL-DEFINE LL_UILANGUAGETYPE_TRIAL                                2 /* (0x00000002)  */
&GLOBAL-DEFINE LL_ADDTABLEOPT_SUPPORTSSTACKEDSORTORDERS               1 /* (0x00000001)  */
&GLOBAL-DEFINE LL_ADDTABLEOPT_SUPPORTSADVANCEDFILTERING               2 /* (0x00000002)  */
&GLOBAL-DEFINE LL_INPLACEDESIGNERINTERACTION_QUERY_CANCLOSE               1 /* (1) wParam = 0, lParam = &BOOL */
&GLOBAL-DEFINE LL_EXPRXLATRESULT_OPTIMAL                              0 /* (0x00000000)  */
&GLOBAL-DEFINE LL_EXPRXLATRESULT_PARTIAL                              1 /* (0x00000001)  */
&GLOBAL-DEFINE LL_EXPRXLATRESULT_FAIL                                 2 /* (0x00000002)  */
&GLOBAL-DEFINE LL_EXPRXLATRESULT_MASK                                 7 /* (0x00000007) some reserve */
&GLOBAL-DEFINE LLJOBOPENCOPYEXFLAG_NO_COPY_FIELDLIST                  1 /* (0x0001)  */
&GLOBAL-DEFINE LLJOBOPENCOPYEXFLAG_NO_COPY_DBSTRUCTS                  2 /* (0x0002)  */
&GLOBAL-DEFINE LLJOBOPENCOPYEXFLAG_NO_COPY_XLATTABLES                 4 /* (0x0004)  */

/* These preprocessor commands tell LL to be loaded and stay loaded until the end of application. */
/* Modify the global defines if you require different behavior. */

&IF DEFINED(CMLL21_PERSISTENT)=0 &THEN
 &GLOBAL-DEFINE CMLL21_PERSISTENT PERSISTENT
&ENDIF

&IF DEFINED(CMLL21_DLLNAME)=0 &THEN
 &GLOBAL-DEFINE CMLL21_DLLNAME CMLL21.DLL
&ENDIF

/* functions */

    PROCEDURE LlJobOpen EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 10 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER nLanguage AS LONG.
    END.
    PROCEDURE LlJobOpenLCID EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 12 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER nLCID AS LONG.
    END.
    PROCEDURE LlJobClose EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 11 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlSetDebug EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 13 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER nOnOff AS LONG.
    END.
    PROCEDURE LlGetVersion EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 14 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER nCmd AS LONG.
    END.
    PROCEDURE LlGetNotificationMessage EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 15 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlSetNotificationMessage EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 16 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nMessage AS LONG.
    END.
    PROCEDURE LlDefineField EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 18 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
      DEFINE INPUT PARAMETER lpbufContents AS CHARACTER.
    END.
    PROCEDURE LlDefineFieldExt EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 19 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
      DEFINE INPUT PARAMETER lpbufContents AS CHARACTER.
      DEFINE INPUT PARAMETER lPara AS LONG.
      DEFINE INPUT PARAMETER lpPtr AS LONG.
    END.
    PROCEDURE LlDefineFieldExtHandle EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 20 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
      DEFINE INPUT PARAMETER hContents AS LONG.
      DEFINE INPUT PARAMETER lPara AS LONG.
      DEFINE INPUT PARAMETER lpPtr AS LONG.
    END.
    PROCEDURE LlDefineFieldStart EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 21 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlDefineVariable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 22 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
      DEFINE INPUT PARAMETER lpbufContents AS CHARACTER.
    END.
    PROCEDURE LlDefineVariableExt EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 23 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
      DEFINE INPUT PARAMETER lpbufContents AS CHARACTER.
      DEFINE INPUT PARAMETER lPara AS LONG.
      DEFINE INPUT PARAMETER lpPtr AS LONG.
    END.
    PROCEDURE LlDefineVariableExtHandle EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 24 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
      DEFINE INPUT PARAMETER hContents AS LONG.
      DEFINE INPUT PARAMETER lPara AS LONG.
      DEFINE INPUT PARAMETER lpPtr AS LONG.
    END.
    PROCEDURE LlDefineVariableName EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 25 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
    END.
    PROCEDURE LlDefineVariableStart EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 26 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlDefineSumVariable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 27 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
      DEFINE INPUT PARAMETER lpbufContents AS CHARACTER.
    END.
    PROCEDURE LlDefineLayout EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 28 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT PARAMETER pszTitle AS CHARACTER.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
    END.
    PROCEDURE LlDlgEditLine EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 29 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER lpBuf AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlDlgEditLineEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 30 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
      DEFINE INPUT PARAMETER nParaTypes AS LONG.
      DEFINE INPUT PARAMETER pszTitle AS CHARACTER.
      DEFINE INPUT PARAMETER bTable AS LONG.
      DEFINE INPUT PARAMETER pvReserved AS LONG.
    END.
    PROCEDURE LlPreviewSetTempPath EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 31 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszPath AS CHARACTER.
    END.
    PROCEDURE LlPreviewDeleteFiles EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 32 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
      DEFINE INPUT PARAMETER pszPath AS CHARACTER.
    END.
    PROCEDURE LlPreviewDisplay EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 33 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
      DEFINE INPUT PARAMETER pszPath AS CHARACTER.
      DEFINE INPUT PARAMETER Wnd AS LONG.
    END.
    PROCEDURE LlPreviewDisplayEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 34 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
      DEFINE INPUT PARAMETER pszPath AS CHARACTER.
      DEFINE INPUT PARAMETER Wnd AS LONG.
      DEFINE INPUT PARAMETER nOptions AS LONG.
      DEFINE INPUT PARAMETER pOptions AS LONG.
    END.
    PROCEDURE LlPrint EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 35 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintAbort EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 36 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintCheckLineFit EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 37 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintEnd EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 38 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nPages AS LONG.
    END.
    PROCEDURE LlPrintFields EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 39 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintFieldsEnd EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 40 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintGetCurrentPage EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 41 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintGetItemsPerPage EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 42 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintGetItemsPerTable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 43 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintGetRemainingItemsPerTable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 44 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszField AS CHARACTER.
    END.
    PROCEDURE LlPrintGetRemItemsPerTable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 45 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszField AS CHARACTER.
    END.
    PROCEDURE LlPrintGetOption EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 46 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nIndex AS LONG.
    END.
    PROCEDURE LlPrintGetPrinterInfo EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 47 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszPrn AS MEMPTR.
      DEFINE INPUT PARAMETER nPrnLen AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszPort AS MEMPTR.
      DEFINE INPUT PARAMETER nPortLen AS LONG.
    END.
    PROCEDURE LlPrintOptionsDialog EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 48 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT PARAMETER pszText AS CHARACTER.
    END.
    PROCEDURE LlPrintSelectOffsetEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 49 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
    END.
    PROCEDURE LlPrintSetBoxText EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 50 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER szText AS CHARACTER.
      DEFINE INPUT PARAMETER nPercentage AS LONG.
    END.
    PROCEDURE LlPrintSetOption EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 51 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nIndex AS LONG.
      DEFINE INPUT PARAMETER nValue AS LONG.
    END.
    PROCEDURE LlPrintUpdateBox EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 52 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintStart EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 53 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
      DEFINE INPUT PARAMETER nPrintOptions AS LONG.
      DEFINE INPUT PARAMETER nReserved AS LONG.
    END.
    PROCEDURE LlPrintWithBoxStart EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 54 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
      DEFINE INPUT PARAMETER nPrintOptions AS LONG.
      DEFINE INPUT PARAMETER nBoxType AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT PARAMETER pszTitle AS CHARACTER.
    END.
    PROCEDURE LlPrinterSetup EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 55 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
    END.
    PROCEDURE LlSelectFileDlgTitleEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 56 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT PARAMETER pszTitle AS CHARACTER.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszObjName AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
      DEFINE INPUT PARAMETER pReserved AS LONG.
    END.
    PROCEDURE LlSetDlgboxMode EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 57 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER nMode AS LONG.
    END.
    PROCEDURE LlGetDlgboxMode EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 58 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
    END.
    PROCEDURE LlExprParse EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 59 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lpExprText AS CHARACTER.
      DEFINE INPUT PARAMETER bIncludeFields AS LONG.
    END.
    PROCEDURE LlExprType EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 60 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lpExpr AS LONG.
    END.
    PROCEDURE LlExprError EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 61 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuf AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlExprFree EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 62 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lpExpr AS LONG.
    END.
    PROCEDURE LlExprEvaluate EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 63 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lpExpr AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuf AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlExprGetUsedVars EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 162 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lpExpr AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlSetOption EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 64 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nMode AS LONG.
      DEFINE INPUT PARAMETER nValue AS LONG.
    END.
    PROCEDURE LlGetOption EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 65 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nMode AS LONG.
    END.
    PROCEDURE LlSetOptionString EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 66 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nIndex AS LONG.
      DEFINE INPUT PARAMETER pszBuffer AS CHARACTER.
    END.
    PROCEDURE LlGetOptionString EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 67 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nIndex AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlPrintSetOptionString EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 68 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nIndex AS LONG.
      DEFINE INPUT PARAMETER pszBuffer AS CHARACTER.
    END.
    PROCEDURE LlPrintGetOptionString EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 69 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nIndex AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlDesignerProhibitAction EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 70 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nMenuID AS LONG.
    END.
    PROCEDURE LlDesignerProhibitFunction EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 1 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszFunction AS CHARACTER.
    END.
    PROCEDURE LlPrintEnableObject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 71 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszObjectName AS CHARACTER.
      DEFINE INPUT PARAMETER bEnable AS LONG.
    END.
    PROCEDURE LlSetFileExtensions EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 72 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER pszObjectExt AS CHARACTER.
      DEFINE INPUT PARAMETER pszPrinterExt AS CHARACTER.
      DEFINE INPUT PARAMETER pszSketchExt AS CHARACTER.
    END.
    PROCEDURE LlPrintGetTextCharsPrinted EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 73 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszObjectName AS CHARACTER.
    END.
    PROCEDURE LlPrintGetFieldCharsPrinted EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 74 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszObjectName AS CHARACTER.
      DEFINE INPUT PARAMETER pszField AS CHARACTER.
    END.
    PROCEDURE LlPrintIsVariableUsed EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 75 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
    END.
    PROCEDURE LlPrintIsFieldUsed EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 76 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszFieldName AS CHARACTER.
    END.
    PROCEDURE LlPrintOptionsDialogTitle EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 77 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT PARAMETER pszTitle AS CHARACTER.
      DEFINE INPUT PARAMETER pszText AS CHARACTER.
    END.
    PROCEDURE LlSetPrinterToDefault EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 78 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
    END.
    PROCEDURE LlDefineSortOrderStart EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 79 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlDefineSortOrder EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 80 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszIdentifier AS CHARACTER.
      DEFINE INPUT PARAMETER pszText AS CHARACTER.
    END.
    PROCEDURE LlPrintGetSortOrder EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 81 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlDefineGrouping EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 82 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszSortorder AS CHARACTER.
      DEFINE INPUT PARAMETER pszIdentifier AS CHARACTER.
      DEFINE INPUT PARAMETER pszText AS CHARACTER.
    END.
    PROCEDURE LlPrintGetGrouping EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 83 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlAddCtlSupport EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 84 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
      DEFINE INPUT PARAMETER pszInifile AS CHARACTER.
    END.
    PROCEDURE LlPrintBeginGroup EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 85 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lParam AS LONG.
      DEFINE INPUT PARAMETER lpParam AS LONG.
    END.
    PROCEDURE LlPrintEndGroup EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 86 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lParam AS LONG.
      DEFINE INPUT PARAMETER lpParam AS LONG.
    END.
    PROCEDURE LlPrintGroupLine EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 87 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lParam AS LONG.
      DEFINE INPUT PARAMETER lpParam AS LONG.
    END.
    PROCEDURE LlPrintGroupHeader EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 88 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lParam AS LONG.
    END.
    PROCEDURE LlPrintGetFilterExpression EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 89 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlPrintWillMatchFilter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 90 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlPrintDidMatchFilter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 91 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlGetFieldContents EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 93 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlGetVariableContents EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 92 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlGetSumVariableContents EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 94 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlGetUserVariableContents EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 95 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlGetVariableType EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 96 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
    END.
    PROCEDURE LlGetFieldType EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 97 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
    END.
    PROCEDURE LlSetPrinterDefaultsDir EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 200 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszDir AS CHARACTER.
    END.
    PROCEDURE LlCreateSketch EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 201 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER lpszObjName AS CHARACTER.
    END.
    PROCEDURE LlViewerProhibitAction EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 202 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nMenuID AS LONG.
    END.
    PROCEDURE LlPrintCopyPrinterConfiguration EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 203 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lpszFilename AS CHARACTER.
      DEFINE INPUT PARAMETER nFunction AS LONG.
    END.
    PROCEDURE LlSetPrinterInPrinterFileA EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 204 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
      DEFINE INPUT PARAMETER nPrinterIndex AS LONG.
      DEFINE INPUT PARAMETER pszPrinter AS CHARACTER.
      DEFINE INPUT PARAMETER pDevMode AS LONG.
    END.
    PROCEDURE LlRTFCreateObject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 228 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlRTFDeleteObject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 229 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
    END.
    PROCEDURE LlRTFSetText EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 230 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
      DEFINE INPUT PARAMETER pszText AS CHARACTER.
    END.
    PROCEDURE LlRTFGetTextLength EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 231 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlRTFGetText EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 232 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlRTFEditObject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 233 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
      DEFINE INPUT PARAMETER hWnd AS LONG.
      DEFINE INPUT PARAMETER hPrnDC AS LONG.
      DEFINE INPUT PARAMETER nProjectType AS LONG.
      DEFINE INPUT PARAMETER bModal AS LONG.
    END.
    PROCEDURE LlRTFCopyToClipboard EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 234 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
    END.
    PROCEDURE LlRTFDisplay EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 235 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
      DEFINE INPUT PARAMETER hDC AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pRC AS MEMPTR.
      DEFINE INPUT PARAMETER bRestart AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pnState AS HANDLE TO LONG.
    END.
    PROCEDURE LlRTFEditorProhibitAction EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 109 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
      DEFINE INPUT PARAMETER nControlID AS LONG.
    END.
    PROCEDURE LlRTFEditorInvokeAction EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 117 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
      DEFINE INPUT PARAMETER nControlID AS LONG.
    END.
    PROCEDURE LlDebugOutput EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 240 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER nIndent AS LONG.
      DEFINE INPUT PARAMETER pszText AS CHARACTER.
    END.
    PROCEDURE LlEnumGetFirstVar EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 241 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlEnumGetFirstField EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 242 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlEnumGetNextEntry EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 243 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nPos AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlEnumGetEntry EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 244 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nPos AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszNameBuf AS MEMPTR.
      DEFINE INPUT PARAMETER nNameBufSize AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszContBuf AS MEMPTR.
      DEFINE INPUT PARAMETER nContBufSize AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pHandle AS HANDLE TO LONG.
      DEFINE INPUT-OUTPUT PARAMETER pType AS HANDLE TO LONG.
    END.
    PROCEDURE LlPrintResetObjectStates EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 245 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlXSetParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 246 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nExtensionType AS LONG.
      DEFINE INPUT PARAMETER pszExtensionName AS CHARACTER.
      DEFINE INPUT PARAMETER pszKey AS CHARACTER.
      DEFINE INPUT PARAMETER pszValue AS CHARACTER.
    END.
    PROCEDURE LlXGetParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 247 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nExtensionType AS LONG.
      DEFINE INPUT PARAMETER pszExtensionName AS CHARACTER.
      DEFINE INPUT PARAMETER pszKey AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlPrintResetProjectState EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 248 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
    END.
    PROCEDURE LlDefineChartFieldStart EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 2 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlDefineChartFieldExt EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 3 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszVarName AS CHARACTER.
      DEFINE INPUT PARAMETER pszContents AS CHARACTER.
      DEFINE INPUT PARAMETER lPara AS LONG.
      DEFINE INPUT PARAMETER lpPtr AS LONG.
    END.
    PROCEDURE LlPrintDeclareChartRow EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 4 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlPrintGetChartObjectCount EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 6 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nType AS LONG.
    END.
    PROCEDURE LlPrintIsChartFieldUsed EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 5 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszFieldName AS CHARACTER.
    END.
    PROCEDURE LlGetChartFieldContents EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 8 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlEnumGetFirstChartField EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 9 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlPrintGetRemainingSpacePerTable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 102 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszField AS CHARACTER.
      DEFINE INPUT PARAMETER nDimension AS LONG.
    END.
    PROCEDURE LlSetDefaultProjectParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 108 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszParameter AS CHARACTER.
      DEFINE INPUT PARAMETER pszValue AS CHARACTER.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlGetDefaultProjectParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 110 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszParameter AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pnFlags AS HANDLE TO LONG.
    END.
    PROCEDURE LlPrintSetProjectParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 113 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszParameter AS CHARACTER.
      DEFINE INPUT PARAMETER pszValue AS CHARACTER.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlPrintGetProjectParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 114 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszParameter AS CHARACTER.
      DEFINE INPUT PARAMETER bEvaluated AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pnFlags AS HANDLE TO LONG.
    END.
    PROCEDURE LlExprContainsVariable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 7 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hExpr AS LONG.
      DEFINE INPUT PARAMETER pszVariable AS CHARACTER.
    END.
    PROCEDURE LlExprIsConstant EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 116 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hExpr AS LONG.
    END.
    PROCEDURE LlProfileStart EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 136 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hThread AS LONG.
      DEFINE INPUT PARAMETER pszDescr AS CHARACTER.
      DEFINE INPUT PARAMETER pszFilename AS CHARACTER.
      DEFINE INPUT PARAMETER nTicksMS AS LONG.
    END.
    PROCEDURE LlProfileEnd EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 137 {&CMLL21_PERSISTENT}:
      DEFINE INPUT PARAMETER hThread AS LONG.
    END.
    PROCEDURE LlDbAddTable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 139 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER pszTableID AS CHARACTER.
      DEFINE INPUT PARAMETER pszDisplayName AS CHARACTER.
    END.
    PROCEDURE LlDbAddTableRelation EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 140 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER pszTableID AS CHARACTER.
      DEFINE INPUT PARAMETER pszParentTableID AS CHARACTER.
      DEFINE INPUT PARAMETER pszRelationID AS CHARACTER.
      DEFINE INPUT PARAMETER pszRelationDisplayName AS CHARACTER.
    END.
    PROCEDURE LlDbAddTableSortOrder EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 141 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER pszTableID AS CHARACTER.
      DEFINE INPUT PARAMETER pszSortOrderID AS CHARACTER.
      DEFINE INPUT PARAMETER pszSortOrderDisplayName AS CHARACTER.
    END.
    PROCEDURE LlPrintDbGetCurrentTable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 142 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszTableID AS MEMPTR.
      DEFINE INPUT PARAMETER nTableIDLength AS LONG.
      DEFINE INPUT PARAMETER bCompletePath AS LONG.
    END.
    PROCEDURE LlPrintDbGetCurrentTableRelation EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 143 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszRelationID AS MEMPTR.
      DEFINE INPUT PARAMETER nRelationIDLength AS LONG.
    END.
    PROCEDURE LlPrintDbGetCurrentTableSortOrder EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 146 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszSortOrderID AS MEMPTR.
      DEFINE INPUT PARAMETER nSortOrderIDLength AS LONG.
    END.
    PROCEDURE LlDbDumpStructure EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 149 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
    END.
    PROCEDURE LlPrintDbGetRootTableCount EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 151 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
    END.
    PROCEDURE LlDbSetMasterTable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 152 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER pszTableID AS CHARACTER.
    END.
    PROCEDURE LlDbGetMasterTable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 157 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlXSetExportParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 158 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszExtensionName AS CHARACTER.
      DEFINE INPUT PARAMETER pszKey AS CHARACTER.
      DEFINE INPUT PARAMETER pszValue AS CHARACTER.
    END.
    PROCEDURE LlXGetExportParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 160 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszExtensionName AS CHARACTER.
      DEFINE INPUT PARAMETER pszKey AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlXlatName EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 164 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlDesignerProhibitEditingObject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 185 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszObject AS CHARACTER.
    END.
    PROCEDURE LlGetUsedIdentifiers EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 186 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszProjectName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlExprGetUsedVarsEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 205 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lpExpr AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
      DEFINE INPUT PARAMETER OrgName AS LONG.
    END.
    PROCEDURE LlDomGetProject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 206 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER phDOMObj AS HANDLE TO LONG.
    END.
    PROCEDURE LlDomGetProperty EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 207 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hDOMObj AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlDomSetProperty EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 208 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hDOMObj AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT PARAMETER pszValue AS CHARACTER.
    END.
    PROCEDURE LlDomGetObject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 209 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hDOMObj AS LONG.
      DEFINE INPUT PARAMETER pszName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER phDOMSubObj AS HANDLE TO LONG.
    END.
    PROCEDURE LlDomGetSubobjectCount EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 210 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hDOMObj AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pnCount AS HANDLE TO LONG.
    END.
    PROCEDURE LlDomGetSubobject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 211 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hDOMObj AS LONG.
      DEFINE INPUT PARAMETER nPosition AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER phDOMSubObj AS HANDLE TO LONG.
    END.
    PROCEDURE LlDomCreateSubobject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 212 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hDOMObj AS LONG.
      DEFINE INPUT PARAMETER nPosition AS LONG.
      DEFINE INPUT PARAMETER pszType AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER phDOMSubObj AS HANDLE TO LONG.
    END.
    PROCEDURE LlDomDeleteSubobject EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 213 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hDOMObj AS LONG.
      DEFINE INPUT PARAMETER nPosition AS LONG.
    END.
    PROCEDURE LlProjectOpen EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 214 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nObjType AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
      DEFINE INPUT PARAMETER nOpenMode AS LONG.
    END.
    PROCEDURE LlProjectSave EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 215 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszObjName AS CHARACTER.
    END.
    PROCEDURE LlProjectClose EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 216 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlAssociatePreviewControl EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 218 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hWndControl AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlGetErrortext EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 219 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER nError AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlSetPreviewOption EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 221 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nOption AS LONG.
      DEFINE INPUT PARAMETER nValue AS LONG.
    END.
    PROCEDURE LlGetPreviewOption EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 222 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nOption AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pnValue AS HANDLE TO LONG.
    END.
    PROCEDURE LlDesignerInvokeAction EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 223 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nMenuID AS LONG.
    END.
    PROCEDURE LlDesignerRefreshWorkspace EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 224 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
    END.
    PROCEDURE LlDesignerFileOpen EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 225 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszFilename AS CHARACTER.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlDesignerFileSave EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 226 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlDesignerAddAction EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 227 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nID AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
      DEFINE INPUT PARAMETER pszMenuText AS CHARACTER.
      DEFINE INPUT PARAMETER pszMenuHierarchy AS CHARACTER.
      DEFINE INPUT PARAMETER pszTooltipText AS CHARACTER.
      DEFINE INPUT PARAMETER nIcon AS LONG.
      DEFINE INPUT PARAMETER pvReserved AS LONG.
    END.
    PROCEDURE LlDesignerGetOptionString EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 236 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nIndex AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlDesignerSetOptionString EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 237 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nIndex AS LONG.
      DEFINE INPUT PARAMETER pszBuffer AS CHARACTER.
    END.
    PROCEDURE LlJobOpenCopy EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 239 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
    END.
    PROCEDURE LlGetProjectParameter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 249 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszProjectName AS CHARACTER.
      DEFINE INPUT PARAMETER pszParameter AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlConvertBLOBToString EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 250 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER pBytes AS BYTE.
      DEFINE INPUT PARAMETER nBytes AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
      DEFINE INPUT PARAMETER bWithCompression AS LONG.
    END.
    PROCEDURE LlConvertStringToBLOB EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 251 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER pszText AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pBytes AS HANDLE TO BYTE.
      DEFINE INPUT PARAMETER nBytes AS LONG.
    END.
    PROCEDURE LlDbAddTableRelationEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 238 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER pszTableID AS CHARACTER.
      DEFINE INPUT PARAMETER pszParentTableID AS CHARACTER.
      DEFINE INPUT PARAMETER pszRelationID AS CHARACTER.
      DEFINE INPUT PARAMETER pszRelationDisplayName AS CHARACTER.
      DEFINE INPUT PARAMETER pszKeyField AS CHARACTER.
      DEFINE INPUT PARAMETER pszParentKeyField AS CHARACTER.
    END.
    PROCEDURE LlDbAddTableSortOrderEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 257 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER pszTableID AS CHARACTER.
      DEFINE INPUT PARAMETER pszSortOrderID AS CHARACTER.
      DEFINE INPUT PARAMETER pszSortOrderDisplayName AS CHARACTER.
      DEFINE INPUT PARAMETER pszField AS CHARACTER.
    END.
    PROCEDURE LlGetUsedIdentifiersEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 258 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszProjectName AS CHARACTER.
      DEFINE INPUT PARAMETER nIdentifierTypes AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlGetTempFileName EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 259 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER pszPrefix AS CHARACTER.
      DEFINE INPUT PARAMETER pszExt AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
      DEFINE INPUT PARAMETER nOptions AS LONG.
    END.
    PROCEDURE LlGetDebug EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 260 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
    END.
    PROCEDURE LlRTFEditorGetRTFControlHandle EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 261 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
    END.
    PROCEDURE LlGetDefaultPrinter EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 262 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszPrinter AS MEMPTR.
      DEFINE INPUT-OUTPUT PARAMETER pnPrinterBufSize AS HANDLE TO LONG.
      DEFINE INPUT PARAMETER pDevMode AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pnDevModeBufSize AS HANDLE TO LONG.
      DEFINE INPUT PARAMETER nOptions AS LONG.
    END.
    PROCEDURE LlLocAddDictionaryEntry EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 263 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nLCID AS LONG.
      DEFINE INPUT PARAMETER pszKey AS CHARACTER.
      DEFINE INPUT PARAMETER pszValue AS CHARACTER.
      DEFINE INPUT PARAMETER nType AS LONG.
    END.
    PROCEDURE LlLocAddDesignLCID EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 264 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nLCID AS LONG.
    END.
    PROCEDURE LlIsUILanguageAvailable EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 265 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER nLanguage AS LONG.
      DEFINE INPUT PARAMETER nTypesToLookFor AS LONG.
    END.
    PROCEDURE LlIsUILanguageAvailableLCID EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 266 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER nLCID AS LONG.
      DEFINE INPUT PARAMETER nTypesToLookFor AS LONG.
    END.
    PROCEDURE LlDbAddTableEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 267 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER pszTableID AS CHARACTER.
      DEFINE INPUT PARAMETER pszDisplayName AS CHARACTER.
      DEFINE INPUT PARAMETER nOptions AS LONG.
    END.
    PROCEDURE LlRTFSetTextEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 269 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER hRTF AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
      DEFINE INPUT PARAMETER pszText AS CHARACTER.
    END.
    PROCEDURE LlInplaceDesignerInteraction EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 270 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nAction AS LONG.
      DEFINE INPUT PARAMETER wParam AS LONG.
      DEFINE INPUT PARAMETER lParam AS LONG.
    END.
    PROCEDURE LlGetProjectDescription EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 280 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER pszProjectName AS CHARACTER.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlSRTriggerExport EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 289 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER hSessionJob AS LONG.
      DEFINE INPUT PARAMETER pszID AS CHARACTER.
      DEFINE INPUT PARAMETER pszExportFormat AS CHARACTER.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.
    PROCEDURE LlExprGetUsedFunctions EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 292 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER lpExpr AS LONG.
      DEFINE INPUT-OUTPUT PARAMETER pszBuffer AS MEMPTR.
      DEFINE INPUT PARAMETER nBufSize AS LONG.
    END.
    PROCEDURE LlDesignerTriggerJobInUIThread EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 293 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hLlJob AS LONG.
      DEFINE INPUT PARAMETER nUserData AS LONG.
    END.
    PROCEDURE LlJobOpenCopyEx EXTERNAL "{&CMLL21_DLLNAME}" ORDINAL 299 {&CMLL21_PERSISTENT}:
      DEFINE RETURN PARAMETER rc AS LONG.
      DEFINE INPUT PARAMETER hJob AS LONG.
      DEFINE INPUT PARAMETER nFlags AS LONG.
    END.

