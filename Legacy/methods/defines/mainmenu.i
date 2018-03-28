/* mainmenu.i */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

g_mainmenu = THIS-PROCEDURE.

DEFINE VARIABLE popup-ptr        AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE menu-bar-ptr     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE sub-menu-ptr     AS WIDGET-HANDLE EXTENT 60 NO-UNDO.
DEFINE VARIABLE menu-item-ptr    AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE exit-item-ptr    AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE current-widget   AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE m_est-only       AS LOGICAL       NO-UNDO.
DEFINE VARIABLE m_menu-lst       AS CHARACTER     EXTENT 2 NO-UNDO.
DEFINE VARIABLE m_item1          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE m_item2          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE m_item3          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE i                AS INTEGER       NO-UNDO.
DEFINE VARIABLE j                AS INTEGER       NO-UNDO.
DEFINE VARIABLE v_image_filename AS CHARACTER     NO-UNDO.
DEFINE VARIABLE ldummy           AS LOGICAL       NO-UNDO.

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE TEMP-TABLE wk-ptrs NO-UNDO
    FIELD menu-name AS CHARACTER
    FIELD smenu-ptr AS WIDGET-HANDLE
    .
DEFINE TEMP-TABLE ttblMenuBar NO-UNDO
    FIELD menuBarName AS CHARACTER
    FIELD menuBarPtr  AS HANDLE
        INDEX ttblMenuBar IS PRIMARY menuBarName
        .
DEFINE FRAME FRAME-USER.
