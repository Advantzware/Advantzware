/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SubmenuCustomizer
    Purpose     : Sample implementation of the ISubmenuCustomizer Interface
                  Typically replaced in a project with custom logic.
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Sep 30 10:41:41 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.WindowIntegrationKit.SubmenuCustomizer
    IMPLEMENTS ISubmenuCustomizer:

    /*------------------------------------------------------------------------------
        Purpose: Customizes the properties of a new create PopupMenuTool in relation
                 to a SUBMENU widget handle
        Notes:
        @param phSubmenu The handle of the SUBMENU widget
        @param poPopupMenuTool The reference to the PopupMenuTool rendered for the SUBMENU
        @return Return TRUE when changes have been made, otherwise FALSE
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL AssignPopupMenuToolProperties (phSubmenu AS HANDLE,
                                                         poPopupMenuTool AS Infragistics.Win.UltraWinToolbars.PopupMenuTool):

        DEFINE VARIABLE hProgram AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cLabel   AS CHARACTER NO-UNDO.

        ASSIGN hProgram = phSubmenu:INSTANTIATING-PROCEDURE .

        IF phSubmenu:NAME = "m_File":U THEN
            CASE hProgram:FILE-NAME:
                WHEN "demo~\ordermaint~\orderwin_embedded.w":U OR
                WHEN "demo/ordermaint/orderwin_embedded.w":U THEN
                    ASSIGN cLabel = "Order Maintenance"{&TRAN} .
            END CASE .

        IF cLabel > "":U THEN DO:
            poPopupMenuTool:SharedProps:Caption = cLabel .
            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD.

END CLASS.
