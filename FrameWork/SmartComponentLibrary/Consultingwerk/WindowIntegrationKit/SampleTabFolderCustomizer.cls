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
    File        : SampleTabFolderCustomizer
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 20 13:06:41 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.*       FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING Infragistics.Win.UltraWinTabControl.*       FROM ASSEMBLY .

CLASS Consultingwerk.WindowIntegrationKit.SampleTabFolderCustomizer
    IMPLEMENTS ITabFolderCustomizer:

    /*------------------------------------------------------------------------------
        Purpose: Callback method into Service classes, allows the customization of
                 UltraTabs for the EmbeddedWindowTabFolderForm (or similar
                 IEmbeddedWindowForm implementations)
        Notes:   Allows customization of UltraTab properties like the appearance
        @param phProcedure The procedure handle that contains the Tab folder
        @param poForm The IEmbeddedWindowForm instance containing the Tab Control
        @param piPageNumber The current tab page number
        @param pcPageLabel The current tab page label
        @param poTabControl The reference to the UltraTabControl
        @param poTab The generated UltrTab object (tab page)
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AssignTabProperties (phProcedure AS HANDLE,
                                            poForm AS IEmbeddedWindowForm,
                                            piPageNumber AS INTEGER,
                                            pcPageLabel AS CHARACTER,
                                            poTabControl AS UltraTabControl,
                                            poTab AS UltraTab):

        IF phProcedure:FILENAME MATCHES "*demo~\ordermaint~\orderwin_embedded_plus*":U OR
           phProcedure:FILENAME MATCHES "*demo/ordermaint/orderwin_embedded_plus*":U THEN DO:
            CASE piPageNumber:
                WHEN 1 THEN DO:
                    poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document.png":U) .
                END.
                WHEN 2 THEN DO:
                    poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/businesspeople.png":U) .
                END.
            END CASE .

            RETURN .
        END.

        CASE piPageNumber:
            WHEN 1 THEN DO:
                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_green.png":U) .
            END.
            WHEN 2 THEN DO:
                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_yellow.png":U) .
            END.
            WHEN 3 THEN DO:
                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_red.png":U) .
            END.
        END CASE .

    END METHOD.

END CLASS.

