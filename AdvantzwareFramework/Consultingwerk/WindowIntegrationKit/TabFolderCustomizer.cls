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
    File        : TabFolderCustomizer
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jan 20 13:06:41 CET 2012
    Notes       : rstark 4.17.2017 - copied from SampleTabFolderCustomizer.cls
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.*       FROM PROPATH .
USING Consultingwerk.WindowIntegrationKit.Forms.* FROM PROPATH .
USING Consultingwerk.Util.*                       FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING Infragistics.Win.UltraWinTabControl.*       FROM ASSEMBLY .

CLASS Consultingwerk.WindowIntegrationKit.TabFolderCustomizer
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
        
        poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/navigate_right.png":U) .

/*        CASE piPageNumber:                                                                                                                    */
/*            WHEN  1 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_green.png":U) . */
/*            WHEN  2 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_yellow.png":U) .*/
/*            WHEN  3 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_red.png":U) .   */
/*            WHEN  4 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_blue.png":U) .  */
/*            WHEN  5 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_green.png":U) . */
/*            WHEN  6 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_yellow.png":U) .*/
/*            WHEN  7 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_red.png":U) .   */
/*            WHEN  8 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_blue.png":U) .  */
/*            WHEN  9 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_green.png":U) . */
/*            WHEN 10 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_yellow.png":U) .*/
/*            WHEN 11 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_red.png":U) .   */
/*            WHEN 12 THEN                                                                                                                      */
/*                poTab:Appearance:Image = ImageHelper:Load ("Consultingwerk/WindowIntegrationKit/Samples/Images/document_plain_blue.png":U) .  */
/*                                                                                                                                              */
/*            WHEN  1 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_1.png":U) .*/
/*            WHEN  2 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_2.png":U) .*/
/*            WHEN  3 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_3.png":U) .*/
/*            WHEN  4 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_4.png":U) .*/
/*            WHEN  5 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_5.png":U) .*/
/*            WHEN  6 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_6.png":U) .*/
/*            WHEN  7 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_7.png":U) .*/
/*            WHEN  8 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_8.png":U) .*/
/*            WHEN  9 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/nav_plain.png":U) .     */
/*            WHEN 10 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/object_ball.png":U) .   */
/*            WHEN 11 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/photographic_filter.png":U) .*/
/*            WHEN 12 THEN                                                                           */
/*                poTab:Appearance:Image = ImageHelper:Load ("Graphics/32x32/keyboard_key_b.png":U) .*/
/*        END CASE .*/

    END METHOD.

END CLASS.

