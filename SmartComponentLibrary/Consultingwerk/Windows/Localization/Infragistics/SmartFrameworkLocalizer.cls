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
    File        : SmartFrameworkLocalizer
    Purpose     : Performs translation of Infragistics Resource strings
                  from the SmartFramework
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jan 16 10:00:13 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.Localization.*              FROM PROPATH .
USING Consultingwerk.Windows.Localization.Infragistics.* FROM PROPATH .
USING Infragistics.Shared.*                              FROM ASSEMBLY .
USING Progress.Lang.*                                    FROM PROPATH .

CLASS Consultingwerk.Windows.Localization.Infragistics.SmartFrameworkLocalizer
    IMPLEMENTS ILocalizer:

    /*------------------------------------------------------------------------------
        Purpose: Performs user interface localization during the startup of a
                 session
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Localize ():

        DEFINE VARIABLE rc AS ResourceCustomizer NO-UNDO .

        /* MDI Manager */

        ASSIGN rc = Infragistics.Win.UltraWinTabbedMdi.Resources:Customizer .

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinTabbedMdi":U, "MenuItemCancel":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinTabbedMdi":U, "MenuItemClose":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinTabbedMdi":U, "MenuItemNewHorizontalGroup":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinTabbedMdi":U, "MenuItemNewVerticalGroup":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinTabbedMdi":U, "MenuItemMoveToNextGroup":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinTabbedMdi":U, "MenuItemMoveToPreviousGroup":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinTabbedMdi":U, "MenuItemMaximize":U).

        /* Toolbar */
        ASSIGN rc = Infragistics.Win.UltraWinToolbars.Resources:Customizer .

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "MdiCommandCloseWindows":U) .

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "QatQuickCustomizeToolTip":U).

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "AddToQuickAccessToolbar":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "RemoveFromQuickAccessToolbar":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "QuickAccessToolbarAboveRibbon":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "QuickAccessToolbarAboveRibbonCustomize":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "QuickAccessToolbarBelowRibbon":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "QuickAccessToolbarBelowRibbonCustomize":U).

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "MinimizeRibbon":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "Customize":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinToolbars":U, "LockToolbars":U).

        /* Outlook Bar */
        ASSIGN rc = Infragistics.Win.UltraWinExplorerBar.Resources:Customizer .

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationQuickCustomizeButton_Tooltip":U) .

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationQuickCustomizeMenu_AddOrRemoveButtons":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationQuickCustomizeMenu_NavigationPaneOptions":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationQuickCustomizeMenu_ShowFewerButtons":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationQuickCustomizeMenu_ShowMoreButtons":U) .

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_AccessibleDescription":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_CancelButton":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_Caption":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_Label":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_MoveDownButton":U) .
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_MoveDownButton_AccessibleDescription":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_MoveUpButton":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_MoveUpButton_AccessibleDescription":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_OKButton":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_ResetButton":U).
        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinExplorerBar":U, "NavigationPaneOptionsDialog_ResetButton_AccessibleDescription":U).

        /* UltraGrid */
        ASSIGN rc = Infragistics.Win.UltraWinGrid.Resources:Customizer .

        THIS-OBJECT:PerformTranslation (rc, "Infragistics.Win.UltraWinGrid":U, "GroupByBoxDefaultPrompt":U).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs translation for a single Infragistics resource string
        Notes:
        @param poResourceCustomer The Infragistics Resource Customizer
        @param pcTranslationScope The translation scope
        @param pcTranslationKey The translation key
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PerformTranslation (poResourceCustomer AS ResourceCustomizer,
                                              pcTranslationScope AS CHARACTER,
                                              pcTranslationKey AS CHARACTER):

        DEFINE VARIABLE cText AS CHARACTER NO-UNDO.

        ASSIGN cText = {Consultingwerk/translate.i &scope=pcTranslationScope &key=pcTranslationKey} .

        IF cText > "":U THEN
            poResourceCustomer:SetCustomizedString (pcTranslationKey, cText).
        ELSE
            poResourceCustomer:ResetCustomizedString (pcTranslationKey).

    END METHOD .

END CLASS.
