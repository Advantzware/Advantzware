/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IWidgetFacade
    Purpose     : Represents standard ABL Widget properties
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:14:58 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.* FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

INTERFACE Consultingwerk.Windows.LegacyGuiMigration.Widgets.IWidgetFacade:  

    /*------------------------------------------------------------------------------
        Purpose: A character value that represents the data type of the field 
                 associated with the widget 
        Notes:   For example, the DATA-TYPE attribute of a slider widget always 
                 returns the value "INTEGER" because slider widgets can only 
                 represent integer values
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DATA-TYPE AS CHARACTER NO-UNDO  
    GET.
    
    /*------------------------------------------------------------------------------
        Purpose: The text format of a widget or browse-cell 
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FORMAT AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Indicates whether to "hide" a widget
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY HIDDEN AS LOGICAL NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Indicates whether the value of the SCREEN-VALUE attribute for the 
                 widget has changed
        Notes:   For all widgets, the MODIFIED attribute is set to TRUE when the 
                 SCREEN-VALUE attribute for the widget is changed, whether or not 
                 the field is enabled. For all widgets except the editor widget, 
                 the MODIFIED attribute is set to TRUE if the SCREEN-VALUE attribute 
                 for the widget is changed using an ABL statement, such as assignment 
                 or DISPLAY. DISPLAY sets MODIFIED to TRUE only when the field is 
                 enabled. You can then reset the attribute to FALSE for each widget 
                 that can receive input focus or otherwise change value after it is 
                 initially displayed.
                 For editors, the successful execution of either the SAVE-FILE( ) or 
                 the READ-FILE( ) methods sets the MODIFIED attribute to FALSE.
                 For browses, if any browse cell changes, the AVM sets MODIFIED to 
                 TRUE. The application can reset MODIFIED to FALSE as necessary. If 
                 the query associated with a browse is reopened, the AVM resets MODIFIED 
                 to FALSE.
                 You can use the SENSITIVE attribute to reset the MODIFIED attribute. 
                 For example, when the screen-value of a fill-in field is changed, 
                 MODIFIED is set to TRUE. If you then set SENSITIVE to FALSE and then 
                 to TRUE, you enable the fill-in for further input and the MODIFIED 
                 attribute is reset to FALSE.
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY MODIFIED AS LOGICAL NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: A string identifier for the specified object or widget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY NAME AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: An arbitrary string associated with the handle of an object or widget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PRIVATE-DATA AS CHARACTER NO-UNDO 
    GET.
    SET. 
  
    /*------------------------------------------------------------------------------
        Purpose: The data value in the screen buffer associated with the widget
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SCREEN-VALUE AS CHARACTER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Indicates whether a widget can receive input focus or events
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SENSITIVE AS LOGICAL NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: The type of a handle
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY TYPE AS CHARACTER NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Indicates whether a widget is currently visible on the display
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY VISIBLE AS LOGICAL NO-UNDO 
	GET.
	SET. 

END INTERFACE.
