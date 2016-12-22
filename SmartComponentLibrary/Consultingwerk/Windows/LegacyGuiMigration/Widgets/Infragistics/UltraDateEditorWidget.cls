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
    File        : UltraDateEditorWidget
    Purpose     : Represents a text-editor
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:22:12 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.*              FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.* FROM PROPATH .  
USING Infragistics.Win.UltraWinEditors.*                               FROM ASSEMBLY .
USING Progress.Lang.*                                                  FROM PROPATH .

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Infragistics.UltraDateEditorWidget 
    INHERITS InfragisticsControlWidget
    IMPLEMENTS IDateInputValueWidget: 

    DEFINE VARIABLE oControl AS UltraDateTimeEditor NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: A character value that represents the data type of the field 
                 associated with the widget 
        Notes:   For example, the DATA-TYPE attribute of a slider widget always 
                 returns the value "INTEGER" because slider widgets can only 
                 represent integer values
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC OVERRIDE PROPERTY DATA-TYPE AS CHARACTER NO-UNDO INIT "DATE":U  
    GET.
    PROTECTED SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Used for data-representation widgets, such as field-level widgets 
                 that represent variables or database fields
        Notes:   The value for the INPUT-VALUE attribute is the unformatted 
                 SCREEN-VALUE of a widget                          
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY INPUT-VALUE AS DATE NO-UNDO 
	GET:
	    RETURN DATE(oControl:DateTime) . 
	END GET .
	SET (arg AS DATE):
	    ASSIGN oControl:DateTime = arg . 
	END SET . 

	/*------------------------------------------------------------------------------
        Purpose: The type of a handle
	    Notes:   FILL-IN
	------------------------------------------------------------------------------*/
	DEFINE PUBLIC OVERRIDE PROPERTY TYPE AS CHARACTER NO-UNDO INIT "FILL-IN":U 
	GET .
	PROTECTED SET . 

	/*------------------------------------------------------------------------------
	   Purpose: Constructor for the UltraDateTimeEditorWidget class
	   Notes:
	   @param poEditor The reference to the UltraTextEditor this instance should represent
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC UltraDateEditorWidget (poEditor AS UltraDateTimeEditor):
		SUPER (poEditor).

        ASSIGN oControl = poEditor .
        
	END CONSTRUCTOR.

END CLASS.
