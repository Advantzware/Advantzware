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
    File        : RepositionedRow
    Purpose     : Holder structure for the values set by the 
                  SET-REPOSITIONED-ROW method  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Feb 16 20:48:18 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH . 
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.Controls.RepositionedRow: 


    /*------------------------------------------------------------------------------
        Purpose: The reposition mode 
        Notes:   "ALWAYS"
                 Specifies that the REPOSITION TO ROWID statement always uses the 
                 indicated row number. If all of the records in a result set fit in 
                 the browse viewport, the REPOSITION statement only moves focus to 
                 the existing row.
                 "CONDITIONAL"
                 Specifies that the REPOSITION TO ROWID statement uses the indicated 
                 row number unless the new row is already in the browse viewport. In 
                 this case, the REPOSITION statement moves focus to the existing row.
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RepositionMode AS RepositionModeEnum NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: The row index  
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY RowIndex AS INTEGER NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RepositionedRow class
        Notes:   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RepositionedRow ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RepositionedRow class
        Notes:   
        @param piRowIndex The row index  
        @param poRepositionMode The character representation for of the reposition mode
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RepositionedRow (piRowIndex AS INTEGER,
                                        poRepositionMode AS RepositionModeEnum):
        
        SUPER ().
        
        ASSIGN THIS-OBJECT:RowIndex       = piRowIndex
               THIS-OBJECT:RepositionMode = poRepositionMode . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RepositionedRow class
        Notes:   
        @param piRowIndex The row index  
        @param pcRepositionMode The character representation for of the reposition mode
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RepositionedRow (piRowIndex AS INTEGER,
	                                    pcRepositionMode AS CHARACTER):
		
		DEFINE VARIABLE oMode AS RepositionModeEnum NO-UNDO . 
		
		SUPER ().
		
		CASE pcRepositionMode:
		    WHEN "ALWAYS":U      THEN oMode = RepositionModeEnum:Always .
		    WHEN "CONDITIONAL":U THEN oMode = RepositionModeEnum:Conditional .
		  
		    OTHERWISE 
		        UNDO, THROW NEW Consultingwerk.Exceptions.InvalidValueException (pcRepositionMode, "Reposition Mode":U) .
		
		END CASE .
		
		ASSIGN THIS-OBJECT:RowIndex       = piRowIndex
		       THIS-OBJECT:RepositionMode = oMode . 
		
	END CONSTRUCTOR.

END CLASS.
