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
    File        : RenderedBrowseMultiSelection
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Nov 20 14:59:26 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.WindowIntegrationKit.Controls.* FROM PROPATH .
USING Infragistics.Win.UltraWinGrid.*                FROM ASSEMBLY .   
USING Progress.Lang.*                                FROM PROPATH .

CLASS Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseMultiSelection: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Grid AS RenderedBrowseControl NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the number of selected rows
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY NUM-SELECTED-ROWS AS INTEGER NO-UNDO 
	GET:
        RETURN THIS-OBJECT:Grid:Selected:Rows:Count . 		
	END GET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the RenderedBrowseMultiSelection class
        Notes:   
        @param poGrid The RenderdBrowseControl that should receive multiple selection
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC RenderedBrowseMultiSelection (poGrid AS RenderedBrowseControl):
		SUPER ().
		
		THIS-OBJECT:Grid = poGrid . 
		
		poGrid:DisplayLayout:Override:SelectTypeRow = SelectType:Extended .
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Fetches the piRow'th selected row in a browse and puts the row into 
                 the database buffer. In other words, this method specifies one row 
                 from the one-based index into all currently selected rows and puts 
                 that row into the record buffer. 
        Notes:   
        @param piRow An integer expression that specifies a selected row within the browse.
        @return Logical value indicating if the selected action was successful
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC LOGICAL FETCH-SELECTED-ROW (piRow AS INTEGER):
		
		IF piRow > THIS-OBJECT:NUM-SELECTED-ROWS THEN 
		    RETURN FALSE . 
		    
		THIS-OBJECT:Grid:ActiveRow = THIS-OBJECT:Grid:Selected:Rows [piRow - 1] .
		
		RETURN TRUE . 
		
		CATCH err AS Progress.Lang.Error :
        	RETURN FALSE . 	
        END CATCH.    

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Selects all Rows in the Grid 
        Notes:   http://devcenter.infragistics.com/Support/KnowledgeBaseArticle.aspx?ArticleID=6546
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SelectAll ():
		
		THIS-OBJECT:Grid:Selected:Rows:AddRange (THIS-OBJECT:Grid:Rows:GetAllNonGroupByRows()) . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deselects all Rows in the Grid 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID SelectNone ():
		
		THIS-OBJECT:Grid:Selected:Rows:Clear () .

	END METHOD .

END CLASS.
