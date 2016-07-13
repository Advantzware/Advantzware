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
    File        : ValueBasedAppearanceFactory
    Purpose     : Helper class to dynamically build value based appearances
                  for UltraGrid Columns 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Feb 13 13:00:23 CET 2014
    Notes       : See http://confluence.consultingwerkcloud.com/wiki/display/SCL/Value+based+formatting+in+the+SmartDataBrowser+using+the+ValueBasedAppearanceFactory
                  for more information.
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Infragistics.Win.*                FROM ASSEMBLY . 
USING Infragistics.Win.UltraWinGrid.*   FROM ASSEMBLY . 
USING Progress.Lang.*                   FROM PROPATH .

CLASS Consultingwerk.Windows.Util.ValueBasedAppearanceFactory: 

    DEFINE VARIABLE oConditions  AS "System.Collections.Generic.List<ICondition>" NO-UNDO .
    DEFINE VARIABLE oAppearances AS "System.Collections.Generic.List<Appearance>" NO-UNDO .
    
    DEFINE VARIABLE oSourceColumn AS UltraGridColumn NO-UNDO . 

    /*------------------------------------------------------------------------------
       Purpose: Returns the Reference to the UltraGrid
       Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Grid AS UltraGrid NO-UNDO 
	GET.
	PRIVATE SET. 

    /*------------------------------------------------------------------------------
       Purpose: Returns the Source Column as the condition provider
       Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY SourceColumn AS CHARACTER NO-UNDO 
	GET.
	PRIVATE SET. 

	/*------------------------------------------------------------------------------
	   Purpose: Constructror for the ValueBasedAppearanceFactory
	   Notes:
	   @param poGrid The reference to the UltraGrid that owns the source column
	   @param pcSourceColumn The source column as the Condition Provider 
	------------------------------------------------------------------------------*/
	CONSTRUCTOR PUBLIC ValueBasedAppearanceFactory (poGrid AS UltraGrid,
	                                                pcSourceColumn AS CHARACTER):
		SUPER ().
		
		ASSIGN THIS-OBJECT:Grid         = poGrid 
		       THIS-OBJECT:SourceColumn = pcSourceColumn
		       oSourceColumn            = poGrid:DisplayLayout:Bands[0]:Columns[pcSourceColumn]
		       oConditions              = NEW "System.Collections.Generic.List<ICondition>" () 
               oAppearances             = NEW "System.Collections.Generic.List<Appearance>" () 
               .
		
		Consultingwerk.Assertion.ObjectAssert:IsValid (oSourceColumn, "Source Column":U) .
		
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	   Purpose: Adds an condition and assocated appearance to the ValueBasedAppearance
	            being created
	   Notes:   The Appearance instance is returned so that you can assign appearance attributes  
	   @param pcCondition The condition to add an appearance for
	   @return The Infragistics.Win.Appearance instance for the condition
	------------------------------------------------------------------------------*/
	METHOD PUBLIC Infragistics.Win.Appearance AddAppearanceForCondition (pcCondition AS CHARACTER):
		
		DEFINE VARIABLE oAppearance AS Appearance NO-UNDO.

        oConditions:Add (CAST (NEW FormulaCondition (oSourceColumn, pcCondition),
                               Infragistics.Win.ICondition)) .

        oAppearance = NEW Appearance () .
        oAppearances:Add (oAppearance) .

		RETURN oAppearance.

	END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Adds an condition and assigns assocated appearance to the ValueBasedAppearance
                being created
       Notes:   The Appearance instance is returned so that you can assign appearance attributes
       @param pcCondition The condition to add an appearance for
       @param poAppearance The appearance to assign to the condition
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID AddAppearanceForCondition (pcCondition AS CHARACTER, 
                                                  poAppearance as Infragistics.Win.Appearance):

        oConditions:Add (CAST (NEW FormulaCondition (oSourceColumn, pcCondition),
                               Infragistics.Win.ICondition)) .

        oAppearances:Add (poAppearance) .

    END METHOD.

	/*------------------------------------------------------------------------------
	    Purpose: Applies the ValueBasedAppearance to all Columns of the Grid.
	    Notes:
	------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ApplyToAllColumns ():
		
		DEFINE VARIABLE oConditionValueAppearance AS ConditionValueAppearance NO-UNDO . 
		
		oConditionValueAppearance = THIS-OBJECT:GetValueBasedAppearance () .  

        {Consultingwerk/foreach.i UltraGridColumn oColumn in THIS-OBJECT:Grid:DisplayLayout:Bands[0]:Columns}

            oColumn:ValueBasedAppearance = oConditionValueAppearance .

        END.

	END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Applies the ValueBasedAppearance to the source Columns of the Grid
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID ApplyToSourceColumn ():
		
        DEFINE VARIABLE oConditionValueAppearance AS ConditionValueAppearance NO-UNDO . 
        
        oConditionValueAppearance = THIS-OBJECT:GetValueBasedAppearance () .  

        THIS-OBJECT:Grid:DisplayLayout:Bands[0]:Columns[THIS-OBJECT:SourceColumn]:ValueBasedAppearance = oConditionValueAppearance .

	END METHOD .

	/*------------------------------------------------------------------------------
	    Purpose: Creaes and returns the ConditionValueAppearance that consists of the 
	             conditions and appearances added to the factory class  
	    Notes:   The ConditionValueAppearance  can be assigned to the ValueBasedApprearance 
	             of an UltraGrid Column
	    @return The reference to the ConditionValueAppearance instance 
	------------------------------------------------------------------------------*/
	METHOD PUBLIC ConditionValueAppearance GetValueBasedAppearance ():
		
		RETURN NEW ConditionValueAppearance (oConditions:ToArray(),
		                                     oAppearances:ToArray()) .
		
	END METHOD.

END CLASS.
