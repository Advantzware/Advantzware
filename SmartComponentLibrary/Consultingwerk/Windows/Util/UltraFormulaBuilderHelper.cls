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
    File        : UltraFormulaBuilderHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 12 22:05:53 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.


USING Consultingwerk.Windows.Util.*                         FROM PROPATH . 
USING Infragistics.Win.CalcEngine.*                         FROM ASSEMBLY .  
USING Infragistics.Win.UltraWinCalcManager.FormulaBuilder.* FROM ASSEMBLY . 
USING Progress.Lang.*                                       FROM PROPATH .
USING System.Windows.Forms.*                                FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.Util.UltraFormulaBuilderHelper: 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the UltraFormulaBuilderHelper class
        Notes:   Disallow instance creation   
    ------------------------------------------------------------------------------*/
	CONSTRUCTOR PRIVATE UltraFormulaBuilderHelper ():
		SUPER ().
		
	END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Shows the FormulaBuilderDialog
        Notes:   
        @param oFormulaProvider The FormulaProvider to show the Dialog Form
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC VOID ShowFormulaBuilderDialog (oFormulaProvider AS IFormulaProvider):
		
		DEFINE VARIABLE oDialog       AS FormulaBuilderDialog NO-UNDO . 
		DEFINE VARIABLE oDialogResult AS DialogResult         NO-UNDO .
		
		oDialog = NEW FormulaBuilderDialog (oFormulaProvider) .

        WAIT-FOR oDialog:ShowDialog () SET oDialogResult . 
		
		{Consultingwerk/check-dialogresult-ok.i oDialogResult} .

        ASSIGN oFormulaProvider:Formula = oDialog:Formula . 

	END METHOD .

END CLASS.
