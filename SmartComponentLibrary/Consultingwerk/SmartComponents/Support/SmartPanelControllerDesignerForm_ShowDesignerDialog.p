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
    File        : SmartPanelControllerDesignerForm_ShowDesignerDialog.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Dec 30 18:23:08 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Design.* FROM ASSEMBLY .

DEFINE INPUT-OUTPUT PARAMETER pcNavigationTools AS CHARACTER NO-UNDO . 
DEFINE INPUT-OUTPUT PARAMETER pcTableIOTools    AS CHARACTER NO-UNDO . 

DEFINE VARIABLE cNavigationTools AS CHARACTER NO-UNDO . 
DEFINE VARIABLE cTableIOTools    AS CHARACTER NO-UNDO . 

DEFINE VARIABLE oDialogResult AS System.Windows.Forms.DialogResult NO-UNDO . 

/* ***************************  Main Block  *************************** */

ASSIGN cNavigationTools = pcNavigationTools
       cTableIOTools    = pcTableIOTools .

oDialogResult = SmartPanelControllerDesignerForm:ShowDesignerDialog (INPUT-OUTPUT cNavigationTools,
                                                                     INPUT-OUTPUT cTableIOTools) .
                                                                     
IF Progress.Util.EnumHelper:AreEqual (oDialogResult, System.Windows.Forms.DialogResult:Cancel) THEN 
    RETURN .
    
ASSIGN pcNavigationTools = cNavigationTools
       pcTableIOTools    = cTableIOTools .
                                                                         
                                                                     