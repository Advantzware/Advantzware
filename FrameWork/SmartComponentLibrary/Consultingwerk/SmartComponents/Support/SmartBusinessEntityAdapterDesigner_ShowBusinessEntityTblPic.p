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
    File        : SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTblPic.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 14 23:07:28 CEST 2010
    Notes       : Renamed from SmartBusinessEntityAdapterDesigner_ShowBusinessEntityTablePicker.p
                  as the total file name (including DOT and extension was
                  > 64 characters and that seems to have blown the Progress
                  xcode utility on UNIX (reported by RAW). 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW .

USING Consultingwerk.SmartComponents.Design.* FROM ASSEMBLY . 
USING Consultingwerk.Util.* FROM PROPATH .

DEFINE INPUT        PARAMETER phDataset    AS HANDLE    NO-UNDO .  
DEFINE INPUT        PARAMETER pcTableList  AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER pcTables     AS CHARACTER NO-UNDO .
DEFINE INPUT-OUTPUT PARAMETER pcEntityJoin AS CHARACTER NO-UNDO.

DEFINE VARIABLE oTreeView AS System.Windows.Forms.TreeView     NO-UNDO . 
DEFINE VARIABLE oNodes    AS "System.Windows.Forms.TreeNode[]" NO-UNDO .

/* ***************************  Main Block  *************************** */

oTreeView = NEW System.Windows.Forms.TreeView () . 

Consultingwerk.SmartComponents.Support.DatasetRelationNodeParser:FillTreeViewWithTableNames 
        (phDataset, 
         oTreeView:Nodes,
         pcTableList) .

oNodes = {Consultingwerk/new-array.i System.Windows.Forms.TreeNode oTreeView:Nodes:Count} .

oTreeView:Nodes:CopyTo (oNodes, 0) .

pcTables = SmartBusinessEntityAdapterDesigner:ShowBusinessEntityTablePicker (oNodes, pcTableList, INPUT-OUTPUT pcEntityJoin) .
