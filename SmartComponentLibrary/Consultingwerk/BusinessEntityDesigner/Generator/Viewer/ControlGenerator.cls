/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ControlGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Nov 16 23:13:08 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.ControlGenerator 
    ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the reference to the SmartViewerGenerator used by the
                 IControlGenerator implementation
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ContainerControlGenerator AS &IF PROVERSION BEGINS "10.2":U &THEN Progress.Lang.Object &ELSE IContainerControlGenerator &ENDIF NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Inserts an event handler
        Notes:
        @param pcName The name of the event handler
        @param pcEvent The name of the event
        @param pcControlName The name of the contorl
        @param pcEventArgs The class name of the event args class
        @param pcCode The source code
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InsertEventHandler (pcName AS CHARACTER,
                                              pcEvent AS CHARACTER,
                                              pcControlName AS CHARACTER,
                                              pcEventArgs AS CHARACTER, 
                                              pcCode AS LONGCHAR):
        
        DEFINE  VARIABLE lcCode AS LONGCHAR NO-UNDO .
        
        ASSIGN lcCode = SUBSTITUTE ("    /*------------------------------------------------------------------------------~n":U +
                                    "        Purpose: Event handler or the &1 event of the &2~n":U +
                                    "        Notes:~n":U +
                                    "        @param sender The reference to the control that raised the event~n":U +
                                    "        @param e The &3 with the data for this event~n":U +
                                    "    ------------------------------------------------------------------------------*/~n":U +
                                    BusinessEntityGenerator:CaseKeywords ("    METHOD PRIVATE VOID &4 (sender AS System.Object, e AS &3):~n":U, "(sender ":U + CHR (1) + " System.Object, e":U) +
                                    "~n" +        
                                    "&5~n" +
                                    BusinessEntityGenerator:CaseKeywords ("    END METHOD.~n":U) +
                                    "~n",
                                    pcEvent,
                                    pcControlName,
                                    pcEventArgs,
                                    pcName,
                                    pcCode) .       

        CAST (THIS-OBJECT:ContainerControlGenerator, IContainerControlGenerator):CreateMethod (pcName, lcCode) .
        
    END METHOD .

END CLASS.
