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
    File        : ClassCache
    Purpose     : Provides cache functionality for classes derived from 
                  a given base type, typically during design time 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 22 20:59:37 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*               FROM PROPATH.
USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .  
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Support.ClassCache 
    IMPLEMENTS IClassCache: 

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    DEFINE VARIABLE lCached AS LOGICAL NO-UNDO INIT FALSE .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the ClassName this class is caching 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the class names temp-table
        Notes:   
        @param ttClassNames The temp-table with the class names 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetClassNames (OUTPUT TABLE FOR ttClassNames):
        
        IF NOT lCached THEN 
            THIS-OBJECT:Refresh () .
             
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the Class Names Cache 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Refresh ():

        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i THIS-OBJECT:ClassName "'ClassName':U"} .
        
        ASSIGN lCached = TRUE . 
        
        Consultingwerk.Util.ClassHelper:GetClassNamesInClassPath (THIS-OBJECT:ClassName, 
                                                                  FALSE, 
                                                                  OUTPUT TABLE ttClassNames BY-REFERENCE) .

    END METHOD .
    
END CLASS.
