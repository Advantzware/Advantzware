/**********************************************************************
 * Copyright (C) 2006-2012 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ClassCacheProvider
    Purpose     : Provides caching functionality for the list of physical
                  classes implementing a given base type
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Sep 22 21:14:14 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Support.* FROM PROPATH .
USING Progress.Lang.*                          FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Support.ClassCacheProvider IMPLEMENTS IClassCacheProvider:

    {Consultingwerk/Util/TempTables/ttClassNames.i}

    DEFINE VARIABLE oCaches AS DictionaryIClassCache NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ClassCacheProvider class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ClassCacheProvider ():
        SUPER ().

        ASSIGN oCaches = NEW DictionaryIClassCache () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Class names Cache instance for the given base class
        Notes:
        @param pcClassName The base type (class or interface) that the returned classes need to match
        @return The Class names cache instance for the given base type
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED IClassCache GetCache (pcClassName AS CHARACTER):

        DEFINE VARIABLE oCache AS IClassCache NO-UNDO.

        IF NOT oCaches:ContainsKey (pcClassName) THEN DO:

            oCache = oCaches:Add (pcClassName,
                                  NEW ClassCache ()) .

            oCache:ClassName = pcClassName .
        END.

        RETURN oCaches:GetItem (pcClassName) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of Class Names for the given base class
        Notes:
        @param pcClassName The base type (class or interface) that the returned classes need to match
        @param ttClassNames OUTPUT Temp-Table with the list of matching classnames
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID GetClassNames (pcClassName AS CHARACTER,
                                      OUTPUT TABLE ttClassNames):

        THIS-OBJECT:GetCache (pcClassName):GetClassNames (OUTPUT TABLE ttClassNames) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Refreshes the Cache for the given base type
        Notes:
        @param pcClassName The base type (class or interface) that the returned classes need to match
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Refresh (pcClassName AS CHARACTER):

        THIS-OBJECT:GetCache (pcClassName):Refresh () .

    END METHOD .

END CLASS.
