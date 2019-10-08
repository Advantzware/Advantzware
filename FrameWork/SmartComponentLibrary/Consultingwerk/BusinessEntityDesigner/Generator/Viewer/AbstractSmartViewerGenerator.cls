/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : AbstractSmartViewerGenerator
    Purpose     : Abstract base class for ISmartViewerGenertor implementations
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 04 06:54:05 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.Generator.Viewer.* FROM PROPATH .
USING Progress.Lang.*                                          FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.Viewer.AbstractSmartViewerGenerator
    IMPLEMENTS ISmartViewerGenerator ABSTRACT:

    DEFINE PROTECTED TEMP-TABLE ttMethods NO-UNDO
        FIELD Name AS CHARACTER
        FIELD Code AS CLOB
        INDEX Name Name .

    DEFINE PROTECTED TEMP-TABLE ttObjectNames NO-UNDO
        FIELD ObjectName AS CHARACTER
        INDEX ObjectName IS UNIQUE PRIMARY ObjectName.

    /**
     * Purpose: Returns the List of Columns in the BindingSource Schema
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BindingSourceColumns AS "System.Collections.Generic.List<Progress.Data.ColumnPropDesc>" NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Gets and sets the Class Name for the generated SmartViewerControl
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ClassName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the List of IControlGenerators used by this SmartViewerGenerator
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ControlGenerators AS Consultingwerk.BusinessEntityDesigner.Generator.ListIControlGenerator NO-UNDO
    GET.
    PROTECTED SET.

    /**
     * Purpose: Gets and sets if the generated User Control should contain a
                Binding Source or not
     * Notes:
     */
    DEFINE PUBLIC PROPERTY CreateBindingSource AS LOGICAL NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the EntityJoin property of the SmartBusinessEntityBindingSource
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityJoin AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the EntityName property of the SmartBusinessEntityBindingSource
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the EntityTable property of the SmartBusinessEntityBindingSource
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the EntityView property of the SmartBusinessEntityBindingSource
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the Class Name for the generated SmartViewerControl
     * Notes:
     */
    DEFINE PUBLIC PROPERTY PackageName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Creates a method in the container control
     * Notes:
     * @param pcMethodName The name of the method to create
     * @param pcCode The source code of the method
     */
    METHOD PUBLIC VOID CreateMethod (INPUT pcMethodName AS CHARACTER,
                               INPUT pcCode AS LONGCHAR):

        CREATE ttMethods .
        ASSIGN ttMethods.Name = pcMethodName .

        COPY-LOB FROM pcCode TO ttMethods.Code .

    END METHOD .

    /**
     * Purpose: Generates the source code for the SmartViewerControl
     * Notes:
     * @return The file name of the generated source
     */
    METHOD PUBLIC ABSTRACT CHARACTER Generate () .

    /**
     * Purpose: Generates a temporary object name (e.g. appearance1) based on a
     *          provided name (e.g. appearance)
     * Notes:
     * @param pcObjectName The name to be used for a temporary object
     * @return The name to be used for a temporary object
     */
    METHOD PUBLIC CHARACTER GenerateObjectName (INPUT pcObjectName AS CHARACTER):

        DEFINE VARIABLE iCounter AS INTEGER   NO-UNDO INITIAL 0 .
        DEFINE VARIABLE cName    AS CHARACTER NO-UNDO.

        REPEAT ON ERROR UNDO, THROW:
            ASSIGN iCounter = iCounter + 1
                   cName    = SUBSTITUTE ("&1&2":U,
                                          pcObjectName,
                                          iCounter).

            IF NOT CAN-FIND (ttObjectNames WHERE ttObjectNames.ObjectName = cName) THEN DO:
                CREATE ttObjectNames .
                ASSIGN ttObjectNames.ObjectName = cName .
                RELEASE ttObjectNames .

                RETURN cName .
            END.
        END .

    END METHOD .

END CLASS.
