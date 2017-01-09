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
    File        : ContainerControlGenerator
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Oct 28 00:44:22 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.BusinessEntityDesigner.*           FROM PROPATH .
USING Consultingwerk.BusinessEntityDesigner.Generator.* FROM PROPATH .
USING Progress.Lang.*                                   FROM PROPATH .

CLASS Consultingwerk.BusinessEntityDesigner.Generator.ContainerControlGenerator
    ABSTRACT:

    DEFINE TEMP-TABLE ttObjectNames NO-UNDO
        FIELD ObjectName AS CHARACTER
        INDEX ObjectName IS UNIQUE PRIMARY ObjectName.


    /*------------------------------------------------------------------------------
        Purpose: Returns the List of IControlGenerators used by this SmartViewerGenerator
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ControlGenerators AS ListIControlGenerator NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ContainerControlGenerator class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ContainerControlGenerator ():
        SUPER ().

        THIS-OBJECT:ControlGenerators = NEW ListIControlGenerator () .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Generates the Code block for the BeginInit or EndInit calls
        Notes:
        @param plInit Logical value indicating if the method should generate BeginInit or EndInit calls
        @return The code block with the BeginInit or EndInit for the Controls
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateControlInit (plInit AS LOGICAL):

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE cMethod     AS CHARACTER NO-UNDO INITIAL "BeginInit":U.

        IF plInit = FALSE THEN
            ASSIGN cMethod = "EndInit":U .

        {Consultingwerk/foreachABL.i IControlGenerator oControl in this-object:ControlGenerators}

            IF oControl:SupportInitialize THEN
                ASSIGN lcCodeBlock = lcCodeBlock + SUBSTITUTE (IF BusinessEntityDesignerSettings:KeywordCasing = "UPPER":U THEN
                                                                    "        CAST(THIS-OBJECT:&1, System.ComponentModel.ISupportInitialize):&2().~n":U
                                                               ELSE
                                                                    "        cast(this-object:&1, System.ComponentModel.ISupportInitialize):&2().~n":U,
                                                               oControl:Name,
                                                               cMethod) .
        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the code block NEW's the Controls
        Notes:
        @return The code block that NEW's the Controls
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateControlNews ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcCode      AS LONGCHAR NO-UNDO .

        {Consultingwerk/foreachABL.i IControlGenerator oControl in this-object:ControlGenerators}

            ASSIGN lcCode = oControl:GenerateControlNew () .

            IF lcCode = ? THEN
                UNDO, THROW NEW ControlGeneratorDidNotReturnCodeException (oControl:Name, oControl:GetClass():TypeName, "GenerateControlNew":U) .

            ASSIGN lcCodeBlock = lcCodeBlock + lcCode .

        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the Code block that assigns the Control properties
        Notes:
        @return The code block that assigns the Control properties
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateControlsPropertyAssign ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcCode      AS LONGCHAR NO-UNDO .

        {Consultingwerk/foreachABL.i IControlGenerator oControl in this-object:ControlGenerators}

            ASSIGN lcCode = oControl:GenerateAssignPropertiesBlock () .

            IF lcCode = ? THEN
                UNDO, THROW NEW ControlGeneratorDidNotReturnCodeException (oControl:Name, oControl:GetClass():TypeName, "GenerateAssignPropertiesBlock":U) .

            ASSIGN lcCodeBlock = lcCodeBlock + lcCode .

        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates the code block that defines and initializes the Control Support Variables
        Notes:
        @return The code block that defines and initialized the Control Support Variables
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateControlSupportVariables ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcCode      AS LONGCHAR NO-UNDO .

        {Consultingwerk/foreachABL.i IControlGenerator oControl in this-object:ControlGenerators}

            ASSIGN lcCode = oControl:GenerateSupportingObjectDefinition () .

            IF lcCode = ? THEN
                UNDO, THROW NEW ControlGeneratorDidNotReturnCodeException (oControl:Name, oControl:GetClass():TypeName, "GenerateSupportingObjectDefinition":U) .

            ASSIGN lcCodeBlock = lcCodeBlock + lcCode .

        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .


    /*------------------------------------------------------------------------------
        Purpose: Generates the code block that defines the Control Variables
        Notes:
        @return The code block that defines the Control Variables
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LONGCHAR GenerateDefineControlVariables ():

        DEFINE VARIABLE lcCodeBlock AS LONGCHAR NO-UNDO .
        DEFINE VARIABLE lcCode      AS LONGCHAR NO-UNDO .

        {Consultingwerk/foreachABL.i IControlGenerator oControl in this-object:ControlGenerators}

            ASSIGN lcCode = oControl:GenerateVariableDefinition () .

            IF lcCode = ? THEN
                UNDO, THROW NEW ControlGeneratorDidNotReturnCodeException (oControl:Name, oControl:GetClass():TypeName, "GenerateVariableDefinition":U) .

            ASSIGN lcCodeBlock = lcCodeBlock + lcCode .

        END.

        RETURN TRIM (lcCodeBlock, "~n":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Generates a temporary object name (e.g. appearance1) based on a
                 provided name (e.g. appearance)
        Notes:
        @param pcObjectName The name to be used for a temporary object
        @return The name to be used for a temporary object
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER GenerateObjectName (pcObjectName AS CHARACTER):

        DEFINE VARIABLE iCounter AS INTEGER   NO-UNDO INITIAL 0 .
        DEFINE VARIABLE cName    AS CHARACTER NO-UNDO.

        REPEAT:
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
