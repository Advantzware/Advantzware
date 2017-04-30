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
    File        : Annotation
    Purpose     : Value class for the result of the AnnotationParser
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jan 27 22:32:16 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING com.joanju.proparse.NodeTypes          FROM ASSEMBLY .
&ENDIF
USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH . .
USING Consultingwerk.Studio.*                FROM PROPATH .
USING Consultingwerk.Studio.Proparse.*       FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING org.prorefactor.core.JPNode            FROM ASSEMBLY .
&ENDIF
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Studio.Annotation:

    /*------------------------------------------------------------------------------
        Purpose: Returns the Column number containing the Annotation
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Column AS INTEGER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the FileName containing the Annotation
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY FileName AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Line number containing the Annotation
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Line AS INTEGER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Name of the Annotation
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the JPNode of the Annotation
        Notes:   This property is of type Progress.Lang.Object and not JPNode to
                 allow reuse on platforms where Proparse is not accessible (UNIX)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Node AS Progress.Lang.Object NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Annotation Parameters
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Parameters AS ListNameValuePair NO-UNDO
    GET.
    PROTECTED SET.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Annotation class
        Notes:
        @param poNode The reference to the JPNode for the annotation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Annotation (poNode AS JPNode):

        THIS-OBJECT (poNode:getFileName (),
                     poNode:getLine (),
                     poNode:getColumn (),
                     poNode).

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Annotation class
        Notes:
        @param pcFileName The file name containing the annotation
        @param piLine The line number of the Annotation
        @param piColumn The column number of the Annotation
        @param poNode The reference to the JPNode for the annotation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Annotation (pcFileName AS CHARACTER,
                                   piLine AS INTEGER,
                                   piColumn AS INTEGER,
                                   poNode AS JPNode):
        SUPER ().

        Consultingwerk.Assertion.ObjectAssert:IsValid (poNode, "poNode":U) .

        THIS-OBJECT:Parameters = NEW ListNameValuePair () .

        ASSIGN THIS-OBJECT:FileName = pcFileName
               THIS-OBJECT:Line     = piLine
               THIS-OBJECT:Column   = piColumn
               THIS-OBJECT:Node     = poNode
               THIS-OBJECT:Name     = THIS-OBJECT:GetAnnotationName (poNode) .

        IF ProparseHelper:HasChildNodeOfNodeType(poNode, "LEFTPAREN":U) THEN
            THIS-OBJECT:ParseAnnotationParameters (poNode).

    END CONSTRUCTOR.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Annotation class
        Notes:
        @param pcFileName The file name containing the annotation
        @param pcName The name of the Annotation (including @)
        @param piLine The line number of the Annotation
        @param piColumn The column number of the Annotation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Annotation (pcFileName AS CHARACTER,
                                   pcName AS CHARACTER,
                                   piLine AS INTEGER,
                                   piColumn AS INTEGER):

        THIS-OBJECT (pcFileName,
                     pcName,
                     "":U,
                     piLine,
                     piColumn) .

    END CONSTRUCTOR .

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Annotation class
        Notes:
        @param pcFileName The file name containing the annotation
        @param pcName The name of the Annotation (including @)
        @param pcParameter The parameter list of the annotation (typically in parenthesis)
        @param piLine The line number of the Annotation
        @param piColumn The column number of the Annotation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Annotation (pcFileName AS CHARACTER,
                                   pcName AS CHARACTER,
                                   pcParameter AS CHARACTER,
                                   piLine AS INTEGER,
                                   piColumn AS INTEGER):
        SUPER ().


        THIS-OBJECT:Parameters = NEW ListNameValuePair () .

        IF NOT pcName BEGINS "@":U THEN
            ASSIGN pcName = SUBSTITUTE ("@&1":U, TRIM (pcName)) .

        ASSIGN THIS-OBJECT:FileName = pcFileName
               THIS-OBJECT:Line     = piLine
               THIS-OBJECT:Column   = piColumn
               THIS-OBJECT:Name     = pcName  .

        IF pcParameter > "":U THEN
            THIS-OBJECT:ParseAnnotationParameters (pcParameter).

    END CONSTRUCTOR.

    /**
     * Purpose: Returns the Annotation Name
     * Notes:   Returns the JPNode's text and concatenates any occurences of ID namedot/id nodes
     * @param poNode The JPNode to return the annotation's name from
     * @return The Annotation Name
     */
&IF DEFINED (DotNetAccessible) NE 0 &THEN
    METHOD PROTECTED CHARACTER GetAnnotationName (poNode AS JPNode):

        DEFINE VARIABLE cName AS CHARACTER NO-UNDO.

        ASSIGN cName  = poNode:getText ()
               poNode = poNode:firstChild ().

        namedotloop:
        DO WHILE VALID-OBJECT (poNode) AND NodeTypes:getTypeName (poNode:getType()) = "NAMEDOT":U
            ON ERROR UNDO, THROW:

            poNode = poNode:nextSibling () .

            IF VALID-OBJECT (poNode) AND NodeTypes:getTypeName (poNode:getType()) <> "LEFTPAREN":U
                                     AND NodeTypes:getTypeName (poNode:getType()) <> "PERIOD":U THEN
                ASSIGN cName = cName + ".":U + poNode:getText () .
            ELSE
                LEAVE namedotloop .

            poNode = poNode:nextSibling ().
        END.

        RETURN cName .

    END METHOD.
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Parses the List of Annotation Parameters
        Notes:   The list of parameters may be including or excluding the surrounding
                 paranthesis
        @param pcParameters The list of annotation parameters as a characer string
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseAnnotationParameters (pcParameters AS CHARACTER):

        DEFINE VARIABLE cName   AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cValue  AS CHARACTER        NO-UNDO .

        DEFINE VARIABLE oTokens    AS CharacterList NO-UNDO .
        DEFINE VARIABLE oTokenizer AS Tokenizer     NO-UNDO .

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        ASSIGN pcParameters = TRIM (pcParameters) .

        /* Remove paranthesis if present */
        IF pcParameters BEGINS "(":U THEN
            ASSIGN pcParameters = SUBSTRING (pcParameters, 2, -1, "CHARACTER":U) .

        IF pcParameters MATCHES "*)":U THEN
            ASSIGN pcParameters = SUBSTRING (pcParameters, 1, LENGTH (pcParameters, "CHARACTER":U) - 1, "CHARACTER":U) .

        oTokenizer = NEW Tokenizer () .
        oTokens = oTokenizer:Tokenize (pcParameters, FALSE, ",=":U) .

        DO i = 1 TO oTokens:Count - 1 BY 2:

            ASSIGN cName  = oTokens:GetValue (i)
                   cValue = oTokens:GetValue (i + 1) .

            THIS-OBJECT:Parameters:Add (cName, cValue) .
        END.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oTokenizer) .
            GarbageCollectorHelper:DeleteObject (oTokens) .
        END FINALLY.

    END METHOD .

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /*------------------------------------------------------------------------------
        Purpose: Parses the List of Annotation Parameters
        Notes:
        @param poNode The JPNode reference of the Annotation
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ParseAnnotationParameters (poNode AS JPNode):

        DEFINE VARIABLE oNode  AS JPNode    NO-UNDO .
        DEFINE VARIABLE cName  AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO .

        oNode = ProparseHelper:FindChildNodeOfNodeType(poNode, "LEFTPAREN":U) .

        DO WHILE VALID-OBJECT (oNode):

            IF LOOKUP (NodeTypes:getTypeName(oNode:getType()), "ID,FIELD,VALUE,NAME,FIELDS":U) > 0 THEN DO:
                ASSIGN cName = oNode:getText () .

                oNode = oNode:nextSibling () .

                IF VALID-OBJECT (oNode) AND NodeTypes:getTypeName(oNode:getType()) = "EQUAL":U THEN DO:

                    oNode = oNode:nextSibling () .

                    IF VALID-OBJECT (oNode) THEN DO:
                        ASSIGN cValue = StringHelper:Unquote (oNode:getText ()) .

                        THIS-OBJECT:Parameters:Add (cName, cValue) .
                    END.
                END.
            END.

            IF VALID-OBJECT (oNode) THEN
                oNode = oNode:nextSibling () .
        END.

    END METHOD .
&ENDIF

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the Annotation class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC Annotation ():

    END DESTRUCTOR .

END CLASS.
