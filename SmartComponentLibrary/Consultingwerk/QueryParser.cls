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
    File        : QueryParser
    Purpose     : Allow parsing of an ABL Query String for a single buffer into
                  an IQueryExpression (QueryExpression, ListQueryExpression, ...)
    Syntax      :
    Description : Default IQueryParser implementation, SCL-501
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 25 09:52:24 CEST 2014
    Notes       : Known limitations - currently not supported:
                  - Array field names are only supported when there is no white space character between the field name and the []
                  - Expressions other than constant values as the value of a query term
                  - AND needs to be following a SPACE, i.o.W. cannot parse (term)AND(term), need to be given as (term) AND (term)
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*                       FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.Exceptions.*            FROM PROPATH .
USING Consultingwerk.Framework.Collections.* FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .

&SCOPED-DEFINE ExceptionTitle "Error parsing Query String"{&TRAN}

CLASS Consultingwerk.QueryParser IMPLEMENTS IQueryParser:

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Buffer to be appended to unqualified
                 field references
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the ByClause of the previous parsing operation
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ByClause AS CHARACTER NO-UNDO
    GET.
    PROTECTED SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the Enum member with the given name
        Notes:   Supports the Enum member names as well as =, <>, >=, >, <= and <
        @param pcName The Name of the Enum member
        @return The Enum member with the given name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC OperatorEnum OperatorFromString (pcName AS CHARACTER):

        CASE pcName:
            WHEN "EQ":U       OR WHEN "=":U  THEN
                RETURN OperatorEnum:EQ .
            WHEN "NE":U       OR WHEN "<>":U THEN
                RETURN OperatorEnum:NE .
            WHEN "Begins":U                  THEN
                RETURN OperatorEnum:Begins .
            WHEN "Contains":U                THEN
                RETURN OperatorEnum:Contains .
            WHEN "Matches":U                 THEN
                RETURN OperatorEnum:Matches .
            WHEN "GE":U       OR WHEN ">=":U THEN
                RETURN OperatorEnum:GE .
            WHEN "GT":U       OR WHEN ">":U  THEN
                RETURN OperatorEnum:GT .
            WHEN "LE":U       OR WHEN "<=":U THEN
                RETURN OperatorEnum:LE .
            WHEN "LT":U       OR WHEN "<":U  THEN
                RETURN OperatorEnum:LT .

            WHEN "AND":U                     THEN
                RETURN OperatorEnum:And .
            WHEN "OR":U                      THEN
                RETURN OperatorEnum:Or .

        END CASE .

        UNDO, THROW NEW InvalidValueException (pcName, "Consultingwerk.OperatorEnum":U) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a DATE(month, day, year) function call into a DATE value
        Notes:
        @param pcString The CHARACTER Value representing the DATE() function
        @return The DATE value represented by the DATE() function
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED DATE ParseDateFunction (pcString AS CHARACTER):

        ASSIGN pcString = TRIM (TRIM (REPLACE (pcString, "DATE":U, "":U), ")":U), "(":U)  .

        RETURN DATE (INTEGER (TRIM (ENTRY (1, pcString))),
                     INTEGER (TRIM (ENTRY (2, pcString))),
                     INTEGER (TRIM (ENTRY (3, pcString)))).
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses a DATETIME(month, day, year) function call into a DATE value
        Notes:
        @param pcString The CHARACTER Value representing the DATETIME() function
        @return The DATETIME value represented by the DATETIME() function
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED DATETIME ParseDateTimeFunction (pcString AS CHARACTER):

        ASSIGN pcString = TRIM (TRIM (REPLACE (pcString, "DATETIME":U, "":U), ")":U), "(":U)  .

        RETURN DATETIME (INTEGER (TRIM (ENTRY (1, pcString))),
                         INTEGER (TRIM (ENTRY (2, pcString))),
                         INTEGER (TRIM (ENTRY (3, pcString))),
                         INTEGER (TRIM (ENTRY (4, pcString))),
                         INTEGER (TRIM (ENTRY (5, pcString))),
                         INTEGER (TRIM (ENTRY (6, pcString))),
                         INTEGER (TRIM (ENTRY (7, pcString)))).
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses a DATETIME-TZ(month, day, year) function call into a DATE value
        Notes:
        @param pcString The CHARACTER Value representing the DATETIME-TZ() function
        @return The DATETIME-TZ value represented by the DATETIME-TZ() function
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED DATETIME-TZ ParseDateTimeTzFunction (pcString AS CHARACTER):

        ASSIGN pcString = TRIM (TRIM (REPLACE (pcString, "DATETIME-TZ":U, "":U), ")":U), "(":U)  .

        RETURN DATETIME-TZ (INTEGER (TRIM (ENTRY (1, pcString))),
                            INTEGER (TRIM (ENTRY (2, pcString))),
                            INTEGER (TRIM (ENTRY (3, pcString))),
                            INTEGER (TRIM (ENTRY (4, pcString))),
                            INTEGER (TRIM (ENTRY (5, pcString))),
                            INTEGER (TRIM (ENTRY (6, pcString))),
                            INTEGER (TRIM (ENTRY (7, pcString))),
                            INTEGER (TRIM (ENTRY (8, pcString)))).
    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Parses a QueryOperator from a character value
        Notes:
        Throws:  Consultingwerk.Exceptions.InvalidQueryOperatorException
        @param pcOperatorValue The value for parse, e.g. "=", "EQ", ">=", etc.
        @return The resulting Query Operator
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OperatorEnum ParseQueryTermOperator (pcOperatorValue AS CHARACTER):

        DEFINE VARIABLE oValue AS OperatorEnum                  NO-UNDO .
        DEFINE VARIABLE ex     AS InvalidQueryOperatorException NO-UNDO .

        oValue = QueryParser:OperatorFromString (pcOperatorValue) .

        IF oValue = OperatorEnum:And OR oValue = OperatorEnum:Or THEN

            UNDO, THROW NEW InvalidQueryOperatorException (SUBSTITUTE ("&1 not supported as operator in query term."{&TRAN}, pcOperatorValue),
                                                           0,
                                                           {&ExceptionTitle},
                                                           pcOperatorValue) .

        RETURN oValue .

        CATCH err AS Progress.Lang.Error:
            ex = NEW InvalidQueryOperatorException (SUBSTITUTE ("Unrecognized query operator: &1"{&TRAN}, pcOperatorValue),
                                                    0,
                                                    {&ExceptionTitle},
                                                    pcOperatorValue) .

            IF err:NumMessages > 0 THEN
                ex:AddMessage (err:GetMessage (1), err:GetMessageNum (1)) .

            UNDO, THROW ex .

        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses an ABL Query String for a single buffer info an IQueryExpression
        Notes:   The Consultingwerk.QueryExpression class contains static methods to help
                 testing if the result of the method ParseQueryString contains expressions
                 for a given buffer field name
                 - LOGICAL HasExpressionForField (pcFieldName, poExpression)
                 - QueryExpression GetExpressionForField (pcFieldName, poExpression)
        @param pcQueryString The ABL QueryString to parse
        @return The IQueryExpression instance representing the query string
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC IQueryExpression ParseQueryString (pcQueryString AS CHARACTER):

        DEFINE VARIABLE oList       AS ListQueryExpression NO-UNDO .
        DEFINE VARIABLE oReturn     AS ListQueryExpression NO-UNDO .
        DEFINE VARIABLE oOr         AS ListQueryExpression NO-UNDO .
        DEFINE VARIABLE oExpression AS IQueryExpression    NO-UNDO .
        DEFINE VARIABLE iPos        AS INTEGER             NO-UNDO INITIAL 1.
        DEFINE VARIABLE iStart      AS INTEGER             NO-UNDO .
        DEFINE VARIABLE iClosing    AS INTEGER             NO-UNDO .
        DEFINE VARIABLE cChar       AS CHARACTER           NO-UNDO .

        /* Characters that we need to locate while parsing the string */
        DEFINE VARIABLE cDelimiters AS CHARACTER NO-UNDO INITIAL "(,~",', AND , BY , OR ":U.

        ASSIGN pcQueryString        = TRIM (pcQueryString)
               oList                = NEW ListQueryExpression ()
               THIS-OBJECT:ByClause = "":U .

        IF pcQueryString BEGINS "FOR ":U OR pcQueryString BEGINS "PRESELECT ":U THEN
            ASSIGN pcQueryString = THIS-OBJECT:TrimForOrPreselect (pcQueryString) .

        IF pcQueryString BEGINS "WHERE ":U THEN
            ASSIGN pcQueryString = THIS-OBJECT:TrimWhere (pcQueryString) .

        IF pcQueryString = "":U THEN
            RETURN oList .

        IF pcQueryString BEGINS "BY ":U THEN DO:
            ASSIGN THIS-OBJECT:ByClause = pcQueryString .
            RETURN oList .
        END.

        oReturn = oList .

        iPosLoop:
        DO WHILE iPos > 0:
            ASSIGN iStart = iPos .

            ASSIGN iPos = StringHelper:FirstOccurenceOf (pcQueryString, cDelimiters, iPos) .

            IF iPos > 0 THEN DO:
                ASSIGN cChar = SUBSTRING (pcQueryString, iPos, 1, "CHARACTER":U) .

                IF cChar = "'":U OR cChar = "~"":U THEN DO:
                    ASSIGN iPos = StringHelper:FindClosingQuote (pcQueryString, iPos) + 1.

                    IF iPos > LENGTH (pcQueryString, "CHARACTER":U) THEN
                        ASSIGN iPos = 0 .
                    ELSE
                        ASSIGN iPos = StringHelper:FirstOccurenceOf (pcQueryString, cDelimiters, iPos) .
                END.
                /* Special handling for DATE function */
                ELSE IF cChar = "(":U AND iPos > 4 AND SUBSTRING (pcQueryString, iPos - 4, 4, "CHARACTER":U) = "DATE":U THEN DO:
                    iPos = StringHelper:FindClosingBracket (pcQueryString, iPos) + 1.

                    IF iPos > LENGTH (pcQueryString, "CHARACTER":U) THEN iPos = 0 .
                END.
                ELSE IF cChar = "(":U AND iPos > 8 AND SUBSTRING (pcQueryString, iPos - 8, 8, "CHARACTER":U) = "DATETIME":U THEN DO:
                    iPos = StringHelper:FindClosingBracket (pcQueryString, iPos) + 1.

                    IF iPos > LENGTH (pcQueryString, "CHARACTER":U) THEN iPos = 0 .
                END.
                ELSE IF cChar = "(":U AND iPos > 11 AND SUBSTRING (pcQueryString, iPos - 11, 11, "CHARACTER":U) = "DATETIME-TZ":U THEN DO:
                    iPos = StringHelper:FindClosingBracket (pcQueryString, iPos) + 1.

                    IF iPos > LENGTH (pcQueryString, "CHARACTER":U) THEN iPos = 0 .
                END.
            END.

            /* iPos may have changed in block above, so need to re-evaluate */
            IF iPos = 0 THEN DO:
                /* Consider remaining string a term */
                oExpression = THIS-OBJECT:ParseTerm (SUBSTRING (pcQueryString, iStart, -1, "CHARACTER":U)) .

                oList:Add (oExpression) .
            END.
            ELSE DO:
                ASSIGN cChar = SUBSTRING (pcQueryString, iPos, 1, "CHARACTER":U) .

                IF cChar = " ":U AND SUBSTRING (pcQueryString, iPos, 5, "CHARACTER":U) = " AND ":U THEN DO:

                    /* Consider string since start a term */
                    oExpression = THIS-OBJECT:ParseTerm (SUBSTRING (pcQueryString, iStart, iPos - iStart, "CHARACTER":U)) .

                    IF VALID-OBJECT (oExpression) THEN
                        oList:Add (oExpression) .

                    iPos = iPos + 5 .
                END.
                ELSE IF cChar = " ":U AND SUBSTRING (pcQueryString, iPos, 4, "CHARACTER":U) = " OR ":U THEN DO:


                    /* Consider string since start a term */
                    IF SUBSTRING (pcQueryString, iStart, iPos - iStart, "CHARACTER":U) > "":U THEN DO:
                        oExpression = THIS-OBJECT:ParseTerm (SUBSTRING (pcQueryString, iStart, iPos - iStart, "CHARACTER":U)) .

                        oOr = NEW ListQueryExpression ().
                        oOr:Operator = OperatorEnum:Or .

                        oList:Add (oOr) .

                        IF VALID-OBJECT (oExpression) THEN
                            oOr:ADD (oExpression) .

                        oList = oOr .
                    END.
                    ELSE DO:
                        oOr = NEW ListQueryExpression ().
                        oOr:Operator = OperatorEnum:Or .

                        oOr:Add (oList:GetItem (oList:Count)).
                        oList:RemoveAt (oList:Count) .

                        oList:ADD (oOr) .

                        oList = oOr .
                    END.

                    iPos = iPos + 4 .

                END.
                ELSE IF cChar = " ":U AND SUBSTRING (pcQueryString, iPos, 4, "CHARACTER":U) = " BY ":U THEN DO:

                    /* Consider string since start a term */
                    oExpression = THIS-OBJECT:ParseTerm (SUBSTRING (pcQueryString, iStart, iPos - iStart, "CHARACTER":U)) .
                    oList:Add (oExpression) .

                    ASSIGN THIS-OBJECT:ByClause = SUBSTRING (pcQueryString, iPos + 1, -1, "CHARACTER":U) .

                    /* BY terminates Query String */
                    LEAVE iPosLoop .
                END.
                ELSE IF cChar = "(":U THEN DO:
                    ASSIGN iClosing = StringHelper:FindClosingBracket (pcQueryString, iPos) .

                    ASSIGN oExpression = THIS-OBJECT:ParseQueryString (SUBSTRING (pcQueryString, iPos + 1, iClosing - iPos - 1, "CHARACTER":U)) .

                    oList:Add (oExpression) .

                    IF iClosing = LENGTH (pcQueryString, "CHARACTER":U) THEN
                        ASSIGN iPos = 0 .
                    ELSE
                        ASSIGN iPos = iClosing + 1 .
                END.
                ELSE
                    UNDO, THROW NEW QueryParsingException (SUBSTITUTE("Error parsing query string, unexpected token ~"&1~" at position &2."{&TRAN},
                                                                      cChar,
                                                                      iPos),
                                                           0,
                                                           {&ExceptionTitle},
                                                           pcQueryString) .
            END.
        END.

        IF oReturn:Count = 1 THEN
            RETURN oReturn:GetItem (1) .
        ELSE
            RETURN oReturn .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Parses a single query term
        Notes:   A term consists from a field name (including DB and Table), the operator and a value
        Throws:  Consultingwerk.Exceptions.InvalidQueryTermException
        @param pcQueryTerm The query term to parse
        @return The IQueryExpression for the query term
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED IQueryExpression ParseTerm (pcQueryTerm AS CHARACTER):

        DEFINE VARIABLE oExpression          AS IQueryExpression NO-UNDO .
        DEFINE VARIABLE cFieldName           AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cOperator            AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE cRemaining           AS CHARACTER        NO-UNDO .
        DEFINE VARIABLE oField               AS BufferFieldName  NO-UNDO .

        DEFINE VARIABLE iPos                 AS INTEGER          NO-UNDO .

        /* Characters that may end the field name */
        DEFINE VARIABLE cFieldNameTerminator AS CHARACTER        NO-UNDO INITIAL " ,=,>,<":U.
        /* Characters that may end the operator */
        DEFINE VARIABLE cOperatorTerminator  AS CHARACTER        NO-UNDO INITIAL " ,~",0,1,2,3,4,5,6,7,8,9":U.

        IF TRIM (pcQueryTerm) = "":U THEN
            RETURN ? .

        /* Mike Fechner, Consultingwerk Ltd. 17.03.2015
           SCL-702: Need to left-trim to get rid of spaces before the field name */
        ASSIGN pcQueryTerm = LEFT-TRIM (pcQueryTerm) .

        ASSIGN iPos = StringHelper:FirstOccurenceOf (pcQueryTerm,
                                                     cFieldNameTerminator) .

        /* The operator may not start before pos 2, otherwise there would be no room for a field name */
        IF iPos < 2 THEN
            UNDO, THROW NEW InvalidQueryTermException (SUBSTITUTE ("Unable to locate operator in query term: &1"{&TRAN}, pcQueryTerm),
                                                       0,
                                                       {&ExceptionTitle},
                                                       pcQueryTerm) .

        ASSIGN cFieldName = TRIM (SUBSTRING (pcQueryTerm, 1, iPos - 1, "CHARACTER":U))
               cRemaining = TRIM (SUBSTRING (pcQueryTerm, iPos, -1, "CHARACTER":U)) .

        ASSIGN iPos = StringHelper:FirstOccurenceOf (cRemaining,
                                                     cOperatorTerminator) .

        /* The value may not start before pos 2, otherwise there would be no room for a field name */
        IF iPos < 2 THEN
            UNDO, THROW NEW InvalidQueryTermException (SUBSTITUTE ("Unable to locate value for comparison in query term: &1"{&TRAN}, pcQueryTerm),
                                                       0,
                                                       {&ExceptionTitle},
                                                       pcQueryTerm) .

        ASSIGN cOperator  = TRIM (SUBSTRING (cRemaining, 1, iPos - 1, "CHARACTER":U))
               cRemaining = TRIM (SUBSTRING (cRemaining, iPos, -1, "CHARACTER":U)) .

        oField = BufferHelper:ParseFieldName (cFieldName) .

        IF oField:TableName = "":U OR oField:TableName = ? THEN
            ASSIGN oField:TableName = THIS-OBJECT:BufferName .

        /* Mike Fechner, Consultingwerk Ltd. 14.08.2015
           SCL-964: JSDO style passing of DATE, DATETIME, DATETIME-TZ */
        IF cRemaining BEGINS "DATE(":U THEN
            oExpression = NEW QueryExpression (oField,
                                               THIS-OBJECT:ParseQueryTermOperator (cOperator),
                                               NEW DateHolder (THIS-OBJECT:ParseDateFunction (cRemaining))) .
        ELSE IF cRemaining BEGINS "DATETIME(":U THEN
            oExpression = NEW QueryExpression (oField,
                                               THIS-OBJECT:ParseQueryTermOperator (cOperator),
                                               NEW DateHolder (THIS-OBJECT:ParseDateTimeFunction (cRemaining))) .
        ELSE IF cRemaining BEGINS "DATETIME-TZ(":U THEN
            oExpression = NEW QueryExpression (oField,
                                               THIS-OBJECT:ParseQueryTermOperator (cOperator),
                                               NEW DateHolder (THIS-OBJECT:ParseDateTimeTzFunction (cRemaining))) .
        ELSE oExpression = NEW QueryExpression (oField,
                                               THIS-OBJECT:ParseQueryTermOperator (cOperator),
                                               StringHelper:Unquote (cRemaining)) .

        RETURN oExpression.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Trims FOR EACH|FIRST|... <table> or PRESELECT EACH|FIRST|... <table>
                 from the Query String
        Notes:
        @param pcQueryString The ABL QueryString to parse
        @return The Query String without the leading FOR EACH/PRESELECT EACH <table>
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER TrimForOrPreselect (pcQueryString AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        IF NOT (pcQueryString BEGINS "FOR ":U OR pcQueryString BEGINS "PRESELECT ":U) THEN
            RETURN pcQueryString .

        /* Trim first three words form QueryString */
        DO i = 1 TO 3:
            ASSIGN ENTRY (1, pcQueryString, " ":U) = "":U
                   pcQueryString = TRIM (pcQueryString) .
        END.

        RETURN pcQueryString .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Trims FOR EACH|FIRST|... <table> or PRESELECT EACH|FIRST|... <table>
                 from the Query String
        Notes:
        @param pcQueryString The ABL QueryString to parse
        @return The Query String without the leading FOR EACH/PRESELECT EACH <table>
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED CHARACTER TrimWhere (pcQueryString AS CHARACTER):

        IF NOT pcQueryString BEGINS "WHERE ":U THEN
            RETURN pcQueryString .

        RETURN TRIM (SUBSTRING (pcQueryString, 6, -1, "CHARACTER":U)) .

    END METHOD .

END CLASS.
