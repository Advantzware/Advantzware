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
    File        : ProparseAssert
    Purpose     : Provides Proparse related validation routines
    Syntax      :
    Description : Routines in this class typically raise an error condition
                  when the condition is not met.
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 28 10:48:43 CET 2013
    Notes       : Requires GUI for .NET
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING com.joanju.proparse.NodeTypes FROM ASSEMBLY .
&ENDIF
USING Consultingwerk.Assertion.*    FROM PROPATH .
&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING org.prorefactor.core.JPNode   FROM ASSEMBLY .
&ENDIF
USING Progress.Lang.*               FROM PROPATH .

CLASS Consultingwerk.Assertion.ProparseAssert:

    /**
     * Purpose: Constructor for the ProparseAssert class
     * Notes:   Disallow instance creation
     */
    CONSTRUCTOR PRIVATE ProparseAssert ():
        SUPER ().

    END CONSTRUCTOR.

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    /**
     * Purpose: Verifies the Node Type of the given JPNode reference
     * Notes:
     * Throws:  Consultingwerk.Assertion.AssertException
     * @param poNode The JPNode to verify
     * @param pcNodeType The expected Node Type
     */
    METHOD PUBLIC STATIC VOID NodeType (poNode AS JPNode,
                                        pcNodeType AS CHARACTER):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poNode """The JPNode to verify""{&TRAN}"} .

        IF NodeTypes:getTypeName(poNode:getType()) <> pcNodeType THEN
            UNDO, THROW NEW AssertException (SUBSTITUTE ("The Node Type ~"&1~" should be ~"&2~""{&TRAN},
                                                         NodeTypes:getTypeName(poNode:getType()),
                                                         pcNodeType)) .
    END METHOD .
&ENDIF

END CLASS.
