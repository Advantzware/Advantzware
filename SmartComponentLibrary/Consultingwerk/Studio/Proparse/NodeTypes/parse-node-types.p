/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : parse-node-types.p
    Purpose     : Parses the propoarse node types

    Syntax      :

    Description : Development tool to extract proparse node types from the 
                  Java source code of proparse at development. 
                  Only recommended to execute, when proparse is updated. 

    Author(s)   : 
    Created     : Wed Jul 22 11:36:18 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

USING Consultingwerk.Studio.Proparse.NodeTypes.* FROM PROPATH .
USING Consultingwerk.Util.*                      FROM PROPATH .
USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH.

DEFINE VARIABLE oNodeTypesParser AS NodeTypesParser NO-UNDO . 
DEFINE VARIABLE lcSource         AS LONGCHAR        NO-UNDO .
DEFINE VARIABLE lcProperties     AS LONGCHAR        NO-UNDO .

{Consultingwerk/Studio/Proparse/NodeTypes/ttNodeTypes.i}

/* ***************************  Main Block  *************************** */

oNodeTypesParser = NEW NodeTypesParser () .

oNodeTypesParser:ParseNodeTypesJava ("..\..\..\Proparse\SVN\proparse\src\com\joanju\proparse\NodeTypes.java":U,
                                     OUTPUT TABLE ttNodeTypes) .

TEMP-TABLE ttNodeTypes:WRITE-XML ("FILE":U, 
                                  "Consultingwerk/Studio/Proparse/NodeTypes/nodetypes.xml":U,
                                  TRUE) .
                                  
COPY-LOB FROM FILE "Consultingwerk/Studio/Proparse/NodeTypes/ProparseNodeTypes.template":U TO lcSource . 

FOR EACH ttNodeTypes ON ERROR UNDO, THROW:

    ASSIGN lcProperties = lcProperties + 
                          SUBSTITUTE ("    /*------------------------------------------------------------------------------~n" +
                                      "        Purpose: Node type for the &2 keyword ~n" +
                                      "        Notes:   ~n" +
                                      "    ------------------------------------------------------------------------------*/~n" +
                                      "    DEFINE PUBLIC STATIC PROPERTY N_&1 AS CHARACTER INIT ~"&1~":U NO-UNDO GET. ~n" +
                                      "~n":U, 
                                      ttNodeTypes.NodeType,
                                      ttNodeTypes.AblKeyWord) . 
END.

ASSIGN lcSource = REPLACE (lcSource, 
                           "@PROPERTIES@":U, 
                           lcProperties) .

COPY-LOB FROM lcSource TO FILE "Consultingwerk/Studio/Proparse/NodeTypes/ProparseNodeTypes.cls":U . 
