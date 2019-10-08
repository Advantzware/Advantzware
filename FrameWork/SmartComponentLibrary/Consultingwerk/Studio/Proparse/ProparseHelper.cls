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
    File        : ProparseHelper
    Purpose     : Provides advanced routines to analyse the Proparse AST
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 28 20:14:06 CEST 2011
    Notes       : http://www.joanju.com/analyst/javadoc/
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

&IF DEFINED (DotNetAccessible) NE 0 &THEN
USING com.joanju.proparse.NodeTypes              FROM ASSEMBLY .
USING Consultingwerk.*                           FROM PROPATH .
USING Consultingwerk.Assertion.*                 FROM PROPATH .
USING Consultingwerk.Exceptions.*                FROM PROPATH .
USING Consultingwerk.Framework.*                 FROM PROPATH .
USING Consultingwerk.Studio.Proparse.*           FROM PROPATH .
USING Consultingwerk.Studio.Proparse.NodeTypes.* FROM PROPATH.
USING Consultingwerk.Util.*                      FROM PROPATH .
USING org.prorefactor.core.JPNode                FROM ASSEMBLY .
USING org.prorefactor.nodetypes.*                FROM ASSEMBLY .
USING org.prorefactor.treeparser.*               FROM ASSEMBLY .
&ENDIF
USING Progress.Lang.*                            FROM PROPATH .

CLASS Consultingwerk.Studio.Proparse.ProparseHelper:

&IF DEFINED (DotNetAccessible) NE 0 &THEN
    DEFINE PRIVATE STATIC VARIABLE proparseEnv AS com.joanju.proparse.Environment NO-UNDO.
    DEFINE PRIVATE STATIC VARIABLE proparseSchema AS org.prorefactor.core.schema.Schema NO-UNDO.

    DEFINE PRIVATE STATIC VARIABLE prsession AS org.prorefactor.refactor.RefactorSession NO-UNDO .

    DEFINE PRIVATE STATIC VARIABLE oBackward AS org.proparse.api.Backward NO-UNDO.

    DEFINE PRIVATE STATIC VARIABLE lEnvironment  AS LOGICAL NO-UNDO INIT FALSE .
    DEFINE PRIVATE STATIC VARIABLE lInitalized   AS LOGICAL NO-UNDO INIT FALSE .

    /* To verify if ExportDatabaseSchema needs to reimport the schema */
    DEFINE PRIVATE STATIC VARIABLE cDbNamesSignature AS CHARACTER NO-UNDO INIT ? .

    {Consultingwerk/Studio/Proparse/ttRunStatements.i STATIC}

    /**
     * Purpose: Disallow instance creation
     * Notes:
     */
    CONSTRUCTOR PRIVATE ProparseHelper ():
        SUPER ().

    END CONSTRUCTOR.

    /**
     * Purpose: Static Constructor of the ProparseHelper class
     * Notes:
     */
    CONSTRUCTOR STATIC ProparseHelper ():

        proparseEnv = com.joanju.proparse.Environment:instance().
        proparseSchema = org.prorefactor.core.schema.Schema:getInstance().

        oBackward = NEW org.proparse.api.Backward().

    END CONSTRUCTOR.

    /**
     * Purpose: Returns if the child node with the given node path exists
     * Notes:   Pathes are separated either by / or \
     *          Supported path references, samples
     *          EQUAL\Field_ref\ID returns the ID node of an assignment
     *          RECORD_NAME\WHERE\AND\AND\2\QSTRING - The quoted string "b" in FOR EACH Customer WHERE Customer.Name BEGINS "Lift" AND Customer.City    BEGINS "b" AND Customer.Country = "USA": (the second node, a BEGINS is referenced by the 2)
     *          2 - returns the second child of the parent node
     * @param poParentNode The reference to the parent JPNode
     * @param pcPath The path reference
     * @return Logical value indicating if the child node with the given path exists
     */
    METHOD PUBLIC STATIC LOGICAL ExistsChildNodeByPath (poParentNode AS JPNode,
                                                        pcPath AS CHARACTER):

        RETURN VALID-OBJECT (ProparseHelper:GetChildNodeByPath (poParentNode,
                                                                pcPath)) .

    END METHOD .

    /**
     * Purpose: Exports the Database Schema to Proparse
     * Notes:   Will only export the database schema to proparse when there are new
     *          databases connected or new aliases defined
     */
    METHOD PUBLIC STATIC VOID ExportDatabaseSchema ():

        DEFINE VARIABLE iAlias                   AS INTEGER        NO-UNDO .
        DEFINE VARIABLE schemaDumpFile           AS CHARACTER      NO-UNDO .
        DEFINE VARIABLE cCurrentDbNamesSignature AS CHARACTER      NO-UNDO .

        DEFINE VARIABLE oStatusManager           AS IStatusManager NO-UNDO .

        IF cDbNamesSignature = ? THEN
            ASSIGN cDbNamesSignature = DatabaseHelper:GetDbNameSignature() .
        ELSE DO:
            ASSIGN cCurrentDbNamesSignature = DatabaseHelper:GetDbNameSignature() .

            IF cCurrentDbNamesSignature = cDbNamesSignature THEN
                RETURN .

            ASSIGN cDbNamesSignature = cCurrentDbNamesSignature .
        END.

        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager} .

        IF VALID-OBJECT (oStatusManager) THEN
            oStatusManager:DisplayStatus ("Exporting database schema to proparse."{&TRAN}) .

        proparseSchema:clear().

        IF NUM-DBS > 0 THEN DO:
            schemaDumpFile = Consultingwerk.Util.FileHelper:GetTempFileName() .

            RUN Consultingwerk/Studio/Proparse/schemadump1.p (schemaDumpFile).

            proparseSchema:loadSchema(schemaDumpFile).

            DO iAlias = 1 TO NUM-ALIASES:
                proparseSchema:aliasCreate (ALIAS (iAlias), LDBNAME (ALIAS (iAlias))) .
            END.
        END.

        FINALLY:
            OS-DELETE VALUE (schemaDumpFile) .

            IF VALID-OBJECT (oStatusManager) THEN
                oStatusManager:ClearStatus() .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Searches for a child node of the given node with the matching node type
     * Notes:   This method does not work recursively, so only direct child nodes are
     *          processed
     * @param poASTNode The parent JPNode
     * @param pcChildNodeType The searched child not type as returned by NodeTypes:getTypeName
     * @return The found JPNode or the unknown value
     */
    METHOD PUBLIC STATIC JPNode FindChildNodeOfNodeType (poASTNode AS JPNode,
                                                         pcChildNodeType AS CHARACTER):

        DEFINE VARIABLE oChild      AS JPNode NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = pcChildNodeType THEN
                RETURN oChild .

            oChild = oChild:nextSibling () .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Searches for a child node of the given node with the matching node type
     *          and text
     * Notes:   This method does not work recursively, so only direct child nodes are
     *          processed
     *          Useful for finding Annotations with getText() = "@Annotation"
     * @param poASTNode The parent JPNode
     * @param pcChildNodeType The searched child node type as returned by NodeTypes:getTypeName
     * @param pcChildNodeText The searched child node getText() value
     * @return The found JPNode or the unknown value
     */
    METHOD PUBLIC STATIC JPNode FindChildNodeOfNodeTypeWithText (poASTNode AS JPNode,
                                                                 pcChildNodeType AS CHARACTER,
                                                                 pcChildNodeText AS CHARACTER):

        DEFINE VARIABLE oChild      AS JPNode NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = pcChildNodeType AND
               oChild:getText()                        = pcChildNodeText THEN
                RETURN oChild .

            oChild = oChild:nextSibling () .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Searches for a child node of the given node with the matching node type
     *          and ID childnode text
     * Notes:   This method does not work recursively, so only direct child nodes are
     *          processed
     *          Useful for finding
     * @param poASTNode The parent JPNode
     * @param pcChildNodeType The searched child node type as returned by NodeTypes:getTypeName
     * @param pcChildNodeID The searched child node ID node value
     * @return The found JPNode or the unknown value
     */
    METHOD PUBLIC STATIC JPNode FindChildNodeOfNodeTypeWithID (poASTNode AS JPNode,
                                                               pcChildNodeType AS CHARACTER,
                                                               pcChildNodeID AS CHARACTER):

        DEFINE VARIABLE oChild      AS JPNode NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = pcChildNodeType AND
               ProparseHelper:HasChildNodeOfNodeTypeWithText (oChild,
                                                              "ID":U,
                                                              pcChildNodeID) THEN
                RETURN oChild .

            oChild = oChild:nextSibling () .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns the first JPNode in a AST by position
     * Notes:
     * @param poNode The parent JPNode
     * @return The found JPNode or the unknown value
     */
    METHOD PUBLIC STATIC JPNode FindFirstChildByPositionInCode (poNode AS JPNode):

        DEFINE VARIABLE oFirstNode AS JPNode                                      NO-UNDO .
        DEFINE VARIABLE oNodes     AS "System.Collections.Generic.List<JPNode>":U NO-UNDO .

        oNodes = NEW "System.Collections.Generic.List<JPNode>" () .

        ProparseHelper:GetNodeChildren (poNode, oNodes) .

        {Consultingwerk/foreach.i JPNode oNode in oNodes}
            IF NOT VALID-OBJECT (oFirstNode) THEN DO:

                IF oNode:getLine() > 0 AND oNode:getColumn() > 0 THEN
                    oFirstNode = oNode .
            END.
            ELSE
                IF oNode:getLine() > 0 AND oNode:getColumn() > 0 AND
                   (oNode:getLine() < oFirstNode:getLine() OR
                   (oNode:getLine() = oFirstNode:getLine() AND oNode:getColumn() < oFirstNode:getColumn())) THEN
                   oFirstNode = oNode .
        END. /* foreach.i */

        RETURN oFirstNode .

    END METHOD.

    /**
     * Purpose: Returns a BufferFieldName based on the given FieldRefNode instance
     * Notes:
     * @param poFieldNode The FieldRefNode to parse
     * @return The BufferFieldName with the data of the FieldRefNode
     */
    METHOD PUBLIC STATIC BufferFieldName GetBufferFieldName (poFieldNode AS FieldRefNode):

        DEFINE VARIABLE oField  AS BufferFieldName NO-UNDO .
        DEFINE VARIABLE iExtent AS INTEGER         NO-UNDO .

        oField = BufferHelper:ParseFieldName (poFieldNode:getIdNode():getText()) .

        IF ProparseHelper:HasChildNodeOfNodeType (poFieldNode, "Array_subscript":U) THEN DO:
            ASSIGN iExtent = INTEGER (ProparseHelper:GetChildNodeByPath (poFieldNode, "Array_subscript~\NUMBER":U):getText()) NO-ERROR .

            IF iExtent > 0 THEN
                oField:Index = iExtent .
        END.

        RETURN oField .

    END METHOD .

    /**
     * Purpose: Returns the reference to the child node with the given node path
     * Notes:   Pathes are separated either by / or \
     *          Supported path references, samples
     *          EQUAL\Field_ref\ID returns the ID node of an assignment
     *          RECORD_NAME\WHERE\AND\AND\2\QSTRING - The quoted string "b" in FOR EACH Customer WHERE Customer.Name BEGINS "Lift" AND Customer.City    BEGINS "b" AND Customer.Country = "USA": (the second node, a BEGINS is referenced by the 2)
     *          2 - returns the second child of the parent node
     * @param poParentNode The reference to the parent JPNode
     * @param pcPath The path reference
     * @return The reference to the child JPNode with the given node path
     */
    METHOD PUBLIC STATIC JPNode GetChildNodeByPath (poParentNode AS JPNode,
                                                    pcPath AS CHARACTER):

        DEFINE VARIABLE oNode  AS JPNode    NO-UNDO .
        DEFINE VARIABLE oChild AS JPNode    NO-UNDO .
        DEFINE VARIABLE cEntry AS CHARACTER NO-UNDO .
        DEFINE VARIABLE iEntry AS INTEGER   NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER   NO-UNDO .

        ASSIGN pcPath = TRIM (REPLACE (pcPath, "~\":U, "/":U)) .

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poParentNode "'poParentNode':U"}
        {Consultingwerk/Assertion/Assert/NotNullOrEmpty.i pcPath "'pcPath':U"}

        ASSIGN oNode = poParentNode .

        DO iEntry = 1 TO NUM-ENTRIES (pcPath, "/":U):

            ASSIGN cEntry = TRIM (ENTRY (iEntry, pcPath, "/":U)) .

            IF cEntry = "" OR cEntry = ? THEN
                UNDO, THROW NEW InvalidValueException (pcPath, "pcPath":U)  .

            IF SUBSTRING (cEntry, 1, 1) >= "0":U AND
               SUBSTRING (cEntry, 1, 1) <= "9":U THEN DO:

                ASSIGN oChild = oNode:firstChild () .

                IF NOT VALID-OBJECT (oChild) THEN
                    RETURN ? .

                DO i = 2 TO INTEGER (cEntry):
                    oChild = oChild:nextSibling () .

                    IF NOT VALID-OBJECT (oChild) THEN
                        RETURN ? .
                END.
            END.
            ELSE
                ASSIGN oChild = ProparseHelper:FindChildNodeOfNodeType (oNode, cEntry) .

            IF NOT VALID-OBJECT (oChild) THEN
                RETURN ? .

            ASSIGN oNode = oChild .
        END.

        RETURN oNode .

    END METHOD .

    /**
     * Purpose: Returns a temp-table with all interal RUN statements in the given
     *          node (typically the Root node of a parse unit)
     * Notes:
     * @param poNode The node to parse
     * @param ttRunStatements The temp-table with the located run statements (from Consultingwerk/Studio/Proparse/ttRunStatements.i)
     */
    METHOD PUBLIC STATIC VOID GetInternalRunStatements (poNode AS JPNode,
                                                        OUTPUT TABLE ttRunStatements):

        EMPTY TEMP-TABLE ttRunStatements .

        ProparseHelper:GetInternalRunStatements (poNode) .

    END METHOD.

    /**
     * Purpose: Returns a temp-table with all interal RUN statements in the given
     *          node (typically the Root node of a parse unit)
     * Notes:
     * @param poNode The node to parse
     */
    METHOD PROTECTED STATIC VOID GetInternalRunStatements (poNode AS JPNode):

        DEFINE VARIABLE oParent    AS JPNode    NO-UNDO .
        DEFINE VARIABLE oCall      AS Call      NO-UNDO .

        nodeloop: DO WHILE VALID-OBJECT (poNode):

            IF NodeTypes:getTypeName(poNode:getType()) = "RUN":U THEN DO:

                oCall = poNode:getCall ().

                IF NOT VALID-OBJECT (oCall) THEN DO:
                    /* Ignore RUN in ON phrase - as that's a RUN to an internal procedure */
                    poNode = poNode:nextSibling () .
                    NEXT nodeloop .
                END.

                                                                      /* Consider RUN IN ... like not in a HANDLE */
                IF (NOT oCall:isInHandle() OR (oCall:isInHandle() AND ProparseHelper:ExistsChildNodeByPath (poNode, "IN~\THISPROCEDURE":U)))
                   AND NOT oCall:isPersistent()
                   AND oCall:getInternalName () > "":U

                THEN DO:

                    oParent = ProparseHelper:FindRoutineParent (poNode) .

                    CREATE ttRunStatements .
                    ASSIGN ttRunStatements.ProcedureName    = oCall:getInternalName ()
                           ttRunStatements.RunFileName      = poNode:getFileName ()
                           ttRunStatements.RunLineNumber    = poNode:getLine ()
                           ttRunStatements.Statement        = poNode:toStringFullText ()
                           ttRunStatements.RunNode          = poNode
                           ttRunStatements.ParentNode       = oParent
                           ttRunStatements.ParentType       = NodeTypes:getTypeName(oParent:getType())
                           ttRunStatements.ParentFileName   = oParent:getFileName ()
                           ttRunStatements.ParentLineNumber = oParent:getLine ()
                           .

                    CASE ttRunStatements.ParentType:
                        WHEN "PROCEDURE":U THEN
                            ASSIGN ttRunStatements.ParentProcedureName = ProparseHelper:FindChildNodeOfNodeType (oParent, "ID":U):getText ().

                        WHEN "ON":U THEN
                            ASSIGN ttRunStatements.TriggerEvent  = ProparseHelper:TriggerEventList (oParent)
                                   ttRunStatements.TriggerWidget = ProparseHelper:TriggerWidgetRef (oParent) .
                    END CASE .
                END.
            END.

            IF VALID-OBJECT (poNode:firstChild()) THEN
                ProparseHelper:GetInternalRunStatements (poNode:firstChild()) .

            poNode = poNode:nextSibling () .
        END.

    END METHOD.

    /**
     * Purpose: Returns the List with all children (direct and indirect added)
     * Notes:
     * @param poNode The parent JPNode
     * @param poNodes The generic List of JPNode object
     */
    METHOD PUBLIC STATIC VOID GetNodeChildren (poNode AS JPNode,
                                               poNodes AS "System.Collections.Generic.List<JPNode>":U):

        DEFINE VARIABLE oNode AS JPNode NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poNode, "Parent Node":U) .
        Consultingwerk.Assertion.ObjectAssert:IsValid (poNode, "Nodes List":U) .

        ASSIGN oNode = poNode:firstChild () .

        DO WHILE VALID-OBJECT (oNode):
            poNodes:Add (oNode) .

            IF VALID-OBJECT (oNode:firstChild()) THEN
                GetNodeChildren (oNode, poNodes) .

            oNode = oNode:nextSibling () .
        END.

    END METHOD.

    /**
     * Purpose: Gets a Node by it's number
     * Notes:
     * @param piNodeNum The node number to return
     * @return The JPNode with the specified number
     */
    METHOD PUBLIC STATIC JPNode GetNodeFromNum (piNodeNum AS INTEGER):

        DEFINE VARIABLE node AS JPNode NO-UNDO.

        node = oBackward:getNode(piNodeNum).

        RETURN node.

    END METHOD.

    /**
     * Purpose: Returns the Subtype name based on Proparse's SubType Index
     * Notes:
     * @param piSubtypeIndex The index of the subtype as returns from the JPNode
     * @return The CHARACTER SubtypeName
     */
    METHOD PUBLIC STATIC CHARACTER GetSubtypeName (piSubtypeIndex AS INTEGER):

        DEFINE VARIABLE cSubtypes AS CHARACTER NO-UNDO EXTENT 5
            INIT ["JPNode":U,
                  "BlockNode":U,
                  "FieldRefNode":U,
                  "RecordNameNode":U,
                  "ProparseDirectiveNode":U].

        IF piSubtypeIndex > EXTENT (cSubtypes) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Unknown subtype index &1"{&TRAN}, piSubtypeIndex), 0) .

        RETURN cSubtypes [piSubtypeIndex] .

    END METHOD.

    /**
     * Purpose: Searches for a Childnode of the given node with the matching node type
     * Notes:
     * @param poASTNode The parent JPNode
     * @param pcChildNodeType The searched child not type as returned by NodeTypes:getTypeName
     * @return True when a child node with the given node type exists
     */
    METHOD PUBLIC STATIC LOGICAL HasChildNodeOfNodeType (poASTNode AS JPNode,
                                                         pcChildNodeType AS CHARACTER):

        DEFINE VARIABLE oChild      AS JPNode NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = pcChildNodeType THEN
                RETURN TRUE .

            oChild = oChild:nextSibling () .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Searches for a Childnode of the given node with the matching node type
     *          and text
     * Notes:   Useful for finding Annotations with getText() = "@Annotation"
     * @param poASTNode The parent JPNode
     * @param pcChildNodeType The searched child not type as returned by NodeTypes:getTypeName
     * @param pcChildNodeText The searched child node getText() value
     * @return True when a child node with the given node type exists
     */
    METHOD PUBLIC STATIC LOGICAL HasChildNodeOfNodeTypeWithText (poASTNode AS JPNode,
                                                                 pcChildNodeType AS CHARACTER,
                                                                 pcChildNodeText AS CHARACTER):

        DEFINE VARIABLE oChild      AS JPNode NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = pcChildNodeType AND
               oChild:getText()                        = pcChildNodeText THEN

                RETURN TRUE .

            oChild = oChild:nextSibling () .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Searches for a Childnode of the given node with the matching node type
     *          and ID childnode text
     * Notes:   Useful for finding PROCEDURE with a certain name etc.
     * @param poASTNode The parent JPNode
     * @param pcChildNodeType The searched child not type as returned by NodeTypes:getTypeName
     * @param pcChildNodeID The searched child node ID node value
     * @return True when a child node with the given node type exists
     */
    METHOD PUBLIC STATIC LOGICAL HasChildNodeOfNodeTypeWithID (poASTNode AS JPNode,
                                                               pcChildNodeType AS CHARACTER,
                                                               pcChildNodeID AS CHARACTER):

        DEFINE VARIABLE oChild      AS JPNode NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = pcChildNodeType AND
               ProparseHelper:HasChildNodeOfNodeTypeWithText (oChild,
                                                              "ID":U,
                                                              pcChildNodeID) THEN
                    RETURN TRUE .

            oChild = oChild:nextSibling () .
        END.

        RETURN FALSE .

    END METHOD .

    /**
     * Purpose: Initializes propase
     * Notes:
     */
    METHOD PUBLIC STATIC VOID Initialize ():

        org.prorefactor.refactor.RefactorSession:invalidateCurrentSettings () .

        prsession = org.prorefactor.refactor.RefactorSession:getInstance().
        prsession:setContextDirName (SESSION:TEMP-DIRECTORY) .

        oBackward:clear().

        ASSIGN lInitalized = TRUE .

    END METHOD.

    /**
     * Purpose: Finds the Routine-Level Block that contains the given JPNode
     * Notes:
     * @param poNode The JPNode to return the reference to the routine-level block for
     * @return The JPNode that describes the routine-level block for the given JPNode
     */
    METHOD PUBLIC STATIC JPNode FindRoutineParent (poNode AS JPNode):

        DEFINE VARIABLE oParent AS JPNode NO-UNDO.

        oParent = poNode:parent () .

        DO WHILE VALID-OBJECT (oParent):

            IF LOOKUP (NodeTypes:getTypeName(oParent:getType()),
                      "ON,PROCEDURE,FUNCTION,METHOD,Program_root,CLASS,CONSTRUCTOR,METHOD,GET,SET":U) > 0 THEN

                RETURN oParent .

            oParent = oParent:parent () .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns the block level relative to the next Routine-Level Block
     *          for the given JPNode
     * Notes:
     * @param poNode The JPNode to return the block level for
     * @return Block level relatively to the parent node
     */
    METHOD PUBLIC STATIC INTEGER GetRoutineDepth (poNode AS JPNode):

        DEFINE VARIABLE oParent AS JPNode  NO-UNDO.
        DEFINE VARIABLE iDepth  AS INTEGER NO-UNDO.

        oParent = poNode:parent () .


        DO WHILE VALID-OBJECT (oParent):

            IF LOOKUP (NodeTypes:getTypeName(oParent:getType()),
                      "ON,PROCEDURE,FUNCTION,METHOD,CLASS,CONSTRUCTOR,METHOD,GET,SET":U) > 0 THEN

                RETURN iDepth .

            IF NodeTypes:getTypeName(oParent:getType()) = "Code_block":U THEN
                ASSIGN iDepth = iDepth + 1.

            oParent = oParent:parent () .
        END.

        RETURN ? .

    END METHOD.

    /**
     * Purpose: Returns if the parent of the given JPNode is a simple DO: block
     *          (without any condision, e.g. DO WITH FRAME:)
     * Notes:
     * @param poNode The JPNode to test the parent block of
     * @return Logical value indicating if the parent of the given node is a trivial DO block
     */
    METHOD PUBLIC STATIC LOGICAL ParentIsTrivialDo (poNode AS JPNode):

        DEFINE VARIABLE oParent AS JPNode  NO-UNDO.

        oParent = poNode:parent () .

        IF NodeTypes:getTypeName(oParent:getType()) = "Code_block":U AND
           NodeTypes:getTypeName(oParent:parent():getType()) = "DO":U AND
           NodeTypes:getTypeName(oParent:parent():parent():getType()) = "Code_block":U THEN

            RETURN TRUE .

        ELSE
            RETURN FALSE .
    END METHOD .

    /**
     * Purpose: Returns the number of child nodes of the given node with the matching
     *          node type
     * Notes:   This method does not work recursively, so only direct child nodes are
     *          counted
     * @param poASTNode The parent JPNode
     * @param pcChildNodeType The searched child not type as returned by NodeTypes:getTypeName
     * @return The number of child nodes with the given type
     */
    METHOD PUBLIC STATIC INTEGER NumChildNodeOfNodeType (poASTNode AS JPNode,
                                                         pcChildNodeType AS CHARACTER):

        DEFINE VARIABLE oChild AS JPNode  NO-UNDO .
        DEFINE VARIABLE iCount AS INTEGER NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = pcChildNodeType THEN
                ASSIGN iCount = iCount + 1 .

            oChild = oChild:nextSibling () .
        END.

        RETURN iCount .

    END METHOD .

    /**
     * Purpose: Returns the number of direct child nodes of the given JPNode
     * Notes:
     * @param poNode The JPNode to return the number of children of
     * @return The number of direct child nods
     */
    METHOD PUBLIC STATIC INTEGER NumChildren (poNode AS JPNode):

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poNode "'poNode':U"}

        RETURN poNode:getDirectChildren():size() .

    END METHOD .

    /**
     * Purpose: Parses a file and returns the reference to the ParseUnit
     * Notes:
     * @param pcFileName The CHARACTER FileName to be used to open the java.io.File
     * @return The proparse ParseUnit reference
     */
    METHOD PUBLIC STATIC ParseUnit ParseFile (pcFilename AS CHARACTER):

        DEFINE VARIABLE javafile       AS java.io.File                         NO-UNDO .
        DEFINE VARIABLE pu             AS org.prorefactor.treeparser.ParseUnit NO-UNDO .

        DEFINE VARIABLE oStatusManager AS IStatusManager                       NO-UNDO .

        oStatusManager = {Consultingwerk/get-service.i Consultingwerk.Framework.IStatusManager} .

        IF VALID-OBJECT (oStatusManager) THEN
            oStatusManager:DisplayStatus (SUBSTITUTE ("Parsing: &1"{&TRAN}, pcFilename)) .

        IF NOT lEnvironment THEN
            SetProparseEnvironment() .

        IF NOT lInitalized THEN
            Initialize() .

        FILE-INFORMATION:FILE-NAME = SUBSTITUTE ("&1~\prorefactor~\temp~\listingfile.txt":U,
                                          SESSION:TEMP-DIRECTORY) .

        IF FILE-INFORMATION:FULL-PATHNAME > "":U AND NOT FILE-INFORMATION:FILE-TYPE MATCHES "*W*":U THEN
            UNDO, THROW NEW ProparseTempFileWriteProtectedException (FILE-INFORMATION:FULL-PATHNAME,
                                                                     "Error invoking Proparse"{&TRAN}) .

        javafile = NEW java.io.File (pcFilename).

        IF (NOT javafile:exists()) THEN
            UNDO, THROW NEW AppError (SUBSTITUTE ("Could not find file: &1."{&TRAN}, pcFileName), 0) .

        pu = NEW ParseUnit(javafile).
        pu:treeParser01().

        DELETE OBJECT javafile .

        RETURN pu .

        FINALLY:
            IF VALID-OBJECT (oStatusManager) THEN
                oStatusManager:ClearStatus() .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Parses the Parameter_list of a Method/Function/Event/Constructor
     *          node
     * Notes:   Returns a value in the form "INPUT CHARACTER,OUTPUT INTEGER,OUTPUT DATASET-HANDLE"
     * @param poParentNode The reference to the JPNode of the Method/Function/Event/Constructor
     * @return The Character representation of the Parameter list
     */
    METHOD PUBLIC STATIC CHARACTER ParseParameterList (poParentNode AS JPNode):

        RETURN ProparseHelper:ParseParameterList (poParentNode, FALSE) .

    END METHOD .

    /**
     * Purpose: Parses the Parameter_list of a Method/Function/Event/Constructor
     *          node
     * Notes:   Returns a value in the form "INPUT CHARACTER,OUTPUT INTEGER,OUTPUT DATASET-HANDLE"
     * @param poParentNode The reference to the JPNode of the Method/Function/Event/Constructor
     * @param plIncludeParameterName Logical value indicating if the parameter name should be included as well
     * @return The Character representation of the Parameter list
     */
    METHOD PUBLIC STATIC CHARACTER ParseParameterList (poParentNode AS JPNode,
                                                       plIncludeParameterName AS LOGICAL):

        DEFINE VARIABLE cSignature AS CHARACTER NO-UNDO .
        DEFINE VARIABLE oNode      AS JPNode    NO-UNDO .
        DEFINE VARIABLE oAsNode    AS JPNode    NO-UNDO .
        DEFINE VARIABLE oTypeNode  AS JPNode    NO-UNDO .
        DEFINE VARIABLE cDataType  AS CHARACTER NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poParentNode, "poParentNode":U) .

        oNode = ProparseHelper:FindChildNodeOfNodeType (poParentNode, "Parameter_list":U) .

        IF NOT VALID-OBJECT (oNode) THEN
            RETURN "":U .

        oNode = oNode:firstChild () .

        DO WHILE VALID-OBJECT (oNode):

            IF NodeTypes:getTypeName(oNode:getType()) = "INPUT":U OR
               NodeTypes:getTypeName(oNode:getType()) = "OUTPUT":U OR
               NodeTypes:getTypeName(oNode:getType()) = "INPUTOUTPUT":U THEN DO:

                IF NodeTypes:getTypeName(oNode:getType()) = "INPUT":U THEN
                    ASSIGN cSignature = cSignature + ",INPUT":U .
                ELSE IF NodeTypes:getTypeName(oNode:getType()) = "OUTPUT":U THEN
                    ASSIGN cSignature = cSignature + ",OUTPUT":U .
                ELSE IF NodeTypes:getTypeName(oNode:getType()) = "INPUTOUTPUT":U THEN
                    ASSIGN cSignature = cSignature + ",INPUT-OUTPUT":U .

                /* AS Node */
                oAsNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "AS":U) .

                IF VALID-OBJECT (oAsNode) THEN DO:

                    oTypeNode = oAsNode:nextSibling () .

                    IF NodeTypes:getTypeName(oTypeNode:getType()) = ProparseNodeTypes:N_CLASS THEN
                        ASSIGN oTypeNode = oTypeNode:nextSibling () .

                    ASSIGN cDataType = DataTypeHelper:UnabbreviateDataTypeName (oTypeNode:getText ()) .

                    IF cDataType > "":U THEN
                        ASSIGN cSignature = cSignature + " ":U + cDataType .
                    ELSE
                        ASSIGN cSignature = cSignature + " ":U + oTypeNode:getText () .
                END.
                ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "DATASET":U) THEN
                    ASSIGN cSignature = cSignature + " DATASET":U .
                ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "DATASETHANDLE":U) THEN
                    ASSIGN cSignature = cSignature + " DATASET-HANDLE":U .
                ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "TABLE":U) THEN
                    ASSIGN cSignature = cSignature + " TABLE":U .
                ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "TABLEHANDLE":U) THEN
                    ASSIGN cSignature = cSignature + " TABLE-HANDLE":U .

                IF plIncludeParameterName THEN
                    ASSIGN cSignature = cSignature + " ":U + ProparseHelper:FindChildNodeOfNodeType(oNode, "ID":U):getText () .
            END.

            oNode = oNode:nextSibling () .
        END.

        RETURN TRIM (cSignature, ",":U) .

    END METHOD .

    /**
     * Purpose: Parses the Parameter_list of a procedure (internal or external) node
     * Notes:   Returns a value in the form "INPUT CHARACTER,OUTPUT INTEGER,OUTPUT DATASET-HANDLE"
     * @param poParentNode The reference to the JPNode of the Procedure or Program_root
     * @return The Character representation of the Parameter list
     */
    METHOD PUBLIC STATIC CHARACTER ParseProcedureParameterList (poParentNode AS JPNode):

        RETURN ProparseHelper:ParseProcedureParameterList (poParentNode, FALSE) .

    END METHOD .

    /**
     * Purpose: Parses the Parameter_list of a procedure (internal or external) node
     * Notes:   Returns a value in the form "INPUT CHARACTER,OUTPUT INTEGER,OUTPUT DATASET-HANDLE"
     * @param poParentNode The reference to the JPNode of the Procedure or Program_root
     * @param plIncludeParameterName Logical value indicating if the parameter name should be included as well
     * @return The Character representation of the Parameter list
     */
    METHOD PUBLIC STATIC CHARACTER ParseProcedureParameterList (poParentNode AS JPNode,
                                                                plIncludeParameterName AS LOGICAL):

        DEFINE VARIABLE cSignature AS CHARACTER       NO-UNDO .
        DEFINE VARIABLE oAsNode    AS JPNode          NO-UNDO .
        DEFINE VARIABLE oTypeNode  AS JPNode          NO-UNDO .

        DEFINE VARIABLE oWrapper    AS JPNodesWrapper NO-UNDO .
        DEFINE VARIABLE cDataType   AS CHARACTER      NO-UNDO .

        Consultingwerk.Assertion.ObjectAssert:IsValid (poParentNode, "poParentNode":U) .

        oWrapper = NEW JPNodesWrapper (ProparseHelper:FindChildNodeOfNodeType(poParentNode, "Code_block":U)) .
        oWrapper:NodeTypeFilter = "DEFINE":U .

        {Consultingwerk/foreachABL.i JPNode oNode IN oWrapper}
            IF NOT ProparseHelper:HasChildNodeOfNodeType(oNode, "PARAMETER":U) THEN

                NEXT .

            IF ProparseHelper:HasChildNodeOfNodeType(oNode, "OUTPUT":U) THEN
                ASSIGN cSignature = cSignature + ",OUTPUT":U .
            ELSE IF ProparseHelper:HasChildNodeOfNodeType(oNode, "INPUTOUTPUT":U) THEN
                ASSIGN cSignature = cSignature + ",INPUT-OUTPUT":U .
            ELSE
                ASSIGN cSignature = cSignature + ",INPUT":U .

            /* AS Node */
            oAsNode = ProparseHelper:FindChildNodeOfNodeType (oNode, "AS":U) .

            IF VALID-OBJECT (oAsNode) THEN DO:
                oTypeNode = oAsNode:firstChild () .

                IF NodeTypes:getTypeName(oTypeNode:getType()) = ProparseNodeTypes:N_CLASS THEN
                    ASSIGN oTypeNode = oTypeNode:nextSibling () .

                ASSIGN cDataType = DataTypeHelper:UnabbreviateDataTypeName (oTypeNode:getText ()) .

                IF cDataType > "":U THEN
                    ASSIGN cSignature = cSignature + " ":U + cDataType .
                ELSE
                    ASSIGN cSignature = cSignature + " ":U + oTypeNode:getText () .
            END.
            ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "DATASET":U) THEN
                ASSIGN cSignature = cSignature + " DATASET":U .
            ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "DATASETHANDLE":U) THEN
                ASSIGN cSignature = cSignature + " DATASET-HANDLE":U .
            ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "TABLE":U) THEN
                ASSIGN cSignature = cSignature + " TABLE":U .
            ELSE IF ProparseHelper:HasChildNodeOfNodeType (oNode, "TABLEHANDLE":U) THEN
                ASSIGN cSignature = cSignature + " TABLE-HANDLE":U .

            IF plIncludeParameterName THEN
                ASSIGN cSignature = cSignature + " ":U + ProparseHelper:FindChildNodeOfNodeType(oNode, "ID":U):getText () .
        END.

        RETURN TRIM (cSignature, ",":U) .

    END METHOD .

    /**
     * Purpose: Returns a parser configuration parameter
     * Notes:
     * @param pcFlag The name of a proparse config flag
     * @return The CHARACTER value of the Proparse config value
     */
    METHOD PUBLIC STATIC CHARACTER ParserConfigGet (pcFlag AS CHARACTER):

       RETURN proparseEnv:configGet (pcFlag).

    END METHOD.

    /**
     * Purpose: Sets a proparse configuration parameter (propath, proversion)
     * Notes:
     * @param pcFlag The name of a proparse config flag
     * @param pcValue The value for the proparse config flag
     * @return Always TRUE
     */
    METHOD PUBLIC STATIC LOGICAL ParserConfigSet (pcFlag AS CHARACTER, pcValue AS CHARACTER):

          proparseEnv:configSet (pcFlag, pcValue).

          RETURN TRUE.

    END METHOD.

    /**
     * Purpose: Parses the passed in String
     * Notes:   Writes the string to a temporary file for parsing. The temporary file
     *          name will be
     * @param plcString The LONGCHAR String to parse
     * @return The proparse ParseUnit reference
     */
    METHOD PUBLIC STATIC ParseUnit ParseString (plcString AS LONGCHAR):

        DEFINE VARIABLE cTempFileName AS CHARACTER NO-UNDO.

        ASSIGN cTempFileName = Consultingwerk.Util.FileHelper:GetTempFileName() .

        COPY-LOB FROM plcString TO FILE cTempFileName NO-CONVERT .

        RETURN ParseFile (cTempFileName) .

        FINALLY:
            IF System.IO.File:Exists (cTempFileName) THEN
                System.IO.File:Delete (cTempFileName) .

            @SuppressUnusedWarnings.
            CATCH ex AS System.IO.IOException:
                /* ignore */
            END CATCH.
        END FINALLY.

    END METHOD .

    /**
     * Purpose: Replaces a routine node (METHOD, CONSTRUCTOR, ...) with the given
     *          text
     * Notes:   Should not be called more than onces for a given Parse Unit as by replacing
     *          a routine node all following line counts may become inaccurate
     *          Really only replaces the text of the given node and not any nested nodes
     * @param poNode The JPNode to replace
     * @param plcNewText The new text for the node
     */
    METHOD PUBLIC STATIC VOID ReplaceIndividualNode (poNode AS JPNode,
                                                     plcNewText AS LONGCHAR):

        DEFINE VARIABLE hFrame    AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hEditor   AS HANDLE    NO-UNDO .

        DEFINE VARIABLE cText     AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE iLen      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iPos      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        cFileName = poNode:getFileName ().

        ObjectAssert:IsValid (poNode, "poNode":U) .

        CREATE FRAME hFrame .

        CREATE EDITOR hEditor
            ASSIGN WIDTH  = 60
                   HEIGHT = 20
                   LARGE  = TRUE
                   SCROLLBAR-HORIZONTAL = TRUE
                   SCROLLBAR-VERTICAL   = TRUE
                   FRAME = hFrame .

        hEditor:READ-FILE (cFileName) .

        /* Use editor widget to resolve lin/col into absolute position */
        hEditor:CURSOR-LINE = poNode:getLine() .
        hEditor:CURSOR-CHAR = poNode:getColumn() .

        iPos = hEditor:CURSOR-OFFSET .

        cText = hEditor:SCREEN-VALUE .

        iLen = LENGTH (poNode:getText ()) .

        /* Replace text of token */
        SUBSTRING (cText, iPos, iLen) = plcNewText .

        COPY-LOB FROM cText TO FILE cFileName .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hEditor) .
            GarbageCollectorHelper:DeleteObject (hFrame) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Replaces a routine node (METHOD, CONSTRUCTOR, ...) with the given
     *          text
     * Notes:   Should not be called more than onces for a given Parse Unit as by replacing
     *          a routine node all following line counts may become inaccurate
     *          Optionally replaces the comments block (a single block) or any white
     *          spaces that preceedes the method block.
     *          When replacing all white spaces preceeding the routine node
     *          (plReplaceAllLeadingHiddenText = TRUE) it is recommended, to start
     *          the plcNewText with two new lines (~n~n)
     * @param poNode The JPNode to replace
     * @param plcNewText The new text for the node
     * @param plReplaceComments Replaces comments preceeding the node
     * @param plReplaceAllLeadingHiddenText Replaces comments and all white spaces preceeding the node
     */
    METHOD PUBLIC STATIC VOID ReplaceRoutineNode (poNode AS JPNode,
                                                  plcNewText AS LONGCHAR,
                                                  plReplaceComments AS LOGICAL,
                                                  plReplaceAllLeadingHiddenText AS LOGICAL):

        DEFINE VARIABLE hFrame    AS HANDLE    NO-UNDO .
        DEFINE VARIABLE hEditor   AS HANDLE    NO-UNDO .

        DEFINE VARIABLE cText     AS LONGCHAR  NO-UNDO .
        DEFINE VARIABLE iLen      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE iPos      AS INTEGER   NO-UNDO .
        DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oJPNode   AS JPNode    NO-UNDO .

        cFileName = poNode:getFileName ().

        ObjectAssert:IsValid (poNode, "poNode":U) .

        IF plReplaceComments AND plReplaceAllLeadingHiddenText THEN
            UNDO, THROW NEW AssertException ("Only plReplaceComments or plReplaceAllLeadingHiddenText may be passed in as true."{&TRAN}, 0) .

        CREATE FRAME hFrame .

        CREATE EDITOR hEditor
            ASSIGN WIDTH  = 60
                   HEIGHT = 20
                   LARGE  = TRUE
                   SCROLLBAR-HORIZONTAL = TRUE
                   SCROLLBAR-VERTICAL   = TRUE
                   FRAME = hFrame .

        hEditor:READ-FILE (cFileName) .

        /* Use editor widget to resolve lin/col into absolute position */
        hEditor:CURSOR-LINE = poNode:getLine() .
        hEditor:CURSOR-CHAR = poNode:getColumn() .

        iPos = hEditor:CURSOR-OFFSET .

        cText = hEditor:SCREEN-VALUE .

        IF plReplaceComments AND poNode:getComments () > "":U AND R-INDEX (cText, "/*":U, iPos) > 0 THEN
            iPos = R-INDEX (cText, "/*":U, iPos)  .

        IF plReplaceAllLeadingHiddenText AND poNode:allLeadingHiddenText () > "":U THEN
            iPos = iPos - LENGTH (poNode:allLeadingHiddenText ()) .

        oJPNode = poNode:lastChild() .

        hEditor:CURSOR-CHAR = 1 .
        hEditor:CURSOR-LINE = oJPNode:getLine() .
        hEditor:CURSOR-CHAR = oJPNode:getColumn() .

        iLen = hEditor:CURSOR-OFFSET - iPos + LENGTH (oJPNode:getText ()).

        /* Replace text of token */
        SUBSTRING (cText, iPos, iLen) = plcNewText .

        COPY-LOB FROM cText TO FILE cFileName .

        FINALLY:
            GarbageCollectorHelper:DeleteObject (hEditor) .
            GarbageCollectorHelper:DeleteObject (hFrame) .
        END FINALLY.

    END METHOD.

    /**
     * Purpose: Set's OpenEdge session enviroment in proparse
     * Notes:
     */
    METHOD PUBLIC STATIC VOID SetProparseEnvironment ():

        ParserConfigSet ("batch-mode":U, STRING(SESSION:BATCH-MODE, "true/false":U)).
        ParserConfigSet ("opsys":U, OPSYS).
        ParserConfigSet ("propath":U, PROPATH).
        ParserConfigSet ("proversion":U, PROVERSION).
        ParserConfigSet ("window-system":U, SESSION:WINDOW-SYSTEM).

        ASSIGN lEnvironment = TRUE .

    END METHOD.

    /**
     * Purpose: Returns the Text of the Event_list node of a Trigger node
     * Notes:   Returns "":U if no Event_list node is present
     * @param poASTNode The JPNode of the Trigger itself (ON / Blocknode)
     * @return The Text of the Event_list node
     */
    METHOD PUBLIC STATIC CHARACTER TriggerEventList (poASTNode AS JPNode):

        DEFINE VARIABLE oChild      AS JPNode NO-UNDO .
        DEFINE VARIABLE oGrandChild AS JPNode NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = "Event_List":U THEN DO:
                oGrandChild = oChild:firstChild () .

                RETURN oGrandChild:getText () .
            END.

            oChild = oChild:nextSibling () .
        END.

        RETURN "":U .

    END METHOD.

    /**
     * Purpose: Returns the complete trigger Event_list node of a Trigger node
     * Notes:   Returns "":U if no Event_list node is present, comma-delimited
     * @param poASTNode The JPNode of the Trigger itself (ON / Blocknode)
     * @return The Text of the Event_list node
     */
    METHOD PUBLIC STATIC CHARACTER TriggerCompleteEventList (poASTNode AS JPNode):

        DEFINE VARIABLE oChild      AS JPNode NO-UNDO .
        DEFINE VARIABLE oGrandChild AS JPNode NO-UNDO .

        DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = "Event_List":U THEN DO:
                oGrandChild = oChild:firstChild () .

                DO WHILE VALID-OBJECT (oGrandChild):
                    IF NodeTypes:getTypeName(oGrandChild:getType()) <> "COMMA":U THEN

                        ASSIGN cReturn = cReturn + ",":U + oGrandChild:getText () .

                    ASSIGN oGrandChild = oGrandChild:nextSibling () .
                END.
            END.

            oChild = oChild:nextSibling () .
        END.

        RETURN TRIM (cReturn, ",":U) .

    END METHOD.

    /**
     * Purpose: Returns the Text of the Widget_ref node of a Trigger node
     * Notes:   Returns "":U if no Widget_ref node is present, drills down into the
     *          Field_ref node if present (for UI widgets)
     * @param poASTNode The JPNode of the Trigger itself (ON / Blocknode)
     * @return The Text of the Widget_ref node
     */
    METHOD PUBLIC STATIC CHARACTER TriggerWidgetRef (poASTNode AS JPNode):

        DEFINE VARIABLE oChild       AS JPNode NO-UNDO .
        DEFINE VARIABLE oGrandChild  AS JPNode NO-UNDO .
        DEFINE VARIABLE oGrandChild2 AS JPNode NO-UNDO .

        ASSIGN oChild = poASTNode:firstChild () .

        DO WHILE VALID-OBJECT (oChild):

            IF NodeTypes:getTypeName(oChild:getType()) = "Widget_ref":U THEN DO:
                oGrandChild = oChild:firstChild () .

                IF NodeTypes:getTypeName(oGrandChild:getType()) = "Field_ref":U THEN DO:
                    oGrandChild2 = oGrandChild:firstChild () .

                    RETURN oGrandChild2:getText () .
                END.
                ELSE
                    RETURN oGrandChild:getText () .
            END.

            oChild = oChild:nextSibling () .
        END.

        RETURN "":U .

    END METHOD.
&ENDIF
END CLASS.
