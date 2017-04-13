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
    File        : BufferSpec
    Purpose     : Describes a dynamic buffer
    Syntax      :
    Description : Parameter object to the BufferDataSource class
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 15 20:36:55 CEST 2016
    Notes       : See http://confluence.consultingwerkcloud.com/wiki/display/SCL/New+API+for+dynamic+DATA-SOURCE+objects
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.* FROM PROPATH .
USING Consultingwerk.OERA.*      FROM PROPATH .
USING Progress.Lang.*            FROM PROPATH .

CLASS Consultingwerk.OERA.BufferSpec
    IMPLEMENTS IBufferSpec:

    /**
     * Purpose: Gets and sets the optional buffer name for the buffer spec
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO
    GET:
        IF THIS-OBJECT:BufferName > "":U THEN
            RETURN THIS-OBJECT:BufferName .
        ELSE
            RETURN ENTRY (NUM-ENTRIES (THIS-OBJECT:TableName, ".":U), THIS-OBJECT:TableName, ".":U) .
    END.
    SET.

    /**
     *  Purpose: Gets and sets the optional parent buffer name for the buffer spec
     *  Notes:   When the parent ParentBufferName is not set, the first query buffer
     *           is assumed
     */
    DEFINE PUBLIC PROPERTY ParentBufferName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     *  Purpose: Gets and sets if an inner join should be used to the parent buffer
     *  Notes:   Optional, defaults to InnerJoin
     */
    DEFINE PUBLIC PROPERTY QueryJoin AS QueryJoinEnum NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the table name for the buffer spec
     * Notes:   May include a database prefix ("sports2000.customer")
     */
    DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Constructor for the BufferSpec class
     * Notes:
     * @param pcTableName The table name for which the buffer should be created
     * @param pcBufferName The alternative name for the dynamic buffer
     * @param poQueryJoin The QueryJoin criteria
     * @param pcParentBufferName The optional parent buffer name
     */
    CONSTRUCTOR PUBLIC BufferSpec (pcTableName AS CHARACTER,
                                   pcBufferName AS CHARACTER,
                                   poQueryJoin AS QueryJoinEnum,
                                   pcParentBufferName AS CHARACTER):

        THIS-OBJECT (pcTableName).

        Assert:NotNullOrEmpty (pcBufferName) .
        Assert:NotNullOrEmpty (pcParentBufferName) .

        ASSIGN THIS-OBJECT:BufferName       = pcBufferName
               THIS-OBJECT:ParentBufferName = pcParentBufferName .

        IF VALID-OBJECT (poQueryJoin) THEN
            ASSIGN THIS-OBJECT:QueryJoin = poQueryJoin .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferSpec class
     * Notes:   Assigns the BufferName to the TableName
     * @param pcTableName The table name for which the buffer should be created
     */
    CONSTRUCTOR PUBLIC BufferSpec (pcTableName AS CHARACTER):
        SUPER ().

        Assert:NotNullOrEmpty (pcTableName) .

        ASSIGN THIS-OBJECT:TableName  = pcTableName
               THIS-OBJECT:BufferName = ENTRY (NUM-ENTRIES (pcTableName, ".":U), pcTableName, ".":U)
               THIS-OBJECT:QueryJoin  = QueryJoinEnum:InnerJoin
            .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferSpec class
     * Notes:
     * @param pcTableName The table name for which the buffer should be created
     * @param pcBufferName The alternative name for the dynamic buffer
     */
    CONSTRUCTOR PUBLIC BufferSpec (pcTableName AS CHARACTER,
                                   pcBufferName AS CHARACTER):
        THIS-OBJECT (pcTableName).

        Assert:NotNullOrEmpty (pcBufferName) .

        ASSIGN THIS-OBJECT:BufferName = pcBufferName .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferSpec class
     * Notes:
     * @param pcTableName The table name for which the buffer should be created
     * @param poQueryJoin The QueryJoin criteria
     */
    CONSTRUCTOR PUBLIC BufferSpec (pcTableName AS CHARACTER,
                                   poQueryJoin AS QueryJoinEnum):
        THIS-OBJECT (pcTableName).

        IF VALID-OBJECT (poQueryJoin) THEN
            ASSIGN THIS-OBJECT:QueryJoin = poQueryJoin .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferSpec class
     * Notes:
     * @param pcTableName The table name for which the buffer should be created
     * @param pcBufferName The alternative name for the dynamic buffer
     * @param poQueryJoin The QueryJoin criteria
     */
    CONSTRUCTOR PUBLIC BufferSpec (pcTableName AS CHARACTER,
                                   pcBufferName AS CHARACTER,
                                   poQueryJoin AS QueryJoinEnum):
        THIS-OBJECT (pcTableName).

        Assert:NotNullOrEmpty (pcBufferName) .

        ASSIGN THIS-OBJECT:BufferName = pcBufferName
            .

        IF VALID-OBJECT (poQueryJoin) THEN
            ASSIGN THIS-OBJECT:QueryJoin = poQueryJoin .

    END CONSTRUCTOR.

    /**
     * Purpose: Constructor for the BufferSpec class
     * Notes:
     * @param pcTableName The table name for which the buffer should be created
     * @param poQueryJoin The QueryJoin criteria
     * @param pcParentBufferName The optional parent buffer name
     */
    CONSTRUCTOR PUBLIC BufferSpec (pcTableName AS CHARACTER,
                                   poQueryJoin AS QueryJoinEnum,
                                   pcParentBufferName AS CHARACTER):

        THIS-OBJECT (pcTableName).

        Assert:NotNullOrEmpty (pcParentBufferName) .

        ASSIGN THIS-OBJECT:ParentBufferName = pcParentBufferName .

        IF VALID-OBJECT (poQueryJoin) THEN
            ASSIGN THIS-OBJECT:QueryJoin = poQueryJoin .

    END CONSTRUCTOR.

END CLASS.
