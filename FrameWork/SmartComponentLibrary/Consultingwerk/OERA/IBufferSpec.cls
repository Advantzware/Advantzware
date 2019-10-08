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
    File        : IBufferSpec
    Purpose     : Describes a dynamic buffer
    Syntax      :
    Description : Interface for parameter objects to the BufferDataSource
                  class
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jun 15 20:36:55 CEST 2016
    Notes       : See http://confluence.consultingwerkcloud.com/wiki/display/SCL/New+API+for+dynamic+DATA-SOURCE+objects
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.* FROM PROPATH .
USING Progress.Lang.*       FROM PROPATH .

INTERFACE Consultingwerk.OERA.IBufferSpec:

    /**
     * Purpose: Gets and sets the optional buffer name for the buffer spec
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BufferName AS CHARACTER NO-UNDO
    GET.

    /**
     *  Purpose: Gets and sets the optional parent buffer name for the buffer spec
     *  Notes:   When the parent ParentBufferName is not set, the first query buffer
     *           is assumed
     */
    DEFINE PUBLIC PROPERTY ParentBufferName AS CHARACTER NO-UNDO
    GET.

    /**
     *  Purpose: Gets and sets if an inner join should be used to the parent buffer
     *  Notes:   Optional, defaults to InnerJoin
     */
    DEFINE PUBLIC PROPERTY QueryJoin AS QueryJoinEnum NO-UNDO
    GET.

    /**
     * Purpose: Gets and sets the table name for the buffer spec
     * Notes:   May include a database prefix ("sports2000.customer")
     */
    DEFINE PUBLIC PROPERTY TableName AS CHARACTER NO-UNDO
    GET.

END INTERFACE .
