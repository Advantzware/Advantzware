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

INTERFACE Consultingwerk.OERA.IBufferDataSource:

    /**
     * Purpose: Returns if the Buffer Data Source is initialized already
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Initialized AS LOGICAL NO-UNDO
    GET.

    /**
     * Purpose: Returns the handle of the data-source object
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DataSourceHandle AS HANDLE NO-UNDO
    GET.

    /**
     * Purpose: Attaches the Data-Source to the ProDataset buffer object handle
     * Notes:
     * @param phProDatasetBuffer The handle of the ProDataset buffer
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PUBLIC LOGICAL AttachDataSource (phProDatasetBuffer AS HANDLE) .

    /**
     * Purpose: Returns the handle of the data-source object
     * Notes:   The implementation is required to remove and white space characters
     *          from pcPairsList that might have been inserted to improve readability
     *          but are not supported by the DATA-SOURCE
     * @param phProDatasetBuffer The handle of the ProDataset buffer
     * @param pcPairsList pairs-list An optional character expression that evaluates to a comma-delimited list of field name pairs that specify a mapping between different field names in the data-source object buffer and the ProDataSet temp-table buffer
     * @return Logical return value of the ATTACH-DATA-SOURCE method
     */
    METHOD PUBLIC LOGICAL AttachDataSource (phProDatasetBuffer AS HANDLE,
                                            pcPairsList AS CHARACTER) .

    /**
     * Purpose: Initializes the DATA-SOURCE object handle and supporting objects
     * Notes:
     */
    METHOD PUBLIC VOID Initialize () .

    /**
     * Purpose: Returns the database field name matching a temp-table field name
     *          contained in a consumers query string (query string vs. temp-table
     *          definition)
     * Notes:   To be used in the DataAccess:SourceColumn method
     * @param pcColumn The temp table column name to return the source (database table) field name for
     * @return The database field name mapped to the passed in temp-table field
     */
    METHOD PUBLIC CHARACTER SourceColumn (pcColumn AS CHARACTER) .

    /**
     * Purpose: Returns the default query string (USING OF) that can be used for the
     *          SourceDefaultQuery method of the DataAccess class
     * Notes:   Can not be used, when the buffers cannot be joined using OF
     * @return The query string for the source default query
     */
    METHOD PUBLIC CHARACTER SourceDefaultQuery () .

END INTERFACE .
