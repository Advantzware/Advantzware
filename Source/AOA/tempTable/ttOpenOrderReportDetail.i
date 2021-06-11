/* ttOpenOrderReportDetail.i */

/* Open Order Report Detail.rpa */ 
DEFINE TEMP-TABLE ttOpenOrderReportDetail NO-UNDO
    {AOA/tempTable/ttFields.i}
    FIELD xxcompany LIKE fg-bin.company
    FIELD xxi-no    LIKE fg-bin.i-no
    FIELD xxord-no  LIKE fg-bin.ord-no
    FIELD job-no    LIKE fg-bin.job-no
    FIELD job-no2   LIKE fg-bin.job-no2
    FIELD loc       LIKE fg-bin.loc
    FIELD loc-bin   LIKE fg-bin.loc-bin
    FIELD tag       LIKE fg-bin.tag
    FIELD qty       LIKE fg-bin.qty
    FIELD xxIndex     AS INTEGER LABEL "Index" FORMAT ">>>>>>9"
    .
RUN spSetSessionParam ("DetailTables", "1").
RUN spSetSessionParam ("DetailHandle1", TEMP-TABLE ttOpenOrderReportDetail:HANDLE).
