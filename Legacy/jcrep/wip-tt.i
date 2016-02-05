DEFINE {1} SHARED TEMP-TABLE tt-rmtags NO-UNDO
    FIELD rmtagno AS CHAR FORMAT "x(20)"
    FIELD used AS LOG FORMAT "YES/NO" 
    FIELD lfqty LIKE loadtag.qty 
    FIELD origshtqty AS INT FORMAT "->>>,>>>,>>9" 
    FIELD availshtqty AS INT FORMAT "->>>,>>>,>>9" 
    INDEX idx1 AS UNIQUE rmtagno ASCENDING.

DEFINE {1} SHARED TEMP-TABLE tt-used-tags NO-UNDO
    FIELD rmtag LIKE loadtag.tag-no
    FIELD rmtag2 LIKE loadtag.tag-no
    FIELD rmtag3 LIKE loadtag.tag-no
    FIELD rm-i-no LIKE job-mat.rm-i-no
    FIELD frm LIKE job-mat.frm
    FIELD blank-no LIKE job-mat.blank-no
    FIELD usedshts AS INT FORMAT "->>>,>>>,>>9"
    FIELD processed AS LOG
    FIELD seq AS INT
    FIELD seq-processed AS LOG
    INDEX idx1 IS PRIMARY rmtag rmtag2 rmtag3 rm-i-no frm blank-no usedshts DESCENDING
    INDEX idx-seq IS UNIQUE seq seq-processed ASCENDING.

DEFINE {1} SHARED TEMP-TABLE tt-issued-wiptags NO-UNDO
    FIELD wiptagno LIKE wiptag.tag-no
    FIELD rmtagno1 LIKE loadtag.tag-no
    FIELD rmtagno2 LIKE loadtag.tag-no
    FIELD job-no LIKE job.job-no
    FIELD job-no2 LIKE job.job-no2
    FIELD rm-i-no LIKE job-mat.rm-i-no
    FIELD frm LIKE job-mat.frm
    FIELD blank-no LIKE job-mat.blank-no
    FIELD qty AS DEC FORMAT ">,>>>,>>9.9<<<<<"
    FIELD sts LIKE wiptag.sts
    INDEX idx1 IS PRIMARY wiptagno.


