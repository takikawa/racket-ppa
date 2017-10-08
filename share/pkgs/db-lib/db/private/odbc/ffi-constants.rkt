#lang racket/base
(provide (all-defined-out))

;; ============================================================

(define SQL_MAX_MESSAGE_LENGTH    512)
(define SQL_MAX_NUMERIC_LEN       16)

(define SQL_HANDLE_ENV            1)
(define SQL_HANDLE_DBC            2)
(define SQL_HANDLE_STMT           3)
(define SQL_HANDLE_DESC           4)

(define SQL_DRIVER_NOPROMPT       0)
(define SQL_DRIVER_COMPLETE       1)
(define SQL_DRIVER_PROMPT         2)
(define SQL_DRIVER_COMPLETE_REQUIRED 3)

(define SQL_IS_POINTER           -4)
(define SQL_IS_UINTEGER          -5)
(define SQL_IS_INTEGER           -6)
(define SQL_IS_USMALLINT         -7)
(define SQL_IS_SMALLINT          -8)

(define SQL_ATTR_ODBC_VERSION   200)
(define SQL_OV_ODBC2              2)
(define SQL_OV_ODBC3              3)

(define SQL_SUCCESS               0)
(define SQL_SUCCESS_WITH_INFO     1)
(define SQL_STILL_EXECUTING       2)
(define SQL_ERROR                -1)
(define SQL_INVALID_HANDLE       -2)
(define SQL_NEED_DATA            99)
(define SQL_NO_DATA             100)

(define SQL_NULL_DATA            -1)
(define SQL_DATA_AT_EXEC         -2)
(define SQL_NO_TOTAL             -4)

(define SQL_UNKNOWN_TYPE          0)
(define SQL_CHAR                  1)
(define SQL_NUMERIC               2)
(define SQL_DECIMAL               3)
(define SQL_INTEGER               4)
(define SQL_SMALLINT              5)
(define SQL_FLOAT                 6)
(define SQL_REAL                  7)
(define SQL_DOUBLE                8)
(define SQL_DATETIME              9)
(define SQL_VARCHAR              12)
(define SQL_TYPE_DATE            91)
(define SQL_TYPE_TIME            92)
(define SQL_TYPE_TIMESTAMP       93)

(define SQL_DATE                  9)
(define SQL_TIME                 10)
(define SQL_TIMESTAMP            11)
(define SQL_LONGVARCHAR          -1)
(define SQL_BINARY               -2)
(define SQL_VARBINARY            -3)
(define SQL_LONGVARBINARY        -4)
(define SQL_BIGINT               -5)
(define SQL_TINYINT              -6)
(define SQL_BIT                  -7)

(define SQL_WCHAR                -8)
(define SQL_WVARCHAR             -9)
(define SQL_WLONGVARCHAR        -10)

(define SQL_CODE_YEAR             1)
(define SQL_CODE_MONTH            2)
(define SQL_CODE_DAY              3)
(define SQL_CODE_HOUR             4)
(define SQL_CODE_MINUTE           5)
(define SQL_CODE_SECOND           6)
(define SQL_CODE_YEAR_TO_MONTH    7)
(define SQL_CODE_DAY_TO_HOUR      8)
(define SQL_CODE_DAY_TO_MINUTE    9)
(define SQL_CODE_DAY_TO_SECOND   10)
(define SQL_CODE_HOUR_TO_MINUTE  11)
(define SQL_CODE_HOUR_TO_SECOND  12)
(define SQL_CODE_MINUTE_TO_SECOND 13)

(define SQL_INTERVAL_YEAR               (+ 100 SQL_CODE_YEAR))
(define SQL_INTERVAL_MONTH              (+ 100 SQL_CODE_MONTH))
(define SQL_INTERVAL_DAY                (+ 100 SQL_CODE_DAY))
(define SQL_INTERVAL_HOUR               (+ 100 SQL_CODE_HOUR))
(define SQL_INTERVAL_MINUTE             (+ 100 SQL_CODE_MINUTE))
(define SQL_INTERVAL_SECOND             (+ 100 SQL_CODE_SECOND))
(define SQL_INTERVAL_YEAR_TO_MONTH      (+ 100 SQL_CODE_YEAR_TO_MONTH))
(define SQL_INTERVAL_DAY_TO_HOUR        (+ 100 SQL_CODE_DAY_TO_HOUR))
(define SQL_INTERVAL_DAY_TO_MINUTE      (+ 100 SQL_CODE_DAY_TO_MINUTE))
(define SQL_INTERVAL_DAY_TO_SECOND      (+ 100 SQL_CODE_DAY_TO_SECOND))
(define SQL_INTERVAL_HOUR_TO_MINUTE     (+ 100 SQL_CODE_HOUR_TO_MINUTE))
(define SQL_INTERVAL_HOUR_TO_SECOND     (+ 100 SQL_CODE_HOUR_TO_SECOND))
(define SQL_INTERVAL_MINUTE_TO_SECOND   (+ 100 SQL_CODE_MINUTE_TO_SECOND))

(define SQL_DATE_LEN             10)
(define SQL_TIME_LEN              8)
(define SQL_TIMESTAMP_LEN        19)

(define SQL_NULL_HENV             0)
(define SQL_NULL_HDBC             0)
(define SQL_NULL_HSTMT            0)
(define SQL_NULL_HDESC            0)
(define SQL_NULL_HANDLE           0)

(define SQL_FETCH_NEXT            1)
(define SQL_FETCH_FIRST           2)

(define SQL_FETCH_LAST            3)
(define SQL_FETCH_PRIOR           4)
(define SQL_FETCH_ABSOLUTE        5)
(define SQL_FETCH_RELATIVE        6)

(define SQL_CLOSE                 0)
(define SQL_DROP                  1)
(define SQL_UNBIND                2)
(define SQL_RESET_PARAMS          3)

(define SQL_COMMIT                0)
(define SQL_ROLLBACK              1)

(define SQL_C_CHAR                SQL_CHAR)
(define SQL_C_LONG                SQL_INTEGER)
(define SQL_C_SHORT               SQL_SMALLINT)
(define SQL_C_FLOAT               SQL_REAL)
(define SQL_C_DOUBLE              SQL_DOUBLE)
(define SQL_C_NUMERIC             SQL_NUMERIC)
(define SQL_C_DEFAULT             99)
(define SQL_ARD_TYPE              -99)

(define SQL_C_DATE                SQL_DATE)
(define SQL_C_TIME                SQL_TIME)
(define SQL_C_TIMESTAMP           SQL_TIMESTAMP)
(define SQL_C_BINARY              SQL_BINARY)
(define SQL_C_BIT                 SQL_BIT)
(define SQL_C_TINYINT             SQL_TINYINT)
#|
(define SQL_C_SLONG               (+ SQL_C_LONG SQL_SIGNED_OFFSET))
(define SQL_C_SSHORT              (+ SQL_C_SHORT SQL_SIGNED_OFFSET))
(define SQL_C_STINYINT            (+ SQL_TINYINT SQL_SIGNED_OFFSET))
(define SQL_C_ULONG               (+ SQL_C_LONG SQL_UNSIGNED_OFFSET))
(define SQL_C_USHORT              (+ SQL_C_SHORT SQL_UNSIGNED_OFFSET))
(define SQL_C_UTINYINT            (+ SQL_TINYINT SQL_UNSIGNED_OFFSET))
|#

(define SQL_C_WCHAR               SQL_WCHAR)

(define SQL_SIGNED_OFFSET         -20)
(define SQL_UNSIGNED_OFFSET       -22)

(define SQL_C_TYPE_DATE           SQL_TYPE_DATE)
(define SQL_C_TYPE_TIME           SQL_TYPE_TIME)
(define SQL_C_TYPE_TIMESTAMP      SQL_TYPE_TIMESTAMP)

(define SQL_C_INTERVAL_YEAR       SQL_INTERVAL_YEAR)
(define SQL_C_INTERVAL_MONTH      SQL_INTERVAL_MONTH)
(define SQL_C_INTERVAL_DAY        SQL_INTERVAL_DAY)
(define SQL_C_INTERVAL_HOUR       SQL_INTERVAL_HOUR)
(define SQL_C_INTERVAL_MINUTE     SQL_INTERVAL_MINUTE)
(define SQL_C_INTERVAL_SECOND     SQL_INTERVAL_SECOND)
(define SQL_C_INTERVAL_YEAR_TO_MONTH      SQL_INTERVAL_YEAR_TO_MONTH)
(define SQL_C_INTERVAL_DAY_TO_HOUR        SQL_INTERVAL_DAY_TO_HOUR)
(define SQL_C_INTERVAL_DAY_TO_MINUTE      SQL_INTERVAL_DAY_TO_MINUTE)
(define SQL_C_INTERVAL_DAY_TO_SECOND      SQL_INTERVAL_DAY_TO_SECOND)
(define SQL_C_INTERVAL_HOUR_TO_MINUTE     SQL_INTERVAL_HOUR_TO_MINUTE)
(define SQL_C_INTERVAL_HOUR_TO_SECOND     SQL_INTERVAL_HOUR_TO_SECOND)
(define SQL_C_INTERVAL_MINUTE_TO_SECOND   SQL_INTERVAL_MINUTE_TO_SECOND)

(define SQL_C_SBIGINT             (+ SQL_BIGINT SQL_SIGNED_OFFSET))
(define SQL_C_UBIGINT             (+ SQL_BIGINT SQL_UNSIGNED_OFFSET))
(define SQL_C_VARBOOKMARK         SQL_C_BINARY)

(define SQL_TYPE_NULL             0)

(define SQL_PARAM_TYPE_UNKNOWN    0)
(define SQL_PARAM_INPUT           1)
(define SQL_PARAM_INPUT_OUTPUT    2)
(define SQL_RESULT_COL            3)
(define SQL_PARAM_OUTPUT          4)
(define SQL_RETURN_VALUE          5)

(define SQL_API_SQLDESCRIBEPARAM 58)

(define SQL_ATTR_AUTOCOMMIT     102)
(define SQL_AUTOCOMMIT_OFF        0)
(define SQL_AUTOCOMMIT_ON         1)

(define SQL_DEFAULT_TXN_ISOLATION 26)
(define SQL_TXN_ISOLATION_OPTION  72)
(define SQL_ATTR_TXN_ISOLATION   108)
(define SQL_TXN_READ_UNCOMMITTED #x1)
(define SQL_TXN_READ_COMMITTED   #x2)
(define SQL_TXN_REPEATABLE_READ  #x4)
(define SQL_TXN_SERIALIZABLE     #x8)

(define SQL_DBMS_NAME 17)

(define SQL_ATTR_APP_PARAM_DESC 10011)
(define SQL_ATTR_APP_ROW_DESC 10010)
(define SQL_DESC_TYPE 1002)
(define SQL_DESC_PRECISION 1005)
(define SQL_DESC_SCALE 1006)
(define SQL_DESC_DATA_PTR 1010)