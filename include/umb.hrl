-define(UMB_CMD_VERSION,      16#20).
-define(UMB_CMD_INFO,         16#2D).
-define(UMB_CMD_ONLINE_DATA,  16#23).
-define(UMB_CMD_MULTICHANNEL, 16#2F).

-define(UMB_FRM_SOH, 1).
-define(UMB_FRM_STX, 2).
-define(UMB_FRM_ETX, 3).
-define(UMB_FRM_EOT, 4).

-define(UMB_VER_1_0, 16#10).

-define(UMB_INF_CNL_NUM,  16#15).
-define(UMB_INF_CNL_LIST, 16#16).
-define(UMB_INF_CNL_FULL, 16#30).

-define(UMB_DEV_R2S,     2).
-define(UMB_DEV_MASTER, 15).

-define(UMB_CNL_R2S_TEMP_C,       100).
-define(UMB_CNL_R2S_LITRES,       600).
-define(UMB_CNL_R2S_LITRES_SINCE, 601).
-define(UMB_CNL_R2S_FILM,         610).
-define(UMB_CNL_R2S_FILM_SINCE,   611).
-define(UMB_CNL_R2S_PREC_TYPE,    700).
-define(UMB_CNL_R2S_DRIZZLES,     715).
-define(UMB_CNL_R2S_RDROPS,       720).
-define(UMB_CNL_R2S_SFLAKES,      725).
-define(UMB_CNL_R2S_HAILS,        730).
-define(UMB_CNL_R2S_DRIZZLE_F,    740).
-define(UMB_CNL_R2S_RAIN_F,       745).
-define(UMB_CNL_R2S_SNOW_F,       750).
-define(UMB_CNL_R2S_HAIL_F,       755).
-define(UMB_CNL_R2S_PREC_INT,     1053).

-define(UMB_DATA_UCHAR,  16).
-define(UMB_DATA_CHAR,   17).
-define(UMB_DATA_USHORT, 18).
-define(UMB_DATA_SHORT,  19).
-define(UMB_DATA_ULONG,  20).
-define(UMB_DATA_LONG,   21).
-define(UMB_DATA_FLOAT,  22).
-define(UMB_DATA_DOUBLE, 23).
