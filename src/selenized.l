#{
  Using selenized palette

  Guidelines for creating a selenized theme for your terminal emulator.

  Each Selenized variant defines 3 background shades (bg_*), 3 content shades (dim_*, fg_*) and 8 accent colors (each with a bright version). This document explains which color to use for what. You can find RGB values of the colors here.
  Color mapping
  Element 	Color
  background 	bg_0
  foreground 	fg_0
  bold foreground 	fg_1
  dim foreground 	dim_0
  cursor color 	fg_1
  selection 	bg_2
  selection text 	(none)

  Notes:

      Most terminals don't allow setting selection colors, they simply reverse foreground and background.
      Some terminals allow setting the opacity/brightness of special "dim" colors. This value should be set to 0.625.
      In case your terminal has a setting like "bright background", set it to bg_1.
      ANSI colors don't include orange and violet, so they are not used in terminal emulators - they are intended for GUI applications.

  ANSI palette:
  # 	ANSI color 	Selenized color
  0 	black 	bg_1
  1 	red 	red
  2 	green 	green
  3 	yellow 	yellow
  4 	blue 	blue
  5 	magenta 	magenta
  6 	cyan 	cyan
  7 	white 	dim_0
      
  8 	bright black 	bg_2
  9 	bright red 	br_red
  10 	bright green 	br_green
  11 	bright yellow 	br_yellow
  12 	bright blue 	br_blue
  13 	bright magenta 	br_magenta
  14 	bright cyan 	br_cyan
  15 	bright white 	fg_1
}#

(de hex-to-rgb-fl (C)
  (make
    (for (S (cdr (chop C)) S)
      (link (*/ 1.0 
                (format (pack (hex (pack (cut 2 'S))) ".0") *Scl)
                255.0)))))

#{
  Selenized dark

  Selenized dark screenshot

  Color        CIE L*a*b*   HSB           sRGB      AppleRGB
  ----------   ----------   -----------   -------   --------
  bg-0         23 -12 -12   193  77  28   #103c48   #112e38
  bg-1         28 -13 -13   193  72  34   #184956   #163945
  bg-2         36 -13 -13   194  57  41   #2d5b69   #254a57
  dim-0        56  -8  -6   191  20  56   #72898f   #61777c
  fg-0         75  -5  -2   182   8  74   #adbcbc   #9faeae
  fg-1         85  -5  -2   182   7  85   #cad8d9   #bfd0d0

  red          60  63  40     2  68  98   #fa5750   #f13c3e
  green        69 -38  55    92  70  73   #75b938   #69ad21
  yellow       75   6  68    46  79  86   #dbb32d   #d1a416
  blue         60   0 -57   213  72  97   #4695f7   #3a82f8
  magenta      66  55 -15   325  52  95   #f275be   #e75bb3
  cyan         73 -40  -4   174  67  78   #41c7b9   #42bdaa
  orange       67  37  50    22  69  93   #ed8649   #e26f35
  violet       64  30 -45   263  42  92   #af88eb   #9b72e9

  br-red       66  63  40     3  65 100   #ff665c   #ff4b49
  br-green     74 -38  55    92  65  78   #84c747   #78be2e
  br-yellow    80   6  68    46  74  92   #ebc13d   #e4b424
  br-blue      66   0 -57   214  66 100   #58a3ff   #4a91ff
  br-magenta   72  55 -15   325  49 100   #ff84cd   #fb69c4
  br-cyan      78 -40  -4   173  61  84   #53d6c7   #50cfba
  br-orange    72  37  50    22  66  99   #fd9456   #f67e41
  br-violet    69  30 -45   263  40  98   #bd96fa   #ab80fc
}#

(def '*seld-bg-0         (hex-to-rgb-fl "#112e38"))
(def '*seld-bg-1         (hex-to-rgb-fl "#163945"))
(def '*seld-bg-2         (hex-to-rgb-fl "#254a57"))
(def '*seld-dim-0        (hex-to-rgb-fl "#61777c"))
(def '*seld-fg-0         (hex-to-rgb-fl "#9faeae"))
(def '*seld-fg-1         (hex-to-rgb-fl "#bfd0d0"))

(def '*seld-red          (hex-to-rgb-fl "#f13c3e"))
(def '*seld-green        (hex-to-rgb-fl "#69ad21"))
(def '*seld-yellow       (hex-to-rgb-fl "#d1a416"))
(def '*seld-blue         (hex-to-rgb-fl "#3a82f8"))
(def '*seld-magenta      (hex-to-rgb-fl "#e75bb3"))
(def '*seld-cyan         (hex-to-rgb-fl "#42bdaa"))
(def '*seld-orange       (hex-to-rgb-fl "#e26f35"))
(def '*seld-violet       (hex-to-rgb-fl "#9b72e9"))

(def '*seld-br-red       (hex-to-rgb-fl "#ff4b49"))
(def '*seld-br-green     (hex-to-rgb-fl "#78be2e"))
(def '*seld-br-yellow    (hex-to-rgb-fl "#e4b424"))
(def '*seld-br-blue      (hex-to-rgb-fl "#4a91ff"))
(def '*seld-br-magenta   (hex-to-rgb-fl "#fb69c4"))
(def '*seld-br-cyan      (hex-to-rgb-fl "#50cfba"))
(def '*seld-br-orange    (hex-to-rgb-fl "#f67e41"))
(def '*seld-br-violet    (hex-to-rgb-fl "#ab80fc"))

#{
  Selenized black

  Selenized black screenshot

  Color        CIE L*a*b*   HSB           sRGB      AppleRGB
  ----------   ----------   -----------   -------   --------
  bg-0          8   0   0    35   0   9   #181818   #121212
  bg-1         15   0   0    35   0  15   #252525   #1c1c1c
  bg-2         25   0   0    35   0  23   #3b3b3b   #2d2d2d
  dim-0        50   0   0    35   0  47   #777777   #636363
  fg-0         75   0   0    35   0  72   #b9b9b9   #aaaaaa
  fg-1         88   0   0    35   0  87   #dedede   #d6d6d6

  red          56  63  40     1  70  93   #ed4a46   #e13136
  green        67 -38  55    92  72  71   #70b433   #64a81d
  yellow       75   6  68    46  79  86   #dbb32d   #d1a416
  blue         56   0 -57   212  77  92   #368aeb   #2d76e9
  magenta      64  55 -15   325  53  92   #eb6eb7   #de54ab
  cyan         72 -40  -4   174  68  77   #3fc5b7   #40bba8
  orange       64  37  50    22  71  90   #e67f43   #da6930
  violet       60  30 -45   263  43  88   #a580e2   #9169dd

  br-red       63  63  40     3  67 100   #ff5e56   #fb4343
  br-green     74 -38  55    92  65  78   #83c746   #77bd2d
  br-yellow    82   6  68    45  73  94   #efc541   #e9b928
  br-blue      63   0 -57   214  69 100   #4f9cfe   #4289ff
  br-magenta   71  55 -15   325  49 100   #ff81ca   #f767c0
  br-cyan      79 -40  -4   173  60  85   #56d8c9   #53d2bd
  br-orange    71  37  50    22  67  98   #fa9153   #f37b3f
  br-violet    67  30 -45   263  41  96   #b891f5   #a67bf5
}#

(def '*selb-bg-0       (hex-to-rgb-fl "#181818"))
(def '*selb-bg-1       (hex-to-rgb-fl "#252525"))
(def '*selb-bg-2       (hex-to-rgb-fl "#3b3b3b"))
(def '*selb-dim-0      (hex-to-rgb-fl "#777777"))
(def '*selb-fg-0       (hex-to-rgb-fl "#b9b9b9"))
(def '*selb-fg-1       (hex-to-rgb-fl "#dedede"))

(def '*selb-red        (hex-to-rgb-fl "#ed4a46"))
(def '*selb-green      (hex-to-rgb-fl "#70b433"))
(def '*selb-yellow     (hex-to-rgb-fl "#dbb32d"))
(def '*selb-blue       (hex-to-rgb-fl "#368aeb"))
(def '*selb-magenta    (hex-to-rgb-fl "#eb6eb7"))
(def '*selb-cyan       (hex-to-rgb-fl "#3fc5b7"))
(def '*selb-orange     (hex-to-rgb-fl "#e67f43"))
(def '*selb-violet     (hex-to-rgb-fl "#a580e2"))

(def '*selb-br-red     (hex-to-rgb-fl "#ff5e56"))
(def '*selb-br-green   (hex-to-rgb-fl "#83c746"))
(def '*selb-br-yellow  (hex-to-rgb-fl "#efc541"))
(def '*selb-br-blue    (hex-to-rgb-fl "#4f9cfe"))
(def '*selb-br-magenta (hex-to-rgb-fl "#ff81ca"))
(def '*selb-br-cyan    (hex-to-rgb-fl "#56d8c9"))
(def '*selb-br-orange  (hex-to-rgb-fl "#fa9153"))
(def '*selb-br-violet  (hex-to-rgb-fl "#b891f5"))

#{
  Selenized light

  Selenized light screenshot

  Color        CIE L*a*b*   HSB           sRGB      AppleRGB
  ----------   ----------   -----------   -------   --------
  bg-0         96   0  13    44  13  99   #fbf3db   #faf0d2
  bg-1         91   0  13    45  13  92   #ece3cc   #e7ddc0
  bg-2         82   0  13    45  15  83   #d5cdb6   #cbc2a6
  dim-0        62  -4   1   155   6  60   #909995   #7e8783
  fg-0         42  -6  -6   195  24  43   #53676d   #43545a
  fg-1         31  -6  -6   195  30  33   #3a4d53   #2d3c42

  red          46  66  42   356  84  82   #d2212d   #c00221
  green        54 -40  58    90 100  57   #489100   #3f8100
  yellow       59   6  71    47 100  68   #ad8900   #9b7600
  blue         46   0 -60   208 100  83   #0072d4   #005dcc
  magenta      52  58 -16   323  64  79   #ca4898   #b73088
  cyan         57 -42  -4   175 100  61   #009c8f   #038d7c
  orange       52  39  52    23  84  76   #c25d1e   #b04713
  violet       49  32 -47   262  50  78   #8762c6   #714cbc

  br-red       44  66  42   354  89  80   #cc1729   #b9001e
  br-green     52 -40  58    92 100  55   #428b00   #3a7b00
  br-yellow    57   6  71    47 100  66   #a78300   #957000
  br-blue      44   0 -60   208 100  81   #006dce   #0059c6
  br-magenta   50  58 -16   323  66  77   #c44392   #b12b82
  br-cyan      55 -42  -4   175 100  59   #00978a   #008777
  br-orange    50  39  52    23  87  74   #bc5819   #a9430f
  br-violet    47  32 -47   262  51  75   #825dc0   #6b47b6
}#

(def '*sell-bg-0         (hex-to-rgb-fl "#fbf3db"))
(def '*sell-bg-1         (hex-to-rgb-fl "#ece3cc"))
(def '*sell-bg-2         (hex-to-rgb-fl "#d5cdb6"))
(def '*sell-dim-0        (hex-to-rgb-fl "#909995"))
(def '*sell-fg-0         (hex-to-rgb-fl "#53676d"))
(def '*sell-fg-1         (hex-to-rgb-fl "#3a4d53"))

(def '*sell-red          (hex-to-rgb-fl "#d2212d"))
(def '*sell-green        (hex-to-rgb-fl "#489100"))
(def '*sell-yellow       (hex-to-rgb-fl "#ad8900"))
(def '*sell-blue         (hex-to-rgb-fl "#0072d4"))
(def '*sell-magenta      (hex-to-rgb-fl "#ca4898"))
(def '*sell-cyan         (hex-to-rgb-fl "#009c8f"))
(def '*sell-orange       (hex-to-rgb-fl "#c25d1e"))
(def '*sell-violet       (hex-to-rgb-fl "#8762c6"))

(def '*sell-br-red       (hex-to-rgb-fl "#cc1729"))
(def '*sell-br-green     (hex-to-rgb-fl "#428b00"))
(def '*sell-br-yellow    (hex-to-rgb-fl "#a78300"))
(def '*sell-br-blue      (hex-to-rgb-fl "#006dce"))
(def '*sell-br-magenta   (hex-to-rgb-fl "#c44392"))
(def '*sell-br-cyan      (hex-to-rgb-fl "#00978a"))
(def '*sell-br-orange    (hex-to-rgb-fl "#bc5819"))
(def '*sell-br-violet    (hex-to-rgb-fl "#825dc0"))

#{
  Selenized white

  Selenized white screenshot

  Color        CIE L*a*b*   HSB           sRGB      AppleRGB
  ----------   ----------   -----------   -------   --------
  bg-0        100   0   0    35   0 100   #ffffff   #ffffff
  bg-1         93   0   0    35   0  92   #ebebeb   #e6e6e6
  bg-2         82   0   0    35   0  80   #cdcdcd   #c2c2c2
  dim-0        56   0   0    35   0  53   #878787   #747474
  fg-0         30   0   0    35   0  28   #474747   #373737
  fg-1         16   0   0    35   0  16   #282828   #1e1e1e

  red          40  88  56   357 100  84   #d6000c   #c5000d
  green        54 -53  77   108 100  59   #1d9700   #288800
  yellow       65   8  95    46 100  77   #c49700   #b58400
  blue         40   0 -80   214 100  89   #0064e4   #004fe0
  magenta      50  77 -21   318  93  87   #dd0f9d   #cc008e
  cyan         61 -56  -6   174 100  68   #00ad9c   #00a08a
  orange       51  52  70    21 100  82   #d04a00   #bf3400
  violet       45  42 -63   261  62  84   #7f51d6   #673ad0

  br-red       33  88  56     0 100  75   #bf0000   #aa0000
  br-green     47 -53  77   120 100  52   #008400   #147300
  br-yellow    58   8  95    46 100  69   #af8500   #9d7100
  br-blue      33   0 -80   216 100  81   #0054cf   #0040c8
  br-magenta   43  77 -21   318 100  78   #c7008b   #b3007a
  br-cyan      54 -56  -6   174 100  60   #009a8a   #008a77
  br-orange    44  52  70    18 100  73   #ba3700   #a62300
  br-violet    38  42 -63   260  67  76   #6b40c3   #542bb9
}#

(def '*selw-bg-0        (hex-to-rgb-fl "#ffffff"))
(def '*selw-bg-1        (hex-to-rgb-fl "#ebebeb"))
(def '*selw-bg-2        (hex-to-rgb-fl "#cdcdcd"))
(def '*selw-dim-0       (hex-to-rgb-fl "#878787"))
(def '*selw-fg-0        (hex-to-rgb-fl "#474747"))
(def '*selw-fg-1        (hex-to-rgb-fl "#282828"))

(def '*selw-red         (hex-to-rgb-fl "#d6000c"))
(def '*selw-green       (hex-to-rgb-fl "#1d9700"))
(def '*selw-yellow      (hex-to-rgb-fl "#c49700"))
(def '*selw-blue        (hex-to-rgb-fl "#0064e4"))
(def '*selw-magenta     (hex-to-rgb-fl "#dd0f9d"))
(def '*selw-cyan        (hex-to-rgb-fl "#00ad9c"))
(def '*selw-orange      (hex-to-rgb-fl "#d04a00"))
(def '*selw-violet      (hex-to-rgb-fl "#7f51d6"))

(def '*selw-br-red      (hex-to-rgb-fl "#bf0000"))
(def '*selw-br-green    (hex-to-rgb-fl "#008400"))
(def '*selw-br-yellow   (hex-to-rgb-fl "#af8500"))
(def '*selw-br-blue     (hex-to-rgb-fl "#0054cf"))
(def '*selw-br-magenta  (hex-to-rgb-fl "#c7008b"))
(def '*selw-br-cyan     (hex-to-rgb-fl "#009a8a"))
(def '*selw-br-orange   (hex-to-rgb-fl "#ba3700"))
(def '*selw-br-violet   (hex-to-rgb-fl "#6b40c3"))
