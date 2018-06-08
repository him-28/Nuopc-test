function main(args)

* Script Arguments
if  (args!='')
levels  = subwrd(args,1)
else
exit 'ERROR: Usage run <script> <levels>'
endif

palette = "ERROR"

* Palletes
if (levels = 12)
 'set rgb 31 204 0 255'
 'set rgb 32 98 42 255'
 'set rgb 33 54 80 204'
 'set rgb 34 26 117 139'
 'set rgb 35 2 154 76'
 'set rgb 36 67 180 56'
 'set rgb 37 131 206 37'
 'set rgb 38 196 232 17'
 'set rgb 39 255 248 0'
 'set rgb 40 255 159 0'
 'set rgb 41 255 71 0'
 'set rgb 42 238 0 0'
 'set rgb 43 153 0 0'
 palette = "31 32 33 34 35 36 37 38 39 40 41 42 43"
endif
if (levels = 15)
 'set rgb 31 204 0 255'
 'set rgb 32 119 34 255'
 'set rgb 33 65 66 230'
 'set rgb 34 43 95 178'
 'set rgb 35 21 125 126'
 'set rgb 36 2 154 76'
 'set rgb 37 54 175 60'
 'set rgb 38 106 196 44'
 'set rgb 39 157 216 29'
 'set rgb 40 209 237 13'
 'set rgb 41 255 248 0'
 'set rgb 42 255 177 0'
 'set rgb 43 255 106 0'
 'set rgb 44 255 35 0'
 'set rgb 45 221 0 0'
 'set rgb 46 153 0 0'
 palette = "31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46"
endif
if (levels = 25)
 'set rgb 31 202 39 251 '
 'set rgb 32 152 44 251 '
 'set rgb 33 102 55 251 '
 'set rgb 34 70 68 235 '
 'set rgb 35 58 82 205 '
 'set rgb 36 46 97 175 '
 'set rgb 37 35 113 145 '
 'set rgb 38 26 130 115 '
 'set rgb 39 22 147 86 '
 'set rgb 40 35 161 73 '
 'set rgb 41 60 172 67 '
 'set rgb 42 89 185 61 '
 'set rgb 43 118 197 56 '
 'set rgb 44 148 209 53 '
 'set rgb 45 178 222 52 '
 'set rgb 46 208 234 52 '
 'set rgb 47 239 246 54 '
 'set rgb 48 254 231 52 '
 'set rgb 49 253 190 44 '
 'set rgb 50 253 147 38 '
 'set rgb 51 252 106 33 '
 'set rgb 52 252 65 29 '
 'set rgb 53 252 28 28 '
 'set rgb 54 231 11 24 '
 'set rgb 55 191 7 18 '
 'set rgb 56 151 4 12 '
 palette = "31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56"
endif

return palette

