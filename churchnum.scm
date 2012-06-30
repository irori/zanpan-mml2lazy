(lazy-def 0 '(lambda (f) (lambda (x) x)))  ; 3B
(lazy-def 1 '(lambda (f) f))  ; 1B
(lazy-def 2 '(|1+| 1))  ; 11B
(lazy-def 3 '(|1+| 2))  ; 18B
(lazy-def 4 '((lambda (x) (x x)) 2))  ; 16B
(lazy-def 5 '(|1+| 4))  ; 26B
(lazy-def 6 '(oblong 2))  ; 22B
(lazy-def 7 '(|1+| 6))  ; 26B
(lazy-def 8 '(* 2 4))  ; 26B
(lazy-def 9 '(^ 3 2))  ; 21B
(lazy-def 10 '(|1+| 9))  ; 25B
(lazy-def 11 '(|1+| 10))  ; 29B
(lazy-def 12 '(oblong 3))  ; 26B
(lazy-def 13 '(|1+| 12))  ; 30B
(lazy-def 14 '(|1+| 13))  ; 34B
(lazy-def 15 '(|1+| 14))  ; 38B
(lazy-def 16 '((lambda (x) ((x x) x)) 2))  ; 20B
(lazy-def 17 '(|1+| 16))  ; 30B
(lazy-def 18 '(+ 9 9))  ; 35B
(lazy-def 19 '(+ 9 10))  ; 37B
(lazy-def 20 '(* 4 5))  ; 28B
(lazy-def 21 '(|1+| 20))  ; 37B
(lazy-def 22 '(|1+| 21))  ; 41B
(lazy-def 23 '(|1+| 22))  ; 45B
(lazy-def 24 '(+ 12 12))  ; 40B
(lazy-def 25 '(^ 5 2))  ; 34B
(lazy-def 26 '(|1+| 25))  ; 44B
(lazy-def 27 '((lambda (x) (x x)) 3))  ; 23B
(lazy-def 28 '(|1+| 27))  ; 32B
(lazy-def 29 '(|1+| 28))  ; 36B
(lazy-def 30 '(oblong 5))  ; 37B
(lazy-def 31 '(|1+| 30))  ; 41B
(lazy-def 32 '(* 2 16))  ; 30B
(lazy-def 33 '(|1+| 32))  ; 36B
(lazy-def 34 '(|1+| 33))  ; 40B
(lazy-def 35 '(|1+| 34))  ; 44B
(lazy-def 36 '(^ 6 2))  ; 25B
(lazy-def 37 '(|1+| 36))  ; 29B
(lazy-def 38 '(|1+| 37))  ; 33B
(lazy-def 39 '(|1+| 38))  ; 37B
(lazy-def 40 '(|1+| 39))  ; 41B
(lazy-def 41 '(|1+| 40))  ; 45B
(lazy-def 42 '(oblong 6))  ; 30B
(lazy-def 43 '(|1+| 42))  ; 34B
(lazy-def 44 '(|1+| 43))  ; 38B
(lazy-def 45 '(|1+| 44))  ; 42B
(lazy-def 46 '(|1+| 45))  ; 46B
(lazy-def 47 '(|1+| 46))  ; 50B
(lazy-def 48 '(+ 6 42))  ; 42B
(lazy-def 49 '(^ 7 2))  ; 33B
(lazy-def 50 '(|1+| 49))  ; 37B
(lazy-def 51 '(|1+| 50))  ; 41B
(lazy-def 52 '(|1+| 51))  ; 45B
(lazy-def 53 '(|1+| 52))  ; 49B
(lazy-def 54 '(* 2 27))  ; 39B
(lazy-def 55 '(+ 54 1))  ; 43B
(lazy-def 56 '(oblong 7))  ; 34B
(lazy-def 57 '(|1+| 56))  ; 38B
(lazy-def 58 '(|1+| 57))  ; 42B
(lazy-def 59 '(|1+| 58))  ; 46B
(lazy-def 60 '(|1+| 59))  ; 50B
(lazy-def 61 '(|1+| 60))  ; 54B
(lazy-def 62 '(+ 6 56))  ; 50B
(lazy-def 63 '(* 7 9))  ; 52B
(lazy-def 64 '(^ 4 3))  ; 28B
(lazy-def 65 '(|1+| 64))  ; 34B
(lazy-def 66 '(|1+| 65))  ; 38B
(lazy-def 67 '(|1+| 66))  ; 42B
(lazy-def 68 '(|1+| 67))  ; 46B
(lazy-def 69 '(|1+| 68))  ; 50B
(lazy-def 70 '(|1+| 69))  ; 54B
(lazy-def 71 '(|1+| 70))  ; 58B
(lazy-def 72 '(oblong 8))  ; 36B
(lazy-def 73 '(|1+| 72))  ; 40B
(lazy-def 74 '(|1+| 73))  ; 44B
(lazy-def 75 '(|1+| 74))  ; 48B
(lazy-def 76 '(* 2 38))  ; 49B
(lazy-def 77 '(+ 76 1))  ; 48B
(lazy-def 78 '(* 2 39))  ; 53B
(lazy-def 79 '(+ 78 1))  ; 52B
(lazy-def 80 '(* 4 20))  ; 32B
(lazy-def 81 '(^ 9 2))  ; 25B
(lazy-def 82 '(|1+| 81))  ; 29B
(lazy-def 83 '(|1+| 82))  ; 33B
(lazy-def 84 '(|1+| 83))  ; 37B
(lazy-def 85 '(|1+| 84))  ; 41B
(lazy-def 86 '(|1+| 85))  ; 45B
(lazy-def 87 '(+ 6 81))  ; 47B
(lazy-def 88 '(|1+| 87))  ; 51B
(lazy-def 89 '(+ 81 8))  ; 53B
(lazy-def 90 '(oblong 9))  ; 29B
(lazy-def 91 '(|1+| 90))  ; 33B
(lazy-def 92 '(|1+| 91))  ; 37B
(lazy-def 93 '(|1+| 92))  ; 41B
(lazy-def 94 '(|1+| 93))  ; 45B
(lazy-def 95 '(|1+| 94))  ; 49B
(lazy-def 96 '(* 6 16))  ; 47B
(lazy-def 97 '(+ 81 16))  ; 49B
(lazy-def 98 '(* 2 49))  ; 49B
(lazy-def 99 '(+ 9 90))  ; 41B
(lazy-def 100 '(^ 10 2))  ; 33B
(lazy-def 101 '(|1+| 100))  ; 37B
(lazy-def 102 '(|1+| 101))  ; 41B
(lazy-def 103 '(|1+| 102))  ; 45B
(lazy-def 104 '(|1+| 103))  ; 49B
(lazy-def 105 '(|1+| 104))  ; 53B
(lazy-def 106 '(+ 90 16))  ; 53B
(lazy-def 107 '(+ 91 16))  ; 57B
(lazy-def 108 '(* 4 27))  ; 44B
(lazy-def 109 '(+ 108 1))  ; 49B
(lazy-def 110 '(oblong 10))  ; 33B
(lazy-def 111 '(|1+| 110))  ; 37B
(lazy-def 112 '(|1+| 111))  ; 41B
(lazy-def 113 '(|1+| 112))  ; 45B
(lazy-def 114 '(|1+| 113))  ; 49B
(lazy-def 115 '(|1+| 114))  ; 53B
(lazy-def 116 '(+ 6 110))  ; 55B
(lazy-def 117 '(+ 36 81))  ; 50B
(lazy-def 118 '(|1+| 117))  ; 54B
(lazy-def 119 '(+ 9 110))  ; 49B
(lazy-def 120 '(* 4 30))  ; 49B
(lazy-def 121 '(^ 11 2))  ; 39B
(lazy-def 122 '(|1+| 121))  ; 43B
(lazy-def 123 '(|1+| 122))  ; 47B
(lazy-def 124 '(|1+| 123))  ; 51B
(lazy-def 125 '(^ 5 3))  ; 43B
(lazy-def 126 '(|1+| 125))  ; 47B
(lazy-def 127 '(|1+| 126))  ; 51B
(lazy-def 128 '(* 2 64))  ; 34B
(lazy-def 129 '(|1+| 128))  ; 38B
(lazy-def 130 '(|1+| 129))  ; 42B
(lazy-def 131 '(|1+| 130))  ; 46B
(lazy-def 132 '(oblong 11))  ; 37B
(lazy-def 133 '(|1+| 132))  ; 41B
(lazy-def 134 '(|1+| 133))  ; 45B
(lazy-def 135 '(|1+| 134))  ; 49B
(lazy-def 136 '(|1+| 135))  ; 53B
(lazy-def 137 '(|1+| 136))  ; 57B
(lazy-def 138 '(+ 6 132))  ; 59B
(lazy-def 139 '(+ 49 90))  ; 62B
(lazy-def 140 '(* 7 20))  ; 59B
(lazy-def 141 '(+ 9 132))  ; 58B
(lazy-def 142 '(+ 10 132))  ; 53B
(lazy-def 143 '(+ 132 11))  ; 55B
(lazy-def 144 '(^ 12 2))  ; 33B
(lazy-def 145 '(|1+| 144))  ; 37B
(lazy-def 146 '(|1+| 145))  ; 41B
(lazy-def 147 '(|1+| 146))  ; 45B
(lazy-def 148 '(|1+| 147))  ; 49B
(lazy-def 149 '(|1+| 148))  ; 53B
(lazy-def 150 '(+ 6 144))  ; 55B
(lazy-def 151 '(|1+| 150))  ; 59B
(lazy-def 152 '(* 4 38))  ; 54B
(lazy-def 153 '(+ 9 144))  ; 54B
(lazy-def 154 '(|1+| 153))  ; 58B
(lazy-def 155 '(|1+| 154))  ; 62B
(lazy-def 156 '(oblong 12))  ; 34B
(lazy-def 157 '(|1+| 156))  ; 38B
(lazy-def 158 '(|1+| 157))  ; 42B
(lazy-def 159 '(|1+| 158))  ; 46B
(lazy-def 160 '(* 2 80))  ; 47B
(lazy-def 161 '(|1+| 160))  ; 53B
(lazy-def 162 '(+ 81 81))  ; 39B
(lazy-def 163 '(|1+| 162))  ; 43B
(lazy-def 164 '(* 2 82))  ; 45B
(lazy-def 165 '(+ 164 1))  ; 44B
(lazy-def 166 '(* 2 83))  ; 49B
(lazy-def 167 '(+ 166 1))  ; 48B
(lazy-def 168 '(* 4 42))  ; 51B
(lazy-def 169 '(^ 13 2))  ; 40B
(lazy-def 170 '(|1+| 169))  ; 44B
(lazy-def 171 '(|1+| 170))  ; 48B
(lazy-def 172 '(|1+| 171))  ; 52B
(lazy-def 173 '(|1+| 172))  ; 56B
(lazy-def 174 '(|1+| 173))  ; 60B
(lazy-def 175 '(+ 6 169))  ; 62B
(lazy-def 176 '(* 11 16))  ; 54B
(lazy-def 177 '(+ 176 1))  ; 59B
(lazy-def 178 '(+ 81 97))  ; 59B
(lazy-def 179 '(+ 9 170))  ; 65B
(lazy-def 180 '(* 2 90))  ; 45B
(lazy-def 181 '(+ 180 1))  ; 44B
(lazy-def 182 '(oblong 13))  ; 38B
(lazy-def 183 '(|1+| 182))  ; 42B
(lazy-def 184 '(|1+| 183))  ; 46B
(lazy-def 185 '(|1+| 184))  ; 50B
(lazy-def 186 '(|1+| 185))  ; 54B
(lazy-def 187 '(|1+| 186))  ; 58B
(lazy-def 188 '(+ 6 182))  ; 60B
(lazy-def 189 '(* 7 27))  ; 54B
(lazy-def 190 '(+ 189 1))  ; 55B
(lazy-def 191 '(+ 81 110))  ; 58B
(lazy-def 192 '(* 3 64))  ; 51B
(lazy-def 193 '(+ 192 1))  ; 53B
(lazy-def 194 '(+ 12 182))  ; 54B
(lazy-def 195 '(+ 182 13))  ; 56B
(lazy-def 196 '(^ 14 2))  ; 44B
(lazy-def 197 '(|1+| 196))  ; 48B
(lazy-def 198 '(|1+| 197))  ; 52B
(lazy-def 199 '(|1+| 198))  ; 56B
(lazy-def 200 '(* 2 100))  ; 49B
(lazy-def 201 '(+ 200 1))  ; 48B
(lazy-def 202 '(* 2 101))  ; 53B
(lazy-def 203 '(+ 202 1))  ; 52B
(lazy-def 204 '(* 2 102))  ; 57B
(lazy-def 205 '(+ 204 1))  ; 56B
(lazy-def 206 '(* 2 103))  ; 61B
(lazy-def 207 '(+ 206 1))  ; 60B
(lazy-def 208 '(* 13 16))  ; 55B
(lazy-def 209 '(+ 208 1))  ; 60B
(lazy-def 210 '(oblong 14))  ; 42B
(lazy-def 211 '(|1+| 210))  ; 46B
(lazy-def 212 '(|1+| 211))  ; 50B
(lazy-def 213 '(|1+| 212))  ; 54B
(lazy-def 214 '(|1+| 213))  ; 58B
(lazy-def 215 '(|1+| 214))  ; 62B
(lazy-def 216 '(^ 6 3))  ; 32B
(lazy-def 217 '(|1+| 216))  ; 36B
(lazy-def 218 '(|1+| 217))  ; 40B
(lazy-def 219 '(|1+| 218))  ; 44B
(lazy-def 220 '(|1+| 219))  ; 48B
(lazy-def 221 '(|1+| 220))  ; 52B
(lazy-def 222 '(* 2 111))  ; 53B
(lazy-def 223 '(+ 222 1))  ; 52B
(lazy-def 224 '(* 4 56))  ; 55B
(lazy-def 225 '(^ 15 2))  ; 48B
(lazy-def 226 '(|1+| 225))  ; 52B
(lazy-def 227 '(|1+| 226))  ; 56B
(lazy-def 228 '(+ 12 216))  ; 58B
(lazy-def 229 '(|1+| 228))  ; 62B
(lazy-def 230 '(|1+| 229))  ; 66B
(lazy-def 231 '(* 7 33))  ; 67B
(lazy-def 232 '(+ 216 16))  ; 56B
(lazy-def 233 '(+ 217 16))  ; 60B
(lazy-def 234 '(+ 90 144))  ; 62B
(lazy-def 235 '(|1+| 234))  ; 66B
(lazy-def 236 '(+ 216 20))  ; 65B
(lazy-def 237 '(+ 81 156))  ; 59B
(lazy-def 238 '(|1+| 237))  ; 63B
(lazy-def 239 '(|1+| 238))  ; 67B
(lazy-def 240 '(oblong 15))  ; 46B
(lazy-def 241 '(|1+| 240))  ; 50B
(lazy-def 242 '(|1+| 241))  ; 54B
(lazy-def 243 '(^ 3 5))  ; 43B
(lazy-def 244 '(|1+| 243))  ; 47B
(lazy-def 245 '(|1+| 244))  ; 51B
(lazy-def 246 '(* 3 82))  ; 52B
(lazy-def 247 '(+ 246 1))  ; 48B
(lazy-def 248 '(|1+| 247))  ; 57B
(lazy-def 249 '(* 3 83))  ; 56B
(lazy-def 250 '(+ 249 1))  ; 52B
(lazy-def 251 '(|1+| 250))  ; 61B
(lazy-def 252 '(* 7 36))  ; 56B
(lazy-def 253 '(+ 252 1))  ; 52B
(lazy-def 254 '(|1+| 253))  ; 61B
(lazy-def 255 '(* 3 85))  ; 64B
(lazy-def 256 '((lambda (x) (x x)) 4))  ; 21B
(lazy-def 257 '(|1+| 256))  ; 31B
(lazy-def 258 '(|1+| 257))  ; 38B
(lazy-def 259 '(|1+| 258))  ; 42B
(lazy-def 260 '(+ 4 256))  ; 39B
(lazy-def 261 '(+ 256 5))  ; 47B
(lazy-def 262 '(+ 6 256))  ; 47B
(lazy-def 263 '(+ 7 256))  ; 51B
(lazy-def 264 '(+ 4 260))  ; 46B
(lazy-def 265 '(+ 9 256))  ; 46B
(lazy-def 266 '(+ 10 256))  ; 50B
(lazy-def 267 '(+ 11 256))  ; 54B
(lazy-def 268 '(+ 4 264))  ; 50B
(lazy-def 269 '(+ 13 256))  ; 55B
(lazy-def 270 '(* 3 90))  ; 52B
(lazy-def 271 '(+ 270 1))  ; 48B
(lazy-def 272 '(* 16 17))  ; 32B
(lazy-def 273 '(|1+| 272))  ; 41B
(lazy-def 274 '(|1+| 273))  ; 45B
(lazy-def 275 '(|1+| 274))  ; 49B
(lazy-def 276 '(+ 256 20))  ; 50B
(lazy-def 277 '(|1+| 276))  ; 60B
(lazy-def 278 '(+ 6 272))  ; 59B
(lazy-def 279 '(|1+| 278))  ; 63B
(lazy-def 280 '(* 10 28))  ; 62B
(lazy-def 281 '(+ 9 272))  ; 58B
(lazy-def 282 '(|1+| 281))  ; 62B
(lazy-def 283 '(+ 27 256))  ; 53B
(lazy-def 284 '(+ 28 256))  ; 57B
(lazy-def 285 '(+ 29 256))  ; 61B
(lazy-def 286 '(+ 30 256))  ; 62B
(lazy-def 287 '(+ 31 256))  ; 66B
(lazy-def 288 '(* 2 144))  ; 49B
(lazy-def 289 '(^ 17 2))  ; 38B
(lazy-def 290 '(|1+| 289))  ; 48B
(lazy-def 291 '(|1+| 290))  ; 55B
(lazy-def 292 '(+ 36 256))  ; 50B
(lazy-def 293 '(+ 37 256))  ; 54B
(lazy-def 294 '(+ 38 256))  ; 58B
(lazy-def 295 '(+ 39 256))  ; 62B
(lazy-def 296 '(* 8 37))  ; 60B
(lazy-def 297 '(* 11 27))  ; 57B
(lazy-def 298 '(+ 42 256))  ; 55B
(lazy-def 299 '(+ 43 256))  ; 59B
(lazy-def 300 '(* 3 100))  ; 56B
(lazy-def 301 '(+ 300 1))  ; 52B
(lazy-def 302 '(|1+| 301))  ; 61B
(lazy-def 303 '(* 3 101))  ; 60B
(lazy-def 304 '(+ 303 1))  ; 56B
(lazy-def 305 '(+ 49 256))  ; 58B
(lazy-def 306 '(oblong 17))  ; 41B
(lazy-def 307 '(|1+| 306))  ; 45B
(lazy-def 308 '(|1+| 307))  ; 49B
(lazy-def 309 '(|1+| 308))  ; 53B
(lazy-def 310 '(|1+| 309))  ; 57B
(lazy-def 311 '(|1+| 310))  ; 61B
(lazy-def 312 '(* 2 156))  ; 50B
(lazy-def 313 '(+ 312 1))  ; 49B
(lazy-def 314 '(* 2 157))  ; 54B
(lazy-def 315 '(+ 314 1))  ; 53B
(lazy-def 316 '(* 2 158))  ; 58B
(lazy-def 317 '(+ 316 1))  ; 57B
(lazy-def 318 '(* 2 159))  ; 62B
(lazy-def 319 '(+ 318 1))  ; 61B
(lazy-def 320 '(* 4 80))  ; 36B
(lazy-def 321 '(|1+| 320))  ; 45B
(lazy-def 322 '(|1+| 321))  ; 49B
(lazy-def 323 '(|1+| 322))  ; 53B
(lazy-def 324 '(^ 18 2))  ; 44B
(lazy-def 325 '(|1+| 324))  ; 48B
(lazy-def 326 '(|1+| 325))  ; 52B
(lazy-def 327 '(|1+| 326))  ; 56B
(lazy-def 328 '(* 4 82))  ; 50B
(lazy-def 329 '(+ 328 1))  ; 50B
(lazy-def 330 '(* 3 110))  ; 56B
(lazy-def 331 '(+ 330 1))  ; 52B
(lazy-def 332 '(* 4 83))  ; 54B
(lazy-def 333 '(+ 332 1))  ; 54B
(lazy-def 334 '(+ 332 2))  ; 62B
(lazy-def 335 '(|1+| 334))  ; 72B
(lazy-def 336 '(+ 256 80))  ; 54B
(lazy-def 337 '(+ 81 256))  ; 50B
(lazy-def 338 '(+ 82 256))  ; 54B
(lazy-def 339 '(+ 83 256))  ; 58B
(lazy-def 340 '(* 2 170))  ; 60B
(lazy-def 341 '(+ 340 1))  ; 59B
(lazy-def 342 '(oblong 18))  ; 43B
(lazy-def 343 '(^ 7 3))  ; 40B
(lazy-def 344 '(|1+| 343))  ; 44B
(lazy-def 345 '(|1+| 344))  ; 48B
(lazy-def 346 '(|1+| 345))  ; 52B
(lazy-def 347 '(|1+| 346))  ; 56B
(lazy-def 348 '(|1+| 347))  ; 60B
(lazy-def 349 '(+ 6 343))  ; 56B
(lazy-def 350 '(+ 343 7))  ; 58B
(lazy-def 351 '(* 13 27))  ; 58B
(lazy-def 352 '(+ 351 1))  ; 59B
(lazy-def 353 '(+ 81 272))  ; 62B
(lazy-def 354 '(|1+| 353))  ; 66B
(lazy-def 355 '(+ 6 349))  ; 63B
(lazy-def 356 '(+ 100 256))  ; 58B
(lazy-def 357 '(+ 101 256))  ; 62B
(lazy-def 358 '(+ 102 256))  ; 66B
(lazy-def 359 '(+ 343 16))  ; 64B
(lazy-def 360 '(* 4 90))  ; 50B
(lazy-def 361 '(^ 19 2))  ; 46B
(lazy-def 362 '(|1+| 361))  ; 50B
(lazy-def 363 '(|1+| 362))  ; 54B
(lazy-def 364 '(* 2 182))  ; 54B
(lazy-def 365 '(+ 364 1))  ; 53B
(lazy-def 366 '(* 2 183))  ; 58B
(lazy-def 367 '(+ 366 1))  ; 57B
(lazy-def 368 '(* 4 92))  ; 58B
(lazy-def 369 '(+ 368 1))  ; 58B
(lazy-def 370 '(* 10 37))  ; 59B
(lazy-def 371 '(+ 370 1))  ; 55B
(lazy-def 372 '(* 4 93))  ; 62B
(lazy-def 373 '(+ 372 1))  ; 62B
(lazy-def 374 '(+ 370 4))  ; 70B
(lazy-def 375 '(* 3 125))  ; 66B
(lazy-def 376 '(+ 375 1))  ; 62B
(lazy-def 377 '(+ 121 256))  ; 64B
(lazy-def 378 '(* 9 42))  ; 56B
(lazy-def 379 '(+ 378 1))  ; 52B
(lazy-def 380 '(oblong 19))  ; 45B
(lazy-def 381 '(|1+| 380))  ; 49B
(lazy-def 382 '(|1+| 381))  ; 53B
(lazy-def 383 '(|1+| 382))  ; 57B
(lazy-def 384 '(* 6 64))  ; 55B
(lazy-def 385 '(+ 384 1))  ; 57B
(lazy-def 386 '(|1+| 385))  ; 66B
(lazy-def 387 '(* 9 43))  ; 60B
(lazy-def 388 '(+ 387 1))  ; 56B
(lazy-def 389 '(+ 9 380))  ; 60B
(lazy-def 390 '(* 6 65))  ; 61B
(lazy-def 391 '(+ 390 1))  ; 61B
(lazy-def 392 '(* 2 196))  ; 60B
(lazy-def 393 '(+ 392 1))  ; 59B
(lazy-def 394 '(* 2 197))  ; 64B
(lazy-def 395 '(+ 394 1))  ; 63B
(lazy-def 396 '(* 11 36))  ; 59B
(lazy-def 397 '(+ 396 1))  ; 55B
(lazy-def 398 '(|1+| 397))  ; 64B
(lazy-def 399 '(* 3 133))  ; 64B
(lazy-def 400 '(^ 20 2))  ; 37B
(lazy-def 401 '(|1+| 400))  ; 47B
(lazy-def 402 '(|1+| 401))  ; 54B
(lazy-def 403 '(|1+| 402))  ; 58B
(lazy-def 404 '(* 4 101))  ; 58B
(lazy-def 405 '(* 5 81))  ; 56B
(lazy-def 406 '(+ 405 1))  ; 55B
(lazy-def 407 '(* 11 37))  ; 63B
(lazy-def 408 '(+ 407 1))  ; 59B
(lazy-def 409 '(+ 9 400))  ; 62B
(lazy-def 410 '(* 5 82))  ; 60B
(lazy-def 411 '(+ 82 329))  ; 59B
(lazy-def 412 '(+ 156 256))  ; 59B
(lazy-def 413 '(+ 157 256))  ; 63B
(lazy-def 414 '(+ 158 256))  ; 67B
(lazy-def 415 '(* 5 83))  ; 64B
(lazy-def 416 '(+ 16 400))  ; 63B
(lazy-def 417 '(+ 208 209))  ; 70B
(lazy-def 418 '(+ 81 337))  ; 60B
(lazy-def 419 '(+ 163 256))  ; 68B
(lazy-def 420 '(oblong 20))  ; 38B
(lazy-def 421 '(|1+| 420))  ; 47B
(lazy-def 422 '(|1+| 421))  ; 51B
(lazy-def 423 '(|1+| 422))  ; 55B
(lazy-def 424 '(|1+| 423))  ; 59B
(lazy-def 425 '(|1+| 424))  ; 63B
(lazy-def 426 '(+ 6 420))  ; 65B
(lazy-def 427 '(|1+| 426))  ; 69B
(lazy-def 428 '(+ 156 272))  ; 71B
(lazy-def 429 '(+ 9 420))  ; 64B
(lazy-def 430 '(* 10 43))  ; 64B
(lazy-def 431 '(+ 430 1))  ; 60B
(lazy-def 432 '(* 2 216))  ; 48B
(lazy-def 433 '(+ 432 1))  ; 47B
(lazy-def 434 '(* 2 217))  ; 52B
(lazy-def 435 '(+ 434 1))  ; 51B
(lazy-def 436 '(* 2 218))  ; 56B
(lazy-def 437 '(+ 436 1))  ; 55B
(lazy-def 438 '(* 2 219))  ; 60B
(lazy-def 439 '(+ 438 1))  ; 59B
(lazy-def 440 '(* 4 110))  ; 54B
(lazy-def 441 '(^ 21 2))  ; 47B
(lazy-def 442 '(|1+| 441))  ; 51B
(lazy-def 443 '(|1+| 442))  ; 55B
(lazy-def 444 '(* 4 111))  ; 58B
(lazy-def 445 '(+ 444 1))  ; 58B
(lazy-def 446 '(+ 444 2))  ; 66B
(lazy-def 447 '(+ 6 441))  ; 69B
(lazy-def 448 '(* 16 28))  ; 57B
(lazy-def 449 '(+ 448 1))  ; 57B
(lazy-def 450 '(* 5 90))  ; 60B
(lazy-def 451 '(+ 450 1))  ; 59B
(lazy-def 452 '(* 4 113))  ; 66B
(lazy-def 453 '(+ 452 1))  ; 66B
(lazy-def 454 '(* 2 227))  ; 72B
(lazy-def 455 '(* 5 91))  ; 64B
(lazy-def 456 '(+ 455 1))  ; 63B
(lazy-def 457 '(+ 441 16))  ; 71B
(lazy-def 458 '(+ 101 357))  ; 72B
(lazy-def 459 '(* 17 27))  ; 58B
(lazy-def 460 '(+ 459 1))  ; 62B
(lazy-def 461 '(+ 92 369))  ; 67B
(lazy-def 462 '(oblong 21))  ; 45B
(lazy-def 463 '(|1+| 462))  ; 49B
(lazy-def 464 '(|1+| 463))  ; 53B
(lazy-def 465 '(|1+| 464))  ; 57B
(lazy-def 466 '(|1+| 465))  ; 61B
(lazy-def 467 '(|1+| 466))  ; 65B
(lazy-def 468 '(* 3 156))  ; 57B
(lazy-def 469 '(+ 468 1))  ; 53B
(lazy-def 470 '(|1+| 469))  ; 62B
(lazy-def 471 '(* 3 157))  ; 61B
(lazy-def 472 '(+ 216 256))  ; 57B
(lazy-def 473 '(+ 217 256))  ; 61B
(lazy-def 474 '(* 3 158))  ; 65B
(lazy-def 475 '(+ 474 1))  ; 61B
(lazy-def 476 '(* 17 28))  ; 67B
(lazy-def 477 '(+ 28 449))  ; 66B
(lazy-def 478 '(+ 462 16))  ; 69B
(lazy-def 479 '(+ 463 16))  ; 73B
(lazy-def 480 '(* 4 120))  ; 53B
(lazy-def 481 '(|1+| 480))  ; 63B
(lazy-def 482 '(* 2 241))  ; 66B
(lazy-def 483 '(+ 482 1))  ; 65B
(lazy-def 484 '(^ 22 2))  ; 51B
(lazy-def 485 '(|1+| 484))  ; 55B
(lazy-def 486 '(* 6 81))  ; 52B
(lazy-def 487 '(+ 486 1))  ; 48B
(lazy-def 488 '(|1+| 487))  ; 57B
(lazy-def 489 '(|1+| 488))  ; 61B
(lazy-def 490 '(* 10 49))  ; 63B
(lazy-def 491 '(+ 490 1))  ; 59B
(lazy-def 492 '(* 6 82))  ; 56B
(lazy-def 493 '(+ 492 1))  ; 52B
(lazy-def 494 '(|1+| 493))  ; 61B
(lazy-def 495 '(+ 486 9))  ; 64B
(lazy-def 496 '(* 16 31))  ; 66B
(lazy-def 497 '(+ 496 1))  ; 66B
(lazy-def 498 '(* 6 83))  ; 60B
(lazy-def 499 '(+ 498 1))  ; 56B
(lazy-def 500 '(* 4 125))  ; 55B
(lazy-def 501 '(+ 500 1))  ; 64B
(lazy-def 502 '(+ 486 16))  ; 67B
(lazy-def 503 '(|1+| 502))  ; 76B
(lazy-def 504 '(* 9 56))  ; 60B
(lazy-def 505 '(+ 504 1))  ; 56B
(lazy-def 506 '(oblong 22))  ; 49B
(lazy-def 507 '(|1+| 506))  ; 53B
(lazy-def 508 '(|1+| 507))  ; 57B
(lazy-def 509 '(|1+| 508))  ; 61B
(lazy-def 510 '(|1+| 509))  ; 65B
(lazy-def 511 '(|1+| 510))  ; 69B
(lazy-def 512 '(^ 2 9))  ; 31B
(lazy-def 513 '(|1+| 512))  ; 35B
(lazy-def 514 '(|1+| 513))  ; 39B
(lazy-def 515 '(|1+| 514))  ; 43B
(lazy-def 516 '(|1+| 515))  ; 47B
(lazy-def 517 '(+ 256 261))  ; 48B
(lazy-def 518 '(+ 6 512))  ; 53B
(lazy-def 519 '(|1+| 518))  ; 57B
(lazy-def 520 '(* 2 260))  ; 54B
(lazy-def 521 '(+ 9 512))  ; 47B
(lazy-def 522 '(|1+| 521))  ; 51B
(lazy-def 523 '(|1+| 522))  ; 55B
(lazy-def 524 '(+ 12 512))  ; 57B
(lazy-def 525 '(|1+| 524))  ; 61B
(lazy-def 526 '(|1+| 525))  ; 65B
(lazy-def 527 '(|1+| 526))  ; 69B
(lazy-def 528 '(+ 512 16))  ; 55B
(lazy-def 529 '(^ 23 2))  ; 55B
(lazy-def 530 '(+ 9 521))  ; 54B
(lazy-def 531 '(|1+| 530))  ; 58B
(lazy-def 532 '(+ 256 276))  ; 57B
(lazy-def 533 '(|1+| 532))  ; 67B
(lazy-def 534 '(* 2 267))  ; 70B
(lazy-def 535 '(+ 534 1))  ; 74B
(lazy-def 536 '(* 2 268))  ; 65B
(lazy-def 537 '(+ 512 25))  ; 69B
(lazy-def 538 '(* 2 269))  ; 71B
(lazy-def 539 '(+ 9 530))  ; 58B
(lazy-def 540 '(* 6 90))  ; 56B
(lazy-def 541 '(+ 540 1))  ; 52B
(lazy-def 542 '(|1+| 541))  ; 61B
(lazy-def 543 '(|1+| 542))  ; 65B
(lazy-def 544 '(* 2 272))  ; 47B
(lazy-def 545 '(|1+| 544))  ; 53B
(lazy-def 546 '(* 2 273))  ; 57B
(lazy-def 547 '(+ 546 1))  ; 56B
(lazy-def 548 '(+ 36 512))  ; 56B
(lazy-def 549 '(|1+| 548))  ; 60B
(lazy-def 550 '(* 5 110))  ; 64B
(lazy-def 551 '(+ 550 1))  ; 63B
(lazy-def 552 '(oblong 23))  ; 53B
(lazy-def 553 '(|1+| 552))  ; 57B
(lazy-def 554 '(|1+| 553))  ; 61B
(lazy-def 555 '(|1+| 554))  ; 65B
(lazy-def 556 '(+ 111 445))  ; 67B
(lazy-def 557 '(+ 36 521))  ; 72B
(lazy-def 558 '(* 6 93))  ; 68B
(lazy-def 559 '(+ 558 1))  ; 64B
(lazy-def 560 '(* 7 80))  ; 63B
(lazy-def 561 '(+ 49 512))  ; 64B
(lazy-def 562 '(+ 306 256))  ; 66B
(lazy-def 563 '(+ 307 256))  ; 70B
(lazy-def 564 '(* 6 94))  ; 72B
(lazy-def 565 '(+ 564 1))  ; 68B
(lazy-def 566 '(* 2 283))  ; 69B
(lazy-def 567 '(* 7 81))  ; 56B
(lazy-def 568 '(+ 567 1))  ; 52B
(lazy-def 569 '(|1+| 568))  ; 61B
(lazy-def 570 '(|1+| 569))  ; 65B
(lazy-def 571 '(+ 567 4))  ; 67B
(lazy-def 572 '(* 13 44))  ; 73B
(lazy-def 573 '(+ 567 6))  ; 69B
(lazy-def 574 '(* 7 82))  ; 60B
(lazy-def 575 '(+ 574 1))  ; 56B
(lazy-def 576 '(* 16 36))  ; 50B
(lazy-def 577 '(+ 576 1))  ; 50B
(lazy-def 578 '(* 2 289))  ; 48B
(lazy-def 579 '(|1+| 578))  ; 54B
(lazy-def 580 '(* 2 290))  ; 56B
(lazy-def 581 '(|1+| 580))  ; 60B
(lazy-def 582 '(|1+| 581))  ; 64B
(lazy-def 583 '(|1+| 582))  ; 68B
(lazy-def 584 '(* 4 146))  ; 62B
(lazy-def 585 '(* 9 65))  ; 60B
(lazy-def 586 '(+ 585 1))  ; 60B
(lazy-def 587 '(|1+| 586))  ; 69B
(lazy-def 588 '(* 12 49))  ; 64B
(lazy-def 589 '(+ 588 1))  ; 60B
(lazy-def 590 '(|1+| 589))  ; 69B
(lazy-def 591 '(* 3 197))  ; 71B
(lazy-def 592 '(* 16 37))  ; 54B
(lazy-def 593 '(+ 592 1))  ; 54B
(lazy-def 594 '(+ 81 513))  ; 60B
(lazy-def 595 '(|1+| 594))  ; 64B
(lazy-def 596 '(+ 592 4))  ; 66B
(lazy-def 597 '(+ 81 516))  ; 72B
(lazy-def 598 '(+ 342 256))  ; 68B
(lazy-def 599 '(+ 343 256))  ; 65B
(lazy-def 600 '(oblong 24))  ; 48B
(lazy-def 601 '(|1+| 600))  ; 52B
(lazy-def 602 '(|1+| 601))  ; 56B
(lazy-def 603 '(|1+| 602))  ; 60B
(lazy-def 604 '(|1+| 603))  ; 64B
(lazy-def 605 '(|1+| 604))  ; 68B
(lazy-def 606 '(* 6 101))  ; 64B
(lazy-def 607 '(+ 606 1))  ; 60B
(lazy-def 608 '(* 16 38))  ; 58B
(lazy-def 609 '(+ 608 1))  ; 58B
(lazy-def 610 '(+ 608 2))  ; 66B
(lazy-def 611 '(+ 90 521))  ; 72B
(lazy-def 612 '(* 2 306))  ; 57B
(lazy-def 613 '(+ 612 1))  ; 56B
(lazy-def 614 '(* 2 307))  ; 61B
(lazy-def 615 '(+ 614 1))  ; 60B
(lazy-def 616 '(+ 360 256))  ; 63B
(lazy-def 617 '(+ 361 256))  ; 71B
(lazy-def 618 '(* 2 309))  ; 69B
(lazy-def 619 '(+ 618 1))  ; 68B
(lazy-def 620 '(* 2 310))  ; 73B
(lazy-def 621 '(+ 540 81))  ; 72B
(lazy-def 622 '(+ 110 512))  ; 64B
(lazy-def 623 '(|1+| 622))  ; 68B
(lazy-def 624 '(* 4 156))  ; 55B
(lazy-def 625 '(^ 5 4))  ; 29B
(lazy-def 626 '(|1+| 625))  ; 37B
(lazy-def 627 '(|1+| 626))  ; 41B
(lazy-def 628 '(|1+| 627))  ; 45B
(lazy-def 629 '(+ 4 625))  ; 47B
(lazy-def 630 '(+ 4 626))  ; 48B
(lazy-def 631 '(|1+| 630))  ; 52B
(lazy-def 632 '(|1+| 631))  ; 56B
(lazy-def 633 '(+ 4 629))  ; 54B
(lazy-def 634 '(+ 4 630))  ; 52B
(lazy-def 635 '(|1+| 634))  ; 56B
(lazy-def 636 '(|1+| 635))  ; 60B
(lazy-def 637 '(+ 4 633))  ; 58B
(lazy-def 638 '(+ 4 634))  ; 56B
(lazy-def 639 '(|1+| 638))  ; 60B
(lazy-def 640 '(* 2 320))  ; 51B
(lazy-def 641 '(+ 16 625))  ; 56B
(lazy-def 642 '(|1+| 641))  ; 60B
(lazy-def 643 '(|1+| 642))  ; 64B
(lazy-def 644 '(* 2 322))  ; 65B
(lazy-def 645 '(+ 644 1))  ; 64B
(lazy-def 646 '(* 17 38))  ; 68B
(lazy-def 647 '(+ 38 609))  ; 67B
(lazy-def 648 '(* 3 216))  ; 55B
(lazy-def 649 '(+ 648 1))  ; 51B
(lazy-def 650 '(* 25 26))  ; 46B
(lazy-def 651 '(|1+| 650))  ; 55B
(lazy-def 652 '(|1+| 651))  ; 59B
(lazy-def 653 '(|1+| 652))  ; 63B
(lazy-def 654 '(* 3 218))  ; 63B
(lazy-def 655 '(+ 654 1))  ; 59B
(lazy-def 656 '(* 8 82))  ; 60B
(lazy-def 657 '(+ 656 1))  ; 59B
(lazy-def 658 '(+ 656 2))  ; 62B
(lazy-def 659 '(|1+| 658))  ; 72B
(lazy-def 660 '(* 6 110))  ; 60B
(lazy-def 661 '(+ 660 1))  ; 56B
(lazy-def 662 '(+ 37 625))  ; 62B
(lazy-def 663 '(+ 38 625))  ; 66B
(lazy-def 664 '(* 8 83))  ; 64B
(lazy-def 665 '(+ 664 1))  ; 63B
(lazy-def 666 '(* 6 111))  ; 64B
(lazy-def 667 '(+ 666 1))  ; 60B
(lazy-def 668 '(+ 156 512))  ; 65B
(lazy-def 669 '(|1+| 668))  ; 69B
(lazy-def 670 '(* 10 67))  ; 72B
(lazy-def 671 '(+ 670 1))  ; 72B
(lazy-def 672 '(* 16 42))  ; 55B
(lazy-def 673 '(+ 672 1))  ; 55B
(lazy-def 674 '(+ 672 2))  ; 63B
(lazy-def 675 '(* 25 27))  ; 62B
(lazy-def 676 '(^ 26 2))  ; 49B
(lazy-def 677 '(|1+| 676))  ; 59B
(lazy-def 678 '(|1+| 677))  ; 64B
(lazy-def 679 '(|1+| 678))  ; 68B
(lazy-def 680 '(* 4 170))  ; 65B
(lazy-def 681 '(+ 680 1))  ; 65B
(lazy-def 682 '(+ 57 625))  ; 71B
(lazy-def 683 '(+ 58 625))  ; 75B
(lazy-def 684 '(* 2 342))  ; 59B
(lazy-def 685 '(+ 684 1))  ; 58B
(lazy-def 686 '(* 2 343))  ; 56B
(lazy-def 687 '(+ 686 1))  ; 55B
(lazy-def 688 '(* 16 43))  ; 59B
(lazy-def 689 '(+ 688 1))  ; 59B
(lazy-def 690 '(* 2 345))  ; 64B
(lazy-def 691 '(+ 690 1))  ; 63B
(lazy-def 692 '(* 2 346))  ; 68B
(lazy-def 693 '(+ 343 350))  ; 65B
(lazy-def 694 '(|1+| 693))  ; 69B
(lazy-def 695 '(+ 686 9))  ; 71B
(lazy-def 696 '(+ 440 256))  ; 67B
(lazy-def 697 '(+ 72 625))  ; 69B
(lazy-def 698 '(* 2 349))  ; 72B
(lazy-def 699 '(+ 698 1))  ; 71B
(lazy-def 700 '(* 7 100))  ; 64B
(lazy-def 701 '(+ 700 1))  ; 60B
(lazy-def 702 '(oblong 26))  ; 55B
(lazy-def 703 '(|1+| 702))  ; 59B
(lazy-def 704 '(* 11 64))  ; 62B
(lazy-def 705 '(+ 704 1))  ; 64B
(lazy-def 706 '(+ 81 625))  ; 58B
(lazy-def 707 '(+ 82 625))  ; 62B
(lazy-def 708 '(+ 83 625))  ; 66B
(lazy-def 709 '(+ 84 625))  ; 70B
(lazy-def 710 '(+ 85 625))  ; 74B
(lazy-def 711 '(+ 9 702))  ; 76B
(lazy-def 712 '(* 2 356))  ; 74B
(lazy-def 713 '(+ 200 513))  ; 78B
(lazy-def 714 '(* 17 42))  ; 65B
(lazy-def 715 '(+ 90 625))  ; 62B
(lazy-def 716 '(+ 91 625))  ; 66B
(lazy-def 717 '(+ 92 625))  ; 70B
(lazy-def 718 '(+ 462 256))  ; 70B
(lazy-def 719 '(+ 463 256))  ; 74B
(lazy-def 720 '(* 9 80))  ; 58B
(lazy-def 721 '(|1+| 720))  ; 68B
(lazy-def 722 '(* 2 361))  ; 62B
(lazy-def 723 '(+ 722 1))  ; 61B
(lazy-def 724 '(* 4 181))  ; 65B
(lazy-def 725 '(+ 100 625))  ; 66B
(lazy-def 726 '(* 6 121))  ; 66B
(lazy-def 727 '(+ 726 1))  ; 62B
(lazy-def 728 '(* 4 182))  ; 59B
(lazy-def 729 '(^ 3 6))  ; 32B
(lazy-def 730 '(|1+| 729))  ; 36B
(lazy-def 731 '(|1+| 730))  ; 40B
(lazy-def 732 '(|1+| 731))  ; 44B
(lazy-def 733 '(|1+| 732))  ; 48B
(lazy-def 734 '(|1+| 733))  ; 52B
(lazy-def 735 '(+ 6 729))  ; 54B
(lazy-def 736 '(|1+| 735))  ; 58B
(lazy-def 737 '(+ 729 8))  ; 60B
(lazy-def 738 '(+ 9 729))  ; 53B
(lazy-def 739 '(|1+| 738))  ; 57B
(lazy-def 740 '(|1+| 739))  ; 61B
(lazy-def 741 '(+ 12 729))  ; 58B
(lazy-def 742 '(|1+| 741))  ; 62B
(lazy-def 743 '(|1+| 742))  ; 66B
(lazy-def 744 '(|1+| 743))  ; 70B
(lazy-def 745 '(+ 729 16))  ; 56B
(lazy-def 746 '(+ 730 16))  ; 60B
(lazy-def 747 '(* 9 83))  ; 59B
(lazy-def 748 '(+ 747 1))  ; 55B
(lazy-def 749 '(|1+| 748))  ; 64B
(lazy-def 750 '(|1+| 749))  ; 68B
(lazy-def 751 '(+ 747 4))  ; 70B
(lazy-def 752 '(* 16 47))  ; 75B
(lazy-def 753 '(+ 625 128))  ; 65B
(lazy-def 754 '(|1+| 753))  ; 69B
(lazy-def 755 '(|1+| 754))  ; 73B
(lazy-def 756 '(oblong 27))  ; 36B
(lazy-def 757 '(|1+| 756))  ; 40B
(lazy-def 758 '(|1+| 757))  ; 44B
(lazy-def 759 '(|1+| 758))  ; 48B
(lazy-def 760 '(|1+| 759))  ; 52B
(lazy-def 761 '(|1+| 760))  ; 56B
(lazy-def 762 '(+ 6 756))  ; 58B
(lazy-def 763 '(|1+| 762))  ; 62B
(lazy-def 764 '(+ 756 8))  ; 64B
(lazy-def 765 '(+ 9 756))  ; 57B
(lazy-def 766 '(|1+| 765))  ; 61B
(lazy-def 767 '(|1+| 766))  ; 65B
(lazy-def 768 '(* 3 256))  ; 44B
(lazy-def 769 '(+ 768 1))  ; 49B
(lazy-def 770 '(|1+| 769))  ; 58B
(lazy-def 771 '(* 3 257))  ; 54B
(lazy-def 772 '(+ 771 1))  ; 53B
(lazy-def 773 '(+ 256 517))  ; 58B
(lazy-def 774 '(* 3 258))  ; 61B
(lazy-def 775 '(+ 774 1))  ; 57B
(lazy-def 776 '(|1+| 775))  ; 66B
(lazy-def 777 '(* 3 259))  ; 65B
(lazy-def 778 '(+ 777 1))  ; 61B
(lazy-def 779 '(+ 49 730))  ; 69B
(lazy-def 780 '(* 3 260))  ; 62B
(lazy-def 781 '(+ 156 625))  ; 67B
(lazy-def 782 '(+ 157 625))  ; 71B
(lazy-def 783 '(+ 756 27))  ; 54B
(lazy-def 784 '(^ 28 2))  ; 42B
(lazy-def 785 '(|1+| 784))  ; 46B
(lazy-def 786 '(|1+| 785))  ; 50B
(lazy-def 787 '(|1+| 786))  ; 54B
(lazy-def 788 '(|1+| 787))  ; 58B
(lazy-def 789 '(|1+| 788))  ; 62B
(lazy-def 790 '(+ 6 784))  ; 64B
(lazy-def 791 '(|1+| 790))  ; 68B
(lazy-def 792 '(+ 36 756))  ; 61B
(lazy-def 793 '(+ 9 784))  ; 63B
(lazy-def 794 '(|1+| 793))  ; 67B
(lazy-def 795 '(* 3 265))  ; 69B
(lazy-def 796 '(+ 12 784))  ; 68B
(lazy-def 797 '(|1+| 796))  ; 72B
(lazy-def 798 '(+ 42 756))  ; 66B
(lazy-def 799 '(|1+| 798))  ; 70B
(lazy-def 800 '(* 2 400))  ; 47B
(lazy-def 801 '(|1+| 800))  ; 53B
(lazy-def 802 '(|1+| 801))  ; 57B
(lazy-def 803 '(|1+| 802))  ; 61B
(lazy-def 804 '(+ 800 4))  ; 63B
(lazy-def 805 '(+ 49 756))  ; 69B
(lazy-def 806 '(+ 6 800))  ; 71B
(lazy-def 807 '(+ 182 625))  ; 71B
(lazy-def 808 '(* 8 101))  ; 68B
(lazy-def 809 '(+ 808 1))  ; 67B
(lazy-def 810 '(* 9 90))  ; 55B
(lazy-def 811 '(+ 810 1))  ; 45B
(lazy-def 812 '(oblong 28))  ; 40B
(lazy-def 813 '(|1+| 812))  ; 44B
(lazy-def 814 '(|1+| 813))  ; 48B
(lazy-def 815 '(|1+| 814))  ; 52B
(lazy-def 816 '(* 3 272))  ; 55B
(lazy-def 817 '(+ 813 4))  ; 64B
(lazy-def 818 '(+ 6 812))  ; 62B
(lazy-def 819 '(+ 810 9))  ; 54B
(lazy-def 820 '(|1+| 819))  ; 58B
(lazy-def 821 '(+ 9 812))  ; 61B
(lazy-def 822 '(|1+| 821))  ; 65B
(lazy-def 823 '(|1+| 822))  ; 69B
(lazy-def 824 '(+ 12 812))  ; 66B
(lazy-def 825 '(|1+| 824))  ; 70B
(lazy-def 826 '(+ 810 16))  ; 64B
(lazy-def 827 '(|1+| 826))  ; 73B
(lazy-def 828 '(* 9 92))  ; 63B
(lazy-def 829 '(+ 828 1))  ; 59B
(lazy-def 830 '(* 10 83))  ; 63B
(lazy-def 831 '(+ 830 1))  ; 59B
(lazy-def 832 '(* 13 64))  ; 63B
(lazy-def 833 '(+ 832 1))  ; 65B
(lazy-def 834 '(|1+| 833))  ; 74B
(lazy-def 835 '(+ 210 625))  ; 75B
(lazy-def 836 '(+ 756 80))  ; 73B
(lazy-def 837 '(+ 81 756))  ; 61B
(lazy-def 838 '(|1+| 837))  ; 65B
(lazy-def 839 '(+ 27 812))  ; 56B
(lazy-def 840 '(* 2 420))  ; 54B
(lazy-def 841 '(^ 29 2))  ; 46B
(lazy-def 842 '(|1+| 841))  ; 50B
(lazy-def 843 '(|1+| 842))  ; 54B
(lazy-def 844 '(|1+| 843))  ; 58B
(lazy-def 845 '(|1+| 844))  ; 62B
(lazy-def 846 '(+ 90 756))  ; 65B
(lazy-def 847 '(+ 6 841))  ; 68B
(lazy-def 848 '(+ 36 812))  ; 65B
(lazy-def 849 '(|1+| 848))  ; 69B
(lazy-def 850 '(+ 9 841))  ; 67B
(lazy-def 851 '(|1+| 850))  ; 71B
(lazy-def 852 '(+ 810 42))  ; 70B
(lazy-def 853 '(+ 12 841))  ; 72B
(lazy-def 854 '(+ 42 812))  ; 70B
(lazy-def 855 '(+ 343 512))  ; 71B
(lazy-def 856 '(+ 100 756))  ; 69B
(lazy-def 857 '(+ 729 128))  ; 70B
(lazy-def 858 '(* 13 66))  ; 73B
(lazy-def 859 '(+ 810 49))  ; 73B
(lazy-def 860 '(* 20 43))  ; 67B
(lazy-def 861 '(+ 132 729))  ; 69B
(lazy-def 862 '(|1+| 861))  ; 73B
(lazy-def 863 '(|1+| 862))  ; 77B
(lazy-def 864 '(* 4 216))  ; 53B
(lazy-def 865 '(+ 864 1))  ; 53B
(lazy-def 866 '(+ 864 2))  ; 61B
(lazy-def 867 '(* 3 289))  ; 61B
(lazy-def 868 '(* 4 217))  ; 57B
(lazy-def 869 '(+ 868 1))  ; 57B
(lazy-def 870 '(oblong 29))  ; 44B
(lazy-def 871 '(|1+| 870))  ; 48B
(lazy-def 872 '(|1+| 871))  ; 52B
(lazy-def 873 '(|1+| 872))  ; 56B
(lazy-def 874 '(|1+| 873))  ; 60B
(lazy-def 875 '(|1+| 874))  ; 64B
(lazy-def 876 '(* 4 219))  ; 65B
(lazy-def 877 '(+ 876 1))  ; 65B
(lazy-def 878 '(+ 870 8))  ; 72B
(lazy-def 879 '(+ 9 870))  ; 65B
(lazy-def 880 '(* 8 110))  ; 64B
(lazy-def 881 '(+ 256 625))  ; 51B
(lazy-def 882 '(+ 256 626))  ; 58B
(lazy-def 883 '(|1+| 882))  ; 62B
(lazy-def 884 '(|1+| 883))  ; 66B
(lazy-def 885 '(+ 156 729))  ; 66B
(lazy-def 886 '(+ 4 882))  ; 62B
(lazy-def 887 '(|1+| 886))  ; 66B
(lazy-def 888 '(* 8 111))  ; 68B
(lazy-def 889 '(+ 888 1))  ; 67B
(lazy-def 890 '(+ 4 886))  ; 66B
(lazy-def 891 '(* 11 81))  ; 59B
(lazy-def 892 '(+ 891 1))  ; 55B
(lazy-def 893 '(|1+| 892))  ; 64B
(lazy-def 894 '(|1+| 893))  ; 68B
(lazy-def 895 '(+ 891 4))  ; 70B
(lazy-def 896 '(* 16 56))  ; 59B
(lazy-def 897 '(+ 896 1))  ; 59B
(lazy-def 898 '(+ 870 28))  ; 66B
(lazy-def 899 '(+ 29 870))  ; 65B
(lazy-def 900 '(^ 30 2))  ; 47B
(lazy-def 901 '(|1+| 900))  ; 51B
(lazy-def 902 '(|1+| 901))  ; 55B
(lazy-def 903 '(|1+| 902))  ; 59B
(lazy-def 904 '(|1+| 903))  ; 63B
(lazy-def 905 '(|1+| 904))  ; 67B
(lazy-def 906 '(+ 6 900))  ; 69B
(lazy-def 907 '(|1+| 906))  ; 73B
(lazy-def 908 '(+ 900 8))  ; 75B
(lazy-def 909 '(* 9 101))  ; 63B
(lazy-def 910 '(+ 909 1))  ; 59B
(lazy-def 911 '(|1+| 910))  ; 68B
(lazy-def 912 '(* 16 57))  ; 63B
(lazy-def 913 '(+ 912 1))  ; 63B
(lazy-def 914 '(+ 912 2))  ; 71B
(lazy-def 915 '(* 5 183))  ; 73B
(lazy-def 916 '(+ 900 16))  ; 71B
(lazy-def 917 '(+ 901 16))  ; 75B
(lazy-def 918 '(* 3 306))  ; 64B
(lazy-def 919 '(+ 918 1))  ; 60B
(lazy-def 920 '(* 10 92))  ; 67B
(lazy-def 921 '(+ 920 1))  ; 63B
(lazy-def 922 '(+ 81 841))  ; 71B
(lazy-def 923 '(|1+| 922))  ; 75B
(lazy-def 924 '(* 2 462))  ; 61B
(lazy-def 925 '(+ 924 1))  ; 60B
(lazy-def 926 '(* 2 463))  ; 65B
(lazy-def 927 '(+ 926 1))  ; 64B
(lazy-def 928 '(* 16 58))  ; 67B
(lazy-def 929 '(+ 928 1))  ; 67B
(lazy-def 930 '(oblong 30))  ; 45B
(lazy-def 931 '(|1+| 930))  ; 49B
(lazy-def 932 '(|1+| 931))  ; 53B
(lazy-def 933 '(|1+| 932))  ; 57B
(lazy-def 934 '(+ 4 930))  ; 60B
(lazy-def 935 '(|1+| 934))  ; 64B
(lazy-def 936 '(* 6 156))  ; 61B
(lazy-def 937 '(+ 936 1))  ; 57B
(lazy-def 938 '(|1+| 937))  ; 66B
(lazy-def 939 '(+ 9 930))  ; 66B
(lazy-def 940 '(|1+| 939))  ; 70B
(lazy-def 941 '(|1+| 940))  ; 74B
(lazy-def 942 '(* 6 157))  ; 65B
(lazy-def 943 '(+ 942 1))  ; 61B
(lazy-def 944 '(|1+| 943))  ; 70B
(lazy-def 945 '(+ 216 729))  ; 64B
(lazy-def 946 '(|1+| 945))  ; 68B
(lazy-def 947 '(|1+| 946))  ; 72B
(lazy-def 948 '(* 6 158))  ; 69B
(lazy-def 949 '(+ 948 1))  ; 65B
(lazy-def 950 '(* 25 38))  ; 72B
(lazy-def 951 '(+ 81 870))  ; 69B
(lazy-def 952 '(* 17 56))  ; 69B
(lazy-def 953 '(+ 56 897))  ; 68B
(lazy-def 954 '(* 6 159))  ; 73B
(lazy-def 955 '(+ 954 1))  ; 69B
(lazy-def 956 '(+ 144 812))  ; 73B
(lazy-def 957 '(+ 27 930))  ; 73B
(lazy-def 958 '(|1+| 957))  ; 77B
(lazy-def 959 '(|1+| 958))  ; 81B
(lazy-def 960 '(* 3 320))  ; 59B
(lazy-def 961 '(^ 31 2))  ; 51B
(lazy-def 962 '(|1+| 961))  ; 55B
(lazy-def 963 '(|1+| 962))  ; 59B
(lazy-def 964 '(|1+| 963))  ; 63B
(lazy-def 965 '(|1+| 964))  ; 67B
(lazy-def 966 '(+ 36 930))  ; 70B
(lazy-def 967 '(+ 6 961))  ; 73B
(lazy-def 968 '(* 2 484))  ; 67B
(lazy-def 969 '(+ 968 1))  ; 66B
(lazy-def 970 '(* 2 485))  ; 71B
(lazy-def 971 '(+ 970 1))  ; 70B
(lazy-def 972 '(* 27 36))  ; 53B
(lazy-def 973 '(+ 972 1))  ; 54B
(lazy-def 974 '(|1+| 973))  ; 63B
(lazy-def 975 '(|1+| 974))  ; 67B
(lazy-def 976 '(* 4 244))  ; 67B
(lazy-def 977 '(+ 976 1))  ; 68B
(lazy-def 978 '(* 6 163))  ; 70B
(lazy-def 979 '(+ 978 1))  ; 66B
(lazy-def 980 '(* 7 140))  ; 66B
(lazy-def 981 '(+ 490 491))  ; 69B
(lazy-def 982 '(+ 972 10))  ; 74B
(lazy-def 983 '(|1+| 982))  ; 78B
(lazy-def 984 '(* 12 82))  ; 60B
(lazy-def 985 '(+ 984 1))  ; 56B
(lazy-def 986 '(+ 730 256))  ; 61B
(lazy-def 987 '(+ 731 256))  ; 65B
(lazy-def 988 '(* 4 247))  ; 69B
(lazy-def 989 '(+ 733 256))  ; 73B
(lazy-def 990 '(* 9 110))  ; 59B
(lazy-def 991 '(+ 990 1))  ; 53B
(lazy-def 992 '(oblong 31))  ; 49B
(lazy-def 993 '(|1+| 992))  ; 53B
(lazy-def 994 '(|1+| 993))  ; 57B
(lazy-def 995 '(|1+| 994))  ; 61B
(lazy-def 996 '(* 12 83))  ; 64B
(lazy-def 997 '(+ 996 1))  ; 60B
(lazy-def 998 '(|1+| 997))  ; 69B
(lazy-def 999 '(* 27 37))  ; 57B
(lazy-def 1000 '(^ 10 3))  ; 39B
(lazy-def 1001 '(|1+| 1000))  ; 43B
(lazy-def 1002 '(|1+| 1001))  ; 47B
(lazy-def 1003 '(|1+| 1002))  ; 51B
(lazy-def 1004 '(|1+| 1003))  ; 55B
(lazy-def 1005 '(|1+| 1004))  ; 59B
(lazy-def 1006 '(+ 6 1000))  ; 61B
(lazy-def 1007 '(|1+| 1006))  ; 65B
(lazy-def 1008 '(* 28 36))  ; 62B
(lazy-def 1009 '(+ 9 1000))  ; 55B
(lazy-def 1010 '(+ 1000 10))  ; 57B
(lazy-def 1011 '(+ 10 1001))  ; 59B
(lazy-def 1012 '(+ 756 256))  ; 61B
(lazy-def 1013 '(+ 757 256))  ; 65B
(lazy-def 1014 '(* 6 169))  ; 67B
(lazy-def 1015 '(+ 1014 1))  ; 63B
(lazy-def 1016 '(+ 1000 16))  ; 63B
(lazy-def 1017 '(+ 1001 16))  ; 67B
(lazy-def 1018 '(+ 9 1009))  ; 62B
(lazy-def 1019 '(|1+| 1018))  ; 66B
(lazy-def 1020 '(|1+| 1019))  ; 70B
(lazy-def 1021 '(|1+| 1020))  ; 74B
(lazy-def 1022 '(* 7 146))  ; 72B
(lazy-def 1023 '(+ 1022 1))  ; 68B
(lazy-def 1024 '(^ 4 5))  ; 29B
