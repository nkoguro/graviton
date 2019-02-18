;;;
;;; bdf2png.scm - Generate cp437 font image from bdf font.
;;;
;;;   Copyright (c) 2019 KOGURO, Naoki (naoki@koguro.net)
;;;   All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(use gauche.parseopt)
(use graviton)
(use srfi-1)

(define *encode-mapping*
  '((#x263a . #x01)
    (#x263b . #x02)
    (#x2665 . #x03)
    (#x2666 . #x04)
    (#x2663 . #x05)
    (#x2660 . #x06)
    (#x2022 . #x07)
    (#x25d8 . #x08)
    (#x25cb . #x09)
    (#x25d9 . #x0a)
    (#x2642 . #x0b)
    (#x2640 . #x0c)
    (#x266a . #x0d)
    (#x266b . #x0e)
    (#x263c . #x0f)
    (#x25ba . #x10)
    (#x25c4 . #x11)
    (#x2195 . #x12)
    (#x203c . #x13)
    (#x00b6 . #x14)
    (#x00a7 . #x15)
    (#x25ac . #x16)
    (#x21a8 . #x17)
    (#x2191 . #x18)
    (#x2193 . #x19)
    (#x2192 . #x1a)
    (#x2190 . #x1b)
    (#x221f . #x1c)
    (#x2194 . #x1d)
    (#x25b2 . #x1e)
    (#x25bc . #x1f)

    (#x0020 . 32)
    (#x0021 . 33)
    (#x0022 . 34)
    (#x0023 . 35)
    (#x0024 . 36)
    (#x0025 . 37)
    (#x0026 . 38)
    (#x0027 . 39)
    (#x0028 . 40)
    (#x0029 . 41)
    (#x002A . 42)
    (#x002B . 43)
    (#x002C . 44)
    (#x002D . 45)
    (#x002E . 46)
    (#x002F . 47)
    (#x0030 . 48)
    (#x0031 . 49)
    (#x0032 . 50)
    (#x0033 . 51)
    (#x0034 . 52)
    (#x0035 . 53)
    (#x0036 . 54)
    (#x0037 . 55)
    (#x0038 . 56)
    (#x0039 . 57)
    (#x003A . 58)
    (#x003B . 59)
    (#x003C . 60)
    (#x003D . 61)
    (#x003E . 62)
    (#x003F . 63)
    (#x0040 . 64)
    (#x0041 . 65)
    (#x0042 . 66)
    (#x0043 . 67)
    (#x0044 . 68)
    (#x0045 . 69)
    (#x0046 . 70)
    (#x0047 . 71)
    (#x0048 . 72)
    (#x0049 . 73)
    (#x004A . 74)
    (#x004B . 75)
    (#x004C . 76)
    (#x004D . 77)
    (#x004E . 78)
    (#x004F . 79)
    (#x0050 . 80)
    (#x0051 . 81)
    (#x0052 . 82)
    (#x0053 . 83)
    (#x0054 . 84)
    (#x0055 . 85)
    (#x0056 . 86)
    (#x0057 . 87)
    (#x0058 . 88)
    (#x0059 . 89)
    (#x005A . 90)
    (#x005B . 91)
    (#x005C . 92)
    (#x005D . 93)
    (#x005E . 94)
    (#x005F . 95)
    (#x0060 . 96)
    (#x0061 . 97)
    (#x0062 . 98)
    (#x0063 . 99)
    (#x0064 . 100)
    (#x0065 . 101)
    (#x0066 . 102)
    (#x0067 . 103)
    (#x0068 . 104)
    (#x0069 . 105)
    (#x006A . 106)
    (#x006B . 107)
    (#x006C . 108)
    (#x006D . 109)
    (#x006E . 110)
    (#x006F . 111)
    (#x0070 . 112)
    (#x0071 . 113)
    (#x0072 . 114)
    (#x0073 . 115)
    (#x0074 . 116)
    (#x0075 . 117)
    (#x0076 . 118)
    (#x0077 . 119)
    (#x0078 . 120)
    (#x0079 . 121)
    (#x007A . 122)
    (#x007B . 123)
    (#x007C . 124)
    (#x007D . 125)
    (#x007E . 126)
    (#x2302 . #x7f)
    (#x00C7 . #x80)
    (#x00FC . 129)
    (#x00E9 . 130)
    (#x00E2 . 131)
    (#x00E4 . 132)
    (#x00E0 . 133)
    (#x00E5 . 134)
    (#x00E7 . 135)
    (#x00EA . 136)
    (#x00EB . 137)
    (#x00E8 . 138)
    (#x00EF . 139)
    (#x00EE . 140)
    (#x00EC . 141)
    (#x00C4 . 142)
    (#x00C5 . 143)
    (#x00C9 . 144)
    (#x00E6 . 145)
    (#x00C6 . 146)
    (#x00F4 . 147)
    (#x00F6 . 148)
    (#x00F2 . 149)
    (#x00FB . 150)
    (#x00F9 . 151)
    (#x00FF . 152)
    (#x00D6 . 153)
    (#x00DC . 154)
    (#x00A2 . 155)
    (#x00A3 . 156)
    (#x00A5 . 157)
    (#x20A7 . 158)
    (#x0192 . 159)
    (#x00E1 . 160)
    (#x00ED . 161)
    (#x00F3 . 162)
    (#x00FA . 163)
    (#x00F1 . 164)
    (#x00D1 . 165)
    (#x00AA . 166)
    (#x00BA . 167)
    (#x00BF . 168)
    (#x2310 . 169)
    (#x00AC . 170)
    (#x00BD . 171)
    (#x00BC . 172)
    (#x00A1 . 173)
    (#x00AB . 174)
    (#x00BB . 175)
    (#x2591 . 176)
    (#x2592 . 177)
    (#x2593 . 178)
    (#x2502 . 179)
    (#x2524 . 180)
    (#x2561 . 181)
    (#x2562 . 182)
    (#x2556 . 183)
    (#x2555 . 184)
    (#x2563 . 185)
    (#x2551 . 186)
    (#x2557 . 187)
    (#x255D . 188)
    (#x255C . 189)
    (#x255B . 190)
    (#x2510 . 191)
    (#x2514 . 192)
    (#x2534 . 193)
    (#x252C . 194)
    (#x251C . 195)
    (#x2500 . 196)
    (#x253C . 197)
    (#x255E . 198)
    (#x255F . 199)
    (#x255A . 200)
    (#x2554 . 201)
    (#x2569 . 202)
    (#x2566 . 203)
    (#x2560 . 204)
    (#x2550 . 205)
    (#x256C . 206)
    (#x2567 . 207)
    (#x2568 . 208)
    (#x2564 . 209)
    (#x2565 . 210)
    (#x2559 . 211)
    (#x2558 . 212)
    (#x2552 . 213)
    (#x2553 . 214)
    (#x256B . 215)
    (#x256A . 216)
    (#x2518 . 217)
    (#x250C . 218)
    (#x2588 . 219)
    (#x2584 . 220)
    (#x258C . 221)
    (#x2590 . 222)
    (#x2580 . 223)
    (#x03B1 . 224)
    (#x00DF . 225)
    (#x0393 . 226)
    (#x03C0 . 227)
    (#x03A3 . 228)
    (#x03C3 . 229)
    (#x00B5 . 230)
    (#x03C4 . 231)
    (#x03A6 . 232)
    (#x0398 . 233)
    (#x03A9 . 234)
    (#x03B4 . 235)
    (#x221E . 236)
    (#x03C6 . 237)
    (#x03B5 . 238)
    (#x2229 . 239)
    (#x2261 . 240)
    (#x00B1 . 241)
    (#x2265 . 242)
    (#x2264 . 243)
    (#x2320 . 244)
    (#x2321 . 245)
    (#x00F7 . 246)
    (#x2248 . 247)
    (#x00B0 . 248)
    (#x2219 . 249)
    (#x00B7 . 250)
    (#x221A . 251)
    (#x207F . 252)
    (#x00B2 . 253)
    (#x25A0 . 254)
    (#x00A0 . 255)
    ))

(define (round8 v)
  (* (ceiling->exact (/. v 8)) 8))

(define (main args)
  (let-args (cdr args)
      ((wide? "w|wide")
       (size "s|size=s" "7x14")
       (filename "f|filename=s" #f)
    . restargs)
  (let* ((w+h (string-split size "x"))
         (width (* (if wide? 2 1) (round8 (string->number (first w+h)))))
         (height (round8 (string->number (second w+h))))
         (img (make-image (* width 16) (* height 16))))
    (call-with-input-file (car restargs)
      (lambda (in)
        (define (%encoding)
          (and-let* ((line (read-line in))
                     ((not (eof-object? line))))
            (rxmatch-if (#/ENCODING\s+(\d+)/ line) (_ arg)
              (let1 code (let1 v (string->number arg)
                           (assoc-ref *encode-mapping* v -1))
                (if (<= 0 code 255)
                    (%bitmap code)
                    (%encoding)))
              (%encoding))))
        (define (%bitmap code)
          (and-let* ((line (read-line in))
                     ((not (eof-object? line))))
            (rxmatch-if (#/BITMAP/ line) (_)
              (%data code)
              (%bitmap code))))
        (define (%data code)
          (let ((sx (* (remainder code 16) width))
                (sy (* (quotient code 16) height)))
            (let loop ((line (read-line in))
                       (y 0))
              (cond
                ((equal? line "ENDCHAR")
                 (%encoding))
                (else
                 (let ((v (string->number line 16))
                       (index (- (* (string-length line) 4) 1)))
                   (do ((v v (ash v 1))
                        (x 0 (+ x (if wide? 2 1))))
                       ((= x width) (loop (read-line in) (+ y 1)))
                     (when (logbit? index v)
                       (draw-point img (list (+ sx x) (+ sy y)) (color 'white))
                       (when wide?
                         (draw-point img (list (+ sx x 1) (+ sy y)) (color 'white)))))))))))
        (%encoding)))
    (cond
      (filename
       (save-image img filename))
      (else
       (display-image img)))))
  0)