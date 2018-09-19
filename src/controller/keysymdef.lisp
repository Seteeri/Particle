;;;; /***********************************************************
;;;; Copyright 1987, 1994, 1998  The Open Group

;;;; Permission to use, copy, modify, distribute, and sell this software and its
;;;; documentation for any purpose is hereby granted without fee, provided that
;;;; the above copyright notice appear in all copies and that both that
;;;; copyright notice and this permission notice appear in supporting
;;;; documentation.

;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; Except as contained in this notice, the name of The Open Group shall
;;;; not be used in advertising or otherwise to promote the sale, use or
;;;; other dealings in this Software without prior written authorization
;;;; from The Open Group.


;;;; Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts

;;;;                         All Rights Reserved

;;;; Permission to use, copy, modify, and distribute this software and its
;;;; documentation for any purpose and without fee is hereby granted,
;;;; provided that the above copyright notice appear in all copies and that
;;;; both that copyright notice and this permission notice appear in
;;;; supporting documentation, and that the name of Digital not be
;;;; used in advertising or publicity pertaining to distribution of the
;;;; software without specific, written prior permission.

;;;; DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;;;; ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;;;; DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;;;; ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;;;; WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;;;; ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;;;; SOFTWARE.

;;;; ******************************************************************/

;;;; /*
;;;;  * The "X11 Window System Protocol" standard defines in Appendix A the
;;;;  * keysym codes. These 29-bit integer values identify characters or
;;;;  * functions associated with each key (e.g., via the visible
;;;;  * engraving) of a keyboard layout. This file assigns mnemonic macro
;;;;  * names for these keysyms.
;;;;  *
;;;;  * This file is also compiled (by src/util/makekeys.c in libX11) into
;;;;  * hash tables that can be accessed with X11 library functions such as
;;;;  * XStringToKeysym() and XKeysymToString().
;;;;  *
;;;;  * Where a keysym corresponds one-to-one to an ISO 10646 / Unicode
;;;;  * character, this is noted in a comment that provides both the U+xxxx
;;;;  * Unicode position, as well as the official Unicode name of the
;;;;  * character.
;;;;  *
;;;;  * Where the correspondence is either not one-to-one or semantically
;;;;  * unclear, the Unicode position and name are enclosed in
;;;;  * parentheses. Such legacy keysyms should be considered deprecated
;;;;  * and are not recommended for use in future keyboard mappings.
;;;;  *
;;;;  * For any future extension of the keysyms with characters already
;;;;  * found in ISO 10646 / Unicode, the following algorithm shall be
;;;;  * used. The new keysym code position will simply be the character's
;;;;  * Unicode number plus 0x01000000. The keysym values in the range
;;;;  * 0x01000100 to 0x0110ffff are reserved to represent Unicode
;;;;  * characters in the range U+0100 to U+10FFFF.
;;;;  * 
;;;;  * While most newer Unicode-based X11 clients do already accept
;;;;  * Unicode-mapped keysyms in the range 0x01000100 to 0x0110ffff, it
;;;;  * will remain necessary for clients -- in the interest of
;;;;  * compatibility with existing servers -- to also understand the
;;;;  * existing legacy keysym values in the range 0x0100 to 0x20ff.
;;;;  *
;;;;  * Where several mnemonic names are defined for the same keysym in this
;;;;  * file, all but the first one listed should be considered deprecated.
;;;;  *
;;;;  * Mnemonic names for keysyms are defined in this file with lines
;;;;  * that match one of these Perl regular expressions:
;;;;  *
;;;;  *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*\/\* U+([0-9A-F]{4,6}) (.*) \*\/\s*$/
;;;;  *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*\/\*\(U+([0-9A-F]{4,6}) (.*)\)\*\/\s*$/
;;;;  *    /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*(\/\*\s*(.*)\s*\*\/)?\s*$/
;;;;  *
;;;;  * Before adding new keysyms, please do consider the following: In
;;;;  * addition to the keysym names defined in this file, the
;;;;  * XStringToKeysym() and XKeysymToString() functions will also handle
;;;;  * any keysym string of the form "U0020" to "U007E" and "U00A0" to
;;;;  * "U10FFFF" for all possible Unicode characters. In other words,
;;;;  * every possible Unicode character has already a keysym string
;;;;  * defined algorithmically, even if it is not listed here. Therefore,
;;;;  * defining an additional keysym macro is only necessary where a
;;;;  * non-hexadecimal mnemonic name is needed, or where the new keysym
;;;;  * does not represent any existing Unicode character.
;;;;  *
;;;;  * When adding new keysyms to this file, do not forget to also update the
;;;;  * following as needed:
;;;;  *
;;;;  *   - the mappings in src/KeyBind.c in the repo
;;;;  *     git://anongit.freedesktop.org/xorg/lib/libX11.git
;;;;  *
;;;;  *   - the protocol specification in specs/keysyms.xml
;;;;  *     in the repo git://anongit.freedesktop.org/xorg/proto/x11proto.git
;;;;  *
;;;;  */

(in-package :protoform.controller)

(defconstant +xk-voidsymbol+ #xffffff) ;  Void symbol 

;;; #ifdef XK_MISCELLANY
;;; /*
;;;  * TTY function keys, cleverly chosen to map to ASCII, for convenience of
;;;  * programming, but could have been arbitrary (at the cost of lookup
;;;  * tables in client code).
;;;  */

(defconstant +xk-backspace+ #xff08) ;  Back space, back char 
(defconstant +xk-tab+ #xff09)
(defconstant +xk-linefeed+ #xff0a) ;  Linefeed, LF 
(defconstant +xk-clear+ #xff0b)
(defconstant +xk-return+ #xff0d) ;  Return, enter 
(defconstant +xk-pause+ #xff13) ;  Pause, hold 
(defconstant +xk-scroll-lock+ #xff14)
(defconstant +xk-sys-req+ #xff15)
(defconstant +xk-escape+ #xff1b)
(defconstant +xk-delete+ #xffff) ;  Delete, rubout 



;;; /* International & multi-key character composition */

(defconstant +xk-multi-key+ #xff20) ;  Multi-key character compose 
(defconstant +xk-codeinput+ #xff37)
(defconstant +xk-singlecandidate+ #xff3c)
(defconstant +xk-multiplecandidate+ #xff3d)
(defconstant +xk-previouscandidate+ #xff3e)

;;; /* Japanese keyboard support */

(defconstant +xk-kanji+ #xff21) ;  Kanji, Kanji convert 
(defconstant +xk-muhenkan+ #xff22) ;  Cancel Conversion 
(defconstant +xk-henkan-mode+ #xff23) ;  Start/Stop Conversion 
(defconstant +xk-henkan+ #xff23) ;  Alias for Henkan_Mode 
(defconstant +xk-romaji+ #xff24) ;  to Romaji 
(defconstant +xk-hiragana+ #xff25) ;  to Hiragana 
(defconstant +xk-katakana+ #xff26) ;  to Katakana 
(defconstant +xk-hiragana-katakana+ #xff27) ;  Hiragana/Katakana toggle 
(defconstant +xk-zenkaku+ #xff28) ;  to Zenkaku 
(defconstant +xk-hankaku+ #xff29) ;  to Hankaku 
(defconstant +xk-zenkaku-hankaku+ #xff2a) ;  Zenkaku/Hankaku toggle 
(defconstant +xk-touroku+ #xff2b) ;  Add to Dictionary 
(defconstant +xk-massyo+ #xff2c) ;  Delete from Dictionary 
(defconstant +xk-kana-lock+ #xff2d) ;  Kana Lock 
(defconstant +xk-kana-shift+ #xff2e) ;  Kana Shift 
(defconstant +xk-eisu-shift+ #xff2f) ;  Alphanumeric Shift 
(defconstant +xk-eisu-toggle+ #xff30) ;  Alphanumeric toggle 
(defconstant +xk-kanji-bangou+ #xff37) ;  Codeinput 
(defconstant +xk-zen-koho+ #xff3d) ;  Multiple/All Candidate(s) 
(defconstant +xk-mae-koho+ #xff3e) ;  Previous Candidate 

;;; /* 0xff31 thru 0xff3f are under XK_KOREAN */

;;; /* Cursor control & motion */

(defconstant +xk-home+ #xff50)
(defconstant +xk-left+ #xff51) ;  Move left, left arrow 
(defconstant +xk-up+ #xff52) ;  Move up, up arrow 
(defconstant +xk-right+ #xff53) ;  Move right, right arrow 
(defconstant +xk-down+ #xff54) ;  Move down, down arrow 
(defconstant +xk-prior+ #xff55) ;  Prior, previous 
(defconstant +xk-page-up+ #xff55)
(defconstant +xk-next+ #xff56) ;  Next 
(defconstant +xk-page-down+ #xff56)
(defconstant +xk-end+ #xff57) ;  EOL 
(defconstant +xk-begin+ #xff58) ;  BOL 


;;; /* Misc functions */

(defconstant +xk-select+ #xff60) ;  Select, mark 
(defconstant +xk-print+ #xff61)
(defconstant +xk-execute+ #xff62) ;  Execute, run, do 
(defconstant +xk-insert+ #xff63) ;  Insert, insert here 
(defconstant +xk-undo+ #xff65)
(defconstant +xk-redo+ #xff66) ;  Redo, again 
(defconstant +xk-menu+ #xff67)
(defconstant +xk-find+ #xff68) ;  Find, search 
(defconstant +xk-cancel+ #xff69) ;  Cancel, stop, abort, exit 
(defconstant +xk-help+ #xff6a) ;  Help 
(defconstant +xk-break+ #xff6b)
(defconstant +xk-mode-switch+ #xff7e) ;  Character set switch 
(defconstant +xk-script-switch+ #xff7e) ;  Alias for mode_switch 
(defconstant +xk-num-lock+ #xff7f)

;;; /* Keypad functions, keypad numbers cleverly chosen to map to ASCII */

(defconstant +xk-kp-space+ #xff80) ;  Space 
(defconstant +xk-kp-tab+ #xff89)
(defconstant +xk-kp-enter+ #xff8d) ;  Enter 
(defconstant +xk-kp-f1+ #xff91) ;  PF1, KP_A, ... 
(defconstant +xk-kp-f2+ #xff92)
(defconstant +xk-kp-f3+ #xff93)
(defconstant +xk-kp-f4+ #xff94)
(defconstant +xk-kp-home+ #xff95)
(defconstant +xk-kp-left+ #xff96)
(defconstant +xk-kp-up+ #xff97)
(defconstant +xk-kp-right+ #xff98)
(defconstant +xk-kp-down+ #xff99)
(defconstant +xk-kp-prior+ #xff9a)
(defconstant +xk-kp-page-up+ #xff9a)
(defconstant +xk-kp-next+ #xff9b)
(defconstant +xk-kp-page-down+ #xff9b)
(defconstant +xk-kp-end+ #xff9c)
(defconstant +xk-kp-begin+ #xff9d)
(defconstant +xk-kp-insert+ #xff9e)
(defconstant +xk-kp-delete+ #xff9f)
(defconstant +xk-kp-equal+ #xffbd) ;  Equals 
(defconstant +xk-kp-multiply+ #xffaa)
(defconstant +xk-kp-add+ #xffab)
(defconstant +xk-kp-separator+ #xffac) ;  Separator, often comma 
(defconstant +xk-kp-subtract+ #xffad)
(defconstant +xk-kp-decimal+ #xffae)
(defconstant +xk-kp-divide+ #xffaf)

(defconstant +xk-kp-0+ #xffb0)
(defconstant +xk-kp-1+ #xffb1)
(defconstant +xk-kp-2+ #xffb2)
(defconstant +xk-kp-3+ #xffb3)
(defconstant +xk-kp-4+ #xffb4)
(defconstant +xk-kp-5+ #xffb5)
(defconstant +xk-kp-6+ #xffb6)
(defconstant +xk-kp-7+ #xffb7)
(defconstant +xk-kp-8+ #xffb8)
(defconstant +xk-kp-9+ #xffb9)



;;; /*
;;;  * Auxiliary functions; note the duplicate definitions for left and right
;;;  * function keys;  Sun keyboards and a few other manufacturers have such
;;;  * function key groups on the left and/or right sides of the keyboard.
;;;  * We've not found a keyboard with more than 35 function keys total.
;;;  */

(defconstant +xk-f1+ #xffbe)
(defconstant +xk-f2+ #xffbf)
(defconstant +xk-f3+ #xffc0)
(defconstant +xk-f4+ #xffc1)
(defconstant +xk-f5+ #xffc2)
(defconstant +xk-f6+ #xffc3)
(defconstant +xk-f7+ #xffc4)
(defconstant +xk-f8+ #xffc5)
(defconstant +xk-f9+ #xffc6)
(defconstant +xk-f10+ #xffc7)
(defconstant +xk-f11+ #xffc8)
(defconstant +xk-l1+ #xffc8)
(defconstant +xk-f12+ #xffc9)
(defconstant +xk-l2+ #xffc9)
(defconstant +xk-f13+ #xffca)
(defconstant +xk-l3+ #xffca)
(defconstant +xk-f14+ #xffcb)
(defconstant +xk-l4+ #xffcb)
(defconstant +xk-f15+ #xffcc)
(defconstant +xk-l5+ #xffcc)
(defconstant +xk-f16+ #xffcd)
(defconstant +xk-l6+ #xffcd)
(defconstant +xk-f17+ #xffce)
(defconstant +xk-l7+ #xffce)
(defconstant +xk-f18+ #xffcf)
(defconstant +xk-l8+ #xffcf)
(defconstant +xk-f19+ #xffd0)
(defconstant +xk-l9+ #xffd0)
(defconstant +xk-f20+ #xffd1)
(defconstant +xk-l10+ #xffd1)
(defconstant +xk-f21+ #xffd2)
(defconstant +xk-r1+ #xffd2)
(defconstant +xk-f22+ #xffd3)
(defconstant +xk-r2+ #xffd3)
(defconstant +xk-f23+ #xffd4)
(defconstant +xk-r3+ #xffd4)
(defconstant +xk-f24+ #xffd5)
(defconstant +xk-r4+ #xffd5)
(defconstant +xk-f25+ #xffd6)
(defconstant +xk-r5+ #xffd6)
(defconstant +xk-f26+ #xffd7)
(defconstant +xk-r6+ #xffd7)
(defconstant +xk-f27+ #xffd8)
(defconstant +xk-r7+ #xffd8)
(defconstant +xk-f28+ #xffd9)
(defconstant +xk-r8+ #xffd9)
(defconstant +xk-f29+ #xffda)
(defconstant +xk-r9+ #xffda)
(defconstant +xk-f30+ #xffdb)
(defconstant +xk-r10+ #xffdb)
(defconstant +xk-f31+ #xffdc)
(defconstant +xk-r11+ #xffdc)
(defconstant +xk-f32+ #xffdd)
(defconstant +xk-r12+ #xffdd)
(defconstant +xk-f33+ #xffde)
(defconstant +xk-r13+ #xffde)
(defconstant +xk-f34+ #xffdf)
(defconstant +xk-r14+ #xffdf)
(defconstant +xk-f35+ #xffe0)
(defconstant +xk-r15+ #xffe0)

;;; /* Modifiers */

(defconstant +xk-shift-l+ #xffe1) ;  Left shift 
(defconstant +xk-shift-r+ #xffe2) ;  Right shift 
(defconstant +xk-control-l+ #xffe3) ;  Left control 
(defconstant +xk-control-r+ #xffe4) ;  Right control 
(defconstant +xk-caps-lock+ #xffe5) ;  Caps lock 
(defconstant +xk-shift-lock+ #xffe6) ;  Shift lock 

(defconstant +xk-meta-l+ #xffe7) ;  Left meta 
(defconstant +xk-meta-r+ #xffe8) ;  Right meta 
(defconstant +xk-alt-l+ #xffe9) ;  Left alt 
(defconstant +xk-alt-r+ #xffea) ;  Right alt 
(defconstant +xk-super-l+ #xffeb) ;  Left super 
(defconstant +xk-super-r+ #xffec) ;  Right super 
(defconstant +xk-hyper-l+ #xffed) ;  Left hyper 
(defconstant +xk-hyper-r+ #xffee) ;  Right hyper 
;;; #endif /* XK_MISCELLANY */

;;; /*
;;;  * Keyboard (XKB) Extension function and modifier keys
;;;  * (from Appendix C of "The X Keyboard Extension: Protocol Specification")
;;;  * Byte 3 = 0xfe
;;;  */

;;; #ifdef XK_XKB_KEYS
(defconstant +xk-iso-lock+ #xfe01)
(defconstant +xk-iso-level2-latch+ #xfe02)
(defconstant +xk-iso-level3-shift+ #xfe03)
(defconstant +xk-iso-level3-latch+ #xfe04)
(defconstant +xk-iso-level3-lock+ #xfe05)
(defconstant +xk-iso-level5-shift+ #xfe11)
(defconstant +xk-iso-level5-latch+ #xfe12)
(defconstant +xk-iso-level5-lock+ #xfe13)
(defconstant +xk-iso-group-shift+ #xff7e) ;  Alias for mode_switch 
(defconstant +xk-iso-group-latch+ #xfe06)
(defconstant +xk-iso-group-lock+ #xfe07)
(defconstant +xk-iso-next-group+ #xfe08)
(defconstant +xk-iso-next-group-lock+ #xfe09)
(defconstant +xk-iso-prev-group+ #xfe0a)
(defconstant +xk-iso-prev-group-lock+ #xfe0b)
(defconstant +xk-iso-first-group+ #xfe0c)
(defconstant +xk-iso-first-group-lock+ #xfe0d)
(defconstant +xk-iso-last-group+ #xfe0e)
(defconstant +xk-iso-last-group-lock+ #xfe0f)

(defconstant +xk-iso-left-tab+ #xfe20)
(defconstant +xk-iso-move-line-up+ #xfe21)
(defconstant +xk-iso-move-line-down+ #xfe22)
(defconstant +xk-iso-partial-line-up+ #xfe23)
(defconstant +xk-iso-partial-line-down+ #xfe24)
(defconstant +xk-iso-partial-space-left+ #xfe25)
(defconstant +xk-iso-partial-space-right+ #xfe26)
(defconstant +xk-iso-set-margin-left+ #xfe27)
(defconstant +xk-iso-set-margin-right+ #xfe28)
(defconstant +xk-iso-release-margin-left+ #xfe29)
(defconstant +xk-iso-release-margin-right+ #xfe2a)
(defconstant +xk-iso-release-both-margins+ #xfe2b)
(defconstant +xk-iso-fast-cursor-left+ #xfe2c)
(defconstant +xk-iso-fast-cursor-right+ #xfe2d)
(defconstant +xk-iso-fast-cursor-up+ #xfe2e)
(defconstant +xk-iso-fast-cursor-down+ #xfe2f)
(defconstant +xk-iso-continuous-underline+ #xfe30)
(defconstant +xk-iso-discontinuous-underline+ #xfe31)
(defconstant +xk-iso-emphasize+ #xfe32)
(defconstant +xk-iso-center-object+ #xfe33)
(defconstant +xk-iso-enter+ #xfe34)

(defconstant +xk-dead-grave+ #xfe50)
(defconstant +xk-dead-acute+ #xfe51)
(defconstant +xk-dead-circumflex+ #xfe52)
(defconstant +xk-dead-tilde+ #xfe53)
(defconstant +xk-dead-perispomeni+ #xfe53) ;  alias for dead_tilde 
(defconstant +xk-dead-macron+ #xfe54)
(defconstant +xk-dead-breve+ #xfe55)
(defconstant +xk-dead-abovedot+ #xfe56)
(defconstant +xk-dead-diaeresis+ #xfe57)
(defconstant +xk-dead-abovering+ #xfe58)
(defconstant +xk-dead-doubleacute+ #xfe59)
(defconstant +xk-dead-caron+ #xfe5a)
(defconstant +xk-dead-cedilla+ #xfe5b)
(defconstant +xk-dead-ogonek+ #xfe5c)
(defconstant +xk-dead-iota+ #xfe5d)
(defconstant +xk-dead-voiced-sound+ #xfe5e)
(defconstant +xk-dead-semivoiced-sound+ #xfe5f)
(defconstant +xk-dead-belowdot+ #xfe60)
(defconstant +xk-dead-hook+ #xfe61)
(defconstant +xk-dead-horn+ #xfe62)
(defconstant +xk-dead-stroke+ #xfe63)
(defconstant +xk-dead-abovecomma+ #xfe64)
(defconstant +xk-dead-psili+ #xfe64) ;  alias for dead_abovecomma 
(defconstant +xk-dead-abovereversedcomma+ #xfe65)
(defconstant +xk-dead-dasia+ #xfe65) ;  alias for dead_abovereversedcomma 
(defconstant +xk-dead-doublegrave+ #xfe66)
(defconstant +xk-dead-belowring+ #xfe67)
(defconstant +xk-dead-belowmacron+ #xfe68)
(defconstant +xk-dead-belowcircumflex+ #xfe69)
(defconstant +xk-dead-belowtilde+ #xfe6a)
(defconstant +xk-dead-belowbreve+ #xfe6b)
(defconstant +xk-dead-belowdiaeresis+ #xfe6c)
(defconstant +xk-dead-invertedbreve+ #xfe6d)
(defconstant +xk-dead-belowcomma+ #xfe6e)
(defconstant +xk-dead-currency+ #xfe6f)

;;; /* extra dead elements for German T3 layout */
(defconstant +xk-dead-lowline+ #xfe90)
(defconstant +xk-dead-aboveverticalline+ #xfe91)
(defconstant +xk-dead-belowverticalline+ #xfe92)
(defconstant +xk-dead-longsolidusoverlay+ #xfe93)

;;; /* dead vowels for universal syllable entry */
;; (defconstant +xk-dead-a+ #xfe80)
;; (defconstant +xk-dead-a+ #xfe81)
;; (defconstant +xk-dead-e+ #xfe82)
;; (defconstant +xk-dead-e+ #xfe83)
;; (defconstant +xk-dead-i+ #xfe84)
;; (defconstant +xk-dead-i+ #xfe85)
;; (defconstant +xk-dead-o+ #xfe86)
;; (defconstant +xk-dead-o+ #xfe87)
;; (defconstant +xk-dead-u+ #xfe88)
;; (defconstant +xk-dead-u+ #xfe89)
(defconstant +xk-dead-small-schwa+ #xfe8a)
(defconstant +xk-dead-capital-schwa+ #xfe8b)

(defconstant +xk-dead-greek+ #xfe8c)

(defconstant +xk-first-virtual-screen+ #xfed0)
(defconstant +xk-prev-virtual-screen+ #xfed1)
(defconstant +xk-next-virtual-screen+ #xfed2)
(defconstant +xk-last-virtual-screen+ #xfed4)
(defconstant +xk-terminate-server+ #xfed5)

(defconstant +xk-accessx-enable+ #xfe70)
(defconstant +xk-accessx-feedback-enable+ #xfe71)
(defconstant +xk-repeatkeys-enable+ #xfe72)
(defconstant +xk-slowkeys-enable+ #xfe73)
(defconstant +xk-bouncekeys-enable+ #xfe74)
(defconstant +xk-stickykeys-enable+ #xfe75)
(defconstant +xk-mousekeys-enable+ #xfe76)
(defconstant +xk-mousekeys-accel-enable+ #xfe77)
(defconstant +xk-overlay1-enable+ #xfe78)
(defconstant +xk-overlay2-enable+ #xfe79)
(defconstant +xk-audiblebell-enable+ #xfe7a)

(defconstant +xk-pointer-left+ #xfee0)
(defconstant +xk-pointer-right+ #xfee1)
(defconstant +xk-pointer-up+ #xfee2)
(defconstant +xk-pointer-down+ #xfee3)
(defconstant +xk-pointer-upleft+ #xfee4)
(defconstant +xk-pointer-upright+ #xfee5)
(defconstant +xk-pointer-downleft+ #xfee6)
(defconstant +xk-pointer-downright+ #xfee7)
(defconstant +xk-pointer-button-dflt+ #xfee8)
(defconstant +xk-pointer-button1+ #xfee9)
(defconstant +xk-pointer-button2+ #xfeea)
(defconstant +xk-pointer-button3+ #xfeeb)
(defconstant +xk-pointer-button4+ #xfeec)
(defconstant +xk-pointer-button5+ #xfeed)
(defconstant +xk-pointer-dblclick-dflt+ #xfeee)
(defconstant +xk-pointer-dblclick1+ #xfeef)
(defconstant +xk-pointer-dblclick2+ #xfef0)
(defconstant +xk-pointer-dblclick3+ #xfef1)
(defconstant +xk-pointer-dblclick4+ #xfef2)
(defconstant +xk-pointer-dblclick5+ #xfef3)
(defconstant +xk-pointer-drag-dflt+ #xfef4)
(defconstant +xk-pointer-drag1+ #xfef5)
(defconstant +xk-pointer-drag2+ #xfef6)
(defconstant +xk-pointer-drag3+ #xfef7)
(defconstant +xk-pointer-drag4+ #xfef8)
(defconstant +xk-pointer-drag5+ #xfefd)

(defconstant +xk-pointer-enablekeys+ #xfef9)
(defconstant +xk-pointer-accelerate+ #xfefa)
(defconstant +xk-pointer-dfltbtnnext+ #xfefb)
(defconstant +xk-pointer-dfltbtnprev+ #xfefc)

;;; /* Single-Stroke Multiple-Character N-Graph Keysyms For The X Input Method */

;; (defconstant +xk-ch+ #xfea0)
;; (defconstant +xk-ch+ #xfea1)
;; (defconstant +xk-ch+ #xfea2)
;; (defconstant +xk-c-h+ #xfea3)
;; (defconstant +xk-c-h+ #xfea4)
;; (defconstant +xk-c-h+ #xfea5)

;;; #endif /* XK_XKB_KEYS */

;;; /*
;;;  * 3270 Terminal Keys
;;;  * Byte 3 = 0xfd
;;;  */

;;; #ifdef XK_3270
(defconstant +xk-3270-duplicate+ #xfd01)
(defconstant +xk-3270-fieldmark+ #xfd02)
(defconstant +xk-3270-right2+ #xfd03)
(defconstant +xk-3270-left2+ #xfd04)
(defconstant +xk-3270-backtab+ #xfd05)
(defconstant +xk-3270-eraseeof+ #xfd06)
(defconstant +xk-3270-eraseinput+ #xfd07)
(defconstant +xk-3270-reset+ #xfd08)
(defconstant +xk-3270-quit+ #xfd09)
(defconstant +xk-3270-pa1+ #xfd0a)
(defconstant +xk-3270-pa2+ #xfd0b)
(defconstant +xk-3270-pa3+ #xfd0c)
(defconstant +xk-3270-test+ #xfd0d)
(defconstant +xk-3270-attn+ #xfd0e)
(defconstant +xk-3270-cursorblink+ #xfd0f)
(defconstant +xk-3270-altcursor+ #xfd10)
(defconstant +xk-3270-keyclick+ #xfd11)
(defconstant +xk-3270-jump+ #xfd12)
(defconstant +xk-3270-ident+ #xfd13)
(defconstant +xk-3270-rule+ #xfd14)
(defconstant +xk-3270-copy+ #xfd15)
(defconstant +xk-3270-play+ #xfd16)
(defconstant +xk-3270-setup+ #xfd17)
(defconstant +xk-3270-record+ #xfd18)
(defconstant +xk-3270-changescreen+ #xfd19)
(defconstant +xk-3270-deleteword+ #xfd1a)
(defconstant +xk-3270-exselect+ #xfd1b)
(defconstant +xk-3270-cursorselect+ #xfd1c)
(defconstant +xk-3270-printscreen+ #xfd1d)
(defconstant +xk-3270-enter+ #xfd1e)
;;; #endif /* XK_3270 */

;;; /*
;;;  * Latin 1
;;;  * (ISO/IEC 8859-1 = Unicode U+0020..U+00FF)
;;;  * Byte 3 = 0
;;;  */
;;; #ifdef XK_LATIN1
(defconstant +xk-space+ #x0020) ;  U+0020 SPACE 
(defconstant +xk-exclam+ #x0021) ;  U+0021 EXCLAMATION MARK 
(defconstant +xk-quotedbl+ #x0022) ;  U+0022 QUOTATION MARK 
(defconstant +xk-numbersign+ #x0023) ;  U+0023 NUMBER SIGN 
(defconstant +xk-dollar+ #x0024) ;  U+0024 DOLLAR SIGN 
(defconstant +xk-percent+ #x0025) ;  U+0025 PERCENT SIGN 
(defconstant +xk-ampersand+ #x0026) ;  U+0026 AMPERSAND 
(defconstant +xk-apostrophe+ #x0027) ;  U+0027 APOSTROPHE 
(defconstant +xk-quoteright+ #x0027) ;  deprecated 
(defconstant +xk-parenleft+ #x0028) ;  U+0028 LEFT PARENTHESIS 
(defconstant +xk-parenright+ #x0029) ;  U+0029 RIGHT PARENTHESIS 
(defconstant +xk-asterisk+ #x002a) ;  U+002A ASTERISK 
(defconstant +xk-plus+ #x002b) ;  U+002B PLUS SIGN 
(defconstant +xk-comma+ #x002c) ;  U+002C COMMA 
(defconstant +xk-minus+ #x002d) ;  U+002D HYPHEN-MINUS 
(defconstant +xk-period+ #x002e) ;  U+002E FULL STOP 
(defconstant +xk-slash+ #x002f) ;  U+002F SOLIDUS 
(defconstant +xk-0+ #x0030) ;  U+0030 DIGIT ZERO 
(defconstant +xk-1+ #x0031) ;  U+0031 DIGIT ONE 
(defconstant +xk-2+ #x0032) ;  U+0032 DIGIT TWO 
(defconstant +xk-3+ #x0033) ;  U+0033 DIGIT THREE 
(defconstant +xk-4+ #x0034) ;  U+0034 DIGIT FOUR 
(defconstant +xk-5+ #x0035) ;  U+0035 DIGIT FIVE 
(defconstant +xk-6+ #x0036) ;  U+0036 DIGIT SIX 
(defconstant +xk-7+ #x0037) ;  U+0037 DIGIT SEVEN 
(defconstant +xk-8+ #x0038) ;  U+0038 DIGIT EIGHT 
(defconstant +xk-9+ #x0039) ;  U+0039 DIGIT NINE 
(defconstant +xk-colon+ #x003a) ;  U+003A COLON 
(defconstant +xk-semicolon+ #x003b) ;  U+003B SEMICOLON 
(defconstant +xk-less+ #x003c) ;  U+003C LESS-THAN SIGN 
(defconstant +xk-equal+ #x003d) ;  U+003D EQUALS SIGN 
(defconstant +xk-greater+ #x003e) ;  U+003E GREATER-THAN SIGN 
(defconstant +xk-question+ #x003f) ;  U+003F QUESTION MARK 
(defconstant +xk-at+ #x0040) ;  U+0040 COMMERCIAL AT 
(defconstant +xk-a-cap+ #x0041) ;  U+0041 LATIN CAPITAL LETTER A 
(defconstant +xk-b-cap+ #x0042) ;  U+0042 LATIN CAPITAL LETTER B 
(defconstant +xk-c-cap+ #x0043) ;  U+0043 LATIN CAPITAL LETTER C 
(defconstant +xk-d-cap+ #x0044) ;  U+0044 LATIN CAPITAL LETTER D 
(defconstant +xk-e-cap+ #x0045) ;  U+0045 LATIN CAPITAL LETTER E 
(defconstant +xk-f-cap+ #x0046) ;  U+0046 LATIN CAPITAL LETTER F 
(defconstant +xk-g-cap+ #x0047) ;  U+0047 LATIN CAPITAL LETTER G 
(defconstant +xk-h-cap+ #x0048) ;  U+0048 LATIN CAPITAL LETTER H 
(defconstant +xk-i-cap+ #x0049) ;  U+0049 LATIN CAPITAL LETTER I 
(defconstant +xk-j-cap+ #x004a) ;  U+004A LATIN CAPITAL LETTER J 
(defconstant +xk-k-cap+ #x004b) ;  U+004B LATIN CAPITAL LETTER K 
(defconstant +xk-l-cap+ #x004c) ;  U+004C LATIN CAPITAL LETTER L 
(defconstant +xk-m-cap+ #x004d) ;  U+004D LATIN CAPITAL LETTER M 
(defconstant +xk-n-cap+ #x004e) ;  U+004E LATIN CAPITAL LETTER N 
(defconstant +xk-o-cap+ #x004f) ;  U+004F LATIN CAPITAL LETTER O 
(defconstant +xk-p-cap+ #x0050) ;  U+0050 LATIN CAPITAL LETTER P 
(defconstant +xk-q-cap+ #x0051) ;  U+0051 LATIN CAPITAL LETTER Q 
(defconstant +xk-r-cap+ #x0052) ;  U+0052 LATIN CAPITAL LETTER R 
(defconstant +xk-s-cap+ #x0053) ;  U+0053 LATIN CAPITAL LETTER S 
(defconstant +xk-t-cap+ #x0054) ;  U+0054 LATIN CAPITAL LETTER T 
(defconstant +xk-u-cap+ #x0055) ;  U+0055 LATIN CAPITAL LETTER U 
(defconstant +xk-v-cap+ #x0056) ;  U+0056 LATIN CAPITAL LETTER V 
(defconstant +xk-w-cap+ #x0057) ;  U+0057 LATIN CAPITAL LETTER W 
(defconstant +xk-x-cap+ #x0058) ;  U+0058 LATIN CAPITAL LETTER X 
(defconstant +xk-y-cap+ #x0059) ;  U+0059 LATIN CAPITAL LETTER Y 
(defconstant +xk-z-cap+ #x005a) ;  U+005A LATIN CAPITAL LETTER Z 
(defconstant +xk-bracketleft+ #x005b) ;  U+005B LEFT SQUARE BRACKET 
(defconstant +xk-backslash+ #x005c) ;  U+005C REVERSE SOLIDUS 
(defconstant +xk-bracketright+ #x005d) ;  U+005D RIGHT SQUARE BRACKET 
(defconstant +xk-asciicircum+ #x005e) ;  U+005E CIRCUMFLEX ACCENT 
(defconstant +xk-underscore+ #x005f) ;  U+005F LOW LINE 
(defconstant +xk-grave+ #x0060) ;  U+0060 GRAVE ACCENT 
(defconstant +xk-quoteleft+ #x0060) ;  deprecated 
(defconstant +xk-a+ #x0061) ;  U+0061 LATIN SMALL LETTER A 
(defconstant +xk-b+ #x0062) ;  U+0062 LATIN SMALL LETTER B 
(defconstant +xk-c+ #x0063) ;  U+0063 LATIN SMALL LETTER C 
(defconstant +xk-d+ #x0064) ;  U+0064 LATIN SMALL LETTER D 
(defconstant +xk-e+ #x0065) ;  U+0065 LATIN SMALL LETTER E 
(defconstant +xk-f+ #x0066) ;  U+0066 LATIN SMALL LETTER F 
(defconstant +xk-g+ #x0067) ;  U+0067 LATIN SMALL LETTER G 
(defconstant +xk-h+ #x0068) ;  U+0068 LATIN SMALL LETTER H 
(defconstant +xk-i+ #x0069) ;  U+0069 LATIN SMALL LETTER I 
(defconstant +xk-j+ #x006a) ;  U+006A LATIN SMALL LETTER J 
(defconstant +xk-k+ #x006b) ;  U+006B LATIN SMALL LETTER K 
(defconstant +xk-l+ #x006c) ;  U+006C LATIN SMALL LETTER L 
(defconstant +xk-m+ #x006d) ;  U+006D LATIN SMALL LETTER M 
(defconstant +xk-n+ #x006e) ;  U+006E LATIN SMALL LETTER N 
(defconstant +xk-o+ #x006f) ;  U+006F LATIN SMALL LETTER O 
(defconstant +xk-p+ #x0070) ;  U+0070 LATIN SMALL LETTER P 
(defconstant +xk-q+ #x0071) ;  U+0071 LATIN SMALL LETTER Q 
(defconstant +xk-r+ #x0072) ;  U+0072 LATIN SMALL LETTER R 
(defconstant +xk-s+ #x0073) ;  U+0073 LATIN SMALL LETTER S 
(defconstant +xk-t+ #x0074) ;  U+0074 LATIN SMALL LETTER T 
(defconstant +xk-u+ #x0075) ;  U+0075 LATIN SMALL LETTER U 
(defconstant +xk-v+ #x0076) ;  U+0076 LATIN SMALL LETTER V 
(defconstant +xk-w+ #x0077) ;  U+0077 LATIN SMALL LETTER W 
(defconstant +xk-x+ #x0078) ;  U+0078 LATIN SMALL LETTER X 
(defconstant +xk-y+ #x0079) ;  U+0079 LATIN SMALL LETTER Y 
(defconstant +xk-z+ #x007a) ;  U+007A LATIN SMALL LETTER Z 
(defconstant +xk-braceleft+ #x007b) ;  U+007B LEFT CURLY BRACKET 
(defconstant +xk-bar+ #x007c) ;  U+007C VERTICAL LINE 
(defconstant +xk-braceright+ #x007d) ;  U+007D RIGHT CURLY BRACKET 
(defconstant +xk-asciitilde+ #x007e) ;  U+007E TILDE 

(defconstant +xk-nobreakspace+ #x00a0) ;  U+00A0 NO-BREAK SPACE 
(defconstant +xk-exclamdown+ #x00a1) ;  U+00A1 INVERTED EXCLAMATION MARK 
(defconstant +xk-cent+ #x00a2) ;  U+00A2 CENT SIGN 
(defconstant +xk-sterling+ #x00a3) ;  U+00A3 POUND SIGN 
(defconstant +xk-currency+ #x00a4) ;  U+00A4 CURRENCY SIGN 
(defconstant +xk-yen+ #x00a5) ;  U+00A5 YEN SIGN 
(defconstant +xk-brokenbar+ #x00a6) ;  U+00A6 BROKEN BAR 
(defconstant +xk-section+ #x00a7) ;  U+00A7 SECTION SIGN 
(defconstant +xk-diaeresis+ #x00a8) ;  U+00A8 DIAERESIS 
(defconstant +xk-copyright+ #x00a9) ;  U+00A9 COPYRIGHT SIGN 
(defconstant +xk-ordfeminine+ #x00aa) ;  U+00AA FEMININE ORDINAL INDICATOR 
(defconstant +xk-guillemotleft+ #x00ab) ;  U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK 
(defconstant +xk-notsign+ #x00ac) ;  U+00AC NOT SIGN 
(defconstant +xk-hyphen+ #x00ad) ;  U+00AD SOFT HYPHEN 
(defconstant +xk-registered+ #x00ae) ;  U+00AE REGISTERED SIGN 
(defconstant +xk-macron+ #x00af) ;  U+00AF MACRON 
(defconstant +xk-degree+ #x00b0) ;  U+00B0 DEGREE SIGN 
(defconstant +xk-plusminus+ #x00b1) ;  U+00B1 PLUS-MINUS SIGN 
(defconstant +xk-twosuperior+ #x00b2) ;  U+00B2 SUPERSCRIPT TWO 
(defconstant +xk-threesuperior+ #x00b3) ;  U+00B3 SUPERSCRIPT THREE 
(defconstant +xk-acute+ #x00b4) ;  U+00B4 ACUTE ACCENT 
(defconstant +xk-mu+ #x00b5) ;  U+00B5 MICRO SIGN 
(defconstant +xk-paragraph+ #x00b6) ;  U+00B6 PILCROW SIGN 
(defconstant +xk-periodcentered+ #x00b7) ;  U+00B7 MIDDLE DOT 
(defconstant +xk-cedilla+ #x00b8) ;  U+00B8 CEDILLA 
(defconstant +xk-onesuperior+ #x00b9) ;  U+00B9 SUPERSCRIPT ONE 
(defconstant +xk-masculine+ #x00ba) ;  U+00BA MASCULINE ORDINAL INDICATOR 
(defconstant +xk-guillemotright+ #x00bb) ;  U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK 
(defconstant +xk-onequarter+ #x00bc) ;  U+00BC VULGAR FRACTION ONE QUARTER 
(defconstant +xk-onehalf+ #x00bd) ;  U+00BD VULGAR FRACTION ONE HALF 
(defconstant +xk-threequarters+ #x00be) ;  U+00BE VULGAR FRACTION THREE QUARTERS 
(defconstant +xk-questiondown+ #x00bf) ;  U+00BF INVERTED QUESTION MARK 
(defconstant +xk-agrave-cap+ #x00c0) ;  U+00C0 LATIN CAPITAL LETTER A WITH GRAVE 
(defconstant +xk-aacute-cap+ #x00c1) ;  U+00C1 LATIN CAPITAL LETTER A WITH ACUTE 
(defconstant +xk-acircumflex-cap+ #x00c2) ;  U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX 
(defconstant +xk-atilde-cap+ #x00c3) ;  U+00C3 LATIN CAPITAL LETTER A WITH TILDE 
(defconstant +xk-adiaeresis-cap+ #x00c4) ;  U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS 
(defconstant +xk-aring-cap+ #x00c5) ;  U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE 
(defconstant +xk-ae-cap+ #x00c6) ;  U+00C6 LATIN CAPITAL LETTER AE 
(defconstant +xk-ccedilla-cap+ #x00c7) ;  U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA 
(defconstant +xk-egrave-cap+ #x00c8) ;  U+00C8 LATIN CAPITAL LETTER E WITH GRAVE 
(defconstant +xk-eacute-cap+ #x00c9) ;  U+00C9 LATIN CAPITAL LETTER E WITH ACUTE 
(defconstant +xk-ecircumflex-cap+ #x00ca) ;  U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX 
(defconstant +xk-ediaeresis-cap+ #x00cb) ;  U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS 
(defconstant +xk-igrave-cap+ #x00cc) ;  U+00CC LATIN CAPITAL LETTER I WITH GRAVE 
(defconstant +xk-iacute-cap+ #x00cd) ;  U+00CD LATIN CAPITAL LETTER I WITH ACUTE 
(defconstant +xk-icircumflex-cap+ #x00ce) ;  U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX 
(defconstant +xk-idiaeresis-cap+ #x00cf) ;  U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS 
(defconstant +xk-eth-cap+ #x00d0) ;  U+00D0 LATIN CAPITAL LETTER ETH 
;; (defconstant +xk-eth+ #x00d0) ;  deprecated 
(defconstant +xk-ntilde-cap+ #x00d1) ;  U+00D1 LATIN CAPITAL LETTER N WITH TILDE 
(defconstant +xk-ograve-cap+ #x00d2) ;  U+00D2 LATIN CAPITAL LETTER O WITH GRAVE 
(defconstant +xk-oacute-cap+ #x00d3) ;  U+00D3 LATIN CAPITAL LETTER O WITH ACUTE 
(defconstant +xk-ocircumflex-cap+ #x00d4) ;  U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX 
(defconstant +xk-otilde-cap+ #x00d5) ;  U+00D5 LATIN CAPITAL LETTER O WITH TILDE 
(defconstant +xk-odiaeresis-cap+ #x00d6) ;  U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS 
(defconstant +xk-multiply+ #x00d7) ;  U+00D7 MULTIPLICATION SIGN 
(defconstant +xk-oslash-cap+ #x00d8) ;  U+00D8 LATIN CAPITAL LETTER O WITH STROKE 
(defconstant +xk-ooblique-cap+ #x00d8) ;  U+00D8 LATIN CAPITAL LETTER O WITH STROKE 
(defconstant +xk-ugrave-cap+ #x00d9) ;  U+00D9 LATIN CAPITAL LETTER U WITH GRAVE 
(defconstant +xk-uacute-cap+ #x00da) ;  U+00DA LATIN CAPITAL LETTER U WITH ACUTE 
(defconstant +xk-ucircumflex-cap+ #x00db) ;  U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX 
(defconstant +xk-udiaeresis-cap+ #x00dc) ;  U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS 
(defconstant +xk-yacute-cap+ #x00dd) ;  U+00DD LATIN CAPITAL LETTER Y WITH ACUTE 
(defconstant +xk-thorn-cap+ #x00de) ;  U+00DE LATIN CAPITAL LETTER THORN 
;; (defconstant +xk-thorn+ #x00de) ;  deprecated 
(defconstant +xk-ssharp+ #x00df) ;  U+00DF LATIN SMALL LETTER SHARP S 
(defconstant +xk-agrave+ #x00e0) ;  U+00E0 LATIN SMALL LETTER A WITH GRAVE 
(defconstant +xk-aacute+ #x00e1) ;  U+00E1 LATIN SMALL LETTER A WITH ACUTE 
(defconstant +xk-acircumflex+ #x00e2) ;  U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX 
(defconstant +xk-atilde+ #x00e3) ;  U+00E3 LATIN SMALL LETTER A WITH TILDE 
(defconstant +xk-adiaeresis+ #x00e4) ;  U+00E4 LATIN SMALL LETTER A WITH DIAERESIS 
(defconstant +xk-aring+ #x00e5) ;  U+00E5 LATIN SMALL LETTER A WITH RING ABOVE 
(defconstant +xk-ae+ #x00e6) ;  U+00E6 LATIN SMALL LETTER AE 
(defconstant +xk-ccedilla+ #x00e7) ;  U+00E7 LATIN SMALL LETTER C WITH CEDILLA 
(defconstant +xk-egrave+ #x00e8) ;  U+00E8 LATIN SMALL LETTER E WITH GRAVE 
(defconstant +xk-eacute+ #x00e9) ;  U+00E9 LATIN SMALL LETTER E WITH ACUTE 
(defconstant +xk-ecircumflex+ #x00ea) ;  U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX 
(defconstant +xk-ediaeresis+ #x00eb) ;  U+00EB LATIN SMALL LETTER E WITH DIAERESIS 
(defconstant +xk-igrave+ #x00ec) ;  U+00EC LATIN SMALL LETTER I WITH GRAVE 
(defconstant +xk-iacute+ #x00ed) ;  U+00ED LATIN SMALL LETTER I WITH ACUTE 
(defconstant +xk-icircumflex+ #x00ee) ;  U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX 
(defconstant +xk-idiaeresis+ #x00ef) ;  U+00EF LATIN SMALL LETTER I WITH DIAERESIS 
(defconstant +xk-eth+ #x00f0) ;  U+00F0 LATIN SMALL LETTER ETH 
(defconstant +xk-ntilde+ #x00f1) ;  U+00F1 LATIN SMALL LETTER N WITH TILDE 
(defconstant +xk-ograve+ #x00f2) ;  U+00F2 LATIN SMALL LETTER O WITH GRAVE 
(defconstant +xk-oacute+ #x00f3) ;  U+00F3 LATIN SMALL LETTER O WITH ACUTE 
(defconstant +xk-ocircumflex+ #x00f4) ;  U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX 
(defconstant +xk-otilde+ #x00f5) ;  U+00F5 LATIN SMALL LETTER O WITH TILDE 
(defconstant +xk-odiaeresis+ #x00f6) ;  U+00F6 LATIN SMALL LETTER O WITH DIAERESIS 
(defconstant +xk-division+ #x00f7) ;  U+00F7 DIVISION SIGN 
(defconstant +xk-oslash+ #x00f8) ;  U+00F8 LATIN SMALL LETTER O WITH STROKE 
(defconstant +xk-ooblique+ #x00f8) ;  U+00F8 LATIN SMALL LETTER O WITH STROKE 
(defconstant +xk-ugrave+ #x00f9) ;  U+00F9 LATIN SMALL LETTER U WITH GRAVE 
(defconstant +xk-uacute+ #x00fa) ;  U+00FA LATIN SMALL LETTER U WITH ACUTE 
(defconstant +xk-ucircumflex+ #x00fb) ;  U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX 
(defconstant +xk-udiaeresis+ #x00fc) ;  U+00FC LATIN SMALL LETTER U WITH DIAERESIS 
(defconstant +xk-yacute+ #x00fd) ;  U+00FD LATIN SMALL LETTER Y WITH ACUTE 
(defconstant +xk-thorn+ #x00fe) ;  U+00FE LATIN SMALL LETTER THORN 
(defconstant +xk-ydiaeresis+ #x00ff) ;  U+00FF LATIN SMALL LETTER Y WITH DIAERESIS 
;;; #endif /* XK_LATIN1 */

;;; /*
;;;  * Latin 2
;;;  * Byte 3 = 1
;;;  */

;;; #ifdef XK_LATIN2
(defconstant +xk-aogonek-cap+ #x01a1) ;  U+0104 LATIN CAPITAL LETTER A WITH OGONEK 
(defconstant +xk-breve+ #x01a2) ;  U+02D8 BREVE 
(defconstant +xk-lstroke-cap+ #x01a3) ;  U+0141 LATIN CAPITAL LETTER L WITH STROKE 
(defconstant +xk-lcaron-cap+ #x01a5) ;  U+013D LATIN CAPITAL LETTER L WITH CARON 
(defconstant +xk-sacute-cap+ #x01a6) ;  U+015A LATIN CAPITAL LETTER S WITH ACUTE 
(defconstant +xk-scaron-cap+ #x01a9) ;  U+0160 LATIN CAPITAL LETTER S WITH CARON 
(defconstant +xk-scedilla-cap+ #x01aa) ;  U+015E LATIN CAPITAL LETTER S WITH CEDILLA 
(defconstant +xk-tcaron-cap+ #x01ab) ;  U+0164 LATIN CAPITAL LETTER T WITH CARON 
(defconstant +xk-zacute-cap+ #x01ac) ;  U+0179 LATIN CAPITAL LETTER Z WITH ACUTE 
(defconstant +xk-zcaron-cap+ #x01ae) ;  U+017D LATIN CAPITAL LETTER Z WITH CARON 
(defconstant +xk-zabovedot-cap+ #x01af) ;  U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE 
(defconstant +xk-aogonek+ #x01b1) ;  U+0105 LATIN SMALL LETTER A WITH OGONEK 
(defconstant +xk-ogonek+ #x01b2) ;  U+02DB OGONEK 
(defconstant +xk-lstroke+ #x01b3) ;  U+0142 LATIN SMALL LETTER L WITH STROKE 
(defconstant +xk-lcaron+ #x01b5) ;  U+013E LATIN SMALL LETTER L WITH CARON 
(defconstant +xk-sacute+ #x01b6) ;  U+015B LATIN SMALL LETTER S WITH ACUTE 
(defconstant +xk-caron+ #x01b7) ;  U+02C7 CARON 
(defconstant +xk-scaron+ #x01b9) ;  U+0161 LATIN SMALL LETTER S WITH CARON 
(defconstant +xk-scedilla+ #x01ba) ;  U+015F LATIN SMALL LETTER S WITH CEDILLA 
(defconstant +xk-tcaron+ #x01bb) ;  U+0165 LATIN SMALL LETTER T WITH CARON 
(defconstant +xk-zacute+ #x01bc) ;  U+017A LATIN SMALL LETTER Z WITH ACUTE 
(defconstant +xk-doubleacute+ #x01bd) ;  U+02DD DOUBLE ACUTE ACCENT 
(defconstant +xk-zcaron+ #x01be) ;  U+017E LATIN SMALL LETTER Z WITH CARON 
(defconstant +xk-zabovedot+ #x01bf) ;  U+017C LATIN SMALL LETTER Z WITH DOT ABOVE 
(defconstant +xk-racute-cap+ #x01c0) ;  U+0154 LATIN CAPITAL LETTER R WITH ACUTE 
(defconstant +xk-abreve-cap+ #x01c3) ;  U+0102 LATIN CAPITAL LETTER A WITH BREVE 
(defconstant +xk-lacute-cap+ #x01c5) ;  U+0139 LATIN CAPITAL LETTER L WITH ACUTE 
(defconstant +xk-cacute-cap+ #x01c6) ;  U+0106 LATIN CAPITAL LETTER C WITH ACUTE 
(defconstant +xk-ccaron-cap+ #x01c8) ;  U+010C LATIN CAPITAL LETTER C WITH CARON 
(defconstant +xk-eogonek-cap+ #x01ca) ;  U+0118 LATIN CAPITAL LETTER E WITH OGONEK 
(defconstant +xk-ecaron-cap+ #x01cc) ;  U+011A LATIN CAPITAL LETTER E WITH CARON 
(defconstant +xk-dcaron-cap+ #x01cf) ;  U+010E LATIN CAPITAL LETTER D WITH CARON 
(defconstant +xk-dstroke-cap+ #x01d0) ;  U+0110 LATIN CAPITAL LETTER D WITH STROKE 
(defconstant +xk-nacute-cap+ #x01d1) ;  U+0143 LATIN CAPITAL LETTER N WITH ACUTE 
(defconstant +xk-ncaron-cap+ #x01d2) ;  U+0147 LATIN CAPITAL LETTER N WITH CARON 
(defconstant +xk-odoubleacute-cap+ #x01d5) ;  U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE 
(defconstant +xk-rcaron-cap+ #x01d8) ;  U+0158 LATIN CAPITAL LETTER R WITH CARON 
(defconstant +xk-uring-cap+ #x01d9) ;  U+016E LATIN CAPITAL LETTER U WITH RING ABOVE 
(defconstant +xk-udoubleacute-cap+ #x01db) ;  U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE 
(defconstant +xk-tcedilla-cap+ #x01de) ;  U+0162 LATIN CAPITAL LETTER T WITH CEDILLA 
(defconstant +xk-racute+ #x01e0) ;  U+0155 LATIN SMALL LETTER R WITH ACUTE 
(defconstant +xk-abreve+ #x01e3) ;  U+0103 LATIN SMALL LETTER A WITH BREVE 
(defconstant +xk-lacute+ #x01e5) ;  U+013A LATIN SMALL LETTER L WITH ACUTE 
(defconstant +xk-cacute+ #x01e6) ;  U+0107 LATIN SMALL LETTER C WITH ACUTE 
(defconstant +xk-ccaron+ #x01e8) ;  U+010D LATIN SMALL LETTER C WITH CARON 
(defconstant +xk-eogonek+ #x01ea) ;  U+0119 LATIN SMALL LETTER E WITH OGONEK 
(defconstant +xk-ecaron+ #x01ec) ;  U+011B LATIN SMALL LETTER E WITH CARON 
(defconstant +xk-dcaron+ #x01ef) ;  U+010F LATIN SMALL LETTER D WITH CARON 
(defconstant +xk-dstroke+ #x01f0) ;  U+0111 LATIN SMALL LETTER D WITH STROKE 
(defconstant +xk-nacute+ #x01f1) ;  U+0144 LATIN SMALL LETTER N WITH ACUTE 
(defconstant +xk-ncaron+ #x01f2) ;  U+0148 LATIN SMALL LETTER N WITH CARON 
(defconstant +xk-odoubleacute+ #x01f5) ;  U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE 
(defconstant +xk-rcaron+ #x01f8) ;  U+0159 LATIN SMALL LETTER R WITH CARON 
(defconstant +xk-uring+ #x01f9) ;  U+016F LATIN SMALL LETTER U WITH RING ABOVE 
(defconstant +xk-udoubleacute+ #x01fb) ;  U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE 
(defconstant +xk-tcedilla+ #x01fe) ;  U+0163 LATIN SMALL LETTER T WITH CEDILLA 
(defconstant +xk-abovedot+ #x01ff) ;  U+02D9 DOT ABOVE 
;;; #endif /* XK_LATIN2 */

;;; /*
;;;  * Latin 3
;;;  * Byte 3 = 2
;;;  */

;;; #ifdef XK_LATIN3
(defconstant +xk-hstroke-cap+ #x02a1) ;  U+0126 LATIN CAPITAL LETTER H WITH STROKE 
(defconstant +xk-hcircumflex-cap+ #x02a6) ;  U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX 
(defconstant +xk-iabovedot-cap+ #x02a9) ;  U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE 
(defconstant +xk-gbreve-cap+ #x02ab) ;  U+011E LATIN CAPITAL LETTER G WITH BREVE 
(defconstant +xk-jcircumflex-cap+ #x02ac) ;  U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX 
(defconstant +xk-hstroke+ #x02b1) ;  U+0127 LATIN SMALL LETTER H WITH STROKE 
(defconstant +xk-hcircumflex+ #x02b6) ;  U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX 
(defconstant +xk-idotless+ #x02b9) ;  U+0131 LATIN SMALL LETTER DOTLESS I 
(defconstant +xk-gbreve+ #x02bb) ;  U+011F LATIN SMALL LETTER G WITH BREVE 
(defconstant +xk-jcircumflex+ #x02bc) ;  U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX 
(defconstant +xk-cabovedot-cap+ #x02c5) ;  U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE 
(defconstant +xk-ccircumflex-cap+ #x02c6) ;  U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX 
(defconstant +xk-gabovedot-cap+ #x02d5) ;  U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE 
(defconstant +xk-gcircumflex-cap+ #x02d8) ;  U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX 
(defconstant +xk-ubreve-cap+ #x02dd) ;  U+016C LATIN CAPITAL LETTER U WITH BREVE 
(defconstant +xk-scircumflex-cap+ #x02de) ;  U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX 
(defconstant +xk-cabovedot+ #x02e5) ;  U+010B LATIN SMALL LETTER C WITH DOT ABOVE 
(defconstant +xk-ccircumflex+ #x02e6) ;  U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX 
(defconstant +xk-gabovedot+ #x02f5) ;  U+0121 LATIN SMALL LETTER G WITH DOT ABOVE 
(defconstant +xk-gcircumflex+ #x02f8) ;  U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX 
(defconstant +xk-ubreve+ #x02fd) ;  U+016D LATIN SMALL LETTER U WITH BREVE 
(defconstant +xk-scircumflex+ #x02fe) ;  U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX 
;;; #endif /* XK_LATIN3 */


;;; /*
;;;  * Latin 4
;;;  * Byte 3 = 3
;;;  */

;;; #ifdef XK_LATIN4
(defconstant +xk-kra+ #x03a2) ;  U+0138 LATIN SMALL LETTER KRA 
(defconstant +xk-kappa+ #x03a2) ;  deprecated 
(defconstant +xk-rcedilla-cap+ #x03a3) ;  U+0156 LATIN CAPITAL LETTER R WITH CEDILLA 
(defconstant +xk-itilde-cap+ #x03a5) ;  U+0128 LATIN CAPITAL LETTER I WITH TILDE 
(defconstant +xk-lcedilla-cap+ #x03a6) ;  U+013B LATIN CAPITAL LETTER L WITH CEDILLA 
(defconstant +xk-emacron-cap+ #x03aa) ;  U+0112 LATIN CAPITAL LETTER E WITH MACRON 
(defconstant +xk-gcedilla-cap+ #x03ab) ;  U+0122 LATIN CAPITAL LETTER G WITH CEDILLA 
(defconstant +xk-tslash-cap+ #x03ac) ;  U+0166 LATIN CAPITAL LETTER T WITH STROKE 
(defconstant +xk-rcedilla+ #x03b3) ;  U+0157 LATIN SMALL LETTER R WITH CEDILLA 
(defconstant +xk-itilde+ #x03b5) ;  U+0129 LATIN SMALL LETTER I WITH TILDE 
(defconstant +xk-lcedilla+ #x03b6) ;  U+013C LATIN SMALL LETTER L WITH CEDILLA 
(defconstant +xk-emacron+ #x03ba) ;  U+0113 LATIN SMALL LETTER E WITH MACRON 
(defconstant +xk-gcedilla+ #x03bb) ;  U+0123 LATIN SMALL LETTER G WITH CEDILLA 
(defconstant +xk-tslash+ #x03bc) ;  U+0167 LATIN SMALL LETTER T WITH STROKE 
(defconstant +xk-eng-cap+ #x03bd) ;  U+014A LATIN CAPITAL LETTER ENG 
(defconstant +xk-eng+ #x03bf) ;  U+014B LATIN SMALL LETTER ENG 
(defconstant +xk-amacron-cap+ #x03c0) ;  U+0100 LATIN CAPITAL LETTER A WITH MACRON 
(defconstant +xk-iogonek-cap+ #x03c7) ;  U+012E LATIN CAPITAL LETTER I WITH OGONEK 
(defconstant +xk-eabovedot-cap+ #x03cc) ;  U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE 
(defconstant +xk-imacron-cap+ #x03cf) ;  U+012A LATIN CAPITAL LETTER I WITH MACRON 
(defconstant +xk-ncedilla-cap+ #x03d1) ;  U+0145 LATIN CAPITAL LETTER N WITH CEDILLA 
(defconstant +xk-omacron-cap+ #x03d2) ;  U+014C LATIN CAPITAL LETTER O WITH MACRON 
(defconstant +xk-kcedilla-cap+ #x03d3) ;  U+0136 LATIN CAPITAL LETTER K WITH CEDILLA 
(defconstant +xk-uogonek-cap+ #x03d9) ;  U+0172 LATIN CAPITAL LETTER U WITH OGONEK 
(defconstant +xk-utilde-cap+ #x03dd) ;  U+0168 LATIN CAPITAL LETTER U WITH TILDE 
(defconstant +xk-umacron-cap+ #x03de) ;  U+016A LATIN CAPITAL LETTER U WITH MACRON 
(defconstant +xk-amacron+ #x03e0) ;  U+0101 LATIN SMALL LETTER A WITH MACRON 
(defconstant +xk-iogonek+ #x03e7) ;  U+012F LATIN SMALL LETTER I WITH OGONEK 
(defconstant +xk-eabovedot+ #x03ec) ;  U+0117 LATIN SMALL LETTER E WITH DOT ABOVE 
(defconstant +xk-imacron+ #x03ef) ;  U+012B LATIN SMALL LETTER I WITH MACRON 
(defconstant +xk-ncedilla+ #x03f1) ;  U+0146 LATIN SMALL LETTER N WITH CEDILLA 
(defconstant +xk-omacron+ #x03f2) ;  U+014D LATIN SMALL LETTER O WITH MACRON 
(defconstant +xk-kcedilla+ #x03f3) ;  U+0137 LATIN SMALL LETTER K WITH CEDILLA 
(defconstant +xk-uogonek+ #x03f9) ;  U+0173 LATIN SMALL LETTER U WITH OGONEK 
(defconstant +xk-utilde+ #x03fd) ;  U+0169 LATIN SMALL LETTER U WITH TILDE 
(defconstant +xk-umacron+ #x03fe) ;  U+016B LATIN SMALL LETTER U WITH MACRON 
;;; #endif /* XK_LATIN4 */

;;; /*
;;;  * Latin 8
;;;  */
;;; #ifdef XK_LATIN8
(defconstant +xk-wcircumflex-cap+ #x1000174) ;  U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX 
(defconstant +xk-wcircumflex+ #x1000175) ;  U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX 
(defconstant +xk-ycircumflex-cap+ #x1000176) ;  U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX 
(defconstant +xk-ycircumflex+ #x1000177) ;  U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX 
(defconstant +xk-babovedot-cap+ #x1001e02) ;  U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE 
(defconstant +xk-babovedot+ #x1001e03) ;  U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE 
(defconstant +xk-dabovedot-cap+ #x1001e0a) ;  U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE 
(defconstant +xk-dabovedot+ #x1001e0b) ;  U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE 
(defconstant +xk-fabovedot-cap+ #x1001e1e) ;  U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE 
(defconstant +xk-fabovedot+ #x1001e1f) ;  U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE 
(defconstant +xk-mabovedot-cap+ #x1001e40) ;  U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE 
(defconstant +xk-mabovedot+ #x1001e41) ;  U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE 
(defconstant +xk-pabovedot-cap+ #x1001e56) ;  U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE 
(defconstant +xk-pabovedot+ #x1001e57) ;  U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE 
(defconstant +xk-sabovedot-cap+ #x1001e60) ;  U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE 
(defconstant +xk-sabovedot+ #x1001e61) ;  U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE 
(defconstant +xk-tabovedot-cap+ #x1001e6a) ;  U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE 
(defconstant +xk-tabovedot+ #x1001e6b) ;  U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE 
(defconstant +xk-wgrave-cap+ #x1001e80) ;  U+1E80 LATIN CAPITAL LETTER W WITH GRAVE 
(defconstant +xk-wgrave+ #x1001e81) ;  U+1E81 LATIN SMALL LETTER W WITH GRAVE 
(defconstant +xk-wacute-cap+ #x1001e82) ;  U+1E82 LATIN CAPITAL LETTER W WITH ACUTE 
(defconstant +xk-wacute+ #x1001e83) ;  U+1E83 LATIN SMALL LETTER W WITH ACUTE 
(defconstant +xk-wdiaeresis-cap+ #x1001e84) ;  U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS 
(defconstant +xk-wdiaeresis+ #x1001e85) ;  U+1E85 LATIN SMALL LETTER W WITH DIAERESIS 
(defconstant +xk-ygrave-cap+ #x1001ef2) ;  U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE 
(defconstant +xk-ygrave+ #x1001ef3) ;  U+1EF3 LATIN SMALL LETTER Y WITH GRAVE 
;;; #endif /* XK_LATIN8 */

;;; /*
;;;  * Latin 9
;;;  * Byte 3 = 0x13
;;;  */

;;; #ifdef XK_LATIN9
(defconstant +xk-oe-cap+ #x13bc) ;  U+0152 LATIN CAPITAL LIGATURE OE 
(defconstant +xk-oe+ #x13bd) ;  U+0153 LATIN SMALL LIGATURE OE 
(defconstant +xk-ydiaeresis-cap+ #x13be) ;  U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS 
;;; #endif /* XK_LATIN9 */

;;; /*
;;;  * Katakana
;;;  * Byte 3 = 4
;;;  */

;;; #ifdef XK_KATAKANA
(defconstant +xk-overline+ #x047e) ;  U+203E OVERLINE 
(defconstant +xk-kana-fullstop+ #x04a1) ;  U+3002 IDEOGRAPHIC FULL STOP 
(defconstant +xk-kana-openingbracket+ #x04a2) ;  U+300C LEFT CORNER BRACKET 
(defconstant +xk-kana-closingbracket+ #x04a3) ;  U+300D RIGHT CORNER BRACKET 
(defconstant +xk-kana-comma+ #x04a4) ;  U+3001 IDEOGRAPHIC COMMA 
(defconstant +xk-kana-conjunctive+ #x04a5) ;  U+30FB KATAKANA MIDDLE DOT 
(defconstant +xk-kana-middledot+ #x04a5) ;  deprecated 
(defconstant +xk-kana-wo+ #x04a6) ;  U+30F2 KATAKANA LETTER WO 
(defconstant +xk-kana-a-sm+ #x04a7) ;  U+30A1 KATAKANA LETTER SMALL A 
(defconstant +xk-kana-i-sm+ #x04a8) ;  U+30A3 KATAKANA LETTER SMALL I 
(defconstant +xk-kana-u-sm+ #x04a9) ;  U+30A5 KATAKANA LETTER SMALL U 
(defconstant +xk-kana-e-sm+ #x04aa) ;  U+30A7 KATAKANA LETTER SMALL E 
(defconstant +xk-kana-o-sm+ #x04ab) ;  U+30A9 KATAKANA LETTER SMALL O 
(defconstant +xk-kana-ya-sm+ #x04ac) ;  U+30E3 KATAKANA LETTER SMALL YA 
(defconstant +xk-kana-yu-sm+ #x04ad) ;  U+30E5 KATAKANA LETTER SMALL YU 
(defconstant +xk-kana-yo-sm+ #x04ae) ;  U+30E7 KATAKANA LETTER SMALL YO 
(defconstant +xk-kana-tsu-sm+ #x04af) ;  U+30C3 KATAKANA LETTER SMALL TU 
(defconstant +xk-kana-tu-sm+ #x04af) ;  deprecated 
(defconstant +xk-prolongedsound+ #x04b0) ;  U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK 
(defconstant +xk-kana-a+ #x04b1) ;  U+30A2 KATAKANA LETTER A 
(defconstant +xk-kana-i+ #x04b2) ;  U+30A4 KATAKANA LETTER I 
(defconstant +xk-kana-u+ #x04b3) ;  U+30A6 KATAKANA LETTER U 
(defconstant +xk-kana-e+ #x04b4) ;  U+30A8 KATAKANA LETTER E 
(defconstant +xk-kana-o+ #x04b5) ;  U+30AA KATAKANA LETTER O 
(defconstant +xk-kana-ka+ #x04b6) ;  U+30AB KATAKANA LETTER KA 
(defconstant +xk-kana-ki+ #x04b7) ;  U+30AD KATAKANA LETTER KI 
(defconstant +xk-kana-ku+ #x04b8) ;  U+30AF KATAKANA LETTER KU 
(defconstant +xk-kana-ke+ #x04b9) ;  U+30B1 KATAKANA LETTER KE 
(defconstant +xk-kana-ko+ #x04ba) ;  U+30B3 KATAKANA LETTER KO 
(defconstant +xk-kana-sa+ #x04bb) ;  U+30B5 KATAKANA LETTER SA 
(defconstant +xk-kana-shi+ #x04bc) ;  U+30B7 KATAKANA LETTER SI 
(defconstant +xk-kana-su+ #x04bd) ;  U+30B9 KATAKANA LETTER SU 
(defconstant +xk-kana-se+ #x04be) ;  U+30BB KATAKANA LETTER SE 
(defconstant +xk-kana-so+ #x04bf) ;  U+30BD KATAKANA LETTER SO 
(defconstant +xk-kana-ta+ #x04c0) ;  U+30BF KATAKANA LETTER TA 
(defconstant +xk-kana-chi+ #x04c1) ;  U+30C1 KATAKANA LETTER TI 
(defconstant +xk-kana-ti+ #x04c1) ;  deprecated 
(defconstant +xk-kana-tsu+ #x04c2) ;  U+30C4 KATAKANA LETTER TU 
(defconstant +xk-kana-tu+ #x04c2) ;  deprecated 
(defconstant +xk-kana-te+ #x04c3) ;  U+30C6 KATAKANA LETTER TE 
(defconstant +xk-kana-to+ #x04c4) ;  U+30C8 KATAKANA LETTER TO 
(defconstant +xk-kana-na+ #x04c5) ;  U+30CA KATAKANA LETTER NA 
(defconstant +xk-kana-ni+ #x04c6) ;  U+30CB KATAKANA LETTER NI 
(defconstant +xk-kana-nu+ #x04c7) ;  U+30CC KATAKANA LETTER NU 
(defconstant +xk-kana-ne+ #x04c8) ;  U+30CD KATAKANA LETTER NE 
(defconstant +xk-kana-no+ #x04c9) ;  U+30CE KATAKANA LETTER NO 
(defconstant +xk-kana-ha+ #x04ca) ;  U+30CF KATAKANA LETTER HA 
(defconstant +xk-kana-hi+ #x04cb) ;  U+30D2 KATAKANA LETTER HI 
(defconstant +xk-kana-fu+ #x04cc) ;  U+30D5 KATAKANA LETTER HU 
(defconstant +xk-kana-hu+ #x04cc) ;  deprecated 
(defconstant +xk-kana-he+ #x04cd) ;  U+30D8 KATAKANA LETTER HE 
(defconstant +xk-kana-ho+ #x04ce) ;  U+30DB KATAKANA LETTER HO 
(defconstant +xk-kana-ma+ #x04cf) ;  U+30DE KATAKANA LETTER MA 
(defconstant +xk-kana-mi+ #x04d0) ;  U+30DF KATAKANA LETTER MI 
(defconstant +xk-kana-mu+ #x04d1) ;  U+30E0 KATAKANA LETTER MU 
(defconstant +xk-kana-me+ #x04d2) ;  U+30E1 KATAKANA LETTER ME 
(defconstant +xk-kana-mo+ #x04d3) ;  U+30E2 KATAKANA LETTER MO 
(defconstant +xk-kana-ya+ #x04d4) ;  U+30E4 KATAKANA LETTER YA 
(defconstant +xk-kana-yu+ #x04d5) ;  U+30E6 KATAKANA LETTER YU 
(defconstant +xk-kana-yo+ #x04d6) ;  U+30E8 KATAKANA LETTER YO 
(defconstant +xk-kana-ra+ #x04d7) ;  U+30E9 KATAKANA LETTER RA 
(defconstant +xk-kana-ri+ #x04d8) ;  U+30EA KATAKANA LETTER RI 
(defconstant +xk-kana-ru+ #x04d9) ;  U+30EB KATAKANA LETTER RU 
(defconstant +xk-kana-re+ #x04da) ;  U+30EC KATAKANA LETTER RE 
(defconstant +xk-kana-ro+ #x04db) ;  U+30ED KATAKANA LETTER RO 
(defconstant +xk-kana-wa+ #x04dc) ;  U+30EF KATAKANA LETTER WA 
(defconstant +xk-kana-n+ #x04dd) ;  U+30F3 KATAKANA LETTER N 
(defconstant +xk-voicedsound+ #x04de) ;  U+309B KATAKANA-HIRAGANA VOICED SOUND MARK 
(defconstant +xk-semivoicedsound+ #x04df) ;  U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK 
(defconstant +xk-kana-switch+ #xff7e) ;  Alias for mode_switch 
;;; #endif /* XK_KATAKANA */

;;; /*
;;;  * Arabic
;;;  * Byte 3 = 5
;;;  */

;;; #ifdef XK_ARABIC
(defconstant +xk-farsi-0+ #x10006f0) ;  U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO 
(defconstant +xk-farsi-1+ #x10006f1) ;  U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE 
(defconstant +xk-farsi-2+ #x10006f2) ;  U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO 
(defconstant +xk-farsi-3+ #x10006f3) ;  U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE 
(defconstant +xk-farsi-4+ #x10006f4) ;  U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR 
(defconstant +xk-farsi-5+ #x10006f5) ;  U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE 
(defconstant +xk-farsi-6+ #x10006f6) ;  U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX 
(defconstant +xk-farsi-7+ #x10006f7) ;  U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN 
(defconstant +xk-farsi-8+ #x10006f8) ;  U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT 
(defconstant +xk-farsi-9+ #x10006f9) ;  U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE 
(defconstant +xk-arabic-percent+ #x100066a) ;  U+066A ARABIC PERCENT SIGN 
(defconstant +xk-arabic-superscript-alef+ #x1000670) ;  U+0670 ARABIC LETTER SUPERSCRIPT ALEF 
(defconstant +xk-arabic-tteh+ #x1000679) ;  U+0679 ARABIC LETTER TTEH 
(defconstant +xk-arabic-peh+ #x100067e) ;  U+067E ARABIC LETTER PEH 
(defconstant +xk-arabic-tcheh+ #x1000686) ;  U+0686 ARABIC LETTER TCHEH 
(defconstant +xk-arabic-ddal+ #x1000688) ;  U+0688 ARABIC LETTER DDAL 
(defconstant +xk-arabic-rreh+ #x1000691) ;  U+0691 ARABIC LETTER RREH 
(defconstant +xk-arabic-comma+ #x05ac) ;  U+060C ARABIC COMMA 
(defconstant +xk-arabic-fullstop+ #x10006d4) ;  U+06D4 ARABIC FULL STOP 
(defconstant +xk-arabic-0+ #x1000660) ;  U+0660 ARABIC-INDIC DIGIT ZERO 
(defconstant +xk-arabic-1+ #x1000661) ;  U+0661 ARABIC-INDIC DIGIT ONE 
(defconstant +xk-arabic-2+ #x1000662) ;  U+0662 ARABIC-INDIC DIGIT TWO 
(defconstant +xk-arabic-3+ #x1000663) ;  U+0663 ARABIC-INDIC DIGIT THREE 
(defconstant +xk-arabic-4+ #x1000664) ;  U+0664 ARABIC-INDIC DIGIT FOUR 
(defconstant +xk-arabic-5+ #x1000665) ;  U+0665 ARABIC-INDIC DIGIT FIVE 
(defconstant +xk-arabic-6+ #x1000666) ;  U+0666 ARABIC-INDIC DIGIT SIX 
(defconstant +xk-arabic-7+ #x1000667) ;  U+0667 ARABIC-INDIC DIGIT SEVEN 
(defconstant +xk-arabic-8+ #x1000668) ;  U+0668 ARABIC-INDIC DIGIT EIGHT 
(defconstant +xk-arabic-9+ #x1000669) ;  U+0669 ARABIC-INDIC DIGIT NINE 
(defconstant +xk-arabic-semicolon+ #x05bb) ;  U+061B ARABIC SEMICOLON 
(defconstant +xk-arabic-question-mark+ #x05bf) ;  U+061F ARABIC QUESTION MARK 
(defconstant +xk-arabic-hamza+ #x05c1) ;  U+0621 ARABIC LETTER HAMZA 
(defconstant +xk-arabic-maddaonalef+ #x05c2) ;  U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE 
(defconstant +xk-arabic-hamzaonalef+ #x05c3) ;  U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE 
(defconstant +xk-arabic-hamzaonwaw+ #x05c4) ;  U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE 
(defconstant +xk-arabic-hamzaunderalef+ #x05c5) ;  U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW 
(defconstant +xk-arabic-hamzaonyeh+ #x05c6) ;  U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE 
(defconstant +xk-arabic-alef+ #x05c7) ;  U+0627 ARABIC LETTER ALEF 
(defconstant +xk-arabic-beh+ #x05c8) ;  U+0628 ARABIC LETTER BEH 
(defconstant +xk-arabic-tehmarbuta+ #x05c9) ;  U+0629 ARABIC LETTER TEH MARBUTA 
(defconstant +xk-arabic-teh+ #x05ca) ;  U+062A ARABIC LETTER TEH 
(defconstant +xk-arabic-theh+ #x05cb) ;  U+062B ARABIC LETTER THEH 
(defconstant +xk-arabic-jeem+ #x05cc) ;  U+062C ARABIC LETTER JEEM 
(defconstant +xk-arabic-hah+ #x05cd) ;  U+062D ARABIC LETTER HAH 
(defconstant +xk-arabic-khah+ #x05ce) ;  U+062E ARABIC LETTER KHAH 
(defconstant +xk-arabic-dal+ #x05cf) ;  U+062F ARABIC LETTER DAL 
(defconstant +xk-arabic-thal+ #x05d0) ;  U+0630 ARABIC LETTER THAL 
(defconstant +xk-arabic-ra+ #x05d1) ;  U+0631 ARABIC LETTER REH 
(defconstant +xk-arabic-zain+ #x05d2) ;  U+0632 ARABIC LETTER ZAIN 
(defconstant +xk-arabic-seen+ #x05d3) ;  U+0633 ARABIC LETTER SEEN 
(defconstant +xk-arabic-sheen+ #x05d4) ;  U+0634 ARABIC LETTER SHEEN 
(defconstant +xk-arabic-sad+ #x05d5) ;  U+0635 ARABIC LETTER SAD 
(defconstant +xk-arabic-dad+ #x05d6) ;  U+0636 ARABIC LETTER DAD 
(defconstant +xk-arabic-tah+ #x05d7) ;  U+0637 ARABIC LETTER TAH 
(defconstant +xk-arabic-zah+ #x05d8) ;  U+0638 ARABIC LETTER ZAH 
(defconstant +xk-arabic-ain+ #x05d9) ;  U+0639 ARABIC LETTER AIN 
(defconstant +xk-arabic-ghain+ #x05da) ;  U+063A ARABIC LETTER GHAIN 
(defconstant +xk-arabic-tatweel+ #x05e0) ;  U+0640 ARABIC TATWEEL 
(defconstant +xk-arabic-feh+ #x05e1) ;  U+0641 ARABIC LETTER FEH 
(defconstant +xk-arabic-qaf+ #x05e2) ;  U+0642 ARABIC LETTER QAF 
(defconstant +xk-arabic-kaf+ #x05e3) ;  U+0643 ARABIC LETTER KAF 
(defconstant +xk-arabic-lam+ #x05e4) ;  U+0644 ARABIC LETTER LAM 
(defconstant +xk-arabic-meem+ #x05e5) ;  U+0645 ARABIC LETTER MEEM 
(defconstant +xk-arabic-noon+ #x05e6) ;  U+0646 ARABIC LETTER NOON 
(defconstant +xk-arabic-ha+ #x05e7) ;  U+0647 ARABIC LETTER HEH 
(defconstant +xk-arabic-heh+ #x05e7) ;  deprecated 
(defconstant +xk-arabic-waw+ #x05e8) ;  U+0648 ARABIC LETTER WAW 
(defconstant +xk-arabic-alefmaksura+ #x05e9) ;  U+0649 ARABIC LETTER ALEF MAKSURA 
(defconstant +xk-arabic-yeh+ #x05ea) ;  U+064A ARABIC LETTER YEH 
(defconstant +xk-arabic-fathatan+ #x05eb) ;  U+064B ARABIC FATHATAN 
(defconstant +xk-arabic-dammatan+ #x05ec) ;  U+064C ARABIC DAMMATAN 
(defconstant +xk-arabic-kasratan+ #x05ed) ;  U+064D ARABIC KASRATAN 
(defconstant +xk-arabic-fatha+ #x05ee) ;  U+064E ARABIC FATHA 
(defconstant +xk-arabic-damma+ #x05ef) ;  U+064F ARABIC DAMMA 
(defconstant +xk-arabic-kasra+ #x05f0) ;  U+0650 ARABIC KASRA 
(defconstant +xk-arabic-shadda+ #x05f1) ;  U+0651 ARABIC SHADDA 
(defconstant +xk-arabic-sukun+ #x05f2) ;  U+0652 ARABIC SUKUN 
(defconstant +xk-arabic-madda-above+ #x1000653) ;  U+0653 ARABIC MADDAH ABOVE 
(defconstant +xk-arabic-hamza-above+ #x1000654) ;  U+0654 ARABIC HAMZA ABOVE 
(defconstant +xk-arabic-hamza-below+ #x1000655) ;  U+0655 ARABIC HAMZA BELOW 
(defconstant +xk-arabic-jeh+ #x1000698) ;  U+0698 ARABIC LETTER JEH 
(defconstant +xk-arabic-veh+ #x10006a4) ;  U+06A4 ARABIC LETTER VEH 
(defconstant +xk-arabic-keheh+ #x10006a9) ;  U+06A9 ARABIC LETTER KEHEH 
(defconstant +xk-arabic-gaf+ #x10006af) ;  U+06AF ARABIC LETTER GAF 
(defconstant +xk-arabic-noon-ghunna+ #x10006ba) ;  U+06BA ARABIC LETTER NOON GHUNNA 
(defconstant +xk-arabic-heh-doachashmee+ #x10006be) ;  U+06BE ARABIC LETTER HEH DOACHASHMEE 
(defconstant +xk-farsi-yeh+ #x10006cc) ;  U+06CC ARABIC LETTER FARSI YEH 
(defconstant +xk-arabic-farsi-yeh+ #x10006cc) ;  U+06CC ARABIC LETTER FARSI YEH 
(defconstant +xk-arabic-yeh-baree+ #x10006d2) ;  U+06D2 ARABIC LETTER YEH BARREE 
(defconstant +xk-arabic-heh-goal+ #x10006c1) ;  U+06C1 ARABIC LETTER HEH GOAL 
(defconstant +xk-arabic-switch+ #xff7e) ;  Alias for mode_switch 
;;; #endif /* XK_ARABIC */

;;; /*
;;;  * Cyrillic
;;;  * Byte 3 = 6
;;;  */
;;; #ifdef XK_CYRILLIC
;; (defconstant +xk-cyrillic-ghe-bar-cap+ #x1000492) ;  U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE 
;; (defconstant +xk-cyrillic-ghe-bar+ #x1000493) ;  U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE 
;; (defconstant +xk-cyrillic-zhe-descender-cap+ #x1000496) ;  U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER 
;; (defconstant +xk-cyrillic-zhe-descender+ #x1000497) ;  U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER 
;; (defconstant +xk-cyrillic-ka-descender-cap+ #x100049a) ;  U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER 
;; (defconstant +xk-cyrillic-ka-descender+ #x100049b) ;  U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER 
;; (defconstant +xk-cyrillic-ka-vertstroke-cap+ #x100049c) ;  U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE 
;; (defconstant +xk-cyrillic-ka-vertstroke+ #x100049d) ;  U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE 
;; (defconstant +xk-cyrillic-en-descender-cap+ #x10004a2) ;  U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER 
;; (defconstant +xk-cyrillic-en-descender+ #x10004a3) ;  U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER 
;; (defconstant +xk-cyrillic-u-straight-cap+ #x10004ae) ;  U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U 
;; (defconstant +xk-cyrillic-u-straight+ #x10004af) ;  U+04AF CYRILLIC SMALL LETTER STRAIGHT U 
;; (defconstant +xk-cyrillic-u-straight-bar-cap+ #x10004b0) ;  U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE 
;; (defconstant +xk-cyrillic-u-straight-bar+ #x10004b1) ;  U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE 
;; (defconstant +xk-cyrillic-ha-descender-cap+ #x10004b2) ;  U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER 
;; (defconstant +xk-cyrillic-ha-descender+ #x10004b3) ;  U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER 
;; (defconstant +xk-cyrillic-che-descender-cap+ #x10004b6) ;  U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER 
;; (defconstant +xk-cyrillic-che-descender+ #x10004b7) ;  U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER 
;; (defconstant +xk-cyrillic-che-vertstroke-cap+ #x10004b8) ;  U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE 
;; (defconstant +xk-cyrillic-che-vertstroke+ #x10004b9) ;  U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE 
;; (defconstant +xk-cyrillic-shha-cap+ #x10004ba) ;  U+04BA CYRILLIC CAPITAL LETTER SHHA 
;; (defconstant +xk-cyrillic-shha+ #x10004bb) ;  U+04BB CYRILLIC SMALL LETTER SHHA 

;; (defconstant +xk-cyrillic-schwa-cap+ #x10004d8) ;  U+04D8 CYRILLIC CAPITAL LETTER SCHWA 
;; (defconstant +xk-cyrillic-schwa+ #x10004d9) ;  U+04D9 CYRILLIC SMALL LETTER SCHWA 
;; (defconstant +xk-cyrillic-i-macron-cap+ #x10004e2) ;  U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON 
;; (defconstant +xk-cyrillic-i-macron+ #x10004e3) ;  U+04E3 CYRILLIC SMALL LETTER I WITH MACRON 
;; (defconstant +xk-cyrillic-o-bar-cap+ #x10004e8) ;  U+04E8 CYRILLIC CAPITAL LETTER BARRED O 
;; (defconstant +xk-cyrillic-o-bar+ #x10004e9) ;  U+04E9 CYRILLIC SMALL LETTER BARRED O 
;; (defconstant +xk-cyrillic-u-macron-cap+ #x10004ee) ;  U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON 
;; (defconstant +xk-cyrillic-u-macron+ #x10004ef) ;  U+04EF CYRILLIC SMALL LETTER U WITH MACRON 

;; (defconstant +xk-serbian-dje+ #x06a1) ;  U+0452 CYRILLIC SMALL LETTER DJE 
;; (defconstant +xk-macedonia-gje+ #x06a2) ;  U+0453 CYRILLIC SMALL LETTER GJE 
;; (defconstant +xk-cyrillic-io+ #x06a3) ;  U+0451 CYRILLIC SMALL LETTER IO 
;; (defconstant +xk-ukrainian-ie+ #x06a4) ;  U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE 
;; (defconstant +xk-ukranian-je+ #x06a4) ;  deprecated 
;; (defconstant +xk-macedonia-dse+ #x06a5) ;  U+0455 CYRILLIC SMALL LETTER DZE 
;; (defconstant +xk-ukrainian-i+ #x06a6) ;  U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I 
;; (defconstant +xk-ukranian-i+ #x06a6) ;  deprecated 
;; (defconstant +xk-ukrainian-yi+ #x06a7) ;  U+0457 CYRILLIC SMALL LETTER YI 
;; (defconstant +xk-ukranian-yi+ #x06a7) ;  deprecated 
;; (defconstant +xk-cyrillic-je+ #x06a8) ;  U+0458 CYRILLIC SMALL LETTER JE 
;; (defconstant +xk-serbian-je+ #x06a8) ;  deprecated 
;; (defconstant +xk-cyrillic-lje+ #x06a9) ;  U+0459 CYRILLIC SMALL LETTER LJE 
;; (defconstant +xk-serbian-lje+ #x06a9) ;  deprecated 
;; (defconstant +xk-cyrillic-nje+ #x06aa) ;  U+045A CYRILLIC SMALL LETTER NJE 
;; (defconstant +xk-serbian-nje+ #x06aa) ;  deprecated 
;; (defconstant +xk-serbian-tshe+ #x06ab) ;  U+045B CYRILLIC SMALL LETTER TSHE 
;; (defconstant +xk-macedonia-kje+ #x06ac) ;  U+045C CYRILLIC SMALL LETTER KJE 
;; (defconstant +xk-ukrainian-ghe-with-upturn+ #x06ad) ;  U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN 
;; (defconstant +xk-byelorussian-shortu+ #x06ae) ;  U+045E CYRILLIC SMALL LETTER SHORT U 
;; (defconstant +xk-cyrillic-dzhe+ #x06af) ;  U+045F CYRILLIC SMALL LETTER DZHE 
;; (defconstant +xk-serbian-dze+ #x06af) ;  deprecated 
;; (defconstant +xk-numerosign+ #x06b0) ;  U+2116 NUMERO SIGN 
;; (defconstant +xk-serbian-dje-cap+ #x06b1) ;  U+0402 CYRILLIC CAPITAL LETTER DJE 
;; (defconstant +xk-macedonia-gje-cap+ #x06b2) ;  U+0403 CYRILLIC CAPITAL LETTER GJE 
;; (defconstant +xk-cyrillic-io-cap+ #x06b3) ;  U+0401 CYRILLIC CAPITAL LETTER IO 
;; (defconstant +xk-ukrainian-ie-cap+ #x06b4) ;  U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE 
;; (defconstant +xk-ukranian-je+ #x06b4) ;  deprecated 
;; (defconstant +xk-macedonia-dse-cap+ #x06b5) ;  U+0405 CYRILLIC CAPITAL LETTER DZE 
;; (defconstant +xk-ukrainian-i-cap+ #x06b6) ;  U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I 
;; (defconstant +xk-ukranian-i+ #x06b6) ;  deprecated 
;; (defconstant +xk-ukrainian-yi-cap+ #x06b7) ;  U+0407 CYRILLIC CAPITAL LETTER YI 
;; (defconstant +xk-ukranian-yi+ #x06b7) ;  deprecated 
;; (defconstant +xk-cyrillic-je-cap+ #x06b8) ;  U+0408 CYRILLIC CAPITAL LETTER JE 
;; (defconstant +xk-serbian-je+ #x06b8) ;  deprecated 
;; (defconstant +xk-cyrillic-lje-cap+ #x06b9) ;  U+0409 CYRILLIC CAPITAL LETTER LJE 
;; (defconstant +xk-serbian-lje+ #x06b9) ;  deprecated 
;; (defconstant +xk-cyrillic-nje-cap+ #x06ba) ;  U+040A CYRILLIC CAPITAL LETTER NJE 
;; (defconstant +xk-serbian-nje+ #x06ba) ;  deprecated 
;; (defconstant +xk-serbian-tshe-cap+ #x06bb) ;  U+040B CYRILLIC CAPITAL LETTER TSHE 
;; (defconstant +xk-macedonia-kje-cap+ #x06bc) ;  U+040C CYRILLIC CAPITAL LETTER KJE 
;; (defconstant +xk-ukrainian-ghe-with-upturn-cap+ #x06bd) ;  U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN 
;; (defconstant +xk-byelorussian-shortu-cap+ #x06be) ;  U+040E CYRILLIC CAPITAL LETTER SHORT U 
;; (defconstant +xk-cyrillic-dzhe-cap+ #x06bf) ;  U+040F CYRILLIC CAPITAL LETTER DZHE 
;; (defconstant +xk-serbian-dze+ #x06bf) ;  deprecated 
;; (defconstant +xk-cyrillic-yu+ #x06c0) ;  U+044E CYRILLIC SMALL LETTER YU 
;; (defconstant +xk-cyrillic-a+ #x06c1) ;  U+0430 CYRILLIC SMALL LETTER A 
;; (defconstant +xk-cyrillic-be+ #x06c2) ;  U+0431 CYRILLIC SMALL LETTER BE 
;; (defconstant +xk-cyrillic-tse+ #x06c3) ;  U+0446 CYRILLIC SMALL LETTER TSE 
;; (defconstant +xk-cyrillic-de+ #x06c4) ;  U+0434 CYRILLIC SMALL LETTER DE 
;; (defconstant +xk-cyrillic-ie+ #x06c5) ;  U+0435 CYRILLIC SMALL LETTER IE 
;; (defconstant +xk-cyrillic-ef+ #x06c6) ;  U+0444 CYRILLIC SMALL LETTER EF 
;; (defconstant +xk-cyrillic-ghe+ #x06c7) ;  U+0433 CYRILLIC SMALL LETTER GHE 
;; (defconstant +xk-cyrillic-ha+ #x06c8) ;  U+0445 CYRILLIC SMALL LETTER HA 
;; (defconstant +xk-cyrillic-i+ #x06c9) ;  U+0438 CYRILLIC SMALL LETTER I 
;; (defconstant +xk-cyrillic-shorti+ #x06ca) ;  U+0439 CYRILLIC SMALL LETTER SHORT I 
;; (defconstant +xk-cyrillic-ka+ #x06cb) ;  U+043A CYRILLIC SMALL LETTER KA 
;; (defconstant +xk-cyrillic-el+ #x06cc) ;  U+043B CYRILLIC SMALL LETTER EL 
;; (defconstant +xk-cyrillic-em+ #x06cd) ;  U+043C CYRILLIC SMALL LETTER EM 
;; (defconstant +xk-cyrillic-en+ #x06ce) ;  U+043D CYRILLIC SMALL LETTER EN 
;; (defconstant +xk-cyrillic-o+ #x06cf) ;  U+043E CYRILLIC SMALL LETTER O 
;; (defconstant +xk-cyrillic-pe+ #x06d0) ;  U+043F CYRILLIC SMALL LETTER PE 
;; (defconstant +xk-cyrillic-ya+ #x06d1) ;  U+044F CYRILLIC SMALL LETTER YA 
;; (defconstant +xk-cyrillic-er+ #x06d2) ;  U+0440 CYRILLIC SMALL LETTER ER 
;; (defconstant +xk-cyrillic-es+ #x06d3) ;  U+0441 CYRILLIC SMALL LETTER ES 
;; (defconstant +xk-cyrillic-te+ #x06d4) ;  U+0442 CYRILLIC SMALL LETTER TE 
;; (defconstant +xk-cyrillic-u+ #x06d5) ;  U+0443 CYRILLIC SMALL LETTER U 
;; (defconstant +xk-cyrillic-zhe+ #x06d6) ;  U+0436 CYRILLIC SMALL LETTER ZHE 
;; (defconstant +xk-cyrillic-ve+ #x06d7) ;  U+0432 CYRILLIC SMALL LETTER VE 
;; (defconstant +xk-cyrillic-softsign+ #x06d8) ;  U+044C CYRILLIC SMALL LETTER SOFT SIGN 
;; (defconstant +xk-cyrillic-yeru+ #x06d9) ;  U+044B CYRILLIC SMALL LETTER YERU 
;; (defconstant +xk-cyrillic-ze+ #x06da) ;  U+0437 CYRILLIC SMALL LETTER ZE 
;; (defconstant +xk-cyrillic-sha+ #x06db) ;  U+0448 CYRILLIC SMALL LETTER SHA 
;; (defconstant +xk-cyrillic-e+ #x06dc) ;  U+044D CYRILLIC SMALL LETTER E 
;; (defconstant +xk-cyrillic-shcha+ #x06dd) ;  U+0449 CYRILLIC SMALL LETTER SHCHA 
;; (defconstant +xk-cyrillic-che+ #x06de) ;  U+0447 CYRILLIC SMALL LETTER CHE 
;; (defconstant +xk-cyrillic-hardsign+ #x06df) ;  U+044A CYRILLIC SMALL LETTER HARD SIGN 
;; (defconstant +xk-cyrillic-yu-cap+ #x06e0) ;  U+042E CYRILLIC CAPITAL LETTER YU 
;; (defconstant +xk-cyrillic-a-cap+ #x06e1) ;  U+0410 CYRILLIC CAPITAL LETTER A 
;; (defconstant +xk-cyrillic-be-cap+ #x06e2) ;  U+0411 CYRILLIC CAPITAL LETTER BE 
;; (defconstant +xk-cyrillic-tse-cap+ #x06e3) ;  U+0426 CYRILLIC CAPITAL LETTER TSE 
;; (defconstant +xk-cyrillic-de-cap+ #x06e4) ;  U+0414 CYRILLIC CAPITAL LETTER DE 
;; (defconstant +xk-cyrillic-ie-cap+ #x06e5) ;  U+0415 CYRILLIC CAPITAL LETTER IE 
;; (defconstant +xk-cyrillic-ef-cap+ #x06e6) ;  U+0424 CYRILLIC CAPITAL LETTER EF 
;; (defconstant +xk-cyrillic-ghe-cap+ #x06e7) ;  U+0413 CYRILLIC CAPITAL LETTER GHE 
;; (defconstant +xk-cyrillic-ha-cap+ #x06e8) ;  U+0425 CYRILLIC CAPITAL LETTER HA 
;; (defconstant +xk-cyrillic-i-cap+ #x06e9) ;  U+0418 CYRILLIC CAPITAL LETTER I 
;; (defconstant +xk-cyrillic-shorti-cap+ #x06ea) ;  U+0419 CYRILLIC CAPITAL LETTER SHORT I 
;; (defconstant +xk-cyrillic-ka-cap+ #x06eb) ;  U+041A CYRILLIC CAPITAL LETTER KA 
;; (defconstant +xk-cyrillic-el-cap+ #x06ec) ;  U+041B CYRILLIC CAPITAL LETTER EL 
;; (defconstant +xk-cyrillic-em-cap+ #x06ed) ;  U+041C CYRILLIC CAPITAL LETTER EM 
;; (defconstant +xk-cyrillic-en-cap+ #x06ee) ;  U+041D CYRILLIC CAPITAL LETTER EN 
;; (defconstant +xk-cyrillic-o-cap+ #x06ef) ;  U+041E CYRILLIC CAPITAL LETTER O 
;; (defconstant +xk-cyrillic-pe-cap+ #x06f0) ;  U+041F CYRILLIC CAPITAL LETTER PE 
;; (defconstant +xk-cyrillic-ya-cap+ #x06f1) ;  U+042F CYRILLIC CAPITAL LETTER YA 
;; (defconstant +xk-cyrillic-er-cap+ #x06f2) ;  U+0420 CYRILLIC CAPITAL LETTER ER 
;; (defconstant +xk-cyrillic-es-cap+ #x06f3) ;  U+0421 CYRILLIC CAPITAL LETTER ES 
;; (defconstant +xk-cyrillic-te-cap+ #x06f4) ;  U+0422 CYRILLIC CAPITAL LETTER TE 
;; (defconstant +xk-cyrillic-u-cap+ #x06f5) ;  U+0423 CYRILLIC CAPITAL LETTER U 
;; (defconstant +xk-cyrillic-zhe-cap+ #x06f6) ;  U+0416 CYRILLIC CAPITAL LETTER ZHE 
;; (defconstant +xk-cyrillic-ve-cap+ #x06f7) ;  U+0412 CYRILLIC CAPITAL LETTER VE 
;; (defconstant +xk-cyrillic-softsign-cap+ #x06f8) ;  U+042C CYRILLIC CAPITAL LETTER SOFT SIGN 
;; (defconstant +xk-cyrillic-yeru-cap+ #x06f9) ;  U+042B CYRILLIC CAPITAL LETTER YERU 
;; (defconstant +xk-cyrillic-ze-cap+ #x06fa) ;  U+0417 CYRILLIC CAPITAL LETTER ZE 
;; (defconstant +xk-cyrillic-sha-cap+ #x06fb) ;  U+0428 CYRILLIC CAPITAL LETTER SHA 
;; (defconstant +xk-cyrillic-e-cap+ #x06fc) ;  U+042D CYRILLIC CAPITAL LETTER E 
;; (defconstant +xk-cyrillic-shcha-cap+ #x06fd) ;  U+0429 CYRILLIC CAPITAL LETTER SHCHA 
;; (defconstant +xk-cyrillic-che-cap+ #x06fe) ;  U+0427 CYRILLIC CAPITAL LETTER CHE 
;; (defconstant +xk-cyrillic-hardsign-cap+ #x06ff) ;  U+042A CYRILLIC CAPITAL LETTER HARD SIGN
;;; #endif /* XK_CYRILLIC */

;;; /*
;;;  * Greek
;;;  * (based on an early draft of, and not quite identical to, ISO/IEC 8859-7)
;;;  * Byte 3 = 7
;;;  */

;;; #ifdef XK_GREEK
(defconstant +xk-greek-alphaaccent-cap+ #x07a1) ;  U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS 
(defconstant +xk-greek-epsilonaccent-cap+ #x07a2) ;  U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS 
(defconstant +xk-greek-etaaccent-cap+ #x07a3) ;  U+0389 GREEK CAPITAL LETTER ETA WITH TONOS 
(defconstant +xk-greek-iotaaccent-cap+ #x07a4) ;  U+038A GREEK CAPITAL LETTER IOTA WITH TONOS 
(defconstant +xk-greek-iotadieresis-cap+ #x07a5) ;  U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA 
(defconstant +xk-greek-iotadiaeresis+ #x07a5) ;  old typo 
(defconstant +xk-greek-omicronaccent-cap+ #x07a7) ;  U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS 
(defconstant +xk-greek-upsilonaccent-cap+ #x07a8) ;  U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS 
(defconstant +xk-greek-upsilondieresis-cap+ #x07a9) ;  U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA 
(defconstant +xk-greek-omegaaccent-cap+ #x07ab) ;  U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS 
(defconstant +xk-greek-accentdieresis+ #x07ae) ;  U+0385 GREEK DIALYTIKA TONOS 
(defconstant +xk-greek-horizbar+ #x07af) ;  U+2015 HORIZONTAL BAR 
(defconstant +xk-greek-alphaaccent+ #x07b1) ;  U+03AC GREEK SMALL LETTER ALPHA WITH TONOS 
(defconstant +xk-greek-epsilonaccent+ #x07b2) ;  U+03AD GREEK SMALL LETTER EPSILON WITH TONOS 
(defconstant +xk-greek-etaaccent+ #x07b3) ;  U+03AE GREEK SMALL LETTER ETA WITH TONOS 
(defconstant +xk-greek-iotaaccent+ #x07b4) ;  U+03AF GREEK SMALL LETTER IOTA WITH TONOS 
(defconstant +xk-greek-iotadieresis+ #x07b5) ;  U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA 
(defconstant +xk-greek-iotaaccentdieresis+ #x07b6) ;  U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS 
(defconstant +xk-greek-omicronaccent+ #x07b7) ;  U+03CC GREEK SMALL LETTER OMICRON WITH TONOS 
(defconstant +xk-greek-upsilonaccent+ #x07b8) ;  U+03CD GREEK SMALL LETTER UPSILON WITH TONOS 
(defconstant +xk-greek-upsilondieresis+ #x07b9) ;  U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA 
(defconstant +xk-greek-upsilonaccentdieresis+ #x07ba) ;  U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS 
(defconstant +xk-greek-omegaaccent+ #x07bb) ;  U+03CE GREEK SMALL LETTER OMEGA WITH TONOS 
(defconstant +xk-greek-alpha-cap+ #x07c1) ;  U+0391 GREEK CAPITAL LETTER ALPHA 
(defconstant +xk-greek-beta-cap+ #x07c2) ;  U+0392 GREEK CAPITAL LETTER BETA 
(defconstant +xk-greek-gamma-cap+ #x07c3) ;  U+0393 GREEK CAPITAL LETTER GAMMA 
(defconstant +xk-greek-delta-cap+ #x07c4) ;  U+0394 GREEK CAPITAL LETTER DELTA 
(defconstant +xk-greek-epsilon-cap+ #x07c5) ;  U+0395 GREEK CAPITAL LETTER EPSILON 
(defconstant +xk-greek-zeta-cap+ #x07c6) ;  U+0396 GREEK CAPITAL LETTER ZETA 
(defconstant +xk-greek-eta-cap+ #x07c7) ;  U+0397 GREEK CAPITAL LETTER ETA 
(defconstant +xk-greek-theta-cap+ #x07c8) ;  U+0398 GREEK CAPITAL LETTER THETA 
(defconstant +xk-greek-iota-cap+ #x07c9) ;  U+0399 GREEK CAPITAL LETTER IOTA 
(defconstant +xk-greek-kappa-cap+ #x07ca) ;  U+039A GREEK CAPITAL LETTER KAPPA 
(defconstant +xk-greek-lamda-cap+ #x07cb) ;  U+039B GREEK CAPITAL LETTER LAMDA 
(defconstant +xk-greek-lambda-cap+ #x07cb) ;  U+039B GREEK CAPITAL LETTER LAMDA 
(defconstant +xk-greek-mu-cap+ #x07cc) ;  U+039C GREEK CAPITAL LETTER MU 
(defconstant +xk-greek-nu-cap+ #x07cd) ;  U+039D GREEK CAPITAL LETTER NU 
(defconstant +xk-greek-xi-cap+ #x07ce) ;  U+039E GREEK CAPITAL LETTER XI 
(defconstant +xk-greek-omicron-cap+ #x07cf) ;  U+039F GREEK CAPITAL LETTER OMICRON 
(defconstant +xk-greek-pi-cap+ #x07d0) ;  U+03A0 GREEK CAPITAL LETTER PI 
(defconstant +xk-greek-rho-cap+ #x07d1) ;  U+03A1 GREEK CAPITAL LETTER RHO 
(defconstant +xk-greek-sigma-cap+ #x07d2) ;  U+03A3 GREEK CAPITAL LETTER SIGMA 
(defconstant +xk-greek-tau-cap+ #x07d4) ;  U+03A4 GREEK CAPITAL LETTER TAU 
(defconstant +xk-greek-upsilon-cap+ #x07d5) ;  U+03A5 GREEK CAPITAL LETTER UPSILON 
(defconstant +xk-greek-phi-cap+ #x07d6) ;  U+03A6 GREEK CAPITAL LETTER PHI 
(defconstant +xk-greek-chi-cap+ #x07d7) ;  U+03A7 GREEK CAPITAL LETTER CHI 
(defconstant +xk-greek-psi-cap+ #x07d8) ;  U+03A8 GREEK CAPITAL LETTER PSI 
(defconstant +xk-greek-omega-cap+ #x07d9) ;  U+03A9 GREEK CAPITAL LETTER OMEGA 
(defconstant +xk-greek-alpha+ #x07e1) ;  U+03B1 GREEK SMALL LETTER ALPHA 
(defconstant +xk-greek-beta+ #x07e2) ;  U+03B2 GREEK SMALL LETTER BETA 
(defconstant +xk-greek-gamma+ #x07e3) ;  U+03B3 GREEK SMALL LETTER GAMMA 
(defconstant +xk-greek-delta+ #x07e4) ;  U+03B4 GREEK SMALL LETTER DELTA 
(defconstant +xk-greek-epsilon+ #x07e5) ;  U+03B5 GREEK SMALL LETTER EPSILON 
(defconstant +xk-greek-zeta+ #x07e6) ;  U+03B6 GREEK SMALL LETTER ZETA 
(defconstant +xk-greek-eta+ #x07e7) ;  U+03B7 GREEK SMALL LETTER ETA 
(defconstant +xk-greek-theta+ #x07e8) ;  U+03B8 GREEK SMALL LETTER THETA 
(defconstant +xk-greek-iota+ #x07e9) ;  U+03B9 GREEK SMALL LETTER IOTA 
(defconstant +xk-greek-kappa+ #x07ea) ;  U+03BA GREEK SMALL LETTER KAPPA 
(defconstant +xk-greek-lamda+ #x07eb) ;  U+03BB GREEK SMALL LETTER LAMDA 
(defconstant +xk-greek-lambda+ #x07eb) ;  U+03BB GREEK SMALL LETTER LAMDA 
(defconstant +xk-greek-mu+ #x07ec) ;  U+03BC GREEK SMALL LETTER MU 
(defconstant +xk-greek-nu+ #x07ed) ;  U+03BD GREEK SMALL LETTER NU 
(defconstant +xk-greek-xi+ #x07ee) ;  U+03BE GREEK SMALL LETTER XI 
(defconstant +xk-greek-omicron+ #x07ef) ;  U+03BF GREEK SMALL LETTER OMICRON 
(defconstant +xk-greek-pi+ #x07f0) ;  U+03C0 GREEK SMALL LETTER PI 
(defconstant +xk-greek-rho+ #x07f1) ;  U+03C1 GREEK SMALL LETTER RHO 
(defconstant +xk-greek-sigma+ #x07f2) ;  U+03C3 GREEK SMALL LETTER SIGMA 
(defconstant +xk-greek-finalsmallsigma+ #x07f3) ;  U+03C2 GREEK SMALL LETTER FINAL SIGMA 
(defconstant +xk-greek-tau+ #x07f4) ;  U+03C4 GREEK SMALL LETTER TAU 
(defconstant +xk-greek-upsilon+ #x07f5) ;  U+03C5 GREEK SMALL LETTER UPSILON 
(defconstant +xk-greek-phi+ #x07f6) ;  U+03C6 GREEK SMALL LETTER PHI 
(defconstant +xk-greek-chi+ #x07f7) ;  U+03C7 GREEK SMALL LETTER CHI 
(defconstant +xk-greek-psi+ #x07f8) ;  U+03C8 GREEK SMALL LETTER PSI 
(defconstant +xk-greek-omega+ #x07f9) ;  U+03C9 GREEK SMALL LETTER OMEGA 
(defconstant +xk-greek-switch+ #xff7e) ;  Alias for mode_switch 
;;; #endif /* XK_GREEK */

;;; /*
;;;  * Technical
;;;  * (from the DEC VT330/VT420 Technical Character Set, http://vt100.net/charsets/technical.html)
;;;  * Byte 3 = 8
;;;  */

;;; #ifdef XK_TECHNICAL
(defconstant +xk-leftradical+ #x08a1) ;  U+23B7 RADICAL SYMBOL BOTTOM 
(defconstant +xk-topleftradical+ #x08a2) ; (U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)
(defconstant +xk-horizconnector+ #x08a3) ; (U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
(defconstant +xk-topintegral+ #x08a4) ;  U+2320 TOP HALF INTEGRAL 
(defconstant +xk-botintegral+ #x08a5) ;  U+2321 BOTTOM HALF INTEGRAL 
(defconstant +xk-vertconnector+ #x08a6) ; (U+2502 BOX DRAWINGS LIGHT VERTICAL)
(defconstant +xk-topleftsqbracket+ #x08a7) ;  U+23A1 LEFT SQUARE BRACKET UPPER CORNER 
(defconstant +xk-botleftsqbracket+ #x08a8) ;  U+23A3 LEFT SQUARE BRACKET LOWER CORNER 
(defconstant +xk-toprightsqbracket+ #x08a9) ;  U+23A4 RIGHT SQUARE BRACKET UPPER CORNER 
(defconstant +xk-botrightsqbracket+ #x08aa) ;  U+23A6 RIGHT SQUARE BRACKET LOWER CORNER 
(defconstant +xk-topleftparens+ #x08ab) ;  U+239B LEFT PARENTHESIS UPPER HOOK 
(defconstant +xk-botleftparens+ #x08ac) ;  U+239D LEFT PARENTHESIS LOWER HOOK 
(defconstant +xk-toprightparens+ #x08ad) ;  U+239E RIGHT PARENTHESIS UPPER HOOK 
(defconstant +xk-botrightparens+ #x08ae) ;  U+23A0 RIGHT PARENTHESIS LOWER HOOK 
(defconstant +xk-leftmiddlecurlybrace+ #x08af) ;  U+23A8 LEFT CURLY BRACKET MIDDLE PIECE 
(defconstant +xk-rightmiddlecurlybrace+ #x08b0) ;  U+23AC RIGHT CURLY BRACKET MIDDLE PIECE 
(defconstant +xk-topleftsummation+ #x08b1)
(defconstant +xk-botleftsummation+ #x08b2)
(defconstant +xk-topvertsummationconnector+ #x08b3)
(defconstant +xk-botvertsummationconnector+ #x08b4)
(defconstant +xk-toprightsummation+ #x08b5)
(defconstant +xk-botrightsummation+ #x08b6)
(defconstant +xk-rightmiddlesummation+ #x08b7)
(defconstant +xk-lessthanequal+ #x08bc) ;  U+2264 LESS-THAN OR EQUAL TO 
(defconstant +xk-notequal+ #x08bd) ;  U+2260 NOT EQUAL TO 
(defconstant +xk-greaterthanequal+ #x08be) ;  U+2265 GREATER-THAN OR EQUAL TO 
(defconstant +xk-integral+ #x08bf) ;  U+222B INTEGRAL 
(defconstant +xk-therefore+ #x08c0) ;  U+2234 THEREFORE 
(defconstant +xk-variation+ #x08c1) ;  U+221D PROPORTIONAL TO 
(defconstant +xk-infinity+ #x08c2) ;  U+221E INFINITY 
(defconstant +xk-nabla+ #x08c5) ;  U+2207 NABLA 
(defconstant +xk-approximate+ #x08c8) ;  U+223C TILDE OPERATOR 
(defconstant +xk-similarequal+ #x08c9) ;  U+2243 ASYMPTOTICALLY EQUAL TO 
(defconstant +xk-ifonlyif+ #x08cd) ;  U+21D4 LEFT RIGHT DOUBLE ARROW 
(defconstant +xk-implies+ #x08ce) ;  U+21D2 RIGHTWARDS DOUBLE ARROW 
(defconstant +xk-identical+ #x08cf) ;  U+2261 IDENTICAL TO 
(defconstant +xk-radical+ #x08d6) ;  U+221A SQUARE ROOT 
(defconstant +xk-includedin+ #x08da) ;  U+2282 SUBSET OF 
(defconstant +xk-includes+ #x08db) ;  U+2283 SUPERSET OF 
(defconstant +xk-intersection+ #x08dc) ;  U+2229 INTERSECTION 
(defconstant +xk-union+ #x08dd) ;  U+222A UNION 
(defconstant +xk-logicaland+ #x08de) ;  U+2227 LOGICAL AND 
(defconstant +xk-logicalor+ #x08df) ;  U+2228 LOGICAL OR 
(defconstant +xk-partialderivative+ #x08ef) ;  U+2202 PARTIAL DIFFERENTIAL 
(defconstant +xk-function+ #x08f6) ;  U+0192 LATIN SMALL LETTER F WITH HOOK 
(defconstant +xk-leftarrow+ #x08fb) ;  U+2190 LEFTWARDS ARROW 
(defconstant +xk-uparrow+ #x08fc) ;  U+2191 UPWARDS ARROW 
(defconstant +xk-rightarrow+ #x08fd) ;  U+2192 RIGHTWARDS ARROW 
(defconstant +xk-downarrow+ #x08fe) ;  U+2193 DOWNWARDS ARROW 
;;; #endif /* XK_TECHNICAL */

;;; /*
;;;  * Special
;;;  * (from the DEC VT100 Special Graphics Character Set)
;;;  * Byte 3 = 9
;;;  */

;;; #ifdef XK_SPECIAL
(defconstant +xk-blank+ #x09df)
(defconstant +xk-soliddiamond+ #x09e0) ;  U+25C6 BLACK DIAMOND 
(defconstant +xk-checkerboard+ #x09e1) ;  U+2592 MEDIUM SHADE 
(defconstant +xk-ht+ #x09e2) ;  U+2409 SYMBOL FOR HORIZONTAL TABULATION 
(defconstant +xk-ff+ #x09e3) ;  U+240C SYMBOL FOR FORM FEED 
(defconstant +xk-cr+ #x09e4) ;  U+240D SYMBOL FOR CARRIAGE RETURN 
(defconstant +xk-lf+ #x09e5) ;  U+240A SYMBOL FOR LINE FEED 
(defconstant +xk-nl+ #x09e8) ;  U+2424 SYMBOL FOR NEWLINE 
(defconstant +xk-vt+ #x09e9) ;  U+240B SYMBOL FOR VERTICAL TABULATION 
(defconstant +xk-lowrightcorner+ #x09ea) ;  U+2518 BOX DRAWINGS LIGHT UP AND LEFT 
(defconstant +xk-uprightcorner+ #x09eb) ;  U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT 
(defconstant +xk-upleftcorner+ #x09ec) ;  U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT 
(defconstant +xk-lowleftcorner+ #x09ed) ;  U+2514 BOX DRAWINGS LIGHT UP AND RIGHT 
(defconstant +xk-crossinglines+ #x09ee) ;  U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL 
(defconstant +xk-horizlinescan1+ #x09ef) ;  U+23BA HORIZONTAL SCAN LINE-1 
(defconstant +xk-horizlinescan3+ #x09f0) ;  U+23BB HORIZONTAL SCAN LINE-3 
(defconstant +xk-horizlinescan5+ #x09f1) ;  U+2500 BOX DRAWINGS LIGHT HORIZONTAL 
(defconstant +xk-horizlinescan7+ #x09f2) ;  U+23BC HORIZONTAL SCAN LINE-7 
(defconstant +xk-horizlinescan9+ #x09f3) ;  U+23BD HORIZONTAL SCAN LINE-9 
(defconstant +xk-leftt+ #x09f4) ;  U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT 
(defconstant +xk-rightt+ #x09f5) ;  U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT 
(defconstant +xk-bott+ #x09f6) ;  U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL 
(defconstant +xk-topt+ #x09f7) ;  U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL 
(defconstant +xk-vertbar+ #x09f8) ;  U+2502 BOX DRAWINGS LIGHT VERTICAL 
;;; #endif /* XK_SPECIAL */

;;; /*
;;;  * Publishing
;;;  * (these are probably from a long forgotten DEC Publishing
;;;  * font that once shipped with DECwrite)
;;;  * Byte 3 = 0x0a
;;;  */

;;; #ifdef XK_PUBLISHING
(defconstant +xk-emspace+ #x0aa1) ;  U+2003 EM SPACE 
(defconstant +xk-enspace+ #x0aa2) ;  U+2002 EN SPACE 
(defconstant +xk-em3space+ #x0aa3) ;  U+2004 THREE-PER-EM SPACE 
(defconstant +xk-em4space+ #x0aa4) ;  U+2005 FOUR-PER-EM SPACE 
(defconstant +xk-digitspace+ #x0aa5) ;  U+2007 FIGURE SPACE 
(defconstant +xk-punctspace+ #x0aa6) ;  U+2008 PUNCTUATION SPACE 
(defconstant +xk-thinspace+ #x0aa7) ;  U+2009 THIN SPACE 
(defconstant +xk-hairspace+ #x0aa8) ;  U+200A HAIR SPACE 
(defconstant +xk-emdash+ #x0aa9) ;  U+2014 EM DASH 
(defconstant +xk-endash+ #x0aaa) ;  U+2013 EN DASH 
(defconstant +xk-signifblank+ #x0aac) ; (U+2423 OPEN BOX)
(defconstant +xk-ellipsis+ #x0aae) ;  U+2026 HORIZONTAL ELLIPSIS 
(defconstant +xk-doubbaselinedot+ #x0aaf) ;  U+2025 TWO DOT LEADER 
(defconstant +xk-onethird+ #x0ab0) ;  U+2153 VULGAR FRACTION ONE THIRD 
(defconstant +xk-twothirds+ #x0ab1) ;  U+2154 VULGAR FRACTION TWO THIRDS 
(defconstant +xk-onefifth+ #x0ab2) ;  U+2155 VULGAR FRACTION ONE FIFTH 
(defconstant +xk-twofifths+ #x0ab3) ;  U+2156 VULGAR FRACTION TWO FIFTHS 
(defconstant +xk-threefifths+ #x0ab4) ;  U+2157 VULGAR FRACTION THREE FIFTHS 
(defconstant +xk-fourfifths+ #x0ab5) ;  U+2158 VULGAR FRACTION FOUR FIFTHS 
(defconstant +xk-onesixth+ #x0ab6) ;  U+2159 VULGAR FRACTION ONE SIXTH 
(defconstant +xk-fivesixths+ #x0ab7) ;  U+215A VULGAR FRACTION FIVE SIXTHS 
(defconstant +xk-careof+ #x0ab8) ;  U+2105 CARE OF 
(defconstant +xk-figdash+ #x0abb) ;  U+2012 FIGURE DASH 
(defconstant +xk-leftanglebracket+ #x0abc) ; (U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
(defconstant +xk-decimalpoint+ #x0abd) ; (U+002E FULL STOP)
(defconstant +xk-rightanglebracket+ #x0abe) ; (U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
(defconstant +xk-marker+ #x0abf)
(defconstant +xk-oneeighth+ #x0ac3) ;  U+215B VULGAR FRACTION ONE EIGHTH 
(defconstant +xk-threeeighths+ #x0ac4) ;  U+215C VULGAR FRACTION THREE EIGHTHS 
(defconstant +xk-fiveeighths+ #x0ac5) ;  U+215D VULGAR FRACTION FIVE EIGHTHS 
(defconstant +xk-seveneighths+ #x0ac6) ;  U+215E VULGAR FRACTION SEVEN EIGHTHS 
(defconstant +xk-trademark+ #x0ac9) ;  U+2122 TRADE MARK SIGN 
(defconstant +xk-signaturemark+ #x0aca) ; (U+2613 SALTIRE)
(defconstant +xk-trademarkincircle+ #x0acb)
(defconstant +xk-leftopentriangle+ #x0acc) ; (U+25C1 WHITE LEFT-POINTING TRIANGLE)
(defconstant +xk-rightopentriangle+ #x0acd) ; (U+25B7 WHITE RIGHT-POINTING TRIANGLE)
(defconstant +xk-emopencircle+ #x0ace) ; (U+25CB WHITE CIRCLE)
(defconstant +xk-emopenrectangle+ #x0acf) ; (U+25AF WHITE VERTICAL RECTANGLE)
(defconstant +xk-leftsinglequotemark+ #x0ad0) ;  U+2018 LEFT SINGLE QUOTATION MARK 
(defconstant +xk-rightsinglequotemark+ #x0ad1) ;  U+2019 RIGHT SINGLE QUOTATION MARK 
(defconstant +xk-leftdoublequotemark+ #x0ad2) ;  U+201C LEFT DOUBLE QUOTATION MARK 
(defconstant +xk-rightdoublequotemark+ #x0ad3) ;  U+201D RIGHT DOUBLE QUOTATION MARK 
(defconstant +xk-prescription+ #x0ad4) ;  U+211E PRESCRIPTION TAKE 
(defconstant +xk-permille+ #x0ad5) ;  U+2030 PER MILLE SIGN 
(defconstant +xk-minutes+ #x0ad6) ;  U+2032 PRIME 
(defconstant +xk-seconds+ #x0ad7) ;  U+2033 DOUBLE PRIME 
(defconstant +xk-latincross+ #x0ad9) ;  U+271D LATIN CROSS 
(defconstant +xk-hexagram+ #x0ada)
(defconstant +xk-filledrectbullet+ #x0adb) ; (U+25AC BLACK RECTANGLE)
(defconstant +xk-filledlefttribullet+ #x0adc) ; (U+25C0 BLACK LEFT-POINTING TRIANGLE)
(defconstant +xk-filledrighttribullet+ #x0add) ; (U+25B6 BLACK RIGHT-POINTING TRIANGLE)
(defconstant +xk-emfilledcircle+ #x0ade) ; (U+25CF BLACK CIRCLE)
(defconstant +xk-emfilledrect+ #x0adf) ; (U+25AE BLACK VERTICAL RECTANGLE)
(defconstant +xk-enopencircbullet+ #x0ae0) ; (U+25E6 WHITE BULLET)
(defconstant +xk-enopensquarebullet+ #x0ae1) ; (U+25AB WHITE SMALL SQUARE)
(defconstant +xk-openrectbullet+ #x0ae2) ; (U+25AD WHITE RECTANGLE)
(defconstant +xk-opentribulletup+ #x0ae3) ; (U+25B3 WHITE UP-POINTING TRIANGLE)
(defconstant +xk-opentribulletdown+ #x0ae4) ; (U+25BD WHITE DOWN-POINTING TRIANGLE)
(defconstant +xk-openstar+ #x0ae5) ; (U+2606 WHITE STAR)
(defconstant +xk-enfilledcircbullet+ #x0ae6) ; (U+2022 BULLET)
(defconstant +xk-enfilledsqbullet+ #x0ae7) ; (U+25AA BLACK SMALL SQUARE)
(defconstant +xk-filledtribulletup+ #x0ae8) ; (U+25B2 BLACK UP-POINTING TRIANGLE)
(defconstant +xk-filledtribulletdown+ #x0ae9) ; (U+25BC BLACK DOWN-POINTING TRIANGLE)
(defconstant +xk-leftpointer+ #x0aea) ; (U+261C WHITE LEFT POINTING INDEX)
(defconstant +xk-rightpointer+ #x0aeb) ; (U+261E WHITE RIGHT POINTING INDEX)
(defconstant +xk-club+ #x0aec) ;  U+2663 BLACK CLUB SUIT 
(defconstant +xk-diamond+ #x0aed) ;  U+2666 BLACK DIAMOND SUIT 
(defconstant +xk-heart+ #x0aee) ;  U+2665 BLACK HEART SUIT 
(defconstant +xk-maltesecross+ #x0af0) ;  U+2720 MALTESE CROSS 
(defconstant +xk-dagger+ #x0af1) ;  U+2020 DAGGER 
(defconstant +xk-doubledagger+ #x0af2) ;  U+2021 DOUBLE DAGGER 
(defconstant +xk-checkmark+ #x0af3) ;  U+2713 CHECK MARK 
(defconstant +xk-ballotcross+ #x0af4) ;  U+2717 BALLOT X 
(defconstant +xk-musicalsharp+ #x0af5) ;  U+266F MUSIC SHARP SIGN 
(defconstant +xk-musicalflat+ #x0af6) ;  U+266D MUSIC FLAT SIGN 
(defconstant +xk-malesymbol+ #x0af7) ;  U+2642 MALE SIGN 
(defconstant +xk-femalesymbol+ #x0af8) ;  U+2640 FEMALE SIGN 
(defconstant +xk-telephone+ #x0af9) ;  U+260E BLACK TELEPHONE 
(defconstant +xk-telephonerecorder+ #x0afa) ;  U+2315 TELEPHONE RECORDER 
(defconstant +xk-phonographcopyright+ #x0afb) ;  U+2117 SOUND RECORDING COPYRIGHT 
(defconstant +xk-caret+ #x0afc) ;  U+2038 CARET 
(defconstant +xk-singlelowquotemark+ #x0afd) ;  U+201A SINGLE LOW-9 QUOTATION MARK 
(defconstant +xk-doublelowquotemark+ #x0afe) ;  U+201E DOUBLE LOW-9 QUOTATION MARK 
(defconstant +xk-cursor+ #x0aff)
;;; #endif /* XK_PUBLISHING */

;;; /*
;;;  * APL
;;;  * Byte 3 = 0x0b
;;;  */

;;; #ifdef XK_APL
(defconstant +xk-leftcaret+ #x0ba3) ; (U+003C LESS-THAN SIGN)
(defconstant +xk-rightcaret+ #x0ba6) ; (U+003E GREATER-THAN SIGN)
(defconstant +xk-downcaret+ #x0ba8) ; (U+2228 LOGICAL OR)
(defconstant +xk-upcaret+ #x0ba9) ; (U+2227 LOGICAL AND)
(defconstant +xk-overbar+ #x0bc0) ; (U+00AF MACRON)
(defconstant +xk-downtack+ #x0bc2) ;  U+22A4 DOWN TACK 
(defconstant +xk-upshoe+ #x0bc3) ; (U+2229 INTERSECTION)
(defconstant +xk-downstile+ #x0bc4) ;  U+230A LEFT FLOOR 
(defconstant +xk-underbar+ #x0bc6) ; (U+005F LOW LINE)
(defconstant +xk-jot+ #x0bca) ;  U+2218 RING OPERATOR 
(defconstant +xk-quad+ #x0bcc) ;  U+2395 APL FUNCTIONAL SYMBOL QUAD 
(defconstant +xk-uptack+ #x0bce) ;  U+22A5 UP TACK 
(defconstant +xk-circle+ #x0bcf) ;  U+25CB WHITE CIRCLE 
(defconstant +xk-upstile+ #x0bd3) ;  U+2308 LEFT CEILING 
(defconstant +xk-downshoe+ #x0bd6) ; (U+222A UNION)
(defconstant +xk-rightshoe+ #x0bd8) ; (U+2283 SUPERSET OF)
(defconstant +xk-leftshoe+ #x0bda) ; (U+2282 SUBSET OF)
(defconstant +xk-lefttack+ #x0bdc) ;  U+22A3 LEFT TACK 
(defconstant +xk-righttack+ #x0bfc) ;  U+22A2 RIGHT TACK 
;;; #endif /* XK_APL */

;;; /*
;;;  * Hebrew
;;;  * Byte 3 = 0x0c
;;;  */

;;; #ifdef XK_HEBREW
(defconstant +xk-hebrew-doublelowline+ #x0cdf) ;  U+2017 DOUBLE LOW LINE 
(defconstant +xk-hebrew-aleph+ #x0ce0) ;  U+05D0 HEBREW LETTER ALEF 
(defconstant +xk-hebrew-bet+ #x0ce1) ;  U+05D1 HEBREW LETTER BET 
(defconstant +xk-hebrew-beth+ #x0ce1) ;  deprecated 
(defconstant +xk-hebrew-gimel+ #x0ce2) ;  U+05D2 HEBREW LETTER GIMEL 
(defconstant +xk-hebrew-gimmel+ #x0ce2) ;  deprecated 
(defconstant +xk-hebrew-dalet+ #x0ce3) ;  U+05D3 HEBREW LETTER DALET 
(defconstant +xk-hebrew-daleth+ #x0ce3) ;  deprecated 
(defconstant +xk-hebrew-he+ #x0ce4) ;  U+05D4 HEBREW LETTER HE 
(defconstant +xk-hebrew-waw+ #x0ce5) ;  U+05D5 HEBREW LETTER VAV 
(defconstant +xk-hebrew-zain+ #x0ce6) ;  U+05D6 HEBREW LETTER ZAYIN 
(defconstant +xk-hebrew-zayin+ #x0ce6) ;  deprecated 
(defconstant +xk-hebrew-chet+ #x0ce7) ;  U+05D7 HEBREW LETTER HET 
(defconstant +xk-hebrew-het+ #x0ce7) ;  deprecated 
(defconstant +xk-hebrew-tet+ #x0ce8) ;  U+05D8 HEBREW LETTER TET 
(defconstant +xk-hebrew-teth+ #x0ce8) ;  deprecated 
(defconstant +xk-hebrew-yod+ #x0ce9) ;  U+05D9 HEBREW LETTER YOD 
(defconstant +xk-hebrew-finalkaph+ #x0cea) ;  U+05DA HEBREW LETTER FINAL KAF 
(defconstant +xk-hebrew-kaph+ #x0ceb) ;  U+05DB HEBREW LETTER KAF 
(defconstant +xk-hebrew-lamed+ #x0cec) ;  U+05DC HEBREW LETTER LAMED 
(defconstant +xk-hebrew-finalmem+ #x0ced) ;  U+05DD HEBREW LETTER FINAL MEM 
(defconstant +xk-hebrew-mem+ #x0cee) ;  U+05DE HEBREW LETTER MEM 
(defconstant +xk-hebrew-finalnun+ #x0cef) ;  U+05DF HEBREW LETTER FINAL NUN 
(defconstant +xk-hebrew-nun+ #x0cf0) ;  U+05E0 HEBREW LETTER NUN 
(defconstant +xk-hebrew-samech+ #x0cf1) ;  U+05E1 HEBREW LETTER SAMEKH 
(defconstant +xk-hebrew-samekh+ #x0cf1) ;  deprecated 
(defconstant +xk-hebrew-ayin+ #x0cf2) ;  U+05E2 HEBREW LETTER AYIN 
(defconstant +xk-hebrew-finalpe+ #x0cf3) ;  U+05E3 HEBREW LETTER FINAL PE 
(defconstant +xk-hebrew-pe+ #x0cf4) ;  U+05E4 HEBREW LETTER PE 
(defconstant +xk-hebrew-finalzade+ #x0cf5) ;  U+05E5 HEBREW LETTER FINAL TSADI 
(defconstant +xk-hebrew-finalzadi+ #x0cf5) ;  deprecated 
(defconstant +xk-hebrew-zade+ #x0cf6) ;  U+05E6 HEBREW LETTER TSADI 
(defconstant +xk-hebrew-zadi+ #x0cf6) ;  deprecated 
(defconstant +xk-hebrew-qoph+ #x0cf7) ;  U+05E7 HEBREW LETTER QOF 
(defconstant +xk-hebrew-kuf+ #x0cf7) ;  deprecated 
(defconstant +xk-hebrew-resh+ #x0cf8) ;  U+05E8 HEBREW LETTER RESH 
(defconstant +xk-hebrew-shin+ #x0cf9) ;  U+05E9 HEBREW LETTER SHIN 
(defconstant +xk-hebrew-taw+ #x0cfa) ;  U+05EA HEBREW LETTER TAV 
(defconstant +xk-hebrew-taf+ #x0cfa) ;  deprecated 
(defconstant +xk-hebrew-switch+ #xff7e) ;  Alias for mode_switch 
;;; #endif /* XK_HEBREW */

;;; /*
;;;  * Thai
;;;  * Byte 3 = 0x0d
;;;  */

;;; #ifdef XK_THAI
(defconstant +xk-thai-kokai+ #x0da1) ;  U+0E01 THAI CHARACTER KO KAI 
(defconstant +xk-thai-khokhai+ #x0da2) ;  U+0E02 THAI CHARACTER KHO KHAI 
(defconstant +xk-thai-khokhuat+ #x0da3) ;  U+0E03 THAI CHARACTER KHO KHUAT 
(defconstant +xk-thai-khokhwai+ #x0da4) ;  U+0E04 THAI CHARACTER KHO KHWAI 
(defconstant +xk-thai-khokhon+ #x0da5) ;  U+0E05 THAI CHARACTER KHO KHON 
(defconstant +xk-thai-khorakhang+ #x0da6) ;  U+0E06 THAI CHARACTER KHO RAKHANG 
(defconstant +xk-thai-ngongu+ #x0da7) ;  U+0E07 THAI CHARACTER NGO NGU 
(defconstant +xk-thai-chochan+ #x0da8) ;  U+0E08 THAI CHARACTER CHO CHAN 
(defconstant +xk-thai-choching+ #x0da9) ;  U+0E09 THAI CHARACTER CHO CHING 
(defconstant +xk-thai-chochang+ #x0daa) ;  U+0E0A THAI CHARACTER CHO CHANG 
(defconstant +xk-thai-soso+ #x0dab) ;  U+0E0B THAI CHARACTER SO SO 
(defconstant +xk-thai-chochoe+ #x0dac) ;  U+0E0C THAI CHARACTER CHO CHOE 
(defconstant +xk-thai-yoying+ #x0dad) ;  U+0E0D THAI CHARACTER YO YING 
(defconstant +xk-thai-dochada+ #x0dae) ;  U+0E0E THAI CHARACTER DO CHADA 
(defconstant +xk-thai-topatak+ #x0daf) ;  U+0E0F THAI CHARACTER TO PATAK 
(defconstant +xk-thai-thothan+ #x0db0) ;  U+0E10 THAI CHARACTER THO THAN 
(defconstant +xk-thai-thonangmontho+ #x0db1) ;  U+0E11 THAI CHARACTER THO NANGMONTHO 
(defconstant +xk-thai-thophuthao+ #x0db2) ;  U+0E12 THAI CHARACTER THO PHUTHAO 
(defconstant +xk-thai-nonen+ #x0db3) ;  U+0E13 THAI CHARACTER NO NEN 
(defconstant +xk-thai-dodek+ #x0db4) ;  U+0E14 THAI CHARACTER DO DEK 
(defconstant +xk-thai-totao+ #x0db5) ;  U+0E15 THAI CHARACTER TO TAO 
(defconstant +xk-thai-thothung+ #x0db6) ;  U+0E16 THAI CHARACTER THO THUNG 
(defconstant +xk-thai-thothahan+ #x0db7) ;  U+0E17 THAI CHARACTER THO THAHAN 
(defconstant +xk-thai-thothong+ #x0db8) ;  U+0E18 THAI CHARACTER THO THONG 
(defconstant +xk-thai-nonu+ #x0db9) ;  U+0E19 THAI CHARACTER NO NU 
(defconstant +xk-thai-bobaimai+ #x0dba) ;  U+0E1A THAI CHARACTER BO BAIMAI 
(defconstant +xk-thai-popla+ #x0dbb) ;  U+0E1B THAI CHARACTER PO PLA 
(defconstant +xk-thai-phophung+ #x0dbc) ;  U+0E1C THAI CHARACTER PHO PHUNG 
(defconstant +xk-thai-fofa+ #x0dbd) ;  U+0E1D THAI CHARACTER FO FA 
(defconstant +xk-thai-phophan+ #x0dbe) ;  U+0E1E THAI CHARACTER PHO PHAN 
(defconstant +xk-thai-fofan+ #x0dbf) ;  U+0E1F THAI CHARACTER FO FAN 
(defconstant +xk-thai-phosamphao+ #x0dc0) ;  U+0E20 THAI CHARACTER PHO SAMPHAO 
(defconstant +xk-thai-moma+ #x0dc1) ;  U+0E21 THAI CHARACTER MO MA 
(defconstant +xk-thai-yoyak+ #x0dc2) ;  U+0E22 THAI CHARACTER YO YAK 
(defconstant +xk-thai-rorua+ #x0dc3) ;  U+0E23 THAI CHARACTER RO RUA 
(defconstant +xk-thai-ru+ #x0dc4) ;  U+0E24 THAI CHARACTER RU 
(defconstant +xk-thai-loling+ #x0dc5) ;  U+0E25 THAI CHARACTER LO LING 
(defconstant +xk-thai-lu+ #x0dc6) ;  U+0E26 THAI CHARACTER LU 
(defconstant +xk-thai-wowaen+ #x0dc7) ;  U+0E27 THAI CHARACTER WO WAEN 
(defconstant +xk-thai-sosala+ #x0dc8) ;  U+0E28 THAI CHARACTER SO SALA 
(defconstant +xk-thai-sorusi+ #x0dc9) ;  U+0E29 THAI CHARACTER SO RUSI 
(defconstant +xk-thai-sosua+ #x0dca) ;  U+0E2A THAI CHARACTER SO SUA 
(defconstant +xk-thai-hohip+ #x0dcb) ;  U+0E2B THAI CHARACTER HO HIP 
(defconstant +xk-thai-lochula+ #x0dcc) ;  U+0E2C THAI CHARACTER LO CHULA 
(defconstant +xk-thai-oang+ #x0dcd) ;  U+0E2D THAI CHARACTER O ANG 
(defconstant +xk-thai-honokhuk+ #x0dce) ;  U+0E2E THAI CHARACTER HO NOKHUK 
(defconstant +xk-thai-paiyannoi+ #x0dcf) ;  U+0E2F THAI CHARACTER PAIYANNOI 
(defconstant +xk-thai-saraa+ #x0dd0) ;  U+0E30 THAI CHARACTER SARA A 
(defconstant +xk-thai-maihanakat+ #x0dd1) ;  U+0E31 THAI CHARACTER MAI HAN-AKAT 
(defconstant +xk-thai-saraaa+ #x0dd2) ;  U+0E32 THAI CHARACTER SARA AA 
(defconstant +xk-thai-saraam+ #x0dd3) ;  U+0E33 THAI CHARACTER SARA AM 
(defconstant +xk-thai-sarai+ #x0dd4) ;  U+0E34 THAI CHARACTER SARA I 
(defconstant +xk-thai-saraii+ #x0dd5) ;  U+0E35 THAI CHARACTER SARA II 
(defconstant +xk-thai-saraue+ #x0dd6) ;  U+0E36 THAI CHARACTER SARA UE 
(defconstant +xk-thai-sarauee+ #x0dd7) ;  U+0E37 THAI CHARACTER SARA UEE 
(defconstant +xk-thai-sarau+ #x0dd8) ;  U+0E38 THAI CHARACTER SARA U 
(defconstant +xk-thai-sarauu+ #x0dd9) ;  U+0E39 THAI CHARACTER SARA UU 
(defconstant +xk-thai-phinthu+ #x0dda) ;  U+0E3A THAI CHARACTER PHINTHU 
(defconstant +xk-thai-maihanakat-maitho+ #x0dde)
(defconstant +xk-thai-baht+ #x0ddf) ;  U+0E3F THAI CURRENCY SYMBOL BAHT 
(defconstant +xk-thai-sarae+ #x0de0) ;  U+0E40 THAI CHARACTER SARA E 
(defconstant +xk-thai-saraae+ #x0de1) ;  U+0E41 THAI CHARACTER SARA AE 
(defconstant +xk-thai-sarao+ #x0de2) ;  U+0E42 THAI CHARACTER SARA O 
(defconstant +xk-thai-saraaimaimuan+ #x0de3) ;  U+0E43 THAI CHARACTER SARA AI MAIMUAN 
(defconstant +xk-thai-saraaimaimalai+ #x0de4) ;  U+0E44 THAI CHARACTER SARA AI MAIMALAI 
(defconstant +xk-thai-lakkhangyao+ #x0de5) ;  U+0E45 THAI CHARACTER LAKKHANGYAO 
(defconstant +xk-thai-maiyamok+ #x0de6) ;  U+0E46 THAI CHARACTER MAIYAMOK 
(defconstant +xk-thai-maitaikhu+ #x0de7) ;  U+0E47 THAI CHARACTER MAITAIKHU 
(defconstant +xk-thai-maiek+ #x0de8) ;  U+0E48 THAI CHARACTER MAI EK 
(defconstant +xk-thai-maitho+ #x0de9) ;  U+0E49 THAI CHARACTER MAI THO 
(defconstant +xk-thai-maitri+ #x0dea) ;  U+0E4A THAI CHARACTER MAI TRI 
(defconstant +xk-thai-maichattawa+ #x0deb) ;  U+0E4B THAI CHARACTER MAI CHATTAWA 
(defconstant +xk-thai-thanthakhat+ #x0dec) ;  U+0E4C THAI CHARACTER THANTHAKHAT 
(defconstant +xk-thai-nikhahit+ #x0ded) ;  U+0E4D THAI CHARACTER NIKHAHIT 
(defconstant +xk-thai-leksun+ #x0df0) ;  U+0E50 THAI DIGIT ZERO 
(defconstant +xk-thai-leknung+ #x0df1) ;  U+0E51 THAI DIGIT ONE 
(defconstant +xk-thai-leksong+ #x0df2) ;  U+0E52 THAI DIGIT TWO 
(defconstant +xk-thai-leksam+ #x0df3) ;  U+0E53 THAI DIGIT THREE 
(defconstant +xk-thai-leksi+ #x0df4) ;  U+0E54 THAI DIGIT FOUR 
(defconstant +xk-thai-lekha+ #x0df5) ;  U+0E55 THAI DIGIT FIVE 
(defconstant +xk-thai-lekhok+ #x0df6) ;  U+0E56 THAI DIGIT SIX 
(defconstant +xk-thai-lekchet+ #x0df7) ;  U+0E57 THAI DIGIT SEVEN 
(defconstant +xk-thai-lekpaet+ #x0df8) ;  U+0E58 THAI DIGIT EIGHT 
(defconstant +xk-thai-lekkao+ #x0df9) ;  U+0E59 THAI DIGIT NINE 
;;; #endif /* XK_THAI */

;;; /*
;;;  * Korean
;;;  * Byte 3 = 0x0e
;;;  */

;;; #ifdef XK_KOREAN

(defconstant +xk-hangul+ #xff31) ;  Hangul start/stop(toggle) 
(defconstant +xk-hangul-start+ #xff32) ;  Hangul start 
(defconstant +xk-hangul-end+ #xff33) ;  Hangul end, English start 
(defconstant +xk-hangul-hanja+ #xff34) ;  Start Hangul->Hanja Conversion 
(defconstant +xk-hangul-jamo+ #xff35) ;  Hangul Jamo mode 
(defconstant +xk-hangul-romaja+ #xff36) ;  Hangul Romaja mode 
(defconstant +xk-hangul-codeinput+ #xff37) ;  Hangul code input mode 
(defconstant +xk-hangul-jeonja+ #xff38) ;  Jeonja mode 
(defconstant +xk-hangul-banja+ #xff39) ;  Banja mode 
(defconstant +xk-hangul-prehanja+ #xff3a) ;  Pre Hanja conversion 
(defconstant +xk-hangul-posthanja+ #xff3b) ;  Post Hanja conversion 
(defconstant +xk-hangul-singlecandidate+ #xff3c) ;  Single candidate 
(defconstant +xk-hangul-multiplecandidate+ #xff3d) ;  Multiple candidate 
(defconstant +xk-hangul-previouscandidate+ #xff3e) ;  Previous candidate 
(defconstant +xk-hangul-special+ #xff3f) ;  Special symbols 
(defconstant +xk-hangul-switch+ #xff7e) ;  Alias for mode_switch 

;;; /* Hangul Consonant Characters */
(defconstant +xk-hangul-kiyeog+ #x0ea1)
(defconstant +xk-hangul-ssangkiyeog+ #x0ea2)
(defconstant +xk-hangul-kiyeogsios+ #x0ea3)
(defconstant +xk-hangul-nieun+ #x0ea4)
(defconstant +xk-hangul-nieunjieuj+ #x0ea5)
(defconstant +xk-hangul-nieunhieuh+ #x0ea6)
(defconstant +xk-hangul-dikeud+ #x0ea7)
(defconstant +xk-hangul-ssangdikeud+ #x0ea8)
(defconstant +xk-hangul-rieul+ #x0ea9)
(defconstant +xk-hangul-rieulkiyeog+ #x0eaa)
(defconstant +xk-hangul-rieulmieum+ #x0eab)
(defconstant +xk-hangul-rieulpieub+ #x0eac)
(defconstant +xk-hangul-rieulsios+ #x0ead)
(defconstant +xk-hangul-rieultieut+ #x0eae)
(defconstant +xk-hangul-rieulphieuf+ #x0eaf)
(defconstant +xk-hangul-rieulhieuh+ #x0eb0)
(defconstant +xk-hangul-mieum+ #x0eb1)
(defconstant +xk-hangul-pieub+ #x0eb2)
(defconstant +xk-hangul-ssangpieub+ #x0eb3)
(defconstant +xk-hangul-pieubsios+ #x0eb4)
(defconstant +xk-hangul-sios+ #x0eb5)
(defconstant +xk-hangul-ssangsios+ #x0eb6)
(defconstant +xk-hangul-ieung+ #x0eb7)
(defconstant +xk-hangul-jieuj+ #x0eb8)
(defconstant +xk-hangul-ssangjieuj+ #x0eb9)
(defconstant +xk-hangul-cieuc+ #x0eba)
(defconstant +xk-hangul-khieuq+ #x0ebb)
(defconstant +xk-hangul-tieut+ #x0ebc)
(defconstant +xk-hangul-phieuf+ #x0ebd)
(defconstant +xk-hangul-hieuh+ #x0ebe)

;;; /* Hangul Vowel Characters */
(defconstant +xk-hangul-a+ #x0ebf)
(defconstant +xk-hangul-ae+ #x0ec0)
(defconstant +xk-hangul-ya+ #x0ec1)
(defconstant +xk-hangul-yae+ #x0ec2)
(defconstant +xk-hangul-eo+ #x0ec3)
(defconstant +xk-hangul-e+ #x0ec4)
(defconstant +xk-hangul-yeo+ #x0ec5)
(defconstant +xk-hangul-ye+ #x0ec6)
(defconstant +xk-hangul-o+ #x0ec7)
(defconstant +xk-hangul-wa+ #x0ec8)
(defconstant +xk-hangul-wae+ #x0ec9)
(defconstant +xk-hangul-oe+ #x0eca)
(defconstant +xk-hangul-yo+ #x0ecb)
(defconstant +xk-hangul-u+ #x0ecc)
(defconstant +xk-hangul-weo+ #x0ecd)
(defconstant +xk-hangul-we+ #x0ece)
(defconstant +xk-hangul-wi+ #x0ecf)
(defconstant +xk-hangul-yu+ #x0ed0)
(defconstant +xk-hangul-eu+ #x0ed1)
(defconstant +xk-hangul-yi+ #x0ed2)
(defconstant +xk-hangul-i+ #x0ed3)

;;; /* Hangul syllable-final (JongSeong) Characters */
(defconstant +xk-hangul-j-kiyeog+ #x0ed4)
(defconstant +xk-hangul-j-ssangkiyeog+ #x0ed5)
(defconstant +xk-hangul-j-kiyeogsios+ #x0ed6)
(defconstant +xk-hangul-j-nieun+ #x0ed7)
(defconstant +xk-hangul-j-nieunjieuj+ #x0ed8)
(defconstant +xk-hangul-j-nieunhieuh+ #x0ed9)
(defconstant +xk-hangul-j-dikeud+ #x0eda)
(defconstant +xk-hangul-j-rieul+ #x0edb)
(defconstant +xk-hangul-j-rieulkiyeog+ #x0edc)
(defconstant +xk-hangul-j-rieulmieum+ #x0edd)
(defconstant +xk-hangul-j-rieulpieub+ #x0ede)
(defconstant +xk-hangul-j-rieulsios+ #x0edf)
(defconstant +xk-hangul-j-rieultieut+ #x0ee0)
(defconstant +xk-hangul-j-rieulphieuf+ #x0ee1)
(defconstant +xk-hangul-j-rieulhieuh+ #x0ee2)
(defconstant +xk-hangul-j-mieum+ #x0ee3)
(defconstant +xk-hangul-j-pieub+ #x0ee4)
(defconstant +xk-hangul-j-pieubsios+ #x0ee5)
(defconstant +xk-hangul-j-sios+ #x0ee6)
(defconstant +xk-hangul-j-ssangsios+ #x0ee7)
(defconstant +xk-hangul-j-ieung+ #x0ee8)
(defconstant +xk-hangul-j-jieuj+ #x0ee9)
(defconstant +xk-hangul-j-cieuc+ #x0eea)
(defconstant +xk-hangul-j-khieuq+ #x0eeb)
(defconstant +xk-hangul-j-tieut+ #x0eec)
(defconstant +xk-hangul-j-phieuf+ #x0eed)
(defconstant +xk-hangul-j-hieuh+ #x0eee)

;;; /* Ancient Hangul Consonant Characters */
(defconstant +xk-hangul-rieulyeorinhieuh+ #x0eef)
(defconstant +xk-hangul-sunkyeongeummieum+ #x0ef0)
(defconstant +xk-hangul-sunkyeongeumpieub+ #x0ef1)
(defconstant +xk-hangul-pansios+ #x0ef2)
(defconstant +xk-hangul-kkogjidalrinieung+ #x0ef3)
(defconstant +xk-hangul-sunkyeongeumphieuf+ #x0ef4)
(defconstant +xk-hangul-yeorinhieuh+ #x0ef5)

;;; /* Ancient Hangul Vowel Characters */
(defconstant +xk-hangul-araea+ #x0ef6)
(defconstant +xk-hangul-araeae+ #x0ef7)

;;; /* Ancient Hangul syllable-final (JongSeong) Characters */
(defconstant +xk-hangul-j-pansios+ #x0ef8)
(defconstant +xk-hangul-j-kkogjidalrinieung+ #x0ef9)
(defconstant +xk-hangul-j-yeorinhieuh+ #x0efa)

;;; /* Korean currency symbol */
(defconstant +xk-korean-won+ #x0eff) ; (U+20A9 WON SIGN)

;;; #endif /* XK_KOREAN */

;;; /*
;;;  * Armenian
;;;  */

;;; #ifdef XK_ARMENIAN
(defconstant +xk-armenian-ligature-ew+ #x1000587) ;  U+0587 ARMENIAN SMALL LIGATURE ECH YIWN 
(defconstant +xk-armenian-full-stop+ #x1000589) ;  U+0589 ARMENIAN FULL STOP 
(defconstant +xk-armenian-verjaket+ #x1000589) ;  U+0589 ARMENIAN FULL STOP 
(defconstant +xk-armenian-separation-mark+ #x100055d) ;  U+055D ARMENIAN COMMA 
(defconstant +xk-armenian-but+ #x100055d) ;  U+055D ARMENIAN COMMA 
(defconstant +xk-armenian-hyphen+ #x100058a) ;  U+058A ARMENIAN HYPHEN 
(defconstant +xk-armenian-yentamna+ #x100058a) ;  U+058A ARMENIAN HYPHEN 
(defconstant +xk-armenian-exclam+ #x100055c) ;  U+055C ARMENIAN EXCLAMATION MARK 
(defconstant +xk-armenian-amanak+ #x100055c) ;  U+055C ARMENIAN EXCLAMATION MARK 
(defconstant +xk-armenian-accent+ #x100055b) ;  U+055B ARMENIAN EMPHASIS MARK 
(defconstant +xk-armenian-shesht+ #x100055b) ;  U+055B ARMENIAN EMPHASIS MARK 
(defconstant +xk-armenian-question+ #x100055e) ;  U+055E ARMENIAN QUESTION MARK 
(defconstant +xk-armenian-paruyk+ #x100055e) ;  U+055E ARMENIAN QUESTION MARK 
(defconstant +xk-armenian-ayb-cap+ #x1000531) ;  U+0531 ARMENIAN CAPITAL LETTER AYB 
(defconstant +xk-armenian-ayb+ #x1000561) ;  U+0561 ARMENIAN SMALL LETTER AYB 
(defconstant +xk-armenian-ben-cap+ #x1000532) ;  U+0532 ARMENIAN CAPITAL LETTER BEN 
(defconstant +xk-armenian-ben+ #x1000562) ;  U+0562 ARMENIAN SMALL LETTER BEN 
(defconstant +xk-armenian-gim-cap+ #x1000533) ;  U+0533 ARMENIAN CAPITAL LETTER GIM 
(defconstant +xk-armenian-gim+ #x1000563) ;  U+0563 ARMENIAN SMALL LETTER GIM 
(defconstant +xk-armenian-da-cap+ #x1000534) ;  U+0534 ARMENIAN CAPITAL LETTER DA 
(defconstant +xk-armenian-da+ #x1000564) ;  U+0564 ARMENIAN SMALL LETTER DA 
(defconstant +xk-armenian-yech-cap+ #x1000535) ;  U+0535 ARMENIAN CAPITAL LETTER ECH 
(defconstant +xk-armenian-yech+ #x1000565) ;  U+0565 ARMENIAN SMALL LETTER ECH 
(defconstant +xk-armenian-za-cap+ #x1000536) ;  U+0536 ARMENIAN CAPITAL LETTER ZA 
(defconstant +xk-armenian-za+ #x1000566) ;  U+0566 ARMENIAN SMALL LETTER ZA 
(defconstant +xk-armenian-e-cap+ #x1000537) ;  U+0537 ARMENIAN CAPITAL LETTER EH 
(defconstant +xk-armenian-e+ #x1000567) ;  U+0567 ARMENIAN SMALL LETTER EH 
(defconstant +xk-armenian-at-cap+ #x1000538) ;  U+0538 ARMENIAN CAPITAL LETTER ET 
(defconstant +xk-armenian-at+ #x1000568) ;  U+0568 ARMENIAN SMALL LETTER ET 
(defconstant +xk-armenian-to-cap+ #x1000539) ;  U+0539 ARMENIAN CAPITAL LETTER TO 
(defconstant +xk-armenian-to+ #x1000569) ;  U+0569 ARMENIAN SMALL LETTER TO 
(defconstant +xk-armenian-zhe-cap+ #x100053a) ;  U+053A ARMENIAN CAPITAL LETTER ZHE 
(defconstant +xk-armenian-zhe+ #x100056a) ;  U+056A ARMENIAN SMALL LETTER ZHE 
(defconstant +xk-armenian-ini-cap+ #x100053b) ;  U+053B ARMENIAN CAPITAL LETTER INI 
(defconstant +xk-armenian-ini+ #x100056b) ;  U+056B ARMENIAN SMALL LETTER INI 
(defconstant +xk-armenian-lyun-cap+ #x100053c) ;  U+053C ARMENIAN CAPITAL LETTER LIWN 
(defconstant +xk-armenian-lyun+ #x100056c) ;  U+056C ARMENIAN SMALL LETTER LIWN 
(defconstant +xk-armenian-khe-cap+ #x100053d) ;  U+053D ARMENIAN CAPITAL LETTER XEH 
(defconstant +xk-armenian-khe+ #x100056d) ;  U+056D ARMENIAN SMALL LETTER XEH 
(defconstant +xk-armenian-tsa-cap+ #x100053e) ;  U+053E ARMENIAN CAPITAL LETTER CA 
(defconstant +xk-armenian-tsa+ #x100056e) ;  U+056E ARMENIAN SMALL LETTER CA 
(defconstant +xk-armenian-ken-cap+ #x100053f) ;  U+053F ARMENIAN CAPITAL LETTER KEN 
(defconstant +xk-armenian-ken+ #x100056f) ;  U+056F ARMENIAN SMALL LETTER KEN 
(defconstant +xk-armenian-ho-cap+ #x1000540) ;  U+0540 ARMENIAN CAPITAL LETTER HO 
(defconstant +xk-armenian-ho+ #x1000570) ;  U+0570 ARMENIAN SMALL LETTER HO 
(defconstant +xk-armenian-dza-cap+ #x1000541) ;  U+0541 ARMENIAN CAPITAL LETTER JA 
(defconstant +xk-armenian-dza+ #x1000571) ;  U+0571 ARMENIAN SMALL LETTER JA 
(defconstant +xk-armenian-ghat-cap+ #x1000542) ;  U+0542 ARMENIAN CAPITAL LETTER GHAD 
(defconstant +xk-armenian-ghat+ #x1000572) ;  U+0572 ARMENIAN SMALL LETTER GHAD 
(defconstant +xk-armenian-tche-cap+ #x1000543) ;  U+0543 ARMENIAN CAPITAL LETTER CHEH 
(defconstant +xk-armenian-tche+ #x1000573) ;  U+0573 ARMENIAN SMALL LETTER CHEH 
(defconstant +xk-armenian-men-cap+ #x1000544) ;  U+0544 ARMENIAN CAPITAL LETTER MEN 
(defconstant +xk-armenian-men+ #x1000574) ;  U+0574 ARMENIAN SMALL LETTER MEN 
(defconstant +xk-armenian-hi-cap+ #x1000545) ;  U+0545 ARMENIAN CAPITAL LETTER YI 
(defconstant +xk-armenian-hi+ #x1000575) ;  U+0575 ARMENIAN SMALL LETTER YI 
(defconstant +xk-armenian-nu-cap+ #x1000546) ;  U+0546 ARMENIAN CAPITAL LETTER NOW 
(defconstant +xk-armenian-nu+ #x1000576) ;  U+0576 ARMENIAN SMALL LETTER NOW 
(defconstant +xk-armenian-sha-cap+ #x1000547) ;  U+0547 ARMENIAN CAPITAL LETTER SHA 
(defconstant +xk-armenian-sha+ #x1000577) ;  U+0577 ARMENIAN SMALL LETTER SHA 
(defconstant +xk-armenian-vo-cap+ #x1000548) ;  U+0548 ARMENIAN CAPITAL LETTER VO 
(defconstant +xk-armenian-vo+ #x1000578) ;  U+0578 ARMENIAN SMALL LETTER VO 
(defconstant +xk-armenian-cha-cap+ #x1000549) ;  U+0549 ARMENIAN CAPITAL LETTER CHA 
(defconstant +xk-armenian-cha+ #x1000579) ;  U+0579 ARMENIAN SMALL LETTER CHA 
(defconstant +xk-armenian-pe-cap+ #x100054a) ;  U+054A ARMENIAN CAPITAL LETTER PEH 
(defconstant +xk-armenian-pe+ #x100057a) ;  U+057A ARMENIAN SMALL LETTER PEH 
(defconstant +xk-armenian-je-cap+ #x100054b) ;  U+054B ARMENIAN CAPITAL LETTER JHEH 
(defconstant +xk-armenian-je+ #x100057b) ;  U+057B ARMENIAN SMALL LETTER JHEH 
(defconstant +xk-armenian-ra-cap+ #x100054c) ;  U+054C ARMENIAN CAPITAL LETTER RA 
(defconstant +xk-armenian-ra+ #x100057c) ;  U+057C ARMENIAN SMALL LETTER RA 
(defconstant +xk-armenian-se-cap+ #x100054d) ;  U+054D ARMENIAN CAPITAL LETTER SEH 
(defconstant +xk-armenian-se+ #x100057d) ;  U+057D ARMENIAN SMALL LETTER SEH 
(defconstant +xk-armenian-vev-cap+ #x100054e) ;  U+054E ARMENIAN CAPITAL LETTER VEW 
(defconstant +xk-armenian-vev+ #x100057e) ;  U+057E ARMENIAN SMALL LETTER VEW 
(defconstant +xk-armenian-tyun-cap+ #x100054f) ;  U+054F ARMENIAN CAPITAL LETTER TIWN 
(defconstant +xk-armenian-tyun+ #x100057f) ;  U+057F ARMENIAN SMALL LETTER TIWN 
(defconstant +xk-armenian-re-cap+ #x1000550) ;  U+0550 ARMENIAN CAPITAL LETTER REH 
(defconstant +xk-armenian-re+ #x1000580) ;  U+0580 ARMENIAN SMALL LETTER REH 
(defconstant +xk-armenian-tso-cap+ #x1000551) ;  U+0551 ARMENIAN CAPITAL LETTER CO 
(defconstant +xk-armenian-tso+ #x1000581) ;  U+0581 ARMENIAN SMALL LETTER CO 
(defconstant +xk-armenian-vyun-cap+ #x1000552) ;  U+0552 ARMENIAN CAPITAL LETTER YIWN 
(defconstant +xk-armenian-vyun+ #x1000582) ;  U+0582 ARMENIAN SMALL LETTER YIWN 
(defconstant +xk-armenian-pyur-cap+ #x1000553) ;  U+0553 ARMENIAN CAPITAL LETTER PIWR 
(defconstant +xk-armenian-pyur+ #x1000583) ;  U+0583 ARMENIAN SMALL LETTER PIWR 
(defconstant +xk-armenian-ke-cap+ #x1000554) ;  U+0554 ARMENIAN CAPITAL LETTER KEH 
(defconstant +xk-armenian-ke+ #x1000584) ;  U+0584 ARMENIAN SMALL LETTER KEH 
(defconstant +xk-armenian-o-cap+ #x1000555) ;  U+0555 ARMENIAN CAPITAL LETTER OH 
(defconstant +xk-armenian-o+ #x1000585) ;  U+0585 ARMENIAN SMALL LETTER OH 
(defconstant +xk-armenian-fe-cap+ #x1000556) ;  U+0556 ARMENIAN CAPITAL LETTER FEH 
(defconstant +xk-armenian-fe+ #x1000586) ;  U+0586 ARMENIAN SMALL LETTER FEH 
(defconstant +xk-armenian-apostrophe+ #x100055a) ;  U+055A ARMENIAN APOSTROPHE 
;;; #endif /* XK_ARMENIAN */

;;; /*
;;;  * Georgian
;;;  */

;;; #ifdef XK_GEORGIAN
(defconstant +xk-georgian-an+ #x10010d0) ;  U+10D0 GEORGIAN LETTER AN 
(defconstant +xk-georgian-ban+ #x10010d1) ;  U+10D1 GEORGIAN LETTER BAN 
(defconstant +xk-georgian-gan+ #x10010d2) ;  U+10D2 GEORGIAN LETTER GAN 
(defconstant +xk-georgian-don+ #x10010d3) ;  U+10D3 GEORGIAN LETTER DON 
(defconstant +xk-georgian-en+ #x10010d4) ;  U+10D4 GEORGIAN LETTER EN 
(defconstant +xk-georgian-vin+ #x10010d5) ;  U+10D5 GEORGIAN LETTER VIN 
(defconstant +xk-georgian-zen+ #x10010d6) ;  U+10D6 GEORGIAN LETTER ZEN 
(defconstant +xk-georgian-tan+ #x10010d7) ;  U+10D7 GEORGIAN LETTER TAN 
(defconstant +xk-georgian-in+ #x10010d8) ;  U+10D8 GEORGIAN LETTER IN 
(defconstant +xk-georgian-kan+ #x10010d9) ;  U+10D9 GEORGIAN LETTER KAN 
(defconstant +xk-georgian-las+ #x10010da) ;  U+10DA GEORGIAN LETTER LAS 
(defconstant +xk-georgian-man+ #x10010db) ;  U+10DB GEORGIAN LETTER MAN 
(defconstant +xk-georgian-nar+ #x10010dc) ;  U+10DC GEORGIAN LETTER NAR 
(defconstant +xk-georgian-on+ #x10010dd) ;  U+10DD GEORGIAN LETTER ON 
(defconstant +xk-georgian-par+ #x10010de) ;  U+10DE GEORGIAN LETTER PAR 
(defconstant +xk-georgian-zhar+ #x10010df) ;  U+10DF GEORGIAN LETTER ZHAR 
(defconstant +xk-georgian-rae+ #x10010e0) ;  U+10E0 GEORGIAN LETTER RAE 
(defconstant +xk-georgian-san+ #x10010e1) ;  U+10E1 GEORGIAN LETTER SAN 
(defconstant +xk-georgian-tar+ #x10010e2) ;  U+10E2 GEORGIAN LETTER TAR 
(defconstant +xk-georgian-un+ #x10010e3) ;  U+10E3 GEORGIAN LETTER UN 
(defconstant +xk-georgian-phar+ #x10010e4) ;  U+10E4 GEORGIAN LETTER PHAR 
(defconstant +xk-georgian-khar+ #x10010e5) ;  U+10E5 GEORGIAN LETTER KHAR 
(defconstant +xk-georgian-ghan+ #x10010e6) ;  U+10E6 GEORGIAN LETTER GHAN 
(defconstant +xk-georgian-qar+ #x10010e7) ;  U+10E7 GEORGIAN LETTER QAR 
(defconstant +xk-georgian-shin+ #x10010e8) ;  U+10E8 GEORGIAN LETTER SHIN 
(defconstant +xk-georgian-chin+ #x10010e9) ;  U+10E9 GEORGIAN LETTER CHIN 
(defconstant +xk-georgian-can+ #x10010ea) ;  U+10EA GEORGIAN LETTER CAN 
(defconstant +xk-georgian-jil+ #x10010eb) ;  U+10EB GEORGIAN LETTER JIL 
(defconstant +xk-georgian-cil+ #x10010ec) ;  U+10EC GEORGIAN LETTER CIL 
(defconstant +xk-georgian-char+ #x10010ed) ;  U+10ED GEORGIAN LETTER CHAR 
(defconstant +xk-georgian-xan+ #x10010ee) ;  U+10EE GEORGIAN LETTER XAN 
(defconstant +xk-georgian-jhan+ #x10010ef) ;  U+10EF GEORGIAN LETTER JHAN 
(defconstant +xk-georgian-hae+ #x10010f0) ;  U+10F0 GEORGIAN LETTER HAE 
(defconstant +xk-georgian-he+ #x10010f1) ;  U+10F1 GEORGIAN LETTER HE 
(defconstant +xk-georgian-hie+ #x10010f2) ;  U+10F2 GEORGIAN LETTER HIE 
(defconstant +xk-georgian-we+ #x10010f3) ;  U+10F3 GEORGIAN LETTER WE 
(defconstant +xk-georgian-har+ #x10010f4) ;  U+10F4 GEORGIAN LETTER HAR 
(defconstant +xk-georgian-hoe+ #x10010f5) ;  U+10F5 GEORGIAN LETTER HOE 
(defconstant +xk-georgian-fi+ #x10010f6) ;  U+10F6 GEORGIAN LETTER FI 
;;; #endif /* XK_GEORGIAN */

;;; /*
;;;  * Azeri (and other Turkic or Caucasian languages)
;;;  */

;;; #ifdef XK_CAUCASUS
;;; /* latin */
(defconstant +xk-xabovedot-cap+ #x1001e8a) ;  U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE 
(defconstant +xk-ibreve-cap+ #x100012c) ;  U+012C LATIN CAPITAL LETTER I WITH BREVE 
(defconstant +xk-zstroke-cap+ #x10001b5) ;  U+01B5 LATIN CAPITAL LETTER Z WITH STROKE 
(defconstant +xk-gcaron-cap+ #x10001e6) ;  U+01E6 LATIN CAPITAL LETTER G WITH CARON 
(defconstant +xk-ocaron-cap+ #x10001d1) ;  U+01D2 LATIN CAPITAL LETTER O WITH CARON 
(defconstant +xk-obarred-cap+ #x100019f) ;  U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE 
(defconstant +xk-xabovedot+ #x1001e8b) ;  U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE 
(defconstant +xk-ibreve+ #x100012d) ;  U+012D LATIN SMALL LETTER I WITH BREVE 
(defconstant +xk-zstroke+ #x10001b6) ;  U+01B6 LATIN SMALL LETTER Z WITH STROKE 
(defconstant +xk-gcaron+ #x10001e7) ;  U+01E7 LATIN SMALL LETTER G WITH CARON 
(defconstant +xk-ocaron+ #x10001d2) ;  U+01D2 LATIN SMALL LETTER O WITH CARON 
(defconstant +xk-obarred+ #x1000275) ;  U+0275 LATIN SMALL LETTER BARRED O 
(defconstant +xk-schwa-cap+ #x100018f) ;  U+018F LATIN CAPITAL LETTER SCHWA 
(defconstant +xk-schwa+ #x1000259) ;  U+0259 LATIN SMALL LETTER SCHWA 
(defconstant +xk-ezh-cap+ #x10001b7) ;  U+01B7 LATIN CAPITAL LETTER EZH 
(defconstant +xk-ezh+ #x1000292) ;  U+0292 LATIN SMALL LETTER EZH 
;;; /* those are not really Caucasus */
;;; /* For Inupiak */
(defconstant +xk-lbelowdot-cap+ #x1001e36) ;  U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW 
(defconstant +xk-lbelowdot+ #x1001e37) ;  U+1E37 LATIN SMALL LETTER L WITH DOT BELOW 
;;; #endif /* XK_CAUCASUS */

;;; /*
;;;  * Vietnamese
;;;  */
;;;  
;;; #ifdef XK_VIETNAMESE
(defconstant +xk-abelowdot-cap+ #x1001ea0) ;  U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW 
(defconstant +xk-abelowdot+ #x1001ea1) ;  U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW 
(defconstant +xk-ahook-cap+ #x1001ea2) ;  U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE 
(defconstant +xk-ahook+ #x1001ea3) ;  U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE 
(defconstant +xk-acircumflexacute-cap+ #x1001ea4) ;  U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE 
(defconstant +xk-acircumflexacute+ #x1001ea5) ;  U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE 
(defconstant +xk-acircumflexgrave-cap+ #x1001ea6) ;  U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE 
(defconstant +xk-acircumflexgrave+ #x1001ea7) ;  U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE 
(defconstant +xk-acircumflexhook-cap+ #x1001ea8) ;  U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE 
(defconstant +xk-acircumflexhook+ #x1001ea9) ;  U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE 
(defconstant +xk-acircumflextilde-cap+ #x1001eaa) ;  U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE 
(defconstant +xk-acircumflextilde+ #x1001eab) ;  U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE 
(defconstant +xk-acircumflexbelowdot-cap+ #x1001eac) ;  U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW 
(defconstant +xk-acircumflexbelowdot+ #x1001ead) ;  U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW 
(defconstant +xk-abreveacute-cap+ #x1001eae) ;  U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE 
(defconstant +xk-abreveacute+ #x1001eaf) ;  U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE 
(defconstant +xk-abrevegrave-cap+ #x1001eb0) ;  U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE 
(defconstant +xk-abrevegrave+ #x1001eb1) ;  U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE 
(defconstant +xk-abrevehook-cap+ #x1001eb2) ;  U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE 
(defconstant +xk-abrevehook+ #x1001eb3) ;  U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE 
(defconstant +xk-abrevetilde-cap+ #x1001eb4) ;  U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE 
(defconstant +xk-abrevetilde+ #x1001eb5) ;  U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE 
(defconstant +xk-abrevebelowdot-cap+ #x1001eb6) ;  U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW 
(defconstant +xk-abrevebelowdot+ #x1001eb7) ;  U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW 
(defconstant +xk-ebelowdot-cap+ #x1001eb8) ;  U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW 
(defconstant +xk-ebelowdot+ #x1001eb9) ;  U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW 
(defconstant +xk-ehook-cap+ #x1001eba) ;  U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE 
(defconstant +xk-ehook+ #x1001ebb) ;  U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE 
(defconstant +xk-etilde-cap+ #x1001ebc) ;  U+1EBC LATIN CAPITAL LETTER E WITH TILDE 
(defconstant +xk-etilde+ #x1001ebd) ;  U+1EBD LATIN SMALL LETTER E WITH TILDE 
(defconstant +xk-ecircumflexacute-cap+ #x1001ebe) ;  U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE 
(defconstant +xk-ecircumflexacute+ #x1001ebf) ;  U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE 
(defconstant +xk-ecircumflexgrave-cap+ #x1001ec0) ;  U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE 
(defconstant +xk-ecircumflexgrave+ #x1001ec1) ;  U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE 
(defconstant +xk-ecircumflexhook-cap+ #x1001ec2) ;  U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE 
(defconstant +xk-ecircumflexhook+ #x1001ec3) ;  U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE 
(defconstant +xk-ecircumflextilde-cap+ #x1001ec4) ;  U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE 
(defconstant +xk-ecircumflextilde+ #x1001ec5) ;  U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE 
(defconstant +xk-ecircumflexbelowdot-cap+ #x1001ec6) ;  U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW 
(defconstant +xk-ecircumflexbelowdot+ #x1001ec7) ;  U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW 
(defconstant +xk-ihook-cap+ #x1001ec8) ;  U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE 
(defconstant +xk-ihook+ #x1001ec9) ;  U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE 
(defconstant +xk-ibelowdot-cap+ #x1001eca) ;  U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW 
(defconstant +xk-ibelowdot+ #x1001ecb) ;  U+1ECB LATIN SMALL LETTER I WITH DOT BELOW 
(defconstant +xk-obelowdot-cap+ #x1001ecc) ;  U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW 
(defconstant +xk-obelowdot+ #x1001ecd) ;  U+1ECD LATIN SMALL LETTER O WITH DOT BELOW 
(defconstant +xk-ohook-cap+ #x1001ece) ;  U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE 
(defconstant +xk-ohook+ #x1001ecf) ;  U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE 
(defconstant +xk-ocircumflexacute-cap+ #x1001ed0) ;  U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE 
(defconstant +xk-ocircumflexacute+ #x1001ed1) ;  U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE 
(defconstant +xk-ocircumflexgrave-cap+ #x1001ed2) ;  U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE 
(defconstant +xk-ocircumflexgrave+ #x1001ed3) ;  U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE 
(defconstant +xk-ocircumflexhook-cap+ #x1001ed4) ;  U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE 
(defconstant +xk-ocircumflexhook+ #x1001ed5) ;  U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE 
(defconstant +xk-ocircumflextilde-cap+ #x1001ed6) ;  U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE 
(defconstant +xk-ocircumflextilde+ #x1001ed7) ;  U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE 
(defconstant +xk-ocircumflexbelowdot-cap+ #x1001ed8) ;  U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW 
(defconstant +xk-ocircumflexbelowdot+ #x1001ed9) ;  U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW 
(defconstant +xk-ohornacute-cap+ #x1001eda) ;  U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE 
(defconstant +xk-ohornacute+ #x1001edb) ;  U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE 
(defconstant +xk-ohorngrave-cap+ #x1001edc) ;  U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE 
(defconstant +xk-ohorngrave+ #x1001edd) ;  U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE 
(defconstant +xk-ohornhook-cap+ #x1001ede) ;  U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE 
(defconstant +xk-ohornhook+ #x1001edf) ;  U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE 
(defconstant +xk-ohorntilde-cap+ #x1001ee0) ;  U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE 
(defconstant +xk-ohorntilde+ #x1001ee1) ;  U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE 
(defconstant +xk-ohornbelowdot-cap+ #x1001ee2) ;  U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW 
(defconstant +xk-ohornbelowdot+ #x1001ee3) ;  U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW 
(defconstant +xk-ubelowdot-cap+ #x1001ee4) ;  U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW 
(defconstant +xk-ubelowdot+ #x1001ee5) ;  U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW 
(defconstant +xk-uhook-cap+ #x1001ee6) ;  U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE 
(defconstant +xk-uhook+ #x1001ee7) ;  U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE 
(defconstant +xk-uhornacute-cap+ #x1001ee8) ;  U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE 
(defconstant +xk-uhornacute+ #x1001ee9) ;  U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE 
(defconstant +xk-uhorngrave-cap+ #x1001eea) ;  U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE 
(defconstant +xk-uhorngrave+ #x1001eeb) ;  U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE 
(defconstant +xk-uhornhook-cap+ #x1001eec) ;  U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE 
(defconstant +xk-uhornhook+ #x1001eed) ;  U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE 
(defconstant +xk-uhorntilde-cap+ #x1001eee) ;  U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE 
(defconstant +xk-uhorntilde+ #x1001eef) ;  U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE 
(defconstant +xk-uhornbelowdot-cap+ #x1001ef0) ;  U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW 
(defconstant +xk-uhornbelowdot+ #x1001ef1) ;  U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW 
(defconstant +xk-ybelowdot-cap+ #x1001ef4) ;  U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW 
(defconstant +xk-ybelowdot+ #x1001ef5) ;  U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW 
(defconstant +xk-yhook-cap+ #x1001ef6) ;  U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE 
(defconstant +xk-yhook+ #x1001ef7) ;  U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE 
(defconstant +xk-ytilde-cap+ #x1001ef8) ;  U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE 
(defconstant +xk-ytilde+ #x1001ef9) ;  U+1EF9 LATIN SMALL LETTER Y WITH TILDE 
(defconstant +xk-ohorn-cap+ #x10001a0) ;  U+01A0 LATIN CAPITAL LETTER O WITH HORN 
(defconstant +xk-ohorn+ #x10001a1) ;  U+01A1 LATIN SMALL LETTER O WITH HORN 
(defconstant +xk-uhorn-cap+ #x10001af) ;  U+01AF LATIN CAPITAL LETTER U WITH HORN 
(defconstant +xk-uhorn+ #x10001b0) ;  U+01B0 LATIN SMALL LETTER U WITH HORN 

;;; #endif /* XK_VIETNAMESE */

;;; #ifdef XK_CURRENCY
(defconstant +xk-ecusign+ #x10020a0) ;  U+20A0 EURO-CURRENCY SIGN 
(defconstant +xk-colonsign+ #x10020a1) ;  U+20A1 COLON SIGN 
(defconstant +xk-cruzeirosign+ #x10020a2) ;  U+20A2 CRUZEIRO SIGN 
(defconstant +xk-ffrancsign+ #x10020a3) ;  U+20A3 FRENCH FRANC SIGN 
(defconstant +xk-lirasign+ #x10020a4) ;  U+20A4 LIRA SIGN 
(defconstant +xk-millsign+ #x10020a5) ;  U+20A5 MILL SIGN 
(defconstant +xk-nairasign+ #x10020a6) ;  U+20A6 NAIRA SIGN 
(defconstant +xk-pesetasign+ #x10020a7) ;  U+20A7 PESETA SIGN 
(defconstant +xk-rupeesign+ #x10020a8) ;  U+20A8 RUPEE SIGN 
(defconstant +xk-wonsign+ #x10020a9) ;  U+20A9 WON SIGN 
(defconstant +xk-newsheqelsign+ #x10020aa) ;  U+20AA NEW SHEQEL SIGN 
(defconstant +xk-dongsign+ #x10020ab) ;  U+20AB DONG SIGN 
(defconstant +xk-eurosign+ #x20ac) ;  U+20AC EURO SIGN 
;;; #endif /* XK_CURRENCY */

;;; #ifdef XK_MATHEMATICAL
;;; /* one, two and three are defined above. */
(defconstant +xk-zerosuperior+ #x1002070) ;  U+2070 SUPERSCRIPT ZERO 
(defconstant +xk-foursuperior+ #x1002074) ;  U+2074 SUPERSCRIPT FOUR 
(defconstant +xk-fivesuperior+ #x1002075) ;  U+2075 SUPERSCRIPT FIVE 
(defconstant +xk-sixsuperior+ #x1002076) ;  U+2076 SUPERSCRIPT SIX 
(defconstant +xk-sevensuperior+ #x1002077) ;  U+2077 SUPERSCRIPT SEVEN 
(defconstant +xk-eightsuperior+ #x1002078) ;  U+2078 SUPERSCRIPT EIGHT 
(defconstant +xk-ninesuperior+ #x1002079) ;  U+2079 SUPERSCRIPT NINE 
(defconstant +xk-zerosubscript+ #x1002080) ;  U+2080 SUBSCRIPT ZERO 
(defconstant +xk-onesubscript+ #x1002081) ;  U+2081 SUBSCRIPT ONE 
(defconstant +xk-twosubscript+ #x1002082) ;  U+2082 SUBSCRIPT TWO 
(defconstant +xk-threesubscript+ #x1002083) ;  U+2083 SUBSCRIPT THREE 
(defconstant +xk-foursubscript+ #x1002084) ;  U+2084 SUBSCRIPT FOUR 
(defconstant +xk-fivesubscript+ #x1002085) ;  U+2085 SUBSCRIPT FIVE 
(defconstant +xk-sixsubscript+ #x1002086) ;  U+2086 SUBSCRIPT SIX 
(defconstant +xk-sevensubscript+ #x1002087) ;  U+2087 SUBSCRIPT SEVEN 
(defconstant +xk-eightsubscript+ #x1002088) ;  U+2088 SUBSCRIPT EIGHT 
(defconstant +xk-ninesubscript+ #x1002089) ;  U+2089 SUBSCRIPT NINE 
(defconstant +xk-partdifferential+ #x1002202) ;  U+2202 PARTIAL DIFFERENTIAL 
(defconstant +xk-emptyset+ #x1002205) ;  U+2205 NULL SET 
(defconstant +xk-elementof+ #x1002208) ;  U+2208 ELEMENT OF 
(defconstant +xk-notelementof+ #x1002209) ;  U+2209 NOT AN ELEMENT OF 
(defconstant +xk-containsas+ #x100220B) ;  U+220B CONTAINS AS MEMBER 
(defconstant +xk-squareroot+ #x100221A) ;  U+221A SQUARE ROOT 
(defconstant +xk-cuberoot+ #x100221B) ;  U+221B CUBE ROOT 
(defconstant +xk-fourthroot+ #x100221C) ;  U+221C FOURTH ROOT 
(defconstant +xk-dintegral+ #x100222C) ;  U+222C DOUBLE INTEGRAL 
(defconstant +xk-tintegral+ #x100222D) ;  U+222D TRIPLE INTEGRAL 
(defconstant +xk-because+ #x1002235) ;  U+2235 BECAUSE 
(defconstant +xk-approxeq+ #x1002248) ;  U+2245 ALMOST EQUAL TO 
(defconstant +xk-notapproxeq+ #x1002247) ;  U+2247 NOT ALMOST EQUAL TO 
(defconstant +xk-notidentical+ #x1002262) ;  U+2262 NOT IDENTICAL TO 
(defconstant +xk-stricteq+ #x1002263) ;  U+2263 STRICTLY EQUIVALENT TO           
;;; #endif /* XK_MATHEMATICAL */

;;; #ifdef XK_BRAILLE
(defconstant +xk-braille-dot-1+ #xfff1)
(defconstant +xk-braille-dot-2+ #xfff2)
(defconstant +xk-braille-dot-3+ #xfff3)
(defconstant +xk-braille-dot-4+ #xfff4)
(defconstant +xk-braille-dot-5+ #xfff5)
(defconstant +xk-braille-dot-6+ #xfff6)
(defconstant +xk-braille-dot-7+ #xfff7)
(defconstant +xk-braille-dot-8+ #xfff8)
(defconstant +xk-braille-dot-9+ #xfff9)
(defconstant +xk-braille-dot-10+ #xfffa)
(defconstant +xk-braille-blank+ #x1002800) ;  U+2800 BRAILLE PATTERN BLANK 
(defconstant +xk-braille-dots-1+ #x1002801) ;  U+2801 BRAILLE PATTERN DOTS-1 
(defconstant +xk-braille-dots-2+ #x1002802) ;  U+2802 BRAILLE PATTERN DOTS-2 
(defconstant +xk-braille-dots-12+ #x1002803) ;  U+2803 BRAILLE PATTERN DOTS-12 
(defconstant +xk-braille-dots-3+ #x1002804) ;  U+2804 BRAILLE PATTERN DOTS-3 
(defconstant +xk-braille-dots-13+ #x1002805) ;  U+2805 BRAILLE PATTERN DOTS-13 
(defconstant +xk-braille-dots-23+ #x1002806) ;  U+2806 BRAILLE PATTERN DOTS-23 
(defconstant +xk-braille-dots-123+ #x1002807) ;  U+2807 BRAILLE PATTERN DOTS-123 
(defconstant +xk-braille-dots-4+ #x1002808) ;  U+2808 BRAILLE PATTERN DOTS-4 
(defconstant +xk-braille-dots-14+ #x1002809) ;  U+2809 BRAILLE PATTERN DOTS-14 
(defconstant +xk-braille-dots-24+ #x100280a) ;  U+280a BRAILLE PATTERN DOTS-24 
(defconstant +xk-braille-dots-124+ #x100280b) ;  U+280b BRAILLE PATTERN DOTS-124 
(defconstant +xk-braille-dots-34+ #x100280c) ;  U+280c BRAILLE PATTERN DOTS-34 
(defconstant +xk-braille-dots-134+ #x100280d) ;  U+280d BRAILLE PATTERN DOTS-134 
(defconstant +xk-braille-dots-234+ #x100280e) ;  U+280e BRAILLE PATTERN DOTS-234 
(defconstant +xk-braille-dots-1234+ #x100280f) ;  U+280f BRAILLE PATTERN DOTS-1234 
(defconstant +xk-braille-dots-5+ #x1002810) ;  U+2810 BRAILLE PATTERN DOTS-5 
(defconstant +xk-braille-dots-15+ #x1002811) ;  U+2811 BRAILLE PATTERN DOTS-15 
(defconstant +xk-braille-dots-25+ #x1002812) ;  U+2812 BRAILLE PATTERN DOTS-25 
(defconstant +xk-braille-dots-125+ #x1002813) ;  U+2813 BRAILLE PATTERN DOTS-125 
(defconstant +xk-braille-dots-35+ #x1002814) ;  U+2814 BRAILLE PATTERN DOTS-35 
(defconstant +xk-braille-dots-135+ #x1002815) ;  U+2815 BRAILLE PATTERN DOTS-135 
(defconstant +xk-braille-dots-235+ #x1002816) ;  U+2816 BRAILLE PATTERN DOTS-235 
(defconstant +xk-braille-dots-1235+ #x1002817) ;  U+2817 BRAILLE PATTERN DOTS-1235 
(defconstant +xk-braille-dots-45+ #x1002818) ;  U+2818 BRAILLE PATTERN DOTS-45 
(defconstant +xk-braille-dots-145+ #x1002819) ;  U+2819 BRAILLE PATTERN DOTS-145 
(defconstant +xk-braille-dots-245+ #x100281a) ;  U+281a BRAILLE PATTERN DOTS-245 
(defconstant +xk-braille-dots-1245+ #x100281b) ;  U+281b BRAILLE PATTERN DOTS-1245 
(defconstant +xk-braille-dots-345+ #x100281c) ;  U+281c BRAILLE PATTERN DOTS-345 
(defconstant +xk-braille-dots-1345+ #x100281d) ;  U+281d BRAILLE PATTERN DOTS-1345 
(defconstant +xk-braille-dots-2345+ #x100281e) ;  U+281e BRAILLE PATTERN DOTS-2345 
(defconstant +xk-braille-dots-12345+ #x100281f) ;  U+281f BRAILLE PATTERN DOTS-12345 
(defconstant +xk-braille-dots-6+ #x1002820) ;  U+2820 BRAILLE PATTERN DOTS-6 
(defconstant +xk-braille-dots-16+ #x1002821) ;  U+2821 BRAILLE PATTERN DOTS-16 
(defconstant +xk-braille-dots-26+ #x1002822) ;  U+2822 BRAILLE PATTERN DOTS-26 
(defconstant +xk-braille-dots-126+ #x1002823) ;  U+2823 BRAILLE PATTERN DOTS-126 
(defconstant +xk-braille-dots-36+ #x1002824) ;  U+2824 BRAILLE PATTERN DOTS-36 
(defconstant +xk-braille-dots-136+ #x1002825) ;  U+2825 BRAILLE PATTERN DOTS-136 
(defconstant +xk-braille-dots-236+ #x1002826) ;  U+2826 BRAILLE PATTERN DOTS-236 
(defconstant +xk-braille-dots-1236+ #x1002827) ;  U+2827 BRAILLE PATTERN DOTS-1236 
(defconstant +xk-braille-dots-46+ #x1002828) ;  U+2828 BRAILLE PATTERN DOTS-46 
(defconstant +xk-braille-dots-146+ #x1002829) ;  U+2829 BRAILLE PATTERN DOTS-146 
(defconstant +xk-braille-dots-246+ #x100282a) ;  U+282a BRAILLE PATTERN DOTS-246 
(defconstant +xk-braille-dots-1246+ #x100282b) ;  U+282b BRAILLE PATTERN DOTS-1246 
(defconstant +xk-braille-dots-346+ #x100282c) ;  U+282c BRAILLE PATTERN DOTS-346 
(defconstant +xk-braille-dots-1346+ #x100282d) ;  U+282d BRAILLE PATTERN DOTS-1346 
(defconstant +xk-braille-dots-2346+ #x100282e) ;  U+282e BRAILLE PATTERN DOTS-2346 
(defconstant +xk-braille-dots-12346+ #x100282f) ;  U+282f BRAILLE PATTERN DOTS-12346 
(defconstant +xk-braille-dots-56+ #x1002830) ;  U+2830 BRAILLE PATTERN DOTS-56 
(defconstant +xk-braille-dots-156+ #x1002831) ;  U+2831 BRAILLE PATTERN DOTS-156 
(defconstant +xk-braille-dots-256+ #x1002832) ;  U+2832 BRAILLE PATTERN DOTS-256 
(defconstant +xk-braille-dots-1256+ #x1002833) ;  U+2833 BRAILLE PATTERN DOTS-1256 
(defconstant +xk-braille-dots-356+ #x1002834) ;  U+2834 BRAILLE PATTERN DOTS-356 
(defconstant +xk-braille-dots-1356+ #x1002835) ;  U+2835 BRAILLE PATTERN DOTS-1356 
(defconstant +xk-braille-dots-2356+ #x1002836) ;  U+2836 BRAILLE PATTERN DOTS-2356 
(defconstant +xk-braille-dots-12356+ #x1002837) ;  U+2837 BRAILLE PATTERN DOTS-12356 
(defconstant +xk-braille-dots-456+ #x1002838) ;  U+2838 BRAILLE PATTERN DOTS-456 
(defconstant +xk-braille-dots-1456+ #x1002839) ;  U+2839 BRAILLE PATTERN DOTS-1456 
(defconstant +xk-braille-dots-2456+ #x100283a) ;  U+283a BRAILLE PATTERN DOTS-2456 
(defconstant +xk-braille-dots-12456+ #x100283b) ;  U+283b BRAILLE PATTERN DOTS-12456 
(defconstant +xk-braille-dots-3456+ #x100283c) ;  U+283c BRAILLE PATTERN DOTS-3456 
(defconstant +xk-braille-dots-13456+ #x100283d) ;  U+283d BRAILLE PATTERN DOTS-13456 
(defconstant +xk-braille-dots-23456+ #x100283e) ;  U+283e BRAILLE PATTERN DOTS-23456 
(defconstant +xk-braille-dots-123456+ #x100283f) ;  U+283f BRAILLE PATTERN DOTS-123456 
(defconstant +xk-braille-dots-7+ #x1002840) ;  U+2840 BRAILLE PATTERN DOTS-7 
(defconstant +xk-braille-dots-17+ #x1002841) ;  U+2841 BRAILLE PATTERN DOTS-17 
(defconstant +xk-braille-dots-27+ #x1002842) ;  U+2842 BRAILLE PATTERN DOTS-27 
(defconstant +xk-braille-dots-127+ #x1002843) ;  U+2843 BRAILLE PATTERN DOTS-127 
(defconstant +xk-braille-dots-37+ #x1002844) ;  U+2844 BRAILLE PATTERN DOTS-37 
(defconstant +xk-braille-dots-137+ #x1002845) ;  U+2845 BRAILLE PATTERN DOTS-137 
(defconstant +xk-braille-dots-237+ #x1002846) ;  U+2846 BRAILLE PATTERN DOTS-237 
(defconstant +xk-braille-dots-1237+ #x1002847) ;  U+2847 BRAILLE PATTERN DOTS-1237 
(defconstant +xk-braille-dots-47+ #x1002848) ;  U+2848 BRAILLE PATTERN DOTS-47 
(defconstant +xk-braille-dots-147+ #x1002849) ;  U+2849 BRAILLE PATTERN DOTS-147 
(defconstant +xk-braille-dots-247+ #x100284a) ;  U+284a BRAILLE PATTERN DOTS-247 
(defconstant +xk-braille-dots-1247+ #x100284b) ;  U+284b BRAILLE PATTERN DOTS-1247 
(defconstant +xk-braille-dots-347+ #x100284c) ;  U+284c BRAILLE PATTERN DOTS-347 
(defconstant +xk-braille-dots-1347+ #x100284d) ;  U+284d BRAILLE PATTERN DOTS-1347 
(defconstant +xk-braille-dots-2347+ #x100284e) ;  U+284e BRAILLE PATTERN DOTS-2347 
(defconstant +xk-braille-dots-12347+ #x100284f) ;  U+284f BRAILLE PATTERN DOTS-12347 
(defconstant +xk-braille-dots-57+ #x1002850) ;  U+2850 BRAILLE PATTERN DOTS-57 
(defconstant +xk-braille-dots-157+ #x1002851) ;  U+2851 BRAILLE PATTERN DOTS-157 
(defconstant +xk-braille-dots-257+ #x1002852) ;  U+2852 BRAILLE PATTERN DOTS-257 
(defconstant +xk-braille-dots-1257+ #x1002853) ;  U+2853 BRAILLE PATTERN DOTS-1257 
(defconstant +xk-braille-dots-357+ #x1002854) ;  U+2854 BRAILLE PATTERN DOTS-357 
(defconstant +xk-braille-dots-1357+ #x1002855) ;  U+2855 BRAILLE PATTERN DOTS-1357 
(defconstant +xk-braille-dots-2357+ #x1002856) ;  U+2856 BRAILLE PATTERN DOTS-2357 
(defconstant +xk-braille-dots-12357+ #x1002857) ;  U+2857 BRAILLE PATTERN DOTS-12357 
(defconstant +xk-braille-dots-457+ #x1002858) ;  U+2858 BRAILLE PATTERN DOTS-457 
(defconstant +xk-braille-dots-1457+ #x1002859) ;  U+2859 BRAILLE PATTERN DOTS-1457 
(defconstant +xk-braille-dots-2457+ #x100285a) ;  U+285a BRAILLE PATTERN DOTS-2457 
(defconstant +xk-braille-dots-12457+ #x100285b) ;  U+285b BRAILLE PATTERN DOTS-12457 
(defconstant +xk-braille-dots-3457+ #x100285c) ;  U+285c BRAILLE PATTERN DOTS-3457 
(defconstant +xk-braille-dots-13457+ #x100285d) ;  U+285d BRAILLE PATTERN DOTS-13457 
(defconstant +xk-braille-dots-23457+ #x100285e) ;  U+285e BRAILLE PATTERN DOTS-23457 
(defconstant +xk-braille-dots-123457+ #x100285f) ;  U+285f BRAILLE PATTERN DOTS-123457 
(defconstant +xk-braille-dots-67+ #x1002860) ;  U+2860 BRAILLE PATTERN DOTS-67 
(defconstant +xk-braille-dots-167+ #x1002861) ;  U+2861 BRAILLE PATTERN DOTS-167 
(defconstant +xk-braille-dots-267+ #x1002862) ;  U+2862 BRAILLE PATTERN DOTS-267 
(defconstant +xk-braille-dots-1267+ #x1002863) ;  U+2863 BRAILLE PATTERN DOTS-1267 
(defconstant +xk-braille-dots-367+ #x1002864) ;  U+2864 BRAILLE PATTERN DOTS-367 
(defconstant +xk-braille-dots-1367+ #x1002865) ;  U+2865 BRAILLE PATTERN DOTS-1367 
(defconstant +xk-braille-dots-2367+ #x1002866) ;  U+2866 BRAILLE PATTERN DOTS-2367 
(defconstant +xk-braille-dots-12367+ #x1002867) ;  U+2867 BRAILLE PATTERN DOTS-12367 
(defconstant +xk-braille-dots-467+ #x1002868) ;  U+2868 BRAILLE PATTERN DOTS-467 
(defconstant +xk-braille-dots-1467+ #x1002869) ;  U+2869 BRAILLE PATTERN DOTS-1467 
(defconstant +xk-braille-dots-2467+ #x100286a) ;  U+286a BRAILLE PATTERN DOTS-2467 
(defconstant +xk-braille-dots-12467+ #x100286b) ;  U+286b BRAILLE PATTERN DOTS-12467 
(defconstant +xk-braille-dots-3467+ #x100286c) ;  U+286c BRAILLE PATTERN DOTS-3467 
(defconstant +xk-braille-dots-13467+ #x100286d) ;  U+286d BRAILLE PATTERN DOTS-13467 
(defconstant +xk-braille-dots-23467+ #x100286e) ;  U+286e BRAILLE PATTERN DOTS-23467 
(defconstant +xk-braille-dots-123467+ #x100286f) ;  U+286f BRAILLE PATTERN DOTS-123467 
(defconstant +xk-braille-dots-567+ #x1002870) ;  U+2870 BRAILLE PATTERN DOTS-567 
(defconstant +xk-braille-dots-1567+ #x1002871) ;  U+2871 BRAILLE PATTERN DOTS-1567 
(defconstant +xk-braille-dots-2567+ #x1002872) ;  U+2872 BRAILLE PATTERN DOTS-2567 
(defconstant +xk-braille-dots-12567+ #x1002873) ;  U+2873 BRAILLE PATTERN DOTS-12567 
(defconstant +xk-braille-dots-3567+ #x1002874) ;  U+2874 BRAILLE PATTERN DOTS-3567 
(defconstant +xk-braille-dots-13567+ #x1002875) ;  U+2875 BRAILLE PATTERN DOTS-13567 
(defconstant +xk-braille-dots-23567+ #x1002876) ;  U+2876 BRAILLE PATTERN DOTS-23567 
(defconstant +xk-braille-dots-123567+ #x1002877) ;  U+2877 BRAILLE PATTERN DOTS-123567 
(defconstant +xk-braille-dots-4567+ #x1002878) ;  U+2878 BRAILLE PATTERN DOTS-4567 
(defconstant +xk-braille-dots-14567+ #x1002879) ;  U+2879 BRAILLE PATTERN DOTS-14567 
(defconstant +xk-braille-dots-24567+ #x100287a) ;  U+287a BRAILLE PATTERN DOTS-24567 
(defconstant +xk-braille-dots-124567+ #x100287b) ;  U+287b BRAILLE PATTERN DOTS-124567 
(defconstant +xk-braille-dots-34567+ #x100287c) ;  U+287c BRAILLE PATTERN DOTS-34567 
(defconstant +xk-braille-dots-134567+ #x100287d) ;  U+287d BRAILLE PATTERN DOTS-134567 
(defconstant +xk-braille-dots-234567+ #x100287e) ;  U+287e BRAILLE PATTERN DOTS-234567 
(defconstant +xk-braille-dots-1234567+ #x100287f) ;  U+287f BRAILLE PATTERN DOTS-1234567 
(defconstant +xk-braille-dots-8+ #x1002880) ;  U+2880 BRAILLE PATTERN DOTS-8 
(defconstant +xk-braille-dots-18+ #x1002881) ;  U+2881 BRAILLE PATTERN DOTS-18 
(defconstant +xk-braille-dots-28+ #x1002882) ;  U+2882 BRAILLE PATTERN DOTS-28 
(defconstant +xk-braille-dots-128+ #x1002883) ;  U+2883 BRAILLE PATTERN DOTS-128 
(defconstant +xk-braille-dots-38+ #x1002884) ;  U+2884 BRAILLE PATTERN DOTS-38 
(defconstant +xk-braille-dots-138+ #x1002885) ;  U+2885 BRAILLE PATTERN DOTS-138 
(defconstant +xk-braille-dots-238+ #x1002886) ;  U+2886 BRAILLE PATTERN DOTS-238 
(defconstant +xk-braille-dots-1238+ #x1002887) ;  U+2887 BRAILLE PATTERN DOTS-1238 
(defconstant +xk-braille-dots-48+ #x1002888) ;  U+2888 BRAILLE PATTERN DOTS-48 
(defconstant +xk-braille-dots-148+ #x1002889) ;  U+2889 BRAILLE PATTERN DOTS-148 
(defconstant +xk-braille-dots-248+ #x100288a) ;  U+288a BRAILLE PATTERN DOTS-248 
(defconstant +xk-braille-dots-1248+ #x100288b) ;  U+288b BRAILLE PATTERN DOTS-1248 
(defconstant +xk-braille-dots-348+ #x100288c) ;  U+288c BRAILLE PATTERN DOTS-348 
(defconstant +xk-braille-dots-1348+ #x100288d) ;  U+288d BRAILLE PATTERN DOTS-1348 
(defconstant +xk-braille-dots-2348+ #x100288e) ;  U+288e BRAILLE PATTERN DOTS-2348 
(defconstant +xk-braille-dots-12348+ #x100288f) ;  U+288f BRAILLE PATTERN DOTS-12348 
(defconstant +xk-braille-dots-58+ #x1002890) ;  U+2890 BRAILLE PATTERN DOTS-58 
(defconstant +xk-braille-dots-158+ #x1002891) ;  U+2891 BRAILLE PATTERN DOTS-158 
(defconstant +xk-braille-dots-258+ #x1002892) ;  U+2892 BRAILLE PATTERN DOTS-258 
(defconstant +xk-braille-dots-1258+ #x1002893) ;  U+2893 BRAILLE PATTERN DOTS-1258 
(defconstant +xk-braille-dots-358+ #x1002894) ;  U+2894 BRAILLE PATTERN DOTS-358 
(defconstant +xk-braille-dots-1358+ #x1002895) ;  U+2895 BRAILLE PATTERN DOTS-1358 
(defconstant +xk-braille-dots-2358+ #x1002896) ;  U+2896 BRAILLE PATTERN DOTS-2358 
(defconstant +xk-braille-dots-12358+ #x1002897) ;  U+2897 BRAILLE PATTERN DOTS-12358 
(defconstant +xk-braille-dots-458+ #x1002898) ;  U+2898 BRAILLE PATTERN DOTS-458 
(defconstant +xk-braille-dots-1458+ #x1002899) ;  U+2899 BRAILLE PATTERN DOTS-1458 
(defconstant +xk-braille-dots-2458+ #x100289a) ;  U+289a BRAILLE PATTERN DOTS-2458 
(defconstant +xk-braille-dots-12458+ #x100289b) ;  U+289b BRAILLE PATTERN DOTS-12458 
(defconstant +xk-braille-dots-3458+ #x100289c) ;  U+289c BRAILLE PATTERN DOTS-3458 
(defconstant +xk-braille-dots-13458+ #x100289d) ;  U+289d BRAILLE PATTERN DOTS-13458 
(defconstant +xk-braille-dots-23458+ #x100289e) ;  U+289e BRAILLE PATTERN DOTS-23458 
(defconstant +xk-braille-dots-123458+ #x100289f) ;  U+289f BRAILLE PATTERN DOTS-123458 
(defconstant +xk-braille-dots-68+ #x10028a0) ;  U+28a0 BRAILLE PATTERN DOTS-68 
(defconstant +xk-braille-dots-168+ #x10028a1) ;  U+28a1 BRAILLE PATTERN DOTS-168 
(defconstant +xk-braille-dots-268+ #x10028a2) ;  U+28a2 BRAILLE PATTERN DOTS-268 
(defconstant +xk-braille-dots-1268+ #x10028a3) ;  U+28a3 BRAILLE PATTERN DOTS-1268 
(defconstant +xk-braille-dots-368+ #x10028a4) ;  U+28a4 BRAILLE PATTERN DOTS-368 
(defconstant +xk-braille-dots-1368+ #x10028a5) ;  U+28a5 BRAILLE PATTERN DOTS-1368 
(defconstant +xk-braille-dots-2368+ #x10028a6) ;  U+28a6 BRAILLE PATTERN DOTS-2368 
(defconstant +xk-braille-dots-12368+ #x10028a7) ;  U+28a7 BRAILLE PATTERN DOTS-12368 
(defconstant +xk-braille-dots-468+ #x10028a8) ;  U+28a8 BRAILLE PATTERN DOTS-468 
(defconstant +xk-braille-dots-1468+ #x10028a9) ;  U+28a9 BRAILLE PATTERN DOTS-1468 
(defconstant +xk-braille-dots-2468+ #x10028aa) ;  U+28aa BRAILLE PATTERN DOTS-2468 
(defconstant +xk-braille-dots-12468+ #x10028ab) ;  U+28ab BRAILLE PATTERN DOTS-12468 
(defconstant +xk-braille-dots-3468+ #x10028ac) ;  U+28ac BRAILLE PATTERN DOTS-3468 
(defconstant +xk-braille-dots-13468+ #x10028ad) ;  U+28ad BRAILLE PATTERN DOTS-13468 
(defconstant +xk-braille-dots-23468+ #x10028ae) ;  U+28ae BRAILLE PATTERN DOTS-23468 
(defconstant +xk-braille-dots-123468+ #x10028af) ;  U+28af BRAILLE PATTERN DOTS-123468 
(defconstant +xk-braille-dots-568+ #x10028b0) ;  U+28b0 BRAILLE PATTERN DOTS-568 
(defconstant +xk-braille-dots-1568+ #x10028b1) ;  U+28b1 BRAILLE PATTERN DOTS-1568 
(defconstant +xk-braille-dots-2568+ #x10028b2) ;  U+28b2 BRAILLE PATTERN DOTS-2568 
(defconstant +xk-braille-dots-12568+ #x10028b3) ;  U+28b3 BRAILLE PATTERN DOTS-12568 
(defconstant +xk-braille-dots-3568+ #x10028b4) ;  U+28b4 BRAILLE PATTERN DOTS-3568 
(defconstant +xk-braille-dots-13568+ #x10028b5) ;  U+28b5 BRAILLE PATTERN DOTS-13568 
(defconstant +xk-braille-dots-23568+ #x10028b6) ;  U+28b6 BRAILLE PATTERN DOTS-23568 
(defconstant +xk-braille-dots-123568+ #x10028b7) ;  U+28b7 BRAILLE PATTERN DOTS-123568 
(defconstant +xk-braille-dots-4568+ #x10028b8) ;  U+28b8 BRAILLE PATTERN DOTS-4568 
(defconstant +xk-braille-dots-14568+ #x10028b9) ;  U+28b9 BRAILLE PATTERN DOTS-14568 
(defconstant +xk-braille-dots-24568+ #x10028ba) ;  U+28ba BRAILLE PATTERN DOTS-24568 
(defconstant +xk-braille-dots-124568+ #x10028bb) ;  U+28bb BRAILLE PATTERN DOTS-124568 
(defconstant +xk-braille-dots-34568+ #x10028bc) ;  U+28bc BRAILLE PATTERN DOTS-34568 
(defconstant +xk-braille-dots-134568+ #x10028bd) ;  U+28bd BRAILLE PATTERN DOTS-134568 
(defconstant +xk-braille-dots-234568+ #x10028be) ;  U+28be BRAILLE PATTERN DOTS-234568 
(defconstant +xk-braille-dots-1234568+ #x10028bf) ;  U+28bf BRAILLE PATTERN DOTS-1234568 
(defconstant +xk-braille-dots-78+ #x10028c0) ;  U+28c0 BRAILLE PATTERN DOTS-78 
(defconstant +xk-braille-dots-178+ #x10028c1) ;  U+28c1 BRAILLE PATTERN DOTS-178 
(defconstant +xk-braille-dots-278+ #x10028c2) ;  U+28c2 BRAILLE PATTERN DOTS-278 
(defconstant +xk-braille-dots-1278+ #x10028c3) ;  U+28c3 BRAILLE PATTERN DOTS-1278 
(defconstant +xk-braille-dots-378+ #x10028c4) ;  U+28c4 BRAILLE PATTERN DOTS-378 
(defconstant +xk-braille-dots-1378+ #x10028c5) ;  U+28c5 BRAILLE PATTERN DOTS-1378 
(defconstant +xk-braille-dots-2378+ #x10028c6) ;  U+28c6 BRAILLE PATTERN DOTS-2378 
(defconstant +xk-braille-dots-12378+ #x10028c7) ;  U+28c7 BRAILLE PATTERN DOTS-12378 
(defconstant +xk-braille-dots-478+ #x10028c8) ;  U+28c8 BRAILLE PATTERN DOTS-478 
(defconstant +xk-braille-dots-1478+ #x10028c9) ;  U+28c9 BRAILLE PATTERN DOTS-1478 
(defconstant +xk-braille-dots-2478+ #x10028ca) ;  U+28ca BRAILLE PATTERN DOTS-2478 
(defconstant +xk-braille-dots-12478+ #x10028cb) ;  U+28cb BRAILLE PATTERN DOTS-12478 
(defconstant +xk-braille-dots-3478+ #x10028cc) ;  U+28cc BRAILLE PATTERN DOTS-3478 
(defconstant +xk-braille-dots-13478+ #x10028cd) ;  U+28cd BRAILLE PATTERN DOTS-13478 
(defconstant +xk-braille-dots-23478+ #x10028ce) ;  U+28ce BRAILLE PATTERN DOTS-23478 
(defconstant +xk-braille-dots-123478+ #x10028cf) ;  U+28cf BRAILLE PATTERN DOTS-123478 
(defconstant +xk-braille-dots-578+ #x10028d0) ;  U+28d0 BRAILLE PATTERN DOTS-578 
(defconstant +xk-braille-dots-1578+ #x10028d1) ;  U+28d1 BRAILLE PATTERN DOTS-1578 
(defconstant +xk-braille-dots-2578+ #x10028d2) ;  U+28d2 BRAILLE PATTERN DOTS-2578 
(defconstant +xk-braille-dots-12578+ #x10028d3) ;  U+28d3 BRAILLE PATTERN DOTS-12578 
(defconstant +xk-braille-dots-3578+ #x10028d4) ;  U+28d4 BRAILLE PATTERN DOTS-3578 
(defconstant +xk-braille-dots-13578+ #x10028d5) ;  U+28d5 BRAILLE PATTERN DOTS-13578 
(defconstant +xk-braille-dots-23578+ #x10028d6) ;  U+28d6 BRAILLE PATTERN DOTS-23578 
(defconstant +xk-braille-dots-123578+ #x10028d7) ;  U+28d7 BRAILLE PATTERN DOTS-123578 
(defconstant +xk-braille-dots-4578+ #x10028d8) ;  U+28d8 BRAILLE PATTERN DOTS-4578 
(defconstant +xk-braille-dots-14578+ #x10028d9) ;  U+28d9 BRAILLE PATTERN DOTS-14578 
(defconstant +xk-braille-dots-24578+ #x10028da) ;  U+28da BRAILLE PATTERN DOTS-24578 
(defconstant +xk-braille-dots-124578+ #x10028db) ;  U+28db BRAILLE PATTERN DOTS-124578 
(defconstant +xk-braille-dots-34578+ #x10028dc) ;  U+28dc BRAILLE PATTERN DOTS-34578 
(defconstant +xk-braille-dots-134578+ #x10028dd) ;  U+28dd BRAILLE PATTERN DOTS-134578 
(defconstant +xk-braille-dots-234578+ #x10028de) ;  U+28de BRAILLE PATTERN DOTS-234578 
(defconstant +xk-braille-dots-1234578+ #x10028df) ;  U+28df BRAILLE PATTERN DOTS-1234578 
(defconstant +xk-braille-dots-678+ #x10028e0) ;  U+28e0 BRAILLE PATTERN DOTS-678 
(defconstant +xk-braille-dots-1678+ #x10028e1) ;  U+28e1 BRAILLE PATTERN DOTS-1678 
(defconstant +xk-braille-dots-2678+ #x10028e2) ;  U+28e2 BRAILLE PATTERN DOTS-2678 
(defconstant +xk-braille-dots-12678+ #x10028e3) ;  U+28e3 BRAILLE PATTERN DOTS-12678 
(defconstant +xk-braille-dots-3678+ #x10028e4) ;  U+28e4 BRAILLE PATTERN DOTS-3678 
(defconstant +xk-braille-dots-13678+ #x10028e5) ;  U+28e5 BRAILLE PATTERN DOTS-13678 
(defconstant +xk-braille-dots-23678+ #x10028e6) ;  U+28e6 BRAILLE PATTERN DOTS-23678 
(defconstant +xk-braille-dots-123678+ #x10028e7) ;  U+28e7 BRAILLE PATTERN DOTS-123678 
(defconstant +xk-braille-dots-4678+ #x10028e8) ;  U+28e8 BRAILLE PATTERN DOTS-4678 
(defconstant +xk-braille-dots-14678+ #x10028e9) ;  U+28e9 BRAILLE PATTERN DOTS-14678 
(defconstant +xk-braille-dots-24678+ #x10028ea) ;  U+28ea BRAILLE PATTERN DOTS-24678 
(defconstant +xk-braille-dots-124678+ #x10028eb) ;  U+28eb BRAILLE PATTERN DOTS-124678 
(defconstant +xk-braille-dots-34678+ #x10028ec) ;  U+28ec BRAILLE PATTERN DOTS-34678 
(defconstant +xk-braille-dots-134678+ #x10028ed) ;  U+28ed BRAILLE PATTERN DOTS-134678 
(defconstant +xk-braille-dots-234678+ #x10028ee) ;  U+28ee BRAILLE PATTERN DOTS-234678 
(defconstant +xk-braille-dots-1234678+ #x10028ef) ;  U+28ef BRAILLE PATTERN DOTS-1234678 
(defconstant +xk-braille-dots-5678+ #x10028f0) ;  U+28f0 BRAILLE PATTERN DOTS-5678 
(defconstant +xk-braille-dots-15678+ #x10028f1) ;  U+28f1 BRAILLE PATTERN DOTS-15678 
(defconstant +xk-braille-dots-25678+ #x10028f2) ;  U+28f2 BRAILLE PATTERN DOTS-25678 
(defconstant +xk-braille-dots-125678+ #x10028f3) ;  U+28f3 BRAILLE PATTERN DOTS-125678 
(defconstant +xk-braille-dots-35678+ #x10028f4) ;  U+28f4 BRAILLE PATTERN DOTS-35678 
(defconstant +xk-braille-dots-135678+ #x10028f5) ;  U+28f5 BRAILLE PATTERN DOTS-135678 
(defconstant +xk-braille-dots-235678+ #x10028f6) ;  U+28f6 BRAILLE PATTERN DOTS-235678 
(defconstant +xk-braille-dots-1235678+ #x10028f7) ;  U+28f7 BRAILLE PATTERN DOTS-1235678 
(defconstant +xk-braille-dots-45678+ #x10028f8) ;  U+28f8 BRAILLE PATTERN DOTS-45678 
(defconstant +xk-braille-dots-145678+ #x10028f9) ;  U+28f9 BRAILLE PATTERN DOTS-145678 
(defconstant +xk-braille-dots-245678+ #x10028fa) ;  U+28fa BRAILLE PATTERN DOTS-245678 
(defconstant +xk-braille-dots-1245678+ #x10028fb) ;  U+28fb BRAILLE PATTERN DOTS-1245678 
(defconstant +xk-braille-dots-345678+ #x10028fc) ;  U+28fc BRAILLE PATTERN DOTS-345678 
(defconstant +xk-braille-dots-1345678+ #x10028fd) ;  U+28fd BRAILLE PATTERN DOTS-1345678 
(defconstant +xk-braille-dots-2345678+ #x10028fe) ;  U+28fe BRAILLE PATTERN DOTS-2345678 
(defconstant +xk-braille-dots-12345678+ #x10028ff) ;  U+28ff BRAILLE PATTERN DOTS-12345678 
;;; #endif /* XK_BRAILLE */

;;; /*
;;;  * Sinhala (http://unicode.org/charts/PDF/U0D80.pdf)
;;;  * http://www.nongnu.org/sinhala/doc/transliteration/sinhala-transliteration_6.html
;;;  */

;;; #ifdef XK_SINHALA
(defconstant +xk-sinh-ng+ #x1000d82) ;  U+0D82 SINHALA ANUSVARAYA 
(defconstant +xk-sinh-h2+ #x1000d83) ;  U+0D83 SINHALA VISARGAYA 
(defconstant +xk-sinh-a+ #x1000d85) ;  U+0D85 SINHALA AYANNA 
(defconstant +xk-sinh-aa+ #x1000d86) ;  U+0D86 SINHALA AAYANNA 
(defconstant +xk-sinh-ae+ #x1000d87) ;  U+0D87 SINHALA AEYANNA 
(defconstant +xk-sinh-aee+ #x1000d88) ;  U+0D88 SINHALA AEEYANNA 
(defconstant +xk-sinh-i+ #x1000d89) ;  U+0D89 SINHALA IYANNA 
(defconstant +xk-sinh-ii+ #x1000d8a) ;  U+0D8A SINHALA IIYANNA 
(defconstant +xk-sinh-u+ #x1000d8b) ;  U+0D8B SINHALA UYANNA 
(defconstant +xk-sinh-uu+ #x1000d8c) ;  U+0D8C SINHALA UUYANNA 
(defconstant +xk-sinh-ri+ #x1000d8d) ;  U+0D8D SINHALA IRUYANNA 
(defconstant +xk-sinh-rii+ #x1000d8e) ;  U+0D8E SINHALA IRUUYANNA 
(defconstant +xk-sinh-lu+ #x1000d8f) ;  U+0D8F SINHALA ILUYANNA 
(defconstant +xk-sinh-luu+ #x1000d90) ;  U+0D90 SINHALA ILUUYANNA 
(defconstant +xk-sinh-e+ #x1000d91) ;  U+0D91 SINHALA EYANNA 
(defconstant +xk-sinh-ee+ #x1000d92) ;  U+0D92 SINHALA EEYANNA 
(defconstant +xk-sinh-ai+ #x1000d93) ;  U+0D93 SINHALA AIYANNA 
(defconstant +xk-sinh-o+ #x1000d94) ;  U+0D94 SINHALA OYANNA 
(defconstant +xk-sinh-oo+ #x1000d95) ;  U+0D95 SINHALA OOYANNA 
(defconstant +xk-sinh-au+ #x1000d96) ;  U+0D96 SINHALA AUYANNA 
(defconstant +xk-sinh-ka+ #x1000d9a) ;  U+0D9A SINHALA KAYANNA 
(defconstant +xk-sinh-kha+ #x1000d9b) ;  U+0D9B SINHALA MAHA. KAYANNA 
(defconstant +xk-sinh-ga+ #x1000d9c) ;  U+0D9C SINHALA GAYANNA 
(defconstant +xk-sinh-gha+ #x1000d9d) ;  U+0D9D SINHALA MAHA. GAYANNA 
(defconstant +xk-sinh-ng2+ #x1000d9e) ;  U+0D9E SINHALA KANTAJA NAASIKYAYA 
(defconstant +xk-sinh-nga+ #x1000d9f) ;  U+0D9F SINHALA SANYAKA GAYANNA 
(defconstant +xk-sinh-ca+ #x1000da0) ;  U+0DA0 SINHALA CAYANNA 
(defconstant +xk-sinh-cha+ #x1000da1) ;  U+0DA1 SINHALA MAHA. CAYANNA 
(defconstant +xk-sinh-ja+ #x1000da2) ;  U+0DA2 SINHALA JAYANNA 
(defconstant +xk-sinh-jha+ #x1000da3) ;  U+0DA3 SINHALA MAHA. JAYANNA 
(defconstant +xk-sinh-nya+ #x1000da4) ;  U+0DA4 SINHALA TAALUJA NAASIKYAYA 
(defconstant +xk-sinh-jnya+ #x1000da5) ;  U+0DA5 SINHALA TAALUJA SANYOOGA NAASIKYAYA 
(defconstant +xk-sinh-nja+ #x1000da6) ;  U+0DA6 SINHALA SANYAKA JAYANNA 
(defconstant +xk-sinh-tta+ #x1000da7) ;  U+0DA7 SINHALA TTAYANNA 
(defconstant +xk-sinh-ttha+ #x1000da8) ;  U+0DA8 SINHALA MAHA. TTAYANNA 
(defconstant +xk-sinh-dda+ #x1000da9) ;  U+0DA9 SINHALA DDAYANNA 
(defconstant +xk-sinh-ddha+ #x1000daa) ;  U+0DAA SINHALA MAHA. DDAYANNA 
(defconstant +xk-sinh-nna+ #x1000dab) ;  U+0DAB SINHALA MUURDHAJA NAYANNA 
(defconstant +xk-sinh-ndda+ #x1000dac) ;  U+0DAC SINHALA SANYAKA DDAYANNA 
(defconstant +xk-sinh-tha+ #x1000dad) ;  U+0DAD SINHALA TAYANNA 
(defconstant +xk-sinh-thha+ #x1000dae) ;  U+0DAE SINHALA MAHA. TAYANNA 
(defconstant +xk-sinh-dha+ #x1000daf) ;  U+0DAF SINHALA DAYANNA 
(defconstant +xk-sinh-dhha+ #x1000db0) ;  U+0DB0 SINHALA MAHA. DAYANNA 
(defconstant +xk-sinh-na+ #x1000db1) ;  U+0DB1 SINHALA DANTAJA NAYANNA 
(defconstant +xk-sinh-ndha+ #x1000db3) ;  U+0DB3 SINHALA SANYAKA DAYANNA 
(defconstant +xk-sinh-pa+ #x1000db4) ;  U+0DB4 SINHALA PAYANNA 
(defconstant +xk-sinh-pha+ #x1000db5) ;  U+0DB5 SINHALA MAHA. PAYANNA 
(defconstant +xk-sinh-ba+ #x1000db6) ;  U+0DB6 SINHALA BAYANNA 
(defconstant +xk-sinh-bha+ #x1000db7) ;  U+0DB7 SINHALA MAHA. BAYANNA 
(defconstant +xk-sinh-ma+ #x1000db8) ;  U+0DB8 SINHALA MAYANNA 
(defconstant +xk-sinh-mba+ #x1000db9) ;  U+0DB9 SINHALA AMBA BAYANNA 
(defconstant +xk-sinh-ya+ #x1000dba) ;  U+0DBA SINHALA YAYANNA 
(defconstant +xk-sinh-ra+ #x1000dbb) ;  U+0DBB SINHALA RAYANNA 
(defconstant +xk-sinh-la+ #x1000dbd) ;  U+0DBD SINHALA DANTAJA LAYANNA 
(defconstant +xk-sinh-va+ #x1000dc0) ;  U+0DC0 SINHALA VAYANNA 
(defconstant +xk-sinh-sha+ #x1000dc1) ;  U+0DC1 SINHALA TAALUJA SAYANNA 
(defconstant +xk-sinh-ssha+ #x1000dc2) ;  U+0DC2 SINHALA MUURDHAJA SAYANNA 
(defconstant +xk-sinh-sa+ #x1000dc3) ;  U+0DC3 SINHALA DANTAJA SAYANNA 
(defconstant +xk-sinh-ha+ #x1000dc4) ;  U+0DC4 SINHALA HAYANNA 
(defconstant +xk-sinh-lla+ #x1000dc5) ;  U+0DC5 SINHALA MUURDHAJA LAYANNA 
(defconstant +xk-sinh-fa+ #x1000dc6) ;  U+0DC6 SINHALA FAYANNA 
(defconstant +xk-sinh-al+ #x1000dca) ;  U+0DCA SINHALA AL-LAKUNA 
(defconstant +xk-sinh-aa2+ #x1000dcf) ;  U+0DCF SINHALA AELA-PILLA 
(defconstant +xk-sinh-ae2+ #x1000dd0) ;  U+0DD0 SINHALA AEDA-PILLA 
(defconstant +xk-sinh-aee2+ #x1000dd1) ;  U+0DD1 SINHALA DIGA AEDA-PILLA 
(defconstant +xk-sinh-i2+ #x1000dd2) ;  U+0DD2 SINHALA IS-PILLA 
(defconstant +xk-sinh-ii2+ #x1000dd3) ;  U+0DD3 SINHALA DIGA IS-PILLA 
(defconstant +xk-sinh-u2+ #x1000dd4) ;  U+0DD4 SINHALA PAA-PILLA 
(defconstant +xk-sinh-uu2+ #x1000dd6) ;  U+0DD6 SINHALA DIGA PAA-PILLA 
(defconstant +xk-sinh-ru2+ #x1000dd8) ;  U+0DD8 SINHALA GAETTA-PILLA 
(defconstant +xk-sinh-e2+ #x1000dd9) ;  U+0DD9 SINHALA KOMBUVA 
(defconstant +xk-sinh-ee2+ #x1000dda) ;  U+0DDA SINHALA DIGA KOMBUVA 
(defconstant +xk-sinh-ai2+ #x1000ddb) ;  U+0DDB SINHALA KOMBU DEKA 
(defconstant +xk-sinh-o2+ #x1000ddc) ;  U+0DDC SINHALA KOMBUVA HAA AELA-PILLA
(defconstant +xk-sinh-oo2+ #x1000ddd) ;  U+0DDD SINHALA KOMBUVA HAA DIGA AELA-PILLA
(defconstant +xk-sinh-au2+ #x1000dde) ;  U+0DDE SINHALA KOMBUVA HAA GAYANUKITTA 
(defconstant +xk-sinh-lu2+ #x1000ddf) ;  U+0DDF SINHALA GAYANUKITTA 
(defconstant +xk-sinh-ruu2+ #x1000df2) ;  U+0DF2 SINHALA DIGA GAETTA-PILLA 
(defconstant +xk-sinh-luu2+ #x1000df3) ;  U+0DF3 SINHALA DIGA GAYANUKITTA 
(defconstant +xk-sinh-kunddaliya+ #x1000df4) ;  U+0DF4 SINHALA KUNDDALIYA 
;;; #endif /* XK_SINHALA */
