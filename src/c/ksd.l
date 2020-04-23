#{
  Copyright 1987, 1994, 1998  The Open Group

  Permission to use, copy, modify, distribute, and sell this software and its
  documentation for any purpose is hereby granted without fee, provided that
  the above copyright notice appear in all copies and that both that
  copyright notice and this permission notice appear in supporting
  documentation.

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
  IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.

  Except as contained in this notice, the name of The Open Group shall
  not be used in advertising or otherwise to promote the sale, use or
  other dealings in this Software without prior written authorization
  from The Open Group.


  Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts

                          All Rights Reserved

  Permission to use, copy, modify, and distribute this software and its
  documentation for any purpose and without fee is hereby granted,
  provided that the above copyright notice appear in all copies and that
  both that copyright notice and this permission notice appear in
  supporting documentation, and that the name of Digital not be
  used in advertising or publicity pertaining to distribution of the
  software without specific, written prior permission.

  DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
  DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
  ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
  SOFTWARE.
}#


#{
  The "X11 Window System Protocol" standard defines in Appendix A the
  keysym codes. These 29-bit integer values identify characters or
  functions associated with each key (e.g., via the visible
  engraving) of a keyboard layout. This file assigns mnemonic macro
  names for these keysyms.
 
  This file is also compiled (by src/util/makekeys.c in libX11) into
  hash tables that can be accessed with X11 library functions such as
  XStringToKeysym() and XKeysymToString().
 
  Where a keysym corresponds one-to-one to an ISO 10646 / Unicode
  character, this is noted in a comment that provides both the U+xxxx
  Unicode position, as well as the official Unicode name of the
  character.
 
  Where the correspondence is either not one-to-one or semantically
  unclear, the Unicode position and name are enclosed in
  parentheses. Such legacy keysyms should be considered deprecated
  and are not recommended for use in future keyboard mappings.
 
  For any future extension of the keysyms with characters already
  found in ISO 10646 / Unicode, the following algorithm shall be
  used. The new keysym code position will simply be the character's
  Unicode number plus 0x01000000. The keysym values in the range
  0x01000100 to 0x0110ffff are reserved to represent Unicode
  characters in the range U+0100 to U+10FFFF.
 
  While most newer Unicode-based X11 clients do already accept
  Unicode-mapped keysyms in the range 0x01000100 to 0x0110ffff, it
  will remain necessary for clients -- in the interest of
  compatibility with existing servers -- to also understand the
  existing legacy keysym values in the range 0x0100 to 0x20ff.
 
  Where several mnemonic names are defined for the same keysym in this
  file, all but the first one listed should be considered deprecated.
 
  Mnemonic names for keysyms are defined in this file with lines
  that match one of these Perl regular expressions:
 
     /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*\/\* U+([0-9A-F]{4,6}) (.*) \*\/\s*$/
     /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*\/\*\(U+([0-9A-F]{4,6}) (.*)\)\*\/\s*$/
     /^\#define XK_([a-zA-Z_0-9]+)\s+0x([0-9a-f]+)\s*(\/\*\s*(.*)\s*\*\/)?\s*$/
 
  Before adding new keysyms, please do consider the following: In
  addition to the keysym names defined in this file, the
  XStringToKeysym() and XKeysymToString() functions will also handle
  any keysym string of the form "U0020" to "U007E" and "U00A0" to
  "U10FFFF" for all possible Unicode characters. In other words,
  every possible Unicode character has already a keysym string
  defined algorithmically, even if it is not listed here. Therefore,
  defining an additional keysym macro is only necessary where a
  non-hexadecimal mnemonic name is needed, or where the new keysym
  does not represent any existing Unicode character.
 
  When adding new keysyms to this file, do not forget to also update the
  following as needed:
 
    - the mappings in src/KeyBind.c in the libX11 repo
      https://gitlab.freedesktop.org/xorg/lib/libx11
 
    - the protocol specification in specs/keysyms.xml in this repo
      https://gitlab.freedesktop.org/xorg/proto/xorgproto
 
}#

(symbols 'ksd 'pico)

(local) (space at e t Tab End m d u less)

(def 'VoidSymbol (hex "ffffff")) # Void symbol

#{
  TTY function keys, cleverly chosen to map to ASCII, for convenience of
  programming, but could have been arbitrary (at the cost of lookup
  tables in client code).
}#

(def 'BackSpace (hex "ff08")) # Back space, back char
(def 'Tab (hex "ff09"))
(def 'Linefeed (hex "ff0a")) # Linefeed, LF
(def 'Clear (hex "ff0b"))
(def 'Return (hex "ff0d")) # Return, enter
(def 'Pause (hex "ff13")) # Pause, hold
(def 'Scroll-Lock (hex "ff14"))
(def 'Sys-Req (hex "ff15"))
(def 'Escape (hex "ff1b"))
(def 'Delete (hex "ffff")) # Delete, rubout



# International & multi-key character composition 

(def 'Multi-key (hex "ff20")) # Multi-key character compose
(def 'Codeinput (hex "ff37"))
(def 'SingleCandidate (hex "ff3c"))
(def 'MultipleCandidate (hex "ff3d"))
(def 'PreviousCandidate (hex "ff3e"))

# Japanese keyboard support 

(def 'Kanji (hex "ff21")) # Kanji, Kanji convert
(def 'Muhenkan (hex "ff22")) # Cancel Conversion
(def 'Henkan-Mode (hex "ff23")) # Start/Stop Conversion
(def 'Henkan (hex "ff23")) # Alias for Henkan_Mode
(def 'Romaji (hex "ff24")) # to Romaji
(def 'Hiragana (hex "ff25")) # to Hiragana
(def 'Katakana (hex "ff26")) # to Katakana
(def 'Hiragana-Katakana (hex "ff27")) # Hiragana/Katakana toggle
(def 'Zenkaku (hex "ff28")) # to Zenkaku
(def 'Hankaku (hex "ff29")) # to Hankaku
(def 'Zenkaku-Hankaku (hex "ff2a")) # Zenkaku/Hankaku toggle
(def 'Touroku (hex "ff2b")) # Add to Dictionary
(def 'Massyo (hex "ff2c")) # Delete from Dictionary
(def 'Kana-Lock (hex "ff2d")) # Kana Lock
(def 'Kana-Shift (hex "ff2e")) # Kana Shift
(def 'Eisu-Shift (hex "ff2f")) # Alphanumeric Shift
(def 'Eisu-toggle (hex "ff30")) # Alphanumeric toggle
(def 'Kanji-Bangou (hex "ff37")) # Codeinput
(def 'Zen-Koho (hex "ff3d")) # Multiple/All Candidate(s)
(def 'Mae-Koho (hex "ff3e")) # Previous Candidate

# 0xff31 thru 0xff3f are under XK_KOREAN 

# Cursor control & motion 

(def 'Home (hex "ff50"))
(def 'Left (hex "ff51")) # Move left, left arrow
(def 'Up (hex "ff52")) # Move up, up arrow
(def 'Right (hex "ff53")) # Move right, right arrow
(def 'Down (hex "ff54")) # Move down, down arrow
(def 'Prior (hex "ff55")) # Prior, previous
(def 'Page-Up (hex "ff55"))
(def 'Next (hex "ff56")) # Next
(def 'Page-Down (hex "ff56"))
(def 'End (hex "ff57")) # EOL
(def 'Begin (hex "ff58")) # BOL


# Misc functions 

(def 'Select (hex "ff60")) # Select, mark
(def 'Print (hex "ff61"))
(def 'Execute (hex "ff62")) # Execute, run, do
(def 'Insert (hex "ff63")) # Insert, insert here
(def 'Undo (hex "ff65"))
(def 'Redo (hex "ff66")) # Redo, again
(def 'Menu (hex "ff67"))
(def 'Find (hex "ff68")) # Find, search
(def 'Cancel (hex "ff69")) # Cancel, stop, abort, exit
(def 'Help (hex "ff6a")) # Help
(def 'Break (hex "ff6b"))
(def 'Mode-switch (hex "ff7e")) # Character set switch
(def 'script-switch (hex "ff7e")) # Alias for mode_switch
(def 'Num-Lock (hex "ff7f"))

# Keypad functions, keypad numbers cleverly chosen to map to ASCII 

(def 'KP-Space (hex "ff80")) # Space
(def 'KP-Tab (hex "ff89"))
(def 'KP-Enter (hex "ff8d")) # Enter
(def 'KP-F1 (hex "ff91")) # PF1, KP_A, ...
(def 'KP-F2 (hex "ff92"))
(def 'KP-F3 (hex "ff93"))
(def 'KP-F4 (hex "ff94"))
(def 'KP-Home (hex "ff95"))
(def 'KP-Left (hex "ff96"))
(def 'KP-Up (hex "ff97"))
(def 'KP-Right (hex "ff98"))
(def 'KP-Down (hex "ff99"))
(def 'KP-Prior (hex "ff9a"))
(def 'KP-Page-Up (hex "ff9a"))
(def 'KP-Next (hex "ff9b"))
(def 'KP-Page-Down (hex "ff9b"))
(def 'KP-End (hex "ff9c"))
(def 'KP-Begin (hex "ff9d"))
(def 'KP-Insert (hex "ff9e"))
(def 'KP-Delete (hex "ff9f"))
(def 'KP-Equal (hex "ffbd")) # Equals
(def 'KP-Multiply (hex "ffaa"))
(def 'KP-Add (hex "ffab"))
(def 'KP-Separator (hex "ffac")) # Separator, often comma
(def 'KP-Subtract (hex "ffad"))
(def 'KP-Decimal (hex "ffae"))
(def 'KP-Divide (hex "ffaf"))

(def 'KP-0 (hex "ffb0"))
(def 'KP-1 (hex "ffb1"))
(def 'KP-2 (hex "ffb2"))
(def 'KP-3 (hex "ffb3"))
(def 'KP-4 (hex "ffb4"))
(def 'KP-5 (hex "ffb5"))
(def 'KP-6 (hex "ffb6"))
(def 'KP-7 (hex "ffb7"))
(def 'KP-8 (hex "ffb8"))
(def 'KP-9 (hex "ffb9"))



#{
  Auxiliary functions; note the duplicate definitions for left and right
  function keys;  Sun keyboards and a few other manufacturers have such
  function key groups on the left and/or right sides of the keyboard.
  We've not found a keyboard with more than 35 function keys total.
}#

(def 'F1 (hex "ffbe"))
(def 'F2 (hex "ffbf"))
(def 'F3 (hex "ffc0"))
(def 'F4 (hex "ffc1"))
(def 'F5 (hex "ffc2"))
(def 'F6 (hex "ffc3"))
(def 'F7 (hex "ffc4"))
(def 'F8 (hex "ffc5"))
(def 'F9 (hex "ffc6"))
(def 'F10 (hex "ffc7"))
(def 'F11 (hex "ffc8"))
(def 'L1 (hex "ffc8"))
(def 'F12 (hex "ffc9"))
(def 'L2 (hex "ffc9"))
(def 'F13 (hex "ffca"))
(def 'L3 (hex "ffca"))
(def 'F14 (hex "ffcb"))
(def 'L4 (hex "ffcb"))
(def 'F15 (hex "ffcc"))
(def 'L5 (hex "ffcc"))
(def 'F16 (hex "ffcd"))
(def 'L6 (hex "ffcd"))
(def 'F17 (hex "ffce"))
(def 'L7 (hex "ffce"))
(def 'F18 (hex "ffcf"))
(def 'L8 (hex "ffcf"))
(def 'F19 (hex "ffd0"))
(def 'L9 (hex "ffd0"))
(def 'F20 (hex "ffd1"))
(def 'L10 (hex "ffd1"))
(def 'F21 (hex "ffd2"))
(def 'R1 (hex "ffd2"))
(def 'F22 (hex "ffd3"))
(def 'R2 (hex "ffd3"))
(def 'F23 (hex "ffd4"))
(def 'R3 (hex "ffd4"))
(def 'F24 (hex "ffd5"))
(def 'R4 (hex "ffd5"))
(def 'F25 (hex "ffd6"))
(def 'R5 (hex "ffd6"))
(def 'F26 (hex "ffd7"))
(def 'R6 (hex "ffd7"))
(def 'F27 (hex "ffd8"))
(def 'R7 (hex "ffd8"))
(def 'F28 (hex "ffd9"))
(def 'R8 (hex "ffd9"))
(def 'F29 (hex "ffda"))
(def 'R9 (hex "ffda"))
(def 'F30 (hex "ffdb"))
(def 'R10 (hex "ffdb"))
(def 'F31 (hex "ffdc"))
(def 'R11 (hex "ffdc"))
(def 'F32 (hex "ffdd"))
(def 'R12 (hex "ffdd"))
(def 'F33 (hex "ffde"))
(def 'R13 (hex "ffde"))
(def 'F34 (hex "ffdf"))
(def 'R14 (hex "ffdf"))
(def 'F35 (hex "ffe0"))
(def 'R15 (hex "ffe0"))

# Modifiers 

(def 'Shift-L (hex "ffe1")) # Left shift
(def 'Shift-R (hex "ffe2")) # Right shift
(def 'Control-L (hex "ffe3")) # Left control
(def 'Control-R (hex "ffe4")) # Right control
(def 'Caps-Lock (hex "ffe5")) # Caps lock
(def 'Shift-Lock (hex "ffe6")) # Shift lock

(def 'Meta-L (hex "ffe7")) # Left meta
(def 'Meta-R (hex "ffe8")) # Right meta
(def 'Alt-L (hex "ffe9")) # Left alt
(def 'Alt-R (hex "ffea")) # Right alt
(def 'Super-L (hex "ffeb")) # Left super
(def 'Super-R (hex "ffec")) # Right super
(def 'Hyper-L (hex "ffed")) # Left hyper
(def 'Hyper-R (hex "ffee")) # Right hyper

#{
  Keyboard (XKB) Extension function and modifier keys
  (from Appendix C of "The X Keyboard Extension: Protocol Specification")
  Byte 3 = 0xfe
}#

(def 'ISO-Lock (hex "fe01"))
(def 'ISO-Level2-Latch (hex "fe02"))
(def 'ISO-Level3-Shift (hex "fe03"))
(def 'ISO-Level3-Latch (hex "fe04"))
(def 'ISO-Level3-Lock (hex "fe05"))
(def 'ISO-Level5-Shift (hex "fe11"))
(def 'ISO-Level5-Latch (hex "fe12"))
(def 'ISO-Level5-Lock (hex "fe13"))
(def 'ISO-Group-Shift (hex "ff7e")) # Alias for mode_switch
(def 'ISO-Group-Latch (hex "fe06"))
(def 'ISO-Group-Lock (hex "fe07"))
(def 'ISO-Next-Group (hex "fe08"))
(def 'ISO-Next-Group-Lock (hex "fe09"))
(def 'ISO-Prev-Group (hex "fe0a"))
(def 'ISO-Prev-Group-Lock (hex "fe0b"))
(def 'ISO-First-Group (hex "fe0c"))
(def 'ISO-First-Group-Lock (hex "fe0d"))
(def 'ISO-Last-Group (hex "fe0e"))
(def 'ISO-Last-Group-Lock (hex "fe0f"))

(def 'ISO-Left-Tab (hex "fe20"))
(def 'ISO-Move-Line-Up (hex "fe21"))
(def 'ISO-Move-Line-Down (hex "fe22"))
(def 'ISO-Partial-Line-Up (hex "fe23"))
(def 'ISO-Partial-Line-Down (hex "fe24"))
(def 'ISO-Partial-Space-Left (hex "fe25"))
(def 'ISO-Partial-Space-Right (hex "fe26"))
(def 'ISO-Set-Margin-Left (hex "fe27"))
(def 'ISO-Set-Margin-Right (hex "fe28"))
(def 'ISO-Release-Margin-Left (hex "fe29"))
(def 'ISO-Release-Margin-Right (hex "fe2a"))
(def 'ISO-Release-Both-Margins (hex "fe2b"))
(def 'ISO-Fast-Cursor-Left (hex "fe2c"))
(def 'ISO-Fast-Cursor-Right (hex "fe2d"))
(def 'ISO-Fast-Cursor-Up (hex "fe2e"))
(def 'ISO-Fast-Cursor-Down (hex "fe2f"))
(def 'ISO-Continuous-Underline (hex "fe30"))
(def 'ISO-Discontinuous-Underline (hex "fe31"))
(def 'ISO-Emphasize (hex "fe32"))
(def 'ISO-Center-Object (hex "fe33"))
(def 'ISO-Enter (hex "fe34"))

(def 'dead-grave (hex "fe50"))
(def 'dead-acute (hex "fe51"))
(def 'dead-circumflex (hex "fe52"))
(def 'dead-tilde (hex "fe53"))
(def 'dead-perispomeni (hex "fe53")) # alias for dead_tilde
(def 'dead-macron (hex "fe54"))
(def 'dead-breve (hex "fe55"))
(def 'dead-abovedot (hex "fe56"))
(def 'dead-diaeresis (hex "fe57"))
(def 'dead-abovering (hex "fe58"))
(def 'dead-doubleacute (hex "fe59"))
(def 'dead-caron (hex "fe5a"))
(def 'dead-cedilla (hex "fe5b"))
(def 'dead-ogonek (hex "fe5c"))
(def 'dead-iota (hex "fe5d"))
(def 'dead-voiced-sound (hex "fe5e"))
(def 'dead-semivoiced-sound (hex "fe5f"))
(def 'dead-belowdot (hex "fe60"))
(def 'dead-hook (hex "fe61"))
(def 'dead-horn (hex "fe62"))
(def 'dead-stroke (hex "fe63"))
(def 'dead-abovecomma (hex "fe64"))
(def 'dead-psili (hex "fe64")) # alias for dead_abovecomma
(def 'dead-abovereversedcomma (hex "fe65"))
(def 'dead-dasia (hex "fe65")) # alias for dead_abovereversedcomma
(def 'dead-doublegrave (hex "fe66"))
(def 'dead-belowring (hex "fe67"))
(def 'dead-belowmacron (hex "fe68"))
(def 'dead-belowcircumflex (hex "fe69"))
(def 'dead-belowtilde (hex "fe6a"))
(def 'dead-belowbreve (hex "fe6b"))
(def 'dead-belowdiaeresis (hex "fe6c"))
(def 'dead-invertedbreve (hex "fe6d"))
(def 'dead-belowcomma (hex "fe6e"))
(def 'dead-currency (hex "fe6f"))

# extra dead elements for German T3 layout 
(def 'dead-lowline (hex "fe90"))
(def 'dead-aboveverticalline (hex "fe91"))
(def 'dead-belowverticalline (hex "fe92"))
(def 'dead-longsolidusoverlay (hex "fe93"))

# dead vowels for universal syllable entry 
(def 'dead-a (hex "fe80"))
(def 'dead-A (hex "fe81"))
(def 'dead-e (hex "fe82"))
(def 'dead-E (hex "fe83"))
(def 'dead-i (hex "fe84"))
(def 'dead-I (hex "fe85"))
(def 'dead-o (hex "fe86"))
(def 'dead-O (hex "fe87"))
(def 'dead-u (hex "fe88"))
(def 'dead-U (hex "fe89"))
(def 'dead-small-schwa (hex "fe8a"))
(def 'dead-capital-schwa (hex "fe8b"))

(def 'dead-greek (hex "fe8c"))

(def 'First-Virtual-Screen (hex "fed0"))
(def 'Prev-Virtual-Screen (hex "fed1"))
(def 'Next-Virtual-Screen (hex "fed2"))
(def 'Last-Virtual-Screen (hex "fed4"))
(def 'Terminate-Server (hex "fed5"))

(def 'AccessX-Enable (hex "fe70"))
(def 'AccessX-Feedback-Enable (hex "fe71"))
(def 'RepeatKeys-Enable (hex "fe72"))
(def 'SlowKeys-Enable (hex "fe73"))
(def 'BounceKeys-Enable (hex "fe74"))
(def 'StickyKeys-Enable (hex "fe75"))
(def 'MouseKeys-Enable (hex "fe76"))
(def 'MouseKeys-Accel-Enable (hex "fe77"))
(def 'Overlay1-Enable (hex "fe78"))
(def 'Overlay2-Enable (hex "fe79"))
(def 'AudibleBell-Enable (hex "fe7a"))

(def 'Pointer-Left (hex "fee0"))
(def 'Pointer-Right (hex "fee1"))
(def 'Pointer-Up (hex "fee2"))
(def 'Pointer-Down (hex "fee3"))
(def 'Pointer-UpLeft (hex "fee4"))
(def 'Pointer-UpRight (hex "fee5"))
(def 'Pointer-DownLeft (hex "fee6"))
(def 'Pointer-DownRight (hex "fee7"))
(def 'Pointer-Button-Dflt (hex "fee8"))
(def 'Pointer-Button1 (hex "fee9"))
(def 'Pointer-Button2 (hex "feea"))
(def 'Pointer-Button3 (hex "feeb"))
(def 'Pointer-Button4 (hex "feec"))
(def 'Pointer-Button5 (hex "feed"))
(def 'Pointer-DblClick-Dflt (hex "feee"))
(def 'Pointer-DblClick1 (hex "feef"))
(def 'Pointer-DblClick2 (hex "fef0"))
(def 'Pointer-DblClick3 (hex "fef1"))
(def 'Pointer-DblClick4 (hex "fef2"))
(def 'Pointer-DblClick5 (hex "fef3"))
(def 'Pointer-Drag-Dflt (hex "fef4"))
(def 'Pointer-Drag1 (hex "fef5"))
(def 'Pointer-Drag2 (hex "fef6"))
(def 'Pointer-Drag3 (hex "fef7"))
(def 'Pointer-Drag4 (hex "fef8"))
(def 'Pointer-Drag5 (hex "fefd"))

(def 'Pointer-EnableKeys (hex "fef9"))
(def 'Pointer-Accelerate (hex "fefa"))
(def 'Pointer-DfltBtnNext (hex "fefb"))
(def 'Pointer-DfltBtnPrev (hex "fefc"))

# Single-Stroke Multiple-Character N-Graph Keysyms For The X Input Method 

(def 'ch (hex "fea0"))
(def 'Ch (hex "fea1"))
(def 'CH (hex "fea2"))
(def 'c-h (hex "fea3"))
(def 'C-h (hex "fea4"))
(def 'C-H (hex "fea5"))


#{
  3270 Terminal Keys
  Byte 3 = 0xfd
}#

(def '3270-Duplicate (hex "fd01"))
(def '3270-FieldMark (hex "fd02"))
(def '3270-Right2 (hex "fd03"))
(def '3270-Left2 (hex "fd04"))
(def '3270-BackTab (hex "fd05"))
(def '3270-EraseEOF (hex "fd06"))
(def '3270-EraseInput (hex "fd07"))
(def '3270-Reset (hex "fd08"))
(def '3270-Quit (hex "fd09"))
(def '3270-PA1 (hex "fd0a"))
(def '3270-PA2 (hex "fd0b"))
(def '3270-PA3 (hex "fd0c"))
(def '3270-Test (hex "fd0d"))
(def '3270-Attn (hex "fd0e"))
(def '3270-CursorBlink (hex "fd0f"))
(def '3270-AltCursor (hex "fd10"))
(def '3270-KeyClick (hex "fd11"))
(def '3270-Jump (hex "fd12"))
(def '3270-Ident (hex "fd13"))
(def '3270-Rule (hex "fd14"))
(def '3270-Copy (hex "fd15"))
(def '3270-Play (hex "fd16"))
(def '3270-Setup (hex "fd17"))
(def '3270-Record (hex "fd18"))
(def '3270-ChangeScreen (hex "fd19"))
(def '3270-DeleteWord (hex "fd1a"))
(def '3270-ExSelect (hex "fd1b"))
(def '3270-CursorSelect (hex "fd1c"))
(def '3270-PrintScreen (hex "fd1d"))
(def '3270-Enter (hex "fd1e"))

#{
  Latin 1
  (ISO/IEC 8859-1 = Unicode U+0020..U+00FF)
  Byte 3 = 0
}#
(def 'space (hex "0020")) # U+0020 SPACE
(def 'exclam (hex "0021")) # U+0021 EXCLAMATION MARK
(def 'quotedbl (hex "0022")) # U+0022 QUOTATION MARK
(def 'numbersign (hex "0023")) # U+0023 NUMBER SIGN
(def 'dollar (hex "0024")) # U+0024 DOLLAR SIGN
(def 'percent (hex "0025")) # U+0025 PERCENT SIGN
(def 'ampersand (hex "0026")) # U+0026 AMPERSAND
(def 'apostrophe (hex "0027")) # U+0027 APOSTROPHE
(def 'quoteright (hex "0027")) # deprecated
(def 'parenleft (hex "0028")) # U+0028 LEFT PARENTHESIS
(def 'parenright (hex "0029")) # U+0029 RIGHT PARENTHESIS
(def 'asterisk (hex "002a")) # U+002A ASTERISK
(def 'plus (hex "002b")) # U+002B PLUS SIGN
(def 'comma (hex "002c")) # U+002C COMMA
(def 'minus (hex "002d")) # U+002D HYPHEN-MINUS
(def 'period (hex "002e")) # U+002E FULL STOP
(def 'slash (hex "002f")) # U+002F SOLIDUS
# Originally numbers but can't use numbers as symbols
(def 'd0 (hex "0030")) # U+0030 DIGIT ZERO
(def 'd1 (hex "0031")) # U+0031 DIGIT ONE
(def 'd2 (hex "0032")) # U+0032 DIGIT TWO
(def 'd3 (hex "0033")) # U+0033 DIGIT THREE
(def 'd4 (hex "0034")) # U+0034 DIGIT FOUR
(def 'd5 (hex "0035")) # U+0035 DIGIT FIVE
(def 'd6 (hex "0036")) # U+0036 DIGIT SIX
(def 'd7 (hex "0037")) # U+0037 DIGIT SEVEN
(def 'd8 (hex "0038")) # U+0038 DIGIT EIGHT
(def 'd9 (hex "0039")) # U+0039 DIGIT NINE
(def 'colon (hex "003a")) # U+003A COLON
(def 'semicolon (hex "003b")) # U+003B SEMICOLON
(def 'less (hex "003c")) # U+003C LESS-THAN SIGN
(def 'equal (hex "003d")) # U+003D EQUALS SIGN
(def 'greater (hex "003e")) # U+003E GREATER-THAN SIGN
(def 'question (hex "003f")) # U+003F QUESTION MARK
(def 'at (hex "0040")) # U+0040 COMMERCIAL AT
(def 'A (hex "0041")) # U+0041 LATIN CAPITAL LETTER A
(def 'B (hex "0042")) # U+0042 LATIN CAPITAL LETTER B
(def 'C (hex "0043")) # U+0043 LATIN CAPITAL LETTER C
(def 'D (hex "0044")) # U+0044 LATIN CAPITAL LETTER D
(def 'E (hex "0045")) # U+0045 LATIN CAPITAL LETTER E
(def 'F (hex "0046")) # U+0046 LATIN CAPITAL LETTER F
(def 'G (hex "0047")) # U+0047 LATIN CAPITAL LETTER G
(def 'H (hex "0048")) # U+0048 LATIN CAPITAL LETTER H
(def 'I (hex "0049")) # U+0049 LATIN CAPITAL LETTER I
(def 'J (hex "004a")) # U+004A LATIN CAPITAL LETTER J
(def 'K (hex "004b")) # U+004B LATIN CAPITAL LETTER K
(def 'L (hex "004c")) # U+004C LATIN CAPITAL LETTER L
(def 'M (hex "004d")) # U+004D LATIN CAPITAL LETTER M
(def 'N (hex "004e")) # U+004E LATIN CAPITAL LETTER N
(def 'O (hex "004f")) # U+004F LATIN CAPITAL LETTER O
(def 'P (hex "0050")) # U+0050 LATIN CAPITAL LETTER P
(def 'Q (hex "0051")) # U+0051 LATIN CAPITAL LETTER Q
(def 'R (hex "0052")) # U+0052 LATIN CAPITAL LETTER R
(def 'S (hex "0053")) # U+0053 LATIN CAPITAL LETTER S
# T is protected symbol
(def 'lT (hex "0054")) # U+0054 LATIN CAPITAL LETTER T
(def 'U (hex "0055")) # U+0055 LATIN CAPITAL LETTER U
(def 'V (hex "0056")) # U+0056 LATIN CAPITAL LETTER V
(def 'W (hex "0057")) # U+0057 LATIN CAPITAL LETTER W
(def 'X (hex "0058")) # U+0058 LATIN CAPITAL LETTER X
(def 'Y (hex "0059")) # U+0059 LATIN CAPITAL LETTER Y
(def 'Z (hex "005a")) # U+005A LATIN CAPITAL LETTER Z
(def 'bracketleft (hex "005b")) # U+005B LEFT SQUARE BRACKET
(def 'backslash (hex "005c")) # U+005C REVERSE SOLIDUS
(def 'bracketright (hex "005d")) # U+005D RIGHT SQUARE BRACKET
(def 'asciicircum (hex "005e")) # U+005E CIRCUMFLEX ACCENT
(def 'underscore (hex "005f")) # U+005F LOW LINE
(def 'grave (hex "0060")) # U+0060 GRAVE ACCENT
(def 'quoteleft (hex "0060")) # deprecated
(def 'a (hex "0061")) # U+0061 LATIN SMALL LETTER A
(def 'b (hex "0062")) # U+0062 LATIN SMALL LETTER B
(def 'c (hex "0063")) # U+0063 LATIN SMALL LETTER C
(def 'd (hex "0064")) # U+0064 LATIN SMALL LETTER D
(def 'e (hex "0065")) # U+0065 LATIN SMALL LETTER E
(def 'f (hex "0066")) # U+0066 LATIN SMALL LETTER F
(def 'g (hex "0067")) # U+0067 LATIN SMALL LETTER G
(def 'h (hex "0068")) # U+0068 LATIN SMALL LETTER H
(def 'i (hex "0069")) # U+0069 LATIN SMALL LETTER I
(def 'j (hex "006a")) # U+006A LATIN SMALL LETTER J
(def 'k (hex "006b")) # U+006B LATIN SMALL LETTER K
(def 'l (hex "006c")) # U+006C LATIN SMALL LETTER L
(def 'm (hex "006d")) # U+006D LATIN SMALL LETTER M
(def 'n (hex "006e")) # U+006E LATIN SMALL LETTER N
(def 'o (hex "006f")) # U+006F LATIN SMALL LETTER O
(def 'p (hex "0070")) # U+0070 LATIN SMALL LETTER P
(def 'q (hex "0071")) # U+0071 LATIN SMALL LETTER Q
(def 'r (hex "0072")) # U+0072 LATIN SMALL LETTER R
(def 's (hex "0073")) # U+0073 LATIN SMALL LETTER S
(def 't (hex "0074")) # U+0074 LATIN SMALL LETTER T
(def 'u (hex "0075")) # U+0075 LATIN SMALL LETTER U
(def 'v (hex "0076")) # U+0076 LATIN SMALL LETTER V
(def 'w (hex "0077")) # U+0077 LATIN SMALL LETTER W
(def 'x (hex "0078")) # U+0078 LATIN SMALL LETTER X
(def 'y (hex "0079")) # U+0079 LATIN SMALL LETTER Y
(def 'z (hex "007a")) # U+007A LATIN SMALL LETTER Z
(def 'braceleft (hex "007b")) # U+007B LEFT CURLY BRACKET
(def 'bar (hex "007c")) # U+007C VERTICAL LINE
(def 'braceright (hex "007d")) # U+007D RIGHT CURLY BRACKET
(def 'asciitilde (hex "007e")) # U+007E TILDE

(def 'nobreakspace (hex "00a0")) # U+00A0 NO-BREAK SPACE
(def 'exclamdown (hex "00a1")) # U+00A1 INVERTED EXCLAMATION MARK
(def 'cent (hex "00a2")) # U+00A2 CENT SIGN
(def 'sterling (hex "00a3")) # U+00A3 POUND SIGN
(def 'currency (hex "00a4")) # U+00A4 CURRENCY SIGN
(def 'yen (hex "00a5")) # U+00A5 YEN SIGN
(def 'brokenbar (hex "00a6")) # U+00A6 BROKEN BAR
(def 'section (hex "00a7")) # U+00A7 SECTION SIGN
(def 'diaeresis (hex "00a8")) # U+00A8 DIAERESIS
(def 'copyright (hex "00a9")) # U+00A9 COPYRIGHT SIGN
(def 'ordfeminine (hex "00aa")) # U+00AA FEMININE ORDINAL INDICATOR
(def 'guillemotleft (hex "00ab")) # U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
(def 'notsign (hex "00ac")) # U+00AC NOT SIGN
(def 'hyphen (hex "00ad")) # U+00AD SOFT HYPHEN
(def 'registered (hex "00ae")) # U+00AE REGISTERED SIGN
(def 'macron (hex "00af")) # U+00AF MACRON
(def 'degree (hex "00b0")) # U+00B0 DEGREE SIGN
(def 'plusminus (hex "00b1")) # U+00B1 PLUS-MINUS SIGN
(def 'twosuperior (hex "00b2")) # U+00B2 SUPERSCRIPT TWO
(def 'threesuperior (hex "00b3")) # U+00B3 SUPERSCRIPT THREE
(def 'acute (hex "00b4")) # U+00B4 ACUTE ACCENT
(def 'mu (hex "00b5")) # U+00B5 MICRO SIGN
(def 'paragraph (hex "00b6")) # U+00B6 PILCROW SIGN
(def 'periodcentered (hex "00b7")) # U+00B7 MIDDLE DOT
(def 'cedilla (hex "00b8")) # U+00B8 CEDILLA
(def 'onesuperior (hex "00b9")) # U+00B9 SUPERSCRIPT ONE
(def 'masculine (hex "00ba")) # U+00BA MASCULINE ORDINAL INDICATOR
(def 'guillemotright (hex "00bb")) # U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
(def 'onequarter (hex "00bc")) # U+00BC VULGAR FRACTION ONE QUARTER
(def 'onehalf (hex "00bd")) # U+00BD VULGAR FRACTION ONE HALF
(def 'threequarters (hex "00be")) # U+00BE VULGAR FRACTION THREE QUARTERS
(def 'questiondown (hex "00bf")) # U+00BF INVERTED QUESTION MARK
(def 'Agrave (hex "00c0")) # U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
(def 'Aacute (hex "00c1")) # U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
(def 'Acircumflex (hex "00c2")) # U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
(def 'Atilde (hex "00c3")) # U+00C3 LATIN CAPITAL LETTER A WITH TILDE
(def 'Adiaeresis (hex "00c4")) # U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
(def 'Aring (hex "00c5")) # U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
(def 'AE (hex "00c6")) # U+00C6 LATIN CAPITAL LETTER AE
(def 'Ccedilla (hex "00c7")) # U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
(def 'Egrave (hex "00c8")) # U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
(def 'Eacute (hex "00c9")) # U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
(def 'Ecircumflex (hex "00ca")) # U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
(def 'Ediaeresis (hex "00cb")) # U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
(def 'Igrave (hex "00cc")) # U+00CC LATIN CAPITAL LETTER I WITH GRAVE
(def 'Iacute (hex "00cd")) # U+00CD LATIN CAPITAL LETTER I WITH ACUTE
(def 'Icircumflex (hex "00ce")) # U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
(def 'Idiaeresis (hex "00cf")) # U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
(def 'ETH (hex "00d0")) # U+00D0 LATIN CAPITAL LETTER ETH
(def 'Eth (hex "00d0")) # deprecated
(def 'Ntilde (hex "00d1")) # U+00D1 LATIN CAPITAL LETTER N WITH TILDE
(def 'Ograve (hex "00d2")) # U+00D2 LATIN CAPITAL LETTER O WITH GRAVE
(def 'Oacute (hex "00d3")) # U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
(def 'Ocircumflex (hex "00d4")) # U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
(def 'Otilde (hex "00d5")) # U+00D5 LATIN CAPITAL LETTER O WITH TILDE
(def 'Odiaeresis (hex "00d6")) # U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
(def 'multiply (hex "00d7")) # U+00D7 MULTIPLICATION SIGN
(def 'Oslash (hex "00d8")) # U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(def 'Ooblique (hex "00d8")) # U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(def 'Ugrave (hex "00d9")) # U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
(def 'Uacute (hex "00da")) # U+00DA LATIN CAPITAL LETTER U WITH ACUTE
(def 'Ucircumflex (hex "00db")) # U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
(def 'Udiaeresis (hex "00dc")) # U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
(def 'Yacute (hex "00dd")) # U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
(def 'THORN (hex "00de")) # U+00DE LATIN CAPITAL LETTER THORN
(def 'Thorn (hex "00de")) # deprecated
(def 'ssharp (hex "00df")) # U+00DF LATIN SMALL LETTER SHARP S
(def 'agrave (hex "00e0")) # U+00E0 LATIN SMALL LETTER A WITH GRAVE
(def 'aacute (hex "00e1")) # U+00E1 LATIN SMALL LETTER A WITH ACUTE
(def 'acircumflex (hex "00e2")) # U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX
(def 'atilde (hex "00e3")) # U+00E3 LATIN SMALL LETTER A WITH TILDE
(def 'adiaeresis (hex "00e4")) # U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
(def 'aring (hex "00e5")) # U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
(def 'ae (hex "00e6")) # U+00E6 LATIN SMALL LETTER AE
(def 'ccedilla (hex "00e7")) # U+00E7 LATIN SMALL LETTER C WITH CEDILLA
(def 'egrave (hex "00e8")) # U+00E8 LATIN SMALL LETTER E WITH GRAVE
(def 'eacute (hex "00e9")) # U+00E9 LATIN SMALL LETTER E WITH ACUTE
(def 'ecircumflex (hex "00ea")) # U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
(def 'ediaeresis (hex "00eb")) # U+00EB LATIN SMALL LETTER E WITH DIAERESIS
(def 'igrave (hex "00ec")) # U+00EC LATIN SMALL LETTER I WITH GRAVE
(def 'iacute (hex "00ed")) # U+00ED LATIN SMALL LETTER I WITH ACUTE
(def 'icircumflex (hex "00ee")) # U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
(def 'idiaeresis (hex "00ef")) # U+00EF LATIN SMALL LETTER I WITH DIAERESIS
(def 'eth (hex "00f0")) # U+00F0 LATIN SMALL LETTER ETH
(def 'ntilde (hex "00f1")) # U+00F1 LATIN SMALL LETTER N WITH TILDE
(def 'ograve (hex "00f2")) # U+00F2 LATIN SMALL LETTER O WITH GRAVE
(def 'oacute (hex "00f3")) # U+00F3 LATIN SMALL LETTER O WITH ACUTE
(def 'ocircumflex (hex "00f4")) # U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
(def 'otilde (hex "00f5")) # U+00F5 LATIN SMALL LETTER O WITH TILDE
(def 'odiaeresis (hex "00f6")) # U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
(def 'division (hex "00f7")) # U+00F7 DIVISION SIGN
(def 'oslash (hex "00f8")) # U+00F8 LATIN SMALL LETTER O WITH STROKE
(def 'ooblique (hex "00f8")) # U+00F8 LATIN SMALL LETTER O WITH STROKE
(def 'ugrave (hex "00f9")) # U+00F9 LATIN SMALL LETTER U WITH GRAVE
(def 'uacute (hex "00fa")) # U+00FA LATIN SMALL LETTER U WITH ACUTE
(def 'ucircumflex (hex "00fb")) # U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
(def 'udiaeresis (hex "00fc")) # U+00FC LATIN SMALL LETTER U WITH DIAERESIS
(def 'yacute (hex "00fd")) # U+00FD LATIN SMALL LETTER Y WITH ACUTE
(def 'thorn (hex "00fe")) # U+00FE LATIN SMALL LETTER THORN
(def 'ydiaeresis (hex "00ff")) # U+00FF LATIN SMALL LETTER Y WITH DIAERESIS

#{
  Latin 2
  Byte 3 = 1
}#

(def 'Aogonek (hex "01a1")) # U+0104 LATIN CAPITAL LETTER A WITH OGONEK
(def 'breve (hex "01a2")) # U+02D8 BREVE
(def 'Lstroke (hex "01a3")) # U+0141 LATIN CAPITAL LETTER L WITH STROKE
(def 'Lcaron (hex "01a5")) # U+013D LATIN CAPITAL LETTER L WITH CARON
(def 'Sacute (hex "01a6")) # U+015A LATIN CAPITAL LETTER S WITH ACUTE
(def 'Scaron (hex "01a9")) # U+0160 LATIN CAPITAL LETTER S WITH CARON
(def 'Scedilla (hex "01aa")) # U+015E LATIN CAPITAL LETTER S WITH CEDILLA
(def 'Tcaron (hex "01ab")) # U+0164 LATIN CAPITAL LETTER T WITH CARON
(def 'Zacute (hex "01ac")) # U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
(def 'Zcaron (hex "01ae")) # U+017D LATIN CAPITAL LETTER Z WITH CARON
(def 'Zabovedot (hex "01af")) # U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
(def 'aogonek (hex "01b1")) # U+0105 LATIN SMALL LETTER A WITH OGONEK
(def 'ogonek (hex "01b2")) # U+02DB OGONEK
(def 'lstroke (hex "01b3")) # U+0142 LATIN SMALL LETTER L WITH STROKE
(def 'lcaron (hex "01b5")) # U+013E LATIN SMALL LETTER L WITH CARON
(def 'sacute (hex "01b6")) # U+015B LATIN SMALL LETTER S WITH ACUTE
(def 'caron (hex "01b7")) # U+02C7 CARON
(def 'scaron (hex "01b9")) # U+0161 LATIN SMALL LETTER S WITH CARON
(def 'scedilla (hex "01ba")) # U+015F LATIN SMALL LETTER S WITH CEDILLA
(def 'tcaron (hex "01bb")) # U+0165 LATIN SMALL LETTER T WITH CARON
(def 'zacute (hex "01bc")) # U+017A LATIN SMALL LETTER Z WITH ACUTE
(def 'doubleacute (hex "01bd")) # U+02DD DOUBLE ACUTE ACCENT
(def 'zcaron (hex "01be")) # U+017E LATIN SMALL LETTER Z WITH CARON
(def 'zabovedot (hex "01bf")) # U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
(def 'Racute (hex "01c0")) # U+0154 LATIN CAPITAL LETTER R WITH ACUTE
(def 'Abreve (hex "01c3")) # U+0102 LATIN CAPITAL LETTER A WITH BREVE
(def 'Lacute (hex "01c5")) # U+0139 LATIN CAPITAL LETTER L WITH ACUTE
(def 'Cacute (hex "01c6")) # U+0106 LATIN CAPITAL LETTER C WITH ACUTE
(def 'Ccaron (hex "01c8")) # U+010C LATIN CAPITAL LETTER C WITH CARON
(def 'Eogonek (hex "01ca")) # U+0118 LATIN CAPITAL LETTER E WITH OGONEK
(def 'Ecaron (hex "01cc")) # U+011A LATIN CAPITAL LETTER E WITH CARON
(def 'Dcaron (hex "01cf")) # U+010E LATIN CAPITAL LETTER D WITH CARON
(def 'Dstroke (hex "01d0")) # U+0110 LATIN CAPITAL LETTER D WITH STROKE
(def 'Nacute (hex "01d1")) # U+0143 LATIN CAPITAL LETTER N WITH ACUTE
(def 'Ncaron (hex "01d2")) # U+0147 LATIN CAPITAL LETTER N WITH CARON
(def 'Odoubleacute (hex "01d5")) # U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
(def 'Rcaron (hex "01d8")) # U+0158 LATIN CAPITAL LETTER R WITH CARON
(def 'Uring (hex "01d9")) # U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
(def 'Udoubleacute (hex "01db")) # U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
(def 'Tcedilla (hex "01de")) # U+0162 LATIN CAPITAL LETTER T WITH CEDILLA
(def 'racute (hex "01e0")) # U+0155 LATIN SMALL LETTER R WITH ACUTE
(def 'abreve (hex "01e3")) # U+0103 LATIN SMALL LETTER A WITH BREVE
(def 'lacute (hex "01e5")) # U+013A LATIN SMALL LETTER L WITH ACUTE
(def 'cacute (hex "01e6")) # U+0107 LATIN SMALL LETTER C WITH ACUTE
(def 'ccaron (hex "01e8")) # U+010D LATIN SMALL LETTER C WITH CARON
(def 'eogonek (hex "01ea")) # U+0119 LATIN SMALL LETTER E WITH OGONEK
(def 'ecaron (hex "01ec")) # U+011B LATIN SMALL LETTER E WITH CARON
(def 'dcaron (hex "01ef")) # U+010F LATIN SMALL LETTER D WITH CARON
(def 'dstroke (hex "01f0")) # U+0111 LATIN SMALL LETTER D WITH STROKE
(def 'nacute (hex "01f1")) # U+0144 LATIN SMALL LETTER N WITH ACUTE
(def 'ncaron (hex "01f2")) # U+0148 LATIN SMALL LETTER N WITH CARON
(def 'odoubleacute (hex "01f5")) # U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
(def 'rcaron (hex "01f8")) # U+0159 LATIN SMALL LETTER R WITH CARON
(def 'uring (hex "01f9")) # U+016F LATIN SMALL LETTER U WITH RING ABOVE
(def 'udoubleacute (hex "01fb")) # U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
(def 'tcedilla (hex "01fe")) # U+0163 LATIN SMALL LETTER T WITH CEDILLA
(def 'abovedot (hex "01ff")) # U+02D9 DOT ABOVE

#{
  Latin 3
  Byte 3 = 2
}#

(def 'Hstroke (hex "02a1")) # U+0126 LATIN CAPITAL LETTER H WITH STROKE
(def 'Hcircumflex (hex "02a6")) # U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX
(def 'Iabovedot (hex "02a9")) # U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
(def 'Gbreve (hex "02ab")) # U+011E LATIN CAPITAL LETTER G WITH BREVE
(def 'Jcircumflex (hex "02ac")) # U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX
(def 'hstroke (hex "02b1")) # U+0127 LATIN SMALL LETTER H WITH STROKE
(def 'hcircumflex (hex "02b6")) # U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX
(def 'idotless (hex "02b9")) # U+0131 LATIN SMALL LETTER DOTLESS I
(def 'gbreve (hex "02bb")) # U+011F LATIN SMALL LETTER G WITH BREVE
(def 'jcircumflex (hex "02bc")) # U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX
(def 'Cabovedot (hex "02c5")) # U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE
(def 'Ccircumflex (hex "02c6")) # U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX
(def 'Gabovedot (hex "02d5")) # U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE
(def 'Gcircumflex (hex "02d8")) # U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX
(def 'Ubreve (hex "02dd")) # U+016C LATIN CAPITAL LETTER U WITH BREVE
(def 'Scircumflex (hex "02de")) # U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX
(def 'cabovedot (hex "02e5")) # U+010B LATIN SMALL LETTER C WITH DOT ABOVE
(def 'ccircumflex (hex "02e6")) # U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX
(def 'gabovedot (hex "02f5")) # U+0121 LATIN SMALL LETTER G WITH DOT ABOVE
(def 'gcircumflex (hex "02f8")) # U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX
(def 'ubreve (hex "02fd")) # U+016D LATIN SMALL LETTER U WITH BREVE
(def 'scircumflex (hex "02fe")) # U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX


#{
  Latin 4
  Byte 3 = 3
}#

(def 'kra (hex "03a2")) # U+0138 LATIN SMALL LETTER KRA
(def 'kappa (hex "03a2")) # deprecated
(def 'Rcedilla (hex "03a3")) # U+0156 LATIN CAPITAL LETTER R WITH CEDILLA
(def 'Itilde (hex "03a5")) # U+0128 LATIN CAPITAL LETTER I WITH TILDE
(def 'Lcedilla (hex "03a6")) # U+013B LATIN CAPITAL LETTER L WITH CEDILLA
(def 'Emacron (hex "03aa")) # U+0112 LATIN CAPITAL LETTER E WITH MACRON
(def 'Gcedilla (hex "03ab")) # U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
(def 'Tslash (hex "03ac")) # U+0166 LATIN CAPITAL LETTER T WITH STROKE
(def 'rcedilla (hex "03b3")) # U+0157 LATIN SMALL LETTER R WITH CEDILLA
(def 'itilde (hex "03b5")) # U+0129 LATIN SMALL LETTER I WITH TILDE
(def 'lcedilla (hex "03b6")) # U+013C LATIN SMALL LETTER L WITH CEDILLA
(def 'emacron (hex "03ba")) # U+0113 LATIN SMALL LETTER E WITH MACRON
(def 'gcedilla (hex "03bb")) # U+0123 LATIN SMALL LETTER G WITH CEDILLA
(def 'tslash (hex "03bc")) # U+0167 LATIN SMALL LETTER T WITH STROKE
(def 'ENG (hex "03bd")) # U+014A LATIN CAPITAL LETTER ENG
(def 'eng (hex "03bf")) # U+014B LATIN SMALL LETTER ENG
(def 'Amacron (hex "03c0")) # U+0100 LATIN CAPITAL LETTER A WITH MACRON
(def 'Iogonek (hex "03c7")) # U+012E LATIN CAPITAL LETTER I WITH OGONEK
(def 'Eabovedot (hex "03cc")) # U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE
(def 'Imacron (hex "03cf")) # U+012A LATIN CAPITAL LETTER I WITH MACRON
(def 'Ncedilla (hex "03d1")) # U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
(def 'Omacron (hex "03d2")) # U+014C LATIN CAPITAL LETTER O WITH MACRON
(def 'Kcedilla (hex "03d3")) # U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
(def 'Uogonek (hex "03d9")) # U+0172 LATIN CAPITAL LETTER U WITH OGONEK
(def 'Utilde (hex "03dd")) # U+0168 LATIN CAPITAL LETTER U WITH TILDE
(def 'Umacron (hex "03de")) # U+016A LATIN CAPITAL LETTER U WITH MACRON
(def 'amacron (hex "03e0")) # U+0101 LATIN SMALL LETTER A WITH MACRON
(def 'iogonek (hex "03e7")) # U+012F LATIN SMALL LETTER I WITH OGONEK
(def 'eabovedot (hex "03ec")) # U+0117 LATIN SMALL LETTER E WITH DOT ABOVE
(def 'imacron (hex "03ef")) # U+012B LATIN SMALL LETTER I WITH MACRON
(def 'ncedilla (hex "03f1")) # U+0146 LATIN SMALL LETTER N WITH CEDILLA
(def 'omacron (hex "03f2")) # U+014D LATIN SMALL LETTER O WITH MACRON
(def 'kcedilla (hex "03f3")) # U+0137 LATIN SMALL LETTER K WITH CEDILLA
(def 'uogonek (hex "03f9")) # U+0173 LATIN SMALL LETTER U WITH OGONEK
(def 'utilde (hex "03fd")) # U+0169 LATIN SMALL LETTER U WITH TILDE
(def 'umacron (hex "03fe")) # U+016B LATIN SMALL LETTER U WITH MACRON

#{
  Latin 8
}#
(def 'Wcircumflex (hex "1000174")) # U+0174 LATIN CAPITAL LETTER W WITH CIRCUMFLEX
(def 'wcircumflex (hex "1000175")) # U+0175 LATIN SMALL LETTER W WITH CIRCUMFLEX
(def 'Ycircumflex (hex "1000176")) # U+0176 LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
(def 'ycircumflex (hex "1000177")) # U+0177 LATIN SMALL LETTER Y WITH CIRCUMFLEX
(def 'Babovedot (hex "1001e02")) # U+1E02 LATIN CAPITAL LETTER B WITH DOT ABOVE
(def 'babovedot (hex "1001e03")) # U+1E03 LATIN SMALL LETTER B WITH DOT ABOVE
(def 'Dabovedot (hex "1001e0a")) # U+1E0A LATIN CAPITAL LETTER D WITH DOT ABOVE
(def 'dabovedot (hex "1001e0b")) # U+1E0B LATIN SMALL LETTER D WITH DOT ABOVE
(def 'Fabovedot (hex "1001e1e")) # U+1E1E LATIN CAPITAL LETTER F WITH DOT ABOVE
(def 'fabovedot (hex "1001e1f")) # U+1E1F LATIN SMALL LETTER F WITH DOT ABOVE
(def 'Mabovedot (hex "1001e40")) # U+1E40 LATIN CAPITAL LETTER M WITH DOT ABOVE
(def 'mabovedot (hex "1001e41")) # U+1E41 LATIN SMALL LETTER M WITH DOT ABOVE
(def 'Pabovedot (hex "1001e56")) # U+1E56 LATIN CAPITAL LETTER P WITH DOT ABOVE
(def 'pabovedot (hex "1001e57")) # U+1E57 LATIN SMALL LETTER P WITH DOT ABOVE
(def 'Sabovedot (hex "1001e60")) # U+1E60 LATIN CAPITAL LETTER S WITH DOT ABOVE
(def 'sabovedot (hex "1001e61")) # U+1E61 LATIN SMALL LETTER S WITH DOT ABOVE
(def 'Tabovedot (hex "1001e6a")) # U+1E6A LATIN CAPITAL LETTER T WITH DOT ABOVE
(def 'tabovedot (hex "1001e6b")) # U+1E6B LATIN SMALL LETTER T WITH DOT ABOVE
(def 'Wgrave (hex "1001e80")) # U+1E80 LATIN CAPITAL LETTER W WITH GRAVE
(def 'wgrave (hex "1001e81")) # U+1E81 LATIN SMALL LETTER W WITH GRAVE
(def 'Wacute (hex "1001e82")) # U+1E82 LATIN CAPITAL LETTER W WITH ACUTE
(def 'wacute (hex "1001e83")) # U+1E83 LATIN SMALL LETTER W WITH ACUTE
(def 'Wdiaeresis (hex "1001e84")) # U+1E84 LATIN CAPITAL LETTER W WITH DIAERESIS
(def 'wdiaeresis (hex "1001e85")) # U+1E85 LATIN SMALL LETTER W WITH DIAERESIS
(def 'Ygrave (hex "1001ef2")) # U+1EF2 LATIN CAPITAL LETTER Y WITH GRAVE
(def 'ygrave (hex "1001ef3")) # U+1EF3 LATIN SMALL LETTER Y WITH GRAVE

#{
  Latin 9
  Byte 3 = 0x13
}#

(def 'OE (hex "13bc")) # U+0152 LATIN CAPITAL LIGATURE OE
(def 'oe (hex "13bd")) # U+0153 LATIN SMALL LIGATURE OE
(def 'Ydiaeresis (hex "13be")) # U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS

#{
  Katakana
  Byte 3 = 4
}#

(def 'overline (hex "047e")) # U+203E OVERLINE
(def 'kana-fullstop (hex "04a1")) # U+3002 IDEOGRAPHIC FULL STOP
(def 'kana-openingbracket (hex "04a2")) # U+300C LEFT CORNER BRACKET
(def 'kana-closingbracket (hex "04a3")) # U+300D RIGHT CORNER BRACKET
(def 'kana-comma (hex "04a4")) # U+3001 IDEOGRAPHIC COMMA
(def 'kana-conjunctive (hex "04a5")) # U+30FB KATAKANA MIDDLE DOT
(def 'kana-middledot (hex "04a5")) # deprecated
(def 'kana-WO (hex "04a6")) # U+30F2 KATAKANA LETTER WO
(def 'kana-a (hex "04a7")) # U+30A1 KATAKANA LETTER SMALL A
(def 'kana-i (hex "04a8")) # U+30A3 KATAKANA LETTER SMALL I
(def 'kana-u (hex "04a9")) # U+30A5 KATAKANA LETTER SMALL U
(def 'kana-e (hex "04aa")) # U+30A7 KATAKANA LETTER SMALL E
(def 'kana-o (hex "04ab")) # U+30A9 KATAKANA LETTER SMALL O
(def 'kana-ya (hex "04ac")) # U+30E3 KATAKANA LETTER SMALL YA
(def 'kana-yu (hex "04ad")) # U+30E5 KATAKANA LETTER SMALL YU
(def 'kana-yo (hex "04ae")) # U+30E7 KATAKANA LETTER SMALL YO
(def 'kana-tsu (hex "04af")) # U+30C3 KATAKANA LETTER SMALL TU
(def 'kana-tu (hex "04af")) # deprecated
(def 'prolongedsound (hex "04b0")) # U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK
(def 'kana-A (hex "04b1")) # U+30A2 KATAKANA LETTER A
(def 'kana-I (hex "04b2")) # U+30A4 KATAKANA LETTER I
(def 'kana-U (hex "04b3")) # U+30A6 KATAKANA LETTER U
(def 'kana-E (hex "04b4")) # U+30A8 KATAKANA LETTER E
(def 'kana-O (hex "04b5")) # U+30AA KATAKANA LETTER O
(def 'kana-KA (hex "04b6")) # U+30AB KATAKANA LETTER KA
(def 'kana-KI (hex "04b7")) # U+30AD KATAKANA LETTER KI
(def 'kana-KU (hex "04b8")) # U+30AF KATAKANA LETTER KU
(def 'kana-KE (hex "04b9")) # U+30B1 KATAKANA LETTER KE
(def 'kana-KO (hex "04ba")) # U+30B3 KATAKANA LETTER KO
(def 'kana-SA (hex "04bb")) # U+30B5 KATAKANA LETTER SA
(def 'kana-SHI (hex "04bc")) # U+30B7 KATAKANA LETTER SI
(def 'kana-SU (hex "04bd")) # U+30B9 KATAKANA LETTER SU
(def 'kana-SE (hex "04be")) # U+30BB KATAKANA LETTER SE
(def 'kana-SO (hex "04bf")) # U+30BD KATAKANA LETTER SO
(def 'kana-TA (hex "04c0")) # U+30BF KATAKANA LETTER TA
(def 'kana-CHI (hex "04c1")) # U+30C1 KATAKANA LETTER TI
(def 'kana-TI (hex "04c1")) # deprecated
(def 'kana-TSU (hex "04c2")) # U+30C4 KATAKANA LETTER TU
(def 'kana-TU (hex "04c2")) # deprecated
(def 'kana-TE (hex "04c3")) # U+30C6 KATAKANA LETTER TE
(def 'kana-TO (hex "04c4")) # U+30C8 KATAKANA LETTER TO
(def 'kana-NA (hex "04c5")) # U+30CA KATAKANA LETTER NA
(def 'kana-NI (hex "04c6")) # U+30CB KATAKANA LETTER NI
(def 'kana-NU (hex "04c7")) # U+30CC KATAKANA LETTER NU
(def 'kana-NE (hex "04c8")) # U+30CD KATAKANA LETTER NE
(def 'kana-NO (hex "04c9")) # U+30CE KATAKANA LETTER NO
(def 'kana-HA (hex "04ca")) # U+30CF KATAKANA LETTER HA
(def 'kana-HI (hex "04cb")) # U+30D2 KATAKANA LETTER HI
(def 'kana-FU (hex "04cc")) # U+30D5 KATAKANA LETTER HU
(def 'kana-HU (hex "04cc")) # deprecated
(def 'kana-HE (hex "04cd")) # U+30D8 KATAKANA LETTER HE
(def 'kana-HO (hex "04ce")) # U+30DB KATAKANA LETTER HO
(def 'kana-MA (hex "04cf")) # U+30DE KATAKANA LETTER MA
(def 'kana-MI (hex "04d0")) # U+30DF KATAKANA LETTER MI
(def 'kana-MU (hex "04d1")) # U+30E0 KATAKANA LETTER MU
(def 'kana-ME (hex "04d2")) # U+30E1 KATAKANA LETTER ME
(def 'kana-MO (hex "04d3")) # U+30E2 KATAKANA LETTER MO
(def 'kana-YA (hex "04d4")) # U+30E4 KATAKANA LETTER YA
(def 'kana-YU (hex "04d5")) # U+30E6 KATAKANA LETTER YU
(def 'kana-YO (hex "04d6")) # U+30E8 KATAKANA LETTER YO
(def 'kana-RA (hex "04d7")) # U+30E9 KATAKANA LETTER RA
(def 'kana-RI (hex "04d8")) # U+30EA KATAKANA LETTER RI
(def 'kana-RU (hex "04d9")) # U+30EB KATAKANA LETTER RU
(def 'kana-RE (hex "04da")) # U+30EC KATAKANA LETTER RE
(def 'kana-RO (hex "04db")) # U+30ED KATAKANA LETTER RO
(def 'kana-WA (hex "04dc")) # U+30EF KATAKANA LETTER WA
(def 'kana-N (hex "04dd")) # U+30F3 KATAKANA LETTER N
(def 'voicedsound (hex "04de")) # U+309B KATAKANA-HIRAGANA VOICED SOUND MARK
(def 'semivoicedsound (hex "04df")) # U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
(def 'kana-switch (hex "ff7e")) # Alias for mode_switch

#{
  Arabic
  Byte 3 = 5
}#

(def 'Farsi-0 (hex "10006f0")) # U+06F0 EXTENDED ARABIC-INDIC DIGIT ZERO
(def 'Farsi-1 (hex "10006f1")) # U+06F1 EXTENDED ARABIC-INDIC DIGIT ONE
(def 'Farsi-2 (hex "10006f2")) # U+06F2 EXTENDED ARABIC-INDIC DIGIT TWO
(def 'Farsi-3 (hex "10006f3")) # U+06F3 EXTENDED ARABIC-INDIC DIGIT THREE
(def 'Farsi-4 (hex "10006f4")) # U+06F4 EXTENDED ARABIC-INDIC DIGIT FOUR
(def 'Farsi-5 (hex "10006f5")) # U+06F5 EXTENDED ARABIC-INDIC DIGIT FIVE
(def 'Farsi-6 (hex "10006f6")) # U+06F6 EXTENDED ARABIC-INDIC DIGIT SIX
(def 'Farsi-7 (hex "10006f7")) # U+06F7 EXTENDED ARABIC-INDIC DIGIT SEVEN
(def 'Farsi-8 (hex "10006f8")) # U+06F8 EXTENDED ARABIC-INDIC DIGIT EIGHT
(def 'Farsi-9 (hex "10006f9")) # U+06F9 EXTENDED ARABIC-INDIC DIGIT NINE
(def 'Arabic-percent (hex "100066a")) # U+066A ARABIC PERCENT SIGN
(def 'Arabic-superscript-alef (hex "1000670")) # U+0670 ARABIC LETTER SUPERSCRIPT ALEF
(def 'Arabic-tteh (hex "1000679")) # U+0679 ARABIC LETTER TTEH
(def 'Arabic-peh (hex "100067e")) # U+067E ARABIC LETTER PEH
(def 'Arabic-tcheh (hex "1000686")) # U+0686 ARABIC LETTER TCHEH
(def 'Arabic-ddal (hex "1000688")) # U+0688 ARABIC LETTER DDAL
(def 'Arabic-rreh (hex "1000691")) # U+0691 ARABIC LETTER RREH
(def 'Arabic-comma (hex "05ac")) # U+060C ARABIC COMMA
(def 'Arabic-fullstop (hex "10006d4")) # U+06D4 ARABIC FULL STOP
(def 'Arabic-0 (hex "1000660")) # U+0660 ARABIC-INDIC DIGIT ZERO
(def 'Arabic-1 (hex "1000661")) # U+0661 ARABIC-INDIC DIGIT ONE
(def 'Arabic-2 (hex "1000662")) # U+0662 ARABIC-INDIC DIGIT TWO
(def 'Arabic-3 (hex "1000663")) # U+0663 ARABIC-INDIC DIGIT THREE
(def 'Arabic-4 (hex "1000664")) # U+0664 ARABIC-INDIC DIGIT FOUR
(def 'Arabic-5 (hex "1000665")) # U+0665 ARABIC-INDIC DIGIT FIVE
(def 'Arabic-6 (hex "1000666")) # U+0666 ARABIC-INDIC DIGIT SIX
(def 'Arabic-7 (hex "1000667")) # U+0667 ARABIC-INDIC DIGIT SEVEN
(def 'Arabic-8 (hex "1000668")) # U+0668 ARABIC-INDIC DIGIT EIGHT
(def 'Arabic-9 (hex "1000669")) # U+0669 ARABIC-INDIC DIGIT NINE
(def 'Arabic-semicolon (hex "05bb")) # U+061B ARABIC SEMICOLON
(def 'Arabic-question-mark (hex "05bf")) # U+061F ARABIC QUESTION MARK
(def 'Arabic-hamza (hex "05c1")) # U+0621 ARABIC LETTER HAMZA
(def 'Arabic-maddaonalef (hex "05c2")) # U+0622 ARABIC LETTER ALEF WITH MADDA ABOVE
(def 'Arabic-hamzaonalef (hex "05c3")) # U+0623 ARABIC LETTER ALEF WITH HAMZA ABOVE
(def 'Arabic-hamzaonwaw (hex "05c4")) # U+0624 ARABIC LETTER WAW WITH HAMZA ABOVE
(def 'Arabic-hamzaunderalef (hex "05c5")) # U+0625 ARABIC LETTER ALEF WITH HAMZA BELOW
(def 'Arabic-hamzaonyeh (hex "05c6")) # U+0626 ARABIC LETTER YEH WITH HAMZA ABOVE
(def 'Arabic-alef (hex "05c7")) # U+0627 ARABIC LETTER ALEF
(def 'Arabic-beh (hex "05c8")) # U+0628 ARABIC LETTER BEH
(def 'Arabic-tehmarbuta (hex "05c9")) # U+0629 ARABIC LETTER TEH MARBUTA
(def 'Arabic-teh (hex "05ca")) # U+062A ARABIC LETTER TEH
(def 'Arabic-theh (hex "05cb")) # U+062B ARABIC LETTER THEH
(def 'Arabic-jeem (hex "05cc")) # U+062C ARABIC LETTER JEEM
(def 'Arabic-hah (hex "05cd")) # U+062D ARABIC LETTER HAH
(def 'Arabic-khah (hex "05ce")) # U+062E ARABIC LETTER KHAH
(def 'Arabic-dal (hex "05cf")) # U+062F ARABIC LETTER DAL
(def 'Arabic-thal (hex "05d0")) # U+0630 ARABIC LETTER THAL
(def 'Arabic-ra (hex "05d1")) # U+0631 ARABIC LETTER REH
(def 'Arabic-zain (hex "05d2")) # U+0632 ARABIC LETTER ZAIN
(def 'Arabic-seen (hex "05d3")) # U+0633 ARABIC LETTER SEEN
(def 'Arabic-sheen (hex "05d4")) # U+0634 ARABIC LETTER SHEEN
(def 'Arabic-sad (hex "05d5")) # U+0635 ARABIC LETTER SAD
(def 'Arabic-dad (hex "05d6")) # U+0636 ARABIC LETTER DAD
(def 'Arabic-tah (hex "05d7")) # U+0637 ARABIC LETTER TAH
(def 'Arabic-zah (hex "05d8")) # U+0638 ARABIC LETTER ZAH
(def 'Arabic-ain (hex "05d9")) # U+0639 ARABIC LETTER AIN
(def 'Arabic-ghain (hex "05da")) # U+063A ARABIC LETTER GHAIN
(def 'Arabic-tatweel (hex "05e0")) # U+0640 ARABIC TATWEEL
(def 'Arabic-feh (hex "05e1")) # U+0641 ARABIC LETTER FEH
(def 'Arabic-qaf (hex "05e2")) # U+0642 ARABIC LETTER QAF
(def 'Arabic-kaf (hex "05e3")) # U+0643 ARABIC LETTER KAF
(def 'Arabic-lam (hex "05e4")) # U+0644 ARABIC LETTER LAM
(def 'Arabic-meem (hex "05e5")) # U+0645 ARABIC LETTER MEEM
(def 'Arabic-noon (hex "05e6")) # U+0646 ARABIC LETTER NOON
(def 'Arabic-ha (hex "05e7")) # U+0647 ARABIC LETTER HEH
(def 'Arabic-heh (hex "05e7")) # deprecated
(def 'Arabic-waw (hex "05e8")) # U+0648 ARABIC LETTER WAW
(def 'Arabic-alefmaksura (hex "05e9")) # U+0649 ARABIC LETTER ALEF MAKSURA
(def 'Arabic-yeh (hex "05ea")) # U+064A ARABIC LETTER YEH
(def 'Arabic-fathatan (hex "05eb")) # U+064B ARABIC FATHATAN
(def 'Arabic-dammatan (hex "05ec")) # U+064C ARABIC DAMMATAN
(def 'Arabic-kasratan (hex "05ed")) # U+064D ARABIC KASRATAN
(def 'Arabic-fatha (hex "05ee")) # U+064E ARABIC FATHA
(def 'Arabic-damma (hex "05ef")) # U+064F ARABIC DAMMA
(def 'Arabic-kasra (hex "05f0")) # U+0650 ARABIC KASRA
(def 'Arabic-shadda (hex "05f1")) # U+0651 ARABIC SHADDA
(def 'Arabic-sukun (hex "05f2")) # U+0652 ARABIC SUKUN
(def 'Arabic-madda-above (hex "1000653")) # U+0653 ARABIC MADDAH ABOVE
(def 'Arabic-hamza-above (hex "1000654")) # U+0654 ARABIC HAMZA ABOVE
(def 'Arabic-hamza-below (hex "1000655")) # U+0655 ARABIC HAMZA BELOW
(def 'Arabic-jeh (hex "1000698")) # U+0698 ARABIC LETTER JEH
(def 'Arabic-veh (hex "10006a4")) # U+06A4 ARABIC LETTER VEH
(def 'Arabic-keheh (hex "10006a9")) # U+06A9 ARABIC LETTER KEHEH
(def 'Arabic-gaf (hex "10006af")) # U+06AF ARABIC LETTER GAF
(def 'Arabic-noon-ghunna (hex "10006ba")) # U+06BA ARABIC LETTER NOON GHUNNA
(def 'Arabic-heh-doachashmee (hex "10006be")) # U+06BE ARABIC LETTER HEH DOACHASHMEE
(def 'Farsi-yeh (hex "10006cc")) # U+06CC ARABIC LETTER FARSI YEH
(def 'Arabic-farsi-yeh (hex "10006cc")) # U+06CC ARABIC LETTER FARSI YEH
(def 'Arabic-yeh-baree (hex "10006d2")) # U+06D2 ARABIC LETTER YEH BARREE
(def 'Arabic-heh-goal (hex "10006c1")) # U+06C1 ARABIC LETTER HEH GOAL
(def 'Arabic-switch (hex "ff7e")) # Alias for mode_switch

#{
  Cyrillic
  Byte 3 = 6
}#
(def 'Cyrillic-GHE-bar (hex "1000492")) # U+0492 CYRILLIC CAPITAL LETTER GHE WITH STROKE
(def 'Cyrillic-ghe-bar (hex "1000493")) # U+0493 CYRILLIC SMALL LETTER GHE WITH STROKE
(def 'Cyrillic-ZHE-descender (hex "1000496")) # U+0496 CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
(def 'Cyrillic-zhe-descender (hex "1000497")) # U+0497 CYRILLIC SMALL LETTER ZHE WITH DESCENDER
(def 'Cyrillic-KA-descender (hex "100049a")) # U+049A CYRILLIC CAPITAL LETTER KA WITH DESCENDER
(def 'Cyrillic-ka-descender (hex "100049b")) # U+049B CYRILLIC SMALL LETTER KA WITH DESCENDER
(def 'Cyrillic-KA-vertstroke (hex "100049c")) # U+049C CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
(def 'Cyrillic-ka-vertstroke (hex "100049d")) # U+049D CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
(def 'Cyrillic-EN-descender (hex "10004a2")) # U+04A2 CYRILLIC CAPITAL LETTER EN WITH DESCENDER
(def 'Cyrillic-en-descender (hex "10004a3")) # U+04A3 CYRILLIC SMALL LETTER EN WITH DESCENDER
(def 'Cyrillic-U-straight (hex "10004ae")) # U+04AE CYRILLIC CAPITAL LETTER STRAIGHT U
(def 'Cyrillic-u-straight (hex "10004af")) # U+04AF CYRILLIC SMALL LETTER STRAIGHT U
(def 'Cyrillic-U-straight-bar (hex "10004b0")) # U+04B0 CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
(def 'Cyrillic-u-straight-bar (hex "10004b1")) # U+04B1 CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
(def 'Cyrillic-HA-descender (hex "10004b2")) # U+04B2 CYRILLIC CAPITAL LETTER HA WITH DESCENDER
(def 'Cyrillic-ha-descender (hex "10004b3")) # U+04B3 CYRILLIC SMALL LETTER HA WITH DESCENDER
(def 'Cyrillic-CHE-descender (hex "10004b6")) # U+04B6 CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
(def 'Cyrillic-che-descender (hex "10004b7")) # U+04B7 CYRILLIC SMALL LETTER CHE WITH DESCENDER
(def 'Cyrillic-CHE-vertstroke (hex "10004b8")) # U+04B8 CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
(def 'Cyrillic-che-vertstroke (hex "10004b9")) # U+04B9 CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
(def 'Cyrillic-SHHA (hex "10004ba")) # U+04BA CYRILLIC CAPITAL LETTER SHHA
(def 'Cyrillic-shha (hex "10004bb")) # U+04BB CYRILLIC SMALL LETTER SHHA

(def 'Cyrillic-SCHWA (hex "10004d8")) # U+04D8 CYRILLIC CAPITAL LETTER SCHWA
(def 'Cyrillic-schwa (hex "10004d9")) # U+04D9 CYRILLIC SMALL LETTER SCHWA
(def 'Cyrillic-I-macron (hex "10004e2")) # U+04E2 CYRILLIC CAPITAL LETTER I WITH MACRON
(def 'Cyrillic-i-macron (hex "10004e3")) # U+04E3 CYRILLIC SMALL LETTER I WITH MACRON
(def 'Cyrillic-O-bar (hex "10004e8")) # U+04E8 CYRILLIC CAPITAL LETTER BARRED O
(def 'Cyrillic-o-bar (hex "10004e9")) # U+04E9 CYRILLIC SMALL LETTER BARRED O
(def 'Cyrillic-U-macron (hex "10004ee")) # U+04EE CYRILLIC CAPITAL LETTER U WITH MACRON
(def 'Cyrillic-u-macron (hex "10004ef")) # U+04EF CYRILLIC SMALL LETTER U WITH MACRON

(def 'Serbian-dje (hex "06a1")) # U+0452 CYRILLIC SMALL LETTER DJE
(def 'Macedonia-gje (hex "06a2")) # U+0453 CYRILLIC SMALL LETTER GJE
(def 'Cyrillic-io (hex "06a3")) # U+0451 CYRILLIC SMALL LETTER IO
(def 'Ukrainian-ie (hex "06a4")) # U+0454 CYRILLIC SMALL LETTER UKRAINIAN IE
(def 'Ukranian-je (hex "06a4")) # deprecated
(def 'Macedonia-dse (hex "06a5")) # U+0455 CYRILLIC SMALL LETTER DZE
(def 'Ukrainian-i (hex "06a6")) # U+0456 CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
(def 'Ukranian-i (hex "06a6")) # deprecated
(def 'Ukrainian-yi (hex "06a7")) # U+0457 CYRILLIC SMALL LETTER YI
(def 'Ukranian-yi (hex "06a7")) # deprecated
(def 'Cyrillic-je (hex "06a8")) # U+0458 CYRILLIC SMALL LETTER JE
(def 'Serbian-je (hex "06a8")) # deprecated
(def 'Cyrillic-lje (hex "06a9")) # U+0459 CYRILLIC SMALL LETTER LJE
(def 'Serbian-lje (hex "06a9")) # deprecated
(def 'Cyrillic-nje (hex "06aa")) # U+045A CYRILLIC SMALL LETTER NJE
(def 'Serbian-nje (hex "06aa")) # deprecated
(def 'Serbian-tshe (hex "06ab")) # U+045B CYRILLIC SMALL LETTER TSHE
(def 'Macedonia-kje (hex "06ac")) # U+045C CYRILLIC SMALL LETTER KJE
(def 'Ukrainian-ghe-with-upturn (hex "06ad")) # U+0491 CYRILLIC SMALL LETTER GHE WITH UPTURN
(def 'Byelorussian-shortu (hex "06ae")) # U+045E CYRILLIC SMALL LETTER SHORT U
(def 'Cyrillic-dzhe (hex "06af")) # U+045F CYRILLIC SMALL LETTER DZHE
(def 'Serbian-dze (hex "06af")) # deprecated
(def 'numerosign (hex "06b0")) # U+2116 NUMERO SIGN
(def 'Serbian-DJE (hex "06b1")) # U+0402 CYRILLIC CAPITAL LETTER DJE
(def 'Macedonia-GJE (hex "06b2")) # U+0403 CYRILLIC CAPITAL LETTER GJE
(def 'Cyrillic-IO (hex "06b3")) # U+0401 CYRILLIC CAPITAL LETTER IO
(def 'Ukrainian-IE (hex "06b4")) # U+0404 CYRILLIC CAPITAL LETTER UKRAINIAN IE
(def 'Ukranian-JE (hex "06b4")) # deprecated
(def 'Macedonia-DSE (hex "06b5")) # U+0405 CYRILLIC CAPITAL LETTER DZE
(def 'Ukrainian-I (hex "06b6")) # U+0406 CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
(def 'Ukranian-I (hex "06b6")) # deprecated
(def 'Ukrainian-YI (hex "06b7")) # U+0407 CYRILLIC CAPITAL LETTER YI
(def 'Ukranian-YI (hex "06b7")) # deprecated
(def 'Cyrillic-JE (hex "06b8")) # U+0408 CYRILLIC CAPITAL LETTER JE
(def 'Serbian-JE (hex "06b8")) # deprecated
(def 'Cyrillic-LJE (hex "06b9")) # U+0409 CYRILLIC CAPITAL LETTER LJE
(def 'Serbian-LJE (hex "06b9")) # deprecated
(def 'Cyrillic-NJE (hex "06ba")) # U+040A CYRILLIC CAPITAL LETTER NJE
(def 'Serbian-NJE (hex "06ba")) # deprecated
(def 'Serbian-TSHE (hex "06bb")) # U+040B CYRILLIC CAPITAL LETTER TSHE
(def 'Macedonia-KJE (hex "06bc")) # U+040C CYRILLIC CAPITAL LETTER KJE
(def 'Ukrainian-GHE-WITH-UPTURN (hex "06bd")) # U+0490 CYRILLIC CAPITAL LETTER GHE WITH UPTURN
(def 'Byelorussian-SHORTU (hex "06be")) # U+040E CYRILLIC CAPITAL LETTER SHORT U
(def 'Cyrillic-DZHE (hex "06bf")) # U+040F CYRILLIC CAPITAL LETTER DZHE
(def 'Serbian-DZE (hex "06bf")) # deprecated
(def 'Cyrillic-yu (hex "06c0")) # U+044E CYRILLIC SMALL LETTER YU
(def 'Cyrillic-a (hex "06c1")) # U+0430 CYRILLIC SMALL LETTER A
(def 'Cyrillic-be (hex "06c2")) # U+0431 CYRILLIC SMALL LETTER BE
(def 'Cyrillic-tse (hex "06c3")) # U+0446 CYRILLIC SMALL LETTER TSE
(def 'Cyrillic-de (hex "06c4")) # U+0434 CYRILLIC SMALL LETTER DE
(def 'Cyrillic-ie (hex "06c5")) # U+0435 CYRILLIC SMALL LETTER IE
(def 'Cyrillic-ef (hex "06c6")) # U+0444 CYRILLIC SMALL LETTER EF
(def 'Cyrillic-ghe (hex "06c7")) # U+0433 CYRILLIC SMALL LETTER GHE
(def 'Cyrillic-ha (hex "06c8")) # U+0445 CYRILLIC SMALL LETTER HA
(def 'Cyrillic-i (hex "06c9")) # U+0438 CYRILLIC SMALL LETTER I
(def 'Cyrillic-shorti (hex "06ca")) # U+0439 CYRILLIC SMALL LETTER SHORT I
(def 'Cyrillic-ka (hex "06cb")) # U+043A CYRILLIC SMALL LETTER KA
(def 'Cyrillic-el (hex "06cc")) # U+043B CYRILLIC SMALL LETTER EL
(def 'Cyrillic-em (hex "06cd")) # U+043C CYRILLIC SMALL LETTER EM
(def 'Cyrillic-en (hex "06ce")) # U+043D CYRILLIC SMALL LETTER EN
(def 'Cyrillic-o (hex "06cf")) # U+043E CYRILLIC SMALL LETTER O
(def 'Cyrillic-pe (hex "06d0")) # U+043F CYRILLIC SMALL LETTER PE
(def 'Cyrillic-ya (hex "06d1")) # U+044F CYRILLIC SMALL LETTER YA
(def 'Cyrillic-er (hex "06d2")) # U+0440 CYRILLIC SMALL LETTER ER
(def 'Cyrillic-es (hex "06d3")) # U+0441 CYRILLIC SMALL LETTER ES
(def 'Cyrillic-te (hex "06d4")) # U+0442 CYRILLIC SMALL LETTER TE
(def 'Cyrillic-u (hex "06d5")) # U+0443 CYRILLIC SMALL LETTER U
(def 'Cyrillic-zhe (hex "06d6")) # U+0436 CYRILLIC SMALL LETTER ZHE
(def 'Cyrillic-ve (hex "06d7")) # U+0432 CYRILLIC SMALL LETTER VE
(def 'Cyrillic-softsign (hex "06d8")) # U+044C CYRILLIC SMALL LETTER SOFT SIGN
(def 'Cyrillic-yeru (hex "06d9")) # U+044B CYRILLIC SMALL LETTER YERU
(def 'Cyrillic-ze (hex "06da")) # U+0437 CYRILLIC SMALL LETTER ZE
(def 'Cyrillic-sha (hex "06db")) # U+0448 CYRILLIC SMALL LETTER SHA
(def 'Cyrillic-e (hex "06dc")) # U+044D CYRILLIC SMALL LETTER E
(def 'Cyrillic-shcha (hex "06dd")) # U+0449 CYRILLIC SMALL LETTER SHCHA
(def 'Cyrillic-che (hex "06de")) # U+0447 CYRILLIC SMALL LETTER CHE
(def 'Cyrillic-hardsign (hex "06df")) # U+044A CYRILLIC SMALL LETTER HARD SIGN
(def 'Cyrillic-YU (hex "06e0")) # U+042E CYRILLIC CAPITAL LETTER YU
(def 'Cyrillic-A (hex "06e1")) # U+0410 CYRILLIC CAPITAL LETTER A
(def 'Cyrillic-BE (hex "06e2")) # U+0411 CYRILLIC CAPITAL LETTER BE
(def 'Cyrillic-TSE (hex "06e3")) # U+0426 CYRILLIC CAPITAL LETTER TSE
(def 'Cyrillic-DE (hex "06e4")) # U+0414 CYRILLIC CAPITAL LETTER DE
(def 'Cyrillic-IE (hex "06e5")) # U+0415 CYRILLIC CAPITAL LETTER IE
(def 'Cyrillic-EF (hex "06e6")) # U+0424 CYRILLIC CAPITAL LETTER EF
(def 'Cyrillic-GHE (hex "06e7")) # U+0413 CYRILLIC CAPITAL LETTER GHE
(def 'Cyrillic-HA (hex "06e8")) # U+0425 CYRILLIC CAPITAL LETTER HA
(def 'Cyrillic-I (hex "06e9")) # U+0418 CYRILLIC CAPITAL LETTER I
(def 'Cyrillic-SHORTI (hex "06ea")) # U+0419 CYRILLIC CAPITAL LETTER SHORT I
(def 'Cyrillic-KA (hex "06eb")) # U+041A CYRILLIC CAPITAL LETTER KA
(def 'Cyrillic-EL (hex "06ec")) # U+041B CYRILLIC CAPITAL LETTER EL
(def 'Cyrillic-EM (hex "06ed")) # U+041C CYRILLIC CAPITAL LETTER EM
(def 'Cyrillic-EN (hex "06ee")) # U+041D CYRILLIC CAPITAL LETTER EN
(def 'Cyrillic-O (hex "06ef")) # U+041E CYRILLIC CAPITAL LETTER O
(def 'Cyrillic-PE (hex "06f0")) # U+041F CYRILLIC CAPITAL LETTER PE
(def 'Cyrillic-YA (hex "06f1")) # U+042F CYRILLIC CAPITAL LETTER YA
(def 'Cyrillic-ER (hex "06f2")) # U+0420 CYRILLIC CAPITAL LETTER ER
(def 'Cyrillic-ES (hex "06f3")) # U+0421 CYRILLIC CAPITAL LETTER ES
(def 'Cyrillic-TE (hex "06f4")) # U+0422 CYRILLIC CAPITAL LETTER TE
(def 'Cyrillic-U (hex "06f5")) # U+0423 CYRILLIC CAPITAL LETTER U
(def 'Cyrillic-ZHE (hex "06f6")) # U+0416 CYRILLIC CAPITAL LETTER ZHE
(def 'Cyrillic-VE (hex "06f7")) # U+0412 CYRILLIC CAPITAL LETTER VE
(def 'Cyrillic-SOFTSIGN (hex "06f8")) # U+042C CYRILLIC CAPITAL LETTER SOFT SIGN
(def 'Cyrillic-YERU (hex "06f9")) # U+042B CYRILLIC CAPITAL LETTER YERU
(def 'Cyrillic-ZE (hex "06fa")) # U+0417 CYRILLIC CAPITAL LETTER ZE
(def 'Cyrillic-SHA (hex "06fb")) # U+0428 CYRILLIC CAPITAL LETTER SHA
(def 'Cyrillic-E (hex "06fc")) # U+042D CYRILLIC CAPITAL LETTER E
(def 'Cyrillic-SHCHA (hex "06fd")) # U+0429 CYRILLIC CAPITAL LETTER SHCHA
(def 'Cyrillic-CHE (hex "06fe")) # U+0427 CYRILLIC CAPITAL LETTER CHE
(def 'Cyrillic-HARDSIGN (hex "06ff")) # U+042A CYRILLIC CAPITAL LETTER HARD SIGN

#{
  Greek
  (based on an early draft of, and not quite identical to, ISO/IEC 8859-7)
  Byte 3 = 7
}#

(def 'Greek-ALPHAaccent (hex "07a1")) # U+0386 GREEK CAPITAL LETTER ALPHA WITH TONOS
(def 'Greek-EPSILONaccent (hex "07a2")) # U+0388 GREEK CAPITAL LETTER EPSILON WITH TONOS
(def 'Greek-ETAaccent (hex "07a3")) # U+0389 GREEK CAPITAL LETTER ETA WITH TONOS
(def 'Greek-IOTAaccent (hex "07a4")) # U+038A GREEK CAPITAL LETTER IOTA WITH TONOS
(def 'Greek-IOTAdieresis (hex "07a5")) # U+03AA GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
(def 'Greek-IOTAdiaeresis (hex "07a5")) # old typo
(def 'Greek-OMICRONaccent (hex "07a7")) # U+038C GREEK CAPITAL LETTER OMICRON WITH TONOS
(def 'Greek-UPSILONaccent (hex "07a8")) # U+038E GREEK CAPITAL LETTER UPSILON WITH TONOS
(def 'Greek-UPSILONdieresis (hex "07a9")) # U+03AB GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
(def 'Greek-OMEGAaccent (hex "07ab")) # U+038F GREEK CAPITAL LETTER OMEGA WITH TONOS
(def 'Greek-accentdieresis (hex "07ae")) # U+0385 GREEK DIALYTIKA TONOS
(def 'Greek-horizbar (hex "07af")) # U+2015 HORIZONTAL BAR
(def 'Greek-alphaaccent (hex "07b1")) # U+03AC GREEK SMALL LETTER ALPHA WITH TONOS
(def 'Greek-epsilonaccent (hex "07b2")) # U+03AD GREEK SMALL LETTER EPSILON WITH TONOS
(def 'Greek-etaaccent (hex "07b3")) # U+03AE GREEK SMALL LETTER ETA WITH TONOS
(def 'Greek-iotaaccent (hex "07b4")) # U+03AF GREEK SMALL LETTER IOTA WITH TONOS
(def 'Greek-iotadieresis (hex "07b5")) # U+03CA GREEK SMALL LETTER IOTA WITH DIALYTIKA
(def 'Greek-iotaaccentdieresis (hex "07b6")) # U+0390 GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
(def 'Greek-omicronaccent (hex "07b7")) # U+03CC GREEK SMALL LETTER OMICRON WITH TONOS
(def 'Greek-upsilonaccent (hex "07b8")) # U+03CD GREEK SMALL LETTER UPSILON WITH TONOS
(def 'Greek-upsilondieresis (hex "07b9")) # U+03CB GREEK SMALL LETTER UPSILON WITH DIALYTIKA
(def 'Greek-upsilonaccentdieresis (hex "07ba")) # U+03B0 GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
(def 'Greek-omegaaccent (hex "07bb")) # U+03CE GREEK SMALL LETTER OMEGA WITH TONOS
(def 'Greek-ALPHA (hex "07c1")) # U+0391 GREEK CAPITAL LETTER ALPHA
(def 'Greek-BETA (hex "07c2")) # U+0392 GREEK CAPITAL LETTER BETA
(def 'Greek-GAMMA (hex "07c3")) # U+0393 GREEK CAPITAL LETTER GAMMA
(def 'Greek-DELTA (hex "07c4")) # U+0394 GREEK CAPITAL LETTER DELTA
(def 'Greek-EPSILON (hex "07c5")) # U+0395 GREEK CAPITAL LETTER EPSILON
(def 'Greek-ZETA (hex "07c6")) # U+0396 GREEK CAPITAL LETTER ZETA
(def 'Greek-ETA (hex "07c7")) # U+0397 GREEK CAPITAL LETTER ETA
(def 'Greek-THETA (hex "07c8")) # U+0398 GREEK CAPITAL LETTER THETA
(def 'Greek-IOTA (hex "07c9")) # U+0399 GREEK CAPITAL LETTER IOTA
(def 'Greek-KAPPA (hex "07ca")) # U+039A GREEK CAPITAL LETTER KAPPA
(def 'Greek-LAMDA (hex "07cb")) # U+039B GREEK CAPITAL LETTER LAMDA
(def 'Greek-LAMBDA (hex "07cb")) # U+039B GREEK CAPITAL LETTER LAMDA
(def 'Greek-MU (hex "07cc")) # U+039C GREEK CAPITAL LETTER MU
(def 'Greek-NU (hex "07cd")) # U+039D GREEK CAPITAL LETTER NU
(def 'Greek-XI (hex "07ce")) # U+039E GREEK CAPITAL LETTER XI
(def 'Greek-OMICRON (hex "07cf")) # U+039F GREEK CAPITAL LETTER OMICRON
(def 'Greek-PI (hex "07d0")) # U+03A0 GREEK CAPITAL LETTER PI
(def 'Greek-RHO (hex "07d1")) # U+03A1 GREEK CAPITAL LETTER RHO
(def 'Greek-SIGMA (hex "07d2")) # U+03A3 GREEK CAPITAL LETTER SIGMA
(def 'Greek-TAU (hex "07d4")) # U+03A4 GREEK CAPITAL LETTER TAU
(def 'Greek-UPSILON (hex "07d5")) # U+03A5 GREEK CAPITAL LETTER UPSILON
(def 'Greek-PHI (hex "07d6")) # U+03A6 GREEK CAPITAL LETTER PHI
(def 'Greek-CHI (hex "07d7")) # U+03A7 GREEK CAPITAL LETTER CHI
(def 'Greek-PSI (hex "07d8")) # U+03A8 GREEK CAPITAL LETTER PSI
(def 'Greek-OMEGA (hex "07d9")) # U+03A9 GREEK CAPITAL LETTER OMEGA
(def 'Greek-alpha (hex "07e1")) # U+03B1 GREEK SMALL LETTER ALPHA
(def 'Greek-beta (hex "07e2")) # U+03B2 GREEK SMALL LETTER BETA
(def 'Greek-gamma (hex "07e3")) # U+03B3 GREEK SMALL LETTER GAMMA
(def 'Greek-delta (hex "07e4")) # U+03B4 GREEK SMALL LETTER DELTA
(def 'Greek-epsilon (hex "07e5")) # U+03B5 GREEK SMALL LETTER EPSILON
(def 'Greek-zeta (hex "07e6")) # U+03B6 GREEK SMALL LETTER ZETA
(def 'Greek-eta (hex "07e7")) # U+03B7 GREEK SMALL LETTER ETA
(def 'Greek-theta (hex "07e8")) # U+03B8 GREEK SMALL LETTER THETA
(def 'Greek-iota (hex "07e9")) # U+03B9 GREEK SMALL LETTER IOTA
(def 'Greek-kappa (hex "07ea")) # U+03BA GREEK SMALL LETTER KAPPA
(def 'Greek-lamda (hex "07eb")) # U+03BB GREEK SMALL LETTER LAMDA
(def 'Greek-lambda (hex "07eb")) # U+03BB GREEK SMALL LETTER LAMDA
(def 'Greek-mu (hex "07ec")) # U+03BC GREEK SMALL LETTER MU
(def 'Greek-nu (hex "07ed")) # U+03BD GREEK SMALL LETTER NU
(def 'Greek-xi (hex "07ee")) # U+03BE GREEK SMALL LETTER XI
(def 'Greek-omicron (hex "07ef")) # U+03BF GREEK SMALL LETTER OMICRON
(def 'Greek-pi (hex "07f0")) # U+03C0 GREEK SMALL LETTER PI
(def 'Greek-rho (hex "07f1")) # U+03C1 GREEK SMALL LETTER RHO
(def 'Greek-sigma (hex "07f2")) # U+03C3 GREEK SMALL LETTER SIGMA
(def 'Greek-finalsmallsigma (hex "07f3")) # U+03C2 GREEK SMALL LETTER FINAL SIGMA
(def 'Greek-tau (hex "07f4")) # U+03C4 GREEK SMALL LETTER TAU
(def 'Greek-upsilon (hex "07f5")) # U+03C5 GREEK SMALL LETTER UPSILON
(def 'Greek-phi (hex "07f6")) # U+03C6 GREEK SMALL LETTER PHI
(def 'Greek-chi (hex "07f7")) # U+03C7 GREEK SMALL LETTER CHI
(def 'Greek-psi (hex "07f8")) # U+03C8 GREEK SMALL LETTER PSI
(def 'Greek-omega (hex "07f9")) # U+03C9 GREEK SMALL LETTER OMEGA
(def 'Greek-switch (hex "ff7e")) # Alias for mode_switch

#{
  Technical
  (from the DEC VT330/VT420 Technical Character Set, http://vt100.net/charsets/technical.html)
  Byte 3 = 8
}#

(def 'leftradical (hex "08a1")) # U+23B7 RADICAL SYMBOL BOTTOM
(def 'topleftradical (hex "08a2")) #(U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT)
(def 'horizconnector (hex "08a3")) #(U+2500 BOX DRAWINGS LIGHT HORIZONTAL)
(def 'topintegral (hex "08a4")) # U+2320 TOP HALF INTEGRAL
(def 'botintegral (hex "08a5")) # U+2321 BOTTOM HALF INTEGRAL
(def 'vertconnector (hex "08a6")) #(U+2502 BOX DRAWINGS LIGHT VERTICAL)
(def 'topleftsqbracket (hex "08a7")) # U+23A1 LEFT SQUARE BRACKET UPPER CORNER
(def 'botleftsqbracket (hex "08a8")) # U+23A3 LEFT SQUARE BRACKET LOWER CORNER
(def 'toprightsqbracket (hex "08a9")) # U+23A4 RIGHT SQUARE BRACKET UPPER CORNER
(def 'botrightsqbracket (hex "08aa")) # U+23A6 RIGHT SQUARE BRACKET LOWER CORNER
(def 'topleftparens (hex "08ab")) # U+239B LEFT PARENTHESIS UPPER HOOK
(def 'botleftparens (hex "08ac")) # U+239D LEFT PARENTHESIS LOWER HOOK
(def 'toprightparens (hex "08ad")) # U+239E RIGHT PARENTHESIS UPPER HOOK
(def 'botrightparens (hex "08ae")) # U+23A0 RIGHT PARENTHESIS LOWER HOOK
(def 'leftmiddlecurlybrace (hex "08af")) # U+23A8 LEFT CURLY BRACKET MIDDLE PIECE
(def 'rightmiddlecurlybrace (hex "08b0")) # U+23AC RIGHT CURLY BRACKET MIDDLE PIECE
(def 'topleftsummation (hex "08b1"))
(def 'botleftsummation (hex "08b2"))
(def 'topvertsummationconnector (hex "08b3"))
(def 'botvertsummationconnector (hex "08b4"))
(def 'toprightsummation (hex "08b5"))
(def 'botrightsummation (hex "08b6"))
(def 'rightmiddlesummation (hex "08b7"))
(def 'lessthanequal (hex "08bc")) # U+2264 LESS-THAN OR EQUAL TO
(def 'notequal (hex "08bd")) # U+2260 NOT EQUAL TO
(def 'greaterthanequal (hex "08be")) # U+2265 GREATER-THAN OR EQUAL TO
(def 'integral (hex "08bf")) # U+222B INTEGRAL
(def 'therefore (hex "08c0")) # U+2234 THEREFORE
(def 'variation (hex "08c1")) # U+221D PROPORTIONAL TO
(def 'infinity (hex "08c2")) # U+221E INFINITY
(def 'nabla (hex "08c5")) # U+2207 NABLA
(def 'approximate (hex "08c8")) # U+223C TILDE OPERATOR
(def 'similarequal (hex "08c9")) # U+2243 ASYMPTOTICALLY EQUAL TO
(def 'ifonlyif (hex "08cd")) # U+21D4 LEFT RIGHT DOUBLE ARROW
(def 'implies (hex "08ce")) # U+21D2 RIGHTWARDS DOUBLE ARROW
(def 'identical (hex "08cf")) # U+2261 IDENTICAL TO
(def 'radical (hex "08d6")) # U+221A SQUARE ROOT
(def 'includedin (hex "08da")) # U+2282 SUBSET OF
(def 'includes (hex "08db")) # U+2283 SUPERSET OF
(def 'intersection (hex "08dc")) # U+2229 INTERSECTION
(def 'union (hex "08dd")) # U+222A UNION
(def 'logicaland (hex "08de")) # U+2227 LOGICAL AND
(def 'logicalor (hex "08df")) # U+2228 LOGICAL OR
(def 'partialderivative (hex "08ef")) # U+2202 PARTIAL DIFFERENTIAL
(def 'function (hex "08f6")) # U+0192 LATIN SMALL LETTER F WITH HOOK
(def 'leftarrow (hex "08fb")) # U+2190 LEFTWARDS ARROW
(def 'uparrow (hex "08fc")) # U+2191 UPWARDS ARROW
(def 'rightarrow (hex "08fd")) # U+2192 RIGHTWARDS ARROW
(def 'downarrow (hex "08fe")) # U+2193 DOWNWARDS ARROW

#{
  Special
  (from the DEC VT100 Special Graphics Character Set)
  Byte 3 = 9
}#

(def 'blank (hex "09df"))
(def 'soliddiamond (hex "09e0")) # U+25C6 BLACK DIAMOND
(def 'checkerboard (hex "09e1")) # U+2592 MEDIUM SHADE
(def 'ht (hex "09e2")) # U+2409 SYMBOL FOR HORIZONTAL TABULATION
(def 'ff (hex "09e3")) # U+240C SYMBOL FOR FORM FEED
(def 'cr (hex "09e4")) # U+240D SYMBOL FOR CARRIAGE RETURN
(def 'lf (hex "09e5")) # U+240A SYMBOL FOR LINE FEED
(def 'nl (hex "09e8")) # U+2424 SYMBOL FOR NEWLINE
(def 'vt (hex "09e9")) # U+240B SYMBOL FOR VERTICAL TABULATION
(def 'lowrightcorner (hex "09ea")) # U+2518 BOX DRAWINGS LIGHT UP AND LEFT
(def 'uprightcorner (hex "09eb")) # U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT
(def 'upleftcorner (hex "09ec")) # U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT
(def 'lowleftcorner (hex "09ed")) # U+2514 BOX DRAWINGS LIGHT UP AND RIGHT
(def 'crossinglines (hex "09ee")) # U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
(def 'horizlinescan1 (hex "09ef")) # U+23BA HORIZONTAL SCAN LINE-1
(def 'horizlinescan3 (hex "09f0")) # U+23BB HORIZONTAL SCAN LINE-3
(def 'horizlinescan5 (hex "09f1")) # U+2500 BOX DRAWINGS LIGHT HORIZONTAL
(def 'horizlinescan7 (hex "09f2")) # U+23BC HORIZONTAL SCAN LINE-7
(def 'horizlinescan9 (hex "09f3")) # U+23BD HORIZONTAL SCAN LINE-9
(def 'leftt (hex "09f4")) # U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT
(def 'rightt (hex "09f5")) # U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT
(def 'bott (hex "09f6")) # U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL
(def 'topt (hex "09f7")) # U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
(def 'vertbar (hex "09f8")) # U+2502 BOX DRAWINGS LIGHT VERTICAL

#{
  Publishing
  (these are probably from a long forgotten DEC Publishing
  font that once shipped with DECwrite)
  Byte 3 = 0x0a
}#

(def 'emspace (hex "0aa1")) # U+2003 EM SPACE
(def 'enspace (hex "0aa2")) # U+2002 EN SPACE
(def 'em3space (hex "0aa3")) # U+2004 THREE-PER-EM SPACE
(def 'em4space (hex "0aa4")) # U+2005 FOUR-PER-EM SPACE
(def 'digitspace (hex "0aa5")) # U+2007 FIGURE SPACE
(def 'punctspace (hex "0aa6")) # U+2008 PUNCTUATION SPACE
(def 'thinspace (hex "0aa7")) # U+2009 THIN SPACE
(def 'hairspace (hex "0aa8")) # U+200A HAIR SPACE
(def 'emdash (hex "0aa9")) # U+2014 EM DASH
(def 'endash (hex "0aaa")) # U+2013 EN DASH
(def 'signifblank (hex "0aac")) #(U+2423 OPEN BOX)
(def 'ellipsis (hex "0aae")) # U+2026 HORIZONTAL ELLIPSIS
(def 'doubbaselinedot (hex "0aaf")) # U+2025 TWO DOT LEADER
(def 'onethird (hex "0ab0")) # U+2153 VULGAR FRACTION ONE THIRD
(def 'twothirds (hex "0ab1")) # U+2154 VULGAR FRACTION TWO THIRDS
(def 'onefifth (hex "0ab2")) # U+2155 VULGAR FRACTION ONE FIFTH
(def 'twofifths (hex "0ab3")) # U+2156 VULGAR FRACTION TWO FIFTHS
(def 'threefifths (hex "0ab4")) # U+2157 VULGAR FRACTION THREE FIFTHS
(def 'fourfifths (hex "0ab5")) # U+2158 VULGAR FRACTION FOUR FIFTHS
(def 'onesixth (hex "0ab6")) # U+2159 VULGAR FRACTION ONE SIXTH
(def 'fivesixths (hex "0ab7")) # U+215A VULGAR FRACTION FIVE SIXTHS
(def 'careof (hex "0ab8")) # U+2105 CARE OF
(def 'figdash (hex "0abb")) # U+2012 FIGURE DASH
(def 'leftanglebracket (hex "0abc")) #(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
(def 'decimalpoint (hex "0abd")) #(U+002E FULL STOP)
(def 'rightanglebracket (hex "0abe")) #(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
(def 'marker (hex "0abf"))
(def 'oneeighth (hex "0ac3")) # U+215B VULGAR FRACTION ONE EIGHTH
(def 'threeeighths (hex "0ac4")) # U+215C VULGAR FRACTION THREE EIGHTHS
(def 'fiveeighths (hex "0ac5")) # U+215D VULGAR FRACTION FIVE EIGHTHS
(def 'seveneighths (hex "0ac6")) # U+215E VULGAR FRACTION SEVEN EIGHTHS
(def 'trademark (hex "0ac9")) # U+2122 TRADE MARK SIGN
(def 'signaturemark (hex "0aca")) #(U+2613 SALTIRE)
(def 'trademarkincircle (hex "0acb"))
(def 'leftopentriangle (hex "0acc")) #(U+25C1 WHITE LEFT-POINTING TRIANGLE)
(def 'rightopentriangle (hex "0acd")) #(U+25B7 WHITE RIGHT-POINTING TRIANGLE)
(def 'emopencircle (hex "0ace")) #(U+25CB WHITE CIRCLE)
(def 'emopenrectangle (hex "0acf")) #(U+25AF WHITE VERTICAL RECTANGLE)
(def 'leftsinglequotemark (hex "0ad0")) # U+2018 LEFT SINGLE QUOTATION MARK
(def 'rightsinglequotemark (hex "0ad1")) # U+2019 RIGHT SINGLE QUOTATION MARK
(def 'leftdoublequotemark (hex "0ad2")) # U+201C LEFT DOUBLE QUOTATION MARK
(def 'rightdoublequotemark (hex "0ad3")) # U+201D RIGHT DOUBLE QUOTATION MARK
(def 'prescription (hex "0ad4")) # U+211E PRESCRIPTION TAKE
(def 'permille (hex "0ad5")) # U+2030 PER MILLE SIGN
(def 'minutes (hex "0ad6")) # U+2032 PRIME
(def 'seconds (hex "0ad7")) # U+2033 DOUBLE PRIME
(def 'latincross (hex "0ad9")) # U+271D LATIN CROSS
(def 'hexagram (hex "0ada"))
(def 'filledrectbullet (hex "0adb")) #(U+25AC BLACK RECTANGLE)
(def 'filledlefttribullet (hex "0adc")) #(U+25C0 BLACK LEFT-POINTING TRIANGLE)
(def 'filledrighttribullet (hex "0add")) #(U+25B6 BLACK RIGHT-POINTING TRIANGLE)
(def 'emfilledcircle (hex "0ade")) #(U+25CF BLACK CIRCLE)
(def 'emfilledrect (hex "0adf")) #(U+25AE BLACK VERTICAL RECTANGLE)
(def 'enopencircbullet (hex "0ae0")) #(U+25E6 WHITE BULLET)
(def 'enopensquarebullet (hex "0ae1")) #(U+25AB WHITE SMALL SQUARE)
(def 'openrectbullet (hex "0ae2")) #(U+25AD WHITE RECTANGLE)
(def 'opentribulletup (hex "0ae3")) #(U+25B3 WHITE UP-POINTING TRIANGLE)
(def 'opentribulletdown (hex "0ae4")) #(U+25BD WHITE DOWN-POINTING TRIANGLE)
(def 'openstar (hex "0ae5")) #(U+2606 WHITE STAR)
(def 'enfilledcircbullet (hex "0ae6")) #(U+2022 BULLET)
(def 'enfilledsqbullet (hex "0ae7")) #(U+25AA BLACK SMALL SQUARE)
(def 'filledtribulletup (hex "0ae8")) #(U+25B2 BLACK UP-POINTING TRIANGLE)
(def 'filledtribulletdown (hex "0ae9")) #(U+25BC BLACK DOWN-POINTING TRIANGLE)
(def 'leftpointer (hex "0aea")) #(U+261C WHITE LEFT POINTING INDEX)
(def 'rightpointer (hex "0aeb")) #(U+261E WHITE RIGHT POINTING INDEX)
(def 'club (hex "0aec")) # U+2663 BLACK CLUB SUIT
(def 'diamond (hex "0aed")) # U+2666 BLACK DIAMOND SUIT
(def 'heart (hex "0aee")) # U+2665 BLACK HEART SUIT
(def 'maltesecross (hex "0af0")) # U+2720 MALTESE CROSS
(def 'dagger (hex "0af1")) # U+2020 DAGGER
(def 'doubledagger (hex "0af2")) # U+2021 DOUBLE DAGGER
(def 'checkmark (hex "0af3")) # U+2713 CHECK MARK
(def 'ballotcross (hex "0af4")) # U+2717 BALLOT X
(def 'musicalsharp (hex "0af5")) # U+266F MUSIC SHARP SIGN
(def 'musicalflat (hex "0af6")) # U+266D MUSIC FLAT SIGN
(def 'malesymbol (hex "0af7")) # U+2642 MALE SIGN
(def 'femalesymbol (hex "0af8")) # U+2640 FEMALE SIGN
(def 'telephone (hex "0af9")) # U+260E BLACK TELEPHONE
(def 'telephonerecorder (hex "0afa")) # U+2315 TELEPHONE RECORDER
(def 'phonographcopyright (hex "0afb")) # U+2117 SOUND RECORDING COPYRIGHT
(def 'caret (hex "0afc")) # U+2038 CARET
(def 'singlelowquotemark (hex "0afd")) # U+201A SINGLE LOW-9 QUOTATION MARK
(def 'doublelowquotemark (hex "0afe")) # U+201E DOUBLE LOW-9 QUOTATION MARK
(def 'cursor (hex "0aff"))

#{
  APL
  Byte 3 = 0x0b
}#

(def 'leftcaret (hex "0ba3")) #(U+003C LESS-THAN SIGN)
(def 'rightcaret (hex "0ba6")) #(U+003E GREATER-THAN SIGN)
(def 'downcaret (hex "0ba8")) #(U+2228 LOGICAL OR)
(def 'upcaret (hex "0ba9")) #(U+2227 LOGICAL AND)
(def 'overbar (hex "0bc0")) #(U+00AF MACRON)
(def 'downtack (hex "0bc2")) # U+22A4 DOWN TACK
(def 'upshoe (hex "0bc3")) #(U+2229 INTERSECTION)
(def 'downstile (hex "0bc4")) # U+230A LEFT FLOOR
(def 'underbar (hex "0bc6")) #(U+005F LOW LINE)
(def 'jot (hex "0bca")) # U+2218 RING OPERATOR
(def 'quad (hex "0bcc")) # U+2395 APL FUNCTIONAL SYMBOL QUAD
(def 'uptack (hex "0bce")) # U+22A5 UP TACK
(def 'circle (hex "0bcf")) # U+25CB WHITE CIRCLE
(def 'upstile (hex "0bd3")) # U+2308 LEFT CEILING
(def 'downshoe (hex "0bd6")) #(U+222A UNION)
(def 'rightshoe (hex "0bd8")) #(U+2283 SUPERSET OF)
(def 'leftshoe (hex "0bda")) #(U+2282 SUBSET OF)
(def 'lefttack (hex "0bdc")) # U+22A3 LEFT TACK
(def 'righttack (hex "0bfc")) # U+22A2 RIGHT TACK

#{
  Hebrew
  Byte 3 = 0x0c
}#

(def 'hebrew-doublelowline (hex "0cdf")) # U+2017 DOUBLE LOW LINE
(def 'hebrew-aleph (hex "0ce0")) # U+05D0 HEBREW LETTER ALEF
(def 'hebrew-bet (hex "0ce1")) # U+05D1 HEBREW LETTER BET
(def 'hebrew-beth (hex "0ce1")) # deprecated
(def 'hebrew-gimel (hex "0ce2")) # U+05D2 HEBREW LETTER GIMEL
(def 'hebrew-gimmel (hex "0ce2")) # deprecated
(def 'hebrew-dalet (hex "0ce3")) # U+05D3 HEBREW LETTER DALET
(def 'hebrew-daleth (hex "0ce3")) # deprecated
(def 'hebrew-he (hex "0ce4")) # U+05D4 HEBREW LETTER HE
(def 'hebrew-waw (hex "0ce5")) # U+05D5 HEBREW LETTER VAV
(def 'hebrew-zain (hex "0ce6")) # U+05D6 HEBREW LETTER ZAYIN
(def 'hebrew-zayin (hex "0ce6")) # deprecated
(def 'hebrew-chet (hex "0ce7")) # U+05D7 HEBREW LETTER HET
(def 'hebrew-het (hex "0ce7")) # deprecated
(def 'hebrew-tet (hex "0ce8")) # U+05D8 HEBREW LETTER TET
(def 'hebrew-teth (hex "0ce8")) # deprecated
(def 'hebrew-yod (hex "0ce9")) # U+05D9 HEBREW LETTER YOD
(def 'hebrew-finalkaph (hex "0cea")) # U+05DA HEBREW LETTER FINAL KAF
(def 'hebrew-kaph (hex "0ceb")) # U+05DB HEBREW LETTER KAF
(def 'hebrew-lamed (hex "0cec")) # U+05DC HEBREW LETTER LAMED
(def 'hebrew-finalmem (hex "0ced")) # U+05DD HEBREW LETTER FINAL MEM
(def 'hebrew-mem (hex "0cee")) # U+05DE HEBREW LETTER MEM
(def 'hebrew-finalnun (hex "0cef")) # U+05DF HEBREW LETTER FINAL NUN
(def 'hebrew-nun (hex "0cf0")) # U+05E0 HEBREW LETTER NUN
(def 'hebrew-samech (hex "0cf1")) # U+05E1 HEBREW LETTER SAMEKH
(def 'hebrew-samekh (hex "0cf1")) # deprecated
(def 'hebrew-ayin (hex "0cf2")) # U+05E2 HEBREW LETTER AYIN
(def 'hebrew-finalpe (hex "0cf3")) # U+05E3 HEBREW LETTER FINAL PE
(def 'hebrew-pe (hex "0cf4")) # U+05E4 HEBREW LETTER PE
(def 'hebrew-finalzade (hex "0cf5")) # U+05E5 HEBREW LETTER FINAL TSADI
(def 'hebrew-finalzadi (hex "0cf5")) # deprecated
(def 'hebrew-zade (hex "0cf6")) # U+05E6 HEBREW LETTER TSADI
(def 'hebrew-zadi (hex "0cf6")) # deprecated
(def 'hebrew-qoph (hex "0cf7")) # U+05E7 HEBREW LETTER QOF
(def 'hebrew-kuf (hex "0cf7")) # deprecated
(def 'hebrew-resh (hex "0cf8")) # U+05E8 HEBREW LETTER RESH
(def 'hebrew-shin (hex "0cf9")) # U+05E9 HEBREW LETTER SHIN
(def 'hebrew-taw (hex "0cfa")) # U+05EA HEBREW LETTER TAV
(def 'hebrew-taf (hex "0cfa")) # deprecated
(def 'Hebrew-switch (hex "ff7e")) # Alias for mode_switch

#{
  Thai
  Byte 3 = 0x0d
}#

(def 'Thai-kokai (hex "0da1")) # U+0E01 THAI CHARACTER KO KAI
(def 'Thai-khokhai (hex "0da2")) # U+0E02 THAI CHARACTER KHO KHAI
(def 'Thai-khokhuat (hex "0da3")) # U+0E03 THAI CHARACTER KHO KHUAT
(def 'Thai-khokhwai (hex "0da4")) # U+0E04 THAI CHARACTER KHO KHWAI
(def 'Thai-khokhon (hex "0da5")) # U+0E05 THAI CHARACTER KHO KHON
(def 'Thai-khorakhang (hex "0da6")) # U+0E06 THAI CHARACTER KHO RAKHANG
(def 'Thai-ngongu (hex "0da7")) # U+0E07 THAI CHARACTER NGO NGU
(def 'Thai-chochan (hex "0da8")) # U+0E08 THAI CHARACTER CHO CHAN
(def 'Thai-choching (hex "0da9")) # U+0E09 THAI CHARACTER CHO CHING
(def 'Thai-chochang (hex "0daa")) # U+0E0A THAI CHARACTER CHO CHANG
(def 'Thai-soso (hex "0dab")) # U+0E0B THAI CHARACTER SO SO
(def 'Thai-chochoe (hex "0dac")) # U+0E0C THAI CHARACTER CHO CHOE
(def 'Thai-yoying (hex "0dad")) # U+0E0D THAI CHARACTER YO YING
(def 'Thai-dochada (hex "0dae")) # U+0E0E THAI CHARACTER DO CHADA
(def 'Thai-topatak (hex "0daf")) # U+0E0F THAI CHARACTER TO PATAK
(def 'Thai-thothan (hex "0db0")) # U+0E10 THAI CHARACTER THO THAN
(def 'Thai-thonangmontho (hex "0db1")) # U+0E11 THAI CHARACTER THO NANGMONTHO
(def 'Thai-thophuthao (hex "0db2")) # U+0E12 THAI CHARACTER THO PHUTHAO
(def 'Thai-nonen (hex "0db3")) # U+0E13 THAI CHARACTER NO NEN
(def 'Thai-dodek (hex "0db4")) # U+0E14 THAI CHARACTER DO DEK
(def 'Thai-totao (hex "0db5")) # U+0E15 THAI CHARACTER TO TAO
(def 'Thai-thothung (hex "0db6")) # U+0E16 THAI CHARACTER THO THUNG
(def 'Thai-thothahan (hex "0db7")) # U+0E17 THAI CHARACTER THO THAHAN
(def 'Thai-thothong (hex "0db8")) # U+0E18 THAI CHARACTER THO THONG
(def 'Thai-nonu (hex "0db9")) # U+0E19 THAI CHARACTER NO NU
(def 'Thai-bobaimai (hex "0dba")) # U+0E1A THAI CHARACTER BO BAIMAI
(def 'Thai-popla (hex "0dbb")) # U+0E1B THAI CHARACTER PO PLA
(def 'Thai-phophung (hex "0dbc")) # U+0E1C THAI CHARACTER PHO PHUNG
(def 'Thai-fofa (hex "0dbd")) # U+0E1D THAI CHARACTER FO FA
(def 'Thai-phophan (hex "0dbe")) # U+0E1E THAI CHARACTER PHO PHAN
(def 'Thai-fofan (hex "0dbf")) # U+0E1F THAI CHARACTER FO FAN
(def 'Thai-phosamphao (hex "0dc0")) # U+0E20 THAI CHARACTER PHO SAMPHAO
(def 'Thai-moma (hex "0dc1")) # U+0E21 THAI CHARACTER MO MA
(def 'Thai-yoyak (hex "0dc2")) # U+0E22 THAI CHARACTER YO YAK
(def 'Thai-rorua (hex "0dc3")) # U+0E23 THAI CHARACTER RO RUA
(def 'Thai-ru (hex "0dc4")) # U+0E24 THAI CHARACTER RU
(def 'Thai-loling (hex "0dc5")) # U+0E25 THAI CHARACTER LO LING
(def 'Thai-lu (hex "0dc6")) # U+0E26 THAI CHARACTER LU
(def 'Thai-wowaen (hex "0dc7")) # U+0E27 THAI CHARACTER WO WAEN
(def 'Thai-sosala (hex "0dc8")) # U+0E28 THAI CHARACTER SO SALA
(def 'Thai-sorusi (hex "0dc9")) # U+0E29 THAI CHARACTER SO RUSI
(def 'Thai-sosua (hex "0dca")) # U+0E2A THAI CHARACTER SO SUA
(def 'Thai-hohip (hex "0dcb")) # U+0E2B THAI CHARACTER HO HIP
(def 'Thai-lochula (hex "0dcc")) # U+0E2C THAI CHARACTER LO CHULA
(def 'Thai-oang (hex "0dcd")) # U+0E2D THAI CHARACTER O ANG
(def 'Thai-honokhuk (hex "0dce")) # U+0E2E THAI CHARACTER HO NOKHUK
(def 'Thai-paiyannoi (hex "0dcf")) # U+0E2F THAI CHARACTER PAIYANNOI
(def 'Thai-saraa (hex "0dd0")) # U+0E30 THAI CHARACTER SARA A
(def 'Thai-maihanakat (hex "0dd1")) # U+0E31 THAI CHARACTER MAI HAN-AKAT
(def 'Thai-saraaa (hex "0dd2")) # U+0E32 THAI CHARACTER SARA AA
(def 'Thai-saraam (hex "0dd3")) # U+0E33 THAI CHARACTER SARA AM
(def 'Thai-sarai (hex "0dd4")) # U+0E34 THAI CHARACTER SARA I
(def 'Thai-saraii (hex "0dd5")) # U+0E35 THAI CHARACTER SARA II
(def 'Thai-saraue (hex "0dd6")) # U+0E36 THAI CHARACTER SARA UE
(def 'Thai-sarauee (hex "0dd7")) # U+0E37 THAI CHARACTER SARA UEE
(def 'Thai-sarau (hex "0dd8")) # U+0E38 THAI CHARACTER SARA U
(def 'Thai-sarauu (hex "0dd9")) # U+0E39 THAI CHARACTER SARA UU
(def 'Thai-phinthu (hex "0dda")) # U+0E3A THAI CHARACTER PHINTHU
(def 'Thai-maihanakat-maitho (hex "0dde"))
(def 'Thai-baht (hex "0ddf")) # U+0E3F THAI CURRENCY SYMBOL BAHT
(def 'Thai-sarae (hex "0de0")) # U+0E40 THAI CHARACTER SARA E
(def 'Thai-saraae (hex "0de1")) # U+0E41 THAI CHARACTER SARA AE
(def 'Thai-sarao (hex "0de2")) # U+0E42 THAI CHARACTER SARA O
(def 'Thai-saraaimaimuan (hex "0de3")) # U+0E43 THAI CHARACTER SARA AI MAIMUAN
(def 'Thai-saraaimaimalai (hex "0de4")) # U+0E44 THAI CHARACTER SARA AI MAIMALAI
(def 'Thai-lakkhangyao (hex "0de5")) # U+0E45 THAI CHARACTER LAKKHANGYAO
(def 'Thai-maiyamok (hex "0de6")) # U+0E46 THAI CHARACTER MAIYAMOK
(def 'Thai-maitaikhu (hex "0de7")) # U+0E47 THAI CHARACTER MAITAIKHU
(def 'Thai-maiek (hex "0de8")) # U+0E48 THAI CHARACTER MAI EK
(def 'Thai-maitho (hex "0de9")) # U+0E49 THAI CHARACTER MAI THO
(def 'Thai-maitri (hex "0dea")) # U+0E4A THAI CHARACTER MAI TRI
(def 'Thai-maichattawa (hex "0deb")) # U+0E4B THAI CHARACTER MAI CHATTAWA
(def 'Thai-thanthakhat (hex "0dec")) # U+0E4C THAI CHARACTER THANTHAKHAT
(def 'Thai-nikhahit (hex "0ded")) # U+0E4D THAI CHARACTER NIKHAHIT
(def 'Thai-leksun (hex "0df0")) # U+0E50 THAI DIGIT ZERO
(def 'Thai-leknung (hex "0df1")) # U+0E51 THAI DIGIT ONE
(def 'Thai-leksong (hex "0df2")) # U+0E52 THAI DIGIT TWO
(def 'Thai-leksam (hex "0df3")) # U+0E53 THAI DIGIT THREE
(def 'Thai-leksi (hex "0df4")) # U+0E54 THAI DIGIT FOUR
(def 'Thai-lekha (hex "0df5")) # U+0E55 THAI DIGIT FIVE
(def 'Thai-lekhok (hex "0df6")) # U+0E56 THAI DIGIT SIX
(def 'Thai-lekchet (hex "0df7")) # U+0E57 THAI DIGIT SEVEN
(def 'Thai-lekpaet (hex "0df8")) # U+0E58 THAI DIGIT EIGHT
(def 'Thai-lekkao (hex "0df9")) # U+0E59 THAI DIGIT NINE

#{
  Korean
  Byte 3 = 0x0e
}#


(def 'Hangul (hex "ff31")) # Hangul start/stop(toggle)
(def 'Hangul-Start (hex "ff32")) # Hangul start
(def 'Hangul-End (hex "ff33")) # Hangul end, English start
(def 'Hangul-Hanja (hex "ff34")) # Start Hangul->Hanja Conversion
(def 'Hangul-Jamo (hex "ff35")) # Hangul Jamo mode
(def 'Hangul-Romaja (hex "ff36")) # Hangul Romaja mode
(def 'Hangul-Codeinput (hex "ff37")) # Hangul code input mode
(def 'Hangul-Jeonja (hex "ff38")) # Jeonja mode
(def 'Hangul-Banja (hex "ff39")) # Banja mode
(def 'Hangul-PreHanja (hex "ff3a")) # Pre Hanja conversion
(def 'Hangul-PostHanja (hex "ff3b")) # Post Hanja conversion
(def 'Hangul-SingleCandidate (hex "ff3c")) # Single candidate
(def 'Hangul-MultipleCandidate (hex "ff3d")) # Multiple candidate
(def 'Hangul-PreviousCandidate (hex "ff3e")) # Previous candidate
(def 'Hangul-Special (hex "ff3f")) # Special symbols
(def 'Hangul-switch (hex "ff7e")) # Alias for mode_switch

# Hangul Consonant Characters 
(def 'Hangul-Kiyeog (hex "0ea1"))
(def 'Hangul-SsangKiyeog (hex "0ea2"))
(def 'Hangul-KiyeogSios (hex "0ea3"))
(def 'Hangul-Nieun (hex "0ea4"))
(def 'Hangul-NieunJieuj (hex "0ea5"))
(def 'Hangul-NieunHieuh (hex "0ea6"))
(def 'Hangul-Dikeud (hex "0ea7"))
(def 'Hangul-SsangDikeud (hex "0ea8"))
(def 'Hangul-Rieul (hex "0ea9"))
(def 'Hangul-RieulKiyeog (hex "0eaa"))
(def 'Hangul-RieulMieum (hex "0eab"))
(def 'Hangul-RieulPieub (hex "0eac"))
(def 'Hangul-RieulSios (hex "0ead"))
(def 'Hangul-RieulTieut (hex "0eae"))
(def 'Hangul-RieulPhieuf (hex "0eaf"))
(def 'Hangul-RieulHieuh (hex "0eb0"))
(def 'Hangul-Mieum (hex "0eb1"))
(def 'Hangul-Pieub (hex "0eb2"))
(def 'Hangul-SsangPieub (hex "0eb3"))
(def 'Hangul-PieubSios (hex "0eb4"))
(def 'Hangul-Sios (hex "0eb5"))
(def 'Hangul-SsangSios (hex "0eb6"))
(def 'Hangul-Ieung (hex "0eb7"))
(def 'Hangul-Jieuj (hex "0eb8"))
(def 'Hangul-SsangJieuj (hex "0eb9"))
(def 'Hangul-Cieuc (hex "0eba"))
(def 'Hangul-Khieuq (hex "0ebb"))
(def 'Hangul-Tieut (hex "0ebc"))
(def 'Hangul-Phieuf (hex "0ebd"))
(def 'Hangul-Hieuh (hex "0ebe"))

# Hangul Vowel Characters 
(def 'Hangul-A (hex "0ebf"))
(def 'Hangul-AE (hex "0ec0"))
(def 'Hangul-YA (hex "0ec1"))
(def 'Hangul-YAE (hex "0ec2"))
(def 'Hangul-EO (hex "0ec3"))
(def 'Hangul-E (hex "0ec4"))
(def 'Hangul-YEO (hex "0ec5"))
(def 'Hangul-YE (hex "0ec6"))
(def 'Hangul-O (hex "0ec7"))
(def 'Hangul-WA (hex "0ec8"))
(def 'Hangul-WAE (hex "0ec9"))
(def 'Hangul-OE (hex "0eca"))
(def 'Hangul-YO (hex "0ecb"))
(def 'Hangul-U (hex "0ecc"))
(def 'Hangul-WEO (hex "0ecd"))
(def 'Hangul-WE (hex "0ece"))
(def 'Hangul-WI (hex "0ecf"))
(def 'Hangul-YU (hex "0ed0"))
(def 'Hangul-EU (hex "0ed1"))
(def 'Hangul-YI (hex "0ed2"))
(def 'Hangul-I (hex "0ed3"))

# Hangul syllable-final (JongSeong) Characters 
(def 'Hangul-J-Kiyeog (hex "0ed4"))
(def 'Hangul-J-SsangKiyeog (hex "0ed5"))
(def 'Hangul-J-KiyeogSios (hex "0ed6"))
(def 'Hangul-J-Nieun (hex "0ed7"))
(def 'Hangul-J-NieunJieuj (hex "0ed8"))
(def 'Hangul-J-NieunHieuh (hex "0ed9"))
(def 'Hangul-J-Dikeud (hex "0eda"))
(def 'Hangul-J-Rieul (hex "0edb"))
(def 'Hangul-J-RieulKiyeog (hex "0edc"))
(def 'Hangul-J-RieulMieum (hex "0edd"))
(def 'Hangul-J-RieulPieub (hex "0ede"))
(def 'Hangul-J-RieulSios (hex "0edf"))
(def 'Hangul-J-RieulTieut (hex "0ee0"))
(def 'Hangul-J-RieulPhieuf (hex "0ee1"))
(def 'Hangul-J-RieulHieuh (hex "0ee2"))
(def 'Hangul-J-Mieum (hex "0ee3"))
(def 'Hangul-J-Pieub (hex "0ee4"))
(def 'Hangul-J-PieubSios (hex "0ee5"))
(def 'Hangul-J-Sios (hex "0ee6"))
(def 'Hangul-J-SsangSios (hex "0ee7"))
(def 'Hangul-J-Ieung (hex "0ee8"))
(def 'Hangul-J-Jieuj (hex "0ee9"))
(def 'Hangul-J-Cieuc (hex "0eea"))
(def 'Hangul-J-Khieuq (hex "0eeb"))
(def 'Hangul-J-Tieut (hex "0eec"))
(def 'Hangul-J-Phieuf (hex "0eed"))
(def 'Hangul-J-Hieuh (hex "0eee"))

# Ancient Hangul Consonant Characters 
(def 'Hangul-RieulYeorinHieuh (hex "0eef"))
(def 'Hangul-SunkyeongeumMieum (hex "0ef0"))
(def 'Hangul-SunkyeongeumPieub (hex "0ef1"))
(def 'Hangul-PanSios (hex "0ef2"))
(def 'Hangul-KkogjiDalrinIeung (hex "0ef3"))
(def 'Hangul-SunkyeongeumPhieuf (hex "0ef4"))
(def 'Hangul-YeorinHieuh (hex "0ef5"))

# Ancient Hangul Vowel Characters 
(def 'Hangul-AraeA (hex "0ef6"))
(def 'Hangul-AraeAE (hex "0ef7"))

# Ancient Hangul syllable-final (JongSeong) Characters 
(def 'Hangul-J-PanSios (hex "0ef8"))
(def 'Hangul-J-KkogjiDalrinIeung (hex "0ef9"))
(def 'Hangul-J-YeorinHieuh (hex "0efa"))

# Korean currency symbol 
(def 'Korean-Won (hex "0eff")) #(U+20A9 WON SIGN)


#{
  Armenian
}#

(def 'Armenian-ligature-ew (hex "1000587")) # U+0587 ARMENIAN SMALL LIGATURE ECH YIWN
(def 'Armenian-full-stop (hex "1000589")) # U+0589 ARMENIAN FULL STOP
(def 'Armenian-verjaket (hex "1000589")) # U+0589 ARMENIAN FULL STOP
(def 'Armenian-separation-mark (hex "100055d")) # U+055D ARMENIAN COMMA
(def 'Armenian-but (hex "100055d")) # U+055D ARMENIAN COMMA
(def 'Armenian-hyphen (hex "100058a")) # U+058A ARMENIAN HYPHEN
(def 'Armenian-yentamna (hex "100058a")) # U+058A ARMENIAN HYPHEN
(def 'Armenian-exclam (hex "100055c")) # U+055C ARMENIAN EXCLAMATION MARK
(def 'Armenian-amanak (hex "100055c")) # U+055C ARMENIAN EXCLAMATION MARK
(def 'Armenian-accent (hex "100055b")) # U+055B ARMENIAN EMPHASIS MARK
(def 'Armenian-shesht (hex "100055b")) # U+055B ARMENIAN EMPHASIS MARK
(def 'Armenian-question (hex "100055e")) # U+055E ARMENIAN QUESTION MARK
(def 'Armenian-paruyk (hex "100055e")) # U+055E ARMENIAN QUESTION MARK
(def 'Armenian-AYB (hex "1000531")) # U+0531 ARMENIAN CAPITAL LETTER AYB
(def 'Armenian-ayb (hex "1000561")) # U+0561 ARMENIAN SMALL LETTER AYB
(def 'Armenian-BEN (hex "1000532")) # U+0532 ARMENIAN CAPITAL LETTER BEN
(def 'Armenian-ben (hex "1000562")) # U+0562 ARMENIAN SMALL LETTER BEN
(def 'Armenian-GIM (hex "1000533")) # U+0533 ARMENIAN CAPITAL LETTER GIM
(def 'Armenian-gim (hex "1000563")) # U+0563 ARMENIAN SMALL LETTER GIM
(def 'Armenian-DA (hex "1000534")) # U+0534 ARMENIAN CAPITAL LETTER DA
(def 'Armenian-da (hex "1000564")) # U+0564 ARMENIAN SMALL LETTER DA
(def 'Armenian-YECH (hex "1000535")) # U+0535 ARMENIAN CAPITAL LETTER ECH
(def 'Armenian-yech (hex "1000565")) # U+0565 ARMENIAN SMALL LETTER ECH
(def 'Armenian-ZA (hex "1000536")) # U+0536 ARMENIAN CAPITAL LETTER ZA
(def 'Armenian-za (hex "1000566")) # U+0566 ARMENIAN SMALL LETTER ZA
(def 'Armenian-E (hex "1000537")) # U+0537 ARMENIAN CAPITAL LETTER EH
(def 'Armenian-e (hex "1000567")) # U+0567 ARMENIAN SMALL LETTER EH
(def 'Armenian-AT (hex "1000538")) # U+0538 ARMENIAN CAPITAL LETTER ET
(def 'Armenian-at (hex "1000568")) # U+0568 ARMENIAN SMALL LETTER ET
(def 'Armenian-TO (hex "1000539")) # U+0539 ARMENIAN CAPITAL LETTER TO
(def 'Armenian-to (hex "1000569")) # U+0569 ARMENIAN SMALL LETTER TO
(def 'Armenian-ZHE (hex "100053a")) # U+053A ARMENIAN CAPITAL LETTER ZHE
(def 'Armenian-zhe (hex "100056a")) # U+056A ARMENIAN SMALL LETTER ZHE
(def 'Armenian-INI (hex "100053b")) # U+053B ARMENIAN CAPITAL LETTER INI
(def 'Armenian-ini (hex "100056b")) # U+056B ARMENIAN SMALL LETTER INI
(def 'Armenian-LYUN (hex "100053c")) # U+053C ARMENIAN CAPITAL LETTER LIWN
(def 'Armenian-lyun (hex "100056c")) # U+056C ARMENIAN SMALL LETTER LIWN
(def 'Armenian-KHE (hex "100053d")) # U+053D ARMENIAN CAPITAL LETTER XEH
(def 'Armenian-khe (hex "100056d")) # U+056D ARMENIAN SMALL LETTER XEH
(def 'Armenian-TSA (hex "100053e")) # U+053E ARMENIAN CAPITAL LETTER CA
(def 'Armenian-tsa (hex "100056e")) # U+056E ARMENIAN SMALL LETTER CA
(def 'Armenian-KEN (hex "100053f")) # U+053F ARMENIAN CAPITAL LETTER KEN
(def 'Armenian-ken (hex "100056f")) # U+056F ARMENIAN SMALL LETTER KEN
(def 'Armenian-HO (hex "1000540")) # U+0540 ARMENIAN CAPITAL LETTER HO
(def 'Armenian-ho (hex "1000570")) # U+0570 ARMENIAN SMALL LETTER HO
(def 'Armenian-DZA (hex "1000541")) # U+0541 ARMENIAN CAPITAL LETTER JA
(def 'Armenian-dza (hex "1000571")) # U+0571 ARMENIAN SMALL LETTER JA
(def 'Armenian-GHAT (hex "1000542")) # U+0542 ARMENIAN CAPITAL LETTER GHAD
(def 'Armenian-ghat (hex "1000572")) # U+0572 ARMENIAN SMALL LETTER GHAD
(def 'Armenian-TCHE (hex "1000543")) # U+0543 ARMENIAN CAPITAL LETTER CHEH
(def 'Armenian-tche (hex "1000573")) # U+0573 ARMENIAN SMALL LETTER CHEH
(def 'Armenian-MEN (hex "1000544")) # U+0544 ARMENIAN CAPITAL LETTER MEN
(def 'Armenian-men (hex "1000574")) # U+0574 ARMENIAN SMALL LETTER MEN
(def 'Armenian-HI (hex "1000545")) # U+0545 ARMENIAN CAPITAL LETTER YI
(def 'Armenian-hi (hex "1000575")) # U+0575 ARMENIAN SMALL LETTER YI
(def 'Armenian-NU (hex "1000546")) # U+0546 ARMENIAN CAPITAL LETTER NOW
(def 'Armenian-nu (hex "1000576")) # U+0576 ARMENIAN SMALL LETTER NOW
(def 'Armenian-SHA (hex "1000547")) # U+0547 ARMENIAN CAPITAL LETTER SHA
(def 'Armenian-sha (hex "1000577")) # U+0577 ARMENIAN SMALL LETTER SHA
(def 'Armenian-VO (hex "1000548")) # U+0548 ARMENIAN CAPITAL LETTER VO
(def 'Armenian-vo (hex "1000578")) # U+0578 ARMENIAN SMALL LETTER VO
(def 'Armenian-CHA (hex "1000549")) # U+0549 ARMENIAN CAPITAL LETTER CHA
(def 'Armenian-cha (hex "1000579")) # U+0579 ARMENIAN SMALL LETTER CHA
(def 'Armenian-PE (hex "100054a")) # U+054A ARMENIAN CAPITAL LETTER PEH
(def 'Armenian-pe (hex "100057a")) # U+057A ARMENIAN SMALL LETTER PEH
(def 'Armenian-JE (hex "100054b")) # U+054B ARMENIAN CAPITAL LETTER JHEH
(def 'Armenian-je (hex "100057b")) # U+057B ARMENIAN SMALL LETTER JHEH
(def 'Armenian-RA (hex "100054c")) # U+054C ARMENIAN CAPITAL LETTER RA
(def 'Armenian-ra (hex "100057c")) # U+057C ARMENIAN SMALL LETTER RA
(def 'Armenian-SE (hex "100054d")) # U+054D ARMENIAN CAPITAL LETTER SEH
(def 'Armenian-se (hex "100057d")) # U+057D ARMENIAN SMALL LETTER SEH
(def 'Armenian-VEV (hex "100054e")) # U+054E ARMENIAN CAPITAL LETTER VEW
(def 'Armenian-vev (hex "100057e")) # U+057E ARMENIAN SMALL LETTER VEW
(def 'Armenian-TYUN (hex "100054f")) # U+054F ARMENIAN CAPITAL LETTER TIWN
(def 'Armenian-tyun (hex "100057f")) # U+057F ARMENIAN SMALL LETTER TIWN
(def 'Armenian-RE (hex "1000550")) # U+0550 ARMENIAN CAPITAL LETTER REH
(def 'Armenian-re (hex "1000580")) # U+0580 ARMENIAN SMALL LETTER REH
(def 'Armenian-TSO (hex "1000551")) # U+0551 ARMENIAN CAPITAL LETTER CO
(def 'Armenian-tso (hex "1000581")) # U+0581 ARMENIAN SMALL LETTER CO
(def 'Armenian-VYUN (hex "1000552")) # U+0552 ARMENIAN CAPITAL LETTER YIWN
(def 'Armenian-vyun (hex "1000582")) # U+0582 ARMENIAN SMALL LETTER YIWN
(def 'Armenian-PYUR (hex "1000553")) # U+0553 ARMENIAN CAPITAL LETTER PIWR
(def 'Armenian-pyur (hex "1000583")) # U+0583 ARMENIAN SMALL LETTER PIWR
(def 'Armenian-KE (hex "1000554")) # U+0554 ARMENIAN CAPITAL LETTER KEH
(def 'Armenian-ke (hex "1000584")) # U+0584 ARMENIAN SMALL LETTER KEH
(def 'Armenian-O (hex "1000555")) # U+0555 ARMENIAN CAPITAL LETTER OH
(def 'Armenian-o (hex "1000585")) # U+0585 ARMENIAN SMALL LETTER OH
(def 'Armenian-FE (hex "1000556")) # U+0556 ARMENIAN CAPITAL LETTER FEH
(def 'Armenian-fe (hex "1000586")) # U+0586 ARMENIAN SMALL LETTER FEH
(def 'Armenian-apostrophe (hex "100055a")) # U+055A ARMENIAN APOSTROPHE

#{
  Georgian
}#

(def 'Georgian-an (hex "10010d0")) # U+10D0 GEORGIAN LETTER AN
(def 'Georgian-ban (hex "10010d1")) # U+10D1 GEORGIAN LETTER BAN
(def 'Georgian-gan (hex "10010d2")) # U+10D2 GEORGIAN LETTER GAN
(def 'Georgian-don (hex "10010d3")) # U+10D3 GEORGIAN LETTER DON
(def 'Georgian-en (hex "10010d4")) # U+10D4 GEORGIAN LETTER EN
(def 'Georgian-vin (hex "10010d5")) # U+10D5 GEORGIAN LETTER VIN
(def 'Georgian-zen (hex "10010d6")) # U+10D6 GEORGIAN LETTER ZEN
(def 'Georgian-tan (hex "10010d7")) # U+10D7 GEORGIAN LETTER TAN
(def 'Georgian-in (hex "10010d8")) # U+10D8 GEORGIAN LETTER IN
(def 'Georgian-kan (hex "10010d9")) # U+10D9 GEORGIAN LETTER KAN
(def 'Georgian-las (hex "10010da")) # U+10DA GEORGIAN LETTER LAS
(def 'Georgian-man (hex "10010db")) # U+10DB GEORGIAN LETTER MAN
(def 'Georgian-nar (hex "10010dc")) # U+10DC GEORGIAN LETTER NAR
(def 'Georgian-on (hex "10010dd")) # U+10DD GEORGIAN LETTER ON
(def 'Georgian-par (hex "10010de")) # U+10DE GEORGIAN LETTER PAR
(def 'Georgian-zhar (hex "10010df")) # U+10DF GEORGIAN LETTER ZHAR
(def 'Georgian-rae (hex "10010e0")) # U+10E0 GEORGIAN LETTER RAE
(def 'Georgian-san (hex "10010e1")) # U+10E1 GEORGIAN LETTER SAN
(def 'Georgian-tar (hex "10010e2")) # U+10E2 GEORGIAN LETTER TAR
(def 'Georgian-un (hex "10010e3")) # U+10E3 GEORGIAN LETTER UN
(def 'Georgian-phar (hex "10010e4")) # U+10E4 GEORGIAN LETTER PHAR
(def 'Georgian-khar (hex "10010e5")) # U+10E5 GEORGIAN LETTER KHAR
(def 'Georgian-ghan (hex "10010e6")) # U+10E6 GEORGIAN LETTER GHAN
(def 'Georgian-qar (hex "10010e7")) # U+10E7 GEORGIAN LETTER QAR
(def 'Georgian-shin (hex "10010e8")) # U+10E8 GEORGIAN LETTER SHIN
(def 'Georgian-chin (hex "10010e9")) # U+10E9 GEORGIAN LETTER CHIN
(def 'Georgian-can (hex "10010ea")) # U+10EA GEORGIAN LETTER CAN
(def 'Georgian-jil (hex "10010eb")) # U+10EB GEORGIAN LETTER JIL
(def 'Georgian-cil (hex "10010ec")) # U+10EC GEORGIAN LETTER CIL
(def 'Georgian-char (hex "10010ed")) # U+10ED GEORGIAN LETTER CHAR
(def 'Georgian-xan (hex "10010ee")) # U+10EE GEORGIAN LETTER XAN
(def 'Georgian-jhan (hex "10010ef")) # U+10EF GEORGIAN LETTER JHAN
(def 'Georgian-hae (hex "10010f0")) # U+10F0 GEORGIAN LETTER HAE
(def 'Georgian-he (hex "10010f1")) # U+10F1 GEORGIAN LETTER HE
(def 'Georgian-hie (hex "10010f2")) # U+10F2 GEORGIAN LETTER HIE
(def 'Georgian-we (hex "10010f3")) # U+10F3 GEORGIAN LETTER WE
(def 'Georgian-har (hex "10010f4")) # U+10F4 GEORGIAN LETTER HAR
(def 'Georgian-hoe (hex "10010f5")) # U+10F5 GEORGIAN LETTER HOE
(def 'Georgian-fi (hex "10010f6")) # U+10F6 GEORGIAN LETTER FI

#{
  Azeri (and other Turkic or Caucasian languages)
}#

# latin 
(def 'Xabovedot (hex "1001e8a")) # U+1E8A LATIN CAPITAL LETTER X WITH DOT ABOVE
(def 'Ibreve (hex "100012c")) # U+012C LATIN CAPITAL LETTER I WITH BREVE
(def 'Zstroke (hex "10001b5")) # U+01B5 LATIN CAPITAL LETTER Z WITH STROKE
(def 'Gcaron (hex "10001e6")) # U+01E6 LATIN CAPITAL LETTER G WITH CARON
(def 'Ocaron (hex "10001d1")) # U+01D1 LATIN CAPITAL LETTER O WITH CARON
(def 'Obarred (hex "100019f")) # U+019F LATIN CAPITAL LETTER O WITH MIDDLE TILDE
(def 'xabovedot (hex "1001e8b")) # U+1E8B LATIN SMALL LETTER X WITH DOT ABOVE
(def 'ibreve (hex "100012d")) # U+012D LATIN SMALL LETTER I WITH BREVE
(def 'zstroke (hex "10001b6")) # U+01B6 LATIN SMALL LETTER Z WITH STROKE
(def 'gcaron (hex "10001e7")) # U+01E7 LATIN SMALL LETTER G WITH CARON
(def 'ocaron (hex "10001d2")) # U+01D2 LATIN SMALL LETTER O WITH CARON
(def 'obarred (hex "1000275")) # U+0275 LATIN SMALL LETTER BARRED O
(def 'SCHWA (hex "100018f")) # U+018F LATIN CAPITAL LETTER SCHWA
(def 'schwa (hex "1000259")) # U+0259 LATIN SMALL LETTER SCHWA
(def 'EZH (hex "10001b7")) # U+01B7 LATIN CAPITAL LETTER EZH
(def 'ezh (hex "1000292")) # U+0292 LATIN SMALL LETTER EZH
# those are not really Caucasus 
# For Inupiak 
(def 'Lbelowdot (hex "1001e36")) # U+1E36 LATIN CAPITAL LETTER L WITH DOT BELOW
(def 'lbelowdot (hex "1001e37")) # U+1E37 LATIN SMALL LETTER L WITH DOT BELOW

#{
  Vietnamese
}#

(def 'Abelowdot (hex "1001ea0")) # U+1EA0 LATIN CAPITAL LETTER A WITH DOT BELOW
(def 'abelowdot (hex "1001ea1")) # U+1EA1 LATIN SMALL LETTER A WITH DOT BELOW
(def 'Ahook (hex "1001ea2")) # U+1EA2 LATIN CAPITAL LETTER A WITH HOOK ABOVE
(def 'ahook (hex "1001ea3")) # U+1EA3 LATIN SMALL LETTER A WITH HOOK ABOVE
(def 'Acircumflexacute (hex "1001ea4")) # U+1EA4 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
(def 'acircumflexacute (hex "1001ea5")) # U+1EA5 LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
(def 'Acircumflexgrave (hex "1001ea6")) # U+1EA6 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
(def 'acircumflexgrave (hex "1001ea7")) # U+1EA7 LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
(def 'Acircumflexhook (hex "1001ea8")) # U+1EA8 LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
(def 'acircumflexhook (hex "1001ea9")) # U+1EA9 LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
(def 'Acircumflextilde (hex "1001eaa")) # U+1EAA LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
(def 'acircumflextilde (hex "1001eab")) # U+1EAB LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
(def 'Acircumflexbelowdot (hex "1001eac")) # U+1EAC LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
(def 'acircumflexbelowdot (hex "1001ead")) # U+1EAD LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
(def 'Abreveacute (hex "1001eae")) # U+1EAE LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
(def 'abreveacute (hex "1001eaf")) # U+1EAF LATIN SMALL LETTER A WITH BREVE AND ACUTE
(def 'Abrevegrave (hex "1001eb0")) # U+1EB0 LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
(def 'abrevegrave (hex "1001eb1")) # U+1EB1 LATIN SMALL LETTER A WITH BREVE AND GRAVE
(def 'Abrevehook (hex "1001eb2")) # U+1EB2 LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
(def 'abrevehook (hex "1001eb3")) # U+1EB3 LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
(def 'Abrevetilde (hex "1001eb4")) # U+1EB4 LATIN CAPITAL LETTER A WITH BREVE AND TILDE
(def 'abrevetilde (hex "1001eb5")) # U+1EB5 LATIN SMALL LETTER A WITH BREVE AND TILDE
(def 'Abrevebelowdot (hex "1001eb6")) # U+1EB6 LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
(def 'abrevebelowdot (hex "1001eb7")) # U+1EB7 LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
(def 'Ebelowdot (hex "1001eb8")) # U+1EB8 LATIN CAPITAL LETTER E WITH DOT BELOW
(def 'ebelowdot (hex "1001eb9")) # U+1EB9 LATIN SMALL LETTER E WITH DOT BELOW
(def 'Ehook (hex "1001eba")) # U+1EBA LATIN CAPITAL LETTER E WITH HOOK ABOVE
(def 'ehook (hex "1001ebb")) # U+1EBB LATIN SMALL LETTER E WITH HOOK ABOVE
(def 'Etilde (hex "1001ebc")) # U+1EBC LATIN CAPITAL LETTER E WITH TILDE
(def 'etilde (hex "1001ebd")) # U+1EBD LATIN SMALL LETTER E WITH TILDE
(def 'Ecircumflexacute (hex "1001ebe")) # U+1EBE LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
(def 'ecircumflexacute (hex "1001ebf")) # U+1EBF LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
(def 'Ecircumflexgrave (hex "1001ec0")) # U+1EC0 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
(def 'ecircumflexgrave (hex "1001ec1")) # U+1EC1 LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
(def 'Ecircumflexhook (hex "1001ec2")) # U+1EC2 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
(def 'ecircumflexhook (hex "1001ec3")) # U+1EC3 LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
(def 'Ecircumflextilde (hex "1001ec4")) # U+1EC4 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
(def 'ecircumflextilde (hex "1001ec5")) # U+1EC5 LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
(def 'Ecircumflexbelowdot (hex "1001ec6")) # U+1EC6 LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
(def 'ecircumflexbelowdot (hex "1001ec7")) # U+1EC7 LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
(def 'Ihook (hex "1001ec8")) # U+1EC8 LATIN CAPITAL LETTER I WITH HOOK ABOVE
(def 'ihook (hex "1001ec9")) # U+1EC9 LATIN SMALL LETTER I WITH HOOK ABOVE
(def 'Ibelowdot (hex "1001eca")) # U+1ECA LATIN CAPITAL LETTER I WITH DOT BELOW
(def 'ibelowdot (hex "1001ecb")) # U+1ECB LATIN SMALL LETTER I WITH DOT BELOW
(def 'Obelowdot (hex "1001ecc")) # U+1ECC LATIN CAPITAL LETTER O WITH DOT BELOW
(def 'obelowdot (hex "1001ecd")) # U+1ECD LATIN SMALL LETTER O WITH DOT BELOW
(def 'Ohook (hex "1001ece")) # U+1ECE LATIN CAPITAL LETTER O WITH HOOK ABOVE
(def 'ohook (hex "1001ecf")) # U+1ECF LATIN SMALL LETTER O WITH HOOK ABOVE
(def 'Ocircumflexacute (hex "1001ed0")) # U+1ED0 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
(def 'ocircumflexacute (hex "1001ed1")) # U+1ED1 LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
(def 'Ocircumflexgrave (hex "1001ed2")) # U+1ED2 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
(def 'ocircumflexgrave (hex "1001ed3")) # U+1ED3 LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
(def 'Ocircumflexhook (hex "1001ed4")) # U+1ED4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
(def 'ocircumflexhook (hex "1001ed5")) # U+1ED5 LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
(def 'Ocircumflextilde (hex "1001ed6")) # U+1ED6 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
(def 'ocircumflextilde (hex "1001ed7")) # U+1ED7 LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
(def 'Ocircumflexbelowdot (hex "1001ed8")) # U+1ED8 LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
(def 'ocircumflexbelowdot (hex "1001ed9")) # U+1ED9 LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
(def 'Ohornacute (hex "1001eda")) # U+1EDA LATIN CAPITAL LETTER O WITH HORN AND ACUTE
(def 'ohornacute (hex "1001edb")) # U+1EDB LATIN SMALL LETTER O WITH HORN AND ACUTE
(def 'Ohorngrave (hex "1001edc")) # U+1EDC LATIN CAPITAL LETTER O WITH HORN AND GRAVE
(def 'ohorngrave (hex "1001edd")) # U+1EDD LATIN SMALL LETTER O WITH HORN AND GRAVE
(def 'Ohornhook (hex "1001ede")) # U+1EDE LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
(def 'ohornhook (hex "1001edf")) # U+1EDF LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
(def 'Ohorntilde (hex "1001ee0")) # U+1EE0 LATIN CAPITAL LETTER O WITH HORN AND TILDE
(def 'ohorntilde (hex "1001ee1")) # U+1EE1 LATIN SMALL LETTER O WITH HORN AND TILDE
(def 'Ohornbelowdot (hex "1001ee2")) # U+1EE2 LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
(def 'ohornbelowdot (hex "1001ee3")) # U+1EE3 LATIN SMALL LETTER O WITH HORN AND DOT BELOW
(def 'Ubelowdot (hex "1001ee4")) # U+1EE4 LATIN CAPITAL LETTER U WITH DOT BELOW
(def 'ubelowdot (hex "1001ee5")) # U+1EE5 LATIN SMALL LETTER U WITH DOT BELOW
(def 'Uhook (hex "1001ee6")) # U+1EE6 LATIN CAPITAL LETTER U WITH HOOK ABOVE
(def 'uhook (hex "1001ee7")) # U+1EE7 LATIN SMALL LETTER U WITH HOOK ABOVE
(def 'Uhornacute (hex "1001ee8")) # U+1EE8 LATIN CAPITAL LETTER U WITH HORN AND ACUTE
(def 'uhornacute (hex "1001ee9")) # U+1EE9 LATIN SMALL LETTER U WITH HORN AND ACUTE
(def 'Uhorngrave (hex "1001eea")) # U+1EEA LATIN CAPITAL LETTER U WITH HORN AND GRAVE
(def 'uhorngrave (hex "1001eeb")) # U+1EEB LATIN SMALL LETTER U WITH HORN AND GRAVE
(def 'Uhornhook (hex "1001eec")) # U+1EEC LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
(def 'uhornhook (hex "1001eed")) # U+1EED LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
(def 'Uhorntilde (hex "1001eee")) # U+1EEE LATIN CAPITAL LETTER U WITH HORN AND TILDE
(def 'uhorntilde (hex "1001eef")) # U+1EEF LATIN SMALL LETTER U WITH HORN AND TILDE
(def 'Uhornbelowdot (hex "1001ef0")) # U+1EF0 LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
(def 'uhornbelowdot (hex "1001ef1")) # U+1EF1 LATIN SMALL LETTER U WITH HORN AND DOT BELOW
(def 'Ybelowdot (hex "1001ef4")) # U+1EF4 LATIN CAPITAL LETTER Y WITH DOT BELOW
(def 'ybelowdot (hex "1001ef5")) # U+1EF5 LATIN SMALL LETTER Y WITH DOT BELOW
(def 'Yhook (hex "1001ef6")) # U+1EF6 LATIN CAPITAL LETTER Y WITH HOOK ABOVE
(def 'yhook (hex "1001ef7")) # U+1EF7 LATIN SMALL LETTER Y WITH HOOK ABOVE
(def 'Ytilde (hex "1001ef8")) # U+1EF8 LATIN CAPITAL LETTER Y WITH TILDE
(def 'ytilde (hex "1001ef9")) # U+1EF9 LATIN SMALL LETTER Y WITH TILDE
(def 'Ohorn (hex "10001a0")) # U+01A0 LATIN CAPITAL LETTER O WITH HORN
(def 'ohorn (hex "10001a1")) # U+01A1 LATIN SMALL LETTER O WITH HORN
(def 'Uhorn (hex "10001af")) # U+01AF LATIN CAPITAL LETTER U WITH HORN
(def 'uhorn (hex "10001b0")) # U+01B0 LATIN SMALL LETTER U WITH HORN


(def 'EcuSign (hex "10020a0")) # U+20A0 EURO-CURRENCY SIGN
(def 'ColonSign (hex "10020a1")) # U+20A1 COLON SIGN
(def 'CruzeiroSign (hex "10020a2")) # U+20A2 CRUZEIRO SIGN
(def 'FFrancSign (hex "10020a3")) # U+20A3 FRENCH FRANC SIGN
(def 'LiraSign (hex "10020a4")) # U+20A4 LIRA SIGN
(def 'MillSign (hex "10020a5")) # U+20A5 MILL SIGN
(def 'NairaSign (hex "10020a6")) # U+20A6 NAIRA SIGN
(def 'PesetaSign (hex "10020a7")) # U+20A7 PESETA SIGN
(def 'RupeeSign (hex "10020a8")) # U+20A8 RUPEE SIGN
(def 'WonSign (hex "10020a9")) # U+20A9 WON SIGN
(def 'NewSheqelSign (hex "10020aa")) # U+20AA NEW SHEQEL SIGN
(def 'DongSign (hex "10020ab")) # U+20AB DONG SIGN
(def 'EuroSign (hex "20ac")) # U+20AC EURO SIGN

# one, two and three are defined above. 
(def 'zerosuperior (hex "1002070")) # U+2070 SUPERSCRIPT ZERO
(def 'foursuperior (hex "1002074")) # U+2074 SUPERSCRIPT FOUR
(def 'fivesuperior (hex "1002075")) # U+2075 SUPERSCRIPT FIVE
(def 'sixsuperior (hex "1002076")) # U+2076 SUPERSCRIPT SIX
(def 'sevensuperior (hex "1002077")) # U+2077 SUPERSCRIPT SEVEN
(def 'eightsuperior (hex "1002078")) # U+2078 SUPERSCRIPT EIGHT
(def 'ninesuperior (hex "1002079")) # U+2079 SUPERSCRIPT NINE
(def 'zerosubscript (hex "1002080")) # U+2080 SUBSCRIPT ZERO
(def 'onesubscript (hex "1002081")) # U+2081 SUBSCRIPT ONE
(def 'twosubscript (hex "1002082")) # U+2082 SUBSCRIPT TWO
(def 'threesubscript (hex "1002083")) # U+2083 SUBSCRIPT THREE
(def 'foursubscript (hex "1002084")) # U+2084 SUBSCRIPT FOUR
(def 'fivesubscript (hex "1002085")) # U+2085 SUBSCRIPT FIVE
(def 'sixsubscript (hex "1002086")) # U+2086 SUBSCRIPT SIX
(def 'sevensubscript (hex "1002087")) # U+2087 SUBSCRIPT SEVEN
(def 'eightsubscript (hex "1002088")) # U+2088 SUBSCRIPT EIGHT
(def 'ninesubscript (hex "1002089")) # U+2089 SUBSCRIPT NINE
(def 'partdifferential (hex "1002202")) # U+2202 PARTIAL DIFFERENTIAL
(def 'emptyset (hex "1002205")) # U+2205 NULL SET
(def 'elementof (hex "1002208")) # U+2208 ELEMENT OF
(def 'notelementof (hex "1002209")) # U+2209 NOT AN ELEMENT OF
(def 'containsas (hex "100220B")) # U+220B CONTAINS AS MEMBER
(def 'squareroot (hex "100221A")) # U+221A SQUARE ROOT
(def 'cuberoot (hex "100221B")) # U+221B CUBE ROOT
(def 'fourthroot (hex "100221C")) # U+221C FOURTH ROOT
(def 'dintegral (hex "100222C")) # U+222C DOUBLE INTEGRAL
(def 'tintegral (hex "100222D")) # U+222D TRIPLE INTEGRAL
(def 'because (hex "1002235")) # U+2235 BECAUSE
(def 'approxeq (hex "1002248")) # U+2245 ALMOST EQUAL TO
(def 'notapproxeq (hex "1002247")) # U+2247 NOT ALMOST EQUAL TO
(def 'notidentical (hex "1002262")) # U+2262 NOT IDENTICAL TO
(def 'stricteq (hex "1002263")) # U+2263 STRICTLY EQUIVALENT TO

(def 'braille-dot-1 (hex "fff1"))
(def 'braille-dot-2 (hex "fff2"))
(def 'braille-dot-3 (hex "fff3"))
(def 'braille-dot-4 (hex "fff4"))
(def 'braille-dot-5 (hex "fff5"))
(def 'braille-dot-6 (hex "fff6"))
(def 'braille-dot-7 (hex "fff7"))
(def 'braille-dot-8 (hex "fff8"))
(def 'braille-dot-9 (hex "fff9"))
(def 'braille-dot-10 (hex "fffa"))
(def 'braille-blank (hex "1002800")) # U+2800 BRAILLE PATTERN BLANK
(def 'braille-dots-1 (hex "1002801")) # U+2801 BRAILLE PATTERN DOTS-1
(def 'braille-dots-2 (hex "1002802")) # U+2802 BRAILLE PATTERN DOTS-2
(def 'braille-dots-12 (hex "1002803")) # U+2803 BRAILLE PATTERN DOTS-12
(def 'braille-dots-3 (hex "1002804")) # U+2804 BRAILLE PATTERN DOTS-3
(def 'braille-dots-13 (hex "1002805")) # U+2805 BRAILLE PATTERN DOTS-13
(def 'braille-dots-23 (hex "1002806")) # U+2806 BRAILLE PATTERN DOTS-23
(def 'braille-dots-123 (hex "1002807")) # U+2807 BRAILLE PATTERN DOTS-123
(def 'braille-dots-4 (hex "1002808")) # U+2808 BRAILLE PATTERN DOTS-4
(def 'braille-dots-14 (hex "1002809")) # U+2809 BRAILLE PATTERN DOTS-14
(def 'braille-dots-24 (hex "100280a")) # U+280a BRAILLE PATTERN DOTS-24
(def 'braille-dots-124 (hex "100280b")) # U+280b BRAILLE PATTERN DOTS-124
(def 'braille-dots-34 (hex "100280c")) # U+280c BRAILLE PATTERN DOTS-34
(def 'braille-dots-134 (hex "100280d")) # U+280d BRAILLE PATTERN DOTS-134
(def 'braille-dots-234 (hex "100280e")) # U+280e BRAILLE PATTERN DOTS-234
(def 'braille-dots-1234 (hex "100280f")) # U+280f BRAILLE PATTERN DOTS-1234
(def 'braille-dots-5 (hex "1002810")) # U+2810 BRAILLE PATTERN DOTS-5
(def 'braille-dots-15 (hex "1002811")) # U+2811 BRAILLE PATTERN DOTS-15
(def 'braille-dots-25 (hex "1002812")) # U+2812 BRAILLE PATTERN DOTS-25
(def 'braille-dots-125 (hex "1002813")) # U+2813 BRAILLE PATTERN DOTS-125
(def 'braille-dots-35 (hex "1002814")) # U+2814 BRAILLE PATTERN DOTS-35
(def 'braille-dots-135 (hex "1002815")) # U+2815 BRAILLE PATTERN DOTS-135
(def 'braille-dots-235 (hex "1002816")) # U+2816 BRAILLE PATTERN DOTS-235
(def 'braille-dots-1235 (hex "1002817")) # U+2817 BRAILLE PATTERN DOTS-1235
(def 'braille-dots-45 (hex "1002818")) # U+2818 BRAILLE PATTERN DOTS-45
(def 'braille-dots-145 (hex "1002819")) # U+2819 BRAILLE PATTERN DOTS-145
(def 'braille-dots-245 (hex "100281a")) # U+281a BRAILLE PATTERN DOTS-245
(def 'braille-dots-1245 (hex "100281b")) # U+281b BRAILLE PATTERN DOTS-1245
(def 'braille-dots-345 (hex "100281c")) # U+281c BRAILLE PATTERN DOTS-345
(def 'braille-dots-1345 (hex "100281d")) # U+281d BRAILLE PATTERN DOTS-1345
(def 'braille-dots-2345 (hex "100281e")) # U+281e BRAILLE PATTERN DOTS-2345
(def 'braille-dots-12345 (hex "100281f")) # U+281f BRAILLE PATTERN DOTS-12345
(def 'braille-dots-6 (hex "1002820")) # U+2820 BRAILLE PATTERN DOTS-6
(def 'braille-dots-16 (hex "1002821")) # U+2821 BRAILLE PATTERN DOTS-16
(def 'braille-dots-26 (hex "1002822")) # U+2822 BRAILLE PATTERN DOTS-26
(def 'braille-dots-126 (hex "1002823")) # U+2823 BRAILLE PATTERN DOTS-126
(def 'braille-dots-36 (hex "1002824")) # U+2824 BRAILLE PATTERN DOTS-36
(def 'braille-dots-136 (hex "1002825")) # U+2825 BRAILLE PATTERN DOTS-136
(def 'braille-dots-236 (hex "1002826")) # U+2826 BRAILLE PATTERN DOTS-236
(def 'braille-dots-1236 (hex "1002827")) # U+2827 BRAILLE PATTERN DOTS-1236
(def 'braille-dots-46 (hex "1002828")) # U+2828 BRAILLE PATTERN DOTS-46
(def 'braille-dots-146 (hex "1002829")) # U+2829 BRAILLE PATTERN DOTS-146
(def 'braille-dots-246 (hex "100282a")) # U+282a BRAILLE PATTERN DOTS-246
(def 'braille-dots-1246 (hex "100282b")) # U+282b BRAILLE PATTERN DOTS-1246
(def 'braille-dots-346 (hex "100282c")) # U+282c BRAILLE PATTERN DOTS-346
(def 'braille-dots-1346 (hex "100282d")) # U+282d BRAILLE PATTERN DOTS-1346
(def 'braille-dots-2346 (hex "100282e")) # U+282e BRAILLE PATTERN DOTS-2346
(def 'braille-dots-12346 (hex "100282f")) # U+282f BRAILLE PATTERN DOTS-12346
(def 'braille-dots-56 (hex "1002830")) # U+2830 BRAILLE PATTERN DOTS-56
(def 'braille-dots-156 (hex "1002831")) # U+2831 BRAILLE PATTERN DOTS-156
(def 'braille-dots-256 (hex "1002832")) # U+2832 BRAILLE PATTERN DOTS-256
(def 'braille-dots-1256 (hex "1002833")) # U+2833 BRAILLE PATTERN DOTS-1256
(def 'braille-dots-356 (hex "1002834")) # U+2834 BRAILLE PATTERN DOTS-356
(def 'braille-dots-1356 (hex "1002835")) # U+2835 BRAILLE PATTERN DOTS-1356
(def 'braille-dots-2356 (hex "1002836")) # U+2836 BRAILLE PATTERN DOTS-2356
(def 'braille-dots-12356 (hex "1002837")) # U+2837 BRAILLE PATTERN DOTS-12356
(def 'braille-dots-456 (hex "1002838")) # U+2838 BRAILLE PATTERN DOTS-456
(def 'braille-dots-1456 (hex "1002839")) # U+2839 BRAILLE PATTERN DOTS-1456
(def 'braille-dots-2456 (hex "100283a")) # U+283a BRAILLE PATTERN DOTS-2456
(def 'braille-dots-12456 (hex "100283b")) # U+283b BRAILLE PATTERN DOTS-12456
(def 'braille-dots-3456 (hex "100283c")) # U+283c BRAILLE PATTERN DOTS-3456
(def 'braille-dots-13456 (hex "100283d")) # U+283d BRAILLE PATTERN DOTS-13456
(def 'braille-dots-23456 (hex "100283e")) # U+283e BRAILLE PATTERN DOTS-23456
(def 'braille-dots-123456 (hex "100283f")) # U+283f BRAILLE PATTERN DOTS-123456
(def 'braille-dots-7 (hex "1002840")) # U+2840 BRAILLE PATTERN DOTS-7
(def 'braille-dots-17 (hex "1002841")) # U+2841 BRAILLE PATTERN DOTS-17
(def 'braille-dots-27 (hex "1002842")) # U+2842 BRAILLE PATTERN DOTS-27
(def 'braille-dots-127 (hex "1002843")) # U+2843 BRAILLE PATTERN DOTS-127
(def 'braille-dots-37 (hex "1002844")) # U+2844 BRAILLE PATTERN DOTS-37
(def 'braille-dots-137 (hex "1002845")) # U+2845 BRAILLE PATTERN DOTS-137
(def 'braille-dots-237 (hex "1002846")) # U+2846 BRAILLE PATTERN DOTS-237
(def 'braille-dots-1237 (hex "1002847")) # U+2847 BRAILLE PATTERN DOTS-1237
(def 'braille-dots-47 (hex "1002848")) # U+2848 BRAILLE PATTERN DOTS-47
(def 'braille-dots-147 (hex "1002849")) # U+2849 BRAILLE PATTERN DOTS-147
(def 'braille-dots-247 (hex "100284a")) # U+284a BRAILLE PATTERN DOTS-247
(def 'braille-dots-1247 (hex "100284b")) # U+284b BRAILLE PATTERN DOTS-1247
(def 'braille-dots-347 (hex "100284c")) # U+284c BRAILLE PATTERN DOTS-347
(def 'braille-dots-1347 (hex "100284d")) # U+284d BRAILLE PATTERN DOTS-1347
(def 'braille-dots-2347 (hex "100284e")) # U+284e BRAILLE PATTERN DOTS-2347
(def 'braille-dots-12347 (hex "100284f")) # U+284f BRAILLE PATTERN DOTS-12347
(def 'braille-dots-57 (hex "1002850")) # U+2850 BRAILLE PATTERN DOTS-57
(def 'braille-dots-157 (hex "1002851")) # U+2851 BRAILLE PATTERN DOTS-157
(def 'braille-dots-257 (hex "1002852")) # U+2852 BRAILLE PATTERN DOTS-257
(def 'braille-dots-1257 (hex "1002853")) # U+2853 BRAILLE PATTERN DOTS-1257
(def 'braille-dots-357 (hex "1002854")) # U+2854 BRAILLE PATTERN DOTS-357
(def 'braille-dots-1357 (hex "1002855")) # U+2855 BRAILLE PATTERN DOTS-1357
(def 'braille-dots-2357 (hex "1002856")) # U+2856 BRAILLE PATTERN DOTS-2357
(def 'braille-dots-12357 (hex "1002857")) # U+2857 BRAILLE PATTERN DOTS-12357
(def 'braille-dots-457 (hex "1002858")) # U+2858 BRAILLE PATTERN DOTS-457
(def 'braille-dots-1457 (hex "1002859")) # U+2859 BRAILLE PATTERN DOTS-1457
(def 'braille-dots-2457 (hex "100285a")) # U+285a BRAILLE PATTERN DOTS-2457
(def 'braille-dots-12457 (hex "100285b")) # U+285b BRAILLE PATTERN DOTS-12457
(def 'braille-dots-3457 (hex "100285c")) # U+285c BRAILLE PATTERN DOTS-3457
(def 'braille-dots-13457 (hex "100285d")) # U+285d BRAILLE PATTERN DOTS-13457
(def 'braille-dots-23457 (hex "100285e")) # U+285e BRAILLE PATTERN DOTS-23457
(def 'braille-dots-123457 (hex "100285f")) # U+285f BRAILLE PATTERN DOTS-123457
(def 'braille-dots-67 (hex "1002860")) # U+2860 BRAILLE PATTERN DOTS-67
(def 'braille-dots-167 (hex "1002861")) # U+2861 BRAILLE PATTERN DOTS-167
(def 'braille-dots-267 (hex "1002862")) # U+2862 BRAILLE PATTERN DOTS-267
(def 'braille-dots-1267 (hex "1002863")) # U+2863 BRAILLE PATTERN DOTS-1267
(def 'braille-dots-367 (hex "1002864")) # U+2864 BRAILLE PATTERN DOTS-367
(def 'braille-dots-1367 (hex "1002865")) # U+2865 BRAILLE PATTERN DOTS-1367
(def 'braille-dots-2367 (hex "1002866")) # U+2866 BRAILLE PATTERN DOTS-2367
(def 'braille-dots-12367 (hex "1002867")) # U+2867 BRAILLE PATTERN DOTS-12367
(def 'braille-dots-467 (hex "1002868")) # U+2868 BRAILLE PATTERN DOTS-467
(def 'braille-dots-1467 (hex "1002869")) # U+2869 BRAILLE PATTERN DOTS-1467
(def 'braille-dots-2467 (hex "100286a")) # U+286a BRAILLE PATTERN DOTS-2467
(def 'braille-dots-12467 (hex "100286b")) # U+286b BRAILLE PATTERN DOTS-12467
(def 'braille-dots-3467 (hex "100286c")) # U+286c BRAILLE PATTERN DOTS-3467
(def 'braille-dots-13467 (hex "100286d")) # U+286d BRAILLE PATTERN DOTS-13467
(def 'braille-dots-23467 (hex "100286e")) # U+286e BRAILLE PATTERN DOTS-23467
(def 'braille-dots-123467 (hex "100286f")) # U+286f BRAILLE PATTERN DOTS-123467
(def 'braille-dots-567 (hex "1002870")) # U+2870 BRAILLE PATTERN DOTS-567
(def 'braille-dots-1567 (hex "1002871")) # U+2871 BRAILLE PATTERN DOTS-1567
(def 'braille-dots-2567 (hex "1002872")) # U+2872 BRAILLE PATTERN DOTS-2567
(def 'braille-dots-12567 (hex "1002873")) # U+2873 BRAILLE PATTERN DOTS-12567
(def 'braille-dots-3567 (hex "1002874")) # U+2874 BRAILLE PATTERN DOTS-3567
(def 'braille-dots-13567 (hex "1002875")) # U+2875 BRAILLE PATTERN DOTS-13567
(def 'braille-dots-23567 (hex "1002876")) # U+2876 BRAILLE PATTERN DOTS-23567
(def 'braille-dots-123567 (hex "1002877")) # U+2877 BRAILLE PATTERN DOTS-123567
(def 'braille-dots-4567 (hex "1002878")) # U+2878 BRAILLE PATTERN DOTS-4567
(def 'braille-dots-14567 (hex "1002879")) # U+2879 BRAILLE PATTERN DOTS-14567
(def 'braille-dots-24567 (hex "100287a")) # U+287a BRAILLE PATTERN DOTS-24567
(def 'braille-dots-124567 (hex "100287b")) # U+287b BRAILLE PATTERN DOTS-124567
(def 'braille-dots-34567 (hex "100287c")) # U+287c BRAILLE PATTERN DOTS-34567
(def 'braille-dots-134567 (hex "100287d")) # U+287d BRAILLE PATTERN DOTS-134567
(def 'braille-dots-234567 (hex "100287e")) # U+287e BRAILLE PATTERN DOTS-234567
(def 'braille-dots-1234567 (hex "100287f")) # U+287f BRAILLE PATTERN DOTS-1234567
(def 'braille-dots-8 (hex "1002880")) # U+2880 BRAILLE PATTERN DOTS-8
(def 'braille-dots-18 (hex "1002881")) # U+2881 BRAILLE PATTERN DOTS-18
(def 'braille-dots-28 (hex "1002882")) # U+2882 BRAILLE PATTERN DOTS-28
(def 'braille-dots-128 (hex "1002883")) # U+2883 BRAILLE PATTERN DOTS-128
(def 'braille-dots-38 (hex "1002884")) # U+2884 BRAILLE PATTERN DOTS-38
(def 'braille-dots-138 (hex "1002885")) # U+2885 BRAILLE PATTERN DOTS-138
(def 'braille-dots-238 (hex "1002886")) # U+2886 BRAILLE PATTERN DOTS-238
(def 'braille-dots-1238 (hex "1002887")) # U+2887 BRAILLE PATTERN DOTS-1238
(def 'braille-dots-48 (hex "1002888")) # U+2888 BRAILLE PATTERN DOTS-48
(def 'braille-dots-148 (hex "1002889")) # U+2889 BRAILLE PATTERN DOTS-148
(def 'braille-dots-248 (hex "100288a")) # U+288a BRAILLE PATTERN DOTS-248
(def 'braille-dots-1248 (hex "100288b")) # U+288b BRAILLE PATTERN DOTS-1248
(def 'braille-dots-348 (hex "100288c")) # U+288c BRAILLE PATTERN DOTS-348
(def 'braille-dots-1348 (hex "100288d")) # U+288d BRAILLE PATTERN DOTS-1348
(def 'braille-dots-2348 (hex "100288e")) # U+288e BRAILLE PATTERN DOTS-2348
(def 'braille-dots-12348 (hex "100288f")) # U+288f BRAILLE PATTERN DOTS-12348
(def 'braille-dots-58 (hex "1002890")) # U+2890 BRAILLE PATTERN DOTS-58
(def 'braille-dots-158 (hex "1002891")) # U+2891 BRAILLE PATTERN DOTS-158
(def 'braille-dots-258 (hex "1002892")) # U+2892 BRAILLE PATTERN DOTS-258
(def 'braille-dots-1258 (hex "1002893")) # U+2893 BRAILLE PATTERN DOTS-1258
(def 'braille-dots-358 (hex "1002894")) # U+2894 BRAILLE PATTERN DOTS-358
(def 'braille-dots-1358 (hex "1002895")) # U+2895 BRAILLE PATTERN DOTS-1358
(def 'braille-dots-2358 (hex "1002896")) # U+2896 BRAILLE PATTERN DOTS-2358
(def 'braille-dots-12358 (hex "1002897")) # U+2897 BRAILLE PATTERN DOTS-12358
(def 'braille-dots-458 (hex "1002898")) # U+2898 BRAILLE PATTERN DOTS-458
(def 'braille-dots-1458 (hex "1002899")) # U+2899 BRAILLE PATTERN DOTS-1458
(def 'braille-dots-2458 (hex "100289a")) # U+289a BRAILLE PATTERN DOTS-2458
(def 'braille-dots-12458 (hex "100289b")) # U+289b BRAILLE PATTERN DOTS-12458
(def 'braille-dots-3458 (hex "100289c")) # U+289c BRAILLE PATTERN DOTS-3458
(def 'braille-dots-13458 (hex "100289d")) # U+289d BRAILLE PATTERN DOTS-13458
(def 'braille-dots-23458 (hex "100289e")) # U+289e BRAILLE PATTERN DOTS-23458
(def 'braille-dots-123458 (hex "100289f")) # U+289f BRAILLE PATTERN DOTS-123458
(def 'braille-dots-68 (hex "10028a0")) # U+28a0 BRAILLE PATTERN DOTS-68
(def 'braille-dots-168 (hex "10028a1")) # U+28a1 BRAILLE PATTERN DOTS-168
(def 'braille-dots-268 (hex "10028a2")) # U+28a2 BRAILLE PATTERN DOTS-268
(def 'braille-dots-1268 (hex "10028a3")) # U+28a3 BRAILLE PATTERN DOTS-1268
(def 'braille-dots-368 (hex "10028a4")) # U+28a4 BRAILLE PATTERN DOTS-368
(def 'braille-dots-1368 (hex "10028a5")) # U+28a5 BRAILLE PATTERN DOTS-1368
(def 'braille-dots-2368 (hex "10028a6")) # U+28a6 BRAILLE PATTERN DOTS-2368
(def 'braille-dots-12368 (hex "10028a7")) # U+28a7 BRAILLE PATTERN DOTS-12368
(def 'braille-dots-468 (hex "10028a8")) # U+28a8 BRAILLE PATTERN DOTS-468
(def 'braille-dots-1468 (hex "10028a9")) # U+28a9 BRAILLE PATTERN DOTS-1468
(def 'braille-dots-2468 (hex "10028aa")) # U+28aa BRAILLE PATTERN DOTS-2468
(def 'braille-dots-12468 (hex "10028ab")) # U+28ab BRAILLE PATTERN DOTS-12468
(def 'braille-dots-3468 (hex "10028ac")) # U+28ac BRAILLE PATTERN DOTS-3468
(def 'braille-dots-13468 (hex "10028ad")) # U+28ad BRAILLE PATTERN DOTS-13468
(def 'braille-dots-23468 (hex "10028ae")) # U+28ae BRAILLE PATTERN DOTS-23468
(def 'braille-dots-123468 (hex "10028af")) # U+28af BRAILLE PATTERN DOTS-123468
(def 'braille-dots-568 (hex "10028b0")) # U+28b0 BRAILLE PATTERN DOTS-568
(def 'braille-dots-1568 (hex "10028b1")) # U+28b1 BRAILLE PATTERN DOTS-1568
(def 'braille-dots-2568 (hex "10028b2")) # U+28b2 BRAILLE PATTERN DOTS-2568
(def 'braille-dots-12568 (hex "10028b3")) # U+28b3 BRAILLE PATTERN DOTS-12568
(def 'braille-dots-3568 (hex "10028b4")) # U+28b4 BRAILLE PATTERN DOTS-3568
(def 'braille-dots-13568 (hex "10028b5")) # U+28b5 BRAILLE PATTERN DOTS-13568
(def 'braille-dots-23568 (hex "10028b6")) # U+28b6 BRAILLE PATTERN DOTS-23568
(def 'braille-dots-123568 (hex "10028b7")) # U+28b7 BRAILLE PATTERN DOTS-123568
(def 'braille-dots-4568 (hex "10028b8")) # U+28b8 BRAILLE PATTERN DOTS-4568
(def 'braille-dots-14568 (hex "10028b9")) # U+28b9 BRAILLE PATTERN DOTS-14568
(def 'braille-dots-24568 (hex "10028ba")) # U+28ba BRAILLE PATTERN DOTS-24568
(def 'braille-dots-124568 (hex "10028bb")) # U+28bb BRAILLE PATTERN DOTS-124568
(def 'braille-dots-34568 (hex "10028bc")) # U+28bc BRAILLE PATTERN DOTS-34568
(def 'braille-dots-134568 (hex "10028bd")) # U+28bd BRAILLE PATTERN DOTS-134568
(def 'braille-dots-234568 (hex "10028be")) # U+28be BRAILLE PATTERN DOTS-234568
(def 'braille-dots-1234568 (hex "10028bf")) # U+28bf BRAILLE PATTERN DOTS-1234568
(def 'braille-dots-78 (hex "10028c0")) # U+28c0 BRAILLE PATTERN DOTS-78
(def 'braille-dots-178 (hex "10028c1")) # U+28c1 BRAILLE PATTERN DOTS-178
(def 'braille-dots-278 (hex "10028c2")) # U+28c2 BRAILLE PATTERN DOTS-278
(def 'braille-dots-1278 (hex "10028c3")) # U+28c3 BRAILLE PATTERN DOTS-1278
(def 'braille-dots-378 (hex "10028c4")) # U+28c4 BRAILLE PATTERN DOTS-378
(def 'braille-dots-1378 (hex "10028c5")) # U+28c5 BRAILLE PATTERN DOTS-1378
(def 'braille-dots-2378 (hex "10028c6")) # U+28c6 BRAILLE PATTERN DOTS-2378
(def 'braille-dots-12378 (hex "10028c7")) # U+28c7 BRAILLE PATTERN DOTS-12378
(def 'braille-dots-478 (hex "10028c8")) # U+28c8 BRAILLE PATTERN DOTS-478
(def 'braille-dots-1478 (hex "10028c9")) # U+28c9 BRAILLE PATTERN DOTS-1478
(def 'braille-dots-2478 (hex "10028ca")) # U+28ca BRAILLE PATTERN DOTS-2478
(def 'braille-dots-12478 (hex "10028cb")) # U+28cb BRAILLE PATTERN DOTS-12478
(def 'braille-dots-3478 (hex "10028cc")) # U+28cc BRAILLE PATTERN DOTS-3478
(def 'braille-dots-13478 (hex "10028cd")) # U+28cd BRAILLE PATTERN DOTS-13478
(def 'braille-dots-23478 (hex "10028ce")) # U+28ce BRAILLE PATTERN DOTS-23478
(def 'braille-dots-123478 (hex "10028cf")) # U+28cf BRAILLE PATTERN DOTS-123478
(def 'braille-dots-578 (hex "10028d0")) # U+28d0 BRAILLE PATTERN DOTS-578
(def 'braille-dots-1578 (hex "10028d1")) # U+28d1 BRAILLE PATTERN DOTS-1578
(def 'braille-dots-2578 (hex "10028d2")) # U+28d2 BRAILLE PATTERN DOTS-2578
(def 'braille-dots-12578 (hex "10028d3")) # U+28d3 BRAILLE PATTERN DOTS-12578
(def 'braille-dots-3578 (hex "10028d4")) # U+28d4 BRAILLE PATTERN DOTS-3578
(def 'braille-dots-13578 (hex "10028d5")) # U+28d5 BRAILLE PATTERN DOTS-13578
(def 'braille-dots-23578 (hex "10028d6")) # U+28d6 BRAILLE PATTERN DOTS-23578
(def 'braille-dots-123578 (hex "10028d7")) # U+28d7 BRAILLE PATTERN DOTS-123578
(def 'braille-dots-4578 (hex "10028d8")) # U+28d8 BRAILLE PATTERN DOTS-4578
(def 'braille-dots-14578 (hex "10028d9")) # U+28d9 BRAILLE PATTERN DOTS-14578
(def 'braille-dots-24578 (hex "10028da")) # U+28da BRAILLE PATTERN DOTS-24578
(def 'braille-dots-124578 (hex "10028db")) # U+28db BRAILLE PATTERN DOTS-124578
(def 'braille-dots-34578 (hex "10028dc")) # U+28dc BRAILLE PATTERN DOTS-34578
(def 'braille-dots-134578 (hex "10028dd")) # U+28dd BRAILLE PATTERN DOTS-134578
(def 'braille-dots-234578 (hex "10028de")) # U+28de BRAILLE PATTERN DOTS-234578
(def 'braille-dots-1234578 (hex "10028df")) # U+28df BRAILLE PATTERN DOTS-1234578
(def 'braille-dots-678 (hex "10028e0")) # U+28e0 BRAILLE PATTERN DOTS-678
(def 'braille-dots-1678 (hex "10028e1")) # U+28e1 BRAILLE PATTERN DOTS-1678
(def 'braille-dots-2678 (hex "10028e2")) # U+28e2 BRAILLE PATTERN DOTS-2678
(def 'braille-dots-12678 (hex "10028e3")) # U+28e3 BRAILLE PATTERN DOTS-12678
(def 'braille-dots-3678 (hex "10028e4")) # U+28e4 BRAILLE PATTERN DOTS-3678
(def 'braille-dots-13678 (hex "10028e5")) # U+28e5 BRAILLE PATTERN DOTS-13678
(def 'braille-dots-23678 (hex "10028e6")) # U+28e6 BRAILLE PATTERN DOTS-23678
(def 'braille-dots-123678 (hex "10028e7")) # U+28e7 BRAILLE PATTERN DOTS-123678
(def 'braille-dots-4678 (hex "10028e8")) # U+28e8 BRAILLE PATTERN DOTS-4678
(def 'braille-dots-14678 (hex "10028e9")) # U+28e9 BRAILLE PATTERN DOTS-14678
(def 'braille-dots-24678 (hex "10028ea")) # U+28ea BRAILLE PATTERN DOTS-24678
(def 'braille-dots-124678 (hex "10028eb")) # U+28eb BRAILLE PATTERN DOTS-124678
(def 'braille-dots-34678 (hex "10028ec")) # U+28ec BRAILLE PATTERN DOTS-34678
(def 'braille-dots-134678 (hex "10028ed")) # U+28ed BRAILLE PATTERN DOTS-134678
(def 'braille-dots-234678 (hex "10028ee")) # U+28ee BRAILLE PATTERN DOTS-234678
(def 'braille-dots-1234678 (hex "10028ef")) # U+28ef BRAILLE PATTERN DOTS-1234678
(def 'braille-dots-5678 (hex "10028f0")) # U+28f0 BRAILLE PATTERN DOTS-5678
(def 'braille-dots-15678 (hex "10028f1")) # U+28f1 BRAILLE PATTERN DOTS-15678
(def 'braille-dots-25678 (hex "10028f2")) # U+28f2 BRAILLE PATTERN DOTS-25678
(def 'braille-dots-125678 (hex "10028f3")) # U+28f3 BRAILLE PATTERN DOTS-125678
(def 'braille-dots-35678 (hex "10028f4")) # U+28f4 BRAILLE PATTERN DOTS-35678
(def 'braille-dots-135678 (hex "10028f5")) # U+28f5 BRAILLE PATTERN DOTS-135678
(def 'braille-dots-235678 (hex "10028f6")) # U+28f6 BRAILLE PATTERN DOTS-235678
(def 'braille-dots-1235678 (hex "10028f7")) # U+28f7 BRAILLE PATTERN DOTS-1235678
(def 'braille-dots-45678 (hex "10028f8")) # U+28f8 BRAILLE PATTERN DOTS-45678
(def 'braille-dots-145678 (hex "10028f9")) # U+28f9 BRAILLE PATTERN DOTS-145678
(def 'braille-dots-245678 (hex "10028fa")) # U+28fa BRAILLE PATTERN DOTS-245678
(def 'braille-dots-1245678 (hex "10028fb")) # U+28fb BRAILLE PATTERN DOTS-1245678
(def 'braille-dots-345678 (hex "10028fc")) # U+28fc BRAILLE PATTERN DOTS-345678
(def 'braille-dots-1345678 (hex "10028fd")) # U+28fd BRAILLE PATTERN DOTS-1345678
(def 'braille-dots-2345678 (hex "10028fe")) # U+28fe BRAILLE PATTERN DOTS-2345678
(def 'braille-dots-12345678 (hex "10028ff")) # U+28ff BRAILLE PATTERN DOTS-12345678

#{
  Sinhala (http://unicode.org/charts/PDF/U0D80.pdf)
  http://www.nongnu.org/sinhala/doc/transliteration/sinhala-transliteration_6.html
}#

(def 'Sinh-ng (hex "1000d82")) # U+0D82 SINHALA ANUSVARAYA
(def 'Sinh-h2 (hex "1000d83")) # U+0D83 SINHALA VISARGAYA
(def 'Sinh-a (hex "1000d85")) # U+0D85 SINHALA AYANNA
(def 'Sinh-aa (hex "1000d86")) # U+0D86 SINHALA AAYANNA
(def 'Sinh-ae (hex "1000d87")) # U+0D87 SINHALA AEYANNA
(def 'Sinh-aee (hex "1000d88")) # U+0D88 SINHALA AEEYANNA
(def 'Sinh-i (hex "1000d89")) # U+0D89 SINHALA IYANNA
(def 'Sinh-ii (hex "1000d8a")) # U+0D8A SINHALA IIYANNA
(def 'Sinh-u (hex "1000d8b")) # U+0D8B SINHALA UYANNA
(def 'Sinh-uu (hex "1000d8c")) # U+0D8C SINHALA UUYANNA
(def 'Sinh-ri (hex "1000d8d")) # U+0D8D SINHALA IRUYANNA
(def 'Sinh-rii (hex "1000d8e")) # U+0D8E SINHALA IRUUYANNA
(def 'Sinh-lu (hex "1000d8f")) # U+0D8F SINHALA ILUYANNA
(def 'Sinh-luu (hex "1000d90")) # U+0D90 SINHALA ILUUYANNA
(def 'Sinh-e (hex "1000d91")) # U+0D91 SINHALA EYANNA
(def 'Sinh-ee (hex "1000d92")) # U+0D92 SINHALA EEYANNA
(def 'Sinh-ai (hex "1000d93")) # U+0D93 SINHALA AIYANNA
(def 'Sinh-o (hex "1000d94")) # U+0D94 SINHALA OYANNA
(def 'Sinh-oo (hex "1000d95")) # U+0D95 SINHALA OOYANNA
(def 'Sinh-au (hex "1000d96")) # U+0D96 SINHALA AUYANNA
(def 'Sinh-ka (hex "1000d9a")) # U+0D9A SINHALA KAYANNA
(def 'Sinh-kha (hex "1000d9b")) # U+0D9B SINHALA MAHA. KAYANNA
(def 'Sinh-ga (hex "1000d9c")) # U+0D9C SINHALA GAYANNA
(def 'Sinh-gha (hex "1000d9d")) # U+0D9D SINHALA MAHA. GAYANNA
(def 'Sinh-ng2 (hex "1000d9e")) # U+0D9E SINHALA KANTAJA NAASIKYAYA
(def 'Sinh-nga (hex "1000d9f")) # U+0D9F SINHALA SANYAKA GAYANNA
(def 'Sinh-ca (hex "1000da0")) # U+0DA0 SINHALA CAYANNA
(def 'Sinh-cha (hex "1000da1")) # U+0DA1 SINHALA MAHA. CAYANNA
(def 'Sinh-ja (hex "1000da2")) # U+0DA2 SINHALA JAYANNA
(def 'Sinh-jha (hex "1000da3")) # U+0DA3 SINHALA MAHA. JAYANNA
(def 'Sinh-nya (hex "1000da4")) # U+0DA4 SINHALA TAALUJA NAASIKYAYA
(def 'Sinh-jnya (hex "1000da5")) # U+0DA5 SINHALA TAALUJA SANYOOGA NAASIKYAYA
(def 'Sinh-nja (hex "1000da6")) # U+0DA6 SINHALA SANYAKA JAYANNA
(def 'Sinh-tta (hex "1000da7")) # U+0DA7 SINHALA TTAYANNA
(def 'Sinh-ttha (hex "1000da8")) # U+0DA8 SINHALA MAHA. TTAYANNA
(def 'Sinh-dda (hex "1000da9")) # U+0DA9 SINHALA DDAYANNA
(def 'Sinh-ddha (hex "1000daa")) # U+0DAA SINHALA MAHA. DDAYANNA
(def 'Sinh-nna (hex "1000dab")) # U+0DAB SINHALA MUURDHAJA NAYANNA
(def 'Sinh-ndda (hex "1000dac")) # U+0DAC SINHALA SANYAKA DDAYANNA
(def 'Sinh-tha (hex "1000dad")) # U+0DAD SINHALA TAYANNA
(def 'Sinh-thha (hex "1000dae")) # U+0DAE SINHALA MAHA. TAYANNA
(def 'Sinh-dha (hex "1000daf")) # U+0DAF SINHALA DAYANNA
(def 'Sinh-dhha (hex "1000db0")) # U+0DB0 SINHALA MAHA. DAYANNA
(def 'Sinh-na (hex "1000db1")) # U+0DB1 SINHALA DANTAJA NAYANNA
(def 'Sinh-ndha (hex "1000db3")) # U+0DB3 SINHALA SANYAKA DAYANNA
(def 'Sinh-pa (hex "1000db4")) # U+0DB4 SINHALA PAYANNA
(def 'Sinh-pha (hex "1000db5")) # U+0DB5 SINHALA MAHA. PAYANNA
(def 'Sinh-ba (hex "1000db6")) # U+0DB6 SINHALA BAYANNA
(def 'Sinh-bha (hex "1000db7")) # U+0DB7 SINHALA MAHA. BAYANNA
(def 'Sinh-ma (hex "1000db8")) # U+0DB8 SINHALA MAYANNA
(def 'Sinh-mba (hex "1000db9")) # U+0DB9 SINHALA AMBA BAYANNA
(def 'Sinh-ya (hex "1000dba")) # U+0DBA SINHALA YAYANNA
(def 'Sinh-ra (hex "1000dbb")) # U+0DBB SINHALA RAYANNA
(def 'Sinh-la (hex "1000dbd")) # U+0DBD SINHALA DANTAJA LAYANNA
(def 'Sinh-va (hex "1000dc0")) # U+0DC0 SINHALA VAYANNA
(def 'Sinh-sha (hex "1000dc1")) # U+0DC1 SINHALA TAALUJA SAYANNA
(def 'Sinh-ssha (hex "1000dc2")) # U+0DC2 SINHALA MUURDHAJA SAYANNA
(def 'Sinh-sa (hex "1000dc3")) # U+0DC3 SINHALA DANTAJA SAYANNA
(def 'Sinh-ha (hex "1000dc4")) # U+0DC4 SINHALA HAYANNA
(def 'Sinh-lla (hex "1000dc5")) # U+0DC5 SINHALA MUURDHAJA LAYANNA
(def 'Sinh-fa (hex "1000dc6")) # U+0DC6 SINHALA FAYANNA
(def 'Sinh-al (hex "1000dca")) # U+0DCA SINHALA AL-LAKUNA
(def 'Sinh-aa2 (hex "1000dcf")) # U+0DCF SINHALA AELA-PILLA
(def 'Sinh-ae2 (hex "1000dd0")) # U+0DD0 SINHALA AEDA-PILLA
(def 'Sinh-aee2 (hex "1000dd1")) # U+0DD1 SINHALA DIGA AEDA-PILLA
(def 'Sinh-i2 (hex "1000dd2")) # U+0DD2 SINHALA IS-PILLA
(def 'Sinh-ii2 (hex "1000dd3")) # U+0DD3 SINHALA DIGA IS-PILLA
(def 'Sinh-u2 (hex "1000dd4")) # U+0DD4 SINHALA PAA-PILLA
(def 'Sinh-uu2 (hex "1000dd6")) # U+0DD6 SINHALA DIGA PAA-PILLA
(def 'Sinh-ru2 (hex "1000dd8")) # U+0DD8 SINHALA GAETTA-PILLA
(def 'Sinh-e2 (hex "1000dd9")) # U+0DD9 SINHALA KOMBUVA
(def 'Sinh-ee2 (hex "1000dda")) # U+0DDA SINHALA DIGA KOMBUVA
(def 'Sinh-ai2 (hex "1000ddb")) # U+0DDB SINHALA KOMBU DEKA
(def 'Sinh-o2 (hex "1000ddc")) # U+0DDC SINHALA KOMBUVA HAA AELA-PILLA
(def 'Sinh-oo2 (hex "1000ddd")) # U+0DDD SINHALA KOMBUVA HAA DIGA AELA-PILLA
(def 'Sinh-au2 (hex "1000dde")) # U+0DDE SINHALA KOMBUVA HAA GAYANUKITTA
(def 'Sinh-lu2 (hex "1000ddf")) # U+0DDF SINHALA GAYANUKITTA
(def 'Sinh-ruu2 (hex "1000df2")) # U+0DF2 SINHALA DIGA GAETTA-PILLA
(def 'Sinh-luu2 (hex "1000df3")) # U+0DF3 SINHALA DIGA GAYANUKITTA
(def 'Sinh-kunddaliya (hex "1000df4")) # U+0DF4 SINHALA KUNDDALIYA
