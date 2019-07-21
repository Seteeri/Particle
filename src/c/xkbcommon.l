(symbols 'xkb 'pico)


# enum xkb_context_flags
(def 'CONTEXT-NO-FLAGS 0)
(def 'CONTEXT-NO-DEFAULT-INCLUDES (>> 0 1))
(def 'CONTEXT-NO-ENVIRONMENT-NAMES (>> -1 1))


# enum xkb_keymap_compile_flags
(def 'KEYMAP-COMPILE-NO-FLAGS 0)


# enum xkb_key_direction
(def 'KEY-UP 0)
(def 'KEY-DOWN 1)

# enum xkb_state_component
(def 'STATE-MODS-DEPRESSED   (>>  0 1))
(def 'STATE-MODS-LATCHED     (>> -1 1))
(def 'STATE-MODS-LOCKED      (>> -2 1))
(def 'STATE-MODS-EFFECTIVE   (>> -3 1))
(def 'STATE-LAYOUT-DEPRESSED (>> -4 1))
(def 'STATE-LAYOUT-LATCHED   (>> -5 1))
(def 'STATE-LAYOUT-LOCKED    (>> -6 1))
(def 'STATE-LAYOUT-EFFECTIVE (>> -7 1))
(def 'STATE-LEDS             (>> -8 1))


#{
const char * 	rules
const char * 	model
const char * 	layout
const char * 	variant
const char * 	options
}#
(def 'SZ-RULE-NAMES (* 5 8))


(def '*buf-keysym (native "@" "malloc" 'N 64))


(de context-new (Flags)
  (native "libxkbcommon.so" "xkb_context_new" 'N Flags))


(de keymap-new-from-names (Context Names Flags)
  (native "libxkbcommon.so" "xkb_keymap_new_from_names" 'N Context Names Flags))


(de state-new (Keymap)
  (native "libxkbcommon.so" "xkb_state_new" 'N Keymap))


(de state-key-get-one-sym (State Keycode)
  (native "libxkbcommon.so" "xkb_state_key_get_one_sym" 'I State Keycode))


(de keysym-get-name (Keysym Buffer Size)
  (let Length (native "libxkbcommon.so" "xkb_keysym_get_name" 'I Keysym (if Buffer Buffer *buf-keysym) (if Size Size 64)))
    (unless (= Length -1)
      (struct (if Buffer Buffer *buf-keysym) 'S)))


(de state-key-get-utf8 (State Keycode Buffer Size)
  (let Length (native "libxkbcommon.so" "xkb_state_key_get_utf8" 'I State Keycode (if Buffer Buffer *buf-keysym) (if Size Size 64)))
    (unless (= Length -1)
      (struct (if Buffer Buffer *buf-keysym) 'S)))


(de state-update-key (State Keycode Direction)
  (native "libxkbcommon.so" "xkb_state_update_key" 'I State Keycode Direction))


(de keymap-key-repeats (Keymap Keycode)
  (native "libxkbcommon.so" "xkb_keymap_key_repeats" 'I Keymap Keycode))


(de state-serialize-mods (State Components)
  (native "libxkbcommon.so" "xkb_state_serialize_mods" 'I State Components))
  
  
(de state-update-mask (State Depressed-Mods Latched-Mods Locked-Mods 
                       Depressed-Layout Latched-Layout Locked-Layout)
  (native "libxkbcommon.so" "xkb_state_update_mask" 'I State Depressed-Mods Latched-Mods Locked-Mods 
                                                       Depressed-Layout Latched-Layout Locked-Layout))
                                                       
                                                       
(de state-serialize-layout (State Components)
  (native "libxkbcommon.so" "xkb_state_serialize_layout" 'I State Components))                                                       