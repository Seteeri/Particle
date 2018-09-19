(in-package :protoform.controller)

(defclass xkb () 
  ((context :accessor context :initarg :context :initform nil)
   (keymap :accessor keymap :initarg :keymap :initform nil)
   (state :accessor state :initarg :state :initform nil)
   
   (repeat-key :accessor repeat-key :initarg :repeat-key :initform nil)
   (repeat-char :accessor repeat-char :initarg :repeat-char :initform nil)
   (repeat-timer :accessor repeat-timer :initarg :repeat-timer :initform nil)
   (repeat-delay :accessor repeat-delay :initarg :repeat-delay :initform 400)
   (repeat-interval :accessor repeat-interval :initarg :repeat-interval :initform 40)

   (mods-depressed :accessor mods-depressed :initarg :mods-depressed :initform nil)
   (mods-locked :accessor mods-locked :initarg :mods-locked :initform nil)
   (mods-latched :accessor mods-latched :initarg :mods-latched :initform nil)
   (mods-group :accessor mods-group :initarg :mods-group :initform nil)))

(defun init-xkb ()

  ;; RMLVO
  ;;   struct xkb_rule_names names = {
  ;;     .rules = NULL,
  ;;     .model = "pc105",
  ;;     .layout = "is",
  ;;     .variant = "dvorak",
  ;;     .options = "terminate:ctrl_alt_bksp"
  ;; };

  (let* ((context (xkb:xkb-context-new 0))
	 (keymap (xkb:new-keymap-from-names context "evdev" "pc104" "us" "" ""))
	 (state (xkb:xkb-state-new keymap))
	 (depressed-mods (xkb:xkb-state-serialize-mods state 1))
	 (latched-mods (xkb:xkb-state-serialize-mods state 2))
	 (locked-mods (xkb:xkb-state-serialize-mods state 4))
	 (group-mods (xkb:xkb-state-serialize-mods state 8))
	 (xkb (make-instance 'xkb
			     :context context
			     :keymap keymap
			     :state state
			     :mods-depressed depressed-mods
			     :mods-latched latched-mods
			     :mods-locked locked-mods
			     :mods-group group-mods)))

    (xkb:xkb-state-update-mask state
			       0 latched-mods locked-mods
			       0 0 0)

    (defparameter *xkb* xkb)
    
    xkb))
