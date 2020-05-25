# RENAME FILE TO LIBINPUT

(symbols 'li 'pico)


(def 'DEVICES-PATH "/dev/input")

#ENUMS
https://wayland.freedesktop.org/libinput/doc/latest/api/group__device.html#ga76c012d8f6d7656fb795dc7bdf9d6551

#DEVICES
(def 'DEVICE-CAP-KEYBOARD 0)
(def 'DEVICE-CAP-POINTER 1)
(def 'DEVICE-CAP-TOUCH 2)
(def 'DEVICE-CAP-TABLET-TOOL 3)
(def 'DEVICE-CAP-TABLET-PAD 4)
(def 'DEVICE-CAP-GESTURE 5)

#EVENT TYPES
(def 'NONE 0)
(def 'DEVICE-ADDED 1)
(def 'DEVICE-REMOVED 2)
(def 'KEYBOARD-KEY 300)
(def 'POINTER-MOTION 400)
(def 'POINTER-MOTION-ABSOLUTE 401)
(def 'POINTER-BUTTON 402)
(def 'POINTER-AXIS 403)
(def 'TOUCH-DOWN 500)
(def 'TOUCH-UP 501)
(def 'TOUCH-MOTION 502)
(def 'TOUCH-CANCEL 503)
(def 'TOUCH-FRAME 504)
(def 'TABLET-TOOL-AXIS 600)
(def 'TABLET-TOOL-PROXIMITY 601)
(def 'TABLET-TOOL-TIP 602)
(def 'TABLET-TOOL-BUTTON 603)
(def 'TABLET-PAD-BUTTON 700)
(def 'TABLET-PAD-RING 701)
(def 'TABLET-PAD-STRIP 702)
(def 'GESTURE-SWIPE-BEGIN 800)
(def 'GESTURE-SWIPE-UPDATE 801)
(def 'GESTURE-SWIPE-END 802)
(def 'GESTURE-PINCH-BEGIN 803)
(def 'GESTURE-PINCH-UPDATE 804)
(def 'GESTURE-PINCH-END 805)
(def 'SWITCH-TOGGLE 900)

(de get-event-type-name (Type)
  (case Type
    (0 'NONE)
    (1 'DEVICE-ADDED)
    (2 'DEVICE-REMOVED)
    (300 'KEYBOARD-KEY)
    (400 'POINTER-MOTION)
    (401 'POINTER-MOTION-ABSOLUTE)
    (402 'POINTER-BUTTON)
    (403 'POINTER-AXIS)
    (500 'TOUCH-DOWN)
    (501 'TOUCH-UP)
    (502 'TOUCH-MOTION)
    (503 'TOUCH-CANCEL)
    (504 'TOUCH-FRAME)
    (600 'TABLET-TOOL-AXIS)
    (601 'TABLET-TOOL-PROXIMITY)
    (602 'TABLET-TOOL-TIP)
    (603 'TABLET-TOOL-BUTTON)
    (700 'TABLET-PAD-BUTTON)
    (701 'TABLET-PAD-RING)
    (702 'TABLET-PAD-STRIP)
    (800 'GESTURE-SWIPE-BEGIN)
    (801 'GESTURE-SWIPE-UPDATE)
    (802 'GESTURE-SWIPE-END)
    (803 'GESTURE-PINCH-BEGIN)
    (804 'GESTURE-PINCH-UPDATE)
    (805 'GESTURE-PINCH-END)
    (900 'SWITCH-TOGGLE)))

#POINTER AXIS SOURCE
(def 'POINTER-AXIS-SOURCE-WHEEL      1)
(def 'POINTER-AXIS-SOURCE-FINGER     2)	
(def 'POINTER-AXIS-SOURCE-CONTINUOUS 3)	
(def 'POINTER-AXIS-SOURCE-WHEEL-TILT 4)

(de get-pointer-axis-source-name (Source)
  (case Source
    (1 'POINTER-AXIS-SOURCE-WHEEL)
    (2 'POINTER-AXIS-SOURCE-FINGER)	
    (3 'POINTER-AXIS-SOURCE-CONTINUOUS)	
    (4 'POINTER-AXIS-SOURCE-WHEEL-TILT)))


#POINTER AXIS
(def 'POINTER-AXIS-SCROLL-VERTICAL 0)
(def 'POINTER-AXIS-SCROLL-HORIZONTAL 1)


(de get-pointer-axis-name (Axis)
  (case Axis
    (0 'POINTER-AXIS-SCROLL-VERTICAL)
    (1 'POINTER-AXIS-SCROLL-HORIZONTAL)))


(class +Li)
(dm T ()
  (=: interface   (make-interface))
  (=: context     (path-create-context (: interface) NIL))
  (=: fd-context  (get-fd (: context)))
  (add-devices (: context)))


(dm end> ()
  
  #This implicitly frees context
  (unref (: context))
  (=: context)
  (=: fd-context)

  (when (: interface)
    (native "@" "free" NIL (: interface))
    (=: interface)))


(de make-interface ()

  (let (Interface (native "@" "malloc" 'N 16) #libinput-interface struct
        OR (lisp 'serv-open-restricted
                 '((Path Flags User-data)
                   (native "@" "open" 'I (struct Path 'S) (struct Flags 'I))))
        CR (lisp 'serv-close-restricted
                 '((Fd User-data)
                   (native "@" "close" 'I (struct Fd 'I)))))
                   
    (struct Interface 'N
                      (cons OR 8)
                      (cons CR 8))))


(de add-devices (Ctx)
  (for File (dir DEVICES-PATH)
    (when (and (not (= File "by-id"))
               (not (= File "by-path")))
      (let Fp (pack DEVICES-PATH "/" File)
        (path-add-device Ctx Fp)))))


(de unref (Ctx)
    (native "libinput.so.10.13.0" "libinput_unref" 'N Ctx))

(de path-create-context (Interface User-date)
    (native "libinput.so.10.13.0" "libinput_path_create_context" 'N Interface User-date))

(de path-add-device (Ctx Path)
    (native "libinput.so.10.13.0" "libinput_path_add_device" 'N Ctx Path))

(de path-remove-device (Dev)
    (native "libinput.so.10.13.0" "libinput_path_remove_device" NIL Dev))

(de get-fd (Ctx)
    (native "libinput.so.10.13.0" "libinput_get_fd" 'I Ctx))

(de dispatch (Ctx)
    (native "libinput.so.10.13.0" "libinput_dispatch" 'I Ctx))

(de device-has-capability (Dev Capability)
    (native "libinput.so.10.13.0" "libinput_device_has_capability" 'I Dev Capability))

(de device-ref (Dev)
    (native "libinput.so.10.13.0" "libinput_device_ref" 'N Dev))

(de device-unref (Dev)
    (native "libinput.so.10.13.0" "libinput_device_unref" 'N Dev))

(de get-event (Ctx)
    (native "libinput.so.10.13.0" "libinput_get_event" 'N Ctx))

(de event-get-type (Ev)
    (native "libinput.so.10.13.0" "libinput_event_get_type" 'I Ev))

(de event-destroy (Ev)
    (native "libinput.so.10.13.0" "libinput_event_destroy" NIL Ev))


(de get-kb-event (Ev)
    (native "libinput.so.10.13.0" "libinput_event_get_keyboard_event" 'N Ev))

(de kb-get-time (Kb-Ev)
    (native "libinput.so.10.13.0" "libinput_event_keyboard_get_time" 'I Kb-Ev))

(de kb-get-time-usec (Kb-Ev)
    (native "libinput.so.10.13.0" "libinput_event_keyboard_get_time_usec" 'N Kb-Ev))

(de kb-get-key (Kb-Ev)
    (native "libinput.so.10.13.0" "libinput_event_keyboard_get_key" 'I Kb-Ev))

(de kb-get-key-state (Kb-Ev)
    #Return symbol (enum)
    (native "libinput.so.10.13.0" "libinput_event_keyboard_get_key_state" 'I Kb-Ev))

(de kb-get-base-event (Ev)
    (native "libinput.so.10.13.0" "libinput_event_keyboard_get_base_event" 'N Ev))

(de kb-get-seat-key-count (Ev)
    (native "libinput.so.10.13.0" "libinput_event_keyboard_get_seat_key_count" 'I Ev))


(de get-ptr-event (Ev)
    (native "libinput.so.10.13.0" "libinput_event_get_pointer_event" 'N Ev))

(de ptr-get-time (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_time" 'I Ptr-Ev))

(de ptr-get-time-usec (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_time_usec" 'N Ptr-Ev))

(de ptr-get-button (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_button" 'I Ptr-Ev))

(de ptr-get-button-state (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_button_state" 'I Ptr-Ev))

(de ptr-get-dx (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_dx" +1.0 Ptr-Ev))

(de ptr-get-dy (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_dy" +1.0 Ptr-Ev))

(de ptr-get-dx-unaccelerated (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_dx_unaccelerated" +1.0 Ptr-Ev))

(de ptr-get-dy-unaccelerated (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_dy_unaccelerated" +1.0 Ptr-Ev))

(de ptr-get-seat-button-count (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_seat_button_count" 'I Ptr-Ev))

(de ptr-has-axis (Ev Axis)
    (native "libinput.so.10.13.0" "libinput_event_pointer_has_axis" 'I Ev Axis))

(de ptr-get-axis-source (Ev)
    #ENUM
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_axis_source" 'I Ev))

(de ptr-get-axis-value (Ev Axis)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_axis_value" '+1.0 Ev Axis))

(de ptr-get-axis-value-discrete (Ev Axis)
    (native "libinput.so.10.13.0" "libinput_event_pointer_get_axis_value_discrete" +1.0 Ev Axis))


(de get-tch-event (Ptr-Ev)
    (native "libinput.so.10.13.0" "libinput_event_get_touch_event" 'N Ptr-Ev))

(de tch-get-time (Tch-Ev)
    (native "libinput.so.10.13.0" "libinput_event_touch_get_time" 'I Tch-Ev))

(de tch-get-time-usec (Tch-Ev)
    (native "libinput.so.10.13.0" "libinput_event_touch_get_time_usec" 'N Tch-Ev))

(de tch-get-slot get-time (Tch-Ev)
    (native "libinput.so.10.13.0" "libinput_event_touch_get_slot" 'I Tch-Ev))

(de tch-get-seat-slot get-time (Tch-Ev)
    (native "libinput.so.10.13.0" "libinput_event_touch_get_seat_slot" 'I Tch-Ev))

(de tch-get-x (Tch-Ev)
    (native "libinput.so.10.13.0" "libinput_event_touch_get_x" +1.0 Tch-Ev))

(de tch-get-y (Tch-Ev)
    (native "libinput.so.10.13.0" "libinput_event_touch_get_y" +1.0 Tch-Ev))

(de tch-get-x-transformed (Tch-Ev Width)
    (native "libinput.so.10.13.0" "libinput_event_touch_get_x_transformed" +1.0 Tch-Ev Width))

(de tch-get-y-transformed (Tch-Ev Width)
    (native "libinput.so.10.13.0" "libinput_event_touch_get_y_transformed" +1.0 Tch-Ev Height))


(de get-tool-event (Tablet-tool-event)
    (native "libinput.so.10.13.0" "libinput_event_get_tablet_tool_event" 'N Tablet-tool-event))

(de tt-get-tool (Tablet-tool)
    (native "libinput.so.10.13.0" "libinput_event_tablet_tool_get_tool" 'N Tablet-tool))

(de tt-get-x (Ev)
    (native "libinput.so.10.13.0" "libinput_event_tablet_tool_get_x" +1.0 Ev))

(de tt-get-y (Ev)
    (native "libinput.so.10.13.0" "libinput_event_tablet_tool_get_y" +1.0 Ev))


(de tt-get-pressure (Ev)
    (native "libinput.so.10.13.0" "libinput_event_tablet_tool_get_pressure" +1.0 Ev))

(de tt-get-distance (Ev)
    (native "libinput.so.10.13.0" "libinput_event_tablet_tool_get_distance" +1.0 Ev))


(de tool-has-pressure (Tool)
    (native "libinput.so.10.13.0" "libinput_tablet_tool_has_pressure" 'I Tool))

(de tool-has-distance (Tool)
    (native "libinput.so.10.13.0" "libinput_tablet_tool_has_distance" 'I Tool))

(de tool-has-tilt (Tool)
    (native "libinput.so.10.13.0" "libinput_tablet_tool_has_tilt" 'I Tool))

(de tool-has-rotation (Tool)
    (native "libinput.so.10.13.0" "libinput_tablet_tool_has_rotation" 'I Tool))
 
(de tool-has-slider (Tool)
    (native "libinput.so.10.13.0" "libinput_tablet_tool_has_slider" 'I Tool))

(de tool-has-wheel (Tool)
    (native "libinput.so.10.13.0" "libinput_tablet_tool_has_wheel" 'I Tool))

(de tool-has-button (Tool Code)
    (native "libinput.so.10.13.0" "libinput_tablet_tool_has_button" 'I Tool Code))

(de tt-get-tip-state (Ev)
    (native "libinput.so.10.13.0" "libinput_event_tablet_tool_get_tip_state" 'I Ev))

(de tt-get-x-transformed (Tch-Ev Width)
    (native "libinput.so.10.13.0" "libinput_event_tablet_tool_get_x_transformed" +1.0 Tch-Ev Width))

(de tt-get-y-transformed (Tch-Ev Height)
    (native "libinput.so.10.13.0" "libinput_event_tablet_tool_get_y_transformed" +1.0 Tch-Ev Height))
