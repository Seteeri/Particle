(symbols 'li 'pico)

(class +Li)
(dm T ()
    (=: interface   (make-interface))
    (=: context     (path-create-context (: interface) NIL))
    (=: fd-context  (get-fd (: context)))
    (add-devices (: context)))


(dm end-input> ()
    
    #This implicitly frees context
    (unref (: context))
    (=: context NIL)
    (=: fd-context NIL)

    (when (: interface)
      (native "@" "free" NIL (: interface))
      (=: interface NIL)))


(de make-interface ()
    (let (Interface (native "@" "malloc" 'N 16) #libinput-interface struct
          OR (lisp 'handle-open-restricted
	       '((Path Flags User-data)
		 (native "@" "open" 'I (struct Path 'S) (struct Flags 'I))))
	  CR (lisp 'handle-close-restricted
	       '((Fd User-data)
		 (native "@" "close" 'I (struct Fd 'I)))))
      (struct Interface 'N
	      (cons OR 8)
	      (cons CR 8))))


(de add-devices (Context)
    (for File (dir DEVICES-PATH)
	 (when (and (not (= File "by-id"))
		    (not (= File "by-path")))
	   (let (Filepath (pack DEVICES-PATH "/" File)
			  Device (path-add-device Context Filepath))
	     (unless (= Device 0)
	       (if (not (or (device-has-capability Device
						   DEVICE-CAP-KEYBOARD)
			    (device-has-capability Device
						   DEVICE-CAP-POINTER)))
		   (path-remove-device Device)))))))    


#{
typedef union epoll_data {
void    *ptr;
int      fd;
uint32_t u32;
uint64_t u64;
} epoll_data_t;
# 8+4+4+8

struct epoll_event {
uint32_t     events;    /* Epoll events */
epoll_data_t data;      /* User data variable */
};
# 4+(8+4+4+8)
}#


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




(de unref (Context)
    (native "libinput.so" "libinput_unref" 'N Context))

(de path-create-context (Interface User-date)
    (native "libinput.so" "libinput_path_create_context" 'N Interface User-date))

(de path-add-device (Context Path)
    (native "libinput.so" "libinput_path_add_device" 'N Context Path))

(de path-remove-device (Device)
    (native "libinput.so" "libinput_path_remove_device" NIL Device))

(de get-fd (Context)
    (native "libinput.so" "libinput_get_fd" 'I Context))

(de dispatch (Context)
    (native "libinput.so" "libinput_dispatch" 'I Context))

(de device-has-capability (Device Capability)
    (native "libinput.so" "libinput_device_has_capability" 'I Device Capability))

(de device-ref (Device)
    (native "libinput.so" "libinput_device_ref" 'N Device))

(de device-unref (Device)
    (native "libinput.so" "libinput_device_unref" 'N Device))

(de get-event (Context)
    (native "libinput.so" "libinput_get_event" 'N Context))

(de event-get-type (Event)
    (native "libinput.so" "libinput_event_get_type" 'I Event))

(de event-destroy (Event)
    (native "libinput.so" "libinput_event_destroy" NIL Event))


(de event-get-keyboard-event (Event)
    (native "libinput.so" "libinput_event_get_keyboard_event" 'N Event))

(de event-keyboard-get-time (Keyboard-event)
    (native "libinput.so" "libinput_event_keyboard_get_time" 'I Keyboard-event))

(de event-keyboard-get-time-usec (Keyboard-event)
    (native "libinput.so" "libinput_event_keyboard_get_time_usec" 'N Keyboard-event))

(de event-keyboard-get-key (Keyboard-event)
    (native "libinput.so" "libinput_event_keyboard_get_key" 'I Keyboard-event))

(de event-keyboard-get-key-state (Keyboard-event)
    #Return symbol (enum)
    (native "libinput.so" "libinput_event_keyboard_get_key_state" 'I Keyboard-event))

(de event-keyboard-get-base-event (Event)
    (native "libinput.so" "libinput_event_keyboard_get_base_event" 'N Event))

(de event-keyboard-get-seat-key-count (Event)
    (native "libinput.so" "libinput_event_keyboard_get_seat_key_count" 'I Event))


(de event-get-pointer-event (Event)
    (native "libinput.so" "libinput_event_get_pointer_event" 'N Event))

(de event-pointer-get-time (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_time" 'I Pointer-event))

(de event-pointer-get-time-usec (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_time_usec" 'N Pointer-event))

(de event-pointer-get-button (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_button" 'I Pointer-event))

(de event-pointer-get-button-state (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_button_state" 'I Pointer-event))

(de event-pointer-get-dx (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_dx" +1.0 Pointer-event))

(de event-pointer-get-dy (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_dy" +1.0 Pointer-event))

(de event-pointer-get-dx-unaccelerated (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_dx_unaccelerated" +1.0 Pointer-event))

(de event-pointer-get-dy-unaccelerated (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_dy_unaccelerated" +1.0 Pointer-event))

(de event-pointer-get-seat-button-count (Pointer-event)
    (native "libinput.so" "libinput_event_pointer_get_seat_button_count" 'I Pointer-event))

(de event-pointer-has-axis (Event Axis)
    (native "libinput.so" "libinput_event_pointer_has_axis" 'I Event Axis))

(de event-pointer-get-axis-source (Event)
    #ENUM
    (native "libinput.so" "libinput_event_pointer_get_axis_source" 'I Event))

(de event-pointer-get-axis-value (Event Axis)
    (native "libinput.so" "libinput_event_pointer_get_axis_value" '+1.0 Event Axis))

(de event-pointer-get-axis-value-discrete (Event Axis)
    (native "libinput.so" "libinput_event_pointer_get_axis_value_discrete" +1.0 Event Axis))


(de event-get-touch-event (Pointer-event)
    (native "libinput.so" "libinput_event_get_touch_event" 'N Pointer-event))

(de event-touch-get-time (Touch-event)
    (native "libinput.so" "libinput_event_touch_get_time" 'I Touch-event))

(de event-touch-get-time-usec (Touch-event)
    (native "libinput.so" "libinput_event_touch_get_time_usec" 'N Touch-event))

(de event-touch-get-slot get-time (Touch-event)
    (native "libinput.so" "libinput_event_touch_get_slot" 'I Touch-event))

(de event-touch-get-seat-slot get-time (Touch-event)
    (native "libinput.so" "libinput_event_touch_get_seat_slot" 'I Touch-event))

(de event-touch-get-x (Touch-event)
    (native "libinput.so" "libinput_event_touch_get_x" +1.0 Touch-event))

(de event-touch-get-y (Touch-event)
    (native "libinput.so" "libinput_event_touch_get_y" +1.0 Touch-event))

(de event-touch-get-x-transformed (Touch-event Width)
    (native "libinput.so" "libinput_event_touch_get_x_transformed" +1.0 Touch-event Width))

(de event-touch-get-y-transformed (Touch-event Width)
    (native "libinput.so" "libinput_event_touch_get_y_transformed" +1.0 Touch-event Height))


(de event-get-tablet-tool-event (Tablet-tool-event)
    (native "libinput.so" "libinput_event_get_tablet_tool_event" 'N Tablet-tool-event))

(de event-tablet-tool-get-tool (Tablet-tool)
    (native "libinput.so" "libinput_event_tablet_tool_get_tool" 'N Tablet-tool))

(de event-tablet-tool-get-x (Event)
    (native "libinput.so" "libinput_event_tablet_tool_get_x" +1.0 Event))

(de event-tablet-tool-get-y (Event)
    (native "libinput.so" "libinput_event_tablet_tool_get_y" +1.0 Event))


(de event-tablet-tool-get-pressure (Event)
    (native "libinput.so" "libinput_event_tablet_tool_get_pressure" +1.0 Event))

(de event-tablet-tool-get-distance (Event)
    (native "libinput.so" "libinput_event_tablet_tool_get_distance" +1.0 Event))


(de tablet-tool-has-pressure (Tool)
    (native "libinput.so" "libinput_tablet_tool_has_pressure" 'I Tool))

(de tablet-tool-has-distance (Tool)
    (native "libinput.so" "libinput_tablet_tool_has_distance" 'I Tool))

(de tablet-tool-has-tilt (Tool)
    (native "libinput.so" "libinput_tablet_tool_has_tilt" 'I Tool))

(de tablet-tool-has-rotation (Tool)
    (native "libinput.so" "libinput_tablet_tool_has_rotation" 'I Tool))
 
(de tablet-tool-has-slider (Tool)
    (native "libinput.so" "libinput_tablet_tool_has_slider" 'I Tool))

(de tablet-tool-has-wheel (Tool)
    (native "libinput.so" "libinput_tablet_tool_has_wheel" 'I Tool))

(de tablet-tool-has-button (Tool Code)
    (native "libinput.so" "libinput_tablet_tool_has_button" 'I Tool Code))

(de tablet-tool-tip-state (Event)
    (native "libinput.so" "libinput_event_tablet_tool_get_tip_state" 'I Event))

(de event-tablet-tool-get-x-transformed (Touch-event Width)
    (native "libinput.so" "libinput_event_tablet_tool_get_x_transformed" +1.0 Touch-event Width))

(de event-tablet-tool-get-y-transformed (Touch-event Height)
    (native "libinput.so" "libinput_event_tablet_tool_get_y_transformed" +1.0 Touch-event Height))
