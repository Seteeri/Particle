(in-package :protoform.model)

(defun handle-event-tablet-tool-axis (event)
  (let* ((event-tablet-tool (libinput:event-get-tablet-tool-event         event))
	 (tool              (libinput:event-tablet-tool-get-tool          event-tablet-tool))
	 (x-mm              (libinput:event-tablet-tool-get-x             event-tablet-tool))
	 (y-mm              (libinput:event-tablet-tool-get-y             event-tablet-tool))
	 (x                 (libinput:event-tablet-tool-get-x-transformed event-tablet-tool 2880))
	 (y                 (libinput:event-tablet-tool-get-y-transformed event-tablet-tool 1920)))

    ;; x and y in mm
    
    (format t "x, y = ~,2f, ~,2f " x y)
    
    (when (or (libinput:tablet-tool-has-distance tool)
	      (libinput:tablet-tool-has-pressure tool))
      (let ((dist (libinput:event-tablet-tool-get-distance event-tablet-tool))
	    (pressure (libinput:event-tablet-tool-get-pressure event-tablet-tool)))

	(format t "| pressure = ~,2f" pressure))))

  (format t "~%"))
  
  ;; struct libinput_event_tablet_tool *t = libinput_event_get_tablet_tool_event(ev);

  ;; print_event_time(libinput_event_tablet_tool_get_time(t));
  ;; print_tablet_axes(t);
  ;; printq("\n");
  

(defun handle-event-tablet-tool-proximity (event)
  (let* ((event-tablet-tool (libinput:event-get-tablet-tool-event         event))
	 (tool              (libinput:event-tablet-tool-get-tool          event-tablet-tool))
	 (x-mm              (libinput:event-tablet-tool-get-x             event-tablet-tool))
	 (y-mm              (libinput:event-tablet-tool-get-y             event-tablet-tool))
	 (x                 (libinput:event-tablet-tool-get-x-transformed event-tablet-tool 2880))
	 (y                 (libinput:event-tablet-tool-get-y-transformed event-tablet-tool 1920)))

    ;; enum libinput_tablet_tool_tip_state {
    ;; 	LIBINPUT_TABLET_TOOL_TIP_UP = 0,
    ;; 	LIBINPUT_TABLET_TOOL_TIP_DOWN = 1,
    ;; };
    
    (format t "tip state = ~S~%" (libinput:event-tablet-tool-get-tip-state event-tablet-tool))))
  
  ;; struct libinput_event_tablet_tool *t = libinput_event_get_tablet_tool_event(ev);
  ;; struct libinput_tablet_tool *tool = libinput_event_tablet_tool_get_tool(t);
  ;; enum libinput_tablet_tool_proximity_state state;
  ;; const char *tool_str,
  ;;            *state_str;

  ;; switch (libinput_tablet_tool_get_type(tool)) {
  ;; case LIBINPUT_TABLET_TOOL_TYPE_PEN:
  ;; 	tool_str = "pen";
  ;; 	break;
  ;; case LIBINPUT_TABLET_TOOL_TYPE_ERASER:
  ;; 	tool_str = "eraser";
  ;; 	break;
  ;; case LIBINPUT_TABLET_TOOL_TYPE_BRUSH:
  ;; 	tool_str = "brush";
  ;; 	break;
  ;; case LIBINPUT_TABLET_TOOL_TYPE_PENCIL:
  ;; 	tool_str = "pencil";
  ;; 	break;
  ;; case LIBINPUT_TABLET_TOOL_TYPE_AIRBRUSH:
  ;; 	tool_str = "airbrush";
  ;; 	break;
  ;; case LIBINPUT_TABLET_TOOL_TYPE_MOUSE:
  ;; 	tool_str = "mouse";
  ;; 	break;
  ;; case LIBINPUT_TABLET_TOOL_TYPE_LENS:
  ;; 	tool_str = "lens";
  ;; 	break;
  ;; default:
  ;; 	abort();
  ;; }

  ;; state = libinput_event_tablet_tool_get_proximity_state(t);

  ;; print_event_time(libinput_event_tablet_tool_get_time(t));

  ;; if (state == LIBINPUT_TABLET_TOOL_PROXIMITY_STATE_IN) {
  ;; 	print_tablet_axes(t);
  ;; 	state_str = "proximity-in";
  ;; } else if (state == LIBINPUT_TABLET_TOOL_PROXIMITY_STATE_OUT) {
  ;; 	print_tablet_axes(t);
  ;; 	state_str = "proximity-out";
  ;; } else {
  ;; 	abort();
  ;; }

  ;; printq("\t%s (%#" PRIx64 ", id %#" PRIx64 ") %s ",
  ;;        tool_str,
  ;;        libinput_tablet_tool_get_serial(tool),
  ;;        libinput_tablet_tool_get_tool_id(tool),
  ;;        state_str);

  ;; if (state == LIBINPUT_TABLET_TOOL_PROXIMITY_STATE_IN) {
  ;; 	printq("\taxes:");
  ;; 	if (libinput_tablet_tool_has_distance(tool))
  ;; 		printq("d");
  ;; 	if (libinput_tablet_tool_has_pressure(tool))
  ;; 		printq("p");
  ;; 	if (libinput_tablet_tool_has_tilt(tool))
  ;; 		printq("t");
  ;; 	if (libinput_tablet_tool_has_rotation(tool))
  ;; 		printq("r");
  ;; 	if (libinput_tablet_tool_has_slider(tool))
  ;; 		printq("s");
  ;; 	if (libinput_tablet_tool_has_wheel(tool))
  ;; 		printq("w");

  ;; 	printq("\tbtn:");
  ;; 	if (libinput_tablet_tool_has_button(tool, BTN_TOUCH))
  ;; 		printq("T");
  ;; 	if (libinput_tablet_tool_has_button(tool, BTN_STYLUS))
  ;; 		printq("S");
  ;; 	if (libinput_tablet_tool_has_button(tool, BTN_STYLUS2))
  ;; 		printq("S2");
  ;; 	if (libinput_tablet_tool_has_button(tool, BTN_LEFT))
  ;; 		printq("L");
  ;; 	if (libinput_tablet_tool_has_button(tool, BTN_MIDDLE))
  ;; 		printq("M");
  ;; 	if (libinput_tablet_tool_has_button(tool, BTN_RIGHT))
  ;; 		printq("R");
  ;; 	if (libinput_tablet_tool_has_button(tool, BTN_SIDE))
  ;; 		printq("Sd");
  ;; 	if (libinput_tablet_tool_has_button(tool, BTN_EXTRA))
  ;; 		printq("Ex");
  ;; }

  ;; printq("\n");
  

(defun handle-event-tablet-tool-tip (event)

  ;; struct libinput_event_tablet_tool *t = libinput_event_get_tablet_tool_event(ev);
  ;; enum libinput_tablet_tool_tip_state state;

  ;; print_event_time(libinput_event_tablet_tool_get_time(t));

  ;; print_tablet_axes(t);

  ;; state = libinput_event_tablet_tool_get_tip_state(t);
  ;; printq(" %s\n", state == LIBINPUT_TABLET_TOOL_TIP_DOWN ? "down" : "up");  

  t)
