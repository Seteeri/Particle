;;;; /*
;;;;  * XFree86 vendor specific keysyms.
;;;;  *
;;;;  * The XFree86 keysym range is 0x10080001 - 0x1008FFFF.
;;;;  *
;;;;  * X.Org will not be adding to the XF86 set of keysyms, though they have
;;;;  * been adopted and are considered a "standard" part of X keysym definitions.
;;;;  * XFree86 never properly commented these keysyms, so we have done our
;;;;  * best to explain the semantic meaning of these keys.
;;;;  *
;;;;  * XFree86 has removed their mail archives of the period, that might have
;;;;  * shed more light on some of these definitions. Until/unless we resurrect
;;;;  * these archives, these are from memory and usage.
;;;;  */

;;;; /*
;;;;  * ModeLock
;;;;  *
;;;;  * This one is old, and not really used any more since XKB offers this
;;;;  * functionality.
;;;;  */

(in-package :protoform.controller)

(defconstant +xf86xk-modelock+ #x1008FF01) ;  Mode Switch Lock 

;;;; /* Backlight controls. */
(defconstant +xf86xk-monbrightnessup+ #x1008FF02) ;  Monitor/panel brightness 
(defconstant +xf86xk-monbrightnessdown+ #x1008FF03) ;  Monitor/panel brightness 
(defconstant +xf86xk-kbdlightonoff+ #x1008FF04) ;  Keyboards may be lit     
(defconstant +xf86xk-kbdbrightnessup+ #x1008FF05) ;  Keyboards may be lit     
(defconstant +xf86xk-kbdbrightnessdown+ #x1008FF06) ;  Keyboards may be lit     

;;;; /*
;;;;  * Keys found on some "Internet" keyboards.
;;;;  */
(defconstant +xf86xk-standby+ #x1008FF10) ;  System into standby mode   
(defconstant +xf86xk-audiolowervolume+ #x1008FF11) ;  Volume control down        
(defconstant +xf86xk-audiomute+ #x1008FF12) ;  Mute sound from the system 
(defconstant +xf86xk-audioraisevolume+ #x1008FF13) ;  Volume control up          
(defconstant +xf86xk-audioplay+ #x1008FF14) ;  Start playing of audio >   
(defconstant +xf86xk-audiostop+ #x1008FF15) ;  Stop playing audio         
(defconstant +xf86xk-audioprev+ #x1008FF16) ;  Previous track             
(defconstant +xf86xk-audionext+ #x1008FF17) ;  Next track                 
(defconstant +xf86xk-homepage+ #x1008FF18) ;  Display user's home page   
(defconstant +xf86xk-mail+ #x1008FF19) ;  Invoke user's mail program 
(defconstant +xf86xk-start+ #x1008FF1A) ;  Start application          
(defconstant +xf86xk-search+ #x1008FF1B) ;  Search                     
(defconstant +xf86xk-audiorecord+ #x1008FF1C) ;  Record audio application   

;;;; /* These are sometimes found on PDA's (e.g. Palm, PocketPC or elsewhere)   */
(defconstant +xf86xk-calculator+ #x1008FF1D) ;  Invoke calculator program  
(defconstant +xf86xk-memo+ #x1008FF1E) ;  Invoke Memo taking program 
(defconstant +xf86xk-todolist+ #x1008FF1F) ;  Invoke To Do List program  
(defconstant +xf86xk-calendar+ #x1008FF20) ;  Invoke Calendar program    
(defconstant +xf86xk-powerdown+ #x1008FF21) ;  Deep sleep the system      
(defconstant +xf86xk-contrastadjust+ #x1008FF22) ;  Adjust screen contrast     
(defconstant +xf86xk-rockerup+ #x1008FF23) ;  Rocker switches exist up   
(defconstant +xf86xk-rockerdown+ #x1008FF24) ;  and down                   
(defconstant +xf86xk-rockerenter+ #x1008FF25) ;  and let you press them     

;;;; /* Some more "Internet" keyboard symbols */
(defconstant +xf86xk-back+ #x1008FF26) ;  Like back on a browser     
(defconstant +xf86xk-forward+ #x1008FF27) ;  Like forward on a browser  
(defconstant +xf86xk-stop+ #x1008FF28) ;  Stop current operation     
(defconstant +xf86xk-refresh+ #x1008FF29) ;  Refresh the page           
(defconstant +xf86xk-poweroff+ #x1008FF2A) ;  Power off system entirely  
(defconstant +xf86xk-wakeup+ #x1008FF2B) ;  Wake up system from sleep  
(defconstant +xf86xk-eject+ #x1008FF2C) ;  Eject device (e.g. DVD)    
(defconstant +xf86xk-screensaver+ #x1008FF2D) ;  Invoke screensaver         
(defconstant +xf86xk-www+ #x1008FF2E) ;  Invoke web browser         
(defconstant +xf86xk-sleep+ #x1008FF2F) ;  Put system to sleep        
(defconstant +xf86xk-favorites+ #x1008FF30) ;  Show favorite locations    
(defconstant +xf86xk-audiopause+ #x1008FF31) ;  Pause audio playing        
(defconstant +xf86xk-audiomedia+ #x1008FF32) ;  Launch media collection app 
(defconstant +xf86xk-mycomputer+ #x1008FF33) ;  Display "My Computer" window 
(defconstant +xf86xk-vendorhome+ #x1008FF34) ;  Display vendor home web site 
(defconstant +xf86xk-lightbulb+ #x1008FF35) ;  Light bulb keys exist       
(defconstant +xf86xk-shop+ #x1008FF36) ;  Display shopping web site   
(defconstant +xf86xk-history+ #x1008FF37) ;  Show history of web surfing 
(defconstant +xf86xk-openurl+ #x1008FF38) ;  Open selected URL           
(defconstant +xf86xk-addfavorite+ #x1008FF39) ;  Add URL to favorites list   
(defconstant +xf86xk-hotlinks+ #x1008FF3A) ;  Show "hot" links            
(defconstant +xf86xk-brightnessadjust+ #x1008FF3B) ;  Invoke brightness adj. UI   
(defconstant +xf86xk-finance+ #x1008FF3C) ;  Display financial site      
(defconstant +xf86xk-community+ #x1008FF3D) ;  Display user's community    
(defconstant +xf86xk-audiorewind+ #x1008FF3E) ;  "rewind" audio track        
(defconstant +xf86xk-backforward+ #x1008FF3F) ;  ??? 
(defconstant +xf86xk-launch0+ #x1008FF40) ;  Launch Application          
(defconstant +xf86xk-launch1+ #x1008FF41) ;  Launch Application          
(defconstant +xf86xk-launch2+ #x1008FF42) ;  Launch Application          
(defconstant +xf86xk-launch3+ #x1008FF43) ;  Launch Application          
(defconstant +xf86xk-launch4+ #x1008FF44) ;  Launch Application          
(defconstant +xf86xk-launch5+ #x1008FF45) ;  Launch Application          
(defconstant +xf86xk-launch6+ #x1008FF46) ;  Launch Application          
(defconstant +xf86xk-launch7+ #x1008FF47) ;  Launch Application          
(defconstant +xf86xk-launch8+ #x1008FF48) ;  Launch Application          
(defconstant +xf86xk-launch9+ #x1008FF49) ;  Launch Application          
(defconstant +xf86xk-launcha+ #x1008FF4A) ;  Launch Application          
(defconstant +xf86xk-launchb+ #x1008FF4B) ;  Launch Application          
(defconstant +xf86xk-launchc+ #x1008FF4C) ;  Launch Application          
(defconstant +xf86xk-launchd+ #x1008FF4D) ;  Launch Application          
(defconstant +xf86xk-launche+ #x1008FF4E) ;  Launch Application          
(defconstant +xf86xk-launchf+ #x1008FF4F) ;  Launch Application          

(defconstant +xf86xk-applicationleft+ #x1008FF50) ;  switch to application, left 
(defconstant +xf86xk-applicationright+ #x1008FF51) ;  switch to application, right
(defconstant +xf86xk-book+ #x1008FF52) ;  Launch bookreader           
(defconstant +xf86xk-cd+ #x1008FF53) ;  Launch CD/DVD player        
(defconstant +xf86xk-calculater+ #x1008FF54) ;  Launch Calculater           
(defconstant +xf86xk-clear+ #x1008FF55) ;  Clear window, screen        
(defconstant +xf86xk-close+ #x1008FF56) ;  Close window                
(defconstant +xf86xk-copy+ #x1008FF57) ;  Copy selection              
(defconstant +xf86xk-cut+ #x1008FF58) ;  Cut selection               
(defconstant +xf86xk-display+ #x1008FF59) ;  Output switch key           
(defconstant +xf86xk-dos+ #x1008FF5A) ;  Launch DOS (emulation)      
(defconstant +xf86xk-documents+ #x1008FF5B) ;  Open documents window       
(defconstant +xf86xk-excel+ #x1008FF5C) ;  Launch spread sheet         
(defconstant +xf86xk-explorer+ #x1008FF5D) ;  Launch file explorer        
(defconstant +xf86xk-game+ #x1008FF5E) ;  Launch game                 
(defconstant +xf86xk-go+ #x1008FF5F) ;  Go to URL                   
(defconstant +xf86xk-itouch+ #x1008FF60) ;  Logitch iTouch- don't use   
(defconstant +xf86xk-logoff+ #x1008FF61) ;  Log off system              
(defconstant +xf86xk-market+ #x1008FF62) ;  ??                          
(defconstant +xf86xk-meeting+ #x1008FF63) ;  enter meeting in calendar   
(defconstant +xf86xk-menukb+ #x1008FF65) ;  distingush keyboard from PB 
(defconstant +xf86xk-menupb+ #x1008FF66) ;  distinuish PB from keyboard 
(defconstant +xf86xk-mysites+ #x1008FF67) ;  Favourites                  
(defconstant +xf86xk-new+ #x1008FF68) ;  New (folder, document...    
(defconstant +xf86xk-news+ #x1008FF69) ;  News                        
(defconstant +xf86xk-officehome+ #x1008FF6A) ;  Office home (old Staroffice)
(defconstant +xf86xk-open+ #x1008FF6B) ;  Open                        
(defconstant +xf86xk-option+ #x1008FF6C) ;  ?? 
(defconstant +xf86xk-paste+ #x1008FF6D) ;  Paste                       
(defconstant +xf86xk-phone+ #x1008FF6E) ;  Launch phone; dial number   
(defconstant +xf86xk-q+ #x1008FF70) ;  Compaq's Q - don't use      
(defconstant +xf86xk-reply+ #x1008FF72) ;  Reply e.g., mail            
(defconstant +xf86xk-reload+ #x1008FF73) ;  Reload web page, file, etc. 
(defconstant +xf86xk-rotatewindows+ #x1008FF74) ;  Rotate windows e.g. xrandr  
(defconstant +xf86xk-rotationpb+ #x1008FF75) ;  don't use                   
(defconstant +xf86xk-rotationkb+ #x1008FF76) ;  don't use                   
(defconstant +xf86xk-save+ #x1008FF77) ;  Save (file, document, state 
(defconstant +xf86xk-scrollup+ #x1008FF78) ;  Scroll window/contents up   
(defconstant +xf86xk-scrolldown+ #x1008FF79) ;  Scrool window/contentd down 
(defconstant +xf86xk-scrollclick+ #x1008FF7A) ;  Use XKB mousekeys instead   
(defconstant +xf86xk-send+ #x1008FF7B) ;  Send mail, file, object     
(defconstant +xf86xk-spell+ #x1008FF7C) ;  Spell checker               
(defconstant +xf86xk-splitscreen+ #x1008FF7D) ;  Split window or screen      
(defconstant +xf86xk-support+ #x1008FF7E) ;  Get support (??)            
(defconstant +xf86xk-taskpane+ #x1008FF7F) ;  Show tasks 
(defconstant +xf86xk-terminal+ #x1008FF80) ;  Launch terminal emulator    
(defconstant +xf86xk-tools+ #x1008FF81) ;  toolbox of desktop/app.     
(defconstant +xf86xk-travel+ #x1008FF82) ;  ?? 
(defconstant +xf86xk-userpb+ #x1008FF84) ;  ?? 
(defconstant +xf86xk-user1kb+ #x1008FF85) ;  ?? 
(defconstant +xf86xk-user2kb+ #x1008FF86) ;  ?? 
(defconstant +xf86xk-video+ #x1008FF87) ;  Launch video player       
(defconstant +xf86xk-wheelbutton+ #x1008FF88) ;  button from a mouse wheel 
(defconstant +xf86xk-word+ #x1008FF89) ;  Launch word processor     
(defconstant +xf86xk-xfer+ #x1008FF8A)
(defconstant +xf86xk-zoomin+ #x1008FF8B) ;  zoom in view, map, etc.   
(defconstant +xf86xk-zoomout+ #x1008FF8C) ;  zoom out view, map, etc.  

(defconstant +xf86xk-away+ #x1008FF8D) ;  mark yourself as away     
(defconstant +xf86xk-messenger+ #x1008FF8E) ;  as in instant messaging   
(defconstant +xf86xk-webcam+ #x1008FF8F) ;  Launch web camera app.    
(defconstant +xf86xk-mailforward+ #x1008FF90) ;  Forward in mail           
(defconstant +xf86xk-pictures+ #x1008FF91) ;  Show pictures             
(defconstant +xf86xk-music+ #x1008FF92) ;  Launch music application  

(defconstant +xf86xk-battery+ #x1008FF93) ;  Display battery information 
(defconstant +xf86xk-bluetooth+ #x1008FF94) ;  Enable/disable Bluetooth    
(defconstant +xf86xk-wlan+ #x1008FF95) ;  Enable/disable WLAN         
(defconstant +xf86xk-uwb+ #x1008FF96) ;  Enable/disable UWB	    

(defconstant +xf86xk-audioforward+ #x1008FF97) ;  fast-forward audio track    
(defconstant +xf86xk-audiorepeat+ #x1008FF98) ;  toggle repeat mode          
(defconstant +xf86xk-audiorandomplay+ #x1008FF99) ;  toggle shuffle mode         
(defconstant +xf86xk-subtitle+ #x1008FF9A) ;  cycle through subtitle      
(defconstant +xf86xk-audiocycletrack+ #x1008FF9B) ;  cycle through audio tracks  
(defconstant +xf86xk-cycleangle+ #x1008FF9C) ;  cycle through angles        
(defconstant +xf86xk-frameback+ #x1008FF9D) ;  video: go one frame back    
(defconstant +xf86xk-frameforward+ #x1008FF9E) ;  video: go one frame forward 
(defconstant +xf86xk-time+ #x1008FF9F) ;  display, or shows an entry for time seeking 
(defconstant +xf86xk-select+ #x1008FFA0) ;  Select button on joypads and remotes 
(defconstant +xf86xk-view+ #x1008FFA1) ;  Show a view options/properties 
(defconstant +xf86xk-topmenu+ #x1008FFA2) ;  Go to a top-level menu in a video 

(defconstant +xf86xk-red+ #x1008FFA3) ;  Red button                  
(defconstant +xf86xk-green+ #x1008FFA4) ;  Green button                
(defconstant +xf86xk-yellow+ #x1008FFA5) ;  Yellow button               
(defconstant +xf86xk-blue+ #x1008FFA6) ;  Blue button                 

(defconstant +xf86xk-suspend+ #x1008FFA7) ;  Sleep to RAM                
(defconstant +xf86xk-hibernate+ #x1008FFA8) ;  Sleep to disk               
(defconstant +xf86xk-touchpadtoggle+ #x1008FFA9) ;  Toggle between touchpad/trackstick 
(defconstant +xf86xk-touchpadon+ #x1008FFB0) ;  The touchpad got switched on 
(defconstant +xf86xk-touchpadoff+ #x1008FFB1) ;  The touchpad got switched off 

(defconstant +xf86xk-audiomicmute+ #x1008FFB2) ;  Mute the Mic from the system 

(defconstant +xf86xk-keyboard+ #x1008FFB3) ;  User defined keyboard related action 

(defconstant +xf86xk-wwan+ #x1008FFB4) ;  Toggle WWAN (LTE, UMTS, etc.) radio 
(defconstant +xf86xk-rfkill+ #x1008FFB5) ;  Toggle radios on/off 

(defconstant +xf86xk-audiopreset+ #x1008FFB6) ;  Select equalizer preset, e.g. theatre-mode 

;;; /* Keys for special action keys (hot keys) */
;;; /* Virtual terminals on some operating systems */
(defconstant +xf86xk-switch-vt-1+ #x1008FE01)
(defconstant +xf86xk-switch-vt-2+ #x1008FE02)
(defconstant +xf86xk-switch-vt-3+ #x1008FE03)
(defconstant +xf86xk-switch-vt-4+ #x1008FE04)
(defconstant +xf86xk-switch-vt-5+ #x1008FE05)
(defconstant +xf86xk-switch-vt-6+ #x1008FE06)
(defconstant +xf86xk-switch-vt-7+ #x1008FE07)
(defconstant +xf86xk-switch-vt-8+ #x1008FE08)
(defconstant +xf86xk-switch-vt-9+ #x1008FE09)
(defconstant +xf86xk-switch-vt-10+ #x1008FE0A)
(defconstant +xf86xk-switch-vt-11+ #x1008FE0B)
(defconstant +xf86xk-switch-vt-12+ #x1008FE0C)

(defconstant +xf86xk-ungrab+ #x1008FE20) ;  force ungrab               
(defconstant +xf86xk-cleargrab+ #x1008FE21) ;  kill application with grab 
(defconstant +xf86xk-next-vmode+ #x1008FE22) ;  next video mode available  
(defconstant +xf86xk-prev-vmode+ #x1008FE23) ;  prev. video mode available 
(defconstant +xf86xk-logwindowtree+ #x1008FE24) ;  print window tree to log   
(defconstant +xf86xk-loggrabinfo+ #x1008FE25) ;  print all active grabs to log 
