#stdio

(de popen (Command Type)
    (native "@" "popen" 'I Command Type))

(de pclose (Stream)
    (native "@" "pclose" 'I Stream))
