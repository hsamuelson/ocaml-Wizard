open ANSITerminal

let intro_screen () = 
  ANSITerminal.print_string [ANSITerminal.red; Bold]
  

 " 
  ▄█     █▄   ▄█   ▄███████▄     ▄████████    ▄████████ ████████▄  
  ███     ███ ███  ██▀     ▄██   ███    ███   ███    ███ ███   ▀███ 
  ███     ███ ███▌       ▄███▀   ███    ███   ███    ███ ███    ███ 
  ███     ███ ███▌  ▀█▀▄███▀▄▄   ███    ███  ▄███▄▄▄▄██▀ ███    ███ 
  ███     ███ ███▌   ▄███▀   ▀ ▀███████████ ▀▀███▀▀▀▀▀   ███    ███ 
  ███     ███ ███  ▄███▀         ███    ███ ▀███████████ ███    ███ 
  ███ ▄█▄ ███ ███  ███▄     ▄█   ███    ███   ███    ███ ███   ▄███ 
   ▀███▀███▀  █▀    ▀████████▀   ███    █▀    ███    ███ ████████▀  
                                              ███    ███            \n";

  ANSITerminal.print_string []
  "                                                 CS3110 Spring 2021\n";
  ANSITerminal.print_string [] 
  "                                              pcm82, ml2359, hes227"




let zero clr () =
  ANSITerminal.print_string [ANSITerminal.Bold; clr]
  "
  ▄█████████████████▄
  █                 █
  █                 █
  █                 █
  █                 █
  █     ▄████▄      █
  █     █    █      █
  █     █    █      █
  █     █    █      █
  █     ▀████▀      █
  █                 █
  █                 █
  █                 █
  █                 █
  █                 █
  ▀█████████████████▀
  
  
  \n"
  let one clr () =
    ANSITerminal.print_string [ANSITerminal.Bold; clr]
    "
    ▄█████████████████▄
    █                 █
    █                 █
    █                 █
    █                 █
    █      ▄███       █
    █        ██       █
    █        ██       █
    █        ██       █
    █      ▄████▄     █
    █                 █
    █                 █
    █                 █
    █                 █
    █                 █
    ▀█████████████████▀
    
    
    \n"
    let two clr () =
      ANSITerminal.print_string [ANSITerminal.Bold; clr]
      "
      ▄█████████████████▄
      █                 █
      █                 █
      █                 █
      █                 █
      █     ▄█████▄     █
      █     ██   ██     █
      █        ▄██▀     █
      █      ▄██▀       █
      █    ▄████▄▄▄     █
      █    ▀▀▀▀▀▀▀▀     █
      █                 █
      █                 █
      █                 █
      █                 █
      ▀█████████████████▀
      
      
      \n"