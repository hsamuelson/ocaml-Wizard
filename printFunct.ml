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
  
  
  "
  let one clr () =
    let pr s = 
      save_cursor ();
      ANSITerminal.print_string [ANSITerminal.Bold; clr] s;
      restore_cursor ();
      move_cursor 0 1;
    in

    save_cursor ();
    
    pr "▄█████████████████▄";
    pr "█                 █";
    pr "█                 █";
    pr "█                 █";
    pr "█                 █";
    pr "█      ▄███       █";
    pr "█        ██       █";
    pr "█        ██       █";
    pr "█        ██       █";
    pr "█      ▄████▄     █";
    pr "█                 █";
    pr "█                 █";
    pr "█                 █";
    pr "█                 █";
    pr "█                 █";
    pr "▀█████████████████▀";

    restore_cursor ()


    let two clr () =
      ANSITerminal.print_string [ANSITerminal.Bold; clr]
      
      "▄█████████████████▄"
  


      let  print_hand (c_list : Card.card list) i = 
        let rec aux c_list i =  
        match  c_list with
          | h :: t -> 
          begin
                      (* let w,h = size () in
                      (* set_cursor (1) (1); *)
                      (* move_cursor (20+(i*10)) 0; *)
                      set_cursor ((w/3)*i) (-1);
                      one ANSITerminal.green ();
                      (* print_string [] "hello\n\n"; *)
                      (* save_cursor (); *)
                      aux t (i+1) *)
                      set_cursor (i*25) 1;
                      one ANSITerminal.cyan ();
                      aux t (i+1)
              
          end
          | [] -> ()
        in
        save_cursor ();
        set_cursor 0 1;
        aux c_list i;
        (* one ANSITerminal.green ();
        set_cursor 25 1;
        one ANSITerminal.green (); *)
        restore_cursor ()